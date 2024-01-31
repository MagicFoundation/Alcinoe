unit Alcinoe.FMX.BreakText;

interface

{$I Alcinoe.inc}

uses
  System.UITypes,
  System.Types,
  System.Generics.Collections,
  {$IF defined(ALSkiaCanvas)}
  System.Skia.API,
  {$ENDIF}
  {$IF defined(ios)}
  iOSapi.CoreGraphics,
  iOSapi.CoreText,
  {$ENDIF}
  {$IF defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  fmx.types3D,
  {$ENDIF}
  Alcinoe.FMX.Graphics,
  Fmx.types,
  FMX.graphics;

{$REGION ' Skia'}
{$IF defined(ALSkiaCanvas)}
type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALSkBreakTextItem = class(Tobject)
  public
    Typeface: sk_typeface_t;
    Line: String;
    pos: TpointF; // << Pos of the bottom of the text (without descent)
    rect: TrectF;
    fontColor: TalphaColor; // << Not initialised by ALBreakText
    fontStyle: TFontStyles; // << Not initialised by ALBreakText
    id: string; // << Not initialised by ALBreakText
    imgSrc: string; // << Not initialised by ALBreakText
    isEllipsis: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;
  TALSkBreakTextItems = class(TobjectList<TALSkBreakTextItem>);

{*********************}
function ALSkBreakText(
           const APaint: sk_paint_t;
           const AFont: sk_font_t;
           const AFontName: String;
           const AFontSize: single;
           const AFontStyle: TFontStyles;
           const AFontColor: TalphaColor;
           var ARect: TRectF;
           const AText: string;
           const AWordWrap: Boolean;
           const AHTextAlign, AVTextAlign: TTextAlign;
           const ATrimming: TTextTrimming;
           const ABreakTextItems: TALSkBreakTextItems;
           out ATotalLines: integer;
           out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
           const AFirstLineIndent: TpointF;
           const ALineSpacing: single = 0;
           const AEllipsisText: string = '…';
           const AEllipsisFontStyle: TFontStyles = [];
           const AEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
           const AMaxlines: integer = 0): boolean; // Return true if the text was broken into several lines (truncated or not)
{$ENDIF}
{$ENDREGION}

{$REGION ' ANDROID'}
{$IF defined(ANDROID)}
type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALJBreakTextItem = class(Tobject)
  public
    TypeFace: JTypeface;
    line: JString;
    pos: TpointF; // << Pos of the bottom of the text (without descent)
    rect: TrectF;
    fontColor: TalphaColor; // << Not initialised by ALBreakText
    fontStyle: TFontStyles; // << Not initialised by ALBreakText
    id: string; // << Not initialised by ALBreakText
    imgSrc: string; // << Not initialised by ALBreakText
    isEllipsis: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;
  TALJBreakTextItems = class(TobjectList<TALJBreakTextItem>);

{********************}
function ALJBreakText(
           const APaint: JPaint;
           const AFontName: String;
           const AFontSize: single;
           const AFontStyle: TFontStyles;
           const AFontColor: TalphaColor;
           var ARect: TRectF;
           const AText: JString;
           const AWordWrap: Boolean;
           const AHTextAlign, AVTextAlign: TTextAlign;
           const ATrimming: TTextTrimming;
           const ABreakTextItems: TALJBreakTextItems;
           out ATotalLines: integer;
           out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
           const AFirstLineIndent: TpointF;
           const ALineSpacing: single = 0;
           const AEllipsisText: JString = nil;
           const AEllipsisFontStyle: TFontStyles = [];
           const AEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
           const AMaxlines: integer = 0): boolean; // Return true if the text was broken into several lines (truncated or not)
{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}
type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCTBreakTextItem = class(Tobject)
  public
    Line: CTLineRef;
    text: String;
    pos: TpointF; // << Pos of the bottom of the text (without descent)
    rect: TrectF;
    fontColor: TalphaColor; // << Not initialised by ALBreakText
    fontStyle: TFontStyles; // << Not initialised by ALBreakText
    id: string; // << Not initialised by ALBreakText
    imgSrc: string; // << Not initialised by ALBreakText
    isEllipsis: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;
  TALCTBreakTextItems = class(TobjectList<TALCTBreakTextItem>);

{*********************}
function ALCTBreakText(
           const AFontName: String;
           const AFontSize: single;
           const AFontStyle: TFontStyles;
           const AFontColor: TalphaColor;
           var ARect: TRectF;
           const AText: string;
           const AWordWrap: Boolean;
           const AHTextAlign, AVTextAlign: TTextAlign;
           const ATrimming: TTextTrimming; // TTextTrimming.word not yet supported - TTextTrimming.character will be used instead (if someone need, it's not really hard to implement)
           const ABreakTextItems: TALCTBreakTextItems;
           out ATotalLines: integer;
           out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
           const AFirstLineIndent: TpointF;
           const ALineSpacing: single = 0;
           const AEllipsisText: string = '…';
           const AEllipsisFontStyle: TFontStyles = [];
           const AEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
           const AMaxlines: integer = 0): boolean; // Return true if the text was broken into several lines (truncated or not)
{$ENDIF}
{$ENDREGION}

{$REGION ' ALL'}
type

  {~~~~~~~~~~~~~~~~~}
  //TL for TextLayout
  TALTLBreakTextItem = class(Tobject)
  public
    Line: String;
    pos: TpointF; // << Pos of the bottom of the text (without descent)
    rect: TrectF;
    fontColor: TalphaColor; // << Not initialised by ALBreakText
    fontStyle: TFontStyles; // << Not initialised by ALBreakText
    id: string; // << Not initialised by ALBreakText
    imgSrc: string; // << Not initialised by ALBreakText
    isEllipsis: Boolean;
    constructor Create;
  end;
  TALTLBreakTextItems = class(TobjectList<TALTLBreakTextItem>);

{*******************}
function ALTLBreakText(
           const AFontName: String;
           const AFontSize: single;
           const AFontStyle: TFontStyles;
           const AFontColor: TalphaColor;
           var ARect: TRectF;
           const AText: string;
           const AWordWrap: Boolean;
           const AHTextAlign, AVTextAlign: TTextAlign;
           const ATrimming: TTextTrimming;
           const ABreakTextItems: TALTLBreakTextItems;
           out ATotalLines: integer;
           out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
           const AFirstLineIndent: TpointF;
           const ALineSpacing: single = 0;
           const AEllipsisText: string = '…';
           const AEllipsisFontStyle: TFontStyles = [];
           const AEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
           const AMaxlines: integer = 0): boolean; // Return true if the text was broken into several lines (truncated or not)
{$ENDREGION}

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
    FailIfTextBroken: boolean; // default = false
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
// Their is a bug, if you write something like: abcd#13#10<b>abcd</b>
// then you will obtain
//     abcd(<b>)abcd(</b>)
// instead of
//     abcd(#13#10)
//     (<b>)abcd(</b>)
// The workaround is to write instead abcd<b>#13#10abcd</b>
// It's not look very hard to correct by the way
function  ALDrawMultiLineText(
            const AText: String; // support only theses EXACT html tag :
                                 //   <b>...</b>
                                 //   <i>...</i>
                                 //   <font color="#xxxxxx">...</font>
                                 //   <span id="xxx">...</span>
                                 //   <img src="xxx">
                                 // other < > must be encoded with &lt; and &gt;
            var ARect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
            out ATextBroken: boolean; // out => True if the text was broken into several lines.
            out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
            out AAscent: single; // out => the Ascent of the last element (in real pixel)
            out ADescent: Single; // out => the Descent of the last element (in real pixel)
            out AFirstPos: TpointF; // out => the point of the start of the text
            out ALastPos: TpointF; // out => the point of the end of the text
            out AElements: TalTextElements; // out => the list of rect describing all span elements
            out AEllipsisRect: TRectF; // out => the rect of the Ellipsis (if present)
            const AOptions: TALDrawMultiLineTextOptions): TALDrawable; overload;
function  ALDrawMultiLineText(
            const AText: String; // support only theses EXACT html tag :
                                 //   <b>...</b>
                                 //   <i>...</i>
                                 //   <font color="#xxxxxx">...</font>
                                 //   <span id="xxx">...</span>
                                 //   <img src="xxx">
                                 // other < > must be encoded with &lt; and &gt;
            var ARect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
            out ATextBroken: boolean; // True if the text was broken into several lines
            out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
            const AOptions: TALDrawMultiLineTextOptions): TALDrawable; inline; overload;
function  ALDrawMultiLineText(
            const AText: String; // support only theses EXACT html tag :
                                 //   <b>...</b>
                                 //   <i>...</i>
                                 //   <font color="#xxxxxx">...</font>
                                 //   <span id="xxx">...</span>
                                 //   <img src="xxx">
                                 // other < > must be encoded with &lt; and &gt;
            var ARect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
            out ATextBroken: boolean; // out => True if the text was broken into several lines
            const AOptions: TALDrawMultiLineTextOptions): TALDrawable; inline; overload;
function  ALDrawMultiLineText(
            const AText: String; // support only theses EXACT html tag :
                                 //   <b>...</b>
                                 //   <i>...</i>
                                 //   <font color="#xxxxxx">...</font>
                                 //   <span id="xxx">...</span>
                                 //   <img src="xxx">
                                 // other < > must be encoded with &lt; and &gt;
            var ARect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
            const AOptions: TALDrawMultiLineTextOptions): TALDrawable; inline; overload;

implementation

uses
  System.Math.Vectors,
  system.SysUtils,
  System.Character,
  System.Math,
  {$IF defined(ANDROID)}
  Androidapi.JNIBridge,
  Androidapi.Helpers,
  Androidapi.JNI.Os,
  {$ENDIF}
  {$IF defined(IOS)}
  iOSapi.CocoaTypes,
  Macapi.CoreFoundation,
  Macapi.Helpers,
  fmx.types3D,
  fmx.surfaces,
  {$ENDIF}
  FMX.TextLayout,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.FMX.Common,
  Alcinoe.Common;

{****************}
{$IF defined(ALSkiaCanvas)}
constructor TALSkBreakTextItem.Create;
begin
  inherited;
  Typeface := 0;
  Line := '';
  isEllipsis := False;
end;
{$ENDIF}

{****************}
{$IF defined(ALSkiaCanvas)}
destructor TALSkBreakTextItem.Destroy;
begin
  if Typeface <> 0 then
    sk4d_refcnt_unref(typeface);
  inherited;
end;
{$ENDIF}

{****************}
{$IF defined(ALSkiaCanvas)}
function ALSkBreakText(
           const APaint: sk_paint_t;
           const AFont: sk_font_t;
           const AFontName: String;
           const AFontSize: single;
           const AFontStyle: TFontStyles;
           const AFontColor: TalphaColor;
           var ARect: TRectF;
           const AText: string;
           const AWordWrap: Boolean;
           const AHTextAlign, AVTextAlign: TTextAlign;
           const ATrimming: TTextTrimming;
           const ABreakTextItems: TALSkBreakTextItems;
           out ATotalLines: integer;
           out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
           const AFirstLineIndent: TpointF;
           const ALineSpacing: single = 0;
           const AEllipsisText: string = '…';
           const AEllipsisFontStyle: TFontStyles = [];
           const AEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
           const AMaxlines: integer = 0): boolean; // Return true if the text was broken into several lines (truncated or not)

begin

  // Init AAllTextDrawn
  AAllTextDrawn := True;

  // Init LBreakTextItemsStartCount
  var LBreakTextItemsStartCount := ABreakTextItems.Count;

  // Init LMaxWidth / LMaxHeight / LMaxLineWidth / LTotalLinesHeight / etc.
  if ARect.Width > 65535 then ARect.Width := 65535;
  if ARect.height > 65535 then ARect.height := 65535;
  var LMaxWidth: single := ARect.width;
  var LMaxHeight: single := ARect.Height;
  var LPrevMaxLineWidth: Single := 0; // << need this var because we must recalculate the LMaxLineWidth for the last lines after the truncation is maded
  var LMaxLineWidth: single := 0;
  var LTotalLinesHeight: single := 0;
  ATotalLines := 0;
  var LLineIndent: single := AFirstLineIndent.x;

  // Init Lparagraphstyle
  var Lparagraphstyle := sk4d_paragraphstyle_create;
  try

    // Init Ltextstyle
    var Ltextstyle := sk4d_textstyle_create;
    try

      // Update Ltextstyle
      sk4d_textstyle_set_color(Ltextstyle, AFontColor);
      sk4d_textstyle_set_font_size(Ltextstyle, AFontSize);
      sk4d_paragraphstyle_set_text_style(Lparagraphstyle, Ltextstyle);

      // Create LParagraphBuilder
      var LParagraphBuilder := sk4d_paragraphbuilder_create(Lparagraphstyle);
      try

        // Add text to LParagraphBuilder
        sk4d_paragraphbuilder_add_text(LParagraphBuilder, MarshaledAString(UTF8String(AText)));

        // Create Lparagraph
        var LParagraph := sk4d_paragraphbuilder_build(LParagraphBuilder);
        try

          // Layout the paragraph
          sk4d_paragraph_layout(Lparagraph, ARect.Width);

          var LMetrics: Tarray<sk_metrics_t>;
          SetLength(LMetrics, sk4d_paragraph_get_line_metrics(Lparagraph, nil));
          sk4d_paragraph_get_line_metrics(Lparagraph, @LMetrics[0]);

        finally
          sk4d_paragraph_destroy(LParagraph);
        end;

      finally
        sk4d_paragraphbuilder_destroy(LParagraphBuilder);
      end;

    finally
      sk4d_textstyle_destroy(Ltextstyle);
    end;

  finally
    sk4d_paragraphstyle_destroy(Lparagraphstyle);
  end;


























(*
  // Create LCGColor
  var LAlphaColor := TAlphaColorCGFloat.Create(AFontColor);
  var LCGColor := CGColorCreate(ALGetGlobalCGColorSpace, @LAlphaColor);
  try

    // Create LFont
    var LFont := ALGetCTFontRef(AFontName, AFontSize, AFontStyle); // Returns a new font reference for the given name.
    if LFont = nil then raise Exception.Create('Failed to create font');
    try

      // Create ATextString
      var LTextString := CFStringCreateWithCharacters(kCFAllocatorDefault, @AText[Low(string)], Length(AText));
      if LTextString = nil then raise Exception.Create('Failed to create CFString');
      try

        // Create ATextAttr
        var LTextAttr := CFAttributedStringCreateMutable(kCFAllocatorDefault{alloc}, 0{maxLength}); // Creates a mutable attributed string.
        try

          CFAttributedStringReplaceString(LTextAttr, CFRangeMake(0, 0), LTextString); // Modifies the string of an attributed string.
          CFAttributedStringBeginEditing(LTextAttr); // Defers internal consistency-checking and coalescing for a mutable attributed string.
          try
            CFAttributedStringSetAttribute(LTextAttr, CFRangeMake(0, CFStringGetLength(LTextString)), kCTFontAttributeName, LFont);
            CFAttributedStringSetAttribute(LTextAttr, CFRangeMake(0, CFStringGetLength(LTextString)), kCTForegroundColorAttributeName, LCGColor);
            //-----
            var LSettings: array of CTParagraphStyleSetting;
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
            if (compareValue(AFirstLineIndent.x, 0, TEpsilon.position) > 0) then begin
              SetLength(LSettings, length(LSettings) + 1);
              var LFirstLineHeadIndent: CGFloat := AFirstLineIndent.x;
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
            if not AWordWrap then begin
              var LLineBreakMode: Byte;
              SetLength(LSettings, length(LSettings) + 1);
              case ATrimming of
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
            if (compareValue(ALineSpacing, 0, TEpsilon.position) > 0) then begin
              SetLength(LSettings, length(LSettings) + 1);
              var LLineSpacingAdjustment: CGFloat := ALineSpacing;
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
              var LParagraphStyle := CTParagraphStyleCreate(@LSettings[0], Length(LSettings));
              try
                CFAttributedStringSetAttribute(LTextAttr, CFRangeMake(0, CFStringGetLength(LTextString)), kCTParagraphStyleAttributeName, LParagraphStyle);
              finally
                CFRelease(LParagraphStyle);
              end;
            end;
          finally
            CFAttributedStringEndEditing(LTextAttr); // Re-enables internal consistency-checking and coalescing for a mutable attributed string.
          end;

          // Declare LStringRange
          var LStringRange: CFRange;


          /////////////////////////////
          //Break the text in line(s)//
          /////////////////////////////

          // Create an immutable path of a rectangle.
          var LFramePath := CGPathCreateWithRect(
                              ALLowerLeftCGRect(
                                tpointf.create(0,0){aUpperLeftOrigin},
                                ARect.Width{aWidth},
                                ARect.Height - AFirstLineIndent.y{aHeight},
                                ARect.Height - AFirstLineIndent.y{aGridHeight}),
                              nil{transform});
          try

            // Creates an immutable framesetter object from an attributed string. The resultant framesetter object can be used to
            // create and fill text frames with the CTFramesetterCreateFrame call.
            var LFrameSetter := CTFramesetterCreateWithAttributedString(CFAttributedStringRef(LTextAttr));
            if LFrameSetter = nil then raise Exception.Create('Failed to create CTFramesetter');
            try

              // Creates an immutable frame using a framesetter.
              var LFrame := CTFramesetterCreateFrame(
                              LFrameSetter, // framesetter: The framesetter used to create the frame.
                              CFRangeMake(0, 0), // stringRange: The range, of the attributed string that was used to create the framesetter,
                                                 // that is to be typeset in lines fitted into the frame. If the length portion of the range is
                                                 // set to 0, then the framesetter continues to add lines until it runs out of text or space.
                              LFramePath, // path: A CGPath object that specifies the shape of the frame. The path may be non-rectangular
                                          // in versions of OS X v10.7 or later and versions of iOS 4.2 or later.
                              nil); // frameAttributes: Additional attributes that control the frame filling process can be specified here,
                                    // or NULL if there are no such attributes.
              if LFrame = nil then raise Exception.Create('Failed to create CTFrame');
              try

                // Init Llines / LLinesCount
                var Llines := CTFrameGetLines(LFrame); // Return a CFArray object containing the CTLine objects that make up the frame, or, if there are no lines in the frame, an array with no elements.
                var LLinesCount := CFArrayGetCount(Llines);

                // Init result
                result := LLinesCount > 1;

                // Update ABreakTextItems - loop on all lines
                for var I := 0 to LLinesCount - 1 do begin

                  // Break if maxline reach
                  if (AMaxlines > 0) and (ATotalLines >= AMaxlines) then break; // << No need to set the result to true because aLinesCount > 1

                  // Break if not wordwrap
                  if (not AWordWrap) and (ATotalLines >= 1) then break; // << No need to set the result to true because aLinesCount > 1

                  // init Lline / LMeasuredWidth
                  var Lline := CFArrayGetValueAtIndex(Llines, I);
                  var LAscent: CGFloat;
                  var LDescent: CGFloat;
                  var LMeasuredWidth: Double := CTLineGetTypographicBounds(
                                                  Lline, // line: The line whose typographic bounds are calculated.
                                                  @LAscent, // ascent: On output, the ascent of the line. This parameter can be set to NULL if not needed.
                                                  @LDescent, // descent: On output, the descent of the line. This parameter can be set to NULL if not needed.
                                                  nil); // leading: On output, the leading of the line. This parameter can be set to NULL if not needed. (it's look like to be always 0)
                                                        // >> return the typographic width of the line. If the line is invalid, this function returns 0.

                  // Unfortunatly the lines that are wrapped are not trimed with the last space
                  // so LMeasuredWidth is inacurate. so i must trim the trailling char
                  if I < LLinesCount - 1 then LMeasuredWidth := LMeasuredWidth - CTLineGetTrailingWhitespaceWidth(Lline);

                  // Update LCurrLineY
                  var LCurrLineY: single;
                  if I = 0 then LCurrLineY := AFirstLineIndent.y + LAscent
                  else LCurrLineY := LTotalLinesHeight + ALineSpacing + LAscent;

                  // Stop the process if the height exceeds the maximum allowed height
                  if (compareValue(LCurrLineY + LDescent, LMaxHeight, TEpsilon.position) > 0) then begin
                    result := True;
                    AAllTextDrawn := False;
                    break;
                  end;

                  // Update LTotalLinesHeight and LTotalLines
                  LTotalLinesHeight := LCurrLineY + LDescent;
                  inc(ATotalLines);

                  // Llineindent must be initialized here (following the break)
                  // because it needs to correspond to the last item in
                  // ABreakTextItems.
                  if I > 0 then LLineIndent := 0;

                  // Calculate LMaxLineWidth
                  LPrevMaxLineWidth := LMaxLineWidth;
                  LMaxLineWidth := max(LMaxLineWidth, LMeasuredWidth + LLineIndent);

                  // Init LStringRange
                  LStringRange := CTLineGetStringRange(Lline); // return a CFRange structure that contains the range over the backing store string that spawned the glyphs

                  // Update LBreakTextItems
                  var LBreakTextItem := TALCTBreakTextItem.Create;
                  try

                    // LBreakTextItem.Line
                    LBreakTextItem.Line := CFRetain(Lline); // Retains a Core Foundation object. we need this because we will later free the aFrame but still need to access the Line

                    // LBreakTextItem.text
                    if LStringRange.length > 0 then begin
                      var LTmpTextAttr := CFAttributedStringCreateWithSubstring(kCFAllocatorDefault, CFAttributedStringRef(LTextAttr), LStringRange); // return A new attributed string whose string and attributes are copied from the specified range of the supplied attributed string. Returns NULL if there was a problem copying the object.
                      if LTmpTextAttr = nil then raise Exception.Create('Failed to create CFAttributedString');
                      try
                        LBreakTextItem.text := CFStringRefToStr(CFAttributedStringGetString(LTmpTextAttr));  // Return An immutable string containing the characters from aStr, or NULL if there was a problem creating the object.
                      finally
                        cfRelease(LTmpTextAttr);
                      end;
                    end
                    else LBreakTextItem.text := '';

                    // LBreakTextItem.pos / LBreakTextItem.rect
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
                    LBreakTextItem.rect := Trectf.Create(
                                             TPointF.Create(
                                               LBreakTextItem.pos.x,
                                               LBreakTextItem.pos.Y - LAscent),
                                             LMeasuredWidth,
                                             LAscent + LDescent);

                    // Add LBreakTextItem to ABreakTextItems
                    ABreakTextItems.Add(LBreakTextItem);

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

          if (ABreakTextItems.Count > LBreakTextItemsStartCount) and  // if the text was broken into at least one line
             (LStringRange.length > 0) and  // LStringRange was initialised previously
             (LStringRange.location + LStringRange.length < CFAttributedStringGetLength(CFAttributedStringRef(LTextAttr))) then begin // if the last line do not contain all the chars

            // Init result
            result := True;
            AAllTextDrawn := False;

            // If ATrimming = TTextTrimming.None or AEllipsisText = '' then nothing todo
            if (ATrimming <> TTextTrimming.None) and
               (AEllipsisText <> '') then begin

              // Create LEllipsisColor
              if AEllipsisFontColor <> TAlphaColorRec.Null then LAlphaColor := TAlphaColorCGFloat.Create(AEllipsisFontColor)
              else LAlphaColor := TAlphaColorCGFloat.Create(AFontColor);
              var LEllipsisColor := CGColorCreate(ALGetGlobalCGColorSpace, @LAlphaColor);
              try

                // Create LEllipsisFont
                var LEllipsisFont := ALGetCTFontRef(AFontName, AFontSize, AEllipsisFontStyle); // Returns a new font reference for the given name.
                if LEllipsisFont = nil then raise Exception.Create('Failed to create font');
                try

                  // Create LEllipsisString
                  var LEllipsisString := CFStringCreateWithCharacters(kCFAllocatorDefault, @AEllipsisText[Low(string)], Length(AEllipsisText));
                  if LEllipsisString = nil then raise Exception.Create('Failed to create CFString');
                  try

                    // Create LEllipsisAttr
                    var LEllipsisAttr := CFAttributedStringCreateMutable(kCFAllocatorDefault{alloc}, 0{maxLength}); // Creates a mutable attributed string.
                    try

                      CFAttributedStringReplaceString(LEllipsisAttr, CFRangeMake(0, 0), LEllipsisString); // Modifies the string of an attributed string.
                      CFAttributedStringBeginEditing(LEllipsisAttr); // Defers internal consistency-checking and coalescing for a mutable attributed string.
                      try
                        CFAttributedStringSetAttribute(LEllipsisAttr, CFRangeMake(0, CFStringGetLength(LEllipsisString)), kCTFontAttributeName, LEllipsisFont);
                        CFAttributedStringSetAttribute(LEllipsisAttr, CFRangeMake(0, CFStringGetLength(LEllipsisString)), kCTForegroundColorAttributeName, LEllipsisColor);
                      finally
                        CFAttributedStringEndEditing(LEllipsisAttr); // Re-enables internal consistency-checking and coalescing for a mutable attributed string.
                      end;

                      // Create LEllipsisLine
                      var LEllipsisLine := CTLineCreateWithAttributedString(CFAttributedStringRef(LEllipsisAttr)); // Creates a single immutable line object directly from an attributed string.
                      if LEllipsisLine = nil then raise exception.create('Failed to create CTLine');               // Return Value: A reference to a CTLine object if the call was successful; otherwise, NULL.
                      try

                        CFAttributedStringBeginEditing(LTextAttr); // Defers internal consistency-checking and coalescing for a mutable attributed string.
                        try
                          //-----
                          var LSettings: array of CTParagraphStyleSetting;
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
                          var LLineBreakMode: Byte;
                          SetLength(LSettings, length(LSettings) + 1);
                          case ATrimming of
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
                            var LParagraphStyle := CTParagraphStyleCreate(@LSettings[0], Length(LSettings));
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

                        // Init LEllipsisWidth
                        var LAscent: CGFloat;
                        var LDescent: CGFloat;
                        var LEllipsisWidth: Double := CTLineGetTypographicBounds(
                                                        LEllipsisLine, // line: The line whose typographic bounds are calculated.
                                                        @LAscent, // ascent: On output, the ascent of the line. This parameter can be set to NULL if not needed.
                                                        @LDescent, // descent: On output, the descent of the line. This parameter can be set to NULL if not needed.
                                                        nil); // leading: On output, the leading of the line. This parameter can be set to NULL if not needed. (it's look like to be always 0)
                                                              // >> return the typographic width of the line. If the line is invalid, this function returns 0.

                        // Create an immutable path of a rectangle.
                        LFramePath := CGPathCreateWithRect(
                                        ALLowerLeftCGRect(
                                          tpointf.create(0,0){aUpperLeftOrigin},
                                          LMaxWidth - LEllipsisWidth - LLineIndent{aWidth},
                                          ceil(LAscent+LDescent){aHeight}, // +1 because it's seam when height is exact then it's not work
                                          ceil(LAscent+LDescent){aGridHeight}),
                                        nil{transform});
                        try

                          // Creates an immutable framesetter object from an attributed string. The resultant framesetter object can be used to
                          // create and fill text frames with the CTFramesetterCreateFrame call.
                          var LFrameSetter := CTFramesetterCreateWithAttributedString(CFAttributedStringRef(LTextAttr));
                          if LFrameSetter = nil then raise Exception.Create('Failed to create CTFramesetter');
                          try

                            // Creates an immutable frame using a framesetter.
                            var LFrame := CTFramesetterCreateFrame(
                                            LFrameSetter, // framesetter: The framesetter used to create the frame.
                                            CFRangeMake(LStringRange.location, 0), // stringRange: The range, of the attributed string that was used to create the framesetter,
                                                                                   // that is to be typeset in lines fitted into the frame. If the length portion of the range is
                                                                                   // set to 0, then the framesetter continues to add lines until it runs out of text or space.
                                            LFramePath, // path: A CGPath object that specifies the shape of the frame. The path may be non-rectangular
                                                        // in versions of OS X v10.7 or later and versions of iOS 4.2 or later.
                                            nil); // frameAttributes: Additional attributes that control the frame filling process can be specified here,
                                                  // or NULL if there are no such attributes.
                            if LFrame = nil then raise Exception.Create('Failed to create CTFrame');
                            try

                              // Init LBreakTextItem
                              var LBreakTextItem := ABreakTextItems[ABreakTextItems.count - 1];
                              cfRelease(LBreakTextItem.Line);
                              LBreakTextItem.Line := nil; // << I use this as a flag

                              // Init Llines / LLinesCount
                              var Llines := CTFrameGetLines(LFrame); // Return a CFArray object containing the CTLine objects that make up the frame, or, if there are no lines in the frame, an array with no elements.
                              var LLinesCount := CFArrayGetCount(Llines);
                              if LLinesCount > 0 then begin

                                // Init Lline / LMeasuredWidth
                                var Lline := CFArrayGetValueAtIndex(Llines, 0);
                                var LMeasuredWidth: Double := CTLineGetTypographicBounds(
                                                                Lline, // line: The line whose typographic bounds are calculated.
                                                                @LAscent, // ascent: On output, the ascent of the line. This parameter can be set to NULL if not needed.
                                                                @LDescent, // descent: On output, the descent of the line. This parameter can be set to NULL if not needed.
                                                                nil); // leading: On output, the leading of the line. This parameter can be set to NULL if not needed. (it's look like to be always 0)
                                                                      // >> return the typographic width of the line. If the line is invalid, this function returns 0.

                                // Init LStringRange
                                LStringRange := CTLineGetStringRange(Lline); // return a CFRange structure that contains the range over the backing store string that spawned the glyphs

                                // If there is enough space for the text plus ellipsis.
                                if (LStringRange.length > 0) and
                                   (compareValue(LMeasuredWidth - CTLineGetTrailingWhitespaceWidth(Lline), 0, TEpsilon.Position) > 0) and
                                   (compareValue(LMeasuredWidth + LEllipsisWidth, LMaxWidth - LLineIndent, TEpsilon.Position) <= 0) then begin

                                  // Calculate LMaxLineWidth
                                  LMaxLineWidth := max(LPrevMaxLineWidth, LMeasuredWidth + LLineIndent + LEllipsisWidth);

                                  // Update LBreakTextItems.Line
                                  LBreakTextItem.Line := CFRetain(Lline); // Retains a Core Foundation object. we need this because we will later free the aFrame but still need to access the Line

                                  // Update LBreakTextItems.text
                                  if LStringRange.length > 0 then begin
                                    var LTmpTextAttr := CFAttributedStringCreateWithSubstring(kCFAllocatorDefault, CFAttributedStringRef(LTextAttr), LStringRange); // return A new attributed string whose string and attributes are copied from the specified range of the supplied attributed string. Returns NULL if there was a problem copying the object.
                                    if LTmpTextAttr = nil then raise Exception.Create('Failed to create CFAttributedString');
                                    try
                                      LBreakTextItem.text := CFStringRefToStr(CFAttributedStringGetString(LTmpTextAttr));  // Return An immutable string containing the characters from aStr, or NULL if there was a problem creating the object.
                                    finally
                                      cfRelease(LTmpTextAttr);
                                    end;
                                  end
                                  else LBreakTextItem.text := '';

                                  // Update LBreakTextItems.pos & LBreakTextItems.rect
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
                                  LBreakTextItem.rect := Trectf.Create(
                                                           TPointF.Create(
                                                             LBreakTextItem.pos.x,
                                                             LBreakTextItem.pos.Y - LAscent),
                                                           LMeasuredWidth,
                                                           LAscent + LDescent);

                                end;

                              end;

                              // Update LBreakTextItem.rect.Width
                              if LBreakTextItem.Line = nil then LBreakTextItem.rect.Width := 0;

                              // Add the ellipsis line
                              var LEllipsisBreakTextItem := TALCTBreakTextItem.Create;
                              try
                                LEllipsisBreakTextItem.Line := CFRetain(LEllipsisLine); // Retains a Core Foundation object.
                                LEllipsisBreakTextItem.text := AEllipsisText;
                                LEllipsisBreakTextItem.isEllipsis := true;
                                LEllipsisBreakTextItem.pos := TpointF.create(LBreakTextItem.pos.x + LBreakTextItem.rect.Width, LBreakTextItem.pos.y);
                                LEllipsisBreakTextItem.rect := Trectf.Create(
                                                                 TPointF.Create(
                                                                   LEllipsisBreakTextItem.pos.x,
                                                                   LEllipsisBreakTextItem.pos.Y - LAscent), // if LBreakTextItem.Line = nil then AAscent = ascent of the ellipsis
                                                                 LEllipsisWidth,
                                                                 LAscent + LDescent); // if LBreakTextItem.Line = nil then AAscent/ADescent = ascent/descent of the ellipsis
                                ABreakTextItems.Add(LEllipsisBreakTextItem);
                              except
                                ALFreeAndNil(LEllipsisBreakTextItem);
                                raise;
                              end;

                              // Delete the last line if there is not enough space
                              if LBreakTextItem.Line = nil then begin
                                LMaxLineWidth := max(LPrevMaxLineWidth, LLineIndent + LEllipsisWidth);
                                ABreakTextItems.Delete(ABreakTextItems.count - 2);
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

                      finally
                        cfRelease(LEllipsisLine);
                      end;

                    finally
                      CFRelease(LEllipsisAttr);
                    end;

                  finally
                    CFRelease(LEllipsisString);
                  end;

                finally
                  CFRelease(LEllipsisFont);
                end;

              finally
                CGColorRelease(LEllipsisColor);
              end;

            end;

          end
          else if (ABreakTextItems.Count = LBreakTextItemsStartCount) and  // If no line was added
                  (not AText.IsEmpty) then begin // and the text was not empty

            //init result
            result := True;
            AAllTextDrawn := False;

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


  // Initialise ARect
  if compareValue(LMaxLineWidth, LMaxWidth, Tepsilon.Position) < 0 then begin
    case AHTextAlign of
       TTextAlign.Center: begin
                            var LOffset: single := Floor((ARect.Right - LMaxLineWidth - ARect.Left) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                            ARect.Left := ARect.Left + LOffset;
                            ARect.right := ARect.right - LOffset;
                            for var I := LBreakTextItemsStartCount to ABreakTextItems.Count - 1 do begin
                              ABreakTextItems[I].pos.X := ABreakTextItems[I].pos.X - LOffset;
                              ABreakTextItems[I].rect.Offset(-LOffset, 0);
                            end;
                          end;
       TTextAlign.Leading: begin
                             ARect.Right := min(ARect.Right, ARect.Left + LMaxLineWidth);
                           end;
       TTextAlign.Trailing: begin
                              var LOffset: single := Floor(ARect.Right - LMaxLineWidth - ARect.Left); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                              ARect.Left := ARect.Left + LOffset;
                              for var I := LBreakTextItemsStartCount to ABreakTextItems.Count - 1 do begin
                                ABreakTextItems[I].pos.X := ABreakTextItems[I].pos.X - LOffset;
                                ABreakTextItems[I].rect.Offset(-LOffset, 0);
                              end;
                            end;
    end;
  end;
  if compareValue(LTotalLinesHeight, LMaxHeight, Tepsilon.Position) < 0 then begin
    case AVTextAlign of
       TTextAlign.Center: begin
                            var LOffset: single := (ARect.bottom - LTotalLinesHeight - ARect.top) / 2;
                            ARect.top := ARect.top + LOffset;
                            ARect.bottom := ARect.bottom - LOffset;
                          end;
       TTextAlign.Leading: begin
                             ARect.bottom := min(ARect.bottom, ARect.top + LTotalLinesHeight);
                           end;
       TTextAlign.Trailing: begin
                              var LOffset: single := ARect.bottom - LTotalLinesHeight - ARect.top;
                              ARect.top := ARect.top + LOffset;
                            end;
    end;
  end;
*)
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
constructor TALJBreakTextItem.Create;
begin
  inherited;
  TypeFace := nil;
  Line := nil;
  isEllipsis := False;
end;
{$ENDIF}

{*********************}
{$IF defined(ANDROID)}
destructor TALJBreakTextItem.Destroy;
begin
  TypeFace := nil;
  line := Nil;
  inherited;
end;
{$ENDIF}

{*****************************************************************************}
// !! This function is duplicated in ALTLBreakText. Should there be any updates
// to this function, ensure to apply the same changes to ALTLBreakText as well.
{$IF defined(ANDROID)}
function ALJBreakText(
           const APaint: JPaint;
           const AFontName: String;
           const AFontSize: single;
           const AFontStyle: TFontStyles;
           const AFontColor: TalphaColor;
           var ARect: TRectF;
           const AText: JString;
           const AWordWrap: Boolean;
           const AHTextAlign, AVTextAlign: TTextAlign;
           const ATrimming: TTextTrimming;
           const ABreakTextItems: TALJBreakTextItems;
           out ATotalLines: integer;
           out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
           const AFirstLineIndent: TpointF;
           const ALineSpacing: single = 0;
           const AEllipsisText: JString = nil;
           const AEllipsisFontStyle: TFontStyles = [];
           const AEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
           const AMaxlines: integer = 0): boolean; // Return true if the text was broken into several lines or truncated

var
  LLineIndent: Single;
  LTypeface: JTypeFace;
  LEllipsisTypeface: JTypeFace;
  LEllipsisLine: Jstring;
  LEllipsisLineLn: single;
  LEllipsisLinePos: TpointF;
  LEllipsisLineRect: TrectF;
  LFontMetrics: JPaint_FontMetricsInt;
  LMaxWidth: single;
  LCurrLineY: single;

  {~~~~~~~~~~~~~~~~~~~~~~}
  procedure _initEllipsis;
  begin
    if LEllipsisLine = nil then begin
      if AEllipsisText = nil then LEllipsisLine := StringtoJString(string('…'))
      else LEllipsisLine := AEllipsisText;
      //-----
      LEllipsisTypeface := TJTypeface.JavaClass.create(StringToJString(AFontName), ALfontStyleToAndroidStyle(AEllipsisFontStyle));
      APaint.setTypeface(LEllipsisTypeface);
      if AEllipsisFontColor <> TAlphaColorRec.Null then APaint.setColor(integer(AEllipsisFontColor));
      //-----
      LEllipsisLineLn := APaint.measureText(LEllipsisLine);
      //-----
      APaint.setTypeface(LTypeFace);
      if AEllipsisFontColor <> TAlphaColorRec.Null then APaint.setColor(integer(integer(AFontColor)));
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
      LEllipsisLineRect := Trectf.Create(
                             TPointF.Create(
                               LEllipsisLinePos.x,
                               LEllipsisLinePos.Y - (-1*LFontMetrics.ascent)),
                             LEllipsisLineLn,
                             (-1*LFontMetrics.ascent) + LFontMetrics.descent);
    end;
  end;

begin

  //init result
  result := false;
  AAllTextDrawn := true;

  //init ABreakTextItemsStartCount
  var LBreakTextItemsStartCount := ABreakTextItems.Count;

  //init aMaxWidth / aMaxHeight / aMaxLineWidth / ATotalLinesHeight
  if ARect.Width > 65535 then ARect.Width := 65535;
  if ARect.height > 65535 then ARect.height := 65535;
  LMaxWidth := ARect.width;
  var LMaxHeight: single := ARect.Height;
  var LMaxLineWidth: single := 0;
  var LTotalLinesHeight: single := 0;

  //init ATextIdx / ATextLn
  var LTextIdx := 0;
  var LTextLn := AText.length;

  //init APaint
  LTypeface := TJTypeface.JavaClass.create(StringToJString(AFontName), ALfontStyleToAndroidStyle(AFontStyle));
  APaint.setTypeface(LTypeface);
  APaint.setColor(integer(AFontColor));

  //Init metrics / aCurrLineY / aLineHeight
  LFontMetrics := APaint.getFontMetricsInt; // aMetrics.top       => The maximum distance above the baseline for the tallest glyph in the font at a given text size.
                                            // aMetrics.ascent    => The recommended distance above the baseline for singled spaced text.
                                            // aMetrics.descent   => The recommended distance below the baseline for singled spaced text.
                                            // aMetrics.bottom    => The maximum distance below the baseline for the lowest glyph in the font at a given text size
                                            // aMetrics.leading   => The recommended additional space to add between lines of text
  LCurrLineY := AFirstLineIndent.y + (-1*LFontMetrics.ascent); // aMetrics.top and aMetrics.ascent are always returned in negative value
  ATotalLines := 0;
  var LLineHeight: single := LFontMetrics.descent + ALineSpacing + (-1*LFontMetrics.ascent);

  //init AEllipsisLine
  LEllipsisLine := nil;
  LEllipsisLineLn := 0;

  //init aLineIndent
  LLineIndent := AFirstLineIndent.x;

  //if we have at least enalf of height to write the 1rt row
  if comparevalue(AFirstLineIndent.y + LFontMetrics.descent + (-1*LFontMetrics.Ascent),LMaxHeight,Tepsilon.position) <= 0 then begin

    //create ameasuredWidth
    var LMeasuredWidth := TJavaArray<Single>.Create(1);
    try

      //loop still their is some chars
      while LTextIdx < LTextLn do begin

        // init aline
        var LLine: jString := nil;
        var I := AText.indexOf($0D {c}, LTextIdx{start}); // find if their is some #13 (MSWINDOWS linebreak = #13#10)
        var J := AText.indexOf($0A {c}, LTextIdx{start}); // find if their is some #10 (UNIX linebreak = #10)
        if (I >= 0) and (J >= 0) then I := min(I,J)
        else I := max(I, J);
        var LLineEndWithBreakLine: Boolean;
        if I = LTextIdx then begin
          LLine := StringtoJString(string(''));
          LLineEndWithBreakLine := True;
          result := true;
        end
        else if I > 0 then begin
          LLine := AText.substring(LTextIdx{start}, I{end_}); // skip the $0D/$0A
          LLineEndWithBreakLine := True;
          result := true;
        end
        else begin
          LLine := AText.substring(LTextIdx{start});
          LLineEndWithBreakLine := False;
        end;

        //Calculate the number of char in the current line (this work good also if aline is empty)
        //http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
        //* getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
        //* measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
        //  bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
        //  it's return for exemple 9 when height = 11
        var LNumberOfChars := APaint.breakText(
                                LLine {text},
                                true {measureForwards},
                                LMaxWidth - LLineIndent, {maxWidth}
                                LMeasuredWidth {measuredWidth});

        //init result
        if LNumberOfChars < LLine.length then result := true;

        //if we need to break the text
        if (LNumberOfChars < LLine.length) or // << if aNumberOfChars < aLine.length it's evident we will need to break the text
           (                                                                                                        // <<
            (LLineEndWithBreakLine) and                                                                             // <<
            (ATrimming <> TTextTrimming.None) and                                                                   // <<
            (                                                                                                       // << we need this check to add the ellipsis on the last line
             (not AWordWrap) or                                                                                     // << when the last line finish by a break line (#13#10)
             ((compareValue(LCurrLineY + LLineHeight + LFontMetrics.descent, LMaxHeight, Tepsilon.position) > 0) or // <<
              ((AMaxlines > 0) and (ATotalLines >= AMaxlines - 1)))                                                 // <<
            )                                                                                                       // <<
           )                                                                                                        // <<
        then begin

          //if not AWordWrap
          if not AWordWrap then begin
            AAllTextDrawn := False; // aNumberOfChars < aLine.length so in anycase we will not draw all the text
            case ATrimming of
              TTextTrimming.None: begin
                                    if LNumberOfChars > 0 then
                                      LLine := LLine.substring(0, LNumberOfChars);
                                  end;
              TTextTrimming.Character: begin
                                         //-----
                                         _initEllipsis;
                                         //-----
                                         //(aNumberOfChars < aLine.length) to know that we are not here
                                         //because of manual linebreak and dec(aNumberOfChars) because initialy
                                         //we considere that AEllipsisText is only one char
                                         if (LNumberOfChars < LLine.length) then dec(LNumberOfChars);
                                         while LNumberOfChars > 0 do begin
                                           LLine := LLine.substring(0, LNumberOfChars);
                                           LNumberOfChars := APaint.breakText(
                                                               LLine {text},
                                                               true {measureForwards},
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
                                    var LSaveNumberOfChars := LNumberOfChars;
                                    var LSaveNumberOfCharsIsAccurate := False;
                                    while LNumberOfChars > 0 do begin
                                      //----
                                      if (LNumberOfChars >= LLine.length) then begin // if (aNumberOfChars >= aLine.length) then we are here because of manual linebreak
                                        LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                                      end
                                      //----
                                      else if LNumberOfChars >= 2 then begin
                                        var LChar := LLine.charAt(LNumberOfChars-2);
                                        if (not LChar.IsWhiteSpace) or (LChar.ToUCS4Char = $00A0{No-break Space}) then begin
                                          dec(LNumberOfChars);
                                          continue;
                                        end;
                                        LLine := LLine.substring(0, LNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish with space
                                      end
                                      //----
                                      else begin
                                        LNumberOfChars := LSaveNumberOfChars;
                                        //(aNumberOfChars < aLine.length) to know that we are not here
                                        //because of manual linebreak and dec(aNumberOfChars) because initialy
                                        //we considere that AEllipsisText is only one char
                                        if (not LSaveNumberOfCharsIsAccurate) and (LNumberOfChars < LLine.length) then dec(LNumberOfChars);
                                        while LNumberOfChars > 0 do begin
                                          LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                                          LNumberOfChars := APaint.breakText(
                                                              LLine {text},
                                                              true {measureForwards},
                                                              LMaxWidth - LEllipsisLineLn - LLineIndent, {maxWidth}
                                                              LMeasuredWidth {measuredWidth});
                                          if LNumberOfChars >= LLine.length then break;
                                        end;
                                        break;
                                      end;
                                      //----
                                      LNumberOfChars := APaint.breakText(
                                                          LLine {text},
                                                          true {measureForwards},
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

          //if AWordWrap
          else begin

            //We are at the last line and ATrimming <> TTextTrimming.None
            if ((compareValue(LCurrLineY + LLineHeight + LFontMetrics.descent, LMaxHeight, Tepsilon.position) > 0) or
                ((AMaxlines > 0) and (ATotalLines >= AMaxlines - 1))) and
               (ATrimming <> TTextTrimming.None) then begin

              //-----
              AAllTextDrawn := False; // if we are at the last line then in anycase we will not draw all the text
              _initEllipsis;
              //-----
              var LSaveNumberOfChars := LNumberOfChars;
              var LSaveNumberOfCharsIsAccurate := False;
              while LNumberOfChars > 0 do begin
                //----
                if (LNumberOfChars >= LLine.length) then begin // if (aNumberOfChars >= aLine.length) then we are here because of manual linebreak
                  LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                end
                //----
                else if (ATrimming = TTextTrimming.Word) and (LNumberOfChars >= 2) then begin
                  var LChar := LLine.charAt(LNumberOfChars-2);
                  if (not LChar.IsWhiteSpace) or (LChar.ToUCS4Char = $00A0{No-break Space}) then begin
                    dec(LNumberOfChars);
                    continue;
                  end;
                  LLine := LLine.substring(0, LNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish with space
                end
                //----
                else begin
                  LNumberOfChars := LSaveNumberOfChars;
                  //(aNumberOfChars < aLine.length) to know that we are not here
                  //because of manual linebreak and dec(aNumberOfChars) because initialy
                  //we considere that AEllipsisText is only one char
                  if (not LSaveNumberOfCharsIsAccurate) and (LNumberOfChars < LLine.length) then dec(LNumberOfChars);
                  while LNumberOfChars > 0 do begin
                    LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                    LNumberOfChars := APaint.breakText(
                                        LLine {text},
                                        true {measureForwards},
                                        LMaxWidth - LEllipsisLineLn - LLineIndent, {maxWidth}
                                        LMeasuredWidth {measuredWidth});
                    if LNumberOfChars >= LLine.length then break;
                  end;
                  break;
                end;
                //----
                LNumberOfChars := APaint.breakText(
                                    LLine {text},
                                    true {measureForwards},
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

            //We are not at the last line or ATrimming = TTextTrimming.None
            else begin

              //We are at the last line and ATrimming = TTextTrimming.None and more line available
              if (ATrimming = TTextTrimming.None) and
                 ((compareValue(LCurrLineY + LLineHeight + LFontMetrics.descent, LMaxHeight, Tepsilon.position) > 0) or
                  ((AMaxlines > 0) and (ATotalLines >= AMaxlines - 1))) then AAllTextDrawn := False;

              //cut the line
              var LSaveNumberOfChars := LNumberOfChars;
              if LNumberOfChars < LLine.length then inc(LNumberOfChars); // in case the space separator is just after aNumberOfChars
              while LNumberOfChars > 0 do begin
                //-----
                if LNumberOfChars >= 2 then begin
                  var LChar := LLine.charAt(LNumberOfChars-1);
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
                    LNumberOfChars := APaint.breakText(
                                        LLine {text},
                                        true {measureForwards},
                                        LMaxWidth - LLineIndent, {maxWidth}
                                        LMeasuredWidth {measuredWidth});
                    if LNumberOfChars >= LLine.length then break;
                  end;
                  break;
                end;
                //-----
                LNumberOfChars := APaint.breakText(
                                    LLine {text},
                                    true {measureForwards},
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
        LMaxLineWidth := max(LMaxLineWidth, LMeasuredWidth[0] + LEllipsisLineLn + LLineIndent);

        //update ATotalLinesHeight
        LTotalLinesHeight := LCurrLineY + LFontMetrics.descent;

        //their is not enalf of place to write at least one char or
        //we are on the #13/#10
        //NOTE: we need to remove the breakline because for exemple we have
        //coco sur le cocotier#13#10
        //then aline will be equal to
        //coco sur le cocotier
        //we draw this line, and then we increase aCurrLineY := aCurrLineY + aLineHeight;
        //so it's mean the #13#10 was already taking in account, so we must delete it
        if LNumberOfChars <= 0 then begin
          if (LTextIdx + 1 < LTextLn) and
             (AText.codePointAt(LTextIdx) = $0D) and
             (AText.codePointAt(LTextIdx + 1) = $0A) then LTextIdx := LTextIdx + 2 // (MSWINDOWS linebreak = #13#10)
          else if (LTextIdx < LTextLn) and
                  (AText.codePointAt(LTextIdx) = $0A) then LTextIdx := LTextIdx + 1 // (UNIX linebreak = #10)
          else if (LTextIdx < LTextLn) and
                  ((AText.charAT(LTextIdx).IsWhiteSpace) and
                   (AText.charAT(LTextIdx).ToUCS4Char <> $00A0{No-break Space})) then LTextIdx := LTextIdx + 1 // (white space) if we don't have place to write
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
               (AText.CharAT(LTextIdx).IsLowSurrogate) then inc(LTextIdx);
          end;
          LCurrLineY := LCurrLineY + LLineHeight; // go to next line
          inc(ATotalLines);
          LLineIndent := 0;
          if (not AWordWrap) or
             ((AMaxlines > 0) and (ATotalLines >= AMaxlines)) or
             (compareValue(LCurrLineY + LFontMetrics.descent, LMaxHeight, TEpsilon.position) > 0) then break
          else continue;
        end
        else begin
          LTextIdx := LTextIdx + LNumberOfChars;
          if (LTextIdx + 1 < LTextLn) and
             (AText.codePointAt(LTextIdx) = $0D) and
             (AText.codePointAt(LTextIdx + 1) = $0A) then LTextIdx := LTextIdx + 2 // (MSWINDOWS linebreak = #13#10)
          else if (LTextIdx < LTextLn) and
                  (AText.codePointAt(LTextIdx) = $0A) then LTextIdx := LTextIdx + 1;
        end;

        //init LBreakTextItem
        var LBreakTextItem := TALJBreakTextItem.Create;
        try

          //update aBreakTextItem
          LBreakTextItem.TypeFace := LTypeFace;
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
          LBreakTextItem.rect := Trectf.Create(
                                   TPointF.Create(
                                     LBreakTextItem.pos.x,
                                     LBreakTextItem.pos.Y - (-1*LFontMetrics.ascent)),
                                   LMeasuredWidth[0],
                                   (-1*LFontMetrics.ascent) + LFontMetrics.descent);

          //update AEllipsisLinePos / AEllipsisLinerect
          if LEllipsisLine <> nil then begin
            LEllipsisLinePos := TpointF.Create(LBreakTextItem.pos.x + LMeasuredWidth[0], LCurrLineY);
            LEllipsisLineRect := Trectf.Create(
                                   TPointF.Create(
                                     LBreakTextItem.pos.x + LMeasuredWidth[0],
                                     LBreakTextItem.pos.Y - (-1*LFontMetrics.ascent)),
                                   LEllipsisLineLn,
                                   (-1*LFontMetrics.ascent) + LFontMetrics.descent);
          end;

          // update ABreakTextItems
          ABreakTextItems.Add(LBreakTextItem);

        except
          ALFreeAndNil(LBreakTextItem);
          raise;
        end;

        //update aCurrLineY
        LCurrLineY := LCurrLineY + LLineHeight;
        inc(ATotalLines);

        //update aLineIndent
        LLineIndent := 0;

        // stop if not AWordWrap or after maxheight
        if (not AWordWrap) or
           ((AMaxlines > 0) and (ATotalLines >= AMaxlines)) or
           (compareValue(LCurrLineY + LFontMetrics.descent, LMaxHeight, TEpsilon.position) > 0) then break;

        //add the last empty row if their is one
        if (LTextIdx >= LTextLn) and LLineEndWithBreakLine and (LEllipsisLine = nil) then begin

          //init LBreakTextItem
          LBreakTextItem := TALJBreakTextItem.Create;
          try

            //update aBreakTextItem
            LBreakTextItem.TypeFace := LTypeFace;
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
            LBreakTextItem.rect := Trectf.Create(
                                     TPointF.Create(
                                       LBreakTextItem.pos.x,
                                       LBreakTextItem.pos.Y - (-1*LFontMetrics.Ascent)),
                                     0,
                                     (-1*LFontMetrics.Ascent) + LFontMetrics.Descent);

            // update ABreakTextItems
            ABreakTextItems.Add(LBreakTextItem);

          except
            ALFreeAndNil(LBreakTextItem);
            raise;
          end;

          //update aCurrLineY
          LCurrLineY := LCurrLineY + LLineHeight;
          inc(ATotalLines);

        end;

      end;

    finally
      ALFreeAndNil(LMeasuredWidth);
    end;

  end
  else result := true;

  //add the end ellipsis
  if LEllipsisLine <> nil then begin
    var LBreakTextItem := TALJBreakTextItem.Create;
    try
      LBreakTextItem.TypeFace := LEllipsisTypeFace;
      LBreakTextItem.line := LEllipsisLine;
      LBreakTextItem.pos := LEllipsisLinePos;
      LBreakTextItem.rect := LEllipsisLineRect;
      LBreakTextItem.isEllipsis := True;
      ABreakTextItems.Add(LBreakTextItem);
    except
      ALFreeAndNil(LBreakTextItem);
      raise;
    end;
  end;

  //initialise ARect
  if compareValue(LMaxLineWidth, LMaxWidth, Tepsilon.Position) < 0 then begin
    case AHTextAlign of
       TTextAlign.Center: begin
                            var LOffset: single := Floor((ARect.Right - LMaxLineWidth - ARect.Left) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                            ARect.Left := ARect.Left + LOffset;
                            ARect.right := ARect.right - LOffset;
                            for var I := LBreakTextItemsStartCount to ABreakTextItems.Count - 1 do begin
                              ABreakTextItems[I].pos.X := ABreakTextItems[I].pos.X - LOffset;
                              ABreakTextItems[I].rect.Offset(-LOffset, 0);
                            end;
                          end;
       TTextAlign.Leading: begin
                             ARect.Right := min(ARect.Right, ARect.Left + LMaxLineWidth);
                           end;
       TTextAlign.Trailing: begin
                              var LOffset: single := Floor(ARect.Right - LMaxLineWidth - ARect.Left); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                              ARect.Left := ARect.Left + LOffset;
                              for var I := LBreakTextItemsStartCount to ABreakTextItems.Count - 1 do begin
                                ABreakTextItems[I].pos.X := ABreakTextItems[I].pos.X - LOffset;
                                ABreakTextItems[I].rect.Offset(-LOffset, 0);
                              end;
                            end;
    end;
  end;
  if compareValue(LTotalLinesHeight, LMaxHeight, Tepsilon.Position) < 0 then begin
    case AVTextAlign of
       TTextAlign.Center: begin
                            var LOffset: single := (ARect.bottom - LTotalLinesHeight - ARect.top) / 2;
                            ARect.top := ARect.top + LOffset;
                            ARect.bottom := ARect.bottom - LOffset;
                          end;
       TTextAlign.Leading: begin
                             ARect.bottom := min(ARect.bottom, ARect.top + LTotalLinesHeight);
                           end;
       TTextAlign.Trailing: begin
                              var LOffset: single := ARect.bottom - LTotalLinesHeight - ARect.top;
                              ARect.top := ARect.top + LOffset;
                            end;
    end;
  end;

end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
constructor TALCTBreakTextItem.Create;
begin
  inherited;
  Line := nil;
  isEllipsis := False;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
destructor TALCTBreakTextItem.Destroy;
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
function ALCTBreakText(
           const AFontName: String;
           const AFontSize: single;
           const AFontStyle: TFontStyles;
           const AFontColor: TalphaColor;
           var ARect: TRectF;
           const AText: string;
           const AWordWrap: Boolean;
           const AHTextAlign, AVTextAlign: TTextAlign;
           const ATrimming: TTextTrimming; // TTextTrimming.word not yet supported - TTextTrimming.character will be used instead (if someone need, it's not really hard to implement)
           const ABreakTextItems: TALCTBreakTextItems;
           out ATotalLines: integer;
           out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
           const AFirstLineIndent: TpointF;
           const ALineSpacing: single = 0;
           const AEllipsisText: string = '…';
           const AEllipsisFontStyle: TFontStyles = [];
           const AEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
           const AMaxlines: integer = 0): boolean; // Return true if the text was broken into several lines (truncated or not)
begin

  // Init AAllTextDrawn
  AAllTextDrawn := True;

  // Init LBreakTextItemsStartCount
  var LBreakTextItemsStartCount := ABreakTextItems.Count;

  // Init LMaxWidth / LMaxHeight / LMaxLineWidth / LTotalLinesHeight / etc.
  if ARect.Width > 65535 then ARect.Width := 65535;
  if ARect.height > 65535 then ARect.height := 65535;
  var LMaxWidth: single := ARect.width;
  var LMaxHeight: single := ARect.Height;
  var LPrevMaxLineWidth: Single := 0; // << need this var because we must recalculate the LMaxLineWidth for the last lines after the truncation is maded
  var LMaxLineWidth: single := 0;
  var LTotalLinesHeight: single := 0;
  ATotalLines := 0;
  var LLineIndent: single := AFirstLineIndent.x;

  // Create LCGColor
  var LAlphaColor := TAlphaColorCGFloat.Create(AFontColor);
  var LCGColor := CGColorCreate(ALGetGlobalCGColorSpace, @LAlphaColor);
  try

    // Create LFont
    var LFont := ALGetCTFontRef(AFontName, AFontSize, AFontStyle); // Returns a new font reference for the given name.
    if LFont = nil then raise Exception.Create('Failed to create font');
    try

      // Create ATextString
      var LTextString := CFStringCreateWithCharacters(kCFAllocatorDefault, @AText[Low(string)], Length(AText));
      if LTextString = nil then raise Exception.Create('Failed to create CFString');
      try

        // Create ATextAttr
        var LTextAttr := CFAttributedStringCreateMutable(kCFAllocatorDefault{alloc}, 0{maxLength}); // Creates a mutable attributed string.
        try

          CFAttributedStringReplaceString(LTextAttr, CFRangeMake(0, 0), LTextString); // Modifies the string of an attributed string.
          CFAttributedStringBeginEditing(LTextAttr); // Defers internal consistency-checking and coalescing for a mutable attributed string.
          try
            CFAttributedStringSetAttribute(LTextAttr, CFRangeMake(0, CFStringGetLength(LTextString)), kCTFontAttributeName, LFont);
            CFAttributedStringSetAttribute(LTextAttr, CFRangeMake(0, CFStringGetLength(LTextString)), kCTForegroundColorAttributeName, LCGColor);
            //-----
            var LSettings: array of CTParagraphStyleSetting;
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
            if (compareValue(AFirstLineIndent.x, 0, TEpsilon.position) > 0) then begin
              SetLength(LSettings, length(LSettings) + 1);
              var LFirstLineHeadIndent: CGFloat := AFirstLineIndent.x;
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
            if not AWordWrap then begin
              var LLineBreakMode: Byte;
              SetLength(LSettings, length(LSettings) + 1);
              case ATrimming of
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
            if (compareValue(ALineSpacing, 0, TEpsilon.position) > 0) then begin
              SetLength(LSettings, length(LSettings) + 1);
              var LLineSpacingAdjustment: CGFloat := ALineSpacing;
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
              var LParagraphStyle := CTParagraphStyleCreate(@LSettings[0], Length(LSettings));
              try
                CFAttributedStringSetAttribute(LTextAttr, CFRangeMake(0, CFStringGetLength(LTextString)), kCTParagraphStyleAttributeName, LParagraphStyle);
              finally
                CFRelease(LParagraphStyle);
              end;
            end;
          finally
            CFAttributedStringEndEditing(LTextAttr); // Re-enables internal consistency-checking and coalescing for a mutable attributed string.
          end;

          // Declare LStringRange
          var LStringRange: CFRange;


          /////////////////////////////
          //Break the text in line(s)//
          /////////////////////////////

          // Create an immutable path of a rectangle.
          var LFramePath := CGPathCreateWithRect(
                              ALLowerLeftCGRect(
                                tpointf.create(0,0){aUpperLeftOrigin},
                                ARect.Width{aWidth},
                                ARect.Height - AFirstLineIndent.y{aHeight},
                                ARect.Height - AFirstLineIndent.y{aGridHeight}),
                              nil{transform});
          try

            // Creates an immutable framesetter object from an attributed string. The resultant framesetter object can be used to
            // create and fill text frames with the CTFramesetterCreateFrame call.
            var LFrameSetter := CTFramesetterCreateWithAttributedString(CFAttributedStringRef(LTextAttr));
            if LFrameSetter = nil then raise Exception.Create('Failed to create CTFramesetter');
            try

              // Creates an immutable frame using a framesetter.
              var LFrame := CTFramesetterCreateFrame(
                              LFrameSetter, // framesetter: The framesetter used to create the frame.
                              CFRangeMake(0, 0), // stringRange: The range, of the attributed string that was used to create the framesetter,
                                                 // that is to be typeset in lines fitted into the frame. If the length portion of the range is
                                                 // set to 0, then the framesetter continues to add lines until it runs out of text or space.
                              LFramePath, // path: A CGPath object that specifies the shape of the frame. The path may be non-rectangular
                                          // in versions of OS X v10.7 or later and versions of iOS 4.2 or later.
                              nil); // frameAttributes: Additional attributes that control the frame filling process can be specified here,
                                    // or NULL if there are no such attributes.
              if LFrame = nil then raise Exception.Create('Failed to create CTFrame');
              try

                // Init Llines / LLinesCount
                var Llines := CTFrameGetLines(LFrame); // Return a CFArray object containing the CTLine objects that make up the frame, or, if there are no lines in the frame, an array with no elements.
                var LLinesCount := CFArrayGetCount(Llines);

                // Init result
                result := LLinesCount > 1;

                // Update ABreakTextItems - loop on all lines
                for var I := 0 to LLinesCount - 1 do begin

                  // Break if maxline reach
                  if (AMaxlines > 0) and (ATotalLines >= AMaxlines) then break; // << No need to set the result to true because aLinesCount > 1

                  // Break if not wordwrap
                  if (not AWordWrap) and (ATotalLines >= 1) then break; // << No need to set the result to true because aLinesCount > 1

                  // init Lline / LMeasuredWidth
                  var Lline := CFArrayGetValueAtIndex(Llines, I);
                  var LAscent: CGFloat;
                  var LDescent: CGFloat;
                  var LMeasuredWidth: Double := CTLineGetTypographicBounds(
                                                  Lline, // line: The line whose typographic bounds are calculated.
                                                  @LAscent, // ascent: On output, the ascent of the line. This parameter can be set to NULL if not needed.
                                                  @LDescent, // descent: On output, the descent of the line. This parameter can be set to NULL if not needed.
                                                  nil); // leading: On output, the leading of the line. This parameter can be set to NULL if not needed. (it's look like to be always 0)
                                                        // >> return the typographic width of the line. If the line is invalid, this function returns 0.

                  // Unfortunatly the lines that are wrapped are not trimed with the last space
                  // so LMeasuredWidth is inacurate. so i must trim the trailling char
                  if I < LLinesCount - 1 then LMeasuredWidth := LMeasuredWidth - CTLineGetTrailingWhitespaceWidth(Lline);

                  // Update LCurrLineY
                  var LCurrLineY: single;
                  if I = 0 then LCurrLineY := AFirstLineIndent.y + LAscent
                  else LCurrLineY := LTotalLinesHeight + ALineSpacing + LAscent;

                  // Stop the process if the height exceeds the maximum allowed height
                  if (compareValue(LCurrLineY + LDescent, LMaxHeight, TEpsilon.position) > 0) then begin
                    result := True;
                    AAllTextDrawn := False;
                    break;
                  end;

                  // Update LTotalLinesHeight and LTotalLines
                  LTotalLinesHeight := LCurrLineY + LDescent;
                  inc(ATotalLines);

                  // Llineindent must be initialized here (following the break)
                  // because it needs to correspond to the last item in
                  // ABreakTextItems.
                  if I > 0 then LLineIndent := 0;

                  // Calculate LMaxLineWidth
                  LPrevMaxLineWidth := LMaxLineWidth;
                  LMaxLineWidth := max(LMaxLineWidth, LMeasuredWidth + LLineIndent);

                  // Init LStringRange
                  LStringRange := CTLineGetStringRange(Lline); // return a CFRange structure that contains the range over the backing store string that spawned the glyphs

                  // Update LBreakTextItems
                  var LBreakTextItem := TALCTBreakTextItem.Create;
                  try

                    // LBreakTextItem.Line
                    LBreakTextItem.Line := CFRetain(Lline); // Retains a Core Foundation object. we need this because we will later free the aFrame but still need to access the Line

                    // LBreakTextItem.text
                    if LStringRange.length > 0 then begin
                      var LTmpTextAttr := CFAttributedStringCreateWithSubstring(kCFAllocatorDefault, CFAttributedStringRef(LTextAttr), LStringRange); // return A new attributed string whose string and attributes are copied from the specified range of the supplied attributed string. Returns NULL if there was a problem copying the object.
                      if LTmpTextAttr = nil then raise Exception.Create('Failed to create CFAttributedString');
                      try
                        LBreakTextItem.text := CFStringRefToStr(CFAttributedStringGetString(LTmpTextAttr));  // Return An immutable string containing the characters from aStr, or NULL if there was a problem creating the object.
                      finally
                        cfRelease(LTmpTextAttr);
                      end;
                    end
                    else LBreakTextItem.text := '';

                    // LBreakTextItem.pos / LBreakTextItem.rect
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
                    LBreakTextItem.rect := Trectf.Create(
                                             TPointF.Create(
                                               LBreakTextItem.pos.x,
                                               LBreakTextItem.pos.Y - LAscent),
                                             LMeasuredWidth,
                                             LAscent + LDescent);

                    // Add LBreakTextItem to ABreakTextItems
                    ABreakTextItems.Add(LBreakTextItem);

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

          if (ABreakTextItems.Count > LBreakTextItemsStartCount) and  // if the text was broken into at least one line
             (LStringRange.length > 0) and  // LStringRange was initialised previously
             (LStringRange.location + LStringRange.length < CFAttributedStringGetLength(CFAttributedStringRef(LTextAttr))) then begin // if the last line do not contain all the chars

            // Init result
            result := True;
            AAllTextDrawn := False;

            // If ATrimming = TTextTrimming.None or AEllipsisText = '' then nothing todo
            if (ATrimming <> TTextTrimming.None) and
               (AEllipsisText <> '') then begin

              // Create LEllipsisColor
              if AEllipsisFontColor <> TAlphaColorRec.Null then LAlphaColor := TAlphaColorCGFloat.Create(AEllipsisFontColor)
              else LAlphaColor := TAlphaColorCGFloat.Create(AFontColor);
              var LEllipsisColor := CGColorCreate(ALGetGlobalCGColorSpace, @LAlphaColor);
              try

                // Create LEllipsisFont
                var LEllipsisFont := ALGetCTFontRef(AFontName, AFontSize, AEllipsisFontStyle); // Returns a new font reference for the given name.
                if LEllipsisFont = nil then raise Exception.Create('Failed to create font');
                try

                  // Create LEllipsisString
                  var LEllipsisString := CFStringCreateWithCharacters(kCFAllocatorDefault, @AEllipsisText[Low(string)], Length(AEllipsisText));
                  if LEllipsisString = nil then raise Exception.Create('Failed to create CFString');
                  try

                    // Create LEllipsisAttr
                    var LEllipsisAttr := CFAttributedStringCreateMutable(kCFAllocatorDefault{alloc}, 0{maxLength}); // Creates a mutable attributed string.
                    try

                      CFAttributedStringReplaceString(LEllipsisAttr, CFRangeMake(0, 0), LEllipsisString); // Modifies the string of an attributed string.
                      CFAttributedStringBeginEditing(LEllipsisAttr); // Defers internal consistency-checking and coalescing for a mutable attributed string.
                      try
                        CFAttributedStringSetAttribute(LEllipsisAttr, CFRangeMake(0, CFStringGetLength(LEllipsisString)), kCTFontAttributeName, LEllipsisFont);
                        CFAttributedStringSetAttribute(LEllipsisAttr, CFRangeMake(0, CFStringGetLength(LEllipsisString)), kCTForegroundColorAttributeName, LEllipsisColor);
                      finally
                        CFAttributedStringEndEditing(LEllipsisAttr); // Re-enables internal consistency-checking and coalescing for a mutable attributed string.
                      end;

                      // Create LEllipsisLine
                      var LEllipsisLine := CTLineCreateWithAttributedString(CFAttributedStringRef(LEllipsisAttr)); // Creates a single immutable line object directly from an attributed string.
                      if LEllipsisLine = nil then raise exception.create('Failed to create CTLine');               // Return Value: A reference to a CTLine object if the call was successful; otherwise, NULL.
                      try

                        CFAttributedStringBeginEditing(LTextAttr); // Defers internal consistency-checking and coalescing for a mutable attributed string.
                        try
                          //-----
                          var LSettings: array of CTParagraphStyleSetting;
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
                          var LLineBreakMode: Byte;
                          SetLength(LSettings, length(LSettings) + 1);
                          case ATrimming of
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
                            var LParagraphStyle := CTParagraphStyleCreate(@LSettings[0], Length(LSettings));
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

                        // Init LEllipsisWidth
                        var LAscent: CGFloat;
                        var LDescent: CGFloat;
                        var LEllipsisWidth: Double := CTLineGetTypographicBounds(
                                                        LEllipsisLine, // line: The line whose typographic bounds are calculated.
                                                        @LAscent, // ascent: On output, the ascent of the line. This parameter can be set to NULL if not needed.
                                                        @LDescent, // descent: On output, the descent of the line. This parameter can be set to NULL if not needed.
                                                        nil); // leading: On output, the leading of the line. This parameter can be set to NULL if not needed. (it's look like to be always 0)
                                                              // >> return the typographic width of the line. If the line is invalid, this function returns 0.

                        // Create an immutable path of a rectangle.
                        LFramePath := CGPathCreateWithRect(
                                        ALLowerLeftCGRect(
                                          tpointf.create(0,0){aUpperLeftOrigin},
                                          LMaxWidth - LEllipsisWidth - LLineIndent{aWidth},
                                          ceil(LAscent+LDescent){aHeight}, // +1 because it's seam when height is exact then it's not work
                                          ceil(LAscent+LDescent){aGridHeight}),
                                        nil{transform});
                        try

                          // Creates an immutable framesetter object from an attributed string. The resultant framesetter object can be used to
                          // create and fill text frames with the CTFramesetterCreateFrame call.
                          var LFrameSetter := CTFramesetterCreateWithAttributedString(CFAttributedStringRef(LTextAttr));
                          if LFrameSetter = nil then raise Exception.Create('Failed to create CTFramesetter');
                          try

                            // Creates an immutable frame using a framesetter.
                            var LFrame := CTFramesetterCreateFrame(
                                            LFrameSetter, // framesetter: The framesetter used to create the frame.
                                            CFRangeMake(LStringRange.location, 0), // stringRange: The range, of the attributed string that was used to create the framesetter,
                                                                                   // that is to be typeset in lines fitted into the frame. If the length portion of the range is
                                                                                   // set to 0, then the framesetter continues to add lines until it runs out of text or space.
                                            LFramePath, // path: A CGPath object that specifies the shape of the frame. The path may be non-rectangular
                                                        // in versions of OS X v10.7 or later and versions of iOS 4.2 or later.
                                            nil); // frameAttributes: Additional attributes that control the frame filling process can be specified here,
                                                  // or NULL if there are no such attributes.
                            if LFrame = nil then raise Exception.Create('Failed to create CTFrame');
                            try

                              // Init LBreakTextItem
                              var LBreakTextItem := ABreakTextItems[ABreakTextItems.count - 1];
                              cfRelease(LBreakTextItem.Line);
                              LBreakTextItem.Line := nil; // << I use this as a flag

                              // Init Llines / LLinesCount
                              var Llines := CTFrameGetLines(LFrame); // Return a CFArray object containing the CTLine objects that make up the frame, or, if there are no lines in the frame, an array with no elements.
                              var LLinesCount := CFArrayGetCount(Llines);
                              if LLinesCount > 0 then begin

                                // Init Lline / LMeasuredWidth
                                var Lline := CFArrayGetValueAtIndex(Llines, 0);
                                var LMeasuredWidth: Double := CTLineGetTypographicBounds(
                                                                Lline, // line: The line whose typographic bounds are calculated.
                                                                @LAscent, // ascent: On output, the ascent of the line. This parameter can be set to NULL if not needed.
                                                                @LDescent, // descent: On output, the descent of the line. This parameter can be set to NULL if not needed.
                                                                nil); // leading: On output, the leading of the line. This parameter can be set to NULL if not needed. (it's look like to be always 0)
                                                                      // >> return the typographic width of the line. If the line is invalid, this function returns 0.

                                // Init LStringRange
                                LStringRange := CTLineGetStringRange(Lline); // return a CFRange structure that contains the range over the backing store string that spawned the glyphs

                                // If there is enough space for the text plus ellipsis.
                                if (LStringRange.length > 0) and
                                   (compareValue(LMeasuredWidth - CTLineGetTrailingWhitespaceWidth(Lline), 0, TEpsilon.Position) > 0) and
                                   (compareValue(LMeasuredWidth + LEllipsisWidth, LMaxWidth - LLineIndent, TEpsilon.Position) <= 0) then begin

                                  // Calculate LMaxLineWidth
                                  LMaxLineWidth := max(LPrevMaxLineWidth, LMeasuredWidth + LLineIndent + LEllipsisWidth);

                                  // Update LBreakTextItems.Line
                                  LBreakTextItem.Line := CFRetain(Lline); // Retains a Core Foundation object. we need this because we will later free the aFrame but still need to access the Line

                                  // Update LBreakTextItems.text
                                  if LStringRange.length > 0 then begin
                                    var LTmpTextAttr := CFAttributedStringCreateWithSubstring(kCFAllocatorDefault, CFAttributedStringRef(LTextAttr), LStringRange); // return A new attributed string whose string and attributes are copied from the specified range of the supplied attributed string. Returns NULL if there was a problem copying the object.
                                    if LTmpTextAttr = nil then raise Exception.Create('Failed to create CFAttributedString');
                                    try
                                      LBreakTextItem.text := CFStringRefToStr(CFAttributedStringGetString(LTmpTextAttr));  // Return An immutable string containing the characters from aStr, or NULL if there was a problem creating the object.
                                    finally
                                      cfRelease(LTmpTextAttr);
                                    end;
                                  end
                                  else LBreakTextItem.text := '';

                                  // Update LBreakTextItems.pos & LBreakTextItems.rect
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
                                  LBreakTextItem.rect := Trectf.Create(
                                                           TPointF.Create(
                                                             LBreakTextItem.pos.x,
                                                             LBreakTextItem.pos.Y - LAscent),
                                                           LMeasuredWidth,
                                                           LAscent + LDescent);

                                end;

                              end;

                              // Update LBreakTextItem.rect.Width
                              if LBreakTextItem.Line = nil then LBreakTextItem.rect.Width := 0;

                              // Add the ellipsis line
                              var LEllipsisBreakTextItem := TALCTBreakTextItem.Create;
                              try
                                LEllipsisBreakTextItem.Line := CFRetain(LEllipsisLine); // Retains a Core Foundation object.
                                LEllipsisBreakTextItem.text := AEllipsisText;
                                LEllipsisBreakTextItem.isEllipsis := true;
                                LEllipsisBreakTextItem.pos := TpointF.create(LBreakTextItem.pos.x + LBreakTextItem.rect.Width, LBreakTextItem.pos.y);
                                LEllipsisBreakTextItem.rect := Trectf.Create(
                                                                 TPointF.Create(
                                                                   LEllipsisBreakTextItem.pos.x,
                                                                   LEllipsisBreakTextItem.pos.Y - LAscent), // if LBreakTextItem.Line = nil then AAscent = ascent of the ellipsis
                                                                 LEllipsisWidth,
                                                                 LAscent + LDescent); // if LBreakTextItem.Line = nil then AAscent/ADescent = ascent/descent of the ellipsis
                                ABreakTextItems.Add(LEllipsisBreakTextItem);
                              except
                                ALFreeAndNil(LEllipsisBreakTextItem);
                                raise;
                              end;

                              // Delete the last line if there is not enough space
                              if LBreakTextItem.Line = nil then begin
                                LMaxLineWidth := max(LPrevMaxLineWidth, LLineIndent + LEllipsisWidth);
                                ABreakTextItems.Delete(ABreakTextItems.count - 2);
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

                      finally
                        cfRelease(LEllipsisLine);
                      end;

                    finally
                      CFRelease(LEllipsisAttr);
                    end;

                  finally
                    CFRelease(LEllipsisString);
                  end;

                finally
                  CFRelease(LEllipsisFont);
                end;

              finally
                CGColorRelease(LEllipsisColor);
              end;

            end;

          end
          else if (ABreakTextItems.Count = LBreakTextItemsStartCount) and  // If no line was added
                  (not AText.IsEmpty) then begin // and the text was not empty

            //init result
            result := True;
            AAllTextDrawn := False;

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


  // Initialise ARect
  if compareValue(LMaxLineWidth, LMaxWidth, Tepsilon.Position) < 0 then begin
    case AHTextAlign of
       TTextAlign.Center: begin
                            var LOffset: single := Floor((ARect.Right - LMaxLineWidth - ARect.Left) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                            ARect.Left := ARect.Left + LOffset;
                            ARect.right := ARect.right - LOffset;
                            for var I := LBreakTextItemsStartCount to ABreakTextItems.Count - 1 do begin
                              ABreakTextItems[I].pos.X := ABreakTextItems[I].pos.X - LOffset;
                              ABreakTextItems[I].rect.Offset(-LOffset, 0);
                            end;
                          end;
       TTextAlign.Leading: begin
                             ARect.Right := min(ARect.Right, ARect.Left + LMaxLineWidth);
                           end;
       TTextAlign.Trailing: begin
                              var LOffset: single := Floor(ARect.Right - LMaxLineWidth - ARect.Left); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                              ARect.Left := ARect.Left + LOffset;
                              for var I := LBreakTextItemsStartCount to ABreakTextItems.Count - 1 do begin
                                ABreakTextItems[I].pos.X := ABreakTextItems[I].pos.X - LOffset;
                                ABreakTextItems[I].rect.Offset(-LOffset, 0);
                              end;
                            end;
    end;
  end;
  if compareValue(LTotalLinesHeight, LMaxHeight, Tepsilon.Position) < 0 then begin
    case AVTextAlign of
       TTextAlign.Center: begin
                            var LOffset: single := (ARect.bottom - LTotalLinesHeight - ARect.top) / 2;
                            ARect.top := ARect.top + LOffset;
                            ARect.bottom := ARect.bottom - LOffset;
                          end;
       TTextAlign.Leading: begin
                             ARect.bottom := min(ARect.bottom, ARect.top + LTotalLinesHeight);
                           end;
       TTextAlign.Trailing: begin
                              var LOffset: single := ARect.bottom - LTotalLinesHeight - ARect.top;
                              ARect.top := ARect.top + LOffset;
                            end;
    end;
  end;

end;
{$ENDIF}

{************************************}
constructor TALTLBreakTextItem.Create;
begin
  inherited;
  Line := '';
  isEllipsis := False;
end;

{****************************************************************************}
// !! This function is duplicated in ALJBreakText. Should there be any updates
// to this function, ensure to apply the same changes to ALJBreakText as well.
function ALTLBreakText(
           const AFontName: String;
           const AFontSize: single;
           const AFontStyle: TFontStyles;
           const AFontColor: TalphaColor;
           var ARect: TRectF;
           const AText: string;
           const AWordWrap: Boolean;
           const AHTextAlign, AVTextAlign: TTextAlign;
           const ATrimming: TTextTrimming;
           const ABreakTextItems: TALTLBreakTextItems;
           out ATotalLines: integer;
           out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
           const AFirstLineIndent: TpointF;
           const ALineSpacing: single = 0;
           const AEllipsisText: string = '…';
           const AEllipsisFontStyle: TFontStyles = [];
           const AEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
           const AMaxlines: integer = 0): boolean; // Return true if the text was broken into several lines (truncated or not)

var
  LEllipsisLine: String;
  LEllipsisLineLn: single;
  LEllipsisLinePos: TpointF;
  LEllipsisLineRect: TrectF;
  LMaxWidth: single;
  LLineIndent: Single;
  LCurrLineY: single;
  LAscent, LDescent: Single;

  {~~~~~~~~~~~~~~~~~~}
  function _BreakText(
             const AFontName: String;
             const AFontSize: single;
             const AFontStyle: TFontStyles;
             const AText: String;
             const AMaxWidth: Single;
             var AMeasuredWidth: Single): integer;
  begin
    // this is true on macos and on windows
    // if AText = 'A' (only 1 char)
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
    var LLayout := TTextLayoutManager.DefaultTextLayout.Create;
    try
      LLayout.BeginUpdate;
      LLayout.Font.Family := AFontName;
      LLayout.Font.Size := AFontSize;
      LLayout.Font.Style := AFontStyle;
      LLayout.MaxSize := Tpointf.Create(aMaxWidth, 65535);
      LLayout.Trimming := TTextTrimming.Character;
      LLayout.VerticalAlign := TTextAlign.Leading;
      LLayout.HorizontalAlign := TTextAlign.Leading;
      //seam to not work when LLayout.WordWrap := False;
      //https://quality.embarcadero.com/browse/RSP-39734
      LLayout.WordWrap := True;
      //https://quality.embarcadero.com/browse/RSP-16649
      if (AText <> '') and (AText.Chars[AText.Length - 1].IsLowSurrogate) then LLayout.Text := AText + ' '
      else LLayout.Text := AText;
      LLayout.EndUpdate;
      aMeasuredWidth := LLayout.TextWidth;
      //On macos this function is buggy and you need to update fmx.canvas.mac
      //see https://quality.embarcadero.com/browse/RSP-16648 and https://quality.embarcadero.com/browse/RSP-16649
      //Tepsilon.Position because if PositionAtPoint = exactly aMeasuredWidth then it's return -1
      result := LLayout.PositionAtPoint(TpointF.Create(aMeasuredWidth - Tepsilon.Position,0));
      // remove the extra space we added because of https://quality.embarcadero.com/browse/RSP-16649
      result := min(AText.Length, result);
      if result < 0 then result := 0;
    finally
      ALFreeAndNil(LLayout);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~}
  procedure _initEllipsis;
  begin
    if LEllipsisLine = '' then begin
      if AEllipsisText = '' then LEllipsisLine := '…'
      else LEllipsisLine := AEllipsisText;
      //-----
      _BreakText(
        AFontName, // const AFontName: String;
        AFontSize, // const AFontSize: single;
        AEllipsisFontStyle, // const AFontStyle: TFontStyles;
        LEllipsisLine, // const AText: String;
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
      LEllipsisLineRect := Trectf.Create(
                             TPointF.Create(
                               LEllipsisLinePos.x,
                               LEllipsisLinePos.Y - (-1*LAscent)),
                             LEllipsisLineLn,
                             (-1*LAscent) + LDescent);
    end;
  end;

begin

  //init result
  result := false;
  AAllTextDrawn := True;

  //init ABreakTextItemsStartCount
  var LBreakTextItemsStartCount := ABreakTextItems.Count;

  //init aMaxWidth / aMaxHeight / aMaxLineWidth / ATotalLinesHeight
  if ARect.Width > 65535 then ARect.Width := 65535;
  if ARect.height > 65535 then ARect.height := 65535;
  LMaxWidth := ARect.width;
  var LMaxHeight: single := ARect.Height;
  var LMaxLineWidth: single := 0;
  var LTotalLinesHeight: single := 0;

  //init ATextIdx / ATextLn
  var LTextIdx := 0;
  var LTextLn := AText.length;

  //Init metrics / aCurrLineY / aLineHeight
  var LLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    LLayout.BeginUpdate;
    LLayout.Text := '^_'; // << seam that aLayout.TextHeight will be the same for all characters so doesn't matter what we set here
    LLayout.Font.Family := AFontName;
    LLayout.Font.Size := AFontSize;
    LLayout.Font.Style := AFontStyle;
    LLayout.EndUpdate;
    LAscent := 0;  // << unfortunatly TTextlayout don't gave any ascent/descent ( https://quality.embarcadero.com/browse/RSP-16645
                   // << also the canvas.FillText don't ask the base line of the text but the top/left corner so it's better to say
                   // << that ascent = 0 and descent = height of the font
    LDescent := LLayout.TextHeight;
  finally
    ALFreeAndNil(LLayout);
  end;
  LCurrLineY := AFirstLineIndent.y + (-1*LAscent); // aMetrics.top and aMetrics.ascent are always returned in negative value
  ATotalLines := 0;
  var LLineHeight: single := LDescent + ALineSpacing + (-1*LAscent);

  //init AEllipsisLine
  LEllipsisLine := '';
  LEllipsisLineLn := 0;

  //init aLineIndent
  LLineIndent := AFirstLineIndent.x;

  //if we have at least enalf of height to write the 1rt row
  if comparevalue(AFirstLineIndent.y + LDescent + (-1*LAscent),LMaxHeight,Tepsilon.position) <= 0 then begin

    //loop still their is some chars
    while LTextIdx < LTextLn do begin

      // init aline
      var LLine: String := '';
      var I := AText.indexOf(#13 {c}, LTextIdx{start}); // find if their is some #13 (MSWINDOWS linebreak = #13#10)
      var J := AText.indexOf(#10 {c}, LTextIdx{start}); // find if their is some #10 (UNIX linebreak = #10)
      if (I >= 0) and (J >= 0) then I := min(I,J)
      else I := max(I, J);
      var LLineEndWithBreakLine: Boolean;
      if I = LTextIdx then begin
        LLine := '';
        LLineEndWithBreakLine := True;
        result := true;
      end
      else if I > 0 then begin
        LLine := AText.substring(LTextIdx{startIndex}, I - LTextIdx{length}); // skip the $0D/$0A
        LLineEndWithBreakLine := True;
        result := true;
      end
      else begin
        LLine := AText.substring(LTextIdx{startIndex});
        LLineEndWithBreakLine := False;
      end;

      //calculate the number of char in the current line (this work good also if aline is empty)
      var LMeasuredWidth: Single;
      var LNumberOfChars := _BreakText(
                              AFontName,
                              AFontSize,
                              AFontStyle,
                              LLine {text},
                              LMaxWidth - LLineIndent, {maxWidth}
                              LMeasuredWidth {measuredWidth});

      //init result
      if LNumberOfChars < LLine.length then result := true;

      //if we need to break the text
      if (LNumberOfChars < LLine.length) or // << if aNumberOfChars < aLine.length it's evident we will need to break the text
         (                                                                                                // <<
          (LLineEndWithBreakLine) and                                                                     // <<
          (ATrimming <> TTextTrimming.None) and                                                           // <<
          (                                                                                               // << we need this check to add the ellipsis on the last line
           (not AWordWrap) or                                                                             // << when the last line finish by a break line (#13#10)
           ((compareValue(LCurrLineY + LLineHeight + LDescent, LMaxHeight, Tepsilon.position) > 0) or     // <<
            ((AMaxlines > 0) and (ATotalLines >= AMaxlines - 1)))                                         // <<
          )                                                                                               // <<
         )                                                                                                // <<
      then begin

        //if not AWordWrap
        if not AWordWrap then begin
          AAllTextDrawn := False; // aNumberOfChars < aLine.length so in anycase we will not draw all the text
          case ATrimming of
            TTextTrimming.None: begin
                                  if LNumberOfChars > 0 then
                                    LLine := LLine.substring(0, LNumberOfChars);
                                end;
            TTextTrimming.Character: begin
                                       //-----
                                       _initEllipsis;
                                       //-----
                                       //(aNumberOfChars < aLine.length) to know that we are not here
                                       //because of manual linebreak and dec(aNumberOfChars) because initialy
                                       //we considere that AEllipsisText is only one char
                                       if (LNumberOfChars < LLine.length) then dec(LNumberOfChars);
                                       while LNumberOfChars > 0 do begin
                                         LLine := LLine.substring(0, LNumberOfChars);
                                         LNumberOfChars := _BreakText(
                                                             AFontName,
                                                             AFontSize,
                                                             AFontStyle,
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
                                  var LSaveNumberOfChars := LNumberOfChars;
                                  var LSaveNumberOfCharsIsAccurate := False;
                                  while LNumberOfChars > 0 do begin
                                    //----
                                    if (LNumberOfChars >= LLine.length) then begin // if (aNumberOfChars >= aLine.length) then we are here because of manual linebreak
                                      LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                                    end
                                    //----
                                    else if LNumberOfChars >= 2 then begin
                                      var LChar := LLine.chars[LNumberOfChars-2];
                                      if (not LChar.IsWhiteSpace) or (LChar.ToUCS4Char = $00A0{No-break Space}) then begin
                                        dec(LNumberOfChars);
                                        continue;
                                      end;
                                      LLine := LLine.substring(0, LNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish with space
                                    end
                                    //----
                                    else begin
                                      LNumberOfChars := LSaveNumberOfChars;
                                      //(aNumberOfChars < aLine.length) to know that we are not here
                                      //because of manual linebreak and dec(aNumberOfChars) because initialy
                                      //we considere that AEllipsisText is only one char
                                      if (not LSaveNumberOfCharsIsAccurate) and (LNumberOfChars < LLine.length) then dec(LNumberOfChars);
                                      while LNumberOfChars > 0 do begin
                                        LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                                        LNumberOfChars := _BreakText(
                                                            AFontName,
                                                            AFontSize,
                                                            AFontStyle,
                                                            LLine {text},
                                                            LMaxWidth - LEllipsisLineLn - LLineIndent, {maxWidth}
                                                            LMeasuredWidth {measuredWidth});
                                        if LNumberOfChars >= LLine.length then break;
                                      end;
                                      break;
                                    end;
                                    //----
                                    LNumberOfChars := _BreakText(
                                                        AFontName,
                                                        AFontSize,
                                                        AFontStyle,
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

        //if AWordWrap
        else begin

          //We are at the last line and ATrimming <> TTextTrimming.None
          if ((compareValue(LCurrLineY + LLineHeight + LDescent, LMaxHeight, Tepsilon.position) > 0) or
              ((AMaxlines > 0) and (ATotalLines >= AMaxlines - 1))) and
             (ATrimming <> TTextTrimming.None) then begin

            //-----
            AAllTextDrawn := False; // if we are at the last line then in anycase we will not draw all the text
            _initEllipsis;
            //-----
            var LSaveNumberOfChars := LNumberOfChars;
            var LSaveNumberOfCharsIsAccurate := False;
            while LNumberOfChars > 0 do begin
              //----
              if (LNumberOfChars >= LLine.length) then begin // if (aNumberOfChars >= aLine.length) then we are here because of manual linebreak
                LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
              end
              //----
              else if (ATrimming = TTextTrimming.Word) and (LNumberOfChars >= 2) then begin
                var LChar := LLine.chars[LNumberOfChars-2];
                if (not LChar.IsWhiteSpace) or (LChar.ToUCS4Char = $00A0{No-break Space}) then begin
                  dec(LNumberOfChars);
                  continue;
                end;
                LLine := LLine.substring(0, LNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish with space
              end
              //----
              else begin
                LNumberOfChars := LSaveNumberOfChars;
                //(aNumberOfChars < aLine.length) to know that we are not here
                //because of manual linebreak and dec(aNumberOfChars) because initialy
                //we considere that AEllipsisText is only one char
                if (not LSaveNumberOfCharsIsAccurate) and (LNumberOfChars < LLine.length) then dec(LNumberOfChars);
                while LNumberOfChars > 0 do begin
                  LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                  LNumberOfChars := _BreakText(
                                      AFontName,
                                      AFontSize,
                                      AFontStyle,
                                      LLine {text},
                                      LMaxWidth - LEllipsisLineLn - LLineIndent, {maxWidth}
                                      LMeasuredWidth {measuredWidth});
                  if LNumberOfChars >= LLine.length then break;
                end;
                break;
              end;
              //----
              LNumberOfChars := _BreakText(
                                  AFontName,
                                  AFontSize,
                                  AFontStyle,
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

          //We are not at the last line or ATrimming = TTextTrimming.None
          else begin

            //We are at the last line and ATrimming = TTextTrimming.None and more line available
            if (ATrimming = TTextTrimming.None) and
               ((compareValue(LCurrLineY + LLineHeight + LDescent, LMaxHeight, Tepsilon.position) > 0) or
                ((AMaxlines > 0) and (ATotalLines >= AMaxlines - 1))) then AAllTextDrawn := False;

            //Cut the line
            var LSaveNumberOfChars := LNumberOfChars;
            if LNumberOfChars < LLine.length then inc(LNumberOfChars); // in case the space separator is just after aNumberOfChars
            while LNumberOfChars > 0 do begin
              //-----
              if LNumberOfChars >= 2 then begin
                var LChar := LLine.chars[LNumberOfChars-1];
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
                  LNumberOfChars := _BreakText(
                                      AFontName,
                                      AFontSize,
                                      AFontStyle,
                                      LLine {text},
                                      LMaxWidth - LLineIndent, {maxWidth}
                                      LMeasuredWidth {measuredWidth});
                  if LNumberOfChars >= LLine.length then break;
                end;
                break;
              end;
              //-----
              LNumberOfChars := _BreakText(
                                  AFontName,
                                  AFontSize,
                                  AFontStyle,
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

      // update ATotalLinesHeight
      LTotalLinesHeight := LCurrLineY + LDescent;

      // their is not enalf of place to write at least one char or
      // we are on the #13/#10
      // NOTE: we need to remove the breakline because for exemple we have
      // coco sur le cocotier#13#10
      // then aline will be equal to
      // coco sur le cocotier
      // we draw this line, and then we increase aCurrLineY := aCurrLineY + aLineHeight;
      // so it's mean the #13#10 was already taking in account, so we must delete it
      if LNumberOfChars <= 0 then begin
        if (LTextIdx + 1 < LTextLn) and
           (AText.Chars[LTextIdx] = #13) and
           (AText.Chars[LTextIdx + 1] = #10) then LTextIdx := LTextIdx + 2 // (MSWINDOWS linebreak = #13#10)
        else if (LTextIdx < LTextLn) and
                (AText.Chars[LTextIdx] = #10) then LTextIdx := LTextIdx + 1 // (UNIX linebreak = #10)
        else if (LTextIdx < LTextLn) and
                ((AText.Chars[LTextIdx].IsWhiteSpace) and
                 (AText.Chars[LTextIdx].ToUCS4Char <> $00A0{No-break Space})) then LTextIdx := LTextIdx + 1 // (white space) if we don't have place to write
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
             (AText.Chars[LTextIdx].IsLowSurrogate) then inc(LTextIdx);
        end;
        LCurrLineY := LCurrLineY + LLineHeight; // go to next line
        inc(ATotalLines);
        LLineIndent := 0;
        if (not AWordWrap) or
           ((AMaxlines > 0) and (ATotalLines >= AMaxlines)) or
           (compareValue(LCurrLineY + LDescent, LMaxHeight, TEpsilon.position) > 0) then break
        else continue;
      end
      else begin
        LTextIdx := LTextIdx + LNumberOfChars;
        if (LTextIdx + 1 < LTextLn) and
           (AText.Chars[LTextIdx] = #13) and
           (AText.Chars[LTextIdx + 1] = #10) then LTextIdx := LTextIdx + 2 // (MSWINDOWS linebreak = #13#10)
        else if (LTextIdx < LTextLn) and
                (AText.Chars[LTextIdx] = #10) then LTextIdx := LTextIdx + 1;
      end;

      //init LBreakTextItem
      var LBreakTextItem := TALTLBreakTextItem.Create;
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
        LBreakTextItem.rect := Trectf.Create(
                                 TPointF.Create(
                                   LBreakTextItem.pos.x,
                                   LBreakTextItem.pos.Y - (-1*LAscent)),
                                 LMeasuredWidth,
                                 (-1*LAscent) + LDescent);

        //update AEllipsisLinePos / AEllipsisLinerect
        if LEllipsisLine <> '' then begin
          LEllipsisLinePos := TpointF.Create(LBreakTextItem.pos.x + LMeasuredWidth, LCurrLineY);
          LEllipsisLineRect := Trectf.Create(
                                 TPointF.Create(
                                   LBreakTextItem.pos.x + LMeasuredWidth,
                                   LBreakTextItem.pos.Y - (-1*LAscent)),
                                 LEllipsisLineLn,
                                 (-1*LAscent) + LDescent);
        end;

        // update ABreakTextItems
        ABreakTextItems.Add(LBreakTextItem);

      except
        ALFreeAndNil(LBreakTextItem);
        raise;
      end;

      //update aCurrLineY
      LCurrLineY := LCurrLineY + LLineHeight;
      inc(ATotalLines);

      //update aLineIndent
      LLineIndent := 0;

      // stop if not AWordWrap or after maxheight
      if (not AWordWrap) or
         ((AMaxlines > 0) and (ATotalLines >= AMaxlines)) or
         (compareValue(LCurrLineY + LDescent, LMaxHeight, TEpsilon.position) > 0) then break;

      //add the last empty row if their is one
      if (LTextIdx >= LTextLn) and LLineEndWithBreakLine and (LEllipsisLine = '') then begin

        //init LBreakTextItem
        LBreakTextItem := TALTLBreakTextItem.Create;
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
          LBreakTextItem.rect := Trectf.Create(
                                   TPointF.Create(
                                     LBreakTextItem.pos.x,
                                     LBreakTextItem.pos.Y - (-1*LAscent)),
                                   0,
                                   (-1*LAscent) + LDescent);

          // update ABreakTextItems
          ABreakTextItems.Add(LBreakTextItem);

        except
          ALFreeAndNil(LBreakTextItem);
          raise;
        end;

        //update aCurrLineY
        LCurrLineY := LCurrLineY + LLineHeight;
        inc(ATotalLines);

      end;

    end;

  end
  else result := true;

  //add the end ellipsis
  if LEllipsisLine <> '' then begin
    var LBreakTextItem := TALTLBreakTextItem.Create;
    try
      LBreakTextItem.line := LEllipsisLine;
      LBreakTextItem.pos := LEllipsisLinePos;
      LBreakTextItem.rect := LEllipsisLineRect;
      LBreakTextItem.isEllipsis := True;
      ABreakTextItems.Add(LBreakTextItem);
    except
      ALFreeAndNil(LBreakTextItem);
      raise;
    end;
  end;

  //initialise ARect
  if compareValue(LMaxLineWidth, LMaxWidth, Tepsilon.Position) < 0 then begin
    case AHTextAlign of
       TTextAlign.Center: begin
                            var LOffset: single := Floor((ARect.Right - LMaxLineWidth - ARect.Left) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                            ARect.Left := ARect.Left + LOffset;
                            ARect.right := ARect.right - LOffset;
                            for var I := LBreakTextItemsStartCount to ABreakTextItems.Count - 1 do begin
                              ABreakTextItems[I].pos.X := ABreakTextItems[I].pos.X - LOffset;
                              ABreakTextItems[I].rect.Offset(-LOffset, 0);
                            end;
                          end;
       TTextAlign.Leading: begin
                             ARect.Right := min(ARect.Right, ARect.Left + LMaxLineWidth);
                           end;
       TTextAlign.Trailing: begin
                              var LOffset: single := Floor(ARect.Right - LMaxLineWidth - ARect.Left); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                              ARect.Left := ARect.Left + LOffset;
                              for var I := LBreakTextItemsStartCount to ABreakTextItems.Count - 1 do begin
                                ABreakTextItems[I].pos.X := ABreakTextItems[I].pos.X - LOffset;
                                ABreakTextItems[I].rect.Offset(-LOffset, 0);
                              end;
                            end;
    end;
  end;
  if compareValue(LTotalLinesHeight, LMaxHeight, Tepsilon.Position) < 0 then begin
    case AVTextAlign of
       TTextAlign.Center: begin
                            var LOffset: single := (ARect.bottom - LTotalLinesHeight - ARect.top) / 2;
                            ARect.top := ARect.top + LOffset;
                            ARect.bottom := ARect.bottom - LOffset;
                          end;
       TTextAlign.Leading: begin
                             ARect.bottom := min(ARect.bottom, ARect.top + LTotalLinesHeight);
                           end;
       TTextAlign.Trailing: begin
                              var LOffset: single := ARect.bottom - LTotalLinesHeight - ARect.top;
                              ARect.top := ARect.top + LOffset;
                            end;
    end;
  end;

end;

{*********************************************}
constructor TALDrawMultiLineTextOptions.Create;
begin
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
  FailIfTextBroken := false;
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
function  ALDrawMultiLineText(
            const AText: String; // support only those html tags :
                                 //   <b>...</b>
                                 //   <i>...</i>
                                 //   <font color="#xxxxxx">...</font>
                                 //   <span id="xxx">...</span>
                                 //   <img src="xxx">
                                 // other < > must be encoded with &lt; and &gt;
            var ARect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
            out ATextBroken: boolean; // out => True if the text was broken into several lines.
            out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
            out AAscent: single; // out => the Ascent of the last element (in real pixel)
            out ADescent: Single; // out => the Descent of the last element (in real pixel)
            out AFirstPos: TpointF; // out => the point of the start of the text
            out ALastPos: TpointF; // out => the point of the end of the text
            out AElements: TalTextElements; // out => the list of rect describing all span elements
            out AEllipsisRect: TRectF; // out => the rect of the Ellipsis (if present)
            const AOptions: TALDrawMultiLineTextOptions): TALDrawable;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _getInfosFromTag(
              const aTag: String; // color="#ffffff" id="xxx"
              const aSpanIds: TALStringListW;
              const AFontColors: Tlist<TalphaColor>);
  begin
    if aTag = '' then begin
      aSpanIds.Add('');
      if AFontColors.Count > 0 then AFontColors.Add(AFontColors[AFontColors.Count - 1])
       else AFontColors.Add(AOptions.FontColor);
      exit;
    end;

    var LParamList := TALStringListW.Create;
    try

      ALExtractHeaderFieldsWithQuoteEscaped(
        [' ', #9, #13, #10],
        [' ', #9, #13, #10],
        ['"', ''''],
        PChar(aTag),
        LParamList,
        False,
        True{StripQuotes});

      aSpanIds.Add(LParamList.Values['id']);

      var LColorStr := LParamList.Values['color'];
      if LColorStr <> '' then begin

        if LColorStr[low(LColorStr)] = '#' then begin
          LColorStr[low(LColorStr)] := '$';
          if length(LColorStr) = 7 then insert('ff', LColorStr, 2); // $ffffffff
          var LcolorInt: integer;
          if not ALTryStrToInt(LColorStr, LcolorInt) then begin
            if AFontColors.Count > 0 then AFontColors.Add(AFontColors[AFontColors.Count - 1])
            else AFontColors.Add(AOptions.FontColor);
          end
          else AFontColors.Add(TalphaColor(LcolorInt));
        end
        else begin
          if AFontColors.Count > 0 then AFontColors.Add(AFontColors[AFontColors.Count - 1])
          else AFontColors.Add(AOptions.FontColor);
        end;

      end
      else begin
        if AFontColors.Count > 0 then AFontColors.Add(AFontColors[AFontColors.Count - 1])
        else AFontColors.Add(AOptions.FontColor);
      end;

    finally
      LParamList.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _getInfosFromImg(
              const aTag: String; // src="xxx"
              var aSrc: String);
  begin
    if aTag = '' then begin
      aSrc := '';
      exit;
    end;

    var LParamList := TALStringListW.Create;
    try

      ALExtractHeaderFieldsWithQuoteEscaped(
        [' ', #9, #13, #10],
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

begin

  {$IF defined(ALGpuCanvas)}
  var LMaxTextureSize := TContextManager.DefaultContextClass.MaxTextureSize;
  if LMaxTextureSize > 0 then begin
    ARect.Width := min(ARect.Width, LMaxTextureSize);
    ARect.height := min(ARect.height, LMaxTextureSize);
  end;
  {$ENDIF}

  //init out var
  ATextBroken := false;
  AAllTextDrawn := True;
  AAscent := 0;
  ADescent := 0;
  AFirstPos := TpointF.Create(0,0);
  ALastPos := TpointF.Create(0,0);
  setlength(AElements, 0);
  AEllipsisRect := TRectF.Create(0,0,0,0);

  //init local var
  {$IFDEF IOS}
  var LWhiteSpace := False;
  {$ENDIF}
  var LFirstLineIndent := AOptions.FirstLineIndent;
  var LMaxWidth: single := 0;
  var LMaxHeight: Single := 0;
  var LTotalLines := 0;
  var LBold := 0;
  var LItalic := 0;
  var LFontColors := Tlist<TalphaColor>.create;
  var LSpanIDs := TALStringListW.create;

  {$IF defined(ALSkiaCanvas)}
  var LPaint := ALSkCheckHandle(sk4d_paint_create);
  sk4d_paint_set_antialias(LPaint, true);
  sk4d_paint_set_dither(LPaint, true);
  //-----
  var Lfont := ALSkCheckHandle(
                 sk4d_font_create(
                    0, // typeface: sk_typeface_t;
                    AOptions.FontSize, // size,
                    1.0, // sx, (AScaleX)
                    0.0)); //kx: float (ASkewX)
  //-----
  var LBreakTextItems := TALSkBreakTextItems.Create(true{aOwnsObjects});
  {$ELSEIF defined(ANDROID)}
  var LPaint := TJPaint.JavaClass.init;
  LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  LPaint.setSubpixelText(true); // Enabling this flag causes glyph advances to be computed with subpixel accuracy.
  LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
  LPaint.setTextSize(AOptions.FontSize);
  //-----
  var LBreakTextItems := TALJBreakTextItems.Create(true{aOwnsObjects});
  {$ELSEIF defined(IOS)}
  var LBreakTextItems := TALCTBreakTextItems.Create(true{aOwnsObjects});
  {$ELSE}
  var LBreakTextItems := TALTLBreakTextItems.Create(true{aOwnsObjects});
  {$ENDIF}
  try

    //loop on all the html elements
    var P1 := 1;
    while P1 <= length(AText) do begin

      var LCurrText: String;
      var LCurrImgSrc: String;

      /////////////////////////////////////
      //if the text contain html elements//
      /////////////////////////////////////
      if AOptions.TextIsHtml then begin

        //extract LTag / LCurrText
        if AText[P1] = '<' then begin

          //-----
          LCurrImgSrc := '';
          LCurrText := '';
          var P2 := ALPosW('>', AText, P1+1); // blablabla <font color="#ffffff">blablabla</font> blobloblo
                                              //           ^P1                  ^P2
          if P2 <= 0 then break;
          var LTag: String := ALCopyStr(AText, P1, P2 - P1 + 1); // <font color="#ffffff">
          P1 := P2 + 1; // blablabla <font color="#ffffff">blablabla</font> blobloblo
                        //                                 ^P1

          //-----
          if (ALPosW('<b ', LTag) = 1) or
             (LTag = '<b>') then begin
            _getInfosFromTag(ALCopyStr(LTag, 4, length(LTag) - 4), LSpanIDs, LFontColors);
            inc(LBold);
          end
          else if LTag = '</b>' then begin
            if LSpanIDs.count > 0 then LSpanIDs.Delete(LSpanIDs.Count - 1);
            if LFontColors.count > 0 then LFontColors.Delete(LFontColors.Count - 1);
            dec(LBold);
          end

          //-----
          else if (ALPosW('<img ', LTag) = 1) or
                  (LTag = '<img/>') then begin // <img src="xxx">
            _getInfosFromImg(ALCopyStr(LTag, 6, length(LTag) - 6), LCurrImgSrc);
            LCurrText := '⬛';
          end

          //-----
          else if (ALPosW('<i ', LTag) = 1) or
                  (LTag = '<i>') then begin
            _getInfosFromTag(ALCopyStr(LTag, 4, length(LTag) - 4), LSpanIDs, LFontColors);
            inc(LItalic)
          end
          else if LTag = '</i>' then begin
            if LSpanIDs.count > 0 then LSpanIDs.Delete(LSpanIDs.Count - 1);
            if LFontColors.count > 0 then LFontColors.Delete(LFontColors.Count - 1);
            dec(LItalic)
          end

          //-----
          else if (ALPosW('<font ', LTag) = 1) or
                  (LTag = '<font>')  then begin   // <font color="#ffffff">
            _getInfosFromTag(ALCopyStr(LTag, 7, length(LTag) - 7), LSpanIDs, LFontColors);
          end
          else if LTag = '</font>' then begin
            if LSpanIDs.count > 0 then LSpanIDs.Delete(LSpanIDs.Count - 1);
            if LFontColors.count > 0 then LFontColors.Delete(LFontColors.Count - 1);
          end

          //-----
          else if (ALPosW('<span ', LTag) = 1) or
                  (LTag = '<span>') then begin // <span id="xxx">
            _getInfosFromTag(ALCopyStr(LTag, 7, length(LTag) - 7), LSpanIDs, LFontColors);
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
          var P2 := ALPosW('<', AText, P1);  // blablabla <font color="#ffffff">blablabla</font> blobloblo
                                             //                                 ^P1      ^P2
          if P2 <= 0 then P2 := Maxint;
          LCurrText := ALCopyStr(AText, P1, P2 - P1);  // blablabla
          LCurrText := ALStringReplaceW(LCurrText, '&gt;', '>', [rfReplaceALL]);
          LCurrText := ALStringReplaceW(LCurrText, '&lt;', '<', [rfReplaceALL]);
          {$IFDEF IOS}
          //because of this http://stackoverflow.com/questions/41334425/ctframesettercreateframe-and-kctparagraphstylespecifierfirstlineheadindent
          if LWhiteSpace then LCurrText := ' ' + LCurrText;
          if (P2 <= length(AText) - 3) and
             (AText[P2 + 1] = 'i') and
             (AText[P2 + 2] = 'm') and
             (AText[P2 + 3] = 'g') then begin
            LWhiteSpace := False;
          end
          else if (P2 > 1) and
                  (P2 <= length(AText)) and
                  (AText[P2 - 1].IsWhiteSpace) then begin
            setlength(LCurrText, length(LCurrText) - 1);
            LWhiteSpace := True;
          end
          else LWhiteSpace := False;
          {$ENDIF}

          P1 := P2; // blablabla <font color="#ffffff">blablabla</font> blobloblo
                    //                                          ^P1
        end;

      end


      ///////////////////////////
      //if the text is NOT html//
      ///////////////////////////
      else begin
        LCurrText := AText;
        LCurrImgSrc := '';
        P1 := Maxint;
      end;


      //////////////////////
      //draw the curr text//
      //////////////////////
      if LCurrText <> '' then begin

        //LFontColor
        var LFontColor: TalphaColor;
        if LFontColors.Count > 0 then LFontColor := LFontColors[LFontColors.Count - 1]
        else LFontColor := AOptions.FontColor;

        //LSpanID
        var LSpanID: String;
        if LSpanIDs.Count > 0 then LSpanID := LSpanIDs[LSpanIDs.Count - 1]
        else LSpanID := '';

        //LStyle
        var LStyle: TfontStyles;
        if ((TFontStyle.fsBold in AOptions.FontStyle) or (LBold > 0)) and
           ((TFontStyle.fsItalic in AOptions.FontStyle) or (LItalic > 0)) then LStyle := [TFontStyle.fsBold, TFontStyle.fsItalic]
        else if ((TFontStyle.fsBold in AOptions.FontStyle) or (LBold > 0)) then LStyle := [TFontStyle.fsBold]
        else if ((TFontStyle.fsItalic in AOptions.FontStyle) or (LItalic > 0)) then LStyle := [TFontStyle.fsItalic]
        else LStyle := [];

        //loop style we draw all the text or at least the ellipsis
        var LTmpRect: TrectF;
        var LTmpTotalLines: integer;
        var LTmpAllTextDrawn: Boolean;
        var LTmpTextBroken: Boolean;
        var LBreakTextItemsCount: integer;
        while True do begin

          //init LTmpRect / LBreakTextItemsCount
          LTmpRect := ARect;
          LTmpRect.Width := LTmpRect.Width - AOptions.Padding.Left - AOptions.Padding.right;
          LTmpRect.Height := LTmpRect.Height - AOptions.Padding.top - AOptions.Padding.bottom;
          LBreakTextItemsCount := LBreakTextItems.Count;

          //break the text
          {$IF defined(ALSkiaCanvas)}
          LTmpTextBroken := ALSkBreakText(
                              LPaint, // const APaint: sk_paint_t;
                              LFont, // const AFont: sk_font_t;
                              AOptions.FontName, // const AFontName: String;
                              AOptions.FontSize, // const AFontSize: single;
                              LStyle, // const AFontStyle: TFontStyles;
                              LFontColor, // const AFontColor: TalphaColor;
                              LTmpRect, // var ARect: TRectF;
                              LCurrText, // const AText: string;
                              AOptions.WordWrap, // const AWordWrap: Boolean;
                              TTextAlign.Leading, TTextAlign.Leading, // const AHTextAlign, AVTextAlign: TTextAlign;
                              AOptions.Trimming, // const ATrimming: TTextTrimming;
                              LBreakTextItems, // const ABreakTextItems: TALBreakTextItems;
                              LTmpTotalLines, // var ATotalLines: integer;
                              LTmpAllTextDrawn, // out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
                              LFirstLineIndent, // const AFirstLineIndent: TpointF;
                              AOptions.LineSpacing, // const ALineSpacing: single = 0;
                              AOptions.EllipsisText, // const AEllipsisText: string = '…';
                              AOptions.EllipsisFontStyle, // const AEllipsisFontStyle: TFontStyles = [];
                              AOptions.EllipsisFontColor, // const AEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                              AOptions.MaxLines - LTotalLines + AlifThen(LTotalLines > 0, 1, 0)); // const AMaxlines: integer = 0
          {$ELSEIF defined(ANDROID)}
          LTmpTextBroken := ALJBreakText(
                              LPaint, // const APaint: JPaint;
                              AOptions.FontName, // const AFontName: String;
                              AOptions.FontSize, // const AFontSize: single;
                              LStyle, // const AFontStyle: TFontStyles;
                              LFontColor, // const AFontColor: TalphaColor;
                              LTmpRect, // var ARect: TRectF;
                              StringtoJString(LCurrText), // const AText: JString;
                              AOptions.WordWrap, //const AWordWrap: Boolean;
                              TTextAlign.Leading, TTextAlign.Leading, //const AHTextAlign, AVTextAlign: TTextAlign;
                              AOptions.Trimming, // const ATrimming: TTextTrimming;
                              LBreakTextItems, // var aBreakTexts: Tarray<Tpair<JString, TpointF>>);
                              LTmpTotalLines, // var ATotalLines: integer
                              LTmpAllTextDrawn, // out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
                              LFirstLineIndent, // const AFirstLineIndent: TpointF;
                              AOptions.LineSpacing, // const ALineSpacing: single = 0;
                              StringtoJString(AOptions.EllipsisText), //  const AEllipsisText: JString = nil;
                              AOptions.EllipsisFontStyle, // const AEllipsisFontStyle: TFontStyles = [];
                              AOptions.EllipsisFontColor, // const AEllipsisFontColor: TalphaColor = TAlphaColorRec.Null
                              AOptions.MaxLines - LTotalLines + AlifThen(LTotalLines > 0, 1, 0)); // const AMaxlines: integer = 0
          {$ELSEIF defined(IOS)}
          LTmpTextBroken := ALCTBreakText(
                              AOptions.FontName, // const AFontName: String;
                              AOptions.FontSize, // const AFontSize: single;
                              LStyle, // const AFontStyle: TFontStyles;
                              LFontColor, // const AFontColor: TalphaColor;
                              LTmpRect, // var ARect: TRectF;
                              LCurrText, // const AText: string;
                              AOptions.WordWrap, // const AWordWrap: Boolean;
                              TTextAlign.Leading, TTextAlign.Leading, // const AHTextAlign, AVTextAlign: TTextAlign;
                              AOptions.Trimming, // const ATrimming: TTextTrimming;
                              LBreakTextItems, // const ABreakTextItems: TALBreakTextItems;
                              LTmpTotalLines, // var ATotalLines: integer;
                              LTmpAllTextDrawn, // out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
                              LFirstLineIndent, // const AFirstLineIndent: TpointF;
                              AOptions.LineSpacing, // const ALineSpacing: single = 0;
                              AOptions.EllipsisText, // const AEllipsisText: string = '…';
                              AOptions.EllipsisFontStyle, // const AEllipsisFontStyle: TFontStyles = [];
                              AOptions.EllipsisFontColor, // const AEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                              AOptions.MaxLines - LTotalLines + AlifThen(LTotalLines > 0, 1, 0)); // const AMaxlines: integer = 0
          {$ELSE}
          LTmpTextBroken := ALTLBreakText(
                              AOptions.FontName, // const AFontName: String;
                              AOptions.FontSize, // const AFontSize: single;
                              LStyle, // const AFontStyle: TFontStyles;
                              LFontColor, // const AFontColor: TalphaColor;
                              LTmpRect, // var ARect: TRectF;
                              LCurrText, // const AText: string;
                              AOptions.WordWrap, // const AWordWrap: Boolean;
                              TTextAlign.Leading, TTextAlign.Leading, // const AHTextAlign, AVTextAlign: TTextAlign;
                              AOptions.Trimming, // const ATrimming: TTextTrimming;
                              LBreakTextItems, // const ABreakTextItems: TALBreakTextItems;
                              LTmpTotalLines, // var ATotalLines: integer;
                              LTmpAllTextDrawn, // out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
                              LFirstLineIndent, // const AFirstLineIndent: TpointF;
                              AOptions.LineSpacing, // const ALineSpacing: single = 0;
                              AOptions.EllipsisText, // const AEllipsisText: string = '…';
                              AOptions.EllipsisFontStyle, // const AEllipsisFontStyle: TFontStyles = [];
                              AOptions.EllipsisFontColor, // const AEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                              AOptions.MaxLines - LTotalLines + AlifThen(LTotalLines > 0, 1, 0)); // const AMaxlines: integer = 0
          {$ENDIF}

          //handle FailIfTextBroken
          if LTmpTextBroken and AOptions.FailIfTextBroken then begin
            ARect.Width := 0;
            ARect.Height := 0;
            exit(ALNullDrawable);
          end;

          //update the img
          if (LCurrImgSrc <> '') and
             (LBreakTextItems.Count - LBreakTextItemsCount = 1) then begin
            var LBreakTextItem := LBreakTextItems[LBreakTextItems.count - 1];
            LBreakTextItem.imgsrc := LCurrImgSrc;
          end;

          //If there was not enough space to write the ellipsis
          if (LBreakTextItems.Count >= 2) and                                                                                        // << more than 2 items
             (LBreakTextItems.Count - LBreakTextItemsCount = 1) and                                                                  // << only 1 item (the ellipsis) was added
             (LBreakTextItems[LBreakTextItems.count - 1].isEllipsis) and                                                             // << last item is an elipsis
             (comparevalue(LBreakTextItems[LBreakTextItems.count - 1].rect.right, LTmpRect.Right, Tepsilon.Position) > 0) then begin // << ellipsis is not inside LTmpRect

            //init LBreakTextItem
            // -1 = the ellipsis '...' (the last item)
            // their is no item for the text that generate the break line
            // because their is even not enalf of place to write the text himself (all the place was used by the ellipsis)
            // -2 = the previous text
            var LBreakTextItem := LBreakTextItems[LBreakTextItems.count - 2];

            //if the ellipsis is not on the same line of the LBreakTextItem then it's mean
            //we don't have enalf of place on one full row to draw the ellipsis so break the loop
            if compareValue(
                 LBreakTextItem.rect.Top,  // << we can not use pos.y because on ios bold text can be 1 or 2 pixel more high the normal text :(
                 LBreakTextItems[LBreakTextItems.count - 1].rect.top,
                 Tepsilon.Position) <> 0 then break;

            //get the params from LBreakTextItem
            LFontColor := LBreakTextItem.fontColor;
            LSpanID := LBreakTextItem.id;
            LStyle := LBreakTextItem.fontStyle;
            setlength(LCurrText, 2 * length(LCurrText));                            // << I put some space in the end of the previous text to force
            for var I := Low(LCurrText) to High(LCurrText) do LCurrText[i] := ' ';  // << the draw of the ellipsis
            {$IF defined(ALSkiaCanvas)}
            LCurrText := LBreakTextItem.line + LCurrText + '_';
            {$ELSEIF defined(ANDROID)}
            LCurrText := JStringtoString(LBreakTextItem.line) + LCurrText + '_';
            {$ELSEIF defined(IOS)}
            LCurrText := LBreakTextItem.text + LCurrText + '_';
            {$ELSE}
            LCurrText := LBreakTextItem.line + LCurrText + '_';
            {$ENDIF}
            LFirstLineIndent := TpointF.Create(LBreakTextItem.rect.left, LBreakTextItem.rect.Top);

            //clean the LBreakTextItems
            for var I := LBreakTextItems.Count - 1 downto LBreakTextItems.Count - 2 do
              LBreakTextItems.Delete(i);

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

        //update ATextBroken
        ATextBroken := ATextBroken or LTmpTextBroken;
        AAllTextDrawn := AAllTextDrawn and LTmpAllTextDrawn;

        //update all the LBreakTextItem
        for var I := LBreakTextItemsCount to LBreakTextItems.Count - 1 do begin
          var LBreakTextItem := LBreakTextItems[i];
          //-----
          if (not LBreakTextItem.isEllipsis) or (AOptions.EllipsisFontColor = TAlphaColorRec.Null) then LBreakTextItem.fontColor := LFontColor
          else LBreakTextItem.fontColor := AOptions.EllipsisFontColor;
          //-----
          if (not LBreakTextItem.isEllipsis) then LBreakTextItem.FontStyle := LStyle
          else LBreakTextItem.FontStyle := AOptions.EllipsisFontStyle;
          //-----
          if (not LBreakTextItem.isEllipsis) then LBreakTextItem.Id := LSpanID
          else LBreakTextItem.Id := '';
        end;

        //Update LFirstLineIndent
        if LBreakTextItems.count > LBreakTextItemsCount then begin
          var LBreakTextItem := LBreakTextItems[LBreakTextItems.count - 1];
          LFirstLineIndent := TpointF.Create(LBreakTextItem.rect.Right, LBreakTextItem.rect.Top);
          if LBreakTextItem.isEllipsis then break;
        end;
        // else break; << we can't break here, it's maybe juste a ' ' we try to write at the end of the line that was deleted by ALBreakText

      end;

    end;

    //initialise ARect
    if AOptions.Autosize or (AOptions.AutosizeX and AOptions.AutosizeY) then begin
      ARect.Width := LMaxWidth + AOptions.Padding.Left + AOptions.Padding.right;
      ARect.Height := LMaxHeight + AOptions.Padding.top + AOptions.Padding.bottom;
    end
    else if AOptions.AutosizeX then ARect.Width := LMaxWidth + AOptions.Padding.Left + AOptions.Padding.right
    else if AOptions.AutosizeY then ARect.Height := LMaxHeight + AOptions.Padding.top + AOptions.Padding.bottom;
    case AOptions.HTextAlign of
      TTextAlign.Center: begin
                           if LBreakTextItems.Count > 0 then begin
                             var LCurrentLineY: single := LBreakTextItems[0].rect.top; // << we can not use pos.y because on ios bold text can be 1 or 2 pixel more high the normal text :(
                             var J := 0;
                             for var I := 1 to LBreakTextItems.Count do begin
                               if (I = LBreakTextItems.Count) or
                                  (compareValue(LCurrentLineY, LBreakTextItems[I].rect.top, Tepsilon.Position) <> 0) then begin
                                 var LOffset: single := Floor(
                                                          (ARect.width -
                                                           LBreakTextItems[I-1].rect.Right -
                                                           AOptions.Padding.Left -
                                                           AOptions.Padding.right) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                                 while J < I do begin
                                   LBreakTextItems[j].pos.X := LBreakTextItems[j].pos.X + AOptions.Padding.Left + LOffset;
                                   LBreakTextItems[j].rect.Offset(AOptions.Padding.Left + LOffset, 0);
                                   inc(J);
                                 end;
                                 if (I <> LBreakTextItems.Count) then LCurrentLineY := LBreakTextItems[I].rect.top;
                               end;
                             end;
                           end;
                         end;
      TTextAlign.Leading: begin
                           for var I := 0 to LBreakTextItems.Count - 1 do begin
                             LBreakTextItems[i].pos.X := LBreakTextItems[i].pos.X + AOptions.Padding.Left;
                             LBreakTextItems[i].rect.Offset(AOptions.Padding.Left, 0);
                           end;
                         end;
      TTextAlign.Trailing: begin
                             if LBreakTextItems.Count > 0 then begin
                               var LCurrentLineY: single := LBreakTextItems[0].rect.top; // << we can not use pos.y because on ios bold text can be 1 or 2 pixel more high the normal text :(
                               var J := 0;
                               for var I := 1 to LBreakTextItems.Count do begin
                                 if (I = LBreakTextItems.Count) or
                                    (compareValue(LCurrentLineY, LBreakTextItems[I].rect.top, Tepsilon.Position) <> 0) then begin
                                   var LOffset: single := Floor(
                                                            (ARect.width -
                                                             LBreakTextItems[I-1].rect.Right -
                                                             AOptions.Padding.Left -
                                                             AOptions.Padding.right)); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                                   while J < I do begin
                                     LBreakTextItems[j].pos.X := LBreakTextItems[j].pos.X + AOptions.Padding.Left + LOffset;
                                     LBreakTextItems[j].rect.Offset(AOptions.Padding.Left + LOffset, 0);
                                     inc(J);
                                   end;
                                   if (I <> LBreakTextItems.Count) then LCurrentLineY := LBreakTextItems[I].rect.top;
                                 end;
                               end;
                             end;
                           end;
    end;
    case AOptions.VTextAlign of
      TTextAlign.Center: begin
                           var LOffset: single := Floor(
                                                    (ARect.height -
                                                     LMaxHeight -
                                                     AOptions.Padding.top -
                                                     AOptions.Padding.bottom) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                           for var I := 0 to LBreakTextItems.Count - 1 do begin
                             LBreakTextItems[I].pos.y := LBreakTextItems[i].pos.y + AOptions.Padding.top + LOffset;
                             LBreakTextItems[I].rect.Offset(0, AOptions.Padding.top + LOffset);
                           end;
                         end;
      TTextAlign.Leading: begin
                           for var I := 0 to LBreakTextItems.Count - 1 do begin
                             LBreakTextItems[i].pos.Y := LBreakTextItems[i].pos.Y + AOptions.Padding.top;
                             LBreakTextItems[i].rect.Offset(0, AOptions.Padding.top);
                           end;
                         end;
      TTextAlign.Trailing: begin
                             var LOffset: single := Floor(
                                                      (ARect.height -
                                                       LMaxHeight -
                                                       AOptions.Padding.top -
                                                       AOptions.Padding.bottom)); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                             for var I := 0 to LBreakTextItems.Count - 1 do begin
                               LBreakTextItems[I].pos.y := LBreakTextItems[i].pos.y + AOptions.Padding.top + LOffset;
                               LBreakTextItems[I].rect.Offset(0, AOptions.Padding.top + LOffset);
                             end;
                           end;
    end;
    ARect := ALAlignDimensionToPixelCeil(ARect, 1{Scale});

    // init out vars
    if LBreakTextItems.count > 0 then begin
      AFirstPos := LBreakTextItems[0].pos;
      var LBreakTextItem := LBreakTextItems[LBreakTextItems.count - 1];
      ALastPos := LBreakTextItem.pos;
      ALastPos.offset(LBreakTextItem.rect.width, 0);
      AAscent := ALastPos.y - LBreakTextItem.rect.top;
      ADescent := LBreakTextItem.rect.bottom - ALastPos.y;
      if LBreakTextItem.isEllipsis then AEllipsisRect := LBreakTextItem.rect
      else AEllipsisRect := Trectf.Create(0,0,0,0);
    end;

    //update AElements
    var J := 0;
    setlength(AElements, LBreakTextItems.count);
    for var i := 0 to LBreakTextItems.count - 1 do begin
      if (LBreakTextItems[i].id <> '') then begin
        AElements[j].Id := LBreakTextItems[i].id;
        AElements[j].rect := LBreakTextItems[i].rect;
        inc(j);
      end;
    end;
    setlength(AElements, J);

    {$REGION 'SKIA'}
    {$IF defined(ALSkiaCanvas)}

    //create the drawing surface
    var LSurface: sk_surface_t;
    var LCanvas: sk_canvas_t;
    ALCreateSurface(
      LSurface, // out ASurface: sk_surface_t;
      LCanvas, // out ACanvas: sk_canvas_t;
      round(max(1, ARect.Width)), // const w: integer;
      round(max(1, ARect.Height)));// const h: integer)
    try

      //draw the background
      if (AOptions.Fill.Kind <> TbrushKind.None) or
         (AOptions.stroke.Kind <> TbrushKind.None) then begin
        ALDrawRectangle(
          LCanvas, // const ACanvas: sk_canvas_t;
          ARect, // const ARect: TrectF;
          AOptions.Fill, // const Fill: TBrush;
          AOptions.Stroke, // const Stroke: TStrokeBrush;
          nil, // const Shadow: TALShadow
          AOptions.Sides, // const Sides: TSides;
          AOptions.Corners, // const Corners: TCorners;
          AOptions.XRadius, // const XRadius: Single = 0;
          AOptions.YRadius); // const YRadius: Single = 0);
      end;

      //draw all texts
      for var i := 0 to LBreakTextItems.count - 1 do begin
        var LBreakTextItem := LBreakTextItems[i];
        if LBreakTextItem.imgSrc <> '' then begin
          LMaxWidth := min(LBreakTextItem.rect.Width, LBreakTextItem.rect.Height);
          var LTmpRect := ALAlignToPixelRound(
                            TrectF.Create(0,0,LMaxWidth,LMaxWidth).
                              CenterAt(LBreakTextItem.rect),
                            1{Scale});
          var LImg := ALLoadFromResourceAndFitIntoToSkImage(LBreakTextItem.imgSrc, LTmpRect.Width, LTmpRect.Height);
(*
          try
            LPaint.setColor(integer(LBreakTextItem.fontColor)); // sean that the bitmap is paint with the alpha value set via setColor
                                                                // ideally I would prefer to draw bitmap with alpha = 1 but drawText
                                                                // don't draw emoji with alpha 1 (that is not the case under iOS) and we
                                                                // we need do work the same way as LCanvas.drawText work :(
            LCanvas.drawBitmap(LImg, LTmpRect.left {left}, LTmpRect.top {top}, LPaint {paint});

          finally
            LImg.recycle;
            LImg := nil;
          end;
*)
        end
        else begin
          sk4d_paint_set_color(LPaint, LBreakTextItem.fontColor);
          sk4d_font_set_typeface(LFont, LBreakTextItem.typeface);
          sk4d_canvas_draw_simple_text(
            LCanvas, //self: sk_canvas_t;
            @LBreakTextItem.line[low(LBreakTextItem.line)], //const text: Pointer;
            Length(LBreakTextItem.line) * sizeOf(Char), //size: size_t;
            sk_textencoding_t.UTF16_SK_TEXTENCODING, //encoding: sk_textencoding_t;
            LBreakTextItem.pos.X, //x,
            LBreakTextItem.pos.Y, //y: float;
            LFont, //const font: sk_font_t;
            LPaint);//const paint: sk_paint_t)
        end;
      end;

      //create the result
      result := ALCreateSkImageFromSurface(LSurface);

    finally
      ALFreeSurface(LSurface, LCanvas);
    end;

    {$ENDIF}
    {$ENDREGION}

    {$REGION 'ANDROID'}
    {$IF (defined(ANDROID)) and (not defined(ALSkiaCanvas))}

    //create the drawing surface
    var LBitmap: Jbitmap;
    var LCanvas: Jcanvas;
    ALCreateSurface(
      LBitmap, // Var aBitmap: Jbitmap;
      LCanvas, // Var aCanvas: Jcanvas;
      round(max(1, ARect.Width)), // const w: integer;
      round(max(1, ARect.Height)));// const h: integer)
    try

      //draw the background
      if (AOptions.Fill.Kind <> TbrushKind.None) or
         (AOptions.stroke.Kind <> TbrushKind.None) then begin
        ALDrawRectangle(
          LCanvas, // const ACanvas: Jcanvas;
          ARect, // const ARect: TrectF;
          AOptions.Fill, // const Fill: TBrush;
          AOptions.Stroke, // const Stroke: TStrokeBrush;
          nil, // const Shadow: TALShadow
          AOptions.Sides, // const Sides: TSides;
          AOptions.Corners, // const Corners: TCorners;
          AOptions.XRadius, // const XRadius: Single = 0;
          AOptions.YRadius); // const YRadius: Single = 0);
      end;

      //draw all texts
      for var i := 0 to LBreakTextItems.count - 1 do begin
        var LBreakTextItem := LBreakTextItems[i];
        if LBreakTextItem.imgSrc <> '' then begin
          LMaxWidth := min(LBreakTextItem.rect.Width, LBreakTextItem.rect.Height);
          var LTmpRect := ALAlignToPixelRound(
                            TrectF.Create(0,0,LMaxWidth,LMaxWidth).
                              CenterAt(LBreakTextItem.rect),
                            1{Scale});
          var LImg := ALLoadFromResourceAndFitIntoToJBitmap(LBreakTextItem.imgSrc, LTmpRect.Width, LTmpRect.Height);
          try
            LPaint.setColor(integer(LBreakTextItem.fontColor)); // sean that the bitmap is paint with the alpha value set via setColor
                                                                // ideally I would prefer to draw bitmap with alpha = 1 but drawText
                                                                // don't draw emoji with alpha 1 (that is not the case under iOS) and we
                                                                // we need do work the same way as LCanvas.drawText work :(
            LCanvas.drawBitmap(LImg, LTmpRect.left {left}, LTmpRect.top {top}, LPaint {paint});
          finally
            LImg.recycle;
            LImg := nil;
          end;
        end
        else begin
          LPaint.setTypeface(LBreakTextItem.TypeFace);
          LPaint.setColor(integer(LBreakTextItem.fontColor));
          //-----
          LCanvas.drawText(
            LBreakTextItem.line{text},
            LBreakTextItem.pos.x {x},
            LBreakTextItem.pos.y {y},
            LPaint {paint});
        end;
      end;

      //create the result
      result := ALJBitmaptoTexture(LBitmap);

    finally
      ALFreeSurface(LBitmap, LCanvas);
    end;

    {$ENDIF}
    {$ENDREGION}

    {$REGION 'IOS'}
    {$IF (defined(IOS)) and (not defined(ALSkiaCanvas))}

    //create the drawing surface
    var LGridHeight := round(max(1, ARect.Height));
    var LContext: CGContextRef;
    ALCreateSurface(
      LContext, // out aContext: CGContextRef;
      round(max(1, ARect.Width)), // const w: integer;
      LGridHeight); // const h: integer;
    try

      //draw the background
      if (AOptions.Fill.Kind <> TbrushKind.None) or
         (AOptions.stroke.Kind <> TbrushKind.None) then begin
        ALDrawRectangle(
          LContext, // const aContext: CGContextRef;
          LGridHeight, // const aGridHeight: Single;
          ARect, // const ARect: TrectF;
          AOptions.Fill, // const Fill: TBrush;
          AOptions.Stroke, // const Stroke: TStrokeBrush;
          nil, // const Shadow: TALShadow
          AOptions.Sides, // const Sides: TSides;
          AOptions.Corners, // const Corners: TCorners;
          AOptions.XRadius, // const XRadius: Single = 0;
          AOptions.YRadius); // const YRadius: Single = 0);
      end;

      //draw all texts
      for var i := 0 to LBreakTextItems.count - 1 do begin
        var LBreakTextItem := LBreakTextItems[i];
        if LBreakTextItem.imgSrc <> '' then begin
          LMaxWidth := min(LBreakTextItem.rect.Width, LBreakTextItem.rect.Height);
          var LTmpRect := ALAlignToPixelRound(
                            TrectF.Create(0,0,LMaxWidth,LMaxWidth).
                              CenterAt(LBreakTextItem.rect),
                            1{Scale});
          var LImg := ALLoadFromResourceAndFitIntoToCGImageRef(LBreakTextItem.imgSrc, LTmpRect.Width, LTmpRect.Height);
          Try
            CGContextSetAlpha(LContext, TAlphaColorF.create(LBreakTextItem.fontColor).A); // to work the same way as with android
            CGContextDrawImage(
              LContext, // c: The graphics context in which to draw the image.
              ALLowerLeftCGRect(
                TPointF.Create(LTmpRect.left, LTmpRect.top),
                LTmpRect.Width,
                LTmpRect.Height,
                LGridHeight), // rect The location and dimensions in user space of the bounding box in which to draw the image.
              LImg); // image The image to draw.
          finally
            CGImageRelease(LImg);
          End;
        end
        else begin
          CGContextSetAlpha(LContext, 1);
          CGContextSetTextPosition(
            LContext,
            LBreakTextItem.pos.x {x},
            LGridHeight - LBreakTextItem.pos.Y);{y}
          CTLineDraw(LBreakTextItem.Line, LContext); // Draws a complete line.
        end;
      end;

      //convert the LContext to texture
      result := ALCGContextReftoTexture(LContext);

    finally
      ALFreeSurface(LContext);
    end;

    {$ENDIF}
    {$ENDREGION}

    {$REGION 'Other'}
    {$IF (not defined(ALSkiaCanvas)) and (not defined(ANDROID)) and (not defined(IOS))}

    //create the drawing surface
    ALCreateSurface(
      result, // Var aBitmap: Jbitmap;
      round(max(1, ARect.Width)), // const w: integer;
      round(max(1, ARect.Height)));// const h: integer)
    try

      //begin the scene
      if result.Canvas.BeginScene then
      try

        //draw the background
        if (AOptions.Fill.Kind <> TbrushKind.None) or
           (AOptions.stroke.Kind <> TbrushKind.None) then begin
          ALDrawRectangle(
            result.Canvas, // const aBitmap: Jbitmap;
            ARect, // const ARect: TrectF;
            AOptions.Fill, // const Fill: TBrush;
            AOptions.Stroke, // const Stroke: TStrokeBrush;
            nil, // const Shadow: TALShadow
            AOptions.Sides, // const Sides: TSides;
            AOptions.Corners, // const Corners: TCorners;
            AOptions.XRadius, // const XRadius: Single = 0;
            AOptions.YRadius); // const YRadius: Single = 0);
        end;

        //draw all texts
        result.Canvas.Fill.Kind := TbrushKind.Solid;
        result.Canvas.Font.Family := AOptions.FontName;
        result.Canvas.Font.size := AOptions.FontSize;
        for var i := 0 to LBreakTextItems.count - 1 do begin
          var LBreakTextItem := LBreakTextItems[i];
          if LBreakTextItem.imgSrc <> '' then begin
            LMaxWidth := min(LBreakTextItem.rect.Width, LBreakTextItem.rect.Height) * 1.15;
            var LTmpRect := ALAlignToPixelRound(
                              TrectF.Create(0,0,LMaxWidth,LMaxWidth).
                                CenterAt(LBreakTextItem.rect),
                              1{Scale});
            var LImg := ALLoadFromResourceAndFitIntoToBitmap(LBreakTextItem.imgSrc, LTmpRect.Width, LTmpRect.Height);
            try
              result.Canvas.drawBitmap(
                LImg,
                TrectF.Create(0,0,LTmpRect.Width,LTmpRect.Height),
                LTmpRect{DstRect},
                TAlphaColorF.create(LBreakTextItem.fontColor).A{AOpacity}, // to work the same way as with android
                false{HighSpeed});
            finally
              ALFreeAndNil(LImg);
            end;
          end
          else begin
            result.Canvas.Fill.Color := LBreakTextItem.fontColor;
            result.Canvas.Font.style := LBreakTextItem.fontStyle;
            //-----
            result.Canvas.FillText(
              LBreakTextItem.rect, // const ARect: TRectF;
              LBreakTextItem.line, // const AText: string;
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
      ALFreeSurface(result);
      raise;
    end;

    {$ENDIF}
    {$ENDREGION}

  finally
    ALFreeAndNil(LBreakTextItems);
    alfreeandnil(LFontColors);
    alfreeandnil(LSpanIDs);
    {$IF defined(ALSkiaCanvas)}
    sk4d_paint_destroy(LPaint);
    {$ELSEIF defined(ANDROID)}
    LPaint := nil;
    {$ENDIF}
  end;

end;
{$IF defined(ALZeroBasedStringsON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{****************************}
function  ALDrawMultiLineText(
            const AText: String; // support only theses EXACT html tag :
                                 //   <b>...</b>
                                 //   <i>...</i>
                                 //   <font color="#xxxxxx">...</font>
                                 //   <span id="xxx">...</span>
                                 //   <img src="xxx">
                                 // other < > must be encoded with &lt; and &gt;
            var ARect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
            out ATextBroken: boolean; // True if the text was broken into several lines.
            out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis).
            const AOptions: TALDrawMultiLineTextOptions): TALDrawable;
begin
  var LAscent: single;
  var LDescent: Single;
  var LFirstPos: TpointF;
  var LLastPos: TpointF;
  var LElements: TalTextElements;
  var LEllipsisRect: TRectF;
  result := ALDrawMultiLineText(
              AText,
              ARect, // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
              ATextBroken, // out => True if the text was broken into several lines
              AAllTextDrawn, // out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
              LAscent, // var AAscent: single; // out => the Ascent of the last element (in real pixel)
              LDescent, // var ADescent: Single; // out => the Descent of the last element (in real pixel)
              LFirstPos, // var AFirstPos: TpointF; // out => the point of the start of the text
              LLastPos, // var ALastPos: TpointF; // out => the point of the end of the text
              LElements, // var AElements: TalTextElements; // out => the list of rect describing all span elements
              LEllipsisRect, // var AEllipsisRect: TRectF; // out => the rect of the Ellipsis (if present)
              AOptions);
end;

{****************************}
function  ALDrawMultiLineText(
            const AText: String; // support only theses EXACT html tag :
                                 //   <b>...</b>
                                 //   <i>...</i>
                                 //   <font color="#xxxxxx">...</font>
                                 //   <span id="xxx">...</span>
                                 //   <img src="xxx">
                                 // other < > must be encoded with &lt; and &gt;
            var ARect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
            out ATextBroken: boolean; // True if the text was broken into several lines
            const AOptions: TALDrawMultiLineTextOptions): TALDrawable;
begin
  var LAscent: single;
  var LDescent: Single;
  var LFirstPos: TpointF;
  var LLastPos: TpointF;
  var LElements: TalTextElements;
  var LEllipsisRect: TRectF;
  var LAllTextDrawn: boolean;
  result := ALDrawMultiLineText(
              AText,
              ARect, // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
              ATextBroken, // out => True if the text was broken into several lines
              LAllTextDrawn, // out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
              LAscent, // var AAscent: single; // out => the Ascent of the last element (in real pixel)
              LDescent, // var ADescent: Single; // out => the Descent of the last element (in real pixel)
              LFirstPos, // var AFirstPos: TpointF; // out => the point of the start of the text
              LLastPos, // var ALastPos: TpointF; // out => the point of the end of the text
              LElements, // var AElements: TalTextElements; // out => the list of rect describing all span elements
              LEllipsisRect, // var AEllipsisRect: TRectF; // out => the rect of the Ellipsis (if present)
              AOptions);
end;

{****************************}
function  ALDrawMultiLineText(
            const AText: String; // support only theses EXACT html tag :
                                 //   <b>...</b>
                                 //   <i>...</i>
                                 //   <font color="#xxxxxx">...</font>
                                 //   <span id="xxx">...</span>
                                 //   <img src="xxx">
                                 // other < > must be encoded with &lt; and &gt;
            var ARect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
            const AOptions: TALDrawMultiLineTextOptions): TALDrawable;
begin
  var LAscent: single;
  var LDescent: Single;
  var LFirstPos: TpointF;
  var LLastPos: TpointF;
  var LElements: TalTextElements;
  var LEllipsisRect: TRectF;
  var LTextBroken: boolean;
  var LAllTextDrawn: boolean;
  result := ALDrawMultiLineText(
              AText,
              ARect, // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
              LTextBroken, // out => True if the text was broken into several lines
              LAllTextDrawn, // out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
              LAscent, // var AAscent: single; // out => the Ascent of the last element (in real pixel)
              LDescent, // var ADescent: Single; // out => the Descent of the last element (in real pixel)
              LFirstPos, // var AFirstPos: TpointF; // out => the point of the start of the text
              LLastPos, // var ALastPos: TpointF; // out => the point of the end of the text
              LElements, // var AElements: TalTextElements; // out => the list of rect describing all span elements
              LEllipsisRect, // var AEllipsisRect: TRectF; // out => the rect of the Ellipsis (if present)
              AOptions);
end;

end.
