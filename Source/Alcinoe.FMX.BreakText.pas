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
  Alcinoe.FMX.Common,
  Alcinoe.FMX.Graphics,
  Fmx.types,
  FMX.graphics;

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
           const AFontFamily: String;
           const AFontSize: single;
           const AFontStyle: TFontStyles;
           const AFontColor: TalphaColor;
           var ARect: TRectF;
           const AText: JString;
           const AWordWrap: Boolean;
           const AHTextAlign, AVTextAlign: TTextAlign;
           const ATrimming: TALTextTrimming;
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
           const AFontFamily: String;
           const AFontSize: single;
           const AFontStyle: TFontStyles;
           const AFontColor: TalphaColor;
           var ARect: TRectF;
           const AText: string;
           const AWordWrap: Boolean;
           const AHTextAlign, AVTextAlign: TTextAlign;
           const ATrimming: TALTextTrimming; // TALTextTrimming.word not yet supported - TALTextTrimming.character will be used instead (if someone need, it's not really hard to implement)
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
           const AFontFamily: String;
           const AFontSize: single;
           const AFontStyle: TFontStyles;
           const AFontColor: TalphaColor;
           var ARect: TRectF;
           const AText: string;
           const AWordWrap: Boolean;
           const AHTextAlign, AVTextAlign: TTextAlign;
           const ATrimming: TALTextTrimming;
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
  TALTextElement = record
    Id: string;
    Rect: TrectF;
    class function Empty: TALTextElement; inline; static;
  end;
  TALTextElements = array of TALTextElement;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDrawMultiLineTextOptions = class(Tobject)
  private
    FFill: TBrush;  // default = nil
    FStroke: TStrokeBrush; // default = nil
    function GetFill: TBrush;
    function GetStroke: TStrokeBrush;
  public
    Scale: Single; // default = 1
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
    // If EllipsisFontFamily is set to an empty string (''), then all ellipsis
    // font settings (like font size, style, weight, decoration, etc.) will be
    // inherited from the main font settings.
    EllipsisText: String; // default = '…';
    EllipsisFontFamily: String; // default = ''
    EllipsisFontSize: single; // default = 14
    EllipsisFontWeight: TFontWeight; // default = TFontWeight.Regular;
    EllipsisFontSlant: TFontSlant; // default = TFontSlant.Regular
    EllipsisFontStretch: TFontStretch; // default = TFontStretch.Regular
    EllipsisFontColor: TalphaColor; // default = TAlphaColors.Black
    //--
    EllipsisDecorationKinds: TALTextDecorationKinds; // default = []
    EllipsisDecorationStyle: TALTextDecorationStyle; // default = TALTextDecorationStyle.Solid
    EllipsisDecorationThicknessMultiplier: Single; // default = 1
    EllipsisDecorationColor: TAlphaColor; // default = TAlphaColors.Null
    //--
    AutoSize: Boolean; // default = True;
    AutoSizeX: Boolean; // default = False;
    AutoSizeY: Boolean; // default = False;
    MaxLines: integer; // default = 65535;
    LineHeight: single; // default = 0;
    LineSpacing: single; // default = 0;
    LetterSpacing: Single; // default = 0;
    Trimming: TALTextTrimming; // default = TALTextTrimming.Word;
    FailIfTextBroken: boolean; // default = false
    //--
    Direction: TALTextDirection; // default = TALTextDirection.LeftToRight
    HTextAlign: TALTextHorzAlign; // default = TALTextHorzAlign.Leading;
    VTextAlign: TALTextVertAlign; // default = TALTextVertAlign.Leading;
    //--
    FillColor: TAlphaColor; // default = TAlphaColors.null - not used if Fill is provided
    StrokeColor: TalphaColor; // default = TAlphaColors.null - not used if Stroke is provided
    StrokeThickness: Single; // default = 1 - not used if Stroke is provided
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
    //   * <font color="#FFFFFF">...</font>
    //   * <span id="xxx"
    //           color="#FFFFFF"
    //           font-weight="bold"
    //           font-style="italic">...</span>
    //   * <img src="{ResourceName}">
    //   * Other "<" and ">" must be encoded with "&lt;" and "&gt;"
    //
    // Tags supported only when Skia is enabled:
    //   * <font color="#FFFFFF"
    //           size="14"
    //           face="Roboto">...</font>
    //   * <span id="xxx"
    //           color="#FFFFFF"
    //           font-family="Roboto"
    //           font-size="14"
    //           font-weight="bold"
    //           font-style="italic"
    //           font-stretch="condensed"
    //           text-decoration-line="underline overline"
    //           text-decoration-style="solid"
    //           text-decoration-thickness="3"
    //           text-decoration-color="#FFFFFF"
    //           line-height="15px"
    //           letter-spacing="2px"
    //           background-color="#FFFFFF">...</span>
    //   * <img src="{ResourceName}"
    //          width="xxx"
    //          height="xxx">
    //
    // Note: You can also use the "style" attribute for inline styling
    // Ex: <span style="font-size=14; font-style=italic">
    TextIsHtml: boolean; // default = false;
    //--
    constructor Create;
    destructor Destroy; override;
    //--
    property Fill: TBrush read GetFill;  // default = nil
    property Stroke: TStrokeBrush read GetStroke; // default = nil
    function IsFillAssigned: Boolean;
    function IsStrokeAssigned: Boolean;
    function GetScaledFontSize: Single; inline;
    function GetScaledEllipsisFontSize: Single; inline;
    function GetScaledLineHeight: Single; inline;
    function GetScaledLineSpacing: Single; inline;
    function GetScaledLetterSpacing: Single; inline;
    function GetScaledStrokeThickness: Single; inline;
    function GetScaledXRadius: Single; inline;
    function GetScaledYRadius: Single; inline;
    function GetScaledPaddingTop: Single; inline;
    function GetScaledPaddingRight: Single; inline;
    function GetScaledPaddingLeft: Single; inline;
    function GetScaledPaddingBottom: Single; inline;
  End;

function ALDrawMultiLineText(
           const AText: String; // When AOptions.TextIsHtml is set to true, the HTML tags supported are described in the TALDrawMultiLineTextOptions.TextIsHtml field declaration.
           var ARect: TRectF; // in => The constraint boundaries in real pixels. out => The calculated rect that contains the text, also in real pixels.
           out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
           out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
           out AElements: TALTextElements; // out => The list of rectangles describing all span elements.
           const AOptions: TALDrawMultiLineTextOptions): TALDrawable; overload;
function ALDrawMultiLineText(
           const AText: String;
           var ARect: TRectF; // in => The constraint boundaries in real pixels. out => The calculated rect that contains the text, also in real pixels.
           out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
           out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
           const AOptions: TALDrawMultiLineTextOptions): TALDrawable; inline; overload;
function ALDrawMultiLineText(
           const AText: String;
           var ARect: TRectF; // in => The constraint boundaries in real pixels. out => The calculated rect that contains the text, also in real pixels.
           out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
           const AOptions: TALDrawMultiLineTextOptions): TALDrawable; inline; overload;
function ALDrawMultiLineText(
           const AText: String;
           var ARect: TRectF; // in => The constraint boundaries in real pixels. out => The calculated rect that contains the text, also in real pixels.
           const AOptions: TALDrawMultiLineTextOptions): TALDrawable; inline; overload;

function ALGetTextElementsByID(Const ATextElements: TALTextElements; Const AId: String): TALTextElements;

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
  {$IF defined(ALSkiaCanvas)}
  Fmx.skia,
  {$ENDIF}
  FMX.TextLayout,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.Common;

{**************************************************}
class function TALTextElement.Empty: TALTextElement;
begin
  Result.ID := '';
  Result.Rect := TRectF.Empty;
end;

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
           const AFontFamily: String;
           const AFontSize: single;
           const AFontStyle: TFontStyles;
           const AFontColor: TalphaColor;
           var ARect: TRectF;
           const AText: JString;
           const AWordWrap: Boolean;
           const AHTextAlign, AVTextAlign: TTextAlign;
           const ATrimming: TALTextTrimming;
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
      LEllipsisTypeface := TJTypeface.JavaClass.create(StringToJString(AFontFamily), ALfontStyleToAndroidStyle(AEllipsisFontStyle));
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
  LTypeface := TJTypeface.JavaClass.create(StringToJString(AFontFamily), ALfontStyleToAndroidStyle(AFontStyle));
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
            (AEllipsisText <> nil) and                                                                              // <<
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
            if AEllipsisText = nil then begin
              if LNumberOfChars > 0 then
                LLine := LLine.substring(0, LNumberOfChars);
            end
            else begin
              case ATrimming of
                TALTextTrimming.Character: begin
                  _initEllipsis;
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
                end;

                TALTextTrimming.Word: begin
                  _initEllipsis;
                  var LSaveNumberOfChars := LNumberOfChars;
                  var LSaveNumberOfCharsIsAccurate := False;
                  while LNumberOfChars > 0 do begin
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
                  end;
                end;

                else
                  raise Exception.Create('Error #F88E3622-E91E-4678-AA84-CA0C3093BCE3');
              end;
            end;
          end

          //if AWordWrap
          else begin

            //We are at the last line and AEllipsisText <> ''
            if ((compareValue(LCurrLineY + LLineHeight + LFontMetrics.descent, LMaxHeight, Tepsilon.position) > 0) or
                ((AMaxlines > 0) and (ATotalLines >= AMaxlines - 1))) and
               (AEllipsisText <> nil) then begin

              AAllTextDrawn := False; // if we are at the last line then in anycase we will not draw all the text
              _initEllipsis;
              //-----
              var LSaveNumberOfChars := LNumberOfChars;
              var LSaveNumberOfCharsIsAccurate := False;
              while LNumberOfChars > 0 do begin
                if (LNumberOfChars >= LLine.length) then begin // if (aNumberOfChars >= aLine.length) then we are here because of manual linebreak
                  LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                end
                //----
                else if (ATrimming = TALTextTrimming.Word) and (LNumberOfChars >= 2) then begin
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
              end;

            end

            //We are not at the last line or AEllipsisText = ''
            else begin

              //We are at the last line and AEllipsisText = '' and more line available
              if (AEllipsisText = nil) and
                 ((compareValue(LCurrLineY + LLineHeight + LFontMetrics.descent, LMaxHeight, Tepsilon.position) > 0) or
                  ((AMaxlines > 0) and (ATotalLines >= AMaxlines - 1))) then AAllTextDrawn := False;

              //cut the line
              var LSaveNumberOfChars := LNumberOfChars;
              if LNumberOfChars < LLine.length then inc(LNumberOfChars); // in case the space separator is just after aNumberOfChars
              while LNumberOfChars > 0 do begin
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
           const AFontFamily: String;
           const AFontSize: single;
           const AFontStyle: TFontStyles;
           const AFontColor: TalphaColor;
           var ARect: TRectF;
           const AText: string;
           const AWordWrap: Boolean;
           const AHTextAlign, AVTextAlign: TTextAlign;
           const ATrimming: TALTextTrimming; // TALTextTrimming.word not yet supported - TALTextTrimming.character will be used instead (if someone need, it's not really hard to implement)
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
    var LFont := ALGetCTFontRef(AFontFamily, AFontSize, AFontStyle); // Returns a new font reference for the given name.
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
                TALTextTrimming.None: LLineBreakMode := kCTLineBreakByClipping;
                TALTextTrimming.Character: LLineBreakMode := kCTLineBreakByCharWrapping;
                TALTextTrimming.Word: LLineBreakMode := kCTLineBreakByWordWrapping;
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

            // If ATrimming = TALTextTrimming.None or AEllipsisText = '' then nothing todo
            if (ATrimming <> TALTextTrimming.None) and
               (AEllipsisText <> '') then begin

              // Create LEllipsisColor
              if AEllipsisFontColor <> TAlphaColorRec.Null then LAlphaColor := TAlphaColorCGFloat.Create(AEllipsisFontColor)
              else LAlphaColor := TAlphaColorCGFloat.Create(AFontColor);
              var LEllipsisColor := CGColorCreate(ALGetGlobalCGColorSpace, @LAlphaColor);
              try

                // Create LEllipsisFont
                var LEllipsisFont := ALGetCTFontRef(AFontFamily, AFontSize, AEllipsisFontStyle); // Returns a new font reference for the given name.
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
                            TALTextTrimming.None: LLineBreakMode := kCTLineBreakByClipping;
                            TALTextTrimming.Character: LLineBreakMode := kCTLineBreakByCharWrapping;
                            TALTextTrimming.Word: LLineBreakMode := kCTLineBreakByWordWrapping;
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
           const AFontFamily: String;
           const AFontSize: single;
           const AFontStyle: TFontStyles;
           const AFontColor: TalphaColor;
           var ARect: TRectF;
           const AText: string;
           const AWordWrap: Boolean;
           const AHTextAlign, AVTextAlign: TTextAlign;
           const ATrimming: TALTextTrimming;
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
             const AFontFamily: String;
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
      LLayout.Font.Family := AFontFamily;
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
        AFontFamily, // const AFontFamily: String;
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
    LLayout.Font.Family := AFontFamily;
    LLayout.Font.Size := AFontSize;
    LLayout.Font.Style := AFontStyle;
    LLayout.EndUpdate;
    // Unfortunatly TTextlayout don't gave any ascent/descent ( https://quality.embarcadero.com/browse/RSP-16645
    // also the canvas.FillText don't ask the base line of the text but the top/left corner so it's better to say
    // that ascent = 0 and descent = height of the font
    LAscent := 0;
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
                              AFontFamily,
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
          (AEllipsisText <> '') and                                                                       // <<
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
          if AEllipsisText = '' then begin
            if LNumberOfChars > 0 then
              LLine := LLine.substring(0, LNumberOfChars);
          end
          else begin
            case ATrimming of
              TALTextTrimming.Character: begin
                _initEllipsis;
                //(aNumberOfChars < aLine.length) to know that we are not here
                //because of manual linebreak and dec(aNumberOfChars) because initialy
                //we considere that AEllipsisText is only one char
                if (LNumberOfChars < LLine.length) then dec(LNumberOfChars);
                while LNumberOfChars > 0 do begin
                  LLine := LLine.substring(0, LNumberOfChars);
                  LNumberOfChars := _BreakText(
                                      AFontFamily,
                                      AFontSize,
                                      AFontStyle,
                                      LLine {text},
                                      LMaxWidth - LEllipsisLineLn - LLineIndent, {maxWidth}
                                      LMeasuredWidth {measuredWidth});
                  if LNumberOfChars >= LLine.length then break;
                end;
              end;

              TALTextTrimming.Word: begin
                _initEllipsis;
                var LSaveNumberOfChars := LNumberOfChars;
                var LSaveNumberOfCharsIsAccurate := False;
                while LNumberOfChars > 0 do begin
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
                                          AFontFamily,
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
                                      AFontFamily,
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
                end;
              end;

              else
                raise Exception.Create('Error #F88E3622-E91E-4678-AA84-CA0C3093BCE3');
            end;
          end;
        end

        //if AWordWrap
        else begin

          //We are at the last line and AEllipsisText <> ''
          if ((compareValue(LCurrLineY + LLineHeight + LDescent, LMaxHeight, Tepsilon.position) > 0) or
              ((AMaxlines > 0) and (ATotalLines >= AMaxlines - 1))) and
             (AEllipsisText <> '') then begin

            AAllTextDrawn := False; // if we are at the last line then in anycase we will not draw all the text
            _initEllipsis;
            //-----
            var LSaveNumberOfChars := LNumberOfChars;
            var LSaveNumberOfCharsIsAccurate := False;
            while LNumberOfChars > 0 do begin
              if (LNumberOfChars >= LLine.length) then begin // if (aNumberOfChars >= aLine.length) then we are here because of manual linebreak
                LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
              end
              //----
              else if (ATrimming = TALTextTrimming.Word) and (LNumberOfChars >= 2) then begin
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
                                      AFontFamily,
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
                                  AFontFamily,
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
            end;

          end

          //We are not at the last line or AEllipsisText = ''
          else begin

            //We are at the last line and AEllipsisText = '' and more line available
            if (AEllipsisText = '') and
               ((compareValue(LCurrLineY + LLineHeight + LDescent, LMaxHeight, Tepsilon.position) > 0) or
                ((AMaxlines > 0) and (ATotalLines >= AMaxlines - 1))) then AAllTextDrawn := False;

            //Cut the line
            var LSaveNumberOfChars := LNumberOfChars;
            if LNumberOfChars < LLine.length then inc(LNumberOfChars); // in case the space separator is just after aNumberOfChars
            while LNumberOfChars > 0 do begin
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
                                      AFontFamily,
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
                                  AFontFamily,
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
  Scale := 1;
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
  LineHeight := 0;
  LineSpacing := 0;
  LetterSpacing := 0;
  Trimming := TALTextTrimming.Word;
  FailIfTextBroken := false;
  //--
  Direction := TALTextDirection.LeftToRight;
  HTextAlign := TALTextHorzAlign.Leading;
  VTextAlign := TALTextVertAlign.Leading;
  //--
  FillColor := TAlphaColors.Null;
  StrokeColor := TalphaColors.Null;
  StrokeThickness := 1;
  //--
  FFill := nil;
  FStroke := nil;
  //--
  Sides := AllSides;
  XRadius := 0;
  YRadius := 0;
  Corners := AllCorners;
  Padding := TRectF.Create(0,0,0,0);
  //--
  TextIsHtml := false;
end;

{***************************************************}
function TALDrawMultiLineTextOptions.GetFill: TBrush;
begin
  if FFill = nil then
    FFill := TBrush.Create(TbrushKind.None, $FFE0E0E0);
  result := FFill;
end;

{***********************************************************}
function TALDrawMultiLineTextOptions.GetStroke: TStrokeBrush;
begin
  if FStroke = nil then
    FStroke := TStrokeBrush.Create(TbrushKind.None,  $FF000000);
  result := FStroke;
end;

{***********************************************************}
function TALDrawMultiLineTextOptions.IsFillAssigned: Boolean;
begin
  result := FFill <> nil;
end;

{*************************************************************}
function TALDrawMultiLineTextOptions.IsStrokeAssigned: Boolean;
begin
  result := FStroke <> nil;
end;

{*************************************************************}
function TALDrawMultiLineTextOptions.GetScaledFontSize: Single;
begin
  Result := FontSize * Scale;
end;

{************************************************************************}
function TALDrawMultiLineTextOptions.GetScaledEllipsisFontSize: Single;
begin
  Result := EllipsisFontSize * Scale;
end;

{****************************************************************}
function TALDrawMultiLineTextOptions.GetScaledLineSpacing: Single;
begin
  Result := LineSpacing * Scale;
end;

{***************************************************************}
function TALDrawMultiLineTextOptions.GetScaledLineHeight: Single;
begin
  Result := LineHeight * Scale;
end;

{******************************************************************}
function TALDrawMultiLineTextOptions.GetScaledLetterSpacing: Single;
begin
  Result := LetterSpacing * Scale;
end;

{********************************************************************}
function TALDrawMultiLineTextOptions.GetScaledStrokeThickness: Single;
begin
  Result := StrokeThickness * Scale;
end;

{************************************************************}
function TALDrawMultiLineTextOptions.GetScaledXRadius: Single;
begin
  Result := XRadius * Scale;
end;

{************************************************************}
function TALDrawMultiLineTextOptions.GetScaledYRadius: Single;
begin
  Result := YRadius * Scale;
end;

{***************************************************************}
function TALDrawMultiLineTextOptions.GetScaledPaddingTop: Single;
begin
  Result := Padding.Top * Scale;
end;

{*****************************************************************}
function TALDrawMultiLineTextOptions.GetScaledPaddingRight: Single;
begin
  Result := Padding.Right * Scale;
end;

{****************************************************************}
function TALDrawMultiLineTextOptions.GetScaledPaddingLeft: Single;
begin
  Result := Padding.Left * Scale;
end;

{******************************************************************}
function TALDrawMultiLineTextOptions.GetScaledPaddingBottom: Single;
begin
  Result := Padding.Bottom * Scale;
end;

{*********************************************}
destructor TALDrawMultiLineTextOptions.Destroy;
begin
  ALFreeAndNil(FFill);
  ALFreeAndNil(FStroke);
  inherited destroy;
end;

{***************************}
function ALDrawMultiLineText(
           const AText: String; // When AOptions.TextIsHtml is set to true, the HTML tags supported are described in the TALDrawMultiLineTextOptions.TextIsHtml field declaration.
           var ARect: TRectF; // in => The constraint boundaries in real pixels. out => The calculated rect that contains the text, also in real pixels.
           out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
           out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
           out AElements: TALTextElements; // out => The list of rectangles describing all span elements.
           const AOptions: TALDrawMultiLineTextOptions): TALDrawable;

  {$REGION '_TryStrColorToInt'}
  Function _TryStrColorToInt(const AColorStr: String; out AColorInt: Cardinal): boolean;
  begin
    Result := False;
    if (AColorStr <> '') and (AColorStr[low(AColorStr)] = '#') then begin
      var LColorStr := AColorStr;
      LColorStr[low(LColorStr)] := '$';
      if length(LColorStr) = 7 then insert('ff', LColorStr, 2); // $ffffffff
      Result := ALTryStrToUInt(LColorStr, AColorInt);
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
              const ALineHeights: TList<Single>;
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
          ALExtractHeaderFieldsWithQuoteEscaped(
            [';']{Separators},
            [' ', #9, #13, #10]{WhiteSpace},
            ['"', '''']{Quotes},
            PChar(LStyle){Content},
            LStyleParamList{Strings},
            False{HttpDecode},
            True{StripQuotes});
          for var I := 0 to LStyleParamList.Count - 1 do
            LParamList.Values[LStyleParamList.Names[i]] := LStyleParamList.ValueFromIndex[i];
        finally
          ALFreeAndNil(LStyleParamList);
        end;
      end;
      //--
      if ASpanIds <> nil then
        ASpanIds.Add(LParamList.Values['id']);
      //--
      if AFontFamilies <> nil then begin
        var LFontFamily := LParamList.Values['face']; // <font face="Arial">
        if LFontFamily = '' then LFontFamily := LParamList.Values['font-family']; // <span font-family="Arial">
        if LFontFamily <> '' then AFontFamilies.Add(LFontFamily)
        else if AFontFamilies.Count > 0 then AFontFamilies.Add(AFontFamilies[AFontFamilies.Count - 1])
        else AFontFamilies.Add(AOptions.FontFamily);
      end;
      //--
      if AFontSizes <> nil then begin
        var LFontSize: Single := ALStrToFloatDef(LParamList.Values['size'], 0, ALDefaultFormatSettingsW); // <font size="16">
        if CompareValue(LFontSize, 0, Tepsilon.FontSize) <= 0 then LFontSize := ALStrToFloatDef(LParamList.Values['font-size'], 0, ALDefaultFormatSettingsW); // <span font-size="16">
        if CompareValue(LFontSize, 0, Tepsilon.FontSize) > 0 then AFontSizes.Add(LFontSize * AOptions.Scale)
        else if AFontSizes.Count > 0 then AFontSizes.Add(AFontSizes[AFontSizes.Count - 1])
        else AFontSizes.Add(AOptions.GetScaledFontSize);
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
        else AFontWeights.Add(AOptions.FontWeight);
      end;
      //--
      if AFontSlants <> nil then begin
        var LFontStyle := ALLowerCase(LParamList.Values['font-style']); // <span font-style="italic">
             if (LFontStyle = 'normal') then AFontSlants.Add(TFontSlant.Regular)
        else if (LFontStyle = 'italic') then AFontSlants.Add(TFontSlant.Italic)
        else if (LFontStyle = 'oblique') then AFontSlants.Add(TFontSlant.Oblique)
        else if AFontSlants.Count > 0 then AFontSlants.Add(AFontSlants[AFontSlants.Count - 1])
        else AFontSlants.Add(AOptions.FontSlant);
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
        else AFontStretchs.Add(AOptions.FontStretch);
      end;
      //--
      if AFontColors <> nil then begin
        var LColorInt: Cardinal;
        if _TryStrColorToInt(LParamList.Values['color'], LColorInt) then AFontColors.Add(TalphaColor(LcolorInt)) // <span color="#xxxxxx">
        else if AFontColors.Count > 0 then AFontColors.Add(AFontColors[AFontColors.Count - 1])
        else AFontColors.Add(AOptions.FontColor);
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
        else ADecorationKinds.Add(AOptions.DecorationKinds);
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
        else ADecorationStyles.Add(AOptions.DecorationStyle);
      end;
      //--
      If ADecorationThicknessMultipliers <> nil then begin
        var LDecorationThicknessMultiplierStr := ALLowerCase(LParamList.Values['text-decoration-thickness']); // <span text-decoration-thickness="3">
        var LDecorationThicknessMultiplierFloat: Single;
        if ALTryStrToFloat(LDecorationThicknessMultiplierStr, LDecorationThicknessMultiplierFloat, ALDefaultFormatSettingsW) then ADecorationThicknessMultipliers.Add(LDecorationThicknessMultiplierFloat)
        else if ADecorationThicknessMultipliers.Count > 0 then ADecorationThicknessMultipliers.Add(ADecorationThicknessMultipliers[ADecorationThicknessMultipliers.Count - 1])
        else ADecorationThicknessMultipliers.Add(AOptions.DecorationThicknessMultiplier);
      end;
      //--
      If ADecorationColors <> nil then begin
        var LColorInt: Cardinal;
        if _TryStrColorToInt(LParamList.Values['text-decoration-color'], LColorInt) then ADecorationColors.Add(TalphaColor(LcolorInt)) // <span text-decoration-color="#xxxxxx">
        else if ADecorationColors.Count > 0 then ADecorationColors.Add(ADecorationColors[ADecorationColors.Count - 1])
        else ADecorationColors.Add(AOptions.DecorationColor);
      end;
      //--
      If ABackgroundColors <> nil then begin
        var LColorInt: Cardinal;
        if _TryStrColorToInt(LParamList.Values['background-color'], LColorInt) then ABackgroundColors.Add(TalphaColor(LcolorInt)) // <span background-color="#xxxxxx">
        else if ABackgroundColors.Count > 0 then ABackgroundColors.Add(ABackgroundColors[ABackgroundColors.Count - 1])
        else ABackgroundColors.Add(TalphaColors.Null);
      end;
      //--
      If ALineHeights <> nil then begin
        var LLineHeightStr := ALLowerCase(LParamList.Values['line-height']); // <span line-height="15px">
        var LIsWithPx := AlPosW('px', LLineHeightStr) > 0;
        if LIsWithPx then LLineHeightStr := ALStringReplaceW(LLineHeightStr, 'px', '', []);
        var LLineHeightFloat: Single;
        if LIsWithPx and ALTryStrToFloat(LLineHeightStr, LLineHeightFloat, ALDefaultFormatSettingsW) then ALineHeights.Add(LLineHeightFloat * AOptions.Scale)
        else if ALineHeights.Count > 0 then ALineHeights.Add(ALineHeights[ALineHeights.Count - 1])
        else ALineHeights.Add(AOptions.GetScaledLineHeight);
      end;
      //--
      If ALetterSpacings <> nil then begin
        var LLetterSpacingStr := ALLowerCase(LParamList.Values['letter-spacing']); // <span letter-spacing="3px">
        var LIsWithPx := AlPosW('px', LLetterSpacingStr) > 0;
        if LIsWithPx then LLetterSpacingStr := ALStringReplaceW(LLetterSpacingStr, 'px', '', []);
        var LLetterSpacingFloat: Single;
        if LIsWithPx and ALTryStrToFloat(LLetterSpacingStr, LLetterSpacingFloat, ALDefaultFormatSettingsW) then ALetterSpacings.Add(LLetterSpacingFloat * AOptions.Scale)
        else if ALetterSpacings.Count > 0 then ALetterSpacings.Add(ALetterSpacings[ALetterSpacings.Count - 1])
        else ALetterSpacings.Add(AOptions.GetScaledLetterSpacing);
      end;
      //--
      AImgSrc := LParamList.Values['src'];
      //--
      AImgWidth := ALStrToFloatDef(LParamList.Values['width'], 0, ALDefaultFormatSettingsW) * AOptions.Scale;
      //--
      AImgHeight := ALStrToFloatDef(LParamList.Values['height'], 0, ALDefaultFormatSettingsW) * AOptions.Scale;
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
              const ALineHeights: TList<Single>;
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
      ALineHeights, // const ALineHeights: TList<Single>;
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
      nil, // const ALineHeights: TList<Single>;
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
      nil, // const ALineHeights: TList<Single>;
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
      nil, // const ALineHeights: TList<Single>;
      nil, // const ALetterSpacings: TList<Single>;
      AImgSrc, // out AImgSrc: String;
      AImgWidth, // out AImgWidth: Single;
      AImgHeight); // out AImgHeight: Single)
  end;
  {$ENDREGION}

  {$REGION '_GetSkFontStyle'}
  {$IF defined(ALSkiaCanvas)}
  Function _GetSkFontStyle(
              const AFontWeight: TFontWeight;
              const AFontSlant: TFontSlant;
              const AFontStretch: TFontStretch): sk_fontstyle_t;
  begin
    {$IFNDEF ALCompilerVersionSupported120}
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
  {$ENDREGION}

  {$REGION 'TRangeIndex'}
  {$IF defined(ALSkiaCanvas)}
  type
    TRangeIndex = record
      SpanID: String;
      start: Integer;
      &end: Integer;
    end;
  {$ENDIF}
  {$ENDREGION}

begin
  ARect.Top := ARect.Top * AOptions.Scale;
  ARect.right := ARect.right * AOptions.Scale;
  ARect.left := ARect.left * AOptions.Scale;
  ARect.bottom := ARect.bottom * AOptions.Scale;
  Try

    {$REGION 'SKIA'}
    {$IF defined(ALSkiaCanvas)}

    // Init out var
    ATextBroken := false;
    AAllTextDrawn := True;
    setlength(AElements, 0);

    // Init local var
    var LPrevInsertEllipsisAt := MaxInt;
    var LInsertEllipsisAt := MaxInt;
    var LMaxLines: Integer;
    if AOptions.MaxLines > 0 then LMaxLines := AOptions.MaxLines
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
    var LLineHeights := TList<Single>.Create;
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
      var Ltext: String := AText;
      if AOptions.TextIsHtml then begin
        LText := ALStringReplaceW(Ltext, #13, ' ', [RfReplaceALL]);
        LText := ALStringReplaceW(Ltext, #10, ' ', [RfReplaceALL]);
        LText := ALStringReplaceW(Ltext, #9, ' ', [RfReplaceALL]);
        While ALPosW('  ', LText) > 0 do
          LText := ALStringReplaceW(Ltext, '  ', ' ', [RfReplaceALL]);
        LText := ALStringReplaceW(Ltext, '<br>', #13#10, [RfReplaceALL, RfIgnoreCase]);
        LText := ALStringReplaceW(Ltext, '<br/>', #13#10, [RfReplaceALL, RfIgnoreCase]);
        LText := ALStringReplaceW(Ltext, '&nbsp;', #160{0x00A0}, [RfReplaceALL, RfIgnoreCase]);
      end;

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
        LLineHeights.clear;
        LLetterSpacings.Clear;
        LSpanIDs.Clear;
        LPlaceHolders.Clear;
        LRangeIndexes.Clear;

        // Declare LTextForRange
        var LTextForRange: String := '';

        // Init LParagraphstyle
        var LParagraphstyle := sk4d_paragraphstyle_create;
        Try

          // https://api.flutter.dev/flutter/painting/StrutStyle-class.html
          // Defines the strut, which sets the minimum height a line can be relative to the baseline.
          // Strut applies to all lines in the paragraph. Strut is a feature that allows minimum
          // line heights to be set. The effect is as if a zero width space was included at the
          // beginning of each line in the paragraph. This imaginary space is 'shaped' according the
          // properties defined in this class.
          // No lines may be shorter than the strut. The ascent and descent of the strut are calculated,
          // and any laid out text that has a shorter ascent or descent than the strut's ascent or
          // descent will take the ascent and descent of the strut. Text with ascents or descents
          // larger than the strut's ascent or descent will layout as normal and extend past the strut.
          var LStrutstyle := sk4d_strutstyle_create;
          try

            // https://api.flutter.dev/flutter/painting/StrutStyle/fontFamily.html
            // the name of the font to use when calculating the strut (e.g., Roboto). No glyphs from
            // the font will be drawn and the font will be used purely for metrics.
            var LUTF16FontFamilies: TArray<String>;
            var LUTF8FontFamilies: TArray<UTF8String>;
            var LMarshaledFontFamilies: TArray<MarshaledAString>;
            LUTF16FontFamilies := AOptions.FontFamily.Split([',', #13, #10], TStringSplitOptions.ExcludeEmpty);
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
                sk4d_strutstyle_set_font_families(LStrutstyle, @LMarshaledFontFamilies[0], I);
            end;

            // https://api.flutter.dev/flutter/painting/StrutStyle/fontSize.html
            // The size of text (in logical pixels) to use when obtaining metrics from the font.
            // The fontSize is used to get the base set of metrics that are then
            // used to calculated the metrics of strut. The height and leading are expressed as a multiple of fontSize.
            // The default fontSize is 14 logical pixels.
            if (not sameValue(AOptions.GetScaledfontsize, 0, TEpsilon.FontSize)) then
              sk4d_strutstyle_set_font_size(LStrutstyle, AOptions.GetScaledFontSize);

            // https://api.flutter.dev/flutter/painting/StrutStyle/fontStyle.html
            // https://api.flutter.dev/flutter/painting/StrutStyle/fontWeight.html
            // The typeface variant to use when calculating the strut (e.g., italics). The default fontStyle is FontStyle.normal.
            // The typeface thickness to use when calculating the strut (e.g., bold). The default fontWeight is FontWeight.w400.
            var LSkfontstyle := _GetSkFontStyle(AOptions.FontWeight, AOptions.FontSlant, AOptions.FontStretch);
            sk4d_strutstyle_set_font_style(LStrutstyle, @LSkfontstyle);

            // https://api.flutter.dev/flutter/painting/StrutStyle/forceStrutHeight.html
            // when true, all lines will be laid out with the height of the strut. All line and
            // run-specific metrics will be ignored/overridden and only strut metrics will be
            // used instead. This property guarantees uniform line spacing, however text in
            // adjacent lines may overlap. This property should be enabled with caution as it
            // bypasses a large portion of the vertical layout system. The default value is false.
            // sk4d_strutstyle_set_force_height(LStrutstyle, True);

            // https://api.flutter.dev/flutter/dart-ui/TextLeadingDistribution.html
            // proportional (half_leading: FALSE)
            //     Distributes the leading of the text proportionally above and below the text,
            //     to the font's ascent/descent ratio. The leading of a text run is defined as
            //     TextStyle.height * TextStyle.fontSize - TextStyle.fontSize.
            //     When TextStyle.height is not set, the text run uses the leading specified
            //     by the font instead.
            // even (half_leading: TRUE)
            //     Distributes the "leading" of the text evenly above and below the text
            //     (i.e. evenly above the font's ascender and below the descender).
            //     The leading of a text run is defined as
            //     TextStyle.height * TextStyle.fontSize - TextStyle.fontSize.
            //     When TextStyle.height is not set, the text run uses the leading specified
            //     by the font instead.
            //     The leading can become negative when TextStyle.height is smaller than 1.0.
            //     This is the default strategy used by CSS, known as "half-leading".
            // Default is proportional
            // Note: This setting work only with sk4d_strutstyle_set_height_multiplier and have no
            // influence on sk4d_strutstyle_set_leading where the spacing is equally distributed
            // above and below the baseline
            sk4d_strutstyle_set_half_leading(LStrutstyle, true);

            // https://api.flutter.dev/flutter/painting/StrutStyle/height.html
            // the multiple of fontSize the line's height should be. The line's height
            // will take the font's ascent and descent values if height{multiplier} is
            // omitted or null. If provided, the EM-square ascent and descent (which sum
            // to fontSize) is scaled by height{multiplier}. The height{multiplier} will
            // impact the spacing above and below the baseline differently depending on
            // the ratios between the font's ascent and descent. This property is separate
            // from the leading multiplier, which is controlled through leading. Default is null.
            if (not sameValue(AOptions.GetScaledLineHeight, 0, TEpsilon.FontSize)) and
               (not sameValue(AOptions.GetScaledfontsize, 0, TEpsilon.FontSize)) then
              sk4d_strutstyle_set_height_multiplier(LStrutstyle, AOptions.GetScaledLineHeight / AOptions.GetScaledfontsize); // LineHeight is given in PX

            // https://api.flutter.dev/flutter/painting/StrutStyle/leading.html
            // the custom leading to apply to the strut as a multiple of fontSize. Leading is
            // additional spacing between lines. Half of the leading is added to the top and
            // the other half to the bottom of the line height. This differs from height since
            // the spacing is equally distributed above and below the baseline. Default is null,
            // which will use the font-specified leading.
            if not sameValue(AOptions.GetScaledLineSpacing, 0, TEpsilon.FontSize) then
              sk4d_strutstyle_set_leading(LStrutstyle, AOptions.GetScaledLineSpacing / AOptions.GetScaledfontsize); // LineSpacing is given in PX

            // Enable strut
            sk4d_strutstyle_set_enabled(LStrutstyle, true);

            // Set strut style
            sk4d_paragraphstyle_set_strut_style(LParagraphstyle, LStrutstyle);

            // Direction
            if AOptions.Direction = TALTextDirection.RightToLeft then
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
            case AOptions.HTextAlign of
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
                  if AOptions.EllipsisFontFamily <> '' then begin
                    LFontFamilies.Add(AOptions.EllipsisFontFamily);
                    LFontSizes.Add(AOptions.GetScaledEllipsisFontSize);
                    LFontWeights.Add(AOptions.EllipsisFontWeight);
                    LFontSlants.Add(AOptions.EllipsisFontSlant);
                    LFontStretchs.Add(AOptions.EllipsisFontStretch);
                    LFontColors.Add(AOptions.EllipsisFontColor);
                    LDecorationKinds.Add(AOptions.EllipsisDecorationKinds);
                    LDecorationStyles.Add(AOptions.EllipsisDecorationStyle);
                    LDecorationThicknessMultipliers.Add(AOptions.EllipsisDecorationThicknessMultiplier);
                    if AOptions.EllipsisDecorationColor <> Talphacolors.Null then LDecorationColors.Add(AOptions.EllipsisDecorationColor)
                    else LDecorationColors.Add(AOptions.DecorationColor);
                    LBackgroundColors.Add(TALphaColors.Null);
                  end;
                  LSpanIDs.Add('ellipsis');
                  if (length(AOptions.EllipsisText) > 2) and (ALPosW('… ', AOptions.EllipsisText) = 1) then
                    LCurrText := ALCopyStr(AOptions.EllipsisText, 3, maxint)
                  else
                    LCurrText := AOptions.EllipsisText;
                  LCurrImgSrc := '';
                  LcurrImgWidth := 0;
                  LcurrImgHeight := 0;
                  LInsertEllipsisAt := Maxint;
                  P1 := Maxint;
                end

                /////////////////////////////////////
                // The text contains HTML elements //
                /////////////////////////////////////

                else if AOptions.TextIsHtml then begin

                  // Extract LCurrText / LCurrImgSrc
                  if Ltext[P1] = '<' then begin

                    // Proper HTML requires that elements are correctly nested and
                    // closed in the order they are opened. Here's your original
                    // snippet for reference:

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
                      _getInfosFromTag(ALCopyStr(LTag, 7, length(LTag) - 7), LSpanIDs, LFontFamilies, LFontSizes, LFontWeights, LFontSlants, LFontStretchs, LFontColors, LDecorationKinds, LDecorationStyles, LDecorationThicknessMultipliers, LDecorationColors, LBackgroundColors, LLineHeights, LLetterSpacings);
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
                      if LLineHeights.count > 0 then LLineHeights.Delete(LLineHeights.Count - 1);
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

                  // Init LSpanID
                  var LSpanID: String;
                  if LSpanIDs.Count > 0 then LSpanID := LSpanIDs[LSpanIDs.Count - 1]
                  else LSpanID := '';

                  // Init LFontFamily
                  var LFontFamily: String;
                  if LFontFamilies.Count > 0 then LFontFamily := LFontFamilies[LFontFamilies.Count - 1]
                  else LFontFamily := AOptions.FontFamily;

                  // Init LFontSize
                  var LFontSize: Single;
                  if LFontSizes.Count > 0 then LFontSize := LFontSizes[LFontSizes.Count - 1]
                  else LFontSize := AOptions.GetScaledFontSize;

                  // Init LFontWeight
                  var LFontWeight: TFontWeight;
                  if LFontWeights.Count > 0 then LFontWeight := LFontWeights[LFontWeights.Count - 1]
                  else LFontWeight := AOptions.FontWeight;

                  // Init LFontSlant
                  var LFontSlant: TFontSlant;
                  if LFontSlants.Count > 0 then LFontSlant := LFontSlants[LFontSlants.Count - 1]
                  else LFontSlant := AOptions.FontSlant;

                  // Init LFontStretch
                  var LFontStretch: TFontStretch;
                  if LFontStretchs.Count > 0 then LFontStretch := LFontStretchs[LFontStretchs.Count - 1]
                  else LFontStretch := AOptions.FontStretch;

                  // Init LFontColor
                  var LFontColor: TalphaColor;
                  if LFontColors.Count > 0 then LFontColor := LFontColors[LFontColors.Count - 1]
                  else LFontColor := AOptions.FontColor;

                  // Init LDecorationKind
                  var LDecorationKind: TALTextDecorationKinds;
                  if LDecorationKinds.Count > 0 then LDecorationKind := LDecorationKinds[LDecorationKinds.Count - 1]
                  else LDecorationKind := AOptions.DecorationKinds;

                  // Init LDecorationStyle
                  var LDecorationStyle: TALTextDecorationStyle;
                  if LDecorationStyles.Count > 0 then LDecorationStyle := LDecorationStyles[LDecorationStyles.Count - 1]
                  else LDecorationStyle := AOptions.DecorationStyle;

                  // Init LDecorationThicknessMultiplier
                  var LDecorationThicknessMultiplier: Single;
                  if LDecorationThicknessMultipliers.Count > 0 then LDecorationThicknessMultiplier := LDecorationThicknessMultipliers[LDecorationThicknessMultipliers.Count - 1]
                  else LDecorationThicknessMultiplier := AOptions.DecorationThicknessMultiplier;

                  // Init LDecorationColor
                  var LDecorationColor: TalphaColor;
                  if LDecorationColors.Count > 0 then LDecorationColor := LDecorationColors[LDecorationColors.Count - 1]
                  else LDecorationColor := AOptions.DecorationColor;

                  // Init LBackgroundColor
                  var LBackgroundColor: TalphaColor;
                  if LBackgroundColors.Count > 0 then LBackgroundColor := LBackgroundColors[LBackgroundColors.Count - 1]
                  else LBackgroundColor := TalphaColors.Null;

                  // Init LLineHeight
                  var LLineHeight: Single;
                  if LLineHeights.Count > 0 then LLineHeight := LLineHeights[LLineHeights.Count - 1]
                  else LLineHeight := AOptions.GetScaledLineHeight;

                  // Init LLetterSpacing
                  var LLetterSpacing: Single;
                  if LLetterSpacings.Count > 0 then LLetterSpacing := LLetterSpacings[LLetterSpacings.Count - 1]
                  else LLetterSpacing := AOptions.GetScaledLetterSpacing;

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
                      raise Exception.Create('Error #90AD8D31-4CEA-4AE3-8D84-51B5EB6E171B');
                    LCurrText := ALCopyStr(LCurrText, 1, LCurrText.Length - (LCurrIndex - LInsertEllipsisAt));
                    if (length(AOptions.EllipsisText) > 2) and (ALPosW('… ', AOptions.EllipsisText) = 1) then begin
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
                    if not sameValue(LFontSize, 0, TEpsilon.FontSize) then
                      sk4d_textstyle_set_font_size(LTextStyle, LFontSize);

                    // Set the font Weight, Slant and Stretch
                    LSkfontstyle := _GetSkFontStyle(LFontWeight, LFontSlant, LFontStretch);
                    sk4d_textstyle_set_font_style(LTextStyle, @LSkfontstyle);

                    // Set the font color
                    sk4d_textstyle_set_color(LTextStyle, LFontColor);

                    // Set decoration kinds
                    // https://api.flutter.dev/flutter/painting/TextStyle/decoration.html
                    // The decorations to paint near the text (e.g., an underline).
                    {$IFNDEF ALCompilerVersionSupported120}
                      {$MESSAGE WARN 'Check if declaration of System.Skia.API.TSkTextDecoration didn''t changed'}
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
                    if (not SameValue(LLineHeight, 0, TEpsilon.FontSize)) and
                       (not SameValue(LFontSize, 0, TEpsilon.FontSize)) then
                      sk4d_textstyle_set_height_multiplier(LTextStyle, LLineHeight / LFontSize); // LineHeight is given in PX

                    // Push the style
                    // https://api.flutter.dev/flutter/dart-ui/ParagraphBuilder/pushStyle.html
                    // Applies the given style to the added text until pop is called.
                    sk4d_paragraphbuilder_push_style(LParagraphBuilder, LTextStyle);

                    // Add the text or PlaceHolder
                    if LCurrImgSrc <> '' then begin
                      {$IFNDEF ALCompilerVersionSupported120}
                        {$MESSAGE WARN 'Check if declaration of System.Skia.API.sk_placeholderstyle_t didn''t changed'}
                      {$ENDIF}
                      var LPlaceholderStyle: sk_placeholderstyle_t;
                      if CompareValue(LCurrImgWidth, 0, TEpsilon.FontSize) <= 0 then LCurrImgWidth := AOptions.GetScaledFontSize;
                      if CompareValue(LCurrImgHeight, 0, TEpsilon.FontSize) <= 0 then LCurrImgHeight := AOptions.GetScaledFontSize;
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
                sk4d_paragraph_layout(Lparagraph, ARect.Width - AOptions.GetScaledPaddingLeft - AOptions.GetScaledPaddingRight);

                // Get the Metrics of each lines
                var LMetrics: Tarray<sk_metrics_t>;
                SetLength(LMetrics, sk4d_paragraph_get_line_metrics(Lparagraph, nil));
                if length(LMetrics) = 0 then begin
                  ARect.Width := 0;
                  ARect.Height := 0;
                  exit(ALNullDrawable);
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
                if AOptions.FailIfTextBroken and ATextBroken then begin
                  ARect.Width := 0;
                  ARect.Height := 0;
                  exit(ALNullDrawable);
                end;

                // Update ARect
                var LReLayout: Boolean := False;
                var LMaxHeight := ARect.Height - AOptions.GetScaledPaddingTop - AOptions.GetScaledPaddingBottom;
                Var LParagraphRect := TrectF.Empty;
                for var I := Low(LMetrics) to High(LMetrics) do begin
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
                  LParagraphRect.Width := Max(LParagraphRect.Width, Ceil(LMetrics[I].width));
                  LParagraphRect.Height := LParagraphRect.Height + Ceil(LMetrics[I].height);
                  // We have too many lines to fit within the maxHeight constraint.
                  if (CompareValue(LParagraphRect.Height, LMaxHeight, TEpsilon.Position) > 0) then begin
                    if I = 0 then begin
                      ARect.Width := 0;
                      ARect.Height := 0;
                      exit(ALNullDrawable);
                    end;
                    // Although it's impossible to have LMaxLines = I, I prefer to crash rather
                    // than enter an infinite loop.
                    if LMaxLines <= I then
                      Raise exception.Create('Error #47772843-A4B1-4133-9FD2-3DA7D4DBB2ED');
                    LMaxLines := I {0 based};
                    AAllTextDrawn := False;
                    LInsertEllipsisAt := LMetrics[i-1].end_excluding_whitespaces;
                    LPrevInsertEllipsisAt := LInsertEllipsisAt;
                    LReLayout := True;
                    Break;
                  end
                  // We have exceeded the maxLines, so add the EllipsisText.
                  else if ((I = High(LMetrics)) and (LDidExceedMaxLines) and (AOptions.EllipsisText <> '')) then begin
                    AAllTextDrawn := False;
                    LInsertEllipsisAt := Min(LMetrics[i].end_excluding_whitespaces, LPrevInsertEllipsisAt - 1);
                    if LInsertEllipsisAt < 0 then begin
                      ARect.Width := 0;
                      ARect.Height := 0;
                      exit(ALNullDrawable);
                    end;
                    case AOptions.Trimming of
                      TALTextTrimming.Word: begin
                        While LInsertEllipsisAt > 0 do begin
                          if (LTextForRange.Chars[LInsertEllipsisAt].IsWhiteSpace) and (not LTextForRange.Chars[LInsertEllipsisAt-1].IsWhiteSpace) then break
                          else dec(LInsertEllipsisAt);
                        end;
                      end;
                      //--
                      TALTextTrimming.Character: begin
                        While LInsertEllipsisAt > 0 do begin
                          if (not LTextForRange.Chars[LInsertEllipsisAt-1].IsWhiteSpace) then break
                          else dec(LInsertEllipsisAt);
                        end;
                      end;
                      //--
                      else
                        raise Exception.Create('Error 3FB3E49C-A73D-468C-A4FA-32830BEBF896');
                    end;
                    LPrevInsertEllipsisAt := LInsertEllipsisAt;
                    LReLayout := True;
                    Break;
                  end;
                end;
                if LReLayout then continue;
                if AOptions.Autosize or (AOptions.AutosizeX and AOptions.AutosizeY) then begin
                  ARect.Width := LParagraphRect.Width + AOptions.GetScaledPaddingLeft + AOptions.GetScaledPaddingRight;
                  ARect.Height := LParagraphRect.Height + AOptions.GetScaledPaddingTop + AOptions.GetScaledPaddingBottom;
                end
                else if AOptions.AutosizeX then ARect.Width := LParagraphRect.Width + AOptions.GetScaledPaddingLeft + AOptions.GetScaledPaddingRight
                else if AOptions.AutosizeY then ARect.Height := LParagraphRect.Height + AOptions.GetScaledPaddingTop + AOptions.GetScaledPaddingBottom;

                // init LParagraphRect.topleft
                case AOptions.VTextAlign of
                  TALTextVertAlign.Center: LParagraphRect.Offset(AOptions.GetScaledPaddingLeft, AOptions.GetScaledPaddingTop + ((ARect.Height - LParagraphRect.Height - AOptions.GetScaledPaddingTop - AOptions.GetScaledPaddingBottom) / 2));
                  TALTextVertAlign.Leading: LParagraphRect.Offset(AOptions.GetScaledPaddingLeft, AOptions.GetScaledPaddingTop);
                  TALTextVertAlign.Trailing: LParagraphRect.Offset(AOptions.GetScaledPaddingLeft, ARect.Height - AOptions.GetScaledPaddingBottom - LParagraphRect.Height);
                  Else raise Exception.Create('Error 6DBA0B08-4F9E-4D89-9998-6B054D527F1F');
                end;

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
                                               (LParagraphRect.Left + Ltextbox.rect.Left) / AOptions.Scale,
                                               (LParagraphRect.Top + Ltextbox.rect.Top) / AOptions.Scale,
                                               (LParagraphRect.Left + Ltextbox.rect.Right) / AOptions.Scale,
                                               (LParagraphRect.Top + Ltextbox.rect.Bottom) / AOptions.Scale);
                    end;
                  end;
                end;

                // Nothing to draw then exit with a null drawable
                If (Round(ARect.Width) = 0) or
                   (Round(ARect.Height) = 0) then exit(ALNullDrawable);

                // Create the drawing surface
                var LSurface: sk_surface_t;
                var LCanvas: sk_canvas_t;
                ALCreateSurface(
                  LSurface, // out ASurface: sk_surface_t;
                  LCanvas, // out ACanvas: sk_canvas_t;
                  round(ARect.Width), // const w: integer;
                  round(ARect.Height));// const h: integer)
                try

                  // Draw the background
                  if ((AOptions.IsFillAssigned) and (AOptions.Fill.Kind <> TbrushKind.None)) or
                     ((AOptions.IsStrokeAssigned) and (AOptions.stroke.Kind <> TbrushKind.None)) then begin
                    Tmonitor.Enter(AOptions.Stroke);
                    var LStrokeOnChangedRestoreValue := AOptions.Stroke.OnChanged;
                    var LStrokeThicknessRestoreValue: Single := AOptions.Stroke.Thickness;
                    try
                      AOptions.Stroke.Thickness := AOptions.Stroke.Thickness * AOptions.Scale;
                      ALDrawRectangle(
                        LCanvas, // const ACanvas: sk_canvas_t;
                        1, // const AScale: Single;
                        ARect, // const ARect: TrectF;
                        AOptions.Fill, // const Fill: TBrush;
                        AOptions.Stroke, // const Stroke: TStrokeBrush;
                        nil, // const Shadow: TALShadow
                        AOptions.Sides, // const Sides: TSides;
                        AOptions.Corners, // const Corners: TCorners;
                        AOptions.GetScaledXRadius, // const XRadius: Single = 0;
                        AOptions.GetScaledYRadius); // const YRadius: Single = 0);
                    finally
                      AOptions.Stroke.Thickness := LStrokeThicknessRestoreValue;
                      AOptions.Stroke.OnChanged := LStrokeOnChangedRestoreValue;
                      Tmonitor.Exit(AOptions.Stroke);
                    end;
                  end
                  else if (AOptions.FillColor <> TalphaColors.Null) or
                          (AOptions.StrokeColor <> TalphaColors.Null) then begin
                    ALDrawRectangle(
                      LCanvas, // const ACanvas: sk_canvas_t;
                      1, // const AScale: Single;
                      ARect, // const ARect: TrectF;
                      AOptions.FillColor, // const AFillColor: TAlphaColor;
                      AOptions.StrokeColor, // const AStrokeColor: TalphaColor;
                      AOptions.GetScaledStrokeThickness, // const AStrokeThickness: Single;
                      TAlphaColors.null, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the rectangle)
                      0, // const AShadowBlur: Single;
                      0, // const AShadowOffsetX: Single;
                      0, // const AShadowOffsetY: Single;
                      AOptions.Sides, // const Sides: TSides;
                      AOptions.Corners, // const Corners: TCorners;
                      AOptions.GetScaledXRadius, // const XRadius: Single = 0;
                      AOptions.GetScaledYRadius); // const YRadius: Single = 0);
                  end;

                  // Paint the paragraph
                  sk4d_paragraph_paint(
                    LParagraph, // self: sk_paragraph_t;
                    LCanvas, // canvas: sk_canvas_t;
                    LParagraphRect.left, // x: float;
                    LParagraphRect.top); // y: float

                  // retrieve the rect of all placeholders
                  SetLength(LTextBoxes, sk4d_paragraph_get_rects_for_placeholders(LParagraph, nil));
                  sk4d_paragraph_get_rects_for_placeholders(LParagraph, @LTextBoxes[0]);

                  // Paint all images
                  if Length(LTextBoxes) > LPlaceHolders.Count then
                    raise Exception.Create('Error 155FE121-9CA2-46F8-A51C-0CB72EADC7EC');
                  For var i := low(LTextBoxes) to high(LTextBoxes) do begin
                    Var LImgSrc := LPlaceHolders[i];
                    If LImgSrc <> '' then begin
                      Var LDstRect := TRectF.Create(
                                        LTextBoxes[i].rect.left,
                                        LTextBoxes[i].rect.Top,
                                        LTextBoxes[i].rect.Right,
                                        LTextBoxes[i].rect.Bottom);
                      LDstRect.Offset(LParagraphRect.TopLeft);
                      var LSrcRect := TRectF.Create(0,0,LDstRect.Width, LDstRect.Height);
                      {$IFDEF ALDPK}
                      var LImg: sk_image_t;
                      var LFileName := ALDPKGetResourceFilename(LImgSrc);
                      if LFileName <> '' then
                        LImg := ALLoadFromFileAndStretchToSkImage(LFileName, LDstRect.Width, LDstRect.Height)
                      else
                        LImg := ALNullDrawable;
                      {$ELSE}
                      var LImg := ALLoadFromResourceAndStretchToSkImage(LImgSrc, LDstRect.Width, LDstRect.Height);
                      {$ENDIF}
                      If not ALIsDrawableNull(LImg) then begin
                        try
                          var LSamplingoptions := ALGetNearestSkSamplingoptions;
                          sk4d_canvas_draw_image_rect(
                            LCanvas, // self: sk_canvas_t;
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

                  // Create the result
                  result := ALCreateSkImageFromSurface(LSurface);

                  // break the loop
                  Break;

                finally
                  ALFreeSurface(LSurface, LCanvas);
                end;

              finally
                sk4d_paragraph_destroy(LParagraph);
              end;

            finally
              sk4d_paragraphbuilder_destroy(LParagraphBuilder);
            end;
          finally
            sk4d_strutstyle_destroy(LStrutstyle);
          end;
        finally
          sk4d_paragraphstyle_destroy(LParagraphstyle);
        end;

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
      ALFreeAndNil(LLineHeights);
      ALFreeAndNil(LLetterSpacings);
      ALFreeAndNil(LSpanIDs);
      ALFreeAndNil(LPlaceHolders);
      ALFreeAndNil(LRangeIndexes);
    end;

    {$ENDIF}
    {$ENDREGION}

    {$REGION 'OTHER'}
    {$IF not defined(ALSkiaCanvas)}

ARect.Width := 0;
ARect.Height := 0;
exit(ALNullDrawable);

(*
    {$IF defined(ALGpuCanvas)}
    var LMaxTextureSize := TContextManager.DefaultContextClass.MaxTextureSize;
    if LMaxTextureSize > 0 then begin
      ARect.Width := min(ARect.Width, LMaxTextureSize);
      ARect.height := min(ARect.height, LMaxTextureSize);
    end;
    {$ENDIF}

    // Init out var
    ATextBroken := false;
    AAllTextDrawn := True;
    AAscent := 0;
    ADescent := 0;
    AFirstPos := TpointF.Create(0,0);
    ALastPos := TpointF.Create(0,0);
    setlength(AElements, 0);
    AEllipsisRect := TRectF.Create(0,0,0,0);

    // Init local var
    {$IFDEF IOS}
    var LWhiteSpace := False;
    {$ENDIF}
    var LFirstLineIndent := AOptions.GetScaledFirstLineIndent;
    var LMaxWidth: single := 0;
    var LMaxHeight: Single := 0;
    var LTotalLines := 0;
    var LBold := 0;
    var LItalic := 0;
    var LFontColors := Tlist<TalphaColor>.create;
    var LSpanIDs := TALStringListW.create;

    {$IF defined(ANDROID)}
    var LPaint := TJPaint.JavaClass.init;
    LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
    LPaint.setSubpixelText(true); // Enabling this flag causes glyph advances to be computed with subpixel accuracy.
    LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
    LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
    LPaint.setTextSize(AOptions.GetScaledFontSize);
    //-----
    var LBreakTextItems := TALJBreakTextItems.Create(true{aOwnsObjects});
    {$ELSEIF defined(IOS)}
    var LBreakTextItems := TALCTBreakTextItems.Create(true{aOwnsObjects});
    {$ELSE}
    var LBreakTextItems := TALTLBreakTextItems.Create(true{aOwnsObjects});
    {$ENDIF}
    try


      ///////////////////////////////////
      // loop on all the html elements //
      ///////////////////////////////////

      var P1 := low(AText);
      while P1 <= high(AText) do begin

        var LCurrText: String;
        var LCurrImgSrc: String;


        /////////////////////////////////////
        // The text contains HTML elements //
        /////////////////////////////////////

        if AOptions.TextIsHtml then begin

          // Extract LCurrText / LCurrImgSrc
          if AText[P1] = '<' then begin

            //-----
            LCurrImgSrc := '';
            LCurrText := '';
            var P2 := ALPosW('>', AText, P1+1); // blablabla <font color="#ffffff">blablabla</font> blablabla
                                                //           ^P1                  ^P2
            if P2 <= 0 then break;
            var LTag: String := ALCopyStr(AText, P1, P2 - P1 + 1); // <font color="#ffffff">
            P1 := P2 + 1; // blablabla <font color="#ffffff">blablabla</font> blablabla
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

          end
          else begin

            LCurrImgSrc := '';
            var P2 := ALPosW('<', AText, P1);  // blablabla <font color="#ffffff">blablabla</font> blablabla
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

            P1 := P2; // blablabla <font color="#ffffff">blablabla</font> blablabla
                      //                                          ^P1
          end;

        end

        /////////////////////////////////////////////
        // The text does NOT contain HTML elements //
        /////////////////////////////////////////////

        else begin
          LCurrText := AText;
          LCurrImgSrc := '';
          P1 := Maxint;
        end;


        ////////////////////////////
        // Break the current text //
        ////////////////////////////

        if LCurrText <> '' then begin

          // Init LFontColor
          var LFontColor: TalphaColor;
          if LFontColors.Count > 0 then LFontColor := LFontColors[LFontColors.Count - 1]
          else LFontColor := AOptions.FontColor;

          // Init LSpanID
          var LSpanID: String;
          if LSpanIDs.Count > 0 then LSpanID := LSpanIDs[LSpanIDs.Count - 1]
          else LSpanID := '';

          // Init LFontStyle
          var LFontStyle: TfontStyles;
          if ((TFontStyle.fsBold in AOptions.FontStyle) or (LBold > 0)) and
             ((TFontStyle.fsItalic in AOptions.FontStyle) or (LItalic > 0)) then LFontStyle := [TFontStyle.fsBold, TFontStyle.fsItalic]
          else if ((TFontStyle.fsBold in AOptions.FontStyle) or (LBold > 0)) then LFontStyle := [TFontStyle.fsBold]
          else if ((TFontStyle.fsItalic in AOptions.FontStyle) or (LItalic > 0)) then LFontStyle := [TFontStyle.fsItalic]
          else LFontStyle := [];

          //loop style we draw all the text or at least the ellipsis
          var LTmpRect: TrectF;
          var LTmpTotalLines: integer;
          var LTmpAllTextDrawn: Boolean;
          var LTmpTextBroken: Boolean;
          var LBreakTextItemsCount: integer;
          while True do begin

            //init LTmpRect / LBreakTextItemsCount
            LTmpRect := ARect;
            LTmpRect.Width := LTmpRect.Width - AOptions.GetScaledPaddingLeft - AOptions.GetScaledPaddingRight;
            LTmpRect.Height := LTmpRect.Height - AOptions.GetScaledPaddingTop - AOptions.GetScaledPaddingBottom;
            LBreakTextItemsCount := LBreakTextItems.Count;

            //break the text
            {$IF defined(ANDROID)}
            LTmpTextBroken := ALJBreakText(
                                LPaint, // const APaint: JPaint;
                                AOptions.FontFamily, // const AFontFamily: String;
                                AOptions.GetScaledFontSize, // const AFontSize: single;
                                LFontStyle, // const AFontStyle: TFontStyles;
                                LFontColor, // const AFontColor: TalphaColor;
                                LTmpRect, // var ARect: TRectF;
                                StringtoJString(LCurrText), // const AText: JString;
                                AOptions.WordWrap, //const AWordWrap: Boolean;
                                TTextAlign.Leading, TTextAlign.Leading, //const AHTextAlign, AVTextAlign: TTextAlign;
                                AOptions.Trimming, // const ATrimming: TALTextTrimming;
                                LBreakTextItems, // var aBreakTexts: Tarray<Tpair<JString, TpointF>>);
                                LTmpTotalLines, // var ATotalLines: integer
                                LTmpAllTextDrawn, // out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
                                LFirstLineIndent, // const AFirstLineIndent: TpointF;
                                AOptions.GetScaledLineSpacing, // const ALineSpacing: single = 0;
                                StringtoJString(AOptions.EllipsisText), //  const AEllipsisText: JString = nil;
                                AOptions.EllipsisFontStyle, // const AEllipsisFontStyle: TFontStyles = [];
                                AOptions.EllipsisFontColor, // const AEllipsisFontColor: TalphaColor = TAlphaColorRec.Null
                                AOptions.MaxLines - LTotalLines + AlifThen(LTotalLines > 0, 1, 0)); // const AMaxlines: integer = 0
            {$ELSEIF defined(IOS)}
            LTmpTextBroken := ALCTBreakText(
                                AOptions.FontFamily, // const AFontFamily: String;
                                AOptions.GetScaledFontSize, // const AFontSize: single;
                                LFontStyle, // const AFontStyle: TFontStyles;
                                LFontColor, // const AFontColor: TalphaColor;
                                LTmpRect, // var ARect: TRectF;
                                LCurrText, // const AText: string;
                                AOptions.WordWrap, // const AWordWrap: Boolean;
                                TTextAlign.Leading, TTextAlign.Leading, // const AHTextAlign, AVTextAlign: TTextAlign;
                                AOptions.Trimming, // const ATrimming: TALTextTrimming;
                                LBreakTextItems, // const ABreakTextItems: TALBreakTextItems;
                                LTmpTotalLines, // var ATotalLines: integer;
                                LTmpAllTextDrawn, // out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
                                LFirstLineIndent, // const AFirstLineIndent: TpointF;
                                AOptions.GetScaledLineSpacing, // const ALineSpacing: single = 0;
                                AOptions.EllipsisText, // const AEllipsisText: string = '…';
                                AOptions.EllipsisFontStyle, // const AEllipsisFontStyle: TFontStyles = [];
                                AOptions.EllipsisFontColor, // const AEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                                AOptions.MaxLines - LTotalLines + AlifThen(LTotalLines > 0, 1, 0)); // const AMaxlines: integer = 0
            {$ELSE}
            LTmpTextBroken := ALTLBreakText(
                                AOptions.FontFamily, // const AFontFamily: String;
                                AOptions.GetScaledFontSize, // const AFontSize: single;
                                LFontStyle, // const AFontStyle: TFontStyles;
                                LFontColor, // const AFontColor: TalphaColor;
                                LTmpRect, // var ARect: TRectF;
                                LCurrText, // const AText: string;
                                AOptions.WordWrap, // const AWordWrap: Boolean;
                                TTextAlign.Leading, TTextAlign.Leading, // const AHTextAlign, AVTextAlign: TTextAlign;
                                AOptions.Trimming, // const ATrimming: TALTextTrimming;
                                LBreakTextItems, // const ABreakTextItems: TALBreakTextItems;
                                LTmpTotalLines, // var ATotalLines: integer;
                                LTmpAllTextDrawn, // out AAllTextDrawn: boolean; // out => True if all the text was drawn (no need for any ellipsis)
                                LFirstLineIndent, // const AFirstLineIndent: TpointF;
                                AOptions.GetScaledLineSpacing, // const ALineSpacing: single = 0;
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
              LFontStyle := LBreakTextItem.fontStyle;
              setlength(LCurrText, 2 * length(LCurrText));                            // << I put some space in the end of the previous text to force
              for var I := Low(LCurrText) to High(LCurrText) do LCurrText[i] := ' ';  // << the draw of the ellipsis
              {$IF defined(ANDROID)}
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

          // Update LTotalLines
          if LTotalLines = 0 then LTotalLines := LTmpTotalLines
          else LTotalLines := LTotalLines + LTmpTotalLines - 1;

          // Update LMaxWidth/LMaxHeight
          LMaxWidth := max(LMaxWidth, LTmpRect.Width);
          LMaxHeight := max(LMaxHeight, LTmpRect.height);

          // Update ATextBroken
          ATextBroken := ATextBroken or LTmpTextBroken;
          AAllTextDrawn := AAllTextDrawn and LTmpAllTextDrawn;

          //update all the LBreakTextItem
          for var I := LBreakTextItemsCount to LBreakTextItems.Count - 1 do begin
            var LBreakTextItem := LBreakTextItems[i];
            //-----
            if (not LBreakTextItem.isEllipsis) or (AOptions.EllipsisFontColor = TAlphaColorRec.Null) then LBreakTextItem.fontColor := LFontColor
            else LBreakTextItem.fontColor := AOptions.EllipsisFontColor;
            //-----
            if (not LBreakTextItem.isEllipsis) then LBreakTextItem.FontStyle := LFontStyle
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


      /////////////////////////
      // Paint the paragraph //
      /////////////////////////

      if AOptions.Autosize or (AOptions.AutosizeX and AOptions.AutosizeY) then begin
        ARect.Width := LMaxWidth + AOptions.GetScaledPaddingLeft + AOptions.GetScaledPaddingRight;
        ARect.Height := LMaxHeight + AOptions.GetScaledPaddingTop + AOptions.GetScaledPaddingBottom;
      end
      else if AOptions.AutosizeX then ARect.Width := LMaxWidth + AOptions.GetScaledPaddingLeft + AOptions.GetScaledPaddingRight
      else if AOptions.AutosizeY then ARect.Height := LMaxHeight + AOptions.GetScaledPaddingTop + AOptions.GetScaledPaddingBottom;
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
                                                             AOptions.GetScaledPaddingLeft -
                                                             AOptions.GetScaledPaddingRight) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                                   while J < I do begin
                                     LBreakTextItems[j].pos.X := LBreakTextItems[j].pos.X + AOptions.GetScaledPaddingLeft + LOffset;
                                     LBreakTextItems[j].rect.Offset(AOptions.GetScaledPaddingLeft + LOffset, 0);
                                     inc(J);
                                   end;
                                   if (I <> LBreakTextItems.Count) then LCurrentLineY := LBreakTextItems[I].rect.top;
                                 end;
                               end;
                             end;
                           end;
        TTextAlign.Leading: begin
                             for var I := 0 to LBreakTextItems.Count - 1 do begin
                               LBreakTextItems[i].pos.X := LBreakTextItems[i].pos.X + AOptions.GetScaledPaddingLeft;
                               LBreakTextItems[i].rect.Offset(AOptions.GetScaledPaddingLeft, 0);
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
                                                               AOptions.GetScaledPaddingLeft -
                                                               AOptions.GetScaledPaddingRight)); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                                     while J < I do begin
                                       LBreakTextItems[j].pos.X := LBreakTextItems[j].pos.X + AOptions.GetScaledPaddingLeft + LOffset;
                                       LBreakTextItems[j].rect.Offset(AOptions.GetScaledPaddingLeft + LOffset, 0);
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
                                                       AOptions.GetScaledPaddingTop -
                                                       AOptions.GetScaledPaddingBottom) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                             for var I := 0 to LBreakTextItems.Count - 1 do begin
                               LBreakTextItems[I].pos.y := LBreakTextItems[i].pos.y + AOptions.GetScaledPaddingTop + LOffset;
                               LBreakTextItems[I].rect.Offset(0, AOptions.GetScaledPaddingTop + LOffset);
                             end;
                           end;
        TTextAlign.Leading: begin
                             for var I := 0 to LBreakTextItems.Count - 1 do begin
                               LBreakTextItems[i].pos.Y := LBreakTextItems[i].pos.Y + AOptions.GetScaledPaddingTop;
                               LBreakTextItems[i].rect.Offset(0, AOptions.GetScaledPaddingTop);
                             end;
                           end;
        TTextAlign.Trailing: begin
                               var LOffset: single := Floor(
                                                        (ARect.height -
                                                         LMaxHeight -
                                                         AOptions.GetScaledPaddingTop -
                                                         AOptions.GetScaledPaddingBottom)); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                               for var I := 0 to LBreakTextItems.Count - 1 do begin
                                 LBreakTextItems[I].pos.y := LBreakTextItems[i].pos.y + AOptions.GetScaledPaddingTop + LOffset;
                                 LBreakTextItems[I].rect.Offset(0, AOptions.GetScaledPaddingTop + LOffset);
                               end;
                             end;
      end;
      ARect := ALAlignDimensionToPixelCeil(ARect, 1{Scale});

      // Init out vars
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

      // Update AElements
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

      {$REGION 'ANDROID'}
      {$IF defined(ANDROID)}

      // Create the drawing surface
      var LBitmap: Jbitmap;
      var LCanvas: Jcanvas;
      ALCreateSurface(
        LBitmap, // Var aBitmap: Jbitmap;
        LCanvas, // Var aCanvas: Jcanvas;
        round(max(1, ARect.Width)), // const w: integer;
        round(max(1, ARect.Height)));// const h: integer)
      try

        // Draw the background
        if ((AOptions.IsFillAssigned) and (AOptions.Fill.Kind <> TbrushKind.None)) or
           ((AOptions.IsStrokeAssigned) and (AOptions.stroke.Kind <> TbrushKind.None)) then begin
          Tmonitor.Enter(AOptions.Stroke);
          var LStrokeOnChangedRestoreValue := AOptions.Stroke.OnChanged;
          var LStrokeThicknessRestoreValue: Single := AOptions.Stroke.Thickness;
          try
            ALDrawRectangle(
              LCanvas, // const ACanvas: Jcanvas;
              1, // const AScale: Single;
              ARect, // const ARect: TrectF;
              AOptions.Fill, // const Fill: TBrush;
              AOptions.Stroke, // const Stroke: TStrokeBrush;
              nil, // const Shadow: TALShadow
              AOptions.Sides, // const Sides: TSides;
              AOptions.Corners, // const Corners: TCorners;
              AOptions.GetScaledXRadius, // const XRadius: Single = 0;
              AOptions.GetScaledYRadius); // const YRadius: Single = 0);
            finally
              AOptions.Stroke.Thickness := LStrokeThicknessRestoreValue;
              AOptions.Stroke.OnChanged := LStrokeOnChangedRestoreValue;
              Tmonitor.Exit(AOptions.Stroke);
            end;
        end
        else if (AOptions.FillColor <> TalphaColors.Null) or
                (AOptions.StrokeColor <> TalphaColors.Null) then begin
          ALDrawRectangle(
            LCanvas, // const ACanvas: Jcanvas;
            1, // const AScale: Single;
            ARect, // const ARect: TrectF;
            AOptions.FillColor, // const AFillColor: TAlphaColor;
            AOptions.StrokeColor, // const AStrokeColor: TalphaColor;
            AOptions.GetScaledStrokeThickness, // const AStrokeThickness: Single;
            TAlphaColors.null, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the rectangle)
            0, // const AShadowBlur: Single;
            0, // const AShadowOffsetX: Single;
            0, // const AShadowOffsetY: Single;
            AOptions.Sides, // const Sides: TSides;
            AOptions.Corners, // const Corners: TCorners;
            AOptions.GetScaledXRadius, // const XRadius: Single = 0;
            AOptions.GetScaledYRadius); // const YRadius: Single = 0);
        end;

        // Draw all texts
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

        // Create the result
        result := ALJBitmaptoTexture(LBitmap);

      finally
        ALFreeSurface(LBitmap, LCanvas);
      end;

      {$ENDIF}
      {$ENDREGION}

      {$REGION 'IOS'}
      {$IF defined(IOS)}

      // Create the drawing surface
      var LGridHeight := round(max(1, ARect.Height));
      var LContext: CGContextRef;
      ALCreateSurface(
        LContext, // out aContext: CGContextRef;
        round(max(1, ARect.Width)), // const w: integer;
        LGridHeight); // const h: integer;
      try

        // Draw the background
        if ((AOptions.IsFillAssigned) and (AOptions.Fill.Kind <> TbrushKind.None)) or
           ((AOptions.IsStrokeAssigned) and (AOptions.stroke.Kind <> TbrushKind.None)) then begin
          Tmonitor.Enter(AOptions.Stroke);
          var LStrokeOnChangedRestoreValue := AOptions.Stroke.OnChanged;
          var LStrokeThicknessRestoreValue: Single := AOptions.Stroke.Thickness;
          try
            ALDrawRectangle(
              LContext, // const aContext: CGContextRef;
              LGridHeight, // const aGridHeight: Single;
              1, // const AScale: Single;
              ARect, // const ARect: TrectF;
              AOptions.Fill, // const Fill: TBrush;
              AOptions.Stroke, // const Stroke: TStrokeBrush;
              nil, // const Shadow: TALShadow
              AOptions.Sides, // const Sides: TSides;
              AOptions.Corners, // const Corners: TCorners;
              AOptions.GetScaledXRadius, // const XRadius: Single = 0;
              AOptions.GetScaledYRadius); // const YRadius: Single = 0);
            finally
              AOptions.Stroke.Thickness := LStrokeThicknessRestoreValue;
              AOptions.Stroke.OnChanged := LStrokeOnChangedRestoreValue;
              Tmonitor.Exit(AOptions.Stroke);
            end;
        end
        else if (AOptions.FillColor <> TalphaColors.Null) or
                (AOptions.StrokeColor <> TalphaColors.Null) then begin
          ALDrawRectangle(
            LContext, // const aContext: CGContextRef;
            LGridHeight, // const aGridHeight: Single;
            1, // const AScale: Single;
            ARect, // const ARect: TrectF;
            AOptions.FillColor, // const AFillColor: TAlphaColor;
            AOptions.StrokeColor, // const AStrokeColor: TalphaColor;
            AOptions.GetScaledStrokeThickness, // const AStrokeThickness: Single;
            TAlphaColors.null, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the rectangle)
            0, // const AShadowBlur: Single;
            0, // const AShadowOffsetX: Single;
            0, // const AShadowOffsetY: Single;
            AOptions.Sides, // const Sides: TSides;
            AOptions.Corners, // const Corners: TCorners;
            AOptions.GetScaledXRadius, // const XRadius: Single = 0;
            AOptions.GetScaledYRadius); // const YRadius: Single = 0);
        end;

        // Draw all texts
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

        // Convert the LContext to texture
        result := ALCGContextReftoTexture(LContext);

      finally
        ALFreeSurface(LContext);
      end;

      {$ENDIF}
      {$ENDREGION}

      {$REGION 'Other'}
      {$IF (not defined(ANDROID)) and (not defined(IOS))}

      // Create the drawing surface
      ALCreateSurface(
        result, // Var aBitmap: Jbitmap;
        round(max(1, ARect.Width)), // const w: integer;
        round(max(1, ARect.Height)));// const h: integer)
      try

        // Begin the scene
        if result.Canvas.BeginScene then
        try

          // Draw the background
          if ((AOptions.IsFillAssigned) and (AOptions.Fill.Kind <> TbrushKind.None)) or
             ((AOptions.IsStrokeAssigned) and (AOptions.stroke.Kind <> TbrushKind.None)) then begin
            Tmonitor.Enter(AOptions.Stroke);
            var LStrokeOnChangedRestoreValue := AOptions.Stroke.OnChanged;
            var LStrokeThicknessRestoreValue: Single := AOptions.Stroke.Thickness;
            try
              ALDrawRectangle(
                result.Canvas, // const ACanvas: Tcanvas;
                1, // const AScale: Single;
                ARect, // const ARect: TrectF;
                AOptions.Fill, // const Fill: TBrush;
                AOptions.Stroke, // const Stroke: TStrokeBrush;
                nil, // const Shadow: TALShadow
                AOptions.Sides, // const Sides: TSides;
                AOptions.Corners, // const Corners: TCorners;
                AOptions.GetScaledXRadius, // const XRadius: Single = 0;
                AOptions.GetScaledYRadius); // const YRadius: Single = 0);
              finally
                AOptions.Stroke.Thickness := LStrokeThicknessRestoreValue;
                AOptions.Stroke.OnChanged := LStrokeOnChangedRestoreValue;
                Tmonitor.Exit(AOptions.Stroke);
              end;
          end
          else if (AOptions.FillColor <> TalphaColors.Null) or
                  (AOptions.StrokeColor <> TalphaColors.Null) then begin
            ALDrawRectangle(
              result.Canvas, // const ACanvas: Tcanvas;
              1, // const AScale: Single;
              ARect, // const ARect: TrectF;
              AOptions.FillColor, // const AFillColor: TAlphaColor;
              AOptions.StrokeColor, // const AStrokeColor: TalphaColor;
              AOptions.GetScaledStrokeThickness, // const AStrokeThickness: Single;
              TAlphaColors.null, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the rectangle)
              0, // const AShadowBlur: Single;
              0, // const AShadowOffsetX: Single;
              0, // const AShadowOffsetY: Single;
              AOptions.Sides, // const Sides: TSides;
              AOptions.Corners, // const Corners: TCorners;
              AOptions.GetScaledXRadius, // const XRadius: Single = 0;
              AOptions.GetScaledYRadius); // const YRadius: Single = 0);
          end;

          // Draw all texts
          result.Canvas.Fill.Kind := TbrushKind.Solid;
          result.Canvas.Font.Family := AOptions.FontFamily;
          result.Canvas.Font.size := AOptions.GetScaledFontSize;
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
      ALFreeAndNil(LFontColors);
      ALFreeAndNil(LSpanIDs);
      {$IF defined(ANDROID)}
      LPaint := nil;
      {$ENDIF}
    end;
*)
    {$ENDIF}
    {$ENDREGION}

  finally
    ARect.Top := ARect.Top / AOptions.Scale;
    ARect.right := ARect.right / AOptions.Scale;
    ARect.left := ARect.left / AOptions.Scale;
    ARect.bottom := ARect.bottom / AOptions.Scale;
  end;
end;

{***************************}
function ALDrawMultiLineText(
           const AText: String;
           var ARect: TRectF; // in => The constraint boundaries in real pixels. out => The calculated rect that contains the text, also in real pixels.
           out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
           out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
           const AOptions: TALDrawMultiLineTextOptions): TALDrawable;
begin
  var LElements: TALTextElements;
  result := ALDrawMultiLineText(
              AText,
              ARect,
              ATextBroken,
              AAllTextDrawn,
              LElements,
              AOptions);
end;

{***************************}
function ALDrawMultiLineText(
           const AText: String;
           var ARect: TRectF; // in => The constraint boundaries in real pixels. out => The calculated rect that contains the text, also in real pixels.
           out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
           const AOptions: TALDrawMultiLineTextOptions): TALDrawable;
begin
  var LElements: TALTextElements;
  var LAllTextDrawn: boolean;
  result := ALDrawMultiLineText(
              AText,
              ARect,
              ATextBroken,
              LAllTextDrawn,
              LElements,
              AOptions);
end;

{***************************}
function ALDrawMultiLineText(
           const AText: String;
           var ARect: TRectF; // in => The constraint boundaries in real pixels. out => The calculated rect that contains the text, also in real pixels.
           const AOptions: TALDrawMultiLineTextOptions): TALDrawable;
begin
  var LElements: TALTextElements;
  var LTextBroken: boolean;
  var LAllTextDrawn: boolean;
  result := ALDrawMultiLineText(
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
