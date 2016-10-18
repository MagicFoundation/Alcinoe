unit ALFmxCommon;

interface

uses System.classes,
     System.UITypes,
     System.Types,
     System.Generics.Collections,
     System.Math.Vectors,
     {$IF defined(ios)}
     iOSapi.CoreGraphics,
     iOSapi.CocoaTypes,
     iOSapi.CoreText,
     Macapi.CoreFoundation,
     {$ENDIF}
     {$IF defined(ANDROID)}
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNI.JavaTypes,
     {$ENDIF}
     Fmx.types,
     FMX.TextLayout,
     FMX.graphics,
     FMX.Effects,
     FMX.controls;

type
  TALCustomConvertFontFamilyProc = function(const AFamily: TFontName; const aFontStyles: TfontStyles): TFontName;

var
  ALCustomConvertFontFamilyProc: TALCustomConvertFontFamilyProc;

function  ALConvertFontFamily(const AFamily: TFontName; const aFontStyles: TfontStyles): TFontName;
function  ALTranslate(const AText: string): string;
Procedure ALFmxMakeBufBitmaps(const aControl: TControl);
function  ALPrepareColor(const SrcColor: TAlphaColor; const Opacity: Single): TAlphaColor;
function  ALAlignDimensionToPixelRound(const Rect: TRectF; const Scale: single): TRectF; overload;
function  ALAlignDimensionToPixelRound(const Dimension: single; const Scale: single): single; overload;
function  ALAlignDimensionToPixelRound(const Rect: TRectF): TRectF; overload;
function  ALAlignDimensionToPixelCeil(const Rect: TRectF; const Scale: single): TRectF; overload;
function  ALAlignDimensionToPixelCeil(const Dimension: single; const Scale: single): single; overload;
function  ALAlignDimensionToPixelCeil(const Rect: TRectF): TRectF; overload;

{$IF defined(IOS)}
type

  PAlphaColorCGFloat = ^TAlphaColorCGFloat;
  TAlphaColorCGFloat = record
  public
    R, G, B, A: CGFloat;
    class function Create(const R, G, B: CGFloat; const A: CGFloat = 1): TAlphaColorCGFloat; overload; static; inline;
    class function Create(const Color: TAlphaColor): TAlphaColorCGFloat; overload; static; inline;
    class function Create(const Color: TAlphaColorF): TAlphaColorCGFloat; overload; static; inline;
  end;

function  ALLowerLeftCGRect(const aUpperLeftOrigin: TPointF; const aWidth, aHeight: single; const aGridHeight: Single): CGRect;
procedure ALGradientEvaluateCallback(info: Pointer; inData: PCGFloat; outData: PAlphaColorCGFloat); cdecl;
function  ALGetCTFontRef(const AFontFamily: String; const aFontSize: single; const aFontStyle: TFontStyles): CTFontRef;
{$ENDIF}

{$IF defined(ANDROID)}

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBreakTextItem = class(Tobject)
  public
    line: JString;
    pos: TpointF;
    rect: TrectF;
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
                     var aBreakTextItems: TALBreakTextItems;
                     const aFirstLineIndent: single = 0;
                     const aLineSpacing: integer = 0;
                     const aEllipsisText: JString = nil;
                     const aEllipsisColor: TalphaColor = TAlphaColorRec.Null): boolean; // return true if text was truncated

{$ENDIF}

{$IF defined(IOS)}

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBreakTextItem = class(Tobject)
  public
    Line: CTLineRef;
    pos: TpointF;
    rect: TrectF;
    constructor Create;
    destructor Destroy; override;
  end;
  TALBreakTextItems = class(TobjectList<TALBreakTextItem>);

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
function ALBreakText(var ARect: TRectF;
                     const aTextAttr: CFAttributedStringRef;
                     const aWordWrap: Boolean;
                     const AHTextAlign, AVTextAlign: TTextAlign;
                     const aTrimming: TTextTrimming;
                     var aBreakTextItems: TALBreakTextItems;
                     const aFirstLineIndent: single = 0;
                     const aLineSpacing: integer = 0;
                     const aEllipsisAttr: CFAttributedStringRef = nil): boolean; overload;// return true if text was truncated

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
                     const aTrimming: TTextTrimming;
                     var aBreakTextItems: TALBreakTextItems;
                     const aFirstLineIndent: single = 0;
                     const aLineSpacing: integer = 0;
                     const aEllipsisText: string = '…';
                     const aEllipsisColor: TalphaColor = TAlphaColorRec.Null): boolean; overload; // return true if text was truncated

{$ENDIF}

Type

  {$IF CompilerVersion <> 31}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl still has the exact same fields and adjust the IFDEF'}
  {$ENDIF}
  TALControlAccessPrivate = class(TFmxObject)
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

  {$IF CompilerVersion <> 31}
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

implementation

uses system.SysUtils,
     System.Character,
     System.UIConsts,
     System.Math,
     {$IF defined(ANDROID)}
     Androidapi.JNIBridge,
     Androidapi.Helpers,
     Androidapi.JNI.Os,
     {$ENDIF}
     fmx.consts,
     fmx.controls.presentation,
     ALFmxObjects,
     AlFmxStdCtrls,
     ALFmxImgList,
     AlCommon;

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

  if aControl is TPresentedControl then TPresentedControl(aControl).ApplyStyleLookup; // this to generate child controls
                                                                                      // that can be TALText for exemple
                                                                                      // (for the Tlabel)
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
  else if (aControl is TALGlyph) then begin
    TALGlyph(aControl).doubleBuffered := True;
    TALGlyph(aControl).MakeBufBitmap;
  end
  else if (aControl is TALCheckBox) then begin
    TALCheckBox(aControl).doubleBuffered := True;
    TALCheckBox(aControl).MakeBufBitmap;
  end
  else if (aControl is TALLine) then begin
    TALLine(aControl).doubleBuffered := True;
    TALLine(aControl).MakeBufBitmap;
  end;

  for aChild in aControl.Controls do
    ALFmxMakeBufBitmaps(aChild);

end;

{****************************************************************************************}
function  ALPrepareColor(const SrcColor: TAlphaColor; const Opacity: Single): TAlphaColor;
begin
  if Opacity < 1 then
  begin
    TAlphaColorRec(Result).R := Round(TAlphaColorRec(SrcColor).R * Opacity);
    TAlphaColorRec(Result).G := Round(TAlphaColorRec(SrcColor).G * Opacity);
    TAlphaColorRec(Result).B := Round(TAlphaColorRec(SrcColor).B * Opacity);
    TAlphaColorRec(Result).A := Round(TAlphaColorRec(SrcColor).A * Opacity);
  end
  else if (TAlphaColorRec(SrcColor).A < $FF) then
    Result := PremultiplyAlpha(SrcColor)
  else
    Result := SrcColor;
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

{****************}
{$IF defined(IOS)}
class function TAlphaColorCGFloat.Create(const R, G, B: CGFloat; const A: CGFloat = 1): TAlphaColorCGFloat;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
end;

{*************************************************************************************}
class function TAlphaColorCGFloat.Create(const Color: TAlphaColor): TAlphaColorCGFloat;
begin
  Result.R := TAlphaColorRec(Color).R / 255;
  Result.G := TAlphaColorRec(Color).G / 255;
  Result.B := TAlphaColorRec(Color).B / 255;
  Result.A := TAlphaColorRec(Color).A / 255;
end;

{**************************************************************************************}
class function TAlphaColorCGFloat.Create(const Color: TAlphaColorf): TAlphaColorCGFloat;
begin
  Result.R := Color.R;
  Result.G := Color.G;
  Result.B := Color.B;
  Result.A := Color.A;
end;
{$ENDIF}

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

(*
static void kiskis_GradientEvaluateCallback(void *info, const float *in, float *out)
{
  /*
  The domain of this function is 0 - 1. For an input value of 0
  this function returns the color to paint at the start point
  of the shading. For an input value of 1 this function returns
  the color to paint at the end point of the shading. This
  is a 1 in, 4 out function where the output values correspond
  to an r,g,b,a color.

  This function evaluates to produce a blend from startColor to endColor.
  Note that the returned results are clipped to the range
  by Quartz so this function doesn't worry about values
  that are outside the range 0-1.
  */

  MyStartEndColor *startEndColorP = (MyStartEndColor * )info;
  float *startColor = startEndColorP->startColor;
  float *endColor = startEndColorP->endColor;
  float input = in[0];
  // Weight the starting and ending color components depending
  // on what position in the blend the input value specifies.
  out[0] = (startColor[0]*(1-input) + endColor[0]*input);
  out[1] = (startColor[1]*(1-input) + endColor[1]*input);
  out[2] = (startColor[2]*(1-input) + endColor[2]*input);
  // The alpha component is always 1, the shading is always opaque.
  out[3] = 1;
}
*)
procedure ALGradientEvaluateCallback(info: Pointer; inData: PCGFloat; outData: PAlphaColorCGFloat); cdecl;
begin
  if info <> nil then
    outData^ := TAlphaColorCGFloat.Create(TGradient(info).InterpolateColor(inData^));
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
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
                     var aBreakTextItems: TALBreakTextItems;
                     const aFirstLineIndent: single = 0;
                     const aLineSpacing: integer = 0;
                     const aEllipsisText: JString = nil;
                     const aEllipsisColor: TalphaColor = TAlphaColorRec.Null): boolean; // return true if text was truncated

var aBreakTextItemsStartCount: integer;
    aBreakTextItem: TALBreakTextItem;
    aNumberOfChars: integer;
    aSaveNumberOfChars: integer;
    aLine: jString;
    aLineManuallyBreaked: boolean;
    aLineIndent: Single;
    aEllipsisLine: Jstring;
    aEllipsisLineLn: single;
    aEllipsisLinePos: TpointF;
    aEllipsisLineRect: TrectF;
    aMaxWidth: single;
    aMaxHeight: single;
    aMaxLineWidth: single;
    aLineHeight: integer;
    aTotalLinesHeight: integer;
    aChar: Char;
    ATextLn: integer;
    ATextIdx: integer;
    aCurrLineY: integer;
    aMetrics: JPaint_FontMetricsInt;
    ameasuredWidth: TJavaArray<Single>;
    aTmpMeasuredWidth: Single;
    aSavedColor: TalphaColor;
    aOffset: single;
    i, j: integer;

  {~~~~~~~~~~~~~~~~~~~~~~}
  procedure _initEllipsis;
  begin
    if aEllipsisLine = nil then begin
      if aEllipsisText = nil then aEllipsisLine := StringtoJString(string('…'))
      else aEllipsisLine := aEllipsisText;
      if aEllipsisColor <> TAlphaColorRec.Null then begin
        aSavedColor := aPaint.getColor;
        aPaint.setColor(aEllipsisColor);
      end;
      aEllipsisLineLn := aPaint.measureText(aEllipsisLine);
      if aEllipsisColor <> TAlphaColorRec.Null then aPaint.setColor(aSavedColor);
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
    end;
  end;

begin

  //init result
  result := false;

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
  aCurrLineY := (-1*aMetrics.ascent); // aMetrics.top and aMetrics.ascent are always returned in negative value
  aLineHeight := aMetrics.descent + aLineSpacing + (-1*aMetrics.ascent);

  //init aEllipsisLine
  aEllipsisLine := nil;
  aEllipsisLineLn := 0;

  //init aLineIndent
  aLineIndent := aFirstLineIndent;

  //if we have at least enalf of height to write the 1rt row
  if comparevalue(aLineHeight,aMaxHeight,Tepsilon.position) <= 0 then begin

    //create ameasuredWidth
    ameasuredWidth := TJavaArray<Single>.Create(1);
    try

      //loop still their is some chars
      while ATextIdx < ATextLn do begin

        // init aline / aLineManuallyBreaked
        i := aText.indexOf($0D {c}, ATextIdx{start}); // find if their is some #13 (MSWINDOWS linebreak = #13#10)
        j := aText.indexOf($0A {c}, ATextIdx{start}); // find if their is some #10 (UNIX linebreak = #10)
        if (i >= 0) and (j >= 0) then I := min(i,j)
        else I := max(I, J);
        if i = ATextIdx then begin
          aLine := StringtoJString(string(''));
          aLineManuallyBreaked := True;
          if (aText.codePointAt(i) = $0D) and
             (i + 1 < ATextLn) and
             (aText.codePointAt(i + 1) = $0A) then ATextIdx := ATextIdx + 1; // (MSWINDOWS linebreak = #13#10)
        end
        else if i > 0 then begin
          aLine := aText.substring(ATextIdx{start}, i{end_}); // skip the $0D/$0A
          aLineManuallyBreaked := True;
          if (aText.codePointAt(i) = $0D) and
             (i + 1 < ATextLn) and
             (aText.codePointAt(i + 1) = $0A) then ATextIdx := ATextIdx + 1; // (MSWINDOWS linebreak = #13#10)
        end
        else begin
          aLine := aText.substring(ATextIdx{start});
          aLineManuallyBreaked := False;
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
        //their is a fucking bug on android 4.4.2 that aNumberOfChars
        //is the number of Glyph and not of char, so ligature like 'fi' are
        //counted like one glyph :(
        //very few comments about this on the internet
        //http://stackoverflow.com/questions/39891726/android-paint-breaktext-not-work-on-kitkat
        if (aNumberOfChars < aLine.length) and
           (TJBuild_VERSION.JavaClass.SDK_INT < 22 {lollipop}) then begin
          while aNumberOfChars < aLine.length  do begin
            aTmpMeasuredWidth := aPaint.measureText(aLine{text},
                                                    0,
                                                    aNumberOfChars + 1);  // measureText seam to be not soo much accurate as breakText unfortunatly (round up)
            if compareValue(aTmpMeasuredWidth, aMaxWidth - aLineIndent, TEpsilon.Position) > 0 then break
            else begin
              inc(aNumberOfChars);
              ameasuredWidth[0] := aTmpMeasuredWidth;
            end;
          end;
        end;

        //the text was breaked
        if (aNumberOfChars < aLine.length)
           or
           ((aLineManuallyBreaked) and
            (not aWordWrap) and
            (aTrimming <> TTextTrimming.None))
           or
           ((aLineManuallyBreaked) and
            (aWordWrap) and
            (compareValue(aCurrLineY + aLineHeight + aMetrics.descent, aMaxHeight, TEpsilon.position) > 0) and
            (aTrimming <> TTextTrimming.None)) then begin

          //init result
          result := True;

          //if not aWordWrap
          if not aWordWrap then begin
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
                                           if (aNumberOfChars < aLine.length) and
                                              (TJBuild_VERSION.JavaClass.SDK_INT < 22 {lollipop}) then begin
                                             while aNumberOfChars < aLine.length  do begin
                                               aTmpMeasuredWidth := aPaint.measureText(aLine{text},
                                                                                       0,
                                                                                       aNumberOfChars + 1);  // measureText seam to be not soo much accurate as breakText unfortunatly (round up)
                                               if compareValue(aTmpMeasuredWidth, aMaxWidth - aEllipsisLineLn - aLineIndent, TEpsilon.Position) > 0 then break
                                               else begin
                                                 inc(aNumberOfChars);
                                                 ameasuredWidth[0] := aTmpMeasuredWidth;
                                              end;
                                             end;
                                           end;
                                           if aNumberOfChars >= aLine.length then break;
                                         end;
                                         //-----
                                       end;
              TTextTrimming.Word: begin
                                    //-----
                                    _initEllipsis;
                                    //-----
                                    aSaveNumberOfChars := aNumberOfChars;
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
                                        if (aNumberOfChars < aLine.length) then dec(aNumberOfChars); // (aNumberOfChars < aLine.length) to know that we are not here
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
                                           if (aNumberOfChars < aLine.length) and
                                              (TJBuild_VERSION.JavaClass.SDK_INT < 22 {lollipop}) then begin
                                             while aNumberOfChars < aLine.length  do begin
                                               aTmpMeasuredWidth := aPaint.measureText(aLine{text},
                                                                                       0,
                                                                                       aNumberOfChars + 1);  // measureText seam to be not soo much accurate as breakText unfortunatly (round up)
                                               if compareValue(aTmpMeasuredWidth, aMaxWidth - aEllipsisLineLn - aLineIndent, TEpsilon.Position) > 0 then break
                                               else begin
                                                 inc(aNumberOfChars);
                                                 ameasuredWidth[0] := aTmpMeasuredWidth;
                                              end;
                                             end;
                                           end;
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
                                      if (aNumberOfChars < aLine.length) and
                                         (TJBuild_VERSION.JavaClass.SDK_INT < 22 {lollipop}) then begin
                                        while aNumberOfChars < aLine.length  do begin
                                          aTmpMeasuredWidth := aPaint.measureText(aLine{text},
                                                                                  0,
                                                                                  aNumberOfChars + 1);  // measureText seam to be not soo much accurate as breakText unfortunatly (round up)
                                          if compareValue(aTmpMeasuredWidth, aMaxWidth - aEllipsisLineLn - aLineIndent, TEpsilon.Position) > 0 then break
                                          else begin
                                            inc(aNumberOfChars);
                                            ameasuredWidth[0] := aTmpMeasuredWidth;
                                          end;
                                        end;
                                      end;
                                      if aNumberOfChars >= aLine.length then break
                                      else aSaveNumberOfChars:= aNumberOfChars;
                                      //----
                                    end;
                                  end;
            end;
          end

          //if aWordWrap
          else begin

            //We are at the last line and aTrimming <> TTextTrimming.None
            if (compareValue(aCurrLineY + aLineHeight + aMetrics.descent, aMaxHeight, Tepsilon.position) > 0) and
               (aTrimming <> TTextTrimming.None) then begin

              //-----
              _initEllipsis;
              //-----
              aSaveNumberOfChars := aNumberOfChars;
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
                  if (aNumberOfChars < aLine.length) then dec(aNumberOfChars); // (aNumberOfChars < aLine.length) to know that we are not here
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
                    if (aNumberOfChars < aLine.length) and
                       (TJBuild_VERSION.JavaClass.SDK_INT < 22 {lollipop}) then begin
                      while aNumberOfChars < aLine.length  do begin
                        aTmpMeasuredWidth := aPaint.measureText(aLine{text},
                                                                0,
                                                                aNumberOfChars + 1);  // measureText seam to be not soo much accurate as breakText unfortunatly (round up)
                        if compareValue(aTmpMeasuredWidth, aMaxWidth - aEllipsisLineLn - aLineIndent, TEpsilon.Position) > 0 then break
                        else begin
                          inc(aNumberOfChars);
                          ameasuredWidth[0] := aTmpMeasuredWidth;
                        end;
                      end;
                    end;
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
                if (aNumberOfChars < aLine.length) and
                   (TJBuild_VERSION.JavaClass.SDK_INT < 22 {lollipop}) then begin
                  while aNumberOfChars < aLine.length  do begin
                    aTmpMeasuredWidth := aPaint.measureText(aLine{text},
                                                            0,
                                                            aNumberOfChars + 1);  // measureText seam to be not soo much accurate as breakText unfortunatly (round up)
                    if compareValue(aTmpMeasuredWidth, aMaxWidth - aEllipsisLineLn - aLineIndent, TEpsilon.Position) > 0 then break
                    else begin
                      inc(aNumberOfChars);
                      ameasuredWidth[0] := aTmpMeasuredWidth;
                    end;
                  end;
                end;
                if aNumberOfChars >= aLine.length then break
                else aSaveNumberOfChars:= aNumberOfChars;
                //----
              end;

            end

            //We are not at the last line or aTrimming = TTextTrimming.None
            else begin

              aSaveNumberOfChars := aNumberOfChars;
              inc(aNumberOfChars); // in case the space separator is just after aNumberOfChars
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
                    if (aNumberOfChars < aLine.length) and
                       (TJBuild_VERSION.JavaClass.SDK_INT < 22 {lollipop}) then begin
                      while aNumberOfChars < aLine.length  do begin
                        aTmpMeasuredWidth := aPaint.measureText(aLine{text},
                                                                0,
                                                                aNumberOfChars + 1);  // measureText seam to be not soo much accurate as breakText unfortunatly (round up)
                        if compareValue(aTmpMeasuredWidth, aMaxWidth - aLineIndent, TEpsilon.Position) > 0 then break
                        else begin
                          inc(aNumberOfChars);
                          ameasuredWidth[0] := aTmpMeasuredWidth;
                        end;
                      end;
                    end;
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
                if (aNumberOfChars < aLine.length) and
                   (TJBuild_VERSION.JavaClass.SDK_INT < 22 {lollipop}) then begin
                  while aNumberOfChars < aLine.length  do begin
                    aTmpMeasuredWidth := aPaint.measureText(aLine{text},
                                                            0,
                                                            aNumberOfChars + 1);  // measureText seam to be not soo much accurate as breakText unfortunatly (round up)
                    if compareValue(aTmpMeasuredWidth, aMaxWidth - aLineIndent, TEpsilon.Position) > 0 then break
                    else begin
                      inc(aNumberOfChars);
                      ameasuredWidth[0] := aTmpMeasuredWidth;
                    end;
                  end;
                end;
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
        if aNumberOfChars <= 0 then begin
          ATextIdx := ATextIdx + 1; // skip the current char or skip the #10
          aCurrLineY := aCurrLineY + aLineHeight; // go to next line
          aLineIndent := 0;
          if (not aWordWrap) or
             (compareValue(aCurrLineY + aMetrics.descent, aMaxHeight, TEpsilon.position) > 0) then break
          else continue;
        end
        else begin
          aTextIdx := ATextIdx + aNumberOfChars;
          if aLineManuallyBreaked then ATextIdx := ATextIdx + 1; // skip the #10
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

        //update aLineIndent
        aLineIndent := 0;

        // stop if not aWordWrap or after maxheight
        if (not aWordWrap) or
           (compareValue(aCurrLineY + aMetrics.descent, aMaxHeight, TEpsilon.position) > 0) then break;

      end;

    finally
      ALFreeAndNil(ameasuredWidth);
    end;

  end
  else result := true;

  //add the end ellipsis
  if aEllipsisLine <> nil then begin
    aBreakTextItem := TalBreakTextItem.Create;
    try
      aBreakTextItem.line := aEllipsisLine;
      aBreakTextItem.pos := aEllipsisLinePos;
      aBreakTextItem.rect := aEllipsisLineRect;
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
                            for I := aBreakTextItemsStartCount to aBreakTextItems.Count - 1 do
                              aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - aOffset;
                          end;
       TTextAlign.Leading: begin
                             aRect.Right := min(aRect.Right, aRect.Left + aMaxLineWidth);
                           end;
       TTextAlign.Trailing: begin
                              aOffset := Floor(aRect.Right - aMaxLineWidth - arect.Left); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                              aRect.Left := aRect.Left + aOffset;
                              for I := aBreakTextItemsStartCount to aBreakTextItems.Count - 1 do
                                aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - aOffset;
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
constructor TALBreakTextItem.Create;
begin
  inherited;
  Line := nil;
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
function ALBreakText(var ARect: TRectF;
                     const aTextAttr: CFAttributedStringRef;
                     const aWordWrap: Boolean;
                     const AHTextAlign, AVTextAlign: TTextAlign;
                     const aTrimming: TTextTrimming; // TTextTrimming.word not yet supported - TTextTrimming.character will be used instead (if someone need, it's not really hard to implement)
                     var aBreakTextItems: TALBreakTextItems;
                     const aFirstLineIndent: single = 0; // kCTParagraphStyleSpecifierFirstLineHeadIndent must also have been set with aFirstLineIndent in aTextAttr
                     const aLineSpacing: integer = 0; // kCTParagraphStyleSpecifierLineSpacingAdjustment must also have been set with aLineSpacing in aTextAttr
                     const aEllipsisAttr: CFAttributedStringRef = nil): boolean; // return true if text was truncated

var aBreakTextItem: TALBreakTextItem;
    aBreakTextItemsStartCount: integer;
    aEllipsisLine: CtLineRef;
    aTextAttrLn: CFIndex;
    aTmpTextAttr: CFAttributedStringRef;
    aFramePath: CGMutablePathRef;
    aFrameSetter: CTFramesetterRef;
    aFrame: CTFrameRef;
    alines: CFArrayRef;
    aline: CTLineRef;
    aTruncatedLine: CTLineRef;
    aLinesCount: CFIndex;
    aMaxWidth: single;
    aMaxHeight: single;
    aPrevMaxLineWidth: Single;
    aMaxLineWidth: single;
    aPrevCurrLineY: single;
    aCurrLineY: single;
    aTotalLinesHeight: single;
    aAscent, aDescent: CGFloat;
    aMeasuredWidth: Double;
    aGlyphMeasuredWidth: Double;
    aLineIndent: single;
    aOffset: single;
    aRuns: CFArrayRef;
    aTruncatedRuns: CFArrayRef;
    aRunsCount: CFIndex;
    aRun: CTRunRef;
    aTruncatedRun: CTRunRef;
    aGlyphCount: CFIndex;
    aStringRange: CFRange;
    aStringIndex: CFIndex;
    aString: CFStringRef;
    aChar: unichar;
    i: CFIndex;

begin

  //init aBreakTextItemsStartCount
  aBreakTextItemsStartCount := aBreakTextItems.Count;

  //init aMaxWidth / aMaxHeight / aMaxLineWidth / aTotalLinesHeight
  if aRect.Width > 65535 then aRect.Width := 65535;
  if aRect.height > 65535 then aRect.height := 65535;
  aMaxWidth := ARect.width;
  aMaxHeight := ARect.Height;
  aPrevMaxLineWidth := 0;
  aMaxLineWidth := 0;
  aTotalLinesHeight := 0;
  aPrevCurrLineY := 0;

  //if aWordWrap
  if aWordWrap then begin

    //Create an immutable path of a rectangle.
    aFramePath := CGPathCreateWithRect(ALLowerLeftCGRect(ARect.TopLeft{aUpperLeftOrigin},
                                                         ARect.Width{aWidth},
                                                         ARect.Height{aHeight},
                                                         ARect.Height{aGridHeight}),
                                                         nil{transform});
    try

      //Creates an immutable framesetter object from an attributed string. The resultant framesetter object can be used to
      //create and fill text frames with the CTFramesetterCreateFrame call.
      aFrameSetter := CTFramesetterCreateWithAttributedString(aTextAttr);
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

          //init aString
          aString := CFAttributedStringGetString(aTextAttr); // Return An immutable string containing the characters from aStr, or NULL if there was a problem creating the object
          if aString = nil then begin ARect.Width := 0; ARect.Height := 0; exit(False); end; // CFAttributedStringGetString can return NULL if there was a problem creating the object

          //init result
          result := aLinesCount > 1;

          //update aBreakTextItems
          for I := 0 to aLinesCount - 1 do begin

            //init aline / aMeasuredWidth
            aline := CFArrayGetValueAtIndex(alines, I);
            aMeasuredWidth := CTLineGetTypographicBounds(aline, // line: The line whose typographic bounds are calculated.
                                                         @aAscent, // ascent: On output, the ascent of the line. This parameter can be set to NULL if not needed.
                                                         @aDescent, // descent: On output, the descent of the line. This parameter can be set to NULL if not needed.
                                                         nil); // leading: On output, the leading of the line. This parameter can be set to NULL if not needed. (it's look like to be always 0)
                                                               // >> return the typographic width of the line. If the line is invalid, this function returns 0.

            //update aCurrLineY / aLineIndent
            if i = 0 then begin
              aCurrLineY := aAscent;
              aLineIndent := aFirstLineIndent;
            end
            else begin
              aPrevCurrLineY := aTotalLinesHeight + aLineSpacing;
              aCurrLineY := aTotalLinesHeight + aLineSpacing + aAscent;
              aLineIndent := 0;
            end;

            // stop if after maxheight
            if (compareValue(aCurrLineY + adescent, aMaxHeight, TEpsilon.position) > 0) then begin
              result := True;
              break;
            end;

            // update aTotalLinesHeight
            aTotalLinesHeight := aCurrLineY + aDescent;

            //unfortunatly the lines that are wrapped are not trimed with the last space
            //so aMeasuredWidth is inacurate. so i must calculate the with of the last char
            //(the space) to remove it from aMeasuredWidth
            aRuns := CTLineGetGlyphRuns(aline); // Returns the array of glyph runs that make up the line object.
                                                // Each CTLine object contains an array of glyph run (CTRun) objects. A glyph run is a
                                                // set of consecutive glyphs that share the same attributes and direction. The typesetter
                                                // creates glyph runs as it produces lines from character strings, attributes, and font objects.
                                                // This means that a line is constructed of one or more glyphs runs. Glyph runs can draw
                                                // themselves into a graphic context, if desired, although most clients have no need to
                                                // interact directly with glyph runs.
            aRunsCount := CFArrayGetCount(aRuns);
            While aRunsCount > 0 do begin
              //-----
              aRun := CFArrayGetValueAtIndex(aRuns, aRunsCount-1);
              aStringRange := CTRunGetStringRange(aRun); // return The range of characters that originally spawned the glyphs, of if run is invalid, an empty range.
                                                         // ex for the string
                                                         // azerty_
                                                         // ^
                                                         // location=0 & length=7
              if aStringRange.length <= 0 then begin // if their is no char in this run (it's possible??) then skip this run
                dec(aRunsCount);
                continue;
              end;
              //-----
              aStringIndex := aStringRange.location + aStringRange.length; // azerty_
                                                                           // 01234567
                                                                           // ^      ^
                                                                           // ^      aStringIndex=7
                                                                           // location=0
              while aStringIndex > aStringRange.location do begin
                aChar := CFStringGetCharacterAtIndex(aString, aStringIndex - 1); // azerty_
                                                                                 // 01234567
                                                                                 //       ^^
                                                                                 //       aStringIndex=6

                if achar.IsWhiteSpace and (Achar.ToUCS4Char <> $00A0{No-break Space}) then dec(aStringIndex)
                else break;
              end;
              if aStringIndex >= aStringRange.location + aStringRange.length then break; // no space was detected then stop the loop
              //-----
              aGlyphCount := CTRunGetGlyphCount(aRun); // return The number of glyphs that the run contains, or if there are no glyphs in this run, a value of 0.
                                                       // 7
              aGlyphCount := aGlyphCount - ((aStringRange.location + aStringRange.length) - aStringIndex); // 7 - (7 - 6) = 6
              if (aGlyphCount >= 0) then begin
                aGlyphMeasuredWidth := CTRunGetTypographicBounds(aRun, // run: The run for which to calculate the typographic bounds.
                                                                 CFRangeMake(aGlyphCount, 0), // range: The portion of the run to measure. If the length of the range is set to 0,
                                                                                              // then the measure operation continues from the range's start index to the end of the run.
                                                                                              // azerty_
                                                                                              // 01234567
                                                                                              //       ^
                                                                                              //       aGlyphCount
                                                                 nil, // ascent: On output, the ascent of the run. This can be set to NULL if not needed.
                                                                 nil, // descent: On output, the descent of the run. This can be set to NULL if not needed.
                                                                 nil); // leading: On output, the leading of the run. This can be set to NULL if not needed.
                aMeasuredWidth := aMeasuredWidth - aGlyphMeasuredWidth;
              end;
              //-----
              if aStringIndex > aStringRange.location then break // it's mean the current run not contain only space, so no need to continue
              else dec(aRunsCount);
              //-----
            end;

            //calculate aMaxLineWidth
            aPrevMaxLineWidth := aMaxLineWidth;
            aMaxLineWidth := max(aMaxLineWidth, aMeasuredWidth);

            //update aBreakTextItems
            aBreakTextItem := TalBreakTextItem.Create;
            try
              aBreakTextItem.Line := CFRetain(aline); // Retains a Core Foundation object. we need this because we will later free the aFrame but still need to access the Line
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
              aBreakTextItems.Add(aBreakTextItem);
            except
              ALFreeAndNil(aBreakTextItem);
              raise;
            end;

          end;

          //trim the last line
          if (aBreakTextItems.Count > aBreakTextItemsStartCount) and
             (aTrimming <> TTextTrimming.None) and
             (aEllipsisAttr <> nil) then begin

            //init aBreakTextItem
            aBreakTextItem := aBreakTextItems[aBreakTextItems.count - 1];
            aTextAttrLn := CFAttributedStringGetLength(aTextAttr);

            //init aStringRange
            aStringRange := CTLineGetStringRange(aBreakTextItem.Line); // return a CFRange structure that contains the range over the backing store string that spawned the glyphs

            //init result
            result := result or (aStringRange.location + aStringRange.length < aTextAttrLn);

            //if the text was breaked
            if (aStringRange.length > 0) and  // if CTLineGetStringRange fails for any reason, an empty range.
               (aStringRange.location + aStringRange.length < aTextAttrLn) then begin

              //create the aEllipsisLine
              aEllipsisLine := CTLineCreateWithAttributedString(aEllipsisAttr); // Creates a single immutable line object directly from an attributed string.
              if aEllipsisLine <> nil then begin                                // Return Value: A reference to a CTLine object if the call was successful; otherwise, NULL.
                try

                  aStringRange.length := aTextAttrLn - aStringRange.location;
                  aTmpTextAttr := CFAttributedStringCreateWithSubstring(kCFAllocatorDefault, aTextAttr, aStringRange); // return a new attributed string whose string and attributes are copied from the specified range of the
                  if aTmpTextAttr <> nil then begin                                                                    // supplied attributed string. Returns NULL if there was a problem copying the object
                    try

                      aLine := CTLineCreateWithAttributedString(aTmpTextAttr); // A reference to a CTLine object if the call was successful;
                      if aLine <> nil then begin                               // otherwise, NULL.
                        try

                          aTruncatedLine :=  CTLineCreateTruncatedLine(aLine, // line: The line from which to create a truncated line.
                                                                       aMaxWidth, // width: The width at which truncation begins. The line is truncated if its width is greater than the width passed in this parameter.
                                                                       kCTLineTruncationEnd, // truncationType: The type of truncation to perform if needed.
                                                                       aEllipsisLine); // truncationToken: This token is added at the point where truncation took place, to indicate that the line was truncated.
                                                                                       //                  Usually, the truncation token is the ellipsis character (U+2026). If this parameter is set to NULL, then no
                                                                                       //                  truncation token is used and the line is simply cut off.
                          if aTruncatedLine <> nil then begin // CTLineCreateTruncatedLine return A reference to a truncated CTLine object if the call was successful; otherwise, NULL.

                            //init aMeasuredWidth
                            aMeasuredWidth := CTLineGetTypographicBounds(aTruncatedLine, // line: The line whose typographic bounds are calculated.
                                                                         @aAscent, // ascent: On output, the ascent of the line. This parameter can be set to NULL if not needed.
                                                                         @aDescent, // descent: On output, the descent of the line. This parameter can be set to NULL if not needed.
                                                                         nil); // leading: On output, the leading of the line. This parameter can be set to NULL if not needed. (it's look like to be always 0)
                                                                               // >> return the typographic width of the line. If the line is invalid, this function returns 0.

                            //calculate aMaxLineWidth
                            aMaxLineWidth := max(aPrevMaxLineWidth, aMeasuredWidth);
                            aCurrLineY := aPrevCurrLineY + aAscent;
                            aTotalLinesHeight := aCurrLineY + aDescent;
                            if aBreakTextItems.Count = aBreakTextItemsStartCount + 1 then aLineIndent := aFirstLineIndent
                            else aLineIndent := 0;

                            //update aBreakTextItems
                            cfRelease(aBreakTextItem.Line);
                            aBreakTextItem.Line := aTruncatedLine;
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

                          end;

                        finally
                          cfRelease(aLine);
                        end;
                      end;

                    finally
                      cfRelease(aTmpTextAttr);
                    end;
                  end;

                finally
                  cfRelease(aEllipsisLine)
                end;
              end;

            end;

          end
          else if (not result) and
                  (aBreakTextItems.Count > aBreakTextItemsStartCount) then begin

            //init aBreakTextItem
            aBreakTextItem := aBreakTextItems[aBreakTextItems.count - 1];
            aTextAttrLn := CFAttributedStringGetLength(aTextAttr);

            //init aStringRange
            aStringRange := CTLineGetStringRange(aBreakTextItem.Line);
            result := (aStringRange.location + aStringRange.length < aTextAttrLn);

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

  end

  //not aWordWrap
  else begin

    //create the aEllipsisLine
    if (aTrimming <> TTextTrimming.None) and
       (aEllipsisAttr <> nil) then aEllipsisLine := CTLineCreateWithAttributedString(aEllipsisAttr) // Creates a single immutable line object directly from an attributed string.
    else aEllipsisLine := nil;                                                                      // Return Value: A reference to a CTLine object if the call was successful; otherwise, NULL.
    try

      aLine := CTLineCreateWithAttributedString(aTextAttr);                             // A reference to a CTLine object if the call was successful;
      if aLine = nil then begin ARect.Width := 0; ARect.Height := 0; exit(False); end;  // otherwise, NULL.
      try

        //init aTruncatedLine
        aTruncatedLine :=  CTLineCreateTruncatedLine(aLine, // line: The line from which to create a truncated line.
                                                     aMaxWidth, // width: The width at which truncation begins. The line is truncated if its width is greater than the width passed in this parameter.
                                                     kCTLineTruncationEnd, // truncationType: The type of truncation to perform if needed.
                                                     aEllipsisLine); // truncationToken: This token is added at the point where truncation took place, to indicate that the line was truncated.
                                                                     //                  Usually, the truncation token is the ellipsis character (U+2026). If this parameter is set to NULL, then no
                                                                     //                  truncation token is used and the line is simply cut off.
        if aTruncatedLine = nil then begin ARect.Width := 0; ARect.Height := 0; exit(True); end; // CTLineCreateTruncatedLine return A reference to a truncated CTLine object if the call was successful; otherwise, NULL.

        //init result - i didn't find a better way to do this !
        aRuns := CTLineGetGlyphRuns(aLine); // Returns the array of glyph runs that make up the line object.
                                            // Each CTLine object contains an array of glyph run (CTRun) objects. A glyph run is a
                                            // set of consecutive glyphs that share the same attributes and direction. The typesetter
                                            // creates glyph runs as it produces lines from character strings, attributes, and font objects.
                                            // This means that a line is constructed of one or more glyphs runs. Glyph runs can draw
                                            // themselves into a graphic context, if desired, although most clients have no need to
                                            // interact directly with glyph runs.
        aRunsCount := CFArrayGetCount(aRuns);
        aTruncatedRuns := CTLineGetGlyphRuns(aTruncatedline);
        result := aRunsCount <> CFArrayGetCount(aTruncatedRuns); // aTruncatedRuns[0] contain all the characters
                                                                 // aTruncatedRuns[1] contain only one character, the last '...' (if the line was truncated)
        if (not result) then begin
          //their is also the case where the aTruncatedline contain ONLY aEllipsisLine ...
          //in this way CFArrayGetCount(aRuns) = CFArrayGetCount(aTruncatedRuns) :(
          //funcking ios i didn't find a good way to compare the char in the run
          for i := aRunsCount - 1 downto 0 do begin
            aRun := CFArrayGetValueAtIndex(aRuns, i);
            aTruncatedRun := CFArrayGetValueAtIndex(aTruncatedRuns, i);
            result := CTRunGetGlyphCount(aRun) <> CTRunGetGlyphCount(aTruncatedRun); // off course if aLine contain the same number of char than in aEllipsis then we are fucked
            if result then break;
          end;
        end;

        //init aMeasuredWidth
        aMeasuredWidth := CTLineGetTypographicBounds(aTruncatedLine, // line: The line whose typographic bounds are calculated.
                                                     @aAscent, // ascent: On output, the ascent of the line. This parameter can be set to NULL if not needed.
                                                     @aDescent, // descent: On output, the descent of the line. This parameter can be set to NULL if not needed.
                                                     nil); // leading: On output, the leading of the line. This parameter can be set to NULL if not needed. (it's look like to be always 0)
                                                           // >> return the typographic width of the line. If the line is invalid, this function returns 0.

        //calculate aMaxLineWidth
        aMaxLineWidth := aMeasuredWidth;
        aCurrLineY := aAscent;
        aTotalLinesHeight := aCurrLineY + aDescent;
        aLineIndent := aFirstLineIndent;

        //update aBreakTextItems
        aBreakTextItem := TalBreakTextItem.Create;
        try
          aBreakTextItem.Line := aTruncatedLine;
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
          aBreakTextItems.Add(aBreakTextItem);
        except
          ALFreeAndNil(aBreakTextItem);
          raise;
        end;

      finally
        cfRelease(aLine);
      end;

    finally
      if aEllipsisLine <> nil then cfRelease(aEllipsisLine)
    end;

  end;

  //initialise ARect
  if compareValue(aMaxLineWidth, aMaxWidth, Tepsilon.Position) < 0 then begin
    case AHTextAlign of
       TTextAlign.Center: begin
                            aOffset := Floor((aRect.Right - aMaxLineWidth - arect.Left) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                            aRect.Left := aRect.Left + aOffset;
                            aRect.right := aRect.right - aOffset;
                            for I := aBreakTextItemsStartCount to aBreakTextItems.Count - 1 do
                              aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - aOffset;
                          end;
       TTextAlign.Leading: begin
                             aRect.Right := min(aRect.Right, aRect.Left + aMaxLineWidth);
                           end;
       TTextAlign.Trailing: begin
                              aOffset := Floor(aRect.Right - aMaxLineWidth - arect.Left); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                              aRect.Left := aRect.Left + aOffset;
                              for I := aBreakTextItemsStartCount to aBreakTextItems.Count - 1 do
                                aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - aOffset;
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
                     const aTrimming: TTextTrimming;
                     var aBreakTextItems: TALBreakTextItems;
                     const aFirstLineIndent: single = 0;
                     const aLineSpacing: integer = 0;
                     const aEllipsisText: string = '…';
                     const aEllipsisColor: TalphaColor = TAlphaColorRec.Null): boolean; // return true if text was truncated

var aCGColor: CGColorRef;
    aCGEllipsisColor: CGColorRef;
    aFont: CTFontRef;
    aTextString: CFStringRef;
    aTextAttr: CFMutableAttributedStringRef;
    aEllipsisString: CFStringRef;
    aEllipsisAttr: CFMutableAttributedStringRef;
    aAlphaColor: TAlphaColorCGFloat;
    aSettings: array of CTParagraphStyleSetting;
    aParagraphStyle: CTParagraphStyleRef;
    aCGFloatValue: CGFloat;
    aByteValue: byte;

begin

  aAlphaColor := TAlphaColorCGFloat.Create(aFontColor);
  aCGColor := CGColorCreate(aColorSpace, @aAlphaColor);
  try

    aFont := ALGetCTFontRef(aFontName, aFontSize, aFontStyle); // Returns a new font reference for the given name.
    if aFont = nil then begin ARect.Width := 0; ARect.Height := 0; exit(False); end;
    try

      aTextString := CFStringCreateWithCharacters(kCFAllocatorDefault, @AText[Low(string)], Length(AText));
      if aTextString = nil then begin ARect.Width := 0; ARect.Height := 0; exit(False); end;
      try

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
            if aWordWrap and (compareValue(aFirstLineIndent, 0, TEpsilon.position) > 0) then begin
              SetLength(aSettings, length(aSettings) + 1);
              aCGFloatValue := aFirstLineIndent;
              aSettings[high(aSettings)].spec := kCTParagraphStyleSpecifierFirstLineHeadIndent;
              aSettings[high(aSettings)].valueSize := SizeOf(aCGFloatValue);
              aSettings[high(aSettings)].value := @aCGFloatValue;
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
            if aWordWrap and (compareValue(aLineSpacing, 0, TEpsilon.position) > 0) then begin
              SetLength(aSettings, length(aSettings) + 1);
              aCGFloatValue := aLineSpacing;
              aSettings[high(aSettings)].spec := kCTParagraphStyleSpecifierLineSpacingAdjustment;
              aSettings[high(aSettings)].valueSize := SizeOf(aCGFloatValue);
              aSettings[high(aSettings)].value := @aCGFloatValue;
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
            //-----
          finally
            CFAttributedStringEndEditing(aTextAttr); // Re-enables internal consistency-checking and coalescing for a mutable attributed string.
          end;

          if (aEllipsisText <> '') and (aTrimming <> TTextTrimming.None) then begin

            if aEllipsisColor <> TAlphaColorRec.Null then aAlphaColor := TAlphaColorCGFloat.Create(aEllipsisColor)
            else aAlphaColor := TAlphaColorCGFloat.Create(aFontColor);
            aCGEllipsisColor := CGColorCreate(aColorSpace, @aAlphaColor);
            try

              aEllipsisString := CFStringCreateWithCharacters(kCFAllocatorDefault, @aEllipsisText[Low(string)], Length(aEllipsisText));
              if aEllipsisString = nil then begin ARect.Width := 0; ARect.Height := 0; exit(False); end;
              try

                aEllipsisAttr := CFAttributedStringCreateMutable(kCFAllocatorDefault{alloc}, 0{maxLength}); // Creates a mutable attributed string.
                try

                  CFAttributedStringReplaceString(aEllipsisAttr, CFRangeMake(0, 0), aEllipsisString); // Modifies the string of an attributed string.
                  CFAttributedStringBeginEditing(aEllipsisAttr); // Defers internal consistency-checking and coalescing for a mutable attributed string.
                  try
                    CFAttributedStringSetAttribute(aEllipsisAttr, CFRangeMake(0, CFStringGetLength(aEllipsisString)), kCTFontAttributeName, aFont);
                    CFAttributedStringSetAttribute(aEllipsisAttr, CFRangeMake(0, CFStringGetLength(aEllipsisString)), kCTForegroundColorAttributeName, aCGEllipsisColor);
                  finally
                    CFAttributedStringEndEditing(aEllipsisAttr); // Re-enables internal consistency-checking and coalescing for a mutable attributed string.
                  end;

                  result := ALBreakText(ARect,
                                        CFAttributedStringRef(aTextAttr),
                                        aWordWrap,
                                        AHTextAlign, AVTextAlign,
                                        aTrimming,
                                        aBreakTextItems,
                                        aFirstLineIndent,
                                        aLineSpacing,
                                        CFAttributedStringRef(aEllipsisAttr));

                finally
                  CFRelease(aEllipsisAttr);
                end;

              finally
                CFRelease(aEllipsisString);
              end;

            finally
              CGColorRelease(aCGEllipsisColor);
            end;

          end
          else begin

            result := ALBreakText(ARect,
                                  CFAttributedStringRef(aTextAttr),
                                  aWordWrap,
                                  AHTextAlign, AVTextAlign,
                                  aTrimming,
                                  aBreakTextItems,
                                  aFirstLineIndent,
                                  aLineSpacing,
                                  nil);

          end;

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

end;
{$ENDIF}

initialization
  ALCustomConvertFontFamilyProc := nil;

end.
