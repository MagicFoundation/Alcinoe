unit Alcinoe.FMX.Common;

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
  iOSapi.CoreText,
  {$ENDIF}
  {$IF defined(ANDROID)}
  Androidapi.JNI.JavaTypes,
  Alcinoe.AndroidApi.Common,
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
    fColor: TAlphaColor;
    FOnChanged: TNotifyEvent;
    procedure SetEnabled(const Value: boolean);
    procedure setblur(const Value: Single);
    procedure setOffsetX(const Value: Single);
    procedure setOffsetY(const Value: Single);
    procedure setColor(const Value: TAlphaColor);
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
    property Color: TAlphaColor read fColor write setColor default $96000000;
  end;

type

  TALCustomConvertFontFamilyProc = function(const AFamily: TFontName): TFontName;

var

  ALCustomConvertFontFamilyProc: TALCustomConvertFontFamilyProc;

{*****************************************************************}
function  ALConvertFontFamily(const AFamily: TFontName): TFontName;
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

  {$IFNDEF ALCompilerVersionSupported}
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

  {$IFNDEF ALCompilerVersionSupported}
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
  System.Math,
  {$IF defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.Helpers,
  FMX.forms,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.ObjectiveC,
  Macapi.CoreFoundation,
  Macapi.Helpers,
  {$ENDIF}
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.StdCtrls;

{***************************}
constructor TALShadow.Create;
begin
  inherited Create;
  fenabled := False;
  fblur := 12;
  fOffsetX := 0;
  fOffsetY := 0;
  fColor := $96000000;
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
    fColor := TALShadow(Source).fColor;
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

{*****************************************************}
procedure TALShadow.setColor(const Value: TAlphaColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end;
end;

{****************************************************************}
function ALConvertFontFamily(const AFamily: TFontName): TFontName;
begin
  if AFamily = '' then Exit('');
  if Assigned(ALCustomConvertFontFamilyProc) then begin
    Result := ALCustomConvertFontFamilyProc(AFamily);
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

  // This is used to generate child controls.
  // For instance, TALText can be used for the TLabel.
  //if aControl is TPresentedControl then TPresentedControl(aControl).ApplyStyleLookup;

  // This ensures the style is retained when the control exits the visible area.
  // Otherwise, the style will be released and reapplied shortly after.
  acontrol.DisableDisappear := true;

  if (aControl is TALText) then TALText(aControl).MakeBufBitmap
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
{$IFNDEF ALCompilerVersionSupported}
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

initialization
  ALCustomConvertFontFamilyProc := nil;
  {$IFDEF ANDROID}
  ALViewStackCount := 0;
  _RenderScript := nil;
  {$ENDIF}

end.
