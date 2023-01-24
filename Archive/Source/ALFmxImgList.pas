unit ALFmxImgList;

interface

uses System.Classes,
     System.UITypes,
     System.Types,
     {$IFDEF DEBUG}
     System.Diagnostics,
     {$ENDIF}
     {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     system.Messaging,
     FMX.types3D,
     {$ENDIF}
     FMX.graphics,
     FMX.imgList;

type

  {~~~~~~~~~~~~~~~~~~~~~~}
  TALGlyph = class(TGlyph) // << todo: delete
  private
    FScreenScale: single;
    fdoubleBuffered: boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    [weak] fBufImages: TCustomImageList;
    FbufImageIndex: TImageIndex;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    FOpenGLContextLostId: integer;
    FOpenGLContextResetId: Integer;
    procedure OpenGLContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure OpenGLContextResetHandler(const Sender : TObject; const Msg : TMessage); // << because of https://quality.embarcadero.com/browse/RSP-16142
    {$ENDIF}
    procedure SetdoubleBuffered(const Value: Boolean);
  protected
    procedure Paint; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
  published
    property Cursor default crDefault;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default true;
    property TouchTargetExpansion;
    property HitTest default False;
    property AutoHide default False; // i think it's better to put autohide to false by default !
                                     // because autohide deactivate the normal behavior of the visible property
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
  end;

{$IFDEF debug}
var
  AlDebugGlyphMakeBufBitmapCount: integer;
  AlDebugGlyphMakeBufBitmapStopWatch: TstopWatch;
{$endif}

procedure Register;

implementation

uses system.Math,
     system.Math.Vectors,
     fmx.types,
     fmx.consts,
     fmx.platform,
     {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     FMX.Canvas.GPU,
     Alcinoe.FMX.Types3D,
     {$ENDIF}
     fmx.controls,
     Alcinoe.Common,
     Alcinoe.FMX.Common;

{**********************************************}
constructor TALGlyph.Create(AOwner: TComponent);
var aScreenSrv: IFMXScreenService;
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, aScreenSrv) then FScreenScale := aScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fdoubleBuffered := true;
  fBufBitmap := nil;
  HitTest := False;
  Autohide := False;
  {$IF defined(ANDROID) or defined(IOS)}
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);
  {$ENDIF}
end;

{***************************}
destructor TALGlyph.Destroy;
begin
  clearBufBitmap;
  {$IF defined(ANDROID) or defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FOpenGLContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FOpenGLContextResetId);
  {$ENDIF}
  inherited;
end;

{*********************************}
procedure TALGlyph.clearBufBitmap;
begin
  ALFreeAndNil(fBufBitmap);
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALGlyph.MakeBufBitmap: TTexture;
{$ELSE}
function TALGlyph.MakeBufBitmap: Tbitmap;
{$ENDIF}

{$IF defined(ANDROID) or defined(IOS)}
var aBitmap: TBitmap;
    aBitmapSize: TSize;
{$ENDIF}

begin

  if ([csLoading, csDestroying, csDesigning] * ComponentState <> []) or
     (not fdoubleBuffered) or
     (Scene = nil) or
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) or
     (Images = nil) or
     (ImageIndex = -1) then begin
    clearBufBitmap;
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) and
     (fBufImages = Images) and
     (FbufImageIndex = ImageIndex) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;
  fBufImages := Images;
  FbufImageIndex := ImageIndex;

  {$IFDEF debug}
  ALLog('TALGlyph.MakeBufBitmap', 'TALGlyph.MakeBufBitmap', TalLogType.verbose);
  inc(AlDebugGlyphMakeBufBitmapCount);
  AlDebugGlyphMakeBufBitmapStopWatch.Start;
  try
  {$endif}

  {$IF defined(ANDROID) or defined(IOS)}

  //init aBitmapSize / aBitmap / fBufBitmapRect
  aBitmapSize := TSize.Create(0, 0);
  aBitmap := nil;
  fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
  if (Images <> nil) and
     (fBufBitmapRect.Width >= 1) and
     (fBufBitmapRect.Height >= 1) and
     (ImageIndex <> -1) and
     ([csLoading, csUpdating, csDestroying] * Images.ComponentState = []) then begin
    aBitmapSize := TSize.Create(Round(fBufBitmapRect.Width * FScreenScale), Round(fBufBitmapRect.Height * FScreenScale));
    if not Stretch then Images.BestSize(ImageIndex, aBitmapSize);
    aBitmap := Images.Bitmap(aBitmapSize, ImageIndex)
  end;

  if aBitmap <> nil then begin

    //init fBufBitmapRect
    fBufBitmapRect := TRectF.Create(0,
                                    0,
                                    aBitmap.Width / FScreenScale,
                                    aBitmap.Height/ FScreenScale).CenterAt(fBufBitmapRect);

    //convert the aBitmapSurface to texture
    //it's important to make a copy of the aBitmap because it's could be destroyed by the TimageList if
    //their is not anymore enalf of place in it's own caching system
    fBufBitmap := TALTexture.Create(True{aVolatile});
    try
      fBufBitmap.Assign(aBitmap);
    except
      ALFreeAndNil(fBufBitmap);
      raise;
    end;

  end;

  {$ENDIF}

  result := fBufBitmap;

  {$IFDEF debug}
  finally
    AlDebugGlyphMakeBufBitmapStopWatch.Stop;
  end;
  {$endif}

end;

{************************}
procedure TALGlyph.Paint;
begin

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    inherited paint;
    exit;
  end;

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}

  TCustomCanvasGpu(Canvas).DrawTexture(canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
                                       TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                       ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
                                       fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(fBufBitmap,
                    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                    canvas.AlignToPixel(fBufBitmapRect), {DestRect}
                    AbsoluteOpacity, {opacity}
                    true{highSpeed});

  {$ENDIF}

end;

{**********************************************************}
procedure TALGlyph.SetdoubleBuffered(const Value: Boolean);
begin
  if Value <> fDoubleBuffered then begin
    fDoubleBuffered := value;
    if not fDoubleBuffered then clearbufBitmap;
  end;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TALGlyph.OpenGLContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TALGlyph.OpenGLContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALGlyph]);
end;

initialization
  RegisterFmxClasses([TALGlyph]);
  {$IFDEF debug}
  AlDebugGlyphMakeBufBitmapStopWatch := TstopWatch.Create;
  {$endif}

end.
