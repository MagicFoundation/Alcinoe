unit ALFmxImgList;

interface

uses System.Classes,
     System.UITypes,
     System.Types,
     {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     FMX.types3D,
     {$ENDIF}
     FMX.graphics,
     FMX.imgList;

type

  {~~~~~~~~~~~~~~~~~~~~~~}
  TALGlyph = class(TGlyph)
  private
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
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default false;
  end;

procedure Register;

implementation

uses system.Math,
     system.Math.Vectors,
     fmx.types,
     fmx.consts,
     {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     FMX.Canvas.GPU,
     {$ENDIF}
     fmx.controls,
     alFmxCommon;

{***********************************************}
constructor TALGlyph.Create(AOwner: TComponent);
begin
  inherited;
  fdoubleBuffered := false;
  fBufBitmap := nil;
end;

{***************************}
destructor TALGlyph.Destroy;
begin
  clearBufBitmap;
  inherited;
end;

{*********************************}
procedure TALGlyph.clearBufBitmap;
begin
  if fBufBitmap <> nil then begin
    fBufBitmap.Free;
    fBufBitmap := nil;
  end;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALGlyph.MakeBufBitmap: TTexture;
{$ELSE}
function TALGlyph.MakeBufBitmap: Tbitmap;
{$ENDIF}

{$IF defined(ANDROID) or defined(IOS)}
var aSceneScale: Single;
    aBitmap: TBitmap;
    aBitmapSize: TSize;
    aBitmapData: TBitmapData;
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

  {$IF defined(ANDROID) or defined(IOS)}

  //init aSceneScale
  if Scene <> nil then aSceneScale := Scene.GetSceneScale
  else aSceneScale := 1;

  //init aBitmapSize / aBitmap / fBufBitmapRect
  aBitmapSize := TSize.Create(0, 0);
  aBitmap := nil;
  fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, aSceneScale); // to have the pixel aligned width and height
  if (Images <> nil) and
     (fBufBitmapRect.Width >= 1) and
     (fBufBitmapRect.Height >= 1) and
     (ImageIndex <> -1) and
     ([csLoading, csUpdating, csDestroying] * Images.ComponentState = []) then begin
    aBitmapSize := TSize.Create(Round(fBufBitmapRect.Width * aSceneScale), Round(fBufBitmapRect.Height * aSceneScale));
    if not Stretch then Images.BestSize(ImageIndex, aBitmapSize);
    aBitmap := Images.Bitmap(aBitmapSize, ImageIndex)
  end;

  if aBitmap <> nil then begin

    //init fBufBitmapRect
    fBufBitmapRect := TRectF.Create(0,
                                    0,
                                    aBitmap.Width / aSceneScale,
                                    aBitmap.Height/ aSceneScale).CenterAt(fBufBitmapRect);

    //convert the aBitmapSurface to texture
    //it's important to make a copy of the aBitmap because it's could be destroyed by the TimageList if
    //their is not anymore enalf of place in it's own caching system
    fBufBitmap := TTexture.Create;
    try
      fBufBitmap.Assign(aBitmap);
      //
      //i don't understand the sheet they do in TTexture.assign! they don't copy the data in the TTexture !
      //
      //procedure TTexture.Assign(Source: TPersistent);
      //begin
      //  ...
      //  if not (TCanvasStyle.NeedGPUSurface in TBitmap(Source).CanvasClass.GetCanvasStyle) then
      //    begin
      //    if TBitmap(Source).Map(TMapAccess.Read, M) then
      //    try
      //      UpdateTexture(M.Data, M.Pitch);
      //    finally
      //      TBitmap(Source).Unmap(M);
      //    end;
      //  end;
      //  ...
      //end;
      //
      {$IF CompilerVersion <> 31}
        {$MESSAGE WARN 'Check if FMX.Types3D.TTexture.assign is still not copying the bitmap data in the TTexture if TCanvasStyle.NeedGPUSurface and adjust the IFDEF'}
      {$ENDIF}
      if (TCanvasStyle.NeedGPUSurface in aBitmap.CanvasClass.GetCanvasStyle) then begin
        if aBitmap.Map(TMapAccess.Read, aBitmapData) then begin
          try
            fBufBitmap.UpdateTexture(aBitmapData.Data, aBitmapData.Pitch);
          finally
            aBitmap.Unmap(aBitmapData);
          end;
        end;
      end;
    except
      fBufBitmap.Free;
      fBufBitmap := nil;
      raise;
    end;

  end;

  {$ENDIF}

  result := fBufBitmap;

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

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALGlyph]);
end;

initialization
  RegisterFmxClasses([TALGlyph]);

end.
