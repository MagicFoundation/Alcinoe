unit ALFmxImgList;

interface

uses System.Classes,
     System.UITypes,
     System.Types,
     {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     FMX.types3D,
     {$ENDIF}
     FMX.graphics,
     FMX.imgList,
     FMX.objects;

type

  {~~~~~~~~~~~~~~~~~~~~~~}
  TALGlyph = class(TGlyph)
  private
    fdoubleBuffered: boolean;
    fBufSize: TsizeF;
    fBufImages: TCustomImageList;
    FbufImageIndex: TImageIndex;
    fBufStretch: boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
  protected
    procedure Paint; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read fBufBitmap ;
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
    property doubleBuffered: Boolean read fdoubleBuffered write fdoubleBuffered default false;
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

{**********************************************}
constructor TALGlyph.Create(AOwner: TComponent);
begin
  fdoubleBuffered := false;
  fBufBitmap := nil;
  inherited;
end;

{**************************}
destructor TALGlyph.Destroy;
begin
  clearBufBitmap;
  inherited;
end;

{********************************}
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
var {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    TmpBitmap: Tbitmap;
    M: TBitmapData;
    {$ENDIF}
    SaveTempCanvas: TCanvas;
    SceneScale: Single;
begin

  if (csDesigning in ComponentState) or
     (not fdoubleBuffered) or
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) then begin
    if fBufBitmap <> nil then begin
      fBufBitmap.Free;
      fBufBitmap := nil;
    end;
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) and
     (fBufImages = Images) and
     (FbufImageIndex = ImageIndex) and
     (fBufStretch = stretch) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;
  fBufImages := Images;
  FbufImageIndex := ImageIndex;
  fBufStretch := stretch;

  if Scene <> nil then SceneScale := Scene.GetSceneScale
  else SceneScale := 1;

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}

  TmpBitmap := Tbitmap.Create(round(Width * SceneScale), round(height * SceneScale));
  try
    TmpBitmap.BitmapScale := SceneScale;
    TmpBitmap.Clear(0);
    if TmpBitmap.Canvas.BeginScene then
    try
      SaveTempCanvas := TempCanvas;
      try
        TempCanvas := TmpBitmap.Canvas;
        inherited paint;
      finally
        TempCanvas := SaveTempCanvas;
      end;
    finally
      TmpBitmap.Canvas.EndScene;
    end;
    //-----
    fBufBitmap := TTexture.Create;
    fBufBitmap.Assign(TmpBitmap);
    if (TCanvasStyle.NeedGPUSurface in TmpBitmap.CanvasClass.GetCanvasStyle) then begin
      if TmpBitmap.Map(TMapAccess.Read, M) then begin
        try
          fBufBitmap.UpdateTexture(M.Data, M.Pitch);
        finally
          TmpBitmap.Unmap(M);
        end;
      end;
    end;
  finally
    TmpBitmap.Free;
  end;

  {$ELSE}

  fBufBitmap := Tbitmap.Create(round(Width * SceneScale), round(height * SceneScale));
  fBufBitmap.BitmapScale := SceneScale;
  fBufBitmap.Clear(0);
  if fBufBitmap.Canvas.BeginScene then
  try
    SaveTempCanvas := TempCanvas;
    try
      TempCanvas := fBufBitmap.Canvas;
      inherited paint;
    finally
      TempCanvas := SaveTempCanvas;
    end;
  finally
    fBufBitmap.Canvas.EndScene;
  end;

  {$ENDIF}

  result := fBufBitmap;

end;

{***********************}
procedure TALGlyph.Paint;
begin

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    inherited paint;
    exit;
  end;

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}

  TCustomCanvasGpu(Canvas).DrawTexture(TRectF.Create(0, 0, Width, Height), // ATexRect (destRec)
                                       TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                       $FFFFFFFF, // AColor (not used in TCanvasGPU)
                                       fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(fBufBitmap,
                    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                    TRectF.Create(0, 0, Width, Height), {DestRect}
                    opacity, {opacity}
                    true{highSpeed});

  {$ENDIF}

end;

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALGlyph]);
end;

initialization
  RegisterFmxClasses([TALGlyph]);

end.
