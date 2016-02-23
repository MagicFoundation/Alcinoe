unit ALFmxRectangle;

interface

uses System.Classes,
     System.Types,
     {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     FMX.types3D,
     {$ENDIF}
     FMX.graphics,
     FMX.objects;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALRectangle = class(TRectangle)
  private
    fdoubleBuffered: boolean;
    fBufSize: TsizeF;
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
     fmx.consts,
     {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     FMX.Canvas.GPU,
     {$ENDIF}
     fmx.controls;

{**************************************************}
constructor TALRectangle.Create(AOwner: TComponent);
begin
  fdoubleBuffered := false;
  fBufBitmap := nil;
  fBufSize := TsizeF.Create(0,0);
  inherited;
end;

{******************************}
destructor TALRectangle.Destroy;
begin
  clearBufBitmap;
  inherited;
end;

{************************************}
procedure TALRectangle.clearBufBitmap;
begin
  if fBufBitmap <> nil then begin
    fBufBitmap.Free;
    fBufBitmap := nil;
  end;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALRectangle.MakeBufBitmap: TTexture;
{$ELSE}
function TALRectangle.MakeBufBitmap: Tbitmap;
{$ENDIF}
var {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    TmpBitmap: Tbitmap;
    M: TBitmapData;
    {$ENDIF}
    SaveTempCanvas: TCanvas;
    SceneScale: Single;
begin

  if (csDesigning in ComponentState) or (not fdoubleBuffered) then begin
    if fBufBitmap <> nil then begin
      fBufBitmap.Free;
      fBufBitmap := nil;
    end;
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;

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

{***************************}
procedure TALRectangle.Paint;
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
  RegisterComponents('Alcinoe', [TALRectangle]);
end;

end.
