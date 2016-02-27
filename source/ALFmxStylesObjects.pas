unit ALFmxStylesObjects;

interface

uses System.Classes,
     System.UITypes,
     System.Types,
     {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     FMX.types3D,
     {$ENDIF}
     FMX.graphics,
     FMX.imgList,
     FMX.styles.Objects;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALStyleObject = class(TStyleObject)
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCheckStyleObject = class(TCheckStyleObject)
  private
    fdoubleBuffered: boolean;
    fBufSize: TsizeF;
    FBufState: set of TALCheckStyleObject.TState;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALButtonStyleObject = class(TButtonStyleObject)
  private
    fdoubleBuffered: boolean;
    fBufSize: TsizeF;
    FBuffCurrent: TButtonAnimation;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALButtonStyleTextObject = class(TButtonStyleTextObject)
  private
    fdoubleBuffered: boolean;
    fBufSize: TsizeF;
    fBufText: String;
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

implementation

uses system.SysUtils,
     system.Math,
     system.Math.Vectors,
     fmx.types,
     fmx.consts,
     {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     FMX.Canvas.GPU,
     {$ENDIF}
     fmx.controls;

{****************************************************}
constructor TALStyleObject.Create(AOwner: TComponent);
begin
  fdoubleBuffered := false;
  fBufBitmap := nil;
  inherited;
end;

{********************************}
destructor TALStyleObject.Destroy;
begin
  clearBufBitmap;
  inherited;
end;

{**************************************}
procedure TALStyleObject.clearBufBitmap;
begin
  if fBufBitmap <> nil then begin
    fBufBitmap.Free;
    fBufBitmap := nil;
  end;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALStyleObject.MakeBufBitmap: TTexture;
{$ELSE}
function TALStyleObject.MakeBufBitmap: Tbitmap;
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
     (Scene = nil) or
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

{*****************************}
procedure TALStyleObject.Paint;
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

Type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCheckStyleObjectState = set of TALCheckStyleObject.TState;
  TALTabStyleObjectHelper = class helper for TTabStyleObject
   function _fstate: TALCheckStyleObjectState;
  end;

{*****************************************************************}
function TALTabStyleObjectHelper._fstate: TALCheckStyleObjectState;
begin
  result := TTabStyleObject(self).fstate;
end;

{*********************************************************}
constructor TALCheckStyleObject.Create(AOwner: TComponent);
begin
  fdoubleBuffered := false;
  fBufBitmap := nil;
  inherited;
end;

{*************************************}
destructor TALCheckStyleObject.Destroy;
begin
  clearBufBitmap;
  inherited;
end;

{*******************************************}
procedure TALCheckStyleObject.clearBufBitmap;
begin
  if fBufBitmap <> nil then begin
    fBufBitmap.Free;
    fBufBitmap := nil;
  end;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALCheckStyleObject.MakeBufBitmap: TTexture;
{$ELSE}
function TALCheckStyleObject.MakeBufBitmap: Tbitmap;
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
     (Scene = nil) or
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
     (FBufState = _fstate) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;
  FBufState := _fstate;

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

{**********************************}
procedure TALCheckStyleObject.Paint;
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

Type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALButtonStyleObjectHelper = class helper for TButtonStyleObject
   function _FCurrent: TButtonAnimation;
  end;


{**************************************************************}
function TALButtonStyleObjectHelper._FCurrent: TButtonAnimation;
begin
  result := TButtonStyleObject(self).FCurrent;
end;

{**********************************************************}
constructor TALButtonStyleObject.Create(AOwner: TComponent);
begin
  fdoubleBuffered := false;
  fBufBitmap := nil;
  inherited;
end;

{**************************************}
destructor TALButtonStyleObject.Destroy;
begin
  clearBufBitmap;
  inherited;
end;

{********************************************}
procedure TALButtonStyleObject.clearBufBitmap;
begin
  if fBufBitmap <> nil then begin
    fBufBitmap.Free;
    fBufBitmap := nil;
  end;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALButtonStyleObject.MakeBufBitmap: TTexture;
{$ELSE}
function TALButtonStyleObject.MakeBufBitmap: Tbitmap;
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
     (Scene = nil) or
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
     (FBuffCurrent = _fCurrent) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;
  FBuffCurrent := _fCurrent;

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

{***********************************}
procedure TALButtonStyleObject.Paint;
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

{**************************************************************}
constructor TALButtonStyleTextObject.Create(AOwner: TComponent);
begin
  fdoubleBuffered := false;
  fBufBitmap := nil;
  inherited;
end;

{******************************************}
destructor TALButtonStyleTextObject.Destroy;
begin
  clearBufBitmap;
  inherited;
end;

{************************************************}
procedure TALButtonStyleTextObject.clearBufBitmap;
begin
  if fBufBitmap <> nil then begin
    fBufBitmap.Free;
    fBufBitmap := nil;
  end;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALButtonStyleTextObject.MakeBufBitmap: TTexture;
{$ELSE}
function TALButtonStyleTextObject.MakeBufBitmap: Tbitmap;
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
     (Scene = nil) or
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) or
     (Text.IsEmpty) then begin
    if fBufBitmap <> nil then begin
      fBufBitmap.Free;
      fBufBitmap := nil;
    end;
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) and
     (fBufText = text) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;
  fBufText := text;

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

{***************************************}
procedure TALButtonStyleTextObject.Paint;
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

initialization
  RegisterFmxClasses([TALStyleObject, TALCheckStyleObject, TALButtonStyleObject, TALButtonStyleTextObject]);

end.
