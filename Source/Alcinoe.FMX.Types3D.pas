unit Alcinoe.FMX.Types3D;

interface

{$I Alcinoe.inc}

uses
  System.Classes,
  System.Messaging,
  {$IFDEF ANDROID}
  Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}
  FMX.types,
  FMX.Types3D;

type

  {*************************************}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Types3D.TTexture still has the exact same fields and adjust the IFDEF'}
  {$ENDIF}
  TALTextureAccessPrivate = class(TInterfacedPersistent)
  public
    FWidth: Integer;
    FHeight: Integer;
    FPixelFormat: TPixelFormat;
    FHandle: TTextureHandle;
    FStyle: TTextureStyles;
    FMagFilter: TTextureFilter;
    FMinFilter: TTextureFilter;
    FTextureScale: Single;
    FRequireInitializeAfterLost: Boolean;
    FBits: Pointer;
    {$IF CompilerVersion >= 36}  // Athens
    FContextLostId: TMessageSubscriptionId;
    FContextResetId: TMessageSubscriptionId;
    {$ELSE}
    FContextLostId: Integer;
    FContextResetId: Integer;
    {$ENDIF}
    FMaterial: TMaterial; // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1549
  end;

  {**************************}
  TALTexture = class(TTexture)
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); overload; override;
    {$IF defined(ANDROID)}
    procedure Assign(Source: jbitmap); reintroduce; overload;
    {$ENDIF}
  end;

  {************************************}
  TALBiPlanarTexture = class(TALTexture)
  private
    FSecondTexture: TTexture;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    property SecondTexture: TTexture read FSecondTexture;
  end;

  {**********************************}
  TALPlanarTexture = class(TALTexture)
  private
    FSecondTexture: TTexture;
    FThirdTexture: TTexture;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    property SecondTexture: TTexture read FSecondTexture;
    property ThirdTexture: TTexture read FThirdTexture;
  end;

{$IF defined(ANDROID)}
procedure ALInitializeExternalOESTexture(const Texture: TTexture);
{$ENDIF}

implementation

uses
  system.sysutils,
  {$IF defined(ANDROID)}
  FMX.Context.GLES.Android,
  Androidapi.JNI.OpenGL,
  Androidapi.Gles2,
  FMX.Context.GLES,
  {$ENDIF}
  fmx.graphics,
  fmx.surfaces,
  FMX.Consts,
  Alcinoe.FMX.Materials.Canvas,
  Alcinoe.StringUtils,
  Alcinoe.Common;

{$IFDEF DEBUG}
var
  TotalMemoryUsedByTextures: int64;
  LastTotalMemoryUsedByTexturesLog: int64;
{$ENDIF}

{****************************}
constructor TALTexture.Create;
begin

  inherited Create;

  // TTextureStyle.Volatile means DO NOT COPY the texture data into internal storage,
  // allowing the texture to be recreated later if the context is lost.
  // Starting from Berlin, the context is no longer lost when the app goes
  // into the background/foreground, so it's unnecessary to set this to false.
  // In Alexandria, TTextureStyle.Volatile is now added by default, but the code below
  // remains in place—it doesn't cause any harm.
   Style := Style + [TTextureStyle.Volatile];

end;

{****************************}
destructor TALTexture.Destroy;
begin
  {$IFDEF DEBUG}
  if (Handle <> 0) and (PixelFormat <> TPixelFormat.None) then AtomicDecrement(TotalMemoryUsedByTextures, Width * Height * BytesPerPixel);
  {$ENDIF}
  inherited Destroy;
end;

{*************************************}
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.Types3D.TTexture.assign is still having the same implementation as in previous version and adjust the IFDEF'}
{$ENDIF}
//
// I don't understand the logic in TTexture.Assign! They don't copy the TBitmap data into the TTexture
// if TCanvasStyle.NeedGPUSurface is in TBitmap(Source).CanvasClass.GetCanvasStyle!
// http://stackoverflow.com/questions/40095054/can-someone-explain-this-strange-behavior-in-ttexture-assign
//
// procedure TTexture.Assign(Source: TPersistent);
// begin
//   ...
//   if not (TCanvasStyle.NeedGPUSurface in TBitmap(Source).CanvasClass.GetCanvasStyle) then
//     begin
//     if TBitmap(Source).Map(TMapAccess.Read, M) then
//     try
//       UpdateTexture(M.Data, M.Pitch);
//     finally
//       TBitmap(Source).Unmap(M);
//     end;
//   end;
//   ...
// end;
//
procedure TALTexture.Assign(Source: TPersistent);
begin

  {$IFDEF DEBUG}
  if (Handle <> 0) and (PixelFormat <> TPixelFormat.None) then AtomicDecrement(TotalMemoryUsedByTextures, Width * Height * BytesPerPixel);
  {$ENDIF}

  if Source is TBitmap then begin
    if Handle <> 0 then TContextManager.DefaultContextClass.FinalizeTexture(Self);
    PixelFormat := TBitmap(Source).PixelFormat;
    Style := [TTextureStyle.Dynamic, TTextureStyle.Volatile];
    TALTextureAccessPrivate(self).fTextureScale := TBitmap(Source).BitmapScale;
    SetSize(TBitmap(Source).Width, TBitmap(Source).Height);
    var M: TBitmapData;
    if TBitmap(Source).Map(TMapAccess.Read, M) then
    try
      UpdateTexture(M.Data, M.Pitch);
    finally
      TBitmap(Source).Unmap(M);
    end;
  end

  else if Source is TBitmapSurface then begin
    if Handle <> 0 then TContextManager.DefaultContextClass.FinalizeTexture(Self);
    Style := [TTextureStyle.Dynamic, TTextureStyle.Volatile];
    SetSize(TBitmapSurface(Source).Width, TBitmapSurface(Source).Height);
    UpdateTexture(TBitmapSurface(Source).Bits, TBitmapSurface(Source).Pitch);
  end

  else
    inherited Assign(Source);

  {$IFDEF DEBUG}
  {$WARNINGS OFF}
  if (Handle <> 0) and (PixelFormat <> TPixelFormat.None) then AtomicIncrement(TotalMemoryUsedByTextures, Width * Height * BytesPerPixel);
  if TThread.GetTickCount - AtomicCmpExchange(LastTotalMemoryUsedByTexturesLog, 0, 0) > 1000 then begin // every 1 sec
    AtomicExchange(LastTotalMemoryUsedByTexturesLog, TThread.GetTickCount); // oki maybe 2 or 3 log can be show simultaneously. i will not died for this !
    ALLog('TALTexture', 'TotalMemoryUsedByTextures: ' + ALFormatFloatW('0.##', AtomicCmpExchange(TotalMemoryUsedByTextures, 0, 0) / 1000000, ALDefaultFormatSettingsW) +' MB');
  end;
  if TALTextureAccessPrivate(self).FBits <> nil then
    ALLog('TALTexture.Assign', 'Bits: ' + ALFormatFloatW('0.##',(Width * Height * BytesPerPixel) / 1000, ALDefaultFormatSettingsW) +' kB', TalLogType.Warn);
  {$WARNINGS ON}
  {$ENDIF}

end;

{********************}
{$IF defined(ANDROID)}
procedure TALTexture.Assign(Source: jbitmap);
begin

  {$IFDEF DEBUG}
  if (Handle <> 0) and (PixelFormat <> TPixelFormat.None) then AtomicDecrement(TotalMemoryUsedByTextures, Width * Height * BytesPerPixel);
  {$ENDIF}

  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if the full flow of FMX.Types3D.TTexture.Assign and FMX.Context.GLES.TCustomContextOpenGL.DoInitializeTexture are still the same as below and adjust the IFDEF'}
  {$ENDIF}
  if (Handle <> 0) and
     (width = Source.getWidth) and
     (Height = Source.getHeight) then begin
    Style := [TTextureStyle.Dynamic, TTextureStyle.Volatile];
    if not (IsEmpty) then begin
      if PixelFormat = TPixelFormat.None then PixelFormat := TCustomAndroidContext.PixelFormat;
      if TCustomAndroidContext.Valid then
      begin
        glBindTexture(GL_TEXTURE_2D, Handle);
        TJGLUtils.JavaClass.texImage2D(
          GL_TEXTURE_2D, // target: Integer;
          0, // level: Integer;
          Source, // bitmap: JBitmap;
          0); // border: Integer  => glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Texture.Width, Texture.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
        glBindTexture(GL_TEXTURE_2D, 0);
        TGlesDiagnostic.RaiseIfHasError(@SCannotCreateTexture, [ClassName]);
      end;
    end;
  end
  else begin
    if Handle <> 0 then TContextManager.DefaultContextClass.FinalizeTexture(Self);
    Style := [TTextureStyle.Dynamic, TTextureStyle.Volatile];
    SetSize(Source.getWidth, Source.getHeight);
    if not (IsEmpty) then begin
      if PixelFormat = TPixelFormat.None then PixelFormat := TCustomAndroidContext.PixelFormat;
      if TCustomAndroidContext.Valid then
      begin
        glActiveTexture(GL_TEXTURE0);
        var Tex: GLuint;
        glGenTextures(1, @Tex);
        glBindTexture(GL_TEXTURE_2D, Tex);
        {$IFDEF IOS}
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        {$ELSE}
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
        {$ENDIF}
        case MagFilter of
          TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
          TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        end;
        if TTextureStyle.MipMaps in Style then
        begin
          case MinFilter of
            TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST);
            TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
          end;
        end
        else
        begin
          case MinFilter of
            TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
            TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
          end;
        end;
        TJGLUtils.JavaClass.texImage2D(
          GL_TEXTURE_2D, // target: Integer;
          0, // level: Integer;
          Source, // bitmap: JBitmap;
          0); // border: Integer  => glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Texture.Width, Texture.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
        glBindTexture(GL_TEXTURE_2D, 0);
        ITextureAccess(self).Handle := Tex;
        TGlesDiagnostic.RaiseIfHasError(@SCannotCreateTexture, [ClassName]);
      end;
    end;
  end;

  {$IFDEF DEBUG}
  if (Handle <> 0) and (PixelFormat <> TPixelFormat.None) then AtomicIncrement(TotalMemoryUsedByTextures, Width * Height * BytesPerPixel);
  if TThread.GetTickCount - AtomicCmpExchange(LastTotalMemoryUsedByTexturesLog, 0, 0) > 1000 then begin // every 1 sec
    AtomicExchange(LastTotalMemoryUsedByTexturesLog, TThread.GetTickCount); // oki maybe 2 or 3 log can be show simultaneously. i will not died for this !
    ALLog('TALTexture', 'TotalMemoryUsedByTextures: ' + ALFormatFloatW('0.##', AtomicCmpExchange(TotalMemoryUsedByTextures, 0, 0) / 1000000, ALDefaultFormatSettingsW) +' MB');
  end;
  {$ENDIF}

end;
{$ENDIF}

{************************************}
constructor TALBiPlanarTexture.Create;
begin
  inherited;
  FSecondTexture := TALTexture.Create;
  {$IF not defined(ALDPK)}
  Material := ALGetDef420YpCbCr8BiPlanarVideoRangeMaterial;
  {$ENDIF}
end;

{************************************}
destructor TALBiPlanarTexture.Destroy;
begin
  alFreeAndNil(FSecondTexture);
  inherited;
end;

{**********************************}
constructor TALPlanarTexture.Create;
begin
  inherited;
  FSecondTexture := TALTexture.Create;
  FThirdTexture := TALTexture.Create;
  {$IF not defined(ALDPK)}
  Material := ALGetDef420YpCbCr8PlanarMaterial;
  {$ENDIF}
end;

{**********************************}
destructor TALPlanarTexture.Destroy;
begin
  alFreeAndNil(FSecondTexture);
  alFreeAndNil(FThirdTexture);
  inherited;
end;

{********************}
{$IF defined(ANDROID)}
procedure ALInitializeExternalOESTexture(const Texture: TTexture);
begin
  Texture.Material := ALGetDefExternalOESMaterial;
  Texture.Style := Texture.Style + [TTextureStyle.External] - [TTextureStyle.MipMaps]; // GL_TEXTURE_EXTERNAL_OES does not support mipmaps
  if Texture.PixelFormat = TPixelFormat.None then Texture.PixelFormat := TCustomAndroidContext.PixelFormat;
  Texture.Initialize;
end;
{$ENDIF}

initialization
  {$IFDEF DEBUG}
  TotalMemoryUsedByTextures := 0;
  LastTotalMemoryUsedByTexturesLog := 0;
  {$ENDIF}

end.
