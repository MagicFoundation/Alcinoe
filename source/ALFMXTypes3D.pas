unit ALFMXTypes3D;

interface

uses System.Classes,
     {$IFDEF ANDROID}
     Androidapi.JNI.GraphicsContentViewText,
     FMX.Context.GLES.Android,
     {$ELSEIF defined(IOS)}
     FMX.Context.GLES.iOS,
     {$ENDIF}
     FMX.types,
     FMX.Types3D,
     FMX.Materials.Canvas;

type

  {************************}
  {$IF CompilerVersion > 32} // tokyo
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
    FTextureScale: Single;  // << i need to access this field
    FRequireInitializeAfterLost: Boolean;
    FBits: Pointer;
    FContextLostId: Integer;
    FContextResetId: Integer;
  protected
  public
  end;

  {**************************}
  TALTexture = class(TTexture)
  private
    FisExternalOES: Boolean;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); overload; override;
    {$IF defined(ANDROID)}
    procedure Assign(Source: jbitmap); reintroduce; overload;
    {$ENDIF}
    property isExternalOES: boolean read fisExternalOES write fisExternalOES;
  end;

  {****************************}
  //420YpCbCr8BiPlanarVideoRange
  //the Y data (luminance) and the CbCr data (chroma or color information)
  //are in two separate memory areas called planes. If you have a full range
  //format, then the values from 0 to 255 are used for each luma or chroma
  //value. Video range format only use values from 16 to 235 (for some
  //historical reasons). The term 420 indicates how much luma and how much
  //chroma information the format contains. It basically says that there is
  //luma information for each pixel and chroma information for each 2x2 block.
  //420YpCbCr8Planar
  //Planar Component Y'CbCr 8-bit 4:2:0
  TALTextureFormat = (f420YpCbCr8BiPlanarVideoRange, f420YpCbCr8Planar);

  {************************************}
  TALBiPlanarTexture = class(TALTexture)
  private
    FSecondTexture: TTexture;
    FFormat: TALTextureFormat;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    property SecondTexture: TTexture read FSecondTexture;
    property Format: TALTextureFormat read fformat write fformat;
  end;

  {**********************************}
  TALPlanarTexture = class(TALTexture)
  private
    FSecondTexture: TTexture;
    FThirdTexture: TTexture;
    FFormat: TALTextureFormat;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    property SecondTexture: TTexture read FSecondTexture;
    property ThirdTexture: TTexture read FThirdTexture;
    property Format: TALTextureFormat read fformat write fformat;
  end;

  {**************}
  {$IFDEF ANDROID}
  TALCustomAndroidContextAccess = class(TCustomAndroidContext); // << i need to access protected CreateSharedContext
  {$ELSEIF defined(IOS)}
  TALCustomContextIOSAccess = class(TCustomContextIOS); // << i need to access protected CreateSharedContext
  {$ENDIF}

{$IF defined(ANDROID)}
procedure ALInitializeExternalOESTexture(const Texture: TALTexture);
{$ENDIF}

type

  {*****************************************************************}
  TALCanvasExternalOESTextureMaterial = class(TCanvasTextureMaterial)
  private
  protected
    procedure DoInitialize; override;
  public
  end;

  {**********************************************************************************}
  TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial = class(TCanvasTextureMaterial)
  private
    [Weak] FCbCrTexture: TTexture;
    procedure SetCbCrTexture(const Value: TTexture);
  protected
    procedure DoApply(const Context: TContext3D); override;
    procedure DoInitialize; override;
  public
    property CbCrTexture: TTexture read FCbCrTexture write SetCbCrTexture;
  end;

  {**********************************************************************}
  TALCanvas420YpCbCr8PlanarTextureMaterial = class(TCanvasTextureMaterial)
  private
    [Weak] FCbTexture: TTexture;
    [Weak] FCrTexture: TTexture;
    procedure SetCbTexture(const Value: TTexture);
    procedure SetCrTexture(const Value: TTexture);
  protected
    procedure DoApply(const Context: TContext3D); override;
    procedure DoInitialize; override;
  public
    property CbTexture: TTexture read FCbTexture write SetCbTexture;
    property CrTexture: TTexture read FCrTexture write SetCrTexture;
  end;

implementation

uses system.sysutils,
     {$IF defined(ANDROID)}
     Androidapi.JNI.OpenGL,
     Androidapi.Gles2,
     Androidapi.Gles2ext,
     FMX.Context.GLES,
     {$ENDIF}
     fmx.graphics,
     fmx.surfaces,
     FMX.Consts,
     {$IFDEF DEBUG}
     AlString,
     {$ENDIF}
     ALCommon;

{$IFDEF DEBUG}
var TotalMemoryUsedByTextures: int64;
    LastTotalMemoryUsedByTexturesLog: int64;
{$ENDIF}

{****************************}
constructor TALTexture.Create;
begin

   //inherited create
   inherited Create;

   //TTextureStyle.Volatile mean DO NOT COPY the bytes in an internal storage
   //to have the possibility to recreate later the texture if we lost the
   //context. From Berlin we don't lost anymore the context when the app go
   //in background/foreground so it's useless to set it to false
   Style := Style + [TTextureStyle.Volatile];

   //init FisExternalOES
   FisExternalOES := False;

end;

{****************************}
destructor TALTexture.Destroy;
begin
  {$IFDEF DEBUG}
  if PixelFormat <> TPixelFormat.None then AtomicDecrement(TotalMemoryUsedByTextures, Width * Height * BytesPerPixel);
  {$ENDIF}
  inherited Destroy;
end;

{************************}
{$IF CompilerVersion > 32}
  {$MESSAGE WARN 'Check if FMX.Types3D.TTexture.assign is still having the same implementation as in previous version and adjust the IFDEF'}
{$ENDIF}
//
// i don't understand the sheet they do in TTexture.assign! they don't copy the Tbitmap data in the TTexture
// if TCanvasStyle.NeedGPUSurface in TBitmap(Source).CanvasClass.GetCanvasStyle !
// http://stackoverflow.com/questions/40095054/can-someone-explain-me-this-strange-behavior-in-ttexture-assign
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
var
  M: TBitmapData;
begin

  {$IFDEF DEBUG}
  if PixelFormat <> TPixelFormat.None then AtomicDecrement(TotalMemoryUsedByTextures, Width * Height * BytesPerPixel);
  {$ENDIF}

  if Source is TBitmap then begin
    {$IF CompilerVersion >= 32} // tokyo
    //TMonitor.Enter(Self);
    //try
    {$ENDIF}
      if Handle <> 0 then TContextManager.DefaultContextClass.FinalizeTexture(Self);
      PixelFormat := TBitmap(Source).PixelFormat;
      Style := [TTextureStyle.Dynamic, TTextureStyle.Volatile];
      TALTextureAccessPrivate(self).fTextureScale := TBitmap(Source).BitmapScale;
      SetSize(TBitmap(Source).Width, TBitmap(Source).Height);
      if TBitmap(Source).Map(TMapAccess.Read, M) then
      try
        UpdateTexture(M.Data, M.Pitch);
      finally
        TBitmap(Source).Unmap(M);
      end;
    {$IF CompilerVersion >= 32} // tokyo
    //finally
    //  TMonitor.exit(Self);
    //end;
    {$ENDIF}
  end

  else if Source is TBitmapSurface then begin
    {$IF CompilerVersion >= 32} // tokyo
    //TMonitor.Enter(Self);
    //try
    {$ENDIF}
      if Handle <> 0 then TContextManager.DefaultContextClass.FinalizeTexture(Self);
      Style := [TTextureStyle.Dynamic, TTextureStyle.Volatile];
      SetSize(TBitmapSurface(Source).Width, TBitmapSurface(Source).Height);
      UpdateTexture(TBitmapSurface(Source).Bits, TBitmapSurface(Source).Pitch);
    {$IF CompilerVersion >= 32} // tokyo
    //finally
    //  TMonitor.exit(Self);
    //end;
    {$ENDIF}
  end

  else if Source is TTexture then begin
    {$IF CompilerVersion >= 32} // tokyo
    //TMonitor.Enter(Self);
    //try
    {$ENDIF}
      if Handle <> 0 then TContextManager.DefaultContextClass.FinalizeTexture(Self);
      TALTextureAccessPrivate(self).FWidth := TTexture(Source).width;
      TALTextureAccessPrivate(self).FHeight := TTexture(Source).height;
      TALTextureAccessPrivate(self).FPixelFormat := TTexture(Source).PixelFormat;
      TALTextureAccessPrivate(self).FHandle := TTexture(Source).Handle;
      TALTextureAccessPrivate(self).FStyle := TTexture(Source).Style + [TTextureStyle.Volatile];
      TALTextureAccessPrivate(self).FMagFilter := TTexture(Source).MagFilter;
      TALTextureAccessPrivate(self).FMinFilter := TTexture(Source).MinFilter;
      TALTextureAccessPrivate(self).FTextureScale := TTexture(Source).TextureScale;
      //FRequireInitializeAfterLost: Boolean;
      //FBits: Pointer;
      //FContextLostId: Integer;
      //FContextResetId: Integer;
    {$IF CompilerVersion >= 32} // tokyo
    //finally
    //  TMonitor.exit(Self);
    //end;
    {$ENDIF}
  end

  else inherited ;

  {$IFDEF DEBUG}
  if PixelFormat <> TPixelFormat.None then AtomicIncrement(TotalMemoryUsedByTextures, Width * Height * BytesPerPixel);
  if TThread.GetTickCount - AtomicCmpExchange(LastTotalMemoryUsedByTexturesLog, 0, 0) > 1000 then begin // every 1 sec
    AtomicExchange(LastTotalMemoryUsedByTexturesLog, TThread.GetTickCount); // oki maybe 2 or 3 log can be show simultaneously. i will not died for this !
    ALLog('TALTexture', 'TotalMemoryUsedByTextures: ' + ALFormatFloatU('0.##', AtomicCmpExchange(TotalMemoryUsedByTextures, 0, 0) / 1000000, ALDefaultFormatSettingsU) +' MB', TalLogType.verbose);
  end;
  if TALTextureAccessPrivate(self).FBits <> nil then
    ALLog('TALTexture.Assign', 'Bits: ' + ALFormatFloatU('0.##',(Width * Height * BytesPerPixel) / 1000, ALDefaultFormatSettingsU) +' kB', TalLogType.Warn);
  {$ENDIF}

end;

{********************}
{$IF defined(ANDROID)}
procedure TALTexture.Assign(Source: jbitmap);
var Tex: GLuint;
begin

  {$IFDEF DEBUG}
  if PixelFormat <> TPixelFormat.None then AtomicDecrement(TotalMemoryUsedByTextures, Width * Height * BytesPerPixel);
  {$ENDIF}

  {$IF CompilerVersion > 32} // tokyo
    {$MESSAGE WARN 'Check if the full flow of FMX.Types3D.TTexture.Assign is still the same as below and adjust the IFDEF'}
  {$ENDIF}
  {$IF CompilerVersion >= 32} // tokyo
  //TMonitor.Enter(Self);
  //try
  {$ENDIF}
    if Handle <> 0 then TContextManager.DefaultContextClass.FinalizeTexture(Self);
    Style := [TTextureStyle.Dynamic, TTextureStyle.Volatile];
    SetSize(Source.getWidth, Source.getHeight);
    if not (IsEmpty) then begin
      if PixelFormat = TPixelFormat.None then PixelFormat := TCustomAndroidContext.PixelFormat;
      {$IF CompilerVersion <= 31} // berlin
      TCustomAndroidContext.SharedContext; // >> because stupidly CreateSharedContext is protected :(
      if TCustomAndroidContext.IsContextAvailable then
      {$ELSE} // Tokyo
      if TCustomAndroidContext.Valid then
      {$ENDIF}
      begin
        glActiveTexture(GL_TEXTURE0);
        glGenTextures(1, @Tex);
        glBindTexture(GL_TEXTURE_2D, Tex);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
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
        TJGLUtils.JavaClass.texImage2D(GL_TEXTURE_2D, // target: Integer;
                                       0, // level: Integer;
                                       Source, // bitmap: JBitmap;
                                       0); // border: Integer  => glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Texture.Width, Texture.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
        glBindTexture(GL_TEXTURE_2D, 0);
        ITextureAccess(self).Handle := Tex;
        if (TCustomAndroidContext.GLHasAnyErrors()) then
          RaiseContextExceptionFmt(@SCannotCreateTexture, ['TALTexture']);
      end;
    end;
  {$IF CompilerVersion >= 32} // tokyo
  //finally
  //  TMonitor.exit(Self);
  //end;
  {$ENDIF}

  {$IFDEF DEBUG}
  if PixelFormat <> TPixelFormat.None then AtomicIncrement(TotalMemoryUsedByTextures, Width * Height * BytesPerPixel);
  if TThread.GetTickCount - AtomicCmpExchange(LastTotalMemoryUsedByTexturesLog, 0, 0) > 1000 then begin // every 1 sec
    AtomicExchange(LastTotalMemoryUsedByTexturesLog, TThread.GetTickCount); // oki maybe 2 or 3 log can be show simultaneously. i will not died for this !
    ALLog('TALTexture', 'TotalMemoryUsedByTextures: ' + ALFormatFloatU('0.##', AtomicCmpExchange(TotalMemoryUsedByTextures, 0, 0) / 1000000, ALDefaultFormatSettingsU) +' MB', TalLogType.verbose);
  end;
  {$ENDIF}

end;
{$ENDIF}

{************************************}
constructor TALBiPlanarTexture.Create;
begin
  inherited;
  FSecondTexture := TALTexture.Create;
  FFormat := TALTextureFormat.f420YpCbCr8BiPlanarVideoRange;
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
  FFormat := TALTextureFormat.f420YpCbCr8Planar;
end;

{**********************************}
destructor TALPlanarTexture.Destroy;
begin
  alFreeAndNil(FSecondTexture);
  alFreeAndNil(FThirdTexture);
  inherited;
end;

{*************************}
{$IF CompilerVersion > 32} // tokyo
  {$MESSAGE WARN 'Check if FMX.Context.GLES.TCustomContextOpenGL.DoInitializeTexture still has the same implementation and adjust the IFDEF'}
{$ENDIF}
{$IF defined(ANDROID)}
procedure ALInitializeExternalOESTexture(const Texture: TALTexture);
var Tex: GLuint;
begin
  Texture.FisExternalOES := True;
  Texture.Style := Texture.Style - [TTextureStyle.MipMaps];

  {$IFDEF ANDROID}
  if Texture.PixelFormat = TPixelFormat.None then Texture.PixelFormat := TALCustomAndroidContextAccess.PixelFormat;
  {$ELSEIF defined(IOS)}
  if Texture.PixelFormat = TPixelFormat.None then Texture.PixelFormat := TALCustomContextIOSAccess.PixelFormat;
  {$ENDIF}

  {$IF CompilerVersion >= 32} // tokyo

  {$IFDEF ANDROID}
  if TALCustomAndroidContextAccess.valid then
  {$ELSEIF defined(IOS)}
  if TALCustomContextIOSAccess.valid then
  {$ENDIF}

  {$ELSE}

  {$IFDEF ANDROID}
  TALCustomAndroidContextAccess.CreateSharedContext;
  if TALCustomAndroidContextAccess.IsContextAvailable then
  {$ELSEIF defined(IOS)}
  TALCustomContextIOSAccess.CreateSharedContext;
  if TALCustomContextIOSAccess.IsContextAvailable then
  {$ENDIF}

  {$ENDIF}

  begin
    glActiveTexture(GL_TEXTURE0);
    glGenTextures(1, @Tex);
    glBindTexture(GL_TEXTURE_EXTERNAL_OES, Tex);
    glTexParameteri(GL_TEXTURE_EXTERNAL_OES, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_EXTERNAL_OES, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    case Texture.MagFilter of
      TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_EXTERNAL_OES, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_EXTERNAL_OES, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    end;
    //if TTextureStyle.MipMaps in Texture.Style then begin
    //  case Texture.MinFilter of
    //    TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_EXTERNAL_OES, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST);
    //    TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_EXTERNAL_OES, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    //  end;
    //end
    //else begin
      case Texture.MinFilter of
        TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_EXTERNAL_OES, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_EXTERNAL_OES, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      end;
    //end;
    //glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Texture.Width, Texture.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
    glBindTexture(GL_TEXTURE_EXTERNAL_OES, 0);
    ITextureAccess(Texture).Handle := Tex;
    {$IFDEF ANDROID}
    if (TALCustomAndroidContextAccess.GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SCannotCreateTexture, [TALCustomAndroidContextAccess.ClassName]);
    {$ELSEIF defined(IOS)}
    if (TALCustomContextIOSAccess.GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SCannotCreateTexture, [TALCustomContextIOSAccess.ClassName]);
    {$ENDIF}
  end;
end;
{$ENDIF}

{************************}
{$IF CompilerVersion > 32}
  {$MESSAGE WARN 'Check if FMX.Materials.Canvas.TCanvasTextureMaterial.DoInitialize is still having the same implementation as in previous version and adjust the IFDEF'}
{$ENDIF}
procedure TALCanvasExternalOESTextureMaterial.DoInitialize;
begin
  inherited;
  FVertexShader := TShaderManager.RegisterShaderFromData('cnv_texture.fvs', TContextShaderKind.VertexShader, '', [
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FE, $FF, $FE, $FF, $1F, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $53, $00, $00, $00, $00, $02, $FE, $FF, $01, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $4C, $00, $00, $00,
      $30, $00, $00, $00, $02, $00, $00, $00, $04, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $00, $AB, $AB, $03, $00, $03, $00, $04, $00, $04, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $76, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65,
      $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $0F, $90, $1F, $00, $00, $02, $0A, $00, $00, $80, $01, $00, $0F, $90, $1F, $00, $00, $02,
      $05, $00, $00, $80, $02, $00, $0F, $90, $05, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $55, $90, $01, $00, $E4, $A0, $04, $00, $00, $04, $00, $00, $0F, $80, $00, $00, $E4, $A0, $00, $00, $00, $90,
      $00, $00, $E4, $80, $04, $00, $00, $04, $00, $00, $0F, $80, $02, $00, $E4, $A0, $00, $00, $AA, $90, $00, $00, $E4, $80, $02, $00, $00, $03, $00, $00, $0F, $C0, $00, $00, $E4, $80, $03, $00, $E4, $A0,
      $01, $00, $00, $02, $00, $00, $03, $E0, $02, $00, $E4, $90, $01, $00, $00, $02, $00, $00, $0F, $D0, $01, $00, $E4, $90, $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    ),
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $09, $25, $BD, $36, $32, $98, $40, $33, $C7, $18, $0B, $ED, $E6, $C0, $C3, $D4, $01, $00, $00, $00, $80, $04, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $20, $01, $00, $00,
      $50, $02, $00, $00, $CC, $02, $00, $00, $9C, $03, $00, $00, $0C, $04, $00, $00, $41, $6F, $6E, $39, $E0, $00, $00, $00, $E0, $00, $00, $00, $00, $02, $FE, $FF, $AC, $00, $00, $00, $34, $00, $00, $00,
      $01, $00, $24, $00, $00, $00, $30, $00, $00, $00, $30, $00, $00, $00, $24, $00, $01, $00, $30, $00, $00, $00, $00, $00, $04, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $02, $FE, $FF,
      $1F, $00, $00, $02, $05, $00, $00, $80, $00, $00, $0F, $90, $1F, $00, $00, $02, $05, $00, $01, $80, $01, $00, $0F, $90, $1F, $00, $00, $02, $05, $00, $02, $80, $02, $00, $0F, $90, $05, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $55, $90, $02, $00, $E4, $A0, $04, $00, $00, $04, $00, $00, $0F, $80, $01, $00, $E4, $A0, $00, $00, $00, $90, $00, $00, $E4, $80, $04, $00, $00, $04, $00, $00, $0F, $80,
      $03, $00, $E4, $A0, $00, $00, $AA, $90, $00, $00, $E4, $80, $02, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80, $04, $00, $E4, $A0, $04, $00, $00, $04, $00, $00, $03, $C0, $00, $00, $FF, $80,
      $00, $00, $E4, $A0, $00, $00, $E4, $80, $01, $00, $00, $02, $00, $00, $0C, $C0, $00, $00, $E4, $80, $01, $00, $00, $02, $00, $00, $03, $E0, $02, $00, $E4, $90, $01, $00, $00, $02, $01, $00, $0F, $E0,
      $01, $00, $C6, $90, $FF, $FF, $00, $00, $53, $48, $44, $52, $28, $01, $00, $00, $40, $00, $01, $00, $4A, $00, $00, $00, $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00, $04, $00, $00, $00,
      $5F, $00, $00, $03, $72, $10, $10, $00, $00, $00, $00, $00, $5F, $00, $00, $03, $F2, $10, $10, $00, $01, $00, $00, $00, $5F, $00, $00, $03, $32, $10, $10, $00, $02, $00, $00, $00, $67, $00, $00, $04,
      $F2, $20, $10, $00, $00, $00, $00, $00, $01, $00, $00, $00, $65, $00, $00, $03, $32, $20, $10, $00, $01, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00, $02, $00, $00, $00, $68, $00, $00, $02,
      $01, $00, $00, $00, $38, $00, $00, $08, $F2, $00, $10, $00, $00, $00, $00, $00, $56, $15, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00, $32, $00, $00, $0A,
      $F2, $00, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $10, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A,
      $F2, $00, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $02, $00, $00, $00, $A6, $1A, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $00, $00, $00, $00, $00, $00, $00, $08,
      $F2, $20, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $36, $00, $00, $05, $32, $20, $10, $00, $01, $00, $00, $00,
      $46, $10, $10, $00, $02, $00, $00, $00, $36, $00, $00, $05, $F2, $20, $10, $00, $02, $00, $00, $00, $66, $1C, $10, $00, $01, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00,
      $07, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46,
      $C8, $00, $00, $00, $01, $00, $00, $00, $48, $00, $00, $00, $01, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FE, $FF, $00, $11, $00, $00, $94, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $24, $47, $6C, $6F, $62, $61, $6C, $73, $00, $AB, $AB, $AB, $3C, $00, $00, $00,
      $01, $00, $00, $00, $60, $00, $00, $00, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $78, $00, $00, $00, $00, $00, $00, $00, $40, $00, $00, $00, $02, $00, $00, $00, $84, $00, $00, $00,
      $00, $00, $00, $00, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $00, $AB, $AB, $03, $00, $03, $00, $04, $00, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66,
      $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34,
      $00, $AB, $AB, $AB, $49, $53, $47, $4E, $68, $00, $00, $00, $03, $00, $00, $00, $08, $00, $00, $00, $50, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00,
      $07, $07, $00, $00, $59, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $0F, $0F, $00, $00, $5F, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $03, $00, $00, $00, $02, $00, $00, $00, $03, $03, $00, $00, $50, $4F, $53, $49, $54, $49, $4F, $4E, $00, $43, $4F, $4C, $4F, $52, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $4F, $53, $47, $4E,
      $6C, $00, $00, $00, $03, $00, $00, $00, $08, $00, $00, $00, $50, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $5C, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $03, $0C, $00, $00, $65, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $02, $00, $00, $00,
      $0F, $00, $00, $00, $53, $56, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $43, $4F, $4C, $4F, $52, $00, $AB], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 64)]
    ),
    //attribute vec2 a_TexCoord0;
    //attribute vec4 a_Color;
    //attribute vec3 a_Position;
    //varying vec4 TEX0;
    //varying vec4 COLOR0;
    //vec4 _o_pos1;
    //vec4 _o_color1;
    //vec2 _o_texcoord01;
    //vec4 _r0003;
    //vec4 _v0003;
    //uniform vec4 _MVPMatrix[4];
    //void main()
    //{
    //    _v0003 = vec4(a_Position.x, a_Position.y, a_Position.z, 1.0);
    //    _r0003.x = dot(_MVPMatrix[0], _v0003);
    //    _r0003.y = dot(_MVPMatrix[1], _v0003);
    //    _r0003.z = dot(_MVPMatrix[2], _v0003);
    //    _r0003.w = dot(_MVPMatrix[3], _v0003);
    //    _o_pos1 = _r0003;
    //    _o_texcoord01 = a_TexCoord0.xy;
    //    _o_color1 = a_Color;
    //    TEX0.xy = a_TexCoord0.xy;
    //    COLOR0 = a_Color;
    //    gl_Position = _r0003;
    //}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $32, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76,
      $65, $63, $34, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $33, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $3B,
      $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $43, $4F, $4C, $4F, $52,
      $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $63, $6F, $6C, $6F, $72, $31, $3B, $0D, $0A, $76, $65, $63, $32,
      $20, $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $76, $30, $30,
      $30, $33, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $34, $20, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $34, $5D, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D,
      $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $5F, $76, $30, $30, $30, $33, $20, $3D, $20, $76, $65, $63, $34, $28, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $78,
      $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $79, $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $7A, $2C, $20, $31, $2E, $30, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $72, $30, $30, $30, $33, $2E, $78, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $30, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $79, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $31, $5D, $2C, $20, $5F, $76, $30, $30, $30,
      $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $7A, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $32, $5D, $2C, $20, $5F,
      $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $77, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $33,
      $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F,
      $63, $6F, $6C, $6F, $72, $31, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $54, $45, $58, $30, $2E, $78, $79, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F,
      $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $43, $4F, $4C, $4F, $52, $30, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $67, $6C, $5F,
      $50, $6F, $73, $69, $74, $69, $6F, $6E, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    )
  ]);
  FPixelShader := TShaderManager.RegisterShaderFromData('cnv_texture.fps', TContextShaderKind.PixelShader, '', [
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $1F, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $53, $00, $00, $00, $00, $02, $FF, $FF, $01, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $4C, $00, $00, $00,
      $30, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00, $74, $65, $78, $74, $75, $72, $65, $30, $00, $AB, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65,
      $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $0F, $90, $1F, $00, $00, $02,
      $00, $00, $00, $90, $00, $08, $0F, $A0, $13, $00, $00, $02, $00, $00, $03, $80, $00, $00, $E4, $B0, $42, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $00, $E4, $90, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $46, $A5, $E7, $2F, $D8, $00, $2D, $BE, $FD, $AB, $93, $DB, $30, $8A, $D3, $32, $01, $00, $00, $00, $40, $03, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $CC, $00, $00, $00,
      $7C, $01, $00, $00, $F8, $01, $00, $00, $98, $02, $00, $00, $0C, $03, $00, $00, $41, $6F, $6E, $39, $8C, $00, $00, $00, $8C, $00, $00, $00, $00, $02, $FF, $FF, $64, $00, $00, $00, $28, $00, $00, $00,
      $00, $00, $28, $00, $00, $00, $28, $00, $00, $00, $28, $00, $01, $00, $24, $00, $00, $00, $28, $00, $00, $00, $00, $00, $00, $02, $FF, $FF, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0,
      $1F, $00, $00, $02, $00, $00, $00, $80, $01, $00, $0F, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $13, $00, $00, $02, $00, $00, $03, $80, $00, $00, $E4, $B0, $42, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80, $01, $00, $E4, $B0, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80,
      $FF, $FF, $00, $00, $53, $48, $44, $52, $A8, $00, $00, $00, $40, $00, $00, $00, $2A, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $00, $00, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00,
      $00, $00, $00, $00, $55, $55, $00, $00, $62, $10, $00, $03, $32, $10, $10, $00, $01, $00, $00, $00, $62, $10, $00, $03, $F2, $10, $10, $00, $02, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00,
      $00, $00, $00, $00, $68, $00, $00, $02, $01, $00, $00, $00, $1A, $00, $00, $05, $32, $00, $10, $00, $00, $00, $00, $00, $46, $10, $10, $00, $01, $00, $00, $00, $45, $00, $00, $09, $F2, $00, $10, $00,
      $00, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $38, $00, $00, $07, $F2, $20, $10, $00, $00, $00, $00, $00,
      $46, $0E, $10, $00, $00, $00, $00, $00, $46, $1E, $10, $00, $02, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00, $04, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00,
      $03, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46, $98, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $02, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FF, $FF, $00, $11, $00, $00, $65, $00, $00, $00, $5C, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00,
      $0C, $00, $00, $00, $74, $65, $78, $74, $75, $72, $65, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43,
      $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $AB, $49, $53, $47, $4E, $6C, $00, $00, $00, $03, $00, $00, $00, $08, $00, $00, $00,
      $50, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $5C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00,
      $01, $00, $00, $00, $03, $03, $00, $00, $65, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $02, $00, $00, $00, $0F, $0F, $00, $00, $53, $56, $5F, $50, $6F, $73, $69, $74,
      $69, $6F, $6E, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $43, $4F, $4C, $4F, $52, $00, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65, $74, $00, $AB, $AB], [
      TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    //
    //ORIGINAL:
    //
    //varying vec4 COLOR0;
    //varying vec4 TEX0;
    //vec4 _ret_0;
    //float _TMP1;
    //float _TMP0;
    //vec2 _c0008;
    //uniform sampler2D _texture0;
    //void main()
    //{
    //    vec4 _texColor;
    //    _TMP0 = fract(TEX0.x);
    //    _TMP1 = fract(TEX0.y);
    //    _c0008 = vec2(_TMP0, _TMP1);
    //    _texColor = texture2D(_texture0, _c0008);
    //    _ret_0 = _texColor*COLOR0;
    //    gl_FragColor = _ret_0;
    //    return;
    //}
    //
    TContextShaderSource.Create(
      TContextShaderArch.GLSL,
      TEncoding.UTF8.GetBytes(

        '#extension GL_OES_EGL_image_external : require'+#13#10+
        'precision highp float;'+
        'varying vec4 COLOR0;'+
        'varying vec4 TEX0;'+
        'uniform samplerExternalOES _texture0;'+

        'void main()'+
        '{'+

           'gl_FragColor = texture2D(_texture0, TEX0.xy) * COLOR0;'+

        '}'

      ),
      [TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0)]
    )
  ]);
end;

{***************************************************************************************************}
procedure TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial.SetCbCrTexture(const Value: TTexture);
begin
  if FCbCrTexture <> Value then
    FCbCrTexture := Value;
end;

{************************************************************************************************}
procedure TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial.DoApply(const Context: TContext3D);
begin
  inherited;
  Context.SetShaderVariable('texture1', FCbCrTexture);
end;

{************************}
{$IF CompilerVersion > 32}
  {$MESSAGE WARN 'Check if FMX.Materials.Canvas.TCanvasTextureMaterial.DoInitialize is still having the same implementation as in previous version and adjust the IFDEF'}
{$ENDIF}
procedure TALCanvas420YpCbCr8BiPlanarVideoRangeTextureMaterial.DoInitialize;
begin
  inherited;
  FVertexShader := TShaderManager.RegisterShaderFromData('cnv_texture.fvs', TContextShaderKind.VertexShader, '', [
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FE, $FF, $FE, $FF, $1F, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $53, $00, $00, $00, $00, $02, $FE, $FF, $01, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $4C, $00, $00, $00,
      $30, $00, $00, $00, $02, $00, $00, $00, $04, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $00, $AB, $AB, $03, $00, $03, $00, $04, $00, $04, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $76, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65,
      $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $0F, $90, $1F, $00, $00, $02, $0A, $00, $00, $80, $01, $00, $0F, $90, $1F, $00, $00, $02,
      $05, $00, $00, $80, $02, $00, $0F, $90, $05, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $55, $90, $01, $00, $E4, $A0, $04, $00, $00, $04, $00, $00, $0F, $80, $00, $00, $E4, $A0, $00, $00, $00, $90,
      $00, $00, $E4, $80, $04, $00, $00, $04, $00, $00, $0F, $80, $02, $00, $E4, $A0, $00, $00, $AA, $90, $00, $00, $E4, $80, $02, $00, $00, $03, $00, $00, $0F, $C0, $00, $00, $E4, $80, $03, $00, $E4, $A0,
      $01, $00, $00, $02, $00, $00, $03, $E0, $02, $00, $E4, $90, $01, $00, $00, $02, $00, $00, $0F, $D0, $01, $00, $E4, $90, $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    ),
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $09, $25, $BD, $36, $32, $98, $40, $33, $C7, $18, $0B, $ED, $E6, $C0, $C3, $D4, $01, $00, $00, $00, $80, $04, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $20, $01, $00, $00,
      $50, $02, $00, $00, $CC, $02, $00, $00, $9C, $03, $00, $00, $0C, $04, $00, $00, $41, $6F, $6E, $39, $E0, $00, $00, $00, $E0, $00, $00, $00, $00, $02, $FE, $FF, $AC, $00, $00, $00, $34, $00, $00, $00,
      $01, $00, $24, $00, $00, $00, $30, $00, $00, $00, $30, $00, $00, $00, $24, $00, $01, $00, $30, $00, $00, $00, $00, $00, $04, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $02, $FE, $FF,
      $1F, $00, $00, $02, $05, $00, $00, $80, $00, $00, $0F, $90, $1F, $00, $00, $02, $05, $00, $01, $80, $01, $00, $0F, $90, $1F, $00, $00, $02, $05, $00, $02, $80, $02, $00, $0F, $90, $05, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $55, $90, $02, $00, $E4, $A0, $04, $00, $00, $04, $00, $00, $0F, $80, $01, $00, $E4, $A0, $00, $00, $00, $90, $00, $00, $E4, $80, $04, $00, $00, $04, $00, $00, $0F, $80,
      $03, $00, $E4, $A0, $00, $00, $AA, $90, $00, $00, $E4, $80, $02, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80, $04, $00, $E4, $A0, $04, $00, $00, $04, $00, $00, $03, $C0, $00, $00, $FF, $80,
      $00, $00, $E4, $A0, $00, $00, $E4, $80, $01, $00, $00, $02, $00, $00, $0C, $C0, $00, $00, $E4, $80, $01, $00, $00, $02, $00, $00, $03, $E0, $02, $00, $E4, $90, $01, $00, $00, $02, $01, $00, $0F, $E0,
      $01, $00, $C6, $90, $FF, $FF, $00, $00, $53, $48, $44, $52, $28, $01, $00, $00, $40, $00, $01, $00, $4A, $00, $00, $00, $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00, $04, $00, $00, $00,
      $5F, $00, $00, $03, $72, $10, $10, $00, $00, $00, $00, $00, $5F, $00, $00, $03, $F2, $10, $10, $00, $01, $00, $00, $00, $5F, $00, $00, $03, $32, $10, $10, $00, $02, $00, $00, $00, $67, $00, $00, $04,
      $F2, $20, $10, $00, $00, $00, $00, $00, $01, $00, $00, $00, $65, $00, $00, $03, $32, $20, $10, $00, $01, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00, $02, $00, $00, $00, $68, $00, $00, $02,
      $01, $00, $00, $00, $38, $00, $00, $08, $F2, $00, $10, $00, $00, $00, $00, $00, $56, $15, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00, $32, $00, $00, $0A,
      $F2, $00, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $10, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A,
      $F2, $00, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $02, $00, $00, $00, $A6, $1A, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $00, $00, $00, $00, $00, $00, $00, $08,
      $F2, $20, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $36, $00, $00, $05, $32, $20, $10, $00, $01, $00, $00, $00,
      $46, $10, $10, $00, $02, $00, $00, $00, $36, $00, $00, $05, $F2, $20, $10, $00, $02, $00, $00, $00, $66, $1C, $10, $00, $01, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00,
      $07, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46,
      $C8, $00, $00, $00, $01, $00, $00, $00, $48, $00, $00, $00, $01, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FE, $FF, $00, $11, $00, $00, $94, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $24, $47, $6C, $6F, $62, $61, $6C, $73, $00, $AB, $AB, $AB, $3C, $00, $00, $00,
      $01, $00, $00, $00, $60, $00, $00, $00, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $78, $00, $00, $00, $00, $00, $00, $00, $40, $00, $00, $00, $02, $00, $00, $00, $84, $00, $00, $00,
      $00, $00, $00, $00, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $00, $AB, $AB, $03, $00, $03, $00, $04, $00, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66,
      $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34,
      $00, $AB, $AB, $AB, $49, $53, $47, $4E, $68, $00, $00, $00, $03, $00, $00, $00, $08, $00, $00, $00, $50, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00,
      $07, $07, $00, $00, $59, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $0F, $0F, $00, $00, $5F, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $03, $00, $00, $00, $02, $00, $00, $00, $03, $03, $00, $00, $50, $4F, $53, $49, $54, $49, $4F, $4E, $00, $43, $4F, $4C, $4F, $52, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $4F, $53, $47, $4E,
      $6C, $00, $00, $00, $03, $00, $00, $00, $08, $00, $00, $00, $50, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $5C, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $03, $0C, $00, $00, $65, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $02, $00, $00, $00,
      $0F, $00, $00, $00, $53, $56, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $43, $4F, $4C, $4F, $52, $00, $AB], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 64)]
    ),
    //attribute vec2 a_TexCoord0;
    //attribute vec4 a_Color;
    //attribute vec3 a_Position;
    //varying vec4 TEX0;
    //varying vec4 COLOR0;
    //vec4 _o_pos1;
    //vec4 _o_color1;
    //vec2 _o_texcoord01;
    //vec4 _r0003;
    //vec4 _v0003;
    //uniform vec4 _MVPMatrix[4];
    //void main()
    //{
    //    _v0003 = vec4(a_Position.x, a_Position.y, a_Position.z, 1.0);
    //    _r0003.x = dot(_MVPMatrix[0], _v0003);
    //    _r0003.y = dot(_MVPMatrix[1], _v0003);
    //    _r0003.z = dot(_MVPMatrix[2], _v0003);
    //    _r0003.w = dot(_MVPMatrix[3], _v0003);
    //    _o_pos1 = _r0003;
    //    _o_texcoord01 = a_TexCoord0.xy;
    //    _o_color1 = a_Color;
    //    TEX0.xy = a_TexCoord0.xy;
    //    COLOR0 = a_Color;
    //    gl_Position = _r0003;
    //}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $32, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76,
      $65, $63, $34, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $33, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $3B,
      $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $43, $4F, $4C, $4F, $52,
      $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $63, $6F, $6C, $6F, $72, $31, $3B, $0D, $0A, $76, $65, $63, $32,
      $20, $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $76, $30, $30,
      $30, $33, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $34, $20, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $34, $5D, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D,
      $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $5F, $76, $30, $30, $30, $33, $20, $3D, $20, $76, $65, $63, $34, $28, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $78,
      $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $79, $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $7A, $2C, $20, $31, $2E, $30, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $72, $30, $30, $30, $33, $2E, $78, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $30, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $79, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $31, $5D, $2C, $20, $5F, $76, $30, $30, $30,
      $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $7A, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $32, $5D, $2C, $20, $5F,
      $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $77, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $33,
      $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F,
      $63, $6F, $6C, $6F, $72, $31, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $54, $45, $58, $30, $2E, $78, $79, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F,
      $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $43, $4F, $4C, $4F, $52, $30, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $67, $6C, $5F,
      $50, $6F, $73, $69, $74, $69, $6F, $6E, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    )
  ]);
  FPixelShader := TShaderManager.RegisterShaderFromData('cnv_texture.fps', TContextShaderKind.PixelShader, '', [
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $1F, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $53, $00, $00, $00, $00, $02, $FF, $FF, $01, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $4C, $00, $00, $00,
      $30, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00, $74, $65, $78, $74, $75, $72, $65, $30, $00, $AB, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65,
      $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $0F, $90, $1F, $00, $00, $02,
      $00, $00, $00, $90, $00, $08, $0F, $A0, $13, $00, $00, $02, $00, $00, $03, $80, $00, $00, $E4, $B0, $42, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $00, $E4, $90, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $46, $A5, $E7, $2F, $D8, $00, $2D, $BE, $FD, $AB, $93, $DB, $30, $8A, $D3, $32, $01, $00, $00, $00, $40, $03, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $CC, $00, $00, $00,
      $7C, $01, $00, $00, $F8, $01, $00, $00, $98, $02, $00, $00, $0C, $03, $00, $00, $41, $6F, $6E, $39, $8C, $00, $00, $00, $8C, $00, $00, $00, $00, $02, $FF, $FF, $64, $00, $00, $00, $28, $00, $00, $00,
      $00, $00, $28, $00, $00, $00, $28, $00, $00, $00, $28, $00, $01, $00, $24, $00, $00, $00, $28, $00, $00, $00, $00, $00, $00, $02, $FF, $FF, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0,
      $1F, $00, $00, $02, $00, $00, $00, $80, $01, $00, $0F, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $13, $00, $00, $02, $00, $00, $03, $80, $00, $00, $E4, $B0, $42, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80, $01, $00, $E4, $B0, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80,
      $FF, $FF, $00, $00, $53, $48, $44, $52, $A8, $00, $00, $00, $40, $00, $00, $00, $2A, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $00, $00, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00,
      $00, $00, $00, $00, $55, $55, $00, $00, $62, $10, $00, $03, $32, $10, $10, $00, $01, $00, $00, $00, $62, $10, $00, $03, $F2, $10, $10, $00, $02, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00,
      $00, $00, $00, $00, $68, $00, $00, $02, $01, $00, $00, $00, $1A, $00, $00, $05, $32, $00, $10, $00, $00, $00, $00, $00, $46, $10, $10, $00, $01, $00, $00, $00, $45, $00, $00, $09, $F2, $00, $10, $00,
      $00, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $38, $00, $00, $07, $F2, $20, $10, $00, $00, $00, $00, $00,
      $46, $0E, $10, $00, $00, $00, $00, $00, $46, $1E, $10, $00, $02, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00, $04, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00,
      $03, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46, $98, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $02, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FF, $FF, $00, $11, $00, $00, $65, $00, $00, $00, $5C, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00,
      $0C, $00, $00, $00, $74, $65, $78, $74, $75, $72, $65, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43,
      $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $AB, $49, $53, $47, $4E, $6C, $00, $00, $00, $03, $00, $00, $00, $08, $00, $00, $00,
      $50, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $5C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00,
      $01, $00, $00, $00, $03, $03, $00, $00, $65, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $02, $00, $00, $00, $0F, $0F, $00, $00, $53, $56, $5F, $50, $6F, $73, $69, $74,
      $69, $6F, $6E, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $43, $4F, $4C, $4F, $52, $00, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65, $74, $00, $AB, $AB], [
      TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    //varying vec4 COLOR0;
    //varying vec4 TEX0;
    //vec4 _ret_0;
    //float _TMP1;
    //float _TMP0;
    //vec2 _c0008;
    //uniform sampler2D _texture0;
    //void main()
    //{
    //    vec4 _texColor;
    //    _TMP0 = fract(TEX0.x);
    //    _TMP1 = fract(TEX0.y);
    //    _c0008 = vec2(_TMP0, _TMP1);
    //    _texColor = texture2D(_texture0, _c0008);
    //    _ret_0 = _texColor*COLOR0;
    //    gl_FragColor = _ret_0;
    //    return;
    //}
    TContextShaderSource.Create(
      TContextShaderArch.GLSL,
      TEncoding.UTF8.GetBytes(

        'varying vec4 COLOR0;'+
        'varying vec4 TEX0;'+
        'uniform sampler2D _texture0;'+
        'uniform sampler2D _texture1;'+

        'void main()'+
        '{'+

           'mediump vec3 yuv;'+
           'lowp vec3 rgb;'+
           'yuv.x = texture2D(_texture0, TEX0.xy).r;'+
           'yuv.yz = texture2D(_texture1, TEX0.xy).rg - vec2(0.5, 0.5);'+

           // BT.601, which is the standard for SDTV is provided as a reference
           // rgb = mat3(
           //   1, 1, 1,
           //   0, -.34413, 1.772,
           //   1.402, -.71414, 0) * yuv;

           // Using BT.709 which is the standard for HDTV
           // https://www.xaymar.com/2017/07/06/how-to-converting-rgb-to-yuv-and-yuv-to-rgb/
           'rgb = mat3('+
             '1.0, 1.0, 1.0, '+
             '0.0, -0.187324, 1.8556, '+
             '1.5748, -0.468124, 0.0) * yuv;'+

           'gl_FragColor = vec4(rgb, 1) * COLOR0;'+

        '}'

      ),
      [TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0),
       TContextShaderVariable.Create('texture1', TContextShaderVariableKind.Texture, 1, 0)]
    )
  ]);
end;

{*************************************************************************************}
procedure TALCanvas420YpCbCr8PlanarTextureMaterial.SetCbTexture(const Value: TTexture);
begin
  if FCbTexture <> Value then
    FCbTexture := Value;
end;

{*************************************************************************************}
procedure TALCanvas420YpCbCr8PlanarTextureMaterial.SetCrTexture(const Value: TTexture);
begin
  if FCrTexture <> Value then
    FCrTexture := Value;
end;

{************************************************************************************}
procedure TALCanvas420YpCbCr8PlanarTextureMaterial.DoApply(const Context: TContext3D);
begin
  inherited;
  Context.SetShaderVariable('texture1', FCbTexture);
  Context.SetShaderVariable('texture2', FCrTexture);
end;

{************************}
{$IF CompilerVersion > 32}
  {$MESSAGE WARN 'Check if FMX.Materials.Canvas.TCanvasTextureMaterial.DoInitialize is still having the same implementation as in previous version and adjust the IFDEF'}
{$ENDIF}
procedure TALCanvas420YpCbCr8PlanarTextureMaterial.DoInitialize;
begin
  inherited;
  FVertexShader := TShaderManager.RegisterShaderFromData('cnv_texture.fvs', TContextShaderKind.VertexShader, '', [
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FE, $FF, $FE, $FF, $1F, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $53, $00, $00, $00, $00, $02, $FE, $FF, $01, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $4C, $00, $00, $00,
      $30, $00, $00, $00, $02, $00, $00, $00, $04, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $00, $AB, $AB, $03, $00, $03, $00, $04, $00, $04, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $76, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65,
      $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $0F, $90, $1F, $00, $00, $02, $0A, $00, $00, $80, $01, $00, $0F, $90, $1F, $00, $00, $02,
      $05, $00, $00, $80, $02, $00, $0F, $90, $05, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $55, $90, $01, $00, $E4, $A0, $04, $00, $00, $04, $00, $00, $0F, $80, $00, $00, $E4, $A0, $00, $00, $00, $90,
      $00, $00, $E4, $80, $04, $00, $00, $04, $00, $00, $0F, $80, $02, $00, $E4, $A0, $00, $00, $AA, $90, $00, $00, $E4, $80, $02, $00, $00, $03, $00, $00, $0F, $C0, $00, $00, $E4, $80, $03, $00, $E4, $A0,
      $01, $00, $00, $02, $00, $00, $03, $E0, $02, $00, $E4, $90, $01, $00, $00, $02, $00, $00, $0F, $D0, $01, $00, $E4, $90, $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    ),
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $09, $25, $BD, $36, $32, $98, $40, $33, $C7, $18, $0B, $ED, $E6, $C0, $C3, $D4, $01, $00, $00, $00, $80, $04, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $20, $01, $00, $00,
      $50, $02, $00, $00, $CC, $02, $00, $00, $9C, $03, $00, $00, $0C, $04, $00, $00, $41, $6F, $6E, $39, $E0, $00, $00, $00, $E0, $00, $00, $00, $00, $02, $FE, $FF, $AC, $00, $00, $00, $34, $00, $00, $00,
      $01, $00, $24, $00, $00, $00, $30, $00, $00, $00, $30, $00, $00, $00, $24, $00, $01, $00, $30, $00, $00, $00, $00, $00, $04, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $02, $FE, $FF,
      $1F, $00, $00, $02, $05, $00, $00, $80, $00, $00, $0F, $90, $1F, $00, $00, $02, $05, $00, $01, $80, $01, $00, $0F, $90, $1F, $00, $00, $02, $05, $00, $02, $80, $02, $00, $0F, $90, $05, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $55, $90, $02, $00, $E4, $A0, $04, $00, $00, $04, $00, $00, $0F, $80, $01, $00, $E4, $A0, $00, $00, $00, $90, $00, $00, $E4, $80, $04, $00, $00, $04, $00, $00, $0F, $80,
      $03, $00, $E4, $A0, $00, $00, $AA, $90, $00, $00, $E4, $80, $02, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80, $04, $00, $E4, $A0, $04, $00, $00, $04, $00, $00, $03, $C0, $00, $00, $FF, $80,
      $00, $00, $E4, $A0, $00, $00, $E4, $80, $01, $00, $00, $02, $00, $00, $0C, $C0, $00, $00, $E4, $80, $01, $00, $00, $02, $00, $00, $03, $E0, $02, $00, $E4, $90, $01, $00, $00, $02, $01, $00, $0F, $E0,
      $01, $00, $C6, $90, $FF, $FF, $00, $00, $53, $48, $44, $52, $28, $01, $00, $00, $40, $00, $01, $00, $4A, $00, $00, $00, $59, $00, $00, $04, $46, $8E, $20, $00, $00, $00, $00, $00, $04, $00, $00, $00,
      $5F, $00, $00, $03, $72, $10, $10, $00, $00, $00, $00, $00, $5F, $00, $00, $03, $F2, $10, $10, $00, $01, $00, $00, $00, $5F, $00, $00, $03, $32, $10, $10, $00, $02, $00, $00, $00, $67, $00, $00, $04,
      $F2, $20, $10, $00, $00, $00, $00, $00, $01, $00, $00, $00, $65, $00, $00, $03, $32, $20, $10, $00, $01, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00, $02, $00, $00, $00, $68, $00, $00, $02,
      $01, $00, $00, $00, $38, $00, $00, $08, $F2, $00, $10, $00, $00, $00, $00, $00, $56, $15, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $01, $00, $00, $00, $32, $00, $00, $0A,
      $F2, $00, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $06, $10, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $00, $00, $00, $00, $32, $00, $00, $0A,
      $F2, $00, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $02, $00, $00, $00, $A6, $1A, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $00, $00, $00, $00, $00, $00, $00, $08,
      $F2, $20, $10, $00, $00, $00, $00, $00, $46, $0E, $10, $00, $00, $00, $00, $00, $46, $8E, $20, $00, $00, $00, $00, $00, $03, $00, $00, $00, $36, $00, $00, $05, $32, $20, $10, $00, $01, $00, $00, $00,
      $46, $10, $10, $00, $02, $00, $00, $00, $36, $00, $00, $05, $F2, $20, $10, $00, $02, $00, $00, $00, $66, $1C, $10, $00, $01, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00,
      $07, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46,
      $C8, $00, $00, $00, $01, $00, $00, $00, $48, $00, $00, $00, $01, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FE, $FF, $00, $11, $00, $00, $94, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $24, $47, $6C, $6F, $62, $61, $6C, $73, $00, $AB, $AB, $AB, $3C, $00, $00, $00,
      $01, $00, $00, $00, $60, $00, $00, $00, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $78, $00, $00, $00, $00, $00, $00, $00, $40, $00, $00, $00, $02, $00, $00, $00, $84, $00, $00, $00,
      $00, $00, $00, $00, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $00, $AB, $AB, $03, $00, $03, $00, $04, $00, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66,
      $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34,
      $00, $AB, $AB, $AB, $49, $53, $47, $4E, $68, $00, $00, $00, $03, $00, $00, $00, $08, $00, $00, $00, $50, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00,
      $07, $07, $00, $00, $59, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $0F, $0F, $00, $00, $5F, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $03, $00, $00, $00, $02, $00, $00, $00, $03, $03, $00, $00, $50, $4F, $53, $49, $54, $49, $4F, $4E, $00, $43, $4F, $4C, $4F, $52, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $4F, $53, $47, $4E,
      $6C, $00, $00, $00, $03, $00, $00, $00, $08, $00, $00, $00, $50, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $5C, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $03, $0C, $00, $00, $65, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $02, $00, $00, $00,
      $0F, $00, $00, $00, $53, $56, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $43, $4F, $4C, $4F, $52, $00, $AB], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 64)]
    ),
    //attribute vec2 a_TexCoord0;
    //attribute vec4 a_Color;
    //attribute vec3 a_Position;
    //varying vec4 TEX0;
    //varying vec4 COLOR0;
    //vec4 _o_pos1;
    //vec4 _o_color1;
    //vec2 _o_texcoord01;
    //vec4 _r0003;
    //vec4 _v0003;
    //uniform vec4 _MVPMatrix[4];
    //void main()
    //{
    //    _v0003 = vec4(a_Position.x, a_Position.y, a_Position.z, 1.0);
    //    _r0003.x = dot(_MVPMatrix[0], _v0003);
    //    _r0003.y = dot(_MVPMatrix[1], _v0003);
    //    _r0003.z = dot(_MVPMatrix[2], _v0003);
    //    _r0003.w = dot(_MVPMatrix[3], _v0003);
    //    _o_pos1 = _r0003;
    //    _o_texcoord01 = a_TexCoord0.xy;
    //    _o_color1 = a_Color;
    //    TEX0.xy = a_TexCoord0.xy;
    //    COLOR0 = a_Color;
    //    gl_Position = _r0003;
    //}
    TContextShaderSource.Create(TContextShaderArch.GLSL, [
      $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $32, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76,
      $65, $63, $34, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $61, $74, $74, $72, $69, $62, $75, $74, $65, $20, $76, $65, $63, $33, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $3B,
      $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $54, $45, $58, $30, $3B, $0D, $0A, $76, $61, $72, $79, $69, $6E, $67, $20, $76, $65, $63, $34, $20, $43, $4F, $4C, $4F, $52,
      $30, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $6F, $5F, $63, $6F, $6C, $6F, $72, $31, $3B, $0D, $0A, $76, $65, $63, $32,
      $20, $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $76, $65, $63, $34, $20, $5F, $76, $30, $30,
      $30, $33, $3B, $0D, $0A, $75, $6E, $69, $66, $6F, $72, $6D, $20, $76, $65, $63, $34, $20, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $34, $5D, $3B, $0D, $0A, $76, $6F, $69, $64, $20, $6D,
      $61, $69, $6E, $28, $29, $0D, $0A, $7B, $0D, $0A, $20, $20, $20, $20, $5F, $76, $30, $30, $30, $33, $20, $3D, $20, $76, $65, $63, $34, $28, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $78,
      $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $79, $2C, $20, $61, $5F, $50, $6F, $73, $69, $74, $69, $6F, $6E, $2E, $7A, $2C, $20, $31, $2E, $30, $29, $3B, $0D, $0A, $20, $20, $20,
      $20, $5F, $72, $30, $30, $30, $33, $2E, $78, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $30, $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D,
      $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $79, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $31, $5D, $2C, $20, $5F, $76, $30, $30, $30,
      $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $7A, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $32, $5D, $2C, $20, $5F,
      $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $72, $30, $30, $30, $33, $2E, $77, $20, $3D, $20, $64, $6F, $74, $28, $5F, $4D, $56, $50, $4D, $61, $74, $72, $69, $78, $5B, $33,
      $5D, $2C, $20, $5F, $76, $30, $30, $30, $33, $29, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F, $70, $6F, $73, $31, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $20, $20, $20, $20,
      $5F, $6F, $5F, $74, $65, $78, $63, $6F, $6F, $72, $64, $30, $31, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F, $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $5F, $6F, $5F,
      $63, $6F, $6C, $6F, $72, $31, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $54, $45, $58, $30, $2E, $78, $79, $20, $3D, $20, $61, $5F, $54, $65, $78, $43, $6F,
      $6F, $72, $64, $30, $2E, $78, $79, $3B, $0D, $0A, $20, $20, $20, $20, $43, $4F, $4C, $4F, $52, $30, $20, $3D, $20, $61, $5F, $43, $6F, $6C, $6F, $72, $3B, $0D, $0A, $20, $20, $20, $20, $67, $6C, $5F,
      $50, $6F, $73, $69, $74, $69, $6F, $6E, $20, $3D, $20, $5F, $72, $30, $30, $30, $33, $3B, $0D, $0A, $7D, $20, $0D, $0A], [
      TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix, 0, 4)]
    )
  ]);
  FPixelShader := TShaderManager.RegisterShaderFromData('cnv_texture.fps', TContextShaderKind.PixelShader, '', [
    TContextShaderSource.Create(TContextShaderArch.DX9, [
      $00, $02, $FF, $FF, $FE, $FF, $1F, $00, $43, $54, $41, $42, $1C, $00, $00, $00, $53, $00, $00, $00, $00, $02, $FF, $FF, $01, $00, $00, $00, $1C, $00, $00, $00, $00, $01, $00, $20, $4C, $00, $00, $00,
      $30, $00, $00, $00, $03, $00, $00, $00, $01, $00, $00, $00, $3C, $00, $00, $00, $00, $00, $00, $00, $74, $65, $78, $74, $75, $72, $65, $30, $00, $AB, $AB, $AB, $04, $00, $0C, $00, $01, $00, $01, $00,
      $01, $00, $00, $00, $00, $00, $00, $00, $70, $73, $5F, $32, $5F, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $44, $33, $44, $58, $39, $20, $53, $68, $61, $64, $65,
      $72, $20, $43, $6F, $6D, $70, $69, $6C, $65, $72, $20, $00, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $0F, $90, $1F, $00, $00, $02,
      $00, $00, $00, $90, $00, $08, $0F, $A0, $13, $00, $00, $02, $00, $00, $03, $80, $00, $00, $E4, $B0, $42, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $00, $E4, $90, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80, $FF, $FF, $00, $00], [
      TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    TContextShaderSource.Create(TContextShaderArch.DX11_level_9, [
      $44, $58, $42, $43, $46, $A5, $E7, $2F, $D8, $00, $2D, $BE, $FD, $AB, $93, $DB, $30, $8A, $D3, $32, $01, $00, $00, $00, $40, $03, $00, $00, $06, $00, $00, $00, $38, $00, $00, $00, $CC, $00, $00, $00,
      $7C, $01, $00, $00, $F8, $01, $00, $00, $98, $02, $00, $00, $0C, $03, $00, $00, $41, $6F, $6E, $39, $8C, $00, $00, $00, $8C, $00, $00, $00, $00, $02, $FF, $FF, $64, $00, $00, $00, $28, $00, $00, $00,
      $00, $00, $28, $00, $00, $00, $28, $00, $00, $00, $28, $00, $01, $00, $24, $00, $00, $00, $28, $00, $00, $00, $00, $00, $00, $02, $FF, $FF, $1F, $00, $00, $02, $00, $00, $00, $80, $00, $00, $03, $B0,
      $1F, $00, $00, $02, $00, $00, $00, $80, $01, $00, $0F, $B0, $1F, $00, $00, $02, $00, $00, $00, $90, $00, $08, $0F, $A0, $13, $00, $00, $02, $00, $00, $03, $80, $00, $00, $E4, $B0, $42, $00, $00, $03,
      $00, $00, $0F, $80, $00, $00, $E4, $80, $00, $08, $E4, $A0, $05, $00, $00, $03, $00, $00, $0F, $80, $00, $00, $E4, $80, $01, $00, $E4, $B0, $01, $00, $00, $02, $00, $08, $0F, $80, $00, $00, $E4, $80,
      $FF, $FF, $00, $00, $53, $48, $44, $52, $A8, $00, $00, $00, $40, $00, $00, $00, $2A, $00, $00, $00, $5A, $00, $00, $03, $00, $60, $10, $00, $00, $00, $00, $00, $58, $18, $00, $04, $00, $70, $10, $00,
      $00, $00, $00, $00, $55, $55, $00, $00, $62, $10, $00, $03, $32, $10, $10, $00, $01, $00, $00, $00, $62, $10, $00, $03, $F2, $10, $10, $00, $02, $00, $00, $00, $65, $00, $00, $03, $F2, $20, $10, $00,
      $00, $00, $00, $00, $68, $00, $00, $02, $01, $00, $00, $00, $1A, $00, $00, $05, $32, $00, $10, $00, $00, $00, $00, $00, $46, $10, $10, $00, $01, $00, $00, $00, $45, $00, $00, $09, $F2, $00, $10, $00,
      $00, $00, $00, $00, $46, $00, $10, $00, $00, $00, $00, $00, $46, $7E, $10, $00, $00, $00, $00, $00, $00, $60, $10, $00, $00, $00, $00, $00, $38, $00, $00, $07, $F2, $20, $10, $00, $00, $00, $00, $00,
      $46, $0E, $10, $00, $00, $00, $00, $00, $46, $1E, $10, $00, $02, $00, $00, $00, $3E, $00, $00, $01, $53, $54, $41, $54, $74, $00, $00, $00, $04, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00,
      $03, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $52, $44, $45, $46, $98, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $02, $00, $00, $00, $1C, $00, $00, $00, $00, $04, $FF, $FF, $00, $11, $00, $00, $65, $00, $00, $00, $5C, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
      $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $5C, $00, $00, $00, $02, $00, $00, $00, $05, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $01, $00, $00, $00,
      $0C, $00, $00, $00, $74, $65, $78, $74, $75, $72, $65, $30, $00, $4D, $69, $63, $72, $6F, $73, $6F, $66, $74, $20, $28, $52, $29, $20, $48, $4C, $53, $4C, $20, $53, $68, $61, $64, $65, $72, $20, $43,
      $6F, $6D, $70, $69, $6C, $65, $72, $20, $39, $2E, $32, $36, $2E, $39, $35, $32, $2E, $32, $38, $34, $34, $00, $AB, $AB, $49, $53, $47, $4E, $6C, $00, $00, $00, $03, $00, $00, $00, $08, $00, $00, $00,
      $50, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $5C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00,
      $01, $00, $00, $00, $03, $03, $00, $00, $65, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $02, $00, $00, $00, $0F, $0F, $00, $00, $53, $56, $5F, $50, $6F, $73, $69, $74,
      $69, $6F, $6E, $00, $54, $45, $58, $43, $4F, $4F, $52, $44, $00, $43, $4F, $4C, $4F, $52, $00, $AB, $4F, $53, $47, $4E, $2C, $00, $00, $00, $01, $00, $00, $00, $08, $00, $00, $00, $20, $00, $00, $00,
      $00, $00, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $0F, $00, $00, $00, $53, $56, $5F, $54, $61, $72, $67, $65, $74, $00, $AB, $AB], [
      TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0)]
    ),
    //varying vec4 COLOR0;
    //varying vec4 TEX0;
    //vec4 _ret_0;
    //float _TMP1;
    //float _TMP0;
    //vec2 _c0008;
    //uniform sampler2D _texture0;
    //void main()
    //{
    //    vec4 _texColor;
    //    _TMP0 = fract(TEX0.x);
    //    _TMP1 = fract(TEX0.y);
    //    _c0008 = vec2(_TMP0, _TMP1);
    //    _texColor = texture2D(_texture0, _c0008);
    //    _ret_0 = _texColor*COLOR0;
    //    gl_FragColor = _ret_0;
    //    return;
    //}
    TContextShaderSource.Create(
      TContextShaderArch.GLSL,
      TEncoding.UTF8.GetBytes(

        'varying vec4 COLOR0;'+
        'varying vec4 TEX0;'+
        'uniform sampler2D _texture0;'+
        'uniform sampler2D _texture1;'+
        'uniform sampler2D _texture2;'+

        'void main()'+
        '{'+

           'mediump vec3 yuv;'+
           'lowp vec3 rgb;'+
           'yuv.x = texture2D(_texture0, TEX0.xy).r;'+
           'yuv.y = texture2D(_texture1, TEX0.xy).r - 0.5;'+
           'yuv.z = texture2D(_texture2, TEX0.xy).r - 0.5;'+

           // BT.601, which is the standard for SDTV is provided as a reference
           // rgb = mat3(
           //   1, 1, 1,
           //   0, -.34413, 1.772,
           //   1.402, -.71414, 0) * yuv;

           // Using BT.709 which is the standard for HDTV
           // https://www.xaymar.com/2017/07/06/how-to-converting-rgb-to-yuv-and-yuv-to-rgb/
           'rgb = mat3('+
             '1.0, 1.0, 1.0, '+
             '0.0, -0.187324, 1.8556, '+
             '1.5748, -0.468124, 0.0) * yuv;'+

           'gl_FragColor = vec4(rgb, 1) * COLOR0;'+

        '}'

      ),
      [TContextShaderVariable.Create('texture0', TContextShaderVariableKind.Texture, 0, 0),
       TContextShaderVariable.Create('texture1', TContextShaderVariableKind.Texture, 1, 0),
       TContextShaderVariable.Create('texture2', TContextShaderVariableKind.Texture, 2, 0)]
    )
  ]);
end;

initialization

  {$IFDEF DEBUG}
  TotalMemoryUsedByTextures := 0;
  LastTotalMemoryUsedByTexturesLog := 0;
  {$ENDIF}

end.
