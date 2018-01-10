{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016 Embarcadero Technologies, Inc.      }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Context.Mac;

interface

{$SCOPEDENUMS ON}

uses
  Macapi.AppKit, System.Generics.Collections, FMX.Types, FMX.Types3D;

type
  TCustomContextOpenGL = class(TContext3D)
  private type
    TShaderProgram = class
    private
      FVertexShader: THandle;
      FPixelShader: THandle;
      FProgram: THandle;
      FVariables: TDictionary<string, TContextShaderVariable>;
    public
      constructor Create;
      destructor Destroy; override;
    end;
  protected class var
    FSharedContext: NSOpenGLContext;
    FSharedPixelFormat: NSOpenGLPixelFormat;
    FCurrentProgram: TShaderProgram;
    FPrograms: TList<TShaderProgram>;
    class procedure CreateSharedContext;
    class procedure DestroySharedContext;
    class function GetSharedContext: NSOpenGLContext; static;
  public
    class property SharedContext: NSOpenGLContext read GetSharedContext;

    class function Style: TContextStyles; override;
    class function PixelFormat: TPixelFormat; override;
    class function MaxTextureSize: Integer; override;
    class function TextureUnitCount: Integer; override;
  end;

procedure RegisterContextClasses;
procedure UnregisterContextClasses;

implementation

uses
  Macapi.CocoaTypes, Macapi.OpenGL, System.UITypes, System.Types, System.SysUtils, System.Math, System.Math.Vectors,
  Macapi.ObjectiveC, FMX.Consts, FMX.Utils, FMX.Platform, FMX.Forms, FMX.Platform.Mac;

const
  ATTRIB_VERTEX = 0;
  ATTRIB_NORMAL = 1;
  ATTRIB_COLOR0 = 2;
  ATTRIB_COLOR1 = 3;
  ATTRIB_COLOR2 = 4;
  ATTRIB_COLOR3 = 5;
  ATTRIB_TEXCOORD0 = 6;
  ATTRIB_TEXCOORD1 = 7;
  ATTRIB_TEXCOORD2 = 8;
  ATTRIB_TEXCOORD3 = 9;

function GLHasAnyErrors: Boolean;
const
  MaxStopLock = 16;
var
  Flags, Flag, StopLock: Integer;
begin
  Flags := 0;
  StopLock := MaxStopLock;
  repeat
    Flag := glGetError;
    if Flag <> GL_NO_ERROR then
      Flags := Flags or Flag;
    Dec(StopLock);
  until (Flag = GL_NO_ERROR) or (StopLock <= 0);
  Result := Flags <> 0;
end;

{ TShaderProgram }

constructor TCustomContextOpenGL.TShaderProgram.Create;
begin
  inherited ;
  FVariables := TDictionary<string, TContextShaderVariable>.Create;
end;

destructor TCustomContextOpenGL.TShaderProgram.Destroy;
begin
  FVariables.Free;
  inherited;
end;

{ TCustomContextOpenGL }

class procedure TCustomContextOpenGL.CreateSharedContext;
const
  SharedAttributes: array[0..2] of NSOpenGLPixelFormatAttribute = (NSOpenGLPFAAccelerated, NSOpenGLPFADoubleBuffer, 0);
begin
  if FSharedContext = nil then
  begin
    FSharedPixelFormat := TNSOpenGLPixelFormat.Wrap(TNSOpenGLPixelFormat.Alloc.initWithAttributes(@SharedAttributes[0]));
    FSharedContext := TNSOpenGLContext.Wrap(TNSOpenGLContext.Alloc.initWithFormat(FSharedPixelFormat, nil));
    FSharedContext.makeCurrentContext;

    glDisable(GL_LIGHT0);
    glDisable(GL_LIGHT1);
    glDisable(GL_LIGHT2);
    glDisable(GL_LIGHT3);
    glDisable(GL_LIGHT4);
    glDisable(GL_LIGHT5);
    glDisable(GL_LIGHT6);
    glDisable(GL_LIGHT7);

    glEnable(GL_NORMALIZE);
  end;
end;

class procedure TCustomContextOpenGL.DestroySharedContext;
var
  Rec: TShaderProgram;
begin
  if FSharedPixelFormat <> nil then
  begin
    FSharedPixelFormat.release;
    FSharedPixelFormat := nil;
  end;
  if FSharedContext <> nil then
  begin
    if FPrograms <> nil then
    begin
      for Rec in FPrograms do
        glDeleteProgram(Rec.FProgram);
      FreeAndNil(FPrograms);
    end;
    FSharedContext := nil;
  end;
end;

class function TCustomContextOpenGL.GetSharedContext: NSOpenGLContext;
begin
  CreateSharedContext;
  if TNSOpenGLContext.OCClass.currentContext <> (FSharedContext as ILocalObject).GetObjectID then
    FSharedContext.makeCurrentContext;
  Result := FSharedContext;
end;

class function TCustomContextOpenGL.MaxTextureSize: Integer;
begin
  CreateSharedContext;
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @Result);
end;

class function TCustomContextOpenGL.PixelFormat: TPixelFormat;
begin
  Result := TPixelFormat.RGBA;
end;

class function TCustomContextOpenGL.Style: TContextStyles;
begin
  Result := [TContextStyle.RenderTargetFlipped];
end;

class function TCustomContextOpenGL.TextureUnitCount: Integer;
begin
  CreateSharedContext;
  glGetIntegerv(GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS, @Result);
end;

type
  TContextOpenGL = class(TCustomContextOpenGL)
  private
    { buffers }
    FRenderBuf: GLuint;
    FFrameBuf: GLuint;
    FDepthBuf: GLuint;
    FContextObject: NSOpenGLContext;
    { bitmap }
    FBitmapBuffer: PAlphaColorArray;
    FBitmapBufferLen: Integer;
    { MS }
    FSupportMS: Boolean;
    FMSValue: Integer;
    FRenderBufMS: GLuint;
    FFrameBufMS: GLuint;
    FDepthBufMS: GLuint;
    { States }
    FOldFBO: GLuint;
  protected
    class function BuildShader(AType: Integer; ACode: TContextShaderCode): Integer;
    class function FindProgram(const VS, PS: TContextShader): TCustomContextOpenGL.TShaderProgram; static;
    class procedure UseProgram(const VS, PS: TContextShader); static;
  protected
    function GetValid: Boolean; override;
    { buffer }
    procedure DoCreateBuffer; override;
    procedure DoResize; override;
    procedure DoFreeBuffer; override;
    procedure DoCopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect); override;
    { scene }
    function DoBeginScene: Boolean; override;
    procedure DoEndScene; override;
    { states }
    procedure DoClear(const ATarget: TClearTargets; const AColor: TAlphaColor; const ADepth: Single;
      const AStencil: Cardinal); override;
    procedure DoSetContextState(AState: TContextState); override;
    procedure DoSetStencilOp(const Fail, ZFail, ZPass: TStencilOp); override;
    procedure DoSetStencilFunc(const Func: TStencilfunc; Ref, Mask: Cardinal); override;
    procedure DoSetScissorRect(const ScissorRect: TRect); override;
    { drawing }
    procedure DoDrawPrimitivesBatch(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
      const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize,
      IndexCount: Integer); override;
    { texture }
    class procedure DoInitializeTexture(const Texture: TTexture); override;
    class procedure DoFinalizeTexture(const Texture: TTexture); override;
    class procedure DoUpdateTexture(const Texture: TTexture; const Bits: Pointer; const Pitch: Integer); override;
    { shaders }
    class procedure DoInitializeShader(const Shader: TContextShader); override;
    class procedure DoFinalizeShader(const Shader: TContextShader); override;
    procedure DoSetShaders(const VertexShader, PixelShader: TContextShader); override;
    procedure DoSetShaderVariable(const Name: string; const Data: array of TVector3D); override;
    procedure DoSetShaderVariable(const Name: string; const Texture: TTexture); override;
    procedure DoSetShaderVariable(const Name: string; const Matrix: TMatrix3D); override;
    { constructors }
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AMultisample: TMultisample; const ADepthStencil: Boolean); override;
    constructor CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
      const ADepthStencil: Boolean); override;
  public
    destructor Destroy; override;
    property ContextObject: NSOpenGLContext read FContextObject;
  end;

function PixelFormatToInternalFormat(Source: TPixelFormat): Integer;
begin
  case Source of
    TPixelFormat.RGBA16:
      Result := GL_RGBA16;
    TPixelFormat.RGB10_A2:
      Result := GL_RGB10_A2;
    TPixelFormat.BGR10_A2:
      Result := GL_RGB10_A2;
    TPixelFormat.BGRA:
      Result := GL_RGBA8;
    TPixelFormat.BGR:
      Result := GL_RGB8;
    TPixelFormat.RGBA:
      Result := GL_RGBA8;
    TPixelFormat.RGB:
      Result := GL_RGB8;
    TPixelFormat.BGR_565:
      Result := GL_RGB8;
    TPixelFormat.BGRA4:
      Result := GL_RGBA4;
    TPixelFormat.BGR5_A1:
      Result := GL_RGB5_A1;
    TPixelFormat.BGR5:
      Result := GL_RGB5;
    TPixelFormat.LA:
      Result := GL_LUMINANCE8_ALPHA8;
    TPixelFormat.LA4:
      Result := GL_LUMINANCE4_ALPHA4;
    TPixelFormat.R16F:
      Result := GL_R16F;
    TPixelFormat.RG16F:
      Result := GL_RG16F;
    TPixelFormat.RGBA16F:
      Result := GL_RGBA_FLOAT16_APPLE;
    TPixelFormat.R32F:
      Result := GL_R32F;
    TPixelFormat.RG32F:
      Result := GL_RG32F;
    TPixelFormat.RGBA32F:
      Result := GL_RGBA_FLOAT32_APPLE;
    TPixelFormat.A:
      Result := GL_ALPHA8;
  else
    Result := -1;
  end;
end;

function PixelFormatToFormat(Source: TPixelFormat): Integer;
begin
  case Source of
    TPixelFormat.RGBA16:
      Result := GL_RGBA;
    TPixelFormat.RGB10_A2:
      Result := GL_RGBA;
    TPixelFormat.BGR10_A2:
      Result := GL_BGRA;
    TPixelFormat.BGRA:
      Result := GL_BGRA;
    TPixelFormat.BGR:
      Result := GL_BGR;
    TPixelFormat.RGBA:
      Result := GL_RGBA;
    TPixelFormat.RGB:
      Result := GL_RGB;
    TPixelFormat.BGR_565:
      Result := GL_BGR;
    TPixelFormat.BGRA4:
      Result := GL_BGRA;
    TPixelFormat.BGR5_A1:
      Result := GL_BGRA;
    TPixelFormat.BGR5:
      Result := GL_BGR;
    TPixelFormat.LA:
      Result := GL_LUMINANCE_ALPHA;
    TPixelFormat.LA4:
      Result := GL_LUMINANCE_ALPHA;
    TPixelFormat.R16F:
      Result := GL_RED;
    TPixelFormat.RG16F:
      Result := GL_RG;
    TPixelFormat.RGBA16F:
      Result := GL_RGBA;
    TPixelFormat.R32F:
      Result := GL_RED;
    TPixelFormat.RG32F:
      Result := GL_RG;
    TPixelFormat.RGBA32F:
      Result := GL_RGBA;
    TPixelFormat.A:
      Result := GL_ALPHA;
  else
    Result := -1;
  end;
end;

function PixelFormatToType(Source: TPixelFormat): Integer;
begin
  case Source of
    TPixelFormat.RGBA16:
      Result := GL_UNSIGNED_SHORT;
    TPixelFormat.RGB10_A2:
      Result := GL_UNSIGNED_INT_2_10_10_10_REV;
    TPixelFormat.BGR10_A2:
      Result := GL_UNSIGNED_INT_2_10_10_10_REV;
    TPixelFormat.BGRA:
      Result := GL_UNSIGNED_INT_8_8_8_8_REV;
    TPixelFormat.BGR:
      Result := GL_UNSIGNED_INT_8_8_8_8_REV;
    TPixelFormat.RGBA:
      Result := GL_UNSIGNED_INT_8_8_8_8_REV;
    TPixelFormat.RGB:
      Result := GL_UNSIGNED_INT_8_8_8_8_REV;
    TPixelFormat.BGR_565:
      Result :=  GL_UNSIGNED_SHORT_5_6_5;
    TPixelFormat.BGRA4:
      Result :=  GL_UNSIGNED_SHORT_4_4_4_4;
    TPixelFormat.BGR5_A1:
      Result := GL_UNSIGNED_SHORT_1_5_5_5_REV;
    TPixelFormat.BGR5:
      Result := GL_UNSIGNED_SHORT_1_5_5_5_REV;
    TPixelFormat.LA:
      Result := GL_UNSIGNED_BYTE;
    TPixelFormat.LA4:
      Result := -1;
    TPixelFormat.R16F:
      Result := GL_FLOAT;
    TPixelFormat.RG16F:
      Result := GL_FLOAT;
    TPixelFormat.RGBA16F:
      Result := GL_FLOAT;
    TPixelFormat.R32F:
      Result := GL_FLOAT;
    TPixelFormat.RG32F:
      Result := GL_FLOAT;
    TPixelFormat.RGBA32F:
      Result := GL_FLOAT;
    TPixelFormat.A:
      Result := GL_UNSIGNED_BYTE;
  else
    Result := -1;
  end;
end;

{ TContextOpenGL }

constructor TContextOpenGL.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AMultisample: TMultisample; const ADepthStencil: Boolean);
type
  TFormatAttributes = array of NSOpenGLPixelFormatAttribute;

  procedure AddAttributes(var ContextAttributes: TFormatAttributes;
    const Attributes: array of NSOpenGLPixelFormatAttribute);
  var
    I, Index: Integer;
  begin
    Index := Length(ContextAttributes);
    SetLength(ContextAttributes, Index + Length(Attributes));

    for I := 0 to Length(Attributes) - 1 do
      ContextAttributes[Index + I] := Attributes[I];
  end;

var
  Multisamples, ColorBits, DepthBits: Integer;
  Stencil: Boolean;
  RenderingSetupService: IFMXRenderingSetupService;
  Attributes: TFormatAttributes;
  LPixelFormat: NSOpenGLPixelFormat;
  LWindowService: IFMXWindowService;
  PaintControl: IPaintControl;
begin
  inherited;
  SharedContext;

  Multisamples := Ord(AMultisample) * 2;
  ColorBits := 0; // zero color bits mean use default values
  DepthBits := 24;
  Stencil := ADepthStencil;

  if TPlatformServices.Current.SupportsPlatformService(IFMXRenderingSetupService, RenderingSetupService) then
    RenderingSetupService.Invoke(ColorBits, DepthBits, Stencil, Multisamples);

  Attributes := [NSOpenGLPFAAccelerated, NSOpenGLPFADoubleBuffer];
  if ColorBits <> 0 then
    AddAttributes(Attributes, [NSOpenGLPFAColorSize, ColorBits]);
  if DepthBits > 0 then
    AddAttributes(Attributes, [NSOpenGLPFADepthSize, DepthBits]);
  if Stencil then
    AddAttributes(Attributes, [NSOpenGLPFAStencilSize, 8]);
  if Multisamples > 0 then
    AddAttributes(Attributes, [NSOpenGLPFAMultisample, NSOpenGLPFASampleBuffers, 1, NSOpenGLPFASamples, Multisamples]);
  AddAttributes(Attributes, [0]);

  LPixelFormat := TNSOpenGLPixelFormat.Wrap(TNSOpenGLPixelFormat.Alloc.initWithAttributes(@Attributes[0]));

  FContextObject := TNSOpenGLContext.Wrap(TNSOpenGLContext.Alloc.initWithFormat(LPixelFormat, FSharedContext));
  if TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, LWindowService) and
    Supports(LWindowService.FindForm(Parent), IPaintControl, PaintControl) then
    TNSOpenGLView.Wrap(Pointer(PaintControl.ContextHandle)).setOpenGLContext(FContextObject);
  CreateBuffer;
end;

constructor TContextOpenGL.CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
  const ADepthStencil: Boolean);
begin
  inherited;
  {$WARNINGS OFF}
  FSupportMS := Pos('gl_ext_framebuffer_multisample', LowerCase(MarshaledAString(glGetString(GL_EXTENSIONS)))) > 0;
  {$WARNINGS ON}
  if Multisample <> TMultisample.TwoSamples then
    FMSValue := 2
  else
    FMSValue := 4;
  CreateBuffer;
end;

destructor TContextOpenGL.Destroy;
begin
  if FBitmapBuffer <> nil then
    FreeMem(FBitmapBuffer);
  FContextObject := nil;
  inherited;
end;

procedure TContextOpenGL.DoCreateBuffer;
var
  Status: Integer;
  OldFBO: GLuint;
begin
  if (FSharedContext <> nil) and (Texture <> nil) then
  begin
    { create buffers }
    if (Multisample <> TMultisample.None) and FSupportMS then
    begin
      glGetIntegerv(GL_FRAMEBUFFER_BINDING, @OldFBO);
      glGenFramebuffersEXT(1, @FFrameBuf);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FFrameBuf);

      glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, Texture.Handle, 0);

      if DepthStencil then
      begin
        glGenRenderbuffersEXT(1, @FDepthBuf);
        glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, FDepthBuf);
        glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH24_STENCIL8_EXT, Width, Height);
        glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, FDepthBuf);
        glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_STENCIL_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, FDepthBuf);
        glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
      end;

      Status := glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
      if Status <> GL_FRAMEBUFFER_COMPLETE_EXT then
        EContext3DException.CreateResFmt(@SCannotCreateRenderBuffers, [ClassName]);

      { MS }
      glGenFramebuffersEXT(1, @FFrameBufMS);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FFrameBufMS);
      glGenRenderbuffersEXT(1, @FRenderBufMS);
      glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, FRenderBufMS);
      glRenderbufferStorageMultisampleEXT(GL_RENDERBUFFER_EXT, FMSValue, GL_RGBA, Width, Height);
      glFrameBufferRenderBufferEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_RENDERBUFFER_EXT, FRenderBufMS);
      if DepthStencil then
      begin
        glGenRenderbuffersEXT(1, @FDepthBufMS);
        glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, FDepthBufMS);
        glRenderbufferStorageMultisampleEXT(GL_RENDERBUFFER_EXT, FMSValue, GL_DEPTH24_STENCIL8_EXT, Width, Height);
        glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, FDepthBufMS);
        glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_STENCIL_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, FDepthBufMS);
        glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
      end;
      Status := glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
      if Status <> GL_FRAMEBUFFER_COMPLETE_EXT then
        EContext3DException.CreateResFmt(@SCannotCreateRenderBuffers, [ClassName]);
      glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);

      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, OldFBO);
    end
    else
    begin
      glGetIntegerv(GL_FRAMEBUFFER_BINDING, @OldFBO);
      glGenFramebuffersEXT(1, @FFrameBuf);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FFrameBuf);

      glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, Texture.Handle, 0);

      if DepthStencil then
      begin
        glGenRenderbuffersEXT(1, @FDepthBuf);
        glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, FDepthBuf);
        glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH24_STENCIL8_EXT, Width, Height);
        glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, FDepthBuf);
        glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_STENCIL_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, FDepthBuf);
        glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
      end;

      Status := glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
      if Status <> GL_FRAMEBUFFER_COMPLETE_EXT then
        EContext3DException.CreateResFmt(@SCannotCreateRenderBuffers, [ClassName]);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, OldFBO);
    end;
    if (GLHasAnyErrors()) then
        EContext3DException.CreateResFmt(@SCannotCreateRenderBuffers, [ClassName]);
  end;
end;

procedure TContextOpenGL.DoResize;
var
  Status: Integer;
  OldFBO: GLuint;
begin
  if Valid then
  begin
    if FFrameBuf <> 0 then
    begin
      glGetIntegerv(GL_FRAMEBUFFER_BINDING, @OldFBO);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FFrameBuf);
      if FRenderBuf <> 0 then
      begin
        glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, FRenderBuf);
        glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_RGBA, Width, Height);
      end;
      if FDepthBuf <> 0 then
      begin
        glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, FDepthBuf);
        glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH24_STENCIL8_EXT, Width, Height);
      end;
      Status := glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
      if Status <> GL_FRAMEBUFFER_COMPLETE_EXT then
        EContext3DException.CreateResFmt(@SCannotCreateRenderBuffers, [ClassName]);
      if FFrameBufMS <> 0 then
      begin
        glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FFrameBufMS);
        glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, FRenderBufMS);
        glRenderbufferStorageMultisampleEXT(GL_RENDERBUFFER_EXT, FMSValue, GL_RGBA, Width, Height);
        if FDepthBufMS <> 0 then
        begin
          glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, FDepthBufMS);
          glRenderbufferStorageMultisampleEXT(GL_RENDERBUFFER_EXT, FMSValue, GL_DEPTH24_STENCIL8_EXT, Width, Height);
        end;
        Status := glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
        if Status <> GL_FRAMEBUFFER_COMPLETE_EXT then
          EContext3DException.CreateResFmt(@SCannotCreateRenderBuffers, [ClassName]);
      end;
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, OldFBO);
      if (GLHasAnyErrors()) then
        EContext3DException.CreateResFmt(@SCannotCreateRenderBuffers, [ClassName]);
    end
    else
      FContextObject.update;
  end;
end;

procedure TContextOpenGL.DoFreeBuffer;
begin
  if FFrameBuf <> 0 then
  begin
    if FDepthBufMS <> 0 then
      glDeleteRenderbuffersEXT(1, @FDepthBufMS);
    if FRenderBufMS <> 0 then
       glDeleteRenderbuffersEXT(1, @FRenderBufMS);
    if FFrameBufMS <> 0 then
       glDeleteFramebuffersEXT(1, @FFrameBufMS);
    FDepthBufMS := 0;
    FFrameBufMS := 0;
    FRenderBufMS := 0;
    if FDepthBuf <> 0 then
      glDeleteRenderbuffersEXT(1, @FDepthBuf);
    if FRenderBuf <> 0 then
      glDeleteRenderbuffersEXT(1, @FRenderBuf);
    if FFrameBuf <> 0 then
      glDeleteFramebuffersEXT(1, @FFrameBuf);
  end;
end;

function TContextOpenGL.GetValid: Boolean;
begin
  Result := (SharedContext <> nil) and ((Texture <> nil) and (FFrameBuf <> 0)) or (FContextObject <> nil);
end;

function TContextOpenGL.DoBeginScene: Boolean;
begin
  Result := False;
  if Valid then
  begin
    glPushAttrib(GL_VIEWPORT_BIT);
    if FFrameBuf <> 0 then
    begin
      glGetIntegerv(GL_FRAMEBUFFER_BINDING, @FOldFBO);
      if FFrameBufMS <> 0 then
        glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FFrameBufMS)
      else
        glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FFrameBuf);
    end
    else
      FContextObject.makeCurrentContext;

    if Texture <> nil then
      glViewport(0, 0, Width, Height)
    else
      glViewport(0, 0, Round(Width * Scale), Round(Height * Scale));

    Result := inherited DoBeginScene;

    if (GLHasAnyErrors()) then
      EContext3DException.CreateResFmt(@SCannotBeginRenderingScene, [ClassName]);
  end;
end;

procedure TContextOpenGL.DoEndScene;
begin
  if Valid then
  begin
    if FFrameBuf <> 0 then
    begin
      if FFrameBufMS <> 0 then
      begin
        glBindFramebufferEXT(GL_READ_FRAMEBUFFER_EXT, FFrameBufMS);
        glBindFramebufferEXT(GL_DRAW_FRAMEBUFFER_EXT, FFrameBuf);
        glBlitFramebufferEXT(0, 0, Texture.Width, Texture.Height, 0, 0, Texture.Width, Texture.Height, GL_COLOR_BUFFER_BIT, GL_NEAREST);
        glBindFramebufferEXT(GL_READ_FRAMEBUFFER_EXT, 0);
        glBindFramebufferEXT(GL_DRAW_FRAMEBUFFER_EXT, 0);
      end;
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FOldFBO);
    end
    else
      FContextObject.flushBuffer;
    glPopAttrib;
  end;
  inherited ;
end;

procedure TContextOpenGL.DoSetContextState(AState: TContextState);
begin
  if Valid then
  begin
    case AState of
      TContextState.csZTestOn:
        begin
          glEnable(GL_DEPTH_TEST);
          glDepthFunc(GL_LEQUAL);
          glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
        end;
      TContextState.csZTestOff: glDisable(GL_DEPTH_TEST);
      TContextState.csZWriteOn: glDepthMask(1);
      TContextState.csZWriteOff: glDepthMask(0);
      TContextState.csAlphaBlendOn:
        begin
          glEnable(GL_BLEND);
          glEnable(GL_ALPHA_TEST);
          glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
        end;
      TContextState.csAlphaBlendOff:
        begin
          glDisable(GL_BLEND);
          glDisable(GL_ALPHA_TEST);
        end;
      TContextState.csStencilOn: glEnable(GL_STENCIL_TEST);
      TContextState.csStencilOff: glDisable(GL_STENCIL_TEST);
      TContextState.csColorWriteOn: glColorMask(1, 1, 1, 1);
      TContextState.csColorWriteOff: glColorMask(0, 0, 0, 0);
      TContextState.csScissorOn: glEnable(GL_SCISSOR_TEST);
      TContextState.csScissorOff: glDisable(GL_SCISSOR_TEST);
      TContextState.csFrontFace:
        begin
          glFrontFace(GL_CW);
          glCullFace(GL_BACK);
          glEnable(GL_CULL_FACE);
        end;
      TContextState.csBackFace:
        begin
          glFrontFace(GL_CW);
          glCullFace(GL_FRONT);
          glEnable(GL_CULL_FACE);
        end;
      TContextState.csAllFace:
        glDisable(GL_CULL_FACE);
    end;
    if (GLHasAnyErrors()) then
      EContext3DException.CreateResFmt(@SErrorInContextMethod, ['DoSetContextState']);
  end;
end;

procedure TContextOpenGL.DoClear(const ATarget: TClearTargets; const AColor: TAlphaColor; const ADepth: single; const AStencil: Cardinal);
var
  Flags: Integer;
  SaveMask: GLint;
begin
  if Valid then
  begin
    Flags := 0;
    if DepthStencil and (TClearTarget.Depth in ATarget) then
    begin
      Flags := Flags or GL_DEPTH_BUFFER_BIT;
      glGetIntegerv(GL_DEPTH_WRITEMASK, @SaveMask);
      glDepthMask(1);
      glClearDepth(ADepth);
    end;
    if DepthStencil and (TClearTarget.Stencil in ATarget) then
    begin
      Flags := Flags or GL_STENCIL_BUFFER_BIT;
      glClearStencil(AStencil);
    end;
    if (TClearTarget.Color in ATarget) then
    begin
      Flags := Flags or GL_COLOR_BUFFER_BIT;
      glClearColor(TAlphaColorRec(AColor).R / $FF, TAlphaColorRec(AColor).G / $FF, TAlphaColorRec(AColor).B / $FF, TAlphaColorRec(AColor).A / $FF);
    end;

    glClear(Flags);

    if DepthStencil and (TClearTarget.Depth in ATarget) then
      glDepthMask(SaveMask);

    if (GLHasAnyErrors()) then
      EContext3DException.CreateResFmt(@SErrorInContextMethod, ['DoClear']);
  end;
end;

procedure TContextOpenGL.DoCopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect);
var
  I: integer;
  OldFBO: GLuint;
begin
  if Valid then
  begin
    if FBitmapBuffer = nil then
    begin
      FBitmapBufferLen := Width * Height * 4;
      GetMem(FBitmapBuffer, FBitmapBufferLen);
    end;
    if FBitmapBufferLen <> Width * Height * 4 then
    begin
      FreeMem(FBitmapBuffer);
      FBitmapBufferLen := Width * Height * 4;
      GetMem(FBitmapBuffer, FBitmapBufferLen);
    end;
    if Texture <> nil then
    begin
      glBindTexture(GL_TEXTURE_2D, Texture.Handle);
      glGetTexImage(GL_TEXTURE_2D, 0, PixelFormatToFormat(TPixelFormat.RGBA), GL_UNSIGNED_BYTE, FBitmapBuffer);

      for I := ARect.Top to ARect.Bottom - 1 do
        Move(FBitmapBuffer[(Height - 1 - I) * Width + ARect.Left], PAlphaColorArray(Bits)[I * (Pitch div 4) + ARect.Left], ARect.Width * 4);
      glBindTexture(GL_TEXTURE_2D, 0);
    end
    else
    begin
      glGetIntegerv(GL_FRAMEBUFFER_BINDING, @OldFBO);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FFrameBuf);
      glReadPixels(0, 0, Width, Height, PixelFormatToFormat(TPixelFormat.RGBA), GL_UNSIGNED_BYTE, FBitmapBuffer);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, OldFBO);

      for I := ARect.Top to ARect.Bottom - 1 do
        Move(FBitmapBuffer[(Height - 1 - I) * Width + ARect.Left], PAlphaColorArray(Bits)[I * (Pitch div 4) + ARect.Left], ARect.Width * 4);
    end;
  end;
end;

{ drawing }

procedure TContextOpenGL.DoDrawPrimitivesBatch(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
  const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize, IndexCount: Integer);
var
  Element: TVertexElement;
  Mode: GLEnum;
begin
  {$WARNINGS OFF}
  if Valid then
  begin
    for Element in VertexDeclaration do
    begin
      case Element.Format of
        TVertexFormat.Vertex:
          begin
            glVertexAttribPointer(ATTRIB_VERTEX, 3, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_VERTEX);
          end;
        TVertexFormat.Normal:
          begin
            glVertexAttribPointer(ATTRIB_NORMAL, 3, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_NORMAL);
          end;
        TVertexFormat.TexCoord0:
          begin
            glVertexAttribPointer(ATTRIB_TEXCOORD0, 2, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_TEXCOORD0);
          end;
        TVertexFormat.TexCoord1:
          begin
            glVertexAttribPointer(ATTRIB_TEXCOORD1, 2, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_TEXCOORD1);
          end;
        TVertexFormat.TexCoord2:
          begin
            glVertexAttribPointer(ATTRIB_TEXCOORD2, 2, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_TEXCOORD2);
          end;
        TVertexFormat.TexCoord3:
          begin
            glVertexAttribPointer(ATTRIB_TEXCOORD3, 2, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_TEXCOORD3);
          end;
        TVertexFormat.Color0:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_UNSIGNED_BYTE, GL_TRUE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
        TVertexFormat.Color1:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_UNSIGNED_BYTE, GL_TRUE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
        TVertexFormat.Color2:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_UNSIGNED_BYTE, GL_TRUE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
        TVertexFormat.Color3:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_UNSIGNED_BYTE, GL_TRUE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
        TVertexFormat.ColorF0:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
        TVertexFormat.ColorF1:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
        TVertexFormat.ColorF2:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
        TVertexFormat.ColorF3:
          begin
            glVertexAttribPointer(ATTRIB_COLOR0, 4, GL_FLOAT, GL_FALSE, VertexSize, Pointer(NativeInt(Vertices) + Element.Offset));
            glEnableVertexAttribArray(ATTRIB_COLOR0);
          end;
      end;
    end;

    case AKind of
      TPrimitivesKind.Points: Mode := GL_POINTS;
      TPrimitivesKind.Lines: Mode := GL_LINES;
    else
      Mode := GL_TRIANGLES;
    end;

    if IndexSize = 4 then
      glDrawElements(Mode, IndexCount, GL_UNSIGNED_INT, Indices)
    else
      glDrawElements(Mode, IndexCount, GL_UNSIGNED_SHORT, Indices);

    glDisableVertexAttribArray(ATTRIB_VERTEX);
    glDisableVertexAttribArray(ATTRIB_NORMAL);
    glDisableVertexAttribArray(ATTRIB_COLOR0);
    glDisableVertexAttribArray(ATTRIB_COLOR1);
    glDisableVertexAttribArray(ATTRIB_COLOR2);
    glDisableVertexAttribArray(ATTRIB_COLOR3);
    glDisableVertexAttribArray(ATTRIB_TEXCOORD0);
    glDisableVertexAttribArray(ATTRIB_TEXCOORD1);
    glDisableVertexAttribArray(ATTRIB_TEXCOORD2);
    glDisableVertexAttribArray(ATTRIB_TEXCOORD3);
    if (GLHasAnyErrors()) then
      EContext3DException.CreateResFmt(@SErrorInContextMethod, ['DoDrawPrimitive']);
  {$WARNINGS ON}
  end;
end;

procedure TContextOpenGL.DoSetStencilOp(const Fail, ZFail, ZPass: TStencilOp);
var
  gFail, gZFail, gZPass: GLenum;
begin
  if Valid then
  begin
    case Fail of
      TStencilOp.Keep: gFail := GL_KEEP;
      TStencilOp.Zero: gFail := GL_ZERO;
      TStencilOp.Replace: gFail := GL_REPLACE;
      TStencilOp.Increase: gFail := GL_INCR;
      TStencilOp.Decrease: gFail := GL_DECR;
      TStencilOp.Invert: gFail := GL_INVERT;
    else
      gFail := GL_INVERT;
    end;
    case ZFail of
      TStencilOp.Keep: gZFail := GL_KEEP;
      TStencilOp.Zero: gZFail := GL_ZERO;
      TStencilOp.Replace: gZFail := GL_REPLACE;
      TStencilOp.Increase: gZFail := GL_INCR;
      TStencilOp.Decrease: gZFail := GL_DECR;
      TStencilOp.Invert: gZFail := GL_INVERT;
    else
      gZFail := GL_INVERT;
    end;
    case ZPass of
      TStencilOp.Keep: gZPass := GL_KEEP;
      TStencilOp.Zero: gZPass := GL_ZERO;
      TStencilOp.Replace: gZPass := GL_REPLACE;
      TStencilOp.Increase: gZPass := GL_INCR;
      TStencilOp.Decrease: gZPass := GL_DECR;
      TStencilOp.Invert: gZPass := GL_INVERT;
    else
      gZPass := GL_INVERT;
    end;
    glStencilOp(gFail, gZFail, gZPass);
    if (GLHasAnyErrors()) then
      EContext3DException.CreateResFmt(@SErrorInContextMethod, ['DoSetStencilOp']);
  end;
end;

procedure TContextOpenGL.DoSetStencilFunc(const Func: TStencilfunc; Ref, Mask: cardinal);
var
  gFunc: GLenum;
begin
  if Valid then
  begin
    case Func of
      TStencilFunc.Never: gFunc := GL_NEVER;
      TStencilFunc.Less: gFunc := GL_LESS;
      TStencilFunc.Lequal: gFunc := GL_LEQUAL;
      TStencilFunc.Greater: gFunc := GL_GREATER;
      TStencilFunc.Gequal: gFunc := GL_GEQUAL;
      TStencilFunc.Equal: gFunc := GL_EQUAL;
      TStencilFunc.NotEqual: gFunc := GL_NOTEQUAL;
      TStencilFunc.Always: gFunc := GL_ALWAYS;
    else
      gFunc := GL_ALWAYS;
    end;
    glStencilFunc(gFunc, Ref, Mask);
    if (GLHasAnyErrors()) then
      EContext3DException.CreateResFmt(@SErrorInContextMethod, ['DoSetStencilFunc']);
  end;
end;

{ Textures }

class procedure TContextOpenGL.DoInitializeTexture(const Texture: TTexture);
var
  Tex: THandle;
begin
  if SharedContext <> nil then
  begin
    if Texture.PixelFormat = TPixelFormat.None then
      Texture.PixelFormat := TPixelFormat.RGBA;
    glGenTextures(1, @Tex);
    glBindTexture(GL_TEXTURE_2D, Tex);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    case Texture.MagFilter of
      TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    end;
    if TTextureStyle.MipMaps in Texture.Style then
    begin
      case Texture.MinFilter of
        TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST);
        TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      end;
    end
    else
    begin
      case Texture.MinFilter of
        TTextureFilter.Nearest: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        TTextureFilter.Linear: glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      end;
    end;
    glTexImage2D(GL_TEXTURE_2D, 0, PixelFormatToInternalFormat(Texture.PixelFormat), Texture.Width, Texture.Height, 0,
      PixelFormatToFormat(Texture.PixelFormat), PixelFormatToType(Texture.PixelFormat), nil);

    if TTextureStyle.MipMaps in Texture.Style then
      glGenerateMipmapEXT(GL_TEXTURE_2D);

    glBindTexture(GL_TEXTURE_2D, 0);
    ITextureAccess(Texture).Handle := Tex;
    if (GLHasAnyErrors()) then
      EContext3DException.CreateResFmt(@SCannotCreateTexture, [ClassName]);
  end;
end;

class procedure TContextOpenGL.DoFinalizeTexture(const Texture: TTexture);
begin
  if FSharedContext <> nil then
    glDeleteTextures(1, @Texture.Handle);
  ITextureAccess(Texture).Handle := 0;
end;

class procedure TContextOpenGL.DoUpdateTexture(const Texture: TTexture; const Bits: Pointer; const Pitch: Integer);
begin
  if FSharedContext <> nil then
  begin
    glBindTexture(GL_TEXTURE_2D, Texture.Handle);
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, Texture.Width, Texture.Height, PixelFormatToFormat(Texture.PixelFormat),
      PixelFormatToType(Texture.PixelFormat), Bits);
    if TTextureStyle.MipMaps in Texture.Style then
      glGenerateMipmapEXT(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, 0);
    if (GLHasAnyErrors()) then
      EContext3DException.CreateResFmt(@SCannotUploadTexture, [ClassName]);
  end;
end;

class function TContextOpenGL.BuildShader(AType: Integer; ACode: TContextShaderCode): Integer;
var
  log: array of Byte;
  len, compiled: Integer;
begin
  Result := 0;
  if SharedContext <> nil then
  begin
    Result := glCreateShader(AType);
    len := Length(ACode);
    glShaderSource(Result, 1, @ACode, @len);
    glCompileShader(Result);
    glGetShaderiv(Result, GL_COMPILE_STATUS, @compiled);
    if compiled = 0 then
    begin
      glGetShaderiv(Result, GL_INFO_LOG_LENGTH, @compiled);
      if (compiled > 0) then
      begin
        SetLength(log, compiled);
        glGetShaderInfoLog(Result, compiled, @compiled, MarshaledAString(log));
        if AType = GL_VERTEX_SHADER then
          EContext3DException.CreateResFmt(@SCannotCreateVertexShader, [ClassName])
        else
          EContext3DException.CreateResFmt(@SCannotCreatePixelShader, [ClassName]);
      end;
    end;
    if (GLHasAnyErrors()) then
      EContext3DException.CreateResFmt(@SCannotCreateShader, [ClassName]);
  end
end;

function GetUniformSize(const ShaderProgram: TCustomContextOpenGL.TShaderProgram; const Name: string): Integer;
var
  I: Integer;
  UniformCount: Integer;
  UniformSize, UniformType: Integer;
  UniformName: array of Byte;
begin
  glGetProgramiv(ShaderProgram.FProgram, GL_ACTIVE_UNIFORMS, @UniformCount);

  SetLength(UniformName, 50);

  for I := 0 to UniformCount - 1 do
  begin
    glGetActiveUniform(ShaderProgram.FProgram, I, 100, nil, @UniformSize, @UniformType, @UniformName[0]);
    if SameText(UTF8ToString(MarshaledAString(UniformName)), Name) then
    begin
      Result := UniformSize;
      Exit;
    end;
  end;
  Result := 0;
end;

class function TContextOpenGL.FindProgram(const VS, PS: TContextShader): TCustomContextOpenGL.TShaderProgram;
var
  ShaderProgram: TShaderProgram;
  compiled: Integer;
  S: TStringBuilder;
  UniformSize, I: Integer;
  Source: TContextShaderSource;
  TexUnit: Integer;
  Variable: TContextShaderVariable;
begin
  Result := nil;
  if (FSharedContext <> nil) and (VS <> nil) and (PS <> nil) and (VS.Handle <> 0) and (PS.Handle <> 0) then
  begin
    if FPrograms <> nil then
      for ShaderProgram in FPrograms do
        if (ShaderProgram.FVertexShader = VS.Handle) and (ShaderProgram.FPixelShader = PS.Handle) then
          Exit(ShaderProgram);

    ShaderProgram := TShaderProgram.Create;
    ShaderProgram.FProgram := glCreateProgram();

    glAttachShader(ShaderProgram.FProgram, VS.Handle);
    glAttachShader(ShaderProgram.FProgram, PS.Handle);

    glBindAttribLocation(ShaderProgram.FProgram, ATTRIB_VERTEX, 'a_Position');
    glBindAttribLocation(ShaderProgram.FProgram, ATTRIB_NORMAL, 'a_Normal');
    glBindAttribLocation(ShaderProgram.FProgram, ATTRIB_TEXCOORD0, 'a_TexCoord0');
    glBindAttribLocation(ShaderProgram.FProgram, ATTRIB_TEXCOORD1, 'a_TexCoord1');
    glBindAttribLocation(ShaderProgram.FProgram, ATTRIB_TEXCOORD2, 'a_TexCoord2');
    glBindAttribLocation(ShaderProgram.FProgram, ATTRIB_TEXCOORD3, 'a_TexCoord3');
    glBindAttribLocation(ShaderProgram.FProgram, ATTRIB_COLOR0, 'a_Color');
    glBindAttribLocation(ShaderProgram.FProgram, ATTRIB_COLOR1, 'a_Color1');
    glBindAttribLocation(ShaderProgram.FProgram, ATTRIB_COLOR2, 'a_Color2');
    glBindAttribLocation(ShaderProgram.FProgram, ATTRIB_COLOR3, 'a_Color3');

    glLinkProgram(ShaderProgram.FProgram);
    glGetProgramiv(ShaderProgram.FProgram, GL_LINK_STATUS, @compiled);
    if compiled = 0 then
    begin
      glDeleteProgram(ShaderProgram.FProgram);
      EContext3DException.CreateResFmt(@SCannotActivateShaderProgram, [ClassName]);
    end;

    S := TStringBuilder.Create;
    try
      Source := VS.GetSourceByArch(TContextShaderArch.Mac);
      if Source.Arch = TContextShaderArch.Undefined then
        Source := VS.GetSourceByArch(TContextShaderArch.GLSL);
      if Source.IsDefined then
      begin
        // set vs
        for I := 0 to High(Source.Variables) do
        begin
          S.Clear;
          S.Append('_').Append(Source.Variables[I].Name);
          if Pos('.', S.ToString) > 0 then
            S.Insert(Pos('.', S.ToString), '_');
          Variable := Source.Variables[I];
          Variable.ShaderKind := TContextShaderKind.VertexShader;
          Variable.Index := glGetUniformLocation(ShaderProgram.FProgram, MarshaledAString(TMarshal.AsAnsi(S.ToString)));
          // correct size if compiler decrease it
          if Variable.Kind = TContextShaderVariableKind.Matrix then
          begin
            UniformSize := GetUniformSize(ShaderProgram, S.ToString);
            if UniformSize = 0 then
            begin
              S.Append('[0]');
              UniformSize := GetUniformSize(ShaderProgram, S.ToString);
            end;
            if (UniformSize > 0) and (Variable.Size <> UniformSize) then
              Variable.Size := UniformSize;
          end;
          ShaderProgram.FVariables.Add(Source.Variables[I].Name, Variable);
        end;
      end;
      // set ps
      Source := PS.GetSourceByArch(TContextShaderArch.Mac);
      if Source.Arch = TContextShaderArch.Undefined then
        Source := PS.GetSourceByArch(TContextShaderArch.GLSL);
      if Source.IsDefined then
      begin
        TexUnit := 0;
        for I := 0 to High(Source.Variables) do
        begin
          S.Clear;
          S.Append('_').Append(Source.Variables[I].Name);
          if Pos('.', S.ToString) > 0 then
            S.Insert(Pos('.', S.ToString), '_');

          Variable := Source.Variables[I];
          Variable.ShaderKind := TContextShaderKind.PixelShader;
          Variable.Index := glGetUniformLocation(ShaderProgram.FProgram, MarshaledAString(TMarshal.AsAnsi(S.ToString)));
          if (Variable.Index >= 0) and (Variable.Kind = TContextShaderVariableKind.Texture) then
          begin
            Variable.TextureUnit := TexUnit;
            TexUnit := TexUnit + 1;
          end;
          // correct size if compiler decrease it
          if Variable.Kind = TContextShaderVariableKind.Matrix then
          begin
            UniformSize := GetUniformSize(ShaderProgram, S.ToString);
            if UniformSize = 0 then
            begin
              S.Append('[0]');
              UniformSize := GetUniformSize(ShaderProgram, S.ToString);
            end;
            if (UniformSize > 0) and (Variable.Size <> UniformSize) then
              Variable.Size := UniformSize;
          end;
          ShaderProgram.FVariables.Add(Source.Variables[I].Name, Variable);
        end;
      end;
    finally
      S.DisposeOf;
    end;

    if FPrograms = nil then
      FPrograms := TList<TShaderProgram>.Create;

    ShaderProgram.FVertexShader := VS.Handle;
    ShaderProgram.FPixelShader := PS.Handle;
    FPrograms.Add(ShaderProgram);

    Result := ShaderProgram;
  end;
end;

class procedure TContextOpenGL.UseProgram(const VS, PS: TContextShader);
var
  Prog: TShaderProgram;
begin
  if (FSharedContext <> nil) and (VS <> nil) and (PS <> nil) and (VS.Handle <> 0) and (PS.Handle <> 0) then
  begin
    Prog := FindProgram(VS, PS);

    FCurrentProgram := Prog;
    glUseProgram(FCurrentProgram.FProgram);
    if (GLHasAnyErrors()) then
      EContext3DException.CreateResFmt(@SCannotActivateShaderProgram, [ClassName]);
  end;
end;

{ shaders }

class procedure TContextOpenGL.DoInitializeShader(const Shader: TContextShader);
var
  Source: TContextShaderSource;
begin
  if SharedContext <> nil then
  begin
    if Shader.Kind = TContextShaderKind.VertexShader then
    begin
      Source := Shader.GetSourceByArch(TContextShaderArch.Mac);
      if Source.Arch = TContextShaderArch.Undefined then
        Source := Shader.GetSourceByArch(TContextShaderArch.GLSL);
      if Source.IsDefined then
        Shader.Handle := BuildShader(GL_VERTEX_SHADER, Source.Code)
    end else begin
      Source := Shader.GetSourceByArch(TContextShaderArch.Mac);
      if Source.Arch = TContextShaderArch.Undefined then
        Source := Shader.GetSourceByArch(TContextShaderArch.GLSL);
      if Source.IsDefined then
        Shader.Handle := BuildShader(GL_FRAGMENT_SHADER, Source.Code)
    end;
  end;
end;

class procedure TContextOpenGL.DoFinalizeShader(const Shader: TContextShader);
begin
  if SharedContext <> nil then
    glDeleteShader(Shader.Handle);
  Shader.Handle := 0;
end;

procedure TContextOpenGL.DoSetScissorRect(const ScissorRect: TRect);
begin
  glScissor(ScissorRect.Left, Round(Height * Scale) - ScissorRect.Bottom, ScissorRect.Width, ScissorRect.Height);
end;

procedure TContextOpenGL.DoSetShaders(const VertexShader, PixelShader: TContextShader);
begin
  if Valid then
    UseProgram(VertexShader, PixelShader);
end;

procedure TContextOpenGL.DoSetShaderVariable(const Name: string;
  const Matrix: TMatrix3D);
var
  Temp: TMatrix3D;
begin
  Temp := Matrix.Transpose;
  DoSetShaderVariable(Name, Temp.M);
end;

procedure TContextOpenGL.DoSetShaderVariable(const Name: string; const Data: array of TVector3D);
var
  J: Integer;
  Variable: TContextShaderVariable;
begin
  if FSharedContext <> nil then
  begin
    if FCurrentProgram <> nil then
    begin
      if FCurrentProgram.FVariables.TryGetValue(Name, Variable) then
      begin
        case Variable.Kind of
          TContextShaderVariableKind.Float:
             glUniform1f(Variable.Index, Data[0].X);
          TContextShaderVariableKind.Float2:
             glUniform2f(Variable.Index, Data[0].X, Data[0].Y);
          TContextShaderVariableKind.Float3:
             glUniform3f(Variable.Index, Data[0].X, Data[0].Y, Data[0].Z);
        else
          for J := 0 to Min(High(Data), Variable.Size - 1) do
            glUniform4f(Variable.Index + J, Data[J].X, Data[J].Y, Data[J].Z, Data[J].W);
        end;
        GLHasAnyErrors;
      end;
    end;
  end;
end;

procedure TContextOpenGL.DoSetShaderVariable(const Name: string; const Texture: TTexture);
var
  Variable: TContextShaderVariable;
begin
  if FSharedContext <> nil then
  begin
    if FCurrentProgram <> nil then
    begin
      if FCurrentProgram.FVariables.TryGetValue(Name, Variable) then
      begin
        glActiveTexture(GL_TEXTURE0 + Variable.TextureUnit);
        if Texture = nil then
          glBindTexture(GL_TEXTURE_2D, 0)
        else
          glBindTexture(GL_TEXTURE_2D, Texture.Handle);
        glActiveTexture(GL_TEXTURE0);
        glUniform1i(Variable.Index, Variable.TextureUnit);
        GLHasAnyErrors;
      end;
    end;
  end;
end;

var
  OpenGLHandle: HMODULE;

procedure RegisterContextClasses;
begin
  OpenGLHandle := InitOpenGL;
  if OpenGLHandle <> 0 then
    TContextManager.RegisterContext(TContextOpenGL, True);
end;

procedure UnregisterContextClasses;
begin
  TCustomContextOpenGL.DestroySharedContext;
  if OpenGLHandle <> 0 then
  begin
    FreeLibrary(OpenGLHandle);
    OpenGLHandle := 0;
  end;
end;

end.
