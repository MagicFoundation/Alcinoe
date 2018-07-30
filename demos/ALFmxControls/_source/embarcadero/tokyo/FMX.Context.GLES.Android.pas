{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Context.GLES.Android;

interface

{$HINTS OFF}
{$SCOPEDENUMS ON}

uses
  System.Types, System.Classes, System.Generics.Collections, Androidapi.Egl, FMX.Types3D, FMX.Context.GLES;

type
  TCustomAndroidContext = class(TCustomContextOpenGL)
  private const
    AndroidMaxLightCount = 1;
    ContextAttributes: array[0..2] of EGLint = (EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);
  private class var
    SharedMultisamples: Integer;
  protected type
    TContextPair = record
      Surface: EGLSurface;
      Context: EGLContext;
    end;
  protected class var
    FSharedConfig: EGLConfig;
    FSharedDisplay: EGLDisplay;
    FSharedSurface: EGLSurface;
    FSharedContext: EGLContext;
    FThreadDictionary: TDictionary<TThreadID, TContextPair>;
    class procedure CreateThreadDictionary; static;
    class procedure DestroyThreadDictionary; static;
  protected
    FSurface: EGLSurface;
    FContext: EGLContext;
    class procedure CreateSharedContext; override;
    class procedure DestroySharedContext; override;
    function GetIndexBufferSupport: TContext3D.TIndexBufferSupport; override;
  public
    class function CreateContextFromActivity(const AWidth, AHeight: Integer; const AMultisample: TMultisample;
      const ADepthStencil: Boolean): TCustomAndroidContext;
    class function ActivateSharedContext: Boolean;
    class function Valid: Boolean; override;
    class property SharedConfig: EGLConfig read FSharedConfig;
    class property SharedDisplay: EGLDisplay read FSharedDisplay;
    class property SharedSurface: EGLSurface read FSharedSurface;
    class property SharedContext: EGLContext read FSharedContext;

    class function MaxLightCount: Integer; override;
    class function Style: TContextStyles; override;
  end;

procedure RegisterContextClasses;
procedure UnregisterContextClasses;

implementation

uses
  Androidapi.Gles2, Androidapi.Gles2ext, Androidapi.Eglext, Androidapi.NativeWindow, FMX.Consts, FMX.Types,
  FMX.Materials, FMX.Graphics, FMX.Forms, FMX.Forms3D, FMX.Platform, FMX.Platform.Android, System.SysUtils,
  System.Messaging, AndroidApi.AppGlue, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge,
  Androidapi.NativeWindowJni;

const
  OES_packed_depth_stencil = 'GL_OES_packed_depth_stencil';
  OES_depth24 = 'GL_OES_depth24';
  NV_depth_nonlinear = 'GL_NV_depth_nonlinear';

function CreateEGLSurface(const Width, Height: Integer): EGLSurface;
var
  Texture: JSurfaceTexture;
  Surface: JSurface;
  Win: PANativeWindow;
begin
  Texture := TJSurfaceTexture.JavaClass.init(0);
  Texture.setDefaultBufferSize(1, 1);
  Surface := TJSurface.JavaClass.init(Texture);
  if not Surface.isValid then
    RaiseContextExceptionFmt(@SCannotCreateOpenGLContextWithCode, ['JSurfaceTexture', eglGetError()]);
  Win := ANativeWindow_fromSurface(TJNIResolver.GetJNIEnv, (Surface as ILocalObject).GetObjectID);
  try
    Result := eglCreateWindowSurface(TCustomAndroidContext.SharedDisplay, TCustomAndroidContext.SharedConfig, Win, nil);
  finally
    ANativeWindow_release(Win);
  end;
end;

function IsMainThread: Boolean; inline;
begin
  Result := MainThreadID = TThread.CurrentThread.ThreadID;
end;

                                      
// Uses to hold own framebuffer per thread for each texture

type
  TFrameBuffers = TDictionary<TFmxHandle, GLuint>;
  TThreadBuffers = TDictionary<TThreadID, TFrameBuffers>;

var
  ThreadBuffers: TThreadBuffers;

{ TCustomAndroidContext }

function TCustomAndroidContext.GetIndexBufferSupport: TContext3D.TIndexBufferSupport;
begin
  // AVD crash workaround: although 32-bit index buffer appears to be supported, using it leads to emulator crash.
  Result := TIndexBufferSupport.Int16;
end;

function CreateSharedConfig: EGLConfig;
type
  TContextAttributes = array of EGLint;

  function GetDesiredMultisamples: Integer;
  const
    HighQualitySamples = 4;
  begin
    Result := 0;
    if Application.MainForm = nil then
      Exit;

    if (Application.MainForm is TCustomForm) and
      (TCustomForm(Application.MainForm).Quality = TCanvasQuality.HighQuality) then
      Exit(HighQualitySamples);

    if Application.MainForm is TCustomForm3D then
      Result := MultisampleTypeToNumber(TCustomForm3D(Application.MainForm).Multisample);
  end;

  procedure AddAttributes(var ContextAttributes: TContextAttributes; const Attributes: array of EGLint);
  var
    I, Index: Integer;
  begin
    Index := Length(ContextAttributes);
    SetLength(ContextAttributes, Index + Length(Attributes));

    for I := 0 to Length(Attributes) - 1 do
      ContextAttributes[Index + I] := Attributes[I];
  end;

var
  ConfigAttribs: TContextAttributes;
  NumConfigs: EGLint;
  RenderingSetupService: IFMXRenderingSetupService;
  ColorBits, DepthBits, Multisamples: Integer;
  Stencil: Boolean;
begin
  // Determine initial number of multisamples based on form quality settings.
  Multisamples := TCustomAndroidContext.SharedMultisamples;
  if Multisamples < 1 then
    Multisamples := GetDesiredMultisamples;

  // Default rendering configuration.
  ColorBits := 24;
  DepthBits := 24;
  Stencil := True;

  // Request adjustment of rendering configuration.
  if TPlatformServices.Current.SupportsPlatformService(IFMXRenderingSetupService, RenderingSetupService) then
    RenderingSetupService.Invoke(ColorBits, DepthBits, Stencil, Multisamples);

  { Extensions must be obtained prior creating main OpenGL ES context for configuring depth buffer that is higher
    than 16 bits or enabling multisampling. In this case, create a dummy OpenGL ES context and read OpenGL ES
    extensions and renderer information. }
  if (DepthBits > 16) or (Multisamples > 0) then
    TCustomAndroidContext.GetExtensions;

  // Prepare final context configuration.
  SetLength(ConfigAttribs, 0);
  AddAttributes(ConfigAttribs, [EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT]);
  AddAttributes(ConfigAttribs, [EGL_SURFACE_TYPE, EGL_WINDOW_BIT]);

  // Color Bitdepth.
  if ColorBits > 16 then
    AddAttributes(ConfigAttribs, [EGL_BUFFER_SIZE, 32, EGL_RED_SIZE, 8, EGL_GREEN_SIZE, 8, EGL_BLUE_SIZE, 8])
  else
    AddAttributes(ConfigAttribs, [EGL_BUFFER_SIZE, 16, EGL_RED_SIZE, 5, EGL_GREEN_SIZE, 5, EGL_BLUE_SIZE, 5]);

  // Depth Buffer.
  if DepthBits > 0 then
  begin
    if DepthBits > 16 then
    begin
      if TCustomAndroidContext.Extensions[OES_depth24] then
        // 24-bit depth buffer is supported.
        AddAttributes(ConfigAttribs, [EGL_DEPTH_SIZE, 24])
      else
      begin // No 24-bit depth support.
        AddAttributes(ConfigAttribs, [EGL_DEPTH_SIZE, 16]);

        // Tegra 3 GPU has extension for improved accuracy of depth buffer.
        if TCustomAndroidContext.Extensions[NV_depth_nonlinear] then
          AddAttributes(ConfigAttribs, [EGL_DEPTH_ENCODING_NV, EGL_DEPTH_ENCODING_NONLINEAR_NV]);
      end;
    end
    else // 16-bit depth buffer
      AddAttributes(ConfigAttribs, [EGL_DEPTH_SIZE, 16]);
  end;

  // Stencil Buffer.
  if Stencil then
    AddAttributes(ConfigAttribs, [EGL_STENCIL_SIZE, 8]);

  // Multisamples.
  if Multisamples > 0 then
  begin
    // Tegra 3 GPU does not support MSAA (it only does CSAA).
    if not TCustomAndroidContext.Extensions.Renderer.Contains('TEGRA 3') then
      AddAttributes(ConfigAttribs, [EGL_SAMPLE_BUFFERS, 1, EGL_SAMPLES, Multisamples]);
  end;

  // Close the configuration.
  AddAttributes(ConfigAttribs, [EGL_NONE]);

  if eglChooseConfig(TCustomAndroidContext.SharedDisplay, @ConfigAttribs[0], @Result, 1, @NumConfigs) <> EGL_TRUE then
    RaiseContextExceptionFmt(@SCannotCreateOpenGLContextWithCode, ['eglChooseConfig', eglGetError()]);
end;

class procedure TCustomAndroidContext.CreateSharedContext;

  function IsAppNotTerminating: Boolean;
  var
    ApplicationService: IFMXApplicationService;
  begin
    Result := TPlatformServices.Current.SupportsPlatformService(IFMXApplicationService, ApplicationService) and
              not ApplicationService.Terminating;
  end;

begin
  if (FSharedContext = nil) and IsAppNotTerminating then
  begin
    FSharedDisplay := eglGetDisplay(EGL_DEFAULT_DISPLAY);

    if eglInitialize(FSharedDisplay, nil, nil) = 0 then
      RaiseContextExceptionFmt(@SCannotCreateOpenGLContextWithCode, ['eglInitialize', eglGetError()]);

    FSharedConfig := CreateSharedConfig;
    FSharedContext := eglCreateContext(TCustomAndroidContext.SharedDisplay, FSharedConfig, nil,
      @TCustomAndroidContext.ContextAttributes[0]);
    if FSharedContext = EGL_NO_CONTEXT then
      RaiseContextExceptionFmt(@SCannotCreateOpenGLContextWithCode, ['eglCreateContext', eglGetError()]);

    FSharedSurface := CreateEGLSurface(1, 1);

    if FSharedSurface = EGL_NO_SURFACE then
      RaiseContextExceptionFmt(@SCannotCreateOpenGLContextWithCode, ['CreateEGLSurface', eglGetError()]);;

    if eglMakeCurrent(FSharedDisplay, FSharedSurface, FSharedSurface, FSharedContext) = 0 then
    begin
      eglDestroyContext(FSharedDisplay, FSharedContext);
      eglDestroySurface(FSharedDisplay, FSharedSurface);
      RaiseContextExceptionFmt(@SCannotCreateOpenGLContextWithCode, ['eglMakeCurrent', eglGetError()]);
    end;

    CreateThreadDictionary;
  end;
end;

class function TCustomAndroidContext.Valid: Boolean;
begin
  Result := ActivateSharedContext;
end;

class procedure TCustomAndroidContext.CreateThreadDictionary;
begin
  FThreadDictionary := TDictionary<TThreadID, TContextPair>.Create;
  ThreadBuffers := TThreadBuffers.Create;
end;

class procedure TCustomAndroidContext.DestroyThreadDictionary;
var
  LPair: TContextPair;
  Buffer: TFrameBuffers;
  FrameBuf: GLuint;
begin
  for Buffer in ThreadBuffers.Values do
  begin
    for FrameBuf in Buffer.Values do
      glDeleteFramebuffers(1, @FrameBuf);
    Buffer.Free;
  end;
  ThreadBuffers.Free;

  for LPair in FThreadDictionary.Values do
  begin
    eglDestroySurface(FSharedDisplay, LPair.Surface);
    eglDestroyContext(FSharedDisplay, LPair.Context);
  end;
  FThreadDictionary.Free;
end;

class procedure TCustomAndroidContext.DestroySharedContext;
begin
  if FSharedContext <> nil then
  begin
    DestroyPrograms;

    DestroyThreadDictionary;

    eglDestroySurface(FSharedDisplay, FSharedSurface);
    eglDestroyContext(FSharedDisplay, FSharedContext);
    FSharedContext := nil;
  end;
end;

class function TCustomAndroidContext.MaxLightCount: Integer;
begin
  Result := AndroidMaxLightCount;
end;

class function TCustomAndroidContext.Style: TContextStyles;
begin
  Result := [TContextStyle.RenderTargetFlipped];
end;

class function TCustomAndroidContext.ActivateSharedContext: Boolean;
var
  Pair: TContextPair;
  LContext: TCustomAndroidContext;
begin
  CreateSharedContext;
  Result := FSharedContext <> nil;
  if IsMainThread then
  begin
    LContext := TCustomAndroidContext(CurrentContext);
    if (LContext <> nil) and (LContext.FSurface <> nil) and (LContext.FSurface <> eglGetCurrentSurface(EGL_DRAW)) then
      Result := eglMakeCurrent(FSharedDisplay, LContext.FSurface, LContext.FSurface, FSharedContext) <> EGL_FALSE
  end
  else
  begin
    if not FThreadDictionary.TryGetValue(TThread.CurrentThread.ThreadID, Pair) then
    begin
      Pair.Context := eglCreateContext(FSharedDisplay, FSharedConfig, FSharedContext, @ContextAttributes[0]);
      Pair.Surface := CreateEGLSurface(1, 1);

      TMonitor.Enter(FThreadDictionary);
      try
        FThreadDictionary.Add(TThread.CurrentThread.ThreadID, Pair);
      finally
        TMonitor.Exit(FThreadDictionary);
      end;
    end;
    if eglGetCurrentContext <> Pair.Context then
      Result := eglMakeCurrent(FSharedDisplay, Pair.Surface, Pair.Surface, Pair.Context) <> EGL_FALSE;
  end;
end;

{ TContextAndroid }

type
  TContextAndroid = class(TCustomAndroidContext)
  private
    FActivity: Boolean;
    function SupportBuffers: Boolean;
    function CreateFrameBuffer(const BufferWidth, BufferHeight: GLint; const TextureHandle: GLuint;
      const DepthStencil: Boolean; var FrameBuf, DepthBuf, StencilBuf: GLuint): Boolean;
    function TryGetFrameBuffer(var AFrameBuf: GLuint): Boolean;
    procedure SetFrameBuffer(const AFrameBuf: GLuint);
    function TryGetOrCreateFrameBuffer(var AFrameBuf: GLuint): Boolean;
  protected
    class function GetShaderArch: TContextShaderArch; override;
    procedure DoSetScissorRect(const ScissorRect: TRect); override;
    function DoBeginScene: Boolean; override;
    procedure DoEndScene; override;
    { buffer }
    procedure DoCreateBuffer; override;
    procedure DoFreeBuffer; override;
    { constructors }
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AMultisample: TMultisample; const ADepthStencil: Boolean); override;
    constructor CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
      const ADepthStencil: Boolean); override;
    constructor CreateFromActivity(const AWidth, AHeight: Integer; const AMultisample: TMultisample;
      const ADepthStencil: Boolean);
  public
    destructor Destroy; override;
  end;

{ TContextAndroid }

constructor TContextAndroid.CreateFromActivity(const AWidth, AHeight: Integer; const AMultisample: TMultisample;
  const ADepthStencil: Boolean);
var
  Format: EGLint;
begin
  FActivity := True;
  inherited CreateFromWindow(nil, AWidth, AHeight, AMultisample, ADepthStencil);
  CreateSharedContext;

  eglGetConfigAttrib(FSharedDisplay, FSharedConfig, EGL_NATIVE_VISUAL_ID, @Format);
  ANativeWindow_setBuffersGeometry(TAndroidApplicationGlue.Current.Window, 0, 0, Format);
  FSurface := eglCreateWindowSurface(FSharedDisplay, FSharedConfig, TAndroidApplicationGlue.Current.Window, nil);
  if eglMakeCurrent(FSharedDisplay, FSurface, FSurface, FSharedContext) = EGL_FALSE then
    RaiseContextExceptionFmt(@SCannotCreateOpenGLContextWithCode, ['eglMakeCurrent', eglGetError()]);
end;

constructor TContextAndroid.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AMultisample: TMultisample; const ADepthStencil: Boolean);
begin
  FSupportMS := False;
  inherited;
  if (FSharedContext = nil) and (SharedMultisamples < 1) then
    SharedMultisamples := MultisampleTypeToNumber(AMultisample);

  CreateSharedContext;
  if SupportBuffers then
    CreateBuffer;
end;

constructor TContextAndroid.CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
  const ADepthStencil: Boolean);
begin
  FSupportMS := False;
  inherited;
end;

destructor TContextAndroid.Destroy;
begin
  if FActivity then
  begin
    eglMakeCurrent(FSharedDisplay, FSharedSurface, FSharedSurface, FSharedContext);
    eglDestroySurface(FSharedDisplay, FSurface);
  end;
  inherited;
end;

function TContextAndroid.SupportBuffers: Boolean;
begin
  Result := (Parent <> nil) and TAndroidWindowHandle(Parent).RequiresComposition;
end;

function TContextAndroid.CreateFrameBuffer(const BufferWidth, BufferHeight: GLint; const TextureHandle: GLuint;
  const DepthStencil: Boolean; var FrameBuf, DepthBuf, StencilBuf: GLuint): Boolean;
var
  Status: GLint;
begin
  if FrameBuf = 0 then
    glGenFramebuffers(1, @FrameBuf);
  glBindFramebuffer(GL_FRAMEBUFFER, FrameBuf);

  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, TextureHandle, 0);

  if DepthStencil then
  begin
    if Extensions[OES_packed_depth_stencil] then
    begin // using OES_packed_depth_stencil extension
      if DepthBuf = 0 then
        glGenRenderbuffers(1, @DepthBuf);
      glBindRenderbuffer(GL_RENDERBUFFER, DepthBuf);
      glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8_OES, BufferWidth, BufferHeight);
      glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, DepthBuf);
      glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, DepthBuf);
      glBindRenderbuffer(GL_RENDERBUFFER, 0);
      StencilBuf := 0;
    end
    else
    begin // attempting more conservative approach
      if DepthBuf = 0 then
        glGenRenderbuffers(1, @DepthBuf);
      glBindRenderbuffer(GL_RENDERBUFFER, DepthBuf);
      glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT16, BufferWidth, BufferHeight);
      glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, DepthBuf);

      if StencilBuf = 0 then
        glGenRenderbuffers(1, @StencilBuf);
      glBindRenderbuffer(GL_RENDERBUFFER, StencilBuf);
      glRenderbufferStorage(GL_RENDERBUFFER, GL_STENCIL_INDEX8, BufferWidth, BufferHeight);
      glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_RENDERBUFFER, StencilBuf);
      glBindRenderbuffer(GL_RENDERBUFFER, 0);
    end;
  end;

  Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);
  if (Status <> GL_FRAMEBUFFER_COMPLETE) or GLHasAnyErrors then
  begin
    if StencilBuf <> 0 then
    begin
      glDeleteRenderbuffers(1, @StencilBuf);
      StencilBuf := 0;
    end;

    if DepthBuf <> 0 then
    begin
      glDeleteRenderbuffers(1, @DepthBuf);
      DepthBuf := 0;
    end;

    if FrameBuf <> 0 then
    begin
      glDeleteFramebuffers(1, @FrameBuf);
      FrameBuf := 0;
    end;

    Result := False;
  end
  else
    Result := True;
end;

function TContextAndroid.DoBeginScene: Boolean;
var
  LFrameBuf: GLuint;
begin
  Result := False;
  if Valid then
  begin
    glGetIntegerv(GL_VIEWPORT, @FOldViewport[0]);

    if (Texture <> nil) or (FFrameBuf <> 0) then
    begin
      glGetIntegerv(GL_FRAMEBUFFER_BINDING, @FOldFBO);
      if TryGetOrCreateFrameBuffer(LFrameBuf) then
        glBindFramebuffer(GL_FRAMEBUFFER, LFrameBuf)
      else
        Exit(False);
    end;

    if Texture = nil then
      glViewport(0, 0, Round(Width * Scale), Round(Height * Scale))
    else
      glViewport(0, 0, Width, Height);

    Result := inherited DoBeginScene;

    if (GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SCannotBeginRenderingScene, [ClassName]);
  end;
end;

procedure TContextAndroid.DoEndScene;
begin
  if (Texture <> nil) or (FFrameBuf <> 0) then
    glBindFramebuffer(GL_FRAMEBUFFER, FOldFBO);

  glViewport(FOldViewport[0], FOldViewport[1], FOldViewport[2], FOldViewport[3]);
  if FSurface <> nil then
    eglSwapBuffers(TCustomAndroidContext.SharedDisplay, FSurface);
  inherited DoEndScene;
end;

function TContextAndroid.TryGetFrameBuffer(var AFrameBuf: GLuint): Boolean;
var
  Buffers: TFrameBuffers;
begin
  AFrameBuf := 0;

  if not IsMainThread then
  begin
    if ThreadBuffers.TryGetValue(TThread.CurrentThread.ThreadID, Buffers) then
      Buffers.TryGetValue(Texture.Handle, AFrameBuf);
  end
  else
    AFrameBuf := FFrameBuf;

  Result := AFrameBuf <> 0;
end;

function TContextAndroid.TryGetOrCreateFrameBuffer(var AFrameBuf: GLuint): Boolean;
begin
  Result := TryGetFrameBuffer(AFrameBuf);
  if not Result then
  begin
    CreateBuffer;
    Result := TryGetFrameBuffer(AFrameBuf);
  end;
  Assert(AFrameBuf <> 0);
end;

procedure TContextAndroid.SetFrameBuffer(const AFrameBuf: GLuint);
var
  Buffers: TFrameBuffers;
begin
  if (AFrameBuf <> 0) and not IsMainThread then
  begin
    if not ThreadBuffers.TryGetValue(TThread.CurrentThread.ThreadID, Buffers) then
    begin
      Buffers := TFrameBuffers.Create;
      ThreadBuffers.Add(TThread.CurrentThread.ThreadID, Buffers);
    end;
    Buffers.AddOrSetValue(Texture.Handle, AFrameBuf);
  end
  else
    FFrameBuf := AFrameBuf;
end;

procedure TContextAndroid.DoCreateBuffer;
var
  OldFBO: GLuint;
  LFrameBuf: GLuint;
  WindowTexture: TTexture;
begin
  if Valid and SupportBuffers and (Width > 0) and (Height > 0) then
  begin
    WindowHandleToPlatform(Parent).CreateTexture;
    WindowTexture := WindowHandleToPlatform(Parent).Texture;

    glGetIntegerv(GL_FRAMEBUFFER_BINDING, @OldFBO);
    try
      if (not CreateFrameBuffer(WindowTexture.Width, WindowTexture.Height, WindowTexture.Handle,
        DepthStencil, FFrameBuf, FDepthBuf, FStencilBuf)) and DepthStencil then
      begin
        if not CreateFrameBuffer(WindowTexture.Width, WindowTexture.Height, WindowTexture.Handle,
          False, FFrameBuf, FDepthBuf, FStencilBuf) then
          RaiseContextExceptionFmt(@SCannotCreateRenderBuffers, [ClassName]);
      end;
    finally
      glBindFramebuffer(GL_FRAMEBUFFER, OldFBO);
    end;
  end;

  if Valid and (Texture <> nil) then
  begin
    if Texture.Handle = 0 then
      Texture.Initialize;

    glGetIntegerv(GL_FRAMEBUFFER_BINDING, @OldFBO);
    try
      TryGetFrameBuffer(LFrameBuf);

      if (not CreateFrameBuffer(Width, Height, Texture.Handle, DepthStencil, LFrameBuf, FDepthBuf, FStencilBuf)) and
        DepthStencil then
      begin
        if not CreateFrameBuffer(Width, Height, Texture.Handle, False, LFrameBuf, FDepthBuf, FStencilBuf) then
          RaiseContextExceptionFmt(@SCannotCreateRenderBuffers, [ClassName]);
      end;

      SetFrameBuffer(LFrameBuf);
    finally
      glBindFramebuffer(GL_FRAMEBUFFER, OldFBO);
    end;
  end;
end;

procedure TContextAndroid.DoFreeBuffer;
begin
  if Valid and SupportBuffers and (Parent <> nil) then
    WindowHandleToPlatform(Parent).DestroyTexture;
  inherited;
end;

class function TContextAndroid.GetShaderArch: TContextShaderArch;
begin
  Result := TContextShaderArch.Android;
end;

procedure TContextAndroid.DoSetScissorRect(const ScissorRect: TRect);
var
  R: TRect;
begin
  R := Rect(Round(ScissorRect.Left * Scale), Round(ScissorRect.Top * Scale),
    Round(ScissorRect.Right * Scale), Round(ScissorRect.Bottom * Scale));

  if Texture <> nil then
    glScissor(R.Left, Height - R.Bottom, R.Width, R.Height)
  else
    glScissor(R.Left, Round(Height * Scale) - R.Bottom, R.Width, R.Height);

  if (GLHasAnyErrors()) then
    RaiseContextExceptionFmt(@SErrorInContextMethod, ['DoSetScissorRect']);
end;

class function TCustomAndroidContext.CreateContextFromActivity(const AWidth, AHeight: Integer;
  const AMultisample: TMultisample; const ADepthStencil: Boolean): TCustomAndroidContext;
begin
  Result := TContextAndroid.CreateFromActivity(AWidth, AHeight, AMultisample, ADepthStencil);
end;

procedure RegisterContextClasses;
begin
  TContextManager.RegisterContext(TContextAndroid, True);
end;

procedure UnregisterContextClasses;
begin
  TContextAndroid.DestroySharedContext;
end;

end.
