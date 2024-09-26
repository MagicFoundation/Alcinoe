{$HINTS OFF}{$WARNINGS OFF}
{************************************************************************}
{                                                                        }
{                              Skia4Delphi                               }
{                                                                        }
{ Copyright (c) 2021-2023 Skia4Delphi Project.                           }
{                                                                        }
{ Use of this source code is governed by the MIT license that can be     }
{ found in the LICENSE file.                                             }
{                                                                        }
{************************************************************************}
unit FMX.Skia.Canvas.GL;

interface

{$SCOPEDENUMS ON}
{$HPPEMIT NOUSINGNAMESPACE}

{$IF DEFINED(MSWINDOWS) or DEFINED(ANDROID) or DEFINED(IOS)}
  {$DEFINE SKIA_GL}
{$ENDIF}

//{$IFDEF SKIA_GL}

uses
  { Skia }
  FMX.Skia.Canvas;

type
  { TGlSharedContextCustom }

  TGlSharedContextCustom = class abstract(TGrSharedContext)
  protected
    FStencilBits: Integer;
  public
    property StencilBits: Integer read FStencilBits;
  end;

implementation

uses
  { Delphi }
  FMX.Graphics,
  FMX.Types,
  System.Classes,
  System.Generics.Collections,
  {$IF DEFINED(ANDROID)}
  FMX.Platform.UI.Android,
  FMX.Presentation.Android.Style,
  Androidapi.Egl,
  Androidapi.Gles2,
  Androidapi.Gles2ext,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.NativeWindow,
  Androidapi.NativeWindowJni,
  {$ELSEIF DEFINED(IOS)}
  FMX.Platform.iOS,
  iOSapi.GLKit,
  iOSapi.OpenGLES,
  {$ELSEIF DEFINED(MSWINDOWS)}
  FMX.Platform.Win,
  Winapi.OpenGL,
  Winapi.OpenGLext,
  Winapi.Windows,
  System.AnsiStrings,
  System.Generics.Collections,
  System.Generics.Defaults,
  {$ENDIF}
  System.Math,
  System.SysUtils,
  System.Types,

  { Skia }
  System.Skia,
  FMX.Forms,
  FMX.Skia,
  Alcinoe.Common;

function IsMainThread: Boolean; inline;
begin
  Result := MainThreadID = TThread.CurrentThread.ThreadID;
end;

type
  EGlError = class(EGrCanvas);

  { TGlCanvas }

  TGlCanvas = class(TGrCanvas)
  {$IF DEFINED(ANDROID)}
  private
    class var FCurrentMainSurface: EGLSurface;
  public
    class property CurrentMainSurface: EGLSurface read FCurrentMainSurface;
  {$ENDIF}
  private
    FBackBufferSurface: ISkSurface;
    {$IF DEFINED(ANDROID)}
    FANativeWindow: PANativeWindow;
    (* FContext: EGLContext; *)
    (* FOldContext: EGLContext; *)
    (* FOldDisplay: EGLDisplay; *)
    (* FOldDraw: EGLSurface; *)
    (* FOldRead: EGLSurface; *)
    FOldMainSurface: EGLSurface;
    FSurface: EGLSurface;
    {$ELSEIF DEFINED(IOS)}
    (* FContext: EAGLContext; *)
    (* FOldContext: EAGLContext; *)
    {$ELSEIF DEFINED(MSWINDOWS)}
    FDC: HDC;
    FGLRC: HGLRC;
    FOldDC: HDC;
    FOldGLRC: HGLRC;
    {$ENDIF}
  protected
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    constructor CreateFromBitmap(const ABitmap: TBitmap; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    constructor CreateFromPrinter(const APrinter: TAbstractPrinter); override;
    procedure BeforeRestore; override;
    function BeginCanvas(const AContextHandle: THandle): ISkCanvas; override;
    procedure EndCanvas(const AContextHandle: THandle); override;
    function CreateSharedContext: IGrSharedContext; override;
    procedure DoDrawBitmap(const ABitmap: FMX.Graphics.TBitmap; const ASrcRect, ADestRect: TRectF; const AOpacity: Single; const AHighSpeed: Boolean); override;
    function GetSurfaceFromWindow(const AContextHandle: THandle): TSkSurface; override;
    procedure Resized; override;
    procedure SwapBuffers(const AContextHandle: THandle); override;
  public
    destructor Destroy; override;
    class function IsSupported: Boolean;
  end;

{$IF DEFINED(MSWINDOWS)}

const
  WGL_DRAW_TO_WINDOW_ARB           = $2001;
  WGL_ACCELERATION_ARB             = $2003;
  WGL_SUPPORT_OPENGL_ARB           = $2010;
  WGL_DOUBLE_BUFFER_ARB            = $2011;
  WGL_COLOR_BITS_ARB               = $2014;
  WGL_ALPHA_BITS_ARB               = $201B;
  WGL_STENCIL_BITS_ARB             = $2023;
  WGL_FULL_ACCELERATION_ARB        = $2027;
  WGL_SAMPLE_BUFFERS_ARB           = $2041;
  WGL_SAMPLES_ARB                  = $2042;
  WGL_CONTEXT_CORE_PROFILE_BIT_ARB = $00000001;
  WGL_CONTEXT_MAJOR_VERSION_ARB    = $2091;
  WGL_CONTEXT_MINOR_VERSION_ARB    = $2092;
  WGL_CONTEXT_PROFILE_MASK_ARB     = $9126;

type
  PFNWGLCHOOSEPIXELFORMATARBPROC      = function (hdc: HDC; const piAttribIList: PInteger; const pfAttribFList: PSingle; nMaxFormats: UINT; piFormats: PInteger; nNumFormats: PUINT): BOOL; stdcall;
  PFNWGLCREATECONTEXTATTRIBSARBPROC   = function (hDC: HDC; hShareContext: HGLRC; const attribList: PInteger): HGLRC; stdcall;
  PFNWGLGETEXTENSIONSSTRINGARBPROC    = function (hdc: HDC): MarshaledAString; stdcall;
  PFNWGLGETPIXELFORMATATTRIBIVARBPROC = function (hdc: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: UINT; const piAttributes: PInteger; piValues: PInteger): BOOL; stdcall;
  PFNWGLSWAPINTERVALEXTPROC           = function (interval: Integer): BOOL; stdcall;

  { TGlInterface }

  TGlInterface = record
  private
    FChoosePixelFormatARB: PFNWGLCHOOSEPIXELFORMATARBPROC;
    FCreateContextAttribsARB: PFNWGLCREATECONTEXTATTRIBSARBPROC;
    FGetExtensionsStringARB: PFNWGLGETEXTENSIONSSTRINGARBPROC;
    FGetPixelFormatAttribivARB: PFNWGLGETPIXELFORMATATTRIBIVARBPROC;
    FSwapIntervalEXT: PFNWGLSWAPINTERVALEXTPROC;
  private
    procedure GetGlProc(const AName: MarshaledAString; out AProc); inline;
  public
    procedure Initialize;
    function HasExtension(const AHDC: HDC; const AName: MarshaledAString): Boolean;
    property ChoosePixelFormatARB: PFNWGLCHOOSEPIXELFORMATARBPROC read FChoosePixelFormatARB;
    property CreateContextAttribsARB: PFNWGLCREATECONTEXTATTRIBSARBPROC read FCreateContextAttribsARB;
    property GetExtensionsStringARB: PFNWGLGETEXTENSIONSSTRINGARBPROC read FGetExtensionsStringARB;
    property GetPixelFormatAttribivARB: PFNWGLGETPIXELFORMATATTRIBIVARBPROC read FGetPixelFormatAttribivARB;
    property SwapIntervalEXT: PFNWGLSWAPINTERVALEXTPROC read FSwapIntervalEXT;
  end;
{$ENDIF}

type
  { TGlSharedContext }

  TGlSharedContext = class(TGlSharedContextCustom)
  private
    type
      TContextAttributes = array of EGLint;
      TThreadContextInfo = record
        Surface: EGLSurface;
        Context: EGLContext;
        GrDirectContext: IGrDirectContext;
      end;
  private
    [Volatile] FBeginContextCount: Integer;
    FGrGlInterface: IGrGlInterface;
    {$IF DEFINED(ANDROID)}
    FConfig: EGLConfig;
    FContext: EGLContext;
    FDisplay: EGLDisplay;
    FOldContext: EGLContext;
    FOldDisplay: EGLDisplay;
    FOldDraw: EGLSurface;
    FOldRead: EGLSurface;
    FSurface: EGLSurface;
    {$ELSEIF DEFINED(IOS)}
    FContext: EAGLContext;
    FLibraryHandle: HMODULE;
    FOldContext: EAGLContext;
    {$ELSEIF DEFINED(MSWINDOWS)}
    FContextAttributes: TArray<Integer>;
    FDC: HDC;
    FGlInterface: TGlInterface;
    FGLRC: HGLRC;
    FOldDC: HDC;
    FOldGLRC: HGLRC;
    FPixelFormat: Integer;
    FPixelFormatDescriptor: TPixelFormatDescriptor;
    FWindow: HWND;
    {$ENDIF}
    FThreadDictionary: TDictionary<TThreadID, TThreadContextInfo>;
    procedure CreateThreadDictionary;
    procedure DestroyThreadDictionary;
    procedure CreateSharedContext;
    procedure DestroySharedContext;
    function ActivateContext: TThreadContextInfo;
  protected
    function GetTextureColorType: TSkColorType; override;
    function GetTextureOrigin: TGrSurfaceOrigin; override;
    function GetGrDirectContext: IGrDirectContext; override;
    procedure RefreshContext; override;
  public
    constructor Create(const AQuality: TCanvasQuality);
    destructor Destroy; override;
    procedure FinalizeTextureCache(const ABitmap: TGrBitmapHandle); override;
    procedure InitializeTextureCache(const ABitmap: TGrBitmapHandle); override;
    property GrGlInterface: IGrGlInterface read FGrGlInterface;
    {$IF DEFINED(ANDROID)}
    function CreateEGLPBufferSurface(const Width, Height: Integer): EGLSurface;
    property Config: EGLConfig read FConfig;
    property Context: EGLContext read FContext;
    property Display: EGLDisplay read FDisplay;
    const ContextAttributes: array[0..2] of EGLint = (EGL_CONTEXT_CLIENT_VERSION, 2, EGL_NONE);
    {$ELSEIF DEFINED(IOS)}
    property Context: EAGLContext read FContext;
    {$ELSEIF DEFINED(MSWINDOWS)}
    property ContextAttributes: TArray<Integer> read FContextAttributes;
    property GlInterface: TGlInterface read FGlInterface;
    property GLRC: HGLRC read FGLRC;
    property PixelFormat: Integer read FPixelFormat;
    property PixelFormatDescriptor: TPixelFormatDescriptor read FPixelFormatDescriptor;
    {$ENDIF}
  end;

{ TGlCanvas }

procedure TGlCanvas.BeforeRestore;
begin
  ALLog('xxx.TGlCanvas.BeforeRestore', 'begin');
  inherited;
  (*
  if Parent <> nil then
    MakeCurrent
  else if Supports(FWrapper, IGrCanvasWrapper) then
    TGlSharedContext(SharedContext).MakeCurrent;
  *)
  ALLog('xxx.TGlCanvas.BeforeRestore', 'end');
end;

function TGlCanvas.BeginCanvas(const AContextHandle: THandle): ISkCanvas;
begin
  ALLog('xxx.TGlCanvas.BeginCanvas', 'begin');
  Result := inherited;
  if result <> nil then
  begin
    if IsMainThread then begin
      FOldMainSurface := FCurrentMainSurface;
      FCurrentMainSurface := FSurface;
    end;
    // If executing in the main thread, FGrDirectContext is sourced from SharedContext.FGrDirectContext.
    // In other cases, it is derived from SharedContext.FThreadDictionary.
    // Additionally, this command binds the appropriate context to the current thread.
    FGrDirectContext := TGlSharedContext(SharedContext).GrDirectContext;
  end;
  ALLog('xxx.TGlCanvas.BeginCanvas', 'end');
end;

procedure TGlCanvas.EndCanvas(const AContextHandle: THandle);
begin
  ALLog('xxx.TGlCanvas.EndCanvas', 'begin');
  inherited;
  if IsMainThread then begin
    FCurrentMainSurface := FOldMainSurface;
    FOldMainSurface := nil;
  end;
  ALLog('xxx.TGlCanvas.EndCanvas', 'end');
end;

constructor TGlCanvas.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault);
begin
  {$IF DEFINED(ANDROID)}
  FSurface := EGL_no_Surface;
  {$ENDIF}
  inherited;
end;

constructor TGlCanvas.CreateFromBitmap(const ABitmap: TBitmap; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault);
begin
  {$IF DEFINED(ANDROID)}
  FSurface := EGL_no_Surface;
  {$ENDIF}
  if not Initialized then
    Raise Exception.Create('Create a Window-based canvas first to establish the shared context before using Bitmap.');
  inherited;
end;

constructor TGlCanvas.CreateFromPrinter(const APrinter: TAbstractPrinter);
begin
  {$IF DEFINED(ANDROID)}
  FSurface := EGL_no_Surface;
  {$ENDIF}
  if not Initialized then
    Raise Exception.Create('Create a Window-based canvas first to establish the shared context before using Bitmap.');
  inherited;
end;

function TGlCanvas.CreateSharedContext: IGrSharedContext;
begin
  ALLog('xxx.TGlCanvas.CreateSharedContext', 'begin');
  Result := TGlSharedContext.Create(Quality);
  ALLog('xxx.TGlCanvas.CreateSharedContext', 'end');
end;

destructor TGlCanvas.Destroy;
begin
  ALLog('xxx.TGlCanvas.Destroy', 'begin');

  FBackBufferSurface := nil;
  FGrDirectContext := nil;

  If FCurrentMainSurface = FSurface then begin
    ALLog('xxx.TGlCanvas.Destroy', 'eglMakeCurrent(EGL_NO_SURFACE) - 0');
    eglMakeCurrent(TGlSharedContext(SharedContext).Display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
    fCurrentMainSurface := EGL_NO_SURFACE;
  end;

  {$IF DEFINED(ANDROID)}
  if FANativeWindow <> nil then
  begin
    eglDestroySurface(TGlSharedContext(SharedContext).Display, FSurface);
    ANativeWindow_release(FANativeWindow);
  end;
  {$ELSEIF DEFINED(MSWINDOWS)}
  if FDC <> 0 then
    ReleaseDC(WindowHandleToPlatform(Parent).Wnd, FDC);
  {$ENDIF}

  inherited;

  ALLog('xxx.TGlCanvas.Destroy', 'end');
end;

procedure TGlCanvas.DoDrawBitmap(const ABitmap: FMX.Graphics.TBitmap; const ASrcRect, ADestRect: TRectF; const AOpacity: Single; const AHighSpeed: Boolean);
begin
  ALLog('xxx.TGlCanvas.DoDrawBitmap', 'begin');
  (*
  if Parent <> nil then
    MakeCurrent
  else if Supports(FWrapper, IGrCanvasWrapper) then
    TGlSharedContext(SharedContext).MakeCurrent;  *)
  inherited;
  ALLog('xxx.TGlCanvas.DoDrawBitmap', 'end');
end;

function TGlCanvas.GetSurfaceFromWindow(const AContextHandle: THandle): TSkSurface;
var
  LGrBackendRenderTarget: IGrBackendRenderTarget;
  LGrGlFramebufferInfo: TGrGlFramebufferInfo;
  LOldMainSurface: EGLSurface;
  {$IFDEF ANDROID}
  LFormat: GLint;
  LJSurface: JSurface;
  LSurface: Pointer;
  {$ENDIF}
begin
  ALLog('xxx.TGlCanvas.GetSurfaceFromWindow', 'begin');

  If not IsMainThread then
    Raise Exception.Create('The window surface can only be accessed from the main thread');

  if FBackBufferSurface = nil then begin
    {$IF DEFINED(ANDROID)}
    if Parent is TAndroidWindowHandle then
    begin
      if TAndroidWindowHandle(Parent).Holder = nil then
        raise EGrCanvas.Create('Holder object is null in TAndroidWindowHandle');
      LSurface := (TAndroidWindowHandle(Parent).Holder.getSurface as ILocalObject).GetObjectID;
    end
    else if Parent is TAndroidHandle then
    begin
      if TAndroidHandle(Parent).Surface = nil then
        raise EGrCanvas.Create('Surface is null in TAndroidHandle');
      LJSurface := TJSurface.JavaClass.init(TAndroidHandle(Parent).Surface);
      LSurface  := TJNIResolver.JavaInstanceToID(LJSurface);
    end
    else
      raise EGrCanvas.Create('Invalid Parent type');
    FANativeWindow := ANativeWindow_fromSurface(TJNIResolver.GetJNIEnv, LSurface);
    if FANativeWindow = nil then
      raise EGrCanvas.Create('Native window creation failed');
    eglGetConfigAttrib(TGlSharedContext(SharedContext).Display, TGlSharedContext(SharedContext).Config, EGL_NATIVE_VISUAL_ID, @LFormat);
    ANativeWindow_setBuffersGeometry(FANativeWindow, 0, 0, LFormat);
    FSurface := eglCreateWindowSurface(TGlSharedContext(SharedContext).Display, TGlSharedContext(SharedContext).Config, FANativeWindow, nil);
    if FSurface = EGL_NO_SURFACE then
      raise Exception.Create('EGL window surface creation failed');
    {$ENDIF}

    LOldMainSurface := FCurrentMainSurface;
    FCurrentMainSurface := FSurface;
    FGrDirectContext := TGlSharedContext(SharedContext).GrDirectContext;
    FCurrentMainSurface := LOldMainSurface;

    {$IF DEFINED(ANDROID)}
    eglSwapInterval(TGlSharedContext(SharedContext).Display, 0);
    {$ELSEIF DEFINED(MSWINDOWS)}
    if TGlSharedContext(SharedContext).GlInterface.HasExtension(FDC, 'WGL_EXT_swap_control') then
      TGlSharedContext(SharedContext).GlInterface.SwapIntervalEXT(0);
    {$ENDIF}

    {$IFDEF IOS}
    if Result then
    begin
      GLKView(WindowHandleToPlatform(Parent).View).setContext(FContext);
      GLKView(WindowHandleToPlatform(Parent).View).bindDrawable;
    end;
    {$ENDIF}

    // Setting FBOID to 0 indicates the usage of the default framebuffer. This default
    // framebuffer is automatically created and managed by the windowing system or the
    // platform where OpenGL operates. Typically, one might retrieve the current
    // framebuffer using the following OpenGL call:
    // glGetIntegerv(GL_FRAMEBUFFER_BINDING, &framebufferID);
    // However, this approach can lead to issues, such as black screen rendering, if an
    // SKSurface or SkImage backed by a GrBackEndTexture is created prior to the call
    // to TGrBackendRenderTarget.CreateGL. This behavior suggests that Skia abstracts
    // the framebuffer in a way that affects subsequent rendering when a texture-based
    // SKSurface/SkImage is involved.
    //
    // To reproduce the issue, comment out the following code block:
    //
    //   var LTextGRBackEndTexture := FGrDirectContext.CreateTexture(100, 100, TSkColorType.BGRA8888, false, true, false);
    //   var LTestSurface := TSKSurface.MakeFromTexture(FGrDirectContext, LTextGRBackEndTexture, TGrSurfaceOrigin.TopLeft, 1, TSkColorType.BGRA8888, nil);
    //   glGetIntegerv(GL_FRAMEBUFFER_BINDING, @GLuint(LGrGlFramebufferInfo.FBOID));
    //
    // And remove or comment out the line: LGrGlFramebufferInfo.FBOID := 0;
    LGrGlFramebufferInfo.FBOID := 0;
    LGrGlFramebufferInfo.Format := {$IF DEFINED(IOS) or DEFINED(ANDROID)}GL_RGBA8_OES{$ELSE}GL_RGBA8{$ENDIF};
    LGrBackendRenderTarget := TGrBackendRenderTarget.CreateGl(Round(Width * Scale), Round(Height * Scale), Min(CanvasQualitySampleCount[Quality], FGrDirectContext.GetMaxSurfaceSampleCountForColorType(TSkColorType.RGBA8888)), TGlSharedContext(SharedContext).StencilBits, LGrGlFramebufferInfo);
    FBackBufferSurface := TSkSurface.MakeFromRenderTarget(FGrDirectContext, LGrBackendRenderTarget, TGrSurfaceOrigin.BottomLeft, TSkColorType.RGBA8888);
  end;

  Result := TSkSurface(FBackBufferSurface);

  ALLog('xxx.TGlCanvas.GetSurfaceFromWindow', 'end');
end;

class function TGlCanvas.IsSupported: Boolean;
{$IFDEF MSWINDOWS}
const
  WGLARBPixelFormatExtName = 'WGL_ARB_pixel_format';
var
  LClass: TWndClass;
  LDC: HDC;
  LEnd: MarshaledAString;
  LExtensions: MarshaledAString;
  LGetExtensionsStringARB: PFNWGLGETEXTENSIONSSTRINGARBPROC;
  LGLRC: HGLRC;
  LPixelFormat: Integer;
  LPixelFormatDescriptor: TPixelFormatDescriptor;
  LWindow: HWND;
{$ENDIF}
begin
  ALLog('xxx.TGlCanvas.IsSupported', 'begin');
{$IF DEFINED(MSWINDOWS)}
  FillChar(LClass, SizeOf(TWndClass), 0);
  LClass.lpfnWndProc   := @DefWindowProc;
  LClass.hInstance     := HInstance;
  LClass.lpszClassName := '_temp';
  if Winapi.Windows.RegisterClass(LClass) = 0 then
    Exit(False);
  try
    LWindow := CreateWindowEx(WS_EX_TOOLWINDOW, '_temp', nil, WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
    if LWindow = 0 then
      Exit(False);
    try
      LDC := GetDC(LWindow);
      if LDC = 0 then
        Exit(False);
      try
        FillChar(LPixelFormatDescriptor, SizeOf(TPixelFormatDescriptor), 0);
        LPixelFormatDescriptor.nSize        := SizeOf(TPixelFormatDescriptor);
        LPixelFormatDescriptor.nVersion     := 1;
        LPixelFormatDescriptor.dwFlags      := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
        LPixelFormatDescriptor.iPixelType   := PFD_TYPE_RGBA;
        LPixelFormatDescriptor.cColorBits   := 24;
        LPixelFormatDescriptor.cAlphaBits   := 8;
        LPixelFormatDescriptor.cStencilBits := 8;
        LPixelFormatDescriptor.iLayerType   := PFD_MAIN_PLANE;
        LPixelFormat := ChoosePixelFormat(LDC, @LPixelFormatDescriptor);
        if (LPixelFormat = 0) or (not SetPixelFormat(LDC, LPixelFormat, @LPixelFormatDescriptor)) then
          Exit(False);
        LGLRC := wglCreateContext(LDC);
        if LGLRC = 0 then
          Exit(False);
        try
          if not wglMakeCurrent(LDC, LGLRC) then
            Exit(False);
          try
            LGetExtensionsStringARB := GetProcAddress(GetModuleHandle(opengl32), 'wglGetExtensionsStringARB');
            if not Assigned(LGetExtensionsStringARB) then
            begin
              LGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
              if not Assigned(LGetExtensionsStringARB) then
                Exit(False);
            end;
            LExtensions := LGetExtensionsStringARB(LDC);
            while LExtensions^ <> #0 do
            begin
              LEnd := LExtensions;
              while (LEnd^ <> ' ') and (LEnd^ <> #0) do
                Inc(LEnd);
              if (LEnd - LExtensions = Length(WGLARBPixelFormatExtName)) and (System.AnsiStrings.StrLIComp(LExtensions, WGLARBPixelFormatExtName, LEnd - LExtensions) = 0) then
                Exit(True);
              if LEnd^ = #0 then
                Break;
              LExtensions := LEnd + 1;
            end;
            Result := False;
          finally
            wglMakeCurrent(0, 0);
          end;
        finally
          wglDeleteContext(LGLRC);
        end;
      finally
        ReleaseDC(LWindow, LDC);
      end;
    finally
      DestroyWindow(LWindow);
    end;
  finally
    Winapi.Windows.UnregisterClass('_temp', HInstance);
  end;
{$ELSE}
  Result := True;
{$ENDIF}
  ALLog('xxx.TGlCanvas.IsSupported', 'end');
end;

procedure TGlCanvas.Resized;
begin
  ALLog('xxx.TGlCanvas.Resized', 'begin');
  inherited;
//  if FBackBufferSurface <> nil then
//  begin
    (* SaveCurrent; *)
//    try
      (* MakeCurrent; *)
//      FBackBufferSurface := nil;
//    finally
      (* RestoreCurrent; *)
//    end;
//  end;
  ALLog('xxx.TGlCanvas.Resized', 'end');
end;

procedure TGlCanvas.SwapBuffers(const AContextHandle: THandle);
begin
  ALLog('xxx.TGlCanvas.SwapBuffers', 'begin');
  inherited;
  {$IF DEFINED(ANDROID)}
  if eglSwapBuffers(TGlSharedContext(SharedContext).Display, FSurface) = EGL_FALSE then
    raise EGrCanvas.Create('Could not swap buffers.');
  {$ELSEIF DEFINED(MSWINDOWS)}
  if not Winapi.Windows.SwapBuffers(FDC) then
    raise EGrCanvas.Create('Could not swap buffers.');
  {$ENDIF}
  (* RestoreCurrent; *)
  ALLog('xxx.TGlCanvas.SwapBuffers', 'end');
end;

{$IFDEF MSWINDOWS}

{ TGlInterface }

procedure TGlInterface.GetGlProc(const AName: MarshaledAString; out AProc);
begin
  ALLog('TGlInterface.GetGlProc', 'begin');
  PPointer(@AProc)^ := GetProcAddress(GetModuleHandle(opengl32), AName);
  if PPointer(@AProc)^ = nil then
    PPointer(@AProc)^ := wglGetProcAddress(AName);
  ALLog('xxx.TGlInterface.GetGlProc', 'end');
end;

function TGlInterface.HasExtension(const AHDC: HDC;
  const AName: MarshaledAString): Boolean;
var
  LEnd: MarshaledAString;
  LExtensions: MarshaledAString;
begin
  ALLog('TGlInterface.HasExtension', 'begin');
  if not Assigned(FGetExtensionsStringARB) then
    Exit(False);
  if System.AnsiStrings.StrComp(AName, 'WGL_ARB_extensions_string') = 0 then
    Exit(True);
  LExtensions := FGetExtensionsStringARB(AHDC);
  if LExtensions <> nil then
  begin
    while LExtensions^ <> #0 do
    begin
      LEnd := LExtensions;
      while (LEnd^ <> ' ') and (LEnd^ <> #0) do
        Inc(LEnd);
      if (LEnd - LExtensions = Length(AName)) and (System.AnsiStrings.StrLIComp(LExtensions, AName, LEnd - LExtensions) = 0) then
        Exit(True);
      if LEnd^ = #0 then
        Break;
      LExtensions := LEnd + 1;
    end;
  end;
  Result := False;
  ALLog('xxx.TGlInterface.HasExtension', 'end');
end;

procedure TGlInterface.Initialize;
begin
  ALLog('TGlInterface.Initialize', 'begin');
  GetGlProc('wglChoosePixelFormatARB', FChoosePixelFormatARB);
  GetGlProc('wglCreateContextAttribsARB', FCreateContextAttribsARB);
  GetGlProc('wglGetExtensionsStringARB', FGetExtensionsStringARB);
  GetGlProc('wglGetPixelFormatAttribivARB', FGetPixelFormatAttribivARB);
  GetGlProc('wglSwapIntervalEXT', FSwapIntervalEXT);
  ALLog('xxx.TGlInterface.Initialize', 'end');
end;

{$ENDIF}

{ TGlSharedContext }

(*
procedure TGlSharedContext.BeginContext;
begin
  inherited;
  try
    Inc(FBeginContextCount);
    if FBeginContextCount = 1 then
      SaveCurrent;
    MakeCurrent;
  except
    inherited EndContext;
    raise;
  end;
end;
*)

constructor TGlSharedContext.Create(const AQuality: TCanvasQuality);
begin
  ALLog('xxx.TGlSharedContext.Create', 'begin');
  inherited Create;
  CreateSharedContext;
  try
    FGrGlInterface   := TGrGlInterface.MakeNative;
    FGrDirectContext := TGrDirectContext.MakeGl(FGrGlInterface);
    if FGrDirectContext = nil then
    begin
      FGrGlInterface := nil;
      DestroySharedContext;
      raise EGrCanvas.Create('Could not create shared direct context.');
    end;
  finally
    (* RestoreCurrent; *)
  end;
  CreateThreadDictionary;
  ALLog('xxx.TGlSharedContext.Create', 'end');
end;

destructor TGlSharedContext.Destroy;
begin
  ALLog('xxx.TGlSharedContext.Destroy', 'begin');
  inherited;
  DestroyThreadDictionary;
  ALLog('xxx.TGlSharedContext.Destroy', 'end');
end;

procedure TGlSharedContext.CreateThreadDictionary;
begin
  ALLog('xxx.TGlSharedContext.CreateThreadDictionary', 'begin');
  FThreadDictionary := TDictionary<TThreadID, TThreadContextInfo>.Create;
  ALLog('xxx.TGlSharedContext.CreateThreadDictionary', 'end');
end;

procedure TGlSharedContext.DestroyThreadDictionary;
var
  LThreadContextInfo: TThreadContextInfo;
begin
  ALLog('xxx.TGlSharedContext.DestroyThreadDictionary', 'begin');
  for LThreadContextInfo in FThreadDictionary.Values do
  begin
    eglDestroySurface(FDisplay, LThreadContextInfo.Surface);
    eglDestroyContext(FDisplay, LThreadContextInfo.Context);
    LThreadContextInfo.GrDirectContext.AbandonContext;
  end;
  FreeAndNil(FThreadDictionary);
  ALLog('xxx.TGlSharedContext.DestroyThreadDictionary', 'end');
end;

procedure TGlSharedContext.CreateSharedContext;

{$IF DEFINED(ANDROID)}
  function SelectConfig(const AAttributes: TArray<Integer>;
    const AMinSampleCount: Integer = 1): EGLConfig;
  var
    LAttributes: TArray<GLint>;
    LCount: GLint;
  begin
    LAttributes := AAttributes;
    if AMinSampleCount > 1 then
    begin
      LAttributes := LAttributes + [
        EGL_SAMPLES        , AMinSampleCount,
        EGL_SAMPLE_BUFFERS , 1];
    end;
    LAttributes := LAttributes + [EGL_NONE];
    if (eglChooseConfig(FDisplay, Pointer(LAttributes), @Result, 1, @LCount) = EGL_FALSE) or (LCount <= 0) then
      Exit(nil);
  end;

var
  LConfigAttributes: TArray<GLint>;
{$ELSEIF DEFINED(MSWINDOWS)}
  // There are some known issues with implementations of certain drivers,
  // especially older ones, related to the "WGL_ARB_pixel_format" extension:
  //
  //  - The order in which pixel formats are returned by the function
  //    wglChoosePixelFormatARB may not follow the order of the smallest number
  //    of samples, meaning that the first chosen may have a higher number than
  //    necessary;
  //
  // - The parameter nNumFormats of the wglChoosePixelFormatARB function may
  //   return all formats available numbers instead of just the number that will
  //   actually be returned;
  //
  // - The "pfAttribFList" parameter cannot be "nil", and attributes must end
  //   with two null values instead of one.
  function SelectPixelFormat(const AAttributes: TArray<Integer>;
    const AMinSampleCount: Integer = 1): Integer;
  type
    TRankPixelFormat = record
      Index: Integer;
      PixelFormat: Integer;
      SampleCount: Integer;
    end;

  const
    AttributesF: array[0..1] of Single = (0, 0);

    QueryAttribute: Integer = WGL_SAMPLES_ARB;
  var
    I: Integer;
    LAttributes: TArray<Integer>;
    LCount: Cardinal;
    LPixelFormats: array[0..63] of Integer;
    LRankPixelFormat: TRankPixelFormat;
    LRankPixelFormats: TList<TRankPixelFormat>;
  begin
    if AMinSampleCount = 1 then
    begin
      LAttributes := AAttributes + [0, 0];
      if (not FGlInterface.ChoosePixelFormatARB(FDC, Pointer(LAttributes), @AttributesF, 1, @Result, @LCount)) or (LCount = 0) then
        Exit(0);
    end
    else
    begin
      LAttributes := AAttributes + [
        WGL_SAMPLE_BUFFERS_ARB , 1               ,
        WGL_SAMPLES_ARB        , AMinSampleCount ,
        0                      , 0];
      if (not FGlInterface.ChoosePixelFormatARB(FDC, Pointer(LAttributes), @AttributesF, 64, @LPixelFormats, @LCount)) or (LCount = 0) then
        Exit(0);
      LCount            := Min(LCount, 64);
      LRankPixelFormats := TList<TRankPixelFormat>.Create;
      try
        for I := 0 to LCount - 1 do
        begin
          LRankPixelFormat.Index := I;
          LRankPixelFormat.PixelFormat := LPixelFormats[I];
          FGlInterface.GetPixelFormatAttribivARB(FDC, LPixelFormats[I], 0, 1, @QueryAttribute, @LRankPixelFormat.SampleCount);
          LRankPixelFormats.Add(LRankPixelFormat);
        end;
        LRankPixelFormats.Sort(TComparer<TRankPixelFormat>.Construct(
          function(const ALeft, ARight: TRankPixelFormat): Integer
          begin
            Result := ALeft.SampleCount - ARight.SampleCount;
            if Result = 0 then
              Result := ALeft.Index - ARight.Index;
          end));
        Result := LRankPixelFormats.First.PixelFormat;
      finally
        LRankPixelFormats.Free;
      end;
    end;
  end;

const
  CoreVersions: array[0..11] of GLInt = (
    4, 3,
    4, 2,
    4, 1,
    4, 0,
    3, 3,
    3, 2);

  PixelFormatDescriptor: TPixelFormatDescriptor = (
    nSize           : SizeOf(TPixelFormatDescriptor);
    nVersion        : 1;
    dwFlags         : PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType      : PFD_TYPE_RGBA;
    cColorBits      : 24;
    cRedBits        : 0;
    cRedShift       : 0;
    cGreenBits      : 0;
    cGreenShift     : 0;
    cBlueBits       : 0;
    cBlueShift      : 0;
    cAlphaBits      : 8;
    cAlphaShift     : 0;
    cAccumBits      : 0;
    cAccumRedBits   : 0;
    cAccumGreenBits : 0;
    cAccumBlueBits  : 0;
    cAccumAlphaBits : 0;
    cDepthBits      : 0;
    cStencilBits    : 8;
    cAuxBuffers     : 0;
    iLayerType      : PFD_MAIN_PLANE;
    bReserved       : 0;
    dwLayerMask     : 0;
    dwVisibleMask   : 0;
    dwDamageMask    : 0);
var
  I: Integer;
  LClass: TWndClass;
  LDC: HDC;
  LGLRC: HGLRC;
  LPixelFormat: Integer;
  LPixelFormatAttributes: TArray<Integer>;
  LWindow: HWND;
{$ENDIF}

{$IF DEFINED(ANDROID) or DEFINED(MSWINDOWS)}
  function GetQuality: TCanvasQuality;
  begin
    if (Application.MainForm is TCustomForm) then result := TCustomForm(Application.MainForm).Quality
    else result := TCanvasQuality.HighPerformance;
  end;

var
  LQuality: TCanvasQuality;
{$ENDIF}

begin
  ALLog('xxx.TGlSharedContext.CreateSharedContext', 'begin');
  {$IF DEFINED(ANDROID)}
  FDisplay := eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if (FDisplay = EGL_NO_DISPLAY) or (eglInitialize(FDisplay, nil, nil) = EGL_FALSE) then
    raise EGlError.Create('Could not initialize the default display.');
  try
    LConfigAttributes := [
      EGL_RENDERABLE_TYPE , EGL_OPENGL_ES2_BIT ,
      EGL_SURFACE_TYPE    , EGL_PBUFFER_BIT    ,
      EGL_RED_SIZE        , 8                  ,
      EGL_GREEN_SIZE      , 8                  ,
      EGL_BLUE_SIZE       , 8                  ,
      EGL_ALPHA_SIZE      , 8                  ,
      EGL_STENCIL_SIZE    , 8];
    LQuality := GetQuality;
    if LQuality <> TCanvasQuality.HighPerformance then
    begin
      if LQuality = TCanvasQuality.HighQuality then
        FConfig := SelectConfig(LConfigAttributes, 4);
      if FConfig = nil then
        FConfig := SelectConfig(LConfigAttributes, 2);
    end;
    if FConfig = nil then
    begin
      FConfig := SelectConfig(LConfigAttributes);
      if FConfig = nil then
        raise EGlError.Create('Could not choose configuration.');
    end;
    eglGetConfigAttrib(FDisplay, FConfig, EGL_STENCIL_SIZE, @FStencilBits);
    FSurface := CreateEGLPBufferSurface(1,1);
    if FSurface = EGL_NO_SURFACE then
      raise EGlError.Create('Could not create shared surface.');
    try
      FContext := eglCreateContext(FDisplay, FConfig, EGL_NO_CONTEXT, @ContextAttributes[0]);
      if FContext = EGL_NO_CONTEXT then
        raise EGlError.Create('Could not create shared context.');
      if eglMakeCurrent(FDisplay, FSurface, FSurface, FContext) <> egl_true then
        raise Exception.Create('EGL context activation failed');
      ALLog('xxx.TGlSharedContext.CreateSharedContext', 'eglMakeCurrent(1,1) - surface: ' + inttostr(integer(eglGetCurrentSurface(EGL_DRAW))) + ' - context: ' + inttostr(integer(eglGetCurrentcontext))  + ' - GrDirectContext: nil');
    except
      eglDestroySurface(FDisplay, FSurface);
      raise;
    end;
  except
    eglTerminate(FDisplay);
    raise;
  end;
  {$ELSEIF DEFINED(IOS)}
  FLibraryHandle := SafeLoadLibrary(libGLKit);
  if FLibraryHandle = 0 then
    raise EGrCanvas.Create('Could not load "GLKit" framework.');
  try
    FContext := TEAGLContext.Wrap(TEAGLContext.Alloc.initWithAPI(kEAGLRenderingAPIOpenGLES2));
    if FContext = nil then
      raise EGlError.Create('Could not create shared context.');
    SaveAndMakeCurrent;
    glGetIntegerv(GL_STENCIL_BITS, @FStencilBits);
  except
    FreeLibrary(FLibraryHandle);
    raise;
  end;
  {$ELSEIF DEFINED(MSWINDOWS)}
  FillChar(LClass, SizeOf(TWndClass), 0);
  LClass.lpfnWndProc   := @DefWindowProc;
  LClass.hInstance     := HInstance;
  LClass.lpszClassName := '_SkDummy';
  if Winapi.Windows.RegisterClass(LClass) = 0 then
    raise EGrCanvas.Create('Could not register class.');
  try
    LWindow := CreateWindowEx(WS_EX_TOOLWINDOW, '_SkDummy', nil, WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
    if LWindow = 0 then
      raise EGrCanvas.Create('Could not create temporary window.');
    try
      SaveCurrent;
      try
        LDC := GetDC(LWindow);
        if LDC = 0 then
          raise EGrCanvas.Create('Could not get temporary device context.');
        try
          LPixelFormat := ChoosePixelFormat(LDC, @PixelFormatDescriptor);
          if (LPixelFormat = 0) or (not SetPixelFormat(LDC, LPixelFormat, @PixelFormatDescriptor)) then
            raise EGrCanvas.Create('Could not choose/set pixel format for temporary device context.');
          LGLRC := wglCreateContext(LDC);
          if LGLRC = 0 then
            raise EGlError.Create('Could not create temporary context.');
          try
            if not wglMakeCurrent(LDC, LGLRC) then
              raise EGlError.Create('Could not make temporary context as current.');
            try
              FGlInterface.Initialize;
            finally
              wglMakeCurrent(0, 0);
            end;
          finally
            wglDeleteContext(LGLRC);
          end;
        finally
          ReleaseDC(LWindow, LDC);
        end;
      finally
        DestroyWindow(LWindow);
      end;
      FWindow := CreateWindowEx(WS_EX_TOOLWINDOW, '_SkDummy', nil, WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
      if FWindow = 0 then
        raise EGrCanvas.Create('Could not create shared window.');
      try
        FDC := GetDC(FWindow);
        if FDC = 0 then
          raise EGrCanvas.Create('Could not get shared device context.');
        try
          LPixelFormatAttributes := [
            WGL_DRAW_TO_WINDOW_ARB , 1                         ,
            WGL_ACCELERATION_ARB   , WGL_FULL_ACCELERATION_ARB ,
            WGL_SUPPORT_OPENGL_ARB , 1                         ,
            WGL_DOUBLE_BUFFER_ARB  , 1                         ,
            WGL_COLOR_BITS_ARB     , 24                        ,
            WGL_ALPHA_BITS_ARB     , 8                         ,
            WGL_STENCIL_BITS_ARB   , 8];
          LQuality := GetQuality;
          if (LQuality <> TCanvasQuality.HighPerformance) and (FGlInterface.HasExtension(FDC, 'WGL_ARB_multisample')) then
          begin
            if LQuality = TCanvasQuality.HighQuality then
              FPixelFormat := SelectPixelFormat(LPixelFormatAttributes, 4);
            if FPixelFormat = 0 then
              FPixelFormat := SelectPixelFormat(LPixelFormatAttributes, 2);
          end;
          if FPixelFormat = 0 then
          begin
            FPixelFormat := SelectPixelFormat(LPixelFormatAttributes);
            if FPixelFormat = 0 then
              raise EGlError.Create('Could not choose pixel format.');
          end;
          if (not DescribePixelFormat(FDC, FPixelFormat, SizeOf(TPixelFormatDescriptor), FPixelFormatDescriptor)) or (not SetPixelFormat(FDC, FPixelFormat, @PixelFormatDescriptor)) then
            raise EGrCanvas.Create('Could not set pixel format for shared device context.');
          FStencilBits := FPixelFormatDescriptor.cStencilBits;
          if FGlInterface.HasExtension(FDC, 'WGL_ARB_create_context') then
          begin
            FContextAttributes := [
              WGL_CONTEXT_MAJOR_VERSION_ARB , -1                               ,
              WGL_CONTEXT_MINOR_VERSION_ARB , -1                               ,
              WGL_CONTEXT_PROFILE_MASK_ARB  , WGL_CONTEXT_CORE_PROFILE_BIT_ARB ,
              0];
            for I := 0 to Length(CoreVersions) div 2 - 1 do
            begin
              FContextAttributes[1] := CoreVersions[I * 2];
              FContextAttributes[3] := CoreVersions[I * 2 + 1];
              FGLRC := FGlInterface.CreateContextAttribsARB(FDC, 0, Pointer(FContextAttributes));
              if FGLRC <> 0 then
                Break;
            end;
          end;
          if FGLRC = 0 then
          begin
            FContextAttributes := nil;
            FGLRC := wglCreateContext(FDC);
            if FGLRC = 0 then
              raise EGlError.Create('Could not create shared context.');
          end;
          MakeCurrent;
        except
          ReleaseDC(FWindow, FDC);
          raise;
        end;
      except
        RestoreCurrent;
        raise;
      end;
    except
      DestroyWindow(FWindow);
      raise;
    end;
  except
    Winapi.Windows.UnregisterClass('_SkDummy', HInstance);
    raise;
  end;
  {$ENDIF}
  ALLog('xxx.TGlSharedContext.CreateSharedContext', 'end');
end;

procedure TGlSharedContext.DestroySharedContext;
begin
  ALLog('xxx.TGlSharedContext.DestroySharedContext', 'begin');
  {$IF DEFINED(ANDROID)}
  eglDestroyContext(FDisplay, FContext);
  eglDestroySurface(FDisplay, FSurface);
  eglTerminate(FDisplay);
  {$ELSEIF DEFINED(IOS)}
  FContext.release;
  {$ELSEIF DEFINED(MSWINDOWS)}
  wglDeleteContext(FGLRC);
  ReleaseDC(FWindow, FDC);
  DestroyWindow(FWindow);
  Winapi.Windows.UnregisterClass('_SkDummy', HInstance);
  {$ENDIF}
  ALLog('xxx.TGlSharedContext.DestroySharedContext', 'end');
end;

(*
procedure TGlSharedContext.DestroyContext;
begin
  SaveAndMakeCurrent;
  try
    FGrGlInterface := nil;
    inherited;
  finally
    RestoreCurrent;
  end;
  DestroySharedContext;
end;
*)

(*
procedure TGlSharedContext.EndContext;
begin
  Dec(FBeginContextCount);
  if FBeginContextCount = 0 then
    RestoreCurrent;
  inherited;
end;
*)

procedure TGlSharedContext.FinalizeTextureCache(const ABitmap: TGrBitmapHandle);
begin
  ALLog('xxx.TGlSharedContext.FinalizeTextureCache', 'begin');
  BeginContext;
  try
    inherited;
  finally
    EndContext;
  end;
  ALLog('xxx.TGlSharedContext.FinalizeTextureCache', 'end');
end;

function TGlSharedContext.GetTextureColorType: TSkColorType;
begin
  ALLog('xxx.TGlSharedContext.GetTextureColorType', 'begin');
  Result := TSkColorType.RGBA8888;
  ALLog('xxx.TGlSharedContext.GetTextureColorType', 'end');
end;

function TGlSharedContext.GetTextureOrigin: TGrSurfaceOrigin;
begin
  ALLog('xxx.TGlSharedContext.GetTextureOrigin', 'begin');
  Result := TGrSurfaceOrigin.BottomLeft;
  ALLog('xxx.TGlSharedContext.GetTextureOrigin', 'end');
end;

function TGlSharedContext.GetGrDirectContext: IGrDirectContext;
begin
  ALLog('xxx.TGlSharedContext.GetGrDirectContext', 'begin');
  result := ActivateContext.GrDirectContext;
  ALLog('xxx.TGlSharedContext.GetGrDirectContext', 'end - ' + inttostr(integer(pointer(result))));
end;

procedure TGlSharedContext.InitializeTextureCache(
  const ABitmap: TGrBitmapHandle);
begin
  ALLog('xxx.TGlSharedContext.InitializeTextureCache', 'begin');
  BeginContext;
  try
    ABitmap.Cache := TSkImage.MakeCrossContext(FGrDirectContext, TSkImageInfo.Create(ABitmap.Width, ABitmap.Height, SkFmxColorType[ABitmap.PixelFormat]), ABitmap.Pixels, ABitmap.Width * PixelFormatBytes[ABitmap.PixelFormat], False);
    FGrDirectContext.Submit(True);
  finally
    EndContext;
  end;
  ALLog('xxx.TGlSharedContext.InitializeTextureCache', 'end');
end;

(*
procedure TGlSharedContext.MakeCurrent;
begin
  {$IF DEFINED(ANDROID)}
  if eglMakeCurrent(FDisplay, FSurface, FSurface, FContext) = EGL_FALSE then
  {$ELSEIF DEFINED(IOS)}
  if not TEAGLContext.OCClass.setCurrentContext(FContext) then
  {$ELSEIF DEFINED(MSWINDOWS)}
  if not wglMakeCurrent(FDC, FGLRC) then
  {$ENDIF}
    raise EGlError.Create('Could not make shared context as current.');
end;
*)

{$IF DEFINED(ANDROID)}
function TGlSharedContext.CreateEGLPBufferSurface(const Width, Height: Integer): EGLSurface;
var
  SurfaceAttributes: TContextAttributes;
begin
  ALLog('xxx.TGlSharedContext.CreateEGLPBufferSurface', 'begin');
  SurfaceAttributes := [EGL_WIDTH, Width, EGL_HEIGHT, Height, EGL_NONE];
  Result := eglCreatePbufferSurface(FDisplay, FConfig, @SurfaceAttributes[0]);
  if Result = EGL_NO_SURFACE then
    raise EGlError.Create('Cannot create EGL PBuffer Surface');
  ALLog('xxx.TGlSharedContext.CreateEGLPBufferSurface', 'end');
end;
{$ENDIF}

function TGlSharedContext.ActivateContext: TThreadContextInfo;
begin
  ALLog('xxx.TGlSharedContext.RefreshContext', 'begin');
  if IsMainThread then
  begin
    result.Surface := eglGetCurrentSurface(EGL_DRAW);
    result.Context := FContext;
    result.GrDirectContext := FGrDirectContext;
    if (TGLCanvas.CurrentMainSurface <> EGL_NO_SURFACE) and
       (TGLCanvas.CurrentMainSurface <> result.Surface) then begin
      Result.Surface := TGLCanvas.CurrentMainSurface;
      if eglMakeCurrent(FDisplay, Result.Surface, Result.Surface, FContext) <> EGL_TRUE then
        raise Exception.Create('EGL context activation failed');
      ALLog('xxx.TGlSharedContext.ActivateContext', 'eglMakeCurrent(CurrentMainSurface) - surface: ' + inttostr(integer(eglGetCurrentSurface(EGL_DRAW))) + ' - context: ' + inttostr(integer(eglGetCurrentcontext))  + ' - GrDirectContext: ' + inttostr(integer(pointer(result.GrDirectContext))));
    end
    else if (TGLCanvas.CurrentMainSurface = EGL_NO_SURFACE) and
            (FSurface <> result.Surface) then begin
      Result.Surface := FSurface;
      if eglMakeCurrent(FDisplay, Result.Surface, Result.Surface, FContext) <> EGL_TRUE then
        raise Exception.Create('EGL context activation failed');
      ALLog('xxx.TGlSharedContext.ActivateContext', 'eglMakeCurrent(1,1) - surface: ' + inttostr(integer(eglGetCurrentSurface(EGL_DRAW))) + ' - context: ' + inttostr(integer(eglGetCurrentcontext))  + ' - GrDirectContext: ' + inttostr(integer(pointer(result.GrDirectContext))));
    end;
  end
  else
  begin
    TMonitor.Enter(FThreadDictionary);
    try
      if not FThreadDictionary.TryGetValue(TThread.CurrentThread.ThreadID, Result) then
      begin
        Result.Context := eglCreateContext(FDisplay, FConfig, FContext, @ContextAttributes[0]);
        Result.Surface := CreateEGLPBufferSurface(1, 1);
        if eglMakeCurrent(FDisplay, Result.Surface, Result.Surface, Result.Context) <> EGL_TRUE then
          raise Exception.Create('EGL context activation failed');
        Result.GrDirectContext := TGrDirectContext.MakeGl(FGrGlInterface);
        ALLog('xxx.TGlSharedContext.ActivateContext', 'eglMakeCurrent(1,1) - surface: ' + inttostr(integer(eglGetCurrentSurface(EGL_DRAW))) + ' - context: ' + inttostr(integer(eglGetCurrentcontext))  + ' - GrDirectContext: ' + inttostr(integer(pointer(result.GrDirectContext))));
        FThreadDictionary.Add(TThread.CurrentThread.ThreadID, Result);
        exit;
      end;
    finally
      TMonitor.Exit(FThreadDictionary);
    end;
    if eglGetCurrentContext <> Result.Context then begin
      if eglMakeCurrent(FDisplay, Result.Surface, Result.Surface, Result.Context) <> EGL_TRUE then
        raise Exception.Create('EGL context activation failed');
    end;
  end;
  ALLog('xxx.TGlSharedContext.RefreshContext', 'end - surface: ' + inttostr(integer(eglGetCurrentSurface(EGL_DRAW))) + ' - context: ' + inttostr(integer(eglGetCurrentcontext))  + ' - GrDirectContext: ' + inttostr(integer(pointer(result.GrDirectContext))));



(*
    {$ELSEIF DEFINED(IOS)}
    FContext := TEAGLContext.Wrap(TEAGLContext.Alloc.initWithAPI(kEAGLRenderingAPIOpenGLES2, TGlSharedContext(SharedContext).Context.sharegroup));
    if FContext = nil then
      Exit;
    {$ELSEIF DEFINED(MSWINDOWS)}
    if FDC = 0 then
    begin
      FDC := GetDC(WindowHandleToPlatform(Parent).Wnd);
      if FDC = 0 then
        Exit;
      SetPixelFormat(FDC, TGlSharedContext(SharedContext).PixelFormat, @TGlSharedContext(SharedContext).PixelFormatDescriptor);
    end;
    if TGlSharedContext(SharedContext).ContextAttributes <> nil then
    begin
      FGLRC := TGlSharedContext(SharedContext).GlInterface.CreateContextAttribsARB(FDC, TGlSharedContext(SharedContext).GLRC, Pointer(TGlSharedContext(SharedContext).ContextAttributes));
      if FGLRC = 0 then
        Exit;
    end
    else
    begin
      FGLRC := wglCreateContext(FDC);
      if FGLRC = 0 then
        Exit;
      if not wglShareLists(TGlSharedContext(SharedContext).GLRC, FGLRC) then
      begin
        wglDeleteContext(FGLRC);
        Exit;
      end;
    end;
    {$ENDIF}
*)
end;

procedure TGlSharedContext.RefreshContext;
begin
  ActivateContext;
end;

(*
procedure TGlSharedContext.RestoreCurrent;
begin
  {$IF DEFINED(ANDROID)}
  eglMakeCurrent(FOldDisplay, FOldDraw, FOldRead, FOldContext);
  {$ELSEIF DEFINED(IOS)}
  TEAGLContext.OCClass.setCurrentContext(FOldContext);
  {$ELSEIF DEFINED(MSWINDOWS)}
  wglMakeCurrent(FOldDC, FOldGLRC);
  {$ENDIF}
end;
*)

(*
procedure TGlSharedContext.SaveAndMakeCurrent;
begin
  SaveCurrent;
  MakeCurrent;
end;
*)

(*
procedure TGlSharedContext.SaveCurrent;
begin
  {$IF DEFINED(ANDROID)}
  FOldDisplay := eglGetCurrentDisplay;
  if FOldDisplay = EGL_NO_DISPLAY then
  begin
    FOldDisplay := FDisplay;
    FOldDraw    := EGL_NO_SURFACE;
    FOldRead    := EGL_NO_SURFACE;
    FOldContext := EGL_NO_CONTEXT;
  end
  else
  begin
    FOldDraw    := eglGetCurrentSurface(EGL_DRAW);
    FOldRead    := eglGetCurrentSurface(EGL_READ);
    FOldContext := eglGetCurrentContext;
  end;
  {$ELSEIF DEFINED(IOS)}
  FOldContext := TEAGLContext.Wrap(TEAGLContext.OCClass.currentContext);
  {$ELSEIF DEFINED(MSWINDOWS)}
  FOldDC   := wglGetCurrentDC;
  FOldGLRC := wglGetCurrentContext;
  {$ENDIF}
end;
*)

{$HPPEMIT END '#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_SKIA_CANVAS_GL)'}
{$HPPEMIT END '    using ::Fmx::Skia::Canvas::Gl::TGlSharedContextCustom;'}
{$HPPEMIT END '#endif'}

initialization
  RegisterSkiaRenderCanvas(TGlCanvas, False,
    function: Boolean
    begin
      Result := TGlCanvas.IsSupported;
    end);
//{$ELSE}
//implementation
//{$ENDIF}
end.
