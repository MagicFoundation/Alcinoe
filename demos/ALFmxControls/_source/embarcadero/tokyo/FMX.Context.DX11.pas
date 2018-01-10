{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Context.DX11;

{.$DEFINE DXDEBUG}

interface

{$SCOPEDENUMS ON}

uses
  Winapi.DXGI, Winapi.D3D11, Winapi.D3DCommon, System.Types, System.UITypes, System.SysUtils, System.Classes,
  System.Math, System.Generics.Collections, System.Math.Vectors, FMX.Types3D, FMX.Types;

type
  TCustomDX11Context = class(TContext3D)
  private class var
    FDriverType: D3D_DRIVER_TYPE;
    FFeatureLevel: D3D_FEATURE_LEVEL;
    FDriverSupportTested: Boolean;
    FSharedDevice: ID3D11Device;
    FSharedContext: ID3D11DeviceContext;
    FDXGIFactory: IDXGIFactory;
    FVB: ID3D11Buffer;
    FIB: ID3D11Buffer;
    FVBLockPos, FIBLockPos: Integer;
    FBlankTexture: ID3D11Texture2D;
  private
    class function GetSharedDevice: ID3D11Device; static;
    class procedure CreateSharedDevice; static;
    class function GetDXGIFactory: IDXGIFactory; static;
    class procedure CreateBlankTexture; static;
    class function GetBlankTexture: ID3D11Texture2D; static;
    class function GetSharedContext: ID3D11DeviceContext; static;
    class function GetFeatureLevel: D3D_FEATURE_LEVEL; static;
  protected
    function GetIndexBufferSupport: TContext3D.TIndexBufferSupport; override;
  public
    class procedure DestroySharedDevice; static;
    class procedure TestDriverSupport(out DriverType: D3D_DRIVER_TYPE; out FeatureLevel: TD3D_FEATURE_LEVEL);
    class property FeatureLevel: D3D_FEATURE_LEVEL read GetFeatureLevel;
    class property SharedDevice: ID3D11Device read GetSharedDevice;
    class property SharedContext: ID3D11DeviceContext read GetSharedContext;
    class property DXGIFactory: IDXGIFactory read GetDXGIFactory;
    class property BlankTexture: ID3D11Texture2D read GetBlankTexture;

    class function PixelFormat: TPixelFormat; override;
    class function MaxTextureSize: Integer; override;
  end;

procedure RegisterContextClasses;
procedure UnregisterContextClasses;

implementation

uses
  Winapi.Windows, Winapi.DXTypes, System.Win.ComObj, FMX.Forms, FMX.Platform.Win, Winapi.DxgiType,
  Winapi.DxgiFormat, FMX.Context.DX9, FMX.Canvas.GPU, FMX.Graphics, FMX.Consts, FMX.Utils, FMX.Platform;

var
  HR: HResult;
  VBSize: Integer = $FFFF * 56;
  IBSize: Integer = $FFFF * 2 * 2;

type
  TDX11Context = class(TCustomDX11Context)
  private class var
    FResources: IInterfaceList;
    FVSSlot: ID3D11Buffer;
    FPSSlot: ID3D11Buffer;
    FVSSlotModified, FPSSlotModified: Boolean;
    FVSBuf, FPSBuf: array of Byte;
    FInputLayout: ID3D11InputLayout;
    FResourceViews: array [0..16] of ID3D11ShaderResourceView;
    FSampleStates: array [0..16] of ID3D11SamplerState;
    FBlendDesc: TD3D11_BLEND_DESC;
    FBlendState: ID3D11BlendState;
    FBlendStateModified: Boolean;
    FRasterizerDesc: TD3D11_RASTERIZER_DESC;
    FRasterizerState: ID3D11RasterizerState;
    FRasterizerStateModified: Boolean;
    FDepthStencilDesc: TD3D11_DEPTH_STENCIL_DESC;
    FDepthStencilState: ID3D11DepthStencilState;
    FDepthStencilModified: Boolean;
    FStencilRef: Integer;
    FBufferSize: TSize;
  private
    class function AddResource(const Resource: IInterface): THandle;
    class procedure RemoveResource(Resource: THandle);
    class function ResourceToVertexShader(Resource: THandle): ID3D11VertexShader;
    class function ResourceToPixelShader(Resource: THandle): ID3D11PixelShader;
    class function ResourceToTexture(Resource: THandle): ID3D11Texture2D;
  private
    { states }
    FSavedRT: ID3D11RenderTargetView;
    FSavedDepth: ID3D11DepthStencilView;
    FSavedViewportNum: Cardinal;
    FSavedViewport: TD3D11_Viewport;
    { swapchain }
    FSwapChain: IDXGISwapChain;
    FRenderTargetView: ID3D11RenderTargetView;
    FDepthStencilTex: ID3D11Texture2D;
    FDepthStencilView: ID3D11DepthStencilView;
    { ms }
    FRenderTargetMSTex: ID3D11Texture2D;
    { copy }
    FCopyBuffer: ID3D11Texture2D;
    procedure FindBestMultisampleType(Format: DXGI_FORMAT; Multisample: TMultisample; out SampleCount, QualityLevel: Integer);
    procedure SetTexture(const AUnit: Integer; const Texture: TTexture);
    class procedure FindBestShaderSource(const Shader: TContextShader; out Source: TContextShaderSource);
  protected
    { assign }
    class function Valid: Boolean; override;
    { buffer }
    procedure DoCreateBuffer; override;
    procedure DoResize; override;
    procedure DoFreeBuffer; override;
    procedure DoCopyToBitmap(const Dest: TBitmap; const ARect: TRect); override;
    procedure DoCopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect); override;
    { scene }
    function DoBeginScene: Boolean; override;
    procedure DoEndScene; override;
    { states }
    procedure DoClear(const ATarget: TClearTargets; const AColor: TAlphaColor; const ADepth: single; const AStencil: Cardinal); override;
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
    { bitmap }
    class function DoBitmapToTexture(const Bitmap: TBitmap): TTexture; override;
    { shaders }
    class procedure DoInitializeShader(const Shader: TContextShader); override;
    class procedure DoFinalizeShader(const Shader: TContextShader); override;
    procedure DoSetShaders(const VertexShader, PixelShader: TContextShader); override;
    procedure DoSetShaderVariable(const Name: string; const Data: array of TVector3D); override;
    procedure DoSetShaderVariable(const Name: string; const Texture: TTexture); override;
    procedure DoSetShaderVariable(const Name: string; const Matrix: TMatrix3D); override;
    { constructors }
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer; const AMultisample: TMultisample;
      const ADepthStencil: Boolean); override;
    constructor CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
      const ADepthStencil: Boolean); override;

    class function PixelFormat: TPixelFormat; override;
  end;

{$R-}

var
  PrevFPUState: TArithmeticExceptionMask;

procedure SaveClearFPUState; inline;
begin
  PrevFPUState:= GetExceptionMask;
  SetExceptionMask(exAllArithmeticExceptions);
end;

procedure RestoreFPUState; inline;
begin
  SetExceptionMask(PrevFPUState);
end;

function ColorToD3DColor(const AColor: TAlphaColor): TFourSingleArray;
begin
  Result[0] := TAlphaColorRec(AColor).R / $FF;
  Result[1] := TAlphaColorRec(AColor).G / $FF;
  Result[2] := TAlphaColorRec(AColor).B / $FF;
  Result[3] := TAlphaColorRec(AColor).A / $FF;
end;

function TexturePixelFormatToDX(PF: TPixelFormat): DXGI_FORMAT;
begin
  case PF of
    TPixelFormat.RGBA16:
      Result := DXGI_FORMAT_R16G16B16A16_UNORM;
    TPixelFormat.RGB10_A2:
      Result := DXGI_FORMAT_R10G10B10A2_UNORM;
    TPixelFormat.BGRA:
      Result := DXGI_FORMAT_B8G8R8A8_UNORM;
    TPixelFormat.BGR:
      Result := DXGI_FORMAT_B8G8R8X8_UNORM;
    TPixelFormat.RGBA:
      Result := DXGI_FORMAT_R8G8B8A8_UNORM;
    TPixelFormat.BGR_565:
      Result := DXGI_FORMAT_B5G6R5_UNORM;
    TPixelFormat.BGR5_A1:
      Result := DXGI_FORMAT_B5G5R5A1_UNORM;
    TPixelFormat.LA:
      Result := DXGI_FORMAT_R8G8_UNORM;
    TPixelFormat.R16F:
      Result := DXGI_FORMAT_R16_FLOAT;
    TPixelFormat.RG16F:
      Result := DXGI_FORMAT_R16G16_FLOAT;
    TPixelFormat.RGBA16F:
      Result := DXGI_FORMAT_R16G16B16A16_FLOAT;
    TPixelFormat.R32F:
      Result := DXGI_FORMAT_R32_FLOAT;
    TPixelFormat.RG32F:
      Result := DXGI_FORMAT_R32G32_FLOAT;
    TPixelFormat.RGBA32F:
      Result := DXGI_FORMAT_R32G32B32A32_FLOAT;
    TPixelFormat.A:
      Result := DXGI_FORMAT_A8_UNORM;
  else
    Result := DXGI_FORMAT_UNKNOWN;
  end;
end;

function GetSlotSize(const Source: TContextShaderSource): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(Source.Variables) do
    Result := Result + Max(Source.Variables[I].Size, 16);
end;

function D3D11CreateDevice1Ex(DriverType: D3D_DRIVER_TYPE; Flags: LongWord;
  out Device: ID3D11Device; out Context: ID3D11DeviceContext; out FeatureLevel: TD3D_FEATURE_LEVEL): HResult;
const
  RequestLevels: array [0..5] of D3D_FEATURE_LEVEL = (
    D3D_FEATURE_LEVEL_11_0,
    D3D_FEATURE_LEVEL_10_1,
    D3D_FEATURE_LEVEL_10_0,
    D3D_FEATURE_LEVEL_9_3,
    D3D_FEATURE_LEVEL_9_2,
    D3D_FEATURE_LEVEL_9_1
  );
  DX9Levels: array [0..2] of D3D_FEATURE_LEVEL = (
    D3D_FEATURE_LEVEL_9_3,
    D3D_FEATURE_LEVEL_9_2,
    D3D_FEATURE_LEVEL_9_1
  );
begin
  if GlobalUseDXInDX9Mode then
    Result := D3D11CreateDevice(nil, DriverType, 0, Flags, @DX9Levels[0], Length(DX9Levels), D3D11_SDK_VERSION, Device,
      FeatureLevel, Context)
  else
    Result := D3D11CreateDevice(nil, DriverType, 0, Flags, @RequestLevels[0], Length(RequestLevels), D3D11_SDK_VERSION,
      Device, FeatureLevel, Context);
end;

{ TCustomDX11Context }

function TCustomDX11Context.GetIndexBufferSupport: TContext3D.TIndexBufferSupport;
begin
  if FFeatureLevel > D3D_FEATURE_LEVEL_9_1 then
    Result := TIndexBufferSupport.Int32
  else
    Result := TIndexBufferSupport.Int16;
end;

class procedure TCustomDX11Context.CreateSharedDevice;
var
  Flags: Cardinal;
  DXGIDevice: IDXGIDevice;
  DXGIAdapter: IDXGIAdapter;
{$IFDEF DXDEBUG}
  DebugText: string;
{$ENDIF}
begin
  if FSharedDevice = nil then
  begin
    SaveClearFPUState;
    try
      Flags := {$IFDEF DXDEBUG}D3D11_CREATE_DEVICE_DEBUG{$ELSE}0{$ENDIF};
      Flags := Flags or D3D11_CREATE_DEVICE_BGRA_SUPPORT;
      if Succeeded(D3D11CreateDevice1Ex(FDriverType, Flags, FSharedDevice, FSharedContext, FFeatureLevel)) then
      begin
        HR := FSharedDevice.CreateBuffer(TD3D11_BUFFER_DESC.Create(VBSize, D3D11_BIND_VERTEX_BUFFER,
          D3D11_USAGE_DYNAMIC, D3D11_CPU_ACCESS_WRITE), nil, FVB);
        HR := FSharedDevice.CreateBuffer(TD3D11_BUFFER_DESC.Create(IBSize, D3D11_BIND_INDEX_BUFFER,
          D3D11_USAGE_DYNAMIC, D3D11_CPU_ACCESS_WRITE), nil, FIB);

        DXGIAdapter := nil;
        FDXGIFactory := nil;

        if Succeeded(FSharedDevice.QueryInterface(IDXGIDevice, DXGIDevice)) and (DXGIDevice <> nil) and
          Succeeded(DXGIDevice.GetParent(IDXGIAdapter, DXGIAdapter)) and (DXGIAdapter <> nil) then
            DXGIAdapter.GetParent(IDXGIFactory, FDXGIFactory);

        if FDXGIFactory = nil then
          raise ECannotAcquireDXGIFactory.CreateFmt(SCannotAcquireDXGIFactory, [ClassName]);
      end
      else
        raise ECannotCreateD3DDevice.CreateFmt(SCannotCreateD3DDevice, [ClassName]);
    finally
      RestoreFPUState;
    end;

{$IFDEF DXDEBUG}
    case FFeatureLevel of
      D3D_FEATURE_LEVEL_9_1:
        DebugText := '9.1';
      D3D_FEATURE_LEVEL_9_2:
        DebugText := '9.2';
      D3D_FEATURE_LEVEL_9_3:
        DebugText := '9.3';
      D3D_FEATURE_LEVEL_10_0:
        DebugText := '10.0';
      D3D_FEATURE_LEVEL_10_1:
        DebugText := '10.1';
      D3D_FEATURE_LEVEL_11_0:
        DebugText := '11.0';
    else
      DebugText := 'unknown';
    end;
    if FDriverType = D3D_DRIVER_TYPE_HARDWARE then
      DebugText := DebugText + ' (HAL)'
    else
      DebugText := DebugText + ' (WARP)';
    OutputDebugString(PChar('DX11Context is using Feature Level ' + DebugText));
{$ENDIF}

    FillChar(TDX11Context.FBlendDesc, SizeOf(TDX11Context.FBlendDesc), 0);
    TDX11Context.FBlendDesc.AlphaToCoverageEnable := False;
    TDX11Context.FBlendDesc.RenderTarget[0].BlendEnable := True;
    TDX11Context.FBlendDesc.RenderTarget[0].SrcBlend := D3D11_BLEND_ONE;
    TDX11Context.FBlendDesc.RenderTarget[0].DestBlend := D3D11_BLEND_INV_SRC_ALPHA;
    TDX11Context.FBlendDesc.RenderTarget[0].BlendOp := D3D11_BLEND_OP_ADD;
    TDX11Context.FBlendDesc.RenderTarget[0].SrcBlendAlpha := D3D11_BLEND_ONE;
    TDX11Context.FBlendDesc.RenderTarget[0].DestBlendAlpha := D3D11_BLEND_INV_SRC_ALPHA;
    TDX11Context.FBlendDesc.RenderTarget[0].BlendOpAlpha := D3D11_BLEND_OP_ADD;
    TDX11Context.FBlendDesc.RenderTarget[0].RenderTargetWriteMask := Byte(D3D11_COLOR_WRITE_ENABLE_ALL);
    TDX11Context.FBlendStateModified := True;

    TDX11Context.FRasterizerDesc.FillMode := D3D11_FILL_SOLID;
    TDX11Context.FRasterizerDesc.CullMode := D3D11_CULL_BACK;
    TDX11Context.FRasterizerDesc.FrontCounterClockwise := False;
    TDX11Context.FRasterizerDesc.DepthBias := 0;
    TDX11Context.FRasterizerDesc.DepthBiasClamp := 0;
    TDX11Context.FRasterizerDesc.SlopeScaledDepthBias := 0;
    TDX11Context.FRasterizerDesc.DepthClipEnable := True;
    TDX11Context.FRasterizerDesc.ScissorEnable := False;
    TDX11Context.FRasterizerDesc.MultisampleEnable := True;
    TDX11Context.FRasterizerDesc.AntialiasedLineEnable := True;
    TDX11Context.FRasterizerStateModified := True;

    FillChar(TDX11Context.FDepthStencilDesc, SizeOf(TDX11Context.FDepthStencilDesc), 0);
    TDX11Context.FDepthStencilDesc.DepthEnable := False;
    TDX11Context.FDepthStencilDesc.DepthWriteMask := D3D11_DEPTH_WRITE_MASK_ALL;
    TDX11Context.FDepthStencilDesc.DepthFunc := D3D11_COMPARISON_LESS_EQUAL;
    TDX11Context.FDepthStencilDesc.StencilEnable := False;
    TDX11Context.FDepthStencilModified := True;
    TDX11Context.FStencilRef := 0;
  end;
end;

class procedure TCustomDX11Context.TestDriverSupport(out DriverType: D3D_DRIVER_TYPE;
  out FeatureLevel: TD3D_FEATURE_LEVEL);
var
  DX11Lib: THandle;
  Device: ID3D11Device;
  Context: ID3D11DeviceContext;
begin
  if not FDriverSupportTested then
  begin
    FDriverSupportTested := True;
    FDriverType := D3D_DRIVER_TYPE_NULL;
    FFeatureLevel := D3D_FEATURE_LEVEL_11_0;
    DX11Lib := LoadLibrary(D3D11dll);
    if DX11Lib <> 0 then
    try
      if GlobalUseDX then
      begin
        SaveClearFPUState;
        try
          if (not GlobalUseDXSoftware) and Succeeded(D3D11CreateDevice1Ex(D3D_DRIVER_TYPE_HARDWARE,
            D3D11_CREATE_DEVICE_BGRA_SUPPORT, Device, Context, FFeatureLevel)) then
          begin
            Device := nil;
            Context := nil;
            FDriverType := D3D_DRIVER_TYPE_HARDWARE;
          end
          else if Succeeded(D3D11CreateDevice1Ex(D3D_DRIVER_TYPE_WARP, D3D11_CREATE_DEVICE_BGRA_SUPPORT, Device,
            Context, FFeatureLevel)) then
          begin
            Device := nil;
            Context := nil;
            FDriverType := D3D_DRIVER_TYPE_WARP;
          end;
        finally
          RestoreFPUState;
        end;
      end;
    finally
      FreeLibrary(DX11Lib);
    end;
  end;

  DriverType := FDriverType;
  FeatureLevel := FFeatureLevel;
end;

class procedure TCustomDX11Context.CreateBlankTexture;
var
  Desc: TD3D11_TEXTURE2D_DESC;
  Data: D3D11_SUBRESOURCE_DATA;
  Color: UInt;
begin
  CreateSharedDevice;

  FillChar(Desc, SizeOf(D3D11_TEXTURE2D_DESC), 0);
  Desc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
  Desc.Width := 1;
  Desc.Height := 1;
  Desc.MipLevels := 1;
  Desc.ArraySize := 1;
  Desc.SampleDesc.Count := 1;
  Desc.SampleDesc.Quality := 0;
  Desc.Usage := D3D11_USAGE_IMMUTABLE;
  Desc.BindFlags := D3D11_BIND_SHADER_RESOURCE;
  Color := $FFFFFFFF;
  Data.pSysMem := @Color;
  Data.SysMemPitch := 4;
  Data.SysMemSlicePitch := 0;
  SaveClearFPUState;
  try
    HR := SharedDevice.CreateTexture2D(Desc, @Data, FBlankTexture);
  finally
    RestoreFPUState;
  end;
end;

class function TCustomDX11Context.GetBlankTexture: ID3D11Texture2D;
begin
  CreateBlankTexture;
  Result := FBlankTexture;
end;

class function TCustomDX11Context.GetDXGIFactory: IDXGIFactory;
begin
  CreateSharedDevice;
  Result := FDXGIFactory;
end;

class function TCustomDX11Context.GetFeatureLevel: D3D_FEATURE_LEVEL;
begin
  CreateSharedDevice;
  Result := FFeatureLevel;
end;

class function TCustomDX11Context.GetSharedContext: ID3D11DeviceContext;
begin
  CreateSharedDevice;
  Result := FSharedContext;
end;

class function TCustomDX11Context.GetSharedDevice: ID3D11Device;
begin
  CreateSharedDevice;
  Result := FSharedDevice;
end;

class function TCustomDX11Context.MaxTextureSize: Integer;
begin
  CreateSharedDevice;
  case FFeatureLevel of
    D3D_FEATURE_LEVEL_9_1:
      Result := 2048;
    D3D_FEATURE_LEVEL_9_2:
      Result := 2048;
    D3D_FEATURE_LEVEL_9_3:
      Result := 4096;
    D3D_FEATURE_LEVEL_10_0:
      Result := 8192;
    D3D_FEATURE_LEVEL_10_1:
      Result := 8192;
    D3D_FEATURE_LEVEL_11_0:
      Result := 16384;
  else
    if FFeatureLevel > D3D_FEATURE_LEVEL_11_0 then
      Result := 16384
    else
      Result := 2048;
  end;
end;

class function TCustomDX11Context.PixelFormat: TPixelFormat;
begin
  Result := TPixelFormat.BGRA;
end;

class procedure TCustomDX11Context.DestroySharedDevice;
begin
  FVB := nil;
  FIB := nil;
  FDXGIFactory := nil;
  FBlankTexture := nil;
  FSharedDevice := nil;
end;

{ TDX11Context }

constructor TDX11Context.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AMultisample: TMultisample; const ADepthStencil: Boolean);
begin
  inherited;
  CreateSharedDevice;
  CreateBuffer;
end;

constructor TDX11Context.CreateFromTexture(const ATexture: TTexture; const AMultisample: TMultisample;
  const ADepthStencil: Boolean);
begin
  inherited;
  CreateSharedDevice;
  CreateBuffer;
end;

class function TDX11Context.Valid: Boolean;
begin
  Result := SharedContext <> nil;
end;

class function TDX11Context.PixelFormat: TPixelFormat;
begin
  Result := TPixelFormat.BGRA;
end;

procedure TDX11Context.FindBestMultisampleType(Format: DXGI_FORMAT; Multisample: TMultisample; out SampleCount, QualityLevel: Integer);
var
  I, MaxSampleNo: Integer;
  QuaLevels: Cardinal;
  MultisampleCount: Integer;
begin
  if Multisample = TMultisample.FourSamples then
    MultisampleCount := 4
  else if Multisample = TMultisample.TwoSamples then
    MultisampleCount := 2
  else
    MultisampleCount := 1;

  SampleCount := 1;
  QualityLevel := 0;
  if (SharedDevice = nil) or (MultisampleCount < 2) or (Format = DXGI_FORMAT_UNKNOWN) then
    Exit;

  MaxSampleNo := Min(MultisampleCount, D3D11_MAX_MULTISAMPLE_SAMPLE_COUNT);

  SaveClearFPUState;
  try
    for I := MaxSampleNo downto 2 do
    begin
      if Failed(SharedDevice.CheckMultisampleQualityLevels(Format, I, QuaLevels)) then
        Continue;
      if QuaLevels > 0 then
      begin
        SampleCount := I;
        QualityLevel:= QuaLevels - 1;
        Break;
      end;
    end;
  finally
    RestoreFPUState;
  end;
end;

class procedure TDX11Context.FindBestShaderSource(const Shader: TContextShader; out Source: TContextShaderSource);
var
  MatchFound: Boolean;
begin
  MatchFound := False;

  if TCustomDX11Context.FeatureLevel >= D3D_FEATURE_LEVEL_11_0 then
  begin
    Source := Shader.GetSourceByArch(TContextShaderArch.DX11);
    MatchFound := Source.IsDefined;
  end;

  if not MatchFound and (TCustomDX11Context.FeatureLevel >= D3D_FEATURE_LEVEL_10_0) then
  begin
    Source := Shader.GetSourceByArch(TContextShaderArch.DX10);
    MatchFound := Source.IsDefined;
  end;

  if not MatchFound and (TCustomDX11Context.FeatureLevel >= D3D_FEATURE_LEVEL_9_1) then
  begin
    Source := Shader.GetSourceByArch(TContextShaderArch.DX11_level_9);
    MatchFound := Source.IsDefined;
  end;

  if not MatchFound then
    raise ECannotFindShader.CreateFmt(SCannotFindSuitableShader, [Shader.Name]);
end;

procedure TDX11Context.DoCreateBuffer;
var
  Tex: ID3D11Texture2D;
  BackBuffer: ID3D11Texture2D;
  SwapDesc: TDXGISwapChainDesc;
  SampleCount, QualityLevel: Integer;
  TexDesc, Desc: TD3D11_TEXTURE2D_DESC;
  Multisamples, ColorBits, DepthBits: Integer;
  Stencil: Boolean;
  RenderingSetupService: IFMXRenderingSetupService;
begin
  SaveClearFPUState;
  try
    if Texture <> nil then
    begin
      FindBestMultisampleType(TexturePixelFormatToDX(Texture.PixelFormat), Multisample, SampleCount, QualityLevel);
      if (Multisample <> TMultisample.None) and (SampleCount > 1) then
      begin
        FillChar(Desc, SizeOf(D3D11_TEXTURE2D_DESC), 0);
        Desc.Format := TexturePixelFormatToDX(Texture.PixelFormat);
        Desc.Width := Texture.Width;
        Desc.Height := Texture.Height;
        Desc.MipLevels := 1;
        Desc.ArraySize := 1;
        Desc.SampleDesc.Count  := SampleCount;
        Desc.SampleDesc.Quality := QualityLevel;
        Desc.Usage := D3D11_USAGE_DEFAULT;
        Desc.BindFlags := D3D11_BIND_RENDER_TARGET;
        SaveClearFPUState;
        try
          HR := SharedDevice.CreateTexture2D(Desc, nil, FRenderTargetMSTex);
        finally
          RestoreFPUState;
        end;
        if FRenderTargetMSTex <> nil then
        begin
          HR := SharedDevice.CreateRenderTargetView(FRenderTargetMSTex, nil, FRenderTargetView);
          if DepthStencil then
          begin
            FRenderTargetMSTex.GetDesc(TexDesc);
            FillChar(Desc, SizeOf(D3D11_TEXTURE2D_DESC), 0);
            Desc.Format := DXGI_FORMAT_D24_UNORM_S8_UINT;
            Desc.Width := TexDesc.Width;
            Desc.Height := TexDesc.Height;
            Desc.MipLevels := 1;
            Desc.ArraySize := 1;
            Desc.SampleDesc.Count  := SampleCount;
            Desc.SampleDesc.Quality:= QualityLevel;
            Desc.Usage := D3D11_USAGE_DEFAULT;
            Desc.BindFlags := D3D11_BIND_DEPTH_STENCIL;
            HR := SharedDevice.CreateTexture2D(Desc, nil, FDepthStencilTex);
            if Succeeded(HR) then
              HR := SharedDevice.CreateDepthStencilView(FDepthStencilTex, nil, FDepthStencilView);
          end;
        end;
      end
      else
      begin
        Tex := ResourceToTexture(Texture.Handle);
        if Tex <> nil then
        begin
          Tex.GetDesc(TexDesc);
          HR := SharedDevice.CreateRenderTargetView(Tex, nil, FRenderTargetView);
          if DepthStencil then
          begin
            FillChar(Desc, SizeOf(D3D11_TEXTURE2D_DESC), 0);
            Desc.Format := DXGI_FORMAT_D24_UNORM_S8_UINT;
            Desc.Width := TexDesc.Width;
            Desc.Height := TexDesc.Height;
            Desc.MipLevels := 1;
            Desc.ArraySize := 1;
            Desc.SampleDesc.Count := 1;
            Desc.SampleDesc.Quality := 0;
            Desc.Usage := D3D11_USAGE_DEFAULT;
            Desc.BindFlags := D3D11_BIND_DEPTH_STENCIL;
            HR := SharedDevice.CreateTexture2D(Desc, nil, FDepthStencilTex);
            if Succeeded(HR) then
              HR := SharedDevice.CreateDepthStencilView(FDepthStencilTex, nil, FDepthStencilView);
          end;
        end;
      end;
    end
    else
    begin
      Multisamples := Ord(Multisample) * 2;
      ColorBits := 0;
      DepthBits := 24;
      Stencil := DepthStencil;
      FBufferSize := TSize.Create(WindowHandleToPlatform(Parent).WndClientSize.Width,
        WindowHandleToPlatform(Parent).WndClientSize.Height);

      if TPlatformServices.Current.SupportsPlatformService(IFMXRenderingSetupService, RenderingSetupService) then
        RenderingSetupService.Invoke(ColorBits, DepthBits, Stencil, Multisamples);

      FillChar(SwapDesc, SizeOf(SwapDesc), 0);
      SwapDesc.BufferCount := 1;
      SwapDesc.BufferDesc.Width := FBufferSize.Width;
      SwapDesc.BufferDesc.Height := FBufferSize.Height;
      SwapDesc.BufferDesc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
      SwapDesc.BufferUsage := DXGI_USAGE_RENDER_TARGET_OUTPUT;
      SwapDesc.OutputWindow := WindowHandleToPlatform(Parent).Wnd;
      FindBestMultisampleType(SwapDesc.BufferDesc.Format, TMultisample(Multisamples div 2), SampleCount, QualityLevel);
      SwapDesc.SampleDesc.Count  := SampleCount;
      SwapDesc.SampleDesc.Quality:= QualityLevel;
      SwapDesc.Windowed := True;
      HR := DXGIFactory.CreateSwapChain(SharedDevice, SwapDesc, FSwapChain);
      if Succeeded(HR) then
      begin
        DXGIFactory.MakeWindowAssociation(WindowHandleToPlatform(Parent).Wnd, DXGI_MWA_NO_WINDOW_CHANGES);
        HR := FSwapChain.GetBuffer(0, ID3D11Texture2D, BackBuffer);
        if Succeeded(HR) then
          HR := SharedDevice.CreateRenderTargetView(BackBuffer, nil, FRenderTargetView);
        if (DepthBits > 0) or Stencil then
        begin
          FillChar(Desc, SizeOf(D3D11_TEXTURE2D_DESC), 0);
          Desc.Format := DXGI_FORMAT_D24_UNORM_S8_UINT;
          Desc.Width := SwapDesc.BufferDesc.Width;
          Desc.Height := SwapDesc.BufferDesc.Height;
          Desc.MipLevels := 1;
          Desc.ArraySize := 1;
          Desc.SampleDesc.Count := SwapDesc.SampleDesc.Count;
          Desc.SampleDesc.Quality := SwapDesc.SampleDesc.Quality;
          Desc.Usage := D3D11_USAGE_DEFAULT;
          Desc.BindFlags := D3D11_BIND_DEPTH_STENCIL;
          HR := SharedDevice.CreateTexture2D(Desc, nil, FDepthStencilTex);
          if Succeeded(HR) then
            HR := SharedDevice.CreateDepthStencilView(FDepthStencilTex, nil, FDepthStencilView);
        end;
      end;
    end;
  finally
    RestoreFPUState;
  end;
end;

procedure TDX11Context.DoResize;
begin
end;

procedure TDX11Context.DoFreeBuffer;
begin
  FRenderTargetMSTex := nil;
  FSwapChain := nil;
  FRenderTargetView := nil;
  FDepthStencilTex := nil;
  FDepthStencilView := nil;
end;

procedure TDX11Context.DoClear(const ATarget: TClearTargets; const AColor: TAlphaColor; const ADepth: single;
  const AStencil: Cardinal);
var
  Flags: TD3D11_CLEAR_FLAG;
begin
  SaveClearFPUState;
  try
    if DepthStencil then
    begin
      Flags := 0;
      if TClearTarget.Depth in ATarget then
        Flags := Flags or D3D11_CLEAR_DEPTH;
      if TClearTarget.Stencil in ATarget then
        Flags := Flags or D3D11_CLEAR_STENCIL;
      SharedContext.ClearDepthStencilView(FDepthStencilView, Flags, ADepth, AStencil);
    end;
    if (TClearTarget.Color in ATarget) and (FRenderTargetView <> nil) then
      SharedContext.ClearRenderTargetView(FRenderTargetView, ColorToD3DColor(AColor));
  finally
    RestoreFPUState;
  end;
end;

procedure TDX11Context.DoCopyToBitmap(const Dest: TBitmap; const ARect: TRect);
var
  CopyRect: TRect;
begin
  if (TCanvasStyle.NeedGPUSurface in Dest.CanvasClass.GetCanvasStyle) and (Texture <> nil) then
  begin
    if TCustomCanvasGpu(Dest.Canvas).BeginScene then
    try
      CopyRect := TRect.Intersect(ARect, TRect.Create(0, 0, Width, Height));

      TCustomCanvasGpu(Dest.Canvas).Clear(0);
      TCustomCanvasGpu(Dest.Canvas).SetMatrix(TMatrix.Identity);
      TCustomCanvasGpu(Dest.Canvas).DrawTexture(TRectF.Create(CopyRect.Left, CopyRect.Top, CopyRect.Right, CopyRect.Bottom),
        TRectF.Create(0, 0, CopyRect.Width, CopyRect.Height), $FFFFFFFF, Texture);
    finally
      TCustomCanvasGpu(Dest.Canvas).EndScene;
    end;
  end
  else
    inherited;
end;

procedure TDX11Context.DoCopyToBits(const Bits: Pointer; const Pitch: Integer; const ARect: TRect);
var
  Desc: TD3D11_TEXTURE2D_DESC;
  BackBuffer: ID3D11Texture2D;
  Mapped: TD3D11_MAPPED_SUBRESOURCE;
  I, W: UInt;
begin
  SaveClearFPUState;
  try
    if FCopyBuffer = nil then
    begin
      FillChar(Desc, SizeOf(D3D11_TEXTURE2D_DESC), 0);
      Desc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
      Desc.Width := Width;
      Desc.Height := Height;
      Desc.MipLevels := 1;
      Desc.ArraySize := 1;
      Desc.SampleDesc.Count := 1;
      Desc.CPUAccessFlags := D3D11_CPU_ACCESS_READ;
      Desc.Usage := D3D11_USAGE_STAGING;
      if Failed(SharedDevice.CreateTexture2D(Desc, nil, FCopyBuffer)) then
        Exit;
    end;

    if Texture = nil then
      HR := FSwapChain.GetBuffer(0, ID3D11Texture2D, BackBuffer)
    else
      BackBuffer := ResourceToTexture(Texture.Handle);

    SharedContext.CopySubresourceRegion(FCopyBuffer, 0, 0, 0, 0, BackBuffer, 0, nil);

    if Succeeded(SharedContext.Map(FCopyBuffer, 0, D3D11_MAP_READ, 0, Mapped)) then
    try
      if (ARect.Left = 0) and (ARect.Top = 0) and (ARect.Width = Width) and (ARect.Height = Height) and
         (Mapped.RowPitch = Cardinal(Pitch)) and (Pitch = Width * 4) then
        Move(Mapped.pData^, Bits^, Pitch * Height)
      else
      begin
        for I := ARect.Top to ARect.Bottom - 1 do
        begin
          W := ARect.Left;
          Move(PAlphaColorArray(Mapped.pData)[W + (I * (Mapped.RowPitch div 4))],
            PAlphaColorArray(Bits)[I * (UInt(Pitch) div 4) + UInt(ARect.Left)], ARect.Width * 4);
        end;
      end;
    finally
      SharedContext.Unmap(FCopyBuffer, 0);
    end;
  finally
    RestoreFPUState;
  end;
end;

function TDX11Context.DoBeginScene: Boolean;
var
  Viewport: TD3D11_Viewport;
begin
  SaveClearFPUState;
  try
    SharedContext.OMGetRenderTargets(1, FSavedRT, FSavedDepth);
    SharedContext.RSGetViewports(FSavedViewportNum, nil);
    if FSavedViewportNum > 0 then
      SharedContext.RSGetViewports(FSavedViewportNum, @FSavedViewport);

    SharedContext.OMSetRenderTargets(1, FRenderTargetView, FDepthStencilView);
    FillChar(Viewport, SizeOf(D3D11_VIEWPORT), 0);
    if Texture <> nil then
    begin
      Viewport.Width := Width;
      Viewport.Height := Height;
    end
    else
    begin
      Viewport.Width := Width * Scale;
      Viewport.Height := Height * Scale;
    end;
    Viewport.MinDepth := 0.0;
    Viewport.MaxDepth := 1.0;
    SharedContext.RSSetViewports(1, @Viewport);

    Result := inherited;
  finally
    RestoreFPUState;
  end;
end;

procedure TDX11Context.DoEndScene;
begin
  SaveClearFPUState;
  try
    if (FRenderTargetMSTex <> nil) and (Texture <> nil) then
      SharedContext.ResolveSubresource(ResourceToTexture(Texture.Handle), 0, FRenderTargetMSTex, 0, TexturePixelFormatToDX(Texture.PixelFormat));

    if (BeginSceneCount = 1) and (Texture = nil) then
      HR := FSwapChain.Present(0, 0);
    SharedContext.OMSetRenderTargets(1, FSavedRT, FSavedDepth);
    FSavedRT := nil;
    FSavedDepth := nil;

    if FSavedViewportNum > 0 then
      SharedContext.RSSetViewports(1, @FSavedViewport);
  finally
    RestoreFPUState;
  end;
  inherited;
end;

class function TDX11Context.DoBitmapToTexture(const Bitmap: TBitmap): TTexture;
begin
  if Bitmap.CanvasClass.InheritsFrom(TCustomCanvasGpu) then
    Result := TBitmapCtx(Bitmap.Handle).PaintingTexture
  else
    Result := inherited DoBitmapToTexture(Bitmap);
end;

procedure TDX11Context.DoSetContextState(AState: TContextState);
begin
  case AState of
    TContextState.csZTestOn:
      begin
        FDepthStencilDesc.DepthEnable := True;
        FDepthStencilModified := True;
      end;
    TContextState.csZTestOff:
      begin
        FDepthStencilDesc.DepthEnable := False;
        FDepthStencilModified := True;
      end;
    TContextState.csZWriteOn:
      begin
        FDepthStencilDesc.DepthWriteMask := D3D11_DEPTH_WRITE_MASK_ALL;
        FDepthStencilModified := True;
      end;
    TContextState.csZWriteOff:
      begin
        FDepthStencilDesc.DepthWriteMask := D3D11_DEPTH_WRITE_MASK_ZERO;
        FDepthStencilModified := True;
      end;
    TContextState.csAlphaBlendOn:
      begin
        FBlendDesc.RenderTarget[0].BlendEnable := True;
        FBlendStateModified := True;
      end;
    TContextState.csAlphaBlendOff:
      begin
        FBlendDesc.RenderTarget[0].BlendEnable := False;
        FBlendStateModified := True;
      end;
    TContextState.csStencilOn:
      begin
        FDepthStencilDesc.StencilEnable := True;
        FDepthStencilModified := True;
      end;
    TContextState.csStencilOff:
      begin
        FDepthStencilDesc.StencilEnable := False;
        FDepthStencilModified := True;
      end;
    TContextState.csColorWriteOn:
      begin
        FBlendDesc.RenderTarget[0].RenderTargetWriteMask := Byte(D3D11_COLOR_WRITE_ENABLE_ALL);
        FBlendStateModified := True;
      end;
    TContextState.csColorWriteOff:
      begin
        FBlendDesc.RenderTarget[0].RenderTargetWriteMask := 0;
        FBlendStateModified := True;
      end;
    TContextState.csScissorOn:
      begin
        FRasterizerDesc.ScissorEnable := True;
        FRasterizerStateModified := True;
      end;
    TContextState.csScissorOff:
      begin
        FRasterizerDesc.ScissorEnable := False;
        FRasterizerStateModified := True;
      end;
    TContextState.csFrontFace:
      begin
        FRasterizerDesc.CullMode := D3D11_CULL_BACK;
        FRasterizerStateModified := True;
      end;
    TContextState.csBackFace:
      begin
        FRasterizerDesc.CullMode := D3D11_CULL_FRONT;
        FRasterizerStateModified := True;
      end;
    TContextState.csAllFace:
      begin
        FRasterizerDesc.CullMode := D3D11_CULL_NONE;
        FRasterizerStateModified := True;
      end;
  end;
end;

procedure TDX11Context.DoSetStencilOp(const Fail, ZFail, ZPass: TStencilOp);
begin
  case Fail of
    TStencilOp.Keep: FDepthStencilDesc.FrontFace.StencilFailOp := D3D11_STENCIL_OP_KEEP;
    TStencilOp.Zero: FDepthStencilDesc.FrontFace.StencilFailOp := D3D11_STENCIL_OP_ZERO;
    TStencilOp.Replace: FDepthStencilDesc.FrontFace.StencilFailOp := D3D11_STENCIL_OP_REPLACE;
    TStencilOp.Increase: FDepthStencilDesc.FrontFace.StencilFailOp := D3D11_STENCIL_OP_INCR_SAT;
    TStencilOp.Decrease: FDepthStencilDesc.FrontFace.StencilFailOp := D3D11_STENCIL_OP_DECR_SAT;
    TStencilOp.Invert: FDepthStencilDesc.FrontFace.StencilFailOp := D3D11_STENCIL_OP_INVERT;
  end;
  case ZFail of
    TStencilOp.Keep: FDepthStencilDesc.FrontFace.StencilDepthFailOp := D3D11_STENCIL_OP_KEEP;
    TStencilOp.Zero: FDepthStencilDesc.FrontFace.StencilDepthFailOp := D3D11_STENCIL_OP_ZERO;
    TStencilOp.Replace: FDepthStencilDesc.FrontFace.StencilDepthFailOp := D3D11_STENCIL_OP_REPLACE;
    TStencilOp.Increase: FDepthStencilDesc.FrontFace.StencilDepthFailOp := D3D11_STENCIL_OP_INCR_SAT;
    TStencilOp.Decrease: FDepthStencilDesc.FrontFace.StencilDepthFailOp := D3D11_STENCIL_OP_DECR_SAT;
    TStencilOp.Invert: FDepthStencilDesc.FrontFace.StencilDepthFailOp := D3D11_STENCIL_OP_INVERT;
  end;
  case ZPass of
    TStencilOp.Keep: FDepthStencilDesc.FrontFace.StencilPassOp := D3D11_STENCIL_OP_KEEP;
    TStencilOp.Zero: FDepthStencilDesc.FrontFace.StencilPassOp := D3D11_STENCIL_OP_ZERO;
    TStencilOp.Replace: FDepthStencilDesc.FrontFace.StencilPassOp := D3D11_STENCIL_OP_REPLACE;
    TStencilOp.Increase: FDepthStencilDesc.FrontFace.StencilPassOp := D3D11_STENCIL_OP_INCR_SAT;
    TStencilOp.Decrease: FDepthStencilDesc.FrontFace.StencilPassOp := D3D11_STENCIL_OP_DECR_SAT;
    TStencilOp.Invert: FDepthStencilDesc.FrontFace.StencilPassOp := D3D11_STENCIL_OP_INVERT;
  end;
  FDepthStencilDesc.BackFace := FDepthStencilDesc.FrontFace;
  FDepthStencilModified := True;
end;

procedure TDX11Context.DoSetStencilFunc(const Func: TStencilfunc; Ref, Mask: Cardinal);
begin
  case Func of
    TStencilFunc.Never: FDepthStencilDesc.FrontFace.StencilFunc := D3D11_COMPARISON_NEVER;
    TStencilFunc.Less: FDepthStencilDesc.FrontFace.StencilFunc := D3D11_COMPARISON_LESS;
    TStencilFunc.Lequal: FDepthStencilDesc.FrontFace.StencilFunc := D3D11_COMPARISON_LESS_EQUAL;
    TStencilFunc.Greater: FDepthStencilDesc.FrontFace.StencilFunc := D3D11_COMPARISON_GREATER;
    TStencilFunc.Gequal: FDepthStencilDesc.FrontFace.StencilFunc := D3D11_COMPARISON_GREATER_EQUAL;
    TStencilFunc.Equal: FDepthStencilDesc.FrontFace.StencilFunc := D3D11_COMPARISON_EQUAL;
    TStencilFunc.NotEqual: FDepthStencilDesc.FrontFace.StencilFunc := D3D11_COMPARISON_NOT_EQUAL;
    TStencilFunc.Always: FDepthStencilDesc.FrontFace.StencilFunc := D3D11_COMPARISON_ALWAYS;
  end;
  FDepthStencilDesc.StencilReadMask := Mask;
  FDepthStencilDesc.StencilWriteMask := Mask;
  FDepthStencilDesc.BackFace := FDepthStencilDesc.FrontFace;
  FStencilRef := Ref;
  FDepthStencilModified := True;
end;

procedure TDX11Context.DoDrawPrimitivesBatch(const AKind: TPrimitivesKind; const Vertices, Indices: Pointer;
  const VertexDeclaration: TVertexDeclaration; const VertexSize, VertexCount, IndexSize, IndexCount: Integer);
var
  PhysIndexSize, I: Integer;
  VtxStride, VtxOffset: LongWord;
  InputElements: array of TD3D11_INPUT_ELEMENT_DESC;
  Source: TContextShaderSource;
  Flags: TD3D11_Map;
  OldInputLayout: ID3D11InputLayout;
  OldDepthStencilState: ID3D11DepthStencilState;
  OldBlendState: ID3D11BlendState;
  OldRasterizerState: ID3D11RasterizerState;
  OldVSSlot: ID3D11Buffer;
  OldPSSlot: ID3D11Buffer;
  Desc: TD3D11_BUFFER_DESC;
  NeedCreatePS, NeedCreateVS: Boolean;
  Element: TVertexElement;
  Mapped: TD3D11_MAPPED_SUBRESOURCE;
begin
  if CurrentVertexShader <> nil then
  begin
    SaveClearFPUState;
    try
      PhysIndexSize := IndexSize;
      if (IndexSize = SizeOf(LongInt)) and (IndexBufferSupport <> TIndexBufferSupport.Int32) then
        PhysIndexSize := SizeOf(Word);

      if VertexSize * VertexCount > VBSize then
      begin
        FVB := nil;
        VBSize := VertexSize * VertexCount;
        HR := FSharedDevice.CreateBuffer(TD3D11_BUFFER_DESC.Create(VBSize, D3D11_BIND_VERTEX_BUFFER, D3D11_USAGE_DYNAMIC,
          D3D11_CPU_ACCESS_WRITE), nil, FVB);
      end;

      if FVBLockPos + VertexSize * VertexCount > VBSize then
      begin
        FVBLockPos := 0;
        Flags := D3D11_MAP_WRITE_DISCARD;
      end
      else
        Flags := D3D11_MAP_WRITE_NO_OVERWRITE;

      if Succeeded(SharedContext.Map(FVB, 0, Flags, 0, Mapped)) then
      try
        Move(Vertices^, PByteArray(Mapped.pData)[FVBLockPos], VertexSize * VertexCount);
      finally
        SharedContext.Unmap(FVB, 0);
      end;

      if IndexCount * PhysIndexSize > IBSize then
      begin
        FIB := nil;
        IBSize := IndexCount * PhysIndexSize;
        HR := FSharedDevice.CreateBuffer(TD3D11_BUFFER_DESC.Create(IBSize, D3D11_BIND_INDEX_BUFFER, D3D11_USAGE_DYNAMIC,
          D3D11_CPU_ACCESS_WRITE), nil, FIB);
      end;

      if FIBLockPos + IndexCount * PhysIndexSize > IBSize then
      begin
        FIBLockPos := 0;
        Flags := D3D11_MAP_WRITE_DISCARD;
      end
      else
        Flags := D3D11_MAP_WRITE_NO_OVERWRITE;

      if Succeeded(SharedContext.Map(FIB, 0, Flags, 0, Mapped)) then
      try
        if PhysIndexSize < IndexSize then
          for I := 0 to IndexCount - 1 do
            PWord(NativeInt(Mapped.pData) + FIBLockPos + I * SizeOf(Word))^ := PLongInt(NativeInt(Indices) + I *
              SizeOf(LongInt))^
        else
          Move(Indices^, PLongInt(NativeInt(Mapped.pData) + FIBLockPos)^, IndexCount * PhysIndexSize);
      finally
        SharedContext.Unmap(FIB, 0);
      end;

      SetLength(InputElements, 0);
      for Element in VertexDeclaration do
      begin
        case Element.Format of
          TVertexFormat.Vertex:
            begin
              SetLength(InputElements, Length(InputElements) + 1);
              InputElements[High(InputElements)].SemanticName := 'POSITION';
              InputElements[High(InputElements)].SemanticIndex := 0;
              InputElements[High(InputElements)].Format := DXGI_FORMAT_R32G32B32_FLOAT;
              InputElements[High(InputElements)].InputSlot := 0;
              InputElements[High(InputElements)].AlignedByteOffset := Element.Offset;
              InputElements[High(InputElements)].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
              InputElements[High(InputElements)].InstanceDataStepRate := 0;
            end;
          TVertexFormat.Normal:
            begin
              SetLength(InputElements, Length(InputElements) + 1);
              InputElements[High(InputElements)].SemanticName := 'NORMAL';
              InputElements[High(InputElements)].SemanticIndex := 0;
              InputElements[High(InputElements)].Format := DXGI_FORMAT_R32G32B32_FLOAT;
              InputElements[High(InputElements)].InputSlot := 0;
              InputElements[High(InputElements)].AlignedByteOffset := Element.Offset;
              InputElements[High(InputElements)].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
              InputElements[High(InputElements)].InstanceDataStepRate := 0;
            end;
          TVertexFormat.Color0:
            begin
              SetLength(InputElements, Length(InputElements) + 1);
              InputElements[High(InputElements)].SemanticName := 'COLOR';
              InputElements[High(InputElements)].SemanticIndex := 0;
              InputElements[High(InputElements)].Format := DXGI_FORMAT_R8G8B8A8_UNORM; // 9_1 doesn't support BGRA
              InputElements[High(InputElements)].InputSlot := 0;
              InputElements[High(InputElements)].AlignedByteOffset := Element.Offset;
              InputElements[High(InputElements)].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
              InputElements[High(InputElements)].InstanceDataStepRate := 0;
            end;
          TVertexFormat.Color1:
            begin
              SetLength(InputElements, Length(InputElements) + 1);
              InputElements[High(InputElements)].SemanticName := 'COLOR';
              InputElements[High(InputElements)].SemanticIndex := 1;
              InputElements[High(InputElements)].Format := DXGI_FORMAT_R8G8B8A8_UNORM;
              InputElements[High(InputElements)].InputSlot := 0;
              InputElements[High(InputElements)].AlignedByteOffset := Element.Offset;
              InputElements[High(InputElements)].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
              InputElements[High(InputElements)].InstanceDataStepRate := 0;
            end;
          TVertexFormat.Color2:
            begin
              SetLength(InputElements, Length(InputElements) + 1);
              InputElements[High(InputElements)].SemanticName := 'COLOR';
              InputElements[High(InputElements)].SemanticIndex := 2;
              InputElements[High(InputElements)].Format := DXGI_FORMAT_R8G8B8A8_UNORM;
              InputElements[High(InputElements)].InputSlot := 0;
              InputElements[High(InputElements)].AlignedByteOffset := Element.Offset;
              InputElements[High(InputElements)].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
              InputElements[High(InputElements)].InstanceDataStepRate := 0;
            end;
          TVertexFormat.Color3:
            begin
              SetLength(InputElements, Length(InputElements) + 1);
              InputElements[High(InputElements)].SemanticName := 'COLOR';
              InputElements[High(InputElements)].SemanticIndex := 3;
              InputElements[High(InputElements)].Format := DXGI_FORMAT_R8G8B8A8_UNORM;
              InputElements[High(InputElements)].InputSlot := 0;
              InputElements[High(InputElements)].AlignedByteOffset := Element.Offset;
              InputElements[High(InputElements)].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
              InputElements[High(InputElements)].InstanceDataStepRate := 0;
            end;
          TVertexFormat.TexCoord0:
            begin
              SetLength(InputElements, Length(InputElements) + 1);
              InputElements[High(InputElements)].SemanticName := 'TEXCOORD';
              InputElements[High(InputElements)].SemanticIndex := 0;
              InputElements[High(InputElements)].Format := DXGI_FORMAT_R32G32_FLOAT;
              InputElements[High(InputElements)].InputSlot := 0;
              InputElements[High(InputElements)].AlignedByteOffset := Element.Offset;
              InputElements[High(InputElements)].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
              InputElements[High(InputElements)].InstanceDataStepRate := 0;
            end;
          TVertexFormat.TexCoord1:
            begin
              SetLength(InputElements, Length(InputElements) + 1);
              InputElements[High(InputElements)].SemanticName := 'TEXCOORD';
              InputElements[High(InputElements)].SemanticIndex := 1;
              InputElements[High(InputElements)].Format := DXGI_FORMAT_R32G32_FLOAT;
              InputElements[High(InputElements)].InputSlot := 0;
              InputElements[High(InputElements)].AlignedByteOffset := Element.Offset;
              InputElements[High(InputElements)].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
              InputElements[High(InputElements)].InstanceDataStepRate := 0;
            end;
          TVertexFormat.TexCoord2:
            begin
              SetLength(InputElements, Length(InputElements) + 1);
              InputElements[High(InputElements)].SemanticName := 'TEXCOORD';
              InputElements[High(InputElements)].SemanticIndex := 2;
              InputElements[High(InputElements)].Format := DXGI_FORMAT_R32G32_FLOAT;
              InputElements[High(InputElements)].InputSlot := 0;
              InputElements[High(InputElements)].AlignedByteOffset := Element.Offset;
              InputElements[High(InputElements)].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
              InputElements[High(InputElements)].InstanceDataStepRate := 0;
            end;
          TVertexFormat.TexCoord3:
            begin
              SetLength(InputElements, Length(InputElements) + 1);
              InputElements[High(InputElements)].SemanticName := 'TEXCOORD';
              InputElements[High(InputElements)].SemanticIndex := 3;
              InputElements[High(InputElements)].Format := DXGI_FORMAT_R32G32_FLOAT;
              InputElements[High(InputElements)].InputSlot := 0;
              InputElements[High(InputElements)].AlignedByteOffset := Element.Offset;
              InputElements[High(InputElements)].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
              InputElements[High(InputElements)].InstanceDataStepRate := 0;
            end;
          TVertexFormat.BiNormal:
            begin
              SetLength(InputElements, Length(InputElements) + 1);
              InputElements[High(InputElements)].SemanticName := 'BINORMAL';
              InputElements[High(InputElements)].SemanticIndex := 0;
              InputElements[High(InputElements)].Format := DXGI_FORMAT_R32G32B32_FLOAT;
              InputElements[High(InputElements)].InputSlot := 0;
              InputElements[High(InputElements)].AlignedByteOffset := Element.Offset;
              InputElements[High(InputElements)].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
              InputElements[High(InputElements)].InstanceDataStepRate := 0;
            end;
          TVertexFormat.Tangent:
            begin
              SetLength(InputElements, Length(InputElements) + 1);
              InputElements[High(InputElements)].SemanticName := 'TANGENT';
              InputElements[High(InputElements)].SemanticIndex := 0;
              InputElements[High(InputElements)].Format := DXGI_FORMAT_R32G32B32_FLOAT;
              InputElements[High(InputElements)].InputSlot := 0;
              InputElements[High(InputElements)].AlignedByteOffset := Element.Offset;
              InputElements[High(InputElements)].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
              InputElements[High(InputElements)].InstanceDataStepRate := 0;
            end;
          TVertexFormat.ColorF0:
            begin
              SetLength(InputElements, Length(InputElements) + 1);
              InputElements[High(InputElements)].SemanticName := 'COLOR';
              InputElements[High(InputElements)].SemanticIndex := 0;
              InputElements[High(InputElements)].Format := DXGI_FORMAT_R32G32B32A32_FLOAT;
              InputElements[High(InputElements)].InputSlot := 0;
              InputElements[High(InputElements)].AlignedByteOffset := Element.Offset;
              InputElements[High(InputElements)].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
              InputElements[High(InputElements)].InstanceDataStepRate := 0;
            end;
        end;
      end;

      if FVSSlotModified then
      begin
        NeedCreateVS := Length(FVSBuf) > 0;
        if FVSSlot <> nil then
        begin
          FVSSlot.GetDesc(Desc);
          if Desc.ByteWidth = UInt(Length(FVSBuf)) then
            NeedCreateVS := False;
        end;
        if NeedCreateVS then
        begin
          OldVSSlot := FVSSlot;
          FVSSlot := nil;
          HR := FSharedDevice.CreateBuffer(TD3D11_BUFFER_DESC.Create(Length(FVSBuf), D3D11_BIND_CONSTANT_BUFFER,
            D3D11_USAGE_DYNAMIC, D3D11_CPU_ACCESS_WRITE), nil, FVSSlot);
        end;
        if FVSSlot <> nil then
        begin
          if Succeeded(SharedContext.Map(FVSSlot, 0, D3D11_MAP_WRITE_DISCARD, 0, Mapped)) then
          try
            Move(FVSBuf[0], Mapped.pData^, Length(FVSBuf));
          finally
            SharedContext.Unmap(FVSSlot, 0);
          end;
          SharedContext.VSSetConstantBuffers(0, 1, FVSSlot);
        end;
        OldVSSlot := nil;
        FVSSlotModified := False;
      end;

      if FPSSlotModified then
      begin
        NeedCreatePS := Length(FPSBuf) > 0;
        if FPSSlot <> nil then
        begin
          FPSSlot.GetDesc(Desc);
          if Desc.ByteWidth = UInt(Length(FPSBuf)) then
            NeedCreatePS := False;
        end;
        if NeedCreatePS then
        begin
          OldPSSlot := FPSSlot;
          FPSSlot := nil;
          HR := FSharedDevice.CreateBuffer(TD3D11_BUFFER_DESC.Create(Length(FPSBuf), D3D11_BIND_CONSTANT_BUFFER,
            D3D11_USAGE_DYNAMIC, D3D11_CPU_ACCESS_WRITE), nil, FPSSlot);
        end;
        if FPSSlot <> nil then
        begin
          if Succeeded(SharedContext.Map(FPSSlot, 0, D3D11_MAP_WRITE_DISCARD, 0, Mapped)) then
          try
            Move(FPSBuf[0], Mapped.pData^, Length(FPSBuf));
          finally
            SharedContext.Unmap(FPSSlot, 0);
          end;
          SharedContext.PSSetConstantBuffers(0, 1, FPSSlot);
        end;

        OldPSSlot := nil;
        FPSSlotModified := False;
      end;

      if FBlendStateModified then
      begin
        OldBlendState := FBlendState;
        FBlendState := nil;
        SharedDevice.CreateBlendState(FBlendDesc, FBlendState);
        SharedContext.OMSetBlendState(FBlendState, ColorToD3DColor($FFFFFFFF), $FFFFFFFF);
        OldBlendState := nil;
        FBlendStateModified := False;
      end
      else
        SharedContext.OMSetBlendState(FBlendState, ColorToD3DColor($FFFFFFFF), $FFFFFFFF);

      if FDepthStencilModified then
      begin
        OldDepthStencilState := FDepthStencilState;
        FDepthStencilState := nil;
        SharedDevice.CreateDepthStencilState(FDepthStencilDesc, FDepthStencilState);
        SharedContext.OMSetDepthStencilState(FDepthStencilState, FStencilRef);
        OldDepthStencilState := nil;
        FDepthStencilModified := False;
      end
      else
        SharedContext.OMSetDepthStencilState(FDepthStencilState, FStencilRef);

      if FRasterizerStateModified then
      begin
        OldRasterizerState := FRasterizerState;
        FRasterizerState := nil;
        SharedDevice.CreateRasterizerState(FRasterizerDesc, FRasterizerState);
        SharedContext.RSSetState(FRasterizerState);
        OldRasterizerState := nil;
        FRasterizerStateModified := False;
      end
      else
        SharedContext.RSSetState(FRasterizerState);

      FindBestShaderSource(CurrentVertexShader, Source);
      OldInputLayout := FInputLayout;
      FInputLayout := nil;
      HR := SharedDevice.CreateInputLayout(@InputElements[0], Length(InputElements), @Source.Code[0], Length(Source.Code),
        FInputLayout);
      if Succeeded(HR) then
      begin
        VtxStride := VertexSize;
        VtxOffset := FVBLockPos;
        SharedContext.IASetVertexBuffers(0, 1, FVB, @VtxStride, @VtxOffset);
        SharedContext.IASetInputLayout(FInputLayout);
        if PhysIndexSize = SizeOf(LongInt) then
          SharedContext.IASetIndexBuffer(FIB, DXGI_FORMAT_R32_UINT, FIBLockPos)
        else
          SharedContext.IASetIndexBuffer(FIB, DXGI_FORMAT_R16_UINT, FIBLockPos);
        case AKind of
          TPrimitivesKind.Points:
            SharedContext.IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_POINTLIST);
          TPrimitivesKind.Lines:
            SharedContext.IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_LINELIST);
        else
          SharedContext.IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
        end;
        SharedContext.DrawIndexed(IndexCount, 0, 0);
        OldInputLayout := nil;
      end;
      FVBLockPos := FVBLockPos + VertexSize * VertexCount;
      FIBLockPos := FIBLockPos + IndexCount * PhysIndexSize;
    finally
      RestoreFPUState;
    end;
  end;
end;

{ Resources }

class function TDX11Context.AddResource(const Resource: IInterface): THandle;
begin
  if FResources = nil then
  begin
    FResources := TInterfaceList.Create;
    // Fill in the first slot with a dummy entry. This will make it so that a TContextShader value of 0 is invalid.
    FResources.Add(TInterfacedObject.Create);
  end;
  Result := 0;
  while (Result < UInt(FResources.Count)) and (FResources[Result] <> nil) do
    Inc(Result);
  if Result < UInt(FResources.Count) then
    FResources[Result] := Resource
  else
    Result := FResources.Add(Resource);
end;

class procedure TDX11Context.RemoveResource(Resource: THandle);
begin
  if (FResources <> nil) and (Resource <> 0) then
    FResources[Resource] := nil;
end;

class function TDX11Context.ResourceToTexture(Resource: THandle): ID3D11Texture2D;
begin
  if (FResources <> nil) and (Resource > 0) and (Resource < UInt(FResources.Count)) then
    Result := FResources[Resource] as ID3D11Texture2D
  else
    Result := nil;
end;

class function TDX11Context.ResourceToPixelShader(Resource: THandle): ID3D11PixelShader;
begin
  if (FResources <> nil) and (Resource > 0) and (Resource < UInt(FResources.Count)) then
    Result := FResources[Resource] as ID3D11PixelShader
  else
    Result := nil;
end;

class function TDX11Context.ResourceToVertexShader(Resource: THandle): ID3D11VertexShader;
begin
  if (FResources <> nil) and (Resource > 0) and (Resource < UInt(FResources.Count)) then
    Result := FResources[Resource] as ID3D11VertexShader
  else
    Result := nil;
end;

{ Textures }

class procedure TDX11Context.DoInitializeTexture(const Texture: TTexture);
var
  Tex: ID3D11Texture2D;
  Desc: TD3D11_TEXTURE2D_DESC;
begin
  CreateSharedDevice;

  FillChar(Desc, SizeOf(D3D11_TEXTURE2D_DESC), 0);
  if Texture.PixelFormat = TPixelFormat.None then
    Texture.PixelFormat := TPixelFormat.BGRA;
  Desc.Format := TexturePixelFormatToDX(Texture.PixelFormat);
  Desc.Width := Texture.Width;
  Desc.Height := Texture.Height;
  if TTextureStyle.MipMaps in Texture.Style then
  begin
    if TTextureStyle.Dynamic in Texture.Style then
      Desc.MipLevels := 1
    else
      Desc.MipLevels := 0;
  end
  else
    Desc.MipLevels := 1;
  Desc.ArraySize := 1;
  Desc.SampleDesc.Count := 1;
  Desc.SampleDesc.Quality := 0;
  if (TTextureStyle.Dynamic in Texture.Style) and not (TTextureStyle.RenderTarget in Texture.Style) then
  begin
    Desc.CPUAccessFlags := D3D11_CPU_ACCESS_WRITE;
    Desc.Usage := D3D11_USAGE_DYNAMIC;
  end
  else
    Desc.Usage := D3D11_USAGE_DEFAULT;
  Desc.BindFlags := D3D11_BIND_SHADER_RESOURCE;
  if TTextureStyle.RenderTarget in Texture.Style then
    Desc.BindFlags := Desc.BindFlags or D3D11_BIND_RENDER_TARGET;
  SaveClearFPUState;
  try
    HR := SharedDevice.CreateTexture2D(Desc, nil, Tex);
  finally
    RestoreFPUState;
  end;
  if Tex <> nil then
    ITextureAccess(Texture).Handle := AddResource(Tex);
end;

class procedure TDX11Context.DoFinalizeTexture(const Texture: TTexture);
begin
  SaveClearFPUState;
  try
    RemoveResource(Texture.Handle);
    ITextureAccess(Texture).Handle := 0;
  finally
    RestoreFPUState;
  end;
end;

procedure TDX11Context.SetTexture(const AUnit: Integer; const Texture: TTexture);
var
  Tex: ID3D11Texture2D;
  Desc: TD3D11_SAMPLER_DESC;
  OldResourceView: ID3D11ShaderResourceView;
  OldSampleState: ID3D11SamplerState;
begin
  SaveClearFPUState;
  try
    if FSampleStates[AUnit] = nil then
    begin
      FillChar(Desc, SizeOf(Desc), 0);
      Desc.AddressU := D3D11_TEXTURE_ADDRESS_CLAMP;
      Desc.AddressV := D3D11_TEXTURE_ADDRESS_CLAMP;
      Desc.AddressW := D3D11_TEXTURE_ADDRESS_CLAMP;
      Desc.Filter := D3D11_FILTER_MIN_MAG_MIP_LINEAR;
      Desc.MipLODBias := 0;
      Desc.MaxAnisotropy := 1;
      Desc.ComparisonFunc := D3D11_COMPARISON_NEVER;
      Desc.MinLOD := -MaxSingle;
      Desc.MaxLOD := MaxSingle;
      OldSampleState := FSampleStates[AUnit];
      FSampleStates[AUnit] := nil;
      if Succeeded(SharedDevice.CreateSamplerState(Desc, FSampleStates[AUnit])) then
      begin
        SharedContext.PSSetSamplers(AUnit, 1, FSampleStates[AUnit]);
        OldSampleState := nil;
      end;
    end;

    if (Texture <> nil) and (Texture.Handle <> 0) then
      Tex := ResourceToTexture(Texture.Handle)
    else
      Tex := BlankTexture;
    OldResourceView := FResourceViews[AUnit];
    FResourceViews[AUnit] := nil;
    if Succeeded(SharedDevice.CreateShaderResourceView(Tex, nil, FResourceViews[AUnit])) then
    begin
      SharedContext.PSSetShaderResources(AUnit, 1, FResourceViews[AUnit]);
      OldResourceView := nil;
    end;
  finally
    RestoreFPUState;
  end;
end;

class procedure TDX11Context.DoUpdateTexture(const Texture: TTexture; const Bits: Pointer; const Pitch: Integer);
var
  Mapped: D3D11_MAPPED_SUBRESOURCE;
  I, BytesToCopy: UInt;
  CopyBuffer, Tex: ID3D11Texture2D;
  Desc: TD3D11_TEXTURE2D_DESC;
begin
  if (Texture <> nil) and (Texture.Handle <> 0) then
  begin
    SaveClearFPUState;
    try
      Tex := ResourceToTexture(Texture.Handle);
      if TTextureStyle.RenderTarget in Texture.Style then
      begin
        FillChar(Desc, SizeOf(D3D11_TEXTURE2D_DESC), 0);
        Desc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
        Desc.Width := Texture.Width;
        Desc.Height := Texture.Height;
        Desc.MipLevels := 1;
        Desc.ArraySize := 1;
        Desc.SampleDesc.Count := 1;
        Desc.SampleDesc.Quality := 0;
        Desc.CPUAccessFlags := D3D11_CPU_ACCESS_WRITE;
        Desc.Usage := D3D11_USAGE_STAGING;
        Desc.BindFlags := 0;
        HR := SharedDevice.CreateTexture2D(Desc, nil, CopyBuffer);

        if Succeeded(SharedContext.Map(CopyBuffer, 0, D3D11_MAP_WRITE, 0, Mapped)) then
        try
          if UInt(Pitch) = Mapped.RowPitch then
            Move(Bits^, Mapped.pData^, Texture.Height * Pitch)
          else
          begin
            BytesToCopy := Min(Pitch, Mapped.RowPitch);
            for I := 0 to Texture.Height - 1 do
              Move(PByteArray(Bits)[UInt(Pitch) * I], PByteArray(Mapped.pData)[Mapped.RowPitch * I], BytesToCopy)
          end;
        finally
          SharedContext.Unmap(CopyBuffer, 0);
        end;

        SharedContext.CopySubresourceRegion(Tex, 0, 0, 0, 0, CopyBuffer, 0, nil);
      end
      else
      begin
        if Succeeded(SharedContext.Map(Tex, 0, D3D11_MAP_WRITE_DISCARD, 0, Mapped)) then
        try
          if UInt(Pitch) = Mapped.RowPitch then
            Move(Bits^, Mapped.pData^, Texture.Height * Pitch)
          else
          begin
            BytesToCopy := Min(Pitch, Mapped.RowPitch);
            for I := 0 to Texture.Height - 1 do
              Move(PByteArray(Bits)[UInt(Pitch) * I], PByteArray(Mapped.pData)[Mapped.RowPitch * I], BytesToCopy)
          end;
        finally
          SharedContext.Unmap(Tex, 0);
        end;
      end;
    finally
      RestoreFPUState;
    end;
  end;
end;

{ Shader }

class procedure TDX11Context.DoInitializeShader(const Shader: TContextShader);
var
  VSShader: ID3D11VertexShader;
  PSShader: ID3D11PixelShader;
  Source: TContextShaderSource;
begin
  CreateSharedDevice;

  FindBestShaderSource(Shader, Source);
  if Source.IsDefined then
  begin
    SaveClearFPUState;
    try
      if Shader.Kind = TContextShaderKind.VertexShader then
      begin
        HR := SharedDevice.CreateVertexShader(Source.Code, Length(Source.Code), nil, @VSShader);
        if VSShader <> nil then
          Shader.Handle := AddResource(VSShader);
      end
      else
      begin
        HR := SharedDevice.CreatePixelShader(Source.Code, Length(Source.Code), nil, PSShader);
        if PSShader <> nil then
          Shader.Handle := AddResource(PSShader);
      end;
    finally
      RestoreFPUState;
    end;
  end;
end;

class procedure TDX11Context.DoFinalizeShader(const Shader: TContextShader);
begin
  SaveClearFPUState;
  try
    RemoveResource(Shader.Handle);
  finally
    RestoreFPUState;
  end;
  Shader.Handle := 0;
end;

procedure TDX11Context.DoSetScissorRect(const ScissorRect: TRect);
begin
  SaveClearFPUState;
  try
    FSharedContext.RSSetScissorRects(1, @ScissorRect);
  finally
    RestoreFPUState;
  end;
end;

procedure TDX11Context.DoSetShaders(const VertexShader, PixelShader: TContextShader);
var
  Source: TContextShaderSource;
begin
  SaveClearFPUState;
  try
    SharedContext.VSSetShader(ResourceToVertexShader(VertexShader.Handle), nil, 0);
    SharedContext.PSSetShader(ResourceToPixelShader(PixelShader.Handle), nil, 0);
  finally
    RestoreFPUState;
  end;
  if VertexShader <> nil then
  begin
    FindBestShaderSource(VertexShader, Source);
    SetLength(FVSBuf, GetSlotSize(Source));
    FVSSlotModified := True;
  end;
  if PixelShader <> nil then
  begin
    FindBestShaderSource(PixelShader, Source);
    SetLength(FPSBuf, GetSlotSize(Source));
    FPSSlotModified := True;
  end;
end;

procedure TDX11Context.DoSetShaderVariable(const Name: string; const Data: array of TVector3D);
var
  I: Integer;
  Source: TContextShaderSource;
begin
  if (CurrentVertexShader <> nil) and (Length(FVSBuf) > 0) then
  begin
    FindBestShaderSource(CurrentVertexShader, Source);
    for I := 0 to High(Source.Variables) do
      if SameText(Source.Variables[I].Name, Name) then
      begin
        Move(Data[0], FVSBuf[Source.Variables[I].Index], Min(SizeOf(Data), Source.Variables[I].Size));
        FVSSlotModified := True;
        Exit;
      end;
  end;
  if (CurrentPixelShader <> nil) and (Length(FPSBuf) > 0) then
  begin
    FindBestShaderSource(CurrentPixelShader, Source);
    for I := 0 to High(Source.Variables) do
      if SameText(Source.Variables[I].Name, Name) then
      begin
        Move(Data[0], FPSBuf[Source.Variables[I].Index], Min(SizeOf(Data), Source.Variables[I].Size));
        FPSSlotModified := True;
        Exit;
      end;
  end;
end;

procedure TDX11Context.DoSetShaderVariable(const Name: string; const Texture: TTexture);
var
  I: Integer;
  Source: TContextShaderSource;
begin
  if CurrentPixelShader <> nil then
  begin
    FindBestShaderSource(CurrentPixelShader, Source);
    for I := 0 to High(Source.Variables) do
      if SameText(Source.Variables[I].Name, Name) then
      begin
        SetTexture(Source.Variables[I].Index, Texture);
        Exit;
      end;
  end;
end;

procedure TDX11Context.DoSetShaderVariable(const Name: string; const Matrix: TMatrix3D);
begin
  SetShaderVariable(Name, Matrix.M);
end;

procedure RegisterContextClasses;
var
  DriverType: D3D_DRIVER_TYPE;
  FeatureLevel: TD3D_FEATURE_LEVEL;
begin
  TCustomDX11Context.TestDriverSupport(DriverType, FeatureLevel);
  if DriverType <> D3D_DRIVER_TYPE_NULL then
    TContextManager.RegisterContext(TDX11Context, True);
end;

procedure UnregisterContextClasses;
begin
  TDX11Context.DestroySharedDevice;
end;

end.
