{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2018 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Canvas.D2D;

{.$DEFINE DXDEBUG}
{.$DEFINE D2DEBUG}

interface

{$SCOPEDENUMS ON}

uses
  Winapi.Windows, Winapi.DXGI, Winapi.D3D10, Winapi.D2D1, Winapi.Wincodec, FMX.Graphics;

type
  TCustomBitmapCodecWIC = class(TCustomBitmapCodec)
  private class var
    FImagingFactory: IWICImagingFactory;
    class destructor DestroySharedResources;
  private
    class function GetImagingFactory: IWICImagingFactory; static;
  public
    class property ImagingFactory: IWICImagingFactory read GetImagingFactory;
  end;

  TCustomCanvasD2D = class(TCanvas)
  public type
    TDirect3DLevel = (Undetermined, Unsupported, Direct3D10, Direct3D10_1);
    TDirect3DSupport = (Undetermined, Direct3D10Legacy, Direct3D10, Direct3D10_1);
  private class var
    FSharedDevice: ID3D10Device;
    FSharedDXGIFactory: IDXGIFactory;
    FSharedFactory: ID2D1Factory;
    FSharedDWriteFactory: IDWriteFactory;
    FDirect3DLevel: TDirect3DLevel;
    FDirect3DSupport: TDirect3DSupport;
    FDirect3DHardware: Boolean;
  private
    class procedure UpdateDirect3DLevel; static;
    class function TryCreateDirect3DDevice: Boolean; static;
    class procedure CreateDirect3DDevice; static;
    class procedure DestroyDirect3DDevice; static;
    class procedure AcquireDXGIFactory; static;
    class procedure CreateDirect2DFactory; static;
    class function GetDirect3DLevel: TDirect3DLevel; static;
    class function GetSharedDevice: ID3D10Device; static;
    class function GetSharedDXGIFactory: IDXGIFactory; static;
    class function GetSharedDWriteFactory: IDWriteFactory; static;
    class function GetSharedFactory: ID2D1Factory; static;
  protected
    class procedure CreateSharedResources; virtual;
    class procedure DestroySharedResources; virtual;
  public
    class property Direct3DLevel: TDirect3DLevel read GetDirect3DLevel;
    class property Direct3DSupport: TDirect3DSupport read FDirect3DSupport;
    class property Direct3DHardware: Boolean read FDirect3DHardware;
    class property SharedDevice: ID3D10Device read GetSharedDevice;
    class property SharedDXGIFactory: IDXGIFactory read GetSharedDXGIFactory;
    class property SharedFactory: ID2D1Factory read GetSharedFactory;
    class property SharedDWriteFactory: IDWriteFactory read GetSharedDWriteFactory;
  end;

procedure RegisterCanvasClasses;
procedure UnregisterCanvasClasses;

implementation

uses
  Winapi.Messages, Winapi.MultiMon, Winapi.ActiveX, Winapi.UxTheme, Winapi.DxgiFormat, Winapi.DxgiType,
  Winapi.D3DCommon, Winapi.D3D10_1, System.Types, System.Classes, System.Character, System.SysUtils, System.Math,
  System.Math.Vectors, System.Win.ComObj, System.Generics.Collections, System.UITypes, System.UIConsts,
  System.Messaging, FMX.Consts, FMX.Types, FMX.Types3D, FMX.Platform.Win, FMX.Forms, FMX.TextLayout, FMX.Surfaces,
  FMX.Utils, FMX.Helpers.Win;

type
  TBitmapCodecWIC = class(TCustomBitmapCodecWIC)
  private
    function DecodeFrame(const Frame: IWICBitmapFrameDecode; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal = 0): Boolean;
  public
    class function GetImageSize(const AFileName: string): TPointF; override;
    class function IsValid(const AStream: TStream): Boolean; override;
    function LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal): Boolean; override;
    function SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface;
      const SaveParams: PBitmapCodecSaveParams = nil): Boolean; override;
    function LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
      const UseEmbedded: Boolean; const Bitmap: TBitmapSurface): Boolean; override;
    function LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal): Boolean; override;
    function SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface; const Extension: string;
      const SaveParams: PBitmapCodecSaveParams = nil): Boolean; override;
  end;

  TD2DCanvasSaveState = class(TCanvasSaveState)
  private
    FStateBlock: ID2D1DrawingStateBlock;
    FLayer: ID2D1Layer;
    // FContextLostId: Integer; => https://quality.embarcadero.com/browse/RSP-19673
    // procedure ContextLostHandler(const Sender: TObject; const Msg: TMessage); => https://quality.embarcadero.com/browse/RSP-19673
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure CreateLayer(const RenderTarget: ID2D1RenderTarget); inline;
    property Layer: ID2D1Layer read FLayer;
  end;

  TD2DBitmapHandle = class
  private
    FTexture: ID3D10Texture2D;
    FSharedBitmap: ID2D1Bitmap;
    FAccess: TMapAccess;
    FMapBuffer: ID3D10Texture2D;
    FWidth: Integer;
    FHeight: Integer;
    // FContextLostId: Integer; => https://quality.embarcadero.com/browse/RSP-19673
    // procedure ContextLostHandler(const Sender: TObject; const Msg: TMessage); => https://quality.embarcadero.com/browse/RSP-19673
  public
    constructor Create(const AWidth, AHeight: Integer; const AAccess: TMapAccess);
    destructor Destroy; override;
    function CreateBitmap(const RenderTarget: ID2D1RenderTarget): ID2D1Bitmap;
    function Texture: ID3D10Texture2D;
  end;

  TCanvasD2D = class(TCustomCanvasD2D)
  private const
    DefaultRenderTargetMode: TD2D1RenderTargetType = D2D1_RENDER_TARGET_TYPE_DEFAULT;
  private class var
    fLock: Tobject; // https://quality.embarcadero.com/browse/RSP-19673
    FSharedTexture: ID3D10Texture2D;
    FSharedRenderTarget: ID2D1RenderTarget;
    class function SharedRenderTarget: ID2D1RenderTarget; static;
  private
    FBrush: ID2D1Brush;
    FLastBrushTransform: TMatrix;
    FStrokeBrush: ID2D1Brush;
    FStrokeStyle: ID2D1StrokeStyle;
    FMetaBrush: TCanvas.TMetaBrush;
    FMetaStroke: TCanvas.TMetaBrush;
    FMetaStrokeBrush: TCanvas.TMetaStrokeBrush;
    FLayer: ID2D1Layer;
    FCurrentSaveState: TD2DCanvasSaveState;
    FTarget: ID2D1RenderTarget;
    FContextHandle: THandle;
    // FContextLostId: Integer; => https://quality.embarcadero.com/browse/RSP-19673
    // window
    FSwapChain: IDXGISwapChain;
    FRenderTargetView: ID3D10RenderTargetView;
    FBufferTexture: ID3D10Texture2D;
    FBufferSize: TSize;
    // copy
    FCopyBuffer: ID3D10Texture2D;
    procedure CreateResources;
    procedure DisposeResources;
    procedure SetClipRects(const ARects: array of TRectF);
    function CreateD2DBitmapBrush(ABitmap: TBitmap; const AWrapMode: TWrapMode; const ARect: TRectF;
      const AOpacity: Single): ID2D1Brush;
    function CreateD2DGradientBrush(AGradient: TGradient; const ARect: TRectF; const AOpacity: Single): ID2D1Brush;
    procedure ApplyFill(const ABrush: TBrush; ARect: TRectF; const AOpacity: Single);
    procedure ApplyStroke(const AStroke: TStrokeBrush; ARect: TRectF; const AOpacity: Single);
    // procedure ContextLostHandler(const Sender: TObject; const Msg: TMessage); => https://quality.embarcadero.com/browse/RSP-19673
    procedure HandleDeviceRemoved;
  protected
    FLocaleName: string;
    procedure FontChanged(Sender: TObject); override;
    function CreateSaveState: TCanvasSaveState; override;
    { TCustomCanvasD2D }
    class procedure CreateSharedResources; override;
    class procedure DestroySharedResources; override;
    { begin and }
    function DoBeginScene(const AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean; override;
    procedure DoEndScene; override;
    procedure DoFlush; override;
    { creation }
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    constructor CreateFromBitmap(const ABitmap: TBitmap; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    constructor CreateFromPrinter(const APrinter: TAbstractPrinter); override;
    { Bitmaps }
    class function DoInitializeBitmap(const Width, Height: Integer; const Scale: Single; var PixelFormat: TPixelFormat): THandle; override;
    class procedure DoFinalizeBitmap(var Bitmap: THandle); override;
    class function DoMapBitmap(const Bitmap: THandle; const Access: TMapAccess; var Data: TBitmapData): Boolean; override;
    class procedure DoUnmapBitmap(const Bitmap: THandle; var Data: TBitmapData); override;
    { drawing }
    procedure DoSetMatrix(const M: TMatrix); override;
    procedure DoFillRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoFillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoFillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoDrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single;
      const HighSpeed: Boolean = False); override;
    procedure DoDrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
  public
    destructor Destroy; override;
    procedure SetSize(const AWidth, AHeight: Integer); override;
    { caps }
    class function GetAttribute(const Value: TCanvasAttribute): Integer; override;
    { buffer }
    procedure Clear(const Color: TAlphaColor); override;
    procedure ClearRect(const ARect: TRectF; const AColor: TAlphaColor = 0); override;
    { cliping }
    procedure IntersectClipRect(const ARect: TRectF); override;
    procedure ExcludeClipRect(const ARect: TRectF); override;
    { drawing }
    procedure MeasureLines(const ALines: TLineMetricInfo; const ARect: TRectF; const AText: string; const WordWrap: Boolean; const Flags: TFillTextFlags;
      const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign = TTextAlign.Center); override;
    function PtInPath(const APoint: TPointF; const APath: TPathData): Boolean; override;
  end;

  TTextLayoutD2D = class(TTextLayout)
  private type
    TDWriteFontDescriptor = record
      FamilyName: string;
      Weight: TDWriteFontWeight;
      Style: TDWriteFontStyle;
      Stretch: TDWriteFontStretch;
      class function Create(const AFamilyName: string; const AWeight: TDWriteFontWeight; const AStyle: TDWriteFontStyle;
        const AStretch: TDWriteFontStretch): TDWriteFontDescriptor; static; inline;
    end;
  private class var
    FDefaultDrawTextOption: Integer;
    FFontCollection: IDWriteFontCollection;
    FLocaleName: string;
    FFontDescriptors: TDictionary<string, TDWriteFontDescriptor>;
  private
    class function EnumFontFamExProc(const lpelfe: TLogFont; const lpntme: TNewTextMetric; FontType: DWORD;
      lParam: LPARAM): Integer; stdcall; static;
    class function GetDefaultDrawTextOption: Integer; static;
    class property DefaultDrawTextOption: Integer read GetDefaultDrawTextOption;
  private
    FLayout: IDWriteTextLayout;
    FBrush: ID2D1Brush;
    FDrawTextOption: Integer;
    FMetrics: TDWriteTextMetrics;
    FOverhangMetrics: TDwriteOverhangMetrics;
    // FContextLostId: Integer; => https://quality.embarcadero.com/browse/RSP-19673
    FTextRect: TRectF;
    function GetFontStyles(const AFont: TFont): TDWriteFontDescriptor;
    procedure UpdateTextRect;
    // procedure ContextLostHandler(const Sender: TObject; const Msg: TMessage); => https://quality.embarcadero.com/browse/RSP-19673
    class procedure DestroyGlobalResources;
  protected
    procedure DoRenderLayout; override;
    procedure DoDrawLayout(const ACanvas: TCanvas); override;
    function GetTextHeight: Single; override;
    function GetTextWidth: Single; override;
    function GetTextRect: TRectF; override;
    function DoPositionAtPoint(const APoint: TPointF): Integer; override;
    function DoRegionForRange(const ARange: TTextRange): TRegion; override;
  public
    constructor Create(const ACanvas: TCanvas = nil); override;
    destructor Destroy; override;
    procedure ConvertToPath(const APath: TPathData); override;
  end;

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

{ TCustomBitmapCodecWIC }

class destructor TCustomBitmapCodecWIC.DestroySharedResources;
begin
  FImagingFactory := nil;
end;

class function TCustomBitmapCodecWIC.GetImagingFactory: IWICImagingFactory;
begin
  if FImagingFactory = nil then
  begin
    if Failed(CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IUnknown,
      FImagingFactory)) or (FImagingFactory = nil) then
      raise ECannotCreateWICImagingFactory.CreateFmt(SCannotCreateWICImagingFactory, [ClassName]);
  end;
  Result := FImagingFactory;
end;

{ TCustomCanvasD2D }

class procedure TCustomCanvasD2D.UpdateDirect3DLevel;
var
  DXLib: THandle;
begin
  if FDirect3DLevel = TDirect3DLevel.Undetermined then
  begin
    if GlobalUseDX then
    begin
      FDirect3DLevel := TDirect3DLevel.Undetermined;

      // Direct3D 10.1
      DXLib := LoadLibrary(D3D10_1_dll);
      if DXLib <> 0 then
      try
        if GetProcAddress(DXLib, 'D3D10CreateDevice1') <> nil then
          FDirect3DLevel := TDirect3DLevel.Direct3D10_1;
      finally
        FreeLibrary(DXLib);
      end;

      if FDirect3DLevel = TDirect3DLevel.Undetermined then
      begin // Direct3D 10.0
        DXLib := LoadLibrary(D3D10dll);
        if DXLib <> 0 then
        try
          if GetProcAddress(DXLib, 'D3D10CreateDevice') <> nil then
            FDirect3DLevel := TDirect3DLevel.Direct3D10;
        finally
          FreeLibrary(DXLib);
        end;
      end;

      if FDirect3DLevel = TDirect3DLevel.Undetermined then
        FDirect3DLevel := TDirect3DLevel.Unsupported;
    end
    else
      FDirect3DLevel := TDirect3DLevel.Unsupported;
  end;
end;

class function TCustomCanvasD2D.GetDirect3DLevel: TDirect3DLevel;
begin
  UpdateDirect3DLevel;
  Result := FDirect3DLevel;
end;

class function TCustomCanvasD2D.TryCreateDirect3DDevice: Boolean;

  function CreateDevice(const DriverType: D3D10_DRIVER_TYPE): HResult;
  var
    Flags: Cardinal;
  begin
    Result := S_OK;
    Flags := {$IFDEF DXDEBUG}D3D10_CREATE_DEVICE_DEBUG{$ELSE}0{$ENDIF} or D3D10_CREATE_DEVICE_BGRA_SUPPORT;

    if FDirect3DLevel = TDirect3DLevel.Direct3D10_1 then
    begin
      // Direct3D 10.1 with full hardware support
      Result := D3D10CreateDevice1(nil, DriverType, 0, Flags, D3D10_FEATURE_LEVEL_10_1,
        D3D10_1_SDK_VERSION, ID3D10Device1(FSharedDevice));

      if Succeeded(Result) then
        FDirect3DSupport := TDirect3DSupport.Direct3D10_1;

      if Failed(Result) then
      begin
        // Direct3D 10.1 with hardware support of 10.0
        Result := D3D10CreateDevice1(nil, DriverType, 0, Flags, D3D10_FEATURE_LEVEL_10_0,
          D3D10_1_SDK_VERSION, ID3D10Device1(FSharedDevice));

        if Succeeded(Result) then
          FDirect3DSupport := TDirect3DSupport.Direct3D10;
      end;
    end;

    if (FDirect3DLevel = TDirect3DLevel.Direct3D10) or Failed(Result) then
    begin
      // Legacy Direct3D 10.0 (on unpatched version of Vista)
      Result := D3D10CreateDevice(nil, DriverType, 0, Flags, D3D10_SDK_VERSION, FSharedDevice);

      if Succeeded(Result) then
      begin
        FDirect3DLevel := TDirect3DLevel.Direct3D10;
        FDirect3DSupport := TDirect3DSupport.Direct3D10Legacy;
      end;
    end;
  end;

var
  Res: HResult;
begin
  SaveClearFPUState;
  try
    if GlobalUseDXSoftware then
    begin
      Res := CreateDevice(D3D10_DRIVER_TYPE_WARP);
      if Failed(Res) then
      begin
        // WARP device might not be supported on pre-DX10.1 system, which may still support DX10 in hardware.
        Res := CreateDevice(D3D10_DRIVER_TYPE_HARDWARE);
        if Succeeded(Res) then
          FDirect3DHardware := True;
      end
      else
        FDirect3DHardware := False;
    end
    else
    begin
      Res := CreateDevice(D3D10_DRIVER_TYPE_HARDWARE);
      if Failed(Res) then
      begin
        Res := CreateDevice(D3D10_DRIVER_TYPE_WARP);
        if Succeeded(Res) then
          FDirect3DHardware := False;
      end
      else
        FDirect3DHardware := True;
    end;
    Result := Succeeded(Res);
  finally
    RestoreFPUState;
  end;
end;

class procedure TCustomCanvasD2D.CreateDirect3DDevice;
{$IFDEF DXDEBUG}
var
  DebugText: string;
{$ENDIF}
begin
  if FDirect3DLevel < TDirect3DLevel.Direct3D10 then
    raise ECannotDetermineDirect3DLevel.CreateFmt(SCannotDetermineDirect3DLevel, [ClassName]);

  if not TryCreateDirect3DDevice then
    raise ECannotCreateD3DDevice.CreateFmt(SCannotCreateD3DDevice, [ClassName]);

{$IFDEF DXDEBUG}
  case FDirect3DSupport of
    TDirect3DSupport.Direct3D10Legacy:
      DebugText := 'Direct2D Canvas is using legacy Direct3D 10';
    TDirect3DSupport.Direct3D10:
      DebugText := 'Direct2D Canvas is using Direct3D 10.1 with 10.0 feature level';
    TDirect3DSupport.Direct3D10_1:
      DebugText := 'Direct2D Canvas is using Direct3D 10.1';
  else
    DebugText := 'Direct2D Canvas is using unknown Direct3D';
  end;
  if FDirect3DHardware then
    DebugText := DebugText + ' (HAL)'
  else
    DebugText := DebugText + ' (WARP)';
  OutputDebugString(PChar(DebugText));
{$ENDIF}
end;

class procedure TCustomCanvasD2D.DestroyDirect3DDevice;
begin
  FSharedDevice := nil;
end;

class procedure TCustomCanvasD2D.AcquireDXGIFactory;
var
  DXGIDevice: IDXGIDevice;
  DXGIAdapter: IDXGIAdapter;
begin
  if Succeeded(FSharedDevice.QueryInterface(IDXGIDevice, DXGIDevice)) and (DXGIDevice <> nil) and
    Succeeded(DXGIDevice.GetParent(IDXGIAdapter, DXGIAdapter)) and (DXGIAdapter <> nil) then
      DXGIAdapter.GetParent(IDXGIFactory, FSharedDXGIFactory);
end;

class procedure TCustomCanvasD2D.CreateDirect2DFactory;
var
{$IFDEF D2DEBUG}
  Opts: TD2D1FactoryOptions;
{$ENDIF}
  FactoryOptions: PD2D1FactoryOptions;
  Res: HResult;
begin
{$IFDEF D2DEBUG}
  Opts.debugLevel := D2D1_DEBUG_LEVEL_INFORMATION;
  FactoryOptions := @Opts;
{$ELSE}
  FactoryOptions := nil;
{$ENDIF}

  Res := D2D1CreateFactory(D2D1_FACTORY_TYPE_MULTI_THREADED, ID2D1Factory, FactoryOptions, FSharedFactory);
  if Failed(Res) or (FSharedFactory = nil) then
    raise ECannotCreateD2DFactory.CreateFmt(SCannotCreateD2DFactory, [ClassName]);

  if Failed(DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, IDWriteFactory, IUnknown(FSharedDWriteFactory))) then
    raise ECannotCreateDWriteFactory.CreateFmt(SCannotCreateDWriteFactory, [ClassName]);
end;

class procedure TCustomCanvasD2D.CreateSharedResources;
begin
  UpdateDirect3DLevel;
  if FSharedDevice = nil then
  begin
    CreateDirect3DDevice;
    AcquireDXGIFactory;
  end;
  if FSharedFactory = nil then
    CreateDirect2DFactory;
end;

class procedure TCustomCanvasD2D.DestroySharedResources;
begin
  FSharedDWriteFactory := nil;
  FSharedFactory := nil;
  FSharedDXGIFactory := nil;
  DestroyDirect3DDevice;
end;

class function TCustomCanvasD2D.GetSharedDevice: ID3D10Device;
begin
  if FSharedDevice = nil then
    CreateSharedResources;
  Result := FSharedDevice;
end;

class function TCustomCanvasD2D.GetSharedDXGIFactory: IDXGIFactory;
begin
  if FSharedDXGIFactory = nil then
    CreateSharedResources;
  Result := FSharedDXGIFactory;
end;

class function TCustomCanvasD2D.GetSharedFactory: ID2D1Factory;
begin
  if FSharedFactory = nil then
    CreateSharedResources;
  Result := FSharedFactory;
end;

class function TCustomCanvasD2D.GetSharedDWriteFactory: IDWriteFactory;
begin
  if FSharedDWriteFactory = nil then
    CreateSharedResources;
  Result := FSharedDWriteFactory;
end;

{ TBitmapCodecWIC }

class function TBitmapCodecWIC.GetImageSize(const AFileName: string): TPointF;
var
  Decoder: IWICBitmapDecoder;
  Frame: IWICBitmapFrameDecode;
  W, H: UINT;
begin
  W := 0;
  H := 0;
  ImagingFactory.CreateDecoderFromFilename(PChar(AFileName), GUID_NULL, $FFFFFFFF, WICDecodeMetadataCacheOnDemand,
    Decoder);
  if Decoder <> nil then
  begin
    Decoder.GetFrame(0, Frame);
    if Frame <> nil then
      Frame.GetSize(W, H);
  end;
  Result := PointF(W, H);
end;

class function TBitmapCodecWIC.IsValid(const AStream: TStream): Boolean;
var
  Decoder: IWICBitmapDecoder;
  Frame: IWICBitmapFrameDecode;
  W, H: UINT;
  CopyStream: TMemoryStream;
  Stream: IWICStream;
  SavePosition: Int64;
begin
  W := 0;
  H := 0;
  SavePosition := AStream.Position;
  try
    CopyStream := TMemoryStream.Create;
    try
      CopyStream.CopyFrom(AStream, AStream.Size);

      ImagingFactory.CreateStream(Stream);
      Stream.InitializeFromMemory(CopyStream.Memory, CopyStream.Size);

      ImagingFactory.CreateDecoderFromStream(stream, GUID_NULL, WICDecodeMetadataCacheOnDemand, Decoder);
      if Decoder <> nil then
      begin
        Decoder.GetFrame(0, Frame);
        if Frame <> nil then
          Frame.GetSize(W, H);
      end;
      Result := W * H <> 0;
    finally
      CopyStream.Free;
    end;
  finally
    AStream.Position := SavePosition;
  end;
end;

function TBitmapCodecWIC.DecodeFrame(const Frame: IWICBitmapFrameDecode; const Bitmap: TBitmapSurface;
  const MaxSizeLimit: Cardinal): Boolean;
var
  Converter: IWICFormatConverter;
  Scaler: IWICBitmapScaler;
  R: TRectF;
  Width, Height: UINT;
begin
  Result := False;
  Frame.GetSize(Width, Height);
  if (MaxSizeLimit > 0) and ((Width > MaxSizeLimit) or (Height > MaxSizeLimit)) then
  begin
    R := TRectF.Create(0, 0, Width, Height);
    R.Fit(TRectF.Create(0, 0, MaxSizeLimit, MaxSizeLimit));

    ImagingFactory.CreateBitmapScaler(Scaler);
    if Succeeded(Scaler.Initialize(Frame, Trunc(R.Width), Trunc(R.Height), WICBitmapInterpolationModeLinear)) then
    begin
      ImagingFactory.CreateFormatConverter(Converter);
      if Succeeded(Converter.Initialize(Scaler, GUID_WICPixelFormat32bppPBGRA, WICBitmapDitherTypeNone, nil, 0, 0)) then
      begin
        Converter.GetSize(Width, Height);

        Bitmap.SetSize(Width, Height, TPixelFormat.BGRA);
        Result := Succeeded(Converter.CopyPixels(nil, Bitmap.Pitch, Height * Cardinal(Bitmap.Pitch), PByte(Bitmap.Bits)));
      end;
    end;
  end
  else
  begin
    ImagingFactory.CreateFormatConverter(Converter);
    if Succeeded(Converter.Initialize(Frame, GUID_WICPixelFormat32bppPBGRA, WICBitmapDitherTypeNone, nil, 0, 0)) then
    begin
      Converter.GetSize(Width, Height);

      Bitmap.SetSize(Width, Height, TPixelFormat.BGRA);
      Result := Succeeded(Converter.CopyPixels(nil, Bitmap.Pitch, Height * Cardinal(Bitmap.Pitch), PByte(Bitmap.Bits)));
    end;
  end;
end;

function TBitmapCodecWIC.LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface;
  const MaxSizeLimit: Cardinal): Boolean;
var
  Decoder: IWICBitmapDecoder;
  Frame: IWICBitmapFrameDecode;
  Stream: TStream;
  Adapter: TStreamAdapter;
begin
  Result := False;
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Adapter := TStreamAdapter.Create(Stream);

    ImagingFactory.CreateDecoderFromStream(Adapter, GUID_NULL, WICDecodeMetadataCacheOnDemand, Decoder);
    if Decoder <> nil then
    begin
      Decoder.GetFrame(0, Frame);
      if Frame <> nil then
        Result := DecodeFrame(Frame, Bitmap, MaxSizeLimit);
    end;
  finally
    Stream.Free;
  end;
end;

function TBitmapCodecWIC.LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
  const UseEmbedded: Boolean; const Bitmap: TBitmapSurface): Boolean;
var
  Bmp: IWICBitmapSource;
  Converter: IWICFormatConverter;
  Scaler: IWICBitmapScaler;
  Decoder: IWICBitmapDecoder;
  Frame: IWICBitmapFrameDecode;
  R: TRectF;
  Width, Height: UINT;
begin
  Result := False;
  ImagingFactory.CreateDecoderFromFilename(PChar(AFileName), GUID_NULL, $FFFFFFFF, WICDecodeMetadataCacheOnDemand,
    Decoder);
  if Decoder <> nil then
  begin
    Decoder.GetFrame(0, Frame);
    if UseEmbedded then
      Frame.GetThumbnail(Bmp);
    if Bmp <> nil then
    begin
      ImagingFactory.CreateFormatConverter(Converter);
      if Succeeded(Converter.Initialize(Bmp, GUID_WICPixelFormat32bppPBGRA, WICBitmapDitherTypeNone, nil, 0, 0)) then
      begin
        Converter.GetSize(Width, Height);

        Bitmap.SetSize(Width, Height, TPixelFormat.BGRA);
        Result := Succeeded(Converter.CopyPixels(nil, Bitmap.Pitch, Height * Cardinal(Bitmap.Pitch),
          PByte(Bitmap.Bits)));
      end;
    end
    else
    if Frame <> nil then
    begin
      Frame.GetSize(Width, Height);

      R := TRectF.Create(0, 0, Width, Height);
      R.Fit(TRectF.Create(0, 0, AFitWidth, AFitHeight));

      ImagingFactory.CreateBitmapScaler(Scaler);
      if Succeeded(Scaler.Initialize(frame, Trunc(R.Width), Trunc(R.Height), WICBitmapInterpolationModeLinear)) then
      begin
        ImagingFactory.CreateFormatConverter(Converter);
        if Succeeded(Converter.Initialize(scaler, GUID_WICPixelFormat32bppPBGRA,
          WICBitmapDitherTypeNone, nil, 0, 0)) then
        begin
          Converter.GetSize(Width, Height);

          Bitmap.SetSize(Width, Height, TPixelFormat.BGRA);
          Result := Succeeded(Converter.CopyPixels(nil, Bitmap.Pitch, Height * Cardinal(Bitmap.Pitch),
            PByte(Bitmap.Bits)));
        end;
      end;
    end;
  end;
end;

function TBitmapCodecWIC.LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface;
  const MaxSizeLimit: Cardinal): Boolean;
var
  Decoder: IWICBitmapDecoder;
  CopyStream: TMemoryStream;
  Stream: IWICStream;
  Frame: IWICBitmapFrameDecode;
begin
  Result := False;
  CopyStream := TMemoryStream.Create;
  try
    CopyStream.CopyFrom(AStream, AStream.Size);

    ImagingFactory.CreateStream(Stream);
    Stream.InitializeFromMemory(CopyStream.Memory, CopyStream.Size);

    ImagingFactory.CreateDecoderFromStream(Stream, GUID_NULL, WICDecodeMetadataCacheOnDemand, Decoder);
    if Decoder <> nil then
    begin
      Decoder.GetFrame(0, Frame);
      if Frame <> nil then
        Result := DecodeFrame(Frame, Bitmap, MaxSizeLimit);
    end;
  finally
    CopyStream.Free;
  end;
end;

function TBitmapCodecWIC.SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface; const Extension: string;
  const SaveParams: PBitmapCodecSaveParams): Boolean;
const
  MaxBitmapHeaderSize = 8192;
var
  Bmp: IWICBitmap;
  Enc: IWICBitmapEncoder;
  EncoderType: TGUID;
  Stream: IWICStream;
  Frame: IWICBitmapFrameEncode;
  Conv: IWICFormatConverter;
  Props: IPropertyBag2;
  Format: WICPixelFormatGUID;
  ParamName: TPropBag2;
  ParamValue: TPropVariant;
  InitPosition, MaxSubstreamSize: UInt64;
begin
  Result := False;

  EncoderType := GUID_ContainerFormatPng;
  if SameText(Extension, SJPEGImageExtension) then
    EncoderType := GUID_ContainerFormatJpeg;
  if SameText(Extension, SJPGImageExtension) then
    EncoderType := GUID_ContainerFormatJpeg;
  if SameText(Extension, SPNGImageExtension) then
    EncoderType := GUID_ContainerFormatPng;
  if SameText(Extension, SBMPImageExtension) then
    EncoderType := GUID_ContainerFormatBmp;
  if SameText(Extension, STIFImageExtension) then
    EncoderType := GUID_ContainerFormatTiff;
  if SameText(Extension, STIFFImageExtension) then
    EncoderType := GUID_ContainerFormatTiff;
  if SameText(Extension, SGIFImageExtension) then
    EncoderType := GUID_ContainerFormatGif;
  if SameText(Extension, SWMPImageExtension) then
    EncoderType := GUID_ContainerFormatWmp;
  ImagingFactory.CreateEncoder(EncoderType, GUID_NULL, Enc);
  if Enc <> nil then
  begin
    ImagingFactory.CreateStream(Stream);

    if AStream.Position = 0 then
    begin
      InitPosition := 0;
      Stream.InitializeFromIStream(TStreamAdapter.Create(AStream) as IStream);
    end
    else
    begin
      InitPosition := AStream.Position;
      MaxSubstreamSize := 2 * Bitmap.BytesPerPixel * Bitmap.Width * Bitmap.Height + MaxBitmapHeaderSize;
      Stream.InitializeFromIStreamRegion(TStreamAdapter.Create(AStream) as IStream, ULARGE_INTEGER(InitPosition),
        ULARGE_INTEGER(MaxSubstreamSize));
    end;

    Enc.Initialize(Stream, WICBitmapEncoderNoCache);
    Enc.CreateNewFrame(Frame, Props);
    if Frame <> nil then
    begin
      if SaveParams <> nil then
      begin
        FillChar(ParamName, SizeOf(ParamName), 0);
        ParamName.dwType := 1; // PROPBAG2_TYPE_DATA;
        ParamName.vt := VT_R4;
        ParamName.pstrName := PChar(string('ImageQuality'));
        ParamValue.vt := VT_R4;
        ParamValue.fltVal := SaveParams.Quality / 100;
        Props.Write(1, @ParamName, @ParamValue);
      end;
      Frame.Initialize(Props);
      Frame.SetSize(Bitmap.Width, Bitmap.Height);
      Format := GUID_WICPixelFormat32bppPBGRA;
      frame.SetPixelFormat(Format);
      if IsEqualGuid(Format, GUID_WICPixelFormat32bppPBGRA) then
      begin
        Frame.WritePixels(Bitmap.Height, Bitmap.Pitch, Bitmap.Pitch * Bitmap.Height, PByte(Bitmap.Bits));
        Frame.Commit;
        Enc.Commit;
      end
      else
      begin
        ImagingFactory.CreateBitmapFromMemory(Bitmap.Width, Bitmap.Height, GUID_WICPixelFormat32bppPBGRA,
          Bitmap.Pitch, Bitmap.Pitch * Bitmap.Height, PByte(Bitmap.Bits), Bmp);
        ImagingFactory.CreateFormatConverter(Conv);
        Conv.Initialize(Bmp, Format, WICBitmapDitherTypeNone, nil, 0, 0);
        Frame.WriteSource(Bmp, nil);
        Frame.Commit;
        Enc.Commit;
      end;

      if InitPosition <> 0 then
        AStream.Seek(0, TSeekOrigin.soEnd);

      Result := True;
    end;
  end;
end;

function TBitmapCodecWIC.SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface;
  const SaveParams: PBitmapCodecSaveParams): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    Result := SaveToStream(Stream, Bitmap, ExtractFileExt(AFileName).ToLower, SaveParams);
  finally
    Stream.Free;
  end;
end;

{ Canvas }

function D2Rect(const R: TRectF): TD2D1RectF; inline;
begin
  Result := TD2D1RectF(R);
end;

function D2Color(const AColor: TAlphaColor; const Opacity: Single): TD2D1ColorF;
var
  D2R: Single;
  D2G: Single;
  D2B: Single;
  D2A: Single;
begin
  D2R := TAlphaColorRec(AColor).R / $FF;
  D2B := TAlphaColorRec(AColor).B / $FF;
  D2G := TAlphaColorRec(AColor).G / $FF;
  D2A := TAlphaColorRec(AColor).A / $FF;
  Result := D2D1ColorF(D2R, D2G, D2B, D2A * Opacity);
end;

function D2Point(const X, Y: Single): TD2D1Point2F; inline;
begin
  Result.x := X;
  Result.y := Y;
end;

function D2Size(const W, H: Cardinal): TD2DSizeU; inline;
begin
  Result.Width := W;
  Result.Height := H;
end;

function D2Matrix(const M: TMatrix): TD2D1Matrix3X2F;
begin
  Result._11 := M.m11;
  Result._12 := M.m12;
  Result._21 := M.m21;
  Result._22 := M.m22;
  Result._31 := M.m31;
  Result._32 := M.m32;
end;

function MatrixToD2(const M: TD2D1Matrix3X2F): TMatrix;
begin
  Result.m11 := M._11;
  Result.m12 := M._12;
  Result.m21 := M._21;
  Result.m22 := M._22;
  Result.m31 := M._31;
  Result.m32 := M._32;
end;

function D2Bezier(const X1, Y1, X2, Y2, X3, Y3: Single): TD2D1BezierSegment;
begin
  Result.point1.x := X1;
  Result.point1.y := Y1;
  Result.point2.x := X2;
  Result.point2.y := Y2;
  Result.point3.x := X3;
  Result.point3.y := Y3;
end;

function D2Ellipse(const R: TRectF): TD2D1Ellipse;
begin
  Result.point.x := (R.Left + R.Right) / 2;
  Result.point.y := (R.Top + R.Bottom) / 2;
  Result.radiusX := R.Width / 2;
  Result.radiusY := R.Height / 2;
end;

function BitmapProp(const PixelFormat: DXGI_FORMAT; const AlphaMode: TD2D1AlphaMode): TD2D1BitmapProperties;
begin
  Result.pixelFormat.format := PixelFormat;
  Result.pixelFormat.alphaMode := AlphaMode;
  Result.dpiX := 0;
  Result.dpiY := 0;
end;

{ TD2DBitmap }

constructor TD2DBitmapHandle.Create(const AWidth, AHeight: Integer; const AAccess: TMapAccess);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FAccess := AAccess;
  // FContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, ContextLostHandler);  => https://quality.embarcadero.com/browse/RSP-19673
end;

destructor TD2DBitmapHandle.Destroy;
begin
  //TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FContextLostId); => https://quality.embarcadero.com/browse/RSP-19673
  inherited;
end;

function TD2DBitmapHandle.CreateBitmap(const RenderTarget: ID2D1RenderTarget): ID2D1Bitmap;
var
  Prop: TD2D1BitmapProperties;
begin
  if Texture <> nil then
  begin
    Prop := BitmapProp(DXGI_FORMAT_UNKNOWN, D2D1_ALPHA_MODE_PREMULTIPLIED);
    if FSharedBitmap = nil then
      TCanvasD2D.SharedRenderTarget.CreateSharedBitmap(IDXGISurface, Pointer(Texture as IDXGISurface), @Prop,
        FSharedBitmap);
    RenderTarget.CreateSharedBitmap(ID2D1Bitmap, Pointer(FSharedBitmap), @Prop, Result);
  end
  else
    Result := nil;
end;

function TD2DBitmapHandle.Texture: ID3D10Texture2D;
var
  Desc: TD3D10_Texture2DDesc;
begin
  if FTexture = nil then
  begin
    FillChar(Desc, SizeOf(D3D10_TEXTURE2D_DESC), 0);
    Desc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
    Desc.Width := FWidth;
    Desc.Height := FHeight;
    Desc.MipLevels := 1;
    Desc.ArraySize := 1;
    Desc.SampleDesc.Count := 1;
    Desc.SampleDesc.Quality := 0;
    Desc.Usage := D3D10_USAGE_DEFAULT;
    Desc.BindFlags := D3D10_BIND_RENDER_TARGET or D3D10_BIND_SHADER_RESOURCE;
    if Failed(TCustomCanvasD2D.SharedDevice.CreateTexture2D(Desc, nil, FTexture)) then
      raise ECannotCreateTexture.CreateFmt(SCannotCreateTexture, [ClassName]);
  end;
  Result := FTexture;
end;

//https://quality.embarcadero.com/browse/RSP-19673
//procedure TD2DBitmapHandle.ContextLostHandler(const Sender: TObject; const Msg: TMessage);
//begin
//  FTexture := nil;
//  FSharedBitmap := nil;
//  FMapBuffer := nil;
//end;

{ TCanvasD2D }

constructor TCanvasD2D.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AQuality: TCanvasQuality);
begin
  inherited;
  FLastBrushTransform := TMatrix.Identity;
  if WindowHandleToPlatform(Parent).Transparency then
    WindowHandleToPlatform(Parent).CreateBuffer(WindowHandleToPlatform(Parent).WndClientSize.Width,
      WindowHandleToPlatform(Parent).WndClientSize.Height);
  // FContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, ContextLostHandler); => https://quality.embarcadero.com/browse/RSP-19673
end;

constructor TCanvasD2D.CreateFromBitmap(const ABitmap: TBitmap; const AQuality: TCanvasQuality);
begin
  inherited;
  FLastBrushTransform := TMatrix.Identity;
  CreateResources;
  // FContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, ContextLostHandler); => https://quality.embarcadero.com/browse/RSP-19673
end;

constructor TCanvasD2D.CreateFromPrinter(const APrinter: TAbstractPrinter);
begin
  // Just a stub implementation - not used.
end;

destructor TCanvasD2D.Destroy;
begin
  // TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FContextLostId); => https://quality.embarcadero.com/browse/RSP-19673
  DisposeResources;
  FreeAndNil(FMetaStrokeBrush);
  FreeAndNil(FMetaStroke);
  FreeAndNil(FMetaBrush);
  inherited;
end;

procedure TCanvasD2D.CreateResources;
const
  TargetDefaultDPI = 96;
var
  Prop: TD2D1RenderTargetProperties;
  BackBuffer: ID3D10Texture2D;
  SwapDesc: TDXGISwapChainDesc;
  Desc: TD3D10_Texture2DDesc;
begin
  if FTarget = nil then
  begin
    if Bitmap <> nil then
    begin
      if Bitmap.HandleAllocated then
      begin
        Prop := D2D1RenderTargetProperties(DefaultRenderTargetMode, D2D1PixelFormat(DXGI_FORMAT_UNKNOWN,
          D2D1_ALPHA_MODE_PREMULTIPLIED));

        if Failed(SharedFactory.CreateDxgiSurfaceRenderTarget(TD2DBitmapHandle(Bitmap.Handle).Texture as IDXGISurface, Prop,
          FTarget)) or (FTarget = nil) then
          raise ECannotCreateRenderTarget.CreateFmt(SCannotCreateRenderTarget, [ClassName]);
      end;
    end
    else if (Parent <> nil) then
    begin
      FBufferSize := TSize.Create(WindowHandleToPlatform(Parent).WndClientSize.Width,
        WindowHandleToPlatform(Parent).WndClientSize.Height);

      if not (SameValue(FBufferSize.Width, 0, TEpsilon.Position) or SameValue(FBufferSize.Height, 0, TEpsilon.Position)) then
      begin
        if WindowHandleToPlatform(Parent).Transparency then
        begin
          WindowHandleToPlatform(Parent).ResizeBuffer(FBufferSize.Width, FBufferSize.Height);

          FillChar(Desc, SizeOf(D3D10_TEXTURE2D_DESC), 0);
          Desc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
          Desc.Width := FBufferSize.Width;
          Desc.Height := FBufferSize.Height;
          Desc.MipLevels := 1;
          Desc.ArraySize := 1;
          Desc.SampleDesc.Count := 1;
          Desc.SampleDesc.Quality := 0;
          Desc.Usage := D3D10_USAGE_DEFAULT;
          Desc.BindFlags := D3D10_BIND_RENDER_TARGET or D3D10_BIND_SHADER_RESOURCE;
          if Failed(SharedDevice.CreateTexture2D(Desc, nil, FBufferTexture)) or (FBufferTexture = nil) then
            raise ECannotCreateTexture.CreateFmt(SCannotCreateTexture, [ClassName]);

          Prop := D2D1RenderTargetProperties(DefaultRenderTargetMode, D2D1PixelFormat(DXGI_FORMAT_UNKNOWN,
            D2D1_ALPHA_MODE_PREMULTIPLIED));

          if Failed(SharedFactory.CreateDxgiSurfaceRenderTarget(FBufferTexture as IDXGISurface, Prop, FTarget)) or
            (FTarget = nil) then
            raise ECannotCreateRenderTarget.CreateFmt(SCannotCreateRenderTarget, [ClassName]);

          FillChar(Desc, SizeOf(D3D10_TEXTURE2D_DESC), 0);
          Desc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
          Desc.Width := FBufferSize.Width;
          Desc.Height := FBufferSize.Height;
          Desc.MipLevels := 1;
          Desc.ArraySize := 1;
          Desc.SampleDesc.Count := 1;
          Desc.SampleDesc.Quality := 0;
          Desc.CPUAccessFlags := D3D10_CPU_ACCESS_READ;
          Desc.Usage := D3D10_USAGE_STAGING;
          Desc.BindFlags := 0;

          if Failed(SharedDevice.CreateTexture2D(Desc, nil, FCopyBuffer)) or (FCopyBuffer = nil) then
            raise ECannotCreateTexture.CreateFmt(SCannotCreateTexture, [ClassName]);
        end
        else
        begin
          FillChar(SwapDesc, SizeOf(SwapDesc), 0);
          SwapDesc.BufferCount:= 1;
          SwapDesc.BufferDesc.Width := FBufferSize.Width;
          SwapDesc.BufferDesc.Height:= FBufferSize.Height;
          SwapDesc.BufferDesc.Format:= DXGI_FORMAT_B8G8R8A8_UNORM;
          SwapDesc.BufferUsage := DXGI_USAGE_RENDER_TARGET_OUTPUT;
          SwapDesc.OutputWindow := WindowHandleToPlatform(Parent).Wnd;
          SwapDesc.SampleDesc.Count  := 1;
          SwapDesc.SampleDesc.Quality:= 0;
          SwapDesc.Windowed := True;

          if Failed(SharedDXGIFactory.CreateSwapChain(SharedDevice, SwapDesc, FSwapChain)) or (FSwapChain = nil) then
            raise ECannotCreateSwapChain.CreateFmt(SCannotCreateSwapChain, [ClassName]);

          if Failed(FSwapChain.GetBuffer(0, ID3D10Texture2D, BackBuffer)) or (BackBuffer = nil) then
            raise ERetrieveSurfaceContents.CreateFmt(SRetrieveSurfaceContents, [ClassName]);

          if Failed(SharedDevice.CreateRenderTargetView(BackBuffer, nil, @FRenderTargetView)) then
            raise ECannotCreateRenderTargetView.CreateFmt(SCannotCreateRenderTargetView, [ClassName]);

          Prop := D2D1RenderTargetProperties(DefaultRenderTargetMode, D2D1PixelFormat(DXGI_FORMAT_UNKNOWN,
            D2D1_ALPHA_MODE_PREMULTIPLIED));

          if Failed(SharedFactory.CreateDxgiSurfaceRenderTarget(BackBuffer as IDXGISurface, Prop, FTarget)) then
            raise ECannotCreateRenderTarget.CreateFmt(SCannotCreateRenderTarget, [ClassName]);

          SharedDXGIFactory.MakeWindowAssociation(WindowHandleToPlatform(Parent).Wnd, DXGI_MWA_NO_WINDOW_CHANGES);
        end;
      end;
    end;
    if FTarget <> nil then
    begin
      FTarget.SetDpi(TargetDefaultDPI, TargetDefaultDPI);
      case Quality of
        TCanvasQuality.HighPerformance:
          FTarget.SetAntialiasMode(D2D1_ANTIALIAS_MODE_ALIASED);
        TCanvasQuality.HighQuality:
          FTarget.SetAntialiasMode(D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
      else
        FTarget.SetAntialiasMode(D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
      end;
      FTarget.SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_DEFAULT);
    end;
  end
end;

procedure TCanvasD2D.DisposeResources;
begin
  if FMetaBrush <> nil then
    FMetaBrush.Valid := False;
  if FMetaStroke <> nil then
    FMetaStroke.Valid := False;
  if FMetaStrokeBrush <> nil then
    FMetaStrokeBrush.Valid := False;
  FLastBrushTransform := TMatrix.Identity;
  FBrush := nil;
  FStrokeBrush := nil;
  FStrokeStyle := nil;
  FLayer := nil;
  FTarget := nil;
  FSwapChain := nil;
  FRenderTargetView := nil;
  FBufferTexture := nil;
  FCopyBuffer := nil;
end;

function TCanvasD2D.CreateSaveState: TCanvasSaveState;
begin
  Result := TD2DCanvasSaveState.Create;
end;

procedure UpdateBrushMatrix(const ABrush: ID2D1Brush; const AMatrix: TMatrix);
var
  LTransformD2D: D2D1_MATRIX_3X2_F;
begin
  LTransformD2D._11 := AMatrix.m11;
  LTransformD2D._12 := AMatrix.m12;
  LTransformD2D._21 := AMatrix.m21;
  LTransformD2D._22 := AMatrix.m22;
  LTransformD2D._31 := AMatrix.m31;
  LTransformD2D._32 := AMatrix.m32;
  ABrush.SetTransform(LTransformD2D);
end;

function TCanvasD2D.CreateD2DGradientBrush(AGradient: TGradient; const ARect: TRectF;
  const AOpacity: Single): ID2D1Brush;
var
  Count: Integer;
  Grad: array of TD2D1GradientStop;
  GradCol: ID2D1GradientStopCollection;
  GradBrushProp: TD2D1LinearGradientBrushProperties;
  RadialGradBrushProp: TD2D1RadialGradientBrushProperties;
  I: Integer;
begin
  if AGradient.Points.Count > 1 then
  begin
    Count := 0;
    // + 2 - two boundary points of a gradient
    SetLength(Grad, AGradient.Points.Count + 2);
    if AGradient.Points[0].Offset > 0 then
    begin
      Grad[Count].Color := D2Color(MakeColor(AGradient.Points[0].IntColor, AOpacity), 1);
      Grad[Count].Position := 0;
      Inc(Count);
    end;
    for I := 0 to AGradient.Points.Count - 1 do
    begin
      Grad[I + Count].Color := D2Color(MakeColor(AGradient.Points[I].IntColor, AOpacity), 1);
      Grad[I + Count].Position := AGradient.Points[I].Offset;
    end;
    if AGradient.Points[AGradient.Points.Count - 1].Offset < 1 then
    begin
      Inc(Count);
      Grad[AGradient.Points.Count + Count - 1].Color :=
        D2Color(MakeColor(AGradient.Points[AGradient.Points.Count - 1].IntColor, AOpacity), 1);
      Grad[AGradient.Points.Count + Count - 1].Position := 1;
    end;

    if AGradient.Style = TGradientStyle.Linear then
    begin
      { Linear }
      FTarget.CreateGradientStopCollection(@Grad[0], AGradient.Points.Count + Count, D2D1_GAMMA_2_2,
        D2D1_EXTEND_MODE_CLAMP, GradCol);
      GradBrushProp.StartPoint := D2Point(ARect.Left + AGradient.StartPosition.X * ARect.Width,
        ARect.Top + AGradient.StartPosition.Y * ARect.Height);
      GradBrushProp.EndPoint := D2Point(ARect.Left + AGradient.StopPosition.X * ARect.Width,
        ARect.Top + AGradient.StopPosition.Y * ARect.Height);
      FTarget.CreateLinearGradientBrush(GradBrushProp, nil, GradCol, ID2D1LinearGradientBrush(Result));
      GradCol := nil;
    end
    else
    begin
      { Radial }
      for I := 0 to AGradient.Points.Count + Count - 1 do
        Grad[I].Position := 1 - Grad[I].Position;
      FTarget.CreateGradientStopCollection(@Grad[0], AGradient.Points.Count + Count, D2D1_GAMMA_2_2,
        D2D1_EXTEND_MODE_CLAMP, GradCol);
      RadialGradBrushProp.GradientOriginOffset := TD2D1Point2F(TPointF.Create(0, 0));
      RadialGradBrushProp.Center := TD2D1Point2F(TPointF.Create(ARect.Width * 0.5, ARect.Height * 0.5));
      RadialGradBrushProp.RadiusX := ARect.Width / 2;
      RadialGradBrushProp.RadiusY := ARect.Height / 2;
      FTarget.CreateRadialGradientBrush(RadialGradBrushProp, nil, GradCol, ID2D1RadialGradientBrush(Result));
      UpdateBrushMatrix(Result, AGradient.RadialTransform.Matrix);
      GradCol := nil;
    end;
  end
  else
    FTarget.CreateSolidColorBrush(D2Color(0, 0), nil, ID2D1SolidColorBrush(Result));
end;

function TCanvasD2D.CreateD2DBitmapBrush(ABitmap: TBitmap; const AWrapMode: TWrapMode; const ARect: TRectF;
  const AOpacity: Single): ID2D1Brush;
var
  BitmapBrushProp: TD2D1BitmapBrushProperties;
  BrushProp: TD2D1BrushProperties;
  D2DBitmap: ID2D1Bitmap;
  M: TMatrix;
begin
  if (ABitmap <> nil) and (ABitmap.Width > 0) and (ABitmap.Height > 0) then
  begin
    BitmapBrushProp.InterpolationMode := D2D1_BITMAP_INTERPOLATION_MODE_LINEAR;
    BrushProp.Opacity := AOpacity;
    M := TMatrix.Identity;
    case AWrapMode of
      TWrapMode.Tile:
        begin
          BitmapBrushProp.ExtendModeX := D2D1_EXTEND_MODE_WRAP;
          BitmapBrushProp.ExtendModeY := D2D1_EXTEND_MODE_WRAP;
        end;
      TWrapMode.TileOriginal:
        begin
          BitmapBrushProp.ExtendModeX := D2D1_EXTEND_MODE_CLAMP;
          BitmapBrushProp.ExtendModeY := D2D1_EXTEND_MODE_CLAMP;
        end;
      TWrapMode.TileStretch:
        begin
          BitmapBrushProp.ExtendModeX := D2D1_EXTEND_MODE_WRAP;
          BitmapBrushProp.ExtendModeY := D2D1_EXTEND_MODE_WRAP;
          if Stroke.Kind = TBrushKind.None then
          begin
            M.m11 := ARect.Width / ABitmap.Width;
            M.m22 := ARect.Height / ABitmap.Height;
          end
          else
          begin
            M.m11 := (ARect.Width + (Stroke.Thickness / 2)) / ABitmap.Width;
            M.m22 := (ARect.Height + (Stroke.Thickness / 2)) / ABitmap.Height;
          end;
          M.m31 := ARect.Left;
          M.m32 := ARect.Top;
        end;
    end;
    BrushProp.Transform := D2Matrix(M);
    D2DBitmap := TD2DBitmapHandle(ABitmap.Handle).CreateBitmap(FTarget);
    FTarget.CreateBitmapBrush(D2DBitmap, @BitmapBrushProp, @BrushProp, ID2D1BitmapBrush(Result));
  end
  else
    FTarget.CreateSolidColorBrush(D2Color(0, 0), nil, ID2D1SolidColorBrush(Result));
end;

procedure TCanvasD2D.Clear(const Color: TAlphaColor);
begin
  if FTarget <> nil then
    FTarget.Clear(D2Color(Color, 1));
end;

procedure TCanvasD2D.ClearRect(const ARect: TRectF; const AColor: TAlphaColor);
begin
  FTarget.PushAxisAlignedClip(D2Rect(ARect), D2D1_ANTIALIAS_MODE_ALIASED);
  try
    FTarget.Clear(D2Color(AColor, 1));
  finally
    FTarget.PopAxisAlignedClip;
  end;
end;

//https://quality.embarcadero.com/browse/RSP-19673
//procedure TCanvasD2D.ContextLostHandler(const Sender: TObject; const Msg: TMessage);
//begin
//  DisposeResources;
//end;

function TCanvasD2D.DoBeginScene(const AClipRects: PClipRects; AContextHandle: THandle): Boolean;
begin
  Tmonitor.Enter(fLock); // https://quality.embarcadero.com/browse/RSP-19673
  try

    if SharedDevice.GetDeviceRemovedReason <> S_OK then
    begin
      HandleDeviceRemoved;
      Exit(False);
    end;

    CreateResources;
    Result := inherited DoBeginScene(AClipRects) and (FTarget <> nil);
    if Result then
    begin
      FCurrentSaveState := nil;
      FContextHandle := AContextHandle;
      if Result then
      begin
        FTarget.BeginDraw;
        if AClipRects <> nil then
          SetClipRects(AClipRects^);
      end;
    end;

  finally
    Tmonitor.Exit(fLock); // https://quality.embarcadero.com/browse/RSP-19673
  end;
end;

procedure TCanvasD2D.DoEndScene;
var
  T1, T2: TD2D1Tag;
  Mapped: D3D10_MAPPED_TEXTURE2D;
  Res: HResult;
  I: Integer;
begin
  Tmonitor.Enter(fLock); // https://quality.embarcadero.com/browse/RSP-19673
  try

    if FTarget <> nil then
    begin
      if FLayer <> nil then
      begin
        FTarget.PopLayer;
        FLayer := nil;
      end;
      FTarget.Flush(@T1, @T2);
      Res := FTarget.EndDraw;
      if Res = D2DERR_RECREATE_TARGET then
      begin
        HandleDeviceRemoved;
        Exit;
      end;
      if (BeginSceneCount = 1) and (FSwapChain <> nil) then
        FSwapChain.Present(0, 0);
      if FBufferTexture <> nil then
      begin
        SharedDevice.CopyResource(FCopyBuffer, FBufferTexture);
        if Succeeded(FCopyBuffer.Map(0, D3D10_MAP_READ, 0, Mapped)) then
        try
          if Mapped.RowPitch <> Cardinal(FBufferSize.Width * 4) then
          begin
            for I := 0 to FBufferSize.Height - 1 do
              Move(PAlphaColorArray(Mapped.pData)[Cardinal(I) * (Mapped.RowPitch div 4)],
                PAlphaColorArray(WindowHandleToPlatform(Parent).BufferBits)[I * FBufferSize.Width], FBufferSize.Width * 4);
          end
          else
            Move(Mapped.pData^, WindowHandleToPlatform(Parent).BufferBits^, FBufferSize.Width * FBufferSize.Height * 4);
        finally
          FCopyBuffer.Unmap(0);
        end;

        // in design-time just draw buffer
        if (WindowHandleToPlatform(Parent).Form <> nil) and
          (csDesigning in WindowHandleToPlatform(Parent).Form.ComponentState) then
          Winapi.Windows.BitBlt(FContextHandle, 0, 0, Width, Height, WindowHandleToPlatform(Parent).BufferHandle, 0, 0,
            SRCCOPY);
      end;
    end;
    inherited;

  finally
    Tmonitor.Exit(fLock); // https://quality.embarcadero.com/browse/RSP-19673
  end;
end;

procedure TCanvasD2D.HandleDeviceRemoved;
begin
  // TMessageManager.DefaultManager.SendMessage(nil, TContextLostMessage.Create, False); => https://quality.embarcadero.com/browse/RSP-19673
  DestroySharedResources;
end;

procedure TCanvasD2D.DoFlush;
begin
  if FTarget <> nil then
    FTarget.Flush(nil, nil);
end;

procedure TCanvasD2D.DoSetMatrix(const M: TMatrix);
begin
  if FTarget <> nil then
    FTarget.SetTransform(D2Matrix(M * TMatrix.CreateScaling(Scale, Scale)));
end;

procedure TCanvasD2D.SetSize(const AWidth, AHeight: Integer);
begin
  if SharedDevice.GetDeviceRemovedReason <> S_OK then
  begin
    if (Parent <> nil) and ((AWidth <> Width) or (AHeight <> Height)) then
      inherited;
    HandleDeviceRemoved;
    Exit;
  end;

  if (Parent <> nil) and ((AWidth <> Width) or (AHeight <> Height)) then
  begin
    FTarget := nil;
    FRenderTargetView := nil;
    FBufferTexture := nil;
    FCopyBuffer := nil;
    FSwapChain := nil;
  end;
  inherited;
end;

procedure TCanvasD2D.SetClipRects(const ARects: array of TRectF);
var
  I: Integer;
  Geoms: array of ID2D1Geometry;
  FClipGeom: ID2D1GeometryGroup;
  LayerPar: TD2D1LayerParameters;
  R: TRectF;
begin
  if Length(ARects) > 0 then
  begin
    if (Length(ARects) = 1) and (ARects[0] = TRectF.Create(0, 0, Width, Height)) then
      Exit;

    FTarget.SetTransform(D2Matrix(TMatrix.CreateScaling(Scale, Scale)));
    // First clear
    for I := 0 to High(ARects) do
    begin
      R := ARects[I];
      if not IsScaleInteger then
        R := AlignToPixel(R);
      FTarget.PushAxisAlignedClip(D2Rect(R), D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
      FTarget.Clear(D2Color(0, 0));
      FTarget.PopAxisAlignedClip;
    end;
    FClipGeom := nil;
    // Second create clipregion
    SetLength(Geoms, Length(ARects));
    for I := 0 to High(ARects) do
    begin
      R := ARects[I];
      if not IsScaleInteger then
        R := AlignToPixel(R);
      SharedFactory.CreateRectangleGeometry(D2Rect(R), ID2D1RectangleGeometry(Geoms[I]));
    end;
    SharedFactory.CreateGeometryGroup(D2D1_FILL_MODE_WINDING, @Geoms[0], Length(Geoms), FClipGeom);
    for I := 0 to High(ARects) do
      Geoms[I] := nil;
    // Apply clips
    FTarget.CreateLayer(nil, FLayer);
    LayerPar.ContentBounds := D2Rect(RectF(0, 0, FWidth, FHeight));
    LayerPar.GeometricMask := FClipGeom;
    LayerPar.MaskAntialiasMode := D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
    LayerPar.MaskTransform := D2Matrix(TMatrix.Identity);
    LayerPar.Opacity := 1;
    LayerPar.OpacityBrush := nil;
    LayerPar.LayerOptions := D2D1_LAYER_OPTIONS_NONE;
    FTarget.PushLayer(LayerPar, FLayer);
    FClipGeom := nil;
  end;
end;

procedure TCanvasD2D.IntersectClipRect(const ARect: TRectF);
var
  Geom: ID2D1RectangleGeometry;
  LayerPar: TD2D1LayerParameters;
  R: TRectF;
begin
  FClippingChangeCount := FClippingChangeCount + 1;
  if FCurrentSaveState <> nil then
  begin
    R := ARect;
    if not SameValue(Frac(Scale), 0, TEpsilon.Scale) then
      R := AlignToPixel(ARect);

    OleCheck(SharedFactory.CreateRectangleGeometry(D2Rect(R), Geom));
    if Geom <> nil then
    begin
      FCurrentSaveState.CreateLayer(FTarget);
      LayerPar.ContentBounds := D2Rect(RectF(-MaxSingle, -MaxSingle, MaxSingle, MaxSingle));
      LayerPar.GeometricMask := Geom;
      LayerPar.MaskAntialiasMode := D2D1_ANTIALIAS_MODE_ALIASED;
      LayerPar.MaskTransform := D2Matrix(TMatrix.Identity);
      LayerPar.Opacity := 1;
      LayerPar.OpacityBrush := nil;
      LayerPar.LayerOptions := D2D1_LAYER_OPTIONS_NONE;
      FTarget.PushLayer(LayerPar, FCurrentSaveState.Layer);
      Geom := nil;
    end;
  end;
end;

procedure TCanvasD2D.ExcludeClipRect(const ARect: TRectF);
var
  i: Integer;
  Geoms: array of ID2D1RectangleGeometry;
  R: TRectF;
  RR: array of TRectF;
  GeomGroup: ID2D1GeometryGroup;
  LayerPar: TD2D1LayerParameters;
begin
  FClippingChangeCount := FClippingChangeCount + 1;
  if FCurrentSaveState <> nil then
  begin
    SetLength(Geoms, 4);
    R := ARect;
    SetLength(RR, 4);
    RR[0] := RectF(-FWidth, -FWidth, R.Left, FHeight);
    RR[1] := RectF(R.Right, -FHeight, FWidth, FHeight);
    RR[2] := RectF(R.Left, -FHeight, R.Right, R.Top);
    RR[3] := RectF(R.Left, R.Bottom, R.Right, FHeight);
    for i := 0 to High(RR) do
      OleCheck(SharedFactory.CreateRectangleGeometry(D2Rect(RR[i]), Geoms[i]));
    OleCheck(SharedFactory.CreateGeometryGroup(D2D1_FILL_MODE_WINDING, @Geoms[0], Length(Geoms), GeomGroup));
    if GeomGroup <> nil then
    begin
      FCurrentSaveState.CreateLayer(FTarget);
      LayerPar.ContentBounds := D2Rect(RectF(-FWidth, -FHeight, 400000, 400000));
      LayerPar.GeometricMask := GeomGroup;
      LayerPar.MaskAntialiasMode := D2D1_ANTIALIAS_MODE_ALIASED;
      LayerPar.MaskTransform := D2Matrix(TMatrix.Identity);
      LayerPar.Opacity := 1;
      LayerPar.OpacityBrush := nil;
      LayerPar.LayerOptions := D2D1_LAYER_OPTIONS_NONE;
      FTarget.PushLayer(LayerPar, FCurrentSaveState.Layer);
    end;
  end;
end;

procedure TCanvasD2D.ApplyFill(const ABrush: TBrush; ARect: TRectF; const AOpacity: Single);
var
  BrushMatrix: TMatrix;
begin
  if (ABrush.Kind = TBrushKind.Resource) and (ABrush.Resource <> nil) and (ABrush.Resource.Brush <> nil) then
    ABrush.Assign(ABrush.Resource.Brush);

  if FMetaBrush = nil then
    FMetaBrush := TMetaBrush.Create;

  if FMetaBrush.Valid and ((FMetaBrush.Kind <> ABrush.Kind) or (not SameValue(FMetaBrush.Opacity, AOpacity,
    TEpsilon.Scale))) then
    FMetaBrush.Valid := False;

  case ABrush.Kind of
    TBrushKind.Solid:
      if (not FMetaBrush.Valid) or (FMetaBrush.Color <> ABrush.Color) then
      begin
        FTarget.CreateSolidColorBrush(D2Color(ABrush.Color, AOpacity), nil, ID2D1SolidColorBrush(FBrush));
        FMetaBrush.Color := ABrush.Color;
      end;

    TBrushKind.Gradient:
      if (FMetaBrush.Gradient.Style <> ABrush.Gradient.Style) or (not FMetaBrush.Valid) or (FMetaBrush.Rect <> ARect) or
        (not FMetaBrush.Gradient.Equal(ABrush.Gradient)) then
      begin
        FBrush := CreateD2DGradientBrush(ABrush.Gradient, ARect, AOpacity);
        FMetaBrush.Rect := ARect;
        FMetaBrush.Gradient.Assign(ABrush.Gradient);
      end
      else
        if FBrush <> nil then
        begin
          BrushMatrix := ABrush.Gradient.RadialTransform.Matrix;
          if not CompareMem(@BrushMatrix, @FLastBrushTransform, SizeOf(TMatrix)) then
          begin
            FLastBrushTransform := BrushMatrix;
            UpdateBrushMatrix(FBrush, BrushMatrix);
          end;
        end;

    TBrushKind.Bitmap:
      if (not FMetaBrush.Valid) or (FMetaBrush.Image <> ABrush.Bitmap.Image) or (FMetaBrush.Rect <> ARect) or
        (FMetaBrush.WrapMode <> ABrush.Bitmap.WrapMode) then
      begin
        FBrush := CreateD2DBitmapBrush(ABrush.Bitmap.Bitmap, ABrush.Bitmap.WrapMode, ARect, AOpacity);
        FMetaBrush.Image := ABrush.Bitmap.Image;
        FMetaBrush.WrapMode := ABrush.Bitmap.WrapMode;
        FMetaBrush.Rect := ARect;
      end;

  else
    FTarget.CreateSolidColorBrush(D2Color(0, 0), nil, ID2D1SolidColorBrush(FBrush));
  end;

  FMetaBrush.Kind := ABrush.Kind;
  FMetaBrush.Opacity := AOpacity;
  FMetaBrush.Valid := True;
end;

procedure TCanvasD2D.ApplyStroke(const AStroke: TStrokeBrush; ARect: TRectF; const AOpacity: Single);
var
  StyleProp: TD2D1StrokeStyleProperties;
begin
  if (AStroke.Kind = TBrushKind.Resource) and (AStroke.Resource <> nil) and (AStroke.Resource.Brush <> nil) then
    AStroke.Assign(AStroke.Resource.Brush);

  if FMetaStroke = nil then
    FMetaStroke := TMetaBrush.Create;

  if FMetaStroke.Valid and ((FMetaStroke.Kind <> AStroke.Kind) or
    (not SameValue(FMetaStroke.Opacity, AOpacity, TEpsilon.Scale))) then
    FMetaStroke.Valid := False;

  case AStroke.Kind of
    TBrushKind.Solid:
      if (not FMetaStroke.Valid) or (FMetaStroke.Color <> AStroke.Color) then
      begin
        FTarget.CreateSolidColorBrush(D2Color(AStroke.Color, AOpacity), nil, ID2D1SolidColorBrush(FStrokeBrush));
        FMetaStroke.Color := AStroke.Color;
      end;

    TBrushKind.Bitmap:
      if (not FMetaStroke.Valid) or (FMetaStroke.Image <> AStroke.Bitmap.Image) or (FMetaStroke.Rect <> ARect) or
        (FMetaStroke.WrapMode <> AStroke.Bitmap.WrapMode) then
      begin
        FStrokeBrush := CreateD2DBitmapBrush(AStroke.Bitmap.Bitmap, AStroke.Bitmap.WrapMode, ARect, AOpacity);
        FMetaStroke.Image := AStroke.Bitmap.Image;
        FMetaStroke.WrapMode := AStroke.Bitmap.WrapMode;
        FMetaStroke.Rect := ARect;
      end;

    TBrushKind.Gradient:
      if (not FMetaStroke.Valid) or (FMetaStroke.Rect <> ARect) or
        (not FMetaStroke.Gradient.Equal(AStroke.Gradient)) then
      begin
        FStrokeBrush := CreateD2DGradientBrush(AStroke.Gradient, ARect, AOpacity);
        FMetaStroke.Rect := ARect;
        FMetaStroke.Gradient.Assign(AStroke.Gradient);
      end;

  else
    FTarget.CreateSolidColorBrush(D2Color(0, 0), nil, ID2D1SolidColorBrush(FStrokeBrush));
  end;

  FMetaStroke.Kind := AStroke.Kind;
  FMetaStroke.Opacity := AOpacity;
  FMetaStroke.Valid := True;

  if FMetaStrokeBrush = nil then
    FMetaStrokeBrush := TMetaStrokeBrush.Create;

  if (not FMetaStrokeBrush.Valid) or (FMetaStrokeBrush.Cap <> AStroke.Cap) or
    (FMetaStrokeBrush.Join <> AStroke.Join) or (FMetaStrokeBrush.Dash <> AStroke.Dash) or
    ((AStroke.Dash = TStrokeDash.Custom) and ((FMetaStrokeBrush.DashArray <> AStroke.DashArray) or
    not SameValue(FMetaStrokeBrush.DashOffset, AStroke.DashOffset, TEpsilon.Vector))) then
  begin
    case AStroke.Cap of
      TStrokeCap.Flat:
        StyleProp.DashCap := D2D1_CAP_STYLE_SQUARE;
      TStrokeCap.Round:
        StyleProp.DashCap := D2D1_CAP_STYLE_ROUND;
    end;

    StyleProp.StartCap := StyleProp.DashCap;
    StyleProp.EndCap := StyleProp.DashCap;

    case AStroke.Join of
      TStrokeJoin.Miter:
        StyleProp.LineJoin := D2D1_LINE_JOIN_MITER;
      TStrokeJoin.Round:
        StyleProp.LineJoin := D2D1_LINE_JOIN_ROUND;
      TStrokeJoin.Bevel:
        StyleProp.LineJoin := D2D1_LINE_JOIN_BEVEL;
    end;

    StyleProp.MiterLimit := 10;
    StyleProp.DashOffset := AStroke.DashOffset;
    StyleProp.DashStyle := TD2D1DashStyle(AStroke.Dash);

    if AStroke.Dash = TStrokeDash.Custom then
      SharedFactory.CreateStrokeStyle(StyleProp, @AStroke.DashArray[0], Length(AStroke.DashArray), FStrokeStyle)
    else
      SharedFactory.CreateStrokeStyle(StyleProp, nil, 0, FStrokeStyle);

    FMetaStrokeBrush.Cap := AStroke.Cap;
    FMetaStrokeBrush.Join := AStroke.Join;
    FMetaStrokeBrush.Dash := AStroke.Dash;
    FMetaStrokeBrush.DashArray := AStroke.DashArray;
    FMetaStrokeBrush.DashOffset := AStroke.DashOffset;
    FMetaStrokeBrush.Valid := True;
  end;
end;

procedure TCanvasD2D.FontChanged(Sender: TObject);
begin
end;

class function TCanvasD2D.GetAttribute(const Value: TCanvasAttribute): Integer;
const
  DefaultMaxTextureSize = 8192; // According DirectX 10 specification.
begin
  case Value of
    TCanvasAttribute.MaxBitmapSize:
      Result := DefaultMaxTextureSize;
  else
    Result := inherited;
  end;
end;

procedure TCanvasD2D.DoDrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  if FTarget <> nil then
  begin
    ApplyStroke(ABrush, TRectF.Create(APt1.X, APt1.y, APt2.X, APt2.y), AOpacity);
    FTarget.DrawLine(D2Point(APt1.X, APt1.y), D2Point(APt2.X, APt2.y), FStrokeBrush, ABrush.Thickness, FStrokeStyle);
  end;
end;

procedure TCanvasD2D.DoDrawRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  if FTarget <> nil then
  begin
    ApplyStroke(ABrush, ARect, AOpacity);
    FTarget.DrawRectangle(D2Rect(ARect), FStrokeBrush, ABrush.Thickness, FStrokeStyle);
  end;
end;

procedure TCanvasD2D.DoFillRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush);
begin
  if FTarget <> nil then
  begin
    ApplyFill(ABrush, ARect, AOpacity);
    FTarget.FillRectangle(D2Rect(ARect), FBrush);
  end;
end;

procedure TCanvasD2D.DoDrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  if FTarget <> nil then
  begin
    ApplyStroke(ABrush, ARect, AOpacity);
    FTarget.DrawEllipse(D2Ellipse(ARect), FStrokeBrush, ABrush.Thickness, FStrokeStyle);
  end;
end;

procedure TCanvasD2D.DoFillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush);
begin
  if FTarget <> nil then
  begin
    ApplyFill(ABrush, ARect, AOpacity);
    FTarget.FillEllipse(D2Ellipse(ARect), FBrush);
  end;
end;

procedure TCanvasD2D.MeasureLines(const ALines: TLineMetricInfo; const ARect: TRectF; const AText: string;
  const WordWrap: Boolean; const Flags: TFillTextFlags; const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign);
var
  TextRange: TDWriteTextRange;
  TextLayout: IDWriteTextLayout;
  TextFormat: IDWriteTextFormat;
  RWidth, RHeight: Single;
  WS: string;
  P, I, LineCount: Cardinal;
  LineMetrics: array of DWRITE_LINE_METRICS;
begin
  if Length(AText) = 0 then
    Exit;

  if not WordWrap then
    RWidth := 0
  else
    RWidth := ARect.Width;
  RHeight := ARect.Height;

  WS := FFont.Family;
  SharedDWriteFactory.CreateTextFormat(PChar(WS), nil, FontWeightToDWrite(FFont.StyleExt.Weight),
    FontSlantToDWrite(FFont.StyleExt.Slant), FontStretchToDWrite(FFont.StyleExt.Stretch), FFont.Size,
	PChar(FLocaleName), TextFormat);
  if TFillTextFlag.RightToLeft in Flags then
    TextFormat.SetReadingDirection(DWRITE_READING_DIRECTION_RIGHT_TO_LEFT);

  SharedDWriteFactory.CreateTextLayout(PChar(AText), Length(AText), TextFormat, RWidth, RHeight, TextLayout);

  TextRange.StartPosition := 0;
  TextRange.Length := Length(AText);

  if not WordWrap then
    TextLayout.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP)
  else
    TextLayout.SetWordWrapping(DWRITE_WORD_WRAPPING_WRAP);

  if TFontStyle.fsStrikeOut in FFont.StyleExt.SimpleStyle then
    TextLayout.SetStrikethrough(True, TextRange);

  if TFontStyle.fsUnderline in FFont.StyleExt.SimpleStyle then
    TextLayout.SetUnderline(True, TextRange);

  case AVTextAlign of
    TTextAlign.Center:
      TextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_CENTER);
    TTextAlign.Leading:
      TextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR);
    TTextAlign.Trailing:
      TextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_FAR);
  end;
  case ATextAlign of
    TTextAlign.Center:
      TextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_CENTER);
    TTextAlign.Leading:
      TextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);
    TTextAlign.Trailing:
      TextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_TRAILING);
  end;

  TextLayout.GetLineMetrics(nil, 0, LineCount);
  if LineCount > 0 then
  begin
    SetLength(LineMetrics, LineCount);
    TextLayout.GetLineMetrics(@(LineMetrics[0]), LineCount, LineCount);

    ALines.Count := LineCount;
    P := 1;
    for I := 0 to LineCount - 1 do
    begin
      ALines.Metrics[I].Index := P;
      P := P + LineMetrics[I].length;
    end;
  end;
  TextFormat := nil;
  TextLayout := nil;
end;

{ Shared }

class procedure TCanvasD2D.CreateSharedResources;
var
  Prop: TD2D1RenderTargetProperties;
  Desc: TD3D10_Texture2DDesc;
begin
  inherited;
  if FSharedRenderTarget = nil then
  begin
    FillChar(Desc, SizeOf(D3D10_TEXTURE2D_DESC), 0);
    Desc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
    Desc.Width := 1;
    Desc.Height := 1;
    Desc.MipLevels := 1;
    Desc.ArraySize := 1;
    Desc.SampleDesc.Count := 1;
    Desc.SampleDesc.Quality := 0;
    Desc.Usage := D3D10_USAGE_DEFAULT;
    Desc.BindFlags := D3D10_BIND_RENDER_TARGET or D3D10_BIND_SHADER_RESOURCE;

    if Failed(SharedDevice.CreateTexture2D(Desc, nil, FSharedTexture)) then
      raise ECannotCreateTexture.CreateFmt(SCannotCreateTexture, [ClassName]);

    Prop := D2D1RenderTargetProperties(DefaultRenderTargetMode, D2D1PixelFormat(DXGI_FORMAT_UNKNOWN,
      D2D1_ALPHA_MODE_PREMULTIPLIED));

    if Failed(SharedFactory.CreateDxgiSurfaceRenderTarget(FSharedTexture as IDXGISurface, Prop,
      FSharedRenderTarget)) then
      raise ECannotCreateRenderTarget.CreateFmt(SCannotCreateRenderTarget, [ClassName]);
  end;
end;

class procedure TCanvasD2D.DestroySharedResources;
begin
  FSharedRenderTarget := nil;
  FSharedTexture := nil;
  inherited;
end;

class function TCanvasD2D.SharedRenderTarget: ID2D1RenderTarget;
begin
  CreateSharedResources;
  Result := FSharedRenderTarget;
end;

{ Bitmaps }

class function TCanvasD2D.DoInitializeBitmap(const Width, Height: Integer; const Scale: Single;
  var PixelFormat: TPixelFormat): THandle;
var
  LHandle: TD2DBitmapHandle;
begin
  CreateSharedResources;
  if FSharedRenderTarget <> nil then
  begin
    PixelFormat := TPixelFormat.BGRA;
    LHandle := TD2DBitmapHandle.Create(Width, Height, TMapAccess.Read);
  end
  else
    LHandle := nil;
  Result := THandle(LHandle);
end;

class procedure TCanvasD2D.DoFinalizeBitmap(var Bitmap: THandle);
var
  LHandle: TD2DBitmapHandle;
begin
  LHandle := TD2DBitmapHandle(Bitmap);
  Bitmap := THandle(nil);
  LHandle.Free;
end;

class function TCanvasD2D.DoMapBitmap(const Bitmap: THandle; const Access: TMapAccess; var Data: TBitmapData): Boolean;
var
  H: TD2DBitmapHandle;
  Desc: TD3D10_Texture2DDesc;
  Mapped: D3D10_MAPPED_TEXTURE2D;
  Flags: TD3D10_Map;
begin
  Tmonitor.Enter(fLock); // https://quality.embarcadero.com/browse/RSP-19673
  try

    Result := False;
    H := TD2DBitmapHandle(Bitmap);
    H.FAccess := Access;
    if H.FMapBuffer = nil then
    begin
      FillChar(Desc, SizeOf(D3D10_TEXTURE2D_DESC), 0);
      Desc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
      Desc.Width := H.FWidth;
      Desc.Height := H.FHeight;
      Desc.MipLevels := 1;
      Desc.ArraySize := 1;
      Desc.SampleDesc.Count := 1;
      Desc.SampleDesc.Quality := 0;
      Desc.CPUAccessFlags := D3D10_CPU_ACCESS_READ or D3D10_CPU_ACCESS_WRITE;
      Desc.Usage := D3D10_USAGE_STAGING;
      Desc.BindFlags := 0;

      if Failed(SharedDevice.CreateTexture2D(Desc, nil, H.FMapBuffer)) then
        raise ECannotCreateTexture.CreateFmt(SCannotCreateTexture, [ClassName]);
    end;
    case Access of
      TMapAccess.Read:
        begin
          Flags := D3D10_MAP_READ;
          SharedDevice.CopyResource(H.FMapBuffer, H.Texture);
        end;
      TMapAccess.Write:
        Flags := D3D10_MAP_WRITE;
    else
      begin
        Flags := D3D10_MAP_READ_WRITE;
        SharedDevice.CopyResource(H.FMapBuffer, H.Texture);
      end;
    end;
    if Succeeded(H.FMapBuffer.Map(0, Flags, 0, Mapped)) then
    begin
      Data.Data := Mapped.pData;
      Data.Pitch := Mapped.RowPitch;
      Result := True;
    end;

  finally
    Tmonitor.exit(fLock); // https://quality.embarcadero.com/browse/RSP-19673
  end;
end;

class procedure TCanvasD2D.DoUnmapBitmap(const Bitmap: THandle; var Data: TBitmapData);
var
  H: TD2DBitmapHandle;
begin
  Tmonitor.Enter(fLock); // https://quality.embarcadero.com/browse/RSP-19673
  try

    H := TD2DBitmapHandle(Bitmap);
    H.FMapBuffer.Unmap(0);
    if H.FAccess in [TMapAccess.ReadWrite, TMapAccess.Write] then
      SharedDevice.CopyResource(H.Texture, H.FMapBuffer);

  finally
    Tmonitor.exit(fLock); // https://quality.embarcadero.com/browse/RSP-19673
  end;
end;

procedure TCanvasD2D.DoDrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single;
  const HighSpeed: Boolean);
var
  SR, DR: TD2D1RectF;
  IntMode: TD2D1BitmapInterpolationMode;
  B: ID2D1Bitmap;
begin
  if FTarget <> nil then
  begin
    SR := D2Rect(SrcRect);
    DR := D2Rect(DstRect);

    if ABitmap.HandleAllocated then
    begin
      if HighSpeed then
        IntMode := D2D1_BITMAP_INTERPOLATION_MODE_NEAREST_NEIGHBOR
      else
        IntMode := D2D1_BITMAP_INTERPOLATION_MODE_LINEAR;

      B := TD2DBitmapHandle(ABitmap.Handle).CreateBitmap(FTarget);
      if B <> nil then
        FTarget.DrawBitmap(B, @DR, AOpacity, IntMode, @SR);
    end;
  end;
end;

{ Path }

procedure TCanvasD2D.DoDrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush);
var
  I: Integer;
  CP1, CP2: TPointF;
  Geometry: ID2D1PathGeometry;
  Path: ID2D1GeometrySink;
begin
  if FTarget <> nil then
  begin
    ApplyStroke(ABrush, APath.GetBounds, AOpacity);

    SharedFactory.CreatePathGeometry(Geometry);
    Geometry.Open(Path);
    I := 0;
    while I < APath.Count do
    begin
      case APath[I].Kind of
        TPathPointKind.MoveTo:
          begin
            if (I > 0) and (APath[I - 1].Kind <> TPathPointKind.Close) then
              Path.EndFigure(D2D1_FIGURE_END_OPEN);
            Path.BeginFigure(D2Point(APath[I].Point.X, APath[I].Point.Y), D2D1_FIGURE_BEGIN_FILLED);
          end;
        TPathPointKind.LineTo:
          Path.AddLine(D2Point(APath[I].Point.X, APath[I].Point.Y));
        TPathPointKind.CurveTo:
          begin
            CP1 := APath[I].Point;
            Inc(I);
            CP2 := APath[I].Point;
            Inc(I);
            Path.AddBezier(D2Bezier(CP1.X, CP1.y, CP2.X, CP2.y, APath[I].Point.X, APath[I].Point.Y));
          end;
        TPathPointKind.Close:
          Path.EndFigure(D2D1_FIGURE_END_CLOSED);
      end;
      Inc(I);
    end;
    if (APath.Count < 1) or (APath[APath.Count - 1].Kind <> TPathPointKind.Close) then
      Path.EndFigure(D2D1_FIGURE_END_OPEN);
    Path.Close;
    FTarget.DrawGeometry(Geometry, FStrokeBrush, ABrush.Thickness, FStrokeStyle);
  end;
end;

procedure TCanvasD2D.DoFillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush);
var
  I: Integer;
  CP1, CP2: TPointF;
  Geometry: ID2D1PathGeometry;
  Path: ID2D1GeometrySink;
begin
  SharedFactory.CreatePathGeometry(Geometry);
  Geometry.Open(Path);
  I := 0;
  while I < APath.Count do
  begin
    case APath[I].Kind of
      TPathPointKind.MoveTo:
        begin
          if (I > 0) and (APath[I - 1].Kind <> TPathPointKind.Close) then
            Path.EndFigure(D2D1_FIGURE_END_OPEN);
          Path.BeginFigure(D2Point(APath[I].Point.X, APath[I].Point.Y), D2D1_FIGURE_BEGIN_FILLED);
        end;
      TPathPointKind.LineTo:
        Path.AddLine(D2Point(APath[I].Point.X, APath[I].Point.Y));
      TPathPointKind.CurveTo:
        begin
          CP1 := APath[I].Point;
          Inc(I);
          CP2 := APath[I].Point;
          Inc(I);
          Path.AddBezier(D2Bezier(CP1.X, CP1.Y, CP2.X, CP2.y, APath[I].Point.X, APath[I].Point.Y));
        end;
      TPathPointKind.Close:
        Path.EndFigure(D2D1_FIGURE_END_CLOSED);
    end;
    Inc(I);
  end;
  if (APath.Count < 1) or (APath[APath.Count - 1].Kind <> TPathPointKind.Close) then
    Path.EndFigure(D2D1_FIGURE_END_OPEN);
  Path.Close;
  ApplyFill(ABrush, APath.GetBounds, AOpacity);
  FTarget.FillGeometry(Geometry, FBrush, nil);
end;

function TCanvasD2D.PtInPath(const APoint: TPointF; const APath: TPathData): Boolean;
var
  I: Integer;
  B: TRectF;
  CP1, CP2: TPointF;
  Geometry: ID2D1PathGeometry;
  Path: ID2D1GeometrySink;
  Cont: LongBool;
  Closed: Boolean;
begin
  Result := False;
  B := APath.GetBounds;
  if not B.Contains(APoint) then
    Result := False
  else
  begin
    if APath.IsEmpty then
      Exit;
    SharedFactory.CreatePathGeometry(Geometry);
    Geometry.Open(Path);
    I := 0;
    Closed := False;
    while I < APath.Count do
    begin
      case APath[I].Kind of
        TPathPointKind.MoveTo:
          begin
            if (I > 0) and (APath[I - 1].Kind <> TPathPointKind.Close) then
              Path.EndFigure(D2D1_FIGURE_END_OPEN);
            Path.BeginFigure(D2Point(APath[I].Point.X, APath[I].Point.Y), D2D1_FIGURE_BEGIN_FILLED);
          end;
        TPathPointKind.LineTo:
          Path.AddLine(D2Point(APath[I].Point.X, APath[I].Point.Y));
        TPathPointKind.CurveTo:
          begin
            CP1 := APath[I].Point;
            Inc(I);
            CP2 := APath[I].Point;
            Inc(I);
            Path.AddBezier(D2Bezier(CP1.X, CP1.y, CP2.X, CP2.y, APath[I].Point.X, APath[I].Point.Y));
          end;
        TPathPointKind.Close:
          begin
            Path.EndFigure(D2D1_FIGURE_END_CLOSED);
            Closed := True;
          end;
      end;
      Inc(I);
    end;
    if not Closed then
      Path.EndFigure(D2D1_FIGURE_END_OPEN);
    Path.Close;
    Geometry.FillContainsPoint(D2Point(APoint.X, APoint.Y), TD2DMatrix3x2F.Identity, 1, Cont);
    Result := Cont;
  end;
end;

{ TTextRendering }

type
  TSink = class(TInterfacedPersistent, ID2D1SimplifiedGeometrySink)
  private
    Path: TPathData;
    procedure SetFillMode(FillMode: TD2D1FillMode); stdcall;
    procedure SetSegmentFlags(VertexFlags: TD2D1PathSegment); stdcall;
    procedure BeginFigure(StartPoint: TD2D1Point2F; FigureBegin: TD2D1FigureBegin); stdcall;
    procedure AddLines(Points: PD2D1Point2F;
      (* __in_ecount(pointsCount) *) PointsCount: LongWord); stdcall;
    procedure AddBeziers(Beziers: PD2D1BezierSegment;
      (* __in_ecount(beziersCount) *) BeziersCount: LongWord); stdcall;
    procedure EndFigure(FigureEnd: D2D1_FIGURE_END); stdcall;
    function Close: HResult; stdcall;
  public
    ShiftX: Single;
    ShiftY: Single;
  end;

{ TSink }

procedure TSink.BeginFigure(StartPoint: TD2D1Point2F; FigureBegin: TD2D1FigureBegin);
begin
  Path.MoveTo(TPointF.Create(StartPoint.X + ShiftX, StartPoint.Y + ShiftY));
end;

procedure TSink.AddBeziers(Beziers: PD2D1BezierSegment; BeziersCount: LongWord);
var
  I: Integer;
begin
  for I := 0 to BeziersCount - 1 do
  begin
    Path.CurveTo(
      TPointF.Create(Beziers.point1.x + ShiftX, Beziers.point1.y + ShiftY),
      TPointF.Create(Beziers.point2.x + ShiftX, Beziers.point2.y + ShiftY),
      TPointF.Create(Beziers.point3.x + ShiftX, Beziers.point3.y + ShiftY));
    Inc(Beziers);
  end;
end;

procedure TSink.AddLines(Points: PD2D1Point2F; PointsCount: LongWord);
var
  I: Integer;
begin
  for I := 0 to PointsCount - 1 do
  begin
    Path.LineTo(TPointF.Create(Points.x + ShiftX, Points.y + ShiftY));
    Inc(Points);
  end;
end;

procedure TSink.EndFigure(FigureEnd: D2D1_FIGURE_END);
begin
  Path.ClosePath;
end;

function TSink.Close: HResult;
begin
  Result := S_OK;
end;

procedure TSink.SetFillMode(FillMode: TD2D1FillMode);
begin
end;

procedure TSink.SetSegmentFlags(VertexFlags: TD2D1PathSegment);
begin
end;

type
  TTextRendering = class(TInterfacedPersistent, IDWriteTextRenderer)
  private
    function IsPixelSnappingDisabled(clientDrawingContext: Pointer; var isDisabled: BOOL): HResult; stdcall;

    function GetCurrentTransform(clientDrawingContext: Pointer; var Transform: TDwriteMatrix): HResult; stdcall;

    function GetPixelsPerDip(clientDrawingContext: Pointer; var pixelsPerDip: Single): HResult; stdcall;
    function DrawGlyphRun(clientDrawingContext: Pointer; baselineOriginX: Single; baselineOriginY: Single;
      measuringMode: TDWriteMeasuringMode; var glyphRun: TDwriteGlyphRun;
      var glyphRunDescription: TDwriteGlyphRunDescription; const clientDrawingEffect: IUnknown): HResult; stdcall;

    function DrawUnderline(clientDrawingContext: Pointer; baselineOriginX: Single; baselineOriginY: Single;
      var underline: TDwriteUnderline; const clientDrawingEffect: IUnknown): HResult; stdcall;

    function DrawStrikethrough(clientDrawingContext: Pointer; baselineOriginX: Single; baselineOriginY: Single;
      var strikethrough: TDwriteStrikethrough; const clientDrawingEffect: IUnknown): HResult; stdcall;

    function DrawInlineObject(clientDrawingContext: Pointer; originX: Single; originY: Single;
      var inlineObject: IDWriteInlineObject; isSideways: BOOL; isRightToLeft: BOOL;
      const clientDrawingEffect: IUnknown): HResult; stdcall;
  end;

function TTextRendering.DrawGlyphRun(clientDrawingContext: Pointer; baselineOriginX: Single; baselineOriginY: Single;
  measuringMode: TDWriteMeasuringMode; var glyphRun: TDwriteGlyphRun;
  var glyphRunDescription: TDwriteGlyphRunDescription; const clientDrawingEffect: IUnknown): HResult;
var
  PathGeometry: ID2D1PathGeometry;
  Sink: ID2D1GeometrySink;
  SimSink: TSink;
  Res: HResult;
begin
  Res := TCanvasD2D.SharedFactory.CreatePathGeometry(PathGeometry);
  if Succeeded(Res) then
    Res := PathGeometry.Open(Sink);
  if Succeeded(Res) then
    Res := glyphRun.fontFace.GetGlyphRunOutline(glyphRun.fontEmSize, glyphRun.glyphIndices, glyphRun.glyphAdvances,
      glyphRun.glyphOffsets, glyphRun.glyphCount, glyphRun.isSideways, False, Sink);
  if Succeeded(Res) then
    Sink.Close;

  SimSink := TSink.Create;
  //instead SimSink.Path.Translate(baselineOriginX, baselineOriginY),if we need to add new figure to an existing path
  SimSink.ShiftX := baselineOriginX;
  SimSink.ShiftY := baselineOriginY;
  SimSink.Path := TPathData(clientDrawingContext);
  PathGeometry.Simplify(D2D1_GEOMETRY_SIMPLIFICATION_OPTION_CUBICS_AND_LINES, TD2DMatrix3x2F.Identity, 0, SimSink);
  SimSink.Close;
  SimSink.Free;
  Result := S_OK;
end;

function TTextRendering.DrawInlineObject(clientDrawingContext: Pointer; originX: Single; originY: Single;
  var inlineObject: IDWriteInlineObject; isSideways: BOOL; isRightToLeft: BOOL;
  const clientDrawingEffect: IUnknown): HResult;
begin
  Result := S_OK;
end;

function TTextRendering.DrawStrikethrough(clientDrawingContext: Pointer; baselineOriginX: Single;
  baselineOriginY: Single; var strikethrough: TDwriteStrikethrough; const clientDrawingEffect: IUnknown): HResult;
begin
  Result := S_OK;
end;

function TTextRendering.DrawUnderline(clientDrawingContext: Pointer; baselineOriginX: Single; baselineOriginY: Single;
  var underline: TDwriteUnderline; const clientDrawingEffect: IUnknown): HResult;
begin
  Result := S_OK;
end;

function TTextRendering.GetCurrentTransform(clientDrawingContext: Pointer; var Transform: TDwriteMatrix): HResult;
begin
  Transform := TDwriteMatrix(D2Matrix(TMatrix.Identity));
  Result := S_OK;
end;

function TTextRendering.GetPixelsPerDip(clientDrawingContext: Pointer; var pixelsPerDip: Single): HResult;
begin
  pixelsPerDip := 1;
  Result := S_OK;
end;

function TTextRendering.IsPixelSnappingDisabled(clientDrawingContext: Pointer; var isDisabled: BOOL): HResult;
begin
  isDisabled := True;
  Result := S_OK;
end;

{ TD2DCanvasSaveState }

constructor TD2DCanvasSaveState.Create;
begin
  inherited;
  // FContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, ContextLostHandler); => https://quality.embarcadero.com/browse/RSP-19673
end;

destructor TD2DCanvasSaveState.Destroy;
begin
  // TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FContextLostId); => https://quality.embarcadero.com/browse/RSP-19673
  inherited;
end;

//https://quality.embarcadero.com/browse/RSP-19673
//procedure TD2DCanvasSaveState.ContextLostHandler(const Sender: TObject; const Msg: TMessage);
//begin
//  FStateBlock := nil;
//  FLayer := nil;
//end;

procedure TD2DCanvasSaveState.Assign(Source: TPersistent);
var
  LSource: TCanvasD2D;
begin
  inherited Assign(Source);
  if Source is TCanvasD2D then
  begin
    LSource := TCanvasD2D(Source);
    if Succeeded(TCanvasD2D.SharedFactory.CreateDrawingStateBlock(nil, nil, FStateBlock)) then
    begin
      LSource.FTarget.SaveDrawingState(FStateBlock);
      LSource.FCurrentSaveState := Self;
    end;
  end;
end;

procedure TD2DCanvasSaveState.AssignTo(Dest: TPersistent);
var
  LDest: TCanvasD2D;
begin
  if Dest is TCanvasD2D then
  begin
    LDest := TCanvasD2D(Dest);
    LDest.FCurrentSaveState := nil;
    LDest.FTarget.RestoreDrawingState(FStateBlock);
    if FLayer <> nil then
    begin
      LDest.FTarget.PopLayer;
      FLayer := nil;
    end;
  end;
  inherited AssignTo(Dest);
end;

procedure TD2DCanvasSaveState.CreateLayer(const RenderTarget: ID2D1RenderTarget);
begin
  OleCheck(RenderTarget.CreateLayer(nil, FLayer));
end;

{ TTextLayoutD2D.TDWriteFontDescriptor }

class function TTextLayoutD2D.TDWriteFontDescriptor.Create(const AFamilyName: string; const AWeight: TDWriteFontWeight;
  const AStyle: TDWriteFontStyle; const AStretch: TDWriteFontStretch): TDWriteFontDescriptor;
begin
  Result.FamilyName := AFamilyName;
  Result.Weight := AWeight;
  Result.Style := AStyle;
  Result.Stretch := AStretch;
end;

{ TTextLayoutD2D }

type
  ID2D1Factory5 = interface // from d2d1_3.h
  ['{c4349994-838e-4b0f-8cab-44997d9eeacc}']
  end;

constructor TTextLayoutD2D.Create(const ACanvas: TCanvas);
begin
  inherited;
  // FContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, ContextLostHandler); => https://quality.embarcadero.com/browse/RSP-19673
  FDrawTextOption := DefaultDrawTextOption;
end;

destructor TTextLayoutD2D.Destroy;
begin
  // TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FContextLostId); => https://quality.embarcadero.com/browse/RSP-19673
  inherited;
end;

class procedure TTextLayoutD2D.DestroyGlobalResources;
begin
  FFontCollection := nil;
  FFontDescriptors.Free;
end;

class function TTextLayoutD2D.GetDefaultDrawTextOption: Integer;
begin
  if FDefaultDrawTextOption = 0 then
  begin
    FDefaultDrawTextOption := D2D1_DRAW_TEXT_OPTIONS_CLIP;
    if Supports(TCanvasD2D.SharedFactory, ID2D1Factory5) then
      FDefaultDrawTextOption := FDefaultDrawTextOption or D2D1_DRAW_TEXT_OPTIONS_ENABLE_COLOR_FONT;
  end;
  Result := FDefaultDrawTextOption;
end;

function TTextLayoutD2D.GetFontStyles(const AFont: TFont): TDWriteFontDescriptor;

  function GetDWriteLocalizedName(const Names: IDWriteLocalizedStrings; const LocaleName: string): string;
  const
    DefaultLocale = 'en-US';
  var
    Index, Length: Cardinal;
    Exists: LongBool;
    Name: array [0..255] of Char;
  begin
    Names.FindLocaleName(PChar(LocaleName), Index, Exists);
    if not Exists then
      Names.FindLocaleName(DefaultLocale, Index, Exists);
    if not Exists then
      Index := 0;
    Names.GetStringLength(Index, Length);
    Names.GetString(Index, Name, Min(Length + 1, High(Name)));
    Result := Name;
  end;

var
  FamilyIndex: Cardinal;
  FamilyExists: LongBool;
  DWriteGdiInterop: IDWriteGdiInterop;
  LogFont: TLogFont;
  DWriteFont: IDWriteFont;
  DWriteFamily: IDWriteFontFamily;
  Names: IDWriteLocalizedStrings;
  LocaleName: array [0..LOCALE_NAME_MAX_LENGTH] of Char;
  LDC: HDC;
begin
  Result := TDWriteFontDescriptor.Create(AFont.Family, FontWeightToDWrite(AFont.StyleExt.Weight),
    FontSlantToDWrite(AFont.StyleExt.Slant), FontStretchToDWrite(AFont.StyleExt.Stretch));

  if FFontCollection = nil then
    TCanvasD2D.SharedDWriteFactory.GetSystemFontCollection(FFontCollection);
  FFontCollection.FindFamilyName(PChar(AFont.Family), FamilyIndex, FamilyExists);
  if not FamilyExists then
  begin
    if FFontDescriptors = nil then
      FFontDescriptors := TDictionary<string, TDWriteFontDescriptor>.Create;
    if not FFontDescriptors.ContainsKey(AFont.Family) then
    begin
      if Succeeded(TCanvasD2D.SharedDWriteFactory.GetGdiInterop(DWriteGdiInterop)) then
      try
        ZeroMemory(@LogFont, SizeOf(LogFont));
        Move(PChar(AFont.Family)^, LogFont.lfFaceName, Length(AFont.Family) * SizeOf(Char));
        LogFont.lfCharSet := DEFAULT_CHARSET;
        LDC := GetDC(0);
        try
          EnumFontFamiliesEx(LDC, LogFont, @EnumFontFamExProc, NativeInt(@LogFont), 0);
        finally
          ReleaseDC(0, LDC);
        end;
        if Succeeded(DWriteGdiInterop.CreateFontFromLOGFONT(LogFont, DWriteFont)) then
        try
          if Succeeded(DWriteFont.GetFontFamily(DWriteFamily)) then
          try
            if FLocaleName.IsEmpty then
            begin
              GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SNAME, LocaleName, LOCALE_NAME_MAX_LENGTH);
              FLocaleName := LocaleName;
            end;
            if Succeeded(DWriteFamily.GetFamilyNames(Names)) then
            try
              Result := TDWriteFontDescriptor.Create(GetDWriteLocalizedName(Names, FLocaleName), DWriteFont.GetWeight,
                DWriteFont.GetStyle, DWriteFont.GetStretch);
              FFontDescriptors.Add(AFont.Family, Result);
            finally
              Names := nil;
            end;
          finally
            DWriteFamily := nil;
          end;
        finally
          DWriteFont := nil;
        end
        else
          FFontDescriptors.Add(AFont.Family, Result);
      finally
        DWriteGdiInterop := nil
      end;
    end
    else
      Result := FFontDescriptors[AFont.Family];
    if not AFont.StyleExt.Slant.IsRegular then
      Result.Style := Result.Style or DWRITE_FONT_STYLE_OBLIQUE;
  end;
  if not AFont.StyleExt.IsRegular then
  begin
    Result.Weight := FontWeightToDWrite(AFont.StyleExt.Weight);
    Result.Stretch := FontStretchToDWrite(AFont.StyleExt.Stretch);
    Result.Style := Result.Style or FontSlantToDWrite(AFont.StyleExt.Slant);
  end;
end;

procedure TTextLayoutD2D.UpdateTextRect;
var
  LWidth, LHeight, Extra: Single;
begin
  Extra := Max(0, FMetrics.layoutWidth - FMetrics.widthIncludingTrailingWhitespace + FOverhangMetrics.right - FMetrics.left);
  LWidth := Min(FMetrics.widthIncludingTrailingWhitespace + Extra, MaxSize.X);
  LHeight := Min(Min(FMetrics.height, FMetrics.layoutHeight), MaxSize.Y);

  FTextRect := TRectF.Create(FMetrics.left, FMetrics.top, FMetrics.left + LWidth, FMetrics.top + LHeight);
  if FMetrics.top < 0 then
    FTextRect.Offset(0, Abs(FMetrics.top));
  if FMetrics.left < 0 then
    FTextRect.Offset(Abs(FMetrics.left), 0);

  FTextRect.Right := Min(FTextRect.Right, MaxSize.X);
end;

//https://quality.embarcadero.com/browse/RSP-19673
//procedure TTextLayoutD2D.ContextLostHandler(const Sender: TObject; const Msg: TMessage);
//begin
//  FLayout := nil;
//  FBrush := nil;
//  SetNeedUpdate;
//end;

procedure TTextLayoutD2D.DoDrawLayout(const ACanvas: TCanvas);

  function SameColor: Boolean;
  var
    LColor: TD2D1ColorF;
  begin
    ID2D1SolidColorBrush(FBrush).GetColor(LColor);
    Result := SameValue(LColor.r, TAlphaColorRec(Color).R / $FF, TEpsilon.Vector) and
      SameValue(LColor.b, TAlphaColorRec(Color).B / $FF, TEpsilon.Vector) and
      SameValue(LColor.g, TAlphaColorRec(Color).G / $FF, TEpsilon.Vector) and
      SameValue(LColor.a, TAlphaColorRec(Color).A / $FF * Opacity, TEpsilon.Vector);
  end;

var
  Target: ID2D1RenderTarget;
begin
  if (ACanvas = nil) or not (ACanvas is TCanvasD2D) or Text.IsEmpty or (FLayout = nil) then
    Exit;

  Target := TCanvasD2D(ACanvas).FTarget;
  if not ((FBrush <> nil) and SameValue(FBrush.GetOpacity, Opacity, TEpsilon.Vector) and SameColor) then
  begin
    FBrush := nil;
    Target.CreateSolidColorBrush(D2Color(Color, Opacity), nil, ID2D1SolidColorBrush(FBrush));
  end;

  if(FBrush <> nil) then
    Target.DrawTextLayout(D2Point(TopLeft.X + Padding.Left, TopLeft.Y + Padding.Top), FLayout, FBrush, FDrawTextOption);
end;

procedure TTextLayoutD2D.DoRenderLayout;
var
  TextRange: TDWriteTextRange;
  TextFormat: IDWriteTextFormat;
  I: Integer;
  Attr: TTextAttributedRange;
  TrimOptions: TDwriteTrimming;
  TrimmingSign: IDWriteInlineObject;
  FontDescriptor: TDWriteFontDescriptor;
  SolidBrush: ID2D1SolidColorBrush;
begin
  FLayout := nil;
  FTextRect := TRectF.Empty;

  FontDescriptor := GetFontStyles(Font);

  if Succeeded(TCanvasD2D.SharedDWriteFactory.CreateTextFormat(PChar(FontDescriptor.FamilyName), FFontCollection,
    FontDescriptor.Weight, FontDescriptor.Style, FontDescriptor.Stretch, Font.Size, PChar(FLocaleName), TextFormat)) then
  try
    if RightToLeft then
      TextFormat.SetReadingDirection(DWRITE_READING_DIRECTION_RIGHT_TO_LEFT)
    else
      TextFormat.SetReadingDirection(DWRITE_READING_DIRECTION_LEFT_TO_RIGHT);

    if WordWrap then
      TextFormat.SetWordWrapping(DWRITE_WORD_WRAPPING_WRAP)
    else
      TextFormat.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP);

    FillChar(TrimOptions, SizeOf(TDwriteTrimming), 0);
    case Trimming of
      TTextTrimming.None:
        TrimOptions.granularity := DWRITE_TRIMMING_GRANULARITY_NONE;
      TTextTrimming.Character:
        TrimOptions.granularity := DWRITE_TRIMMING_GRANULARITY_CHARACTER;
      TTextTrimming.Word:
        TrimOptions.granularity := DWRITE_TRIMMING_GRANULARITY_WORD;
    end;

    TrimmingSign := nil;
    if Trimming <> TTextTrimming.None then
      TCanvasD2D.SharedDWriteFactory.CreateEllipsisTrimmingSign(TextFormat, TrimmingSign);
    TextFormat.SetTrimming(TrimOptions, TrimmingSign);
    TrimmingSign := nil;

    if Succeeded(TCanvasD2D.SharedDWriteFactory.CreateTextLayout(PChar(Text), Text.Length, TextFormat, MaxSize.X -
      Padding.Left - Padding.Right, MaxSize.Y - Padding.Top - Padding.Bottom, FLayout)) then
    begin
      TextRange.StartPosition := 0;
      TextRange.Length := Text.Length;
      FLayout.SetStrikethrough(TFontStyle.fsStrikeOut in Font.StyleExt.SimpleStyle, TextRange);
      FLayout.SetUnderline(TFontStyle.fsUnderline in Font.StyleExt.SimpleStyle, TextRange);
      FLayout.SetFontWeight(FontDescriptor.Weight, TextRange);

      for I := 0 to AttributesCount - 1 do
      begin
        Attr := Attributes[I];
        TextRange.startPosition := Attr.Range.Pos;
        TextRange.length := Attr.Range.Length;
        if Attr.Attribute.Font <> nil then
        begin
          FLayout.SetStrikethrough(TFontStyle.fsStrikeOut in Attr.Attribute.Font.StyleExt.SimpleStyle, TextRange);
          FLayout.SetUnderline(TFontStyle.fsUnderline in Attr.Attribute.Font.StyleExt.SimpleStyle, TextRange);
          if not SameValue(Attr.Attribute.Font.Size, Font.Size, TEpsilon.FontSize) then
            FLayout.SetFontSize(Attr.Attribute.Font.Size, TextRange);

          FontDescriptor := GetFontStyles(Attr.Attribute.Font);
          FLayout.SetFontWeight(FontDescriptor.Weight, TextRange);
          FLayout.SetFontStyle(FontDescriptor.Style, TextRange);
          FLayout.SetFontStretch(FontDescriptor.Stretch, TextRange);
          FLayout.SetFontFamilyName(PChar(FontDescriptor.FamilyName), TextRange);
        end;
        if (Attr.Attribute.Color <> TAlphaColorRec.Null) and
           Succeeded(TCanvasD2D.SharedRenderTarget.CreateSolidColorBrush(D2Color(Attr.Attribute.Color, Opacity), nil,
             SolidBrush)) then
          FLayout.SetDrawingEffect(SolidBrush, TextRange);
      end;

      case HorizontalAlign of
        TTextAlign.Center:
          FLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_CENTER);
        TTextAlign.Leading:
          FLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);
        TTextAlign.Trailing:
          FLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_TRAILING);
      end;
      case VerticalAlign of
        TTextAlign.Center:
          FLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_CENTER);
        TTextAlign.Leading:
          FLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR);
        TTextAlign.Trailing:
          FLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_FAR);
      end;
      FLayout.GetMetrics(FMetrics);
      FLayout.GetOverhangMetrics(FOverhangMetrics);
      UpdateTextRect;
    end;
  finally
    TextFormat := nil;
  end;
end;

class function TTextLayoutD2D.EnumFontFamExProc(const lpelfe: TLogFont; const lpntme: TNewTextMetric; FontType: DWORD;
  lParam: LPARAM): Integer;
var
  LFont: PLogFont;
begin
  LFont := Pointer(lParam);
  Move(lpelfe, LFont^, SizeOf(TLogFont));
  Result := 0;
end;

function TTextLayoutD2D.GetTextHeight: Single;
begin
  Result := FTextRect.Height;
end;

function TTextLayoutD2D.GetTextRect: TRectF;
begin
  Result := FTextRect;
  Result.Offset(TopLeft);
end;

function TTextLayoutD2D.GetTextWidth: Single;
begin
  Result := FTextRect.Width;
end;

function TTextLayoutD2D.DoPositionAtPoint(const APoint: TPointF): Integer;
var
  TrailingHit: BOOL;
  Inside: BOOL;
  HitTestMetrics: TDWriteHitTestMetrics;
  LRect: TRectF;
  Count: Cardinal;
  HitTestMetricsArray: array of TDWriteHitTestMetrics;
begin
  Result := -1;
  if FLayout = nil then
    Exit;

  LRect := Self.TextRect;
  LRect.Width := Max(LRect.Width, MaxSize.X);
  if LRect.Contains(APoint) then
  begin
    FLayout.HitTestPoint(APoint.X - TopLeft.X - Padding.Left, APoint.Y - TopLeft.Y - Padding.Top, TrailingHit, Inside,
      HitTestMetrics);
    Result := HitTestMetrics.textPosition;
    if TrailingHit or Inside then
    begin
      { Checking if we hit after the middle of charecter, when increasingposition. This will handle situation when we
        need to set position after last charecter in text. }
      Count := 1;
      SetLength(HitTestMetricsArray, 1);
      FLayout.HitTestTextRange(Result, 1, 0, 0, HitTestMetricsArray[0], Count, Count);
      if Count > 0 then
      begin
        if (HitTestMetricsArray[0].width > 0) and (APoint.X > (TopLeft.X + HitTestMetricsArray[0].Left +
          HitTestMetricsArray[0].width * 3 / 5)) then
          Inc(Result);
      end
      else
        if (FMetrics.widthIncludingTrailingWhitespace > 0) and (APoint.X > (TopLeft.X + FMetrics.left +
          FMetrics.widthIncludingTrailingWhitespace * 3 / 5)) then
          Inc(Result);
    end;
  end
  else if ((APoint.X < LRect.Left) or SameValue(APoint.X, LRect.Left, TEpsilon.Position)) and
    ((APoint.Y > LRect.Top) or SameValue(APoint.Y, LRect.Top, TEpsilon.Position)) and
    ((APoint.Y < LRect.Bottom) or SameValue(APoint.Y, LRect.Bottom, TEpsilon.Position)) then
      Result := 0;
  if (Result >= 0) and (Result < Text.Length) and Text.Chars[Result].IsLowSurrogate then
    Inc(Result);
end;

function TTextLayoutD2D.DoRegionForRange(const ARange: TTextRange): TRegion;
var
  HitTestMetrics: array of TDWriteHitTestMetrics;
  HitTestMetric: TDWriteHitTestMetrics;
  Count: Cardinal;
  I, RemainsLength, RangeLength, LPos: Integer;
begin
  SetLength(Result, 0);
  if (ARange.Pos < 0) or (ARange.Length < 0) or (FLayout = nil) then
    Exit;

  if Text.IsEmpty then
  begin
    SetLength(Result, 1);
    Result[0] := Self.TextRect;
    Exit;
  end;

  RangeLength := Text.Length;
  if ARange.Pos > RangeLength then
    Exit;

  SetLength(Result, 0);
  RemainsLength := Min(ARange.Length, RangeLength - ARange.Pos);

  if (ARange.Pos < Text.Length) and Text.Chars[ARange.Pos].IsLowSurrogate then
  begin
    LPos := ARange.Pos - 1;
    Inc(RemainsLength);
  end
  else
    LPos := ARange.Pos;

  SetLength(HitTestMetrics, 1);
  FLayout.HitTestTextRange(LPos, RemainsLength, 0, 0, HitTestMetrics[0], 0, Count);
  SetLength(HitTestMetrics, Count);
  FLayout.HitTestTextRange(LPos, RemainsLength, 0, 0, HitTestMetrics[0], Count, Count);
  if Count > 0 then
    for I := 0 to High(HitTestMetrics) do
    begin
      SetLength(Result, Length(Result) + 1);
      HitTestMetric := HitTestMetrics[I];
      Result[High(Result)] := TRectF.Create(HitTestMetric.left, HitTestMetric.top, HitTestMetric.left +
        HitTestMetric.width, HitTestMetric.top + HitTestMetric.height);
      Result[High(Result)].Offset(TopLeft);
      Result[High(Result)].Offset(Padding.Left, Padding.Top);
  end;
end;

procedure TTextLayoutD2D.ConvertToPath(const APath: TPathData);
var
  PathRendering: TTextRendering;
begin
  if Text.IsEmpty or (APath = nil) or (FLayout = nil) then
    Exit;

  PathRendering := TTextRendering.Create;
  try
                                                                                       
    FLayout.Draw(APath, PathRendering, 0, 0);
  finally
    PathRendering.Free;
  end;
end;

procedure RegisterCanvasClasses;
begin
  if GlobalUseDirect2D and (TCustomCanvasD2D.Direct3DLevel >= TCustomCanvasD2D.TDirect3DLevel.Direct3D10) and
    InitThemeLibrary and UseThemes and TCustomCanvasD2D.TryCreateDirect3DDevice then
  begin
    TCustomCanvasD2D.DestroyDirect3DDevice;
    TCanvasManager.RegisterCanvas(TCanvasD2D, True, False);
  end;
end;

procedure UnregisterCanvasClasses;
begin
  TCanvasD2D.DestroySharedResources;
  TTextLayoutD2D.DestroyGlobalResources;
end;

initialization
  TCanvasD2D.fLock := TObject.create; // https://quality.embarcadero.com/browse/RSP-19673
  TTextLayoutManager.RegisterTextLayout(TTextLayoutD2D, TCanvasD2D);

  TBitmapCodecManager.RegisterBitmapCodecClass(SBMPImageExtension, SVBitmaps, True, TBitmapCodecWIC);
  TBitmapCodecManager.RegisterBitmapCodecClass(SJPGImageExtension, SVJPGImages, True, TBitmapCodecWIC);
  TBitmapCodecManager.RegisterBitmapCodecClass(SJPEGImageExtension, SVJPGImages, True, TBitmapCodecWIC);
  TBitmapCodecManager.RegisterBitmapCodecClass(SPNGImageExtension, SVPNGImages, True, TBitmapCodecWIC);
  TBitmapCodecManager.RegisterBitmapCodecClass(SGIFImageExtension, SVGIFImages, True, TBitmapCodecWIC);
  TBitmapCodecManager.RegisterBitmapCodecClass(STIFImageExtension, SVTIFFImages, True, TBitmapCodecWIC);
  TBitmapCodecManager.RegisterBitmapCodecClass(STIFFImageExtension, SVTIFFImages, True, TBitmapCodecWIC);
  TBitmapCodecManager.RegisterBitmapCodecClass(SICOImageExtension, SVIcons, True, TBitmapCodecWIC);
  TBitmapCodecManager.RegisterBitmapCodecClass(SHDPImageExtension, SWMPImages, True, TBitmapCodecWIC);

finalization
  TCanvasD2D.fLock.free; // https://quality.embarcadero.com/browse/RSP-19673

end.
