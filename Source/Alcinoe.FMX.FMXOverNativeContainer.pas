// TALFMXOverNativeContainer
// -------------------------
// A TALNativeControl whose purpose is to display regular FireMonkey controls
// ABOVE native controls (TALMapLibre, TALWebBrowser, TALEdit, TALVideoPlayer,
// etc.). Native views are always rendered on top of the FireMonkey form
// surface, so a FireMonkey control placed over a native control is normally
// hidden behind it. To work around this, each time TALFMXOverNativeContainer is
// painted, its children are rendered (via PaintTo) into an offscreen bitmap,
// and this bitmap is handed to a transparent native view (an ImageView on
// Android, the CALayer contents on iOS/macOS, a layered child window on
// Windows) that is kept on top of the other native views.
unit Alcinoe.FMX.FMXOverNativeContainer;

interface

{$I Alcinoe.inc}

uses
  System.Classes,
  System.Types,
  {$IF defined(android)}
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Widget,
  Alcinoe.FMX.NativeView.Android,
  {$ELSEIF defined(IOS)}
  System.TypInfo,
  iOSapi.UIKit,
  Alcinoe.FMX.NativeView.iOS,
  {$ELSEIF defined(ALMacOS)}
  System.TypInfo,
  Macapi.CocoaTypes,
  Macapi.AppKit,
  Alcinoe.FMX.NativeView.Mac,
  {$ELSEIF defined(MSWINDOWS)}
  Winapi.Windows,
  FMX.Controls.Win,
  Alcinoe.FMX.NativeView.Win,
  {$ENDIF}
  FMX.types,
  FMX.Controls,
  FMX.Graphics,
  Alcinoe.FMX.NativeControl;

type
  {$IF defined(android)}
  TALAndroidFMXOverNativeView = class;
  {$ENDIF}
  {$IF defined(IOS)}
  TALIosFMXOverNativeView = class;
  {$ENDIF}
  {$IF defined(ALMacOS)}
  TALMacFMXOverNativeView = class;
  {$ENDIF}
  {$IF defined(MSWINDOWS)}
  TALWinFMXOverNativeView = class;
  {$ENDIF}

  {*************************}
  [ComponentPlatforms($FFFF)]
  TALFMXOverNativeContainer = class(TALNativeControl, IControlTypeSupportable, IALNativeControl)
  private
    FBitmap: FMX.Graphics.TBitmap;
    {$IF defined(android)}
    function GetNativeView: TALAndroidFMXOverNativeView;
    {$ELSEIF defined(IOS)}
    function GetNativeView: TALIosFMXOverNativeView;
    {$ELSEIF defined(ALMacOS)}
    function GetNativeView: TALMacFMXOverNativeView;
    {$ELSEIF defined(MSWindows)}
    function GetNativeView: TALWinFMXOverNativeView;
    {$ENDIF}
  protected
    {$IF defined(android)}
    Function CreateNativeView: TALAndroidNativeView; override;
    {$ELSEIF defined(IOS)}
    Function CreateNativeView: TALIosNativeView; override;
    {$ELSEIF defined(ALMacOS)}
    Function CreateNativeView: TALMacNativeView; override;
    {$ELSEIF defined(MSWindows)}
    Function CreateNativeView: TALWinNativeView; override;
    {$ENDIF}
    procedure Paint; override;
    procedure PaintChildren; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF defined(android)}
    property NativeView: TALAndroidFMXOverNativeView read GetNativeView;
    {$ELSEIF defined(IOS)}
    property NativeView: TALIosFMXOverNativeView read GetNativeView;
    {$ELSEIF defined(ALMacOS)}
    property NativeView: TALMacFMXOverNativeView read GetNativeView;
    {$ELSEIF defined(MSWindows)}
    property NativeView: TALWinFMXOverNativeView read GetNativeView;
    {$ENDIF}
    function GetNativeViewBoundsRect: TRectF; override;
  published
    //property Action;
    property Align;
    property Anchors;
    property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClickSound;
    property ClipChildren;
    //property ClipParent;
    property Corners;
    property Cursor;
    property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Fill;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Shadow;
    property Sides;
    property Size;
    property Stroke;
    //property TabOrder;
    //property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property XRadius;
    property YRadius;
    //property OnCanFocus;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnClick;
    property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

{$REGION ' ANDROID'}
{$IF defined(android)}

  {*******************************************************}
  TALAndroidFMXOverNativeView = class(TALAndroidNativeView)
  private
    type
      // --------------
      // TTouchListener
      TTouchListener = class(TJavaLocal, JView_OnTouchListener)
      private
        FNativeView: TALAndroidFMXOverNativeView;
      public
        constructor Create(const ANativeView: TALAndroidFMXOverNativeView);
        function onTouch(v: JView; event: JMotionEvent): Boolean; cdecl;
      end;
  private
    FBitmap: JBitmap;
    FTouchListener: TTouchListener;
    function GetView: JImageView;
  protected
    function CreateView: JView; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure PresentBitmap(const ABitmap: FMX.Graphics.TBitmap);
    property View: JImageView read GetView;
  end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

  {*****************************************}
  IALIosFMXOverNativeView = interface(UIView)
    ['{0C4B6E8B-6B0B-4D2B-9A55-2C1E3F2C9D41}']
  end;

  {***********************************************}
  TALIosFMXOverNativeView = class(TALIosNativeView)
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create; override;
    procedure PresentBitmap(const ABitmap: FMX.Graphics.TBitmap);
  end;

{$ENDIF}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}

  {*****************************************}
  IALMacFMXOverNativeView = interface(NSView)
    ['{6F0E3B1C-2D4A-4C7B-8E2F-5A9B7C1D3E6F}']
  end;

  {***********************************************}
  TALMacFMXOverNativeView = class(TALMacNativeView)
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create; override;
    procedure PresentBitmap(const ABitmap: FMX.Graphics.TBitmap);
  end;

{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

  {***********************************************}
  TALWinFMXOverNativeView = class(TALWinNativeView)
  private
    FLayerDC: HDC;
    FLayerBitmap: HBITMAP;
    FLayerOldBitmap: HGDIOBJ;
    FLayerBits: Pointer;
    FLayerWidth: Integer;
    FLayerHeight: Integer;
    FAlpha: Single;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetAlpha(const Value: Single); override;
    procedure PresentBitmap(const ABitmap: FMX.Graphics.TBitmap);
  end;

{$ENDIF}
{$ENDREGION}

procedure Register;

implementation

uses
  System.SysUtils,
  System.Math,
  System.UITypes,
  {$IF defined(android)}
  Androidapi.Helpers,
  Androidapi.Bitmap,
  Androidapi.jni.App,
  FMX.Platform.Android,
  {$ELSEIF defined(IOS)}
  iOSapi.CoreGraphics,
  {$ELSEIF defined(ALMacOS)}
  Macapi.CoreGraphics,
  {$ENDIF}
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  Alcinoe.Common,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.Common;

{***************************************************************}
constructor TALFMXOverNativeContainer.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap := nil;
end;

{*******************************************}
destructor TALFMXOverNativeContainer.Destroy;
begin
  ALFreeAndNil(FBitmap);
  inherited;
end;

{********************}
{$IF defined(android)}
Function TALFMXOverNativeContainer.CreateNativeView: TALAndroidNativeView;
begin
  result := TALAndroidFMXOverNativeView.create(self);
end;
{$ENDIF}

{********************}
{$IF defined(android)}
function TALFMXOverNativeContainer.GetNativeView: TALAndroidFMXOverNativeView;
begin
  result := TALAndroidFMXOverNativeView(inherited NativeView);
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
Function TALFMXOverNativeContainer.CreateNativeView: TALIosNativeView;
begin
  result := TALIosFMXOverNativeView.create(self);
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALFMXOverNativeContainer.GetNativeView: TALIosFMXOverNativeView;
begin
  result := TALIosFMXOverNativeView(inherited NativeView);
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
Function TALFMXOverNativeContainer.CreateNativeView: TALMacNativeView;
begin
  result := TALMacFMXOverNativeView.create(self);
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
function TALFMXOverNativeContainer.GetNativeView: TALMacFMXOverNativeView;
begin
  result := TALMacFMXOverNativeView(inherited NativeView);
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
Function TALFMXOverNativeContainer.CreateNativeView: TALWinNativeView;
begin
  {$IF defined(ALDPK)}
  Result := nil;
  {$ELSE}
  result := TALWinFMXOverNativeView.create(self);
  {$ENDIF}
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
function TALFMXOverNativeContainer.GetNativeView: TALWinFMXOverNativeView;
begin
  result := TALWinFMXOverNativeView(inherited NativeView);
end;
{$ENDIF}

{*****************************************************************}
function TALFMXOverNativeContainer.GetNativeViewBoundsRect: TRectF;
begin
  Result := ALGetShapeSurfaceRect(
              LocalRect, // const ARect: TRectF;
              AutoAlignToPixel, // const AAlignToPixel: Boolean;
              Fill, // const AFill: TALBrush;
              nil, // const AStateLayer: TALStateLayer;
              Shadow); // const AShadow: TALShadow): TRectF;
end;

{****************************************}
procedure TALFMXOverNativeContainer.Paint;
begin
  if (csDesigning in ComponentState) and not Locked then begin
    DrawDesignBorder;
    inherited;
  end
  else if (FInPaintTo) or (FNativeView = nil) then inherited
  else begin
    var LBoundsRect := GetNativeViewBoundsRect;
    var LBitmapWidth := Round(LBoundsRect.Width * ALGetScreenScale);
    var LBitmapHeight := Round(LBoundsRect.Height * ALGetScreenScale);
    if (LBitmapWidth <= 0) or (LBitmapHeight <= 0) then exit;

    if FBitmap = nil then FBitmap := TBitmap.Create(LBitmapWidth, LBitmapHeight)
    else if (FBitmap.Width <> LBitmapWidth) or
            (FBitmap.Height <> LBitmapHeight) then FBitmap.SetSize(LBitmapWidth, LBitmapHeight);

    FBitmap.BitmapScale := ALGetScreenScale;
    FBitmap.Clear(TAlphaColors.Null);

    if not FBitmap.Canvas.BeginScene then exit;
    try
      PaintTo(FBitmap.Canvas, TRectF.Create(-LBoundsRect.Left, -LBoundsRect.Top, -LBoundsRect.Left + Width, -LBoundsRect.Top + Height));
    finally
      FBitmap.Canvas.EndScene;
    end;

    NativeView.PresentBitmap(FBitmap);
  end;
end;

{************************************************}
procedure TALFMXOverNativeContainer.PaintChildren;
begin
  if FInPaintTo or (csDesigning in ComponentState) or (FNativeView = nil) then inherited;
end;

{$REGION ' ANDROID'}
{$IF defined(android)}

{************************************************************************************************************}
constructor TALAndroidFMXOverNativeView.TTouchListener.Create(const ANativeView: TALAndroidFMXOverNativeView);
begin
  inherited Create;
  FNativeView := ANativeView;
end;

{**************************************************************************************************}
function TALAndroidFMXOverNativeView.TTouchListener.onTouch(v: JView; event: JMotionEvent): Boolean;
begin
  if FNativeView.Form = nil then exit(False);
  var LEvent := TJMotionEvent.JavaClass.obtain(Event);
  try
    LEvent.offsetLocation(FNativeView.Layout.getX, FNativeView.Layout.getY);
    WindowHandleToPlatform(FNativeView.Form.Handle).MotionManager.HandleMotionEvent(LEvent);
  finally
    LEvent.recycle;
  end;
  Result := True;
end;

{*********************************************}
constructor TALAndroidFMXOverNativeView.Create;
begin
  inherited; // This will call CreateView
  FBitmap := nil;
  View.setScaleType(TJImageView_ScaleType.JavaClass.MATRIX);
  FTouchListener := TTouchListener.Create(Self);
  View.setOnTouchListener(FTouchListener);
end;

{*********************************************}
destructor TALAndroidFMXOverNativeView.Destroy;
begin
  View.setOnTouchListener(nil);
  ALFreeAndNil(FTouchListener);
  View.setImageBitmap(nil);
  FBitmap := nil;
  inherited;
end;

{*****************************************************}
function TALAndroidFMXOverNativeView.CreateView: JView;
begin
  Result := TJImageView.JavaClass.init(TAndroidHelper.Activity);
end;

{*******************************************************}
function TALAndroidFMXOverNativeView.GetView: JImageView;
begin
  Result := inherited GetView<JImageView>;
end;

{***************************************************************************************}
procedure TALAndroidFMXOverNativeView.PresentBitmap(const ABitmap: FMX.Graphics.TBitmap);
begin
  var LNewBitmap := False;
  if (FBitmap = nil) or (FBitmap.getWidth <> ABitmap.Width) or (FBitmap.getHeight <> ABitmap.Height) then begin
    // We don't recycle the previous bitmap because the RenderThread
    // can still reference it. The GC will take care of it.
    FBitmap := TJBitmap.JavaClass.createBitmap(ABitmap.Width, ABitmap.Height, TJBitmap_Config.JavaClass.ARGB_8888);
    LNewBitmap := True;
  end;

  var LPixelBuffer: Pointer;
  if AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, TJNIResolver.JavaInstanceToID(FBitmap), @LPixelBuffer) <> 0 then Exit;
  try
    var LBitmapData: TBitmapData;
    if not ABitmap.Map(TMapAccess.Read, LBitmapData) then exit;
    try
      Move(LBitmapData.Data^, LPixelBuffer^, ABitmap.BytesPerLine * ABitmap.Height);
    finally
      ABitmap.Unmap(LBitmapData);
    end;
  finally
    AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, TJNIResolver.JavaInstanceToID(FBitmap));
  end;

  if LNewBitmap then View.setImageBitmap(FBitmap)
  else View.invalidate;
end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

{*****************************************}
constructor TALIosFMXOverNativeView.Create;
begin
  inherited; // This will call InitView
  View.setOpaque(False);
  View.setBackgroundColor(TUIColor.OCClass.clearColor);
end;

{*************************************************************}
function TALIosFMXOverNativeView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALIosFMXOverNativeView);
end;

{***********************************************************************************}
procedure TALIosFMXOverNativeView.PresentBitmap(const ABitmap: FMX.Graphics.TBitmap);
begin
  var LColorSpace := CGColorSpaceCreateDeviceRGB;
  try
    var LbitmapInfo: CGBitmapInfo;
    case ABitmap.PixelFormat of
      TPixelFormat.RGB: LbitmapInfo := kCGImageAlphaNone or kCGBitmapByteOrder32Big;
      TPixelFormat.RGBA: LbitmapInfo := kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big;
      TPixelFormat.BGR: LbitmapInfo := kCGImageAlphaNone or kCGBitmapByteOrder32Little;
      TPixelFormat.BGRA: LbitmapInfo := kCGImageAlphaPremultipliedFirst or kCGBitmapByteOrder32Little;
      else raise Exception.Create('The specified bitmap format is not supported');
    end;
    var LBitmapData: TBitmapData;
    if ABitmap.Map(TMapAccess.Read, LBitmapData) then
    try
      var LCtxRef := CGBitmapContextCreate(LBitmapData.Data, ABitmap.Width, ABitmap.Height, 8, 4 * ABitmap.Width, LColorSpace, LBitmapInfo);
      if LCtxRef = nil then raise Exception.Create('Cannot create bitmap context');
      try
        var LImageRef := CGBitmapContextCreateImage(LCtxRef);
        try
          if LImageRef = nil then raise Exception.Create('Cannot create CGImage');
          View.layer.setContents(LImageRef);
        finally
          CGImageRelease(LImageRef);
        end;
      finally
        CGContextRelease(LCtxRef);
      end;
    finally
      ABitmap.Unmap(LBitmapData);
    end;
  finally
    CGColorSpaceRelease(LColorSpace);
  end;
end;

{$ENDIF}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}

{*****************************************}
constructor TALMacFMXOverNativeView.Create;
begin
  inherited;
  View.setWantsLayer(True);
end;

{*************************************************************}
function TALMacFMXOverNativeView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALMacFMXOverNativeView);
end;

{***********************************************************************************}
procedure TALMacFMXOverNativeView.PresentBitmap(const ABitmap: FMX.Graphics.TBitmap);
begin
  var LColorSpace := CGColorSpaceCreateDeviceRGB;
  try
    var LbitmapInfo: CGBitmapInfo;
    case ABitmap.PixelFormat of
      TPixelFormat.RGB: LbitmapInfo := kCGImageAlphaNone or kCGBitmapByteOrder32Big;
      TPixelFormat.RGBA: LbitmapInfo := kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big;
      TPixelFormat.BGR: LbitmapInfo := kCGImageAlphaNone or kCGBitmapByteOrder32Little;
      TPixelFormat.BGRA: LbitmapInfo := kCGImageAlphaPremultipliedFirst or kCGBitmapByteOrder32Little;
      else raise Exception.Create('The specified bitmap format is not supported');
    end;
    var LBitmapData: TBitmapData;
    if ABitmap.Map(TMapAccess.Read, LBitmapData) then
    try
      var LCtxRef := CGBitmapContextCreate(LBitmapData.Data, ABitmap.Width, ABitmap.Height, 8, 4 * ABitmap.Width, LColorSpace, LBitmapInfo);
      if LCtxRef = nil then raise Exception.Create('Cannot create bitmap context');
      try
        var LImageRef := CGBitmapContextCreateImage(LCtxRef);
        try
          if LImageRef = nil then raise Exception.Create('Cannot create CGImage');
          View.layer.setContents(LImageRef);
        finally
          CGImageRelease(LImageRef);
        end;
      finally
        CGContextRelease(LCtxRef);
      end;
    finally
      ABitmap.Unmap(LBitmapData);
    end;
  finally
    CGColorSpaceRelease(LColorSpace);
  end;
end;

{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

{*****************************************}
constructor TALWinFMXOverNativeView.Create;
begin
  inherited;
  FLayerDC := 0;
  FLayerBitmap := 0;
  FLayerOldBitmap := 0;
  FLayerBits := nil;
  FLayerWidth := 0;
  FLayerHeight := 0;
  FAlpha := 1;
end;

{*****************************************}
destructor TALWinFMXOverNativeView.Destroy;
begin
  if FLayerBitmap <> 0 then begin
    SelectObject(FLayerDC, FLayerOldBitmap);
    DeleteObject(FLayerBitmap);
  end;

  if FLayerDC <> 0 then
    DeleteDC(FLayerDC);

  inherited;
end;

{************************************************************************}
procedure TALWinFMXOverNativeView.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_LAYERED;
end;

{**************************************************************}
procedure TALWinFMXOverNativeView.SetAlpha(const Value: Single);
begin
  // Applied on the next PresentBitmap
  FAlpha := Value;
end;

{***********************************************************************************}
procedure TALWinFMXOverNativeView.PresentBitmap(const ABitmap: FMX.Graphics.TBitmap);
begin
  // Create the memory DC once.
  if FLayerDC = 0 then begin
    FLayerDC := CreateCompatibleDC(0{DC: HDC});
    if FLayerDC = 0 then RaiseLastOSError;
  end;

  // Recreate the DIB section only when its dimensions change.
  if (FLayerBitmap = 0) or
     (FLayerWidth <> ABitmap.Width) or
     (FLayerHeight <> ABitmap.Height) then begin

    if FLayerBitmap <> 0 then begin
      SelectObject(FLayerDC, FLayerOldBitmap);
      DeleteObject(FLayerBitmap);
      FLayerBitmap := 0;
      FLayerBits := nil;
    end;

    var LBitmapInfo: TBitmapInfo;
    FillChar(LBitmapInfo, SizeOf(LBitmapInfo), 0);
    LBitmapInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
    LBitmapInfo.bmiHeader.biWidth := ABitmap.Width;
    LBitmapInfo.bmiHeader.biHeight := -ABitmap.Height; // Top-down
    LBitmapInfo.bmiHeader.biPlanes := 1;
    LBitmapInfo.bmiHeader.biBitCount := 32;
    LBitmapInfo.bmiHeader.biCompression := BI_RGB;

    FLayerBitmap := CreateDIBSection(
                      FLayerDC, // DC: HDC;
                      LBitmapInfo, // const p2: TBitmapInfo;
                      DIB_RGB_COLORS, // p3: UINT;
                      FLayerBits, // var p4: Pointer;
                      0, // p5: THandle;
                      0); // p6: DWORD

    if FLayerBitmap = 0 then RaiseLastOSError;

    FLayerOldBitmap := SelectObject(FLayerDC, FLayerBitmap);
    if (FLayerOldBitmap = 0) or
       (FLayerOldBitmap = HGDI_ERROR) then begin
      DeleteObject(FLayerBitmap);
      FLayerBitmap := 0;
      FLayerBits := nil;
      RaiseLastOSError;
    end;
    FLayerWidth := ABitmap.Width;
    FLayerHeight := ABitmap.Height;
  end;

  // Copy the FMX bitmap directly into the DIB section.
  var LBitmapData: TBitmapData;
  if not ABitmap.Map(TMapAccess.Read, LBitmapData) then
    raise Exception.Create('Cannot map bitmap');
  try
    if (LBitmapData.PixelFormat <> TPixelFormat.BGRA) or
       (LBitmapData.BytesPerPixel <> 4) then
      raise Exception.Create('Expected a 32-bit BGRA bitmap');

    if LBitmapData.Pitch = ABitmap.Width * 4 then
      Move(LBitmapData.Data^, FLayerBits^, ABitmap.Height * ABitmap.Width * 4)
    else begin
      for var I := 0 to ABitmap.Height - 1 do
        Move(
          LBitmapData.GetScanline(I)^,
          PByte(NativeUInt(FLayerBits) + NativeUInt(I) * NativeUInt(ABitmap.Width * 4))^,
          ABitmap.Width * 4);
    end;
  finally
    ABitmap.Unmap(LBitmapData);
  end;

  // Update the layered window.
  var LSize := TSize.Create(ABitmap.Width, ABitmap.Height);
  var LSourcePoint := TPoint.Create(0, 0);
  var LBlend: TBlendFunction;
  LBlend.BlendOp := AC_SRC_OVER;
  LBlend.BlendFlags := 0;
  LBlend.SourceConstantAlpha := EnsureRange(Round(FAlpha * 255), 0, 255);
  LBlend.AlphaFormat := AC_SRC_ALPHA;
  if not UpdateLayeredWindow(
           Handle, // Handle: THandle;
           0, // hdcDest: HDC;
           nil, // pptDst: PPoint;
           @LSize, // _psize: PSize;
           FLayerDC, // hdcSrc: HDC;
           @LSourcePoint, // pptSrc: PPoint;
           0, // crKey: COLORREF;
           @LBlend, // pblend: PBLENDFUNCTION;
           ULW_ALPHA) then // dwFlags: DWORD
    RaiseLastOSError;
end;

{$ENDIF}
{$ENDREGION}

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALFMXOverNativeContainer]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALFMXOverNativeContainer, 'Size');
  UnlistPublishedProperty(TALFMXOverNativeContainer, 'StyleName');
  UnlistPublishedProperty(TALFMXOverNativeContainer, 'OnTap');
  {$ENDIF}
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.FMXOverNativeContainer','initialization');
  {$ENDIF}
  RegisterFmxClasses([TALFMXOverNativeContainer]);

end.