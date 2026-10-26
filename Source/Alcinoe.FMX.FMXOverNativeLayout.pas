unit Alcinoe.FMX.FMXOverNativeLayout;

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
  Macapi.ObjectiveC,
  iOSapi.CoreGraphics,
  iOSapi.UIKit,
  Alcinoe.FMX.NativeView.iOS,
  {$ELSEIF defined(ALMacOS)}
  System.TypInfo,
  Macapi.ObjectiveC,
  Macapi.CocoaTypes,
  Macapi.AppKit,
  Alcinoe.FMX.NativeView.Mac,
  {$ELSEIF defined(MSWINDOWS)}
  Winapi.Windows,
  Winapi.Messages,
  FMX.Controls.Win,
  Alcinoe.FMX.NativeView.Win,
  {$ENDIF}
  FMX.types,
  FMX.Controls,
  FMX.Graphics,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.NativeControl;

type

  //
  // TALFMXOverNativeLayout
  // ----------------------
  // A TALNativeControl whose purpose is to display regular FireMonkey controls
  // ABOVE native controls (TALMapLibre, TALWebBrowser, TALEdit, TALVideoPlayer,
  // etc.). Native views are always rendered on top of the FireMonkey form
  // surface, so a FireMonkey control placed over a native control is normally
  // hidden behind it. To work around this, each time TALFMXOverNativeLayout is
  // painted, its children are rendered (via PaintTo) into an offscreen bitmap,
  // and this bitmap is handed to a transparent native view (an ImageView on
  // Android, the CALayer contents on iOS/macOS, a layered child window on
  // Windows) that is kept on top of the other native views.
  //
  // Touches/clicks are forwarded to the FireMonkey form only when they land on
  // one of the FMX children (see IsFMXControlAtPoint). Everywhere else the
  // native view is transparent to input, so the native controls underneath
  // continue to receive their events.
  //
  // Limitations:
  //   * The children are rendered with their RotationAngle reset to 0
  //     (limitation of TControl.PaintTo).
  //   * On Windows, layered child windows require Windows 8 or later and an
  //     application manifest that declares Windows 8+ compatibility.
  //   * The capture is performed just after the paint (queued), so the native
  //     view lags one frame behind the FMX scene.
  //

{$REGION ' ANDROID'}
{$IF defined(android)}

  {**********************************************************}
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
    FTouchListener: TTouchListener;
    FForwardTouches: Boolean;
    FBitmap: JBitmap;
    FPixelBuffer: TBytes;
    function GetView: JImageView;
    function HandleTouch(const AEvent: JMotionEvent): Boolean;
    procedure BringToFront;
  protected
    function CreateView: JView; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure RootChanged(const aRoot: IRoot); override;
    function CaptureScreenshot: TALDrawable; override;
    procedure UpdateContent(const ABitmap: FMX.Graphics.TBitmap);
    property View: JImageView read GetView;
  end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

  {*****************************************************}
  IALIosFMXOverNativeView = interface(UIView)
    ['{0C4B6E8B-6B0B-4D2B-9A55-2C1E3F2C9D41}']
    function pointInside(point: CGPoint; withEvent: UIEvent): Boolean; cdecl;
  end;

  {******************************************************}
  TALIosFMXOverNativeView = class(TALIosNativeView)
  private
    procedure BringToFront;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create; override;
    function pointInside(point: CGPoint; withEvent: UIEvent): Boolean; cdecl;
    procedure RootChanged(const aRoot: IRoot); override;
    function CaptureScreenshot: TALDrawable; override;
    procedure UpdateContent(const ABitmap: FMX.Graphics.TBitmap);
  end;

{$ENDIF}
{$ENDREGION}

{$REGION ' MacOS'}
{$IF defined(ALMacOS)}

  {*****************************************************}
  IALMacFMXOverNativeView = interface(NSView)
    ['{6F0E3B1C-2D4A-4C7B-8E2F-5A9B7C1D3E6F}']
    function hitTest(aPoint: NSPoint): Pointer; cdecl;
    procedure mouseDown(theEvent: NSEvent); cdecl;
    procedure mouseDragged(theEvent: NSEvent); cdecl;
    procedure mouseUp(theEvent: NSEvent); cdecl;
  end;

  {******************************************************}
  TALMacFMXOverNativeView = class(TALMacNativeView)
  private
    function GetFormMousePos: TPointF;
    procedure BringToFront;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create; override;
    function hitTest(aPoint: NSPoint): Pointer; cdecl;
    procedure mouseDown(theEvent: NSEvent); cdecl;
    procedure mouseDragged(theEvent: NSEvent); cdecl;
    procedure mouseUp(theEvent: NSEvent); cdecl;
    procedure RootChanged(const aRoot: IRoot); override;
    function CaptureScreenshot: TALDrawable; override;
    procedure UpdateContent(const ABitmap: FMX.Graphics.TBitmap);
  end;

{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

  {******************************************************}
  TALWinFMXOverNativeView = class(TALWinNativeView)
  private
    FAlpha: Single;
    procedure BringToFront;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  public
    constructor Create; override;
    procedure SetEnabled(const value: Boolean); override;
    procedure SetAlpha(const Value: Single); override;
    procedure RootChanged(const aRoot: IRoot); override;
    function CaptureScreenshot: TALDrawable; override;
    procedure UpdateContent(const ABitmap: FMX.Graphics.TBitmap);
  end;

{$ENDIF}
{$ENDREGION}

  {*************************}
  [ComponentPlatforms($FFFF)]
  TALFMXOverNativeLayout = class(TALNativeControl, IControlTypeSupportable, IALNativeControl)
  private
    FCaptureBitmap: FMX.Graphics.TBitmap;
    FNativeViewContentUpdateQueued: Boolean;
    {$IF defined(android)}
    function GetNativeView: TALAndroidFMXOverNativeView;
    {$ELSEIF defined(IOS)}
    function GetNativeView: TALIosFMXOverNativeView;
    {$ELSEIF defined(ALMacOS)}
    function GetNativeView: TALMacFMXOverNativeView;
    {$ELSEIF defined(MSWindows)}
    function GetNativeView: TALWinFMXOverNativeView;
    {$ENDIF}
    function IsRenderingIntoNativeView: Boolean;
    procedure UpdateNativeViewContent;
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
    Procedure ShowNativeView; override;
    Procedure HideNativeView; override;
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
    /// <summary>
    ///   Returns True when AFormPoint (in form coordinates) lands on one of the
    ///   FMX children. The native view uses it to decide whether a touch must
    ///   be forwarded to the FMX form or left to the native views underneath.
    /// </summary>
    function IsFMXControlAtPoint(const AFormPoint: TPointF): Boolean; virtual;
  published
    //property Action;
    property Align;
    property Anchors;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property ClickSound;
    //property ClipChildren;
    //property ClipParent;
    //property Cursor;
    //property DisableFocusEffect;
    //property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    //property HitTest;
    //property Locked;
    property Margins;
    property Opacity;
    property Padding;
    //property PopupMenu;
    property Position;
    //property RotationAngle;
    //property RotationCenter;
    //property Pivot;
    //property Scale;
    property Size;
    //property TabOrder;
    //property TabStop;
    //property TouchTargetExpansion;
    property Visible;
    property Width;
    //property OnCanFocus;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    //property OnMouseEnter;
    //property OnMouseLeave;
    //property OnMouseDown;
    //property OnMouseUp;
    //property OnMouseMove;
    //property OnMouseWheel;
    //property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    //property OnPainting;
    //property OnPaint;
    //property OnResize;
    property OnResized;
  end;

procedure Register;

implementation

uses
  System.SysUtils,
  System.Math,
  System.UITypes,
  {$IF defined(android)}
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.Helpers,
  FMX.Platform.Android,
  FMX.Platform.UI.Android,
  {$ELSEIF defined(IOS)}
  iOSapi.CoreAnimation,
  Macapi.Helpers,
  {$ELSEIF defined(ALMacOS)}
  Macapi.CoreGraphics,
  Macapi.QuartzCore,
  Macapi.Helpers,
  {$ELSEIF defined(MSWINDOWS)}
  FMX.Platform.Win,
  {$ENDIF}
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  FMX.Forms,
  Alcinoe.Common,
  Alcinoe.FMX.Common;

{*****************************************************************************}
// Copy the pixels of ASrc (premultiplied RGBA or BGRA) into ADst. If ASrc and
// ADst don't have exactly the same dimensions, the extra area of ADst is
// cleared.
procedure _CopyPixels(
            const ASrc: TBitmapData;
            const ADst: Pointer;
            const ADstPitch: Integer;
            const ADstWidth, ADstHeight: Integer;
            const ADstBGRA: Boolean);
begin
  var LSrcBGRA: Boolean;
  case ASrc.PixelFormat of
    TPixelFormat.BGRA: LSrcBGRA := True;
    TPixelFormat.RGBA: LSrcBGRA := False;
    else raise Exception.Create('Error 8B6D1F0A-3C1E-4A8E-9E8A-6E0F5B2C7D14');
  end;
  var LWidth := Min(ASrc.Width, ADstWidth);
  for var Y := 0 to ADstHeight - 1 do begin
    var LDstRow := PByte(ADst) + (Y * ADstPitch);
    if Y >= ASrc.Height then begin
      FillChar(LDstRow^, ADstWidth * 4, 0);
      continue;
    end;
    var LSrcRow := ASrc.GetScanline(Y);
    if LSrcBGRA = ADstBGRA then
      ALMove(LSrcRow^, LDstRow^, LWidth * 4)
    else begin
      var LSrcPixel := PCardinal(LSrcRow);
      var LDstPixel := PCardinal(LDstRow);
      for var X := 0 to LWidth - 1 do begin
        var C := LSrcPixel^;
        LDstPixel^ := (C and $FF00FF00) or ((C and $000000FF) shl 16) or ((C shr 16) and $000000FF);
        inc(LSrcPixel);
        inc(LDstPixel);
      end;
    end;
    if LWidth < ADstWidth then
      FillChar((LDstRow + (LWidth * 4))^, (ADstWidth - LWidth) * 4, 0);
  end;
end;

{$REGION ' ANDROID'}
{$IF defined(android)}

{***********************************************************************************************************}
constructor TALAndroidFMXOverNativeView.TTouchListener.Create(const ANativeView: TALAndroidFMXOverNativeView);
begin
  inherited Create;
  FNativeView := ANativeView;
end;

{************************************************************************************************}
function TALAndroidFMXOverNativeView.TTouchListener.onTouch(v: JView; event: JMotionEvent): Boolean;
begin
  Result := FNativeView.HandleTouch(event);
end;

{*********************************************}
constructor TALAndroidFMXOverNativeView.Create;
begin
  inherited;
  FForwardTouches := False;
  FBitmap := nil;
  setlength(FPixelBuffer, 0);
  // The view must not catch the touches by itself. Only the touches
  // that land on an FMX child are consumed (see HandleTouch), all
  // others are dispatched to the native views underneath.
  View.setClickable(False);
  View.setFocusable(False);
  View.setFocusableInTouchMode(False);
  View.setScaleType(TJImageView_ScaleType.JavaClass.FIT_XY);
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

{***********************************************************}
function TALAndroidFMXOverNativeView.GetView: JImageView;
begin
  Result := inherited GetView<JImageView>;
end;

{********************************************************************}
procedure TALAndroidFMXOverNativeView.RootChanged(const aRoot: IRoot);
begin
  inherited;
  BringToFront;
end;

{**************************************************}
procedure TALAndroidFMXOverNativeView.BringToFront;
begin
  // TALAndroidNativeView.RootChanged always inserts the native views at index 1
  // (just above the form's view), so the last added native view lies below the
  // others. We must stay above all of them.
  var LParent: JViewParent := Layout.getParent;
  if LParent = nil then exit;
  var LParentGroup := TJViewGroup.Wrap(LParent);
  if LParentGroup.indexOfChild(Layout) <> LParentGroup.getChildCount - 1 then
    Layout.bringToFront;
end;

{******************************************************************}
function TALAndroidFMXOverNativeView.CaptureScreenshot: TALDrawable;
begin
  // When the native view is frozen, the FMX children are simply painted
  // on the form, so no screenshot is needed.
  Result := ALNullDrawable;
end;

{*************************************************************************************}
function TALAndroidFMXOverNativeView.HandleTouch(const AEvent: JMotionEvent): Boolean;
begin
  if Form = nil then exit(False);
  var LAction := AEvent.getActionMasked;
  var LScreenScale: Single := Form.Handle.Scale;
  if LAction = TJMotionEvent.JavaClass.ACTION_DOWN then
    FForwardTouches := TALFMXOverNativeLayout(Control).IsFMXControlAtPoint(
                         TPointF.Create(
                           (Layout.getX + AEvent.getX) / LScreenScale,
                           (Layout.getY + AEvent.getY) / LScreenScale));
  if not FForwardTouches then exit(False);
  // Forward the event to the FMX form. The coordinates of AEvent are relative
  // to our view, so translate them to be relative to the form's view.
  var LEvent := TJMotionEvent.JavaClass.obtain(AEvent);
  try
    LEvent.offsetLocation(Layout.getX, Layout.getY);
    WindowHandleToPlatform(Form.Handle).MotionManager.HandleMotionEvent(LEvent);
  finally
    LEvent.recycle;
  end;
  if (LAction = TJMotionEvent.JavaClass.ACTION_UP) or
     (LAction = TJMotionEvent.JavaClass.ACTION_CANCEL) then
    FForwardTouches := False;
  Result := True;
end;

{**********************************************************************************************}
procedure TALAndroidFMXOverNativeView.UpdateContent(const ABitmap: FMX.Graphics.TBitmap);
begin
  var LData: TBitmapData;
  if not ABitmap.Map(TMapAccess.Read, LData) then
    raise Exception.Create('Error 2F4C8E61-7B0D-4E5A-A1C3-9D6B2E8F0A57');
  try
    var LWidth := LData.Width;
    var LHeight := LData.Height;
    if length(FPixelBuffer) <> LWidth * LHeight * 4 then
      setlength(FPixelBuffer, LWidth * LHeight * 4);
    // JBitmap ARGB_8888 is stored in memory as premultiplied RGBA
    _CopyPixels(
      LData, // const ASrc: TBitmapData;
      @FPixelBuffer[0], // const ADst: Pointer;
      LWidth * 4, // const ADstPitch: Integer;
      LWidth, // const ADstWidth: Integer;
      LHeight, // const ADstHeight: Integer;
      False); // const ADstBGRA: Boolean
  finally
    ABitmap.Unmap(LData);
  end;
  var LWidth := ABitmap.Width;
  var LHeight := ABitmap.Height;
  // We don't recycle the previous bitmap because the RenderThread
  // can still reference it. The GC will take care of it.
  if (FBitmap = nil) or (FBitmap.getWidth <> LWidth) or (FBitmap.getHeight <> LHeight) then begin
    FBitmap := TJBitmap.JavaClass.createBitmap(LWidth, LHeight, TJBitmap_Config.JavaClass.ARGB_8888);
    View.setImageBitmap(FBitmap);
  end;
  var LEnv: PJNIEnv := TJNIResolver.GetJNIEnv;
  var LBufferObj: JNIObject := LEnv^.NewDirectByteBuffer(LEnv, @FPixelBuffer[0], length(FPixelBuffer));
  if LBufferObj = nil then
    raise Exception.Create('Error 5A9E3C27-D41B-4F86-B0E2-7C3F1A6D8B95');
  try
    // copyPixelsFromBuffer calls notifyPixelsChanged, so the
    // RenderThread will upload the new content on the next draw
    FBitmap.copyPixelsFromBuffer(TJByteBuffer.Wrap(LBufferObj));
  finally
    LEnv^.DeleteLocalRef(LEnv, LBufferObj);
  end;
  View.invalidate;
  BringToFront;
end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

{*****************************************}
constructor TALIosFMXOverNativeView.Create;
begin
  inherited;
  // The FMX children can be touched at the same time as the
  // native views underneath (e.g. a map)
  View.setExclusiveTouch(False);
  View.setOpaque(False);
  View.setBackgroundColor(TUIColor.OCClass.clearColor);
end;

{*************************************************************}
function TALIosFMXOverNativeView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALIosFMXOverNativeView);
end;

{***************************************************************************************}
function TALIosFMXOverNativeView.pointInside(point: CGPoint; withEvent: UIEvent): Boolean;
begin
  // Returning False lets UIKit deliver the touch to the views
  // underneath (i.e. the other native views or the FMX form view)
  Result := UIView(Super).pointInside(point, withEvent) and
            (Form <> nil) and
            TALFMXOverNativeLayout(Control).IsFMXControlAtPoint(
              TALFMXOverNativeLayout(Control).GetNativeViewAbsoluteRect.TopLeft +
              TPointF.Create(point.x, point.y));
end;

{****************************************************************}
procedure TALIosFMXOverNativeView.RootChanged(const aRoot: IRoot);
begin
  inherited;
  BringToFront;
end;

{**********************************************}
procedure TALIosFMXOverNativeView.BringToFront;
begin
  // TALIosNativeView.RootChanged always inserts the native views at index 0,
  // so the last added native view lies below the others. We must stay above
  // all of them.
  if Form = nil then exit;
  var LFormView := GetFormView(Form);
  if LFormView.subviews.lastObject <> GetObjectID then
    LFormView.bringSubviewToFront(View);
end;

{**************************************************************}
function TALIosFMXOverNativeView.CaptureScreenshot: TALDrawable;
begin
  // When the native view is frozen, the FMX children are simply painted
  // on the form, so no screenshot is needed.
  Result := ALNullDrawable;
end;

{******************************************************************************************}
procedure TALIosFMXOverNativeView.UpdateContent(const ABitmap: FMX.Graphics.TBitmap);
begin
  var LData: TBitmapData;
  if not ABitmap.Map(TMapAccess.Read, LData) then
    raise Exception.Create('Error 7E1B4D93-2A6C-4F08-8B5E-D3C9A0F61E28');
  try
    var LCGContextRef := ALCreateCGContextRef(LData.Width, LData.Height);
    try
      _CopyPixels(
        LData, // const ASrc: TBitmapData;
        CGBitmapContextGetData(LCGContextRef), // const ADst: Pointer;
        CGBitmapContextGetBytesPerRow(LCGContextRef), // const ADstPitch: Integer;
        LData.Width, // const ADstWidth: Integer;
        LData.Height, // const ADstHeight: Integer;
        GlobalUseMetal); // const ADstBGRA: Boolean (see ALCreateCGContextRef)
      var LCGImageRef := CGBitmapContextCreateImage(LCGContextRef);
      if LCGImageRef = nil then
        raise Exception.Create('Error 3D8A5F17-C62E-4B90-9F41-E7B2D0C5A386');
      try
        // The layer retains the image
        View.layer.setContents(LCGImageRef);
      finally
        CGImageRelease(LCGImageRef);
      end;
    finally
      CGContextRelease(LCGContextRef);
    end;
  finally
    ABitmap.Unmap(LData);
  end;
  BringToFront;
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

{****************************************************************}
function TALMacFMXOverNativeView.GetFormMousePos: TPointF;
begin
  Result := Form.ScreenToClient(Screen.MousePos);
end;

{************************************************************************}
function TALMacFMXOverNativeView.hitTest(aPoint: NSPoint): Pointer;
begin
  // aPoint is in the coordinate system of the superview (the form's view).
  // Returning nil lets AppKit deliver the click to the views underneath.
  Result := nil;
  if Form = nil then exit;
  var LFormView := GetFormView(Form);
  var LFormPoint := TPointF.Create(aPoint.x, aPoint.y);
  if not LFormView.isFlipped then
    LFormPoint.Y := LFormView.frame.size.height - LFormPoint.Y;
  if not TALFMXOverNativeLayout(Control).IsFMXControlAtPoint(LFormPoint) then exit;
  var LView := NSView(Super).hitTest(aPoint);
  if LView <> nil then
    Result := NSObjectToID(LView);
end;

{************************************************************************}
procedure TALMacFMXOverNativeView.mouseDown(theEvent: NSEvent);
begin
  if Form = nil then exit;
  var LFormPoint := GetFormMousePos;
  Form.MouseMove([], LFormPoint.X, LFormPoint.Y); // Require for correct IsMouseOver handle
  Form.MouseDown(TMouseButton.mbLeft, [ssLeft], LFormPoint.X, LFormPoint.Y);
end;

{************************************************************************}
procedure TALMacFMXOverNativeView.mouseDragged(theEvent: NSEvent);
begin
  if Form = nil then exit;
  var LFormPoint := GetFormMousePos;
  Form.MouseMove([ssLeft], LFormPoint.X, LFormPoint.Y);
end;

{************************************************************************}
procedure TALMacFMXOverNativeView.mouseUp(theEvent: NSEvent);
begin
  if Form = nil then exit;
  var LFormPoint := GetFormMousePos;
  Form.MouseUp(TMouseButton.mbLeft, [ssLeft], LFormPoint.X, LFormPoint.Y);
  // Don't combine these if statements because in MouseUp the form can unload presentation!
  if Form <> nil then
    Form.MouseLeave;
end;

{****************************************************************}
procedure TALMacFMXOverNativeView.RootChanged(const aRoot: IRoot);
begin
  inherited;
  BringToFront;
end;

{**********************************************}
procedure TALMacFMXOverNativeView.BringToFront;
begin
  // TALMacNativeView.RootChanged adds the native views on top of the others,
  // so a native view added after us would cover us.
  if Form = nil then exit;
  var LFormView := GetFormView(Form);
  if LFormView.subviews.lastObject <> GetObjectID then begin
    View.removeFromSuperview;
    LFormView.addSubview(View);
  end;
end;

{**************************************************************}
function TALMacFMXOverNativeView.CaptureScreenshot: TALDrawable;
begin
  Result := ALNullDrawable;
end;

{******************************************************************************************}
procedure TALMacFMXOverNativeView.UpdateContent(const ABitmap: FMX.Graphics.TBitmap);
begin
  var LData: TBitmapData;
  if not ABitmap.Map(TMapAccess.Read, LData) then
    raise Exception.Create('Error 9C2E6A48-B13F-4D75-A0E8-1F5D7B3C9E62');
  try
    var LCGContextRef := ALCreateCGContextRef(LData.Width, LData.Height);
    try
      _CopyPixels(
        LData, // const ASrc: TBitmapData;
        CGBitmapContextGetData(LCGContextRef), // const ADst: Pointer;
        CGBitmapContextGetBytesPerRow(LCGContextRef), // const ADstPitch: Integer;
        LData.Width, // const ADstWidth: Integer;
        LData.Height, // const ADstHeight: Integer;
        GlobalUseMetal); // const ADstBGRA: Boolean (see ALCreateCGContextRef)
      var LCGImageRef := CGBitmapContextCreateImage(LCGContextRef);
      if LCGImageRef = nil then
        raise Exception.Create('Error 4B7F2D85-E90A-4C36-8D1B-A6E3F5C02B79');
      try
        // The layer retains the image
        View.layer.setContents(LCGImageRef);
      finally
        CGImageRelease(LCGImageRef);
      end;
    finally
      CGContextRelease(LCGContextRef);
    end;
  finally
    ABitmap.Unmap(LData);
  end;
  BringToFront;
end;

{$ENDIF}
{$ENDREGION}

{$REGION ' MSWINDOWS'}
{$IF defined(MSWINDOWS)}

{*****************************************}
constructor TALWinFMXOverNativeView.Create;
begin
  inherited;
  FAlpha := 1;
end;

{************************************************************************}
procedure TALWinFMXOverNativeView.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // A layered window is not drawn until UpdateLayeredWindow is called and
  // supports per-pixel alpha. Child layered windows require Windows 8+.
  Params.ExStyle := Params.ExStyle or WS_EX_LAYERED or WS_EX_NOACTIVATE;
  Params.Style := Params.Style and not WS_TABSTOP;
end;

{****************************************************************************}
procedure TALWinFMXOverNativeView.WMNCHitTest(var Message: TWMNCHitTest);
begin
  // HTTRANSPARENT lets Windows deliver the mouse message to the window
  // underneath (i.e. the other native views or the FMX form)
  if (Form = nil) or
     (csDesigning in Control.ComponentState) or
     (not Form.IsHandleAllocated) then begin
    Message.Result := HTTRANSPARENT;
    exit;
  end;
  var LPoint := TPoint.Create(Message.XPos, Message.YPos); // px
  ScreenToClient(FormToHWND(Form), LPoint); // px
  var LFormPoint := TPointF.Create(LPoint.X / Form.Handle.Scale, LPoint.Y / Form.Handle.Scale); // dp
  if TALFMXOverNativeLayout(Control).IsFMXControlAtPoint(LFormPoint) then
    inherited
  else
    Message.Result := HTTRANSPARENT;
end;

{***********************************************************************}
procedure TALWinFMXOverNativeView.SetEnabled(const value: Boolean);
begin
  // We never disable the window, otherwise it will swallow the mouse clicks
  // instead of letting them reach the windows underneath. The enabled state is
  // handled by TALFMXOverNativeLayout.IsFMXControlAtPoint.
end;

{********************************************************************}
procedure TALWinFMXOverNativeView.SetAlpha(const Value: Single);
begin
  // Applied on the next UpdateContent
  FAlpha := Value;
end;

{****************************************************************}
procedure TALWinFMXOverNativeView.RootChanged(const aRoot: IRoot);
begin
  inherited;
  BringToFront;
end;

{**********************************************}
procedure TALWinFMXOverNativeView.BringToFront;
begin
  if Form = nil then exit;
  SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
end;

{**************************************************************}
function TALWinFMXOverNativeView.CaptureScreenshot: TALDrawable;
begin
  Result := ALNullDrawable;
end;

{******************************************************************************************}
procedure TALWinFMXOverNativeView.UpdateContent(const ABitmap: FMX.Graphics.TBitmap);
begin
  var LData: TBitmapData;
  if not ABitmap.Map(TMapAccess.Read, LData) then
    raise Exception.Create('Error 1E5C9B36-F72D-4A08-B3E6-8D0A4F7C2E91');
  try
    var LWidth := LData.Width;
    var LHeight := LData.Height;
    var LBitmapInfo: Winapi.Windows.TBitmapInfo;
    FillChar(LBitmapInfo, SizeOf(LBitmapInfo), 0);
    LBitmapInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
    LBitmapInfo.bmiHeader.biWidth := LWidth;
    LBitmapInfo.bmiHeader.biHeight := -LHeight; // top-down
    LBitmapInfo.bmiHeader.biPlanes := 1;
    LBitmapInfo.bmiHeader.biBitCount := 32;
    LBitmapInfo.bmiHeader.biCompression := BI_RGB;
    var LBits: Pointer := nil;
    var LDIB := CreateDIBSection(0, LBitmapInfo, DIB_RGB_COLORS, LBits, 0, 0);
    if LDIB = 0 then RaiseLastOSError;
    try
      // UpdateLayeredWindow expects premultiplied BGRA
      _CopyPixels(
        LData, // const ASrc: TBitmapData;
        LBits, // const ADst: Pointer;
        LWidth * 4, // const ADstPitch: Integer;
        LWidth, // const ADstWidth: Integer;
        LHeight, // const ADstHeight: Integer;
        True); // const ADstBGRA: Boolean
      var LDC := CreateCompatibleDC(0);
      if LDC = 0 then RaiseLastOSError;
      try
        var LOldBitmap := SelectObject(LDC, LDIB);
        try
          var LSize := TSize.Create(LWidth, LHeight);
          var LSrcPoint := TPoint.Create(0, 0);
          var LBlend: TBlendFunction;
          LBlend.BlendOp := AC_SRC_OVER;
          LBlend.BlendFlags := 0;
          LBlend.SourceConstantAlpha := EnsureRange(Round(FAlpha * 255), 0, 255);
          LBlend.AlphaFormat := AC_SRC_ALPHA;
          if not UpdateLayeredWindow(
                   Handle, // hWnd: HWND;
                   0, // hdcDst: HDC;
                   nil, // pptDst: PPoint; (keep the current position)
                   @LSize, // psize: PSize;
                   LDC, // hdcSrc: HDC;
                   @LSrcPoint, // pptSrc: PPoint;
                   0, // crKey: COLORREF;
                   @LBlend, // pblend: PBlendFunction;
                   ULW_ALPHA) then // dwFlags: DWORD
            RaiseLastOSError;
        finally
          SelectObject(LDC, LOldBitmap);
        end;
      finally
        DeleteDC(LDC);
      end;
    finally
      DeleteObject(LDIB);
    end;
  finally
    ABitmap.Unmap(LData);
  end;
  BringToFront;
end;

{$ENDIF}
{$ENDREGION}

{************************************************************}
constructor TALFMXOverNativeLayout.Create(AOwner: TComponent);
begin
  inherited;
  FCaptureBitmap := nil;
  FNativeViewContentUpdateQueued := False;
  // The layout itself must be transparent to the touches/clicks.
  // Only its children can be hit.
  HitTest := False;
  CanFocus := False;
  TabStop := False;
end;

{****************************************}
destructor TALFMXOverNativeLayout.Destroy;
begin
  TThread.RemoveQueuedEvents(nil, UpdateNativeViewContent);
  ALFreeAndNil(FCaptureBitmap);
  inherited;
end;

{********************}
{$IF defined(android)}
Function TALFMXOverNativeLayout.CreateNativeView: TALAndroidNativeView;
begin
  result := TALAndroidFMXOverNativeView.create(self);
end;
{$ENDIF}

{********************}
{$IF defined(android)}
function TALFMXOverNativeLayout.GetNativeView: TALAndroidFMXOverNativeView;
begin
  result := TALAndroidFMXOverNativeView(inherited NativeView);
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
Function TALFMXOverNativeLayout.CreateNativeView: TALIosNativeView;
begin
  result := TALIosFMXOverNativeView.create(self);
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function TALFMXOverNativeLayout.GetNativeView: TALIosFMXOverNativeView;
begin
  result := TALIosFMXOverNativeView(inherited NativeView);
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
Function TALFMXOverNativeLayout.CreateNativeView: TALMacNativeView;
begin
  result := TALMacFMXOverNativeView.create(self);
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
function TALFMXOverNativeLayout.GetNativeView: TALMacFMXOverNativeView;
begin
  result := TALMacFMXOverNativeView(inherited NativeView);
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
Function TALFMXOverNativeLayout.CreateNativeView: TALWinNativeView;
begin
  // At design time, the children are simply painted on the form
  {$IF defined(ALDPK)}
  Result := nil;
  {$ELSE}
  result := TALWinFMXOverNativeView.create(self);
  {$ENDIF}
end;
{$ENDIF}

{**********************}
{$IF defined(MSWindows)}
function TALFMXOverNativeLayout.GetNativeView: TALWinFMXOverNativeView;
begin
  result := TALWinFMXOverNativeView(inherited NativeView);
end;
{$ENDIF}

{****************************************************************}
function TALFMXOverNativeLayout.GetNativeViewBoundsRect: TRectF;
begin
  // Unlike other native controls, the children are not laid out around
  // the native view: they are the content of the native view.
  Result := NativeViewMargins.PaddingRect(LocalRect);
end;

{*************************************************************************************}
function TALFMXOverNativeLayout.IsFMXControlAtPoint(const AFormPoint: TPointF): Boolean;
begin
  if (csDesigning in ComponentState) or
     (not AbsoluteEnabled) or
     (not IsRenderingIntoNativeView) or
     (Root = nil) or
     (not (Root.GetObject is TCommonCustomForm)) then exit(False);
  var LObject := ObjectAtPoint(TCommonCustomForm(Root.GetObject).ClientToScreen(AFormPoint));
  // HitTest is False, so ObjectAtPoint can return Self only if someone
  // changed HitTest. In this case the full layout catches the touches.
  Result := LObject <> nil;
end;

{*******************************************************************}
function TALFMXOverNativeLayout.IsRenderingIntoNativeView: Boolean;
begin
  // When the native view is hidden (for instance frozen because a dialog is
  // displayed above), the children are painted as usual on the form.
  Result := (not (csDesigning in ComponentState)) and
            IsNativeViewVisible;
end;

{**********************************************}
Procedure TALFMXOverNativeLayout.ShowNativeView;
begin
  var LWasVisible := IsNativeViewVisible;
  inherited;
  if (not LWasVisible) and IsNativeViewVisible then begin
    // Refresh the content immediately so that the native view doesn't
    // display an outdated image, and repaint to remove the children
    // from the form surface.
    UpdateNativeViewContent;
    Repaint;
  end;
end;

{**********************************************}
Procedure TALFMXOverNativeLayout.HideNativeView;
begin
  var LWasVisible := IsNativeViewVisible;
  inherited;
  // The children must now be painted on the form surface
  if LWasVisible and (not IsNativeViewVisible) then
    Repaint;
end;

{*********************************************}
procedure TALFMXOverNativeLayout.PaintChildren;
begin
  if not IsRenderingIntoNativeView then begin
    inherited;
    exit;
  end;
  // The children are not painted on the form surface (they would be
  // hidden by the native views and/or painted twice). Instead, they are
  // rendered into an offscreen bitmap that is handed to the native view.
  // We don't render them here because we are in the middle of the form's
  // scene, and drawing to another canvas while a scene is active is not
  // supported by all the canvas implementations.
  if not FNativeViewContentUpdateQueued then begin
    FNativeViewContentUpdateQueued := True;
    TThread.ForceQueue(nil, UpdateNativeViewContent);
  end;
end;

{*******************************************************}
procedure TALFMXOverNativeLayout.UpdateNativeViewContent;
begin
  FNativeViewContentUpdateQueued := False;
  if (FNativeView = nil) or
     (not IsRenderingIntoNativeView) or
     (not ParentedVisible) then exit;

  // Compute the size in pixels exactly like TALXXXNativeView.UpdateFrame
  var LBoundsRect := GetNativeViewBoundsRect;
  var LAbsoluteRect := LocalToAbsolute(LBoundsRect);
  var LScale := ALGetScreenScale;
  var LPixelRect := TRectF.Create(
                      LAbsoluteRect.Left * LScale,
                      LAbsoluteRect.Top * LScale,
                      LAbsoluteRect.Right * LScale,
                      LAbsoluteRect.Bottom * LScale).Round;
  if (LPixelRect.Width <= 0) or (LPixelRect.Height <= 0) then exit;

  if FCaptureBitmap = nil then begin
    FCaptureBitmap := FMX.Graphics.TBitmap.Create;
    FCaptureBitmap.BitmapScale := LScale;
  end;
  if (FCaptureBitmap.Width <> LPixelRect.Width) or
     (FCaptureBitmap.Height <> LPixelRect.Height) then
    FCaptureBitmap.SetSize(LPixelRect.Width, LPixelRect.Height);

  if not FCaptureBitmap.Canvas.BeginScene then exit;
  try
    FCaptureBitmap.Canvas.Clear(TAlphaColors.Null);
    for var I := 0 to ControlsCount - 1 do begin
      var LControl := Controls[I];
      if not LControl.Visible then continue;
      // PaintTo uses ARect (expressed in the bitmap's logical coordinates)
      // as the position and the scaled size of the control.
      LControl.PaintTo(
        FCaptureBitmap.Canvas,
        TRectF.Create(
          TPointF.Create(
            LControl.Position.X - LBoundsRect.Left,
            LControl.Position.Y - LBoundsRect.Top),
          LControl.Width * LControl.Scale.X,
          LControl.Height * LControl.Scale.Y));
    end;
  finally
    FCaptureBitmap.Canvas.EndScene;
  end;

  NativeView.UpdateContent(FCaptureBitmap);
end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALFMXOverNativeLayout]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALFMXOverNativeLayout, 'Size');
  UnlistPublishedProperty(TALFMXOverNativeLayout, 'StyleName');
  UnlistPublishedProperty(TALFMXOverNativeLayout, 'OnTap');
  {$ENDIF}
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.FMXOverNativeLayout','initialization');
  {$ENDIF}
  RegisterFmxClasses([TALFMXOverNativeLayout]);

end.
