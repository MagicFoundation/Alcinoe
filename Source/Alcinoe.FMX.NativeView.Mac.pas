unit Alcinoe.FMX.NativeView.Mac;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported132}
  {$MESSAGE WARN 'Check if FMX.Presentation.Mac.pas EXIST and adjust the IFDEF'}
{$ENDIF}

uses
  System.Types,
  system.Messaging,
  System.TypInfo,
  Macapi.ObjectiveC,
  MacApi.AppKit,
  Macapi.CoreGraphics,
  Macapi.CocoaTypes,
  FMX.Controls,
  FMX.Forms,
  FMX.Types,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Graphics;

type

  {********************************}
  TALMacNativeView = class(TOCLocal)
  private
    FControl: TALControl;
    FVisible: Boolean;
    FCaptureContext: CGContextRef;
    function GetView: NSView; overload;
    procedure BeforeDestroyFormHandleListener(const Sender: TObject; const AMessage: TMessage);
    procedure AfterCreateFormHandleListener(const Sender: TObject; const AMessage: TMessage);
    procedure FormSizeChanged(const Sender: TObject; const AMessage: TMessage);
  protected
    procedure InitView; virtual;
    function GetFormView: NSView;
    function GetObjectiveCClass: PTypeInfo; override;
  public
    function GetAbsoluteRect: TRectF;
    procedure SetEnabled(const value: Boolean); virtual; //procedure PMSetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_ABSOLUTE_ENABLED;
    procedure SetVisible(const Value: Boolean); virtual; //procedure PMSetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_VISIBLE;
    procedure SetAlpha(const Value: Single); virtual; //procedure PMSetAlpha(var AMessage: TDispatchMessageWithValue<Single>); message PM_SET_ABSOLUTE_OPACITY;
    procedure BringToFront; virtual;
    function acceptsFirstResponder: Boolean; virtual; cdecl;
    function becomeFirstResponder: Boolean; virtual; cdecl;
  protected
    function GetView<T: NSView>: T; overload;
  public
    constructor Create; overload; virtual;
    constructor Create(const AControl: TALControl); overload; virtual;
    destructor Destroy; override;
    procedure SetFocus; virtual;
    procedure ResetFocus; virtual;
    procedure UpdateParent; virtual;
    procedure UpdateFrame; virtual;
    function CaptureScreenshot: TALDrawable; virtual;
    property Control: TALControl read FControl;
    property View: NSView read GetView;
    property Visible: Boolean read FVisible;
  end;
  TALMacNativeViewClass = class of TALMacNativeView;

implementation

uses
  System.SysUtils,
  {$IF defined(ALSkiaCanvas)}
  System.Skia.API,
  {$ENDIF}
  Macapi.Helpers,
  FMX.Platform.Mac,
  Alcinoe.Common,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.NativeControl;

{**********************************}
constructor TALMacNativeView.Create;
begin
  inherited;
  InitView;
  FVisible := True;
  FCaptureContext := nil;
  TMessageManager.DefaultManager.SubscribeToMessage(TBeforeDestroyFormHandle, BeforeDestroyFormHandleListener);
  TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, AfterCreateFormHandleListener);
  TMessageManager.DefaultManager.SubscribeToMessage(TSizeChangedMessage, FormSizeChanged);
end;

{**************************************************************}
constructor TALMacNativeView.Create(const AControl: TALControl);
begin
  FControl := AControl;
  Create;
  UpdateParent;
end;

{**********************************}
destructor TALMacNativeView.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TBeforeDestroyFormHandle, BeforeDestroyFormHandleListener);
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, AfterCreateFormHandleListener);
  TMessageManager.DefaultManager.Unsubscribe(TSizeChangedMessage, FormSizeChanged);

  var LNativeChildControls := TALNativeControl(Control).GetNativeChildControls;
  for var I := Low(LNativeChildControls) to High(LNativeChildControls) do
    LNativeChildControls[I].NativeView.View.removeFromSuperview;

  View.removeFromSuperview;

  if FCaptureContext <> nil then begin
    CGContextRelease(FCaptureContext);
    FCaptureContext := nil;
  end;

  inherited;
end;

{**********************************************************************************************************}
procedure TALMacNativeView.BeforeDestroyFormHandleListener(const Sender: TObject; const AMessage: TMessage);
begin
  if (AMessage is TBeforeDestroyFormHandle) and
     (TBeforeDestroyFormHandle(AMessage).Value = Control.Form) then begin
    var LParent := view.superview;
    if (LParent <> nil) and
       (SameObjects(TNSView.Wrap(LParent), GetFormView)) then begin
      {$IF defined(DEBUG)}
      ALLog(Classname + '.BeforeDestroyFormHandleListener');
      {$ENDIF}
      View.removeFromSuperview;
    end;
  end;
end;

{********************************************************************************************************}
procedure TALMacNativeView.AfterCreateFormHandleListener(const Sender: TObject; const AMessage: TMessage);
begin
  // This event is called only when the window's handle is recreated.
  if (AMessage is TAfterCreateFormHandle) and
     (TAfterCreateFormHandle(AMessage).Value = Control.Form) then begin
    {$IF defined(DEBUG)}
    ALLog(Classname + '.AfterCreateFormHandleListener');
    {$ENDIF}
    UpdateParent;
  end;
end;

{******************************************************************************************}
procedure TALMacNativeView.FormSizeChanged(const Sender: TObject; const AMessage: TMessage);
begin
  // If the form size changes, we need to reposition the view because the
  // origin is at the bottom/left instead of the top/left.
  if (Sender = Control.Form) then
    UpdateFrame;
end;

{**********************************}
procedure TALMacNativeView.InitView;
begin
  var LAbsoluteRect := GetAbsoluteRect;
  var LGridHeight: Single;
  if Control.Form <> nil then LGridHeight := GetFormView.frame.size.height
  else LGridHeight := 0;
  var V: Pointer := NSView(Super).initWithFrame(
                      ALLowerLeftCGRect(
                        LAbsoluteRect.TopLeft,
                        LAbsoluteRect.Width,
                        LAbsoluteRect.Height,
                        LGridHeight));
  if GetObjectID <> V then
    UpdateObjectID(V);
end;

{******************************************************}
function TALMacNativeView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(NSView);
end;

{********************************************}
function TALMacNativeView.GetFormView: NSView;
begin
  var LFormHandle: TMacWindowHandle := WindowHandleToPlatform(Control.Form.Handle);
  Result := LFormHandle.View;
end;

{****************************************}
function TALMacNativeView.GetView: NSView;
begin
  Result := GetView<NSView>;
end;

{**************************************}
function TALMacNativeView.GetView<T>: T;
begin
  Result := T(Super);
end;

{************************************************}
function TALMacNativeView.GetAbsoluteRect: TRectF;
begin
  Result := TALNativeControl(Control).GetNativeViewAbsoluteRect;
end;

{**************************************}
procedure TALMacNativeView.UpdateParent;
begin

  {$IF defined(DEBUG)}
  ALLog(Classname + '.UpdateParent');
  {$ENDIF}

  var LParentView: NSView;
  if Control.Form = nil then LParentView := nil
  else LParentView := GetFormView;

  var LParentControl := Control.ParentControl;
  while LParentControl <> nil do begin
    if LParentControl is TALNativeControl then begin
      if not TALNativeControl(LParentControl).HasNativeView then exit;
      LParentView := TALNativeControl(LParentControl).NativeView.View;
      break;
    end;
    LParentControl := LParentControl.ParentControl;
  end;

  var LCurrentParent := view.superview;
  if ((LCurrentParent = nil) and
      (LParentView = nil)) or
     ((LCurrentParent <> nil) and
      (LParentView <> nil) and
      (SameObjects(TNSView.Wrap(LCurrentParent), LParentView))) then exit;

  if LCurrentParent <> nil then
    View.removeFromSuperview;

  if LParentView <> nil then begin
    LParentView.addSubview(View);
    UpdateFrame;
  end;

end;

{*************************************}
procedure TALMacNativeView.UpdateFrame;
begin
  {$IF defined(DEBUG)}
  //ALLog(Classname + '.UpdateFrame');
  {$ENDIF}
  var LParentView := View.superview;
  if LParentView = nil then exit;
  var LAbsoluteRect := GetAbsoluteRect;
  var LParentControl := Control.ParentControl;
  while LParentControl <> nil do begin
    if LParentControl is TALNativeControl then begin
      if not TALNativeControl(LParentControl).HasNativeView then exit;
      var LParentRect := TALNativeControl(LParentControl).GetNativeViewAbsoluteRect;
      LAbsoluteRect.Offset(-LParentRect.Left, -LParentRect.Top);
      break;
    end;
    LParentControl := LParentControl.ParentControl;
  end;
  View.setFrame(
    ALLowerLeftCGRect(
      LAbsoluteRect.TopLeft,
      LAbsoluteRect.Width,
      LAbsoluteRect.Height,
      TNSView.Wrap(LParentView).bounds.size.height));
end;

{*******************************************************}
function TALMacNativeView.CaptureScreenshot: TALDrawable;
begin
  var LBounds := View.bounds;
  var LScale: Single := ALGetScreenScale;
  var LWidth: Integer := Round(LBounds.size.width * LScale);
  var LHeight: Integer := Round(LBounds.size.height * LScale);
  if (LWidth <= 0) or (LHeight <= 0) then exit(ALNullDrawable);

  // Reuse the same bitmap context between captures (like FCaptureBitmap /
  // FCaptureCanvas on Android). It is recreated only when the size changes.
  if (FCaptureContext = nil) or
     (Integer(CGBitmapContextGetWidth(FCaptureContext)) <> LWidth) or
     (Integer(CGBitmapContextGetHeight(FCaptureContext)) <> LHeight) then begin
    if FCaptureContext <> nil then begin
      CGContextRelease(FCaptureContext);
      FCaptureContext := nil;
    end;
    FCaptureContext := ALCreateCGContextRef(LWidth, LHeight);
  end;

  // Render the view and all its subviews (the child native views are
  // subviews of View) directly into our bitmap context. AppKit handles
  // flipped views itself, so only the backing scale is applied here.
  CGContextClearRect(FCaptureContext, CGRectMake(0, 0, LWidth, LHeight));
  CGContextSaveGState(FCaptureContext);
  try
    CGContextScaleCTM(FCaptureContext, LScale, LScale);
    var LGraphicsContext := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.graphicsContextWithGraphicsPort(FCaptureContext, False{flipped}));
    if LGraphicsContext = nil then exit(ALNullDrawable);
    View.displayRectIgnoringOpacity(LBounds, LGraphicsContext);
  finally
    CGContextRestoreGState(FCaptureContext);
  end;

  {$IF defined(ALSkiaCanvas)}
  // The SkImage must own its pixels because FCaptureContext will be
  // overwritten by the next capture, so copy them into a new buffer.
  var LBytesPerRow: Integer := CGBitmapContextGetBytesPerRow(FCaptureContext);
  var LBufferSize := LBytesPerRow * LHeight;
  var LPixelData: Pointer;
  GetMem(LPixelData, LBufferSize);
  try
    ALMove(CGBitmapContextGetData(FCaptureContext)^, LPixelData^, LBufferSize);
    var LImageInfo := ALGetSkImageinfo(LWidth, LHeight);
    var LPixmap: sk_pixmap_t := ALSkCheckHandle(
                                  sk4d_pixmap_create(
                                    @LImageInfo,
                                    LPixelData,
                                    LBytesPerRow));
    try
      Result := ALSkCheckHandle(
                  sk4d_image_make_from_raster(
                    LPixmap, // const pixmap: sk_pixmap_t;
                    @ALReleaseMemPixelBufferProc, // proc: sk_image_raster_release_proc;
                    LPixelData)); // proc_context: Pointer
    finally
      sk4d_refcnt_unref(LPixmap);
    end;
  except
    FreeMem(LPixelData);
    raise;
  end;
  {$ELSE}
  Result := ALCreateTBitmapFromCGContextRef(FCaptureContext);
  {$ENDIF}
end;

{**************************************}
procedure TALMacNativeView.BringToFront;
begin
  var LSuperview := View.superview;
  if LSuperview <> nil then begin
    View.removeFromSuperview;
    TNSView.Wrap(LSuperview).addSubview(View);
  end;
end;

{**********************************************************}
procedure TALMacNativeView.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  View.setHidden(not Visible or not Control.ParentedVisible);
end;

{**********************************************************}
procedure TALMacNativeView.SetEnabled(const Value: Boolean);
begin
  // In macOS development using NSView from the AppKit framework, there isn't
  // a direct one-to-one equivalent to UIKit's setUserInteractionEnabled
  // property found in UIView. In iOS, setUserInteractionEnabled enables or
  // disables user interactions on a view. In macOS, user interaction is
  // generally controlled at a different level due to the nature of desktop
  // applications, which assume most elements are interactive unless
  // specifically disabled.
  //
  // Disabling Controls: For controls like buttons, checkboxes, text fields,
  // etc., you use the isEnabled property to control whether they can be
  // interacted with by the user.
end;

{*******************************************************}
procedure TALMacNativeView.SetAlpha(const Value: Single);
begin
  View.setAlphaValue(Value);
end;

{**********************************}
procedure TALMacNativeView.SetFocus;
begin
  // See remarks in TALMacNativeView.ResetFocus;
  //if View.window.FirstResponder <> View then
  //  View.becomeFirstResponder;
  if (View <> nil) and (View.window <> nil) and (View.window.FirstResponder <> View) then
    View.window.makeFirstResponder(View);
end;

{************************************}
procedure TALMacNativeView.ResetFocus;
begin
  // I have sometime this error with View.resignFirstResponder
  // Project Project1 raised exception class 6.
  //View.resignFirstResponder;
  if (View <> nil) and (View.window <> nil) then
    View.window.makeFirstResponder(nil);
end;

{*******************************************************}
function TALMacNativeView.acceptsFirstResponder: Boolean;
begin
  {$IF defined(DEBUG)}
  //ALLog(classname + '.acceptsFirstResponder', 'control.name: ' + Control.Name);
  {$ENDIF}
  Result := NSView(Super).acceptsFirstResponder and Control.canFocus;
end;

{******************************************************}
function TALMacNativeView.becomeFirstResponder: Boolean;
begin
  {$IF defined(DEBUG)}
  //ALLog(classname + '.becomeFirstResponder', 'control.name: ' + Control.Name);
  {$ENDIF}
  Result := NSView(Super).becomeFirstResponder;
  if (not Control.IsFocused) then
    Control.SetFocus;
end;

end.