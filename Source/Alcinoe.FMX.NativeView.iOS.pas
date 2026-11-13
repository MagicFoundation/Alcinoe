unit Alcinoe.FMX.NativeView.iOS;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported132}
  {$MESSAGE WARN 'Check if FMX.Presentation.iOS.pas was not updated and adjust the IFDEF'}
{$ENDIF}

uses
  system.Messaging,
  System.TypInfo,
  System.Types,
  Macapi.ObjectiveC,
  iOSapi.UIKit,
  iOSapi.Foundation,
  iOSapi.CoreGraphics,
  FMX.Controls,
  FMX.Forms,
  FMX.ZOrder.iOS,
  FMX.Types,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Graphics;

type

  {********************************}
  TALIosNativeView = class(TOCLocal)
  private
    FControl: TALControl;
    FVisible: Boolean;
    FIgnoreResetFocus: Boolean;
    FCaptureContext: CGContextRef;
    function GetView: UIView; overload;
    procedure BeforeDestroyFormHandleListener(const Sender: TObject; const AMessage: TMessage);
    procedure AfterCreateFormHandleListener(const Sender: TObject; const AMessage: TMessage);
    procedure FormChangingFocusControlListener(const Sender: TObject; const AMessage: TMessage);
    function ExtractFirstTouchPoint(touches: NSSet): TPointF;
  protected
    procedure InitView; virtual;
    function GetFormView: UIView;
    function GetObjectiveCClass: PTypeInfo; override;
    function GetView<T: UIView>: T; overload;
  public
    constructor Create; overload; virtual;
    constructor Create(const AControl: TALControl); overload; virtual;
    destructor Destroy; override;
    function GetAbsoluteRect: TRectF;
    procedure SetEnabled(const value: Boolean); virtual;
    procedure SetVisible(const Value: Boolean); virtual;
    procedure SetAlpha(const Value: Single); virtual;
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); virtual; cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); virtual; cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); virtual; cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); virtual; cdecl;
    function canBecomeFirstResponder: Boolean; virtual; cdecl;
    function becomeFirstResponder: Boolean; virtual; cdecl;
    procedure SetFocus; virtual;
    procedure ResetFocus; virtual;
    procedure UpdateParent; virtual;
    procedure UpdateFrame; virtual;
    procedure BringToFront; virtual;
    function CaptureScreenshot: TALDrawable; virtual;
    property Control: TALControl read FControl;
    property View: UIView read GetView;
    property Visible: Boolean read FVisible;
  end;
  TALIosNativeViewClass = class of TALIosNativeView;

implementation

uses
  System.Math.Vectors,
  System.Classes,
  System.SysUtils,
  System.UITypes,
  {$IF defined(ALSkiaCanvas)}
  System.Skia.API,
  {$ENDIF}
  Macapi.Helpers,
  FMX.Platform.iOS,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.NativeControl,
  Alcinoe.Common;

{**********************************}
constructor TALIosNativeView.Create;
begin
  inherited;
  InitView;
  View.setExclusiveTouch(True);
  FVisible := True;
  FIgnoreResetFocus := False;
  FCaptureContext := nil;
  TMessageManager.DefaultManager.SubscribeToMessage(TBeforeDestroyFormHandle, BeforeDestroyFormHandleListener);
  TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, AfterCreateFormHandleListener);
  TMessageManager.DefaultManager.SubscribeToMessage(TFormChangingFocusControl, FormChangingFocusControlListener);
end;

{**************************************************************}
constructor TALIosNativeView.Create(const AControl: TALControl);
begin
  FControl := AControl;
  Create;
  UpdateParent;
end;

{**********************************}
destructor TALIosNativeView.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TBeforeDestroyFormHandle, BeforeDestroyFormHandleListener);
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, AfterCreateFormHandleListener);
  TMessageManager.DefaultManager.Unsubscribe(TFormChangingFocusControl, FormChangingFocusControlListener);

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
procedure TALIosNativeView.BeforeDestroyFormHandleListener(const Sender: TObject; const AMessage: TMessage);
begin
  if (AMessage is TBeforeDestroyFormHandle) and
     (TBeforeDestroyFormHandle(AMessage).Value = Control.Form) then begin
    var LParent := view.superview;
    if (LParent <> nil) and
       (SameObjects(LParent, GetFormView)) then begin
      {$IF defined(DEBUG)}
      ALLog(Classname + '.BeforeDestroyFormHandleListener');
      {$ENDIF}
      View.removeFromSuperview;
    end;
  end;
end;

{********************************************************************************************************}
procedure TALIosNativeView.AfterCreateFormHandleListener(const Sender: TObject; const AMessage: TMessage);
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

{***********************************************************************************************************}
procedure TALIosNativeView.FormChangingFocusControlListener(const Sender: TObject; const AMessage: TMessage);
begin
  if not (AMessage is TFormChangingFocusControl) then
    Exit;

  var LMessage := TFormChangingFocusControl(AMessage);
  // When form changes focus from on text-input control to other, it reset focus from current focused control and
  // set focus to new one. In this steps Virtual Keyboard can be hidden and shown again. This causes the keyboard
  // to blink. For avoiding it, we disable reset focus (hiding keyboard), if new focused control also uses Virtual Keyboard.
  //
  // procedure TCommonCustomForm.SetFocused(const Value: IControl);
  // begin
  //   ...
  //   var Message: TFormChangingFocusControl := TFormChangingFocusControl.Create(FFocused, NewFocused, False);
  //   try
  //     TMessageManager.DefaultManager.SendMessage(LParentForm, Message, False);
  //     if LParentForm <> Self then
  //     begin
  //       LParentForm.Active := True;
  //       LParentForm.SetFocused(Value);
  //     end
  //     else if FFocused <> NewFocused then
  //     begin
  //       ClearFocusedControl;
  //       SetFocusedControl(NewFocused);
  //     end;
  //   finally
  //     Message.IsChanged := True;
  //     TMessageManager.DefaultManager.SendMessage(LParentForm, Message, True);
  //   end;
  //   ...
  //
  FIgnoreResetFocus := (not LMessage.IsChanged) and Supports(LMessage.NewFocusedControl, IVirtualKeyboardControl);
end;

{**********************************}
procedure TALIosNativeView.InitView;
begin
  var V: Pointer := UIView(Super).initWithFrame(CGRectFromRect(GetAbsoluteRect));
  if GetObjectID <> V then
    UpdateObjectID(V);
end;

{******************************************************}
function TALIosNativeView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(UIView);
end;

{********************************************}
function TALIosNativeView.GetFormView: UIView;
begin
  var LFormHandle: TiOSWindowHandle := WindowHandleToPlatform(Control.Form.Handle);
  Result := LFormHandle.View;
end;

{****************************************}
function TALIosNativeView.GetView: UIView;
begin
  Result := GetView<UIView>;
end;

{**************************************}
function TALIosNativeView.GetView<T>: T;
begin
  Result := T(Super);
end;

{************************************************}
function TALIosNativeView.GetAbsoluteRect: TRectF;
begin
  Result := TALNativeControl(Control).GetNativeViewAbsoluteRect;
end;

{**************************************}
procedure TALIosNativeView.UpdateParent;
begin

  {$IF defined(DEBUG)}
  ALLog(Classname + '.UpdateParent');
  {$ENDIF}

  var LParentView: UIView;
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
      (SameObjects(LCurrentParent, LParentView))) then exit;

  if LCurrentParent <> nil then
    View.removeFromSuperview;

  if LParentView <> nil then begin
    LParentView.addSubview(View);
    UpdateFrame;
  end;

end;

{*************************************}
procedure TALIosNativeView.UpdateFrame;
begin
  {$IF defined(DEBUG)}
  //ALLog(Classname + '.UpdateFrame');
  {$ENDIF}
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
  View.setFrame(CGRectFromRect(LAbsoluteRect));
end;

{**************************************}
procedure TALIosNativeView.BringToFront;
begin
  var LSuperview := View.superview;
  if LSuperview <> nil then
    LSuperview.bringSubviewToFront(View);
end;

{*******************************************************}
function TALIosNativeView.CaptureScreenshot: TALDrawable;
begin
  var LBounds := View.bounds;
  var LWidth: Integer := Round(LBounds.size.width * ALGetScreenScale);
  var LHeight: Integer := Round(LBounds.size.height * ALGetScreenScale);
  if (LWidth <= 0) or (LHeight <= 0) then exit(ALNullDrawable);

  if (FCaptureContext = nil) or
     (Integer(CGBitmapContextGetWidth(FCaptureContext)) <> LWidth) or
     (Integer(CGBitmapContextGetHeight(FCaptureContext)) <> LHeight) then begin
    if FCaptureContext <> nil then begin
      CGContextRelease(FCaptureContext);
      FCaptureContext := nil;
    end;
    FCaptureContext := ALCreateCGContextRef(LWidth, LHeight);
  end;

  // Render the view hierarchy (including the child native views, which are
  // subviews of View) directly into our bitmap context. This avoids creating
  // a UIImage and then redrawing it into a second CGContext.
  CGContextClearRect(FCaptureContext, CGRectMake(0, 0, LWidth, LHeight));
  CGContextSaveGState(FCaptureContext);
  try
    CGContextTranslateCTM(FCaptureContext, 0, LHeight);
    CGContextScaleCTM(FCaptureContext, ALGetScreenScale, -ALGetScreenScale);
    UIGraphicsPushContext(FCaptureContext);
    try
      View.drawViewHierarchyInRect(LBounds, False{afterUpdates});
    finally
      UIGraphicsPopContext;
    end;
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
  Result := ALCreateTextureFromCGContextRef(FCaptureContext);
  {$ENDIF}
end;

{**********************************************************}
procedure TALIosNativeView.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  View.setHidden(not Visible or not Control.ParentedVisible);
end;

{**********************************************************}
procedure TALIosNativeView.SetEnabled(const Value: Boolean);
begin
  View.setUserInteractionEnabled(Value);
end;

{*******************************************************}
procedure TALIosNativeView.SetAlpha(const Value: Single);
begin
  View.setAlpha(Value);
end;

{************************************}
procedure TALIosNativeView.ResetFocus;
begin
  if not FIgnoreResetFocus then
    View.resignFirstResponder;
end;

{**********************************}
procedure TALIosNativeView.SetFocus;
begin
  if not View.isFirstResponder then
    View.becomeFirstResponder;
end;

{************************************************************************}
function TALIosNativeView.ExtractFirstTouchPoint(touches: NSSet): TPointF;
begin
  var LPointer := touches.anyObject;
  if LPointer=nil then
    raise Exception.Create('Error 46450539-F150-45FC-BA01-0397F2A98B0C');
  var LLocalTouch := TUITouch.Wrap(LPointer);
  if Control.Form=nil then
    raise Exception.Create('Error 5F61EC6E-0C13-46CD-A0C0-8417AA32B62A');
  var LTouchPoint := LLocalTouch.locationInView(GetFormView);
  Result := TPointF.Create(LTouchPoint.X, LTouchPoint.Y);
end;

{**************************************************************************}
procedure TALIosNativeView.touchesBegan(touches: NSSet; withEvent: UIEvent);
begin
  {$IF defined(DEBUG)}
  //ALLog(classname + '.touchesBegan');
  {$ENDIF}
  if (Control.Form <> nil) and
     (not view.isFirstResponder) and
     (touches.count > 0) then begin

    var LHandle: TiOSWindowHandle;
    if Control.Form.IsHandleAllocated then
      LHandle := WindowHandleToPlatform(Control.Form.Handle)
    else
      LHandle := nil;

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := withEvent;

    var LTouchPoint := ExtractFirstTouchPoint(touches);
    Control.Form.MouseMove([ssTouch], LTouchPoint.X, LTouchPoint.Y);
    Control.Form.MouseMove([], LTouchPoint.X, LTouchPoint.Y); // Require for correct IsMouseOver handle
    Control.Form.MouseDown(TMouseButton.mbLeft, [ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y);

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := nil;

  end;
end;

{******************************************************************************}
procedure TALIosNativeView.touchesCancelled(touches: NSSet; withEvent: UIEvent);
begin
  {$IF defined(DEBUG)}
  //ALLog(classname + '.touchesCancelled');
  {$ENDIF}
  if (Control.Form <> nil) and
     // iOS may deliver touchesCancelled (mouse up) instead of touchesEnded
     // in some cases, even when the view remains the first responder.
     //(not view.isFirstResponder) and
     (touches.count > 0) then begin

    var LHandle: TiOSWindowHandle;
    if Control.Form.IsHandleAllocated then
      LHandle := WindowHandleToPlatform(Control.Form.Handle)
    else
      LHandle := nil;

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := withEvent;

    var LTouchPoint := ExtractFirstTouchPoint(touches);
    Control.Form.MouseUp(TMouseButton.mbLeft, [ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y);
    Control.Form.MouseLeave;

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := nil;

  end;
end;

{**************************************************************************}
procedure TALIosNativeView.touchesEnded(touches: NSSet; withEvent: UIEvent);
begin
  {$IF defined(DEBUG)}
  //ALLog(classname + '.touchesEnded');
  {$ENDIF}
  if (Control.Form <> nil) and
     (not view.isFirstResponder) and
     (touches.count > 0) then begin

    var LHandle: TiOSWindowHandle;
    if Control.Form.IsHandleAllocated then
      LHandle := WindowHandleToPlatform(Control.Form.Handle)
    else
      LHandle := nil;

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := withEvent;

    var LTouchPoint := ExtractFirstTouchPoint(touches);
    Control.Form.MouseUp(TMouseButton.mbLeft, [ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y);
    Control.Form.MouseLeave;

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := nil;

  end;
end;

{**************************************************************************}
procedure TALIosNativeView.touchesMoved(touches: NSSet; withEvent: UIEvent);
begin
  {$IF defined(DEBUG)}
  //ALLog(classname + '.touchesMoved');
  {$ENDIF}
  if (Control.Form <> nil) and
     (not view.isFirstResponder) and
     (touches.count > 0) then begin

    var LHandle: TiOSWindowHandle;
    if Control.Form.IsHandleAllocated then
      LHandle := WindowHandleToPlatform(Control.Form.Handle)
    else
      LHandle := nil;

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := withEvent;

    var LTouchPoint := ExtractFirstTouchPoint(touches);
    Control.Form.MouseMove([ssLeft, ssTouch], LTouchPoint.x, LTouchPoint.y);

    if LHandle <> nil then
      LHandle.CurrentTouchEvent := nil;

  end;
end;

{*********************************************************}
function TALIosNativeView.canBecomeFirstResponder: Boolean;
begin
  {$IF defined(DEBUG)}
  //ALLog(classname + '.canBecomeFirstResponder', 'control.name: ' + Control.Name);
  {$ENDIF}
  Result := UIView(Super).canBecomeFirstResponder and Control.canFocus;
end;

{******************************************************}
function TALIosNativeView.becomeFirstResponder: Boolean;
begin
  {$IF defined(DEBUG)}
  //ALLog(classname + '.becomeFirstResponder', 'control.name: ' + Control.Name);
  {$ENDIF}
  Result := UIView(Super).becomeFirstResponder;
  if (not Control.IsFocused) then
    Control.SetFocus;
end;

end.