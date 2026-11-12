unit Alcinoe.FMX.NativeView.Android;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported132}
  {$MESSAGE WARN 'Check if FMX.Presentation.Android.pas was not updated and adjust the IFDEF'}
{$ENDIF}

uses
  System.Types,
  system.Messaging,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.Widget,
  FMX.Types,
  FMX.Forms,
  FMX.Controls,
  FMX.ZOrder.Android,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Graphics;

type

  {*************************************}
  TALAndroidFocusChangedListener = class;

  {**************************}
  TALAndroidNativeView = class
  private
    FControl: TALControl;
    FView: JView;
    FLayout: JViewGroup;
    FCaptureBitmap: JBitmap;
    FCaptureCanvas: JCanvas;
    FVisible: Boolean;
    FFocusChangedListener: TALAndroidFocusChangedListener;
    procedure BeforeDestroyFormHandleListener(const Sender: TObject; const AMessage: TMessage);
    procedure AfterCreateFormHandleListener(const Sender: TObject; const AMessage: TMessage);
  protected
    function GetFormView: JView;
  public
    function GetAbsoluteRect: TRectF;
    procedure SetEnabled(const value: Boolean); virtual; //procedure PMSetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_ABSOLUTE_ENABLED;
    procedure SetVisible(const Value: Boolean); virtual; // procedure PMSetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_VISIBLE;
    procedure SetAlpha(const Value: Single); virtual; // procedure PMSetAlpha(var AMessage: TDispatchMessageWithValue<Single>); message PM_SET_ABSOLUTE_OPACITY;
    procedure BringToFront; virtual;
  protected
    function GetView<T: JView>: T; overload;
    function CreateView: JView; virtual;
    function CreateLayout: JViewGroup; virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(const AControl: TALControl); overload; virtual;
    destructor Destroy; override;
    procedure SetFocus; virtual;
    procedure ResetFocus; virtual;
    procedure UpdateParent; virtual;
    procedure UpdateFrame; virtual;
    procedure Draw(const ACanvas: JCanvas); virtual;
    function CaptureScreenshot: TALDrawable; virtual;
    property Control: TALControl read FControl;
    property Layout: JViewGroup read FLayout;
    property View: JView read FView;
    property Visible: Boolean read FVisible;
  end;
  TALAndroidNativeViewClass = class of TALAndroidNativeView;

  {********************************************}
  TALAndroidBaseViewListener = class(TJavaLocal)
  private
    FView: TALAndroidNativeView;
  public
    constructor Create(const AView: TALAndroidNativeView);
    property View: TALAndroidNativeView read FView;
  end;

  {*********************************************************************************************}
  TALAndroidFocusChangedListener = class(TALAndroidBaseViewListener, JView_OnFocusChangeListener)
  public
    procedure onFocusChange(view: JView; hasFocus: Boolean); cdecl;
  end;

var

  {********************************}
  ALVirtualKeyboardVisible: Boolean;

implementation

uses
  System.Classes,
  System.SysUtils,
  Androidapi.Helpers,
  Androidapi.JNI.App,
  AndroidApi.Jni,
  FMX.Platform.Android,
  FMX.Platform.UI.Android,
  FMX.Consts,
  Alcinoe.StringUtils,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.NativeControl,
  Alcinoe.Common;

{**************************************}
constructor TALAndroidNativeView.Create;
var
  LayoutParams: JRelativeLayout_LayoutParams;
begin
  inherited;

  FVisible := True;

  FLayout := CreateLayout;
  FLayout.setClipChildren(False);
  FLayout.setClipToPadding(False);
  FView := CreateView;
  FView.setClipToOutline(False);

  FCaptureBitmap := nil;
  FCaptureCanvas := nil;

  FFocusChangedListener := TALAndroidFocusChangedListener.Create(Self);
  FView.setOnFocusChangeListener(FFocusChangedListener);

  LayoutParams := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT, TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
  FLayout.addView(FView, LayoutParams);

  FView.setClickable(True);
  FView.setFocusable(True);
  FView.setFocusableInTouchMode(True);

  TMessageManager.DefaultManager.SubscribeToMessage(TBeforeDestroyFormHandle, BeforeDestroyFormHandleListener);
  TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, AfterCreateFormHandleListener);
end;

{******************************************************************}
constructor TALAndroidNativeView.Create(const AControl: TALControl);
begin
  FControl := AControl;
  Create;
  UpdateParent;
end;

{**************************************}
destructor TALAndroidNativeView.Destroy;
begin
  // setOnFocusChangeListener(nil) must come first: both setVisibility(INVISIBLE) and RootChanged(nil)
  // can cause Android to fire onFocusChange, which calls Control.ResetFocus, which accesses
  // Control.NativeView — but ALFreeAndNil already zeroed FNativeView, so GetNativeView would
  // lazily recreate a new TALAndroidNativeView mid-destruction.
  View.setOnFocusChangeListener(nil);
  ALFreeAndNil(FFocusChangedListener);
  View.setVisibility(TJView.JavaClass.INVISIBLE);
  TMessageManager.DefaultManager.Unsubscribe(TBeforeDestroyFormHandle, BeforeDestroyFormHandleListener);
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, AfterCreateFormHandleListener);

  var LNativeChildControls := TALNativeControl(Control).GetNativeChildControls;
  for var I := Low(LNativeChildControls) to High(LNativeChildControls) do
    Layout.removeView(LNativeChildControls[I].NativeView.Layout);

  var LParent := Layout.getParent;
  if LParent <> nil then
    TJViewGroup.Wrap(LParent).removeView(Layout);

  if FCaptureBitmap <> nil then
    FCaptureBitmap.recycle;
  FCaptureCanvas := nil;

  inherited;
end;

{**************************************************************************************************************}
procedure TALAndroidNativeView.BeforeDestroyFormHandleListener(const Sender: TObject; const AMessage: TMessage);
begin
  if (AMessage is TBeforeDestroyFormHandle) and
     (TBeforeDestroyFormHandle(AMessage).Value = Control.Form) then begin
    var LParent := Layout.getParent;
    if (LParent <> nil) and
       (TJNIResolver.IsSameObject(LParent, GetFormView)) then begin
      {$IF defined(DEBUG)}
      ALLog(Classname + '.BeforeDestroyFormHandleListener');
      {$ENDIF}
      TJViewGroup.Wrap(LParent).removeView(Layout);
    end;
  end;
end;

{************************************************************************************************************}
procedure TALAndroidNativeView.AfterCreateFormHandleListener(const Sender: TObject; const AMessage: TMessage);
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

{*****************************************************}
function TALAndroidNativeView.CreateLayout: JViewGroup;
begin
  Result := TJRelativeLayout.JavaClass.init(TAndroidHelper.Context)
end;

{**********************************************}
function TALAndroidNativeView.CreateView: JView;
begin
  Result := TJView.JavaClass.init(TAndroidHelper.Activity);
end;

{***********************************************}
function TALAndroidNativeView.GetFormView: JView;
begin
  var LFormHandle: TAndroidWindowHandle := WindowHandleToPlatform(Control.Form.Handle);
  Result := LFormHandle.FormLayout;
end;

{******************************************}
function TALAndroidNativeView.GetView<T>: T;
begin
  Result := T(FView);
end;

{****************************************************}
function TALAndroidNativeView.GetAbsoluteRect: TRectF;
begin
  Result := TALNativeControl(Control).GetNativeViewAbsoluteRect;
end;

{******************************************}
procedure TALAndroidNativeView.UpdateParent;
begin

  {$IF defined(DEBUG)}
  ALLog(Classname + '.UpdateParent');
  {$ENDIF}

  // Example:
  //
  // FormView
  //   └── MapLibre.Layout
  //        |  | └── MapLibre.View
  //        |  └── FMXOverNative.Layout
  //        |       └── ImageView
  //        └── FMXOverNative.Layout
  //              └── ImageView
  //

  var LParentGroup: JViewGroup;
  if Control.Form = nil then LParentGroup := nil
  else LParentGroup := TJViewGroup.Wrap(GetFormView); // Ex: FormLayout

  var LParentControl := Control.ParentControl;
  while LParentControl <> nil do begin
    if LParentControl is TALNativeControl then begin
      if not TALNativeControl(LParentControl).HasNativeView then exit;
      LParentGroup := TALNativeControl(LParentControl).NativeView.Layout; // Ex: MapLibre.Layout
      break;
    end;
    LParentControl := LParentControl.ParentControl;
  end;

  var LCurrentParent := Layout.getParent;
  if ((LCurrentParent = nil) and
      (LParentGroup = nil)) or
     ((LCurrentParent <> nil) and
      (LParentGroup <> nil) and
      (TJNIResolver.IsSameObject(LCurrentParent, LParentGroup))) then exit;

  if LCurrentParent <> nil then
    TJViewGroup.Wrap(LCurrentParent).removeView(Layout);

  if LParentGroup <> nil then begin
    var LParams: JRelativeLayout_LayoutParams;
    if Layout.getLayoutParams = nil then LParams := TJRelativeLayout_LayoutParams.JavaClass.init(0, 0)
    else LParams := TJRelativeLayout_LayoutParams.Wrap(Layout.getLayoutParams);
    LParentGroup.addView(Layout, LParentGroup.getChildCount, LParams);
    UpdateFrame;
  end;

end;

{*****************************************}
procedure TALAndroidNativeView.UpdateFrame;
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
  var R := TRectF.Create(
             LAbsoluteRect.Left * ALGetScreenScale,
             LAbsoluteRect.Top * ALGetScreenScale,
             LAbsoluteRect.Right * ALGetScreenScale,
             LAbsoluteRect.Bottom * ALGetScreenScale).Round;
  if Layout.getLayoutParams = nil then exit;
  var LParam := TJRelativeLayout_LayoutParams.Wrap(Layout.getLayoutParams);
  // RelativeLayout clamps the measured size of a child to the space available
  // in the parent: Min(LParam.width, ParentWidth - leftMargin - rightMargin)
  // (same for the height). The position is applied with setX/setY (translation),
  // not with margins, so a native view larger than its parent (for example a
  // TALFMXOverNativeContainer whose bounds include a large shadow, or any view
  // bigger than the form or than a parent TALMapLibre) would be measured
  // smaller than requested. The ImageView would still draw the full bitmap
  // (clipChildren is False), but its hit-test area would be the clamped size,
  // so touches would never reach it. A negative right/bottom margin as large as
  // the view itself guarantees that the available space is never smaller than
  // the requested size, and it has no effect on the position.
  if (R.Width <> LParam.width) or (R.Height <> LParam.height) or
     (LParam.rightMargin <> -R.Width) or (LParam.bottomMargin <> -R.Height) then begin
    LParam.width := R.Width;
    LParam.height := R.Height;
    LParam.rightMargin := -R.Width;
    LParam.bottomMargin := -R.Height;
    Layout.setLayoutParams(LParam);
  end;
  if Layout.getX <> R.Left then
    Layout.setX(R.Left);
  if Layout.getY <> R.Top then
    Layout.setY(R.Top);
  // In theory, setLayoutParams already triggers a layout pass, and setX/setY only affect the
  // visual position (translation) without requiring a new layout. However, in practice, when
  // the native view is initially positioned outside the visible screen area, it may remain
  // non-rendered even after being moved back into the visible area using setX/setY alone.
  // Forcing a requestLayout when the view enters the visible region ensures that the Android
  // view hierarchy performs a proper layout pass, after which the view behaves normally.
  // This appears to be related to how the FMX host integrates native views rather than a
  // standard Android behavior.
  if not Layout.isInLayout then
    Layout.requestLayout;
end;

{**********************************************************}
procedure TALAndroidNativeView.Draw(const ACanvas: JCanvas);
begin

  // Draw the current native view.
  // When a view is drawn by its parent, the parent first translates the canvas
  // by (-scrollX, -scrollY). Calling View.draw directly skips that step, so it
  // must be done here. Otherwise a scrolled view (e.g. a multiline EditText
  // scrolled to the bottom) clips its content outside the bitmap and the
  // capture is empty.
  ACanvas.save;
  try
    ACanvas.translate(-View.getScrollX, -View.getScrollY);
    View.draw(ACanvas);
  finally
    ACanvas.restore;
  end;

  // Draw native child controls.
  var LParentRect := GetAbsoluteRect;
  var LNativeChildControls := TALNativeControl(Control).GetNativeChildControls;
  for var I := Low(LNativeChildControls) to High(LNativeChildControls) do begin
    var LNativeView := LNativeChildControls[I].NativeView;
    if LNativeView.Layout.getVisibility <> TJView.JavaClass.VISIBLE then continue;
    var LChildRect := LNativeView.GetAbsoluteRect;
    ACanvas.save;
    try
      ACanvas.translate(
        (LChildRect.Left - LParentRect.Left) * ALGetScreenScale,
        (LChildRect.Top - LParentRect.Top) * ALGetScreenScale);
      LNativeView.Draw(ACanvas);
    finally
      ACanvas.restore;
    end;
  end;

end;

{***********************************************************}
function TALAndroidNativeView.CaptureScreenshot: TALDrawable;
begin
  if (Layout.getWidth <= 0) or (Layout.getHeight <= 0) then exit(ALNullDrawable);

  if (FCaptureBitmap = nil) or
     (FCaptureBitmap.getWidth <> Layout.getWidth) or
     (FCaptureBitmap.getHeight <> Layout.getHeight) then begin

    if FCaptureBitmap <> nil then FCaptureBitmap.recycle;
    FCaptureBitmap := TJBitmap.JavaClass.createBitmap(
                        Layout.getWidth,
                        Layout.getHeight,
                        TJBitmap_Config.JavaClass.ARGB_8888);
    FCaptureCanvas := TJCanvas.JavaClass.init(FCaptureBitmap);
  end
  else
    FCaptureBitmap.eraseColor(TJColor.JavaClass.TRANSPARENT);

  Draw(FCaptureCanvas);
  {$IF defined(ALSkiaCanvas)}
  Result := ALCreateSkImageFromJBitmap(FCaptureBitmap);
  {$ELSE}
  Result := ALCreateTextureFromJBitmap(FCaptureBitmap);
  {$ENDIF}
end;

{******************************************}
procedure TALAndroidNativeView.BringToFront;
begin
  if Layout.getParent <> nil then
    Layout.bringToFront;
end;

{**************************************************************}
procedure TALAndroidNativeView.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  if not Visible or not Control.ParentedVisible then
    Layout.setVisibility(TJView.JavaClass.INVISIBLE)
  else
    Layout.setVisibility(TJView.JavaClass.VISIBLE);
  // In theory, calling requestLayout should not be necessary.
  // However, on Android 16, showing a Dialog temporarily hides
  // all native views on the form. When the Dialog is dismissed and
  // visibility is restored, some views are not properly redrawn
  // unless requestLayout is explicitly triggered. This ensures
  // the EditView becomes visible again.
  if FVisible and (not Layout.isInLayout) then
    Layout.requestLayout;
end;

{**************************************************************}
procedure TALAndroidNativeView.SetEnabled(const value: Boolean);
begin
  FView.setEnabled(value);
end;

{***********************************************************}
procedure TALAndroidNativeView.SetAlpha(const Value: Single);
begin
  FView.setAlpha(Value);
end;

{**************************************}
procedure TALAndroidNativeView.SetFocus;
begin
  FView.requestFocus;
end;

{****************************************}
procedure TALAndroidNativeView.ResetFocus;
begin
  FView.clearFocus;
end;

{*******************************************************************************}
constructor TALAndroidBaseViewListener.Create(const AView: TALAndroidNativeView);
begin
  if AView = nil then
    raise EArgumentNilException.CreateFmt(SWrongParameter, ['AView']);

  inherited Create;
  FView := AView;
end;

{*************************************************************************************}
procedure TALAndroidFocusChangedListener.onFocusChange(view: JView; hasFocus: Boolean);
begin

  {$IF defined(DEBUG)}
  //ALLog(
  //  Classname + '.onFocusChange',
  //  'control.name: ' + self.view.Control.Name + ' | ' +
  //  'hasFocus: ' + ALBoolToStrW(hasFocus) + ' | ' +
  //  'control.IsFocused: ' + ALBoolToStrW(self.View.Control.IsFocused));
  {$ENDIF}

  // Since view can get focus without us, we synchronize native focus and fmx focus. For example, when user makes a tap
  // on Input control, control request focus itself and we can get the situation with two focused controls native and
  // styled Edit.
  if hasFocus and not self.View.Control.IsFocused then
    self.View.Control.SetFocus
  else if not hasFocus and self.View.Control.IsFocused then
    self.View.Control.ResetFocus;

end;

end.