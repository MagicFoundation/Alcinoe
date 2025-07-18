unit Alcinoe.FMX.NativeView.Android;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported123}
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
    FForm: TCommonCustomForm;
    FView: JView;
    FLayout: JViewGroup;
    FVisible: Boolean;
    FFocusChangedListener: TALAndroidFocusChangedListener;
    function GetAbsoluteRect: TRectF;
    procedure BeforeDestroyHandleListener(const Sender: TObject; const AMessage: TMessage);
    procedure AfterCreateHandleListener(const Sender: TObject; const AMessage: TMessage);
  protected
    function GetFormView(const AForm: TCommonCustomForm): JView;
  public
    procedure SetEnabled(const value: Boolean); virtual; //procedure PMSetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_ABSOLUTE_ENABLED;
    procedure SetVisible(const Value: Boolean); virtual; // procedure PMSetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_VISIBLE;
    procedure SetAlpha(const Value: Single); virtual; // procedure PMSetAlpha(var AMessage: TDispatchMessageWithValue<Single>); message PM_SET_ABSOLUTE_OPACITY;
    procedure AncestorVisibleChanged; virtual; // procedure PMAncestorVisibleChanged(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_ANCESSTOR_VISIBLE_CHANGED;
    procedure RootChanged(const aRoot: IRoot); virtual; // procedure PMRootChanged(var AMessage: TDispatchMessageWithValue<IRoot>); message PM_ROOT_CHANGED;
    procedure ChangeOrder; virtual; // PMChangeOrder(var AMessage: TDispatchMessage); message PM_CHANGE_ORDER;
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
    procedure UpdateFrame; virtual;
    function CaptureScreenshot: TALDrawable; virtual;
    property Form: TCommonCustomForm read FForm;
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
  FMX.Platform.Android,
  FMX.Platform.UI.Android,
  FMX.Consts,
  Alcinoe.StringUtils,
  Alcinoe.Common,
  Alcinoe.FMX.NativeControl;

{**************************************}
constructor TALAndroidNativeView.Create;
var
  LayoutParams: JRelativeLayout_LayoutParams;
begin
  inherited;
  FLayout := CreateLayout;
  FView := CreateView;
  FView.setClipToOutline(False);

  FFocusChangedListener := TALAndroidFocusChangedListener.Create(Self);
  FView.setOnFocusChangeListener(FFocusChangedListener);

  { Tree view structure }
  LayoutParams := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT, TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
  FLayout.addView(FView, LayoutParams);

  FView.setClickable(True);
  FView.setFocusable(True);
  FView.setFocusableInTouchMode(True);

  FVisible := True;

  TMessageManager.DefaultManager.SubscribeToMessage(TBeforeDestroyFormHandle, BeforeDestroyHandleListener);
  TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, AfterCreateHandleListener);
end;

{******************************************************************}
constructor TALAndroidNativeView.Create(const AControl: TALControl);
begin
  FControl := AControl;
  Create;
  RootChanged(Control.Root);
end;

{**************************************}
destructor TALAndroidNativeView.Destroy;
begin
  View.setVisibility(TJView.JavaClass.INVISIBLE);
  TMessageManager.DefaultManager.Unsubscribe(TBeforeDestroyFormHandle, BeforeDestroyHandleListener);
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, AfterCreateHandleListener);
  RootChanged(nil);
  View.setOnFocusChangeListener(nil);
  ALFreeAndNil(FFocusChangedListener);
  inherited;
end;

{**********************************************************************************************************}
procedure TALAndroidNativeView.BeforeDestroyHandleListener(const Sender: TObject; const AMessage: TMessage);
begin
  if (AMessage is TBeforeDestroyFormHandle) and (TBeforeDestroyFormHandle(AMessage).Value = Form) then begin
    var LParent: JViewParent := Layout.getParent;
    if LParent <> nil then
      TJViewGroup.Wrap(LParent).removeView(Layout);
  end;
end;

{********************************************************************************************************}
procedure TALAndroidNativeView.AfterCreateHandleListener(const Sender: TObject; const AMessage: TMessage);
begin
  // This event is called only when the window's handle is recreated.
  if (AMessage is TAfterCreateFormHandle) and (TAfterCreateFormHandle(AMessage).Value = Form) then
    RootChanged(Form);
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

{*******************************************************************************}
function TALAndroidNativeView.GetFormView(const AForm: TCommonCustomForm): JView;
begin
  var LFormHandle: TAndroidWindowHandle := WindowHandleToPlatform(AForm.Handle);
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

{*****************************************}
procedure TALAndroidNativeView.UpdateFrame;
begin
  // We cannot use ZOrderManager.UpdateOrderAndBounds(Control) as it causes the edit control to lose focus
  // every time it is moved, which is particularly problematic when moving the edit from the bottom to the top
  // to accommodate the virtual keyboard display. Additionally, we cannot use ZOrderManager.UpdateBounds(Control)
  // because it updates not only the bounds but also the visibility! Code below is taken from
  // TAndroidZOrderManager.UpdateBounds
  if FForm = nil then exit;
  var LAbsoluteRect := GetAbsoluteRect;
  var LScreenScale: Single := FForm.Handle.Scale;
  var R := TRectF.Create(
             LAbsoluteRect.Left * LScreenScale,
             LAbsoluteRect.Top * LScreenScale,
             LAbsoluteRect.Right * LScreenScale,
             LAbsoluteRect.Bottom * LScreenScale).Round;
  If (R.Width <> Layout.getWidth) or (R.Height <> Layout.getHeight) then begin
    var LParam: JRelativeLayout_LayoutParams := TJRelativeLayout_LayoutParams.Wrap(Layout.getLayoutParams);
    LParam.width := R.Width;
    LParam.height := R.Height;
    Layout.setLayoutParams(LParam);
  end;
  if Layout.getLeft <> R.Left then
    Layout.setX(R.Left);
  if Layout.getTop <> R.Top then
    Layout.setY(R.Top);
end;

{***********************************************************}
function TALAndroidNativeView.CaptureScreenshot: TALDrawable;
begin
  var LOldCacheEnabledValue := View.isDrawingCacheEnabled;
  var LOldCacheQualityValue := View.getDrawingCacheQuality;
  View.setDrawingCacheEnabled(True);
  View.setDrawingCacheQuality(TJView.JavaClass.DRAWING_CACHE_QUALITY_HIGH);
  try
    var LBitmap := View.getDrawingCache;
    if LBitmap <> nil then
      {$IF defined(ALSkiaCanvas)}
      Result := ALCreateSkImageFromJBitmap(LBitmap)
      {$ELSE}
      Result := ALCreateTextureFromJBitmap(LBitmap)
      {$ENDIF}
    else
      Result := ALNullDrawable;
  finally
    View.setDrawingCacheEnabled(LOldCacheEnabledValue);
    View.setDrawingCacheQuality(LOldCacheQualityValue);
  end;
end;

{*****************************************}
procedure TALAndroidNativeView.ChangeOrder;
begin
  //if ZOrderManager <> nil then
  //  ZOrderManager.UpdateOrder(Control);
end;

{****************************************************}
procedure TALAndroidNativeView.AncestorVisibleChanged;
begin
  SetVisible(Visible);
end;

{**************************************************************}
procedure TALAndroidNativeView.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  if not Visible or not Control.ParentedVisible then
    Layout.setVisibility(TJView.JavaClass.INVISIBLE)
  else
    Layout.setVisibility(TJView.JavaClass.VISIBLE);
end;

{*************************************************************}
procedure TALAndroidNativeView.RootChanged(const aRoot: IRoot);
begin
  var LParent: JViewParent := Layout.getParent;
  if LParent <> nil then
    TJViewGroup.Wrap(LParent).removeView(Layout);
  if aRoot is TCommonCustomForm then begin
    FForm := TCommonCustomForm(aRoot);
    var LParam: JRelativeLayout_LayoutParams;
    var LParentGroup: JViewGroup := TJViewGroup.Wrap(GetFormView(FForm));
    if Layout.getLayoutParams = nil then
      LParam := TJRelativeLayout_LayoutParams.JavaClass.init(0, 0)
    else
      LParam := TJRelativeLayout_LayoutParams.Wrap(Layout.getLayoutParams);
    // FMX form consists from two parts: form's view and layout for other native controls. Form's view embedded into Layout.
    // So it should always holds 0 index. So when we rearrange native controls, we should keep in mind, that for native
    // controls first index is 1.
    LParentGroup.addView(Layout, 1, LParam);
    UpdateFrame;
  end
  else FForm := nil;
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
