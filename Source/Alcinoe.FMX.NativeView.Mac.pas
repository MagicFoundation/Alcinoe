unit Alcinoe.FMX.NativeView.Mac;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported130}
  {$MESSAGE WARN 'Check if FMX.Presentation.Mac.pas EXIST and adjust the IFDEF'}
{$ENDIF}

uses
  System.Types,
  system.Messaging,
  System.TypInfo,
  Macapi.ObjectiveC,
  MacApi.AppKit,
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
    FForm: TCommonCustomForm;
    FVisible: Boolean;
    function GetView: NSView; overload;
    function GetAbsoluteRect: TRectF;
    procedure BeforeDestroyHandleListener(const Sender: TObject; const AMessage: TMessage);
    procedure AfterCreateHandleListener(const Sender: TObject; const AMessage: TMessage);
    procedure FormSizeChanged(const Sender: TObject; const AMessage: TMessage);
  protected
    procedure InitView; virtual;
    function GetFormView(const AForm: TCommonCustomForm): NSView;
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure SetEnabled(const value: Boolean); virtual; //procedure PMSetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_ABSOLUTE_ENABLED;
    procedure SetVisible(const Value: Boolean); virtual; //procedure PMSetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_VISIBLE;
    procedure SetAlpha(const Value: Single); virtual; //procedure PMSetAlpha(var AMessage: TDispatchMessageWithValue<Single>); message PM_SET_ABSOLUTE_OPACITY;
    procedure AncestorVisibleChanged; virtual; //procedure PMAncesstorVisibleChanged(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_ANCESSTOR_VISIBLE_CHANGED;
    procedure RootChanged(const aRoot: IRoot); virtual; //procedure PMRootChanged(var AMessage: TDispatchMessageWithValue<IRoot>); message PM_ROOT_CHANGED;
    procedure ChangeOrder; virtual; //procedure PMChangeOrder(var AMessage: TDispatchMessage); message PM_CHANGE_ORDER;
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
    procedure UpdateFrame; virtual;
    function CaptureScreenshot: TALDrawable; virtual;
    property Form: TCommonCustomForm read FForm;
    property Control: TALControl read FControl;
    property View: NSView read GetView;
    property Visible: Boolean read FVisible;
  end;
  TALMacNativeViewClass = class of TALMacNativeView;

implementation

uses
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
  TMessageManager.DefaultManager.SubscribeToMessage(TBeforeDestroyFormHandle, BeforeDestroyHandleListener);
  TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, AfterCreateHandleListener);
  TMessageManager.DefaultManager.SubscribeToMessage(TSizeChangedMessage, FormSizeChanged);
end;

{**************************************************************}
constructor TALMacNativeView.Create(const AControl: TALControl);
begin
  FControl := AControl;
  Create;
  RootChanged(Control.Root);
end;

{**********************************}
destructor TALMacNativeView.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TBeforeDestroyFormHandle, BeforeDestroyHandleListener);
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, AfterCreateHandleListener);
  TMessageManager.DefaultManager.Unsubscribe(TSizeChangedMessage, FormSizeChanged);
  RootChanged(nil);
  inherited;
end;

{******************************************************************************************************}
procedure TALMacNativeView.BeforeDestroyHandleListener(const Sender: TObject; const AMessage: TMessage);
begin
  if (AMessage is TBeforeDestroyFormHandle) and (TBeforeDestroyFormHandle(AMessage).Value = Form) then
    View.removeFromSuperview;
end;

{****************************************************************************************************}
procedure TALMacNativeView.AfterCreateHandleListener(const Sender: TObject; const AMessage: TMessage);
begin
  // This event is called only when the window's handle is recreated.
  if (AMessage is TAfterCreateFormHandle) and (TAfterCreateFormHandle(AMessage).Value = Form) then
    RootChanged(Form);
end;

{******************************************************************************************}
procedure TALMacNativeView.FormSizeChanged(const Sender: TObject; const AMessage: TMessage);
begin
  // If the form size changes, we need to reposition the view because the
  // origin is at the bottom/left instead of the top/left.
  if (Sender = Form) then
    UpdateFrame;
end;

{**********************************}
procedure TALMacNativeView.InitView;
begin
  var LAbsoluteRect := GetAbsoluteRect;
  var LGridHeight: Single;
  if FForm <> nil then LGridHeight := GetFormView(FForm).frame.size.height
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

{****************************************************************************}
function TALMacNativeView.GetFormView(const AForm: TCommonCustomForm): NSView;
begin
  var LFormHandle: TMacWindowHandle := WindowHandleToPlatform(AForm.Handle);
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

{*************************************}
procedure TALMacNativeView.UpdateFrame;
begin
  if FForm = nil then exit;
  var LAbsoluteRect := GetAbsoluteRect;
  View.setFrame(
    ALLowerLeftCGRect(
      LAbsoluteRect.TopLeft,
      LAbsoluteRect.Width,
      LAbsoluteRect.Height,
      GetFormView(FForm).frame.size.height));
end;

{*******************************************************}
function TALMacNativeView.CaptureScreenshot: TALDrawable;
begin
  Result := AlNullDrawable;
end;

{*************************************}
procedure TALMacNativeView.ChangeOrder;
begin
  //if HasZOrderManager then
  //  ZOrderManager.UpdateOrder(Control);
end;

{************************************************}
procedure TALMacNativeView.AncestorVisibleChanged;
begin
  SetVisible(Visible);
end;

{**********************************************************}
procedure TALMacNativeView.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  View.setHidden(not Visible or not Control.ParentedVisible);
end;

{*********************************************************}
procedure TALMacNativeView.RootChanged(const aRoot: IRoot);
begin
  View.removeFromSuperview;
  if aRoot is TCommonCustomForm then begin
    FForm := TCommonCustomForm(aRoot);
    GetFormView(FForm).addSubview(View);
    UpdateFrame;
  end
  else FForm := nil;
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