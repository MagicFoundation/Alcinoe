unit Alcinoe.FMX.NativeView.iOS;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.Presentation.iOS.pas was not updated and adjust the IFDEF'}
{$ENDIF}

uses
  system.Messaging,
  System.TypInfo,
  Macapi.ObjectiveC,
  iOSapi.UIKit,
  iOSapi.Foundation,
  iOSapi.CoreGraphics,
  FMX.Controls,
  FMX.Forms,
  FMX.ZOrder.iOS,
  FMX.Types;

type

  {********************************}
  TALIosNativeView = class(TOCLocal)
  private
    FControl: TControl;
    FForm: TCommonCustomForm;
    FVisible: Boolean;
    FIgnoreResetFocus: Boolean;
    function GetView: UIView; overload;
    procedure BeforeDestroyHandleListener(const Sender: TObject; const AMessage: TMessage);
    procedure AfterCreateHandleListener(const Sender: TObject; const AMessage: TMessage);
    procedure FormChangingFocusControlListener(const Sender: TObject; const AMessage: TMessage);
  protected
    procedure InitView; virtual;
    function GetFormView(const AForm: TCommonCustomForm): UIView;
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure SetEnabled(const value: Boolean); virtual; //procedure PMSetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_ABSOLUTE_ENABLED;
    procedure SetVisible(const Value: Boolean); virtual; //procedure PMSetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_VISIBLE;
    procedure SetAlpha(const Value: Single); virtual; //procedure PMSetAlpha(var AMessage: TDispatchMessageWithValue<Single>); message PM_SET_ABSOLUTE_OPACITY;
    procedure AncestorVisibleChanged; virtual; //procedure PMAncesstorVisibleChanged(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_ANCESSTOR_VISIBLE_CHANGED;
    procedure RootChanged(const aRoot: IRoot); virtual; //procedure PMRootChanged(var AMessage: TDispatchMessageWithValue<IRoot>); message PM_ROOT_CHANGED;
    procedure ChangeOrder; virtual; //procedure PMChangeOrder(var AMessage: TDispatchMessage); message PM_CHANGE_ORDER;
  protected
    function GetView<T: UIView>: T; overload;
  public
    constructor Create; overload; virtual;
    constructor Create(const AControl: TControl); overload; virtual;
    destructor Destroy; override;
    procedure SetFocus; virtual;
    procedure ResetFocus; virtual;
    procedure UpdateFrame;
    property Form: TCommonCustomForm read FForm;
    property Control: TControl read FControl;
    property View: UIView read GetView;
    property Visible: Boolean read FVisible;
  end;
  TALIosNativeViewClass = class of TALIosNativeView;

implementation

uses
  System.SysUtils,
  Macapi.Helpers,
  FMX.Platform.iOS;

{**********************************}
constructor TALIosNativeView.Create;
begin
  inherited;
  InitView;
  View.setExclusiveTouch(True);
  FVisible := True;
  FIgnoreResetFocus := False;
  TMessageManager.DefaultManager.SubscribeToMessage(TBeforeDestroyFormHandle, BeforeDestroyHandleListener);
  TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, AfterCreateHandleListener);
  TMessageManager.DefaultManager.SubscribeToMessage(TFormChangingFocusControl, FormChangingFocusControlListener);
end;

{************************************************************}
constructor TALIosNativeView.Create(const AControl: TControl);
begin
  FControl := AControl;
  Create;
  RootChanged(Control.Root);
end;

{**********************************}
destructor TALIosNativeView.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TBeforeDestroyFormHandle, BeforeDestroyHandleListener);
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, AfterCreateHandleListener);
  TMessageManager.DefaultManager.Unsubscribe(TFormChangingFocusControl, FormChangingFocusControlListener);
  RootChanged(nil);
  inherited;
end;

{****************************************************************************************************}
procedure TALIosNativeView.AfterCreateHandleListener(const Sender: TObject; const AMessage: TMessage);
begin
  // This event is called only when the window's handle is recreated.
  if (AMessage is TAfterCreateFormHandle) and (TAfterCreateFormHandle(AMessage).Value = Form) then
    RootChanged(Form);
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

{******************************************************************************************************}
procedure TALIosNativeView.BeforeDestroyHandleListener(const Sender: TObject; const AMessage: TMessage);
begin
  if (AMessage is TBeforeDestroyFormHandle) and (TBeforeDestroyFormHandle(AMessage).Value = Form) then
    View.removeFromSuperview;
end;

{**********************************}
procedure TALIosNativeView.InitView;
begin
  var V: Pointer := UIView(Super).initWithFrame(CGRectFromRect(Control.AbsoluteRect));
  if GetObjectID <> V then
    UpdateObjectID(V);
end;

{******************************************************}
function TALIosNativeView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(UIView);
end;

{****************************************************************************}
function TALIosNativeView.GetFormView(const AForm: TCommonCustomForm): UIView;
begin
  var LFormHandle: TiOSWindowHandle := WindowHandleToPlatform(AForm.Handle);
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

{*************************************}
procedure TALIosNativeView.UpdateFrame;
begin
  if FForm = nil then exit;
  View.setFrame(CGRectFromRect(Control.AbsoluteRect));
end;

{*************************************}
procedure TALIosNativeView.ChangeOrder;
begin
  //if HasZOrderManager then
  //  ZOrderManager.UpdateOrder(Control);
end;

{************************************************}
procedure TALIosNativeView.AncestorVisibleChanged;
begin
  SetVisible(Visible);
end;

{**********************************************************}
procedure TALIosNativeView.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  View.setHidden(not Visible or not Control.ParentedVisible);
end;

{*********************************************************}
procedure TALIosNativeView.RootChanged(const aRoot: IRoot);
begin
  View.removeFromSuperview;
  if aRoot is TCommonCustomForm then begin
    FForm := TCommonCustomForm(aRoot);
    GetFormView(FForm).insertSubview(View, 0);
    UpdateFrame;
  end
  else FForm := nil;
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

end.
