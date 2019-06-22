unit ALAndroidNativeView;

{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if FMX.Presentation.Android.pas was not updated and adjust the IFDEF'}
{$ENDIF}

interface

{$SCOPEDENUMS ON}

uses System.Classes,
     System.Types,
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNIBridge,
     Androidapi.JNI.Widget,
     FMX.Types,
     FMX.Forms,
     FMX.Controls,
     FMX.ZOrder.Android;

type

  {*************************************}
  TALAndroidFocusChangedListener = class;

  {**************************}
  TALAndroidNativeView = class
  private
    [Weak] FControl: TControl;
    [Weak] FForm: TCommonCustomForm;
    FView: JView;
    FLayout: JViewGroup;
    FSize: TSizeF;
    FVisible: Boolean;
    FFocusChangedListener: TALAndroidFocusChangedListener;
    function GetZOrderManager: TAndroidZOrderManager;
  protected
    procedure SetSize(const ASize: TSizeF); virtual;
    procedure UpdateVisible;
  protected
    //procedure PMGetNativeObject(var AMessage: TDispatchMessageWithValue<IInterface>); message PM_GET_NATIVE_OBJECT;
    procedure SetAbsoluteEnabled(const value: Boolean); //procedure PMSetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_ABSOLUTE_ENABLED;
    procedure SetVisible(const Value: Boolean); // procedure PMSetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_VISIBLE;
    //procedure PMGetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_GET_VISIBLE;
    procedure SetAlpha(const Value: Single); // procedure PMSetAlpha(var AMessage: TDispatchMessageWithValue<Single>); message PM_SET_ABSOLUTE_OPACITY;
    //procedure PMGetAlpha(var AMessage: TDispatchMessageWithValue<Single>); message PM_GET_ABSOLUTE_OPACITY;
    //procedure PMSetSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_SET_SIZE;
    //procedure PMGetSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_SIZE;
    //procedure PMIsFocused(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_IS_FOCUSED;
    //procedure PMDoExit(var AMessage: TDispatchMessage); message PM_DO_EXIT;
    //procedure PMDoEnter(var AMessage: TDispatchMessage); message PM_DO_ENTER;
    //procedure PMResetFocus(var AMessage: TDispatchMessage); message PM_RESET_FOCUS;
    procedure SetClipChildren(const Value: Boolean); // procedure PMSetClipChildren(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_CLIP_CHILDREN;
    procedure AncestorVisibleChanged; // procedure PMAncestorVisibleChanged(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_ANCESSTOR_VISIBLE_CHANGED;
    //procedure PMAncestorPresentationLoaded(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_ANCESTOR_PRESENTATION_LOADED;
    //procedure PMAncestorPresentationUnloading(var AMessage: TDispatchMessageWithValue<TFmxObject>); message PM_ANCESTOR_PRESENTATION_UNLOADING;
    //procedure PMUnload(var AMessage: TDispatchMessage); message PM_UNLOAD;
    //procedure PMRefreshParent(var AMessage: TDispatchMessage); message PM_REFRESH_PARENT;
    //procedure PMAbsoluteChanged(var AMessage: TDispatchMessage); message PM_ABSOLUTE_CHANGED;
    function PointInObjectLocal(X: Single; Y: Single): Boolean; //procedure PMPointInObject(var AMessage: TDispatchMessageWithValue<TPointInObjectLocalInfo>); message PM_POINT_IN_OBJECT_LOCAL;
    procedure ChangeOrder; // PMChangeOrder(var AMessage: TDispatchMessage); message PM_CHANGE_ORDER;
    procedure RootChanged(const aRoot: IRoot); // procedure PMRootChanged(var AMessage: TDispatchMessageWithValue<IRoot>); message PM_ROOT_CHANGED;
  protected
    function GetView<T: JView>: T; overload;
    procedure UpdateFrame;
    procedure RefreshNativeParent; virtual;
    function CreateView: JView; virtual;
    function CreateLayout: JViewGroup; virtual;
    procedure InitView; virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(const AControl: TControl); overload; virtual;
    destructor Destroy; override;
    function HasZOrderManager: Boolean;
    procedure SetFocus; virtual;
    procedure ResetFocus; virtual;
    property Form: TCommonCustomForm read FForm;
    property ZOrderManager: TAndroidZOrderManager read GetZOrderManager;
  public
    property Control: TControl read FControl;
    property Size: TSizeF read FSize write SetSize;
    property Layout: JViewGroup read FLayout;
    property View: JView read FView;
    property Visible: Boolean read FVisible;
  end;
  TALAndroidNativeViewClass = class of TALAndroidNativeView;

  {********************************************}
  TALAndroidBaseViewListener = class(TJavaLocal)
  private
    [Weak] FView: TALAndroidNativeView;
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

uses System.SysUtils,
     Androidapi.Helpers,
     Androidapi.JNI.App,
     FMX.Platform,
     FMX.Platform.Android,
     FMX.Consts,
     ALString,
     ALCommon;

{****************************************************************}
constructor TALAndroidNativeView.Create(const AControl: TControl);
begin
  FControl := AControl;
  Create;
  RefreshNativeParent;
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

{**************************************}
constructor TALAndroidNativeView.Create;
var LayoutParams: JRelativeLayout_LayoutParams;
begin
  inherited;
  FLayout := CreateLayout;
  FView := CreateView;
  FView.setClipToOutline(False);

  FFocusChangedListener := TALAndroidFocusChangedListener.Create(Self);
  View.setOnFocusChangeListener(FFocusChangedListener);

  { Tree view structure }
  LayoutParams := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT, TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
  FLayout.addView(FView, LayoutParams);

  InitView;

  FVisible := True;
end;

{**************************************}
destructor TALAndroidNativeView.Destroy;
begin
  if HasZOrderManager then ZOrderManager.RemoveLink(Control);
  View.setOnFocusChangeListener(nil); // << https://quality.embarcadero.com/browse/RSP-24666
  ALFreeAndNil(FFocusChangedListener);
  inherited;
end;

{******************************************}
function TALAndroidNativeView.GetView<T>: T;
begin
  Result := T(FView);
end;

{********************************************************************}
function TALAndroidNativeView.GetZOrderManager: TAndroidZOrderManager;
begin
  if Form <> nil then Result := WindowHandleToPlatform(Form.Handle).ZOrderManager
  else Result := nil;
end;

{******************************************************}
function TALAndroidNativeView.HasZOrderManager: Boolean;
begin
  Result := (Form <> nil) and (Form.Handle <> nil);
end;

{**************************************}
procedure TALAndroidNativeView.InitView;
begin
end;

{****************************************************}
procedure TALAndroidNativeView.AncestorVisibleChanged;
begin
  UpdateVisible;
end;

{*****************************************}
procedure TALAndroidNativeView.ChangeOrder;
begin
  if ZOrderManager <> nil then
    ZOrderManager.UpdateOrder(Control);
end;

{******************************************************************************}
function TALAndroidNativeView.PointInObjectLocal(X: Single; Y: Single): Boolean;
var HitTestPoint: TPointF;
begin
  HitTestPoint := TPointF.Create(x,y);
  Result := Control.LocalRect.Contains(HitTestPoint);
end;

{*************************************************************}
procedure TALAndroidNativeView.RootChanged(const aRoot: IRoot);
begin
  // Changing root for native control means changing ZOrderManager, because one form owns ZOrderManager.
  // So we need to remove itself from old one and add to new one.
  if HasZOrderManager then ZOrderManager.RemoveLink(Control);

  if aRoot is TCommonCustomForm then FForm := TCommonCustomForm(aRoot)
  else FForm := nil;

  if HasZOrderManager then ZOrderManager.AddOrSetLink(Control, Layout, nil);
  RefreshNativeParent;
end;

{**********************************************************************}
procedure TALAndroidNativeView.SetAbsoluteEnabled(const value: Boolean);
begin
  FView.setEnabled(value);
end;

{***********************************************************}
procedure TALAndroidNativeView.SetAlpha(const Value: Single);
begin
  FView.setAlpha(Value);
end;

{*******************************************************************}
procedure TALAndroidNativeView.SetClipChildren(const Value: Boolean);
begin
  FLayout.setClipToPadding(Value);
  FLayout.setClipToOutline(Value);
  FLayout.setClipChildren(Value);
end;

{**************************************************************}
procedure TALAndroidNativeView.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  UpdateVisible;
end;

{*************************************************}
procedure TALAndroidNativeView.RefreshNativeParent;
begin
  if HasZOrderManager then
    ZOrderManager.UpdateOrderAndBounds(Control);
end;

{****************************************}
procedure TALAndroidNativeView.ResetFocus;
begin
  FView.clearFocus;
end;

{**************************************}
procedure TALAndroidNativeView.SetFocus;
begin
  FView.requestFocus;
end;

{**********************************************************}
procedure TALAndroidNativeView.SetSize(const ASize: TSizeF);
begin
  FSize := ASize;
  UpdateFrame;
end;

{*****************************************}
procedure TALAndroidNativeView.UpdateFrame;
begin
  if ZOrderManager <> nil then
    ZOrderManager.UpdateBounds(Control); // UpdateBounds instead of UpdateOrderAndBounds because else everytime
                                         // we will move the edit we will loose the focus and this is problematic
                                         // if we for exemple move the edit from the bottom to the top to let some
                                         // place to show the virtual keyboard
end;

{*******************************************}
procedure TALAndroidNativeView.UpdateVisible;
begin
  if not Visible or not Control.ParentedVisible then Layout.setVisibility(TJView.JavaClass.GONE)
  else if ZOrderManager = nil then Layout.setVisibility(TJView.JavaClass.VISIBLE)
  else ZOrderManager.UpdateOrderAndBounds(Control);
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
  ALLog('TALAndroidFocusChangedListener.onFocusChange', 'control.parent.name: ' + self.view.Control.parent.Name + // control is TALAndroidEdit and Control.parent is TalEdit
                                                        ' - hasFocus: ' + ALBoolToStrU(hasFocus) +
                                                        ' - control.IsFocused: ' + ALBoolToStrU(self.View.Control.IsFocused) +
                                                        ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  // Since view can get focus without us, we synchronize native focus and fmx focus. For example, when user makes a tap
  // on Input control, control request focus itself and we can get the situation with two focused controls native and
  // styled Edit.
  if hasFocus and not self.View.Control.IsFocused then self.View.Control.SetFocus
  else if not hasFocus and self.View.Control.IsFocused then self.View.Control.ResetFocus;

end;

end.
