unit ALIosNativeView;

{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if FMX.Presentation.iOS.pas was not updated and adjust the IFDEF'}
{$ENDIF}

interface

{$SCOPEDENUMS ON}

uses System.TypInfo,
     System.Types,
     System.Classes,
     Macapi.ObjectiveC,
     iOSapi.Foundation,
     iOSapi.UIKit,
     iOSApi.CoreGraphics,
     FMX.Controls,
     FMX.Forms,
     FMX.ZOrder.iOS,
     FMX.Types;

type

  {***********************}
  TALIosNativeView = class;

  {*******************************}
  IALUIClipView = interface(UIView)
    ['{19644DED-4972-4D82-9C70-5173EA6ED7EF}']
    function pointInside(point: CGPoint; withEvent: UIEvent): Boolean; cdecl;
  end;

  {******************************}
  TALIosClipView = class(TOCLocal)
  private
    [Weak] FContentView: TALIosNativeView;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    function pointInside(point: CGPoint; withEvent: UIEvent): Boolean; cdecl;
  public
    constructor Create(const AContentView: TALIosNativeView);
    destructor Destroy; override;
    property ContentView: TALIosNativeView read FContentView;
  end;

  {***************************}
  IALUIView = interface(UIView)
    ['{617EB128-774A-4E65-A50E-57FFA7CB0D6E}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
  end;

  {********************************}
  TALIosNativeView = class(TOCLocal)
  private
    [Weak] FParentView: UIView;
    [Weak] FControl: TControl;
    [Weak] FForm: TCommonCustomForm;
    FClipView: TALIosClipView;
    FSize: TSizeF;
    FVisible: Boolean;
    function GetAncestorClipRect: TRectF;
    function GetView: UIView; overload;
    function GetClipView: UIView;
    function GetZOrderManager: TiOSZOrderManager;
  protected
    procedure InitView; virtual;
    function GetViewFrame: NSRect;
    function GetFormView(const AForm: TCommonCustomForm): UIView;
    procedure SetSize(const ASize: TSizeF); virtual;
    function GetObjectiveCClass: PTypeInfo; override;
  protected
    //procedure PMGetNativeObject(var AMessage: TDispatchMessageWithValue<IInterface>); message PM_GET_NATIVE_OBJECT;
    procedure SetAbsoluteEnabled(const value: Boolean); //procedure PMSetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_ABSOLUTE_ENABLED;
    procedure SetVisible(const Value: Boolean); //procedure PMSetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_VISIBLE;
    //procedure PMGetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_GET_VISIBLE;
    procedure SetAlpha(const Value: Single); //procedure PMSetAlpha(var AMessage: TDispatchMessageWithValue<Single>); message PM_SET_ABSOLUTE_OPACITY;
    //procedure PMGetAlpha(var AMessage: TDispatchMessageWithValue<Single>); message PM_GET_ABSOLUTE_OPACITY;
    //procedure PMSetSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_SET_SIZE;
    //procedure PMGetSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_SIZE;
    //procedure PMIsFocused(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_IS_FOCUSED;
    //procedure PMDoExit(var AMessage: TDispatchMessage); message PM_DO_EXIT;
    //procedure PMDoEnter(var AMessage: TDispatchMessage); message PM_DO_ENTER;
    //procedure PMResetFocus(var AMessage: TDispatchMessage); message PM_RESET_FOCUS;
    //procedure PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_RECOMMEND_SIZE;
    procedure SetClipChildren(const Value: Boolean); //procedure PMSetClipChildren(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_CLIP_CHILDREN;
    procedure AncestorVisibleChanged; //procedure PMAncesstorVisibleChanged(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_ANCESSTOR_VISIBLE_CHANGED;
    //procedure PMAncestorPresentationLoaded(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_ANCESTOR_PRESENTATION_LOADED;
    //procedure PMAncestorPresentationUnloading(var AMessage: TDispatchMessageWithValue<TFmxObject>); message PM_ANCESTOR_PRESENTATION_UNLOADING;
    //procedure PMUnload(var AMessage: TDispatchMessage); message PM_UNLOAD;
    //procedure PMRefreshParent(var AMessage: TDispatchMessage); message PM_REFRESH_PARENT;
    //procedure PMAbsoluteChanged(var AMessage: TDispatchMessage); message PM_ABSOLUTE_CHANGED;
    function PointInObjectLocal(X: Single; Y: Single): Boolean; //procedure PMPointInObject(var AMessage: TDispatchMessageWithValue<TPointInObjectLocalInfo>); message PM_POINT_IN_OBJECT_LOCAL;
    procedure ChangeOrder; //procedure PMChangeOrder(var AMessage: TDispatchMessage); message PM_CHANGE_ORDER;
    procedure RootChanged(const aRoot: IRoot); //procedure PMRootChanged(var AMessage: TDispatchMessageWithValue<IRoot>); message PM_ROOT_CHANGED;
  protected
    function GetView<T: UIView>: T; overload;
    function FindParentView([Weak] out AParentView: UIView): Boolean; virtual;
    procedure UpdateFrame;
    procedure RefreshNativeParent; virtual;
    function IsParentViewChanged: Boolean;
  public
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    function pointInside(point: CGPoint; withEvent: UIEvent): Boolean; virtual; cdecl;
  public
    constructor Create; overload; virtual;
    constructor Create(const AControl: TControl); overload; virtual;
    destructor Destroy; override;
    function HasZOrderManager: Boolean;
    procedure SetFocus; virtual;
    procedure ResetFocus; virtual;
    function LocalToParentView(const ALocalPoint: TPointF): TPointF;
    property Form: TCommonCustomForm read FForm;
  public
    property AncestorClipRect: TRectF read GetAncestorClipRect;
    property Control: TControl read FControl;
    property ClipView: UIView read GetClipView;
    property ParentView: UIView read FParentView;
    property Size: TSizeF read FSize write SetSize;
    property View: UIView read GetView;
    property ViewFrame: NSRect read GetViewFrame;
    property Visible: Boolean read FVisible;
    property ZOrderManager: TiOSZOrderManager read GetZOrderManager;
  end;
  TALIosNativeViewClass = class of TALIosNativeView;

implementation

uses System.UITypes,
     System.SysUtils,
     Macapi.Helpers,
     Macapi.ObjCRuntime,
     FMX.Platform.iOS,
     ALCommon;

{****************************************************}
function TALIosNativeView.GetAncestorClipRect: TRectF;
var ControlTmp: TControl;
    ClipRect: TRectF;
    AbsoluteOffset: TPointF;
begin
  Result := TRectF.Create(0, 0, FSize.width, FSize.height);
  ControlTmp := Control.ParentControl;
  AbsoluteOffset := Control.Position.Point;
  while (ControlTmp <> nil) do begin
    if ControlTmp.ClipChildren then begin
      ClipRect := TRectF.Create(ControlTmp.ClipRect.TopLeft - AbsoluteOffset, ControlTmp.ClipRect.Width, ControlTmp.ClipRect.Height);
      Result := TRectF.Intersect(Result, ClipRect);
    end;
    AbsoluteOffset := AbsoluteOffset + ControlTmp.Position.Point;
    ControlTmp := ControlTmp.ParentControl;
  end;
end;

{********************************************}
function TALIosNativeView.GetClipView: UIView;
begin
  Result := UIView(FClipView.Super);
end;

{************************************************************}
function TALIosNativeView.GetZOrderManager: TiOSZOrderManager;
begin
  if HasZOrderManager then Result := WindowHandleToPlatform(Form.Handle).ZOrderManager
  else Result := nil;
end;

{****************************************************************************}
function TALIosNativeView.GetFormView(const AForm: TCommonCustomForm): UIView;
var FormHandle: TiOSWindowHandle;
begin
  FormHandle := WindowHandleToPlatform(AForm.Handle);
  Result := FormHandle.View;
end;

{**********************************}
constructor TALIosNativeView.Create;
begin
  inherited;
  InitView;
  View.setExclusiveTouch(True);
  FClipView := TALIosClipView.Create(Self);
  ClipView.initWithFrame(GetViewFrame);
  ClipView.setClipsToBounds(True);
  ClipView.addSubview(View);
  FVisible := True;
end;

{**********************************}
procedure TALIosNativeView.InitView;
var V: Pointer;
begin
  V := UIView(Super).initWithFrame(GetViewFrame);
  if GetObjectID <> V then UpdateObjectID(V);
end;

{************************************************************}
constructor TALIosNativeView.Create(const AControl: TControl);
begin
  FControl := AControl;
  Create;
end;

{**********************************}
destructor TALIosNativeView.Destroy;
begin
  if HasZOrderManager then ZOrderManager.RemoveLink(Control);
  ALFreeAndNil(FClipView);
  inherited;
end;

{********************************************************************************}
function TALIosNativeView.FindParentView([Weak] out AParentView: UIView): Boolean;
var OutView: UIView;
begin
  if HasZOrderManager then begin
    Result := ZOrderManager.FindParentView(Control, OutView);
    AParentView := OutView;
  end
  else begin
    Result := False;
    AParentView := nil;
  end;
end;

{******************************************************}
function TALIosNativeView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALUIView);
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

{*********************************************}
function TALIosNativeView.GetViewFrame: NSRect;
var AbsoluteRect: TRectF;
begin
  AbsoluteRect := Control.AbsoluteRect;
  AbsoluteRect.TopLeft := LocalToParentView(TPointF.Zero);
  Result := CGRectFromRect(AbsoluteRect);
end;

{**************************************************}
function TALIosNativeView.HasZOrderManager: Boolean;
begin
  Result := (Form <> nil) and (Form.Handle <> nil);
end;

{*****************************************************}
function TALIosNativeView.IsParentViewChanged: Boolean;
begin
  Result := (ParentView <> nil) and (ClipView.superview <> NSObjectToID(ParentView));
end;

{*******************************************************************************}
function TALIosNativeView.LocalToParentView(const ALocalPoint: TPointF): TPointF;
begin
  Result := Control.LocalToAbsolute(ALocalPoint);
end;

{*********************************************}
procedure TALIosNativeView.RefreshNativeParent;
begin
  FParentView := nil;
  FindParentView(FParentView);
  if HasZOrderManager then ZOrderManager.UpdateOrder(Control);
  UpdateFrame;
end;

{************************************}
procedure TALIosNativeView.ResetFocus;
begin
  View.resignFirstResponder;
end;

{*************************************}
procedure TALIosNativeView.UpdateFrame;
var LocalClipRect: TRectF;
    AbsoluteClipRect: TRectF;
    ViewRect: TRectF;
    NeedClip: Boolean;
begin
  LocalClipRect := AncestorClipRect;
  AbsoluteClipRect := TRectF.Create(LocalToParentView(LocalClipRect.TopLeft), LocalClipRect.Width, LocalClipRect.Height);
  ClipView.setFrame(CGRectFromRect(AbsoluteClipRect));

  ViewRect := TRectF.Create(-LocalClipRect.TopLeft, FSize.Width, FSize.Height);
  View.setFrame(CGRectFromRect(ViewRect));

  NeedClip := not (ViewRect.TopLeft.IsZero and (ViewRect.Size = AbsoluteClipRect.Size));
  ClipView.setClipsToBounds(NeedClip);
end;

{**********************************}
procedure TALIosNativeView.SetFocus;
begin
  if not View.isFirstResponder then
    View.becomeFirstResponder;
end;

{******************************************************}
procedure TALIosNativeView.SetSize(const ASize: TSizeF);
begin
  FSize := ASize;
  UpdateFrame;
end;

{**************************************************************************}
procedure TALIosNativeView.touchesBegan(touches: NSSet; withEvent: UIEvent);
begin
  UIView(Super).touchesBegan(touches, withEvent);
end;

{******************************************************************************}
procedure TALIosNativeView.touchesCancelled(touches: NSSet; withEvent: UIEvent);
begin
  UIView(Super).touchesCancelled(touches, withEvent);
end;

{**************************************************************************}
procedure TALIosNativeView.touchesEnded(touches: NSSet; withEvent: UIEvent);
begin
  UIView(Super).touchesEnded(touches, withEvent);
end;

{**************************************************************************}
procedure TALIosNativeView.touchesMoved(touches: NSSet; withEvent: UIEvent);
begin
  UIView(Super).touchesMoved(touches, withEvent);
end;

{************************************************}
procedure TALIosNativeView.AncestorVisibleChanged;
begin
  ClipView.setHidden(not Visible or not Control.ParentedVisible);
end;

{*************************************}
procedure TALIosNativeView.ChangeOrder;
begin
  if HasZOrderManager then
    ZOrderManager.UpdateOrder(Control);
end;

{**************************************************************************}
function TALIosNativeView.PointInObjectLocal(X: Single; Y: Single): Boolean;
var HitTestPoint: TPointF;
begin
  HitTestPoint := TPointF.Create(x,y);
  Result := pointInside(CGPointMake(HitTestPoint.X, HitTestPoint.Y), nil);
end;

{*********************************************************}
procedure TALIosNativeView.RootChanged(const aRoot: IRoot);
begin
  // Changing root for native control means changing ZOrderManager, because one form owns ZOrderManager.
  // So we need to remove itself from old one and add to new one.
  if HasZOrderManager then ZOrderManager.RemoveLink(Control);

  if aRoot is TCommonCustomForm then FForm := TCommonCustomForm(aRoot)
  else FForm := nil;

  if HasZOrderManager then ZOrderManager.AddOrSetLink(Control, ClipView, View);
  RefreshNativeParent;
end;

{*******************************************************}
procedure TALIosNativeView.SetAlpha(const Value: Single);
begin
  View.setAlpha(Value);
end;

{***************************************************************}
procedure TALIosNativeView.SetClipChildren(const Value: Boolean);
begin
  View.setClipsToBounds(Value);
end;

{******************************************************************}
procedure TALIosNativeView.SetAbsoluteEnabled(const Value: Boolean);
begin
  View.setUserInteractionEnabled(Value);
  ClipView.setUserInteractionEnabled(Value);
end;

{**********************************************************}
procedure TALIosNativeView.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  ClipView.setHidden(not Visible or not Control.ParentedVisible);
end;

{*********************************************************************************}
function TALIosNativeView.pointInside(point: CGPoint; withEvent: UIEvent): Boolean;
begin
  point := View.convertPoint(point, ClipView);
  Result := View.pointInside(point, withEvent);
end;

{**********************************************************************}
constructor TALIosClipView.Create(const AContentView: TALIosNativeView);
begin
  inherited Create;
  FContentView := AContentView;
end;

{********************************}
destructor TALIosClipView.Destroy;
begin
  UIView(Super).removeFromSuperView;
  UIView(Super).release;
  inherited;
end;

{****************************************************}
function TALIosClipView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALUIClipView);
end;

{*******************************************************************************}
function TALIosClipView.pointInside(point: CGPoint; withEvent: UIEvent): Boolean;
begin
  if ContentView <> nil then Result := ContentView.pointInside(point, withEvent)
  else Result := UIView(Super).pointInside(point, withEvent);
end;

end.
