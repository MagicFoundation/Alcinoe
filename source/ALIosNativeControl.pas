unit ALIosNativeControl;

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
    constructor Create(const AContentView: TALIosNativeView);
    destructor Destroy; override;
    function pointInside(point: CGPoint; withEvent: UIEvent): Boolean; cdecl;
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
    procedure SendDblTapToGestureControl(TouchPoint: TPointF);
    function GetZOrderManager: TiOSZOrderManager;
  protected
    procedure InitView; virtual;
    function GetViewFrame: NSRect;
    function GetFormView(const AForm: TCommonCustomForm): UIView;
    procedure SetSize(const ASize: TSizeF); virtual;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure SetAbsoluteEnabled(const value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetAlpha(const Value: Single);
    procedure SetClipChildren(const Value: Boolean);
    procedure AncestorVisibleChanged;
    function PointInObjectLocal(X: Single; Y: Single): Boolean;
    procedure ChangeOrder;
    procedure RootChanged(const aRoot: IRoot);
    function GetView<T: UIView>: T; overload;
    function FindParentView([Weak] out AParentView: UIView): Boolean; virtual;
    procedure UpdateFrame;
    procedure RefreshNativeParent; virtual;
    function IsParentViewChanged: Boolean;
  public
    constructor Create; overload; virtual;
    constructor Create(const AControl: TControl); overload; virtual;
    destructor Destroy; override;
    function ExtractFirstTouchPoint(touches: NSSet): TPointF;
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    function pointInside(point: CGPoint; withEvent: UIEvent): Boolean; virtual; cdecl;
    function HasControl: Boolean;
    procedure SetFocus; virtual;
    procedure ResetFocus; virtual;
    function LocalToParentView(const ALocalPoint: TPointF): TPointF;
    property Form: TCommonCustomForm read FForm;
    property ZOrderManager: TiOSZOrderManager read GetZOrderManager;
    property AncestorClipRect: TRectF read GetAncestorClipRect;
    property Control: TControl read FControl;
    property ClipView: UIView read GetClipView;
    property ParentView: UIView read FParentView;
    property Size: TSizeF read FSize write SetSize;
    property View: UIView read GetView;
    property ViewFrame: NSRect read GetViewFrame;
    property Visible: Boolean read FVisible;
  end;
  TALIosNativeViewClass = class of TALIosNativeView;

  {*********************************}
  IALUIControl = interface(IALUIView)
    ['{0DD88FF2-E87A-4E2E-A4E6-93601109479E}']
  end;

  {*******************************************}
  TALIosNativeControl = class(TALIosNativeView)
  private
    function GetView: UIControl;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
    procedure RegisterNativeEventHandler(const ASelector: string; const AEvents: UIControlEvents);
    procedure UnRegisterNativeEventHandler(const ASelector: string; const AEvents: UIControlEvents);
  public
    procedure ControlEventEditingDidBegin; virtual; cdecl;
    property View: UIControl read GetView;
  end;

implementation

uses System.UITypes,
     System.SysUtils,
     Macapi.Helpers,
     Macapi.ObjCRuntime,
     FMX.Platform.iOS;

{****************************************************}
function TALIosNativeView.GetAncestorClipRect: TRectF;
var ControlTmp: TControl;
    ClipRect: TRectF;
    AbsoluteOffset: TPointF;
begin
  Result := TRectF.Create(0, 0, FSize.width, FSize.height);
  if Control = nil then Exit;
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
  if Form <> nil then Result := WindowHandleToPlatform(Form.Handle).ZOrderManager
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

{*************************************************************************}
procedure TALIosNativeView.SendDblTapToGestureControl(TouchPoint: TPointF);

  function FindTappedControl: TFmxObject;
  var Obj: IControl;
  begin
    Obj := Form.ObjectAtPoint(Form.ClientToScreen(TouchPoint));
    if Obj <> nil then Result := Obj.GetObject
    else Result := Form;
  end;

var EventInfo: TGestureEventInfo;
    LGObj: IGestureControl;
    GestureControl: TComponent;
begin
  GestureControl := FindTappedControl;
  if Supports(GestureControl, IGestureControl, LGObj) then
    GestureControl := LGObj.GetFirstControlWithGesture(TInteractiveGesture.DoubleTap)
  else
    GestureControl := nil;

  if Supports(GestureControl, IGestureControl, LGObj) then begin
    FillChar(EventInfo, Sizeof(EventInfo), 0);
    EventInfo.Location := TouchPoint;
    EventInfo.GestureID := igiDoubleTap;
    LGObj.CMGesture(EventInfo);
  end;
end;

{************************************************************}
constructor TALIosNativeView.Create(const AControl: TControl);
begin
  FControl := AControl;
  Create;
  RefreshNativeParent;
end;

{**********************************}
destructor TALIosNativeView.Destroy;
begin
  View.removeFromSuperview;
  FClipView.Free;
  inherited;
end;

{************************************************************************}
function TALIosNativeView.ExtractFirstTouchPoint(touches: NSSet): TPointF;
var LocalTouch: UITouch;
    TouchPoint: NSPoint;
begin
  LocalTouch := TUITouch.Wrap(touches.anyObject);
  TouchPoint := LocalTouch.locationInView(GetFormView(Form));
  Result := TPointF.Create(TouchPoint.X, TouchPoint.Y);
end;

{********************************************************************************}
function TALIosNativeView.FindParentView([Weak] out AParentView: UIView): Boolean;
var OutView: UIView;
begin
  if ZOrderManager <> nil then begin
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
  if HasControl then begin
    AbsoluteRect := Control.AbsoluteRect;
    AbsoluteRect.TopLeft := LocalToParentView(TPointF.Zero);
    Result := CGRectFromRect(AbsoluteRect);
  end
  else Result := CGRectMake(0, 0, 0, 0);
end;

{********************************************}
function TALIosNativeView.HasControl: Boolean;
begin
  Result := FControl <> nil;
end;

{*****************************************************}
function TALIosNativeView.IsParentViewChanged: Boolean;
begin
  Result := (ParentView <> nil) and (ClipView.superview <> (ParentView as ILocalObject).GetObjectID);
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
  if HasControl then begin
    FindParentView(FParentView);
    if FParentView = nil then begin
      if ZOrderManager <> nil then ZOrderManager.RemoveLink(Control);
      ClipView.removeFromSuperview
    end
    else
      if IsParentViewChanged then begin
        if ZOrderManager <> nil then begin
          ZOrderManager.UpdateOrder(Control, ClipView);
          ZOrderManager.AddOrSetLink(Control, View);
        end
        else ParentView.addSubview(ClipView);
      end;
    ClipView.setHidden(not FControl.ParentedVisible);
    UpdateFrame;
  end;
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
  if Control = nil then Exit;

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
var
  TouchPoint: TPointF;
begin
  try
    TouchPoint := ExtractFirstTouchPoint(touches);
    if Form <> nil then begin
      Form.MouseMove([ssTouch], TouchPoint.X, TouchPoint.Y);
      Form.MouseMove([], TouchPoint.X, TouchPoint.Y); // Require for correct IsMouseOver handle
      Form.MouseDown(TMouseButton.mbLeft, [ssLeft, ssTouch], TouchPoint.x, TouchPoint.y);
    end;
  finally
    UIView(Super).touchesBegan(touches, withEvent);
  end;
end;

{******************************************************************************}
procedure TALIosNativeView.touchesCancelled(touches: NSSet; withEvent: UIEvent);
var TouchPoint: TPointF;
begin
  try
    TouchPoint := ExtractFirstTouchPoint(touches);
    if Form <> nil then
      Form.MouseUp(TMouseButton.mbLeft, [ssLeft, ssTouch], TouchPoint.x, TouchPoint.y);
    // Don't combine these if statements because in MouseUp the form can unload presentation!
    if Form <> nil then
      Form.MouseLeave;
  finally
    UIView(Super).touchesCancelled(touches, withEvent);
  end;
end;

{**************************************************************************}
procedure TALIosNativeView.touchesEnded(touches: NSSet; withEvent: UIEvent);
var TouchPoint: TPointF;
    Touch: UITouch;
begin
  try
    TouchPoint := ExtractFirstTouchPoint(touches);
    if Form <> nil then
      Form.MouseUp(TMouseButton.mbLeft, [ssLeft, ssTouch], TouchPoint.x, TouchPoint.y);
    // Don't combine these if statements because in MouseUp the form can unload presentation!
    if Form <> nil then
      Form.MouseLeave;

    Touch := TUITouch.Wrap(touches.anyObject);
    if Touch.tapCount = 2 then
      SendDblTapToGestureControl(TouchPoint);
  finally
    UIView(Super).touchesEnded(touches, withEvent);
  end;
end;

{**************************************************************************}
procedure TALIosNativeView.touchesMoved(touches: NSSet; withEvent: UIEvent);
var TouchPoint: TPointF;
begin
  try
    TouchPoint := ExtractFirstTouchPoint(touches);
    if Form <> nil then
      Form.MouseMove([ssLeft, ssTouch], TouchPoint.x, TouchPoint.y);
  finally
    UIView(Super).touchesMoved(touches, withEvent);
  end;
end;

{************************************************}
procedure TALIosNativeView.AncestorVisibleChanged;
begin
  ClipView.setHidden(not Visible or not Control.ParentedVisible);
end;

{*************************************}
procedure TALIosNativeView.ChangeOrder;
begin
  if ZOrderManager <> nil then
    ZOrderManager.UpdateOrder(Control, ClipView);
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
  if aRoot is TCommonCustomForm then FForm := TCommonCustomForm(aRoot)
  else FForm := nil;
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

{********************************************************}
procedure TALIosNativeControl.ControlEventEditingDidBegin;
begin
  Control.SetFocus;
end;

{*********************************************************}
function TALIosNativeControl.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALUIControl)
end;

{**********************************************}
function TALIosNativeControl.GetView: UIControl;
begin
  Result := inherited GetView<UIControl>;
end;

{****************************************************************************************************************}
procedure TALIosNativeControl.RegisterNativeEventHandler(const ASelector: string; const AEvents: UIControlEvents);
begin
  View.addTarget(GetObjectID, sel_getUid(MarshaledAString(TMarshal.AsAnsi(ASelector))), AEvents);
end;

{******************************************************************************************************************}
procedure TALIosNativeControl.UnRegisterNativeEventHandler(const ASelector: string; const AEvents: UIControlEvents);
begin
  View.removeTarget(GetObjectID, sel_getUid(MarshaledAString(TMarshal.AsAnsi(ASelector))), AEvents);
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
