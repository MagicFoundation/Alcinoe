{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2014-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Presentation.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.TypInfo, System.Types, System.Classes, System.Messaging, Macapi.ObjectiveC, iOSapi.Foundation,
  iOSapi.CocoaTypes, iOSapi.UIKit, iOSApi.CoreGraphics, FMX.Graphics, FMX.Controls.Presentation, FMX.Controls,
  FMX.Presentation.Messages, FMX.Forms, FMX.ZOrder.iOS, FMX.Types, FMX.Controls.Model;

type

{ TiOSNativeView }

  TiOSNativeView = class;

  /// <summary>List of native methods (messages), which can received by <c>TiOSClipView</c>.</summary>
  IFMXUIClipView = interface(UIView)
  ['{C045708E-2F18-483F-A214-E58D6199442C}']
    function pointInside(point: CGPoint; withEvent: UIEvent): Boolean; cdecl;
  end;

  /// <summary>Clipping view for <c>TiOSNativeView</c></summary>
  TiOSClipView = class(TOCLocal)
  private
    [Weak] FContentView: TiOSNativeView;
  protected
    /// <summary>Returns class type <c>IFMXUIClipView</c></summary>
    function GetObjectiveCClass: PTypeInfo; override;
  public
    /// <summary>Make redirect request of hit-test to <c>ContentView</c></summary>
    function pointInside(point: CGPoint; withEvent: UIEvent): Boolean; cdecl;
  public
    constructor Create(const AContentView: TiOSNativeView);
    destructor Destroy; override;
    /// <summary>Content view of current ClipView</summary>
    property ContentView: TiOSNativeView read FContentView;
  end;

  /// <summary>
  ///   List of native methods (messages), which can received by native presentation |<b>TiOSNativeView</b>|
  /// </summary>
  IFMXUIView = interface(UIView)
  ['{BA60202C-47E6-4FD2-9999-30EC62C6384C}']
    { Touches }
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
  end;

  /// <summary>
  ///   Basic iOS presentation, which is UIView object.
  ///   It provides positioning, sizing, alignment, supporting touches, order, clipping and focussing. This class allows
  ///   to override native UIView methods
  /// </summary>
  TiOSNativeView = class(TOCLocal)
  private
    [Weak] FParentView: UIView;
    [Weak] FControl: TControl;
    [Weak] FModel: TDataModel;
    [Weak] FParentPlatformControl: TControl;
    [Weak] FForm: TCommonCustomForm;
    FClipView: TiOSClipView;
    FIsParentNativeForm: Boolean;
    FSize: TSizeF;
    FVisible: Boolean;
    function GetAncestorClipRect: TRectF;
    function GetObservers: TObservers;
    function GetView: UIView; overload;
    function GetClipView: UIView;
    procedure SendDblTapToGestureControl(TouchPoint: TPointF);
    function GetZOrderManager: TiOSZOrderManager;
    procedure BeforeDestroyHandleListener(const Sender: TObject; const M: TMessage);
    procedure AfterCreateHandleListener(const Sender: TObject; const M: TMessage);
  protected
    /// <summary>Place of initialization of <see cref="FMX.Presentation.iOS|TiOSNativeView.View">View</see></summary>
    procedure InitView; virtual;
    /// <summary>Calculates frame of <see cref="FMX.Presentation.iOS|TiOSNativeView.View">View</see> in coordinate
    /// systems of <see cref="FMX.Presentation.iOS|TiOSNativeView.ParentView">ParentView</see> by using position and
    /// size of <see cref="FMX.Presentation.iOS|TiOSNativeView.Control">Control</see></summary>
    /// <remarks>If <b>TiOSNativeView</b> doesn't have Control, method returns empty rect.</remarks>
    function GetViewFrame: NSRect;
    /// <summary>Gets <b>UIView</b> of form</summary>
    function GetFormView(const AForm: TCommonCustomForm): UIView;
    /// <summary>Sets new size and refresh position and size of
    /// <see cref="FMX.Presentation.iOS|TiOSNativeView.ClipView">ClipView</see> and
    /// <see cref="FMX.Presentation.iOS|TiOSNativeView.View">View</see></summary>
    procedure SetSize(const ASize: TSizeF); virtual;
    /// <summary>Returns class type of <see cref="FMX.Presentation.iOS|TiOSNativeView.View">View</see></summary>
    function GetObjectiveCClass: PTypeInfo; override;
  protected
    { Messages from PresentationProxy }
    procedure PMGetNativeObject(var AMessage: TDispatchMessageWithValue<IInterface>); message PM_GET_NATIVE_OBJECT;
    procedure PMSetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_ABSOLUTE_ENABLED;
    procedure PMSetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_VISIBLE;
    procedure PMGetVisible(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_GET_VISIBLE;
    procedure PMSetAlpha(var AMessage: TDispatchMessageWithValue<Single>); message PM_SET_ABSOLUTE_OPACITY;
    procedure PMGetAlpha(var AMessage: TDispatchMessageWithValue<Single>); message PM_GET_ABSOLUTE_OPACITY;
    procedure PMSetSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_SET_SIZE;
    procedure PMGetSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_SIZE;
    procedure PMIsFocused(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_IS_FOCUSED;
    procedure PMDoExit(var AMessage: TDispatchMessage); message PM_DO_EXIT;
    procedure PMDoEnter(var AMessage: TDispatchMessage); message PM_DO_ENTER;
    procedure PMResetFocus(var AMessage: TDispatchMessage); message PM_RESET_FOCUS;
    procedure PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_RECOMMEND_SIZE;
    procedure PMSetClipChildren(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_SET_CLIP_CHILDREN;
    procedure PMAncesstorVisibleChanged(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_ANCESSTOR_VISIBLE_CHANGED;
    procedure PMAncesstorPresentationLoaded(var AMessage: TDispatchMessageWithValue<Boolean>); message PM_ANCESTOR_PRESENTATION_LOADED;
    procedure PMRefreshParent(var AMessage: TDispatchMessage); message PM_REFRESH_PARENT;
    procedure PMAbsoluteChanged(var AMessage: TDispatchMessage); message PM_ABSOLUTE_CHANGED;
    procedure PMPointInObject(var AMessage: TDispatchMessageWithValue<TPointInObjectLocalInfo>); message PM_POINT_IN_OBJECT_LOCAL;
    /// <summary>This handler reorders the native view by using order of TControl. TPresentedControl sends it when it
    /// changes order in children list of parent control.</summary>
    procedure PMChangeOrder(var AMessage: TDispatchMessage); message PM_CHANGE_ORDER;
    procedure PMRootChanged(var AMessage: TDispatchMessageWithValue<IRoot>); message PM_ROOT_CHANGED;
  protected
    /// <summary>Returns native UIView, which is casted to type |T|</summary>
    /// <remarks>Type T must be compatible with type of native View</remarks>
    function GetView<T: UIView>: T; overload;
    /// <summary>Searches ancestor TPresentedControl, which has native iOS presentation </summary>
    /// <param name="APlatformControl">Will contains ancestor TPresentedControl, if it was found</param>
    /// <returns>Result of searching: True - found, False - not found</returns>
    function FindAncestorPresentedControl([Weak] out APlatformControl: TControl): Boolean; virtual;
    /// <summary>Searches a parent UIView of <see cref="FMX.Presentation.iOS|TiOSNativeView.View">View</see></summary>
    /// <param name="AParentView">Will contains parent UIView, if it was found</param>
    /// <returns>Result of searching: True - found, False - not found</returns>
    function FindParentView([Weak] out AParentView: UIView): Boolean; virtual;
    /// <summary>Updates size and position of <see cref="FMX.Presentation.iOS|TiOSNativeView.View">View</see> and
    /// <see cref="FMX.Presentation.iOS|TiOSNativeView.ParentView">ParentView</see></summary>
    procedure UpdateFrame;
    /// <summary>Searches and updates ancestor <b>TPresentedControl</b>, which has native presentation. Finds parent
    /// <b>UIView</b>. Refreshes visible of native <b>UIView</b>.
    /// </summary>
    procedure RefreshNativeParent; virtual;
    /// <summary>Is new parent <see cref="FMX.Presentation.iOS|TiOSNativeView.ParentView">ParentView</see> the same as
    /// a real parent of <see cref="FMX.Presentation.iOS|TiOSNativeView.View">View</see>?</summary>
    function IsParentViewChanged: Boolean;
    /// <summary>Defines a class of model. If ancesstor overrides model class, presentation can check class of model
    /// in moment, when presented control send a model.</summary>
    function DefineModelClass: TDataModelClass; virtual;
    /// <summary>Tries to cast current model to type <c>T</c>. If specified type <c>T</c> is not compatible with
    /// <c>Model</c>, return nil.</summary>
    function GetModel<T: TDataModel>: T;
  public
    /// <summary>Extracts first touch and converts it into form's coordinate systems.</summary>
    /// <returns>Coordinate of touch in coordinate systems of form</returns>
    function ExtractFirstTouchPoint(touches: NSSet): TPointF;
    { Touches }
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    { Hit-Testing }
    /// <summary>Returns a Boolean value indicating whether the receiver contains the specified point.</summary>
    function pointInside(point: CGPoint; withEvent: UIEvent): Boolean; virtual; cdecl;
  public
    constructor Create; overload; virtual;
    constructor Create(const AModel: TDataModel; const AControl: TControl); overload; virtual;
    destructor Destroy; override;
    /// <summary>Returns True if native control links with FMX control. Returns False otherwise.</summary>
    function HasControl: Boolean;
    /// <summary>Sets focus of native control, if native control doesn't have a focus</summary>
    procedure SetFocus; virtual;
    /// <summary>Resets focus of native control</summary>
    procedure ResetFocus; virtual;
    /// <summary>Converts <paramref name="ALocalPoint" /> in local coordinate systems of
    /// <see cref="FMX.Presentation.iOS|TiOSNativeView.View">View</see> into coordinates
    /// of <see cref="FMX.Presentation.iOS|TiOSNativeView.ParentView">ParentView</see></summary>
    /// <param name="ALocalPoint">Local coordinate in native presentation</param>
    /// <returns>Absolute coordinates of ALocalPoint in parent native UIView (Form or native presentation)</returns>
    function LocalToParentView(const ALocalPoint: TPointF): TPointF;
    /// <summary>Perform OnKeyDown/OnKeyUp events</summary>
    /// <returns>Return False if key was blocked</returns>
    function PressKey(const AKey: Word; const AKeyChar: Char; const AShift: TShiftState): Boolean;
    /// <summary><see cref="FMX.Presentation.iOS|TiOSNativeView.ParentView">ParentView</see> is view of form?</summary>
    property IsParentNativeForm: Boolean read FIsParentNativeForm;
    /// <summary>The root form, which contains this presentation</summary>
    property Form: TCommonCustomForm read FForm;
    /// <summary>Link to Root form's ZOrderManager</summary>
    property ZOrderManager: TiOSZOrderManager read GetZOrderManager;
    property Model: TDataModel read FModel;
  public
    /// <summary>Clipping rectangle of <see cref="FMX.Presentation.iOS|TiOSNativeView.View">View</see> in coordinate
    /// systems of <see cref="FMX.Presentation.iOS|TiOSNativeView.ParentView">ParentView</see></summary>
    /// <remarks>Clipping rectangle depends from ancestor property
    /// <see cref="FMX.Controls|TControl.ClipChildren">ClipChildren</see></remarks>
    property AncestorClipRect: TRectF read GetAncestorClipRect;
    /// <summary>Presented Control</summary>
    property Control: TControl read FControl;
    /// <summary>Instance of native UIView, which contains <see cref="FMX.Presentation.iOS|TiOSNativeView.View">View</see>
    /// and emulate clipping by <see cref="FMX.Presentation.iOS|TiOSNativeView.AncestorClipRect">AncestorClipRect</see></summary>
    property ClipView: UIView read GetClipView;
    /// <summary>Observers of current <see cref="FMX.Presentation.iOS|TiOSNativeView.Control">Control</see> for
    /// integration with Live Binding</summary>
    /// <remarks>If <see cref="FMX.Presentation.iOS|TiOSNativeView.Control">Control</see> is not linked with current
    /// presentation, result will be nil</remarks>
    property Observers: TObservers read GetObservers;
    /// <summary>Parent of presented control, which has platform presentation </summary>
    property ParentControl: TControl read FParentPlatformControl;
    /// <summary>Native parent of <see cref="FMX.Presentation.iOS|TiOSNativeView.View">View</see></summary>
    property ParentView: UIView read FParentView;
    /// <summary>Size of <see cref="FMX.Presentation.iOS|TiOSNativeView.Control">Control</see></summary>
    property Size: TSizeF read FSize write SetSize;
    /// <summary>Instance of native view of current platform presentation</summary>
    property View: UIView read GetView;
    /// <summary>Frame of native <see cref="FMX.Presentation.iOS|TiOSNativeView.View">View</see> in coordinate systems
    /// of <see cref="FMX.Presentation.iOS|TiOSNativeView.ParentView">ParentView</see></summary>
    property ViewFrame: NSRect read GetViewFrame;
    /// <summary>Visible of <see cref="FMX.Presentation.iOS|TiOSNativeView.Control">Control</see></summary>
    property Visible: Boolean read FVisible;
  end;
  TiOSNativeViewClass = class of TiOSNativeView;

{ TiOSNativeControl }

  /// <summary>
  ///   List of native methods (messages), which can received by native presentation |<b>TiOSNativeControl</b>|
  /// </summary>
  IFMXUIControl = interface(IFMXUIView)
  ['{6C2C163B-3646-4A79-82A6-4FDFB60CF5E5}']
  end;

  /// <summary>
  ///   Adds to |<b>TiOSNativeView</b>| functions for subscription on ObjectiveC Target-Actions
  /// </summary>
  TiOSNativeControl = class(TiOSNativeView)
  private
    function GetView: UIControl;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
    /// <summary>Registration event handler for specifield |<b>Events</b>|</summary>
    /// <param name="ASelector">Name of method (message), which will invoked on reach |<b>Events</b>|</param>
    /// <param name="AEvents">Value of native view events</param>
    procedure RegisterNativeEventHandler(const ASelector: string; const AEvents: UIControlEvents);
    /// <summary>Unregister event handler specified by |<b>Events</b>|</summary>
    procedure UnRegisterNativeEventHandler(const ASelector: string; const AEvents: UIControlEvents);
  public
    /// <summary>Notify, that control begin editing of |View|</summary>
    procedure ControlEventEditingDidBegin; virtual; cdecl;
  public
    property View: UIControl read GetView;
  end;

  /// <summary>Generics proxy for all ios native presentations</summary>
  TiOSPresentationProxy<T: TiOSNativeView, constructor> = class(TPresentationProxy)
  protected
    function CreateReceiver: TObject; override;
  end;

implementation

uses
  System.UITypes, System.SysUtils, Macapi.Helpers, Macapi.ObjCRuntime, FMX.Platform.iOS, FMX.Surfaces, FMX.Helpers.iOS,
  FMX.Consts;

{ TiOSNativeView }

function TiOSNativeView.GetAncestorClipRect: TRectF;
var
  ControlTmp: TControl;
  ClipRect: TRectF;
  AbsoluteOffset: TPointF;
begin
  Result := TRectF.Create(0, 0, FSize.width, FSize.height);
  if Control = nil then
    Exit;
  ControlTmp := Control.ParentControl;
  AbsoluteOffset := Control.Position.Point;
  while (ControlTmp <> nil) and (ControlTmp <> FParentPlatformControl) do
  begin
    if ControlTmp.ClipChildren then
    begin
      ClipRect := TRectF.Create(ControlTmp.ClipRect.TopLeft - AbsoluteOffset, ControlTmp.ClipRect.Width, ControlTmp.ClipRect.Height);
      Result := TRectF.Intersect(Result, ClipRect);
    end;
    AbsoluteOffset := AbsoluteOffset + ControlTmp.Position.Point;
    ControlTmp := ControlTmp.ParentControl;
  end;
end;

function TiOSNativeView.GetClipView: UIView;
begin
  Result := UIView(FClipView.Super);
end;

function TiOSNativeView.GetZOrderManager: TiOSZOrderManager;
begin
  if Form <> nil then
    Result := WindowHandleToPlatform(Form.Handle).ZOrderManager
  else
    Result := nil;
end;

function TiOSNativeView.GetFormView(const AForm: TCommonCustomForm): UIView;
var
  FormHandle: TiOSWindowHandle;
begin
  FormHandle := WindowHandleToPlatform(AForm.Handle);
  Result := FormHandle.View;
end;

function TiOSNativeView.GetModel<T>: T;
begin
  Result := FModel as T;
end;

constructor TiOSNativeView.Create;
begin
  inherited;
  InitView;
  View.setExclusiveTouch(True);
  FClipView := TiOSClipView.Create(Self);
  ClipView.initWithFrame(GetViewFrame);
  ClipView.setClipsToBounds(True);
  ClipView.addSubview(View);
  FVisible := True;
  FParentPlatformControl := nil;
  FIsParentNativeForm := False;

  TMessageManager.DefaultManager.SubscribeToMessage(TBeforeDestroyFormHandle, BeforeDestroyHandleListener);
  TMessageManager.DefaultManager.SubscribeToMessage(TAfterCreateFormHandle, AfterCreateHandleListener);
end;

procedure TiOSNativeView.InitView;
var
  V: Pointer;
begin
  V := UIView(Super).initWithFrame(GetViewFrame);
  if GetObjectID <> V then
    UpdateObjectID(V);
end;

procedure TiOSNativeView.SendDblTapToGestureControl(TouchPoint: TPointF);

  function FindTappedControl: TFmxObject;
  var
    Obj: IControl;
  begin
    Obj := Form.ObjectAtPoint(Form.ClientToScreen(TouchPoint));
    if Obj <> nil then
      Result := Obj.GetObject
    else
      Result := Form;
  end;

var
  EventInfo: TGestureEventInfo;
  LGObj: IGestureControl;
  GestureControl: TComponent;
begin
  GestureControl := FindTappedControl;
  if Supports(GestureControl, IGestureControl, LGObj) then
    GestureControl := LGObj.GetFirstControlWithGesture(TInteractiveGesture.DoubleTap)
  else
    GestureControl := nil;

  if Supports(GestureControl, IGestureControl, LGObj) then
  begin
    FillChar(EventInfo, Sizeof(EventInfo), 0);
    EventInfo.Location := TouchPoint;
    EventInfo.GestureID := igiDoubleTap;
    LGObj.CMGesture(EventInfo);
  end;
end;

procedure TiOSNativeView.AfterCreateHandleListener(const Sender: TObject; const M: TMessage);
begin
  if (M is TAfterCreateFormHandle) and IsParentNativeForm then
    RefreshNativeParent;
end;

procedure TiOSNativeView.BeforeDestroyHandleListener(const Sender: TObject; const M: TMessage);
begin
  if (M is TBeforeDestroyFormHandle) and IsParentNativeForm then
    ClipView.removeFromSuperview;
end;

constructor TiOSNativeView.Create(const AModel: TDataModel; const AControl: TControl);
begin
  FControl := AControl;
  FModel := AModel;
  if FModel is DefineModelClass then
    FModel.Receiver := Self
  else
    raise EPresentationWrongModel.CreateFmt(SWrongModelClassType, [DefineModelClass.ClassName, FModel.ClassName]);

  Create;
  RefreshNativeParent;
end;

function TiOSNativeView.DefineModelClass: TDataModelClass;
begin
  Result := TDataModel;
end;

destructor TiOSNativeView.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TBeforeDestroyFormHandle, BeforeDestroyHandleListener);
  TMessageManager.DefaultManager.Unsubscribe(TAfterCreateFormHandle, AfterCreateHandleListener);
  View.removeFromSuperview;
  FClipView.Free;
  inherited;
end;

function TiOSNativeView.ExtractFirstTouchPoint(touches: NSSet): TPointF;
var
  LocalTouch: UITouch;
  TouchPoint: NSPoint;
begin
  LocalTouch := TUITouch.Wrap(touches.anyObject);
  TouchPoint := LocalTouch.locationInView(GetFormView(Form));
  Result := TPointF.Create(TouchPoint.X, TouchPoint.Y);
end;

function TiOSNativeView.FindAncestorPresentedControl([Weak] out APlatformControl: TControl): Boolean;
var
  OutControl: TControl;
begin
  if ZOrderManager <> nil then
  begin
    Result := ZOrderManager.FindParentControl(Control, OutControl);
    APlatformControl := OutControl;
  end
  else
  begin
    Result := False;
    APlatformControl := nil;
  end;
end;

function TiOSNativeView.FindParentView([Weak] out AParentView: UIView): Boolean;
var
  OutView: UIView;
begin
  if ZOrderManager <> nil then
  begin
    Result := ZOrderManager.FindParentView(Control, OutView);
    AParentView := OutView;
  end
  else
  begin
    Result := False;
    AParentView := nil;
  end;
end;

function TiOSNativeView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IFMXUIView);
end;

function TiOSNativeView.GetObservers: TObservers;
begin
  if Control <> nil then
    Result := Control.Observers
  else
    Result := nil;
end;

function TiOSNativeView.GetView: UIView;
begin
  Result := GetView<UIView>;
end;

function TiOSNativeView.GetView<T>: T;
begin
  Result := T(Super);
end;

function TiOSNativeView.GetViewFrame: NSRect;
var
  AbsoluteRect: TRectF;
begin
  if HasControl then
  begin
    AbsoluteRect := Control.AbsoluteRect;
    AbsoluteRect.TopLeft := LocalToParentView(TPointF.Zero);
    Result := CGRectFromRect(AbsoluteRect);
  end
  else
    Result := CGRectMake(0, 0, 0, 0);
end;

function TiOSNativeView.HasControl: Boolean;
begin
  Result := FControl <> nil;
end;

function TiOSNativeView.IsParentViewChanged: Boolean;
begin
  Result := (ParentView <> nil) and (ClipView.superview <> (ParentView as ILocalObject).GetObjectID);
end;

function TiOSNativeView.LocalToParentView(const ALocalPoint: TPointF): TPointF;
var
  AbsolutePos: TPointF;
  ParentTmp: TControl;
  FoundPresentedControl: Boolean;
  ControlSettings: IIgnoreControlPosition;
begin
  if IsParentNativeForm then
    Result := Control.LocalToAbsolute(ALocalPoint)
  else
  begin
    ParentTmp := FControl.ParentControl;
    FoundPresentedControl := False;
    AbsolutePos := FControl.Position.Point;
    while (ParentTmp <> nil) and not FoundPresentedControl do
    begin
      if ParentTmp is TPresentedControl then
        FoundPresentedControl := True
      else
      begin
        if not Supports(ParentTmp, IIgnoreControlPosition, ControlSettings) or ((ControlSettings <> nil) and
           not ControlSettings.GetIgnoreControlPosition) then
          AbsolutePos := AbsolutePos + ParentTmp.Position.Point;
        ParentTmp := ParentTmp.ParentControl;
      end;
    end;

    if FoundPresentedControl then
      Result := AbsolutePos
    else
      Result := Control.LocalToAbsolute(ALocalPoint);
  end;
end;

function TiOSNativeView.PressKey(const AKey: Word; const AKeyChar: Char; const AShift: TShiftState): Boolean;
var
  Key: Word;
  KeyChar: Char;
begin
  Result := True;
  Key := AKey;
  KeyChar := AKeyChar;
  Form.KeyDown(Key, KeyChar, AShift);
  if (Key <> AKey) or (KeyChar <> AKeyChar) then
  begin
    //Blocking input. Documentation provides information about filtering in KeyDown only.
    Result := False;
    Key := AKey;
    KeyChar := AKeyChar;
  end;
  Form.KeyUp(Key, KeyChar, AShift);
end;

procedure TiOSNativeView.PMAbsoluteChanged(var AMessage: TDispatchMessage);
begin
  UpdateFrame;
end;

procedure TiOSNativeView.RefreshNativeParent;
begin
  FParentView := nil;
  FParentPlatformControl := nil;
  if HasControl then
  begin
    FIsParentNativeForm := not FindAncestorPresentedControl(FParentPlatformControl);
    FindParentView(FParentView);
    if FParentView = nil then
    begin
      if ZOrderManager <> nil then
        ZOrderManager.RemoveLink(Control);
      ClipView.removeFromSuperview;
    end
    else
      if IsParentViewChanged then
      begin
        if ZOrderManager <> nil then
        begin
          ZOrderManager.UpdateOrder(Control, ClipView);
          ZOrderManager.AddOrSetLink(Control, View);
        end
        else
          ParentView.addSubview(ClipView);
      end;
    ClipView.setHidden(not FControl.ParentedVisible);
    UpdateFrame;
  end;
end;

procedure TiOSNativeView.ResetFocus;
begin
  View.resignFirstResponder;
end;

procedure TiOSNativeView.UpdateFrame;
var
  LocalClipRect: TRectF;
  AbsoluteClipRect: TRectF;
  ViewRect: TRectF;
  NeedClip: Boolean;
begin
  if Control = nil then
    Exit;

  LocalClipRect := AncestorClipRect;
  AbsoluteClipRect := TRectF.Create(LocalToParentView(LocalClipRect.TopLeft), LocalClipRect.Width, LocalClipRect.Height);
  ClipView.setFrame(CGRectFromRect(AbsoluteClipRect));

  ViewRect := TRectF.Create(-LocalClipRect.TopLeft, FSize.Width, FSize.Height);
  View.setFrame(CGRectFromRect(ViewRect));

  NeedClip := not (ViewRect.TopLeft.IsZero and (ViewRect.Size = AbsoluteClipRect.Size));
  ClipView.setClipsToBounds(NeedClip);
end;

procedure TiOSNativeView.SetFocus;
begin
  if not View.isFirstResponder then
    View.becomeFirstResponder;
end;

procedure TiOSNativeView.SetSize(const ASize: TSizeF);
begin
  FSize := ASize;
  UpdateFrame;
end;

procedure TiOSNativeView.touchesBegan(touches: NSSet; withEvent: UIEvent);
var
  TouchPoint: TPointF;
begin
  try
    TouchPoint := ExtractFirstTouchPoint(touches);
    if Form <> nil then
    begin
      Form.MouseMove([ssTouch], TouchPoint.X, TouchPoint.Y);
      Form.MouseMove([], TouchPoint.X, TouchPoint.Y); // Require for correct IsMouseOver handle
      Form.MouseDown(TMouseButton.mbLeft, [ssLeft, ssTouch], TouchPoint.x, TouchPoint.y);
    end;
  finally
    UIView(Super).touchesBegan(touches, withEvent);
  end;
end;

procedure TiOSNativeView.touchesCancelled(touches: NSSet; withEvent: UIEvent);
var
  TouchPoint: TPointF;
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

procedure TiOSNativeView.touchesEnded(touches: NSSet; withEvent: UIEvent);
var
  TouchPoint: TPointF;
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

procedure TiOSNativeView.touchesMoved(touches: NSSet; withEvent: UIEvent);
var
  TouchPoint: TPointF;
begin
  try
    TouchPoint := ExtractFirstTouchPoint(touches);
    if Form <> nil then
      Form.MouseMove([ssLeft, ssTouch], TouchPoint.x, TouchPoint.y);
  finally
    UIView(Super).touchesMoved(touches, withEvent);
  end;
end;

procedure TiOSNativeView.PMAncesstorPresentationLoaded(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  RefreshNativeParent;
end;

procedure TiOSNativeView.PMAncesstorVisibleChanged(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  ClipView.setHidden(not Visible or not Control.ParentedVisible);
end;

procedure TiOSNativeView.PMChangeOrder(var AMessage: TDispatchMessage);
begin
  if ZOrderManager <> nil then
    ZOrderManager.UpdateOrder(Control, ClipView);
end;

procedure TiOSNativeView.PMGetAlpha(var AMessage: TDispatchMessageWithValue<Single>);
begin
  AMessage.Value := View.Alpha;
end;

procedure TiOSNativeView.PMGetNativeObject(var AMessage: TDispatchMessageWithValue<IInterface>);
begin
  AMessage.Value := View;
end;

procedure TiOSNativeView.PMGetSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
var
  Size: NSSize;
begin
  Size := View.Bounds.Size;
  AMessage.Value := TSizeF.Create(Size.width, Size.height);
end;

procedure TiOSNativeView.PMGetVisible(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  AMessage.Value := not ClipView.isHidden;
end;

procedure TiOSNativeView.PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
var
  RequiredSize: NSSize;
  BoundedSize: NSSize;
begin
  RequiredSize := CGSizeMake(AMessage.Value.Width, AMessage.Value.Height);
  BoundedSize := View.SizeThatFits(RequiredSize);
  AMessage.Value := TSizeF.Create(BoundedSize.width, BoundedSize.height);
end;

procedure TiOSNativeView.PMIsFocused(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  AMessage.Value := View.isFirstResponder;
end;

procedure TiOSNativeView.PMPointInObject(var AMessage: TDispatchMessageWithValue<TPointInObjectLocalInfo>);
var
  HitTestPoint: TPointF;
begin
  HitTestPoint := AMessage.Value.Point;
  AMessage.Value.Result := pointInside(CGPointMake(HitTestPoint.X, HitTestPoint.Y), nil);
end;

procedure TiOSNativeView.PMRefreshParent(var AMessage: TDispatchMessage);
begin
  RefreshNativeParent;
end;

procedure TiOSNativeView.PMResetFocus(var AMessage: TDispatchMessage);
begin
  ResetFocus;
end;

procedure TiOSNativeView.PMRootChanged(var AMessage: TDispatchMessageWithValue<IRoot>);
begin
  if AMessage.Value is TCommonCustomForm then
    FForm := TCommonCustomForm(AMessage.Value)
  else
    FForm := nil;
  RefreshNativeParent;
end;

procedure TiOSNativeView.PMDoExit(var AMessage: TDispatchMessage);
begin
  ResetFocus;
end;

procedure TiOSNativeView.PMSetAlpha(var AMessage: TDispatchMessageWithValue<Single>);
begin
  View.setAlpha(AMessage.Value);
end;

procedure TiOSNativeView.PMSetClipChildren(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  View.setClipsToBounds(AMessage.Value);
end;

procedure TiOSNativeView.PMSetAbsoluteEnabled(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  View.setUserInteractionEnabled(AMessage.Value);
  ClipView.setUserInteractionEnabled(AMessage.Value);
end;

procedure TiOSNativeView.PMDoEnter(var AMessage: TDispatchMessage);
begin
  SetFocus;
end;

procedure TiOSNativeView.PMSetSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
begin
  Size := AMessage.Value;
end;

procedure TiOSNativeView.PMSetVisible(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  FVisible := AMessage.Value;
  ClipView.setHidden(not Visible or not Control.ParentedVisible);
end;

function TiOSNativeView.pointInside(point: CGPoint; withEvent: UIEvent): Boolean;
begin
  point := View.convertPoint(point, ClipView);
  Result := View.pointInside(point, withEvent);
end;

{ TiOSNativeControl }

procedure TiOSNativeControl.ControlEventEditingDidBegin;
begin
  Control.SetFocus;
end;

function TiOSNativeControl.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IFMXUIControl)
end;

function TiOSNativeControl.GetView: UIControl;
begin
  Result := inherited GetView<UIControl>;
end;

procedure TiOSNativeControl.RegisterNativeEventHandler(const ASelector: string; const AEvents: UIControlEvents);
begin
  View.addTarget(GetObjectID, sel_getUid(MarshaledAString(TMarshal.AsAnsi(ASelector))), AEvents);
end;

procedure TiOSNativeControl.UnRegisterNativeEventHandler(const ASelector: string; const AEvents: UIControlEvents);
begin
  View.removeTarget(GetObjectID, sel_getUid(MarshaledAString(TMarshal.AsAnsi(ASelector))), AEvents);
end;

{ TiOSClipView }

constructor TiOSClipView.Create(const AContentView: TiOSNativeView);
begin
  inherited Create;
  FContentView := AContentView;
end;

destructor TiOSClipView.Destroy;
begin
  UIView(Super).removeFromSuperView;
  UIView(Super).release;
  inherited;
end;

function TiOSClipView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IFMXUIClipView);
end;

function TiOSClipView.pointInside(point: CGPoint; withEvent: UIEvent): Boolean;
begin
  if ContentView <> nil then
    Result := ContentView.pointInside(point, withEvent)
  else
    Result := UIView(Super).pointInside(point, withEvent);
end;

{ TiOSPresentationProxy<TPresentation> }

function TiOSPresentationProxy<T>.CreateReceiver: TObject;
var
  PresentationClass: TiOSNativeViewClass;
begin
  PresentationClass := T;
  Result := PresentationClass.Create(Model, PresentedControl);
end;

end.
