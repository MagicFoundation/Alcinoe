{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2014-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.ScrollBox.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.TypInfo, Macapi.ObjectiveC, iOSapi.UIKit, iOSapi.CoreGraphics, iOSapi.Foundation, iOSapi.CocoaTypes,
  FMX.Controls, FMX.Controls.Presentation, FMX.Presentation.Messages, FMX.Presentation.iOS, FMX.ScrollBox,
  FMX.BehaviorManager, FMX.Controls.Model;

type
  TiOSScrollBoxDelegate = class;

  /// <summary>
  ///   List of native methods (messages), which can received by native presentation <c>TiOSScrollBox</c>
  /// </summary>
  IFMXUIScrollView = interface(UIScrollView)
  ['{0403CDE7-1347-4E4F-9AA8-0AF4F2D437B3}']
    { Touches }
    /// <summary>Override <c>touchesBegan</c> method of UIScrollView.</summary>
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    /// <summary>Override <c>touchesCancelled</c> method of UIScrollView.</summary>
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    /// <summary>Override <c>touchesEnded</c> method of UIScrollView.</summary>
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    /// <summary>Override <c>touchesMoved</c> method of UIScrollView.</summary>
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    { Gestures }
    /// <summary>Processes <c>TiOSScrollBox.PanRecognizer</c>.</summary>
    procedure HandlePan(gestureRecognizer: UIPanGestureRecognizer); cdecl;
    /// <summary>Override <c>gestureRecognizerShouldBegin</c> method of UIScrollView.</summary>
    function gestureRecognizerShouldBegin(gestureRecognizer: UIGestureRecognizer): Boolean; cdecl;
  end;

  /// <summary>
  ///   Native presentation for <c>TScrollBox</c>, whis is a UIScrollView object.
  /// </summary>
  TiOSScrollBox = class(TiOSNativeView)
  private
    [Weak] FModel: TCustomScrollBoxModel;
    FDelegate: TiOSScrollBoxDelegate;
    FPanRecognizer: UIPanGestureRecognizer;
    FPanGestureRecognized: Boolean;
    function GetView: UIScrollView;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
    function DefineModelClass: TDataModelClass; override;
  protected
    /// <summary>Creates delegate for <c>UIScrollView</c>. Successors can override it for creating custom delegate and directly
    /// receiving of native events of <c>UIScrollView</c></summary>
    function CreateDelegate: TiOSScrollBoxDelegate; virtual;
    { Messages from Model }
    /// <summary>Notification about changing <c>TCustomScrollBoxModel.Bounces</c> from PresentedControl</summary>
    procedure MMBouncesChanged(var AMessage: TDispatchMessageWithValue<TBehaviorBoolean>); message MM_BOUNCES_CHANGED;
    /// <summary>Notification about changing visibility of Scroll Bars <c>TCustomScrollBoxModel.ShowScrollBar</c>
    /// from PresentedControl</summary>
    procedure MMShowScrollBarChanged(var AMessage: TDispatchMessageWithValue<Boolean>); message MM_SHOW_SCROLLBAR_CHANGED;
    /// <summary>Notification about changing <c>TCustomScrollBoxModel.EnabledScroll</c> from PresentedControl</summary>
    procedure MMScrollEnabledChanged(var AMessage: TDispatchMessageWithValue<Boolean>); message MM_ENABLED_SCROLL_CHANGED;
    /// <summary>Notification about need to set new content size from PresentedControl</summary>
    procedure MMSetContentBounds(var AMessage: TDispatchMessageWithValue<TRectF>); message MM_SET_CONTENT_BOUNDS;
    { Messages from PresentationProxy }
    /// <summary>Notification about initialization of presentation</summary>
    procedure PMInit(var AMessage: TDispatchMessage); message PM_INIT;
    procedure PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_GET_RECOMMEND_SIZE;
    /// <summary>Settings scontent (<c>TScrollContent</c>) of TScrollBox</summary>
    procedure PMSetContent(var Message: TDispatchMessageWithValue<TScrollContent>); message PM_SET_CONTENT;
    /// <summary>Notification about need to scroll viewport to specified rect</summary>
    procedure PMScrollInRect(var AMessage: TDispatchMessageWithValue<TCustomScrollBoxModel.TInViewRectInfo>); message PM_SCROLL_IN_RECT;
    /// <summary>Notification about need to scroll viewport by specified vector</summary>
    procedure PMScrollBy(var AMessage: TDispatchMessageWithValue<TCustomScrollBoxModel.TScrollByInfo>); message PM_SCROLL_BY;
    /// <summary>Notification about need to set new viewport position</summary>
    procedure PMSetViewPortPosition(var AMessage: TDispatchMessageWithValue<TPointF>); message MM_SET_VIEWPORT_POSITION;
    /// <summary>Getter of viewport position from presentation (native control)</summary>
    procedure PMGetViewPortPosition(var AMessage: TDispatchMessageWithValue<TPointF>); message MM_GET_VIEWPORT_POSITION;
    /// <summary>Getter of viewport size from presentation (native control)</summary>
    procedure PMGetViewPortSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message MM_GET_VIEWPORT_SIZE;
  public
    /// <summary>Default constructor. Creates instance of delegate and pan gesture recognizer for correcting of
    /// FireMonkey MouseXXX events system.</summary>
    constructor Create; override;
    // <summary>Destroys ScrollBox delegate and pan gesture recognizer.</summary>
    destructor Destroy; override;
  public
    { Gestures }
    /// <summary>Is caused when our gesture <c>PanRecognizer</c> is recognized. Processes OnMouseMove, OnMouseUp  and
    /// MouseLeave events of <c>TPresentedControl</c></summary>
    procedure HandlePan(gestureRecognizer: UIPanGestureRecognizer); cdecl;
    /// <summary>Controls actuating of gestures. If <c>gestureRecognizer</c> is our <c>PanRecognizer</c> and
    /// if hovered control cannot process Pan gesture, we reject our <c>PanRecognizer</c></summary>
    function gestureRecognizerShouldBegin(gestureRecognizer: UIGestureRecognizer): Boolean; cdecl;
    { Touches }
    /// <summary>Filters of MouseUp event, if our <c>PanRecognizer</c> was recognized. We don't need to invoke OnMouseUp,
    /// because <c>PanRecognizer</c> continue  processing of MouseMove and MouseUp events. </summary>
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
  public
    /// <summary>Delegate of <c>UIScrollView</c></summary>
    property Delegate: TiOSScrollBoxDelegate read FDelegate;
    /// <summary>Model of <c>TScrollBox</c></summary>
    property Model: TCustomScrollBoxModel read FModel;
    /// <summary>Instance of native scroll view <c>UIScrollView</c> of current platform presentation</summary>
    property View: UIScrollView read GetView;
    /// <summary>Pan gesture recognizer used for correct working of MouseXXX events sysmtem of FMX. It's not the same
    /// as <c>UIScrollView.panGestureRecognizer</c></summary>
    property PanRecognizer: UIPanGestureRecognizer read FPanRecognizer;
  end;

  /// <summary>
  ///   Base delegate for <c>UIScrollView</c>, which implements <c>UIScrollViewDelegate</c> protocol. Receives events,
  ///   moves a content of <c>TScrollBox</c> and invokes events of <c>TScrollBox</c>
  /// </summary>
  TiOSScrollBoxDelegate = class(TOCLocal, UIScrollViewDelegate)
  private
    [Weak] FModel: TCustomScrollBoxModel;
    [Weak] FControl: TControl;
    [Weak] FContent: TScrollContent;
    FPreviousOffset: CGPoint;
  public
    { UIScrollViewDelegate }
    procedure scrollViewDidEndDecelerating(scrollView: UIScrollView); cdecl;
    procedure scrollViewDidEndDragging(scrollView: UIScrollView; willDecelerate: Boolean); cdecl;
    procedure scrollViewDidEndScrollingAnimation(scrollView: UIScrollView); cdecl;
    procedure scrollViewDidEndZooming(scrollView: UIScrollView; withView: UIView; atScale: CGFloat); cdecl;
    procedure scrollViewDidScroll(scrollView: UIScrollView); cdecl;
    procedure scrollViewDidScrollToTop(scrollView: UIScrollView); cdecl;
    procedure scrollViewDidZoom(scrollView: UIScrollView); cdecl;
    function scrollViewShouldScrollToTop(scrollView: UIScrollView): Boolean; cdecl;
    procedure scrollViewWillBeginDecelerating(scrollView: UIScrollView); cdecl;
    procedure scrollViewWillBeginDragging(scrollView: UIScrollView); cdecl;
    procedure scrollViewWillBeginZooming(scrollView: UIScrollView; withView: UIView); cdecl;
    procedure scrollViewWillEndDragging(scrollView: UIScrollView; withVelocity: CGPoint; targetContentOffset: Pointer); cdecl;
    function viewForZoomingInScrollView(scrollView: UIScrollView): UIView; cdecl;
  public
    /// <summary>Returns True if Delegate has a model of <c>TScrollBox</c>. Returns False otherwise.</summary>
    function HasModel: Boolean;
    /// <summary>Presented control. Can be nil</summary>
    property Control: TControl read FControl write FControl;
    /// <summary>Content of <c>TScrollBox</c>. Content can be nil</summary>
    property Content: TScrollContent read FContent write FContent;
    /// <summary>Model of <c>TScrollBox</c>.</summary>
    property Model: TCustomScrollBoxModel read FModel write FModel;
  end;

implementation

uses
  System.SysUtils, System.UITypes, System.Classes, System.Math, Macapi.ObjCRuntime, FMX.Types, FMX.Consts, FMX.Forms,
  FMX.Presentation.Factory, FMX.Platform.iOS;

function BehaviorBooleanToBool(const AValue: TBehaviorBoolean): Boolean;
begin
  Result := AValue <> TBehaviorBoolean.False;
end;

{ TiOSScrollBox }

procedure TiOSScrollBox.PMInit(var AMessage: TDispatchMessage);
var
  ContentBounds: TRectF;
begin
  if Model <> nil then
    ContentBounds := Model.ContentBounds;
  View.setContentSize(CGSizeMake(ContentBounds.Width, ContentBounds.Height));
  View.setBounces(BehaviorBooleanToBool(Model.Bounces));
  View.setShowsHorizontalScrollIndicator(Model.ShowScrollBars);
  View.setShowsVerticalScrollIndicator(Model.ShowScrollBars);
  View.setScrollEnabled(Model.EnabledScroll);
  View.setContentSize(CGSizeMake(Model.ContentSize.Width, Model.ContentSize.Height));
end;

procedure TiOSScrollBox.PMScrollInRect(var AMessage: TDispatchMessageWithValue<TCustomScrollBoxModel.TInViewRectInfo>);
var
  NewViewPort: TRectF;
begin
  NewViewPort := AMessage.Value.Rect;
  View.scrollRectToVisible(CGRectMake(NewViewPort.Left, NewViewPort.Top, NewViewPort.Width, NewViewPort.Height),
    AMessage.Value.Animated);
end;

procedure TiOSScrollBox.MMSetContentBounds(var AMessage: TDispatchMessageWithValue<TRectF>);
begin
  View.setContentSize(CGSizeMake(AMessage.Value.Width, AMessage.Value.Height));
end;

procedure TiOSScrollBox.PMSetContent(var Message: TDispatchMessageWithValue<TScrollContent>);
begin
  if Message.Value = nil then
    raise EArgumentException.Create(SContentCannotBeNil);
  Delegate.Content := Message.Value;
end;

procedure TiOSScrollBox.PMSetViewPortPosition(var AMessage: TDispatchMessageWithValue<TPointF>);
var
  NewPosition: TPointF;
begin
  NewPosition := AMessage.Value;
  View.setContentOffset(CGPointMake(NewPosition.X, NewPosition.Y), True);
end;

procedure TiOSScrollBox.touchesCancelled(touches: NSSet; withEvent: UIEvent);
begin
  // A recognized gesture cancels Touches and sends TouchesCancelled message. In this case we don't have to
  // send MouseUp event to our scene.
  if not FPanGestureRecognized then
    inherited;
end;

procedure TiOSScrollBox.PMScrollBy(var AMessage: TDispatchMessageWithValue<TCustomScrollBoxModel.TScrollByInfo>);
var
  ContentOffset: NSPoint;
begin
  ContentOffset := View.contentOffset;
  ContentOffset.x := ContentOffset.x + AMessage.Value.Vector.X;
  ContentOffset.y := ContentOffset.y + AMessage.Value.Vector.Y;
  View.setContentOffset(ContentOffset, AMessage.Value.Animated);
end;

constructor TiOSScrollBox.Create;
begin
  inherited;
  FModel := TCustomScrollBoxModel(inherited Model);
  FModel.Receiver := Self;
  FDelegate := CreateDelegate;
  FDelegate.Model := Model;
  FDelegate.Control := Control;
  View.setDelegate(FDelegate.GetObjectID);
  FPanGestureRecognized := False;
  FPanRecognizer := TUIPanGestureRecognizer.Alloc;
  FPanRecognizer.initWithTarget(GetObjectID, sel_getUid('HandlePan:'));
  FPanRecognizer.setDelaysTouchesBegan(False);
  FPanRecognizer.setCancelsTouchesInView(True);
  View.addGestureRecognizer(FPanRecognizer);
  View.setClipsToBounds(True);
end;

function TiOSScrollBox.CreateDelegate: TiOSScrollBoxDelegate;
begin
  Result := TiOSScrollBoxDelegate.Create;
end;

function TiOSScrollBox.DefineModelClass: TDataModelClass;
begin
  Result := TCustomScrollBoxModel;
end;

destructor TiOSScrollBox.Destroy;
begin
  FPanRecognizer.release;
  View.setDelegate(nil);
  FDelegate.Free;
  inherited;
end;

function TiOSScrollBox.gestureRecognizerShouldBegin(gestureRecognizer: UIGestureRecognizer): Boolean;

  function IsOurPanGesture: Boolean;
  begin
    Result := (gestureRecognizer as ILocalObject).GetObjectID = (FPanRecognizer as ILocalObject).GetObjectID;
  end;

var
  Captured: IControl;
  CapturedControl: IGestureControl;
begin
  if (Form <> nil) and IsOurPanGesture then
  begin
    Captured := Form.Hovered;
    if Supports(Captured, IGestureControl, CapturedControl) then
      Result := CapturedControl.GetFirstControlWithGesture(TInteractiveGesture.Pan) <> nil
    else
      Result := False;
  end
  else
    Result := View.gestureRecognizerShouldBegin(gestureRecognizer);
end;

function TiOSScrollBox.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IFMXUIScrollView);
end;

function TiOSScrollBox.GetView: UIScrollView;
begin
  Result := inherited GetView<UIScrollView>;
end;

procedure TiOSScrollBox.HandlePan(gestureRecognizer: UIPanGestureRecognizer);
var
  TouchPoint: NSPoint;
begin
  if Form <> nil then
  begin
    TouchPoint := gestureRecognizer.locationInView(WindowHandleToPlatform(Form.Handle).View);
    case gestureRecognizer.state of
      UIGestureRecognizerStateBegan:
        FPanGestureRecognized := True;
      UIGestureRecognizerStateChanged:
        Form.MouseMove([ssLeft, ssTouch], TouchPoint.x, TouchPoint.y);
    else
      FPanGestureRecognized := False;
      Form.MouseUp(TMouseButton.mbLeft, [ssLeft, ssTouch], TouchPoint.x, TouchPoint.y);
      Form.MouseLeave;
    end;
  end;
end;

procedure TiOSScrollBox.MMBouncesChanged(var AMessage: TDispatchMessageWithValue<TBehaviorBoolean>);
begin
  View.setBounces(BehaviorBooleanToBool(AMessage.Value));
end;

procedure TiOSScrollBox.MMScrollEnabledChanged(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  View.setScrollEnabled(AMessage.Value);
end;

procedure TiOSScrollBox.MMShowScrollBarChanged(var AMessage: TDispatchMessageWithValue<Boolean>);
begin
  View.setShowsHorizontalScrollIndicator(AMessage.Value);
  View.setShowsVerticalScrollIndicator(AMessage.Value);
end;

procedure TiOSScrollBox.PMGetRecommendSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
begin
end;

procedure TiOSScrollBox.PMGetViewPortPosition(var AMessage: TDispatchMessageWithValue<TPointF>);
var
  ContentOffset: NSPoint;
begin
  ContentOffset := View.contentOffset;
  AMessage.Value := TPointF.Create(ContentOffset.X, ContentOffset.Y);
end;

procedure TiOSScrollBox.PMGetViewPortSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
var
  LNSSize: NSSize;
begin
  LNSSize := View.bounds.size;
  AMessage.Value := TSizeF.Create(LNSSize.width, LNSSize.height);
end;

{ TiOSScrollBoxDelegate }

function TiOSScrollBoxDelegate.HasModel: Boolean;
begin
  Result := FModel <> nil;
end;

procedure TiOSScrollBoxDelegate.scrollViewDidEndDecelerating(scrollView: UIScrollView);
begin
end;

procedure TiOSScrollBoxDelegate.scrollViewDidEndDragging(scrollView: UIScrollView; willDecelerate: Boolean);
begin
end;

procedure TiOSScrollBoxDelegate.scrollViewDidEndScrollingAnimation(scrollView: UIScrollView);
begin
end;

procedure TiOSScrollBoxDelegate.scrollViewDidEndZooming(scrollView: UIScrollView; withView: UIView; atScale: CGFloat);
begin
end;

procedure TiOSScrollBoxDelegate.scrollViewDidScroll(scrollView: UIScrollView);
var
  NewOffset: NSPoint;
begin
  NewOffset := scrollView.contentOffset;

  if Control is TCustomPresentedScrollBox then
  begin
    if Content <> nil then
      Content.Position.Point := TPointF.Create(-NewOffset.x, -NewOffset.y);
    Model.DoViewportPositionChange(TPointF.Create(FPreviousOffset.x, FPreviousOffset.y),
      TPointF.Create(NewOffset.x, NewOffset.y), False);
  end;

  FPreviousOffset := scrollView.contentOffset;
end;

procedure TiOSScrollBoxDelegate.scrollViewDidScrollToTop(scrollView: UIScrollView);
begin
end;

procedure TiOSScrollBoxDelegate.scrollViewDidZoom(scrollView: UIScrollView);
begin
end;

function TiOSScrollBoxDelegate.scrollViewShouldScrollToTop(scrollView: UIScrollView): Boolean;
begin
  Result := True;
end;

procedure TiOSScrollBoxDelegate.scrollViewWillBeginDecelerating(scrollView: UIScrollView);
begin
end;

procedure TiOSScrollBoxDelegate.scrollViewWillBeginDragging(scrollView: UIScrollView);
begin
end;

procedure TiOSScrollBoxDelegate.scrollViewWillBeginZooming(scrollView: UIScrollView; withView: UIView);
begin
end;

procedure TiOSScrollBoxDelegate.scrollViewWillEndDragging(scrollView: UIScrollView; withVelocity: CGPoint;
  targetContentOffset: Pointer);
begin
end;

function TiOSScrollBoxDelegate.viewForZoomingInScrollView(scrollView: UIScrollView): UIView;
begin
end;

initialization
  TPresentationProxyFactory.Current.Register(TPresentedScrollBox, TControlType.Platform, TiOSPresentationProxy<TiOSScrollBox>);
finalization
  TPresentationProxyFactory.Current.Unregister(TPresentedScrollBox, TControlType.Platform, TiOSPresentationProxy<TiOSScrollBox>);
end.
