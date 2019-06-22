unit ALIosScrollBox;

{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if FMX.ScrollBox.iOS.pas was not updated and adjust the IFDEF'}
{$ENDIF}

interface

{$SCOPEDENUMS ON}

uses System.TypInfo,
     Macapi.ObjectiveC,
     iOSapi.Foundation,
     iOSapi.UIKit,
     iOSapi.CocoaTypes,
     iOSApi.CoreGraphics,
     ALIosNativeView;

type

  {******************************}
  TALIosScrollBoxDelegate = class;

  {***************************************}
  IALUIScrollView = interface(UIScrollView)
    ['{3748E94E-432B-4022-8134-0A5C0AC3E088}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure HandlePan(gestureRecognizer: UIPanGestureRecognizer); cdecl;
    function gestureRecognizerShouldBegin(gestureRecognizer: UIGestureRecognizer): Boolean; cdecl;
  end;

  {***************************************}
  TALIosScrollBox = class(TALIosNativeView)
  private
    FDelegate: TALIosScrollBoxDelegate;
    FPanRecognizer: UIPanGestureRecognizer;
    FPanGestureRecognized: Boolean;
    function GetView: UIScrollView;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
    function CreateDelegate: TALIosScrollBoxDelegate; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure HandlePan(gestureRecognizer: UIPanGestureRecognizer); cdecl;
    function gestureRecognizerShouldBegin(gestureRecognizer: UIGestureRecognizer): Boolean; cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure ScrollBy(const x, y: Single; const aAnimated: boolean);
    property Delegate: TALIosScrollBoxDelegate read FDelegate;
    property View: UIScrollView read GetView;
    property PanRecognizer: UIPanGestureRecognizer read FPanRecognizer;
  end;

  {*************************************************************}
  TALIosScrollBoxDelegate = class(TOCLocal, UIScrollViewDelegate)
  private
  public
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
  end;

implementation

uses System.Classes,
     System.UITypes,
     System.SysUtils,
     Macapi.ObjCRuntime,
     FMX.Platform.iOS,
     FMX.Types,
     ALCommon;

{*********************************}
constructor TALIosScrollBox.Create;
begin
  inherited;
  FDelegate := CreateDelegate;
  View.setDelegate(FDelegate.GetObjectID);
  FPanGestureRecognized := False;
  FPanRecognizer := TUIPanGestureRecognizer.Alloc;
  FPanRecognizer.initWithTarget(GetObjectID, sel_getUid('HandlePan:'));
  FPanRecognizer.setDelaysTouchesBegan(False);
  FPanRecognizer.setCancelsTouchesInView(True);
  View.addGestureRecognizer(FPanRecognizer);
  View.setClipsToBounds(True); // << subviews are confined to the bounds of the view.
end;

{*********************************}
destructor TALIosScrollBox.Destroy;
begin
  FPanRecognizer.release;
  View.setDelegate(nil);
  AlFreeAndNil(FDelegate);
  inherited;
end;

{***************************************************************}
function TALIosScrollBox.CreateDelegate: TALIosScrollBoxDelegate;
begin
  Result := TALIosScrollBoxDelegate.Create;
end;

{*****************************************************************************}
procedure TALIosScrollBox.touchesCancelled(touches: NSSet; withEvent: UIEvent);
begin
  // A recognized gesture cancels Touches and sends TouchesCancelled message. In this case we don't have to
  // send MouseUp event to our scene.
  if not FPanGestureRecognized then
    inherited;
end;

{*******************************************************************************}
procedure TALIosScrollBox.ScrollBy(const x, y: Single; const aAnimated: boolean);
var aContentOffset: NSPoint;
begin
  aContentOffset := View.contentOffset;
  aContentOffset.x := aContentOffset.x + X;
  aContentOffset.y := aContentOffset.y + Y;
  View.setContentOffset(aContentOffset, aAnimated);
end;

{*****************************************************************************************************}
function TALIosScrollBox.gestureRecognizerShouldBegin(gestureRecognizer: UIGestureRecognizer): Boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function IsOurPanGesture: Boolean;
  begin
    Result := (gestureRecognizer as ILocalObject).GetObjectID = (FPanRecognizer as ILocalObject).GetObjectID;
  end;

var Captured: IControl;
    CapturedControl: IGestureControl;

begin

  if (Form <> nil) and IsOurPanGesture then begin
    Captured := Form.Hovered;
    if Supports(Captured, IGestureControl, CapturedControl) then Result := CapturedControl.GetFirstControlWithGesture(TInteractiveGesture.Pan) <> nil
    else Result := False;
  end
  else Result := View.gestureRecognizerShouldBegin(gestureRecognizer);

end;

{*****************************************************}
function TALIosScrollBox.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IALUIScrollView);
end;

{*********************************************}
function TALIosScrollBox.GetView: UIScrollView;
begin
  Result := inherited GetView<UIScrollView>;
end;

{*****************************************************************************}
procedure TALIosScrollBox.HandlePan(gestureRecognizer: UIPanGestureRecognizer);
var TouchPoint: NSPoint;
begin

  if Form <> nil then begin

    TouchPoint := gestureRecognizer.locationInView(WindowHandleToPlatform(Form.Handle).View);
    case gestureRecognizer.state of

      UIGestureRecognizerStateBegan: FPanGestureRecognized := True;

      UIGestureRecognizerStateChanged: Form.MouseMove([ssLeft, ssTouch], TouchPoint.x, TouchPoint.y);

      else begin
        FPanGestureRecognized := False;
        Form.MouseUp(TMouseButton.mbLeft, [ssLeft, ssTouch], TouchPoint.x, TouchPoint.y);
        Form.MouseLeave;
      end;

    end;
  end;

end;

{***************************************************************************************}
procedure TALIosScrollBoxDelegate.scrollViewDidEndDecelerating(scrollView: UIScrollView);
begin
end;

{************************************************************************************************************}
procedure TALIosScrollBoxDelegate.scrollViewDidEndDragging(scrollView: UIScrollView; willDecelerate: Boolean);
begin
end;

{*********************************************************************************************}
procedure TALIosScrollBoxDelegate.scrollViewDidEndScrollingAnimation(scrollView: UIScrollView);
begin
end;

{**********************************************************************************************************************}
procedure TALIosScrollBoxDelegate.scrollViewDidEndZooming(scrollView: UIScrollView; withView: UIView; atScale: CGFloat);
begin
end;

{******************************************************************************}
procedure TALIosScrollBoxDelegate.scrollViewDidScroll(scrollView: UIScrollView);
begin
end;

{***********************************************************************************}
procedure TALIosScrollBoxDelegate.scrollViewDidScrollToTop(scrollView: UIScrollView);
begin
end;

{****************************************************************************}
procedure TALIosScrollBoxDelegate.scrollViewDidZoom(scrollView: UIScrollView);
begin
end;

{**********************************************************************************************}
function TALIosScrollBoxDelegate.scrollViewShouldScrollToTop(scrollView: UIScrollView): Boolean;
begin
  Result := True;
end;

{******************************************************************************************}
procedure TALIosScrollBoxDelegate.scrollViewWillBeginDecelerating(scrollView: UIScrollView);
begin
end;

{**************************************************************************************}
procedure TALIosScrollBoxDelegate.scrollViewWillBeginDragging(scrollView: UIScrollView);
begin
end;

{*******************************************************************************************************}
procedure TALIosScrollBoxDelegate.scrollViewWillBeginZooming(scrollView: UIScrollView; withView: UIView);
begin
end;

{*****************************************************************************************************************************************}
procedure TALIosScrollBoxDelegate.scrollViewWillEndDragging(scrollView: UIScrollView; withVelocity: CGPoint; targetContentOffset: Pointer);
begin
end;

{********************************************************************************************}
function TALIosScrollBoxDelegate.viewForZoomingInScrollView(scrollView: UIScrollView): UIView;
begin
end;

end.
