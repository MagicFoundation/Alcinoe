unit ALFmxTabControl;

interface

{$SCOPEDENUMS ON}

uses System.Classes,
     System.UITypes,
     System.Types,
     System.Messaging,
     FMX.Types,
     FMX.Controls,
     FMX.Ani,
     ALFmxInertialMovement;

type

  {********************}
  TALTabControl = class;
  TALTabItem = class;
  TALTabItemClass = class of TALTabItem;

  {*******************************}
  TALTabTransition = (None, Slide);

  {*************************************************************************************************************************}
  TALTabPositionChangeEvent = procedure (Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF) of object;
  TALTabAniTransitionInit = procedure(const sender: TObject;
                                      const aVelocity: Double;
                                      var aDuration: Single;
                                      var aAnimationType: TAnimationType;
                                      var aInterpolation: TInterpolationType) of object;

  {**************************}
  TALTabItem = class(TControl)
  private
    [Weak] FTabControl: TALTabControl;
    FIsSelected: Boolean;
    fViewPortOffset: single;
    procedure SetIsSelected(const Value: Boolean);
    procedure SetSelectedInternal(const Value: Boolean);
  protected
    procedure ParentChanged; override;
    procedure DoRealign; override;
    property Align;
    property RotationAngle;
    property RotationCenter;
    property Position;
    property Margins;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    property TabControl: TALTabControl read FTabControl;
    property ViewPortOffset: single read fViewPortOffset;
  published
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled default True;
    property HitTest default false;
    property IsSelected: Boolean read FIsSelected write SetIsSelected;
    property Index stored False;
    property Padding;
    property Opacity;
    property PopupMenu;
    property Visible default True;
    property ParentShowHint;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
  end;

  {**********************************************}
  TALTabControl = class(TControl, IItemsContainer)
  public type
    TFindKind = (Next, Back, First, Last, Current);
  private
    FTabCount: integer;
    FMouseEvents: Boolean;
    fGestureEvents: Boolean;
    FchildreenChanging: boolean;
    FTabIndex: Integer;
    FRealigningTabs: Boolean;
    FOnChange: TNotifyEvent;
    FAniCalculations: TALAniCalculations;
    fLastViewportPosition: TpointF;
    FOnViewportPositionChange: TALTabPositionChangeEvent;
    FAniTransition: TFloatAnimation;
    fOnAniTransitionInit: TALTabAniTransitionInit;
    fOnAniStart: TnotifyEvent;
    fOnAniStop: TnotifyEvent;
    fMouseDownPos: single;
    FDeadZoneBeforeAcquireScrolling: Integer;
    fScrollingAcquiredByMe: boolean;
    fScrollingAcquiredByOther: boolean;
    fScrollingAcquiredByOtherMessageID: integer;
    procedure setScrollingAcquiredByMe(const Value: boolean);
    procedure ScrollingAcquiredByOtherHandler(const Sender: TObject; const M: TMessage);
    procedure SetTabIndex(const Value: Integer);
    function GetActiveTab: TALTabItem;
    procedure SetActiveTab(const Value: TALTabItem);
    procedure AniTransitionProcess(Sender: TObject);
    procedure AniTransitionFinish(Sender: TObject);
    { IItemContainer }
    function GetItemsCount: Integer;
    function GetItem(const AIndex: Integer): TFmxObject;
    { IItemContainer }
  protected
    procedure Paint; override;
    function GetTabItem(AIndex: Integer): TALTabItem;
    procedure Loaded; override;
    procedure RealignTabs; virtual;
    procedure ChangeChildren; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    procedure DoDeleteChildren; override;
    procedure DoChange; virtual;
    procedure CMGesture(var EventInfo: TGestureEventInfo); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    procedure Resize; override;
    property ClipChildren default True;
    property Padding;
    property AutoCapture;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SetActiveTabWithTransition(const ATab: TALTabItem;
                                        const ATransition: TALTabTransition;
                                        const AVelocity: double=0;
                                        const ALaunchAniStartEvent: boolean = True): boolean; overload;
    function SetActiveTabWithTransition(const ATabIndex: integer; ATransition: TALTabTransition): boolean; overload;
    function FindVisibleTab(var Index: Integer; const FindKind: TFindKind): Boolean; overload;
    function FindVisibleTab(const FindKind: TFindKind): Integer; overload;
    function Next(ATransition: TALTabTransition = TALTabTransition.Slide): Boolean;
    function Previous(ATransition: TALTabTransition = TALTabTransition.Slide): Boolean;
    function First(ATransition: TALTabTransition = TALTabTransition.Slide): Boolean;
    function Last(ATransition: TALTabTransition = TALTabTransition.Slide): Boolean;
    function Delete(const Index: Integer): Boolean;
    function Add(const TabClass: TALTabItemClass = nil): TALTabItem;
    function Insert(const Index: Integer; const TabClass: TALTabItemClass = nil): TALTabItem;
    function HasActiveTab: Boolean;
    property TabCount: Integer read fTabCount;
    property Tabs[AIndex: Integer]: TALTabItem read GetTabItem;
    property AniCalculations: TALAniCalculations read FAniCalculations;
    property AniTransition: TFloatAnimation read FAniTransition;
    property DeadZoneBeforeAcquireScrolling: Integer read FDeadZoneBeforeAcquireScrolling write FDeadZoneBeforeAcquireScrolling default 16;
  published
    property Align;
    property Anchors;
    property ActiveTab: TALTabItem read GetActiveTab write SetActiveTab stored False;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled default True;
    property Locked;
    property Height;
    property HitTest;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TabIndex: Integer read FTabIndex write SetTabIndex default -1;
    property Visible default True;
    property Width;
    property OnChange: TNotifyEvent read FOnChange write FOnChange; // Fired immediately after the selected tab changes.
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnViewportPositionChange: TALTabPositionChangeEvent read FOnViewportPositionChange write FOnViewportPositionChange;
    property OnAniTransitionInit: TALTabAniTransitionInit read fOnAniTransitionInit write fOnAniTransitionInit;
    property OnAniStart: TnotifyEvent read fOnAniStart write fOnAniStart;
    property OnAniStop: TnotifyEvent read fOnAniStop write fOnAniStop;
  end;

procedure Register;

implementation

uses System.SysUtils,
     System.Math,
     System.Math.Vectors,
     FMX.Utils,
     FMX.Consts,
     ALCommon;

{************************************************}
constructor TALTabItem.Create(AOwner: TComponent);
begin
  inherited;
  FTabControl := nil;
  FIsSelected := false;
  fViewPortOffset := 0;
  Locked := True;
  Enabled := True;
  Visible := True;
  HitTest := False;
end;

{************************************************************}
procedure TALTabItem.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  if (FTabControl = nil) or FTabControl.FRealigningTabs then inherited SetBounds(X, Y, AWidth, AHeight);
end;

{*************************************************************}
procedure TALTabItem.SetSelectedInternal(const Value: Boolean);
begin
  FIsSelected := Value;
  StartTriggerAnimation(Self, 'IsSelected');
  ApplyTriggerEffect(Self, 'IsSelected');
end;

{*******************************************************}
procedure TALTabItem.SetIsSelected(const Value: Boolean);
begin
  if not Value or (FTabControl = nil) or (FTabControl.TabIndex = Index) then
    SetSelectedInternal(Value)
  else if FTabControl <> nil then
    FTabControl.TabIndex := Index;
end;

{*********************************}
procedure TALTabItem.ParentChanged;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function FindTabControl: TALTabControl;
  var P: TFmxObject;
  begin
    P := Parent;
    while P <> nil do begin
      if P is TALTabControl then begin
        Result := P as TALTabControl;
        Exit;
      end;
      P := P.Parent;
    end;
    Result := nil;
  end;

begin
  inherited ParentChanged;
  FTabControl := FindTabControl;
end;

{*****************************}
procedure TALTabItem.DoRealign;
begin
  // The loaded will be first called by children and then after by parents,
  // this mean that loaded (and realign that go with it) will be first called by TabItem (with a default
  // size of 50x50) and then later called also at the parent level. so better to
  // avoid the realign at this step
  if (FTabControl = nil) or (not (csloading in FTabControl.ComponentState)) then
    inherited DoRealign;
end;

{**}
type
  TALTabControlAniCalculations = class(TALAniCalculations)
  private
    [Weak] FTabControl: TALTabControl;
    fRunning: Boolean;
    Procedure LaunchAniTransition;
  protected
    procedure DoStart; override;
    procedure DoStop; override;
    procedure DoChanged; override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure MouseDown(X, Y: Double); override;
    procedure MouseUp(X, Y: Double); override;
    procedure MouseLeave; override;
    property TabControl: TALTabControl read FTabControl;
  end;

{*******************************************************************}
constructor TALTabControlAniCalculations.Create(AOwner: TPersistent);
begin
  ValidateInheritance(AOwner, TALTabControl, False);
  inherited;
  FTabControl := TALTabControl(AOwner);
  fRunning := False;
end;

{***********************************************}
procedure TALTabControlAniCalculations.DoChanged;
begin
  inherited DoChanged;
  if (csDestroying in FTabControl.ComponentState) or
     (FTabControl.FAniTransition.Running) or  // if this event was called from AniTransitionProcess then exit
     (not FTabControl.HasActiveTab) then exit; // if their is no activetab then nothing to do then exit
  FTabControl.activeTab.Position.X := FTabControl.activeTab.ViewPortOffset - ViewportPosition.x;
  FTabControl.AniTransitionProcess(nil); // will realign all the tab around activeTab (and update also activetab if neccessary) and fire OnViewportPositionChange
end;

{*********************************************}
procedure TALTabControlAniCalculations.DoStart;
begin
  inherited DoStart;

  if (not fRunning) and
     (assigned(FTabControl.fOnAniStart)) and
     (not FTabControl.AniTransition.Running) and
     (not (csDestroying in FTabControl.ComponentState)) then FTabControl.fOnAniStart(FTabControl);

  if down then fRunning := true;
end;

{********************************************}
procedure TALTabControlAniCalculations.DoStop;
begin
  inherited DoStop;

  if (fRunning) and
     (not FTabControl.AniTransition.Running) and
     (assigned(FTabControl.fOnAniStop)) and
     (not (csDestroying in FTabControl.ComponentState)) then FTabControl.fOnAniStop(FTabControl);

  if (not down) then fRunning := False;
end;

{*************************************************************}
procedure TALTabControlAniCalculations.MouseDown(X, Y: Double);
begin
  inherited MouseDown(X, Y);
  if down then FTabControl.FAniTransition.StopAtCurrent;
end;

{*********************************************************}
Procedure TALTabControlAniCalculations.LaunchAniTransition;
var aVelocity: double;
    aTargetTabIndex: integer;
begin

  // exit or if their is no activetab
  if (not FTabControl.HasActiveTab) then exit;

  // init aVelocity
  aVelocity := currentVelocity.X;

  // init aTargetTabIndex
  aTargetTabIndex := fTabControl.tabindex;
  if compareValue(aVelocity, 0, Tepsilon.Position) > 0 then begin
    if compareValue(FTabControl.activeTab.Position.x, 0, Tepsilon.Position) <= 0 then
      fTabControl.FindVisibleTab(aTargetTabIndex, TALTabControl.TFindKind.next); // if not found the bounds animation will play
  end
  else if compareValue(aVelocity, 0, Tepsilon.Position) < 0 then begin
    if compareValue(FTabControl.activeTab.Position.x, 0, Tepsilon.Position) >= 0 then
      fTabControl.FindVisibleTab(aTargetTabIndex, TALTabControl.TFindKind.back); // if not found the bounds animation will play
  end;

  // Stop the animation
  fRunning := False;
  Enabled := False; // this to stop the timer
                    // because timer it's problem when we are in bound
                    // Enabled := False will work only because interval = 0
                    // ##{^#{^ anicalculations is very hard to understand :(

  // SetActiveTabWithTransition + aVelocity
  fTabControl.SetActiveTabWithTransition(fTabControl.tabs[aTargetTabIndex], TALTabTransition.Slide, aVelocity, false{ALaunchAniStartEvent});

end;

{***********************************************************}
procedure TALTabControlAniCalculations.MouseUp(X, Y: Double);
var aDown: Boolean;
begin
  aDown := Down;
  fRunning := False;
  inherited MouseUp(X, Y);
  if aDown and Moved then LaunchAniTransition;
end;

{************************************************}
procedure TALTabControlAniCalculations.MouseLeave;
var aDown: Boolean;
begin
  aDown := Down;
  fRunning := False;
  inherited MouseLeave;
  if aDown and Moved then LaunchAniTransition;
end;

{***************************************************}
constructor TALTabControl.Create(AOwner: TComponent);
begin
  inherited;
  //-----
  FAniTransition := TFloatAnimation.Create(Self);
  FAniTransition.OnProcess := AniTransitionProcess;
  FAniTransition.OnFinish := AniTransitionFinish;
  FAniTransition.PropertyName := 'Position.X';
  FAniTransition.StartFromCurrent := True;
  FAniTransition.StopValue := 0;
  fOnAniTransitionInit := nil;
  fOnAniStart := nil;
  fOnAniStop := nil;
  //-----
  FTabCount := 0;
  FMouseEvents := False;
  fGestureEvents := False;
  FOnChange := nil;
  Visible := true;
  Enabled := true;
  ClipChildren := true;
  FTabIndex := -1;
  FchildreenChanging := False;
  FRealigningTabs := False;
  AutoCapture := True;
  SetAcceptsControls(True);
  FDeadZoneBeforeAcquireScrolling := 16;
  fMouseDownPos := 0;
  fScrollingAcquiredByMe := False;
  fScrollingAcquiredByOther := False;
  fScrollingAcquiredByOtherMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TALScrollingAcquiredMessage, ScrollingAcquiredByOtherHandler);
  //-----
  FAniCalculations := TALTabControlAniCalculations.Create(Self);
  FAniCalculations.Animation := true; // Specifies whether to customize inertial scrolling by taking into account the DecelerationRate property when calculating parameters of inertial scrolling.
  FAniCalculations.AutoShowing := False;  // Enables the smooth hiding and showing of scroll bars.
  FAniCalculations.averaging := false;  // Specifies whether to average an inertial moving velocity taking into account all existing points on a trajectory.
  FAniCalculations.BoundsAnimation := true; // the TTargetType.Min and TTargetType.Max must be set. this make a bound the the animation reach the min or max of the scrooling area
  FAniCalculations.Interval := 0; // DefaultIntervalOfAni 10 ms for ~100 frames per second - Defines the interval between successive updates of the pictures of the inertial moving.
                                  // we don't need any inertial moving with FAniCalculations - we use FAniCalculations just to calculate the velocity
  FAniCalculations.TouchTracking := [ttHorizontal]; // Defines the possible directions of the inertial scrolling initiated by the touch input.
  FAniCalculations.DecelerationRate := ALDecelerationRateNormal; // DecelerationRateNormal (1.95) - Determines the deceleration rate of inertial scrolling.
  FAniCalculations.Elasticity := ALDefaultElasticity; // By default, the scroll view "bounces" back when scrolling exceeds the bounds of the content.
                                                      // Elasticity defines the velocity of this "BOUNCING" (not the velocity of the scroll, just the elasticity of the bouncing)
  FAniCalculations.StorageTime := ALDefaultStorageTime; // DefaultStorageTime (0.15) - The duration of time in which the scrolling engine stores obtained trajectory points.
                                                        // so if you make a big number here, then it's something like if you press the screen without moving for few second, and suddenly
                                                        // move fastly then the velocity will be taken with the time when you move slowly (so a slow velocity)
                                                        // on the other side if to slow then only one point (or even 0) will be keep and their will be no amims at all
  FOnViewportPositionChange := nil;
  fLastViewportPosition := TpointF.Create(0,0);
  //-----
  Touch.DefaultInteractiveGestures := Touch.DefaultInteractiveGestures + [TInteractiveGesture.Pan];
  Touch.InteractiveGestures := Touch.InteractiveGestures + [TInteractiveGesture.Pan];
  //-----
  SetBounds(0, 0, 200, 200);
end;

{*******************************}
destructor TALTabControl.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TALScrollingAcquiredMessage, fScrollingAcquiredByOtherMessageID);
  ALFreeAndNil(FAniCalculations); // >> will call disposeOF if necessary
  ALFreeAndNil(FAniTransition);   // >> will call disposeOF if necessary
  inherited;
end;

{******************************************************************************}
// this method is called on Child.ChangeOrder (like changing the index property)
// DoInsertObject and Sort but NOT on DoAddObject !
procedure TALTabControl.ChangeChildren;
var aNeedRealignTabs: Boolean;
    i, k: integer;
begin

  //this will update also the controls list (that is different from the childreen list)
  //so we need to take care because the property index in base of childreen list not
  //on controls list but this is not really matter we will add only Tcontrol descendant here
  //but inherited will also call realign if it's detect some change !
  inherited;

  //keep the TalTabItem at the beginning of the controls list and the
  //other child objects at the end of the controls list
  if not FchildreenChanging then begin
    aNeedRealignTabs := False;
    FchildreenChanging := True;
    try
      K := controlsCount - 1;
      for I := controlsCount - 1 downto 0 do begin
        if not (controls[i] is TalTabItem) then begin
          if (i <> k) then begin
            aNeedRealignTabs := True;
            controls[i].Index := K;
          end;
          dec(k);
        end;
      end;
    finally
      FchildreenChanging := False;
    end;
    if aNeedRealignTabs then RealignTabs;
  end;

end;

{*************************************************************}
procedure TALTabControl.DoAddObject(const AObject: TFmxObject);
var aIdx: integer;
begin
  //keep the TalTabItem as the beginning of the controls list and the
  //other child objects at the end of the controls list
  if AObject is TALTabItem then begin
    aIdx := TabCount;
    inherited DoAddObject(AObject);
    AObject.Index := aIdx;
    inc(FTabCount);
    RealignTabs;
  end
  else begin
    inherited DoAddObject(AObject);
    AObject.Index := controlsCount - 1;
  end;
end;

{********************************************************************************}
procedure TALTabControl.DoInsertObject(Index: Integer; const AObject: TFmxObject);
begin
  //TALTabControl.ChangeChildren will be call so we don't need to check that
  //TalTabItem is at the beginning of the control list
  inherited DoInsertObject(Index, AObject);
  if AObject is TALTabItem then inc(FTabCount);
end;

{****************************************************************}
procedure TALTabControl.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited DoRemoveObject(AObject);
  if AObject is TALTabItem then dec(FTabCount);
end;

{***************************************}
procedure TALTabControl.DoDeleteChildren;
begin
  inherited DoDeleteChildren;
  fTabCount := 0;
end;

{*****************************}
procedure TALTabControl.Resize;
begin
  RealignTabs;
  inherited Resize;
end;

{*****************************}
procedure TALTabControl.Loaded;
begin
  inherited Loaded;
  RealignTabs;
end;

{**********************************************}
function TALTabControl.GetActiveTab: TALTabItem;
begin
  if InRange(TabIndex, 0, TabCount - 1) then
    Result := Tabs[TabIndex]
  else
    Result := nil;
end;

{************************************************************}
procedure TALTabControl.SetActiveTab(const Value: TALTabItem);
begin
  if (Value <> nil) and IsChild(Value) then
    TabIndex := Value.Index
  else
    TabIndex := -1;
end;

{*******************************************}
function TALTabControl.HasActiveTab: Boolean;
begin
  Result := ActiveTab <> nil;
end;

{********************************************************}
procedure TALTabControl.SetTabIndex(const Value: Integer);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure DeselectActiveTab;
  begin
    if HasActiveTab then
      ActiveTab.SetSelectedInternal(False);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure SelectActiveTab;
  begin
    if HasActiveTab then
      ActiveTab.SetSelectedInternal(True);
  end;

begin
  if FTabIndex <> Value then begin
    DeselectActiveTab;
    FTabIndex := Value;
    SelectActiveTab;
    RealignTabs;
    if [csLoading, csDestroying] * ComponentState = [] then DoChange;
  end;
end;

{*************************************************************}
function TALTabControl.GetTabItem(AIndex: Integer): TALTabItem;
begin
  if InRange(AIndex, 0, TabCount - 1) then
    Result := Controls[AIndex] as TALTabItem
  else
    Result := nil;
end;

{****************************************************************}
function TALTabControl.GetItem(const AIndex: Integer): TFmxObject;
begin
  Result := Tabs[AIndex];
end;

{********************************************}
function TALTabControl.GetItemsCount: Integer;
begin
  Result := TabCount;
end;

{****************************}
procedure TALTabControl.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{*******************************}
procedure TALTabControl.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{*********************************************************************}
procedure TALTabControl.setScrollingAcquiredByMe(const Value: boolean);
begin
  if Value <> fScrollingAcquiredByMe  then begin
    fScrollingAcquiredByMe := Value;
    TMessageManager.DefaultManager.SendMessage(self, TALScrollingAcquiredMessage.Create(Value), True);
  end;
end;

{************************************************************************************************}
procedure TALTabControl.ScrollingAcquiredByOtherHandler(const Sender: TObject; const M: TMessage);
begin
  //the scrolling was acquired or released by another control (like a scrollbox for exemple)
  //the problem is that the scrolling could be acquired BEFORE the mousedown is fired in parent control (baah yes)
  //so we need the var fScrollingAcquiredByOther to handle this
  if (Sender = self) then exit;
  if TALScrollingAcquiredMessage(M).Acquired then begin
    if fAniCalculations.down then begin
      fAniCalculations.MouseLeave; // instead of down := false to reposition the tabitem
      FMouseEvents := False;
      fGestureEvents := False;
      if not FAniTransition.Running then RealignTabs; // << if i try with fAniCalculations.launchanimation i have a flickr because the anim stop and restart at a different speed
    end;
    fScrollingAcquiredByOther := True;
  end
  else fScrollingAcquiredByOther := False;
end;

{******************************************************************}
procedure TALTabControl.CMGesture(var EventInfo: TGestureEventInfo);
var LP: TPointF;
begin

  //note: Mouse events (with autocapture) are more accurate than CMGesture
  //      to detect when mouse go out of the screen. with CMGesture their
  //      will be (often) no final event with TInteractiveGestureFlag.gfEnd
  //      https://quality.embarcadero.com/browse/RSP-15617

  //This is used when scrolling with the finger on top of a control (like a TButton).
  if (not FMouseEvents) and (EventInfo.GestureID = igiPan) then begin
    LP := AbsoluteToLocal(EventInfo.Location);
    if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then begin
      if not fScrollingAcquiredByOther then begin
        setScrollingAcquiredByMe(False);
        fMouseDownPos := LP.X;
        fGestureEvents := true;
        FAniCalculations.averaging := true;
        AniCalculations.MouseDown(LP.X, LP.Y);
      end;
    end
    else if fGestureEvents and
            (EventInfo.Flags = []) then begin
      if (not fScrollingAcquiredByMe) and
         (abs(fMouseDownPos - LP.X) > fDeadZoneBeforeAcquireScrolling) then setScrollingAcquiredByMe(True);
      AniCalculations.MouseMove(LP.X, LP.Y)
    end
    else if fGestureEvents and
            (TInteractiveGestureFlag.gfEnd in EventInfo.Flags) then begin
      setScrollingAcquiredByMe(False);
      AniCalculations.MouseUp(LP.X, LP.Y);
      fGestureEvents := False;
    end;
  end;

  //important to let parent (like TScrollBox) continue
  //to handle this gesture event
  inherited;

end;

{****************************************************************************************}
procedure TALTabControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FMouseEvents := not fGestureEvents;
  inherited;
  if (not fScrollingAcquiredByOther) and FMouseEvents and (Button = TMouseButton.mbLeft) then begin
    setScrollingAcquiredByMe(False);
    fMouseDownPos := x;
    FAniCalculations.averaging := ssTouch in Shift;
    AniCalculations.MouseDown(X, Y);
  end;
end;

{******************************************************************}
procedure TALTabControl.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FMouseEvents then begin
    if (not fScrollingAcquiredByMe) and
       (abs(fMouseDownPos - x) > fDeadZoneBeforeAcquireScrolling) then setScrollingAcquiredByMe(True);
    AniCalculations.MouseMove(X, Y);
  end;
end;

{**************************************************************************************}
procedure TALTabControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FMouseEvents and (Button = TMouseButton.mbLeft) then begin
    setScrollingAcquiredByMe(False);
    AniCalculations.MouseUp(X, Y);
    FMouseEvents := False;
  end;
end;

{***********************************}
procedure TALTabControl.DoMouseLeave;
begin
  inherited;
  if FMouseEvents then begin
    setScrollingAcquiredByMe(False);
    AniCalculations.MouseLeave;
    FMouseEvents := False;
  end;
end;

{**********************************}
procedure TALTabControl.RealignTabs;
var aBoundsRect: TrectF;
    aTmpRect: TrectF;
    aTargets: array of TALAniCalculations.TTarget;
    i, k: integer;
begin

  //we do not need to realign tab during transition because transition
  //process is already aligning all tabs
  if FRealigningTabs or (csloading in ComponentState) then exit;

  //this to permit Tabitem to resize (else tabitem are forbiden to resize)
  FRealigningTabs := True;
  try

    //update the boundsRect of all the visible tab
    aBoundsRect := LocalRect;
    if HasActiveTab then begin
      //-----
      aTmpRect := aBoundsRect;
      ActiveTab.SetBoundsRect(aTmpRect);
      //-----
      k := 0;
      i := tabindex;
      while FindVisibleTab(i, TFindKind.Back) do begin
        dec(k);
        aTmpRect := aBoundsRect;
        aTmpRect.Offset(k * width,0);
        tabs[i].SetBoundsRect(aTmpRect);
      end;
      //-----
      k := 0;
      i := tabindex;
      while FindVisibleTab(i, TFindKind.Next) do begin
        inc(k);
        aTmpRect := aBoundsRect;
        aTmpRect.Offset(k * width,0);
        tabs[i].SetBoundsRect(aTmpRect);
      end;
    end
    else begin
      k := 0;
      i := -1;
      while FindVisibleTab(i, TFindKind.Next) do begin
        inc(k);
        aTmpRect := aBoundsRect;
        aTmpRect.Offset(k * width,0);
        tabs[i].SetBoundsRect(aTmpRect);
      end;
    end;

    //update the ViewPortOffset of all the visible tabs
    k := 0;
    i := -1;
    while FindVisibleTab(i, TFindKind.Next) do begin
      tabs[i].fViewPortOffset := k * width;
      inc(k);
    end;
    dec(k);

    //update the FAniCalculations target
    SetLength(aTargets, 2);
    aTargets[0].TargetType := TALAniCalculations.TTargetType.Min;
    aTargets[0].Point := TALPointD.Create(0, 0);
    aTargets[1].TargetType := TALAniCalculations.TTargetType.Max;
    aTargets[1].Point := TALPointD.Create(max(0,k) * width, 0);
    FAniCalculations.SetTargets(aTargets);
    if HasActiveTab then FAniCalculations.ViewportPosition := TAlPointD.Create(ActiveTab.ViewPortOffset, 0)
    else FAniCalculations.ViewportPosition := TAlPointD.Create(0,0);

  finally
    FRealigningTabs := False;
  end;

end;

{************************************************************}
procedure TALTabControl.AniTransitionProcess(Sender: TObject);
var aTargetTabItem: TALTabItem;
    aNewViewportPosition: TPointF;
    i, k: integer;
begin

  //security check
  if not HasActiveTab then exit;

  //init aTargetTabItem
  if (Sender <> nil) then aTargetTabItem := TALTabItem((Sender as TFloatAnimation).TagObject)
  else aTargetTabItem := activeTab;

  //offset all the items before aTargetTabItem
  k := aTargetTabItem.Index;
  for I := aTargetTabItem.Index - 1 downto 0 do begin
    if tabs[i].Visible then begin
      tabs[i].Position.X := tabs[k].Position.X - tabs[i].Width;
      k := i;
    end;
  end;

  //offset all the items after aTargetTabItem
  k := aTargetTabItem.Index;
  for I := aTargetTabItem.Index + 1 to tabcount - 1 do begin
    if tabs[i].Visible then begin
      tabs[i].Position.X := tabs[k].Position.X + tabs[k].Width;
      k := i;
    end;
  end;

  //update the activeTab
  if compareValue(abs(activeTab.Position.x), width / 2, Tepsilon.Position) > 0  then begin
    FRealigningTabs := True; //to deactivate RealignTabs during SetTabIndex
    try
      if activeTab.Position.x > 0 then begin
        i := tabindex;
        if FindVisibleTab(i, TFindKind.back) then TabIndex := i;
      end
      else if activeTab.Position.x < 0 then begin
        i := tabindex;
        if FindVisibleTab(i, TFindKind.next) then TabIndex := i;
      end;
    finally
      FRealigningTabs := False;
    end;
  end;

  //update FAniCalculations.ViewportPosition if the AniTransitionProcess
  //was called from the fAniTransition
  if (Sender <> nil) then
    FAniCalculations.ViewportPosition := TAlPointD.Create(activeTab.ViewPortOffset - activeTab.Position.Point.X, 0);

  //fire the OnViewportPositionChange
  aNewViewportPosition := TpointF.Create(FAniCalculations.ViewportPosition.X, FAniCalculations.ViewportPosition.Y);
  if (assigned(FOnViewportPositionChange)) and
     (not fLastViewportPosition.EqualsTo(aNewViewportPosition, TEpsilon.Position)) then
    FOnViewportPositionChange(self, fLastViewportPosition, aNewViewportPosition);
  fLastViewportPosition := aNewViewportPosition;

end;

{***********************************************************}
procedure TALTabControl.AniTransitionFinish(Sender: TObject);
begin
  if (assigned(fOnAniStop)) and
     (not fAniCalculations.down) then
    fOnAniStop(Self);
end;

{***********************************************************************}
function TALTabControl.SetActiveTabWithTransition(const ATab: TALTabItem;
                                                  const ATransition: TALTabTransition;
                                                  const AVelocity: double=0;
                                                  const ALaunchAniStartEvent: boolean = True): boolean;
var aDuration: Single;
    aAnimationType: TAnimationType;
    aInterpolation: TInterpolationType;
begin

  FAniTransition.StopAtCurrent;

  if (ATab = nil) then exit(false);
  result := true;

  if (ATransition = TALTabTransition.Slide) and
     (HasActiveTab) then begin

    FAniTransition.Parent := ATab;
    FAniTransition.TagObject := ATab;
    aDuration := 0.3;
    aAnimationType := TAnimationType.In;
    aInterpolation := TInterpolationType.Linear;
    if assigned(fOnAniTransitionInit) then
      fOnAniTransitionInit(self,
                           AVelocity,
                           aDuration,
                           aAnimationType,
                           aInterpolation);
    FAniTransition.Duration := aDuration;
    FAniTransition.AnimationType := aAnimationType;
    FAniTransition.Interpolation := aInterpolation;
    if (ALaunchAniStartEvent) and
       (assigned(fOnAniStart)) then fOnAniStart(self);
    FAniTransition.start;

  end
  else ActiveTab := ATab;

end;

{******************************************************************************************************************}
function TALTabControl.SetActiveTabWithTransition(const ATabIndex: integer; ATransition: TALTabTransition): boolean;
begin
  result := SetActiveTabWithTransition(tabs[ATabIndex], ATransition);
end;

{********************************************************************************************}
function TALTabControl.FindVisibleTab(var Index: Integer; const FindKind: TFindKind): Boolean;
var I: Integer;
begin
  I := Index;
  if (I < 0) or (FindKind = TFindKind.First) then
    I := -1
  else if (I > TabCount) or (FindKind = TFindKind.Last) then
    I := TabCount;

  if FindKind = TFindKind.Current then
  begin
    while (I < TabCount) and not Tabs[I].Visible do
      Inc(I);
    if I >= TabCount then
    begin
      I := TabCount - 1;
      while (I >= 0) and not Tabs[I].Visible do
        Dec(I);
    end;
  end
  else if FindKind in [TFindKind.Next, TFindKind.First] then
    repeat
      Inc(I);
    until (I >= TabCount) or Tabs[I].Visible
  else
    repeat
      Dec(I);
    until (I < 0) or Tabs[I].Visible;

  Result := (I >= 0) and (I < TabCount);
  if Result then
    Index := I;
end;

{************************************************************************}
function TALTabControl.FindVisibleTab(const FindKind: TFindKind): Integer;
var I: Integer;
begin
  I := TabIndex;
  if FindVisibleTab(I, FindKind) then Result := I
  else Result := -1;
end;

{******************************************************************}
function TALTabControl.Next(ATransition: TALTabTransition): Boolean;
begin
  Result := SetActiveTabWithTransition(FindVisibleTab(TFindKind.Next), ATransition);
end;

{**********************************************************************}
function TALTabControl.Previous(ATransition: TALTabTransition): Boolean;
begin
  Result := SetActiveTabWithTransition(FindVisibleTab(TFindKind.Back), ATransition);
end;

{*******************************************************************}
function TALTabControl.First(ATransition: TALTabTransition): Boolean;
begin
  Result := SetActiveTabWithTransition(FindVisibleTab(TFindKind.First), ATransition);
end;

{******************************************************************}
function TALTabControl.Last(ATransition: TALTabTransition): Boolean;
begin
  Result := SetActiveTabWithTransition(FindVisibleTab(TFindKind.Last), ATransition);
end;

{***********************************************************}
function TALTabControl.Delete(const Index: Integer): Boolean;
var LTabIndex: Integer;
    Obj: TFmxObject;
begin
  Result := False;
  if (Index >= 0) and (Index < TabCount) then begin
    if TabIndex > Index then LTabIndex := TabIndex - 1
    else if TabIndex = Index then LTabIndex := Index
    else LTabIndex := -1;
    Obj := (Self as IItemsContainer).GetItem(Index);
    ALFreeAndNil(Obj); // >> will call disposeOF if necessary
    if (LTabIndex >= 0) and FindVisibleTab(LTabIndex, TFindKind.Current) then
      TabIndex := LTabIndex;
  end;
end;

{**********************************************************************}
function TALTabControl.Add(const TabClass: TALTabItemClass): TALTabItem;
var LTabClass: TALTabItemClass;
begin
  if TabClass = nil then LTabClass := TALTabItem
  else LTabClass := TabClass;
  Result := LTabClass.Create(Self);
  try
    Result.Parent := Self;
  except
    ALFreeAndNil(Result);
    Raise;
  end;
end;

{*****************************************************************************************************}
function TALTabControl.Insert(const Index: Integer; const TabClass: TALTabItemClass = nil): TALTabItem;
var LTabIndex: Integer;
begin
  Result := nil;
  if (Index >= 0) and (Index <= TabCount) then begin
    if TabIndex >= Index then LTabIndex := TabIndex + 1
    else LTabIndex := -1;
    Result := Add(TabClass);
    if Index < TabCount - 1 then begin
      Result.Index := Index;
      if (LTabIndex >= 0) and FindVisibleTab(LTabIndex, TFindKind.Current) then
        TabIndex := LTabIndex;
    end;
  end;
end;


procedure Register;
begin
  RegisterComponents('Alcinoe', [TALTabControl]);
end;

initialization
  RegisterFmxClasses([TALTabControl, TALTabItem]);

end.
