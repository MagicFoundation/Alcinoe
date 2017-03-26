unit ALFmxLayouts;

{$IF CompilerVersion > 32} // tokyo
  {$MESSAGE WARN 'Check if FMX.Layouts.pas was not updated and adjust the IFDEF'}
{$ENDIF}

interface

{$SCOPEDENUMS ON}

uses System.Classes,
     System.Types,
     System.UITypes,
     System.Messaging,
     FMX.layouts,
     FMX.Types,
     FMX.Controls,
     ALFmxStdCtrls,
     ALFmxInertialMovement;

type

  {************************}
  TALLayout = class(TLayout)
  protected
    procedure DoRealign; override;
  end;

  {*************************}
  TALCustomScrollBox = class;

  {***********************************************************}
  //as i understand Tcontent is specially usefull to get warned
  //when a child control do
  //  * DoMatrixChanged (ie for exemple change it's position) => ParentContent.Changed;
  //  * SetAlign (ie for exemple Bottom or top) => ParentContent.Changed;
  //  * SetVisible => ParentContent.Changed;
  //  * InternalSizeChanged => FParentControl.Realign
  //  All of this are especially important to launch the realign of the scrollbox
  TALScrollBoxContent = class(TContent)
  private
    [weak] FScrollBox: TALCustomScrollBox;
  protected
    procedure ContentChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    property ScrollBox: TALCustomScrollBox read FScrollBox;
  end;

  {******************************************************}
  TALScrollBoxAniCalculations = class (TALAniCalculations)
  private
    [Weak] FScrollBox: TALCustomScrollBox;
    fLastViewportPosition: TpointF;
    fScreenScale: single;
  protected
    procedure DoChanged; override;
    procedure DoStart; override;
    procedure DoStop; override;
  public
    constructor Create(AOwner: TPersistent); override;
    property ScrollBox: TALCustomScrollBox read FScrollBox;
  end;

  {***********************************}
  TALScrollBoxBar = class(TALScrollBar)
  private
    [weak] FScrollBox: TALCustomScrollBox;
  protected
    procedure DoChanged; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Locked stored false;
    property Min stored false;
    property Max stored false;
    property Orientation stored false;
    property Position stored false;
    property Value stored false;
    property ViewportSize stored false;
    property Opacity stored false;
    property Visible stored false;
    property Enabled stored false;
    property HitTest default false;
  end;

  {*******************************************************************************************************************************}
  TALScrollBoxPositionChangeEvent = procedure (Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF) of object;
  TALScrollBoxBarInit = procedure(const sender: TObject; const aScrollBar: TALScrollBoxBar) of object;

  {**********************************}
  TALCustomScrollBox = class(TControl)
  private
    FScreenScale: single;
    FAniCalculations: TALScrollBoxAniCalculations;
    [Weak] FContent: TALScrollBoxContent;
    [Weak] FHScrollBar: TALScrollBoxBar;
    [Weak] FVScrollBar: TALScrollBoxBar;
    FDisableMouseWheel: Boolean;
    fdisableScrollChange: Boolean;
    FHasTouchScreen: Boolean;
    FShowScrollBars: Boolean;
    FAutoHide: Boolean;
    FMouseEvents: Boolean;
    fGestureEvents: Boolean;
    FOnViewportPositionChange: TALScrollBoxPositionChangeEvent;
    fOnScrollBarInit: TALScrollBoxBarInit;
    fOnAniStart: TnotifyEvent;
    fOnAniStop: TnotifyEvent;
    fMouseDownPos: TpointF;
    FDeadZoneBeforeAcquireScrolling: Integer;
    fScrollingAcquiredByMe: boolean;
    fScrollingAcquiredByOther: boolean;
    fScrollingAcquiredByOtherMessageID: integer;
    fMaxContentWidth: Single;
    fMaxContentHeight: single;
    fAnchoredContentOffset: TPointF;
    procedure setScrollingAcquiredByMe(const Value: boolean);
    procedure ScrollingAcquiredByOtherHandler(const Sender: TObject; const M: TMessage);
    procedure SetShowScrollBars(const Value: Boolean);
    procedure SetAutoHide(const Value: Boolean);
    procedure setAniCalculations(const Value: TALScrollBoxAniCalculations);
    function isMaxContentHeightStored: Boolean;
    function isMaxContentWidthStored: Boolean;
  protected
    procedure Loaded; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRealign; override;
    procedure CMGesture(var EventInfo: TGestureEventInfo); override;
    function CreateScrollBar(const aOrientation: TOrientation): TALScrollBoxBar; virtual;
    function CreateContent: TALScrollBoxContent; virtual;
    function CreateAniCalculations: TALScrollBoxAniCalculations; virtual;
    function CalcContentBounds: TRectF; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    property HScrollBar: TALScrollBoxBar read fHScrollBar;
    property VScrollBar: TALScrollBoxBar read fVScrollBar;
    property MaxContentWidth: Single read fMaxContentWidth write fMaxContentWidth stored isMaxContentWidthStored;
    property MaxContentHeight: Single read fMaxContentHeight write fMaxContentHeight stored isMaxContentHeightStored;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AniCalculations: TALScrollBoxAniCalculations read FAniCalculations write setAniCalculations;
    procedure Sort(Compare: TFmxObjectSortCompare); override;
    procedure ScrollBy(const Dx, Dy: Single);
    function GetTabList: ITabList; override;
    property Content: TALScrollBoxContent read FContent;
    property HasTouchScreen: Boolean read FHasTouchScreen;
    property AutoHide: Boolean read FAutoHide write SetAutoHide default True;
    property DisableMouseWheel: Boolean read FDisableMouseWheel write FDisableMouseWheel default False;
    property ShowScrollBars: Boolean read FShowScrollBars write SetShowScrollBars default True;
    property OnViewportPositionChange: TALScrollBoxPositionChangeEvent read FOnViewportPositionChange write FOnViewportPositionChange;
    property DeadZoneBeforeAcquireScrolling: Integer read FDeadZoneBeforeAcquireScrolling write FDeadZoneBeforeAcquireScrolling default 32;
    property OnScrollBarInit: TALScrollBoxBarInit read fOnScrollBarInit write fOnScrollBarInit;
    property ClipChildren default true;
    property OnAniStart: TnotifyEvent read fOnAniStart write fOnAniStart;
    property OnAniStop: TnotifyEvent read fOnAniStop write fOnAniStop;
  end;

  {**************************************}
  TALScrollBox = class(TALCustomScrollBox)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HScrollBar;
    property VScrollBar;
    property AniCalculations;
    property Align;
    property Anchors;
    property AutoHide;
    property ClipParent;
    property Cursor;
    property DisableMouseWheel;
    property DragMode;
    property Enabled;
    property EnableDragHighlight;
    property Height;
    property HitTest;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property ShowScrollBars;
    property Size;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    { Events }
    property OnPainting;
    property OnPaint;
    property OnResize;
    {$IF CompilerVersion > 32} // tokyo
    property OnResized;
    {$ENDIF}
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    { ScrollBox events }
    property OnViewportPositionChange;
    property OnScrollBarInit;
    property OnAniStart;
    property OnAniStop;
  end;

  {******************************************}
  TALVertScrollBox = class(TALCustomScrollBox)
  protected
    function CalcContentBounds: TRectF; override;
    procedure Paint; override;
    function CreateAniCalculations: TALScrollBoxAniCalculations; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MaxContentWidth;
    property VScrollBar;
    property AniCalculations;
    property Align;
    property Anchors;
    property AutoHide;
    property ClipParent;
    property Cursor;
    property DisableMouseWheel;
    property DragMode;
    property Enabled;
    property EnableDragHighlight;
    property Height;
    property HitTest;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property ShowScrollBars;
    property Size;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    { Events }
    property OnPainting;
    property OnPaint;
    property OnResize;
    {$IF CompilerVersion > 32} // tokyo
    property OnResized;
    {$ENDIF}
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    { ScrollBox events }
    property OnViewportPositionChange;
    property OnScrollBarInit;
    property OnAniStart;
    property OnAniStop;
  end;

  {******************************************}
  TALHorzScrollBox = class(TALCustomScrollBox)
  protected
    function CalcContentBounds: TRectF; override;
    procedure Paint; override;
    function CreateAniCalculations: TALScrollBoxAniCalculations; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MaxContentHeight;
    property HScrollBar;
    property AniCalculations;
    property Align;
    property Anchors;
    property AutoHide;
    property ClipParent;
    property Cursor;
    property DisableMouseWheel;
    property DragMode;
    property Enabled;
    property EnableDragHighlight;
    property Height;
    property HitTest;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property ShowScrollBars;
    property Size;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    { Events }
    property OnPainting;
    property OnPaint;
    property OnResize;
    {$IF CompilerVersion > 32} // tokyo
    property OnResized;
    {$ENDIF}
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    { ScrollBox events }
    property OnViewportPositionChange;
    property OnScrollBarInit;
    property OnAniStart;
    property OnAniStop;
  end;

procedure Register;

implementation

uses System.SysUtils,
     System.Math,
     System.Math.Vectors,
     {$IFDEF ALDPK}
     DesignIntf,
     {$ENDIF}
     FMX.Platform,
     FMX.Consts,
     FMX.Effects,
     FMX.utils,
     FMX.Ani,
     AlFmxCommon,
     ALCommon;

{*******************************************************************************************************}
// http://stackoverflow.com/questions/39317984/does-the-delphi-firemonkey-dorealign-implemented-correctly
// https://quality.embarcadero.com/browse/RSP-15768
// often we assign some event to some control onresize (like TText with autosize=True) to
// resize their parentcontrols to the same size as them. But in this way the problem is that if 
// we resize the parentcontrol during it's dorealign process then it will not call again dorealign
procedure TALLayout.DoRealign;
var aOriginalSize: TpointF;
begin
  aOriginalSize := Size.Size;
  inherited;
  if not aOriginalSize.EqualsTo(Size.Size) then DoRealign;
end;

{*********************************************************}
constructor TALScrollBoxContent.Create(AOwner: TComponent);
begin
  ValidateInheritance(AOwner, TALCustomScrollBox, False{CanBeNil});
  inherited Create(AOwner);
  FScrollBox := TALCustomScrollBox(AOwner);
  SetAcceptsControls(False);
end;

{*******************************************}
procedure TALScrollBoxContent.ContentChanged;
begin
  inherited;
  if (not IsUpdating) then FScrollBox.Realign; // << if we are in csloading this will actually like a no-ops
end;

{******************************************************************}
constructor TALScrollBoxAniCalculations.Create(AOwner: TPersistent);
begin
  ValidateInheritance(AOwner, TALCustomScrollBox, False{CanBeNil});
  inherited Create(AOwner);
  FScrollBox := TALCustomScrollBox(AOwner);
  fLastViewportPosition := TpointF.Create(0,0);
  FScreenScale := FScrollBox.FScreenScale;
end;

{**********************************************}
procedure TALScrollBoxAniCalculations.DoChanged;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _getPixelAlignedViewportPosition: TPointF;
  var X, Y: Single;
  begin
    X := Round(ViewportPosition.X * FScreenScale) / FScreenScale;
    Y := Round(ViewportPosition.Y * FScreenScale) / FScreenScale;
    Result := TPointF.Create(X - FScrollBox.fAnchoredContentOffset.X,  // FScrollBox.fAnchoredContentOffset.X is already pixel aligned and if present then x = 0 so return -FScrollBox.fAnchoredContentOffset.X
                             Y - FScrollBox.fAnchoredContentOffset.Y); // FScrollBox.fAnchoredContentOffset.Y is already pixel aligned and if present then y = 0 so return -FScrollBox.fAnchoredContentOffset.Y
  end;

var aNewViewportPosition: TPointF;

begin
  if (not (csDestroying in FScrollBox.ComponentState)) then begin

    //update FScrollBox.Content.Position
    if FScrollBox.Content <> nil then
      FScrollBox.Content.Position.Point := -_getPixelAlignedViewportPosition;

    //update the Shown
    if (not Down) and LowVelocity then Shown := False
    else Shown := true;

    //update the opacity of the scrollBar
    if FScrollBox.VScrollBar <> nil then FScrollBox.VScrollBar.Opacity := Opacity;
    if FScrollBox.HScrollBar <> nil then FScrollBox.HScrollBar.Opacity := Opacity;

    //update the VScrollBar/HScrollBar
    if not FScrollBox.fdisableScrollChange then begin
      FScrollBox.fdisableScrollChange := True;
      try
        if FScrollBox.VScrollBar <> nil then FScrollBox.VScrollBar.Value := ViewportPosition.Y;
        if FScrollBox.HScrollBar <> nil then FScrollBox.HScrollBar.Value := ViewportPosition.X;
      finally
        FScrollBox.fdisableScrollChange := False;
      end;
    end;

    //fire the OnViewportPositionChange
    aNewViewportPosition := TpointF.Create(ViewportPosition.X, ViewportPosition.Y);
    if (assigned(FScrollBox.FOnViewportPositionChange)) and
       (not fLastViewportPosition.EqualsTo(aNewViewportPosition, TEpsilon.Position)) then
      FScrollBox.FOnViewportPositionChange(self, fLastViewportPosition, aNewViewportPosition);
    fLastViewportPosition := aNewViewportPosition;

  end;
  inherited DoChanged;
end;

{********************************************}
procedure TALScrollBoxAniCalculations.DoStart;
begin
  inherited DoStart;

  if (FScrollBox.Scene <> nil) and
     (not (csDestroying in FScrollBox.ComponentState)) then
    FScrollBox.Scene.ChangeScrollingState(FScrollBox, True);

  if (assigned(fscrollBox.fOnAniStart)) and
     (not (csDestroying in FScrollBox.ComponentState)) then
    fscrollBox.fOnAniStart(fscrollBox);
end;

{*******************************************}
procedure TALScrollBoxAniCalculations.DoStop;
begin
  inherited DoStop;

  if (FScrollBox.Scene <> nil) and
     (not (csDestroying in FScrollBox.ComponentState)) then
    FScrollBox.Scene.ChangeScrollingState(nil, False);

  if (assigned(fscrollBox.fOnAniStop)) and
     (not (csDestroying in FScrollBox.ComponentState)) then
    fscrollBox.fOnAniStop(fscrollBox);
end;

{*****************************************************}
constructor TALScrollBoxBar.Create(AOwner: TComponent);
begin
  ValidateInheritance(AOwner, TALCustomScrollBox, False{CanBeNil});
  inherited Create(AOwner);
  FScrollBox := TALCustomScrollBox(AOwner);
  HitTest := False;
end;

{**********************************}
procedure TALScrollBoxBar.DoChanged;
begin
  if FScrollBox.fdisableScrollChange then exit;
  FScrollBox.fdisableScrollChange := True;
  try
    if Orientation=TOrientation.vertical then FScrollBox.fAniCalculations.ViewportPosition := TAlPointD.Create(FScrollBox.fAniCalculations.ViewportPosition.x, Value)
    else FScrollBox.fAniCalculations.ViewportPosition := TAlPointD.Create(Value, FScrollBox.fAniCalculations.ViewportPosition.Y);
  finally
    FScrollBox.fdisableScrollChange := False;
  end;
  inherited DoChanged;
end;

{*******************************}
procedure TALScrollBoxBar.Resize;
begin
  inherited;
  FScrollBox.Realign;
end;

{********************************************************}
constructor TALCustomScrollBox.Create(AOwner: TComponent);
var aDeviceService: IFMXDeviceService;
    aScreenSrv: IFMXScreenService;
begin
  inherited Create(AOwner);
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, aScreenSrv) then FScreenScale := aScreenSrv.GetScreenScale
  else FScreenScale := 1;
  ClipChildren := True;
  SetAcceptsControls(True);
  AutoCapture := True;
  if SupportsPlatformService(IFMXDeviceService, aDeviceService) then FHasTouchScreen := TDeviceFeature.HasTouchScreen in aDeviceService.GetFeatures
  else FHasTouchScreen := False;
  Touch.DefaultInteractiveGestures := Touch.DefaultInteractiveGestures + [TInteractiveGesture.Pan];
  Touch.InteractiveGestures := Touch.InteractiveGestures + [TInteractiveGesture.Pan];
  FAutoHide := True;
  FShowScrollBars := True;
  fdisableScrollChange := False;
  FDisableMouseWheel := False;
  FMouseEvents := False;
  fGestureEvents := False;
  FOnViewportPositionChange := nil;
  fOnScrollBarInit := nil;
  fOnAniStart := nil;
  fOnAniStop := nil;
  fAniCalculations := CreateAniCalculations;
  fMaxContentWidth := 0;
  fMaxContentHeight := 0;
  fAnchoredContentOffset := TpointF.Create(0,0);
  //-----
  FVScrollBar := nil;
  FHScrollBar := nil;
  FContent := CreateContent;
  //-----
  fMouseDownPos := TpointF.Create(0,0);
  FDeadZoneBeforeAcquireScrolling := 32;
  fScrollingAcquiredByMe := False;
  fScrollingAcquiredByOther := False;
  fScrollingAcquiredByOtherMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TALScrollingAcquiredMessage, ScrollingAcquiredByOtherHandler);
end;

{************************************}
destructor TALCustomScrollBox.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TALScrollingAcquiredMessage, fScrollingAcquiredByOtherMessageID);
  ALFreeAndNil(FAniCalculations);
  inherited;
end;

{************************************************************}
function TALCustomScrollBox.isMaxContentHeightStored: Boolean;
begin
  result := not sameValue(fMaxContentHeight, 0, Tepsilon.Position);
end;

{***********************************************************}
function TALCustomScrollBox.isMaxContentWidthStored: Boolean;
begin
  result := not sameValue(fMaxContentWidth, 0, Tepsilon.Position);
end;

{****************************************************}
function TALCustomScrollBox.CalcContentBounds: TRectF;
var I: Integer;
begin
  Result := LocalRect;
  if (FContent <> nil) then
    for I := 0 to FContent.ControlsCount - 1 do
      if FContent.Controls[I].Visible then begin
        {$IFDEF MSWINDOWS}
        if (csDesigning in ComponentState) and Supports(FContent.Controls[I], IDesignerControl) then
          Continue;
        {$ENDIF}
        Result.Union(FContent.Controls[I].BoundsRect);
      end;
  if result.Top < 0 then result.Top := 0;
  if result.left < 0 then result.left := 0;
  fAnchoredContentOffset := TpointF.Create(0, 0);
end;

{***********************************************}
function TALCustomScrollBox.GetTabList: ITabList;
begin
  if FContent <> nil then Result := FContent.GetTabList
  else Result := inherited GetTabList;
end;

{*************************************}
procedure TALCustomScrollBox.DoRealign;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateVScrollBar(const AContentRect: TRectF);
  begin
    if fVScrollBar <> nil then begin
      fVScrollBar.Enabled := AContentRect.Height > Height;
      fVScrollBar.Visible := FShowScrollBars and
                             ((AContentRect.Height > Height) or (not FAutoHide));
      fVScrollBar.ValueRange.BeginUpdate;
      try
        fVScrollBar.ValueRange.Min := 0;
        fVScrollBar.ValueRange.Max := AContentRect.Height;
        fVScrollBar.ValueRange.ViewportSize := height;
      finally
        fVScrollBar.ValueRange.EndUpdate;
      end;
      fVScrollBar.SetBounds(width - fVScrollBar.Width - fVScrollBar.Margins.Right,
                            fVScrollBar.Margins.top,
                            fVScrollBar.Width,
                            height-fVScrollBar.Margins.top-fVScrollBar.Margins.bottom);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateHScrollBar(const AContentRect: TRectF);
  begin
    if fHScrollBar <> nil then begin
      fHScrollBar.Enabled := AContentRect.Width > Width;
      fHScrollBar.Visible := FShowScrollBars and
                             ((AContentRect.Width > Width) or (not FAutoHide));
      fHScrollBar.ValueRange.BeginUpdate;
      try
        fHScrollBar.ValueRange.Min := 0;
        fHScrollBar.ValueRange.Max := AContentRect.Width;
        fHScrollBar.ValueRange.ViewportSize := Width;
      finally
        fHScrollBar.ValueRange.EndUpdate;
      end;
      fHScrollBar.SetBounds(fVScrollBar.Margins.left,
                            height - fHScrollBar.Height - fVScrollBar.Margins.bottom,
                            width - fVScrollBar.Margins.left - fVScrollBar.Margins.right,
                            fHScrollBar.Height);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateAnimationTargets(const aContentRect: TRectF);
  var aNewTargets: array of TALAniCalculations.TTarget;
  begin
    SetLength(aNewTargets, 2);
    aNewTargets[0].TargetType := TALAniCalculations.TTargetType.Min;
    aNewTargets[0].Point := TALPointD.Create(0,0);
    aNewTargets[1].TargetType := TALAniCalculations.TTargetType.Max;
    if (fHScrollBar <> nil) and
       (fVScrollBar <> nil) then aNewTargets[1].Point := TALPointD.Create(aContentRect.Width - width, aContentRect.Height - height)
    else if (fVScrollBar <> nil) then aNewTargets[1].Point := TALPointD.Create(0, aContentRect.Height - height)
    else if (fHScrollBar <> nil) then aNewTargets[1].Point := TALPointD.Create(aContentRect.Width - width, 0)
    else aNewTargets[1].Point := TALPointD.Create(0, 0);
    AniCalculations.SetTargets(aNewTargets);
  end;

var aContentRect: TrectF;
    aDoRealignAgain: boolean;
begin

  if fDisableAlign then exit;
  fDisableAlign := True;
  try

    aDoRealignAgain := False;
    if (FContent <> nil) then begin
      aContentRect := CalcContentBounds;
      Content.SetBounds(aContentRect.Left + fAnchoredContentOffset.x,
                        aContentRect.Top + fAnchoredContentOffset.y,
                        aContentRect.Width,
                        aContentRect.Height);
      if aContentRect.EqualsTo(CalcContentBounds, Tepsilon.Position) then  begin
        _UpdateVScrollBar(aContentRect);
        _UpdateHScrollBar(aContentRect);
        _UpdateAnimationTargets(aContentRect);
        fAniCalculations.DoChanged;
      end
      else aDoRealignAgain := True;
    end;

  finally
    fDisableAlign := false;
  end;

  if aDoRealignAgain then DoRealign;

end;

{*********************************************************************************************}
function TALCustomScrollBox.CreateScrollBar(const aOrientation: TOrientation): TALScrollBoxBar;
begin
  Result := TALScrollBoxBar.Create(self);
  Result.Parent := self;
  Result.Stored := False;
  Result.SetSubComponent(True);
  Result.Locked := True;
  Result.Beginupdate;
  try
    Result.Orientation := aOrientation;
    result.Thumb.HitTest := not HasTouchScreen; // at design time (windows) will be always true = default value
                                                // this mean that true will be never save in the dfm! only false
                                                // can be stored in the dfm. so only false in the dfm can override
                                                // this settings
    if aOrientation = TOrientation.Vertical then Result.Name := 'VScrollBar'
    else Result.Name := 'HScrollBar';
    Result.Visible := False;
  finally
    result.EndUpdate;
  end;
end;

{*************************************************************}
function TALCustomScrollBox.CreateContent: TALScrollBoxContent;
begin
  Result := TALScrollBoxContent.Create(Self);
  Result.parent := self;
  Result.Stored := False;
  Result.Locked := True;
  Result.HitTest := False;
end;

{*****************************************************************************}
function TALCustomScrollBox.CreateAniCalculations: TALScrollBoxAniCalculations;
begin
  Result := TALScrollBoxAniCalculations.Create(self);
  Result.BeginUpdate;
  try
    Result.Animation := HasTouchScreen;
    if HasTouchScreen then Result.TouchTracking := [ttVertical, ttHorizontal]
    else Result.TouchTracking := [];
    Result.BoundsAnimation := HasTouchScreen;
    Result.AutoShowing := HasTouchScreen;
  finally
    Result.EndUpdate;
  end;
end;

{****************************************************************************************}
procedure TALCustomScrollBox.setAniCalculations(const Value: TALScrollBoxAniCalculations);
begin
  FAniCalculations.Assign(Value);
end;

{**************************************************************************}
procedure TALCustomScrollBox.setScrollingAcquiredByMe(const Value: boolean);
begin
  if Value <> fScrollingAcquiredByMe  then begin
    fScrollingAcquiredByMe := Value;
    TMessageManager.DefaultManager.SendMessage(self, TALScrollingAcquiredMessage.Create(Value), True);
  end;
end;

{*****************************************************************************************************}
procedure TALCustomScrollBox.ScrollingAcquiredByOtherHandler(const Sender: TObject; const M: TMessage);
begin
  //the scrolling was acquired or released by another control (like a scrollbox for exemple)
  //the problem is that the scrolling could be acquired BEFORE the mousedown is fired in parent control (baah yes)
  //so we need the var fScrollingAcquiredByOther to handle this
  if (Sender = self) then exit;
  if TALScrollingAcquiredMessage(M).Acquired then begin
    if fAniCalculations.down then begin
      fAniCalculations.Down := false;
      fAniCalculations.CurrentVelocity := TalPointD.Create(0,0);
      FMouseEvents := False;
      fGestureEvents := False;
    end;
    fScrollingAcquiredByOther := True;
  end
  else fScrollingAcquiredByOther := False;
end;

{***********************************************************************}
procedure TALCustomScrollBox.CMGesture(var EventInfo: TGestureEventInfo);
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
        fMouseDownPos := TpointF.Create(LP.X, LP.Y);
        fGestureEvents := true;
        FAniCalculations.averaging := true;
        AniCalculations.MouseDown(LP.X, LP.Y);
      end;
    end
    else if fGestureEvents and
            (EventInfo.Flags = []) then begin
      if (not fScrollingAcquiredByMe) and
         (((ttHorizontal in fAniCalculations.TouchTracking) and
           (abs(fMouseDownPos.x - LP.x) > fDeadZoneBeforeAcquireScrolling)) or
          ((ttVertical in fAniCalculations.TouchTracking) and
           (abs(fMouseDownPos.y - LP.y) > fDeadZoneBeforeAcquireScrolling))) then setScrollingAcquiredByMe(True);
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

{*********************************************************************************************}
procedure TALCustomScrollBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FMouseEvents := not fGestureEvents;
  inherited;
  if (not fScrollingAcquiredByOther) and FMouseEvents and (Button = TMouseButton.mbLeft) then begin
    setScrollingAcquiredByMe(False);
    fMouseDownPos := TpointF.Create(X,Y);
    FAniCalculations.averaging := ssTouch in Shift;
    AniCalculations.MouseDown(X, Y);
  end;
end;

{***********************************************************************}
procedure TALCustomScrollBox.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FMouseEvents then begin
    if (not fScrollingAcquiredByMe) and
       (((ttHorizontal in fAniCalculations.TouchTracking) and
         (abs(fMouseDownPos.x - x) > fDeadZoneBeforeAcquireScrolling)) or
        ((ttVertical in fAniCalculations.TouchTracking) and
         (abs(fMouseDownPos.y - y) > fDeadZoneBeforeAcquireScrolling))) then setScrollingAcquiredByMe(True);
    AniCalculations.MouseMove(X, Y);
  end;
end;

{*******************************************************************************************}
procedure TALCustomScrollBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FMouseEvents and (Button = TMouseButton.mbLeft) then begin
    setScrollingAcquiredByMe(False);
    AniCalculations.MouseUp(X, Y);
    FMouseEvents := False;
  end;
end;

{****************************************}
procedure TALCustomScrollBox.DoMouseLeave;
begin
  inherited;
  if FMouseEvents then begin
    setScrollingAcquiredByMe(False);
    AniCalculations.MouseLeave;
    FMouseEvents := False;
  end;
end;

{*****************************************************************************************************}
procedure TALCustomScrollBox.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
var Offset: Single;
begin
  inherited;
  if (not (Handled or FDisableMouseWheel)) then begin
    if ssHorizontal in Shift then begin
      if FContent.Width > Width then begin
        Offset := (Width / 5) * -1 * (WheelDelta / 120);
        AniCalculations.MouseWheel(Offset, 0);
        Handled := True;
      end;
    end
    else if FContent.Height > Height then begin
      Offset := (Height / 5) * -1 * (WheelDelta / 120);
      AniCalculations.MouseWheel(0, Offset);
      Handled := True;
    end
    else if FContent.Width > Width then begin
      Offset := (Width / 5) * -1 * (WheelDelta / 120);
      AniCalculations.MouseWheel(Offset, 0);
      Handled := True;
    end;
  end;
end;

{******************************************************************}
procedure TALCustomScrollBox.DoAddObject(const AObject: TFmxObject);
begin
  if (FContent <> nil) and
     (AObject <> FContent) and
     (not (AObject is TEffect)) and
     (not (AObject is TAnimation)) and
     (not (AObject is TALScrollBoxBar)) then FContent.AddObject(AObject)
  else inherited;
end;

{**********************************}
procedure TALCustomScrollBox.Loaded;
begin
  inherited Loaded;
  if assigned(fOnScrollBarInit) then begin
    if assigned(fVscrollBar) then fOnScrollBarInit(self, fVscrollBar);
    if assigned(fHscrollBar) then fOnScrollBarInit(self, fHscrollBar);
  end;
end;

{**********************************************************}
procedure TALCustomScrollBox.ScrollBy(const Dx, Dy: Single);
begin
  if VScrollBar <> nil then VScrollBar.Value := VScrollBar.Value - Dy;
  if HScrollBar <> nil then HScrollBar.Value := HScrollBar.Value - Dx;
end;

{*************************************************************}
procedure TALCustomScrollBox.SetAutoHide(const Value: Boolean);
begin
  if FAutoHide <> Value then begin
    FAutoHide := Value;
    Realign; // << i m lazzy i now !
  end;
end;

{*******************************************************************}
procedure TALCustomScrollBox.SetShowScrollBars(const Value: Boolean);
begin
  if FShowScrollBars <> Value then begin
    FShowScrollBars := Value;
    Realign; // << i m lazzy i now !
  end;
end;

{****************************************************************}
procedure TALCustomScrollBox.Sort(Compare: TFmxObjectSortCompare);
begin
  FContent.Sort(Compare);
end;

{**************************************************}
constructor TALScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FVScrollBar := CreateScrollBar(Torientation.Vertical);
  FHScrollBar := CreateScrollBar(Torientation.Horizontal);
end;

{***************************}
procedure TALScrollBox.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{**************************************************}
function TALVertScrollBox.CalcContentBounds: TRectF;
begin
  Result := inherited CalcContentBounds;
  if not sameValue(fMaxContentWidth, 0, Tepsilon.Position) then begin
    result.Width := Min(fMaxContentWidth, Width);
    fAnchoredContentOffset.X := Round(((Width - result.Width) / 2) * FScreenScale) / FScreenScale;
  end
  else Result.Width := Width;
end;

{******************************************************}
constructor TALVertScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FVScrollBar := CreateScrollBar(Torientation.Vertical);
end;

{***************************************************************************}
function TALVertScrollBox.CreateAniCalculations: TALScrollBoxAniCalculations;
begin
  result := inherited CreateAniCalculations;
  result.TouchTracking := result.TouchTracking - [ttHorizontal];
end;

{*******************************}
procedure TALVertScrollBox.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{**************************************************}
function TALHorzScrollBox.CalcContentBounds: TRectF;
begin
  Result := inherited CalcContentBounds;
  Result.Height := Height;
  if not sameValue(fMaxContentHeight, 0, Tepsilon.Position) then begin
    result.Height := min(Height, fMaxContentHeight);
    fAnchoredContentOffset.Y := Round(((Height - result.Height) / 2) * FScreenScale) / FScreenScale;
  end
  else Result.Height := Height;
end;

{******************************************************}
constructor TALHorzScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FHScrollBar := CreateScrollBar(Torientation.Horizontal);
end;

{***************************************************************************}
function TALHorzScrollBox.CreateAniCalculations: TALScrollBoxAniCalculations;
begin
  result := inherited CreateAniCalculations;
  result.TouchTracking := result.TouchTracking - [ttVertical];
end;

{*******************************}
procedure TALHorzScrollBox.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALLayout, TALScrollBox, TALVertScrollBox, TALHorzScrollBox]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALScrollBoxBar, 'Locked');
  UnlistPublishedProperty(TALScrollBoxBar, 'StyleName');
  UnlistPublishedProperty(TALScrollBoxBar, 'Anchors'); // not work https://quality.embarcadero.com/browse/RSP-15684
  UnlistPublishedProperty(TALScrollBoxBar, 'Align');
  UnlistPublishedProperty(TALScrollBoxBar, 'Position');
  UnlistPublishedProperty(TALScrollBoxBar, 'PopupMenu');
  UnlistPublishedProperty(TALScrollBoxBar, 'DragMode');
  UnlistPublishedProperty(TALScrollBoxBar, 'OnDragEnter');
  UnlistPublishedProperty(TALScrollBoxBar, 'OnDragLeave');
  UnlistPublishedProperty(TALScrollBoxBar, 'OnDragOver');
  UnlistPublishedProperty(TALScrollBoxBar, 'OnDragDrop');
  UnlistPublishedProperty(TALScrollBoxBar, 'OnDragEnd');
  UnlistPublishedProperty(TALScrollBoxBar, 'EnableDragHighlight');
  UnlistPublishedProperty(TALScrollBoxBar, 'Max');
  UnlistPublishedProperty(TALScrollBoxBar, 'Min');
  UnlistPublishedProperty(TALScrollBoxBar, 'Orientation');
  UnlistPublishedProperty(TALScrollBoxBar, 'Value');
  UnlistPublishedProperty(TALScrollBoxBar, 'ViewportSize');
  UnlistPublishedProperty(TALScrollBoxBar, 'Enabled'); // this is changeable by the algo
  UnlistPublishedProperty(TALScrollBoxBar, 'Opacity'); // this is changeable by the algo
  UnlistPublishedProperty(TALScrollBoxBar, 'Visible'); // this is changeable by the algo and by showscrollbars
  UnlistPublishedProperty(TALScrollBoxBar, 'TouchTargetExpansion');
  UnlistPublishedProperty(TALScrollBoxBar, 'Touch');
  {$ENDIF}
end;

end.


