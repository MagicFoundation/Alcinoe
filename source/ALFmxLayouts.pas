unit ALFmxLayouts;

{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if FMX.Layouts.pas was not updated and adjust the IFDEF'}
{$ENDIF}

interface

{$SCOPEDENUMS ON}
{$DEFINE IPUB_FIXES}

uses
  System.Classes,
  System.Types,
  System.UITypes,
  System.Messaging,
  FMX.layouts,
  FMX.Types,
  FMX.Controls,
  ALFmxStdCtrls,
  ALFmxInertialMovement;

type

  {*************************}
  [ComponentPlatforms($FFFF)]
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
    // -------------------------------------------------------------------------
    // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
    // Expose the IsTracking property. It is very usefull in many cases
    {$IFDEF IPUB_FIXES} // -----------------------------------------------------
    property IsTracking;
    {$ENDIF} // ----------------------------------------------------------------
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
    // -------------------------------------------------------------------------
    // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
    // Improving the compatibility with TCustomScrollBox. This is important 
    // because in a custom scrollbox we can easily change the parent class of
    // TScrollBox to TALScrollBox
    {$IFDEF IPUB_FIXES} // -----------------------------------------------------
    FOnViewportPositionChange: TPositionChangeEvent;
    {$ELSE} // -----------------------------------------------------------------
    FOnViewportPositionChange: TALScrollBoxPositionChangeEvent;
    {$ENDIF} // ----------------------------------------------------------------
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
    procedure internalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure internalMouseMove(Shift: TShiftState; X, Y: Single);
    procedure internalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure internalMouseLeave;
  protected
    procedure Loaded; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    // -------------------------------------------------------------------------
    // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
    // Improving the compatibility with TCustomScrollBox. This is important 
    // because in a custom scrollbox we can easily change the parent class of
    // TScrollBox to TALScrollBox
    {$IFDEF IPUB_FIXES} // -----------------------------------------------------
    procedure DoRealignContent(R: TRectF); virtual;
    function IsAddToContent(const AObject: TFmxObject): Boolean; virtual;
    procedure ViewportPositionChange(const AOldViewportPosition, 
	  ANewViewportPosition: TPointF; const AContentSizeChanged: Boolean); virtual;
    {$ENDIF} // ----------------------------------------------------------------
    procedure DoRealign; override;
    function CreateScrollBar(const aOrientation: TOrientation): TALScrollBoxBar; virtual;
    function CreateContent: TALScrollBoxContent; virtual;
    function CreateAniCalculations: TALScrollBoxAniCalculations; virtual;
    function CalcContentBounds: TRectF; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    // -------------------------------------------------------------------------
    // [Stephane Vander Clock - github.com/Zeus64] - 28/05/2020 - Delphi 10.4
    // https://quality.embarcadero.com/browse/RSP-24397
    {$IFDEF DELPHI_FIXES} // ---------------------------------------------------
    procedure ChildrenMouseDown(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseMove(const AObject: TControl; Shift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseUp(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseLeave(const AObject: TControl); override;
    {$ENDIF} // ----------------------------------------------------------------
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    // -------------------------------------------------------------------------
    // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
    // Expose to write the fields fAnchoredContentOffset, fHScrollBar and 
    // fVScrollBar in descendant classes. The fAnchoredContentOffset could be
    // changed in an override of DoCalcContentBounds method will be very
    // usefull. The fHScrollBar and fVScrollBar fields need to be acessed to be
    // created by custom descendent classes. The TALScrollBox have access to
    // these private fields because is in same unit, but others custom classes
    // should have access too.
    {$IFDEF IPUB_FIXES} // -----------------------------------------------------
    property AnchoredContentOffset: TPointF read FAnchoredContentOffset write FAnchoredContentOffset;
    property HScrollBar: TALScrollBoxBar read fHScrollBar write fHScrollBar;
    property VScrollBar: TALScrollBoxBar read fVScrollBar write fVScrollBar;
    {$ELSE} // -----------------------------------------------------------------
    property HScrollBar: TALScrollBoxBar read fHScrollBar;
    property VScrollBar: TALScrollBoxBar read fVScrollBar;
    {$ENDIF} // ----------------------------------------------------------------
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
    // -------------------------------------------------------------------------
    // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
    // Improving the compatibility with TCustomScrollBox. This is important 
    // because in a custom scrollbox we can easily change the parent class of
    // TScrollBox to TALScrollBox
    {$IFDEF IPUB_FIXES} // -----------------------------------------------------
    property OnViewportPositionChange: TPositionChangeEvent read FOnViewportPositionChange write FOnViewportPositionChange;
    {$ELSE} // -----------------------------------------------------------------
    property OnViewportPositionChange: TALScrollBoxPositionChangeEvent read FOnViewportPositionChange write FOnViewportPositionChange;
    {$ENDIF} // ----------------------------------------------------------------
    property DeadZoneBeforeAcquireScrolling: Integer read FDeadZoneBeforeAcquireScrolling write FDeadZoneBeforeAcquireScrolling default 32;
    property OnScrollBarInit: TALScrollBoxBarInit read fOnScrollBarInit write fOnScrollBarInit;
    property ClipChildren default true;
    property OnAniStart: TnotifyEvent read fOnAniStart write fOnAniStart;
    property OnAniStop: TnotifyEvent read fOnAniStop write fOnAniStop;
  end;

  {*************************}
  [ComponentPlatforms($FFFF)]
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
    property Hint;
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
    property ParentShowHint;
    property ShowHint;
    { Events }
    property OnPainting;
    property OnPaint;
    property OnResize;
    {$IF CompilerVersion >= 32} // tokyo
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

  {*************************}
  [ComponentPlatforms($FFFF)]
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
    property Hint;
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
    property ParentShowHint;
    property ShowHint;
    { Events }
    property OnPainting;
    property OnPaint;
    property OnResize;
    {$IF CompilerVersion >= 32} // tokyo
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

  {*************************}
  [ComponentPlatforms($FFFF)]
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
    property Hint;
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
    property ParentShowHint;
    property ShowHint;
    { Events }
    property OnPainting;
    property OnPaint;
    property OnResize;
    {$IF CompilerVersion >= 32} // tokyo
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

uses
  System.SysUtils,
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
  // ---------------------------------------------------------------------------
  // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
  // Fixing to avoid the scrollbox to movement when the user click in one
  // ITextInput control inside the scrollbox, like TEdit
  {$IFDEF IPUB_FIXES} // -------------------------------------------------------
  FMX.Text,
  {$ENDIF} // ------------------------------------------------------------------
  AlFmxCommon,
  ALCommon;

{*******************************************************************************************************}
// http://stackoverflow.com/questions/39317984/does-the-delphi-firemonkey-dorealign-implemented-correctly
// https://quality.embarcadero.com/browse/RSP-15768
// often we assign some event to some control onresize (like TText with autosize=True) to
// resize their parentcontrols to the same size as them. But in this way the problem is that if
// we resize the parentcontrol during it's dorealign process then it will not call again dorealign
procedure TALLayout.DoRealign;
var LOriginalSize: TpointF;
begin
  LOriginalSize := Size.Size;
  inherited;
  if not LOriginalSize.EqualsTo(Size.Size) then DoRealign;
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

// -----------------------------------------------------------------------------
// [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
// Fix the pixel alignment of ViewportPosition and minor improvements
{$IFDEF IPUB_FIXES} // ---------------------------------------------------------
  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _getPixelAlignedViewportPosition(const AOffset: TPointF): TPointF;
  var X, Y: Single;
  begin
    if SameValue(ViewportPosition.X, 0, TEpsilon.Position) then
      X := 0
    else
      X := (Round((ViewportPosition.X + AOffset.X) * FScreenScale) / FScreenScale) - AOffset.X;
    if SameValue(ViewportPosition.Y, 0, TEpsilon.Position) then
      Y := 0
    else
      Y := (Round((ViewportPosition.Y + AOffset.Y) * FScreenScale) / FScreenScale) - AOffset.Y;
    Result := TPointF.Create(X - FScrollBox.fAnchoredContentOffset.X,  // FScrollBox.fAnchoredContentOffset.X is already pixel aligned and if present then x = 0 so return -FScrollBox.fAnchoredContentOffset.X
                             Y - FScrollBox.fAnchoredContentOffset.Y); // FScrollBox.fAnchoredContentOffset.Y is already pixel aligned and if present then y = 0 so return -FScrollBox.fAnchoredContentOffset.Y
  end;

var
  LViewportPositionAligned: TPointF;
  LOffset: TPointF;
begin
  if (not (csDestroying in FScrollBox.ComponentState)) then begin

    if FScrollBox <> nil then
      LOffset := FScrollBox.AbsoluteRect.TopLeft
    else
      LOffset := TPointF.Zero;

    LViewportPositionAligned := _getPixelAlignedViewportPosition(LOffset);
    if not fLastViewportPosition.EqualsTo(LViewportPositionAligned, TEpsilon.Position) then
    begin
      //update FScrollBox.Content.Position
      if FScrollBox.Content <> nil then
        FScrollBox.Content.Position.Point := -LViewportPositionAligned;

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
      FScrollBox.ViewportPositionChange(fLastViewportPosition, LViewportPositionAligned, False);
      fLastViewportPosition := LViewportPositionAligned;
    end;

  end;
{$ELSE} // ---------------------------------------------------------------------
  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _getPixelAlignedViewportPosition: TPointF;
  var X, Y: Single;
  begin
    X := Round(ViewportPosition.X * FScreenScale) / FScreenScale;
    Y := Round(ViewportPosition.Y * FScreenScale) / FScreenScale;
    Result := TPointF.Create(X - FScrollBox.fAnchoredContentOffset.X,  // FScrollBox.fAnchoredContentOffset.X is already pixel aligned and if present then x = 0 so return -FScrollBox.fAnchoredContentOffset.X
                             Y - FScrollBox.fAnchoredContentOffset.Y); // FScrollBox.fAnchoredContentOffset.Y is already pixel aligned and if present then y = 0 so return -FScrollBox.fAnchoredContentOffset.Y
  end;

var LNewViewportPosition: TPointF;

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
    LNewViewportPosition := TpointF.Create(ViewportPosition.X, ViewportPosition.Y);
    if (assigned(FScrollBox.FOnViewportPositionChange)) and
       (not fLastViewportPosition.EqualsTo(LNewViewportPosition, TEpsilon.Position)) then
      FScrollBox.FOnViewportPositionChange(self, fLastViewportPosition, LNewViewportPosition);
    fLastViewportPosition := LNewViewportPosition;

  end;
{$ENDIF} // --------------------------------------------------------------------
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
var LDeviceService: IFMXDeviceService;
    LScreenSrv: IFMXScreenService;
begin
  inherited Create(AOwner);
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, LScreenSrv) then FScreenScale := LScreenSrv.GetScreenScale
  else FScreenScale := 1;
  ClipChildren := True;
  SetAcceptsControls(True);
  AutoCapture := True;
  if SupportsPlatformService(IFMXDeviceService, LDeviceService) then FHasTouchScreen := TDeviceFeature.HasTouchScreen in LDeviceService.GetFeatures
  else FHasTouchScreen := False;
  Touch.DefaultInteractiveGestures := Touch.DefaultInteractiveGestures + [TInteractiveGesture.Pan];
  Touch.InteractiveGestures := Touch.InteractiveGestures + [TInteractiveGesture.Pan];
  FAutoHide := True;
  FShowScrollBars := True;
  fdisableScrollChange := False;
  FDisableMouseWheel := False;
  FMouseEvents := False;
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
      // -----------------------------------------------------------------------
      // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
      // Fix the bars in desktop when the mouse is over the scrollbox
      {$IFDEF IPUB_FIXES} // ---------------------------------------------------
      {$IF defined(Android) or defined(iOS)}
      fVScrollBar.Visible := FShowScrollBars and
                             ((AContentRect.Height > Height) or (not FAutoHide));
      {$ELSE}
      fVScrollBar.Visible := (IsMouseOver or fVScrollBar.IsMouseOver or fVScrollBar.Thumb.IsMouseOver or fVScrollBar.IsTracking) and
                             ((AContentRect.Height > Height) or (not FAutoHide));
      if fVScrollBar.Visible then
        fVScrollBar.BringToFront;
      {$ENDIF}
      {$ELSE} // ---------------------------------------------------------------
      fVScrollBar.Visible := FShowScrollBars and
                             ((AContentRect.Height > Height) or (not FAutoHide));
      {$ENDIF} // --------------------------------------------------------------
      fVScrollBar.ValueRange.BeginUpdate;
      try
        fVScrollBar.ValueRange.Min := 0;
        fVScrollBar.ValueRange.Max := AContentRect.Height;
        fVScrollBar.ValueRange.ViewportSize := height;
      finally
        fVScrollBar.ValueRange.EndUpdate;
      end;
      // -----------------------------------------------------------------------
      // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
      // Added possibility to change the bars position by the padding of the 
      // FContent. This is usefull because sometimes we have an control over the
      // scrollbox (like a navigation menu) and we don't want the bars inside
      // the navigation menu like any control of the scrollbox, then we can just
      // set the FContent padding
      {$IFDEF IPUB_FIXES} // ---------------------------------------------------
      fVScrollBar.SetBounds(width - fVScrollBar.Width - fVScrollBar.Margins.Right - FContent.Padding.Right,
                            fVScrollBar.Margins.top + FContent.Padding.Top,
                            fVScrollBar.Width,
                            height-fVScrollBar.Margins.top-fVScrollBar.Margins.bottom - FContent.Padding.Top - FContent.Padding.Bottom);
      {$ELSE} // ---------------------------------------------------------------
      fVScrollBar.SetBounds(width - fVScrollBar.Width - fVScrollBar.Margins.Right,
                            fVScrollBar.Margins.top,
                            fVScrollBar.Width,
                            height-fVScrollBar.Margins.top-fVScrollBar.Margins.bottom);
      {$ENDIF} // --------------------------------------------------------------
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateHScrollBar(const AContentRect: TRectF);
  begin
    if fHScrollBar <> nil then begin
      fHScrollBar.Enabled := AContentRect.Width > Width;
      // -----------------------------------------------------------------------
      // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
      // Fix the bars in desktop when the mouse is over the scrollbox
      {$IFDEF IPUB_FIXES} // ---------------------------------------------------
      {$IF defined(Android) or defined(iOS)}
      fHScrollBar.Visible := FShowScrollBars and
                             ((AContentRect.Width > Width) or (not FAutoHide));
      {$ELSE}
      fHScrollBar.Visible := (IsMouseOver or fHScrollBar.IsMouseOver or fHScrollBar.Thumb.IsMouseOver or fHScrollBar.IsTracking) and
                             ((AContentRect.Width > Width) or (not FAutoHide));
      if fHScrollBar.Visible then
        fHScrollBar.BringToFront;
      {$ENDIF}
      {$ELSE} // ---------------------------------------------------------------
      fHScrollBar.Visible := FShowScrollBars and
                             ((AContentRect.Width > Width) or (not FAutoHide));
      {$ENDIF} // --------------------------------------------------------------
      fHScrollBar.ValueRange.BeginUpdate;
      try
        fHScrollBar.ValueRange.Min := 0;
        fHScrollBar.ValueRange.Max := AContentRect.Width;
        fHScrollBar.ValueRange.ViewportSize := Width;
      finally
        fHScrollBar.ValueRange.EndUpdate;
      end;
      // -----------------------------------------------------------------------
      // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
      // Added possibility to change the bars position by the padding of the 
      // FContent. This is usefull because sometimes we have an control over the
      // scrollbox (like a navigation menu) and we don't want the bars inside
      // the navigation menu like any control of the scrollbox, then we can just
      // set the FContent padding
      {$IFDEF IPUB_FIXES} // ---------------------------------------------------
      fHScrollBar.SetBounds(fHScrollBar.Margins.left +  FContent.Padding.Left,
                            height - fHScrollBar.Height - fHScrollBar.Margins.bottom - FContent.Padding.Bottom,
                            width - fHScrollBar.Margins.left - fHScrollBar.Margins.right -  FContent.Padding.Left -  FContent.Padding.Right,
                            fHScrollBar.Height);
      {$ELSE} // ---------------------------------------------------------------
      fHScrollBar.SetBounds(fHScrollBar.Margins.left,
                            height - fHScrollBar.Height - fHScrollBar.Margins.bottom,
                            width - fHScrollBar.Margins.left - fHScrollBar.Margins.right,
                            fHScrollBar.Height);
      {$ENDIF} // --------------------------------------------------------------
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateAnimationTargets(const aContentRect: TRectF);
  var LNewTargets: array of TALAniCalculations.TTarget;
  begin
    SetLength(LNewTargets, 2);
    LNewTargets[0].TargetType := TALAniCalculations.TTargetType.Min;
    LNewTargets[0].Point := TALPointD.Create(0,0);
    LNewTargets[1].TargetType := TALAniCalculations.TTargetType.Max;
    if (fHScrollBar <> nil) and
       (fVScrollBar <> nil) then LNewTargets[1].Point := TALPointD.Create(aContentRect.Width - width, aContentRect.Height - height)
    else if (fVScrollBar <> nil) then LNewTargets[1].Point := TALPointD.Create(0, aContentRect.Height - height)
    else if (fHScrollBar <> nil) then LNewTargets[1].Point := TALPointD.Create(aContentRect.Width - width, 0)
    else LNewTargets[1].Point := TALPointD.Create(0, 0);
    AniCalculations.SetTargets(LNewTargets);
  end;

var LContentRect: TrectF;
    LDoRealignAgain: boolean;

begin

  if fDisableAlign then exit;
  fDisableAlign := True;
  try

    LDoRealignAgain := False;
    if (FContent <> nil) then begin
      LContentRect := CalcContentBounds;
      Content.SetBounds(LContentRect.Left + fAnchoredContentOffset.x,
                        LContentRect.Top + fAnchoredContentOffset.y,
                        LContentRect.Width,
                        LContentRect.Height);
      if LContentRect.EqualsTo(CalcContentBounds, Tepsilon.Position) then  begin
        _UpdateVScrollBar(LContentRect);
        _UpdateHScrollBar(LContentRect);
        _UpdateAnimationTargets(LContentRect);
        fAniCalculations.DoChanged;
        // ---------------------------------------------------------------------
        // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
        // Improving the compatibility with TCustomScrollBox. This is important
        // because in a custom scrollbox we can easily change the parent class of
        // TScrollBox to TALScrollBox
        {$IFDEF IPUB_FIXES} // -------------------------------------------------
        DoRealignContent(LContentRect);
        {$ENDIF} // ------------------------------------------------------------
      end
      else LDoRealignAgain := True;
    end;

  finally
    fDisableAlign := false;
  end;

  if LDoRealignAgain then DoRealign;

end;

// -----------------------------------------------------------------------------
// [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
// Improving the compatibility with TCustomScrollBox. This is important 
// because in a custom scrollbox we can easily change the parent class of 
// TScrollBox to TALScrollBox
{$IFDEF IPUB_FIXES} // ---------------------------------------------------------
{*********************************************************************************************}
procedure TALCustomScrollBox.DoRealignContent(R: TRectF);
begin
end;

{*********************************************************************************************}
function TALCustomScrollBox.IsAddToContent(const AObject: TFmxObject): Boolean;
begin
  Result := (FContent <> nil) and
     (AObject <> FContent) and
     (not (AObject is TEffect)) and
     (not (AObject is TAnimation)) and
     (not (AObject is TALScrollBoxBar));
end;

{*********************************************************************************************}
procedure TALCustomScrollBox.ViewportPositionChange(const AOldViewportPosition, 
  ANewViewportPosition: TPointF; const AContentSizeChanged: Boolean);
begin
  if Assigned(FOnViewportPositionChange) then
    FOnViewportPositionChange(Self, AOldViewportPosition, ANewViewportPosition,
      AContentSizeChanged);
end;
{$ENDIF} // --------------------------------------------------------------------

{*********************************************************************************************}
function TALCustomScrollBox.CreateScrollBar(const aOrientation: TOrientation): TALScrollBoxBar;
// -----------------------------------------------------------------------------
// [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
// Added a small margins in bars by default to the bars don't "leave" the screen 
// especially in maximized forms on windows
{$IFDEF IPUB_FIXES} // ---------------------------------------------------------
const
  SCROLL_BAR_PARALLEL_MARGIN = {$IFDEF MSWINDOWS}3{$ELSE}2{$ENDIF};
  SCROLL_BAR_TIP_MARGIN = 4;
{$ENDIF} // --------------------------------------------------------------------
begin
  Result := TALScrollBoxBar.Create(self);
  Result.Parent := self;
  Result.Stored := False;
  Result.SetSubComponent(True);
  Result.Locked := True;
  Result.Beginupdate;
  try
    // -------------------------------------------------------------------------
    // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
    // Added a small margins in bars by default to the bars don't "leave" the
    // screen especially in maximized forms on windows
    {$IFDEF IPUB_FIXES} // -----------------------------------------------------
    if aOrientation = TOrientation.Vertical then
      Result.Margins.Rect := TRectF.Create(0, SCROLL_BAR_TIP_MARGIN, SCROLL_BAR_PARALLEL_MARGIN, SCROLL_BAR_TIP_MARGIN)
    else if aOrientation = TOrientation.Horizontal then
      Result.Margins.Rect := TRectF.Create(SCROLL_BAR_TIP_MARGIN, 0, SCROLL_BAR_TIP_MARGIN, SCROLL_BAR_PARALLEL_MARGIN);
    {$ENDIF} // ----------------------------------------------------------------
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
    end;
    fScrollingAcquiredByOther := True;
  end
  else fScrollingAcquiredByOther := False;
end;

{*****************************************************************************************************}
procedure TALCustomScrollBox.internalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  // ---------------------------------------------------------------------------
  // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
  // Fixed tracking with mouse in desktop, it was firing to movements at same 
  // time (the tracking (bars dragging) and the tap operations (mouse down, 
  // move, up))
  {$IFDEF IPUB_FIXES} // -------------------------------------------------------
  if (not (Assigned(FVScrollBar) and FVScrollBar.IsTracking)) and (not (Assigned(FHScrollBar) and FHScrollBar.IsTracking)) then
  {$ENDIF} // ------------------------------------------------------------------
  FMouseEvents := true;
  inherited;
  if (not fScrollingAcquiredByOther) and FMouseEvents and (Button = TMouseButton.mbLeft) then begin
    setScrollingAcquiredByMe(False);
    fMouseDownPos := TpointF.Create(X,Y);
    FAniCalculations.averaging := ssTouch in Shift;
    AniCalculations.MouseDown(X, Y);
  end;
end;

{*******************************************************************************}
procedure TALCustomScrollBox.internalMouseMove(Shift: TShiftState; X, Y: Single);
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
  // ---------------------------------------------------------------------------
  // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
  // Fix the bars in desktop when the mouse is over the scrollbox
  {$IFDEF IPUB_FIXES} // -------------------------------------------------------
  {$IF not defined(ANDROID) and not defined(iOS)}
  if Assigned(FHScrollBar) then
  begin
    FHScrollBar.Visible := FHScrollBar.Enabled or (fAniCalculations.ViewportPosition.X <> 0);
    if FHScrollBar.Visible then
      FHScrollBar.BringToFront;
  end;
  if Assigned(FVScrollBar) then
  begin
    FVScrollBar.Visible := FVScrollBar.Enabled or (fAniCalculations.ViewportPosition.Y <> 0);
    if FVScrollBar.Visible then
      FVScrollBar.BringToFront;
  end;
  {$ENDIF}
  {$ENDIF} // ------------------------------------------------------------------
end;

{***************************************************************************************************}
procedure TALCustomScrollBox.internalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FMouseEvents and (Button = TMouseButton.mbLeft) then begin
    setScrollingAcquiredByMe(False);
    AniCalculations.MouseUp(X, Y);
    FMouseEvents := False;
  end;
end;

{**********************************************}
procedure TALCustomScrollBox.internalMouseLeave;
begin
  inherited;
  if FMouseEvents then begin
    setScrollingAcquiredByMe(False);
    AniCalculations.MouseLeave;
    FMouseEvents := False;
  end;
  // ---------------------------------------------------------------------------
  // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
  // Fix the bars in desktop when the mouse is not over the scrollbox. This is 
  // how modern apps work, the scroll only appears when the mouse is inside the 
  // scrollbox
  {$IFDEF IPUB_FIXES} // -------------------------------------------------------
  {$IF not defined(ANDROID) and not defined(iOS)}
  if Assigned(FHScrollBar) then
    FHScrollBar.Visible := False;
  if Assigned(FVScrollBar) then
    FVScrollBar.Visible := False;
  {$ENDIF}
  {$ENDIF} // ------------------------------------------------------------------
end;

{*********************************************************************************************}
procedure TALCustomScrollBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  internalMouseDown(Button, Shift, X, Y);
end;

{***********************************************************************}
procedure TALCustomScrollBox.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  internalMouseMove(Shift, X, Y);
end;

{*******************************************************************************************}
procedure TALCustomScrollBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  internalMouseUp(Button, Shift, X, Y);
end;

{****************************************}
procedure TALCustomScrollBox.DoMouseLeave;
begin
  inherited;
  internalMouseLeave;
end;

{**}
Type
  _TALControlAccessProtected = class(Tcontrol);

// -----------------------------------------------------------------------------
// [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
// Fixing to avoid the scrollbox to movement when the user click in one
// ITextInput control inside the scrollbox, like TEdit
{$IFDEF IPUB_FIXES} // ---------------------------------------------------------
function CheckControlInParentTree(AControl, AParent: TControl): Boolean;
begin
  Result := False;
  while Assigned(AControl) do
  begin
    if AControl.Parent = AParent then
      Exit(True);
    if AControl.Parent is TControl then
      AControl := TControl(AControl.Parent)
    else
      Break;
  end;
end;
{$ENDIF} // --------------------------------------------------------------------

{*************}
// -----------------------------------------------------------------------------
// [Stephane Vander Clock - github.com/Zeus64] - 28/05/2020 - Delphi 10.4
// https://quality.embarcadero.com/browse/RSP-24397
{$IFDEF DELPHI_FIXES} // -------------------------------------------------------
procedure TALCustomScrollBox.ChildrenMouseDown(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var P: Tpointf;
begin
  // ---------------------------------------------------------------------------
  // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
  // Fixing to avoid the scrollbox to movement when the user click in one
  // ITextInput control inside the scrollbox, like TEdit
  {$IFDEF IPUB_FIXES} // -------------------------------------------------------
  if (Assigned(Root) and Assigned(Root.Captured) and (TControl(Root.Captured) <> AObject)) or
    Supports(AObject, ITextInput) or
    (Assigned(Root) and Supports(Root.Focused, ICaret) and
      ((TObject(Root.Captured) = TObject(Root.Focused)) or
        CheckControlInParentTree(AObject, TControl(Root.Focused)))) then
  begin
    Exit;
  end;
  {$ENDIF} // ------------------------------------------------------------------
  if not aObject.AutoCapture then _TALControlAccessProtected(aObject).capture;
  P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
  internalMouseDown(Button, Shift, P.X, P.Y);
  inherited;
end;
{$ENDIF} // --------------------------------------------------------------------

{*************}
// -----------------------------------------------------------------------------
// [Stephane Vander Clock - github.com/Zeus64] - 28/05/2020 - Delphi 10.4
// https://quality.embarcadero.com/browse/RSP-24397
{$IFDEF DELPHI_FIXES} // -------------------------------------------------------
procedure TALCustomScrollBox.ChildrenMouseMove(const AObject: TControl; Shift: TShiftState; X, Y: Single);
var P: Tpointf;
begin
  // ---------------------------------------------------------------------------
  // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
  // Fixing to avoid the scrollbox to movement when the user click in one
  // ITextInput control inside the scrollbox, like TEdit
  {$IFDEF IPUB_FIXES} // -------------------------------------------------------
  if (Assigned(Root) and Assigned(Root.Captured) and (TControl(Root.Captured) <> AObject)) or
    Supports(AObject, ITextInput) or
    (Assigned(Root) and Supports(Root.Focused, ICaret) and
      ((TObject(Root.Captured) = TObject(Root.Focused)) or
        CheckControlInParentTree(AObject, TControl(Root.Focused)))) then
  begin
    Exit;
  end;
  {$ENDIF} // ------------------------------------------------------------------
  P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
  internalMouseMove(Shift, P.X, P.Y);
  inherited;
end;
{$ENDIF} // --------------------------------------------------------------------

{*************}
// -----------------------------------------------------------------------------
// [Stephane Vander Clock - github.com/Zeus64] - 28/05/2020 - Delphi 10.4
// https://quality.embarcadero.com/browse/RSP-24397
{$IFDEF DELPHI_FIXES} // -------------------------------------------------------
procedure TALCustomScrollBox.ChildrenMouseUp(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var P: Tpointf;
begin
  // ---------------------------------------------------------------------------
  // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
  // Fixing to avoid the scrollbox to movement when the user click in one
  // ITextInput control inside the scrollbox, like TEdit
  {$IFDEF IPUB_FIXES} // -------------------------------------------------------
  if (Assigned(Root) and Assigned(Root.Captured) and (TControl(Root.Captured) <> AObject)) or
    Supports(AObject, ITextInput) or
    (Assigned(Root) and Supports(Root.Focused, ICaret) and
      ((TObject(Root.Captured) = TObject(Root.Focused)) or
        CheckControlInParentTree(AObject, TControl(Root.Focused)))) then
  begin
    Exit;
  end;
  {$ENDIF} // ------------------------------------------------------------------
  if not aObject.AutoCapture then _TALControlAccessProtected(aObject).releasecapture;
  P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
  internalMouseUp(Button, Shift, P.X, P.Y);
  inherited;
end;
{$ENDIF} // --------------------------------------------------------------------

{*************}
// -----------------------------------------------------------------------------
// [Stephane Vander Clock - github.com/Zeus64] - 28/05/2020 - Delphi 10.4
// https://quality.embarcadero.com/browse/RSP-24397
{$IFDEF DELPHI_FIXES} // -------------------------------------------------------
procedure TALCustomScrollBox.ChildrenMouseLeave(const AObject: TControl);
begin
  internalMouseLeave;
  inherited;
end;
{$ENDIF} // --------------------------------------------------------------------

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
  // ---------------------------------------------------------------------------
  // [iPub - github.com/viniciusfbb] - 06/08/2021 - Delphi 10.4
  // Improving the compatibility with TCustomScrollBox. This is important 
  // because in a custom scrollbox we can easily change the parent class of 
  // TScrollBox to TALScrollBox
  {$IFDEF IPUB_FIXES} // -------------------------------------------------------
  if IsAddToContent(AObject) then
  begin
    FContent.AddObject(AObject);
    Realign;
  end
  {$ELSE} // -------------------------------------------------------------------
  if (FContent <> nil) and
     (AObject <> FContent) and
     (not (AObject is TEffect)) and
     (not (AObject is TAnimation)) and
     (not (AObject is TALScrollBoxBar)) then FContent.AddObject(AObject)
  {$ENDIF} // ------------------------------------------------------------------
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


