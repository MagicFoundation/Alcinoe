unit ALFmxLayouts;

//unfortunatly because emb decide to forbid access to private member via class helper
//https://quality.embarcadero.com/browse/RSP-15273
{$IF CompilerVersion > 31}
  {$MESSAGE WARN 'Check if FMX.Layouts.pas was not updated from the version in delphi berlin 10.1 and adjust the IFDEF'}
{$ENDIF}

interface

{$SCOPEDENUMS ON}

uses System.Classes,
     System.Types,
     System.UITypes,
     System.SysUtils,
     System.Math.Vectors,
     System.Messaging,
     FMX.Types,
     FMX.StdCtrls,
     FMX.Platform,
     FMX.Controls,
     ALFmxInertialMovement;

type

  {*************************}
  TALCustomScrollBox = class;

  {********************************}
  TALScrollContent = class(TContent)
  private
    [weak] FScrollBox: TALCustomScrollBox;
    FIsContentChanged: Boolean;
  protected
    function GetClipRect: TRectF; override;
    function GetChildrenRect: TRectF; override;
    function ObjectAtPoint(P: TPointF): IControl; override;
    function DoGetUpdateRect: TRectF; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    procedure DoRealign; override;
    procedure ContentChanged; override;
    /// <summary> This flag is set in the method ContentChanged. Used to optimize ScrollBox </summary>
    property IsContentChanged: Boolean read FIsContentChanged write FIsContentChanged;
  public
    constructor Create(AOwner: TComponent); override;
    property ScrollBox: TALCustomScrollBox read FScrollBox;
    function PointInObjectLocal(X, Y: Single): Boolean; override;
  end;

  {************************************************}
  TALScrollCalculations = class (TALAniCalculations)
  private
    [Weak] FScrollBox: TALCustomScrollBox;
  protected
    procedure DoChanged; override;
    procedure DoStart; override;
    procedure DoStop; override;
  public
    constructor Create(AOwner: TPersistent); override;
    property ScrollBox: TALCustomScrollBox read FScrollBox;
  end;

  {**************************************************}
  TALPositionChangeEvent = procedure (Sender: TObject;
                                      const OldViewportPosition, NewViewportPosition: TPointF;
                                      const ContentSizeChanged: Boolean) of object;

  {*******************************************************}
  TALOnCalcContentBoundsEvent = procedure (Sender: TObject;
                                           var ContentBounds: TRectF) of object;

  {****************************************}
  TALCustomScrollBox = class(TStyledControl)
  private
  const
    SmallChangeFraction = 5;
    DesignBorderColor: TAlphaColor = $80A070A0;
  type
    TScrollInfo = record
      [Weak] Scroll: TScrollBar;
      Align: TAlignLayout;
      Margins: TRectF;
    end;
  var
    FSystemInfoSrv: IFMXSystemInformationService;
    FDisableMouseWheel: Boolean;
    //-----
    FAniCalculations: TALScrollCalculations;
    FLastViewportPosition: TPointF;
    FInInternalAlign: Boolean;
    //-----
    FBackground: TControl;
    FContent: TALScrollContent;
    FContentLayout: TControl;
    FContentBounds: TRectF;
    FCachedContentSize: TSizeF;
    FCachedAutoShowing: Boolean;
    FOriginalContentLayoutSize: TSizeF;
    //-----
    FShowScrollBars: Boolean;
    FAutoHide: Boolean;
    FHScrollInfo: array of TScrollInfo;
    FVScrollInfo: array of TScrollInfo;
    FContentMargins: TRectF;
    FVDisablePaint: Boolean;
    FHDisablePaint: Boolean;
    FGDisablePaint: Boolean;
    //-----
    FSizeGripContent: TControl;
    FSizeGripParent: TControl;
    FSizeGrip: TControl;
    FShowSizeGrip: Boolean;
    FOnViewportPositionChange: TALPositionChangeEvent;
    FOnHScrollChange: TNotifyEvent;
    FOnVScrollChange: TNotifyEvent;
    FOnCalcContentBounds: TALOnCalcContentBoundsEvent;
    FMouseEvents: Boolean;
    FContentCalculated: Boolean;
    //-----
    fMouseDownPos: TpointF;
    FDeadZoneBeforeAcquireScrolling: Integer;
    fScrollingAcquiredByMe: boolean;
    fScrollingAcquiredByOtherMessageID: integer;
    procedure ScrollingAcquiredByOtherHandler(const Sender: TObject; const M: TMessage);
    //-----
    function HScrollIndex: Integer;
    function VScrollIndex: Integer;
    function GetHScrollAlign: TAlignLayout;
    function GetVScrollAlign: TAlignLayout;
    function GetHScrollMargins: TRectF;
    function GetVScrollMargins: TRectF;
    function GetSceneScale: Single;
    procedure SetShowScrollBars(const Value: Boolean);
    procedure SetShowSizeGrip(const Value: Boolean);
    function GetVScrollBar: TScrollBar;
    function GetHScrollBar: TScrollBar;
    procedure UpdateSizeGrip;
    procedure UpdateVScrollBar(const Value: Single; const ViewportSize: Single);
    procedure UpdateHScrollBar(const Value: Single; const ViewportSize: Single);
    procedure InternalAlign;
    procedure HScrollChangeProc(Sender: TObject);
    procedure VScrollChangeProc(Sender: TObject);
    procedure MousePosToAni(var X, Y: Single);
    procedure SetAutoHide(const Value: Boolean);
    procedure SaveDisablePaint;
    procedure RestoreDisablePaint;
    procedure SetDisablePaint;
    function GetViewportPosition: TPointF;
    procedure SetViewportPosition(const Value: TPointF);
    procedure StartScrolling;
    procedure StopScrolling;
    procedure UpdateOriginalContentLayoutSize(const Force: Boolean);
    procedure ReadPartSize(Reader: TReader; var Size: Single);
    procedure ReadContentLayoutHeight(Reader: TReader);
    procedure ReadContentLayoutWidth(Reader: TReader);
    procedure WriteContentLayoutHeight(Writer: TWriter);
    procedure WriteContentLayoutWidth(Writer: TWriter);
  protected
    //Animation mouse events
    procedure AniMouseDown(const Touch: Boolean; const X, Y: Single); virtual;
    procedure AniMouseMove(const Touch: Boolean; const X, Y: Single); virtual;
    procedure AniMouseUp(const Touch: Boolean; const X, Y: Single); virtual;
    //Animation mouse events
    function GetScrollingBehaviours: TScrollingBehaviours;
    procedure Loaded; override;
    procedure PaddingChanged; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRealign; override;
    function IsAddToContent(const AObject: TFmxObject): Boolean; virtual;
    procedure ContentAddObject(const AObject: TFmxObject); virtual;
    procedure ContentInsertObject(Index: Integer; const AObject: TFmxObject); virtual;
    procedure ContentBeforeRemoveObject(AObject: TFmxObject); virtual;
    procedure ContentRemoveObject(const AObject: TFmxObject); virtual;
    procedure HScrollChange; virtual;
    procedure VScrollChange; virtual;
    procedure ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF;
                                     const ContentSizeChanged: boolean); virtual;
    procedure CMGesture(var EventInfo: TGestureEventInfo); override;
    procedure Painting; override;
    procedure AfterPaint; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    function IsOpaque: Boolean; virtual;
    function ContentRect: TRectF;
    function VScrollBarValue: Single;
    function HScrollBarValue: Single;
    function CreateScrollContent: TALScrollContent; virtual;
    function CreateAniCalculations: TALScrollCalculations; virtual;
    procedure DoUpdateAniCalculations(const AAniCalculations: TALScrollCalculations); virtual;
    procedure UpdateAniCalculations;
    function DoCalcContentBounds: TRectF; virtual;
    procedure DoRealignContent(R: TRectF); virtual;
    function GetContentBounds: TRectF;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    property ContentLayout: TControl read FContentLayout;
    property Content: TALScrollContent read FContent;
    property HScrollAlign: TAlignLayout read GetHScrollAlign;
    property VScrollAlign: TAlignLayout read GetVScrollAlign;
    property HScrollMargins: TRectF read GetHScrollMargins;
    property VScrollMargins: TRectF read GetVScrollMargins;
    property InInternalAlign: Boolean read FInInternalAlign;
    property HScrollBar: TScrollBar read GetHScrollBar;
    property VScrollBar: TScrollBar read GetVScrollBar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AniCalculations: TALScrollCalculations read FAniCalculations;
    property ViewportPosition: TPointF read GetViewportPosition write SetViewportPosition;
    procedure Sort(Compare: TFmxObjectSortCompare); override;
    procedure Center;
    procedure ScrollTo(const Dx, Dy: Single); deprecated 'use ScrollBy(const Dx, Dy: Single)';
    procedure ScrollBy(const Dx, Dy: Single);
    procedure InViewRect(const Rect: TRectF);
    function ClientWidth: Single;
    function ClientHeight: Single;
    function GetTabList: ITabList; override;
    property ContentBounds: TRectF read GetContentBounds;
    procedure InvalidateContentSize;
    procedure RealignContent;
    property AutoHide: Boolean read FAutoHide write SetAutoHide default True;
    property DisableMouseWheel: Boolean read FDisableMouseWheel write FDisableMouseWheel default False;
    property ShowScrollBars: Boolean read FShowScrollBars write SetShowScrollBars default True;
    property ShowSizeGrip: Boolean read FShowSizeGrip write SetShowSizeGrip default False;
    property OnViewportPositionChange: TALPositionChangeEvent read FOnViewportPositionChange write FOnViewportPositionChange;
    property OnHScrollChange: TNotifyEvent read FOnHScrollChange write FOnHScrollChange;
    property OnVScrollChange: TNotifyEvent read FOnVScrollChange write FOnVScrollChange;
    property OnCalcContentBounds: TALOnCalcContentBoundsEvent read FOnCalcContentBounds write FOnCalcContentBounds;
    property DeadZoneBeforeAcquireScrolling: Integer read FDeadZoneBeforeAcquireScrolling write FDeadZoneBeforeAcquireScrolling default ALDefaultDeadZoneBeforeAcquireScrolling;
  published
    property Align;
    property Anchors;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Locked;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property HitTest;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property StyleLookup;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    { Events }
    property OnApplyStyleLookup;
    property OnPainting;
    property OnPaint;
    property OnResize;
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
  end;

  {**************************************}
  TALScrollBox = class(TALCustomScrollBox)
  protected
    procedure Paint; override;
  public
    property Content;
  published
    property Align;
    property Anchors;
    property AutoHide;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DisableMouseWheel;
    property DragMode;
    property Enabled;
    property EnableDragHighlight;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
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
    property ShowSizeGrip;
    property Size;
    property StyleLookup;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    { Events }
    property OnApplyStyleLookup;
    property OnPainting;
    property OnPaint;
    property OnResize;
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
    property OnHScrollChange;
    property OnVScrollChange;
    property OnCalcContentBounds;
  end;

  {******************************************}
  TALVertScrollBox = class(TALCustomScrollBox)
  protected
    function GetDefaultStyleLookupName: string; override;
    function DoCalcContentBounds: TRectF; override;
    procedure Paint; override;
    procedure DoUpdateAniCalculations(const AAniCalculations: TALScrollCalculations); override;
  public
    property Content;
  published
    property Align;
    property Anchors;
    property AutoHide;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DisableMouseWheel;
    property DragMode;
    property Enabled;
    property EnableDragHighlight;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
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
    property ShowSizeGrip;
    property Size;
    property StyleLookup;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    { Events }
    property OnApplyStyleLookup;
    property OnPainting;
    property OnPaint;
    property OnResize;
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
    property OnVScrollChange;
    property OnCalcContentBounds;
  end;

  {******************************************}
  TALHorzScrollBox = class(TALCustomScrollBox)
  protected
    function GetDefaultStyleLookupName: string; override;
    function DoCalcContentBounds: TRectF; override;
    procedure Paint; override;
    procedure DoUpdateAniCalculations(const AAniCalculations: TALScrollCalculations); override;
  public
    property Content;
  published
    property Align;
    property Anchors;
    property AutoHide;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DisableMouseWheel;
    property DragMode;
    property Enabled;
    property EnableDragHighlight;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
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
    property ShowSizeGrip;
    property Size;
    property StyleLookup;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    { Events }
    property OnApplyStyleLookup;
    property OnPainting;
    property OnPaint;
    property OnResize;
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
    property OnHScrollChange;
    property OnCalcContentBounds;
  end;

procedure Register;

implementation

uses System.Generics.Collections,
     System.Math,
     System.RTLConsts,
     System.TypInfo,
     FMX.Styles,
     FMX.Consts,
     FMX.Effects,
     FMX.Ani,
     FMX.Graphics;

{ TScrollContent }

{******************************************************}
constructor TALScrollContent.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TALCustomScrollBox then
    FScrollBox := TALCustomScrollBox(AOwner);
  ClipChildren := True;
  FIsContentChanged := True;
  SetAcceptsControls(False);
end;

{********************************************}
function TALScrollContent.GetClipRect: TRectF;
begin
  if FScrollBox <> nil then
    if FScrollBox.ContentLayout <> nil then
    begin
      Result := FScrollBox.ContentLayout.AbsoluteRect;
      Result.TopLeft := AbsoluteToLocal(Result.TopLeft);
      Result.BottomRight := AbsoluteToLocal(Result.BottomRight);
    end
    else
    begin
      Result := TRectF.Create(0, 0, FScrollBox.Width, FScrollBox.Height);
      Result.Offset(FScrollBox.ViewportPosition);
    end
  else
    Result := inherited GetClipRect;
end;

{************************************************************}
function TALScrollContent.ObjectAtPoint(P: TPointF): IControl;
begin
  Result := inherited ObjectAtPoint(P);
  if Result <> nil then
  begin
    if FScene <> nil then
      P := FScene.ScreenToLocal(P);
    P := AbsoluteToLocal(P);
    if not ClipRect.Contains(P) then
      Result := nil;
  end;
end;

{******************************************************************}
function TALScrollContent.PointInObjectLocal(X, Y: Single): Boolean;
var
  ClipRect: TRectF;
begin
  ClipRect := GetClipRect;
  Result := (X >= (ClipRect.TopLeft.X - TouchTargetExpansion.Left)) and
    (X <= (ClipRect.TopLeft.X + ClipRect.Width + TouchTargetExpansion.Right)) and
    (Y >= (ClipRect.TopLeft.Y + TouchTargetExpansion.Top)) and
    (Y <= (ClipRect.TopLeft.Y + ClipRect.Height + TouchTargetExpansion.Bottom));
end;

{****************************************************************}
procedure TALScrollContent.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  if FScrollBox <> nil then
    FScrollBox.ContentAddObject(AObject);
end;

{***********************************************************************************}
procedure TALScrollContent.DoInsertObject(Index: Integer; const AObject: TFmxObject);
begin
  inherited;
  if FScrollBox <> nil then
    FScrollBox.ContentInsertObject(Index, AObject);
end;

{***********************************}
procedure TALScrollContent.DoRealign;
begin
  if (ScrollBox <> nil) and ScrollBox.FContentCalculated then
    inherited;
  FLastWidth := Width;
  FLastHeight := Height;
end;

{*******************************************************************}
procedure TALScrollContent.DoRemoveObject(const AObject: TFmxObject);
begin
  if FScrollBox <> nil then
    FScrollBox.ContentBeforeRemoveObject(AObject);
  inherited;
  if FScrollBox <> nil then
    FScrollBox.ContentRemoveObject(AObject);
end;

{************************************************}
function TALScrollContent.DoGetUpdateRect: TRectF;
begin
  if ParentControl is TALCustomScrollBox then
  begin
    if TALCustomScrollBox(ParentControl).ContentLayout <> nil then
      Result := TALCustomScrollBox(ParentControl).ContentLayout.UpdateRect
    else
      Result := TALCustomScrollBox(ParentControl).UpdateRect;
  end
  else
    Result := inherited DoGetUpdateRect;
end;

{************************************************}
function TALScrollContent.GetChildrenRect: TRectF;
begin
  Result := GetUpdateRect;
end;

{****************************************}
procedure TALScrollContent.ContentChanged;
begin
  inherited;
  if (FScrollBox <> nil) and not FScrollBox.Released and ([csLoading, csDestroying] * FScrollBox.ComponentState = [])
    and not ScrollBox.InInternalAlign then
  begin
    FIsContentChanged := True;
    FScrollBox.InvalidateContentSize;
    if not IsUpdating then
      FScrollBox.Realign;
  end;
end;

{ TScrollCalculations }

{************************************************************}
constructor TALScrollCalculations.Create(AOwner: TPersistent);
begin
  if not (AOwner is TALCustomScrollBox) then
    raise EArgumentException.Create(sArgumentInvalid);
  inherited Create(AOwner);
  FScrollBox := TALCustomScrollBox(AOwner);
end;

{****************************************}
procedure TALScrollCalculations.DoChanged;
begin
  if (FScrollBox <> nil) and not (csDestroying in FScrollBox.ComponentState) then
    FScrollBox.InternalAlign;
  inherited;
end;

{**************************************}
procedure TALScrollCalculations.DoStart;
begin
  inherited;
  if (FScrollBox <> nil) and not (csDestroying in FScrollBox.ComponentState) then
    FScrollBox.StartScrolling;
end;

{*************************************}
procedure TALScrollCalculations.DoStop;
begin
  inherited;
  if (FScrollBox <> nil) and not (csDestroying in FScrollBox.ComponentState) then
    FScrollBox.StopScrolling;
end;

{ TCustomScrollBox }

type
  TALOpenControl = class (TControl);

{********************************************************}
constructor TALCustomScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DisableDisappear := True;
  FOriginalContentLayoutSize := TSizeF.Create(-1, -1);
  SetAcceptsControls(True);
  SetLength(FHScrollInfo, 2);
  SetLength(FVScrollInfo, 2);
  SupportsPlatformService(IFMXSystemInformationService, FSystemInfoSrv);
  AutoCapture := True;
  FAutoHide := True;
  FShowScrollBars := True;
  FContent := CreateScrollContent;
  FContent.Parent := Self;
  FContent.Stored := False;
  FContent.Locked := True;
  FContent.HitTest := False;
  UpdateAniCalculations;
  Touch.DefaultInteractiveGestures := Touch.DefaultInteractiveGestures + [TInteractiveGesture.Pan];
  Touch.InteractiveGestures := Touch.InteractiveGestures + [TInteractiveGesture.Pan];
  //-----
  fMouseDownPos := TpointF.Create(0,0);
  FDeadZoneBeforeAcquireScrolling := ALDefaultDeadZoneBeforeAcquireScrolling;
  fScrollingAcquiredByMe := False;
  fScrollingAcquiredByOtherMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TALScrollingAcquiredMessage, ScrollingAcquiredByOtherHandler);
end;

{************************************}
destructor TALCustomScrollBox.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TALScrollingAcquiredMessage, fScrollingAcquiredByOtherMessageID);
  FContent := nil;
  FreeAndNil(FAniCalculations);
  inherited;
end;

{***********************************************************************}
function TALCustomScrollBox.CreateAniCalculations: TALScrollCalculations;
begin
  Result := TALScrollCalculations.Create(self);
end;

{**************************************************************************************************}
procedure TALCustomScrollBox.DoUpdateAniCalculations(const AAniCalculations: TALScrollCalculations);
begin
  AAniCalculations.Animation := TScrollingBehaviour.Animation in GetScrollingBehaviours;
  if TScrollingBehaviour.TouchTracking in GetScrollingBehaviours then
    AAniCalculations.TouchTracking := [ttVertical, ttHorizontal]
  else
    AAniCalculations.TouchTracking := [];
  AAniCalculations.BoundsAnimation := TScrollingBehaviour.BoundsAnimation in GetScrollingBehaviours;
  AAniCalculations.AutoShowing := TScrollingBehaviour.AutoShowing in GetScrollingBehaviours;
end;

{*************************************************}
procedure TALCustomScrollBox.UpdateAniCalculations;
begin
  if not (csDestroying in ComponentState) then
  begin
    if FAniCalculations = nil then
      FAniCalculations := CreateAniCalculations;
    FAniCalculations.BeginUpdate;
    try
      DoUpdateAniCalculations(FAniCalculations);
    finally
      FAniCalculations.EndUpdate;
    end;
  end;
end;

{************************************************}
function TALCustomScrollBox.GetSceneScale: Single;
begin
  Result := 0;
  if Scene <> nil then
    Result := Scene.GetSceneScale;
  if Result <= 0 then
    Result := 1;
end;

{******************************************}
procedure TALCustomScrollBox.UpdateSizeGrip;
var
  R, GripRect: TRectF;
  GripLeft, GripTop, SizeGripVisible, Both: Boolean;
begin
  SizeGripVisible := ShowSizeGrip and (FSizeGrip <> nil) and (FSizeGripParent <> nil) and (FSizeGripContent <> nil);
  Both := ((VScrollBar <> nil) and VScrollBar.Visible) and ((HScrollBar <> nil) and HScrollBar.Visible);
  if Both or (SizeGripVisible and (((VScrollBar <> nil) and VScrollBar.Visible) or
    ((HScrollBar <> nil) and HScrollBar.Visible))) then
  begin
    {$REGION 'size'}
    GripRect := TRectF.Empty;
    if HScrollBar <> nil then
      GripRect.Height := HScrollBar.Height
    else
      GripRect.Height := VScrollBar.Width;
    if VScrollBar <> nil then
      GripRect.Width := VScrollBar.Width
    else
      GripRect.Width := HScrollBar.Height;
    {$ENDREGION}
    {$REGION 'vert align'}
    GripTop := (HScrollBar <> nil) and (not (HScrollAlign in [TAlignLayout.Bottom, TAlignLayout.MostBottom]));
    if FSizeGrip <> nil then
    begin
      if GripTop then
        FSizeGrip.Align := TAlignLayout.Top
      else
        FSizeGrip.Align := TAlignLayout.Bottom;
      FSizeGrip.BoundsRect := GripRect;
    end;
    {$ENDREGION}
    {$REGION 'horz align'}
    GripLeft := (VScrollBar <> nil) and (not (VScrollAlign in [TAlignLayout.Right, TAlignLayout.MostRight]));
    if FSizeGripContent <> nil then
      FSizeGripContent.Align := TAlignLayout.Contents;
    if FSizeGripParent <> nil then
    begin
      if GripLeft then
        FSizeGripParent.Align := TAlignLayout.Left
      else
        FSizeGripParent.Align := TAlignLayout.Right;
      FSizeGripParent.Width := GripRect.Width;
    end;
    {$ENDREGION}
    if (HScrollBar <> nil) and HScrollBar.Visible then
    begin
      R := HScrollMargins;
      if GripLeft then
        R.Left := R.Left + GripRect.Width
      else
        R.Right := R.Right + GripRect.Width;
      HScrollBar.Margins.Rect := R;
      if VScrollBar <> nil then
        VScrollBar.Margins.Rect := VScrollMargins;
    end
    else if (VScrollBar <> nil) and VScrollBar.Visible then
    begin
      R := VScrollMargins;
      if GripTop then
        R.Top := R.Top + GripRect.Height
      else
        R.Bottom := R.Bottom + GripRect.Height;
      VScrollBar.Margins.Rect := R;
    end;
    if FSizeGripParent <> nil then
      FSizeGripParent.Visible := True;
    if FSizeGripContent <> nil then
      FSizeGripContent.Visible := True;
    if FSizeGrip <> nil then
    begin
      FSizeGrip.Opacity := AniCalculations.Opacity;
      FSizeGrip.Visible := SizeGripVisible;
      FSizeGrip.Enabled := (Align in [TAlignLayout.Client, TAlignLayout.Contents]) and (not GripTop) and (not GripLeft);
    end;
  end
  else
  begin
    if FSizeGrip <> nil then
      FSizeGrip.Visible := False;
    if FSizeGripContent <> nil then
      FSizeGrip.Visible := False;
    if (VScrollBar <> nil) and (VScrollBar.Margins <> nil) then
      VScrollBar.Margins.Rect := VScrollMargins;
    if (HScrollBar <> nil) and (HScrollBar.Margins <> nil) then
      HScrollBar.Margins.Rect := HScrollMargins;
  end;
end;

{*********************************************************************************************}
procedure TALCustomScrollBox.UpdateVScrollBar(const Value: Single; const ViewportSize: Single);
begin
  if VScrollBar <> nil then
  begin
    VScrollBar.ValueRange.BeginUpdate;
    try
      VScrollBar.ValueRange.Min := Min(Value, FContentBounds.Top);
      VScrollBar.ValueRange.Max := Max(Value + ViewportSize, FContentBounds.Bottom);
      VScrollBar.ValueRange.ViewportSize := ViewportSize;
      VScrollBar.Value := Value;
    finally
      VScrollBar.ValueRange.EndUpdate;
    end;
    VScrollBar.SmallChange := VScrollBar.ViewportSize / SmallChangeFraction;
  end;
end;

{*********************************************************************************************}
procedure TALCustomScrollBox.UpdateHScrollBar(const Value: Single; const ViewportSize: Single);
begin
  if HScrollBar <> nil then
  begin
    HScrollBar.ValueRange.BeginUpdate;
    try
      HScrollBar.ValueRange.Min := Min(Value, FContentBounds.Left);
      HScrollBar.ValueRange.Max := Max(Value + ViewportSize, FContentBounds.Right);
      HScrollBar.ValueRange.ViewportSize := ViewportSize;
      HScrollBar.Value := Value;
    finally
      HScrollBar.ValueRange.EndUpdate;
    end;
    HScrollBar.SmallChange := HScrollBar.ViewportSize / SmallChangeFraction;
  end;
end;

{*****************************************}
procedure TALCustomScrollBox.InternalAlign;
const
  MaxAlignIterations = 5;

  procedure UpdateScrollbarVisibility(const ScrollBar: TScrollBar; const OverBounds, Reset: Boolean);
  begin
    ScrollBar.Opacity := AniCalculations.Opacity;
    ScrollBar.Enabled := OverBounds or AniCalculations.AutoShowing;
    ScrollBar.Visible := FShowScrollBars and (((not Reset or AniCalculations.AutoShowing) and OverBounds) or
      not FAutoHide) and (AniCalculations.Opacity > TEpsilon.Position);
  end;

  procedure UpdateScrollbarsVisibility(const AContentRect: TRectF; const Reset: Boolean);
  begin
    if FVScrollInfo[Integer(not AniCalculations.AutoShowing)].Scroll <> nil then
      FVScrollInfo[Integer(not AniCalculations.AutoShowing)].Scroll.Visible := False;
    if FHScrollInfo[Integer(not AniCalculations.AutoShowing)].Scroll <> nil then
      FHScrollInfo[Integer(not AniCalculations.AutoShowing)].Scroll.Visible := False;
    if VScrollBar <> nil then
      UpdateScrollbarVisibility(VScrollBar, FContentBounds.Height > AContentRect.Height, Reset);
    if HScrollBar <> nil then
      UpdateScrollbarVisibility(HScrollBar, FContentBounds.Width > AContentRect.Width, Reset);
  end;

  procedure UpdateContentLayoutMargins;
  begin
    ContentLayout.Margins.Rect := FContentMargins;
    if (ContentLayout.Align = TAlignLayout.Contents) and (FBackground <> nil) then
      ContentLayout.Margins.Rect.Inflate(-FBackground.Padding.Left, -FBackground.Padding.Top,
        FBackground.Padding.Right, FBackground.Padding.Bottom);
  end;

  function CalcContentLayoutRect(const Reset: Boolean): TRectF;
  begin
    Result := TRectF.Create(ContentLayout.Position.Point, ContentLayout.Width, ContentLayout.Height);
    UpdateScrollbarsVisibility(Result, Reset);
    Result.TopLeft := Result.TopLeft - ViewportPosition;
    if FDisableAlign and (FBackground <> nil) then
      TALOpenControl(FBackground).Realign;
    Result.Width := ContentLayout.Width;
    Result.Height := ContentLayout.Height;
  end;

  procedure UpdateAnimationTargets(const ContentLayoutRect: TRectF);
  var
    I, J: Integer;
    LTargets: array of TALAniCalculations.TTarget;
    NewTargets: array of TALAniCalculations.TTarget;
  begin
    SetLength(LTargets, AniCalculations.TargetCount);
    AniCalculations.GetTargets(LTargets);
    SetLength(NewTargets, 2);
    NewTargets[0].TargetType := TALAniCalculations.TTargetType.Min;
    NewTargets[0].Point := FContentBounds.TopLeft;
    NewTargets[1].TargetType := TALAniCalculations.TTargetType.Max;
    NewTargets[1].Point := TALPointD.Create(Max(FContentBounds.Left, FContentBounds.Right - ContentLayoutRect.Width),
      Max(FContentBounds.Top, FContentBounds.Bottom - ContentLayoutRect.Height));
    for I := 0 to Length(LTargets) - 1 do
      if not (LTargets[I].TargetType in [TALAniCalculations.TTargetType.Min, TALAniCalculations.TTargetType.Max]) then
      begin
        J := Length(NewTargets);
        SetLength(NewTargets, J + 1);
        NewTargets[J].TargetType := LTargets[I].TargetType;
        NewTargets[J].Point := LTargets[I].Point;
      end;
    AniCalculations.SetTargets(NewTargets);
  end;

  procedure AssignContentBounds(NewContentBounds: TRectF);
  begin
    if Assigned(OnCalcContentBounds) then
      OnCalcContentBounds(Self, NewContentBounds);
    FContentBounds := NewContentBounds;
  end;

  function TakeControl(I: Integer; var AControl: TControl): Boolean;
  begin
    AControl := FContent.Controls[I];
    Result := (AControl.Align = TAlignLayout.None) and
      ([TAnchorKind.akRight, TAnchorKind.akBottom] * AControl.Anchors <> []);
  end;

  procedure SaveControlRects(const ControlList: TDictionary<TControl, TRectF>);
  var
    I: Integer;
    Ctrl: TControl;
  begin
    if (FOriginalContentLayoutSize.cx >= 0) and (FOriginalContentLayoutSize.cy >= 0) then
      for I := 0 to FContent.ControlsCount - 1 do
        if TakeControl(I, Ctrl) then
          ControlList.Add(Ctrl, Ctrl.BoundsRect);
    if ControlList.Count = 0 then
      FContentCalculated := True;
  end;

  procedure RestoreControlRects(const ControlList: TDictionary<TControl, TRectF>);
  var
    I: Integer;
    R: TRectF;
    Ctrl: TControl;
    LParentSize: TSizeF;
    Dx, Dy: Single;
  begin
    if (ControlList <> nil) and (ControlList.Count > 0) then
    begin
      LParentSize := ContentLayout.BoundsRect.Size;
      Dx := LParentSize.cx - FOriginalContentLayoutSize.cx;
      Dy := LParentSize.cy - FOriginalContentLayoutSize.cy;
      for I := 0 to FContent.ControlsCount - 1 do
        if TakeControl(I, Ctrl) and ControlList.TryGetValue(Ctrl, R) then
        begin
          if TAnchorKind.akRight in Ctrl.Anchors then
            if TAnchorKind.akLeft in Ctrl.Anchors then
              R.Right := R.Right + Dx
            else
              R.Offset(Dx, 0);

          if TAnchorKind.akBottom in Ctrl.Anchors then
            if TAnchorKind.akTop in Ctrl.Anchors then
              R.Bottom := R.Bottom + Dy
            else
              R.Offset(0, Dy);

          Ctrl.BoundsRect := R;
          if FContentCalculated then
            TALOpenControl(Ctrl).UpdateAnchorRules(True);
        end;
      if not FContentCalculated then
      begin
        FContentCalculated := True;
        FContent.Realign;
      end;
    end;
  end;

  function FindEqual(const Sizes: array of TSizeF; const Index: Integer): Boolean;
  var
    I: Integer;
  begin
    for I := Index - 1 downto 0 do
      if TPointF(Sizes[Index]).EqualsTo(TPointF(Sizes[I]), TEpsilon.Position) then
        Exit(True);
    Result := False;
  end;

  function InternalContentRealigned(const ContentLayoutRect: TRectF; var ContentSizeChanged: Boolean): Boolean;
  begin
    Result := (FContent <> nil) and (FContent.IsContentChanged or
      not TPointF(FContent.BoundsRect.Size).EqualsTo(TPointF(ContentLayoutRect.Size), TEpsilon.Position));
    if Result then
    begin
      ContentSizeChanged := True;
      DoRealignContent(ContentLayoutRect);
      FContent.IsContentChanged := False;
    end
  end;

  function Adjust(var ContentLayoutRect: TRectF; var SizeAdjusted, ContentPosChanged,
    ContentSizeChanged: Boolean): Boolean;
  type
    TCalculationPhase = (OldAnchoredControls, NewAnchoredControls, Finish);
  var
    Sizes: array of TSizeF;
    AnchoredControlList: TDictionary<TControl, TRectF>;
    AlignCount: Integer;
    LEpsilon: Single;
    Step: TCalculationPhase;
    InvalidCachedContentSize: Boolean;
  begin
    Result := False;
    Step := Low(Step);
    SizeAdjusted := False;
    ContentLayoutRect := TRectF.Empty;
    ContentPosChanged := False;
    ContentSizeChanged := False;
    AnchoredControlList := nil;
    LEpsilon := 1 / Max(2, GetSceneScale * Max(GetAbsoluteScale.X, GetAbsoluteScale.Y));
    SetLength(Sizes, MaxAlignIterations + 1);
    try
      if not FContentCalculated then
      begin
        AnchoredControlList := TDictionary<TControl, TRectF>.Create;
        SaveControlRects(AnchoredControlList);
      end;
      while Step < TCalculationPhase.Finish do
      begin
        if (AnchoredControlList = nil) or (AnchoredControlList.Count = 0) then
          Step := Succ(Step);
        Sizes[0] := FCachedContentSize;
        InvalidCachedContentSize := FCachedContentSize.IsZero;
        for AlignCount := 0 to MaxAlignIterations - 1 do
        begin
          UpdateContentLayoutMargins;
          ContentLayoutRect := CalcContentLayoutRect(InvalidCachedContentSize and (AlignCount = 0));
          if AlignCount = 0 then
            InternalContentRealigned(ContentLayoutRect, ContentSizeChanged);
          Sizes[AlignCount + 1] := ContentLayoutRect.Size;
          if not FindEqual(Sizes, AlignCount + 1) then
            AssignContentBounds(DoCalcContentBounds)
          else
            Break;
        end;
        SizeAdjusted := not TPointF(FCachedContentSize).EqualsTo(TPointF(ContentLayoutRect.Size), TEpsilon.Position);
        if SizeAdjusted then
        begin
          FCachedContentSize := ContentLayoutRect.Size;
          UpdateAnimationTargets(ContentLayoutRect);
        end;
        if InternalContentRealigned(ContentLayoutRect, ContentSizeChanged) then
          Result := True
        else if SizeAdjusted or not FContent.Position.Point.EqualsTo(ContentLayoutRect.TopLeft, LEpsilon) then
        begin
          FContent.Position.Point := ContentLayoutRect.TopLeft;
          ContentPosChanged := True;
          Result := True;
        end
        else
          Break;
        RestoreControlRects(AnchoredControlList);
        Step := Succ(Step);
      end;
    finally
      FContentCalculated := True;
      AnchoredControlList.Free;
    end;
  end;

var
  LViewportPosition: TPointF;
  ContentLayoutRect: TRectF;
  SizeAdjusted, ContentPosChanged, ContentSizeChanged: Boolean;
  LScale: Single;
begin
  if (not FInInternalAlign) and (ContentLayout <> nil) and (Content <> nil) and (AniCalculations <> nil) then
  begin
    LScale := GetSceneScale;
    FInInternalAlign := True;
    try
      if (AniCalculations <> nil) and not Released then
      begin
        if (FCachedAutoShowing <> AniCalculations.AutoShowing) and not AniCalculations.AutoShowing then
          InvalidateContentSize;
        FCachedAutoShowing := AniCalculations.AutoShowing;
      end;
      if (not AniCalculations.Down) and AniCalculations.LowVelocity then
        AniCalculations.Shown := False;
      if not Adjust(ContentLayoutRect, SizeAdjusted, ContentPosChanged, ContentSizeChanged) then
        Exit;
      LViewportPosition := ViewportPosition;
      UpdateVScrollBar(LViewportPosition.Y, ContentLayoutRect.Height);
      UpdateHScrollBar(LViewportPosition.X, ContentLayoutRect.Width);
      UpdateSizeGrip;
      LViewportPosition := TPointF.Create((LViewportPosition * LScale).Round) / LScale;
      if ContentSizeChanged or ContentPosChanged or not FLastViewportPosition.EqualsTo(LViewportPosition,
        TEpsilon.Position) then
        try
          ViewportPositionChange(FLastViewportPosition, LViewportPosition, ContentSizeChanged or ContentPosChanged);
        finally
          FLastViewportPosition := LViewportPosition;
        end;
      if SizeAdjusted then
      begin
        if not (csDesigning in ComponentState) and not AniCalculations.Animation then
          AniCalculations.UpdatePosImmediately(True);
        Repaint;
      end;
    finally
      FInInternalAlign := False;
    end;
  end;
end;

{**************************************}
procedure TALCustomScrollBox.ApplyStyle;

  function CheckParent(Control: TControl): boolean;
  begin
    Result := (Control <> nil) and
              (Control.Parent <> FBackground) and
              (Control.Parent <> self) and
              (Control.Parent is TControl)
  end;

  procedure UpdateScroll(AStyleLookup: string;
                         var Info: array of TScrollInfo;
                         Small: Boolean;
                         Proc: TNotifyEvent);
  var
    ScrollBar: TScrollBar;
  begin
    if FindStyleResource<TScrollBar>(AStyleLookup, ScrollBar) then
    begin
      Info[Integer(Small)].Scroll := ScrollBar;
      Info[Integer(Small)].Scroll.OnChange := Proc;
      Info[Integer(Small)].Scroll.Visible := False;
      Info[Integer(Small)].Scroll.Locked := True;
      Info[Integer(Small)].Align := ScrollBar.Align;
      Info[Integer(Small)].Margins := ScrollBar.Margins.Rect;
    end
    else
    begin
      Info[Integer(Small)].Scroll := nil;
      Info[Integer(Small)].Align := TAlignLayout.None;
      Info[Integer(Small)].Margins := TRectF.Create(0, 0, 0, 0);
    end;
  end;
begin
  inherited;
  FindStyleResource<TControl>('background', FBackground);

  UpdateScroll('vscrollbar', FVScrollInfo, False, VScrollChangeProc);
  UpdateScroll('hscrollbar', FHScrollInfo, False, HScrollChangeProc);
  UpdateScroll('vsmallscrollbar', FVScrollInfo, True, VScrollChangeProc);
  UpdateScroll('hsmallscrollbar', FHScrollInfo, True, HScrollChangeProc);

  if FindStyleResource<TControl>('sizegrip', FSizeGrip) then
  begin
    FSizeGrip.Visible := False;
    FSizeGrip.Align := TAlignLayout.Bottom;

    if CheckParent(FSizeGrip) then
      FSizeGripParent := TControl(FSizeGrip.Parent);
    if CheckParent(FSizeGripParent) then
      FSizeGripContent := TControl(FSizeGripParent.Parent);

    if FSizeGripParent <> nil then
      FSizeGripParent.Align := TAlignLayout.Right;
    if FSizeGripContent <> nil then
    begin
      FSizeGripContent.Visible := False;
      FSizeGripParent.Align := TAlignLayout.Contents;
    end;
  end;

  if FindStyleResource<TControl>('content', FContentLayout) then
  begin
    FContentMargins := FContentLayout.Margins.Rect;
    FContentLayout.DisableDisappear := True;
  end;
  if ResourceControl <> nil then
    ResourceControl.DisableDisappear := True;
  RealignContent;
end;

{*************************************}
procedure TALCustomScrollBox.FreeStyle;
var
  I: Integer;
begin
  inherited;
  for I := Low(FHScrollInfo) to High(FHScrollInfo) do
  begin
    FHScrollInfo[I].Scroll := nil;
    FHScrollInfo[I].Margins := TRectF.Create(0, 0, 0, 0);
  end;
  for I := Low(FVScrollInfo) to High(FVScrollInfo) do
  begin
    FVScrollInfo[I].Scroll := nil;
    FVScrollInfo[I].Margins := TRectF.Create(0, 0, 0, 0);
  end;
  UpdateOriginalContentLayoutSize(True);
  FContentCalculated := False;
  FSizeGripParent := nil;
  FSizeGripContent := nil;
  FContentLayout := nil;
  FBackground := nil;
  FSizeGrip := nil;
end;

{******************************************************}
function TALCustomScrollBox.DoCalcContentBounds: TRectF;
const
  RightSide = [TAlignLayout.Top, TAlignLayout.MostTop, TAlignLayout.Bottom, TAlignLayout.MostBottom];
  BottomSide = [TAlignLayout.Left, TAlignLayout.MostLeft, TAlignLayout.Right, TAlignLayout.MostRight];
  NeedAddBottom = [TAlignLayout.Top, TAlignLayout.MostTop];
  NeedAddRight = [TAlignLayout.Left, TAlignLayout.MostLeft];
var
  I: Integer;
  R, LocalR: TRectF;
begin
  Result := TRectF.Create(0, 0, 0, 0);
  if (FContent <> nil) and (ContentLayout <> nil) then
  begin
    R := ContentLayout.LocalRect;
    for I := 0 to FContent.ControlsCount - 1 do
      if FContent.Controls[I].Visible then
      begin
        {$IFDEF MSWINDOWS}
        if (csDesigning in ComponentState) and Supports(FContent.Controls[I], IDesignerControl) then
          Continue;
        {$ENDIF}
        LocalR := FContent.Controls[I].BoundsRect;
        if (FContent.Controls[I].Align in RightSide) or (TAnchorKind.akRight in FContent.Controls[I].Anchors) then
        begin
          LocalR.Left := R.Left;
          LocalR.Right := R.Right;
        end;
        if (FContent.Controls[I].Align in BottomSide) or (TAnchorKind.akBottom in FContent.Controls[I].Anchors) then
        begin
          LocalR.Top := R.Top;
          LocalR.Bottom := R.Bottom;
        end;
        if FContent.Controls[I].Align in NeedAddBottom then
          LocalR.Bottom := LocalR.Bottom + Padding.Bottom;
        if FContent.Controls[I].Align in NeedAddRight then
          LocalR.Right := LocalR.Right + Padding.Right;
        Result.Union(LocalR);
      end;
  end;
end;

{***************************************************}
function TALCustomScrollBox.GetContentBounds: TRectF;
begin
  Result := FContentBounds;
end;

{************************************************}
function TALCustomScrollBox.HScrollIndex: Integer;
var
  B: Boolean;
begin
  B := AniCalculations.AutoShowing;
  if FHScrollInfo[Integer(B)].Scroll <> nil then
    Result := Integer(B)
  else
  begin
    B := not B;
    if FHScrollInfo[Integer(B)].Scroll <> nil then
      Result := Integer(B)
    else
      Result := -1;
  end;
end;

{************************************************}
function TALCustomScrollBox.VScrollIndex: Integer;
var
  B: Boolean;
begin
  B := AniCalculations.AutoShowing;
  if FVScrollInfo[Integer(B)].Scroll <> nil then
    Result := Integer(B)
  else
  begin
    B := not B;
    if FVScrollInfo[Integer(B)].Scroll <> nil then
      Result := Integer(B)
    else
      Result := -1;
  end;
end;

{********************************************************}
function TALCustomScrollBox.GetHScrollAlign: TAlignLayout;
var
  I: Integer;
begin
  I := HScrollIndex;
  if I >= 0 then
    Result := FHScrollInfo[I].Align
  else
    Result := TAlignLayout.None;
end;

{****************************************************}
function TALCustomScrollBox.GetHScrollBar: TScrollBar;
var
  I: Integer;
begin
  I := HScrollIndex;
  if I >= 0 then
    Result := FHScrollInfo[I].Scroll
  else
    Result := nil;
end;

{****************************************************}
function TALCustomScrollBox.GetHScrollMargins: TRectF;
var
  I: Integer;
begin
  I := HScrollIndex;
  if I >= 0 then
    Result := FHScrollInfo[I].Margins
  else
    Result := TRectF.Create(0, 0, 0, 0);
end;

{********************************************************}
function TALCustomScrollBox.GetVScrollAlign: TAlignLayout;
var
  I: Integer;
begin
  I := VScrollIndex;
  if I >= 0 then
    Result := FVScrollInfo[I].Align
  else
    Result := TAlignLayout.None;
end;

{****************************************************}
function TALCustomScrollBox.GetVScrollBar: TScrollBar;
var
  I: Integer;
begin
  I := VScrollIndex;
  if I >= 0 then
    Result := FVScrollInfo[I].Scroll
  else
    Result := nil;
end;

{****************************************************}
function TALCustomScrollBox.GetVScrollMargins: TRectF;
var
  I: Integer;
begin
  I := VScrollIndex;
  if I >= 0 then
    Result := FVScrollInfo[I].Margins
  else
    Result := TRectF.Create(0, 0, 0, 0);
end;

{***********************************************}
function TALCustomScrollBox.GetTabList: ITabList;
begin
  if FContent <> nil then
    Result := FContent.GetTabList
  else
    Result := inherited GetTabList;
end;

{*******************************************************}
procedure TALCustomScrollBox.DoRealignContent(R: TRectF);
begin
  if FContent <> nil then
  begin
    FContent.SetBounds(R.Left, R.Top, R.Width, R.Height);
    FContent.FRecalcUpdateRect := True;
    FContent.Realign;
  end;
end;

{*************************************}
procedure TALCustomScrollBox.DoRealign;
var
  LDisablePaint, LDisableInternalAlign: Boolean;
begin
  LDisableInternalAlign := (csDestroying in ComponentState) or FDisableAlign or (FUpdating > 0) or
    (csLoading in ComponentState) or (ContentLayout = nil) or (Content = nil);
  LDisablePaint := FDisablePaint;
  try
    FDisablePaint := True;
    inherited;
    if not LDisableInternalAlign then
    begin
      InternalAlign;
    end;
  finally
    FDisablePaint := LDisablePaint;
  end;
end;

{**********************************************}
function TALCustomScrollBox.ContentRect: TRectF;
begin
  if ContentLayout <> nil then
    Result := ContentLayout.BoundsRect
  else
    Result := LocalRect;
end;

{**********************************************************************************************************************************************}
procedure TALCustomScrollBox.ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: boolean);
begin
  if Assigned(FOnViewportPositionChange) then
    FOnViewportPositionChange(self,
                              OldViewportPosition,
                              NewViewportPosition,
                              ContentSizeChanged);
end;

{**************************************************}
function TALCustomScrollBox.VScrollBarValue: Single;
begin
  if FAniCalculations <> nil then
    Result := ViewportPosition.Y
  else
    Result := 0;
end;

{**************************************************}
function TALCustomScrollBox.HScrollBarValue: Single;
begin
  if FAniCalculations <> nil then
    Result := ViewportPosition.X
  else
    Result := 0;
end;

{**************************************************************}
procedure TALCustomScrollBox.HScrollChangeProc(Sender: TObject);
begin
  if (not FInInternalAlign) and (AniCalculations <> nil) then
    HScrollChange;
end;

{**************************************************************}
procedure TALCustomScrollBox.VScrollChangeProc(Sender: TObject);
begin
  if (not FInInternalAlign) and (AniCalculations <> nil) then
    VScrollChange;
end;

{*****************************************}
procedure TALCustomScrollBox.HScrollChange;
begin
  ViewportPosition := PointF(HScrollBar.Value, ViewportPosition.Y);
  if not IsOpaque then
    UpdateEffects;
  if Assigned(FOnHScrollChange) then
    FOnHScrollChange(self);
  AniCalculations.Shown := True;
end;

{*****************************************}
procedure TALCustomScrollBox.VScrollChange;
begin
  ViewportPosition := TPointF.Create(ViewportPosition.X, VScrollBar.Value);
  if not IsOpaque then
    UpdateEffects;
  if Assigned(FOnVScrollChange) then
    FOnVScrollChange(self);
  AniCalculations.Shown := True;
end;

{****************************************************************}
function TALCustomScrollBox.CreateScrollContent: TALScrollContent;
begin
  Result := TALScrollContent.Create(Self);
end;

{***********************************************************************}
function TALCustomScrollBox.GetScrollingBehaviours: TScrollingBehaviours;
var
  StyleDescriptor: TStyleDescription;
begin
  if Scene <> nil then
    StyleDescriptor := TStyleManager.GetStyleDescriptionForControl(Self)
  else
    StyleDescriptor := nil;
  if (StyleDescriptor <> nil) and StyleDescriptor.PlatformTarget.Contains('[METROPOLISUI]') then
    Result := [TScrollingBehaviour.AutoShowing]
  else
  begin
    if FSystemInfoSrv <> nil then
      Result := FSystemInfoSrv.GetScrollingBehaviour
    else
      Result := [];
  end;
end;

{***********************************************************}
procedure TALCustomScrollBox.MousePosToAni(var X, Y: Single);
var
  LPoint: TPointF;
begin
  LPoint := TPointF.Create(X, Y);
  if ContentLayout <> nil then
  begin
    LPoint := ContentLayout.AbsoluteToLocal(LocalToAbsolute(LPoint));
    X := LPoint.X;
    Y := LPoint.Y;
  end;
end;

{*********************************************************************************************}
procedure TALCustomScrollBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FMouseEvents := True;
  inherited;
  if (Button = TMouseButton.mbLeft) then
  begin
    MousePosToAni(X, Y);
    AniMouseDown(ssTouch in Shift, X, Y);
  end;
end;

{***********************************************************************}
procedure TALCustomScrollBox.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  FMouseEvents := True;
  inherited;
  if AniCalculations.Down then
  begin
    MousePosToAni(X, Y);
    AniMouseMove(ssTouch in Shift, X, Y);
  end;
end;

{*******************************************************************************************}
procedure TALCustomScrollBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FMouseEvents := True;
  inherited;
  if (Button = TMouseButton.mbLeft) then
  begin
    MousePosToAni(X, Y);
    AniMouseUp(ssTouch in Shift, X, Y);
  end;
end;

{****************************************}
procedure TALCustomScrollBox.DoMouseLeave;
begin
  inherited;
  if FMouseEvents and AniCalculations.Down then
  begin
    AniCalculations.MouseLeave;
    if (AniCalculations.LowVelocity) or
       (not AniCalculations.Animation) then
      AniCalculations.Shown := False;
  end;
end;

{*****************************************************************************************************}
procedure TALCustomScrollBox.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
var
  Offset: Single;
begin
  inherited;
  if (not (Handled or FDisableMouseWheel)) and (ContentLayout <> nil) then
  begin
  if ssHorizontal in Shift then
    begin
      if FContentBounds.Width > ContentLayout.Width then
      begin
        AniCalculations.Shown := True;
        if HScrollBar <> nil then
          Offset := HScrollBar.SmallChange
        else
          Offset := ContentLayout.Width / 5;
        Offset := Offset * -1 * (WheelDelta / 120);
        AniCalculations.MouseWheel(Offset, 0);
        Handled := True;
      end;
    end
    else if FContentBounds.Height > ContentLayout.Height then
    begin
      AniCalculations.Shown := True;
      if VScrollBar <> nil then
        Offset := VScrollBar.SmallChange
      else
        Offset := ContentLayout.Height / 5;
      Offset := Offset * -1 * (WheelDelta / 120);
      AniCalculations.MouseWheel(0, Offset);
      Handled := True;
    end
    else if FContentBounds.Width > ContentLayout.Width then
    begin
      AniCalculations.Shown := True;
      if HScrollBar <> nil then
        Offset := HScrollBar.SmallChange
      else
        Offset := ContentLayout.Width / 5;
      Offset := Offset * -1 * (WheelDelta / 120);
      AniCalculations.MouseWheel(Offset, 0);
      Handled := True;
    end;
  end;
end;

{********************************************}
procedure TALCustomScrollBox.SaveDisablePaint;
begin
  FVDisablePaint := False;
  FHDisablePaint := False;
  FGDisablePaint := False;
  if VScrollBar <> nil then
  begin
    FVDisablePaint := TALOpenControl(VScrollBar).FDisablePaint;
    TALOpenControl(VScrollBar).FDisablePaint := True;
  end;
  if HScrollBar <> nil then
  begin
    FHDisablePaint := TALOpenControl(HScrollBar).FDisablePaint;
    TALOpenControl(HScrollBar).FDisablePaint := True;
  end;
  if FSizeGrip <> nil then
  begin
    FGDisablePaint := TALOpenControl(FSizeGrip).FDisablePaint;
    TALOpenControl(FSizeGrip).FDisablePaint := True;
  end;
end;

{*******************************************}
procedure TALCustomScrollBox.SetDisablePaint;
begin
  if VScrollBar <> nil then
    TALOpenControl(VScrollBar).FDisablePaint := True;
  if HScrollBar <> nil then
    TALOpenControl(HScrollBar).FDisablePaint := True;
  if FSizeGrip <> nil then
    TALOpenControl(FSizeGrip).FDisablePaint := True;
end;

{*************************************************}
procedure TALCustomScrollBox.InvalidateContentSize;
begin
  FCachedContentSize := TSizeF.Create(0, 0);
  FContentBounds := TRectF.Empty;
end;

{******************************************}
procedure TALCustomScrollBox.RealignContent;
begin
  InvalidateContentSize;
  Realign;
end;

{***********************************************}
procedure TALCustomScrollBox.RestoreDisablePaint;
begin
  if VScrollBar <> nil then
    TALOpenControl(VScrollBar).FDisablePaint := FVDisablePaint;
  if HScrollBar <> nil then
    TALOpenControl(HScrollBar).FDisablePaint := FHDisablePaint;
  if FSizeGrip <> nil then
    TALOpenControl(FSizeGrip).FDisablePaint := FGDisablePaint;
end;

{************************************}
procedure TALCustomScrollBox.Painting;
begin
  inherited;
  SaveDisablePaint;
  try
    SetDisablePaint;
  except
    RestoreDisablePaint;
    raise;
  end;
end;

{**************************************}
procedure TALCustomScrollBox.AfterPaint;
begin
  try
    RestoreDisablePaint;
    if (VScrollBar <> nil) and VScrollBar.Visible and (VScrollBar.Opacity > 0) then
      TALOpenControl(VScrollBar).PaintInternal;
    if (HScrollBar <> nil) and HScrollBar.Visible and (HScrollBar.Opacity > 0) then
      TALOpenControl(HScrollBar).PaintInternal;
    if (FSizeGrip <> nil) and FSizeGrip.Visible and (FSizeGrip.Opacity > 0) then
      TALOpenControl(FSizeGrip).PaintInternal;
  finally
    inherited;
  end;
end;

{**********************************************************************************}
procedure TALCustomScrollBox.AniMouseDown(const Touch: Boolean; const X, Y: Single);
begin
  fScrollingAcquiredByMe := False;
  fMouseDownPos := TpointF.Create(X,Y);
  AniCalculations.Averaging := Touch;
  AniCalculations.MouseDown(X, Y);
end;

{**********************************************************************************}
procedure TALCustomScrollBox.AniMouseMove(const Touch: Boolean; const X, Y: Single);
begin
  if (AniCalculations.Down) and
     (not fScrollingAcquiredByMe) and
     (((ttHorizontal in fAniCalculations.TouchTracking) and
       (abs(fMouseDownPos.x - x) > fDeadZoneBeforeAcquireScrolling)) or
      ((ttVertical in fAniCalculations.TouchTracking) and
       (abs(fMouseDownPos.y - y) > fDeadZoneBeforeAcquireScrolling))) then begin
    fScrollingAcquiredByMe := True;
    TMessageManager.DefaultManager.SendMessage(self, TALScrollingAcquiredMessage.Create, True);
  end;
  AniCalculations.MouseMove(X, Y);
  if AniCalculations.Moved then
    AniCalculations.Shown := True;
end;

{********************************************************************************}
procedure TALCustomScrollBox.AniMouseUp(const Touch: Boolean; const X, Y: Single);
begin
  AniCalculations.MouseUp(X, Y);
  if (AniCalculations.LowVelocity) or
     (not AniCalculations.Animation) then
    AniCalculations.Shown := False;
end;

{******************************************************************}
procedure TALCustomScrollBox.DoAddObject(const AObject: TFmxObject);
begin
  if IsAddToContent(AObject) then
    FContent.AddObject(AObject)
  else
    inherited;
end;

{*********************************************************************************}
procedure TALCustomScrollBox.UpdateOriginalContentLayoutSize(const Force: Boolean);
begin
  if Force or (FOriginalContentLayoutSize.cx < 0) then
    if ContentLayout <> nil then
      FOriginalContentLayoutSize.cx := ContentLayout.LocalRect.Width
    else
      FOriginalContentLayoutSize.cx := LocalRect.Width;
  if Force or (FOriginalContentLayoutSize.cy < 0) then
    if ContentLayout <> nil then
      FOriginalContentLayoutSize.cy := ContentLayout.LocalRect.Height
    else
      FOriginalContentLayoutSize.cy := LocalRect.Height;
end;

{**********************************}
procedure TALCustomScrollBox.Loaded;
begin
  inherited;
  FContent.Loaded; // ensure that FixupTabList is called for FContent
  UpdateOriginalContentLayoutSize(False);
end;

{******************************************}
procedure TALCustomScrollBox.PaddingChanged;
begin
  Content.Padding.Assign(Padding);
end;

{**********************************}
procedure TALCustomScrollBox.Center;
begin
  if ContentLayout <> nil then
    ViewportPosition := TPointF.Create((ContentBounds.Width - ContentLayout.Width) / 2,
      (ContentBounds.Height - ContentLayout.Height) / 2);
end;

{**********************************************************}
procedure TALCustomScrollBox.ScrollBy(const Dx, Dy: Single);
begin
  if VScrollBar <> nil then
    VScrollBar.Value := VScrollBar.Value - Dy;
  if HScrollBar <> nil then
    HScrollBar.Value := HScrollBar.Value - Dx;
end;

{**********************************************************}
procedure TALCustomScrollBox.ScrollTo(const Dx, Dy: Single);
begin
  ScrollBy(Dx, Dy);
end;

{**********************************************************}
procedure TALCustomScrollBox.InViewRect(const Rect: TRectF);
begin
end;

{*************************************************************}
procedure TALCustomScrollBox.SetAutoHide(const Value: Boolean);
begin
  if FAutoHide <> Value then
  begin
    FAutoHide := Value;
    Realign;
  end;
end;

{*******************************************************************}
procedure TALCustomScrollBox.SetShowScrollBars(const Value: Boolean);
begin
  if FShowScrollBars <> Value then
  begin
    FShowScrollBars := Value;
    if FShowScrollBars and FAutoHide then
      InvalidateContentSize;
    Realign;
  end;
end;

{*****************************************************************}
procedure TALCustomScrollBox.SetShowSizeGrip(const Value: Boolean);
begin
  if FShowSizeGrip <> Value then
  begin
    FShowSizeGrip := Value;
    UpdateSizeGrip;
  end;
end;

{*******************************************************}
function TALCustomScrollBox.GetViewportPosition: TPointF;
var
  LScale, X, Y: Double;
begin
  LScale := GetSceneScale;
  X := Round(AniCalculations.ViewportPosition.X * LScale) / LScale;
  Y := Round(AniCalculations.ViewportPosition.Y * LScale) / LScale;
  Result := TPointF.Create(X, Y);
end;

{*********************************************************************}
procedure TALCustomScrollBox.SetViewportPosition(const Value: TPointF);
var
  LScale, X, Y: Double;
begin
  LScale := GetSceneScale;
  X := Value.X;
  Y := Value.Y;
  AniCalculations.ViewportPosition := TALPointD.Create(Round(X * LScale) / LScale, Round(Y * LScale) / LScale);
end;

{***********************************************************}
procedure TALCustomScrollBox.DefineProperties(Filer: TFiler);
const
  VP = 'Viewport';
begin
  inherited;
  Filer.DefineProperty('UseSmallScrollBars', IgnoreBooleanValue, nil, False);
  Filer.DefineProperty('MouseTracking', IgnoreBooleanValue, nil, False);
  Filer.DefineProperty(VP + '.Width', ReadContentLayoutWidth, WriteContentLayoutWidth, True);
  Filer.DefineProperty(VP + '.Height', ReadContentLayoutHeight, WriteContentLayoutHeight, True);
end;

{***************************************************************************}
procedure TALCustomScrollBox.ReadPartSize(Reader: TReader; var Size: Single);
var
  LValue: Double;
begin
  case Reader.NextValue of
    vaExtended: LValue := Reader.ReadFloat;
    vaDouble: LValue := Reader.ReadDouble;
    vaSingle: LValue := Reader.ReadSingle;
    vaInt32: LValue := Reader.ReadInteger;
  else
    Reader.SkipValue;
    Exit;
  end;
  Size := RoundTo(LValue, -1);
end;

{********************************************************************}
procedure TALCustomScrollBox.ReadContentLayoutHeight(Reader: TReader);
begin
  ReadPartSize(Reader, FOriginalContentLayoutSize.cy);
end;

{*******************************************************************}
procedure TALCustomScrollBox.ReadContentLayoutWidth(Reader: TReader);
begin
  ReadPartSize(Reader, FOriginalContentLayoutSize.cx);
end;

{*********************************************************************}
procedure TALCustomScrollBox.WriteContentLayoutHeight(Writer: TWriter);
begin
  if ContentLayout <> nil then
    FOriginalContentLayoutSize.cy := ContentLayout.LocalRect.Height;
  Writer.WriteFloat(RoundTo(FOriginalContentLayoutSize.cy, -1));
end;

{********************************************************************}
procedure TALCustomScrollBox.WriteContentLayoutWidth(Writer: TWriter);
begin
  if ContentLayout <> nil then
    FOriginalContentLayoutSize.cx := ContentLayout.LocalRect.Width;
  Writer.WriteFloat(RoundTo(FOriginalContentLayoutSize.cx, -1));
end;

{****************************************************************}
procedure TALCustomScrollBox.Sort(Compare: TFmxObjectSortCompare);
begin
  FContent.Sort(Compare);
end;

{******************************************}
procedure TALCustomScrollBox.StartScrolling;
begin
  if Scene <> nil then
    Scene.ChangeScrollingState(Self, True);
end;

{*****************************************}
procedure TALCustomScrollBox.StopScrolling;
begin
  if Scene <> nil then
    Scene.ChangeScrollingState(nil, False);
end;

{***********************************************}
function TALCustomScrollBox.ClientHeight: Single;
begin
  if ContentLayout <> nil then
    Result := ContentLayout.Height
  else
    Result := Height;
end;

{**********************************************}
function TALCustomScrollBox.ClientWidth: Single;
begin
  if ContentLayout <> nil then
    Result := ContentLayout.Width
  else
    Result := Width;
end;

{*****************************************************************************************************}
procedure TALCustomScrollBox.ScrollingAcquiredByOtherHandler(const Sender: TObject; const M: TMessage);
begin
  //the scrolling was acquired by another control (like a scrollbox for exemple)
  if Sender <> self then begin
    fAniCalculations.MouseLeave;
    fAniCalculations.animation := False;
    fAniCalculations.animation := true;
  end;
end;

{***********************************************************************}
procedure TALCustomScrollBox.CMGesture(var EventInfo: TGestureEventInfo);
var
  LP: TPointF;
begin
  //This is used when scrolling with the finger on top of a control (like a TButton on a TListItem).
  if (ContentLayout <> nil) and (EventInfo.GestureID = igiPan) then
  begin
    FMouseEvents := False;
    LP := ContentLayout.AbsoluteToLocal(EventInfo.Location);
    if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then
      AniMouseDown(True, LP.X, LP.Y)
    else
      if EventInfo.Flags = [] then
        AniMouseMove(True, LP.X, LP.Y)
      else
      if AniCalculations.Down then
        AniMouseUp(True, LP.X, LP.Y);
  end
  else
    inherited;
end;

{*****************************************************************************}
function TALCustomScrollBox.IsAddToContent(const AObject: TFmxObject): Boolean;
begin
  Result := (FContent <> nil)
    and (AObject <> FContent)
    and (AObject <> ResourceLink)
    and not (AObject is TEffect)
    and not (AObject is TAnimation)
    and not ((AObject = FVScrollInfo[0].Scroll) or
             (AObject = FVScrollInfo[1].Scroll) or
             (AObject = FHScrollInfo[0].Scroll) or
             (AObject = FHScrollInfo[1].Scroll) or
             (AObject = FSizeGrip));
end;

{********************************************}
function TALCustomScrollBox.IsOpaque: Boolean;
begin
  Result := False;
end;

{***********************************************************************}
procedure TALCustomScrollBox.ContentAddObject(const AObject: TFmxObject);
begin
  RealignContent;
end;

{******************************************************************************************}
procedure TALCustomScrollBox.ContentInsertObject(Index: Integer; const AObject: TFmxObject);
begin
  RealignContent;
end;

{**************************************************************************}
procedure TALCustomScrollBox.ContentRemoveObject(const AObject: TFmxObject);
begin
  RealignContent;
end;

{**************************************************************************}
procedure TALCustomScrollBox.ContentBeforeRemoveObject(AObject: TFmxObject);
begin
end;

{ TScrollBox }

{***************************}
procedure TALScrollBox.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder(DesignBorderColor, DesignBorderColor);
end;

{ TVertScrollBox }

{****************************************************}
function TALVertScrollBox.DoCalcContentBounds: TRectF;
begin
  if (FContent <> nil) and (ContentLayout <> nil) then
    FContent.Width := ContentLayout.Width; // Only for compatibility with old code
  Result := inherited DoCalcContentBounds;
  if ContentLayout <> nil then
    Result.Width := ContentLayout.Width;
end;

{************************************************************************************************}
procedure TALVertScrollBox.DoUpdateAniCalculations(const AAniCalculations: TALScrollCalculations);
begin
  inherited DoUpdateAniCalculations(AAniCalculations);
  AAniCalculations.TouchTracking := AAniCalculations.TouchTracking - [ttHorizontal];
end;

{*******************************}
procedure TALVertScrollBox.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder(DesignBorderColor or TAlphaColorRec.Alpha, DesignBorderColor);
end;

{**********************************************************}
function TALVertScrollBox.GetDefaultStyleLookupName: string;
begin
  Result := 'scrollboxstyle';
end;

{ THorzScrollBox }

{****************************************************}
function TALHorzScrollBox.DoCalcContentBounds: TRectF;
begin
  if (FContent <> nil) and (ContentLayout <> nil) then
    FContent.Height := ContentLayout.Height; // Only for compatibility with old code
  Result := inherited DoCalcContentBounds;
  if ContentLayout <> nil then
    Result.Height := ContentLayout.Height;
end;

{************************************************************************************************}
procedure TALHorzScrollBox.DoUpdateAniCalculations(const AAniCalculations: TALScrollCalculations);
begin
  inherited DoUpdateAniCalculations(AAniCalculations);
  AAniCalculations.TouchTracking := AAniCalculations.TouchTracking - [ttVertical];
end;

{**********************************************************}
function TALHorzScrollBox.GetDefaultStyleLookupName: string;
begin
  Result := 'scrollboxstyle';
end;

{*******************************}
procedure TALHorzScrollBox.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder(DesignBorderColor, DesignBorderColor or TAlphaColorRec.Alpha);
end;

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALScrollBox, TALVertScrollBox, TALHorzScrollBox]);
end;

end.
