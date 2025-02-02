unit Alcinoe.FMX.Layouts;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if FMX.Layouts.pas was not updated and adjust the IFDEF'}
{$ENDIF}

uses
  System.Classes,
  System.Types,
  System.UITypes,
  System.Messaging,
  System.Generics.Collections,
  FMX.layouts,
  FMX.Types,
  FMX.Controls,
  Alcinoe.FMX.Controls,
  ALcinoe.FMX.Common,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.ScrollEngine;

type

  {*************************}
  [ComponentPlatforms($FFFF)]
  TALLayout = class(TALControl)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    //property Action;
    property Align;
    property Anchors;
    property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    //property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest default False;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    //property OnCanFocus;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {**********************************************************}
  TALCustomScrollBox = class(TALControl, IALScrollableControl)
  public
    type
      // --------
      // TContent
      TContent = class(TALContent)
      private
        FScrollBox: TALCustomScrollBox;
      protected
        procedure DoAddObject(const AObject: TFmxObject); override;
        procedure DoRemoveObject(const AObject: TFmxObject); override;
        procedure DoDeleteChildren; override;
        procedure ContentChanged; override;
        {$IFNDEF ALDPK}
        function IsVisibleObject(const AObject: TControl): Boolean; override;
        {$ENDIF}
      public
        constructor Create(AOwner: TComponent); override;
        property ScrollBox: TALCustomScrollBox read FScrollBox;
      end;
      // -------------
      // TScrollEngine
      TScrollEngine = class (TALScrollEngine)
      private
        FScrollBox: TALCustomScrollBox;
        fLastViewportPosition: TpointF;
      protected
        procedure DoChanged; override;
        procedure DoStart; override;
        procedure DoStop; override;
      public
        constructor Create(const AScrollBox: TALCustomScrollBox); reintroduce;
        property ScrollBox: TALCustomScrollBox read FScrollBox;
      end;
      // ----------
      // TScrollBar
      TScrollBar = class(TALCustomScrollBar)
      private
        FScrollBox: TALCustomScrollBox;
      protected
        function GetDefaultSize: TSizeF; override;
        procedure DoChanged; override;
        procedure Resize; override;
      public
        constructor Create(AOwner: TComponent); override;
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
      published
        //property Action;
        //property Align;
        //property Anchors;
        //property AutoSize;
        property CanFocus;
        //property CanParentFocus;
        //property DisableFocusEffect;
        property DoubleBuffered;
        //property ClipChildren;
        //property ClipParent;
        //property Cursor;
        //property DoubleBuffered;
        //property DragMode;
        //property EnableDragHighlight;
        //property Enabled;
        property Height;
        //property Hint;
        //property ParentShowHint;
        //property ShowHint;
        property HitTest default false;
        //property Locked;
        property Margins;
        //property Min;
        //property Max;
        //property Opacity;
        //property Orientation;
        property Padding;
        //property PopupMenu;
        //property Position;
        //property RotationAngle;
        //property RotationCenter;
        //property Pivot;
        //property Scale;
        property Size;
        property TabOrder;
        property TabStop;
        property Thumb;
        //property TouchTargetExpansion;
        //property Value;
        //property ViewportSize;
        //property Visible;
        property Width;
        property OnCanFocus;
        property OnChange;
        //property OnDragEnter;
        //property OnDragLeave;
        //property OnDragOver;
        //property OnDragDrop;
        //property OnDragEnd;
        property OnEnter;
        property OnExit;
        property OnMouseEnter;
        property OnMouseLeave;
        property OnMouseDown;
        property OnMouseUp;
        property OnMouseMove;
        property OnMouseWheel;
        property OnClick;
        //property OnDblClick;
        property OnKeyDown;
        property OnKeyUp;
        property OnPainting;
        property OnPaint;
        //property OnResize;
        property OnResized;
      end;
      // --------------------
      // TPositionChangeEvent
      TViewportPositionChangeEvent = procedure (Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF) of object;
  private
    FScrollEngine: TScrollEngine;
    FContent: TContent;
    FHScrollBar: TScrollBar;
    FVScrollBar: TScrollBar;
    FDisableMouseWheel: Boolean;
    fdisableScrollChange: Boolean;
    FHasTouchScreen: Boolean;
    FShowScrollBars: Boolean;
    FAutoHide: Boolean;
    FMouseEvents: Boolean;
    FOnViewportPositionChange: TViewportPositionChangeEvent;
    fOnAniStart: TnotifyEvent;
    fOnAniStop: TnotifyEvent;
    fMouseDownPos: TpointF;
    fScrollCapturedByMe: boolean;
    fMaxContentWidth: Single;
    fMaxContentHeight: single;
    fAnchoredContentOffset: TPointF;
    procedure ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
    procedure SetShowScrollBars(const Value: Boolean);
    procedure SetAutoHide(const Value: Boolean);
    function IsMaxContentHeightStored: Boolean;
    function IsMaxContentWidthStored: Boolean;
    procedure InternalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Single);
    procedure InternalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure InternalMouseLeave;
    { IALScrollableControl }
    function GetScrollEngine: TALScrollEngine;
  protected
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRealign; override;
    function CreateScrollBar(const aOrientation: TOrientation): TScrollBar; virtual;
    function CreateContent: TContent; virtual;
    function CreateScrollEngine: TScrollEngine; virtual;
    function CalcContentBounds: TRectF; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    {$IFNDEF ALDPK}
    procedure ChildrenMouseDown(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseMove(const AObject: TControl; Shift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseUp(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseLeave(const AObject: TControl); override;
    {$ENDIF}
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    property HScrollBar: TScrollBar read fHScrollBar;
    property VScrollBar: TScrollBar read fVScrollBar;
    property MaxContentWidth: Single read fMaxContentWidth write fMaxContentWidth stored IsMaxContentWidthStored nodefault;
    property MaxContentHeight: Single read fMaxContentHeight write fMaxContentHeight stored IsMaxContentHeightStored nodefault;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ScrollEngine: TALScrollEngine read GetScrollEngine;
    procedure Sort(Compare: TFmxObjectSortCompare); override;
    procedure ScrollBy(const Dx, Dy: Single);
    function GetTabList: ITabList; override;
    property Content: TContent read FContent;
    property HasTouchScreen: Boolean read FHasTouchScreen;
    property AutoHide: Boolean read FAutoHide write SetAutoHide default True;
    property DisableMouseWheel: Boolean read FDisableMouseWheel write FDisableMouseWheel default False;
    property ShowScrollBars: Boolean read FShowScrollBars write SetShowScrollBars default True;
    property OnViewportPositionChange: TViewportPositionChangeEvent read FOnViewportPositionChange write FOnViewportPositionChange;
    property ClipChildren default true;
    property OnAniStart: TnotifyEvent read fOnAniStart write fOnAniStart;
    property OnAniStop: TnotifyEvent read fOnAniStop write fOnAniStop;
  end;

  {*************************}
  [ComponentPlatforms($FFFF)]
  TALScrollBox = class(TALCustomScrollBox)
  protected
    procedure Paint; override;
  published
    property HScrollBar;
    property VScrollBar;
    //property Action;
    property Align;
    property Anchors;
    property AutoHide;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DisableMouseWheel;
    //property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property ShowScrollBars;
    property Size;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property OnAniStart;
    property OnAniStop;
    //property OnCanFocus;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
    property OnViewportPositionChange;
  end;

  {*************************}
  [ComponentPlatforms($FFFF)]
  TALVertScrollBox = class(TALCustomScrollBox)
  protected
    function CalcContentBounds: TRectF; override;
    procedure Paint; override;
    function CreateScrollBar(const aOrientation: TOrientation): TALCustomScrollBox.TScrollBar; override;
    function CreateScrollEngine: TALCustomScrollBox.TScrollEngine; override;
  published
    property VScrollBar;
    //property Action;
    property Align;
    property Anchors;
    property AutoHide;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DisableMouseWheel;
    //property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property Locked;
    property Margins;
    property MaxContentWidth;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property ShowScrollBars;
    property Size;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property OnAniStart;
    property OnAniStop;
    //property OnCanFocus;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
    property OnViewportPositionChange;
  end;

  {*************************}
  [ComponentPlatforms($FFFF)]
  TALHorzScrollBox = class(TALCustomScrollBox)
  protected
    function CalcContentBounds: TRectF; override;
    procedure Paint; override;
    function CreateScrollBar(const aOrientation: TOrientation): TALCustomScrollBox.TScrollBar; override;
    function CreateScrollEngine: TALCustomScrollBox.TScrollEngine; override;
  published
    property HScrollBar;
    //property Action;
    property Align;
    property Anchors;
    property AutoHide;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DisableMouseWheel;
    //property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property Locked;
    property Margins;
    property MaxContentHeight;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property ShowScrollBars;
    property Size;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property OnAniStart;
    property OnAniStop;
    //property OnCanFocus;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
    property OnViewportPositionChange;
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
  Alcinoe.StringUtils,
  Alcinoe.FMX.Objects,
  Alcinoe.Common;

{**}
Type
  _TControlAccessProtected = class(Tcontrol);

{***********************************************}
constructor TALLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CanParentFocus := True;
  HitTest := False;
end;

{************************}
procedure TALLayout.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{*****************************************************************}
constructor TALCustomScrollBox.TContent.Create(AOwner: TComponent);
begin
  ValidateInheritance(AOwner, TALCustomScrollBox, False{CanBeNil});
  inherited Create(AOwner);
  FScrollBox := TALCustomScrollBox(AOwner);
  SetAcceptsControls(False);
end;

{*************}
{$IFNDEF ALDPK}
function TALCustomScrollBox.TContent.IsVisibleObject(const AObject: TControl): Boolean;
begin
  if AObject.Visible then begin

    If Height > ScrollBox.Height then
      Result := (AObject.Position.Y < -Position.Y + FscrollBox.Height) and
                (AObject.Position.Y + (AObject.Height * _TControlAccessProtected(AObject).scale.y) > -Position.Y)
    else
      // Handle the maxContentHeight
      Result := (AObject.Position.Y < Height) and
                (AObject.Position.Y + (AObject.Height * _TControlAccessProtected(AObject).scale.y) > 0);

    If Width > ScrollBox.Width then
      Result := Result and
                (AObject.Position.X < -Position.X + FscrollBox.Width) and
                (AObject.Position.X + (AObject.Width * _TControlAccessProtected(AObject).scale.x) > Position.X)
    else
      // Handle the maxContentWidth
      Result := Result and
                (AObject.Position.X < Width) and
                (AObject.Position.X + (AObject.Width * _TControlAccessProtected(AObject).scale.x) > 0);

  end
  else
    result := False;
end;
{$ENDIF}

{***************************************************************************}
procedure TALCustomScrollBox.TContent.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  ContentChanged;
end;

{******************************************************************************}
procedure TALCustomScrollBox.TContent.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  ContentChanged;
end;

{*****************************************************}
procedure TALCustomScrollBox.TContent.DoDeleteChildren;
begin
  inherited;
  ContentChanged;
end;

{***************************************************}
procedure TALCustomScrollBox.TContent.ContentChanged;
begin
  inherited;
  if (not IsUpdating) and (not FDisableAlign) then FScrollBox.Realign;
end;

{****************************************************************************************}
constructor TALCustomScrollBox.TScrollEngine.Create(const AScrollBox: TALCustomScrollBox);
begin
  inherited Create;
  FScrollBox := AScrollBox;
  fLastViewportPosition := TpointF.Create(0,0);
end;

{***************************************************}
procedure TALCustomScrollBox.TScrollEngine.DoChanged;
begin
  {$IF defined(debug)}
  //ALLog(ClassName + '.TALCustomScrollBox.TScrollEngine.DoChanged');
  {$ENDIF}
  if (not (csDestroying in FScrollBox.ComponentState)) then begin

    // Update the position of FScrollBox.Content
    if FScrollBox.Content <> nil then begin
      var LSaveDisableAlign := FScrollBox.FDisableAlign;
      FScrollBox.FDisableAlign := True;
      try
        FScrollBox.Content.Position.Point := -TPointF.Create(
                                                ViewportPosition.X - FScrollBox.fAnchoredContentOffset.X,
                                                ViewportPosition.Y - FScrollBox.fAnchoredContentOffset.Y);
      finally
        FScrollBox.FDisableAlign := LSaveDisableAlign;
      end;
    end;

    // Update the opacity of the scroll bar
    if FScrollBox.VScrollBar <> nil then FScrollBox.VScrollBar.Opacity := Opacity;
    if FScrollBox.HScrollBar <> nil then FScrollBox.HScrollBar.Opacity := Opacity;

    // Update the vertical and horizontal scroll bars (VScrollBar/HScrollBar)
    if not FScrollBox.fdisableScrollChange then begin
      FScrollBox.fdisableScrollChange := True;
      try
        if FScrollBox.VScrollBar <> nil then FScrollBox.VScrollBar.Value := ViewportPosition.Y;
        if FScrollBox.HScrollBar <> nil then FScrollBox.HScrollBar.Value := ViewportPosition.X;
      finally
        FScrollBox.fdisableScrollChange := False;
      end;
    end;

    // Trigger the OnViewportPositionChange event
    var LNewViewportPosition := TpointF.Create(ViewportPosition.X, ViewportPosition.Y);
    if (assigned(FScrollBox.FOnViewportPositionChange)) and
       (not fLastViewportPosition.EqualsTo(LNewViewportPosition, TEpsilon.Position)) then
      FScrollBox.FOnViewportPositionChange(self, fLastViewportPosition, LNewViewportPosition);
    fLastViewportPosition := LNewViewportPosition;

  end;
  inherited DoChanged;
end;

{*************************************************}
procedure TALCustomScrollBox.TScrollEngine.DoStart;
begin
  inherited DoStart;

  if (FScrollBox.Scene <> nil) and
     (not (csDestroying in FScrollBox.ComponentState)) then
    FScrollBox.Scene.ChangeScrollingState(FScrollBox, True);

  if (assigned(fscrollBox.fOnAniStart)) and
     (not (csDestroying in FScrollBox.ComponentState)) then
    fscrollBox.fOnAniStart(fscrollBox);
end;

{************************************************}
procedure TALCustomScrollBox.TScrollEngine.DoStop;
begin
  inherited DoStop;

  if (FScrollBox.Scene <> nil) and
     (not (csDestroying in FScrollBox.ComponentState)) then
    FScrollBox.Scene.ChangeScrollingState(nil, False);

  if (assigned(fscrollBox.fOnAniStop)) and
     (not (csDestroying in FScrollBox.ComponentState)) then
    fscrollBox.fOnAniStop(fscrollBox);
end;

{*******************************************************************}
constructor TALCustomScrollBox.TScrollBar.Create(AOwner: TComponent);
begin
  ValidateInheritance(AOwner, TALCustomScrollBox, False{CanBeNil});
  inherited Create(AOwner);
  FScrollBox := TALCustomScrollBox(AOwner);
  HitTest := False;
end;

{************************************************************}
function TALCustomScrollBox.TScrollBar.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(150, 4);
end;

{************************************************}
procedure TALCustomScrollBox.TScrollBar.DoChanged;
begin
  if FScrollBox.fdisableScrollChange then exit;
  FScrollBox.fdisableScrollChange := True;
  try
    if Orientation=TOrientation.vertical then FScrollBox.fScrollEngine.ViewportPosition := TAlPointD.Create(FScrollBox.fScrollEngine.ViewportPosition.x, Value)
    else FScrollBox.fScrollEngine.ViewportPosition := TAlPointD.Create(Value, FScrollBox.fScrollEngine.ViewportPosition.Y);
  finally
    FScrollBox.fdisableScrollChange := False;
  end;
  inherited DoChanged;
end;

{*********************************************}
procedure TALCustomScrollBox.TScrollBar.Resize;
begin
  inherited;
  FScrollBox.Realign;
end;

{********************************************************}
constructor TALCustomScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ClipChildren := True;
  SetAcceptsControls(True);
  AutoCapture := True;
  var LDeviceService: IFMXDeviceService;
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
  fOnAniStart := nil;
  fOnAniStop := nil;
  fScrollEngine := CreateScrollEngine;
  fMaxContentWidth := 0;
  fMaxContentHeight := 0;
  fAnchoredContentOffset := TpointF.Create(0,0);
  //-----
  // It is important that FVScrollBar and FHScrollBar are created AFTER FContent;
  // otherwise, FVScrollBar and FHScrollBar will not be added to the TabList of FContent.
  FContent := CreateContent;
  FVScrollBar := CreateScrollBar(Torientation.Vertical);
  FHScrollBar := CreateScrollBar(Torientation.Horizontal);
  if FHScrollBar <> nil then FHScrollBar.BringToFront;
  if FVScrollBar <> nil then FVScrollBar.BringToFront;
  //-----
  fMouseDownPos := TpointF.zero;
  fScrollCapturedByMe := False;
  TMessageManager.DefaultManager.SubscribeToMessage(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
end;

{************************************}
destructor TALCustomScrollBox.Destroy;
begin
  // https://github.com/pleriche/FastMM5/issues/31
  var LTabList := GetTabList;
  If FVScrollBar <> nil then LTabList.Remove(FVScrollBar);
  If FHScrollBar <> nil then LTabList.Remove(FHScrollBar);
  LTabList := nil;
  TMessageManager.DefaultManager.Unsubscribe(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  ALFreeAndNil(FScrollEngine);
  inherited Destroy;
end;

{************************************************************}
function TALCustomScrollBox.IsMaxContentHeightStored: Boolean;
begin
  result := not sameValue(fMaxContentHeight, 0, Tepsilon.Position);
end;

{***********************************************************}
function TALCustomScrollBox.IsMaxContentWidthStored: Boolean;
begin
  result := not sameValue(fMaxContentWidth, 0, Tepsilon.Position);
end;

{****************************************************}
function TALCustomScrollBox.CalcContentBounds: TRectF;
begin
  Result := LocalRect;
  if (FContent <> nil) then begin
    for var I := 0 to FContent.ControlsCount - 1 do begin
      var LControl := FContent.Controls[I];
      if LControl.Visible then begin
        {$IFDEF MSWINDOWS}
        if (csDesigning in ComponentState) and Supports(LControl, IDesignerControl) then
          Continue;
        {$ENDIF}
        var LBoundsRect := LControl.BoundsRect;
        LBoundsRect.Right := LBoundsRect.Right + LControl.Margins.Right;
        LBoundsRect.Bottom := LBoundsRect.Bottom + LControl.Margins.Bottom;
        Result.Union(LBoundsRect);
      end;
    end;
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
      fVScrollBar.BeginUpdate;
      try
        fVScrollBar.Min := 0;
        fVScrollBar.Max := AContentRect.Height;
        fVScrollBar.ViewportSize := height;
      finally
        fVScrollBar.EndUpdate;
      end;
      fVScrollBar.SetBounds(
        width - fVScrollBar.Width - fVScrollBar.Margins.Right,
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
      fHScrollBar.BeginUpdate;
      try
        fHScrollBar.Min := 0;
        fHScrollBar.Max := AContentRect.Width;
        fHScrollBar.ViewportSize := Width;
      finally
        fHScrollBar.EndUpdate;
      end;
      fHScrollBar.SetBounds(
        fHScrollBar.Margins.left,
        height - fHScrollBar.Height - fHScrollBar.Margins.bottom,
        width - fHScrollBar.Margins.left - fHScrollBar.Margins.right,
        fHScrollBar.Height);
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateScrollEngineLimits(const aContentRect: TRectF);
  begin
    ScrollEngine.MinScrollLimit := TALPointD.Create(0,0);
    if (fHScrollBar <> nil) and
       (fVScrollBar <> nil) then ScrollEngine.MaxScrollLimit := TALPointD.Create(aContentRect.Width - width, aContentRect.Height - height)
    else if (fVScrollBar <> nil) then ScrollEngine.MaxScrollLimit := TALPointD.Create(0, aContentRect.Height - height)
    else if (fHScrollBar <> nil) then ScrollEngine.MaxScrollLimit := TALPointD.Create(aContentRect.Width - width, 0)
    else ScrollEngine.MaxScrollLimit := TALPointD.Create(0, 0);
  end;

begin

  var LDoRealignAgain := False;
  if fDisableAlign then exit;
  fDisableAlign := True;
  try

    if (FContent <> nil) then begin
      var LContentRect := CalcContentBounds;
      Content.SetBounds(
        LContentRect.Left + fAnchoredContentOffset.x,
        LContentRect.Top + fAnchoredContentOffset.y,
        LContentRect.Width,
        LContentRect.Height);
      if LContentRect.EqualsTo(CalcContentBounds, Tepsilon.Position) then begin
        _UpdateVScrollBar(LContentRect);
        _UpdateHScrollBar(LContentRect);
        _UpdateScrollEngineLimits(LContentRect);
        fScrollEngine.DoChanged;
      end
      else LDoRealignAgain := True;
    end;

  finally
    fDisableAlign := false;
  end;

  if LDoRealignAgain then DoRealign;

end;

{****************************************************************************************}
function TALCustomScrollBox.CreateScrollBar(const aOrientation: TOrientation): TScrollBar;
begin
  Result := TScrollBar.Create(self);
  Result.Parent := self;
  Result.Stored := False;
  Result.SetSubComponent(True);
  Result.Locked := True;
  Result.Beginupdate;
  try
    Result.Orientation := aOrientation;
    // At design time (on Windows), this will always be True, which is the default value.
    // This means that True will never be saved in the DFM; only False can be stored in the DFM.
    // Therefore, only False in the DFM can override this setting.
    result.Thumb.HitTest := not HasTouchScreen;
    if aOrientation = TOrientation.Vertical then Result.Name := 'VScrollBar'
    else Result.Name := 'HScrollBar';
    Result.Visible := False;
  finally
    result.EndUpdate;
  end;
end;

{**************************************************}
function TALCustomScrollBox.CreateContent: TContent;
begin
  Result := TContent.Create(Self);
  Result.parent := self;
  Result.Stored := False;
  Result.Locked := True;
  Result.HitTest := False;
end;

{************************************************************}
function TALCustomScrollBox.CreateScrollEngine: TScrollEngine;
begin
  Result := TScrollEngine.Create(self);
  if HasTouchScreen then Result.TouchTracking := [ttVertical, ttHorizontal]
  else Result.TouchTracking := [];
  Result.MinEdgeSpringbackEnabled := HasTouchScreen;
  Result.MaxEdgeSpringbackEnabled := HasTouchScreen;
  Result.AutoShowing := HasTouchScreen;
end;

{***********************************************************}
function TALCustomScrollBox.GetScrollEngine: TALScrollEngine;
begin
  result := FScrollEngine;
end;

{**************************************************************************************************}
procedure TALCustomScrollBox.ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
begin
  if (Sender = self) then exit;
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.ScrollCapturedByOtherHandler',
  //  'Captured: ' + ALBoolToStrW(TALScrollCapturedMessage(M).Captured)+ ' | ' +
  //  'ScrollEngine.down: ' + ALBoolToStrW(fScrollEngine.down));
  {$ENDIF}
  if TALScrollCapturedMessage(M).Captured then begin
    {$IFDEF DEBUG}
    if fScrollCapturedByMe then
      raise Exception.Create('Error 8EA8C349-8441-4D2F-BC5A-872772A40513');
    {$ENDIF}
    if fScrollEngine.down then begin
      fScrollEngine.Down := false;
      FMouseEvents := False;
    end;
  end;
end;

{*****************************************************************************************************}
procedure TALCustomScrollBox.InternalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.MouseDown',
  //  'Position:' + ALFormatFloatW('0.##', x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', y, ALDefaultFormatSettingsW));
  {$ENDIF}
  if (Button = TMouseButton.mbLeft) then begin
    FMouseEvents := true;
    fMouseDownPos := TpointF.Create(X,Y);
    ScrollEngine.MouseDown(X, Y);
  end;
end;

{*******************************************************************************}
procedure TALCustomScrollBox.InternalMouseMove(Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.InternalMouseMove',
  //  'Position:' + ALFormatFloatW('0.##', x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', y, ALDefaultFormatSettingsW));
  {$ENDIF}
  if FMouseEvents then begin
    if (not fScrollCapturedByMe) and
       (((ttHorizontal in fScrollEngine.TouchTracking) and
         (abs(fMouseDownPos.x - x) > abs(fMouseDownPos.y - y)) and
         (abs(fMouseDownPos.x - x) > TALScrollEngine.DefaultTouchSlop)) or
        ((ttVertical in fScrollEngine.TouchTracking) and
         (abs(fMouseDownPos.y - y) > abs(fMouseDownPos.x - x)) and
         (abs(fMouseDownPos.y - y) > TALScrollEngine.DefaultTouchSlop))) then begin
      {$IFDEF DEBUG}
      //ALLog(
      //  ClassName + '.InternalMouseMove',
      //  'ScrollCapturedByMe');
      {$ENDIF}
      fScrollCapturedByMe := True;
      TMessageManager.DefaultManager.SendMessage(self, TALScrollCapturedMessage.Create(True));
    end;
    ScrollEngine.MouseMove(X, Y);
  end;
end;

{***************************************************************************************************}
procedure TALCustomScrollBox.InternalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.InternalMouseUp',
  //  'Position:' + ALFormatFloatW('0.##', x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', y, ALDefaultFormatSettingsW));
  {$ENDIF}
  if FMouseEvents and (Button = TMouseButton.mbLeft) then begin
    FScrollCapturedByMe := False;
    ScrollEngine.MouseUp(X, Y);
    FMouseEvents := False;
  end;
end;

{**********************************************}
procedure TALCustomScrollBox.InternalMouseLeave;
begin
  {$IFDEF DEBUG}
  //ALLog(ClassName + '.InternalMouseLeave');
  {$ENDIF}
  if FMouseEvents then begin
    FScrollCapturedByMe := False;
    ScrollEngine.MouseLeave;
    FMouseEvents := False;
  end;
end;

{*********************************************************************************************}
procedure TALCustomScrollBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  InternalMouseDown(Button, Shift, X, Y);
end;

{***********************************************************************}
procedure TALCustomScrollBox.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  InternalMouseMove(Shift, X, Y);
  inherited;
end;

{*******************************************************************************************}
procedure TALCustomScrollBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  InternalMouseUp(Button, Shift, X, Y);
end;

{****************************************}
procedure TALCustomScrollBox.DoMouseLeave;
begin
  inherited;
  InternalMouseLeave;
end;

{*************}
{$IFNDEF ALDPK}
procedure TALCustomScrollBox.ChildrenMouseDown(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not aObject.AutoCapture then begin
    {$IF defined(MSWindows)}
    // On Windows, calling doCapture will invoke Winapi.Windows.SetCapture(FormToHWND(AForm));
    // This action deactivates some functionalities in the native control, such as the right-click menu.
    if not Supports(aObject, IALNativeControl) then
    {$ENDIF}
      _TControlAccessProtected(aObject).capture;
  end;
  var P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
  InternalMouseDown(Button, Shift, P.X, P.Y);
  inherited;
end;
{$ENDIF}

{*************}
{$IFNDEF ALDPK}
procedure TALCustomScrollBox.ChildrenMouseMove(const AObject: TControl; Shift: TShiftState; X, Y: Single);
begin
  var P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
  InternalMouseMove(Shift, P.X, P.Y);
  inherited;
end;
{$ENDIF}

{*************}
{$IFNDEF ALDPK}
procedure TALCustomScrollBox.ChildrenMouseUp(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not aObject.AutoCapture then begin
    {$IF defined(MSWindows)}
    // On Windows, calling doCapture will invoke Winapi.Windows.SetCapture(FormToHWND(AForm));
    // This action deactivates some functionalities in the native control, such as the right-click menu.
    if not Supports(aObject, IALNativeControl) then
    {$ENDIF}
      _TControlAccessProtected(aObject).releasecapture;
  end;
  var P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
  InternalMouseUp(Button, Shift, P.X, P.Y);
  inherited;
end;
{$ENDIF}

{*************}
{$IFNDEF ALDPK}
procedure TALCustomScrollBox.ChildrenMouseLeave(const AObject: TControl);
begin
  InternalMouseLeave;
  inherited;
end;
{$ENDIF}

{*****************************************************************************************************}
procedure TALCustomScrollBox.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
  if (not (Handled or FDisableMouseWheel)) then begin
    if ssHorizontal in Shift then begin
      if FContent.Width > Width then begin
        var Offset: Single := (Width / 5) * -1 * (WheelDelta / 120);
        ScrollEngine.MouseWheel(Offset, 0);
        Handled := True;
      end;
    end
    else if FContent.Height > Height then begin
      var Offset: Single := (Height / 5) * -1 * (WheelDelta / 120);
      ScrollEngine.MouseWheel(0, Offset);
      Handled := True;
    end
    else if FContent.Width > Width then begin
      var Offset: Single := (Width / 5) * -1 * (WheelDelta / 120);
      ScrollEngine.MouseWheel(Offset, 0);
      Handled := True;
    end;
  end;
end;

{******************************************************************}
procedure TALCustomScrollBox.DoAddObject(const AObject: TFmxObject);
begin
  if (FContent <> nil) and
     (AObject <> FContent) and
     (AObject is TControl) and
     //(not (AObject is TEffect)) and
     //(not (AObject is TAnimation)) and
     (not (AObject is TScrollBar)) then FContent.AddObject(AObject)
  else inherited;
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
    fAnchoredContentOffset.X := (Width - result.Width) / 2;
  end
  else Result.Width := Width;
end;

{*********************************************************************************************************}
function TALVertScrollBox.CreateScrollBar(const aOrientation: TOrientation): TALCustomScrollBox.TScrollBar;
begin
  if aOrientation <> TOrientation.Vertical then exit(nil);
  result := inherited CreateScrollBar(aOrientation);
end;

{*****************************************************************************}
function TALVertScrollBox.CreateScrollEngine: TALCustomScrollBox.TScrollEngine;
begin
  result := inherited CreateScrollEngine;
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
    fAnchoredContentOffset.Y := (Height - result.Height) / 2;
  end
  else Result.Height := Height;
end;

{*********************************************************************************************************}
function TALHorzScrollBox.CreateScrollBar(const aOrientation: TOrientation): TALCustomScrollBox.TScrollBar;
begin
  if aOrientation <> TOrientation.Horizontal then exit(nil);
  result := inherited CreateScrollBar(aOrientation);
end;

{*****************************************************************************}
function TALHorzScrollBox.CreateScrollEngine: TALCustomScrollBox.TScrollEngine;
begin
  result := inherited CreateScrollEngine;
  result.TouchTracking := result.TouchTracking - [ttVertical];
end;

{*******************************}
procedure TALHorzScrollBox.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALLayout, TALScrollBox, TALVertScrollBox, TALHorzScrollBox]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALLayout, 'Size');
  UnlistPublishedProperty(TALLayout, 'StyleName');
  UnlistPublishedProperty(TALLayout, 'OnTap');
  //--
  UnlistPublishedProperty(TALScrollBox, 'Size');
  UnlistPublishedProperty(TALScrollBox, 'StyleName');
  UnlistPublishedProperty(TALScrollBox, 'OnTap');
  //--
  UnlistPublishedProperty(TALVertScrollBox, 'Size');
  UnlistPublishedProperty(TALVertScrollBox, 'StyleName');
  UnlistPublishedProperty(TALVertScrollBox, 'OnTap');
  //--
  UnlistPublishedProperty(TALHorzScrollBox, 'Size');
  UnlistPublishedProperty(TALHorzScrollBox, 'StyleName');
  UnlistPublishedProperty(TALHorzScrollBox, 'OnTap');
  //--
  UnlistPublishedProperty(TALCustomScrollBox.TScrollBar, 'Size');
  UnlistPublishedProperty(TALCustomScrollBox.TScrollBar, 'StyleName');
  UnlistPublishedProperty(TALCustomScrollBox.TScrollBar, 'OnTap');
  {$ENDIF}
end;

initialization
  RegisterFmxClasses([TALLayout, TALScrollBox, TALVertScrollBox, TALHorzScrollBox]);

end.
