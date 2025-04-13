unit Alcinoe.FMX.Layouts;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported123}
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
  ALcinoe.Common,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Objects,
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

  {****************************************************************}
  TALCustomScrollBox = class(TALBaseRectangle, IALScrollableControl)
  public
    type
      // -----
      // TFill
      TFill = class(TALBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      // -------
      // TStroke
      TStroke = class(TALStrokeBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      // --------
      // TContent
      TContent = class(TALContent)
      private
        FScrollBox: TALCustomScrollBox;
      protected
        procedure DoContentChanged; override;
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
        FScrollBox: TALCustomScrollBox; // 8 bytes
        fLastViewportPosition: TALPointD; // 16 bytes
      protected
        procedure DoStart; override;
        procedure DoStop; override;
        procedure DoChanged; override;
      public
        constructor Create(const AScrollBox: TALCustomScrollBox); reintroduce;
        property ScrollBox: TALCustomScrollBox read FScrollBox;
      end;
      // ----------
      // TScrollBar
      TScrollBar = class(TALCustomScrollBar)
      public
        type
          TFadeMode = (Disabled, Enabled, Auto);
        const
          DefaultFadeDuration = 0.25; // From Android scrollbarFadeDuration
          DefaultFadeDelay = 0; // instead of 0.3 From Android scrollbarDefaultDelayBeforeFade
      private
        FScrollBox: TALCustomScrollBox; // 8 Bytes
        FFadeAnimation: TALFloatPropertyAnimation; // 8 Bytes;
        FExplicitVisible: Boolean; // 1 byte
        FFadeMode: TFadeMode; // 1 byte
        FFadeDuration: Single; // 4 bytes
        FFadeDelay: Single; // 4 bytes
        function GetFadeEnabled: Boolean;
        function IsFadeDurationStored: boolean;
        function IsFadeDelayStored: boolean;
        procedure FadeAnimationFinish(Sender: TObject);
      protected
        function GetDefaultSize: TSizeF; override;
        procedure SetVisible(const Value: Boolean); override;
        procedure DoChanged; override;
        procedure Resize; override;
      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure BeforeDestruction; override;
        property FadeEnabled: boolean read GetFadeEnabled;
        property Locked stored false;
        property Min stored false;
        property Max stored false;
        property Orientation stored false;
        property Position stored false;
        property Value stored false;
        property ViewportSize stored false;
        property Opacity stored false;
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
        property FadeMode: TFadeMode read FFadeMode write FFadeMode default TFadeMode.Auto;
        property FadeDuration: Single read FFadeDuration write FFadeDuration stored IsFadeDurationStored nodefault;
        property FadeDelay: Single read FFadeDelay write FFadeDelay stored IsFadeDelayStored nodefault;
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
        property Visible;
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
      TViewportPositionChangeEvent = procedure (Sender: TObject; const OldViewportPosition, NewViewportPosition: TALPointD) of object;
  private
    FScrollEngine: TScrollEngine;
    FContent: TContent;
    FHScrollBar: TScrollBar;
    FVScrollBar: TScrollBar;
    FDisableMouseWheel: Boolean;
    FDisableScrollChange: Boolean;
    FHandleMouseEvents: Boolean;
    FOnViewportPositionChange: TViewportPositionChangeEvent;
    fOnAniStart: TnotifyEvent;
    fOnAniStop: TnotifyEvent;
    fMouseDownPos: TpointF;
    fScrollCapturedByMe: boolean;
    fMaxContentWidth: Single;
    fMaxContentHeight: single;
    FContentSideMargin: TPointF;
    procedure ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
    function IsMaxContentWidthStored: Boolean;
    function IsMaxContentHeightStored: Boolean;
    procedure InternalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Single);
    procedure InternalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure InternalMouseLeave;
    { IALScrollableControl }
    function GetScrollEngine: TALScrollEngine;
    procedure SetScrollEngine(const Value: TALScrollEngine);
  protected
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRealign; override;
    function GetDefaultSize: TSizeF; override;
    function CreateFill: TALBrush; override;
    function CreateStroke: TALStrokeBrush; override;
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
    procedure BeforeDestruction; override;
    property ScrollEngine: TALScrollEngine read GetScrollEngine write SetScrollEngine;
    procedure Sort(Compare: TFmxObjectSortCompare); override;
    procedure ScrollBy(const Dx, Dy: Single);
    function GetTabList: ITabList; override;
    property Content: TContent read FContent;
    property DisableMouseWheel: Boolean read FDisableMouseWheel write FDisableMouseWheel default False;
    property OnViewportPositionChange: TViewportPositionChangeEvent read FOnViewportPositionChange write FOnViewportPositionChange;
    property ClipChildren default true;
    property OnAniStart: TnotifyEvent read fOnAniStart write fOnAniStart;
    property OnAniStop: TnotifyEvent read fOnAniStop write fOnAniStop;
    property CacheEngine;
    property CacheIndex;
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
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DisableMouseWheel;
    property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Fill;
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
    property ScrollEngine;
    property Sides;
    property Size;
    property Stroke;
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
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DisableMouseWheel;
    property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Fill;
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
    property ScrollEngine;
    property Sides;
    property Size;
    property Stroke;
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
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DisableMouseWheel;
    property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Fill;
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
    property ScrollEngine;
    property Sides;
    property Size;
    property Stroke;
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
  {$IF defined(MSWindows)}
  Winapi.Windows,
  FMX.Platform.Win,
  {$ENDIF}
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  FMX.Platform,
  FMX.Consts,
  FMX.Effects,
  FMX.utils,
  FMX.Ani,
  Alcinoe.StringUtils;

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

{******************************************************}
function TALCustomScrollBox.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{********************************************************}
function TALCustomScrollBox.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{*****************************************************************}
constructor TALCustomScrollBox.TContent.Create(AOwner: TComponent);
begin
  ValidateInheritance(AOwner, TALCustomScrollBox, False{CanBeNil});
  inherited Create(AOwner);
  FScrollBox := TALCustomScrollBox(AOwner);
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
                (AObject.Position.X + (AObject.Width * _TControlAccessProtected(AObject).scale.x) > -Position.X)
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

{*****************************************************}
procedure TALCustomScrollBox.TContent.DoContentChanged;
begin
  FScrollBox.Realign;
end;

{****************************************************************************************}
constructor TALCustomScrollBox.TScrollEngine.Create(const AScrollBox: TALCustomScrollBox);
begin
  inherited Create;
  FScrollBox := AScrollBox;
  fLastViewportPosition := TALPointD.Create(0,0);
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
                                                ViewportPosition.X - FScrollBox.FContentSideMargin.X,
                                                ViewportPosition.Y - FScrollBox.FContentSideMargin.Y);
      finally
        FScrollBox.FDisableAlign := LSaveDisableAlign;
      end;
    end;

    // Update the vertical and horizontal scroll bars (VScrollBar/HScrollBar)
    if not FScrollBox.FDisableScrollChange then begin
      FScrollBox.FDisableScrollChange := True;
      try
        if FScrollBox.VScrollBar <> nil then FScrollBox.VScrollBar.Value := ViewportPosition.Y;
        if FScrollBox.HScrollBar <> nil then FScrollBox.HScrollBar.Value := ViewportPosition.X;
      finally
        FScrollBox.FDisableScrollChange := False;
      end;
    end;

    // Trigger the OnViewportPositionChange event
    var LNewViewportPosition := ViewportPosition;
    if (assigned(FScrollBox.FOnViewportPositionChange)) and
       (not fLastViewportPosition.EqualsTo(LNewViewportPosition, TEpsilon.Position)) then
      FScrollBox.FOnViewportPositionChange(self, fLastViewportPosition, LNewViewportPosition);
    fLastViewportPosition := LNewViewportPosition;

  end;
  inherited DoChanged;
end;

{*******************************************************************}
constructor TALCustomScrollBox.TScrollBar.Create(AOwner: TComponent);
begin
  ValidateInheritance(AOwner, TALCustomScrollBox, False{CanBeNil});
  inherited Create(AOwner);
  FScrollBox := TALCustomScrollBox(AOwner);
  FFadeAnimation := TALFloatPropertyAnimation.Create(Self);
  FFadeAnimation.Parent := Self;
  FFadeAnimation.PropertyName := 'Opacity';
  FFadeAnimation.OnFinish := FadeAnimationFinish;
  FExplicitVisible := Visible;
  FFadeMode := TFadeMode.Auto;
  FFadeDuration := DefaultFadeDuration;
  FFadeDelay := DefaultFadeDelay;
  HitTest := False;
end;

{***********************************************}
destructor TALCustomScrollBox.TScrollBar.Destroy;
begin
  ALFreeAndNil(FFadeAnimation);
  inherited;
end;

{***********************************************}
procedure TALCustomScrollBox.TScrollBar.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  // This must be done in BeforeDestruction rather than in Destroy,
  // because the control might be freed in the background via ALFreeAndNil(..., delayed),
  // and BeforeDestruction is guaranteed to execute on the main thread.
  FFadeAnimation.Enabled := False;
  Inherited;
end;

{*************************************************************}
function TALCustomScrollBox.TScrollBar.GetFadeEnabled: Boolean;
begin
  case FFadeMode of
    TFadeMode.Disabled: Result := False;
    TFadeMode.Enabled: Result := True;
    TFadeMode.Auto: Result := ALGetHasTouchScreen;
    else Raise Exception.Create('Error A6F89CC3-8E1C-468C-A6D2-3347C4C0AE6A')
  end;
end;

{*******************************************************************}
function TALCustomScrollBox.TScrollBar.IsFadeDurationStored: boolean;
begin
  result := not samevalue(FFadeDuration, DefaultFadeDuration, TEpsilon.Vector);
end;

{****************************************************************}
function TALCustomScrollBox.TScrollBar.IsFadeDelayStored: boolean;
begin
  result := not samevalue(FFadeDelay, DefaultFadeDelay, TEpsilon.Vector);
end;

{************************************************************}
function TALCustomScrollBox.TScrollBar.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(150, 4);
end;

{***********************************************************************}
procedure TALCustomScrollBox.TScrollBar.SetVisible(const Value: Boolean);
begin
  if not FScrollBox.fDisableAlign then
    FExplicitVisible := Value;
  inherited;
end;

{************************************************************}
procedure TALCustomScrollBox.TScrollBar.FadeAnimationFinish(Sender: TObject);
begin
  if FFadeAnimation.StopValue = 1{FadeIn} then begin
    FFadeAnimation.StartFromCurrent := True;
    FFadeAnimation.StopValue := 0;
    FFadeAnimation.Delay := FFadeDelay;
    FFadeAnimation.Duration := FFadeDuration;
    FFadeAnimation.Start;
  end
end;

{************************************************}
procedure TALCustomScrollBox.TScrollBar.DoChanged;
begin
  If FadeEnabled and (not FScrollBox.fDisableAlign) then begin
    If not FFadeAnimation.Running then begin
      FFadeAnimation.StartFromCurrent := True;
      FFadeAnimation.StopValue := 1;
      FFadeAnimation.Delay := 0;
      FFadeAnimation.Duration := FFadeDuration;
      FFadeAnimation.Start;
    end
    else If FFadeAnimation.StopValue = 0{FadeOut} then begin
      FFadeAnimation.StopAtCurrent;
      FFadeAnimation.StartFromCurrent := True;
      FFadeAnimation.StopValue := 1;
      FFadeAnimation.Delay := 0;
      FFadeAnimation.Duration := 1 - FFadeAnimation.CurrentTime;
      FFadeAnimation.Start;
    end;
  end;
  //--
  if FScrollBox.FDisableScrollChange then exit;
  FScrollBox.FDisableScrollChange := True;
  try
    if Orientation=TOrientation.vertical then FScrollBox.fScrollEngine.ViewportPosition := TAlPointD.Create(FScrollBox.fScrollEngine.ViewportPosition.x, Value)
    else FScrollBox.fScrollEngine.ViewportPosition := TAlPointD.Create(Value, FScrollBox.fScrollEngine.ViewportPosition.Y);
  finally
    FScrollBox.FDisableScrollChange := False;
  end;
  //--
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
  Touch.DefaultInteractiveGestures := Touch.DefaultInteractiveGestures + [TInteractiveGesture.Pan];
  Touch.InteractiveGestures := Touch.InteractiveGestures + [TInteractiveGesture.Pan];
  FDisableScrollChange := False;
  FDisableMouseWheel := False;
  FHandleMouseEvents := False;
  FOnViewportPositionChange := nil;
  fOnAniStart := nil;
  fOnAniStop := nil;
  fScrollEngine := CreateScrollEngine;
  fMaxContentWidth := 0;
  fMaxContentHeight := 0;
  FContentSideMargin := TpointF.Create(0,0);
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
  ALFreeAndNil(FScrollEngine);
  inherited Destroy;
end;

{**********************************************}
procedure TALCustomScrollBox.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  // Unsubscribe from TALScrollCapturedMessage to stop receiving messages.
  // This must be done in BeforeDestruction rather than in Destroy,
  // because the control might be freed in the background via ALFreeAndNil(..., delayed),
  // and BeforeDestruction is guaranteed to execute on the main thread.
  TMessageManager.DefaultManager.Unsubscribe(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  FScrollEngine.Stop(true{AAbruptly});
  inherited;
end;

{***********************************************************}
function TALCustomScrollBox.IsMaxContentWidthStored: Boolean;
begin
  result := not sameValue(fMaxContentWidth, 0, Tepsilon.Position);
end;

{************************************************************}
function TALCustomScrollBox.IsMaxContentHeightStored: Boolean;
begin
  result := not sameValue(fMaxContentHeight, 0, Tepsilon.Position);
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
  FContentSideMargin := TpointF.Create(0, 0);
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
      fVScrollBar.BeginUpdate;
      try
        fVScrollBar.Enabled := AContentRect.Height > Height;
        if not fVScrollBar.FFadeAnimation.Running then begin
          if fVScrollBar.FadeEnabled then fVScrollBar.Opacity := 0
          else fVScrollBar.Opacity := 1;
        end;
        fVScrollBar.Visible := fVScrollBar.FExplicitVisible and ((AContentRect.Height > Height) or (not fVScrollBar.FadeEnabled));
        fVScrollBar.Min := 0;
        fVScrollBar.Max := AContentRect.Height;
        fVScrollBar.ViewportSize := height;
        fVScrollBar.SetBounds(
          width - fVScrollBar.Width - fVScrollBar.Margins.Right,
          fVScrollBar.Margins.top,
          fVScrollBar.Width,
          height-fVScrollBar.Margins.top-fVScrollBar.Margins.bottom);
      finally
        fVScrollBar.EndUpdate;
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateHScrollBar(const AContentRect: TRectF);
  begin
    if fHScrollBar <> nil then begin
      fHScrollBar.BeginUpdate;
      try
        fHScrollBar.Enabled := AContentRect.Width > Width;
        if not fHScrollBar.FFadeAnimation.Running then begin
          if fHScrollBar.FadeEnabled then fHScrollBar.Opacity := 0
          else fHScrollBar.Opacity := 1;
        end;
        fHScrollBar.Visible := fHScrollBar.FExplicitVisible and ((AContentRect.Width > Width) or (not fHScrollBar.FadeEnabled));
        fHScrollBar.Min := 0;
        fHScrollBar.Max := AContentRect.Width;
        fHScrollBar.ViewportSize := Width;
        fHScrollBar.SetBounds(
          fHScrollBar.Margins.left,
          height - fHScrollBar.Height - fHScrollBar.Margins.bottom,
          width - fHScrollBar.Margins.left - fHScrollBar.Margins.right,
          fHScrollBar.Height);
      finally
        fHScrollBar.EndUpdate;
      end;
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

  if FContent = nil then exit;
  if fDisableAlign then exit;
  var LDoRealignAgain := False;
  fDisableAlign := True;
  try
    var LContentRect := CalcContentBounds;
    Content.SetBounds(
      -fScrollEngine.ViewportPosition.X + FContentSideMargin.X,
      -fScrollEngine.ViewportPosition.Y + FContentSideMargin.Y,
      LContentRect.Width,
      LContentRect.Height);
    if LContentRect.EqualsTo(CalcContentBounds, Tepsilon.Position) then begin
      _UpdateVScrollBar(LContentRect);
      _UpdateHScrollBar(LContentRect);
      _UpdateScrollEngineLimits(LContentRect);
      fScrollEngine.DoChanged;
    end
    else LDoRealignAgain := True;
  finally
    fDisableAlign := false;
  end;

  // Imagine that the content initially has a width and height of 50px, matching the initial dimensions
  // of its owner, TALVertScrollBox. In this scenario, all controls are initially aligned to 50px in width and height,
  // and CalcContentBounds returns bounds based on that alignment, while also expanding the bounds to match the
  // local rectangle of the TALVertScrollBox owner (for example, 380px wide). After calling Content.SetBounds
  // with a width of 380px, some controls with AutoSize set to true (such as TALText) might adjust their height,
  // causing CalcContentBounds to return more accurate bounds. Therefore, we must reapply the alignment.
  if LDoRealignAgain then DoRealign;

end;

{**********************************************}
function TALCustomScrollBox.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(200, 200);
end;

{****************************************}
function TALCustomScrollBox.CreateFill: TALBrush;
begin
  result := TFill.Create;
end;

{************************************************}
function TALCustomScrollBox.CreateStroke: TALStrokeBrush;
begin
  result := TStroke.Create;
end;

{****************************************************************************************}
function TALCustomScrollBox.CreateScrollBar(const aOrientation: TOrientation): TScrollBar;
begin
  Result := TScrollBar.Create(self);
  Result.Parent := self;
  Result.Stored := False;
  Result.SetSubComponent(True);
  Result.Locked := True;
  Result.Orientation := aOrientation;
  // At design time on Windows, this property defaults to True, which means it is not stored in the DFM.
  // Only a False value is saved in the DFM and can thus override the default.
  // However, since Thumb.HitTest is not published, its value is never saved in the DFM.
  result.Thumb.HitTest := not ALGetHasTouchScreen;
  if aOrientation = TOrientation.Vertical then Result.Name := 'VScrollBar'
  else Result.Name := 'HScrollBar';
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
end;

{***********************************************************}
function TALCustomScrollBox.GetScrollEngine: TALScrollEngine;
begin
  result := FScrollEngine;
end;

{********************************************}
procedure TALCustomScrollBox.SetScrollEngine(const Value: TALScrollEngine);
begin
  FScrollEngine.Assign(Value);
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
      FHandleMouseEvents := False;
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
    FHandleMouseEvents := true;
    fMouseDownPos := TpointF.Create(X,Y);
    {$IF defined(ANDROID) or defined(IOS)}
    if form <> nil then
      ScrollEngine.MouseDown(form.Handle);
    {$ELSE}
    ScrollEngine.MouseDown(X, Y);
    {$ENDIF}
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
  if FHandleMouseEvents then begin
    if (not fScrollCapturedByMe) and
       (fScrollEngine.TouchEnabled) and
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
    {$IF defined(ANDROID) or defined(IOS)}
    if form <> nil then
      ScrollEngine.MouseMove(form.Handle);
    {$ELSE}
    ScrollEngine.MouseMove(X, Y);
    {$ENDIF}
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
  if FHandleMouseEvents and (Button = TMouseButton.mbLeft) then begin
    {$IF defined(ANDROID) or defined(IOS)}
    if form <> nil then
      ScrollEngine.MouseUp(form.Handle);
    {$ELSE}
    ScrollEngine.MouseUp(X, Y);
    {$ENDIF}
    FScrollCapturedByMe := False;
    FHandleMouseEvents := False;
  end;
end;

{**********************************************}
procedure TALCustomScrollBox.InternalMouseLeave;
begin
  {$IFDEF DEBUG}
  //ALLog(ClassName + '.InternalMouseLeave');
  {$ENDIF}
  if FHandleMouseEvents then begin
    ScrollEngine.MouseLeave;
    FScrollCapturedByMe := False;
    FHandleMouseEvents := False;
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
  // Inherited at the end because of
  // https://github.com/MagicFoundation/Alcinoe/issues/381
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
    FContentSideMargin.X := (Width - result.Width) / 2;
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
  result.TouchTracking := [ttVertical];
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
    FContentSideMargin.Y := (Height - result.Height) / 2;
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
  result.TouchTracking := [ttHorizontal];
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
