unit Alcinoe.FMX.Controls;

interface

{$I Alcinoe.inc}

uses
  System.UITypes,
  system.Types,
  System.Classes,
  FMX.Forms,
  FMX.Controls,
  FMX.Types;

type

  {**************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Types.TAlignLayout was not updated and adjust the IFDEF'}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2342 was implemented and adjust the IFDEF'}
  {$ENDIF}
  TALAlignLayout = (
    None,
    Top,
    Left,
    Right,
    Bottom,
    MostTop,
    MostBottom,
    MostLeft,
    MostRight,
    Client,
    Contents,
    Center,
    VertCenter,
    HorzCenter,
    Horizontal,
    Vertical,
    Scale,
    Fit,
    FitLeft,
    FitRight,
    TopCenter,    // Added from TAlignLayout - Works like TAlignLayout.Top, then centers the control horizontally
    TopLeft,      // Added from TAlignLayout - Works like TAlignLayout.Top, then aligns the control to the left.
    TopRight,     // Added from TAlignLayout - Works like TAlignLayout.Top, then aligns the control to the right.
    LeftCenter,   // Added from TAlignLayout - Works like TAlignLayout.Left, then centers the control vertically.
    LeftTop,      // Added from TAlignLayout - Works like TAlignLayout.Left, then aligns the control to the top.
    LeftBottom,   // Added from TAlignLayout - Works like TAlignLayout.Left, then aligns the control to the bottom.
    RightCenter,  // Added from TAlignLayout - Works like TAlignLayout.Right, then centers the control vertically.
    RightTop,     // Added from TAlignLayout - Works like TAlignLayout.Right, then aligns the control to the top.
    RightBottom,  // Added from TAlignLayout - Works like TAlignLayout.Right, then aligns the control to the bottom.
    BottomCenter, // Added from TAlignLayout - Works like TAlignLayout.Bottom, then centers the control horizontally
    BottomLeft,   // Added from TAlignLayout - Works like TAlignLayout.Bottom, then aligns the control to the left.
    BottomRight); // Added from TAlignLayout - Works like TAlignLayout.Bottom, then aligns the control to the right.

  {**************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl was not updated and adjust the IFDEF'}
  {$ENDIF}
  TALControl = class(TControl)
  private
    FForm: TCommonCustomForm;
    FFocusOnMouseDown: Boolean;
    FFocusOnMouseUp: Boolean;
    FControlAbsolutePosAtMouseDown: TpointF;
    FMouseDownAtLowVelocity: Boolean;
    FDisableDoubleClickHandling: Boolean;
    FIsPixelAlignmentEnabled: Boolean;
    FFormerMarginsChangedHandler: TNotifyEvent;
    FAlign: TALAlignLayout;
    FIsAligning: Boolean;
    function GetPressed: Boolean;
    procedure SetPressed(const AValue: Boolean);
    procedure DelayOnResize(Sender: TObject);
    procedure DelayOnResized(Sender: TObject);
    procedure MarginsChangedHandler(Sender: TObject);
  protected
    function GetIsPixelAlignmentEnabled: Boolean; virtual;
    procedure SetIsPixelAlignmentEnabled(const AValue: Boolean); Virtual;
    property FocusOnMouseDown: Boolean read FFocusOnMouseDown write FFocusOnMouseDown;
    property FocusOnMouseUp: Boolean read FFocusOnMouseUp write FFocusOnMouseUp;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    function GetParentedVisible: Boolean; override;
    procedure DoRootChanged; override;
    procedure IsMouseOverChanged; virtual;
    procedure IsFocusedChanged; virtual;
    procedure PressedChanged; virtual;
    Procedure MarginsChanged; Virtual;
    procedure Loaded; override;
    function IsOwnerLoading: Boolean;
    function IsSizeStored: Boolean; override;
    function GetAlign: TALAlignLayout; Reintroduce;
    procedure SetAlign(const Value: TALAlignLayout); Reintroduce; virtual;
    procedure DoRealign; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetNewScene(AScene: IScene); override;
    function IsVisibleWithinFormBounds: Boolean;
    property Form: TCommonCustomForm read FForm;
    property DisableDoubleClickHandling: Boolean read FDisableDoubleClickHandling write FDisableDoubleClickHandling;
    {$IFNDEF ALCompilerVersionSupported122}
      {$MESSAGE WARN 'Check if property FMX.Controls.TControl.Pressed still not fire a PressChanged event when it gets updated, and adjust the IFDEF'}
    {$ENDIF}
    property Pressed: Boolean read GetPressed write SetPressed;
    procedure AlignToPixel; virtual;
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    property IsPixelAlignmentEnabled: Boolean read GetIsPixelAlignmentEnabled write SetIsPixelAlignmentEnabled;
    property Align: TALAlignLayout read FAlign write SetAlign default TALAlignLayout.None;
  end;

  {**************************************}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Controls.TContent was not updated and adjust the IFDEF'}
  {$ENDIF}
  TALContent = class(TALControl, IContent)
  private
    FParentAligning: Boolean;
  protected
    function GetTabStopController: ITabStopController; override;
    procedure DoRealign; override;
    procedure IContent.Changed = ContentChanged;
    procedure ContentChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function GetTabListClass: TTabListClass; override;
  published
    //property Action;
    property Align;
    //property Anchors;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClipChildren default False;
    //property ClipParent;
    property Cursor;
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
    property RotationCenter;
    property Scale;
    property Size;
    //property TabOrder;
    //property TabStop;
    //property TouchTargetExpansion;
    property Visible;
    property Width;
    property OnCanFocus;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
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

implementation

uses
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.ScrollEngine,
  Alcinoe.StringUtils,
  Alcinoe.Common;

{************************************************}
constructor TALControl.Create(AOwner: TComponent);
begin
  inherited;
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if MarginsChanged is not implemented in FMX.Controls.TControl and adjust the IFDEF'}
  {$ENDIF}
  FFormerMarginsChangedHandler := Margins.OnChange;
  Margins.OnChange := MarginsChangedHandler;
  Size.SetPlatformDefaultWithoutNotification(False);
  FForm := nil;
  FFocusOnMouseDown := False;
  FFocusOnMouseUp := False;
  FControlAbsolutePosAtMouseDown := TpointF.zero;
  FMouseDownAtLowVelocity := True;
  // Double-clicks, or double-taps, are rarely used in mobile design due to
  // touch screen challenges and user experience considerations. Mobile devices
  // favor simpler, more intuitive gestures like swiping and pinching, which are
  // better suited to smaller screens and prevent confusion with similar
  // actions. Consequently, functionalities often tied to double-clicks on
  // desktops are handled by different gestures or interface elements in
  // mobile apps, leading to a more user-friendly experience.
  FDisableDoubleClickHandling := True;
  FIsPixelAlignmentEnabled := True;
  FAlign := TALAlignLayout.None;
  FIsAligning := False;
end;

{**************************}
procedure TALControl.Loaded;
begin
  {$IF not DEFINED(ALDPK)}
  if IsPixelAlignmentEnabled then
    AlignToPixel;
  {$ENDIF}
  Inherited;
end;

{******************************************}
function TALControl.IsOwnerLoading: Boolean;
begin
  result := (Owner <> nil) and
            (csloading in Owner.ComponentState);
end;

{****************************************}
function TALControl.IsSizeStored: Boolean;
begin
  var LDefaultSize := GetDefaultSize;
  result := (not SameValue(LDefaultSize.cx, Size.Size.cx, TEpsilon.Position)) or
            (not SameValue(LDefaultSize.cy, Size.Size.cy, TEpsilon.Position));
end;

{*******************************************}
function TALControl.GetAlign: TALAlignLayout;
begin
  Result := FAlign;
end;

{*********************************************************}
procedure TALControl.SetAlign(const Value: TALAlignLayout);
begin
  If FAlign <> Value then begin
    FAlign := Value;
    var LLegacyAlign: TAlignLayout;
    Case Value of
      TALAlignLayout.None: LLegacyAlign := TAlignLayout.None;
      TALAlignLayout.Top: LLegacyAlign := TAlignLayout.Top;
      TALAlignLayout.Left: LLegacyAlign := TAlignLayout.Left;
      TALAlignLayout.Right: LLegacyAlign := TAlignLayout.Right;
      TALAlignLayout.Bottom: LLegacyAlign := TAlignLayout.Bottom;
      TALAlignLayout.MostTop: LLegacyAlign := TAlignLayout.MostTop;
      TALAlignLayout.MostBottom: LLegacyAlign := TAlignLayout.MostBottom;
      TALAlignLayout.MostLeft: LLegacyAlign := TAlignLayout.MostLeft;
      TALAlignLayout.MostRight: LLegacyAlign := TAlignLayout.MostRight;
      TALAlignLayout.Client: LLegacyAlign := TAlignLayout.Client;
      TALAlignLayout.Contents: LLegacyAlign := TAlignLayout.Contents;
      TALAlignLayout.Center: LLegacyAlign := TAlignLayout.Center;
      TALAlignLayout.VertCenter: LLegacyAlign := TAlignLayout.VertCenter;
      TALAlignLayout.HorzCenter: LLegacyAlign := TAlignLayout.HorzCenter;
      TALAlignLayout.Horizontal: LLegacyAlign := TAlignLayout.Horizontal;
      TALAlignLayout.Vertical: LLegacyAlign := TAlignLayout.Vertical;
      TALAlignLayout.Scale: LLegacyAlign := TAlignLayout.Scale;
      TALAlignLayout.Fit: LLegacyAlign := TAlignLayout.Fit;
      TALAlignLayout.FitLeft: LLegacyAlign := TAlignLayout.FitLeft;
      TALAlignLayout.FitRight: LLegacyAlign := TAlignLayout.FitRight;
      TALAlignLayout.TopCenter: LLegacyAlign := TAlignLayout.Top;
      TALAlignLayout.TopLeft: LLegacyAlign := TAlignLayout.Top;
      TALAlignLayout.TopRight: LLegacyAlign := TAlignLayout.Top;
      TALAlignLayout.LeftCenter: LLegacyAlign := TAlignLayout.Left;
      TALAlignLayout.LeftTop: LLegacyAlign := TAlignLayout.Left;
      TALAlignLayout.LeftBottom: LLegacyAlign := TAlignLayout.Left;
      TALAlignLayout.RightCenter: LLegacyAlign := TAlignLayout.Right;
      TALAlignLayout.RightTop: LLegacyAlign := TAlignLayout.Right;
      TALAlignLayout.RightBottom: LLegacyAlign := TAlignLayout.Right;
      TALAlignLayout.BottomCenter: LLegacyAlign := TAlignLayout.Bottom;
      TALAlignLayout.BottomLeft: LLegacyAlign := TAlignLayout.Bottom;
      TALAlignLayout.BottomRight: LLegacyAlign := TAlignLayout.Bottom;
      else Raise Exception.Create('Error D527A470-23AC-4E3C-BCC5-4C2DB578A691');
    end;
    Inherited SetAlign(LLegacyAlign);
  end;
end;

{*****************************}
procedure TALControl.DoRealign;
begin
  if not FNeedAlign then
    Exit;
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-15768 was implemented and adjust the IFDEF'}
  {$ENDIF}
  var LOriginalSize: TPointF := Size.Size; // https://quality.embarcadero.com/browse/RSP-15768
  FIsAligning := True;
  Try
    inherited;
  finally
    FIsAligning := False;
  end;
  if not LOriginalSize.EqualsTo(Size.Size) then DoRealign; // https://quality.embarcadero.com/browse/RSP-15768
end;

{************************************************************}
procedure TALControl.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2342 was implemented and adjust the IFDEF'}
  {$ENDIF}
  If (FAlign in [TALAlignLayout.TopCenter,
                 TALAlignLayout.TopLeft,
                 TALAlignLayout.TopRight,
                 TALAlignLayout.LeftCenter,
                 TALAlignLayout.LeftTop,
                 TALAlignLayout.LeftBottom,
                 TALAlignLayout.RightCenter,
                 TALAlignLayout.RightTop,
                 TALAlignLayout.RightBottom,
                 TALAlignLayout.BottomCenter,
                 TALAlignLayout.BottomLeft,
                 TALAlignLayout.BottomRight]) and
     (ParentControl is TALControl) and
     (TALControl(ParentControl).FIsAligning) then begin
    case FAlign of
      TALAlignLayout.TopCenter,
      TALAlignLayout.BottomCenter: begin
        X := X + ((AWidth - Width) / 2);
        AWidth := Width;
      end;
      TALAlignLayout.TopLeft,
      TALAlignLayout.BottomLeft: begin
        AWidth := Width;
      end;
      TALAlignLayout.TopRight,
      TALAlignLayout.BottomRight: begin
        X := X + (AWidth - Width);
        AWidth := Width;
      end;
      TALAlignLayout.LeftCenter,
      TALAlignLayout.RightCenter: begin
        Y := Y + ((AHeight - Height) / 2);
        AHeight := Height;
      end;
      TALAlignLayout.LeftTop,
      TALAlignLayout.RightTop: begin
        AHeight := Height;
      end;
      TALAlignLayout.LeftBottom,
      TALAlignLayout.RightBottom: begin
        Y := Y + (AHeight - Height);
        AHeight := Height;
      end;
      else
        raise Exception.Create('Error 9431A388-3F2F-4F06-8296-210708F60C66');
    end;
  end;
  inherited;
end;

{********************************}
procedure TALControl.AlignToPixel;
begin
  // Note: We do not align the position here. The position is aligned during
  // the painting process (e.g., via ALDrawDrawable).
  BeginUpdate;
  Try
    // OnResize and OnResized will be called in loaded
    var LOldOnResize := OnResize;
    var LOldOnResized := OnResized;
    if CSLoading in componentState then begin
      OnResize := DelayOnResize;
      OnResized := DelayOnResized;
    end;
    try
      Margins.Rect := ALAlignEdgesToPixelRound(Margins.Rect, ALGetScreenScale, TEpsilon.Position);
      Padding.Rect := ALAlignEdgesToPixelRound(Padding.Rect, ALGetScreenScale, TEpsilon.Position);
      case Align of
        TALAlignLayout.None,
        TALAlignLayout.Center,
        TALAlignLayout.TopCenter,
        TALAlignLayout.TopLeft,
        TALAlignLayout.TopRight,
        TALAlignLayout.LeftCenter,
        TALAlignLayout.LeftTop,
        TALAlignLayout.LeftBottom,
        TALAlignLayout.RightCenter,
        TALAlignLayout.RightTop,
        TALAlignLayout.RightBottom,
        TALAlignLayout.BottomCenter,
        TALAlignLayout.BottomLeft,
        TALAlignLayout.BottomRight:
          Size.Size := ALAlignDimensionToPixelRound(Size.Size, ALGetScreenScale, TEpsilon.Position);
        //--
        TALAlignLayout.Top,
        TALAlignLayout.MostTop,
        TALAlignLayout.Bottom,
        TALAlignLayout.MostBottom,
        TALAlignLayout.Horizontal,
        TALAlignLayout.VertCenter:
          Size.Height := ALAlignDimensionToPixelRound(Size.Height , ALGetScreenScale, TEpsilon.Position);
        //--
        TALAlignLayout.Left,
        TALAlignLayout.MostLeft,
        TALAlignLayout.Right,
        TALAlignLayout.MostRight,
        TALAlignLayout.Vertical,
        TALAlignLayout.HorzCenter:
          Size.Width := ALAlignDimensionToPixelRound(Size.Width , ALGetScreenScale, TEpsilon.Position);
        //--
        TALAlignLayout.Client,
        TALAlignLayout.Contents,
        TALAlignLayout.Scale,
        TALAlignLayout.Fit,
        TALAlignLayout.FitLeft,
        TALAlignLayout.FitRight:;
        //--
        else
          Raise Exception.Create('Error AC54DF90-F880-4BD5-8474-E62BD8D099FB')
      end;
    finally
      OnResize := LOldOnResize;
      OnResized := LOldOnResized;
    end;
  finally
    EndUpdate;
  end;
end;

{**************************************************}
procedure TALControl.DelayOnResize(Sender: TObject);
begin
  Include(TALControlAccessPrivate(Self).FDelayedEvents, TALControlAccessPrivate.TDelayedEvent.Resize);
end;

{***************************************************}
procedure TALControl.DelayOnResized(Sender: TObject);
begin
  Include(TALControlAccessPrivate(Self).FDelayedEvents, TALControlAccessPrivate.TDelayedEvent.Resized);
end;

{******************************************************}
function TALControl.GetIsPixelAlignmentEnabled: Boolean;
begin
  Result := FIsPixelAlignmentEnabled;
end;

{*********************************************************************}
procedure TALControl.SetIsPixelAlignmentEnabled(const AValue: Boolean);
begin
  FIsPixelAlignmentEnabled := AValue;
end;

{*****************************************************}
function TALControl.IsVisibleWithinFormBounds: Boolean;
begin
  Result := GetParentedVisible;
  if not result then exit;
  if FForm <> nil then
    Result := FForm.ClientRect.IntersectsWith(LocalToAbsolute(LocalRect));
end;

{***********************************************}
procedure TALControl.SetNewScene(AScene: IScene);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.Pressed is still changed only in SetNewScene/DoMouseLeave/MouseDown/MouseUp/MouseClick'}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.IsFocused is still changed only in SetNewScene/DoEnter/DoExit'}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.IsMouseOver is still changed only in SetNewScene/DoMouseEnter/DoMouseLeave'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  var LPrevIsFocused := IsFocused;
  var LPrevIsMouseOver := IsMouseOver;
  inherited;
  {$IF defined(ANDROID) or defined(IOS)}
  FIsMouseOver := False;
  {$ENDIF}
  if LPrevPressed <> Pressed then PressedChanged;
  if LPrevIsFocused <> IsFocused then IsFocusedChanged;
  if LPrevIsMouseOver <> IsMouseOver then IsMouseOverChanged;
end;

{**************************************}
function TALControl.GetPressed: Boolean;
begin
  result := inherited Pressed;
end;

{*****************************************************}
procedure TALControl.SetPressed(const AValue: Boolean);
begin
  if AValue <> GetPressed then begin
    inherited Pressed := AValue;
    pressedChanged;
  end;
end;

{***************************}
procedure TALControl.DoEnter;
begin
  var LPrevIsFocused := IsFocused;
  inherited;
  if LPrevIsFocused <> IsFocused then IsFocusedChanged;
end;

{**************************}
procedure TALControl.DoExit;
begin
  var LPrevIsFocused := IsFocused;
  inherited;
  if LPrevIsFocused <> IsFocused then IsFocusedChanged;
end;

{********************************}
procedure TALControl.DoMouseEnter;
begin
  var LPrevIsMouseOver := IsMouseOver;
  inherited;
  {$IF defined(ANDROID) or defined(IOS)}
  FIsMouseOver := False;
  {$ENDIF}
  if LPrevIsMouseOver <> IsMouseOver then IsMouseOverChanged;
end;

{********************************}
procedure TALControl.DoMouseLeave;
begin
  var LPrevIsMouseOver := IsMouseOver;
  inherited;
  {$IF defined(ANDROID) or defined(IOS)}
  FIsMouseOver := False;
  {$ENDIF}
  if not AutoCapture then
    Pressed := False;
  if LPrevIsMouseOver <> IsMouseOver then IsMouseOverChanged;
end;

{*************************************************************************************}
procedure TALControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  //--
  FControlAbsolutePosAtMouseDown := LocalToAbsolute(TPointF.Zero);
  FMouseDownAtLowVelocity := True;
  //--
  if FDisableDoubleClickHandling then Shift := Shift - [ssDouble];
  //--
  var LScrollableControl: IALScrollableControl;
  var LParent := Parent;
  while LParent <> nil do begin
    if Supports(LParent, IALScrollableControl, LScrollableControl) then begin
      if not LScrollableControl.GetScrollEngine.IsVelocityLow then begin
        FMouseDownAtLowVelocity := False;
        Break;
      end
      else LParent := LParent.Parent;
    end
    else LParent := LParent.Parent;
  end;
  //--
  if (not FFocusOnMouseDown) or (FFocusOnMouseUp) or (not FMouseDownAtLowVelocity) then begin
    Var LOldIsfocused := FIsfocused;
    FIsfocused := True;
    Try
      inherited;
    finally
      FIsfocused := LOldIsfocused;
    End;
  end
  else
    inherited;
  //--
  if LPrevPressed <> Pressed then PressedChanged;
end;

{***********************************************************************************}
procedure TALControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  inherited;
  if LPrevPressed <> Pressed then PressedChanged;
  var LControlAbsolutePos := LocalToAbsolute(TPointF.Zero);
  if (FFocusOnMouseUp) and
     (FMouseDownAtLowVelocity) and
     (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) <= TALScrollEngine.DefaultTouchSlop) and
     (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) <= TALScrollEngine.DefaultTouchSlop) and
     (not (csDesigning in ComponentState)) and
     (not FIsFocused) then
    SetFocus;
end;

{**************************************************************************************}
procedure TALControl.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  var LControlAbsolutePos := LocalToAbsolute(TPointF.Zero);
  if (not FMouseDownAtLowVelocity) or
     (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) > TALScrollEngine.DefaultTouchSlop) or
     (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) > TALScrollEngine.DefaultTouchSlop) then begin
    {$IF defined(debug)}
    if (not FMouseDownAtLowVelocity) then
      ALLog('MouseClick', 'Skipped | Mouse Down was not made at Low Velocity')
    else if (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) > TALScrollEngine.DefaultTouchSlop) then
      ALLog('MouseClick', 'Skipped | Control moved by '+ALFormatFloatW('0.##', abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x), ALDefaultFormatSettingsW) + ' horizontally')
    else if (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) > TALScrollEngine.DefaultTouchSlop) then
      ALLog('MouseClick', 'Skipped | Control moved by '+ALFormatFloatW('0.##', abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y), ALDefaultFormatSettingsW) + ' vertically')
    else
      raise Exception.Create('Error 79BF6F83-8725-476D-A283-507BE9CC671C');
    {$ENDIF}
    exit;
  end;
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  inherited;
  if LPrevPressed <> Pressed then PressedChanged;
end;

{*************************************}
// Optimized GetParentedVisible to work
// exclusively with ParentControl.
function TALControl.GetParentedVisible: Boolean;
begin
  {$IF defined(ALDPK)}
  result := Inherited GetParentedVisible;
  {$ELSE}
  var P: TControl := Self;
  Result := False;
  while P <> nil do begin
    if not p.Visible then Exit;
    P := P.ParentControl;
  end;
  // We do not care about FForm.visible like in
  // Inherited GetParentedVisible
  // Result := (FForm = nil) or (FForm.visible);
  Result := True;
  {$ENDIF}
end;

{*********************************}
procedure TALControl.DoRootChanged;
begin
  inherited;
  if Root is TCommonCustomForm then FForm := TCommonCustomForm(Root)
  else FForm := Nil;
end;

{**************************************}
procedure TALControl.IsMouseOverChanged;
begin
  // virtual
end;

{************************************}
procedure TALControl.IsFocusedChanged;
begin
  // virtual
end;

{**********************************}
procedure TALControl.PressedChanged;
begin
  // virtual
end;

{**********************************}
Procedure TALControl.MarginsChanged;
begin
  // virtual
end;

{**********************************************************}
procedure TALControl.MarginsChangedHandler(Sender: TObject);
begin
  if Assigned(FFormerMarginsChangedHandler) then
    FFormerMarginsChangedHandler(Sender);
  MarginsChanged;
end;

{**}
Type
  _TControlAccessProtected = class(Tcontrol);
  _TStyledControlAccessProtected = class(TStyledControl);

{*************************************}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if FMX.Controls.TContentTabList was not updated and adjust the IFDEF'}
{$ENDIF}
type
  TALContentTabList = class(TTabList)
  protected
    function IsAddable(const TabStop: IControl): Boolean; override;
  end;

{*********************************************************************}
function TALContentTabList.IsAddable(const TabStop: IControl): Boolean;
var
  Content: IControl;
  Parent: TFmxObject;
begin
  Result := True;
  if Supports(Controller, IControl, Content) then
  begin
    Parent := Content.GetObject.Parent;
    if (Parent <> nil) and (Parent is TStyledControl) then
      Result := not TabStop.GetObject.Equals(_TStyledControlAccessProtected(Parent).ResourceLink);
  end;

  Result := Result and inherited IsAddable(TabStop);
end;

{************************************************}
constructor TALContent.Create(AOwner: TComponent);
begin
  inherited;
  SetAcceptsControls(False);
end;

{*****************************}
procedure TALContent.DoRealign;
var
  AlignRoot: IAlignRoot;
begin
  if (Parent <> nil) and not(csLoading in Parent.ComponentState) then
    inherited;
  if (Parent <> nil) and not FParentAligning and not(csLoading in ComponentState) then
  begin
    FParentAligning := True;
    if ParentControl <> nil then
      _TControlAccessProtected(ParentControl).Realign
    else
      if not(csLoading in ComponentState) and Supports(Parent, IAlignRoot, AlignRoot) then
        AlignRoot.Realign;
    FParentAligning := False;
  end;
end;

{*************************************************}
function TALContent.GetTabListClass: TTabListClass;
begin
  Result := TALContentTabList;
end;

{***********************************************************}
function TALContent.GetTabStopController: ITabStopController;
var
  Control: IControl;
begin
  if Supports(Parent, IControl, Control) then
    Result := Control.GetTabStopController
  else
    Result := nil;
end;

{**********************************}
procedure TALContent.ContentChanged;
begin
end;

end.
