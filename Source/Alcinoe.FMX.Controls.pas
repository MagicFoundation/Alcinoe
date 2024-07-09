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
  {$IFNDEF ALCompilerVersionSupported120}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl was not updated and adjust the IFDEF'}
  {$ENDIF}
  TALControl = class(TControl)
  private
    FForm: TCommonCustomForm;
    FFocusOnMouseDown: Boolean;
    FFocusOnMouseUp: Boolean;
    FMouseDownPos: TpointF;
    FMouseDownAtLowVelocity: Boolean;
  protected
    property FocusOnMouseDown: Boolean read FFocusOnMouseDown write FFocusOnMouseDown;
    property FocusOnMouseUp: Boolean read FFocusOnMouseUp write FFocusOnMouseUp;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    function GetParentedVisible: Boolean; override;
    procedure DoRootChanged; override;
    procedure IsMouseOverChanged; virtual;
    procedure PressedChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetNewScene(AScene: IScene); override;
    function IsVisibleWithinFormBounds: Boolean;
    property Form: TCommonCustomForm read FForm;
  end;

  {**************************************}
  {$IFNDEF ALCompilerVersionSupported120}
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
    property OnDblClick;
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
  Alcinoe.FMX.Common,
  Alcinoe.FMX.ScrollEngine;

{************************************************}
constructor TALControl.Create(AOwner: TComponent);
begin
  inherited;
  Size.SetPlatformDefaultWithoutNotification(False);
  FForm := nil;
  FFocusOnMouseDown := False;
  FFocusOnMouseUp := False;
  FMouseDownPos := TpointF.zero;
  FMouseDownAtLowVelocity := True;
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
  {$IFNDEF ALCompilerVersionSupported120}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  var LPrevIsMouseOver := IsMouseOver;
  inherited;
  if LPrevIsMouseOver <> IsMouseOver then IsMouseOverChanged;
  if LPrevPressed <> Pressed then PressedChanged;
end;

{*****************************}
procedure TALControl.DoMouseEnter;
begin
  var LPrevIsMouseOver := IsMouseOver;
  inherited;
  if LPrevIsMouseOver <> IsMouseOver then IsMouseOverChanged;
end;

{*****************************}
procedure TALControl.DoMouseLeave;
begin
  var LPrevIsMouseOver := IsMouseOver;
  inherited;
  if LPrevIsMouseOver <> IsMouseOver then IsMouseOverChanged;
end;

{*************************************************************************************}
procedure TALControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported120}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  //--
  fMouseDownPos := TpointF.Create(X,Y);
  FMouseDownAtLowVelocity := True;
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
  {$IFNDEF ALCompilerVersionSupported120}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  inherited;
  if LPrevPressed <> Pressed then PressedChanged;
  if (FFocusOnMouseUp) and
     (FMouseDownAtLowVelocity) and
     (abs(fMouseDownPos.x - x) <= TALScrollEngine.DefaultTouchSlop) and
     (abs(fMouseDownPos.y - y) <= TALScrollEngine.DefaultTouchSlop) and
     (not (csDesigning in ComponentState)) and
     (not FIsFocused) then
    SetFocus;
end;

{**************************************************************************************}
procedure TALControl.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (not FMouseDownAtLowVelocity) or
     (abs(fMouseDownPos.x - x) > TALScrollEngine.DefaultTouchSlop) or
     (abs(fMouseDownPos.y - y) > TALScrollEngine.DefaultTouchSlop) then exit;
  {$IFNDEF ALCompilerVersionSupported120}
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
  Result := (FForm = nil) or (FForm.visible);
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

{*********************************}
procedure TALControl.PressedChanged;
begin
  // virtual
end;

{**}
Type
  _TControlAccessProtected = class(Tcontrol);
  _TStyledControlAccessProtected = class(TStyledControl);

{*************************************}
{$IFNDEF ALCompilerVersionSupported120}
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
