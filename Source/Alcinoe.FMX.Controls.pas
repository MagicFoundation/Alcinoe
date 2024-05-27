unit Alcinoe.FMX.Controls;

interface

{$I Alcinoe.inc}

uses
  System.UITypes,
  system.Types,
  System.Classes,
  FMX.Controls,
  FMX.Types;

type

  {**************************}
  {$IFNDEF ALCompilerVersionSupported120}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl was not updated and adjust the IFDEF'}
  {$ENDIF}
  TALControl = class(TControl)
  private
    FFocusOnMouseUp: Boolean;
    FMouseDownPos: TpointF;
    FMouseDownAtLowVelocity: Boolean;
  protected
    property FocusOnMouseUp: Boolean read FFocusOnMouseUp write FFocusOnMouseUp;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
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
    property Align;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Visible;
    property Width;
    property Size;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
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
  FFocusOnMouseUp := False;
  FMouseDownPos := TpointF.zero;
  FMouseDownAtLowVelocity := True;
end;

{*************************************************************************************}
procedure TALControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
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
  if (FFocusOnMouseUp) or (not FMouseDownAtLowVelocity) then begin
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
end;

{***********************************************************************************}
procedure TALControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
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
  inherited;
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
