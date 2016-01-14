unit ALFmxRangeTrackBar;

interface

uses System.Classes,
     System.UITypes,
     FMX.layouts,
     FMX.Controls,
     FMX.StdCtrls;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALMaxRangeTrackBar = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALMinRangeTrackBar = Class(TTrackBar)
  private
    [weak] fMaxTrackBar: TALMaxRangeTrackBar;
    procedure OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseLeave(Sender: TObject);
    procedure OnThumbMouseEnter(Sender: TObject);
  protected
    procedure Resize; override;
    procedure ApplyStyle; override;
    procedure DoChanged; override;
    procedure DoTracking; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALMaxRangeTrackBar = Class(TTrackBar)
  private
    [weak] fMinTrackBar: TALMinRangeTrackBar;
    procedure OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseLeave(Sender: TObject);
    procedure OnThumbMouseEnter(Sender: TObject);
  protected
    procedure Resize; override;
    procedure ApplyStyle; override;
    procedure DoChanged; override;
    procedure DoTracking; override;
    procedure UpdateHighlight;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALRangeTrackBar = class(TControl)
  private
    fMinTrackBar: TALMinRangeTrackBar;
    fMaxTrackBar: TALMaxRangeTrackBar;
    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    function GetMax: Single;
    function GetMin: Single;
    function GetFrequency: Single;
    function GetMaxValue: Single;
    function GetMinValue: Single;
    function GetOrientation: TOrientation;
    function GetSmallChange: Single;
    procedure SetFrequency(const Value: Single);
    procedure SetMaxValue(const Value: Single);
    procedure SetMinValue(const Value: Single);
    procedure SetOrientation(const Value: TOrientation);
    procedure SetSmallChange(const Value: Single);
  protected
    FOnChange, FOnTracking: TNotifyEvent;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Frequency: Single read GetFrequency write SetFrequency;
    property SmallChange: Single read GetSmallChange write SetSmallChange;
    property Min: Single read GetMin write SetMin;
    property Max: Single read GetMax write SetMax;
    property MinValue: Single read GetMinValue write SetMinValue;
    property MaxValue: Single read GetMaxValue write SetMaxValue;
    property Orientation: TOrientation read GetOrientation write SetOrientation;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnTracking: TNotifyEvent read FOnTracking write FOnTracking;
    property Align;
    property Anchors;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Locked;
    property Height;
    //property HitTest default False;
    property Padding;
    property Opacity;
    property Margins;
    //property PopupMenu;
    property Position;
    //property RotationAngle;
    //property RotationCenter;
    property Scale;
    property Size;
    //property TouchTargetExpansion;
    property Visible;
    property Width;
    property TabOrder;
    property TabStop;
    { Events }
    //property OnPainting;
    //property OnPaint;
    property OnResize;
    { Drag and Drop events }
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    { Mouse events }
    //property OnClick;
    //property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    //property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

procedure Register;

implementation

uses System.SysUtils,
     FMX.Types;

{**********************************************************************************************************************}
procedure TALMinRangeTrackBar.OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  tcontrol(Sender).BringToFront;
  if assigned(TALRangeTrackBar(Parent).OnMouseDown) then
    TALRangeTrackBar(Parent).OnMouseDown(Parent, Button, Shift, X, Y);
end;

{********************************************************************************************************************}
procedure TALMinRangeTrackBar.OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseUp) then
    TALRangeTrackBar(Parent).OnMouseUp(Parent, Button, Shift, X, Y);
end;

{************************************************************************************************}
procedure TALMinRangeTrackBar.OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseMove) then
    TALRangeTrackBar(Parent).OnMouseMove(Parent, Shift, X, Y);
end;

{***************************************************************}
procedure TALMinRangeTrackBar.OnThumbMouseLeave(Sender: TObject);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseLeave) then
    TALRangeTrackBar(Parent).OnMouseLeave(Parent);
end;

{***************************************************************}
procedure TALMinRangeTrackBar.OnThumbMouseEnter(Sender: TObject);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseEnter) then
    TALRangeTrackBar(Parent).OnMouseEnter(Parent);
end;

{***************************************}
procedure TALMinRangeTrackBar.ApplyStyle;
begin
  inherited;
  if assigned(FTrack) then FTrack.visible := False;
  if assigned(FTrackHighlight) then FTrackHighlight.visible := False;
  if assigned(Thumb) then begin
    Thumb.Parent := self.Parent;
    Thumb.OnMouseDown := OnThumbMouseDown;
    Thumb.OnMouseUp := OnThumbMouseUp;
    Thumb.OnMouseMove := OnThumbMouseMove;
    Thumb.OnMouseLeave := OnThumbMouseLeave;
    Thumb.OnMouseEnter := OnThumbMouseEnter;
  end;
  fMaxTrackBar.UpdateHighlight;
end;

{***********************************}
procedure TALMinRangeTrackBar.Resize;
begin
  inherited;
  fMaxTrackBar.UpdateHighlight;
end;

{***************************************}
procedure TALMinRangeTrackBar.DoTracking;
begin
  if Value > fMaxTrackBar.value then Value := fMaxTrackBar.value
  else begin
    inherited;
    if not (csLoading in ComponentState) and Assigned(TALRangeTrackBar(parent).FOnTracking) then
      TALRangeTrackBar(parent).FOnTracking(parent);
    fMaxTrackBar.UpdateHighlight;
  end;
end;

{**************************************}
procedure TALMinRangeTrackBar.DoChanged;
begin
  inherited;
  if not (csLoading in ComponentState) and Assigned(TALRangeTrackBar(parent).FOnChange) then
    TALRangeTrackBar(parent).FOnChange(parent);
  fMaxTrackBar.UpdateHighlight;
end;

{**********************************************************************************************************************}
procedure TALMaxRangeTrackBar.OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  tcontrol(Sender).BringToFront;
  if assigned(TALRangeTrackBar(Parent).OnMouseDown) then
    TALRangeTrackBar(Parent).OnMouseDown(Parent, Button, Shift, X, Y);
end;

{********************************************************************************************************************}
procedure TALMaxRangeTrackBar.OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseUp) then
    TALRangeTrackBar(Parent).OnMouseUp(Parent, Button, Shift, X, Y);
end;

{************************************************************************************************}
procedure TALMaxRangeTrackBar.OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseMove) then
    TALRangeTrackBar(Parent).OnMouseMove(Parent, Shift, X, Y);
end;

{***************************************************************}
procedure TALMaxRangeTrackBar.OnThumbMouseLeave(Sender: TObject);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseLeave) then
    TALRangeTrackBar(Parent).OnMouseLeave(Parent);
end;

{***************************************************************}
procedure TALMaxRangeTrackBar.OnThumbMouseEnter(Sender: TObject);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseEnter) then
    TALRangeTrackBar(Parent).OnMouseEnter(Parent);
end;

{***************************************}
procedure TALMaxRangeTrackBar.ApplyStyle;
begin
  inherited;
  if assigned(FTrackHighlight) then FTrackHighlight.Align := TALignLayout.None;
  if assigned(Thumb) then begin
    Thumb.OnMouseDown := OnThumbMouseDown;
    Thumb.OnMouseUp := OnThumbMouseUp;
    Thumb.OnMouseMove := OnThumbMouseMove;
    Thumb.OnMouseLeave := OnThumbMouseLeave;
    Thumb.OnMouseEnter := OnThumbMouseEnter;
    thumb.Parent := Self.Parent;
  end;
  TALRangeTrackBar(Parent).MinClipWidth := MinClipWidth;
  TALRangeTrackBar(Parent).MinClipHeight := MinClipHeight;
  UpdateHighlight;
end;

{***********************************}
procedure TALMaxRangeTrackBar.Resize;
begin
  inherited;
  UpdateHighlight;
end;

{***************************************}
procedure TALMaxRangeTrackBar.DoTracking;
begin
  if Value < fMinTrackBar.value then value := fMinTrackBar.value
  else begin
    inherited;
    if not (csLoading in ComponentState) and Assigned(TALRangeTrackBar(parent).FOnTracking) then
      TALRangeTrackBar(parent).FOnTracking(parent);
    UpdateHighlight;
  end;
end;

{**************************************}
procedure TALMaxRangeTrackBar.DoChanged;
begin
  inherited;
  if not (csLoading in ComponentState) and Assigned(TALRangeTrackBar(parent).FOnChange) then
    TALRangeTrackBar(parent).FOnChange(parent);
  UpdateHighlight;
end;

{********************************************}
procedure TALMaxRangeTrackBar.UpdateHighlight;
begin
  if FTrackHighlight <> nil then
  begin
    case Orientation of
      TOrientation.Horizontal: begin
        FTrackHighlight.position.X := Round((fMinTrackBar.GetThumbRect.Left + fMinTrackBar.GetThumbRect.Right) / 2);
        FTrackHighlight.Width := Round((GetThumbRect.Left + GetThumbRect.Right) / 2) - FTrackHighlight.position.X;
      end;
      TOrientation.Vertical: begin
        FTrackHighlight.position.Y := Round((fMinTrackBar.GetThumbRect.Top + fMinTrackBar.GetThumbRect.Bottom) / 2);
        FTrackHighlight.Height := Round((GetThumbRect.Top + GetThumbRect.Bottom) / 2) - FTrackHighlight.position.Y;
      end;
    end;
  end;
end;

{******************************************************}
constructor TALRangeTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  //-----
  CanParentFocus := True;
  HitTest := False;
  //-----
  FOnChange := nil;
  FOnTracking := nil;
  //-----
  fMaxTrackBar := TALMaxRangeTrackBar.Create(self);
  fMaxTrackBar.Stored := False;
  fMaxTrackBar.Parent := Self;
  fMaxTrackBar.Align := TalignLayout.Client;
  fMaxTrackBar.HitTest := False;
  //-----
  fMinTrackBar := TALMinRangeTrackBar.Create(self);
  fMinTrackBar.Stored := False;
  fMinTrackBar.fMaxTrackBar := fMaxTrackBar;
  fMaxTrackBar.fMinTrackBar := fMinTrackBar;
  Width := fMinTrackBar.Width;
  height := fMinTrackBar.Height;
  fMinTrackBar.Parent := Self;
  fMinTrackBar.Align := TalignLayout.Client;
  fMinTrackBar.HitTest := False;
  //-----
  SetMin(fMinTrackBar.Min);
  SetMax(fMaxTrackBar.Max);
end;

{*******************************}
procedure TALRangeTrackBar.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{**********************************}
destructor TALRangeTrackBar.Destroy;
begin
  freeandnil(fMaxTrackBar);
  freeandnil(fMinTrackBar);
  inherited;
end;

{********************************}
procedure TALRangeTrackBar.Resize;
begin
  inherited;
  case Orientation of
    TOrientation.Horizontal: height := fMinTrackBar.Height;
    TOrientation.Vertical: Width := fMinTrackBar.Width;
  end;
end;

{***************************************}
function TALRangeTrackBar.GetMax: Single;
begin
  result := fMaxTrackBar.Max;
end;

{***************************************}
function TALRangeTrackBar.GetMin: Single;
begin
  result := fMinTrackBar.Min;
end;

{*****************************************************}
procedure TALRangeTrackBar.SetMax(const Value: Single);
begin
  fMinTrackBar.Max := Value;
  fMaxTrackBar.Max := Value;
  //-----
  fMinTrackBar.Value := Min;
  fMaxTrackBar.Value := max;
end;

{*****************************************************}
procedure TALRangeTrackBar.SetMin(const Value: Single);
begin
  fMinTrackBar.Min := Value;
  fMaxTrackBar.Min := Value;
  //-----
  fMinTrackBar.Value := Min;
  fMaxTrackBar.Value := max;
end;

{*********************************************}
function TALRangeTrackBar.GetFrequency: Single;
begin
  result := fMaxTrackBar.Frequency;
end;

{********************************************}
function TALRangeTrackBar.GetMaxValue: Single;
begin
  result := fMaxTrackBar.Value;
end;

{********************************************}
function TALRangeTrackBar.GetMinValue: Single;
begin
  result := fMinTrackBar.Value;
end;

{*****************************************************}
function TALRangeTrackBar.GetOrientation: TOrientation;
begin
  result := fMaxTrackBar.Orientation;
end;

{***********************************************}
function TALRangeTrackBar.GetSmallChange: Single;
begin
  result := fMaxTrackBar.SmallChange;
end;

{***********************************************************}
procedure TALRangeTrackBar.SetFrequency(const Value: Single);
begin
  fMaxTrackBar.Frequency := Value;
  fMinTrackBar.Frequency := Value;
end;

{**********************************************************}
procedure TALRangeTrackBar.SetMaxValue(const Value: Single);
begin
  fMaxTrackBar.value := Value;
end;

{**********************************************************}
procedure TALRangeTrackBar.SetMinValue(const Value: Single);
begin
  fMinTrackBar.value := Value;
end;

{*******************************************************************}
procedure TALRangeTrackBar.SetOrientation(const Value: TOrientation);
begin
  fMaxTrackBar.Orientation := Value;
  fMinTrackBar.Orientation := Value;
end;

{*************************************************************}
procedure TALRangeTrackBar.SetSmallChange(const Value: Single);
begin
  fMaxTrackBar.SmallChange := Value;
  fMinTrackBar.SmallChange := Value;
end;

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALRangeTrackBar]);
end;

end.
