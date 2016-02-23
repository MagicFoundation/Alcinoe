unit ALFmxRangeTrackBar;

interface

uses System.Classes,
     System.UITypes,
     FMX.layouts,
     FMX.Controls,
     FMX.StdCtrls;

type

  //TTrackBar
  //  TLayout
  //    TStyleObject (ftrack - horizontal)
  //      TStyleObject (fTrackHighlight)
  //        TRectangle
  //    TStyleObject (ftrack - vertical)
  //      TStyleObject (fTrackHighlight)
  //        TRectangle
  //    TThumb
  //      TbuttonStyleObject

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALMinThumbTrackBar = class;
  TALMaxThumbTrackBar = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBGTrackBar = Class(TTrackBar)
  private
    [weak] fMinThumbTrackBar: TALMinThumbTrackBar;
    [weak] fMaxThumbTrackBar: TALMaxThumbTrackBar;
  protected
    procedure Resize; override;
    procedure ApplyStyle; override;
    procedure DoRealign; override;
    procedure UpdateHighlight;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALMinThumbTrackBar = Class(TTrackBar)
  private
    [weak] fBGTrackBar: TALBGTrackBar;
    [weak] fMaxThumbTrackBar: TALMaxThumbTrackBar;
    procedure OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseLeave(Sender: TObject);
    procedure OnThumbMouseEnter(Sender: TObject);
    procedure onThumbApplyStyleLookup(Sender: TObject);
  protected
    procedure Resize; override;
    procedure ApplyStyle; override;
    procedure DoChanged; override;
    procedure DoTracking; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALMaxThumbTrackBar = Class(TTrackBar)
  private
    [weak] fBGTrackBar: TALBGTrackBar;
    [weak] fMinThumbTrackBar: TALMinThumbTrackBar;
    procedure OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure OnThumbMouseLeave(Sender: TObject);
    procedure OnThumbMouseEnter(Sender: TObject);
    procedure onThumbApplyStyleLookup(Sender: TObject);
  protected
    procedure Resize; override;
    procedure ApplyStyle; override;
    procedure DoChanged; override;
    procedure DoTracking; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALRangeTrackBar = class(TControl)
  private
    fBGTrackBar: TALBGTrackBar;
    fMinThumbTrackBar: TALMinThumbTrackBar;
    fMaxThumbTrackBar: TALMaxThumbTrackBar;
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
    procedure SetonApplyStyleLookup(const Value: TNotifyEvent);
    function GetOnApplyStyleLookup: TNotifyEvent;
    function GetStyleLookup: string;
    procedure SetStyleLookup(const Value: string);
  protected
    FOnChange, FOnTracking, fonThumbApplyStyleLookup: TNotifyEvent;
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
    property onThumbApplyStyleLookup: TNotifyEvent read fonThumbApplyStyleLookup write fonThumbApplyStyleLookup;
    property onApplyStyleLookup: TNotifyEvent read GetOnApplyStyleLookup write SetonApplyStyleLookup;
    property StyleLookup: string read GetStyleLookup write SetStyleLookup;
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
     FMX.styles.objects,
     FMX.Types;

{*********************************}
procedure TALBGTrackBar.ApplyStyle;
begin
  inherited;
  if assigned(FTrackHighlight) then FTrackHighlight.Align := TALignLayout.None;
  if assigned(Thumb) then Thumb.Visible := False;
  TALRangeTrackBar(Parent).MinClipWidth := MinClipWidth;
  TALRangeTrackBar(Parent).MinClipHeight := MinClipHeight;
  UpdateHighlight;
end;

{********************************}
procedure TALBGTrackBar.DoRealign;
begin
  inherited;
  if assigned(Thumb) then Thumb.Visible := False;
end;

{*****************************}
procedure TALBGTrackBar.Resize;
begin
  inherited;
  case Orientation of
    TOrientation.Horizontal: if height <> TALRangeTrackBar(Parent).width then TALRangeTrackBar(Parent).Height := height;
    TOrientation.Vertical: if width <> TALRangeTrackBar(Parent).width then TALRangeTrackBar(Parent).width := width;
  end;
end;

{**************************************}
procedure TALBGTrackBar.UpdateHighlight;
begin
  if FTrackHighlight <> nil then
  begin
    case Orientation of
      TOrientation.Horizontal: begin
        if (fMinThumbTrackBar.Thumb <> nil) and (fMaxThumbTrackBar.Thumb <> nil) then begin
          FTrackHighlight.position.X := fMinThumbTrackBar.GetThumbRect.Left - fMinThumbTrackBar.Thumb.Margins.Left;
          FTrackHighlight.Width := Round((fMaxThumbTrackBar.GetThumbRect.Left + fMaxThumbTrackBar.GetThumbRect.Right) / 2) - FTrackHighlight.position.X;
        end
        else begin
          FTrackHighlight.position.X := 0;
          FTrackHighlight.Width := 0;
        end;
      end;
      TOrientation.Vertical: begin
        if (fMinThumbTrackBar.Thumb <> nil) and (fMaxThumbTrackBar.Thumb <> nil) then begin
          FTrackHighlight.position.y := fMinThumbTrackBar.GetThumbRect.Top - fMinThumbTrackBar.Thumb.Margins.Top;
          FTrackHighlight.Height := Round((fMaxThumbTrackBar.GetThumbRect.Top + fMaxThumbTrackBar.GetThumbRect.Bottom) / 2) - FTrackHighlight.position.y;
        end
        else begin
          FTrackHighlight.position.y := 0;
          FTrackHighlight.Height := 0;
        end;
      end;
    end;
  end;
end;

{**********************************************************************************************************************}
procedure TALMinThumbTrackBar.OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  BringToFront;
  if assigned(TALRangeTrackBar(Parent).OnMouseDown) then
    TALRangeTrackBar(Parent).OnMouseDown(Parent, Button, Shift, X, Y);
end;

{********************************************************************************************************************}
procedure TALMinThumbTrackBar.OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseUp) then
    TALRangeTrackBar(Parent).OnMouseUp(Parent, Button, Shift, X, Y);
end;

{************************************************************************************************}
procedure TALMinThumbTrackBar.OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseMove) then
    TALRangeTrackBar(Parent).OnMouseMove(Parent, Shift, X, Y);
end;

{***************************************************************}
procedure TALMinThumbTrackBar.OnThumbMouseLeave(Sender: TObject);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseLeave) then
    TALRangeTrackBar(Parent).OnMouseLeave(Parent);
end;

{***************************************************************}
procedure TALMinThumbTrackBar.OnThumbMouseEnter(Sender: TObject);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseEnter) then
    TALRangeTrackBar(Parent).OnMouseEnter(Parent);
end;

{*********************************************************************}
procedure TALMinThumbTrackBar.onThumbApplyStyleLookup(Sender: TObject);
begin

  // TThumb
  //   TbuttonStyleObject

  if assigned(TALRangeTrackBar(Parent).onThumbApplyStyleLookup) then
    TALRangeTrackBar(Parent).onThumbApplyStyleLookup(Sender);

  if (value >= fMaxThumbTrackBar.Max) and
     (assigned(Thumb)) then BringToFront
  else if (value <= Min) and
          (assigned(fMaxThumbTrackBar.Thumb)) then fMaxThumbTrackBar.BringToFront;


end;

{***************************************}
procedure TALMinThumbTrackBar.ApplyStyle;
begin
  inherited;
  if assigned(FTrack) then FTrack.visible := False;
  if assigned(Thumb) then begin
    Thumb.OnMouseDown := OnThumbMouseDown;
    Thumb.OnMouseUp := OnThumbMouseUp;
    Thumb.OnMouseMove := OnThumbMouseMove;
    Thumb.OnMouseLeave := OnThumbMouseLeave;
    Thumb.OnMouseEnter := OnThumbMouseEnter;
    Thumb.OnApplyStyleLookup := onThumbApplyStyleLookup;
  end;
  fBGTrackBar.UpdateHighlight;
end;

{***********************************}
procedure TALMinThumbTrackBar.Resize;
begin
  inherited;
  fBGTrackBar.UpdateHighlight;
end;

{***************************************}
procedure TALMinThumbTrackBar.DoTracking;
begin
  if Value > fMaxThumbTrackBar.value then Value := fMaxThumbTrackBar.value
  else begin
    inherited;
    if not (csLoading in ComponentState) and Assigned(TALRangeTrackBar(parent).FOnTracking) then
      TALRangeTrackBar(parent).FOnTracking(parent);
    fBGTrackBar.UpdateHighlight;
  end;
end;

{**************************************}
procedure TALMinThumbTrackBar.DoChanged;
begin
  inherited;
  if not (csLoading in ComponentState) and Assigned(TALRangeTrackBar(parent).FOnChange) then
    TALRangeTrackBar(parent).FOnChange(parent);
  fBGTrackBar.UpdateHighlight;
end;

{**********************************************************************************************************************}
procedure TALMaxThumbTrackBar.OnThumbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  BringToFront;
  if assigned(TALRangeTrackBar(Parent).OnMouseDown) then
    TALRangeTrackBar(Parent).OnMouseDown(Parent, Button, Shift, X, Y);
end;

{********************************************************************************************************************}
procedure TALMaxThumbTrackBar.OnThumbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseUp) then
    TALRangeTrackBar(Parent).OnMouseUp(Parent, Button, Shift, X, Y);
end;

{************************************************************************************************}
procedure TALMaxThumbTrackBar.OnThumbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseMove) then
    TALRangeTrackBar(Parent).OnMouseMove(Parent, Shift, X, Y);
end;

{***************************************************************}
procedure TALMaxThumbTrackBar.OnThumbMouseLeave(Sender: TObject);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseLeave) then
    TALRangeTrackBar(Parent).OnMouseLeave(Parent);
end;

{***************************************************************}
procedure TALMaxThumbTrackBar.OnThumbMouseEnter(Sender: TObject);
begin
  if assigned(TALRangeTrackBar(Parent).OnMouseEnter) then
    TALRangeTrackBar(Parent).OnMouseEnter(Parent);
end;

{*********************************************************************}
procedure TALMaxThumbTrackBar.onThumbApplyStyleLookup(Sender: TObject);
begin

  // TThumb
  //   TbuttonStyleObject

  if assigned(TALRangeTrackBar(Parent).onThumbApplyStyleLookup) then
    TALRangeTrackBar(Parent).onThumbApplyStyleLookup(Sender);

  if (value <= fMinThumbTrackBar.min) and
     (assigned(Thumb)) then BringToFront
  else if (value >= max) and
          (assigned(fMinThumbTrackBar.Thumb)) then fMinThumbTrackBar.BringToFront;

end;

{***************************************}
procedure TALMaxThumbTrackBar.ApplyStyle;
begin
  inherited;
  if assigned(FTrack) then FTrack.visible := False;
  if assigned(Thumb) then begin
    Thumb.OnMouseDown := OnThumbMouseDown;
    Thumb.OnMouseUp := OnThumbMouseUp;
    Thumb.OnMouseMove := OnThumbMouseMove;
    Thumb.OnMouseLeave := OnThumbMouseLeave;
    Thumb.OnMouseEnter := OnThumbMouseEnter;
    Thumb.OnApplyStyleLookup := onThumbApplyStyleLookup;
  end;
  fBGTrackBar.UpdateHighlight;
end;

{***********************************}
procedure TALMaxThumbTrackBar.Resize;
begin
  inherited;
  fBGTrackBar.UpdateHighlight;
end;

{***************************************}
procedure TALMaxThumbTrackBar.DoTracking;
begin
  if Value < fMinThumbTrackBar.value then value := fMinThumbTrackBar.value
  else begin
    inherited;
    if not (csLoading in ComponentState) and Assigned(TALRangeTrackBar(parent).FOnTracking) then
      TALRangeTrackBar(parent).FOnTracking(parent);
    fBGTrackBar.UpdateHighlight;
  end;
end;

{**************************************}
procedure TALMaxThumbTrackBar.DoChanged;
begin
  inherited;
  if not (csLoading in ComponentState) and Assigned(TALRangeTrackBar(parent).FOnChange) then
    TALRangeTrackBar(parent).FOnChange(parent);
  fBGTrackBar.UpdateHighlight;
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
  fonThumbApplyStyleLookup := nil;
  //-----
  fBGTrackBar := TALBGTrackBar.Create(self);
  //-----
  Width := fBGTrackBar.Width;
  height := fBGTrackBar.Height;
  //-----
  fBGTrackBar.Stored := False;
  fBGTrackBar.Parent := Self;
  fBGTrackBar.Align := TalignLayout.Client;
  fBGTrackBar.HitTest := False;
  //-----
  fMaxThumbTrackBar := TALMaxThumbTrackBar.Create(self);
  fMaxThumbTrackBar.Stored := False;
  fMaxThumbTrackBar.Parent := Self;
  fMaxThumbTrackBar.Align := TalignLayout.Client;
  fMaxThumbTrackBar.HitTest := False;
  //-----
  fMinThumbTrackBar := TALMinThumbTrackBar.Create(self);
  fMinThumbTrackBar.Stored := False;
  fMinThumbTrackBar.Parent := Self;
  fMinThumbTrackBar.Align := TalignLayout.Client;
  fMinThumbTrackBar.HitTest := False;
  //-----
  fBGTrackBar.fMinThumbTrackBar := fMinThumbTrackBar;
  fBGTrackBar.fMaxThumbTrackBar := fMaxThumbTrackBar;
  //-----
  fMinThumbTrackBar.fBGTrackBar := fBGTrackBar;
  fMinThumbTrackBar.fMaxThumbTrackBar := fMaxThumbTrackBar;
  //-----
  fMaxThumbTrackBar.fBGTrackBar := fBGTrackBar;
  fMaxThumbTrackBar.fMinThumbTrackBar := fMinThumbTrackBar;
  //-----
  SetMin(fMinThumbTrackBar.Min);
  SetMax(fMaxThumbTrackBar.Max);
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
  freeandnil(fMaxThumbTrackBar);
  freeandnil(fMinThumbTrackBar);
  freeandnil(fBGTrackBar);
  inherited;
end;

{********************************}
procedure TALRangeTrackBar.Resize;
begin
  inherited;
  case Orientation of
    TOrientation.Horizontal: if height <> fbgTrackBar.Height then height := fbgTrackBar.Height;
    TOrientation.Vertical: if Width <> fbgTrackBar.Width then Width := fbgTrackBar.Width;
  end;
end;

{***************************************}
function TALRangeTrackBar.GetMax: Single;
begin
  result := fMaxThumbTrackBar.Max;
end;

{*****************************************************}
procedure TALRangeTrackBar.SetMax(const Value: Single);
begin
  fBGTrackBar.Max := Value;
  fMinThumbTrackBar.Max := Value;
  fMaxThumbTrackBar.Max := Value;
  //-----
  fMinThumbTrackBar.Value := Min;
  fMaxThumbTrackBar.Value := max;
end;

{***************************************}
function TALRangeTrackBar.GetMin: Single;
begin
  result := fMinThumbTrackBar.Min;
end;

{*****************************************************}
procedure TALRangeTrackBar.SetMin(const Value: Single);
begin
  fBGTrackBar.Min := Value;
  fMinThumbTrackBar.Min := Value;
  fMaxThumbTrackBar.Min := Value;
  //-----
  fMinThumbTrackBar.Value := Min;
  fMaxThumbTrackBar.Value := max;
end;

{*********************************************}
function TALRangeTrackBar.GetFrequency: Single;
begin
  result := fBGTrackBar.Frequency;
end;

{***********************************************************}
procedure TALRangeTrackBar.SetFrequency(const Value: Single);
begin
  fBGTrackBar.Frequency := Value;
  fMaxThumbTrackBar.Frequency := Value;
  fMinThumbTrackBar.Frequency := Value;
end;

{********************************************}
function TALRangeTrackBar.GetMaxValue: Single;
begin
  result := fMaxThumbTrackBar.Value;
end;

{**********************************************************}
procedure TALRangeTrackBar.SetMaxValue(const Value: Single);
begin
  fMaxThumbTrackBar.value := Value;
end;

{********************************************}
function TALRangeTrackBar.GetMinValue: Single;
begin
  result := fMinThumbTrackBar.Value;
end;

{**********************************************************}
procedure TALRangeTrackBar.SetMinValue(const Value: Single);
begin
  fMinThumbTrackBar.value := Value;
end;

{************************************************************}
function TALRangeTrackBar.GetOnApplyStyleLookup: TNotifyEvent;
begin
  result := fbgTrackBar.OnApplyStyleLookup;
end;

{**************************************************************************}
procedure TALRangeTrackBar.SetonApplyStyleLookup(const Value: TNotifyEvent);
begin
  fbgTrackBar.OnApplyStyleLookup := Value;
end;

{*****************************************************}
function TALRangeTrackBar.GetOrientation: TOrientation;
begin
  result := fBGTrackBar.Orientation;
end;

{*******************************************************************}
procedure TALRangeTrackBar.SetOrientation(const Value: TOrientation);
begin
  fBGTrackBar.Orientation := Value;
  fMaxThumbTrackBar.Orientation := Value;
  fMinThumbTrackBar.Orientation := Value;
end;

{***********************************************}
function TALRangeTrackBar.GetSmallChange: Single;
begin
  result := fBGTrackBar.SmallChange;
end;

{*************************************************************}
procedure TALRangeTrackBar.SetSmallChange(const Value: Single);
begin
  fBGTrackBar.SmallChange := Value;
  fMaxThumbTrackBar.SmallChange := Value;
  fMinThumbTrackBar.SmallChange := Value;
end;

{***********************************************}
function TALRangeTrackBar.GetStyleLookup: string;
begin
  result := fBGTrackBar.StyleLookup;
end;

{*************************************************************}
procedure TALRangeTrackBar.SetStyleLookup(const Value: string);
begin
  fBGTrackBar.StyleLookup := Value;
  fBGTrackBar.StyleLookup := Value;
  fBGTrackBar.StyleLookup := Value;
end;

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALRangeTrackBar]);
end;

end.
