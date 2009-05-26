{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALScrollBar
Version:      3.50

Description:  ScrollBar with onPaint property
              - Flat design (no clt3D feature)
              - Custom property (string to add custom informations
                like "tag" property)
              - Onpaint event (powerfull event fired each time the
                control needed to be
                repaint and permit to change, before the repaint
                process, some property of the control like it's color.
                For exemple we can easily change the font or border
                color of the control each time the mouse enter or
                leave the control.

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALScrollBar;

interface

uses Windows,
     Messages,
     Classes,
     Graphics,
     Controls,
     Forms,
     ExtCtrls,
     stdctrls;

type

  {--------------------------------------------------------------------------------------------}
  TALScrollbarArea = (saBtnArrowDown, saBtnArrowUp, saTrackDown, saTrackUp, saScroller, saNone);
  TALAreaRects = array[TALScrollbarArea] of TRect;
  TALArrowPoly = array[0..2] of TPoint;
  TAlScrollBarPaintEvent = procedure (Sender: TObject; var continue: boolean; Area: TALScrollbarArea) of object;

  {----------------------------------------}
  TAlCustomScrollBar = class(TCustomControl)
  private
    FBarColor : Tcolor;
    FArrowColor : Tcolor;
    FColor : Tcolor;
    FBorderColor : Tcolor;
    FmouseInControl : Boolean;
    FMouseIsDown: Boolean;
    FMousePos: Tpoint;
    FMouseDownAt: TALScrollbarArea;
    FOnPaint: TAlScrollBarPaintEvent;
    FOnMouseIn : TNotifyEvent;
    FOnMouseOut: TNotifyEvent;
    FOnChange : TNotifyEvent;
    FonScroll: TScrollEvent;
    FArrowPoly: array[saBtnArrowDown..saBtnArrowUp] of TALArrowPoly;
    FAreaRect: TALAreaRects;
    FButtonWidth: Integer;
    FStoreButtonWidth: Integer;
    FScrollerWidth: Integer;
    FMax: Integer;
    FMin: Integer;
    FLargeChange: Integer;
    FSmallChange: Integer;
    FPosition: Integer;
    FKind: TScrollBarKind;
    Scrolling: Boolean;
    ScrollPos: TPoint;
    ScrollOffset: TPoint;
    FScrollTimer: TTimer;
    FScrollArea: TALScrollbarArea;
    FScrollerVisible : Boolean;
    procedure PaintScrollBar;
    procedure SetKind(const Value: TScrollBarKind);
    procedure SetScrollerWidth(const Value: Integer);
    procedure SetButtonWidth(const Value: Integer);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetLargeChange(const Value: Integer);
    procedure SetSmallChange(const Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure PaintArea(const Area: TALScrollbarArea);
    function CheckButtonWidth(const AWidth: Integer): Integer;
    function CheckScrollerWidth(const AWidth: Integer): Integer;
    procedure ScrollerChanged;
    procedure ScrollToPos(const X, Y: Integer);
    procedure GetScrollerInfo(var X, Y, W, H: Integer);
    procedure RecalculateArrows;
    procedure RecalculateAreas;
    procedure RecalculateScroller;
    procedure DoScrollTimer(Sender: TObject);
    Procedure TriggerScrollEvent(ScrollCode: TScrollCode; Var Pos: Integer);
  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure WndProc(var Message: TMessage); override;
    procedure Resize; override;
    property BarColor : Tcolor read FBarColor write FBarColor default ClBlack;
    property ArrowColor : Tcolor read FArrowColor write FArrowColor Default ClBlack;
    property Color: Tcolor read Fcolor Write Fcolor Default clBtnFace;
    property BorderColor : Tcolor read FBorderColor write FBorderColor default clBlack;
    property mouseInControl : Boolean read FmouseInControl;
    property MouseIsDown: Boolean read FMouseIsDown;
    property MouseDownAt: TALScrollbarArea read FMouseDownAt;
    Property OnPaint: TAlScrollBarPaintEvent read FOnPaint write FOnPaint;
    property Kind: TScrollBarKind read FKind write SetKind default sbhorizontal;
    property ButtonWidth: Integer read FStoreButtonWidth write SetButtonWidth default 18;
    property LargeChange: Integer read FLargeChange write SetLargeChange default 5;
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property Position: Integer read FPosition write SetPosition default 0;
    property SmallChange: Integer read FSmallChange write SetSmallChange default 1;
    property ScrollerWidth: Integer read FScrollerWidth write SetScrollerWidth default 15;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseIn: TNotifyEvent read FOnMouseIn write FOnMouseIn;
    property OnMouseOut: TNotifyEvent read FOnMouseOut write FOnMouseOut;
    Property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    Procedure Paint; Override;
  published
  end;

  {--------------------------------------}
  TALScrollBar = class(TALCustomScrollBar)
  published
    property BarColor;
    property ArrowColor;
    property Color;
    property BorderColor;
    property mouseInControl;
    property MouseIsDown;
    property MouseDownAt;
    Property OnPaint;
    Property OnScroll;
    property Kind;
    property Align;
    property Anchors;
    property ButtonWidth;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property LargeChange;
    property Max;
    property Min;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property ShowHint;
    property SmallChange;
    property Visible;
    property ScrollerWidth;
    property OnChange;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnStartDock;
    property OnStartDrag;
    property OnMouseIn;
    property OnMouseOut;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

{$R ..\resource\ALScrollBar.dcr}

{--------------------------------------------------------------------------------------------------------------------}
const AreaOrder: array[0..4] of TALScrollbarArea = (saBtnArrowDown, saBtnArrowUp, saTrackDown, saTrackUp, saScroller);

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALScrollBar]);
end;

{********************************************************}
constructor TALCustomScrollBar.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FBarColor := ClBlack;
  FArrowColor := ClBlack;
  FColor := clBtnFace;
  FBorderColor := ClBlack;
  FmouseInControl := False;
  FMouseIsDown:= False;
  Scrolling:= False;
  FScrollArea:= saNone;
  FLargeChange := 5;
  FSmallChange := 1;
  FMin := 0;
  FMax := 100;
  FPosition := 0;
  FScrollTimer := TTimer.Create(Self);
  with FScrollTimer do begin
    Enabled := False;
    Interval := 50;
    OnTimer := DoScrollTimer;
  end;
  FKind := SbHorizontal;
  Width := 150;
  FButtonWidth := 18;
  FStoreButtonWidth := 18;
  Height := 18;
  FScrollerWidth := 15;
  FScrollerVisible := True;
  FMouseDownAt := saNone;
  RecalculateAreas;
end;

{************************************}
destructor TALCustomScrollBar.Destroy;
begin
  FScrollTimer.Free;
  inherited;
end;

{*********************************}
procedure TALCustomScrollBar.Paint;
begin
  PaintScrollBar;
end;

{******************************************}
procedure TALCustomScrollBar.PaintScrollBar;
var Area: TALScrollbarArea;
begin
  for Area := saBtnArrowDown to saScroller do PaintArea(Area);
end;

{*******************************************************************}
procedure TALCustomScrollBar.PaintArea(const Area: TALScrollbarArea);
var AreaRect: TRect;
  Poly: TALArrowPoly;
  Continue : Boolean;
begin
  If area=SaNone then exit;

  continue := true;
  If assigned(FonPaint) then FOnPaint(Self, continue, Area);
  if not continue then exit;

  AreaRect := FAreaRect[Area];
  if (area=saTrackUp) and not enabled and FScrollerVisible then AreaRect.Top := AreaRect.Top - FScrollerWidth;

  If (area <> saScroller) or enabled then
    with Canvas do begin
      Brush.Color := Color;
      Brush.Style := bsSolid;
      iF (area <> saScroller) or FscrollerVisible then FillRect(AreaRect);

      If (area=saTrackUp) or (area=SaTrackDown) then begin
        pen.Color := borderColor;
        If FKind = sbvertical then begin
          MoveTo(AreaRect.left,AreaRect.Bottom);
          Lineto(AreaRect.left,AreaRect.top - 1);
          MoveTo(AreaRect.right-1,AreaRect.Bottom);
          Lineto(AreaRect.right-1,AreaRect.top - 1);
        end else begin
          MoveTo(AreaRect.left-1,AreaRect.Bottom-1);
          Lineto(AreaRect.right,AreaRect.bottom-1);
          MoveTo(AreaRect.left-1,AreaRect.top);
          Lineto(AreaRect.right,AreaRect.top);
        end;
      end
      else begin
        Brush.Color := borderColor;
        iF (area <> saScroller) or FscrollerVisible then FrameRect(AreaRect);
      end;
    end;

  If (area = saScroller) and enabled and FscrollerVisible then begin
    with Canvas do begin
      pen.Color := FBarColor;
      If (Fkind = SBVertical) and (AreaRect.Bottom - AreaRect.Top > 7) then begin
        MoveTo(4,AreaRect.top + (AreaRect.Bottom - AreaRect.top - 7) div 2);
        lineto(arearect.Right - 4, Penpos.Y);
        MoveTo(4,Penpos.Y + 2);
        lineto(arearect.Right - 4, Penpos.Y);
        MoveTo(4,Penpos.Y + 2);
        lineto(arearect.Right - 4, Penpos.Y);
        MoveTo(4,Penpos.Y + 2);
        lineto(arearect.Right - 4, Penpos.Y);
      end
      else if (Fkind = SBhorizontal) and (AreaRect.right - AreaRect.left > 7) then Begin
        MoveTo(AreaRect.left + (AreaRect.right - AreaRect.left - 7) div 2,4);
        lineto(Penpos.x, arearect.Bottom - 4);
        MoveTo(Penpos.x + 2,4);
        lineto(Penpos.x, arearect.Bottom - 4);
        MoveTo(Penpos.x + 2,4);
        lineto(Penpos.x, arearect.Bottom - 4);
        MoveTo(Penpos.x + 2,4);
        lineto(Penpos.x, arearect.Bottom - 4);
      end;
    end;
  end
  else If area in [saBtnArrowDown, saBtnArrowUp] then
    with Canvas do begin
        Poly := FArrowPoly[Area];
        Brush.Color := FarrowColor;
        Pen.Color := FarrowColor;
        Polygon(Poly);
    end;
end;

{**********************************************************}
procedure TALCustomScrollBar.WndProc(var Message: TMessage);
var I: Integer;
  Area: TALScrollbarArea;
begin
  case Message.Msg of
    {-------------------}
    WM_LButtonDown: begin
                      inherited;
                      If not (csDesigning in ComponentState) then begin
                        FMouseIsDown:= true;
                        FMousePos := Point(TWMLButtonDown(Message).XPos, TWMLButtonDown(Message).YPos);

                        for I := 0 to 4 do begin
                          Area := AreaOrder[I];
                          if PtInRect(FAreaRect[Area], FMousePos) then begin
                              FmouseDownAt := Area;
                              if Area = saScroller then begin
                                Scrolling := True;
                                ScrollPos := FMousePos;
                                ScrollOffset := Point(ScrollPos.x - FAreaRect[saScroller].Left, ScrollPos.y - FAreaRect[saScroller].Top);
                              end else
                              begin
                                FScrollArea := Area;
                                FScrollTimer.Enabled := True;
                              end;
                              PaintArea(Area);
                          end;
                        end;
                      end;

                    end;
    {-----------------}
    WM_LButtonUp: begin
                    inherited;
                    If FMouseIsDown then begin
                      FMouseIsDown:= False;
                      FMousePos := Point(TWMLButtonUP(Message).XPos, TWMLButtonUP(Message).YPos);
                      FmouseDownAt := saNone;

                      Scrolling := False;
                      FScrollTimer.Enabled := False;

                      for I := 0 to 4 do begin
                        Area := AreaOrder[I];
                        PaintArea(Area);
                      end;
                    end;
                   end;
    {---------------------}
    WM_LButtonDblClk: begin
                        inherited;
                        FMouseIsDown:= true;
                        FMousePos := Point(TWMLButtonDown(Message).XPos, TWMLButtonDown(Message).YPos);

                        for I := 0 to 4 do begin
                          Area := AreaOrder[I];
                          if PtInRect(FAreaRect[Area], FMousePos) then begin
                              FmouseDownAt := Area;
                              if Area = saScroller then begin
                                Scrolling := True;
                                ScrollPos := FMousePos;
                                ScrollOffset := Point(ScrollPos.x - FAreaRect[saScroller].Left, ScrollPos.y - FAreaRect[saScroller].Top);
                              end else
                              begin
                                FScrollArea := Area;
                                FScrollTimer.Enabled := True;
                              end;
                              PaintArea(Area);
                          end;
                        end;
                      end;
    {-----------------}
    WM_MOUSEMOVE: Begin
                    inherited;
                    If not (csDesigning in ComponentState) then begin
                      FMousePos := Point(TWMMouseMove(Message).XPos, TWMMouseMove(Message).YPos);

                      if FMouseDownAt = saScroller then begin
                        ScrollToPos(FMousePos.X, FMousePos.Y);
                        Exit;
                      end;
                    end;
                  end;
    {------------------}
    CM_MouseEnter: Begin
                     inherited;
                     FmouseInControl := true;
                     paintScrollBar;
                     if Assigned(FOnMouseIn) then FOnMouseIn(Self);
                   End;
    {------------------}
    CM_MouseLeave: Begin
                    Inherited;
                    FmouseInControl := False;
                    paintScrollBar;
                    MouseMove([], -1, -1);
                    if Assigned(FOnMouseOut) then FOnMouseOut(Self);
                   End;
    {-------------}
    WM_Paint: Begin
                inherited;
                PaintScrollBar;
              end;
    {------------}
    WM_Size: begin
             inherited;
             Resize;
            end;
     {-------------}
     else inherited;
  end;
end;

{**********************************************************}
procedure TALCustomScrollBar.DoScrollTimer(Sender: TObject);
var P: Integer;
begin
  if NOT PtInRect(FAreaRect[FScrollArea], FMousePos) or (FScrollArea <> FmouseDownAt) then begin
    FScrollTimer.Enabled := False;
    MouseMove([], FMousePos.x, FmousePos.y);
    Exit;
  end;

  if FScrollArea = saBtnArrowDown then begin
    P := Position - SmallChange;
    if P < Min then
      P := Min;
    Position := P;
  end else
  if FScrollArea = saBtnArrowUp then begin
    P := Position + SmallChange;
    if P > Max then
      P := Max;
    Position := P;
  end else
  if FScrollArea = saTrackDown then begin
    P := Position - LargeChange;
    if P < Min then
      P := Min;
    Position := P;
  end else
  if FScrollArea = saTrackUp then begin
    P := Position + LargeChange;
    if P > Max then
      P := Max;
    Position := P;
  end;
end;

{************************************************************}
procedure TALCustomScrollBar.ScrollToPos(const X, Y: Integer);
var AWidth, NewPos, ToPos: Integer;
    Percent: Double;
begin
  if Kind = sbHorizontal then begin
    AWidth := Width - 2 * FButtonWidth - FScrollerWidth;
    ToPos := X - FButtonWidth - ScrollOffset.x;
    Percent := ToPos / AWidth * 100;
  end else
  if Kind = sbVertical then begin
    AWidth := Height - 2 * FButtonWidth - FScrollerWidth;
    ToPos := Y - FButtonWidth - ScrollOffset.y;
    Percent := ToPos / AWidth * 100;
  end
  else Exit;

  NewPos := Round(Percent * Max / 100);
  Position := NewPos;
end;

{********************************************************************}
procedure TALCustomScrollBar.GetScrollerInfo(var X, Y, W, H: Integer);
var AWidth: Integer;
  Percent: Double;
begin
  case FKind of
    sbHorizontal: begin
      AWidth := FAreaRect[saBtnArrowUp].Left - FAreaRect[saBtnArrowDown].Right;
      H := Height;
      Y := 0;
      If FScrollerWidth < AWidth then begin
        FScrollerVisible := True;
        W := FScrollerWidth;
        AWidth := AWidth - FScrollerWidth;
        If Fmax>0 then Percent := FPosition / FMax * 100 else Percent :=0;
        X := FAreaRect[saBtnArrowDown].Right + Round(Percent * AWidth / 100);
      end
      else begin
        FScrollerVisible := False;
        W := 0;
        X := FAreaRect[saBtnArrowDown].Right;
      end;

    end;

    sbVertical: begin
      AWidth := FAreaRect[saBtnArrowUp].Top - FAreaRect[saBtnArrowDown].Bottom;
      W := Width;
      X := 0;
      If FScrollerWidth < AWidth then begin
        FScrollerVisible := True;
        H := FScrollerWidth;
        AWidth := AWidth - FScrollerWidth;
        If Fmax>0 then Percent := FPosition / FMax * 100 else Percent := 0;
        Y := FAreaRect[saBtnArrowDown].Bottom + Round(Percent * AWidth / 100);
      end
      else begin
        FScrollerVisible := False;
        H := 0;
        Y := FAreaRect[saBtnArrowDown].Bottom;
      end;
    end;
  end;
end;

{***********************************************}
procedure TALCustomScrollBar.RecalculateScroller;
var X, Y, W, H: Integer;
begin
  GetScrollerInfo(X, Y, W, H);
  FAreaRect[saScroller] := Bounds(X, Y, W, H);

  case FKind of
    sbHorizontal: begin
      FAreaRect[saTrackDown] := Rect(FAreaRect[saBtnArrowDown].Right, 0, FAreaRect[saScroller].Left, Height);
      FAreaRect[saTrackUp] := Rect(FAreaRect[saScroller].Right, 0, FAreaRect[saBtnArrowUp].Left, Height);
    end;

    sbVertical: begin
      FAreaRect[saTrackDown] := Rect(0, FAreaRect[saBtnArrowDown].Bottom, Width, FAreaRect[saScroller].Top);
      FAreaRect[saTrackUp] := Rect(0, FAreaRect[saScroller].Bottom, Width, FAreaRect[saBtnArrowUp].Top);
    end;
  end;
end;

{**************************************************************************************************************}
procedure CalculateArrowPoints(R: TRect; var P: TALArrowPoly; AArrowDirection: Integer; AProportional: Boolean);
var
  ASize: TPoint;
  ADelta: Integer;

  function GetSize: TPoint;
  begin
    if AArrowDirection in [1,2 {adUp, adDown}] then begin
      Result.X := (R.Right - R.Left - 1) div 2;
      if not Odd(Result.X) then Inc(Result.X);
      Result.Y := Result.X div 2 + 1;
    end
    else begin
      Result.Y := (R.Bottom - R.Top - 1) div 2;
      if not Odd(Result.Y) then Inc(Result.Y);
      Result.X := Result.Y div 2 + 1;
    end;
  end;

begin
  with R do begin
    if AProportional then begin
      ADelta := (Right - Left) - (Bottom - Top);
      if ADelta > 0 then InflateRect(R, -ADelta div 2, 0)
      else InflateRect(R, 0, ADelta div 2);
    end;
    ASize := GetSize;
    case AArrowDirection of
      1: //adUp:
        begin
          P[0] := Point((Left + Right - 1) div 2, MulDiv(Top + Bottom - ASize.Y, 1, 2) - 1);
          P[1] := Point((Left + Right - ASize.X) div 2, P[0].Y + ASize.Y - 1);
          P[2] := Point(P[1].X + ASize.X - 1, P[1].Y);
        end;
      2: //adDown:
        begin
          P[0] := Point((Left + Right - ASize.X) div 2, MulDiv(Top + Bottom - ASize.Y, 1, 2));
          P[1] := Point(P[0].X + ASize.X - 1, P[0].Y);
          P[2] := Point((Left + Right - 1) div 2, P[0].Y + ASize.Y - 1);
        end;
      3: //adLeft:
        begin
          P[0] := Point((Left + Right - ASize.X) div 2, (Top + Bottom) div 2);
          P[1] := Point(P[0].X + ASize.X - 1, MulDiv(Top + Bottom - ASize.Y, 1, 2));
          P[2] := Point(P[1].X, P[1].Y + ASize.Y - 1);
        end;
      4: //adRight:
        begin
          P[0] := Point((Left + Right - ASize.X) div 2, MulDiv(Top + Bottom - ASize.Y, 1, 2));
          P[1] := Point(P[0].X + ASize.X - 1, (Top + Bottom) div 2);
          P[2] := Point(P[0].X, P[0].Y + ASize.Y - 1);
        end;
    end;
  end;
end;

{*********************************************}
procedure TALCustomScrollBar.RecalculateArrows;
Var P: TALArrowPoly;
begin
  Fbuttonwidth := CheckButtonWidth(FStorebuttonwidth);

  case FKind of
    sbHorizontal: begin
      FAreaRect[saBtnArrowDown] := Bounds(0, 0, FButtonWidth, Height);
      CalculateArrowPoints(FAreaRect[saBtnArrowDown],P,3,True);
      FArrowPoly[saBtnArrowDown] := P;

      FAreaRect[saBtnArrowUp] := Bounds(Width - FButtonWidth, 0, FButtonWidth, Height);
      CalculateArrowPoints(FAreaRect[saBtnArrowUp],P,4,True);
      FArrowPoly[saBtnArrowUp] := P;
    end;

    sbVertical: begin
      FAreaRect[saBtnArrowDown] := Bounds(0, 0, Width, FButtonWidth);
      CalculateArrowPoints(FAreaRect[saBtnArrowDown],P,1,True);
      FArrowPoly[saBtnArrowDown] := P;

      FAreaRect[saBtnArrowUp] := Bounds(0, Height - FButtonWidth, Width, FButtonWidth);
      CalculateArrowPoints(FAreaRect[saBtnArrowUp],P,2,True);
      FArrowPoly[saBtnArrowUp] := P;
    end;
  end;
end;

{********************************************}
procedure TALCustomScrollBar.RecalculateAreas;
begin
  RecalculateArrows;
  RecalculateScroller;
end;

{*****************************************************************************}
function TALCustomScrollBar.CheckScrollerWidth(const AWidth: Integer): Integer;
var AMin: Integer;
begin
  AMin := 3;
  if AWidth < AMin then Result := AMin
  else Result := AWidth;
end;

{***************************************************************************}
function TALCustomScrollBar.CheckButtonWidth(const AWidth: Integer): Integer;
var AMin, AMax: Integer;
begin
  AMin := 0;
  case FKind of
    sbHorizontal : AMax := Width div 2;
    sbVertical   : AMax := Height div 2;
    else begin
      Result := AMin;
      Exit;
    end;
  end;

  if AWidth < AMin then  Result := AMin
  else if AWidth > AMax then Result := AMax
  else Result := AWidth;
end;

{****************************************************************}
procedure TALCustomScrollBar.SetKind(const Value: TScrollBarKind);
Var X : Integer;
begin
  if FKind <> Value then begin
    FKind := Value;

          if ((csDesigning in ComponentState) and
             (not (csLoading in ComponentState))) then begin
            X := Width;
            Width := Height;
            Height := X;
          end;

    RecalculateAreas;
    PaintScrollBar;
  end;
end;

{****************************************************************************}
procedure TALCustomScrollBar.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  Resize;
end;

{******************************************************************}
procedure TALCustomScrollBar.SetScrollerWidth(const Value: Integer);
var NewWidth: Integer;
begin
  NewWidth := CheckScrollerWidth(Value);
  if NewWidth <> FScrollerWidth then begin
    FScrollerWidth := NewWidth;
    RecalculateScroller;
    PaintScrollBar;
  end;
end;

{****************************************************************}
procedure TALCustomScrollBar.SetButtonWidth(const Value: Integer);
var NewWidth: Integer;
begin
  NewWidth := CheckButtonWidth(Value);
  if NewWidth <> FStoreButtonWidth then begin
    FStoreButtonWidth := NewWidth;
    FButtonWidth := NewWidth;
    RecalculateAreas;
    PaintScrollBar;
  end;
end;

{********************************************************}
procedure TALCustomScrollBar.SetMax(const Value: Integer);
begin
  if (Value <> FMax) and (Value >= 0) and (Value >= FMin) then begin
    FMax := Value;
    If position>Fmax then position := Fmax;
    RecalculateScroller;
    PaintScrollBar;
  end;
end;

{********************************************************}
procedure TALCustomScrollBar.SetMin(const Value: Integer);
begin
  if (Value <> FMin) and (Value >= 0) and (Value <= FMax)  then begin
    FMin := Value;
    RecalculateScroller;
    PaintScrollBar;
  end;
end;

{****************************************************************}
procedure TALCustomScrollBar.SetLargeChange(const Value: Integer);
begin
  if (Value <> FLargeChange) and (Value >= FSmallChange) and (Value > 0) then FLargeChange := Value;
end;

{****************************************************************}
procedure TALCustomScrollBar.SetSmallChange(const Value: Integer);
begin
  if (Value <> FSmallChange) and (Value > 0) and (Value < FLargeChange) then FSmallChange := Value;
end;

{*******************************************************}
procedure TALCustomScrollBar.SetPosition(Value: Integer);
begin
  If not enabled then exit;
  If Value <= FMin then Value := Fmin;
  If Value >= FMax then Value := Fmax;

  if (Value <> FPosition) then begin
    FPosition := Value;
    TriggerScrollEvent(scPosition, Fposition);
    RecalculateScroller;
    PaintArea(saTrackDown);
    PaintArea(saScroller);
    PaintArea(saTrackUp);
    ScrollerChanged;
  end;
end;

{******************************************************}
procedure TALCustomScrollBar.SetEnabled(Value: Boolean);
Begin
  If Enabled <> value then begin
    Position := 0;
    Inherited;
    PaintScrollBar;
  end;
end;

{*******************************************}
procedure TALCustomScrollBar.ScrollerChanged;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{**********************************}
procedure TALCustomScrollBar.Resize;
begin
  RecalculateAreas;
  inherited;
end;

{*****************************************************************************************}
Procedure TALCustomScrollBar.TriggerScrollEvent(ScrollCode: TScrollCode; Var Pos: Integer);
begin
  if assigned(FOnScroll) then FOnScroll(Self, ScrollCode, Pos);
end;

end.
