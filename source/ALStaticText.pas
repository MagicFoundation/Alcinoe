{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALStaticText
Version:      3.50

Description:  StaticText with onPaint property
              - Flat design (no clt3D feature)
              - Draw focus rect
              - Key is down (property to know if one key is push)
              - Mouse is down (property to know if the mouse is down)
              - Mouse in control (property to know if the mouse is in
                the control area)
              - Custom property (string to add custom informations
                like "tag" property)
              - Onpaint event (powerfull event fired each time the
                control needed to be repaint and permit to change,
                before the repaint process, some property of the
                control like it's color. For exemple we can easily
                change the font or border color of the control each
                time the mouse enter or leave the control.

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

History :     08/11/2004: correct the disabled look to flat
              08/11/2004: draw focus rect in same color of the font
              19/11/2004: change CM_FontChanged to replace the Invalidate
                          call by DrawLabelFace procedure
                          
Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALStaticText;

interface

uses Windows,
     Messages,
     Classes,
     Controls,
     Graphics,
     forms,
     ALCommon;

type

  {--------------------------------------}
  TALCustomStaticText = class(TWinControl)
  private
    FDrawFocusRect: Boolean;
    FMouseInControl: boolean;
    FMouseIsDown: boolean;
    FKeyIsDown: boolean;
    FOnPaint: TAlPaintEvent;
    FCustomProperty: string;
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FFocusControl: TWinControl;
    FShowAccelChar: Boolean;
    Procedure PaintStaticText;
    procedure SetAlignment(Value: TAlignment);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetShowAccelChar(Value: Boolean);
    Procedure ReplaceInvalidateInQueueByRefresh;
  protected
    procedure WndProc(var Message: TMessage); override;
    property OnPaint: TAlPaintEvent read FOnPaint write FOnPaint;
    Property CustomProperty: String read FcustomProperty write FcustomProperty;
    Property DrawFocusRect: Boolean read FDrawFocusRect write FDrawFocusRect default True;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAutoSize(Value: Boolean); override;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
  public
    constructor Create(AOwner: TComponent); override;
    property MouseInControl: Boolean read FMouseInControl;
    property MouseIsDown: Boolean read FMouseIsDown;
    property KeyIsDown: Boolean read FKeyIsDown;
    procedure Click; override;
    Function AdjustBounds: Boolean;
  published
  end;

  {----------------------------------------}
  TALStaticText = class(TALCustomStaticText)
  published
    property OnPaint;
    Property DrawFocusRect;
    Property CustomProperty;
    //property BevelEdges;
    //property BevelInner;
    //property BevelKind default bkNone;
    //property BevelOuter;
    //property BorderStyle;
    //property Transparent;
    property FocusControl;
    property ShowAccelChar;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property tabOrder;
    property tabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

{$R ..\resource\ALStaticText.dcr}

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALStaticText]);
end;

{*******************************************************************}
Procedure DrawLabelFace(C: Tcanvas; StaticText: TALCustomStaticText);
const Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var R: Trect;
    Flags: Longint;
    i: integer;
begin
  With StaticText do begin
    If not HandleAllocated then exit;
    if AdjustBounds then begin
      ReplaceInvalidateInQueueByRefresh;
      exit;
    end;

    R.Left := 0;
    R.Top := 0;
    R.Bottom := Height;
    R.Right := Width;
    C.Brush.Color := Color;
    C.Brush.Style := bsSolid;
    C.Font := Font;

    If enabled and focused and DrawFocusRect then begin
      i :=0;
      while i < R.Right do begin
        C.Pixels[i,0] := Font.Color;
        C.Pixels[i,R.Bottom - 1] := Font.Color;
        inc(i,2);
      end;
      i :=0;
      while i < R.Bottom do begin
        C.Pixels[0,i] := Font.Color;
        C.Pixels[r.right-1,i] := Font.Color;
        inc(i,2);
      end;
    end
    else C.FrameRect(r);

    Flags := DT_EXPANDTABS or Alignments[Alignment];
    if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
    Flags := DrawTextBiDiModeFlags(Flags);
    R.Left := R.Left + 1;
    R.Top := R.Top + 1;
    R.Bottom := R.Bottom - 1;
    R.Right := R.Right - 1;
    DrawText(C.Handle, PChar(caption), Length(caption), R, Flags);
  end;
end;




////////////////////////////////////////////////////////////////////////////////
///////////// TALCustomStaticText  /////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{**************************************************************}
procedure TALCustomStaticText.ReplaceInvalidateInQueueByRefresh;
begin
  ValidateRect(handle,nil);
  Refresh;
end;

{*************************************************}
Function TALCustomStaticText.AdjustBounds: Boolean;
const Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var DC: HDC;
    SaveFont: HFont;
    Rect: TRect;
    aText: string;
    Flags: Longint;
begin
  Result := False;
  if not (csReading in ComponentState) and AutoSize then begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    aText := Caption;
    if (aText = '') or ShowAccelChar and (aText[1] = '&') and (aText[2] = #0) then aText := aText + ' ';
    Flags := DT_CALCRECT or DT_EXPANDTABS or Alignments[Alignment];
    if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
    Flags := DrawTextBiDiModeFlags(Flags);
    Rect.Left := 0;
    Rect.Top := 0;
    Rect.Bottom := 0;
    Rect.Right := 0;
    DrawText(DC, PChar(aText), Length(aText), Rect, Flags);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    If (width <> Rect.Right + 2) or (Height <> Rect.Bottom + 2) then begin
      SetBounds(Left, Top, Rect.Right + 2, Rect.Bottom + 2);
      Result := True;
    end;
  end;
end;

{********************************************}
Procedure TALCustomStaticText.PaintStaticText;
var Continue : Boolean;
    c : TcontrolCanvas;
begin
  continue := True;
  If assigned(FonPaint) then FOnPaint(Self, continue);

  if not continue then Begin
    ReplaceInvalidateInQueueByRefresh;
    exit;
  end;

  C := TControlCanvas.Create;
  try
    C.Control := self;
    DrawLabelFace(
                  c,
                  self
                 );
  finally
    c.Free;
  end;
end;

{*********************************************************}
constructor TALCustomStaticText.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);

  FMouseInControl := False;
  FMouseIsDown := False;
  FKeyIsDown:= False;
  FDrawFocusRect := True;
  FCustomProperty := '';

  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption, csReplicatable, csDoubleClicks, csOpaque];
  Width := 65;
  Height := 17;
  FAutoSize := True;
  FShowAccelChar := True;
end;

{****************************************************************************************}
procedure TALCustomStaticText.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then FFocusControl := nil;
end;

{************************************************************}
procedure TALCustomStaticText.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    PaintStaticText;
  end;
end;

{********************************************************}
procedure TALCustomStaticText.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then begin
    FAutoSize := Value;
    if value then PaintStaticText;
  end;
end;

{****************************************************************}
procedure TALCustomStaticText.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

{*************************************************************}
procedure TALCustomStaticText.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then begin
    FShowAccelChar := Value;
    PaintStaticText;
  end;
end;

{***********************************************************}
procedure TALCustomStaticText.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    {---------------}
    WM_KeyDown: begin
                  inherited;
                  with TWMKey(Message) do
                    if  (CharCode = VK_SPACE) and
                        Focused and
                        (KeyDataToShiftState(KeyData) = []) and
                        not FMouseIsDown and
                        not FKeyIsDown then
                      begin
                        FKeyIsDown := True;
                        PaintStaticText;
                      end;
                end;
    {---------------}
    WM_KEYUP:   begin
                  inherited;
                  If FKeyIsDown then begin
                    click;
                    FKeyIsDown := False;
                    PaintStaticText;
                  end;
                end;
    {-------------------}
    WM_LButtonDown: begin
                      inherited;
                      If not (csDesigning in ComponentState) and
                         not FKeyIsDown then begin
                        FMouseIsDown := True;
                        PaintStaticText;
                      end;
                    end;
    {-----------------}
    WM_LButtonUp: begin
                    inherited;
                    If FMouseIsDown then begin
                      FMouseIsDown := False;
                      PaintStaticText;
                    end;
                  end;
    {---------------------}
    WM_LButtonDblClk: begin
                          inherited;
                          If not FKeyIsDown then begin
                          FMouseIsDown := True;
                          PaintStaticText;
                        end;
                      end;
    {-------------------}
    CM_DIALOGCHAR:  begin
                       inherited;
                       if enabled and ShowAccelChar and IsAccel(TWMKey(Message).CharCode, Caption) and not FKeyIsDown and not FMouseIsDown then begin
                         if (FFocusControl <> nil) and (FFocusControl.CanFocus) then begin
                           FFocusControl.SetFocus;
                           Message.Result := 1;
                         end
                         else if canfocus then begin
                            FKeyIsDown := True;
                            PaintStaticText;
                            Click;
                            FKeyIsDown := False;
                            PaintStaticText;
                            Message.Result := 1;
                          end;
                       end;
                    end;
    {------------------}
    CM_MouseEnter: Begin
                     inherited;
                     FmouseinControl := true;
                     PaintStaticText;
                   End;
    {------------------}
    CM_MouseLeave: Begin
                     inherited;
                     FmouseinControl := false;
                     PaintStaticText;
                   End;
    {-------------}
    CM_Enter: begin
                inherited;
                PaintStaticText;
              end;
    {------------}
    CM_Exit: begin
               inherited;
               If FKeyIsDown then FKeyIsDown := False;
               If FMouseIsDown then FMouseIsDown := False;
               PaintStaticText;
             end;
    {-------------}
    WM_Paint: Begin
                inherited;
                PaintStaticText;
              end;
    {----------------------}
    CM_EnabledChanged: Begin
                         inherited;
                         PaintStaticText;
                       end;
    {-------------------}
    CM_TextChanged: Begin
                      inherited;
                      PaintStaticText;
                    end;
    {-------------}
    else inherited;
  end;
end;

{**********************************}
procedure TALCustomStaticText.Click;
begin
  If not focused then setfocus;
  inherited;
end;

end.
