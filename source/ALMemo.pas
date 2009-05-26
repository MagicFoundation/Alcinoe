{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALMemo
Version:      3.50

Description:  Memo with onPaint property
              - Flat design (no clt3D feature)
              - Border color property
              - Mouse is down (property to know if the mouse is down)
              - Mouse in control (property to know if the mouse is
                in the control area)
              - Custom property (string to add custom informations
                like "tag" property)
              - Onpaint event (powerfull event fired each time the
                control needed to be repaint and permit to change,
                before the repaint process, some property of the control
                like it's color. For exemple we can easily change the
                font or border color of the control each time the mouse
                enter or leave the control.

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

History :     5/02/2005: correct SetEditRect problem with font changed
              30/10/2005: move WM_MEMOSCROLL = WM_user + 1 to
                          WM_MEMOSCROLL = WM_user + 1000

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALMemo;

interface

uses Windows,
     Classes,
     Messages,
     Forms,
     Graphics,
     StdCtrls,
     Controls,
     ExtCtrls,
     ALCommon,
     ALScrollBar;

{-----------------------------------}
Const WM_MEMOSCROLL = WM_user + 1000;

type

  {-------------------------------------------}
  TALMemoScrollBarProperty = class(TPersistent)
  Private
    FScrollBar : TalScrollBar;
    FOnChange: TNotifyEvent;
  protected
    procedure SetBarColor(const Value: TColor);
    procedure SetArrowColor(const Value: TColor);
    procedure Setcolor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    Function GetBarColor: Tcolor;
    Function GetArrowColor: Tcolor;
    Function Getcolor: Tcolor;
    Function GetBorderColor: Tcolor;
    Function GetmouseInControl : Boolean;
    Function GetMouseIsDown: Boolean;
    Function GetMouseDownAt: TALScrollbarArea;
    Function GetEnabled: Boolean;
  public
    procedure Changed;
    constructor Create(AOwner: TalScrollBar);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property MouseIsDown: Boolean read GetMouseIsDown;
    property MouseDownAt: TALScrollbarArea read GetMouseDownAt;
    property mouseInControl : Boolean read GetmouseInControl;
    property Enabled : Boolean read GetEnabled;
  published
    property BarColor : Tcolor read GetBarColor write SetBarColor default ClBlack;
    property ArrowColor : Tcolor read GetArrowColor write SetArrowColor Default ClBlack;
    property Color: Tcolor read Getcolor Write Setcolor Default clBtnFace;
    property BorderColor : Tcolor read GetBorderColor write SetBorderColor default clBlack;
  end;

  {--------------------------------}
  TALCustomMemo = class(TCustomMemo)
  private
    FMouseMoveTimer : Ttimer;
    FCanvas:Tcanvas;
    FcurPos: Integer;
    FVertScrollBar: TALScrollBar;
    FVertScrollBarProperty: TALMemoScrollBarProperty;
    FBorderColor: Tcolor;
    FmouseInControl : Boolean;
    FmouseIsDown: Boolean;
    FOnPaint: TAlPaintEvent;
    FOnPaintScrollBar: TAlScrollBarPaintEvent;
    FcustomProperty: String;
    Procedure PaintMemo;
    procedure PaintScrollBar(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
    procedure SetEditRect;
    Procedure ScrollBarMove(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    function GetLineHeight: Integer;
    Procedure LineChanged;
    Procedure TriggerScrollEvent(Pos: Integer);
    procedure WMMemoScroll(var message: TMessage); message WM_MEMOSCROLL;
    procedure DoMouseMoveTimer(Sender: TObject);
  protected
    procedure Change; Override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
    Property VertScrollBar: TALMemoScrollBarProperty read FVertScrollBarProperty write FVertScrollBarProperty;
    Property BorderColor: Tcolor Read FBorderColor Write FborderColor Default ClBlack;
    Property OnPaint :TAlPaintEvent read FonPaint write FonPaint;
    Property OnPaintScrollBar :TAlScrollBarPaintEvent read FonPaintScrollBar write FonPaintScrollBar;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    Property CustomProperty: String read FcustomProperty write FcustomProperty;
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy; override;
    Property MouseInControl: Boolean read FmouseInControl;
    property MouseIsDown: Boolean read FMouseIsDown;
    function Focused: Boolean; override;
  end;

  {----------------------------}
  TALMemo = class(TALCustomMemo)
  published
    Property OnPaint;
    Property OnPaintScrollBar;
    Property VertScrollBar;
    property BorderColor;
    Property CustomProperty;
    //property BevelEdges;
    //property BevelInner;
    //property BevelKind default bkNone;
    //property BevelOuter;
    //property BorderStyle;
    //property Ctl3D;
    //property ParentCtl3D;
    //property ScrollBars;
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property Lines;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

{$R ..\resource\ALMemo.dcr}

{*****************}
procedure Register;
begin
  RegisterComponents('alcinoe', [TALMemo]);
end;

////////////////////////////////////////////////////////////////////////////////
///////////////////// TALMemoScrollBarProperty /////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{****************************************************************}
constructor TALMemoScrollBarProperty.Create(AOwner: TalScrollBar);
begin
  inherited Create;
  FScrollBar := AOwner;
end;

{*****************************************}
procedure TALMemoScrollBarProperty.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{******************************************************************}
procedure TALMemoScrollBarProperty.SetBarColor(const Value: TColor);
begin
  FScrollBar.BarColor := Value;
  Changed;
end;

{********************************************************************}
procedure TALMemoScrollBarProperty.SetArrowColor(const Value: TColor);
begin
    FScrollBar.ArrowColor := Value;
    Changed;
end;

{***************************************************************}
procedure TALMemoScrollBarProperty.Setcolor(const Value: TColor);
begin
    FScrollBar.Color := Value;
    Changed;
end;

{*********************************************************************}
procedure TALMemoScrollBarProperty.SetBorderColor(const Value: TColor);
begin
    FScrollBar.BorderColor := Value;
    Changed;
end;

{****************************************************}
Function TALMemoScrollBarProperty.GetBarColor: Tcolor;
begin
  Result := FScrollBar.BarColor;
end;

{******************************************************}
Function TALMemoScrollBarProperty.GetArrowColor: Tcolor;
begin
  Result := FScrollBar.arrowColor;
end;

{*************************************************}
Function TALMemoScrollBarProperty.Getcolor: Tcolor;
begin
  Result := FScrollBar.Color;
end;

{*******************************************************}
Function TALMemoScrollBarProperty.GetBorderColor: Tcolor;
begin
  Result := FScrollBar.BorderColor;
end;

{************************************************************}
Function TALMemoScrollBarProperty.GetmouseInControl : Boolean;
begin
  Result := FScrollBar.MouseInControl;
end;

{********************************************************}
Function TALMemoScrollBarProperty.GetMouseIsDown: Boolean;
begin
  Result := FScrollBar.MouseIsDown;
end;

{*****************************************************************}
Function TALMemoScrollBarProperty.GetMouseDownAt: TALScrollbarArea;
begin
  Result := FScrollBar.MouseDownAt;
end;

{****************************************************}
Function TALMemoScrollBarProperty.GetEnabled: Boolean;
begin
  Result := FScrollBar.Enabled;
end;





////////////////////////////////////////////////////////////////////////////////
///////////////////// TALMemo                  /////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{*******************************************************}
Procedure TALCustomMemo.TriggerScrollEvent(Pos: Integer);
Var NbLineVisible: Integer;
    LineCount : Integer;
    Str : String;
Begin
 LineCount := Lines.Count;

 str := lines.text;
 If (str<>'') and (str[length(str)] in [#13,#10]) then inc(LineCount);

 If Pos > LineCount - 1 then Pos := LineCount - 1;
 If Pos < 0 then pos := 0;

 IF (FcurPos <> Pos) then begin
   FcurPos := Pos;
   NbLineVisible := LineCount - FVertScrollBar.Max;
   IF (Pos < FVertScrollBar.position) then FVertScrollBar.position := Pos
   else if (Pos >= FVertScrollBar.position + NbLineVisible) then  FVertScrollBar.position := Pos - NbLineVisible + 1;
 end;
End;

{********************************************}
function TALCustomMemo.GetLineHeight: Integer;
begin
   FCanvas.Font := Font;
   Result  := FCanvas.TextHeight('Xy');
end;

{**********************************}
Procedure TALCustomMemo.LineChanged;
Var LineHeight: Integer;
    LineCount : Integer;
    Str : String;
Begin
  IF HandleAllocated then begin
    LineHeight:= GetLineHeight;
    LineCount := Lines.Count;

    str := lines.text;
    If (str<>'') and (str[length(str)] in [#13,#10]) then inc(LineCount);

    If LineCount <= (clientheight-3) div LineHeight then FVertScrollBar.enabled := False
    else begin
      FVertScrollBar.Max := LineCount - ((clientheight-3) div LineHeight);
      FVertScrollBar.enabled := True;
    end;
  end;
end;

{***************************************************************}
procedure TalCustomMemo.CMMouseWheel(var Message: TCMMouseWheel);
begin
  If not FVertScrollBar.Enabled then Inherited
  else If (TCMMouseWheel(message).wheelDelta) < 0 then FvertScrollBar.Position := FvertScrollBar.Position + 3
  else FvertScrollBar.Position := FvertScrollBar.Position - 3
end;

{******************************************************************************************************}
Procedure TalCustomMemo.ScrollBarMove(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
Begin
  If ScrollCode = ScPosition then
    SendMessage(handle, WM_VScroll,MakeLParam(SB_THUMBPOSITION, Scrollpos), 0);
end;

{****************************************************}
constructor TAlCustomMemo.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];

  ParentCtl3D := False;
  Ctl3D := False;
  BevelInner := bvnone;
  BevelKind := bknone;
  BevelOuter := BVNone;
  BorderStyle := bsSingle;
  BevelEdges := [];
  ScrollBars := ssNone;

  FBorderColor:= clblack;
  FMouseInControl := False;
  FMouseIsDown := False;
  FcurPos := 0;
  FcustomProperty := '';

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  FVertScrollBar:= TALScrollBar.create(Self);
  FVertScrollBar.parent := Self;

  FVertScrollBarProperty:= TALMemoScrollBarProperty.create(FVertScrollBar);
  FVertScrollBar.OnScroll := ScrollBarMove;

  FMouseMoveTimer := TTimer.Create(Self);
  with FMouseMoveTimer do begin
    Enabled := False;
    Interval := 50;
    OnTimer := DoMouseMoveTimer;
  end;
end;

{*****************************}
procedure TAlCustomMemo.Loaded;
Var Pt : Tpoint;
begin
  Inherited Loaded;
  FVertScrollBar.ButtonWidth := 18;
  LineChanged;
  TriggerScrollEvent(0);
  Pt.X := 0;
  Pt.Y := 0;
  SetCaretPos(Pt);
end;

{**********************************************************************}
procedure TALCustomMemo.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);

  With FVertScrollBar do begin
    OnPaint := PaintScrollBar;
    Kind := sbVertical;
    Width := 16;
    Top := 0;
    Left := parent.Width-width;
    Height := Parent.Height;
    anchors := [AkTop,AkRight,AkBottom]
  end;

  LineChanged;
end;

{*******************************}
destructor TAlCustomMemo.Destroy;
begin
  FVertScrollBar.free;
  FVertScrollBarProperty.free;
  FCanvas.Free;
  FMouseMoveTimer.Enabled := True;
  FMouseMoveTimer.Free;
  inherited;
end;

{*****************************************************************************************************}
procedure TALCustomMemo.PaintScrollBar(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
Var     C: TControlCanvas;
Begin
  If assigned(FonPaintScrollBar) then FOnPaintScrollBar(Self, continue, Area);
  If not continue then exit;
  
  If not FVertScrollBar.Enabled then Begin
    C:= TControlCanvas.Create;
    Try
      with C do begin
        control := FVertScrollBar;
        Pen.Color := BorderColor;
        MoveTo(0,0);
        LineTo(FVertscrollBar.ClientWidth-1,0);
        LineTo(FVertscrollBar.ClientWidth-1,FVertscrollBar.Clientheight-1);
        LineTo(-1,FVertscrollBar.Clientheight-1);
      end;

    Finally
      C.free;
    end;
  end;
End;

{********************************}
Procedure TALCustomMemo.PaintMemo;
var Continue : Boolean;
begin
  continue := true;
  If assigned(FonPaint) then FOnPaint(Self, continue);
  if not continue then exit;

  with FCanvas do begin
    Brush.Color := BorderColor;
    FrameRect(ClientRect);
  end;

  FVertScrollBar.paint;
end;

{*****************************************************}
procedure TALCustomMemo.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    {-------------------}
    WM_LButtonDown: begin
                      inherited;
                      If not (csDesigning in ComponentState) then begin
                        FMouseMoveTimer.Enabled := True;
                        FMouseIsDown:= true;
                        PaintMemo;
                      end;
                    end;
    {-----------------}
    WM_LButtonUp: begin
                    inherited;
                    If FMouseIsDown then begin
                      FMouseMoveTimer.Enabled := False;
                      POSTMESSAGE(self.Handle,WM_MEMOSCROLL,0,0);
                      FMouseIsDown:= False;
                      PaintMemo;
                    end;
                   end;
    {---------------------}
    WM_LButtonDblClk: begin
                        inherited;
                        FMouseMoveTimer.Enabled := True;
                        FMouseIsDown:= true;
                        PaintMemo;
                      end;
    {------------------}
    CM_MouseEnter: Begin
                     inherited;
                     If not mouseInControl then begin
                       FmouseInControl := true;
                       PaintMemo;
                     end;
                   End;
    {------------------}
    CM_MouseLeave: Begin
                    Inherited;
                    If ptinrect(ClientRect,screentoclient(Mouse.CursorPos)) then exit;
                    FmouseInControl := False;
                    PaintMemo;
                   End;
    {-------------------}
    CM_FontChanged: begin
                      inherited;
                      seteditRect;
                    end;
    {-------------}
    WM_Paint: Begin
                inherited;
                PaintMemo;
              end;
    {------------}
    WM_Size: begin
             inherited;
             Resize;
            end;
    {-------------}
    CM_Enter: begin
                inherited;
                PaintMemo;
              end;
    {------------}
    CM_Exit: begin
               inherited;
               If FMouseIsDown then FMouseIsDown := False;
               PaintMemo;
             end;
    {-----------------}
    WM_MouseMove: begin
                    inherited;
                    If (FVertScrollBar.Visible) and (TWMMouseMove(message).XPos > width - FVertScrollBar.Width) then cursor := crArrow
                    else Cursor := crdefault;
                  end;
     {-------------}
     else inherited;
  end;
end;

{***************************************************************************}
procedure TAlCustomMemo.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  SetEditRect;
end;

{********************************}
procedure TAlCustomMemo.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

{**************************************************************}
procedure TAlCustomMemo.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

{**********************************}
procedure TAlCustomMemo.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
  Loc.Bottom := ClientHeight + 1;
  Loc.Right := ClientWidth - 16;
  Loc.Top := 0;
  Loc.Left := 3;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
end;

{**************************************}
Function TALCustomMemo.Focused: Boolean;
Begin
  Result := FVertscrollBar.Focused or inherited Focused;
End;

{*****************************}
Procedure TALCustomMemo.Change;
begin
  Inherited;
  LineChanged;
end;

{**********************************************************}
procedure TALCustomMemo.WMMemoScroll(var message: TMessage);
Var Pt : Tpoint;
    CurrentCharPos : Integer;
Begin
  If sellength = 0 then CurrentCharPos := SelStart
  Else Begin
    Windows.GetCaretPos(Pt);
    If (Pt.X < 0) or (pt.Y < 0) then exit;
    CurrentCharPos := LoWord(perform(EM_CHARFROMPOS, 0, MakeLong(Pt.X, Pt.Y)));
    If (CurrentCharPos <> SelStart) and (CurrentCharPos <> SelStart + SelLength) then begin
      If abs(CurrentCharPos-SelStart) < abs(CurrentCharPos-(SelStart+SelLength)) then CurrentCharPos := SelStart
      else CurrentCharPos := SelStart + SelLength;
    end;
  end;

  TriggerScrollEvent(perform(em_linefromchar,CurrentCharPos,0));
end;

{*****************************************************************}
procedure TALCustomMemo.KeyDown(var Key: Word; Shift: TShiftState);
Var I : Integer;
    LineCount : Integer;
    Str : String;
begin
  inherited;
  If (Key=34) or (key=33) then Begin
    LineCount := Lines.Count;

    str := lines.text;
    If (str<>'') and (str[length(str)] in [#13,#10]) then inc(LineCount);

    If Key=34 then begin
      For i := 1 to LineCount - FVertScrollBar.Max do begin
        PostMessage(Handle, WM_KEYDOWN, VK_DOWN, 0);
        PostMessage(Handle, WM_KEYUP, VK_DOWN, 0);
      end;
    end
    else begin
      For i := 1 to LineCount - FVertScrollBar.Max do begin
        PostMessage(Handle, WM_KEYDOWN, VK_UP, 0);
        PostMessage(Handle, WM_KEYUP, VK_UP, 0);
      end;
    end;

    Key:= 0;
  End
  else POSTMESSAGE(self.Handle,WM_MEMOSCROLL,0,0);
end;

{**********************************************}
procedure TALCustomMemo.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  POSTMESSAGE(self.Handle,WM_MEMOSCROLL,0,0);
end;

{********************************************************}
procedure TALCustomMemo.DoMouseMoveTimer(Sender: TObject);
begin
  POSTMESSAGE(self.Handle,WM_MEMOSCROLL,0,0);
end;

end.
