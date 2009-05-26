{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALEdit
Version:      3.51

Description:  Edit control with flat button, skin and more
              properties.
              - Flat design (no clt3D feature)
              - Alignment property
              - Button property (button with image or text in the
                Tedit)
              - Border color property
              - Mouse is down (property to know if the mouse is
                down)
              - Mouse in control (property to know if the mouse
                is in the control area)
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

Know bug :    1. Property Autosize don't work with the button

History :     30/11/2004: correct SetEditRect problem with button
              08/11/2004: correct password char
              06/11/2004: correct the button probleme when we change
                          the font
              21/06/2003: Add Mask Edit
              01/06/2003: Correct password char
              02/05/2002: add OnButtonclik Event
              16/04/2002: correct the multiline probleme
Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALEdit;

interface

uses Windows,
     Messages,
     SysUtils,
     Classes,
     Controls,
     StdCtrls,
     Graphics,
     forms,
     Clipbrd,
     MaskUtils,
     Mask,
     ALCommon;

type

  {--------------------------------}
  TALCustomEdit = class(TCustomEdit)
  private
    FCanvas: TControlCanvas;
    FFocused: Boolean;
    FbtnVisible: Boolean;
    FbtnWidth: integer;
    FbtnCaption: string;
    FGlyph: TimageList;
    FGlyphIndex: Integer;
    FBorderColor: Tcolor;
    FbtnColor: Tcolor;
    FbtnFont: Tfont;
    FMouseInControl: boolean;
    FMouseIsDown: boolean;
    FAlignment : TAlignment;
    FOnButtonClick: TNotifyEvent;
    FOnPaint: TAlPaintEvent;
    FCustomProperty: string;
    procedure SetGlyph (Value : TImageList);
    procedure SetBtnFont(Value: Tfont);
    Procedure PaintEdit;
    procedure SetEditRect;
    Procedure SetbtnVisible(Value: Boolean);
    Procedure SetBtnWidth(Value: Integer);
    Procedure SetFocused(Value: Boolean);
    function GetTextMargins: TPoint;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    Procedure ReplaceInvalidateInQueueByRefresh;    
  protected
    Procedure Loaded; override;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams);  override;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAlignment(Value: TAlignment);
    procedure KeyPress(var Key: Char); override;
    Property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property btnVisible: Boolean read FbtnVisible write SetbtnVisible default False;
    property btnWidth: integer read FbtnWidth write SetbtnWidth default 16;
    property btnCaption: string read FbtnCaption write FbtnCaption;
    property Glyph: TimageList read FGlyph  write SetGlyph;
    property GlyphIndex: Integer read FGlyphIndex  write FGlyphIndex default -1;
    property BorderColor: Tcolor read FBorderColor write FBorderColor default clblack;
    property btnColor: Tcolor read FbtnColor write FbtnColor default clBtnFace;
    property btnFont: Tfont read FbtnFont  write SetBtnFont;
    property OnPaint: TAlPaintEvent read FOnPaint write FOnPaint;
    Property CustomProperty: String read FcustomProperty write FcustomProperty;
    Property Canvas: TcontrolCanvas read FCanvas write FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ButtonClick;
    property MouseInControl: Boolean read FMouseInControl;
    property MouseIsDown: Boolean read FMouseIsDown;
  published
  end;

  {----------------------------}
  TALEdit = class(TALCustomEdit)
  published
    Property OnButtonClick;
    property Alignment;
    property btnVisible;
    property btnWidth;
    property btnCaption;
    property Glyph;
    property GlyphIndex;
    property BorderColor;
    property btnColor;
    property btnFont;
    property OnPaint;
    Property CustomProperty;
    //property BevelEdges;
    //property BevelInner;
    //property BevelKind default bkNone;
    //property BevelOuter;
    //property BiDiMode;
    //property BorderStyle;
    //property Constraints;
    //property Ctl3D;
    //property ImeMode;
    //property ImeName;
    //property ParentBiDiMode;
    //property ParentCtl3D;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property CharCase;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
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
    Property Color;
    Property Font;
    Property ParentColor;
    Property ParentFont;
  end;

  {--------------------------------------}
  TALCustomMaskEdit = class(TALCustomEdit)
  private
    FEditMask: TEditMask;
    FMaskBlank: Char;
    FMaxChars: Integer;
    FMaskSave: Boolean;
    FMaskState: TMaskedState;
    FCaretPos: Integer;
    FBtnDownX: Integer;
    FOldValue: string;
    FSettingCursor: Boolean;
    function DoInputChar(var NewChar: Char; MaskOffset: Integer): Boolean;
    function InputChar(var NewChar: Char; Offset: Integer): Boolean;
    function DeleteSelection(var Value: string; Offset: Integer; Len: Integer): Boolean;
    function InputString(var Value: string; const NewValue: string; Offset: Integer): Integer;
    function AddEditFormat(const Value: string; Active: Boolean): string;
    function RemoveEditFormat(const Value: string): string;
    function FindLiteralChar (MaskOffset: Integer; InChar: Char): Integer;
    function GetEditText: string;
    function GetMasked: Boolean;
    function GetText: TMaskedText;
    function GetMaxLength: Integer;
    function CharKeys(var CharCode: Char): Boolean;
    procedure SetEditText(const Value: string);
    procedure SetEditMask(const Value: TEditMask);
    procedure SetMaxLength(Value: Integer);
    procedure SetText(const Value: TMaskedText);
    procedure DeleteKeys(CharCode: Word);
    procedure HomeEndKeys(CharCode: Word; Shift: TShiftState);
    procedure CursorInc(CursorPos: Integer; Incr: Integer);
    procedure CursorDec(CursorPos: Integer);
    procedure ArrowKeys(CharCode: Word; Shift: TShiftState);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
  protected
    procedure ReformatText(const NewMask: string);
    procedure GetSel(var SelStart: Integer; var SelStop: Integer);
    procedure SetSel(SelStart: Integer; SelStop: Integer);
    procedure SetCursor(Pos: Integer);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function EditCanModify: Boolean; virtual;
    procedure Reset; virtual;
    function GetFirstEditChar: Integer;
    function GetLastEditChar: Integer;
    function GetNextEditChar(Offset: Integer): Integer;
    function GetPriorEditChar(Offset: Integer): Integer;
    function GetMaxChars: Integer;
    function Validate(const Value: string; var Pos: Integer): Boolean; virtual;
    procedure ValidateError; virtual;
    procedure CheckCursor;
    property EditMask: TEditMask read FEditMask write SetEditMask;
    property MaskState: TMaskedState read FMaskState write FMaskState;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default 0;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ValidateEdit; virtual;
    procedure Clear; override;
    function GetTextLen: Integer;
    property IsMasked: Boolean read GetMasked;
    property EditText: string read GetEditText write SetEditText;
    property Text: TMaskedText read GetText write SetText;
  end;

  {------------------------------------}
  TALMaskEdit = class(TALCustomMaskEdit)
  published
    Property OnButtonClick;
    property Alignment;
    property btnVisible;
    property btnWidth;
    property btnCaption;
    property Glyph;
    property GlyphIndex;
    property BorderColor;
    property btnColor;
    property btnFont;
    property OnPaint;
    Property CustomProperty;
    //property BevelEdges;
    //property BevelInner;
    //property BevelOuter;
    //property BevelKind;
    //property BevelWidth;
    //property BiDiMode;
    //property BorderStyle;
    //property Constraints;
    //property Ctl3D;
    //property ImeMode;
    //property ImeName;
    //property ParentBiDiMode;
    //property ParentCtl3D;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property CharCase;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EditMask;
    property MaxLength;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
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
    Property Color;
    Property Font;
    Property ParentColor;
    Property ParentFont;
  end;


procedure Register;

implementation

uses AlFcnMisc,
     Consts;

{$R ..\resource\ALEdit.dcr}

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALEdit]);
  RegisterComponents('Alcinoe', [TALMaskEdit]);
end;

{*****************************************}
Procedure DrawEditFace(edt: TAlCustomEdit);
var
  R: TRect;
  Wt,Ht, Wg, Hg, Xt, Yt, Xg, Yg, BordWidth: integer;

begin
    with edt do begin
      If canvas = nil then exit;

      {------- init bordwidth  -----------------}
      If BorderColor = ClNone then BordWidth := 0
      else BordWidth := 1;

      {------- Buttontext et glyph with, height  -------}
      canvas.TextFlags := canvas.TextFlags or ETO_OPAQUE;

      canvas.Brush.Style := bsclear;
      canvas.Font := BtnFont;
      Wt := canvas.TextWidth(BtnCaption);
      Ht := canvas.Textheight(BtnCaption);

      if (glyph <> nil) and (Glyphindex in [0..Glyph.Count-1]) then begin
        Wg := Glyph.width;
        Hg := Glyph.height;
      end
      else begin
        Wg := 0;
        Hg := 0;
      end;

      {--- calcul des positions des glyph et ou text -------------------}
      if (glyph <> nil) and (Glyphindex in [0..Glyph.Count-1]) then begin
        Xg := width - bordwidth - BtnWidth + ALMediumPos(BtnWidth,0,Wg);
        Yg := ALMediumPos(Height,BordWidth,Hg);
        Xt := -1;
        Yt := -1;
      end
      else begin
          Xt := width - bordwidth - BtnWidth + ALMediumPos(BtnWidth,0,Wt);
          Yt := ALMediumPos(Height,BordWidth,Ht);
          Xg := -1;
          Yg := -1;
      end;

      If Xg + wg > width - BordWidth then xg := -1;
      If Yg + Hg > height - BordWidth then Yg := -1;
      If Xt + wt > width - BordWidth then xt := -1;
      If Yt + Ht > height - BordWidth then yt := -1;

      {--------------}
      R := ClientRect;
      canvas.Brush.Style := BsSolid;

      If BorderColor <> ClNone then Begin
        canvas.Brush.Color := borderColor;
        canvas.FrameRect(R);
      end
      else begin
        canvas.Brush.Color := Color;
        canvas.FrameRect(R);
      end;

      If not BtnVisible then exit;

      inflaterect(r,-1*BordWidth,-1*BordWidth);
      R.Left := R.Right - BtnWidth;
      canvas.Brush.Color := BtnColor;
      canvas.FillRect(r);

      If BorderColor <> ClNone then begin
        canvas.pen.Style := psSolid;
        canvas.Pen.Color := BorderColor;
        canvas.Moveto(R.left - 1, R.Top);
        canvas.LineTo(R.left - 1, R.Bottom);
      end;

      If (Xg <> -1) and (Yg <> -1) then Glyph.Draw(canvas,Xg,Yg,GlyphIndex);

      If (Xt <> -1) and (Yt <> -1) then begin
        canvas.Brush.Style := bsclear;
        canvas.Font := BtnFont;
        canvas.Textout(Xt,Yt,BtnCaption);
      End;
    end;
end;




////////////////////////////////////////////////////////////////////////////////
///////////// TALCustomEdit    /////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{***************************************************}
constructor TALCustomEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCanvas := Nil;
  FFocused := False;
  FbtnVisible:= False;
  FbtnWidth:= 16;
  FbtnCaption:= '';
  FGlyph:= nil;
  FGlyphIndex:= -1;
  FBorderColor:= clblack;
  FbtnColor:= clBtnFace;
  FbtnFont:= Tfont.Create;
  FMouseInControl := False;
  FMouseIsDown := False;
  Falignment := taleftjustify;
  FCustomProperty := '';

  ParentCtl3D := False;
  Ctl3D := False;
  BevelInner := bvnone;
  BevelKind := bknone;
  BevelOuter := BVNone;
  BorderStyle := bsSingle;
  BevelEdges := [];
  ParentBiDiMode := False;
  BiDiMode := bdLeftToRight;
  ImeMode := imDontCare;
  ImeName := '';
  Text := '';

  ControlStyle := ControlStyle - [csSetCaption];
end;

{*******************************}
destructor TALCustomEdit.Destroy;
begin
  FbtnFont.free;
  Fglyph := nil;
  FCanvas.Free;
  inherited Destroy;
end;

{**********************************************************************************}
procedure TAlCustomEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then begin
    if Acomponent = Fglyph then begin
      FGlyph := nil;
      Paintedit;
    end;
  end;
end;

{*************************************************}
procedure TAlCustomEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) then Refresh;
  end;
end;

{**************************************************************}
procedure TAlCustomEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  //ES_password ne semble pas marcher avec ES_multiline (du moins en D7)
  //ES_password implique SetEditRect ne marche pas ...
  If passwordchar = #0 then Params.Style := Params.Style or ES_MULTILINE;
end;

{********************************}
procedure TALCustomEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

{*****************************}
procedure TALCustomEdit.loaded;
begin
  inherited;
  seteditRect;
end;

{**********************************}
procedure TALCustomEdit.SetEditRect;
var Loc: TRect;
    BordWidth : integer;
begin
  //This function is not compatible with passwordchar (passwordchar work only on no multiline edit
  If (not (csloading in ComponentState)) and (passwordChar = #0) then begin
    SendMessage(Handle, EM_GETRECT, 0, LongInt(@Loc));
    Loc.Bottom := ClientHeight + 1;

    If FBorderColor <> ClNone then BordWidth := 1
    Else BordWidth := 0;

    If FBtnVisible then Loc.Right := ClientWidth - FBtnWidth - BordWidth
    else Loc.Right := ClientWidth;

    Loc.Top := 0;
    Loc.Left := 1;
    SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  end;
end;

{*****************************************************}
procedure TALCustomEdit.WMPaint(var Message: TWMPaint);
const
  AlignStyle : array[Boolean, TAlignment] of DWORD =
   ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
var
  Left: Integer;
  Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
  AAlignment: TAlignment;
  BordWidth: integer;
begin
  if FCanvas = nil then begin
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
  end;

  AAlignment := FAlignment;
  if (AAlignment = taLeftJustify) or FFocused then begin
    inherited;
    Paintedit;
    Exit;
  end;

{ Since edit controls do not handle justification unless multi-line (and
  then only poorly) we will draw right and center justify manually unless
  the edit has the focus. }

  DC := Message.DC;
  if DC = 0 then DC := BeginPaint(Handle, PS);
  FCanvas.Handle := DC;
  try
    FCanvas.Font := Font;
    with FCanvas do begin

      R := ClientRect;
      If FBorderColor = ClNone then BordWidth := 0
      else BordWidth := 1;
      inflaterect(r,-1*BordWidth,-1*BordWidth);
      If BtnVisible then R.Right := R.Right - BtnWidth;

      Brush.Color := Color;
      S := Text;
      if PasswordChar <> #0 then FillChar(S[1], Length(S), PasswordChar);
      Margins := GetTextMargins;
      case AAlignment of
        taRightJustify: Left := R.Right - R.Left - TextWidth(S) - Margins.X - 1;
        Else Left := (R.Right - R.Left - TextWidth(S)) div 2;
      end;
      TextRect(R, Left, Margins.Y, S);
    end;
  finally
    FCanvas.Handle := 0;
    if Message.DC = 0 then EndPaint(Handle, PS);
  end;

  Paintedit;
end;

{********************************************************}
procedure TALCustomEdit.ReplaceInvalidateInQueueByRefresh;
begin
  ValidateRect(handle,nil);
  Refresh;
end;

{********************************}
Procedure TALCustomEdit.PaintEdit;
var  Continue : Boolean;
begin
  continue := true;
  If assigned(FonPaint) then FOnPaint(Self, continue);

  if not continue then Begin
    ReplaceInvalidateInQueueByRefresh;
    exit;
  end;

  DrawEditFace(self);
end;

{**********************************}
procedure TALCustomEdit.ButtonClick;
begin
  If not focused then setfocus;
  if Assigned(FOnButtonClick) then FOnButtonClick(Self);
end;

{**********************************************}
procedure TALCustomEdit.KeyPress(var Key: Char);
begin
  if key = chr(vk_return) then Key := #0;
  inherited KeyPress(Key);
end;

{********************************************}
function TALCustomEdit.GetTextMargins: TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  Result.X := 1;

  if NewStyleControls then Result.Y := 2
  else begin
    DC := GetDC(0);
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then I := Metrics.tmHeight;
    I := I div 4;
    Result.Y := I;
  end;
end;

{*****************************************************}
procedure TALCustomEdit.WndProc(var Message: TMessage);
Var ClipBoardText : string;
    x,z : integer;

   {------------------------------}
   Function GetBorderWidth:Integer;
   Begin
     If BorderColor <> ClNone then result := 1
     Else result := 0;
   End;


begin
  case Message.Msg of
    {-------------------}
    WM_LButtonDown: begin
                      x := selstart;
                      z := sellength;
                      inherited;
                      If not (csDesigning in ComponentState) and
                          FBtnVisible and
                         (TWMLButtonDown(Message).XPos > width - FBtnWidth - 2*Getborderwidth) then begin
                        selstart := x;
                        sellength := z;
                        FMouseIsDown := True;
                        paintEdit;
                      end;
                    end;
    {-----------------}
    WM_LButtonUp: begin
                    if FMouseIsDown then begin
                      if (TWMLButtonUP(Message).XPos > width - FBtnWidth - 2*Getborderwidth) and
                         (TWMLButtonUP(Message).XPos < width) and
                         FmouseInControl then begin
                           buttonClick;
                           TWMLButtonUP(message).XPos := width+1 ;
                         end;
                      FMouseIsDown := False;
                      PaintEdit;
                    end;
                    inherited;
                  end;
    {---------------------}
    WM_LButtonDblClk: begin
                        x := selstart;
                        z := sellength;
                        inherited;
                        If not (csDesigning in ComponentState) and
                            FBtnVisible and
                           (TWMLButtonDown(Message).XPos > width - FBtnWidth - 2*Getborderwidth) then begin
                          selstart := x;
                          sellength := z;
                          FMouseIsDown := True;
                          paintEdit;
                        end
                      end;
    {------------------}
    CM_MouseEnter: Begin
                     inherited;
                     FmouseinControl := true;
                     Paintedit;
                   End;
    {------------------}
    CM_MouseLeave: Begin
                     inherited;
                     FmouseinControl := false;
                     Paintedit;
                   End;
    {-------------------}
    CM_FontChanged: begin
                      inherited;
                      seteditRect;
                    end;
    {-------------}
    CM_Enter: begin
                SetFocused(True);
                inherited;
                SelStart := Length(Text);
                Sellength := 0;
                Paintedit;
              end;
    {------------}
    CM_Exit: begin
               inherited;
               SetFocused(False);
               If FMouseIsDown then FMouseIsDown := False;
               Paintedit;
             end;
    {------------}
    WM_Size: begin
               inherited;
               SetEditRect;
               Paintedit;
             end;
    {-----------------}
    WM_MouseMove: begin
                    inherited;
                    If FBtnVisible then begin
                      If TWMMouseMove(message).XPos > width - FBtnWidth - 2*Getborderwidth then cursor := crArrow
                      else Cursor := crdefault;
                    end;
                  end;
    {-------------}
    WM_PASTE: Begin
                Clipboard.Open;
                if Clipboard.HasFormat(CF_TEXT) then ClipBoardText := Clipboard.AsText
                else ClipBoardText := '';
                Clipboard.Close;

                If (pos(#13,ClipBoardText) = 0) and (pos(#10,ClipBoardText) = 0) then inherited;
              end;
    {--------------}
     else inherited;
  end;
end;

{****************************************************}
Procedure TALCustomEdit.SetbtnVisible(Value: Boolean);
Begin
  If Value <> FbtnVisible then Begin
    FbtnVisible := Value;
    SetEditRect;
    Refresh;
  end;
End;

{**************************************************}
Procedure TALCustomEdit.SetBtnWidth(Value: Integer);
Begin
  If Value <> Fbtnwidth then Begin
    Fbtnwidth := Value;
    SetEditRect;
    Refresh;
  end;
End;

{***********************************************}
procedure TALCustomEdit.setbtnFont(Value: TFont);
begin
  FbtnFont.Assign(Value);
end;

{****************************************************}
procedure TALCustomEdit.SetGlyph (Value : TImageList);
begin
  If Value <> FGlyph then begin
    FGlyph := Value;
    If value <> nil then value.FreeNotification(self);
    paintEdit;
  end;
end;

{***********************************}
procedure TALCustomEdit.SetAlignment;
begin
   if FAlignment <> Value then begin
    FAlignment := Value;
    Refresh;
  end;
end;




////////////////////////////////////////////////////////////////////////////////
///////////// TALCustomMaskEdit    /////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{*******************************************************}
constructor TALCustomMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaskState := [];
  FMaskBlank := DefaultBlank;
end;

{*********************************************************************}
procedure TALCustomMaskEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not FSettingCursor then inherited KeyDown(Key, Shift);
  if IsMasked and (Key <> 0) and not (ssAlt in Shift) then
  begin
    if (Key = VK_LEFT) or(Key = VK_RIGHT) then
    begin
      ArrowKeys(Key, Shift);
      if not ((ssShift in Shift) or (ssCtrl in Shift)) then
        Key := 0;
      Exit;
    end
    else if (Key = VK_UP) or(Key = VK_DOWN) then
    begin
      Key := 0;
      Exit;
    end
    else if (Key = VK_HOME) or(Key = VK_END) then
    begin
      HomeEndKeys(Key, Shift);
      Key := 0;
      Exit;
    end
    else if ((Key = VK_DELETE) and not (ssShift in Shift)) or
      (Key = VK_BACK) then
    begin
      if EditCanModify then
        DeleteKeys(Key);
      Key := 0;
      Exit;
    end;
    CheckCursor;
  end;
end;

{*******************************************************************}
procedure TALCustomMaskEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if not FSettingCursor then inherited KeyUp(Key, Shift);
  if IsMasked and (Key <> 0) then
  begin
    if ((Key = VK_LEFT) or(Key = VK_RIGHT)) and (ssCtrl in Shift) then
      CheckCursor;
  end;
end;

{**************************************************}
procedure TALCustomMaskEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if IsMasked and (Key <> #0) and not (Char(Key) in [^V, ^X, ^C]) then
  begin
    CharKeys(Key);
    Key := #0;
  end;
end;

{*********************************************************************}
procedure TALCustomMaskEdit.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  FBtnDownX := Message.XPos;
end;

{*****************************************************************}
procedure TALCustomMaskEdit.WMLButtonUp(var Message: TWMLButtonUp);
var
  SelStart, SelStop : Integer;
begin
  inherited;
  if (IsMasked) then
  begin
    GetSel(SelStart, SelStop);
    FCaretPos := SelStart;
    if (SelStart <> SelStop) and (Message.XPos > FBtnDownX) then
      FCaretPos := SelStop;
    CheckCursor;
  end;
end;

{***************************************************************}
procedure TALCustomMaskEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if (IsMasked) then
    CheckCursor;
end;

{***********************************************************}
procedure TALCustomMaskEdit.SetEditText(const Value: string);
begin
  if GetEditText <> Value then
  begin
    SetTextBuf(PChar(Value));
    CheckCursor;
  end;
end;

{*********************************************}
function TALCustomMaskEdit.GetEditText: string;
begin
  Result := inherited Text;
end;

{*********************************************}
function TALCustomMaskEdit.GetTextLen: Integer;
begin
  Result := Length(Text);
end;

{**********************************************}
function TALCustomMaskEdit.GetText: TMaskedText;
begin
  if not IsMasked then
    Result := inherited Text
  else
  begin
    Result := RemoveEditFormat(EditText);
    if FMaskSave then
      Result := AddEditFormat(Result, False);
  end;
end;

{************************************************************}
procedure TALCustomMaskEdit.SetText(const Value: TMaskedText);
var
  OldText: string;
  Pos: Integer;
begin
  if not IsMasked then
    inherited Text := Value
  else
  begin
    OldText := Value;
    if FMaskSave then
      OldText := PadInputLiterals(EditMask, OldText, FMaskBlank)
    else
      OldText := AddEditFormat(OldText, True);
    if not (msDBSetText in FMaskState) and
      (csDesigning in ComponentState) and
      not (csLoading in ComponentState) and
      not Validate(OldText, Pos) then
      raise EDBEditError.CreateRes(@SMaskErr);
    EditText := OldText;
  end;
end;

{*******************************************************}
procedure TALCustomMaskEdit.WMCut(var Message: TMessage);
begin
  if not (IsMasked) then
    inherited
  else
  begin
    CopyToClipboard;
    DeleteKeys(VK_DELETE);
  end;
end;

{*********************************************************}
procedure TALCustomMaskEdit.WMPaste(var Message: TMessage);
var
  Value: string;
  Str: string;
  SelStart, SelStop : Integer;
begin
  if not (IsMasked) or ReadOnly then
    inherited
  else
  begin
    Clipboard.Open;
    Value := Clipboard.AsText;
    Clipboard.Close;

    GetSel(SelStart, SelStop);
    Str := EditText;
    DeleteSelection(Str, SelStart, SelStop - SelStart);
    EditText := Str;
    SelStart := InputString(Str, Value, SelStart);
    EditText := Str;
    SetCursor(SelStart);
  end;
end;

{********************************************}
function TALCustomMaskEdit.GetMasked: Boolean;
begin
  Result := EditMask <> '';
end;

{**********************************************}
function TALCustomMaskEdit.GetMaxChars: Integer;
begin
  if IsMasked then
    Result := FMaxChars
  else
    Result := inherited GetTextLen;
end;

{**************************************************************}
procedure TALCustomMaskEdit.ReformatText(const NewMask: string);
var
  OldText: string;
begin
  OldText := RemoveEditFormat(EditText);
  FEditMask := NewMask;
  FMaxChars  := MaskOffsetToOffset(EditMask, Length(NewMask));
  FMaskSave  := MaskGetMaskSave(NewMask);
  FMaskBlank := MaskGetMaskBlank(NewMask);
  OldText := AddEditFormat(OldText, True);
  EditText := OldText;
end;

{**************************************************************}
procedure TALCustomMaskEdit.SetEditMask(const Value: TEditMask);
var
  SelStart, SelStop: Integer;
begin
  if Value <> EditMask then
  begin
    if (csDesigning in ComponentState) and (Value <> '') and
      not (csLoading in ComponentState) then
      EditText := '';
    if HandleAllocated then GetSel(SelStart, SelStop);
    ReformatText(Value);
    Exclude(FMaskState, msMasked);
    if EditMask <> '' then Include(FMaskState, msMasked);
    inherited MaxLength := 0;
    if IsMasked and (FMaxChars > 0) then
      inherited MaxLength := FMaxChars;
    if HandleAllocated and (GetFocus = Handle) and
       not (csDesigning in ComponentState) then
      SetCursor(SelStart);
  end;
end;

{***********************************************}
function TALCustomMaskEdit.GetMaxLength: Integer;
begin
  Result := inherited MaxLength;
end;

{*******************************************************}
procedure TALCustomMaskEdit.SetMaxLength(Value: Integer);
begin
  if not IsMasked then
    inherited MaxLength := Value
  else
    inherited MaxLength := FMaxChars;
end;

{******************************************************************************}
procedure TALCustomMaskEdit.GetSel(var SelStart: Integer; var SelStop: Integer);
begin
  SendMessage(Handle, EM_GETSEL, Integer(@SelStart), Integer(@SelStop));
end;

{**********************************************************************}
procedure TALCustomMaskEdit.SetSel(SelStart: Integer; SelStop: Integer);
begin
  SendMessage(Handle, EM_SETSEL, SelStart, SelStop);
end;

{**************************************************}
procedure TALCustomMaskEdit.SetCursor(Pos: Integer);
const
  ArrowKey: array[Boolean] of Word = (VK_LEFT, VK_RIGHT);
var
  SelStart, SelStop: Integer;
  KeyState: TKeyboardState;
  NewKeyState: TKeyboardState;
  I: Integer;
begin
  if (Pos >= 1) and (ByteType(EditText, Pos) = mbLeadByte) then Dec(Pos);
  SelStart := Pos;
  if (IsMasked) then
  begin
    if SelStart < 0 then
      SelStart := 0;
    SelStop  := SelStart + 1;
    if (Length(EditText) > SelStop) and (EditText[SelStop] in LeadBytes) then
      Inc(SelStop);
    if SelStart >= FMaxChars then
    begin
      SelStart := FMaxChars;
      SelStop  := SelStart;
    end;

    SetSel(SelStop, SelStop);
    
    if SelStart <> SelStop then
    begin
      GetKeyboardState(KeyState);
      for I := Low(NewKeyState) to High(NewKeyState) do
        NewKeyState[I] := 0;
      NewKeyState [VK_SHIFT] := $81;
      NewKeyState [ArrowKey[UseRightToLeftAlignment]] := $81;
      SetKeyboardState(NewKeyState);
      FSettingCursor := True;
      try
        SendMessage(Handle, WM_KEYDOWN, ArrowKey[UseRightToLeftAlignment], 1);
        SendMessage(Handle, WM_KEYUP, ArrowKey[UseRightToLeftAlignment], 1);
      finally
        FSettingCursor := False;
      end;
      SetKeyboardState(KeyState);
    end;
    FCaretPos := SelStart;
  end
  else
  begin
    if SelStart < 0 then
      SelStart := 0;
    if SelStart >= Length(EditText) then
      SelStart := Length(EditText);
    SetSel(SelStart, SelStart);
  end;
end;

{**************************************}
procedure TALCustomMaskEdit.CheckCursor;
var
  SelStart, SelStop: Integer;
begin
  if not HandleAllocated then  Exit;
  if (IsMasked) then
  begin
    GetSel(SelStart, SelStop);
    if SelStart = SelStop then
      SetCursor(SelStart);
  end;
end;

{********************************}
procedure TALCustomMaskEdit.Clear;
begin
  Text := '';
end;

{************************************************}
function TALCustomMaskEdit.EditCanModify: Boolean;
begin
  Result := True;
end;

{********************************}
procedure TALCustomMaskEdit.Reset;
begin
  if Modified then
  begin
    EditText := FOldValue;
    Modified := False;
  end;
end;

{***************************************************************}
function TALCustomMaskEdit.CharKeys(var CharCode: Char): Boolean;
var
  SelStart, SelStop : Integer;
  Txt: string;
  CharMsg: TMsg;
begin
  Result := False;
  if Word(CharCode) = VK_ESCAPE then
  begin
    Reset;
    Exit;
  end;
  if not EditCanModify or ReadOnly then Exit;
  if (Word(CharCode) = VK_BACK) then Exit;
  if (Word(CharCode) = VK_RETURN) then
  begin
    ValidateEdit;
    Exit;
  end;

  GetSel(SelStart, SelStop);
  if (SelStop - SelStart) > 1 then
  begin
    DeleteKeys(VK_DELETE);
    SelStart := GetNextEditChar(SelStart);
    SetCursor(SelStart);
  end;

  if (CharCode in LeadBytes) then
    if PeekMessage(CharMsg, Handle, WM_CHAR, WM_CHAR, PM_REMOVE) then
      if CharMsg.Message = WM_Quit then
        PostQuitMessage(CharMsg.wparam);
  Result := InputChar(CharCode, SelStart);
  if Result then
  begin
    if (CharCode in LeadBytes) then
    begin
      Txt := CharCode + Char(CharMsg.wParam);
      SetSel(SelStart, SelStart + 2);
    end
    else
      Txt := CharCode;
    SendMessage(Handle, EM_REPLACESEL, 0, LongInt(PChar(Txt)));
    GetSel(SelStart, SelStop);
    CursorInc(SelStart, 0);
  end;
end;

{************************************************************************}
procedure TALCustomMaskEdit.ArrowKeys(CharCode: Word; Shift: TShiftState);
var
  SelStart, SelStop : Integer;
begin
  if (ssCtrl in Shift) then Exit;
  GetSel(SelStart, SelStop);
  if (ssShift in Shift) then
  begin
    if (CharCode = VK_RIGHT) then
    begin
      Inc(FCaretPos);
      if (SelStop = SelStart + 1) then
      begin
        SetSel(SelStart, SelStop);  {reset caret to end of string}
        Inc(FCaretPos);
      end;
      if FCaretPos > FMaxChars then FCaretPos := FMaxChars;
    end
    else  {if (CharCode = VK_LEFT) then}
    begin
      Dec(FCaretPos);
      if (SelStop = SelStart + 2) and
        (FCaretPos > SelStart) then
      begin
        SetSel(SelStart + 1, SelStart + 1);  {reset caret to show up at start}
        Dec(FCaretPos);
      end;
      if FCaretPos < 0 then FCaretPos := 0;
    end;
  end
  else
  begin
    if (SelStop - SelStart) > 1 then
    begin
      if ((SelStop - SelStart) = 2) and (EditText[SelStart+1] in LeadBytes) then
      begin
        if (CharCode = VK_LEFT) then
          CursorDec(SelStart)
        else
          CursorInc(SelStart, 2);
        Exit;
      end;
      if SelStop = FCaretPos then
        Dec(FCaretPos);
      SetCursor(FCaretPos);
    end
    else if (CharCode = VK_LEFT) then
      CursorDec(SelStart)
    else   { if (CharCode = VK_RIGHT) then  }
    begin
      if SelStop = SelStart then
        SetCursor(SelStart)
      else
        if EditText[SelStart+1] in LeadBytes then
          CursorInc(SelStart, 2)
        else
          CursorInc(SelStart, 1);
    end;
  end;
end;

{***********************************************************************}
procedure TALCustomMaskEdit.CursorInc(CursorPos: Integer; Incr: Integer);
var
  NuPos: Integer;
begin
  NuPos := CursorPos + Incr;
  NuPos := GetNextEditChar(NuPos);
  if IsLiteralChar(EditMask, nuPos) then
    NuPos := CursorPos;
  SetCursor(NuPos);
end;

{********************************************************}
procedure TALCustomMaskEdit.CursorDec(CursorPos: Integer);
var
  nuPos: Integer;
begin
  nuPos := CursorPos;
  Dec(nuPos);
  nuPos := GetPriorEditChar(nuPos);
  SetCursor(NuPos);
end;

{***************************************************}
function TALCustomMaskEdit.GetFirstEditChar: Integer;
begin
  Result := 0;
  if IsMasked then
    Result := GetNextEditChar(0);
end;

{**************************************************}
function TALCustomMaskEdit.GetLastEditChar: Integer;
begin
  Result := GetMaxChars;
  if IsMasked then
    Result := GetPriorEditChar(Result - 1);
end;

{*******************************************************************}
function TALCustomMaskEdit.GetNextEditChar(Offset: Integer): Integer;
begin
  Result := Offset;
  while(Result < FMaxChars) and (IsLiteralChar(EditMask, Result)) do
    Inc(Result);
end;

{********************************************************************}
function TALCustomMaskEdit.GetPriorEditChar(Offset: Integer): Integer;
begin
  Result := Offset;
  while(Result >= 0) and (IsLiteralChar(EditMask, Result)) do
    Dec(Result);
  if Result < 0 then
    Result := GetNextEditChar(Result);
end;

{**************************************************************************}
procedure TALCustomMaskEdit.HomeEndKeys(CharCode: Word; Shift: TShiftState);
var
  SelStart, SelStop : Integer;
begin
  GetSel(SelStart, SelStop);
  if (CharCode = VK_HOME) then
  begin
    if (ssShift in Shift) then
    begin
      if (SelStart <> FCaretPos) and (SelStop <> (SelStart + 1)) then
        SelStop := SelStart + 1;
      SetSel(0, SelStop);
      CheckCursor;
    end
    else
      SetCursor(0);
    FCaretPos := 0;
  end
  else
  begin
    if (ssShift in Shift) then
    begin
      if (SelStop <> FCaretPos) and (SelStop <> (SelStart + 1)) then
        SelStart := SelStop - 1;
      SetSel(SelStart, FMaxChars);
      CheckCursor;
    end
    else
      SetCursor(FMaxChars);
    FCaretPos := FMaxChars;
  end;
end;

{*****************************************************}
procedure TALCustomMaskEdit.DeleteKeys(CharCode: Word);
var
  SelStart, SelStop : Integer;
  NuSelStart: Integer;
  Str: string;
begin
  if ReadOnly then Exit;
  GetSel(SelStart, SelStop);
  if ((SelStop - SelStart) <= 1) and (CharCode = VK_BACK) then
  begin
    NuSelStart := SelStart;
    CursorDec(SelStart);
    GetSel(SelStart, SelStop);
    if SelStart = NuSelStart then Exit;
  end;

  if (SelStop - SelStart) < 1 then Exit;

  Str := EditText;
  DeleteSelection(Str, SelStart, SelStop - SelStart);
  Str := Copy(Str, SelStart+1, SelStop - SelStart);
  SendMessage(Handle, EM_REPLACESEL, 0, LongInt(PChar(Str)));
  if (SelStop - SelStart) <> 1 then
  begin
    SelStart := GetNextEditChar(SelStart);
    SetCursor(SelStart);
  end
  else begin
    GetSel(SelStart, SelStop);
    SetCursor(SelStart - 1);
  end;
end;

{*********************************************************}
procedure TALCustomMaskEdit.CMEnter(var Message: TCMEnter);
begin
  if IsMasked and not (csDesigning in ComponentState) then
  begin
    if not (msReEnter in FMaskState) then
    begin
      FOldValue := EditText;
      inherited;
    end;
    Exclude(FMaskState, msReEnter);
    CheckCursor;
  end
  else
    inherited;
end;

{***************************************************************}
procedure TALCustomMaskEdit.CMTextChanged(var Message: TMessage);
var
  SelStart, SelStop : Integer;
  Temp: Integer;
begin
  inherited;
  FOldValue := EditText;
  if HandleAllocated then
  begin
    GetSel(SelStart, SelStop);
    Temp := GetNextEditChar(SelStart);
    if Temp <> SelStart then
      SetCursor(Temp);
  end;
end;

{***************************************************************************}
procedure TALCustomMaskEdit.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  inherited;
  if (Message.CharCode = VK_ESCAPE) and IsMasked and Modified then
    Message.Result := 1;
end;

{*******************************************************}
procedure TALCustomMaskEdit.CMExit(var Message: TCMExit);
begin
  if IsMasked and not (csDesigning in ComponentState) then
  begin
    ValidateEdit;
    CheckCursor;
  end;
  inherited;
end;

{***************************************}
procedure TALCustomMaskEdit.ValidateEdit;
var
  Str: string;
  Pos: Integer;
begin
  Str := EditText;
  if IsMasked and Modified then
  begin
    if not Validate(Str, Pos) then
    begin
      if not (csDesigning in ComponentState) then
      begin
        Include(FMaskState, msReEnter);
        SetFocus;
      end;
      SetCursor(Pos);
      ValidateError;
    end;
  end;
end;

{****************************************}
procedure TALCustomMaskEdit.ValidateError;
begin
  MessageBeep(0);
  raise EDBEditError.CreateResFmt(@SMaskEditErr, [EditMask]);
end;

{*************************************************************************************}
function TALCustomMaskEdit.AddEditFormat(const Value: string; Active: Boolean): string;
begin
  if not Active then
    Result := MaskDoFormatText(EditMask, Value, ' ')
  else
    Result := MaskDoFormatText(EditMask, Value, FMaskBlank);
end;

{***********************************************************************}
function TALCustomMaskEdit.RemoveEditFormat(const Value: string): string;
var
  I: Integer;
  OldLen: Integer;
  Offset, MaskOffset: Integer;
  CType: TMaskCharType;
  Dir: TMaskDirectives;
begin
  Offset := 1;
  Result := Value;
  for MaskOffset := 1 to Length(EditMask) do
  begin
    CType := MaskGetCharType(EditMask, MaskOffset);

    if CType in [mcLiteral, mcIntlLiteral] then
      Result := Copy(Result, 1, Offset - 1) +
        Copy(Result, Offset + 1, Length(Result) - Offset);
    if CType in [mcMask, mcMaskOpt] then Inc(Offset);
  end;

  Dir := MaskGetCurrentDirectives(EditMask, 1);
  if mdReverseDir in Dir then
  begin
    Offset := 1;
    for I := 1 to Length(Result) do
    begin
      if Result[I] = FMaskBlank then
        Inc(Offset)
      else
        break;
    end;
    if Offset <> 1 then
      Result := Copy(Result, Offset, Length(Result) - Offset + 1);
  end
  else begin
    OldLen := Length(Result);
    for I := 1 to OldLen do
    begin
      if Result[OldLen - I + 1] = FMaskBlank then
        SetLength(Result, Length(Result) - 1)
      else Break;
    end;
  end;
  if FMaskBlank <> ' ' then
  begin
    OldLen := Length(Result);
    for I := 1 to OldLen do
    begin
      if Result[I] = FMaskBlank then
        Result[I] := ' ';
      if I > OldLen then Break;
    end;
  end;
end;

{********************************************************************************}
function TALCustomMaskEdit.InputChar(var NewChar: Char; Offset: Integer): Boolean;
var
  MaskOffset: Integer;
  CType: TMaskCharType;
  InChar: Char;
begin
  Result := True;
  if EditMask <> '' then
  begin
    Result := False;
    MaskOffset := OffsetToMaskOffset(EditMask, Offset);
    if MaskOffset >= 0 then
    begin
      CType := MaskGetCharType(EditMask, MaskOffset);
      InChar := NewChar;
      Result := DoInputChar(NewChar, MaskOffset);
      if not Result and (CType in [mcMask, mcMaskOpt]) then
      begin
        MaskOffset := FindLiteralChar (MaskOffset, InChar);
        if MaskOffset > 0 then
        begin
          MaskOffset := MaskOffsetToOffset(EditMask, MaskOffset);
          SetCursor (MaskOffset);
          Exit;
        end;
      end;
    end;
  end;
  if not Result then
    MessageBeep(0)
end;

{**************************************************************************************}
function TALCustomMaskEdit.DoInputChar(var NewChar: Char; MaskOffset: Integer): Boolean;
var
  Dir: TMaskDirectives;
  Str: string;
  CType: TMaskCharType;

  function IsKatakana(const Chr: Byte): Boolean;
  begin
    Result := (SysLocale.PriLangID = LANG_JAPANESE) and (Chr in [$A1..$DF]);
  end;

  function TestChar(NewChar: Char): Boolean;
  var
    Offset: Integer;
  begin
    Offset := MaskOffsetToOffset(EditMask, MaskOffset);
    Result := not ((MaskOffset < Length(EditMask)) and
               (UpCase(EditMask[MaskOffset]) = UpCase(EditMask[MaskOffset+1]))) or
               (ByteType(EditText, Offset) = mbTrailByte) or
               (ByteType(EditText, Offset+1) = mbLeadByte);
  end;

begin
  Result := True;
  CType := MaskGetCharType(EditMask, MaskOffset);
  if CType in [mcLiteral, mcIntlLiteral] then
    NewChar := MaskIntlLiteralToChar(EditMask[MaskOffset])
  else
  begin
    Dir := MaskGetCurrentDirectives(EditMask, MaskOffset);
    case EditMask[MaskOffset] of
      mMskNumeric, mMskNumericOpt:
        begin
          if not ((NewChar >= '0') and (NewChar <= '9')) then
            Result := False;
        end;
      mMskNumSymOpt:
        begin
          if not (((NewChar >= '0') and (NewChar <= '9')) or
                 (NewChar = ' ') or(NewChar = '+') or(NewChar = '-')) then
            Result := False;
        end;
      mMskAscii, mMskAsciiOpt:
        begin
          if (NewChar in LeadBytes) and TestChar(NewChar) then
          begin
            Result := False;
            Exit;
          end;
          if IsCharAlpha(NewChar) then
          begin
            Str := ' ';
            Str[1] := NewChar;
            if (mdUpperCase in Dir)  then
              Str := AnsiUpperCase(Str)
            else if mdLowerCase in Dir then
              Str := AnsiLowerCase(Str);
            NewChar := Str[1];
          end;
        end;
      mMskAlpha, mMskAlphaOpt, mMskAlphaNum, mMskAlphaNumOpt:
        begin
          if (NewChar in LeadBytes) then
          begin
            if TestChar(NewChar) then
              Result := False;
            Exit;
          end;
          Str := ' ';
          Str[1] := NewChar;
          if IsKatakana(Byte(NewChar)) then
          begin
              NewChar := Str[1];
              Exit;
          end;
          if not IsCharAlpha(NewChar) then
          begin
            Result := False;
            if ((EditMask[MaskOffset] = mMskAlphaNum) or
                (EditMask[MaskOffset] = mMskAlphaNumOpt)) and
                (IsCharAlphaNumeric(NewChar)) then
              Result := True;
          end
          else if mdUpperCase in Dir then
            Str := AnsiUpperCase(Str)
          else if mdLowerCase in Dir then
            Str := AnsiLowerCase(Str);
          NewChar := Str[1];
        end;
    end;
  end;
end;

{**********************************************************************************}
function TALCustomMaskEdit.Validate(const Value: string; var Pos: Integer): Boolean;
var
  Offset, MaskOffset: Integer;
  CType: TMaskCharType;
begin
  Result := True;
  Offset := 1;
  for MaskOffset := 1 to Length(EditMask) do
  begin
    CType := MaskGetCharType(EditMask, MaskOffset);

    if CType in [mcLiteral, mcIntlLiteral, mcMaskOpt] then
      Inc(Offset)
    else if (CType = mcMask) and (Value <> '') then
    begin
      if (Value [Offset] = FMaskBlank) or
        ((Value [Offset] = ' ') and (EditMask[MaskOffset] <> mMskAscii)) then
      begin
        Result := False;
        Pos := Offset - 1;
        Exit;
      end;
      Inc(Offset);
    end;
  end;
end;

{****************************************************************************}
function TALCustomMaskEdit.DeleteSelection(var Value: string; Offset: Integer;
  Len: Integer): Boolean;
var
  EndDel: Integer;
  StrOffset, MaskOffset, Temp: Integer;
  CType: TMaskCharType;
begin
  Result := True;
  if Len = 0 then Exit;

  StrOffset := Offset + 1;
  EndDel := StrOffset + Len;
  Temp := OffsetToMaskOffset(EditMask, Offset);
  if Temp < 0 then  Exit;
  for MaskOffset := Temp to Length(EditMask) do
  begin
    CType := MaskGetCharType(EditMask, MaskOffset);
    if CType in [mcLiteral, mcIntlLiteral] then
      Inc(StrOffset)
    else if CType in [mcMask, mcMaskOpt] then
    begin
      Value[StrOffset] := FMaskBlank;
      Inc(StrOffset);
    end;
    if StrOffset >= EndDel then Break;
  end;
end;

{*******************************************************************************}
function TALCustomMaskEdit.InputString(var Value: string; const NewValue: string;
  Offset: Integer): Integer;
var
  NewOffset, MaskOffset, Temp: Integer;
  CType: TMaskCharType;
  NewVal: string;
  NewChar: Char;
begin
  Result := Offset;
  if NewValue = '' then Exit;
  { replace chars with new chars, except literals }
  NewOffset := 1;
  NewVal := NewValue;
  Temp := OffsetToMaskOffset(EditMask, Offset);
  if Temp < 0 then  Exit;
  MaskOffset := Temp;
  While MaskOffset <= Length(EditMask) do
  begin
    CType := MaskGetCharType(EditMask, MaskOffset);
    if CType in [mcLiteral, mcIntlLiteral, mcMask, mcMaskOpt] then
    begin
      NewChar := NewVal[NewOffset];
      if not (DoInputChar(NewChar, MaskOffset)) then
      begin
        if (NewChar in LeadBytes) then
          NewVal[NewOffset + 1] := FMaskBlank;
        NewChar := FMaskBlank;
      end;
        { if pasted text does not contain a literal in the right place,
          insert one }
      if not ((CType in [mcLiteral, mcIntlLiteral]) and
        (NewChar <> NewVal[NewOffset])) then
      begin
        NewVal[NewOffset] := NewChar;
        if (NewChar in LeadBytes) then
        begin
          Inc(NewOffset);
          Inc(MaskOffset);
        end;
      end
      else
        NewVal := Copy(NewVal, 1, NewOffset-1) + NewChar +
          Copy(NewVal, NewOffset, Length (NewVal));
      Inc(NewOffset);
    end;
    if (NewOffset + Offset) > FMaxChars then Break;
    if (NewOffset) > Length(NewVal) then Break;
    Inc(MaskOffset);
  end;

  if (Offset + Length(NewVal)) < FMaxChars then
  begin
    if ByteType(Value, OffSet + Length(NewVal) + 1) = mbTrailByte then
    begin
      NewVal := NewVal + FMaskBlank;
      Inc(NewOffset);
    end;
    Value := Copy(Value, 1, Offset) + NewVal +
      Copy(Value, OffSet + Length(NewVal) + 1,
        FMaxChars -(Offset + Length(NewVal)));
  end
  else
  begin
    Temp := Offset;
    if (ByteType(NewVal, FMaxChars - Offset) = mbLeadByte) then
      Inc(Temp);
    Value := Copy(Value, 1, Offset) +
             Copy(NewVal, 1, FMaxChars - Temp);
  end;
  Result := NewOffset + Offset - 1;
end;

{*************************************************************************************}
function TALCustomMaskEdit.FindLiteralChar(MaskOffset: Integer; InChar: Char): Integer;
var
  CType: TMaskCharType;
  LitChar: Char;
begin
  Result := -1;
  while MaskOffset < Length(EditMask) do
  begin
    Inc(MaskOffset);
    CType := MaskGetCharType(EditMask, MaskOffset);
    if CType in [mcLiteral, mcIntlLiteral] then
    begin
      LitChar := EditMask[MaskOffset];
      if CType = mcIntlLiteral then
        LitChar := MaskIntlLiteralToChar(LitChar);
      if LitChar = InChar then
        Result := MaskOffset;
      Exit;
    end;
  end;
end;

end.
