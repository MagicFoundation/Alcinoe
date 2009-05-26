{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALButton
Version:      3.50

Description:  TALButton
              Button control with OnPaint event, transparent, glyphs, ...

              TALRadioButton  Version 3.00
              RadioControl with OnPaint event, transparent, glyphs, ...

              TAlCheckBox Version 3.00
              CheckBox with OnPaint event, transparent, glyphs, ...

              For all of this component:
              - Flat design (no clt3D feature)
              - Border color property
              - Mark color property (for check box)
              - Transparent property
              - Focus style property
              - Key is down (property to know if one key is push)
              - Mouse is down (property to know if the mouse is down)
              - Mouse in control (property to know if the mouse is
                in the control area)
              - Custom property (string to add custom informations
                like "tag" property)
              - Onpaint event (powerfull event fired each time the control
                needed to be repaint and permit to change, before the
                repaint process, some property of the control like it's color.
                For exemple we can easily change the font or border color
                of the control each time the mouse enter or leave the control.

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

History :     19/11/2004: change CM_FontChanged and CM_ColorChanged to
                          replace the Invalidate call by paintbutton
                          procedure
              30/11/2004: Remplace refresh by paint in TalGraphicButton
                          if transparent and Not full Repaint

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
Unit ALButton;

interface

Uses Windows,
     Messages,
     Classes,
     Controls,
     Forms,
     Graphics,
     StdCtrls,
     Buttons,
     ALCommon;

type
  {-------------------------------------}
  TALFocusStyle = (ALFSNone, ALFSBorder);

  {---------------------------------------}
  TALButtonCategory = (Btn, Radio, ChkBox);

  {-------------------------------------}
  TALCustomButton = class(TButtonControl)
  private
    FAllowGrayed: Boolean;
    FMarkColor: Tcolor;
    FBlankColor: Tcolor;
    FSpacing : Integer;
    FLayout : TButtonLayout;
    FFocusStyle : TALFocusStyle;
    FGlyph : TImageList;
    FBorderColor : Tcolor;
    FGlyphIndex : integer;
    FState : TCheckBoxState;
    FGroupIndex: Integer;
    FMouseInControl: boolean;
    FMouseIsDown: boolean;
    FKeyIsDown: boolean;
    FModalResult: TModalResult;
    FCancel: Boolean;
    FDefault: Boolean;
    Fautosize: Boolean;
    FOnPaint: TAlPaintEvent;
    FcustomProperty : String;
    procedure SetGlyph (Value : TImageList);
    procedure SetState(Value: TCheckBoxState);
    Procedure PaintButton;
    Procedure ReplaceInvalidateInQueueByRefresh;
  protected
    procedure Toggle; virtual;
    function GetChecked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    Function GetButtonCategory : TALButtonCategory; Virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure SetAutoSize(Value: Boolean); override;
    property Autosize: Boolean read FAutoSize Write SetAutoSize default False;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property Default: Boolean read FDefault write FDefault default False;
    property Cancel: Boolean read FCancel write FCancel default False;
    property State: TCheckBoxState read FState write SetState default cbunchecked;
    Property GroupIndex: Integer read FgroupIndex Write FGroupIndex Default 0;
    Property GlyphIndex: integer read FGlyphIndex Write FGlyphIndex Default -1;
    property Glyph : TImageList read FGlyph write SetGlyph;
    property Spacing: Integer read FSpacing write FSpacing default 4;
    property Layout: TButtonLayout read FLayout write FLayout default blGlyphLeft;
    property FocusStyle: TALFocusStyle read FFocusStyle write FFocusStyle default ALFSBorder;
    property OnPaint: TAlPaintEvent read FOnPaint write FOnPaint;
    Property BorderColor: Tcolor read FBorderColor Write FBorderColor Default Clblack;
    property Markcolor: Tcolor Read FMarkcolor Write FMarkcolor default clBlack;
    property Blankcolor: Tcolor Read FBlankcolor Write FBlankcolor default clwhite;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    Property CustomProperty: String read FcustomProperty write FcustomProperty;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property MouseInControl: Boolean read FMouseInControl;
    property MouseIsDown: Boolean read FMouseIsDown;
    property KeyIsDown: Boolean read FKeyIsDown;
  end;

  {---------------------------------------------}
  TALCustomGraphicButton = class(TGraphicControl)
  private
    FAllowGrayed: Boolean;
    FMarkColor: Tcolor;
    FBlankColor: Tcolor;
    FSpacing : Integer;
    FLayout : TButtonLayout;
    FGlyph : TImageList;
    FBorderColor : Tcolor;
    FGlyphIndex : integer;
    FState : TCheckBoxState;
    FGroupIndex: Integer;
    FTransparent : Boolean;
    FFullRepaint : Boolean;
    FMouseInControl: boolean;
    FKeyIsDown: boolean;
    FMouseIsDown: boolean;
    FModalResult: TModalResult;
    FCancel: Boolean;
    FDefault: Boolean;
    Fautosize: Boolean;
    FOnPaint: TAlPaintEvent;
    FCustomProperty: String;
    procedure SetGlyph (Value : TImageList);
    procedure SetState(Value: TCheckBoxState);
    procedure SetTransparent(Value: Boolean);
    Procedure Fastinvalidate;
    Procedure ReplaceInvalidateInQueueByRefresh;
  protected
    procedure Toggle; virtual;
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
    Function GetButtonCategory : TALButtonCategory; Virtual;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure Paint; override;
    Procedure SetAutoSize(Value: Boolean); override;
    property Glyph : TImageList read FGlyph write SetGlyph;
    Property BorderColor: Tcolor read FBorderColor Write FBorderColor Default Clblack;
    Property GlyphIndex: integer read FGlyphIndex Write FGlyphIndex Default -1;
    property Autosize: Boolean read FAutoSize Write SetAutoSize default False;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property Default: Boolean read FDefault write FDefault default False;
    property Cancel: Boolean read FCancel write FCancel default False;
    property State: TCheckBoxState read FState write SetState default cbunchecked;
    Property GroupIndex: Integer read FgroupIndex Write FGroupIndex Default 0;
    Property FullRepaint: Boolean read FFullRepaint write FFullRepaint Default True;
    property Spacing: Integer read FSpacing write FSpacing default 4;
    property Layout: TButtonLayout read FLayout write FLayout default blGlyphLeft;
    property OnPaint: TAlPaintEvent read FOnPaint write FOnPaint;
    property Markcolor: Tcolor Read FMarkcolor Write FMarkcolor default clBlack;
    property Blankcolor: Tcolor Read FBlankcolor Write FBlankcolor default clwhite;
    property checked: boolean Read GetChecked Write SetChecked;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    Property CustomProperty: String read FcustomProperty write FcustomProperty;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property MouseInControl: Boolean read FMouseInControl;
    property MouseIsDown: Boolean read FMouseIsDown;
    property KeyIsDown: Boolean read FKeyIsDown;
  end;

  {--------------------------------}
  TALButton = class(TALCustomButton)
  published
    Property CustomProperty;
    property Spacing;
    property Layout;
    property FocusStyle;
    property BorderColor;
    property GlyphIndex;
    Property Font;
    property ParentFont;
    Property Color;
    Property ParentColor;    
    property Glyph;
    Property State;
    Property GroupIndex;
    Property AutoSize;
    property Caption;
    property Anchors;
    property Cancel;
    property Default;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ModalResult;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
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
    Property OnPaint;
  end;

  {----------------------------------------------}
  TALGraphicButton = class(TALCustomGraphicButton)
  published
    Property CustomProperty;
    property Spacing;
    property Layout;
    property BorderColor;
    property GlyphIndex;
    Property Font;
    property ParentFont;
    Property Color;
    Property ParentColor;
    property Glyph;
    Property State;
    Property GroupIndex;
    Property Transparent;
    Property FullRepaint;
    Property AutoSize;
    property Caption;
    property Anchors;
    property Cancel;
    property Default;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ModalResult;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    Property OnPaint;
  end;

  {-------------------------------------------}
  TALCustomRadioButton = class(TALCustomButton)
  private
  protected
    Function GetButtonCategory : TALButtonCategory; override;
  public
  published
  end;

  {------------------------------------------}
  TALRadioButton = class(TALCustomRadioButton)
  published
    Property CustomProperty;
    Property BlankColor;
    Property MarkColor;
    property Checked;
    property Spacing;
    property Layout;
    property BorderColor;
    property GlyphIndex;
    Property Font;
    property ParentFont;
    Property Color;
    Property ParentColor;
    property Glyph;
    Property AutoSize;
    property Caption;
    property Anchors;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
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
    Property OnPaint;
  end;

  {--------------------------------------------------}
  TALCustomGraphicRadioButton = class(TALCustomButton)
  private
  protected
    Function GetButtonCategory : TALButtonCategory; override;
  public
  published
  end;

  {----------------------------------------}
  TALCustomCheckBox = class(TALCustomButton)
  private
  protected
    Function GetButtonCategory : TALButtonCategory; override;
  public
  published
  end;

  {------------------------------------}
  TALCheckBox = class(TALCustomCheckBox)
  published
    Property CustomProperty;
    property AllowGrayed;
    property Checked;
    Property BlankColor;
    Property MarkColor;
    property Spacing;
    property Layout;
    property BorderColor;
    property GlyphIndex;
    Property Font;
    property ParentFont;
    Property Color;
    Property ParentColor;
    property Glyph;
    Property State;
    Property AutoSize;
    property Caption;
    property Anchors;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
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
    Property OnPaint;
  end;

procedure Register;

implementation

uses Math,
     AlFcnMisc;

{$R ..\resource\ALButton.dcr}

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TAlButton]);
  RegisterComponents('Alcinoe', [TALGraphicButton]);
  RegisterComponents('alcinoe', [TALRadioButton]);
  RegisterComponents('alcinoe', [TALCheckBox]);
end;

{**********************************}
Procedure DrawButtonFace(C: Tcanvas;
                         Btn : Tcontrol;
                         Glyph: TImageList;
                         GlyphIndex,
                         Spacing: Integer;
                         Layout : TButtonLayout;
                         Caption: string;
                         FocusStyle : TALFocusStyle;
                         Focused,
                         Autosize,
                         Transparent : boolean;
                         Font : Tfont;
                         Color,
                         BorderColor,
                         MarkColor,
                         BlankColor : Tcolor;
                         State : TCheckBoxState;
                         ButtonCategory : TAlButtonCategory);
var
  R: TRect;
  i, J, AccelCharPos, Wt,Ht, Wg, Hg, Xt, Yt, Xg, Yg, MinWidth, MinHeight : integer;
  Text, TextBeforeAccelChar, TextAccelChar, TextAfterAccelChar : String;
  BorderWidth : integer;
  MaxBorderWidth : integer;
begin
  {-----}
  if not(
         (btn.Visible or (csDesigning in Btn.ComponentState) and
         not (csNoDesignVisible in Btn.ControlStyle)) and
         (btn.Parent <> nil) and
         Btn.Parent.HandleAllocated
        )
  then Exit;

  {--------------------------------------------------------------------------}
  If (ButtonCategory in [radio, chkbox]) or (borderColor = CLnone) then begin
    BorderWidth := 0;
    MaxBorderWidth := 0;
  end
  else begin
    BorderWidth := 1;
    MaxBorderWidth := 1;
    If (FocusStyle = ALFSBorder)  then begin
      inc(MaxBorderWidth);
      if Focused then inc(BorderWidth)
    end;
  end;

  {------- text with  et text height avec accel Char--------------}
  if transparent then c.TextFlags := c.TextFlags and not ETO_OPAQUE
  else c.TextFlags := c.TextFlags or ETO_OPAQUE;

  C.Brush.Style := bsclear;
  C.Font.Assign(Font);
  Text := Caption;
  i := 1;
  AccelCharPos := 0;
  while i <= length(text)-1 do begin
    If (text[i] = '&') then
      if (text[i+1] <> '&') then begin
        AccelCharPos := i;
        delete(text,i,1);
      end
      else delete(text,i,1);
    i := i + 1;
  end;

  if (AccelCharPos > 0) and not (FsUnderline in C.Font.Style) then begin
    TextBeforeAccelChar := copy(text,1,AccelCharPos - 1);
    TextAccelChar := copy(text,AccelCharPos,1);
    TextAfterAccelChar := Copy(text,AccelCharPos + 1,length(text));

    Wt := C.TextWidth(TextBeforeAccelChar);
    C.Font.Style := C.Font.Style + [FSUnderline];
    Wt := wt + C.TextWidth(TextAccelChar);
    C.Font.Style := C.Font.Style - [FSUnderline];
    Wt := wt + C.TextWidth(TextAfterAccelChar);
  end
  else Wt := C.TextWidth(text);
  Ht := C.Textheight(text);

  {-----------------------------------------------------------------}
  if (glyph <> nil) and (Glyphindex in [0..Glyph.Count-1]) then begin
    Wg := Glyph.Width;
    Hg := Glyph.height;
  end
  else begin
    Wg := 0;
    Hg := 0;
  end;
  Xt := -1;
  Yt := -1;
  xg := -1;
  Yg := -1;

  {------- procedure Image with et height pour radioButton--------}
  If (glyph = nil) then begin
    If ButtonCategory=Radio then begin
      Wg := 11;
      Hg := 11;
    end
    else If ButtonCategory=chkbox then begin
      Wg := 11;
      Hg := 11;
    end;
  end;

  {-procedure Autosize--}
  If Autosize then  begin

    If (ButtonCategory in [radio, chkbox]) then begin
      Inc(wt,4);
      Inc(ht,4);
    end;

    If (wt > 0) and (Wg > 0) then begin
      If Layout in [blGlyphLeft,blGlyphRight] then minWidth := Wt + spacing + Wg
      else minWidth := Max(Wt,Wg)
    end
    else minWidth := Max(Wt,Wg);

    If (ht > 0) and (hg > 0) then begin
      If Layout in [blGlyphLeft, blGlyphRight] then minheight := max(Ht,Hg)
      else minheight := Ht + spacing + Hg
    end
    else minheight := Max(Ht,Hg);

    If ((Minwidth <> 0) and (minHeight <> 0)) and
       ((minWidth <> btn.width - Maxborderwidth*2) or (MinHeight <> btn.height - Maxborderwidth*2)) then begin
      btn.SetBounds(btn.left,btn.top,minWidth + (Maxborderwidth*2),minHeight + (Maxborderwidth*2));
      If Btn is TALCustomButton then (btn as TALCustomButton).ReplaceInvalidateInQueueByRefresh
      else if Btn is TALCustomGraphicButton then (btn as TALCustomGraphicButton).ReplaceInvalidateInQueueByRefresh;
      Exit;
    end;

    If (ButtonCategory in [radio, chkbox]) then begin
      Dec(wt,4);
      Dec(ht,4);
    end;

  end;

  {--Début des Calculs des positions-}
  If (Wg <> 0) or (Hg <> 0) then begin
    If (text = '') then begin
        Xg := ALMediumPos(btn.width,BorderWidth,Wg);
        Yg := ALMediumPos(btn.height,BorderWidth,Hg);
        Xt := -1;
        Yt := -1;
    end
    else begin
      Case layout of
        blGlyphLeft : begin
                        Xg := ALMediumPos(btn.width,BorderWidth,Wg+Spacing+Wt);
                        Yg := ALMediumPos(btn.height,BorderWidth,Hg);
                        Xt := Xg + Wg + Spacing;
                        Yt := ALMediumPos(btn.height,BorderWidth,Ht);
                      end;
        blGlyphRight : begin
                        Xt := ALMediumPos(btn.width,BorderWidth,Wt+Spacing+Wg);
                        Yt := ALMediumPos(btn.height,BorderWidth,Ht);
                        XG := Xt + wt + Spacing;
                        YG := ALMediumPos(btn.height,BorderWidth,Ht);
                       end;
        blGlyphTop : begin
                        XG := ALMediumPos(btn.width,BorderWidth,Wg);
                        YG := ALMediumPos(btn.height,BorderWidth,ht+Spacing+hg);
                        Xt := ALMediumPos(btn.width,BorderWidth,Wt);
                        Yt := YG + Hg + Spacing;
                     end;
        blGlyphBottom : begin
                          Xt := ALMediumPos(btn.width,BorderWidth,Wt);
                          Yt := ALMediumPos(btn.height,BorderWidth,Ht+Spacing+Hg);
                          XG := ALMediumPos(btn.width,BorderWidth,Wg);
                          YG := Yt + Ht + Spacing;
                        end;
      end;
    end;
  end
  else begin
    If (text = '') then begin
      Xt := -1;
      Yt := -1;
      Xg := -1;
      Yg := -1;
    end
    else begin
      Xt := ALMediumPos(btn.width,BorderWidth,Wt);
      Yt := ALMediumPos(btn.height,BorderWidth,Ht);
      Xg := -1;
      Yg := -1;
    end;
  end;

  if not autosize then begin
    If Xg + wg > btn.width - BorderWidth then xg := -1;
    If Yg + Hg > btn.height - BorderWidth then Yg := -1;
    If Xt + wt > btn.width - BorderWidth then xt := -1;
    If Yt + Ht > btn.height - BorderWidth then yt := -1;
  end;

  {--Draw------------}
  R := btn.ClientRect;

  If not transparent then begin
    C.Brush.Style := BsSolid;
    C.Brush.Color := Color;
    C.FillRect(R);
  end;


  If BorderWidth > 0 then Begin
    C.Brush.Style := BsSolid;
    C.Brush.Color := borderColor;
    C.FrameRect(R);
    if focused and (FocusStyle = ALFSBorder) then begin
      inflaterect(r,-1,-1);
      C.FrameRect(R);
    end;
  end;


  If (Xg <> -1) and (Yg <> -1) then begin
    If (glyph <> nil) then Glyph.Draw(c,Xg,Yg,GlyphIndex)
    else if buttonCategory = Radio then begin
      C.Brush.Style := BsSolid;
      C.Brush.Color := BlankColor;
      C.Pen.Color := BorderColor;
      C.Ellipse(Xg, Yg, Xg+wg, Yg+Hg);

      If state in [CbChecked,cbGrayed] then begin
        C.Brush.Color := MarkColor;
        C.Pen.Color := MarkColor;
        C.Ellipse(Xg+3, Yg+3, Xg+wg-3, Yg+Hg-3);
      end;
    end
    else if buttonCategory = chkbox then begin
      C.brush.Style := bsSolid;
      C.Brush.Color := BlankColor;
      C.Pen.Color := BorderColor;
      C.Rectangle(Xg, Yg, Xg+Wg, Yg+Hg);

      If State=CBGrayed then begin
         C.Pen.Mode := pmNotXor	;
         For j := Yg + 1 to Yg + Hg - 2 do begin
           If J mod 2 = 0 then I := Xg + 2
           else I := Xg + 1;
           while i < Xg+Wg - 1 do begin
             C.Pixels[i,J] := BlankColor;
             Inc(i,2);
           end;
         end;
         C.Pen.Mode := pmCopy;
      end;

      If state in [CbChecked,cbGrayed] then begin
        C.Brush.Color := MarkColor;
        C.Pen.Color := MarkColor;
        C.MoveTo(Xg + 2, Yg + Hg - 5);
        C.LineTo(Xg + 4, Yg + Hg - 3);
        C.LineTo(Xg + 9, Yg + Hg - 8);

        C.MoveTo(Xg + 2, Yg + Hg - 6);
        C.LineTo(Xg + 4, Yg + Hg - 4);
        C.LineTo(Xg + 9, Yg + Hg - 9);

        C.MoveTo(Xg + 2, Yg + Hg - 7);
        C.LineTo(Xg + 4, Yg + Hg - 5);
        C.LineTo(Xg + 9, Yg + Hg - 10);
      end;
    end;
  end;

  If (Xt <> -1) and (Yt <> -1) then begin
    C.Brush.Style := bsclear;
    R.Left := Xt - 2;
    R.Top := Yt - 2;
    R.Bottom := Yt+ht + 2;
    R.Right :=  Xt+wt + 2;

    if (AccelCharPos > 0) and not (FsUnderline in C.Font.Style) then begin
      C.TextOut(xt,yt,TextBeforeAccelChar);
      wt := c.Textwidth(TextBeforeAccelChar);
      C.Font.Style := C.Font.Style + [FSUnderline];
      C.TextOut(xt + wt ,yt,TextAccelChar);
      wt := wt + c.Textwidth(TextAccelChar);
      C.Font.Style := C.Font.Style - [FSUnderline];
      C.TextOut(xt + wt ,yt,TextAfterAccelChar);
    end
    else c.Textout(Xt,Yt,text);

    If focused and (ButtonCategory in [radio, chkbox]) then begin
      C.Brush.Style := bssolid;
      C.DrawFocusRect(r);
    end;
  End;
end;




////////////////////////////////////////////////////////////////////////////////
///////////// TALCustomButton   ////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{*****************************************************}
constructor TALCustomButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetcaption, csOpaque];
  FGlyph := nil;
  FSpacing:=4;
  FLayout:= blGlyphLeft;
  FFocusStyle:= ALFSBorder;
  FgroupIndex := 0;
  FState := cbunchecked;
  FmouseinControl := false;
  FKeyIsDown:= False;
  FMouseIsDown:=False;
  FBorderColor := ClBlack;
  FMarkColor := ClBlack;
  FBlankColor := Clwhite;
  FGlyphIndex := -1;
  FModalResult:= 0;
  FCancel := False;
  FDefault := False;
  Fautosize := False;
  Caption := '';
  Width := 75;
  Height := 25;
  TabStop := True;
  FAllowGrayed := False;
  FcustomProperty := '';
end;

{*********************************}
destructor TALCustomButton.Destroy;
begin
  FGlyph := nil;
  inherited Destroy;
end;

{****************************************************************}
procedure TALCustomButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'BUTTON');
  Params.Style := params.Style or BS_OWNERDRAW;
end;

{************************************************************************************}
procedure TALCustomButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then begin
    if Acomponent = Fglyph then begin
      FGlyph := nil;
      PaintButton;
    end;
  end;
end;

{************************************}
procedure TALCustomButton.PaintButton;
var c : TcontrolCanvas;
    Continue : Boolean;
begin
  continue := true;
  If assigned(FonPaint) then FOnPaint(Self, continue);

  if not continue then Begin
    ReplaceInvalidateInQueueByRefresh;
    exit;
  end;

  C := TControlCanvas.Create;
  try
    C.Control := self;
    DrawButtonFace(C,
                   self,
                   Glyph,
                   GlyphIndex, Spacing,
                   Layout,
                   caption,
                   FocusStyle,
                   Focused, Autosize, False,
                   Font,
                   color, BorderColor, MarkColor, BlankColor,
                   State,
                   GetButtonCategory);

  finally
    c.Free;
  end;
end;

{*******************************}
procedure TALCustomButton.Toggle;
begin
  If GetButtonCategory=chkbox then begin
    case State of
      cbUnchecked:
        if AllowGrayed then State := cbGrayed else State := cbChecked;
      cbChecked: State := cbUnchecked;
      cbGrayed: State := cbChecked;
    end;
  end;
end;

{******************************}
procedure TALCustomButton.Click;
var Form: TCustomForm;
begin
  If not focused then setfocus;

  If GetButtonCategory=Radio then State := CbChecked
  else If GetButtonCategory=ChkBox then Toggle
  else begin
    Form := GetParentForm(Self);
    if Form <> nil then Form.ModalResult := ModalResult;

    If groupIndex = 0 then FState := cbunchecked
    else begin
      If (FState = cbunchecked) or (FState = cbGrayed) then FState := cbchecked
      Else FState := cbunchecked
    end;
  end;

  inherited;
end;

{*******************************************************}
procedure TALCustomButton.WndProc(var Message: TMessage);
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
                        PaintButton;
                      end;
                end;
    {---------------}
    WM_KEYUP:   begin
                  inherited;
                  If FKeyIsDown then begin
                    click;
                    FKeyIsDown := False;
                    paintbutton;
                  end;
                end;
    {-------------------}
    WM_LButtonDown: begin
                      inherited;
                      If not (csDesigning in ComponentState) and
                         not FKeyIsDown then begin
                        FMouseIsDown := True;
                        paintButton;
                      end;
                    end;
    {-----------------}
    WM_LButtonUp: begin
                    inherited;
                    If FMouseIsDown then begin
                      FMouseIsDown := False;
                      paintButton;
                    end;
                  end;
    {---------------------}
    WM_LButtonDblClk: begin
                          inherited;
                          If not FKeyIsDown then begin
                          FMouseIsDown := True;
                          paintButton;
                        end;
                      end;
    {-----------------}
    CN_COMMAND:   begin
                   inherited;
                   // NB: le message click est recu AVANT le msg wm_lbbuttonUp !
                   if (TWMCommand(Message).NotifyCode = BN_CLICKED) and not FKeyIsDown then Click;
                  end;
    {-------------------}
    CM_DIALOGCHAR:  begin
                       inherited;
                       if IsAccel(TCMDialogChar(Message).CharCode, Caption) and
                          CanFocus and
                          not FKeyIsDown and
                          not FMouseIsDown then begin
                            FKeyIsDown := True;
                            PaintButton;
                            Click;
                            FKeyIsDown := False;
                            PaintButton;
                            TCMDialogChar(Message).Result := 1;
                          end;
                    end;
    {-----------------}
    CM_DialogKey: begin
                     inherited;
                     with TWMKey(Message) do
                       if  (((CharCode = VK_RETURN) and FDefault) or ((CharCode = VK_ESCAPE) and FCancel)) and
                           (KeyDataToShiftState(KeyData) = []) and
                           CanFocus and
                           not FKeyIsDown and
                           not FMouseIsDown then begin
                            FKeyIsDown := True;
                            PaintButton;
                            Click;
                            FKeyIsDown := False;
                            PaintButton;
                           end;
                   end;
    {------------------}
    CM_MouseEnter: Begin
                     inherited;
                     FmouseinControl := true;
                     PaintButton;
                   End;
    {------------------}
    CM_MouseLeave: Begin
                     inherited;
                     FmouseinControl := false;
                     Paintbutton;
                   End;
    {-------------}
    CM_Enter: begin
                inherited;
                PaintButton;
              end;
    {------------}
    CM_Exit: begin
               inherited;
               If FKeyIsDown then FKeyIsDown := False;
               If FMouseIsDown then FMouseIsDown := False;
               PaintButton;
             end;
    {-------------}
    WM_Paint: Begin
                inherited;
                PaintButton;
              end;
    {----------------------}
    CM_EnabledChanged: Begin
                         inherited;
                         PaintButton;
                       end;
    {-------------------}
    CM_TextChanged: Begin
                      inherited;
                      PaintButton;
                    end;
    {--------------}
     else inherited;
  end;
end;

{********************************************************}
procedure TALCustomButton.SetState(Value: TCheckBoxState);

  procedure TurnSiblingsOff;
  var
    I: Integer;
    Sibling: TControl;
  begin
    if Parent <> nil then
      with Parent do
        for I := 0 to ControlCount - 1 do begin
          Sibling := Controls[I];
          if (Sibling <> Self) and (Sibling is TALCustomRadioButton) then
            TAlCustomRadioButton(Sibling).State := CbUnchecked;
        end;
  end;

begin
  if FState <> Value then begin
    FState := Value;
    If GetButtonCategory=radio then begin
      TabStop := FState = CbChecked;
      if FState = CbChecked then begin
        TurnSiblingsOff;
        inherited Changed;
      end;
    end;
    PaintButton;
  end;
end;

{******************************************************}
procedure TALCustomButton.SetGlyph (Value : TImageList);
begin
  If Value <> FGlyph then begin
    FGlyph := Value;
    If value <> nil then value.FreeNotification(self);
    paintButton;
  end;
end;

{*****************************************************}
Procedure TALCustomButton.SetAutoSize(Value: Boolean);
begin
  if Value <> FAutosize then begin
    Fautosize  :=  Value;
    refresh;
  end;
End;

{*************************************************************}
Function TALCustomButton.GetButtonCategory : TALButtonCategory;
Begin
  Result := Btn;
End;

{*******************************************}
function TALCustomButton.GetChecked: Boolean;
Begin
  Result := State=CBChecked;
End;

{***************************************************}
procedure TALCustomButton.SetChecked(Value: Boolean);
Begin
  If Value then state := cbChecked
  else state := cbunchecked;
end;

{**********************************************************}
procedure TALCustomButton.ReplaceInvalidateInQueueByRefresh;
begin
  ValidateRect(handle,nil);
  Refresh;
end;



////////////////////////////////////////////////////////////////////////////////
///////////// TALCustomGraphicButton   /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{************************************************************}
constructor TALCustomGraphicButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csSetcaption, csOpaque];
  FGlyph := nil;
  FSpacing:= 4;
  FLayout:= blGlyphLeft;
  FgroupIndex := 0;
  FState := cbunchecked;
  FmouseinControl := false;
  FKeyIsDown:= False;
  FMouseIsDown:=False;
  FBorderColor := ClBlack;
  FMarkColor := ClBlack;
  FBlankColor := Clwhite;
  FGlyphIndex := -1;
  Fautosize := False;
  FTransparent := False;
  FFullRepaint := True;
  FModalResult:= 0;
  FCancel := False;
  FDefault := False;
  Caption := '';
  SetBounds(0, 0, 75, 25);
  FAllowGrayed := False;
  FcustomProperty := '';
end;

{****************************************}
destructor TALCustomGraphicButton.Destroy;
begin
  FGlyph := nil;
  inherited Destroy;
end;

{*******************************************************************************************}
procedure TALCustomGraphicButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then begin
    if Acomponent = Fglyph then begin
      FGlyph := nil;
      Refresh;
    end;
  end;
end;

{**********************************************}
Procedure TALCustomGraphicButton.Fastinvalidate;
Begin
  If transparent and FullRepaint then Refresh
  else Paint;
End;

{*************************************}
procedure TALCustomGraphicButton.Paint;
var  Continue : Boolean;
begin
  continue := true;
  If assigned(FonPaint) then FOnPaint(Self, continue);

  if not continue then Begin
    ReplaceInvalidateInQueueByRefresh;
    exit;
  end;

  DrawButtonFace(Canvas,
                 self,
                 Glyph,
                 GlyphIndex, Spacing,
                 Layout,
                 caption,
                 ALFSNone,
                 False, Autosize, transparent,
                 Font,
                 color, BorderColor, MarkColor, BlankColor,
                 State,
                 GetButtonCategory);
end;

{**************************************}
procedure TALCustomGraphicButton.Toggle;
begin
  If GetButtonCategory=chkbox then begin
    case State of
      cbUnchecked:
        if AllowGrayed then State := cbGrayed else State := cbChecked;
      cbChecked: State := cbUnchecked;
      cbGrayed: State := cbChecked;
    end;
  end;
end;

{*************************************}
procedure TALCustomGraphicButton.Click;
var Form: TCustomForm;
begin
  If GetButtonCategory=Radio then State := CbChecked
  else If GetButtonCategory=ChkBox then Toggle
  else begin
    Form := GetParentForm(Self);
    if Form <> nil then Form.ModalResult := ModalResult;

    If groupIndex = 0 then FState := cbunchecked
    else begin
      If (FState = cbunchecked) or (FState = cbGrayed) then FState := cbchecked
      Else FState := cbunchecked
    end;
  end;

  inherited;
end;

{**************************************************************}
procedure TALCustomGraphicButton.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    {-------------------}
    WM_LButtonDown: begin
                      inherited;
                      If not (csDesigning in ComponentState) and
                         not FKeyIsDown then begin
                        FMouseIsDown := True;
                        Fastinvalidate;
                      end;
                    end;
    {-----------------}
    WM_LButtonUp: begin
                    inherited;
                    If FMouseIsDown then begin
                      FMouseIsDown := False;
                      Fastinvalidate;
                    end;
                  end;
    {---------------------}
    WM_LButtonDblClk: begin
                        inherited;
                        If not FKeyIsDown then begin
                          FMouseIsDown := True;
                          Fastinvalidate;
                        end;
                      end;
    {-----------------}
    CN_COMMAND:   begin
                   inherited;
                   // NB: le message click est recu AVANT le msg wm_lbbuttonUp !
                   if (TWMCommand(Message).NotifyCode = BN_CLICKED) and not FKeyIsDown then Click;
                  end;
    {-------------------}
    CM_DIALOGCHAR:  begin
                       inherited;
                       if IsAccel(TCMDialogChar(Message).CharCode, Caption) and
                          Enabled and Visible and
                          not FKeyIsDown and
                          not FMouseIsDown then begin
                            FKeyIsDown := True;
                            Click;
                            FKeyIsDown := False;
                            Fastinvalidate;
                            TCMDialogChar(Message).Result := 1;
                          end;
                    end;
    {-----------------}
    CM_DialogKey: begin
                     inherited;
                     with TWMKey(Message) do
                       if  (((CharCode = VK_RETURN) and FDefault) or ((CharCode = VK_ESCAPE) and FCancel)) and
                           (KeyDataToShiftState(KeyData) = []) and
                           Enabled and Visible and
                           not FKeyIsDown and
                           not FMouseIsDown then begin
                            FKeyIsDown := True;
                            Click;
                            FKeyIsDown := False;
                            Fastinvalidate;
                           end;
                   end;
    {------------------}
    CM_MouseEnter: Begin
                     inherited;
                     FmouseinControl := true;
                     Fastinvalidate;
                   End;
    {------------------}
    CM_MouseLeave: Begin
                     inherited;
                     FmouseinControl := false;
                     Fastinvalidate;
                   End;
    {-------------}
    CM_Enter: begin
                inherited;
                Fastinvalidate;
              end;
    {------------}
    CM_Exit: begin
               inherited;
               If FKeyIsDown then FKeyIsDown := False;
               If FMouseIsDown then FMouseIsDown := False;
               Fastinvalidate;
             end;
    {----------------------}
    CM_EnabledChanged: Begin
                         inherited;
                         Fastinvalidate;
                       end;
    {-------------------}
    CM_TextChanged: Begin
                      inherited;
                      Fastinvalidate;
                    end;
     {-------------}
     else inherited;
  end;
end;

{***************************************************************}
procedure TALCustomGraphicButton.SetState(Value: TCheckBoxState);

  procedure TurnSiblingsOff;
  var
    I: Integer;
    Sibling: TControl;
  begin
    if Parent <> nil then
      with Parent do
        for I := 0 to ControlCount - 1 do begin
          Sibling := Controls[I];
          if (Sibling <> Self) and (Sibling is TALCustomGraphicRadioButton) then
            TAlCustomGraphicRadioButton(Sibling).State := CbUnchecked;
        end;
  end;

begin
  if FState <> Value then begin
    FState := Value;
    If GetButtonCategory=Radio then begin
      if FState = CbChecked then begin
        TurnSiblingsOff;
        inherited Changed;
      end;
    end;
    Fastinvalidate;
  end;
end;

{*************************************************************}
procedure TALCustomGraphicButton.SetGlyph (Value : TImageList);
begin
  If Value <> FGlyph then begin
    FGlyph := Value;
    If value <> nil then value.FreeNotification(self);
    Refresh;
  end;
end;

{***********************************************************}
Procedure TALCustomGraphicButton.SetAutoSize(Value: Boolean);
begin
  if Value <> FAutosize then begin
    Fautosize  :=  Value;
    Refresh;
  end;
End;

{**************************************************************}
procedure TALCustomGraphicButton.SetTransparent(Value: Boolean);
Begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    if Value then ControlStyle := ControlStyle - [csOpaque]
    else ControlStyle := ControlStyle + [csOpaque];
    Refresh;
  end;
End;

{********************************************************************}
Function TALCustomGraphicButton.GetButtonCategory : TALButtonCategory;
Begin
  Result := Btn;
End;

{**************************************************}
function TALCustomGraphicButton.GetChecked: Boolean;
Begin
  Result := State=CBChecked;
End;

{**********************************************************}
procedure TALCustomGraphicButton.SetChecked(Value: Boolean);
Begin
  If Value then state := cbChecked
  else state := cbunchecked;
end;

{*****************************************************************}
procedure TALCustomGraphicButton.ReplaceInvalidateInQueueByRefresh;
var Rect: TRect;
begin
  if (Visible or (csDesigning in ComponentState) and
     not (csNoDesignVisible in ControlStyle)) and
     (Parent <> nil) and
     Parent.HandleAllocated then begin

    Rect := BoundsRect;
    ValidateRect(Parent.Handle, @Rect);
    If transparent and FullRepaint then Refresh
    else paint;

  end;
end;




////////////////////////////////////////////////////////////////////////////////
///////////// TALRadioButton           /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{******************************************************************}
Function TALCustomRadioButton.GetButtonCategory : TALButtonCategory;
Begin
  Result := radio;
End;


////////////////////////////////////////////////////////////////////////////////
///////////// TALGraficRadioButton           ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{*************************************************************************}
Function TALCustomGraphicRadioButton.GetButtonCategory : TALButtonCategory;
Begin
  Result := radio;
End;


////////////////////////////////////////////////////////////////////////////////
///////////// TALCheckBox              /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{***************************************************************}
Function TALCustomCheckBox.GetButtonCategory : TALButtonCategory;
Begin
  Result := chkBox;
End;

end.
