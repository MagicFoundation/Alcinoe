{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALComboBox
Version:      3.50

Description:  ComboBox with onPaint property
              - Flat design (no clt3D feature)
              - Mouse in control (property to know if the mouse is
                in the control area)
              - Custom property (string to add custom informations
                like "tag" property)
              - Border color property
              - Button color property
              - Arrow color property
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

History :

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALComboBox;

interface

uses Windows,
     Messages,
     Classes,
     Controls,
     StdCtrls,
     Graphics,
     ALCommon;

type

  {----------------------------------------}
  TALCustomComboBox = class(TCustomComboBox)
  private
    FButtonColor : Tcolor;
    FBorderColor : Tcolor;
    FArrowColor : Tcolor;
    FForceDrawFocused : Boolean;
    FMouseInControl: boolean;
    FOnPaint: TAlPaintEvent;
    FCustomProperty: String;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    Procedure PaintCombo;
    Procedure ReplaceInvalidateInQueueByRefresh;    
  protected
    procedure WndProc(var Message: TMessage); override;
    Property BorderColor: Tcolor read FBorderColor Write FBorderColor Default Clblack;
    Property ArrowColor: Tcolor read FArrowColor Write FArrowColor Default Clblack;
    Property ButtonColor: Tcolor read FButtonColor Write FButtonColor Default clBtnFace;
    property OnPaint: TAlPaintEvent read FOnPaint write FOnPaint;
    Property CustomProperty: String read FcustomProperty write FcustomProperty;
  public
    property ForceDrawFocused: Boolean read FForceDrawFocused;
    Property MouseInControl : boolean read FMouseInControl;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  {------------------------------------}
  TALComboBox = class(TALCustomComboBox)
  published
    Property BorderColor;
    Property ArrowColor;
    Property ButtonColor;
    property OnPaint;
    Property CustomProperty;
    //property AutoCloseUp default False;
    //property BevelEdges;
    //property BevelInner;
    //property BevelKind default bkNone;
    //property BevelOuter;
    //property BiDiMode;
    //property Constraints;
    //property Ctl3D;
    //property ImeMode;
    //property ImeName;
    //property ParentBiDiMode;
    //property ParentCtl3D;
    property AutoComplete default True;
    property AutoDropDown default False;
    property Style;
    property Anchors;
    property CharCase;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Items;
    property Font;
    property Color;
    property ParentFont;
    property ParentColor;
  end;

  {---------------------------------}
  TALKeyComboBox = class(TALComboBox)
  private
    FitemsKey : Tstrings;
  protected
    procedure SetItemsKey(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Procedure AddItemKey(Value,key: String);
    procedure Clear; override;
    procedure CopySelection(Destination: TCustomListControl); override;
    procedure DeleteSelected; override;
    function IndexOfKey(key: String):integer;
    property ItemsKey: TStrings read FitemsKey write SetItemsKey;
  published
  end;

procedure Register;

implementation

{$R ..\resource\ALComboBox.dcr}

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALComboBox]);
  RegisterComponents('Alcinoe', [TALKeyComboBox]);
end;

{***********************************************}
Procedure DrawComboFace(Combo: TALCustomComboBox;
                        MouseInControl, DroppedDown, ForceDrawFocused : boolean);
var
  C: TControlCanvas;
  R: TRect;
  X : integer;

Begin
  C := TControlCanvas.Create;
  try
    C.Control := Combo;
    with combo do begin

      R := ClientRect;
      C.Brush.Style := BsSolid;
      If BorderColor <> clNone then Begin
        C.Brush.Color := borderColor;
        C.FrameRect(R);
      end
      else begin
        C.Brush.Color := Color;
        C.FrameRect(R);
      end;

      //On ne prned pas en charge le style = cssimple ! Delphi ne le prend pas en charge lui meme !
      //If combo is TALCustomComboBox and ((combo as TALCustomComboBox).Style = csSimple) then exit;

      {------ont paint ici la surface total du button------}
      R.Left := R.Right - GetSystemMetrics(SM_CXHTHUMB) - 3;
      dec(r.Right);
      InflateRect(R, 0, -1);
      C.Brush.Color := buttonColor;
      C.FillRect(R);

      {ont reduit ici la surface total du button}
      R.Right := R.Left + 5;
      C.Brush.Color := Color;
      C.FillRect(R);

      {---ont trace ici le border--------}
      If BorderColor <> clNone then begin
        dec(r.Right);
        C.Pen.Color := BorderColor;
        C.Moveto(R.Right, R.Top);
        C.LineTo(R.Right, R.Bottom);
      end;

      {--ont dessine maintenant la fleche--}
      C.Pen.Color := ArrowColor;
      R := ClientRect;
      X := R.Right - 10;
      C.Moveto(X + 0, R.Top + 10);
      C.LineTo(X + 5, R.Top + 10);
      C.Moveto(X + 1, R.Top + 11);
      C.LineTo(X + 4, R.Top + 11);
      C.Moveto(X + 2, R.Top + 12);
      C.LineTo(X + 3, R.Top + 12);
    end;

  finally
    C.Free;
  end;
end;




////////////////////////////////////////////////////////////////////////////////
///////////// TalCustomComboBox ////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{*******************************************************}
constructor TALCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBorderColor:= Clblack;
  FarrowColor:= Clblack;
  FButtonColor:= clBtnFace;
  FMouseInControl := False;
  FForceDrawFocused := False;
  FcustomProperty := '';

  BevelEdges := [];
  BevelInner := bvnone;
  BevelKind := bkFlat;
  BevelOuter := BVNone;
  BiDiMode := bdLeftToRight;
  Ctl3D := False;
  ImeMode := imDontCare;
  ImeName := '';
  ParentBiDiMode := False;
  ParentCtl3D := False;

  ControlStyle := ControlStyle - [csSetCaption];
end;

{***********************************}
destructor TALCustomComboBox.Destroy;
begin
  inherited Destroy;
end;

{************************************************************}
procedure TALCustomComboBox.ReplaceInvalidateInQueueByRefresh;
begin
  ValidateRect(handle,nil);
  Refresh;
end;

{*************************************}
Procedure TALCustomComboBox.PaintCombo;
var continue : boolean;
begin
  continue := true;
  If assigned(FonPaint) then FOnPaint(Self, continue);

  if not continue then Begin
    ReplaceInvalidateInQueueByRefresh;
    exit;
  end;

  DrawComboFace(self, FMouseInControl, DroppedDown, FForceDrawFocused);
end;

{*********************************************************}
procedure TALCustomComboBox.WndProc(var Message: TMessage);
begin
  case Message.Msg of
  {------------------}
   WM_LButtonUp: begin
                    inherited;
                    paintcombo;
                  end;
    {------------------}
    CM_MouseEnter: Begin
                     inherited;
                     FmouseinControl := true;
                     PaintCombo;
                   End;
    {------------------}
    CM_MouseLeave: Begin
                     inherited;
                     FmouseinControl := false;
                     PaintCombo;
                   End;
    {------------}
    CM_Exit: begin
               inherited;
               FForceDrawFocused := False;
               PaintCombo;
             end;
    {-------------}
    WM_Paint: Begin
                inherited;
                PaintCombo;
              end;
    {----------------}
    WM_setfocus: Begin
                inherited;
                FForceDrawFocused := True;
                PaintCombo;
              end;
    {--------------}
     else inherited;
  end;
end;

{*************************************************************}
procedure TALCustomComboBox.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    {----------------}
    CBN_CLOSEUP: begin
                   inherited;
                   PaintCombo;
                 end;
    {--------------}
     else inherited;
  end;
end;




////////////////////////////////////////////////////////////////////////////////
///////////// TalKeyCustomComboBox /////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{****************************************************}
constructor TalKeyComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemsKey := TStringList.Create;
end;

{********************************}
destructor TALKeyComboBox.Destroy;
begin
  FItemsKey.Free;
  inherited Destroy;
end;

{******************************************************}
function TalKeyComboBox.IndexOfKey(key: String):integer;
begin
   Result := FitemsKey.indexOf(key);
end;

{*****************************************************}
Procedure TALKeyComboBox.AddItemKey(Value,key: String);
begin
  ItemsKey.Insert(Items.add(Value),Key);
end;

{*****************************}
Procedure TALKeyComboBox.Clear;
begin
  Inherited;
  ItemsKey.clear;
end;

{**********************************************************}
procedure TALKeyComboBox.SetItemsKey(const Value: TStrings);
begin
  FItemsKey.Assign(Value)
end;

{**************************************}
procedure TALKeyComboBox.DeleteSelected;
begin
  if ItemIndex <> -1 then begin
    ItemsKey.Delete(ItemIndex);
    Items.Delete(ItemIndex);
  end;
end;

{**********************************************************************}
procedure TALKeyComboBox.CopySelection(Destination: TCustomListControl);
begin
  if (ItemIndex <> -1) then begin
    If (Destination is TALKeyComboBox) then (Destination as TALKeyComboBox).AddItemKey(Items[ItemIndex],ItemsKey[ItemIndex])
    else Destination.AddItem(Items[ItemIndex], Items.Objects[ItemIndex]);
  end;
end;

end.
