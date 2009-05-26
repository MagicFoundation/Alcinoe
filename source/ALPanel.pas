{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALPanel
Version:      3.50

Description:  Tpanel with special feature :
              - Flat design (no clt3D feature)
              - Border color
              - Background image
              - Background image tile mode
              - Mouse in control (property to know if the mouse
                is in the control area)
              - Custom property (string to add custom informations
                like "tag" property)
              - Onpaint event (powerfull event fired each time the
                control needed to be
                repaint and permit to change, before the repaint process,
                some property of the control like it's color. For exemple
                we can easily change the font or border color of the
                control each time the mouse enter or leave the control.

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

History :     30/11/2004: Remove doublebuffered=True because that force
                          the control and ALL his child to be completely
                          redraw all the time one of his child is redrawed...
                          it's too much data intensive

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALPanel;

interface

uses Windows,
     Messages,
     Classes,
     Controls,
     ExtCtrls,
     graphics,
     forms,
     ALCommon;

type
  {------------------------------------------------}
  TALTileMode = (ALTMTile, ALTMstretch, ALTMCenter);

  {----------------------------------}
  TALCustomPanel = class(TCustomPanel)
  private
    FBackGroundImage : TImageList;
    FBackGroundImageIndex : Integer;
    FBackGroundImageTileMode : TALTileMode;
    FBorderColor: Tcolor;
    FMouseInControl: boolean;
    FOnPaint: TAlPaintEvent;
    FcustomProperty: String;
    FBackGroundTileMode: TALTileMode;
    procedure SetBackGroundImage(Value : TImageList);
    procedure SetBorderColor(const Value: Tcolor);
  protected
    procedure WM_NCPaint(var Msg: TWMNCPaint); message WM_NCPaint;
    procedure Paint; override;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Property BackGroundImageIndex: integer read FBackGroundImageIndex Write FBackGroundImageIndex Default -1;
    property BackGroundImage: TImageList read FBackGroundImage write SetBackGroundImage;
    property BackGroundTileMode: TALTileMode read FBackGroundTileMode write FBackGroundTileMode default ALTMTile;
    property OnPaint: TAlPaintEvent read FOnPaint write FOnPaint;
    Property BorderColor: Tcolor read FBorderColor Write SetBorderColor Default Clblack;
    Property CustomProperty: String read FcustomProperty write FcustomProperty;
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    property MouseInControl: Boolean read FMouseInControl;
  published
  end;

  {------------------------------}
  TALPanel = class(TALCustomPanel)
  public
    property DockManager;
  published
    Property DoubleBuffered Default False;
    Property CustomProperty;
    Property BorderColor;
    Property OnPaint;
    property BackGroundTileMode;
    Property BackGroundImage;
    Property BackGroundImageIndex;
    //property BevelInner;
    //property BevelOuter;
    //property BevelWidth;
    //property BorderWidth;
    //property BorderStyle;
    //property Ctl3D;
    //property FullRepaint;
    //property ParentBackground;
    //property ParentCtl3D;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Locked;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


procedure Register;

implementation

uses AlFcnMisc;

{$R ..\resource\ALPanel.dcr}

{*****************}
procedure Register;
begin
  RegisterComponents('alcinoe', [TALPanel]);
end;

{**********************************************}
Procedure DrawPanelFace(APanel: TALCustomPanel);

  {---------------------}
   procedure paintborder;
   var
     DC: HDC;
     OldBrush: HBRUSH;
     OldPen: HPEN;
   begin
     With aPanel do begin
       DC := 0;
       OldBrush := 0;
       OldPen := 0;
       try
         DC := GetWindowDC(Handle);
         OldBrush := SelectObject(DC, GetStockObject(NULL_BRUSH));
         Canvas.Pen.Color := BorderColor;
         OldPen := SelectObject(DC, Canvas.Pen.Handle);
         Rectangle(DC, 0, 0, Width, Height);
       finally
         if DC <> 0 then begin
           if OldPen <> 0 then SelectObject(DC, OldPen);
           if OldBrush <> 0 then SelectObject(DC, OldBrush);
           ReleaseDC(Handle, DC);
         end;
       end;
     end;
   end;

const Alignments: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);

var R: TRect;
    Xg,Yg,Wg,Hg: Integer;
    aBitmap: Tbitmap;
    FontHeight: Integer;
    Flags: Longint;

begin
  With aPanel do begin

    {------- glyph with, height  -------------------------------------------------------------------}
    if (BackGroundImage <> nil) and (BackGroundImageindex in [0..BackGroundImage.Count-1]) then begin
      Wg := BackGroundImage.width;
      Hg := BackGroundImage.height;
      If (FBackGroundImageTilemode = ALTMCenter) then begin
        Xg := ALMediumPos(width,0,Wg);
        Yg := ALMediumPos(Height,0,Hg);
      end
      else begin
        Xg := 0;
        Yg := 0;
      end;
    end
    else begin
      Wg := -1;
      Hg := -1;
      xg := -1;
      yg := -1;
    end;

    {--fill panel--}
    R := ClientRect;
    Canvas.Brush.Style := BsSolid;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(R);

    {--fill bitmap------------------------}
    If (wg <> -1) and (hg <> -1) then begin
      if FBackGroundImageTileMode = ALTMTile then begin
        while Yg <= height do begin
          xg := 0;
          while Xg <= width do begin
            BackgroundImage.Draw(canvas,Xg,Yg,BackgroundImageIndex);
            Xg := Xg + Wg
          end;
          Yg := Yg + Hg;
        end;
      End
      else if FBackGroundImageTileMode = ALTMCenter then BackgroundImage.Draw(canvas,Xg,Yg,BackgroundImageIndex)
      Else begin
        aBitmap := Tbitmap.create;
        try
          BackgroundImage.GetBitmap(BackgroundImageIndex, aBitmap);
          canvas.StretchDraw(clientrect,aBitmap);
        finally
          aBitmap.Free;
        end;
      end;
    end;

    {--caption---------}
    with Canvas do begin
      Brush.Style := bsClear;
      Font := APanel.Font;
      FontHeight := TextHeight('W');
      with R do begin
        Top := ((Bottom + Top) - FontHeight) div 2;
        Bottom := Top + FontHeight;
      end;
      Flags := DT_EXPANDTABS or DT_VCENTER or Alignments[Alignment];
      Flags := DrawTextBiDiModeFlags(Flags);
      DrawText(Handle, PChar(Caption), -1, R, Flags);
    end;

    {--border--------------------------------}
    If BorderColor <> ClNone then paintborder;
  end;
end;




////////////////////////////////////////////////////////////////////////////////
///////////// TALCustomPanel   /////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{****************************************************}
constructor TALCustomPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBackgroundImage := nil;
  FBackgroundImageIndex := -1;
  FBackGroundImageTileMode := ALTMTile;
  FmouseinControl := false;
  FBorderColor := ClBlack;
  FcustomProperty := '';

  BevelInner := bvnone;
  BevelOuter := BVNone;
  BevelWidth := 1;
  BorderWidth := 1;
  BorderStyle := bsSingle;
  Ctl3D := False;
  ParentCtl3D := False;
  FullRepaint := true;
  ParentBackGround := False;
end;

{********************************}
destructor TALCustomPanel.Destroy;
begin
  FBackgroundImage := nil;
  inherited;
end;

{***********************************************************************************}
procedure TALCustomPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then begin
    if Acomponent = FBackGroundImage then begin
      FBackGroundImage := nil;
      Refresh;
    end;
  end;
end;

{*****************************}
procedure TALCustomPanel.Paint;
var Continue : Boolean;
begin
  continue := true;
  If assigned(FonPaint) then FOnPaint(Self, continue);
  if not continue then exit;

  DrawPanelFace(self);
end;

{******************************************************}
procedure TALCustomPanel.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    {------------------}
    CM_MouseEnter: Begin
                     inherited;
                     FmouseinControl := true;
                     If assigned(FonPaint) then Paint;
                   End;
    {------------------}
    CM_MouseLeave: Begin
                     inherited;
                     FmouseinControl := false;
                     If assigned(FonPaint) then Paint;
                   End;
    {-------------}
    CM_Enter: begin
                inherited;
                If assigned(FonPaint) then Paint;
              end;
    {------------}
    CM_Exit: begin
               inherited;
               If assigned(FonPaint) then Paint;
             end;
    {--------------}
     else inherited;
  end;
end;

{*************************************************************}
procedure TALCustomPanel.SetBackGroundImage(Value: TImageList);
begin
  If Value <> FBackGroundImage then begin
    FBackGroundImage := Value;
    If value <> nil then value.FreeNotification(self);
    Refresh;
  end;
end;

{*******************************************************}
procedure TALCustomPanel.WM_NCPaint(var Msg: TWMNCPaint);
begin
  Msg.Result := 0;
end;

{***********************************************************}
procedure TALCustomPanel.SetBorderColor(const Value: Tcolor);
begin
  If FborderColor <> Value then begin
    FBorderColor := Value;
    If FBorderColor=ClNone then BorderStyle := BsNone
    else BorderStyle := BsSingle;
    Paint;
  end;
end;

end.
