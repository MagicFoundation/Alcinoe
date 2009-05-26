{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALHintballon
Version:      3.50

Description:  Custom Hint with good look (Balloon). Good for use
              in help tips

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

History:      01/03/2004: fix CloseHintBalloons by Hilton Janfield
                          (hjanfield@shaw.ca)

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALHintBalloon;

interface

uses Windows,
     Forms,
     Classes,
     Controls,
     StdCtrls,
     ExtCtrls,
     Graphics,
     Messages,
     SysUtils;

type

  {-------------------}
  TALHintBalloon=Class;
  TALHintBalloonMsgType = (bmtInfo, bmtError, bmtWarning, bmtNone);
  TALHintBalloonArrowPosition = (bapTopLeft, bapTopRight, bapBottomLeft, bapBottomRight);
  TALHintBalloonAnimationType = (batNone, batBlend, batCenter, batSlideLeftToRight, batSlideRightToLeft, batSlideTopToBottom, batSlideBottomToTop, batWipeLeftToRight, batWipeRightToLeft, batWipeTopToBottom, batWipeBottomToTop);
  TALHintBalloonCreateContentProcess = procedure (ALHintBalloonObject: TALHintBalloon);

  {---------------------------------}
  TALHintBalloon = class(TCustomForm)
  private
    FMainPanel: Tpanel;
    FcustomProperty: String;
    FLastActiveForm: Tform;
    fOldLastActiveFormWndProc: TWndMethod;
    FBorderWidth: Integer;
    procedure FormPaint(Sender: TObject);
    function IsWinXP: boolean;
    procedure LastActiveFormWndProcWndProc(var Message: TMessage);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure OnFormDeactivate(Sender: TObject);
    procedure OnExitTimer(Sender: TObject);
  public
    constructor CreateNew(AOwner: TComponent; Dummy: integer = 0); override;
    destructor Destroy; override;
    procedure OnFormClick(Sender: TObject); Virtual;
    procedure ShowHintBalloon(blnCreateContentProcess: TALHintBalloonCreateContentProcess; blnLeft, blnTop: integer; blnSourceControl: TControl; blnDuration: integer; blnArrowPosition: TALHintBalloonArrowPosition; blnAnimationType: TALHintBalloonAnimationType; blnAnimationSpeed: cardinal);
    function TranslateAnimation(Value: TALHintBalloonAnimationType): cardinal;
    property MainPanel: Tpanel read FMainPanel write FMainPanel;
    property CustomProperty: String read FcustomProperty write FcustomProperty;
    Property BorderWidth: Integer read FborderWidth write FborderWidth;
  end;

  {---------------------------------------}
  TALHintBalloonControl = class(TComponent)
  private
    FALHintBalloon: TALHintBalloon;
    FDuration: integer;
    FAnimationSpeed: cardinal;
    FAnimationType: TALHintBalloonAnimationType;
  public
    procedure ShowTextHintBalloon(blnMsgType: TALHintBalloonMsgType; blnTitle, blnText: String; blnBestWidth, blnLeft, blnTop: integer; blnSourceControl: Tcontrol; blnArrowPosition: TALHintBalloonArrowPosition);
    procedure ShowPictureHintBalloon(blnImageFilename: String; blnLeft, blnTop: integer; blnSourceControl: TControl; blnArrowPosition: TALHintBalloonArrowPosition);
    procedure ShowCustomHintBalloon(blnCreateContentProcess: TALHintBalloonCreateContentProcess; BlncustomProperty: String; blnLeft, blnTop: integer; blnSourceControl: TControl; blnArrowPosition: TALHintBalloonArrowPosition);
    function CloseHintBalloons: integer;
    constructor Create(AOwner: TComponent); override;
  published
    property Duration: integer read FDuration write FDuration Default 0;
    property AnimationType: TALHintBalloonAnimationType read FAnimationType write FAnimationType default BatBlend;
    property AnimationSpeed: cardinal read FAnimationSpeed write FAnimationSpeed Default 300;
  end;

procedure Register;

implementation

{$R ..\resource\ALHintBalloon.res}
{$R ..\resource\ALHintballoon.dcr}

uses math;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALHintBalloonControl]);
end;




///////////////////////////////////////////////////////////////
//////////Function TALHintBalloonCreateContentProcess//////////
///////////////////////////////////////////////////////////////

{*******************************************************************************}
procedure ALHintBalloonConstructTextContent(ALHintBalloonObject: TALHintBalloon);
var lblTitle, LblText: Tlabel;
    IcoImage: Timage;
    aScrollBox: TscrollBox;
    str: String;
    LastLabelWidth, MaxPanelWidth, MaxPanelHeight, BestPanelWidth : integer;
    LstCustomPropertyParams: TstringList;
    DecLabelWidth: Boolean;
begin
  LstCustomPropertyParams := TstringList.create;
  try
    LstCustomPropertyParams.Delimiter := ';';
    LstCustomPropertyParams.QuoteChar := '''';
    LstCustomPropertyParams.DelimitedText := ALHintBalloonObject.CustomProperty;

    {------------------------------------}
    ALHintBalloonObject.Color := $00E1FFFF;
    ALHintBalloonObject.Font.Name := 'Tahoma';
    ALHintBalloonObject.Font.Size := 8;
    ALHintBalloonObject.Font.Color := clBlack;

    {--------------}
    lblTitle := nil;
    IcoImage := nil;

    {--------------------------------------}
    str := LstCustomPropertyParams.Values['IconeResName'];
    if (str <> '') then begin
      IcoImage := TImage.Create(ALHintBalloonObject.MainPanel);
      with IcoImage do begin
        Parent := ALHintBalloonObject.MainPanel;
        Transparent := True;
        Left := 10;
        Top := 10;
        Autosize := True;
        Picture.Bitmap.LoadFromResourceName(HInstance, str);
        Onclick := ALHintBalloonObject.OnFormClick;
      end;
    end;

    {-------------------------------}
    str := LstCustomPropertyParams.Values['Title'];
    If (str <> '') then begin
      lblTitle := TLabel.Create(ALHintBalloonObject.MainPanel);
      With lblTitle do begin
        Parent := ALHintBalloonObject.MainPanel;
        ParentColor := True;
        ParentFont := True;
        AutoSize := True;
        Font.Style := [fsBold];
        if assigned(IcoImage) then left := IcoImage.Left + IcoImage.width + 8
        else Left := 10;
        Top := 12;
        Caption := str;
        Onclick := ALHintBalloonObject.OnFormClick;
      end;
    end;

    {--------------------------------------------------------}
    LblText := TLabel.Create(ALHintBalloonObject.MainPanel);
    with LblText do begin
      Parent := ALHintBalloonObject.MainPanel;
      ParentColor := True;
      ParentFont := True;
      if assigned(lblTitle) and assigned(IcoImage) then Top := max(lblTitle.Top + lblTitle.Height + 8,IcoImage.Top + IcoImage.Height + 8)
      else if assigned(lblTitle) then Top := lblTitle.Top + lblTitle.Height + 8
      else if assigned(IcoImage) then Top := IcoImage.Top + IcoImage.Height + 8
      else top := 10;
      Left := 10;
      WordWrap := True;
      BestPanelWidth := strtoint(LstCustomPropertyParams.Values['MainPanelBestWidth']);
      Width := BestPanelWidth - left - 10;
      AutoSize := True;
      caption := LstCustomPropertyParams.Values['Text'];
      Onclick := ALHintBalloonObject.OnFormClick;

      {-------------------------------------------------------}
      ALHintBalloonObject.MainPanel.Width := Left + Width + 10;
      ALHintBalloonObject.MainPanel.Height := Top + Height + 10;

      {----------------------------------------------------------}
      MaxPanelWidth := strtoint(LstCustomPropertyParams.Values['MainPanelMaxWidth']);
      MaxPanelHeight := strtoint(LstCustomPropertyParams.Values['MainPanelMaxHeight']);
      IF ALHintBalloonObject.MainPanel.height > MaxPanelheight then begin
        Width := MaxPanelWidth - Left - 10;
        AutoSize := False;
        AutoSize := True;
        ALHintBalloonObject.MainPanel.Width := Left + Width + 10;
        ALHintBalloonObject.MainPanel.Height := Top + Height + 10;
        LastLabelWidth := width;
        while ALHintBalloonObject.MainPanel.Height < MaxPanelheight do begin
          LastLabelWidth := width;
          if width <= BestPanelWidth then break;
          width := width - 10;
          AutoSize := False;
          AutoSize := True;
          ALHintBalloonObject.MainPanel.Width := Left + Width + 10;
          ALHintBalloonObject.MainPanel.Height := Top + Height + 10;
        end;
        if ALHintBalloonObject.MainPanel.Height > MaxPanelheight then begin
          width := LastLabelWidth;
          AutoSize := False;
          AutoSize := True;
          ALHintBalloonObject.MainPanel.Width := Left + Width + 10;
          ALHintBalloonObject.MainPanel.Height := Top + Height + 10;
        end;
      end;

      while ALHintBalloonObject.MainPanel.Width > MaxPanelWidth do begin
        if ALHintBalloonObject.MainPanel.height >= MaxPanelheight then break;
        width := width-10;
        AutoSize := False;
        AutoSize := True;
        ALHintBalloonObject.MainPanel.Width := Left + Width + 10;
        ALHintBalloonObject.MainPanel.Height := Top + Height + 10;
      end;

      if assigned(lblTitle) and ((lblTitle.Left + lblTitle.Width + 10) > (ALHintBalloonObject.MainPanel.Width)) then Begin
        ALHintBalloonObject.MainPanel.Width := (lblTitle.Left + lblTitle.Width) + 10;
        Width := lblTitle.Width + lblTitle.Left - left;
        AutoSize := False;
        AutoSize := True;
        ALHintBalloonObject.MainPanel.Height := Top + Height + 10;        
      end;

      If (ALHintBalloonObject.MainPanel.Width > MaxPanelWidth) or (ALHintBalloonObject.MainPanel.Height > MaxPanelHeight) then begin
        ALHintBalloonObject.MainPanel.Width := ALHintBalloonObject.MainPanel.Width + 20;
        DecLabelWidth := False;
        If (ALHintBalloonObject.MainPanel.Width > MaxPanelWidth) then begin
          ALHintBalloonObject.MainPanel.Width := MaxPanelWidth;
          DecLabelWidth := True;
        end;
        If (ALHintBalloonObject.MainPanel.Height > MaxPanelHeight) then ALHintBalloonObject.MainPanel.Height := MaxPanelHeight;
        aScrollBox := TscrollBox.Create(ALHintBalloonObject.MainPanel);
        With aScrollBox do begin
          Parent := ALHintBalloonObject.MainPanel;
          BevelEdges := [];
          BevelInner := bvNone;
          BevelOuter := bvNone;
          BorderStyle := BsNone;
          VertScrollBar.ParentColor := True;
          VertScrollBar.Style := ssFlat;
          HorzScrollBar.ParentColor := True;
          HorzScrollBar.Style := ssFlat;
          Align := AlClient;
          IcoImage.Parent := aScrollBox;
          LblTitle.Parent := aScrollBox;
          LblText.parent := aScrollBox;
          if DecLabelWidth then begin
            LblText.Width := LblText.Width - 20;
            LblText.AutoSize := False;
            LblText.AutoSize := True;
          end;
          If LblText.Top + LblText.Height + 10 > height then begin
            LblText.AutoSize := False;
            LblText.Height := LblText.Height + 10;
          end;
        end;

      end;
    end;

  finally
    LstCustomPropertyParams.free;
  end;

end;


{**********************************************************************************}
procedure ALHintBalloonConstructPictureContent(ALHintBalloonObject: TALHintBalloon);
var MaxPanelWidth, MaxPanelHeight : integer;
    LstCustomPropertyParams: TstringList;
begin
  LstCustomPropertyParams := TstringList.create;
  try
    LstCustomPropertyParams.Delimiter := ';';
    LstCustomPropertyParams.QuoteChar := '''';
    LstCustomPropertyParams.DelimitedText := ALHintBalloonObject.CustomProperty;

    {------------------------------------}
    ALHintBalloonObject.Color := ClBlack;

    {--------------------------------------}
    with TImage.Create(ALHintBalloonObject.MainPanel) do begin
      Parent := ALHintBalloonObject.MainPanel;
      Transparent := False;
      Left := 0;
      Top := 0;
      Autosize := True;
      Center := True;
      stretch := False;
      Proportional := False;
      Picture.LoadFromFile(LstCustomPropertyParams.Values['ImageFileName']);
      Onclick := ALHintBalloonObject.OnFormClick;

      {-------------------------------------------}
      ALHintBalloonObject.MainPanel.Width := Width;
      ALHintBalloonObject.MainPanel.Height := Height;

      {-----------------------------------------------------------------------------}
      MaxPanelWidth := strtoint(LstCustomPropertyParams.Values['MainPanelMaxWidth']);
      MaxPanelHeight := strtoint(LstCustomPropertyParams.Values['MainPanelMaxHeight']);
      IF ALHintBalloonObject.MainPanel.height > MaxPanelheight then begin
        Autosize := False;
        Center := True;
        Proportional := True;
        width := round((MaxPanelheight / height) * width);
        height := MaxPanelheight;
        ALHintBalloonObject.MainPanel.Width := Width;
        ALHintBalloonObject.MainPanel.Height := Height;
      end;

      If ALHintBalloonObject.MainPanel.Width > MaxPanelWidth then begin
        Autosize := False;
        Center := True;
        Proportional := True;
        height := round((MaxPanelWidth / Width) * height);
        width := MaxPanelwidth;
        ALHintBalloonObject.MainPanel.Width := Width;
        ALHintBalloonObject.MainPanel.Height := Height;
      end;
    end;

  finally
    LstCustomPropertyParams.free;
  end;

end;




/////////////////////////////////////////
//////////TALHintBalloonControl//////////
/////////////////////////////////////////

{***********************************************************}
constructor TALHintBalloonControl.Create(AOwner: TComponent);
begin
  inherited;
  FAnimationType    := batBlend;
  FAnimationSpeed   := 300;
  FDuration         := 0;
end;

{************************************************************************************}
procedure TALHintBalloonControl.ShowTextHintBalloon(blnMsgType: TALHintBalloonMsgType;
                                                    blnTitle, blnText: String;
                                                    blnBestWidth, blnLeft, blnTop: integer;
                                                    blnSourceControl: TControl;
                                                    blnArrowPosition: TALHintBalloonArrowPosition);
begin
  FALHintBalloon := TALHintBalloon.CreateNew(Application);
  With FALHintBalloon do begin
    Case blnMsgType of
      bmtError:   CustomProperty := quotedStr('IconeResName=ERROR');
      bmtInfo:    CustomProperty := quotedStr('IconeResName=INFO');
      bmtWarning: CustomProperty := quotedStr('IconeResName=WARNING');
      else        CustomProperty := quotedStr('IconeResName=');
    end;
    CustomProperty := CustomProperty + ';' + quotedStr('Title='+blnTitle);
    CustomProperty := CustomProperty + ';' + quotedStr('Text='+blnText);
    CustomProperty := CustomProperty + ';' + quotedStr('MainPanelBestWidth='+inttostr(blnBestWidth));

    ShowHintBalloon(
                    ALHintBalloonConstructTextContent,
                    blnLeft,
                    blnTop,
                    blnSourceControl,
                    FDuration,
                    blnArrowPosition,
                    FAnimationType,
                    FAnimationSpeed
                   );
  end;
end;

{******************************************************************************}
procedure TALHintBalloonControl.ShowPictureHintBalloon(blnImageFilename: String;
                                                       blnLeft, blnTop: integer;
                                                       blnSourceControl: TControl;
                                                       blnArrowPosition: TALHintBalloonArrowPosition);
begin
  FALHintBalloon := TALHintBalloon.CreateNew(Application);
  With FALHintBalloon do begin
    CustomProperty := quotedStr('ImageFileName='+blnImageFilename);
    ShowHintBalloon(
                    ALHintBalloonConstructPictureContent,
                    blnLeft,
                    blnTop,
                    blnSourceControl,
                    FDuration,
                    blnArrowPosition,
                    FAnimationType,
                    FAnimationSpeed
                   );
  end;
end;

{****************************************************************************************************************}
procedure TALHintBalloonControl.ShowCustomHintBalloon(blnCreateContentProcess: TALHintBalloonCreateContentProcess;
                                                      BlncustomProperty: String;
                                                      blnLeft, blnTop: integer;
                                                      blnSourceControl: TControl;
                                                      blnArrowPosition: TALHintBalloonArrowPosition);
begin
  FALHintBalloon := TALHintBalloon.CreateNew(Application);
  With FALHintBalloon do begin
    CustomProperty := BlncustomProperty;
    ShowHintBalloon(
                    blnCreateContentProcess,
                    blnLeft,
                    blnTop,
                    blnSourceControl,
                    FDuration,
                    blnArrowPosition,
                    FAnimationType,
                    FAnimationSpeed
                   );
  end;
end;

{********************************************************}
function TALHintBalloonControl.CloseHintBalloons: integer;
var i: Integer;
begin
  Result := 0;
  i := 0;

  if Application.ComponentCount = 0 then Exit;
  repeat
    if (Application.Components[i].ClassName = 'TALHintBalloon') then begin
      Application.Components[i].Free;
      Inc(Result);
    end
    else inc(I);
  until (i >= Application.ComponentCount);
end;




/////////////////////////////////////////
//////////TALHintBalloonControl//////////
/////////////////////////////////////////

{***************************************************************************************}
function TALHintBalloon.TranslateAnimation(Value: TALHintBalloonAnimationType): cardinal;
begin
  case (Value) of
    batCenter: Result           := AW_CENTER;
    batBlend: Result            := AW_BLEND;
    batSlideLeftToRight: Result := AW_SLIDE or AW_HOR_POSITIVE;
    batSlideRightToLeft: Result := AW_SLIDE or AW_HOR_NEGATIVE;
    batSlideTopToBottom: Result := AW_SLIDE or AW_VER_POSITIVE;
    batSlideBottomToTop: Result := AW_SLIDE or AW_VER_NEGATIVE;
    batWipeLeftToRight:  Result := AW_HOR_POSITIVE;
    batWipeRightToLeft:  Result := AW_HOR_NEGATIVE;
    batWipeTopToBottom:  Result := AW_VER_POSITIVE;
    batWipeBottomToTop:  Result := AW_VER_NEGATIVE;
    else Result := 0;
  end;
end;

{***************************************************************}
procedure TALHintBalloon.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style   := (Params.Style and not WS_CAPTION) or WS_POPUP;
  Params.ExStyle := Params.ExStyle or WS_EX_TOOLWINDOW or WS_EX_NOACTIVATE or WS_EX_TOPMOST;
  if (IsWinXP) then Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
  Params.WndParent := GetDesktopWindow;
end;

{*****************************************************}
procedure TALHintBalloon.OnFormClick(Sender: TObject);
begin
  Release;
end;

{*********************************************************}
procedure TALHintBalloon.OnFormDeactivate(Sender: TObject);
begin
  Release;
end;

{****************************************************}
procedure TALHintBalloon.OnExitTimer(Sender: TObject);
begin
  Release;
end;

{***************************************************************************}
constructor TALHintBalloon.CreateNew(AOwner: TComponent; Dummy: integer = 0);
begin
  inherited;
  FLastActiveForm:= nil;
  fOldLastActiveFormWndProc:= nil;
  FCustomProperty := '';
  FBorderWidth := 1;
  OnDeactivate := OnFormDeactivate;
  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;
  OnPaint := FormPaint;
  OnClick := OnFormClick;

  FMainPanel := TPanel.Create(Self);
  with FMainPanel do begin
    Parent := Self;
    OnClick := OnFormClick;
    BevelOuter := BvNone;
    Caption := '';
    ParentBackGround := True;
    ParentColor := True;
    Top := 0;
    Left := 0;
  end;
end;

{********************************}
destructor TALHintBalloon.Destroy;
begin
  If FLastActiveForm <> nil then FLastActiveForm.WindowProc := FOldLastActiveFormWndProc;
  inherited;
end;

{**************************************************}
procedure TALHintBalloon.FormPaint(Sender: TObject);
var TempRegion: HRGN;
begin
  with Canvas.Brush do begin
    Color := clBlack;
    Style := bsSolid;
  end;

  TempRegion := CreateRectRgn(0,0,1,1);
  GetWindowRgn(Handle, TempRegion);
  FrameRgn(Canvas.Handle, TempRegion, Canvas.Brush.handle, 1, 1);
  DeleteObject(TempRegion);
end;

{***************************************}
function TALHintBalloon.IsWinXP: boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) and (Win32MinorVersion >= 1);
end;

{***************************************************************************************************}
procedure TALHintBalloon.ShowHintBalloon(blnCreateContentProcess: TALHintBalloonCreateContentProcess;
                                         blnLeft, blnTop: integer;
                                         blnSourceControl: TControl;
                                         blnDuration: integer;
                                         blnArrowPosition: TALHintBalloonArrowPosition;
                                         blnAnimationType: TALHintBalloonAnimationType;
                                         blnAnimationSpeed: cardinal);

Const ArrowHeight: integer = 20;
      ArrowWidth: Integer = 20;
      Arrowdx: Integer = 20;

var FormRegion, ArrowRegion: HRGN;
    Arrow: array [0..2] of TPoint;
    hModule: cardinal;
    AnimateWindowFN: function (hWnd: HWND; dwTime: cardinal; dwFlags: cardinal): longbool; stdcall;
    SourceControlRect: Trect;
    SourceControlRect_Ldiv2,SourceControlRect_Hdiv2 : Integer;
    Area1,Area2,Area3,Area4: integer;
    L1, L2, H1, H2 : Integer;
    MaxArea: Integer;
    i : Integer;
    MainPanelMaxWidth, MainPanelMaxHeight: Integer;
begin

  {---last focus form----}
  FLastActiveForm := screen.ActiveForm;
  If FLastActiveForm <> nil then begin
    FOldLastActiveFormWndProc := FLastActiveForm.WindowProc;
    FLastActiveForm.WindowProc := LastActiveFormWndProcWndProc;
  end;

  {---Calcul du MaxPanelWidth et MaxPanelHeight----}
  if assigned(blnSourceControl) then begin
    If blnSourceControl is TWincontrol then GetWindowRect((blnSourceControl as TWinControl).Handle, SourceControlRect)
    else begin
      SourceControlRect.TopLeft := blnSourceControl.parent.ClientToScreen(blnSourceControl.BoundsRect.TopLeft);
      SourceControlRect.BottomRight := blnSourceControl.Parent.ClientToScreen(blnSourceControl.BoundsRect.BottomRight);
    end;
    SourceControlRect_Hdiv2 := ((SourceControlRect.Bottom - SourceControlRect.Top) div 2);
    SourceControlRect_Ldiv2 := ((SourceControlRect.Right - SourceControlRect.Left) div 2);
    blnTop := SourceControlRect.Top + ((SourceControlRect.Bottom - SourceControlRect.Top) div 2);
    blnLeft := SourceControlRect.Left + ((SourceControlRect.Right - SourceControlRect.Left) div 2);
  end
  else begin
    SourceControlRect_Hdiv2 := 0;
    SourceControlRect_Ldiv2 := 0;
  end;

  L1 := (blnLeft - SourceControlRect_Ldiv2 + Arrowdx - 10);
  H1 := (blnTop - SourceControlRect_Hdiv2 - ArrowHeight - 10);
  L2 := (screen.Width - blnLeft  - SourceControlRect_Ldiv2 + Arrowdx - 10);
  H2 := (screen.Height - blntop - SourceControlRect_Hdiv2 - ArrowHeight - 10);
  Area1 := L1 * H1;
  Area2 := L1 * H2;
  Area3 := L2 * H1;
  Area4 := L2 * H2;
  MaxArea := Maxintvalue([Area1,Area2,Area3,Area4]);
  If MaxArea = Area1 then begin
    MainPanelMaxWidth := L1;
    MainPanelMaxheight := H1;
  end
  else If MaxArea = Area2 then begin
    MainPanelMaxWidth := L1;
    MainPanelMaxheight := H2;
  end
  else if MaxArea = Area3 then begin
    MainPanelMaxWidth := L2;
    MainPanelMaxheight := H1;
  end
  else begin
    MainPanelMaxWidth := L2;
    MainPanelMaxheight := H2;
  end;


  {-----Creation du mainPanel------------------------------------------------------}
  CustomProperty := CustomProperty + ';' + quotedStr('MainPanelMaxWidth='+inttostr(MainPanelMaxWidth));
  CustomProperty := CustomProperty + ';' + quotedStr('MainPanelMaxHeight='+inttostr(MainPanelMaxHeight));
  blnCreateContentProcess(self);


  {-----Positonnement du hint---------------}
  if not assigned(blnSourceControl) then begin
    SourceControlRect.Top := blnTop;
    SourceControlRect.Left := blnLeft;
    SourceControlRect.Bottom := blnTop;
    SourceControlRect.Right := blnLeft;
  end;

  I := 1;
  Repeat
    if blnArrowPosition = bapTopLeft then begin
      blnTop := SourceControlRect.Top ;
      blnLeft := SourceControlRect.Left;
      if (MainPanel.Width > L1) or (MainPanel.Height > H1) then blnArrowPosition := bapTopRight
      else break;
    end;
    if blnArrowPosition = bapTopRight then begin
      blnTop := SourceControlRect.Top ;
      blnLeft := SourceControlRect.right;
      if (MainPanel.Width > L2) or (MainPanel.Height > H1) then blnArrowPosition := bapBottomLeft
      else break;
    end;
    if blnArrowPosition = bapBottomLeft then begin
      blnTop := SourceControlRect.Bottom ;
      blnLeft := SourceControlRect.Left;
      if (MainPanel.Width > L1) or (MainPanel.Height > H2) then blnArrowPosition := bapBottomRight
      else break;
    end;
    if blnArrowPosition = bapBottomRight then begin
      blnTop := SourceControlRect.Bottom ;
      blnLeft := SourceControlRect.Right;
      if (MainPanel.Width > L2) or (MainPanel.Height > H2) then blnArrowPosition := bapTopLeft
      else break;
    end;
    inc(i);
  until (i > 4);

  If I > 4 then begin
    If MaxArea = Area1 then begin
      blnArrowPosition := bapTopLeft;
      blnTop := SourceControlRect.Top ;
      blnLeft := SourceControlRect.Left;
    end
    else If MaxArea = Area2 then begin
      blnArrowPosition := bapBottomLeft;
      blnTop := SourceControlRect.Bottom ;
      blnLeft := SourceControlRect.Left;
    end
    else if MaxArea = Area3 then begin
      blnArrowPosition := bapTopRight;
      blnTop := SourceControlRect.Top ;
      blnLeft := SourceControlRect.right;
    end
    else begin
      blnArrowPosition := bapBottomRight;
      blnTop := SourceControlRect.Bottom ;
      blnLeft := SourceControlRect.Right;
    end;
  end;


  {-----Creation des bordures--------------------------------------}
  FMainPanel.Left := 0;
  if blnArrowPosition in [bapBottomRight, bapBottomLeft] then begin
    FMainPanel.Top := ArrowHeight + 2;
    ClientHeight := FMainPanel.Top + FMainPanel.Height + 1;
  end
  else begin
    FMainPanel.Top := 0;
    ClientHeight := FMainPanel.Top + FMainPanel.Height + ArrowHeight - 1;
  end;
  ClientWidth := FMainPanel.Width + 1;
  if ClientWidth < Arrowdx +  ArrowWidth + 1 then clientWidth := Arrowdx +  ArrowWidth + 1;
  if Clientheight < ArrowHeight + 2 then Clientheight := ArrowHeight + 2;

  if blnArrowPosition = bapTopLeft then begin
    Left := blnLeft - (Width - Arrowdx);
    Top  := blnTop - (Height);
  end
  else if blnArrowPosition = bapTopRight then begin
    Left := blnLeft - Arrowdx;
    Top  := blnTop - (Height);
  end
  else if blnArrowPosition = bapBottomRight then begin
    Left := blnLeft - Arrowdx;
    Top  := blnTop - 2;
  end
  else if blnArrowPosition = bapBottomLeft then begin
    Left := blnLeft - (Width - Arrowdx);
    Top  := blnTop - 2;
  end;

  if blnArrowPosition = bapTopLeft then begin
    FormRegion := CreateRoundRectRgn(0, 0, Width, Height - (ArrowHeight - 2), 7, 7);
    Arrow[0] := Point(Width - ArrowWidth - Arrowdx, Height - ArrowHeight);
    Arrow[1] := Point(Width - Arrowdx, Height);
    Arrow[2] := Point(Width - Arrowdx, Height - ArrowHeight);
  end
  else if blnArrowPosition = bapTopRight then begin
    FormRegion := CreateRoundRectRgn(0, 0, Width, Height - (ArrowHeight - 2), 7, 7);
    Arrow[0] := Point(Arrowdx, Height - ArrowHeight);
    Arrow[1] := Point(Arrowdx, Height);
    Arrow[2] := Point(Arrowdx + ArrowWidth, Height - ArrowHeight);
  end
  else if blnArrowPosition = bapBottomRight then begin
    FormRegion := CreateRoundRectRgn(0, ArrowHeight + 2, Width, Height, 7, 7);
    Arrow[0] := Point(Arrowdx, 2);
    Arrow[1] := Point(Arrowdx, ArrowHeight + 2);
    Arrow[2] := Point(Arrowdx + ArrowWidth, ArrowHeight + 2);
  end
  else begin
    FormRegion := CreateRoundRectRgn(0, ArrowHeight + 2, Width, Height, 7, 7);
    Arrow[0] := Point(Width - Arrowdx, 2);
    Arrow[1] := Point(Width - Arrowdx, ArrowHeight + 2);
    Arrow[2] := Point(Width - Arrowdx - ArrowWidth, ArrowHeight + 2);
  end;

  ArrowRegion := CreatePolygonRgn(Arrow, 3, WINDING);
  CombineRgn(FormRegion, FormRegion, ArrowRegion, RGN_OR);
  SetWindowRgn(Handle, FormRegion, True);
  DeleteObject(ArrowRegion);
  DeleteObject(FormRegion);

  If BorderWidth > 0 then begin
    FormRegion := CreateRoundRectRgn(BorderWidth, BorderWidth, MainPanel.Width - BorderWidth +1, MainPanel.Height - BorderWidth + 1, 7, 7);
    SetWindowRgn(MainPanel.Handle, FormRegion, True);
    DeleteObject(FormRegion);
  end;

  {---affichage---}
  Visible := False;
  if (blnAnimationType <> batNone) and (blnAnimationSpeed > 0) then begin
    hModule := LoadLibrary('user32.dll');
    if hModule <> 0 then begin
      AnimateWindowFN := GetProcAddress(hModule, 'AnimateWindow');
      if (@AnimateWindowFN <> NIL) then AnimateWindowFN(Handle, blnAnimationSpeed, TranslateAnimation(blnAnimationType));
      FreeLibrary(hModule);
    end;
  end;

  ShowWindow(Handle, SW_SHOW);
  Visible := True;
  SetFocus;
  Invalidate;

  {---Timer-------------}
  if blnDuration > 0 then
    With TTimer.Create(Self) do begin
      OnTimer := OnExitTimer;
      Interval := blnDuration;
      Enabled  := True;
    end;
end;

{***************************************************************************}
procedure TALHintBalloon.LastActiveFormWndProcWndProc(var Message: TMessage);
begin
 if Message.Msg = WM_NCACTIVATE then Message.Result := 1
 else FOldLastActiveFormWndProc (Message);
end;

end.
