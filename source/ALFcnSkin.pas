{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      Alcinoe Skin Function for Visual Components
Version:      3.50

Description:  Function to use with the onpaint property of alcinoe
              component (TalEdit, TALButton, TalComboBox, etc.

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
Unit ALFcnSkin;

interface

uses AlScrollBar;

Procedure PaintALButtonBlueSkin(Sender: Tobject; var continue: boolean; Const UpdateFontColor: Boolean=True);
Procedure PaintALGraphicButtonBlueSkin(Sender: Tobject;  var continue: boolean; Const UpdateFontColor: Boolean=True);
Procedure PaintALRadioButtonBlueSkin(Sender: Tobject; var continue: boolean; Const UpdateFontColor: Boolean=True);
Procedure PaintALCheckBoxBlueSkin(Sender: Tobject; var continue: boolean; Const UpdateFontColor: Boolean=True);
procedure PaintALComboBoxBlueSkin(Sender: Tobject; var continue: boolean; Const UpdateFontColor: Boolean=True);
procedure PaintALEditBlueSkin(Sender: Tobject; var continue: boolean; Const UpdateFontColor: Boolean=True);
procedure PaintALMaskEditBlueSkin(Sender: Tobject; var continue: boolean; Const UpdateFontColor: Boolean=True);
procedure PaintALStaticTextBlueSkin(Sender: Tobject; var continue: boolean);
procedure PaintALListBoxBlueSkin(Sender: TObject; var continue: boolean; Const UpdateFontColor: Boolean=True);
procedure PaintALListBoxScrollBarBlueSkin(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
procedure PaintALMemoBlueSkin(Sender: TObject; var continue: boolean; Const UpdateFontColor: Boolean=True);
procedure PaintALMemoScrollBarBlueSkin(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);

implementation

uses stdctrls,
     graphics,
     ALStaticText,
     AlComboBox,
     AlButton,
     ALEdit,
     ALListBox,
     AlMemo;


////////////////////////////////////////////////////////////////////////////////
///////////////////////// Blue skin ////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{***********************************************************************************************************}
Procedure PaintALButtonBlueSkin(Sender: Tobject; var continue: boolean; Const UpdateFontColor: Boolean=True);
Var oldFontColor, OldColor: Tcolor;
Begin
  With Sender As TalButton Do begin
    oldFontColor := Font.Color;
    OldColor := Color;

    {Ctrl Enabled}
    if Enabled then begin
       If UpdateFontColor then Font.Color := ClBlack;

       {CTRL down}
       If State = cbchecked then begin
         {ctrl pushed}
         If MouseIsDown or KeyIsDown then begin
           BorderColor := $006B2408;
           Color := $00B59284;
         end
         {ctrl Focused}
         else If Focused then begin
           BorderColor := $006B2408;
           Color := $00B59284;
         end
         {ctrl MouseIn}
         else If MouseInControl then begin
           borderColor := $006B2408;
           Color := $00B59284;
         end
         {ctrl MouseOut}
         else begin
           borderColor := $006B2408;
           Color := $00B59284;
         end;
       end

       {CTRL UP}
       else if State = cbunchecked then begin
         {ctrl pushed}
         If MouseIsDown or KeyIsDown then begin
           BorderColor := $006B2408;
           Color := $00B59284;
         end
         {ctrl Focused}
         else If Focused then begin
           borderColor := $006B2408;
           Color := $00CEBAB5;
         end
         {ctrl MouseIn}
         else If MouseInControl then begin
           borderColor := $006B2408;
           Color := $00CEBAB5;
         end
         {ctrl MouseOut}
         else begin
           borderColor := $00C6A69C;
           Color := $00E7D3CE;
         end;
       end
    end

    {Button Disabled}
    else begin
       borderColor := $00BAC7C9;
       Color := $00E1E9EA;
       If UpdateFontColor then Font.Color := ClGray;
    end;

    Continue := (oldFontColor = Font.Color) and (OldColor = Color);
  end;
end;

{******************************************************************************************************************}
Procedure PaintALGraphicButtonBlueSkin(Sender: Tobject; var continue: boolean; Const UpdateFontColor: Boolean=True);
Var oldFontColor, OldColor: Tcolor;
begin
  With Sender As TalGraphicButton Do begin
    oldFontColor := Font.Color;
    OldColor := Color;

    {Ctrl Enabled}
    if Enabled then begin
       If UpdateFontColor then Font.Color := ClBlack;

       {CTRL down}
       If State = cbchecked then begin
         {ctrl pushed}
         If MouseIsDown or KeyIsDown then begin
           BorderColor := $006B2408;
           Color := $00B59284;
         end
         {ctrl MouseIn}
         else If MouseInControl then begin
           borderColor := $006B2408;
           Color := $00B59284;
         end
         {ctrl MouseOut}
         else begin
           borderColor := $006B2408;
           Color := $00B59284;
         end;
       end

       {CTRL UP}
       else if State = cbunchecked then begin
         {ctrl pushed}
         If MouseIsDown or KeyIsDown then begin
           BorderColor := $006B2408;
           Color := $00B59284;
         end
         {ctrl MouseIn}
         else If MouseInControl then begin
           borderColor := $006B2408;
           Color := $00CEBAB5;
         end
         {ctrl MouseOut}
         else begin
           borderColor := $00C6A69C;
           Color := $00E7D3CE;
         end;
       end
    end

    {Button Disabled}
    else begin
       borderColor := $00BAC7C9;
       Color := $00E1E9EA;
       If UpdateFontColor then Font.Color := clGray;
    end;

    Continue := (oldFontColor = Font.Color) and (OldColor = Color);
  end;
End;

{****************************************************************************************************************}
Procedure PaintALRadioButtonBlueSkin(Sender: Tobject; var continue: boolean; Const UpdateFontColor: Boolean=True);
Var oldFontColor: Tcolor;
Begin
  With Sender As TalRadioButton Do begin
    oldFontColor := Font.Color;

    {Ctrl Enabled}
    if Enabled then begin
       If UpdateFontColor then Font.Color := ClBlack;
       MarkColor := clblack;

       {CTRL down}
       If checked then begin
         {ctrl pushed}
         If MouseIsDown or KeyIsDown then begin
           BorderColor := $006B2408;
           BlankColor := $00B59284;
         end
         {ctrl Focused}
         else If Focused then begin
           borderColor := $006B2408;
           BlankColor := $00CEBAB5;
         end
         {ctrl MouseIn}
         else If MouseInControl then begin
           borderColor := $006B2408;
           BlankColor := $00CEBAB5;
         end
         {ctrl MouseOut}
         else begin
           borderColor := $006B2408;
           BlankColor := clwhite;
         end;
       end

       {CTRL UP}
       else begin
         {ctrl pushed}
         If MouseIsDown or KeyIsDown then begin
           BorderColor := $006B2408;
           BlankColor := $00B59284;
         end
         {ctrl Focused}
         else If Focused then begin
           borderColor := $006B2408;
           BlankColor := $00CEBAB5;
         end
         {ctrl MouseIn}
         else If MouseInControl then begin
           borderColor := $006B2408;
           BlankColor := $00CEBAB5;
         end
         {ctrl MouseOut}
         else begin
           borderColor := $00C6A69C;
           BlankColor := clwhite;
         end;
       end
    end

    {Button Disabled}
    else begin
       MarkColor := $009F9F9F;
       borderColor := $00BAC7C9;
       BlankColor := $00E1E9EA;
       If UpdateFontColor then Font.Color := $009F9F9F;
    end;

    Continue := (oldFontColor = Font.Color);
  end;
end;

{*************************************************************************************************************}
Procedure PaintALCheckBoxBlueSkin(Sender: Tobject; var continue: boolean; Const UpdateFontColor: Boolean=True);
Var oldFontColor: Tcolor;
Begin
  With Sender As TalCheckBox Do begin
    oldFontColor := Font.Color;

    {Ctrl Enabled}
    if Enabled then begin
       If UpdateFontColor then Font.Color := ClBlack;

       {CTRL down}
       If state=cbchecked then begin
         MarkColor := clblack;

         {ctrl pushed}
         If MouseIsDown or KeyIsDown then begin
           BorderColor := $006B2408;
           BlankColor := $00B59284;
         end
         {ctrl Focused}
         else If Focused then begin
           borderColor := $006B2408;
           BlankColor := $00CEBAB5;
         end
         {ctrl MouseIn}
         else If MouseInControl then begin
           borderColor := $006B2408;
           BlankColor := $00CEBAB5;
         end
         {ctrl MouseOut}
         else begin
           borderColor := $006B2408;
           BlankColor := clwhite;
         end;
       end

       {CTRL UP}
       else If state=cbunchecked then begin
         MarkColor := clblack;

         {ctrl pushed}
         If MouseIsDown or KeyIsDown then begin
           BorderColor := $006B2408;
           BlankColor := $00B59284;
         end
         {ctrl Focused}
         else If Focused then begin
           borderColor := $006B2408;
           BlankColor := $00CEBAB5;
         end
         {ctrl MouseIn}
         else If MouseInControl then begin
           borderColor := $006B2408;
           BlankColor := $00CEBAB5;
         end
         {ctrl MouseOut}
         else begin
           borderColor := $00C6A69C;
           BlankColor := clwhite;
         end;
       end

       {CTRL grayed}
       else If state=cbgrayed then begin
         MarkColor :=  $009F9F9F ;

         {ctrl pushed}
         If MouseIsDown or KeyIsDown then begin
           BorderColor := $006B2408;
           BlankColor := $00B59284;
         end
         {ctrl Focused}
         else If Focused then begin
           borderColor := $006B2408;
           BlankColor := $00CEBAB5;
         end
         {ctrl MouseIn}
         else If MouseInControl then begin
           borderColor := $006B2408;
           BlankColor := $00CEBAB5;
         end
         {ctrl MouseOut}
         else begin
           borderColor := $00C6A69C;
           BlankColor :=  $00C8D0D4
         end;
       end

    end

    {Button Disabled}
    else begin
       MarkColor := $009F9F9F;
       borderColor := $00BAC7C9;
       BlankColor := $00E1E9EA;
       If UpdateFontColor then Font.Color := $009F9F9F;
    end;

    Continue := (oldFontColor = Font.Color);
  end;
end;

{*************************************************************************************************************}
procedure PaintALComboBoxBlueSkin(Sender: Tobject; var continue: boolean; Const UpdateFontColor: Boolean=True);
begin
  Continue := True;
  with sender as TalComboBox do begin

    {Edit enabled}
    if Enabled then begin
       If UpdateFontColor and (font.Color <> ClBlack) then begin
         Font.Color := ClBlack;
         Continue := False;
       end;

       {Edit down}
       If DroppedDown then begin
         borderColor := $006B2408;
         ButtonColor := $00B59284;
         ArrowColor := clWhite;
       end
       {Edit Focused}
       else If Focused or ForceDrawFocused then begin
         borderColor := $006B2408;
         ButtonColor := $00CEBAB5;
         ArrowColor := clblack;
       end
       {Edit MouseIn}
       else if mouseinControl then begin
         borderColor := $006B2408;
         ButtonColor := $00CEBAB5;
         ArrowColor := clblack;
       end
       {edit MouseOut}
       else begin
         borderColor := $00C6A69C;
         ButtonColor := $00E7D3CE;
         ArrowColor := clblack;
       end;
    end
    {edit disabled}
    else begin
      borderColor := $00BAC7C9;
      ButtonColor := $00E1E9EA;
      ArrowColor := $00C3CAC8;
      If UpdateFontColor and (font.Color <> ClGray) then begin
         Font.Color := ClGray;
         Continue := False;
      end;
    end;

  end;
end;

{*********************************************************************************************************}
procedure PaintALEditBlueSkin(Sender: Tobject; var continue: boolean; Const UpdateFontColor: Boolean=True);
begin
  Continue := True;
  with sender as TalEdit do begin

    {Edit enabled}
    if Enabled then begin
       If UpdateFontColor and (font.Color <> ClBlack) then begin
         Font.Color := ClBlack;
         Continue := False;
       end;

       {Edit down}
       If MouseIsDown then begin
         borderColor := $006B2408;
         BtnColor := $00B59284;
       end
       {Edit Focused}
       else If Focused then begin
         borderColor := $006B2408;
         BtnColor :=  $00CEBAB5;
       end
       {Edit MouseIn}
       else if (mouseinControl) or Focused then begin
         borderColor := $006B2408;
         BtnColor := $00CEBAB5;
       end
       {edit MouseOut}
       else begin
         borderColor := $00C6A69C;
         BtnColor := $00E7D3CE;
       end;
    end

    {edit disabled}
    else begin
      borderColor := $00BAC7C9;
      BtnColor := $00E1E9EA;
      If UpdateFontColor and (font.Color <> ClGray) then begin
         Font.Color := ClGray;
         Continue := False;
      end;
    end;

  end;
end;

{*************************************************************************************************************}
procedure PaintALMaskEditBlueSkin(Sender: Tobject; var continue: boolean; Const UpdateFontColor: Boolean=True);
begin
  Continue := True;
  with sender as TalMaskEdit do begin

    {Edit enabled}
    if Enabled then begin
       If UpdateFontColor and (font.Color <> ClBlack) then begin
         Font.Color := ClBlack;
         Continue := False;
       end;

       {Edit down}
       If MouseIsDown then begin
         borderColor := $006B2408;
         BtnColor := $00B59284;
       end
       {Edit Focused}
       else If Focused then begin
         borderColor := $006B2408;
         BtnColor :=  $00CEBAB5;
       end
       {Edit MouseIn}
       else if (mouseinControl) or Focused then begin
         borderColor := $006B2408;
         BtnColor := $00CEBAB5;
       end
       {edit MouseOut}
       else begin
         borderColor := $00C6A69C;
         BtnColor := $00E7D3CE;
       end;
    end

    {edit disabled}
    else begin
      borderColor := $00BAC7C9;
      BtnColor := $00E1E9EA;
      If UpdateFontColor and (font.Color <> ClGray) then begin
         Font.Color := ClGray;
         Continue := False;
      end;
    end;

  end;
end;

{**************************************************************************}
procedure PaintALStaticTextBlueSkin(Sender: Tobject; var continue: boolean);
Var oldFontColor: Tcolor;
begin
  with sender as TalStaticText do begin
    oldFontColor := font.color;

    {Edit enabled}
    if Enabled then begin
       {Edit down}
       If MouseIsDown then font.color := $00B16029
       {Edit Focused}
       else If Focused then font.color := $00B16029
       {Edit MouseIn}
       else if (mouseinControl) or Focused then font.color := $00B16029
       {edit MouseOut}
       else font.color := clblack;
    end

    {edit disabled}
    else font.color := clgray;

    continue := (font.color = oldFontColor);
  end;
end;

{********************************************************************************************************}
procedure PaintALListBoxScrollBarBlueSkin(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
begin
  Continue := True;
  With sender as TalListBox Do Begin

    If enabled and not ReadOnly then begin
      With VertScrollBar Do Begin
        If not enabled then begin
          ArrowColor := $00DAB8AF;
          BarColor := $00DAB8AF;
          borderColor := $00DAB8AF;
          Case Area of
             sabtnArrowDown, sabtnArrowUp, saScroller: Color := $00E7D3CE;
             saTrackDown, saTrackUp : Color := $00EBE3E0;
          end;
          exit;
        end;

        ArrowColor := ClBlack;
        BarColor := ClBlack;
        If (sender as TalListBox).MouseInControl or (sender as TalListBox).focused then begin
          borderColor := $006B2408;
          Case Area of
             sabtnArrowDown, sabtnArrowUp, saScroller: Color := $00CEBAB5;
             saTrackDown, saTrackUp : Color := $00E0D5D1;
          end
        end
        else begin
          borderColor := $00C6A69C;
          Case Area of
             sabtnArrowDown, sabtnArrowUp, saScroller: Color := $00E7D3CE;
             saTrackDown, saTrackUp : Color := $00EBE3E0;
          end
        end;

        If mouseIsDown then Begin
          Case MouseDownAt of
              sabtnArrowDown: If Area in [sabtnArrowDown,saTrackDown] then begin
                                 borderColor := $006B2408;
                                 Color := $00B59284;
                              end;
              sabtnArrowUp : If Area in [saBtnArrowup,saTrackup] then begin
                               borderColor := $006B2408;
                               Color := $00B59284;
                             end;
              saTrackDown :  If Area in [saTrackDown] then begin
                               borderColor := $006B2408;
                               Color := $00B59284;
                             end;
              saTrackUp : If Area in [saTrackUp] then begin
                            borderColor := $006B2408;
                            Color := $00B59284;
                          end;
              saScroller : If Area in [saScroller] then begin
                             borderColor := $006B2408;
                             Color := $00B59284;
                           end;
          end;
        end;
      end;
    end

    else begin
      With VertScrollBar Do Begin
        ArrowColor := $00C3CAC8;
        BarColor := $00C3CAC8;
        borderColor := $00BAC7C9;
        Case Area of
           sabtnArrowDown, sabtnArrowUp, saScroller: Color := $00E1E9EA;
           saTrackDown, saTrackUp : Color := $00EBF3F4;
        end;
        exit;
      end;
    end;

  end;
end;

{************************************************************************************************************}
procedure PaintALListBoxBlueSkin(Sender: TObject; var continue: boolean; Const UpdateFontColor: Boolean=True);
begin
  Continue := True;
  With sender as TalListBox Do Begin

     {Edit disabled}
     If not enabled or readonly then begin
       borderColor := $00BAC7C9;
       If UpdateFontColor and (Font.Color <> clGray) then begin
         Font.color := ClGray;
         continue := False;
       end;
     end

     {Edit enabled}
     else begin
       If UpdateFontColor and (Font.Color <> clBlack) then begin
         Font.color := ClBlack;
         Continue := false;
       end;

       {Edit mouseIn or focused}
       If mouseInControl or focused then BorderColor := $006B2408

       {Edit mouseOut or not focused}
       else BorderColor := $00C6A69C;
     end;
  end;
end;


{*****************************************************************************************************}
procedure PaintALMemoScrollBarBlueSkin(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
begin
  Continue := True;
  With sender as TalMemo Do Begin

    If enabled and not ReadOnly then begin
      With VertScrollBar Do Begin
        If not enabled then begin
          ArrowColor := $00DAB8AF;
          BarColor := $00DAB8AF;
          borderColor := $00DAB8AF;
          Case Area of
             sabtnArrowDown, sabtnArrowUp, saScroller: Color := $00E7D3CE;
             saTrackDown, saTrackUp : Color := $00EBE3E0;
          end;
          exit;
        end;

        ArrowColor := ClBlack;
        BarColor := ClBlack;
        If (sender as TalMemo).MouseInControl or (sender as TalMemo).focused then begin
          borderColor := $006B2408;
          Case Area of
             sabtnArrowDown, sabtnArrowUp, saScroller: Color := $00CEBAB5;
             saTrackDown, saTrackUp : Color := $00E0D5D1;
          end
        end
        else begin
          borderColor := $00C6A69C;
          Case Area of
             sabtnArrowDown, sabtnArrowUp, saScroller: Color := $00E7D3CE;
             saTrackDown, saTrackUp : Color := $00EBE3E0;
          end
        end;

        If mouseIsDown then Begin
          Case MouseDownAt of
              sabtnArrowDown: If Area in [sabtnArrowDown,saTrackDown] then begin
                                 borderColor := $006B2408;
                                 Color := $00B59284;
                              end;
              sabtnArrowUp : If Area in [saBtnArrowup,saTrackup] then begin
                               borderColor := $006B2408;
                               Color := $00B59284;
                             end;
              saTrackDown :  If Area in [saTrackDown] then begin
                               borderColor := $006B2408;
                               Color := $00B59284;
                             end;
              saTrackUp : If Area in [saTrackUp] then begin
                            borderColor := $006B2408;
                            Color := $00B59284;
                          end;
              saScroller : If Area in [saScroller] then begin
                             borderColor := $006B2408;
                             Color := $00B59284;
                           end;
          end;
        end;
      end;
    end

    else begin
      With VertScrollBar Do Begin
        ArrowColor := $00C3CAC8;
        BarColor := $00C3CAC8;
        borderColor := $00BAC7C9;
        Case Area of
           sabtnArrowDown, sabtnArrowUp, saScroller: Color := $00E1E9EA;
           saTrackDown, saTrackUp : Color := $00EBF3F4;
        end;
        exit;
      end;
    end;

  end;
end;

{*********************************************************************************************************}
procedure PaintALMemoBlueSkin(Sender: TObject; var continue: boolean; Const UpdateFontColor: Boolean=True);
begin
  Continue := True;
  With sender as TalMemo Do Begin

     {Edit disabled}
     If not enabled or readonly then begin
       borderColor := $00BAC7C9;
       If UpdateFontColor and (Font.Color <> clGray) then begin
         Font.color := ClGray;
         continue := False;
       end;
     end

     {Edit enabled}
     else begin
       If UpdateFontColor and (Font.Color <> clBlack) then begin
         Font.color := ClBlack;
         Continue := false;
       end;

       {Edit mouseIn or focused}
       If mouseInControl or focused then BorderColor := $006B2408

       {Edit mouseout or not focused}
       else BorderColor := $00C6A69C;
     end;
  end;
end;

end.
