{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALCalendar
Version:      3.52

Description:  Functions to draw a calendar on a canvas

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

History :     31/03/2007: rename the function in AL*
              12/01/2008: renove call to Controls.pas
              28/05/2008: Update it in WideString/Utf8

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALCalendar;

interface

uses windows,
     types,
     Graphics,
     SysUtils;

Type

  {----------------}
  TALDaybox = Record
    R : Trect;
    BackgroundColor : Tcolor;
    FontColor : Tcolor;
  end;
  TALLstDayBox = array of TALDayBox;

{********************************}
Function ALDrawCalendar(C:Tcanvas;
                        Font,
                        HeaderFont,
                        disableFont:Tfont;
                        Color,
                        HeaderColor,
                        BorderColorLo,
                        BorderColorHi : Tcolor;
                        Year : Integer;
                        FormatSettings: TformatSettings;
                        Var WCalendar : Integer;
                        Var HCalendar : Integer;
                        Var LstDayBox: TALLstdayBox;
                        Const DayOn2Char: Boolean = False;
                        Const WBoxPadding: integer = 4;
                        Const HBoxPadding: integer = 2;
                        Const WCalMonthPadding: integer = 7;
                        Const HCalMonthPadding: integer = 5): Boolean;

implementation

uses dateutils,
     ALFcnUnicode,
     ALFcnString;

{***********************************************************************************}
Function ALDrawCalendarGetTextSizeW(aCanvas: Tcanvas; const Text: WideString): TSize;
begin
  Result.cX := 0;
  Result.cY := 0;
  Windows.GetTextExtentPoint32w(aCanvas.Handle, PWideChar(text), Length(Text), Result);
end;

{****************************************************************************************}
procedure ALDrawCalendarTextOutW(aCanvas: Tcanvas; X, Y: Integer; const Text: WideString);
begin
  TextOutW(aCanvas.Handle, x, Y, PWideChar(Text), length(Text));
end;

{**************************************************************************************************}
Procedure ALDrawCalendarBox(Text: WideString; BackColor, FontColor : Tcolor; Rect:Trect; C:TCanvas);
Var Wt,Ht,xt,yt: Integer;
    R: Trect;
    aSize: Tsize;
Begin
  R := rect;
  inflateRect(r,-1,-1);
  R.Bottom := R.Bottom + 1;
  r.Right := r.Right + 1;
  C.Brush.Color := BackColor;
  C.FillRect(R);

  c.Font.Color := FontColor;
  aSize := ALDrawCalendarGetTextSizeW(c,Text);
  wt := aSize.cx;
  ht := aSize.cy;
  xt := rect.left + ((rect.right - rect.left - wt) div 2);
  yt := rect.top + ((rect.bottom - rect.top - ht) div 2);
  ALDrawCalendarTextOutW(C, xt, yt, text);
end;

{********************************************************************}
Procedure ALDrawCalendarHeader(Month1,Month2,Month3,Year : WideString;
                               R: TRect;
                               C: TCanvas;
                               W1Calendar, HHeader1 : Integer;
                               HeaderColor, BorderColorLo, BorderColorHi : Tcolor);
Var xt, yt, wt, ht: Integer;
Begin
  c.Brush.Color := HeaderColor;
  c.FillRect(r);

  {--Ligne du bas-------------}
  c.Pen.Color := BorderColorLo;
  C.MoveTo(r.Left, r.Bottom);
  C.LineTo(r.Right,r.Bottom);

  {--Separateur1--------------------------}
  C.MoveTo(r.left + W1Calendar, r.Top + 1);
  C.LineTo(r.left + W1Calendar, r.Top + HHeader1 - 1);
  c.Pen.Color := BorderColorHi;
  C.MoveTo(r.left + W1Calendar+1,r.Top + 1);
  C.LineTo(r.Left + W1Calendar+1,r.Top + HHeader1 - 1);

  {--Separateur2--------------}
  c.Pen.Color := BorderColorLo;
  C.MoveTo(r.left + 2*W1Calendar + 2, r.Top + 1);
  C.LineTo(r.left + 2*W1Calendar + 2, r.Top + HHeader1 - 1);
  c.Pen.Color := BorderColorHi;
  C.MoveTo(r.left + 2*W1Calendar + 3, r.Top + 1);
  C.LineTo(r.left + 2*W1Calendar + 3, r.Top + HHeader1 - 1);

  {--Month1---------------}
  ht := ALDrawCalendarGetTextSizeW(C,'^_').cy;
  wt := ALDrawCalendarGetTextSizeW(C, Month1 + ' ' + year).cx;
  xt := R.left + ((W1Calendar - wt) div 2);
  yt := R.Top + ((HHeader1 - ht) div 2);
  ALDrawCalendarTextOutW(C, xt, yt, Month1 + ' ' + year);

  {--Month2-----------------------------}
  wt := ALDrawCalendarGetTextSizeW(C, Month2 + ' ' + year).cx;
  xt := r.left + W1Calendar + 2 + ((W1Calendar - wt) div 2);
  ALDrawCalendarTextOutW(C, xt, yt, Month2 + ' ' + year);

  {--Month3-----------------------------}
  wt := ALDrawCalendarGetTextSizeW(C, Month3 + ' ' + year).cx;
  xt := r.left + 2*(W1Calendar + 2) + ((W1Calendar - wt) div 2);
  ALDrawCalendarTextOutW(C, xt, yt, Month3 + ' ' + year);
end;

{********************************************************************************}
Procedure ALDrawCalendarHeaderDay(Day1,Day2,Day3,Day4,Day5,Day6,Day7 : WideString;
                                  Wblank, Yheader, wbox, hbox:Integer;
                                  C: TCanvas;
                                  BackColor, FontColor, BorderColorLo : Tcolor);
Var r : Trect;
Begin
  r := rect(Wblank,Yheader,Wblank + wbox,Yheader + hbox);
  ALDrawCalendarBox(Day1, BackColor, FontColor, R, C);
  r := rect(r.Right,Yheader,r.Right+wbox,Yheader + hbox);
  ALDrawCalendarBox(Day2, BackColor, FontColor, R, C);
  r := rect(r.Right,Yheader,r.Right+wbox,Yheader + hbox);
  ALDrawCalendarBox(Day3, BackColor, FontColor, R, C);
  r := rect(r.Right,Yheader,r.Right+wbox,Yheader + hbox);
  ALDrawCalendarBox(Day4, BackColor, FontColor, R, C);
  r := rect(r.Right,Yheader,r.Right+wbox,Yheader + hbox);
  ALDrawCalendarBox(Day5, BackColor, FontColor, R, C);
  r := rect(r.Right,Yheader,r.Right+wbox,Yheader + hbox);
  ALDrawCalendarBox(Day6, BackColor, FontColor, R, C);
  r := rect(r.Right,Yheader,r.Right+wbox,Yheader + hbox);
  ALDrawCalendarBox(Day7, BackColor, FontColor, R, C);

  c.Pen.Color := BorderColorLo;
  C.MoveTo(wblank, Yheader + hbox-1);
  C.LineTo(Wblank + 7*wbox,Yheader + hbox-1);
end;


{******************************************************************************************************************}
Procedure ALCalculateDayBoxRect(Year, Wblank, HeaderHeight, Wbox, Hbox, wborder, W1calendar, HMonthPadding: Integer;
                                Var LstdayBox: TALLstDayBox;
                                Var YHeader2, YHeader3, YHeader4, bottom : Integer);
Var Dt: Tdatetime;
    xSamedi, xDimanche, xLundi, xMardi, xMercredi, xJeudi, xVendredi : Integer;
    Month : Integer;
    i : integer;
    YDay : Integer;

    {--------------------------------------}
    Procedure UpdateXday(_Wblank : Integer);
    Begin
      xSamedi := _wblank;
      xDimanche := _wblank + wbox;
      xLundi := _wblank  + wbox*2;
      xMardi := _wblank + wbox*3;
      xMercredi := _wblank + wbox*4;
      xJeudi := _wblank + wbox*5;
      xVendredi := _wblank + wbox*6;
    end;

Begin
  setlength(LstDayBox,daysInAyear(year));
  Dt := encodeDate(year,1,1);
  Yday := HeaderHeight;
  UpdateXday(Wblank);
  YHeader2 := 0;
  YHeader3 := 0;
  YHeader4 := 0;
  Month := 1;

  For i := 0 to high(lstdaybox) do begin

    case DayOfTheWeek(dt) of
      DayMonday : LstDayBox[i].R := Rect(xLundi,Yday,xLundi + wbox, Yday + HBox);
      DayTuesday : LstDayBox[i].R := Rect(xMardi,Yday,xMardi + wbox, Yday + HBox);
      DayWednesday : LstDayBox[i].R := Rect(xMercredi,Yday,xMercredi + wbox, Yday + HBox);
      DayThursday : LstDayBox[i].r := Rect(xJeudi,Yday,xJeudi + wbox, Yday + HBox);
      DayFriday : begin
                    LstDayBox[i].R := Rect(xVendredi,Yday,xVendredi + wbox, Yday + HBox);
                    inc(yday,HBox);
                  end;
      DaySaturday : LstDayBox[i].r := Rect(xSamedi,Yday,xSamedi + wbox, Yday + HBox);
      DaySunday : LstDayBox[i].r := Rect(xDimanche,Yday,xDimanche + wbox, Yday + HBox);
    end;
    Dt := Dt+1;

    If month <> MonthOF(Dt) then begin
      Month := MonthOF(Dt);
      Case Month of
        2 : begin
              UpdateXday(Wblank + wborder + W1calendar);
              YHeader2 := LstdayBox[i].R.Bottom;
              Yday := HeaderHeight;
            end;
        3 : begin
              UpdateXday(Wblank + 2*(Wborder + W1calendar));
              if LstdayBox[i].r.Bottom > YHeader2 then YHeader2 := LstdayBox[i].R.Bottom;
              Yday := HeaderHeight;
            end;
        4 : begin
              UpdateXday(Wblank);
              if LstdayBox[i].R.Bottom > YHeader2 then YHeader2 := LstdayBox[i].R.Bottom;
              YHeader2 := YHeader2 + HMonthPadding;
              Yday := YHeader2 + HeaderHeight;
            end;
        5 : begin
              UpdateXday(Wblank + wborder + W1calendar);
              YHeader3 := LstdayBox[i].R.Bottom;
              Yday := YHeader2 + HeaderHeight;
            end;
        6 : begin
              UpdateXday(Wblank + 2*(Wborder + W1calendar));
              if LstdayBox[i].R.Bottom > YHeader3 then YHeader3 := LstdayBox[i].R.Bottom;
              Yday := YHeader2 + HeaderHeight;
            end;
        7 : begin
              UpdateXday(Wblank);
              If LstdayBox[i].R.Bottom > YHeader3 then YHeader3 := LstdayBox[i].R.Bottom;
              YHeader3 := YHeader3 + HMonthPadding;
              Yday := YHeader3 + HeaderHeight;
            end;
        8 : begin
              UpdateXday(Wblank + wborder + W1calendar);
              YHeader4 := LstdayBox[i].R.Bottom;
              Yday := YHeader3 + HeaderHeight;
            end;
        9 : begin
              UpdateXday(Wblank + 2*(Wborder + W1calendar));
              if LstdayBox[i].R.Bottom > YHeader4 then YHeader4 := LstdayBox[i].R.Bottom;
              Yday := YHeader3 + HeaderHeight;
            end;
        10 : begin
              UpdateXday(Wblank);
              If LstdayBox[i].R.Bottom > YHeader4 then YHeader4 := LstdayBox[i].R.Bottom;
              YHeader4 := YHeader4 + HMonthPadding;
              Yday := YHeader4 + HeaderHeight;
            end;
        11 : begin
              UpdateXday(Wblank + wborder + W1calendar);
              Yday := YHeader4 + HeaderHeight;
              Bottom := LstdayBox[i].R.Bottom;
            end;
        12 : begin
              UpdateXday(Wblank + 2*(Wborder + W1calendar));
              Yday := YHeader4 + HeaderHeight;
              if LstdayBox[i].R.Bottom > Bottom then Bottom := LstdayBox[i].R.Bottom;
            end;
      end;
    end;

  end;

  if LstdayBox[high(LstdayBox)].R.Bottom > Bottom then Bottom := LstdayBox[high(LstdayBox)].R.Bottom;
end;

{********************************}
Function ALDrawCalendar(C:Tcanvas;
                        Font,
                        HeaderFont,
                        disableFont:Tfont;
                        Color,
                        HeaderColor,
                        BorderColorLo,
                        BorderColorHi : Tcolor;
                        Year : Integer;
                        FormatSettings: TformatSettings;
                        Var WCalendar : Integer;
                        Var HCalendar : Integer;
                        Var LstDayBox: TALLstdayBox;
                        Const DayOn2Char: Boolean = False;
                        Const WBoxPadding: integer = 4;
                        Const HBoxPadding: integer = 2;
                        Const WCalMonthPadding: integer = 7;
                        Const HCalMonthPadding: integer = 5): Boolean;

Var R : Trect;
    HBox,WBox: Integer;
    i: Integer;
    Jour, GrayJour : Tdatetime;
    Wblank : Integer;
    YHeader2,YHeader3,YHeader4 : Integer;
    HHeader1, Hheader2 : Integer;
    w1calendar : Integer;
    CalchCalendar : Integer;
    DisableFontColor : Tcolor;
    WideLongMonthNames: array[1..12] of WideString;
    WideLongDayNames: array[1..7] of Widestring;
begin
    {---init des valeurs utilisé----}
    Result := True;
    with FormatSettings do begin
      For i := 1 to 12 do begin
        WideLongMonthNames[i] := WideLowerCase(Utf8Decode(LongMonthNames[i]));
        if WideLongMonthNames[i] <> '' then WideLongMonthNames[i][1] := WideUpperCase(WideLongMonthNames[i][1])[1];
      end;
      For i := 1 to 7 do begin
        WideLongDayNames[i] := Utf8Decode(LongDayNames[i]);
        if DayOn2Char then begin
          if (length(WideLongDayNames[i]) >= 2) then WideLongDayNames[i] := WideUpperCase(WideLongDayNames[i][1]) + WideLowerCase(WideLongDayNames[i][2])
          else if (WideLongDayNames[i] <> '') then WideLongDayNames[i] := WideUpperCase(WideLongDayNames[i][1]);
        end
        else begin
          if (WideLongDayNames[i] <> '') then WideLongDayNames[i] := WideUpperCase(WideLongDayNames[i][1]);
        end;
      end;
    end;


    If assigned(Font) then c.Font.Assign(Font)
    else begin
      c.Font.Color := ClBlack;
      C.Font.Name := 'Ms Sans Sherif';
      C.Font.Size := 8;
    end;
    Hbox := ALDrawCalendarGetTextSizeW(C,'^_').cy + HBoxPadding;
    wbox := ALDrawCalendarGetTextSizeW(C,'__').cx + WBoxPadding;
    If assigned(HeaderFont) then c.Font.Assign(HeaderFont);
    Hheader1 := ALDrawCalendarGetTextSizeW(C,'^_').cy + 2;
    Hheader2 := Hbox;
    W1Calendar := (wcalendar - 4) div 3; // the number 4 is the width of the header1 separator (their is 2 separators of 2 pixels each)
    Wblank := ((W1Calendar - (7*wbox)) div 2);
    if wblank <> WCalMonthPadding then begin
      Result := False;
      Wcalendar := 3*7*wbox + WCalMonthPadding*6 + 4;
    end;
    ALCalculateDayBoxRect(Year, Wblank, Hheader1+hheader2 + 1, Wbox, Hbox, 2, w1calendar, HCalMonthPadding, LstdayBox, YHeader2, YHeader3, YHeader4, CalcHCalendar);
    if CalcHCalendar + 2 <> HCalendar then begin
      Result := False;
      Hcalendar := CalcHCalendar + 2;
    end;
    if not result then exit;    
    If assigned(DisableFont) then DisableFontColor := DisableFont.Color
    Else DisableFontColor := ClSilver;


    {--- On dessine les header1------}
    R := Rect(0,0,Wcalendar,Hheader1);
    ALDrawCalendarHeader(WideLongMonthNames[1],WideLongMonthNames[2],WideLongMonthNames[3],inttostr(year), R, C, W1Calendar, Hheader1, HeaderColor, BorderColorLo, BorderColorHi);
    R := Rect(0,YHeader2,wcalendar,YHeader2+Hheader1);
    ALDrawCalendarHeader(WideLongMonthNames[4],WideLongMonthNames[5],WideLongMonthNames[6],inttostr(year), R, C, W1Calendar, Hheader1, HeaderColor, BorderColorLo, BorderColorHi);
    R := Rect(0,YHeader3,wcalendar,YHeader3 + Hheader1);
    ALDrawCalendarHeader(WideLongMonthNames[7],WideLongMonthNames[8],WideLongMonthNames[9],inttostr(year), R, C, W1Calendar, Hheader1, HeaderColor, BorderColorLo, BorderColorHi);
    R := Rect(0,YHeader4,wcalendar,YHeader4 + Hheader1);
    ALDrawCalendarHeader(WideLongMonthNames[10],WideLongMonthNames[11],WideLongMonthNames[12],inttostr(year), R, C, W1Calendar, Hheader1, HeaderColor, BorderColorLo, BorderColorHi);


    {--- On dessine les header2--------------}
    If assigned(Font) then c.Font.Assign(Font)
    else begin
      c.Font.Color := ClBlack;
      C.Font.Name := 'Ms Sans Sherif';
      C.Font.Size := 8;
    end;
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank, hheader1, wbox, hbox, C, Color, c.Font.Color, BorderColorLo);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + W1Calendar + 2, hheader1, wbox, hbox, C, Color, c.Font.Color, BorderColorLo);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + 2*(W1Calendar + 2), hheader1, wbox, hbox, C, Color, c.Font.Color, BorderColorLo);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank, YHeader2 + hheader1, wbox, hbox, C, Color, c.Font.Color, BorderColorLo);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + W1Calendar + 2, YHeader2 + hheader1, wbox, hbox, C, Color, c.Font.Color, BorderColorLo);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + 2*(W1Calendar + 2), YHeader2 + hheader1, wbox, hbox, C, Color, c.Font.Color, BorderColorLo);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank, YHeader3 + hheader1, wbox, hbox, C, Color, c.Font.Color, BorderColorLo);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + W1Calendar + 2, YHeader3 + hheader1, wbox, hbox, C, Color, c.Font.Color, BorderColorLo);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + 2*(W1Calendar + 2), YHeader3 + hheader1, wbox, hbox, C, Color, c.Font.Color, BorderColorLo);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank, YHeader4 + hheader1, wbox, hbox, C, Color, c.Font.Color, BorderColorLo);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + W1Calendar + 2, YHeader4 + hheader1, wbox, hbox, C, Color, c.Font.Color, BorderColorLo);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + 2*(W1Calendar + 2), YHeader4 + hheader1, wbox, hbox, C, Color, c.Font.Color, BorderColorLo);


    {--- On dessine les Jours-----}
    Jour := encodeDate(year,01,01);
    For i := 0 to high(lstdaybox) do begin
      if DayOfTheMonth(Jour) = 1 then begin
        GrayJour := Jour - 1;
        R := LstDayBox[i].R;
        r.Left := r.Left - Wbox;
        R.Right := r.Right - wbox;
        while DayOfTheWeek(GrayJour) <> DayFriday do begin
          ALDrawCalendarBox(inttostr(DayOfTheMonth(GrayJour)), Color, DisableFontColor, R, C);
          r.Left := r.Left - Wbox;
          R.Right := r.Right - wbox;
          GrayJour := GrayJour - 1;
        end;
      end;
      ALDrawCalendarBox(inttostr(DayOfTheMonth(Jour)), LstDayBox[i].BackgroundColor, LstDayBox[i].FontColor, LstDayBox[i].R, C);
      jour := jour + 1;
      if DayOfTheMonth(Jour) = 1 then begin
        GrayJour := Jour;
        R := LstDayBox[i].R;
        r.Left := r.Left + Wbox;
        R.Right := r.Right + wbox;
        while DayOfTheWeek(GrayJour) <> DaySaturday do begin
          ALDrawCalendarBox(inttostr(DayOfTheMonth(GrayJour)), Color, DisableFontColor, R, C);
          r.Left := r.Left + Wbox;
          R.Right := r.Right + wbox;
          GrayJour := GrayJour + 1;
        end;
      end;
    end;
end;

end.
