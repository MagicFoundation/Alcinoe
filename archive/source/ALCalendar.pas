{*******************************************************************************
Functions to draw a calendar on a canvas
*******************************************************************************}

unit ALCalendar;

interface

uses
  System.types,
  Vcl.Graphics,
  ALString;

Type

  {---------------------------}
  TALDrawCalendarStyle = Record
    FontColor: TColor;
    FontHeight: Integer;
    FontName: TFontName;
    FontStyle: TFontStyles;
    BackGroundColor: TColor;
    PaddingLeft: integer;
    PaddingRight: integer;
    PaddingTop: integer;
    PaddingBottom: integer;
  end;

  {-------------------------}
  TALDrawCalendarDay = Record
    Rect : Trect;
    Style: TALDrawCalendarStyle;
  end;
  TALDrawCalendarDays = array of TALDrawCalendarDay;

{-------------------------------------}
Function ALDrawCalendar(Canvas:Tcanvas;
                        //----
                        const MonthHeaderStyle,
                              DayHeaderStyle,
                              DayDisabledStyle: TALDrawCalendarStyle;
                        //----
                        BackGroundColor,
                        //----
                        MonthsHeaderBorderColorBottom,
                        MonthsHeaderBorderColorLo,
                        MonthsHeaderBorderColorHi,
                        DaysHeaderBorderColorBottom: Tcolor;
                        //----
                        Year : Integer;
                        const FormatSettings: TALFormatSettings; // LongMonthNames and LongDayNames must be in UTF8
                        //----
                        Var CalendarWidth : Integer;
                        Var CalendarHeight : Integer;
                        //----
                        Var Days: TALDrawCalendarDays;
                        //----
                        Const DayHeaderOn2Char: Boolean = False;
                        //----
                        Const MonthBoxPaddingLeftAndRight: integer = 7;
                        Const MonthBoxPaddingTop: integer = 5): Boolean;

implementation

uses
  Winapi.windows,
  System.SysUtils,
  System.dateutils;

{***********************************************************************************}
Function ALDrawCalendarGetTextSizeW(aCanvas: Tcanvas; const Text: WideString): TSize;
begin
  Result.cX := 0;
  Result.cY := 0;
  GetTextExtentPoint32w(aCanvas.Handle, PWideChar(text), Length(Text), Result);
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
                               HeaderColor, BorderColorBottom, BorderColorLo, BorderColorHi : Tcolor);
Var xt, yt, wt, ht: Integer;
Begin

  {--init canvas--------------}
  c.Brush.Color := HeaderColor;
  c.FillRect(r);

  {--Bottom border----------------}
  c.Pen.Color := BorderColorBottom;
  C.MoveTo(r.Left, r.Bottom);
  C.LineTo(r.Right,r.Bottom);

  {--Separator1---------------------------}
  C.MoveTo(r.left + W1Calendar, r.Top + 1);
  C.LineTo(r.left + W1Calendar, r.Top + HHeader1 - 1);
  c.Pen.Color := BorderColorHi;
  C.MoveTo(r.left + W1Calendar+1,r.Top + 1);
  C.LineTo(r.Left + W1Calendar+1,r.Top + HHeader1 - 1);

  {--Separator2---------------}
  c.Pen.Color := BorderColorLo;
  C.MoveTo(r.left + 2*W1Calendar + 2, r.Top + 1);
  C.LineTo(r.left + 2*W1Calendar + 2, r.Top + HHeader1 - 1);
  c.Pen.Color := BorderColorHi;
  C.MoveTo(r.left + 2*W1Calendar + 3, r.Top + 1);
  C.LineTo(r.left + 2*W1Calendar + 3, r.Top + HHeader1 - 1);

  {--Month1----------------------------------}
  ht := ALDrawCalendarGetTextSizeW(C,'^_').cy;
  wt := ALDrawCalendarGetTextSizeW(C, Month1 + ' ' + year).cx;
  xt := R.left + ((W1Calendar - wt) div 2);
  yt := R.Top + ((HHeader1 - ht) div 2);
  ALDrawCalendarTextOutW(C, xt, yt, Month1 + ' ' + year);

  {--Month2--------------------------------------------------}
  wt := ALDrawCalendarGetTextSizeW(C, Month2 + ' ' + year).cx;
  xt := r.left + W1Calendar + 2 + ((W1Calendar - wt) div 2);
  ALDrawCalendarTextOutW(C, xt, yt, Month2 + ' ' + year);

  {--Month3--------------------------------------------------}
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
                                Var Days: TALDrawCalendarDays;
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
  setlength(Days,daysInAyear(year));
  Dt := encodeDate(year,1,1);
  Yday := HeaderHeight;
  UpdateXday(Wblank);
  YHeader2 := 0;
  YHeader3 := 0;
  YHeader4 := 0;
  Month := 1;

  For i := low(Days) to high(Days) do begin

    case DayOfTheWeek(dt) of
      DayMonday : Days[i].Rect := Rect(xLundi,Yday,xLundi + wbox, Yday + HBox);
      DayTuesday : Days[i].Rect := Rect(xMardi,Yday,xMardi + wbox, Yday + HBox);
      DayWednesday : Days[i].Rect := Rect(xMercredi,Yday,xMercredi + wbox, Yday + HBox);
      DayThursday : Days[i].Rect := Rect(xJeudi,Yday,xJeudi + wbox, Yday + HBox);
      DayFriday : begin
                    Days[i].Rect := Rect(xVendredi,Yday,xVendredi + wbox, Yday + HBox);
                    inc(yday,HBox);
                  end;
      DaySaturday : Days[i].Rect := Rect(xSamedi,Yday,xSamedi + wbox, Yday + HBox);
      DaySunday : Days[i].Rect := Rect(xDimanche,Yday,xDimanche + wbox, Yday + HBox);
    end;
    Dt := Dt+1;

    If month <> MonthOF(Dt) then begin
      Month := MonthOF(Dt);
      Case Month of
        2 : begin
              UpdateXday(Wblank + wborder + W1calendar);
              YHeader2 := Days[i].Rect.Bottom;
              Yday := HeaderHeight;
            end;
        3 : begin
              UpdateXday(Wblank + 2*(Wborder + W1calendar));
              if Days[i].Rect.Bottom > YHeader2 then YHeader2 := Days[i].Rect.Bottom;
              Yday := HeaderHeight;
            end;
        4 : begin
              UpdateXday(Wblank);
              if Days[i].Rect.Bottom > YHeader2 then YHeader2 := Days[i].Rect.Bottom;
              YHeader2 := YHeader2 + HMonthPadding;
              Yday := YHeader2 + HeaderHeight;
            end;
        5 : begin
              UpdateXday(Wblank + wborder + W1calendar);
              YHeader3 := Days[i].Rect.Bottom;
              Yday := YHeader2 + HeaderHeight;
            end;
        6 : begin
              UpdateXday(Wblank + 2*(Wborder + W1calendar));
              if Days[i].Rect.Bottom > YHeader3 then YHeader3 := Days[i].Rect.Bottom;
              Yday := YHeader2 + HeaderHeight;
            end;
        7 : begin
              UpdateXday(Wblank);
              If Days[i].Rect.Bottom > YHeader3 then YHeader3 := Days[i].Rect.Bottom;
              YHeader3 := YHeader3 + HMonthPadding;
              Yday := YHeader3 + HeaderHeight;
            end;
        8 : begin
              UpdateXday(Wblank + wborder + W1calendar);
              YHeader4 := Days[i].Rect.Bottom;
              Yday := YHeader3 + HeaderHeight;
            end;
        9 : begin
              UpdateXday(Wblank + 2*(Wborder + W1calendar));
              if Days[i].Rect.Bottom > YHeader4 then YHeader4 := Days[i].Rect.Bottom;
              Yday := YHeader3 + HeaderHeight;
            end;
        10 : begin
              UpdateXday(Wblank);
              If Days[i].Rect.Bottom > YHeader4 then YHeader4 := Days[i].Rect.Bottom;
              YHeader4 := YHeader4 + HMonthPadding;
              Yday := YHeader4 + HeaderHeight;
            end;
        11 : begin
              UpdateXday(Wblank + wborder + W1calendar);
              Yday := YHeader4 + HeaderHeight;
              Bottom := Days[i].Rect.Bottom;
            end;
        12 : begin
              UpdateXday(Wblank + 2*(Wborder + W1calendar));
              Yday := YHeader4 + HeaderHeight;
              if Days[i].Rect.Bottom > Bottom then Bottom := Days[i].Rect.Bottom;
            end;
      end;
    end;

  end;

  if Days[high(Days)].Rect.Bottom > Bottom then Bottom := Days[high(Days)].Rect.Bottom;
end;

{*************************************}
Function ALDrawCalendar(Canvas:Tcanvas;
                        //----
                        const MonthHeaderStyle,
                              DayHeaderStyle,
                              DayDisabledStyle: TALDrawCalendarStyle;
                        //----
                        BackGroundColor,
                        //----
                        MonthsHeaderBorderColorBottom,
                        MonthsHeaderBorderColorLo,
                        MonthsHeaderBorderColorHi,
                        DaysHeaderBorderColorBottom: Tcolor;
                        //----
                        Year : Integer;
                        const FormatSettings: TALFormatSettings; // LongMonthNames and LongDayNames must be in UTF8
                        //----
                        Var CalendarWidth : Integer;
                        Var CalendarHeight : Integer;
                        //----
                        Var Days: TALDrawCalendarDays;
                        //----
                        Const DayHeaderOn2Char: Boolean = False;
                        //----
                        Const MonthBoxPaddingLeftAndRight: integer = 7;
                        Const MonthBoxPaddingTop: integer = 5): Boolean;

Var HBox, WBox: Integer;
    Day, GrayDay: Tdatetime;
    Wblank: Integer;
    YHeader2, YHeader3, YHeader4: Integer;
    HHeader1, Hheader2: Integer;
    w1calendar: Integer;
    CalcHCalendar: Integer;
    WideLongMonthNames: array[1..12] of WideString;
    WideLongDayNames: array[1..7] of Widestring;
    R : Trect;
    i: Integer;

begin

    //init WideLongMonthNames and WideLongDayNames
    Result := True;
    with FormatSettings do begin
      For i := 1 to 12 do begin
        {$IFDEF UNICODE}
        WideLongMonthNames[i] := WideLowerCase(Utf8ToWideString(LongMonthNames[i]));
        {$ELSE}
        WideLongMonthNames[i] := WideLowerCase(Utf8Decode(LongMonthNames[i]));
        {$ENDIF}
        if WideLongMonthNames[i] <> '' then WideLongMonthNames[i][1] := WideUpperCase(WideLongMonthNames[i][1])[1];
      end;
      For i := 1 to 7 do begin
        {$IFDEF UNICODE}
        WideLongDayNames[i] := Utf8ToWideString(LongDayNames[i]);
        {$ELSE}
        WideLongDayNames[i] := Utf8Decode(LongDayNames[i]);
        {$ENDIF}
        if DayHeaderOn2Char then begin
          if (length(WideLongDayNames[i]) >= 2) then WideLongDayNames[i] := WideUpperCase(WideLongDayNames[i][1]) + WideLowerCase(WideLongDayNames[i][2])
          else if (WideLongDayNames[i] <> '') then WideLongDayNames[i] := WideUpperCase(WideLongDayNames[i][1]);
        end
        else begin
          if (WideLongDayNames[i] <> '') then WideLongDayNames[i] := WideUpperCase(WideLongDayNames[i][1]);
        end;
      end;
    end;


    //Calc the width and the height
    Canvas.Font.Color := DayHeaderStyle.FontColor;
    Canvas.Font.Name := DayHeaderStyle.FontName;
    Canvas.Font.Height := DayHeaderStyle.FontHeight;
    Canvas.Font.style := DayHeaderStyle.FontStyle;
    Hbox := ALDrawCalendarGetTextSizeW(Canvas,'^_').cy + DayHeaderStyle.PaddingBottom + DayHeaderStyle.PaddingTop;
    wbox := ALDrawCalendarGetTextSizeW(Canvas,'__').cx + DayHeaderStyle.PaddingLeft   + DayHeaderStyle.PaddingRight;
    Canvas.Font.Color := MonthHeaderStyle.FontColor;
    Canvas.Font.Name := MonthHeaderStyle.FontName;
    Canvas.Font.Height := MonthHeaderStyle.FontHeight;
    Canvas.Font.Style := MonthHeaderStyle.FontStyle;
    Hheader1 := ALDrawCalendarGetTextSizeW(Canvas,'^_').cy + MonthHeaderStyle.PaddingBottom + MonthHeaderStyle.PaddingTop;
    Hheader2 := Hbox;
    W1Calendar := (CalendarWidth - 4) div 3; // the number 4 is the width of the header1 separator (their is 2 separators of 2 pixels each)
    Wblank := ((W1Calendar - (7*wbox)) div 2);
    if wblank <> MonthBoxPaddingLeftAndRight then begin
      Result := False;
      CalendarWidth := 3*7*wbox + MonthBoxPaddingLeftAndRight*6 + 4;
    end;
    ALCalculateDayBoxRect(Year,
                          Wblank,
                          Hheader1+hheader2+1,
                          Wbox,
                          Hbox,
                          2,
                          w1calendar,
                          MonthBoxPaddingTop,
                          Days,
                          YHeader2,
                          YHeader3,
                          YHeader4,
                          CalcHCalendar);
    if CalcHCalendar <> CalendarHeight then begin
      Result := False;
      CalendarHeight := CalcHCalendar;
    end;
    if not result then exit;


    //draw the background
    Canvas.Brush.Color := BackGroundColor;
    canvas.FillRect(Rect(0,0,CalendarWidth, CalendarHeight));


    //Draw Month titles
    Canvas.Font.Color := MonthHeaderStyle.FontColor;
    Canvas.Font.Name := MonthHeaderStyle.FontName;
    Canvas.Font.Height := MonthHeaderStyle.FontHeight;
    Canvas.Font.Style := MonthHeaderStyle.FontStyle;
    R := Rect(0,0,CalendarWidth,Hheader1);
    ALDrawCalendarHeader(WideLongMonthNames[1],WideLongMonthNames[2],WideLongMonthNames[3],IntToStr(year), R, Canvas, W1Calendar, Hheader1, MonthHeaderStyle.BackGroundColor, MonthsHeaderBorderColorBottom, MonthsHeaderBorderColorLo, MonthsHeaderBorderColorHi);
    R := Rect(0,YHeader2,CalendarWidth,YHeader2+Hheader1);
    ALDrawCalendarHeader(WideLongMonthNames[4],WideLongMonthNames[5],WideLongMonthNames[6],IntToStr(year), R, Canvas, W1Calendar, Hheader1, MonthHeaderStyle.BackGroundColor, MonthsHeaderBorderColorBottom, MonthsHeaderBorderColorLo, MonthsHeaderBorderColorHi);
    R := Rect(0,YHeader3,CalendarWidth,YHeader3 + Hheader1);
    ALDrawCalendarHeader(WideLongMonthNames[7],WideLongMonthNames[8],WideLongMonthNames[9],IntToStr(year), R, Canvas, W1Calendar, Hheader1, MonthHeaderStyle.BackGroundColor, MonthsHeaderBorderColorBottom, MonthsHeaderBorderColorLo, MonthsHeaderBorderColorHi);
    R := Rect(0,YHeader4,CalendarWidth,YHeader4 + Hheader1);
    ALDrawCalendarHeader(WideLongMonthNames[10],WideLongMonthNames[11],WideLongMonthNames[12],IntToStr(year), R, Canvas, W1Calendar, Hheader1, MonthHeaderStyle.BackGroundColor, MonthsHeaderBorderColorBottom, MonthsHeaderBorderColorLo, MonthsHeaderBorderColorHi);


    //Draw Day titles
    Canvas.Font.Color := DayHeaderStyle.FontColor;
    Canvas.Font.Name := DayHeaderStyle.FontName;
    Canvas.Font.Height := DayHeaderStyle.FontHeight;
    Canvas.Font.Style := DayHeaderStyle.FontStyle;
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank, hheader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color, DaysHeaderBorderColorBottom);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + W1Calendar + 2, hheader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color, DaysHeaderBorderColorBottom);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + 2*(W1Calendar + 2), hheader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color, DaysHeaderBorderColorBottom);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank, YHeader2 + hheader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color, DaysHeaderBorderColorBottom);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + W1Calendar + 2, YHeader2 + hheader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color, DaysHeaderBorderColorBottom);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + 2*(W1Calendar + 2), YHeader2 + hheader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color, DaysHeaderBorderColorBottom);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank, YHeader3 + hheader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color, DaysHeaderBorderColorBottom);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + W1Calendar + 2, YHeader3 + hheader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color, DaysHeaderBorderColorBottom);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + 2*(W1Calendar + 2), YHeader3 + hheader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color, DaysHeaderBorderColorBottom);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank, YHeader4 + hheader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color, DaysHeaderBorderColorBottom);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + W1Calendar + 2, YHeader4 + hheader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color, DaysHeaderBorderColorBottom);
    ALDrawCalendarHeaderDay(WideLongDayNames[7],WideLongDayNames[1],WideLongDayNames[2],WideLongDayNames[3],WideLongDayNames[4],WideLongDayNames[5],WideLongDayNames[6], Wblank + 2*(W1Calendar + 2), YHeader4 + hheader1, wbox, hbox, Canvas, DayHeaderStyle.BackGroundColor, Canvas.Font.Color, DaysHeaderBorderColorBottom);


    //Draw Days
    Day := encodeDate(year,01,01);
    For i := low(Days) to high(Days) do begin
      if DayOfTheMonth(Day) = 1 then begin
        GrayDay := Day - 1;
        R := Days[i].Rect;
        r.Left := r.Left - Wbox;
        R.Right := r.Right - wbox;
        Canvas.Font.Color := DayDisabledStyle.FontColor;
        Canvas.Font.Name := DayDisabledStyle.FontName;
        Canvas.Font.Height := DayDisabledStyle.FontHeight;
        Canvas.Font.Style := DayDisabledStyle.FontStyle;
        while DayOfTheWeek(GrayDay) <> DayFriday do begin
          ALDrawCalendarBox(IntToStr(DayOfTheMonth(GrayDay)),
                            DayDisabledStyle.BackGroundColor,
                            DayDisabledStyle.FontColor,
                            R,
                            Canvas);
          r.Left := r.Left - Wbox;
          R.Right := r.Right - wbox;
          GrayDay := GrayDay - 1;
        end;
      end;
      Canvas.Font.Color := Days[i].Style.FontColor;
      Canvas.Font.Name := Days[i].Style.FontName;
      Canvas.Font.Height := Days[i].Style.FontHeight;
      Canvas.Font.Style := Days[i].Style.FontStyle;
      ALDrawCalendarBox(IntToStr(DayOfTheMonth(Day)),
                        Days[i].Style.BackGroundColor,
                        Days[i].Style.FontColor,
                        Days[i].Rect,
                        Canvas);
      Day := Day + 1;
      if DayOfTheMonth(Day) = 1 then begin
        GrayDay := Day;
        R := Days[i].Rect;
        r.Left := r.Left + Wbox;
        R.Right := r.Right + wbox;
        Canvas.Font.Color := DayDisabledStyle.FontColor;
        Canvas.Font.Name := DayDisabledStyle.FontName;
        Canvas.Font.Height := DayDisabledStyle.FontHeight;
        Canvas.Font.Style := DayDisabledStyle.FontStyle;
        while DayOfTheWeek(GrayDay) <> DaySaturday do begin
          ALDrawCalendarBox(IntToStr(DayOfTheMonth(GrayDay)),
                            DayDisabledStyle.BackGroundColor,
                            DayDisabledStyle.FontColor,
                            R,
                            Canvas);
          r.Left := r.Left + Wbox;
          R.Right := r.Right + wbox;
          GrayDay := GrayDay + 1;
        end;
      end;
    end;

end;

end.
