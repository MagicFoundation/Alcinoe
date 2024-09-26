unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, Shellapi, System.Generics.Collections,
  system.Diagnostics, Vcl.ComCtrls, System.Generics.Defaults, VCLTee.TeEngine,
  VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart, Vcl.Samples.Spin, VclTee.TeeGDIPlus;

type
  TForm1 = class(TForm)
    Chart1: TChart;
    Button4: TButton;
    Series1: TBarSeries;
    Series2: TBarSeries;
    Series3: TBarSeries;
    Series4: TBarSeries;
    Series5: TBarSeries;
    SpinEditNbItems: TSpinEdit;
    Label2: TLabel;
    CheckBoxALAVLStringList: TCheckBox;
    CheckBoxALHashedStringList: TCheckBox;
    CheckBoxALStringList: TCheckBox;
    CheckBoxStringList: TCheckBox;
    CheckBoxALIntegerList: TCheckBox;
    CheckBoxALNvStringList: TCheckBox;
    Series6: TBarSeries;
    procedure Button4Click(Sender: TObject);
  private
  public
  end;

var Form1: TForm1;

implementation

uses
  Alcinoe.StringUtils,
  Alcinoe.QuickSortList,
  Alcinoe.StringList,
  Alcinoe.AVLBinaryTree;

{$R *.dfm}

{*********************************************}
procedure TForm1.Button4Click(Sender: TObject);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoALAVLStringListBench(Count: integer);
  Var LLst: TALAVLStringListA;
      LStopWatch: TstopWatch;
      S1, S2: ansiString;
      I,J: integer;
  begin
    LLst := TALAVLStringListA.create;
    try
      LLst.CaseSensitive := false;
      LLst.duplicates := DupIgnore;
      LStopWatch := TstopWatch.Create;

      //indexOF
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1));
        S1 := ALRandomStrA(Random(30)+1);
        LStopWatch.start;
        LLst.IndexOf(S1);
        LStopWatch.Stop;
      end;
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'IndexOf');
      application.ProcessMessages;

      //indexOFName
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1) + '=' + ALRandomStrA(50));
        S1 := ALRandomStrA(Random(30)+1);
        LStopWatch.start;
        LLst.IndexOfName(S1);
        LStopWatch.Stop;
      end;
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'IndexOfName');
      application.ProcessMessages;

      //add
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1));
        S1 := ALRandomStrA(Random(30)+1);
        LStopWatch.start;
        LLst.add(S1);
        LStopWatch.Stop;
      end;
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Add');
      application.ProcessMessages;

      //Value['xxx'] := 'yyy';
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1) + '=' + ALRandomStrA(50));
        S1 := ALRandomStrA(Random(30)+1);
        S2 := ALRandomStrA(50);
        LStopWatch.start;
        LLst.Values[S1] := S2;
        LStopWatch.Stop;
      end;
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Value[xxx]:=yyy');
      application.ProcessMessages;

    finally
      LLst.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoALHashedStringListBench(Count: integer);
  Var LLst: TALHashedStringListA;
      LStopWatch: TstopWatch;
      S1, S2: ansiString;
      I,J: integer;
  begin
    LLst := TALHashedStringListA.create;
    try
      LLst.CaseSensitive := false;
      LLst.duplicates := DupIgnore;
      LStopWatch := TstopWatch.Create;

      //indexOF
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1));
        S1 := ALRandomStrA(Random(30)+1);
        LStopWatch.start;
        LLst.IndexOf(S1);
        LStopWatch.Stop;
      end;
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'IndexOf');
      application.ProcessMessages;

      //indexOFName
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1) + '=' + ALRandomStrA(50));
        S1 := ALRandomStrA(Random(30)+1);
        LStopWatch.start;
        LLst.IndexOfName(S1);
        LStopWatch.Stop;
      end;
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'IndexOfName');
      application.ProcessMessages;

      //add
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1));
        S1 := ALRandomStrA(Random(30)+1);
        LStopWatch.start;
        LLst.add(S1);
        LStopWatch.Stop;
      end;
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Add');
      application.ProcessMessages;

      //Value['xxx'] := 'yyy';
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1) + '=' + ALRandomStrA(50));
        S1 := ALRandomStrA(Random(30)+1);
        S2 := ALRandomStrA(50);
        LStopWatch.start;
        LLst.Values[S1] := S2;
        LStopWatch.Stop;
      end;
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Value[xxx]:=yyy');
      application.ProcessMessages;

    finally
      LLst.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoALStringListBench(Count: integer);
  Var LLst: TALStringListA;
      LStopWatch: TstopWatch;
      S1, S2: ansiString;
      I,J: integer;
  begin
    LLst := TALStringListA.create;
    try
      LLst.CaseSensitive := false;
      LLst.duplicates := DupIgnore;
      LLst.sorted := true;
      LStopWatch := TstopWatch.Create;

      //indexOF
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1));
        S1 := ALRandomStrA(Random(30)+1);
        LStopWatch.start;
        LLst.IndexOf(S1);
        LStopWatch.Stop;
      end;
      chart1.Series[2].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'IndexOf');
      application.ProcessMessages;

      //indexOFName
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1) + '=' + ALRandomStrA(50));
        S1 := ALRandomStrA(Random(30)+1);
        LStopWatch.start;
        LLst.IndexOfName(S1);
        LStopWatch.Stop;
      end;
      chart1.Series[2].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'IndexOfName');
      application.ProcessMessages;

      //add
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1));
        S1 := ALRandomStrA(Random(30)+1);
        LStopWatch.start;
        LLst.add(S1);
        LStopWatch.Stop;
      end;
      chart1.Series[2].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Add');
      application.ProcessMessages;

      //Value['xxx'] := 'yyy';
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1) + '=' + ALRandomStrA(50));
        S1 := ALRandomStrA(Random(30)+1);
        S2 := ALRandomStrA(50);
        LStopWatch.start;
        LLst.Values[S1] := S2;
        LStopWatch.Stop;
      end;
      chart1.Series[2].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Value[xxx]:=yyy');
      application.ProcessMessages;

    finally
      LLst.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoALNvStringListBench(Count: integer);
  Var LLst: TALNVStringListA;
      LStopWatch: TstopWatch;
      S1, S2: ansiString;
      I,J: integer;
  begin
    LLst := TALNVStringListA.create;
    try
      LLst.CaseSensitive := false;
      LLst.duplicates := DupIgnore;
      LLst.sorted := true;
      LStopWatch := TstopWatch.Create;

      //indexOF
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1));
        S1 := ALRandomStrA(Random(30)+1);
        LStopWatch.start;
        LLst.IndexOf(S1);
        LStopWatch.Stop;
      end;
      chart1.Series[5].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'IndexOf');
      application.ProcessMessages;

      //indexOFName
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1) + '=' + ALRandomStrA(50));
        S1 := ALRandomStrA(Random(30)+1);
        LStopWatch.start;
        LLst.IndexOfName(S1);
        LStopWatch.Stop;
      end;
      chart1.Series[5].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'IndexOfName');
      application.ProcessMessages;

      //add
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1));
        S1 := ALRandomStrA(Random(30)+1);
        LStopWatch.start;
        LLst.add(S1);
        LStopWatch.Stop;
      end;
      chart1.Series[5].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Add');
      application.ProcessMessages;

      //Value['xxx'] := 'yyy';
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrA(Random(30)+1) + '=' + ALRandomStrA(50));
        S1 := ALRandomStrA(Random(30)+1);
        S2 := ALRandomStrA(50);
        LStopWatch.start;
        LLst.Values[S1] := S2;
        LStopWatch.Stop;
      end;
      chart1.Series[5].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Value[xxx]:=yyy');
      application.ProcessMessages;

    finally
      LLst.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoStringListBench(Count: integer);
  Var LLst: TStringList;
      LStopWatch: TstopWatch;
      S1, S2: String;
      I,J: integer;
  begin
    LLst := TStringList.create;
    try
      LLst.CaseSensitive := false;
      LLst.duplicates := DupIgnore;
      LLst.sorted := true;
      LStopWatch := TstopWatch.Create;

      //indexOF
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrW(Random(30)+1));
        S1 := ALRandomStrW(Random(30)+1);
        LStopWatch.start;
        LLst.IndexOf(S1);
        LStopWatch.Stop;
      end;
      chart1.Series[3].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'IndexOf');
      application.ProcessMessages;

      //indexOFName
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrW(Random(30)+1) + '=' + ALRandomStrW(50));
        S1 := ALRandomStrW(Random(30)+1);
        LStopWatch.start;
        LLst.IndexOfName(S1);
        LStopWatch.Stop;
      end;
      chart1.Series[3].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'IndexOfName');
      application.ProcessMessages;

      //add
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrW(Random(30)+1));
        S1 := ALRandomStrW(Random(30)+1);
        LStopWatch.start;
        LLst.add(S1);
        LStopWatch.Stop;
      end;
      chart1.Series[3].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Add');
      application.ProcessMessages;

      //Value['xxx'] := 'yyy';
      LLst.sorted := False;
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(ALRandomStrW(Random(30)+1) + '=' + ALRandomStrW(50));
        S1 := ALRandomStrW(Random(30)+1);
        S2 := ALRandomStrW(50);
        LStopWatch.start;
        LLst.Values[S1] := S2;
        LStopWatch.Stop;
      end;
      chart1.Series[3].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Value[xxx]:=yyy');
      application.ProcessMessages;

    finally
      LLst.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoALIntegerListBench(Count: integer);
  Var LLst: TALIntegerList;
      LStopWatch: TstopWatch;
      I1: integer;
      I,J: integer;
  begin
    LLst := TALIntegerList.create;
    try
      LLst.duplicates := DupIgnore;
      LLst.sorted := true;
      LStopWatch := TstopWatch.Create;

      //indexOF
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(Random(maxint));
        I1 := Random(maxint);
        LStopWatch.start;
        LLst.IndexOf(I1);
        LStopWatch.Stop;
      end;
      chart1.Series[4].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'IndexOf');
      application.ProcessMessages;

      //indexOFName
      chart1.Series[4].AddY(0, 'IndexOfName');

      //add
      LStopWatch.Reset;
      for I := 1 to 1000 do begin
        LLst.Clear;
        for J := 1 to Count do
          LLst.Add(Random(maxint));
        I1 := Random(maxint);
        LStopWatch.start;
        LLst.add(I1);
        LStopWatch.Stop;
      end;
      chart1.Series[4].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Add');
      application.ProcessMessages;

      //Value['xxx'] := 'yyy';
      chart1.Series[4].AddY(0, 'Value[xxx]:=yyy');

    finally
      LLst.Free;
    end;
  end;

begin

  chart1.Series[0].Clear;
  chart1.Series[1].Clear;
  chart1.Series[2].Clear;
  chart1.Series[3].Clear;
  chart1.Series[4].Clear;
  chart1.Series[5].Clear;
  if CheckBoxALAVLStringList.Checked then _DoALAVLStringListBench(StrToInt(SpinEditNbItems.Text));
  if CheckBoxALHashedStringList.Checked then _DoALHashedStringListBench(StrToInt(SpinEditNbItems.Text));
  if CheckBoxALStringList.Checked then _DoALStringListBench(StrToInt(SpinEditNbItems.Text));
  if CheckBoxStringList.Checked then _DoStringListBench(StrToInt(SpinEditNbItems.Text));
  if CheckBoxALIntegerList.Checked then _DoALIntegerListBench(StrToInt(SpinEditNbItems.Text));
  if CheckBoxALNvStringList.Checked then _DoALNvStringListBench(StrToInt(SpinEditNbItems.Text));

end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
