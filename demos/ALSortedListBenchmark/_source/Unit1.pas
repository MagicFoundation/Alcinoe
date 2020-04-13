unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, Shellapi, System.Generics.Collections,
  system.Diagnostics,
  Vcl.ComCtrls, System.Generics.Defaults, VCLTee.TeEngine, VCLTee.Series,
  VCLTee.TeeProcs, VCLTee.Chart, Vcl.Samples.Spin, VclTee.TeeGDIPlus;

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

uses ALString,
     alQuickSortList,
     ALStringList,
     alavlBinaryTree;

{$R *.dfm}

{*********************************************}
procedure TForm1.Button4Click(Sender: TObject);

  procedure _DoALAVLStringListBench(Count: integer);
  Var aLst: TALAVLStringList;
      aStopWatch: TstopWatch;
      S1, S2: ansiString;
      i,j: integer;
  begin
    aLst := TALAVLStringList.create;
    try
      aLst.CaseSensitive := false;
      aLst.duplicates := DupIgnore;
      aStopWatch := TstopWatch.Create;

      //indexOF
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1));
        S1 := AlRandomStr(Random(30)+1);
        aStopWatch.start;
        aLst.IndexOf(S1);
        aStopWatch.Stop;
      end;
      chart1.Series[0].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'IndexOf');
      application.ProcessMessages;

      //indexOFName
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1) + '=' + AlRandomStr(50));
        S1 := AlRandomStr(Random(30)+1);
        aStopWatch.start;
        aLst.IndexOfName(S1);
        aStopWatch.Stop;
      end;
      chart1.Series[0].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'IndexOfName');
      application.ProcessMessages;

      //add
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1));
        S1 := AlRandomStr(Random(30)+1);
        aStopWatch.start;
        aLst.add(S1);
        aStopWatch.Stop;
      end;
      chart1.Series[0].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'Add');
      application.ProcessMessages;

      //Value['xxx'] := 'yyy';
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1) + '=' + AlRandomStr(50));
        S1 := AlRandomStr(Random(30)+1);
        S2 := AlRandomStr(50);
        aStopWatch.start;
        aLst.Values[S1] := S2;
        aStopWatch.Stop;
      end;
      chart1.Series[0].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'Value[xxx]:=yyy');
      application.ProcessMessages;

    finally
      aLst.Free;
    end;
  end;

  procedure _DoALHashedStringListBench(Count: integer);
  Var aLst: TALHashedStringList;
      aStopWatch: TstopWatch;
      S1, S2: ansiString;
      i,j: integer;
  begin
    aLst := TALHashedStringList.create;
    try
      aLst.CaseSensitive := false;
      aLst.duplicates := DupIgnore;
      aStopWatch := TstopWatch.Create;

      //indexOF
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1));
        S1 := AlRandomStr(Random(30)+1);
        aStopWatch.start;
        aLst.IndexOf(S1);
        aStopWatch.Stop;
      end;
      chart1.Series[1].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'IndexOf');
      application.ProcessMessages;

      //indexOFName
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1) + '=' + AlRandomStr(50));
        S1 := AlRandomStr(Random(30)+1);
        aStopWatch.start;
        aLst.IndexOfName(S1);
        aStopWatch.Stop;
      end;
      chart1.Series[1].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'IndexOfName');
      application.ProcessMessages;

      //add
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1));
        S1 := AlRandomStr(Random(30)+1);
        aStopWatch.start;
        aLst.add(S1);
        aStopWatch.Stop;
      end;
      chart1.Series[1].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'Add');
      application.ProcessMessages;

      //Value['xxx'] := 'yyy';
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1) + '=' + AlRandomStr(50));
        S1 := AlRandomStr(Random(30)+1);
        S2 := AlRandomStr(50);
        aStopWatch.start;
        aLst.Values[S1] := S2;
        aStopWatch.Stop;
      end;
      chart1.Series[1].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'Value[xxx]:=yyy');
      application.ProcessMessages;

    finally
      aLst.Free;
    end;
  end;

  procedure _DoALStringListBench(Count: integer);
  Var aLst: TALStringList;
      aStopWatch: TstopWatch;
      S1, S2: ansiString;
      i,j: integer;
  begin
    aLst := TALStringList.create;
    try
      aLst.CaseSensitive := false;
      aLst.duplicates := DupIgnore;
      aLst.sorted := true;
      aStopWatch := TstopWatch.Create;

      //indexOF
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1));
        S1 := AlRandomStr(Random(30)+1);
        aStopWatch.start;
        aLst.IndexOf(S1);
        aStopWatch.Stop;
      end;
      chart1.Series[2].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'IndexOf');
      application.ProcessMessages;

      //indexOFName
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1) + '=' + AlRandomStr(50));
        S1 := AlRandomStr(Random(30)+1);
        aStopWatch.start;
        aLst.IndexOfName(S1);
        aStopWatch.Stop;
      end;
      chart1.Series[2].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'IndexOfName');
      application.ProcessMessages;

      //add
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1));
        S1 := AlRandomStr(Random(30)+1);
        aStopWatch.start;
        aLst.add(S1);
        aStopWatch.Stop;
      end;
      chart1.Series[2].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'Add');
      application.ProcessMessages;

      //Value['xxx'] := 'yyy';
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1) + '=' + AlRandomStr(50));
        S1 := AlRandomStr(Random(30)+1);
        S2 := AlRandomStr(50);
        aStopWatch.start;
        aLst.Values[S1] := S2;
        aStopWatch.Stop;
      end;
      chart1.Series[2].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'Value[xxx]:=yyy');
      application.ProcessMessages;

    finally
      aLst.Free;
    end;
  end;

  procedure _DoALNvStringListBench(Count: integer);
  Var aLst: TALNvStringList;
      aStopWatch: TstopWatch;
      S1, S2: ansiString;
      i,j: integer;
  begin
    aLst := TALNvStringList.create;
    try
      aLst.CaseSensitive := false;
      aLst.duplicates := DupIgnore;
      aLst.sorted := true;
      aStopWatch := TstopWatch.Create;

      //indexOF
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1));
        S1 := AlRandomStr(Random(30)+1);
        aStopWatch.start;
        aLst.IndexOf(S1);
        aStopWatch.Stop;
      end;
      chart1.Series[5].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'IndexOf');
      application.ProcessMessages;

      //indexOFName
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1) + '=' + AlRandomStr(50));
        S1 := AlRandomStr(Random(30)+1);
        aStopWatch.start;
        aLst.IndexOfName(S1);
        aStopWatch.Stop;
      end;
      chart1.Series[5].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'IndexOfName');
      application.ProcessMessages;

      //add
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1));
        S1 := AlRandomStr(Random(30)+1);
        aStopWatch.start;
        aLst.add(S1);
        aStopWatch.Stop;
      end;
      chart1.Series[5].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'Add');
      application.ProcessMessages;

      //Value['xxx'] := 'yyy';
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStr(Random(30)+1) + '=' + AlRandomStr(50));
        S1 := AlRandomStr(Random(30)+1);
        S2 := AlRandomStr(50);
        aStopWatch.start;
        aLst.Values[S1] := S2;
        aStopWatch.Stop;
      end;
      chart1.Series[5].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'Value[xxx]:=yyy');
      application.ProcessMessages;

    finally
      aLst.Free;
    end;
  end;

  procedure _DoStringListBench(Count: integer);
  Var aLst: TStringList;
      aStopWatch: TstopWatch;
      S1, S2: String;
      i,j: integer;
  begin
    aLst := TStringList.create;
    try
      aLst.CaseSensitive := false;
      aLst.duplicates := DupIgnore;
      aLst.sorted := true;
      aStopWatch := TstopWatch.Create;

      //indexOF
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStrU(Random(30)+1));
        S1 := AlRandomStrU(Random(30)+1);
        aStopWatch.start;
        aLst.IndexOf(S1);
        aStopWatch.Stop;
      end;
      chart1.Series[3].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'IndexOf');
      application.ProcessMessages;

      //indexOFName
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStrU(Random(30)+1) + '=' + AlRandomStrU(50));
        S1 := AlRandomStrU(Random(30)+1);
        aStopWatch.start;
        aLst.IndexOfName(S1);
        aStopWatch.Stop;
      end;
      chart1.Series[3].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'IndexOfName');
      application.ProcessMessages;

      //add
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStrU(Random(30)+1));
        S1 := AlRandomStrU(Random(30)+1);
        aStopWatch.start;
        aLst.add(S1);
        aStopWatch.Stop;
      end;
      chart1.Series[3].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'Add');
      application.ProcessMessages;

      //Value['xxx'] := 'yyy';
      aLst.sorted := False;
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(AlRandomStrU(Random(30)+1) + '=' + AlRandomStrU(50));
        S1 := AlRandomStrU(Random(30)+1);
        S2 := AlRandomStrU(50);
        aStopWatch.start;
        aLst.Values[S1] := S2;
        aStopWatch.Stop;
      end;
      chart1.Series[3].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'Value[xxx]:=yyy');
      application.ProcessMessages;

    finally
      aLst.Free;
    end;
  end;

  procedure _DoALIntegerListBench(Count: integer);
  Var aLst: TALIntegerList;
      aStopWatch: TstopWatch;
      I1: integer;
      i,j: integer;
  begin
    aLst := TALIntegerList.create;
    try
      aLst.duplicates := DupIgnore;
      aLst.sorted := true;
      aStopWatch := TstopWatch.Create;

      //indexOF
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(Random(maxint));
        I1 := Random(maxint);
        aStopWatch.start;
        aLst.IndexOf(I1);
        aStopWatch.Stop;
      end;
      chart1.Series[4].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'IndexOf');
      application.ProcessMessages;

      //indexOFName
      chart1.Series[4].AddY(0, 'IndexOfName');

      //add
      aStopWatch.Reset;
      for I := 1 to 1000 do begin
        aLst.Clear;
        for j := 1 to Count do
          aLst.Add(Random(maxint));
        I1 := Random(maxint);
        aStopWatch.start;
        aLst.add(I1);
        aStopWatch.Stop;
      end;
      chart1.Series[4].AddY(aStopWatch.Elapsed.TotalMilliseconds, 'Add');
      application.ProcessMessages;

      //Value['xxx'] := 'yyy';
      chart1.Series[4].AddY(0, 'Value[xxx]:=yyy');

    finally
      aLst.Free;
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
