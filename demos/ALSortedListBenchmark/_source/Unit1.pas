unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  cxLabel, Shellapi, System.Generics.Collections, system.Diagnostics,
  Vcl.ComCtrls, System.Generics.Defaults, VCLTee.TeEngine, VCLTee.Series,
  VCLTee.TeeProcs, VCLTee.Chart;

type
  TForm1 = class(TForm)
    Panel2: TPanel;
    ALButton10: TButton;
    ALButton11: TButton;
    ALButton33: TButton;
    cxLabel1: TcxLabel;
    cxLabel2: TcxLabel;
    cxWwwArkadiaComLabel: TcxLabel;
    cxLabel18: TcxLabel;
    cxLabel17: TcxLabel;
    Button2: TButton;
    Chart1: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    Button1: TButton;
    Series3: TLineSeries;
    Series4: TLineSeries;
    Series5: TLineSeries;
    Series6: TLineSeries;
    Button3: TButton;
    Label1: TLabel;
    procedure FormClick(Sender: TObject);
    procedure ALButton11Click(Sender: TObject);
    procedure ALButton10Click(Sender: TObject);
    procedure ALButton33Click(Sender: TObject);
    procedure cxWwwArkadiaComLabelClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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

type

  {**************************************************}
  PPROCESS_MEMORY_COUNTERS = ^PROCESS_MEMORY_COUNTERS;
  PROCESS_MEMORY_COUNTERS = record
    cb : DWORD;
    PageFaultCount : DWORD;
    PeakWorkingSetSize : DWORD;
    WorkingSetSize : DWORD; //Task managers MemUsage number
    QuotaPeakPagedPoolUsage : DWORD;
    QuotaPagedPoolUsage : DWORD;
    QuotaPeakNonPagedPoolUsage : DWORD;
    QuotaNonPagedPoolUsage : DWORD;
    PagefileUsage : DWORD; //TaskMan's VM Size number
    PeakPagefileUsage : DWORD;
  end;
  TProcessMemoryCounters = PROCESS_MEMORY_COUNTERS;

{**********************************************************************************************************************************************}
function GetProcessMemoryInfo(Process : THandle; var MemoryCounters : TProcessMemoryCounters; cb : DWORD) : BOOL; stdcall; external 'psapi.dll';

{****************************************************}
function ProcessMemoryUsage(ProcessID : DWORD): DWORD;
var ProcessHandle : THandle;
    MemCounters   : TProcessMemoryCounters;
begin
  Result := 0;
  ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
                               false,
                               ProcessID);
  try
    if GetProcessMemoryInfo(ProcessHandle,
                            MemCounters,
                            sizeof(MemCounters))
    then Result := MemCounters.WorkingSetSize;
  finally
    CloseHandle(ProcessHandle);
  end;
end;

{******************************************}
procedure TForm1.FormClick(Sender: TObject);
begin
  Windows.SetFocus(0);
end;

{*********************************************}
procedure TForm1.Button1Click(Sender: TObject);

    procedure _DoBench(Count: integer);
    Var aLst: TALStringList;
        aStopWatch: TstopWatch;
        i,j: integer;
    begin
      aLst := TALStringList.create;
      try
        aLst.CaseSensitive := false;
        aLst.duplicates := DupIgnore;
        aStopWatch := TstopWatch.Create;
        for I := 1 to 1000 do begin
          aLst.Clear;
          for j := 1 to Count do
            aLst.Add(AlRandomStr(Random(30)+1) + '=' + AlRandomStr(50));
          aStopWatch.start;
          aLst.Values[AlRandomStr(Random(30)+1)];
          aStopWatch.Stop;
        end;
        chart1.Series[2].Addxy(Count, aStopWatch.Elapsed.TotalMilliseconds , '', clpurple);
        application.ProcessMessages;
      finally
        aLst.Free;
      end;
    end;

Var i: integer;

begin

chart1.Series[2].Clear;
for I := 1 to 20 do
  _DoBench(i);

end;

procedure TForm1.Button2Click(Sender: TObject);

    procedure _DoBench(Count: integer);
    Var aLst: TALHashedStringList;
        aStopWatch: TstopWatch;
        i,j: integer;
    begin
      aLst := TALHashedStringList.create;
      try
        aLst.CaseSensitive := false;
        aLst.duplicates := DupIgnore;
        aStopWatch := TstopWatch.Create;
        for I := 1 to 1000 do begin
          aLst.Clear;
          for j := 1 to Count do
            aLst.Add(AlRandomStr(Random(30)+1) + '=' + AlRandomStr(50));
          aStopWatch.start;
          aLst.Values[AlRandomStr(Random(30)+1)];
          aStopWatch.Stop;
        end;
        chart1.Series[0].Addxy(Count, aStopWatch.Elapsed.TotalMilliseconds , '', clred);
        application.ProcessMessages;
      finally
        aLst.Free;
      end;
    end;

Var i: integer;

begin

chart1.Series[0].Clear;
for I := 1 to 50 do
  _DoBench(i);
end;


procedure TForm1.Button3Click(Sender: TObject);

    procedure _DoBench(Count: integer);
    Var aLst: TStringList;
        aStopWatch: TstopWatch;
        i,j: integer;
    begin
      aLst := TStringList.create;
      try
        aLst.CaseSensitive := false;
        aLst.duplicates := DupIgnore;
        aStopWatch := TstopWatch.Create;
        for I := 1 to 1000 do begin
          aLst.Clear;
          for j := 1 to Count do
            aLst.Add(ALRandomStrU(Random(30)+1) + '=' + ALRandomStrU(50));
          aStopWatch.start;
          aLst.Values[ALRandomStrU(Random(30)+1)];
          aStopWatch.Stop;
        end;
        chart1.Series[5].Addxy(Count, aStopWatch.Elapsed.TotalMilliseconds , '', clGray);
        application.ProcessMessages;
      finally
        aLst.Free;
      end;
    end;

Var i: integer;

begin

chart1.Series[5].Clear;
for I := 1 to 20 do
  _DoBench(i);

end;

{**********************************************************}
procedure TForm1.cxWwwArkadiaComLabelClick(Sender: TObject);
begin
  ShellExecute(Application.Handle,'open','http://www.arkadia.com',nil,nil, SW_SHOWNORMAL);
end;

{************************************************}
procedure TForm1.ALButton10Click(Sender: TObject);

    procedure _DoBench(Count: integer);
    Var aLst: TALStringList;
        aStopWatch: TstopWatch;
        i,j: integer;
    begin
      aLst := TALStringList.create;
      try
        aLst.CaseSensitive := false;
        aLst.duplicates := DupIgnore;
        aStopWatch := TstopWatch.Create;
        for I := 1 to 1000 do begin
          aLst.Clear;
          for j := 1 to Count do
            aLst.Add(AlRandomStr(Random(30)+1) + '=' + AlRandomStr(50));
          aLst.Sorted := True;
          aStopWatch.start;
          aLst.Values[AlRandomStr(Random(30)+1)];
          aStopWatch.Stop;
        end;
        chart1.Series[1].Addxy(Count, aStopWatch.Elapsed.TotalMilliseconds , '', clgreen);
        application.ProcessMessages;
      finally
        aLst.Free;
      end;
    end;

Var i: integer;

begin

chart1.Series[1].Clear;
for I := 1 to 20 do
  _DoBench(i);

end;

{************************************************}
procedure TForm1.ALButton11Click(Sender: TObject);

    procedure _DoBench(Count: integer);
    Var aLst: TStringList;
        aStopWatch: TstopWatch;
        i,j: integer;
    begin
      aLst := TStringList.create;
      try
        aLst.CaseSensitive := false;
        aLst.duplicates := DupIgnore;
        aStopWatch := TstopWatch.Create;
        for I := 1 to 1000 do begin
          aLst.Clear;
          for j := 1 to Count do
            aLst.Add(string(AlRandomStr(Random(30)+1)) + '=' + string(AlRandomStr(50)));
          aLst.Sorted := true;
          aStopWatch.start;
          aLst.Values[string(AlRandomStr(Random(30)+1))];
          aStopWatch.Stop;
        end;
        chart1.Series[4].Addxy(Count, aStopWatch.Elapsed.TotalMilliseconds , '', clMaroon);
        application.ProcessMessages;
      finally
        aLst.Free;
      end;
    end;

Var i: integer;

begin

chart1.Series[4].Clear;
for I := 1 to 20 do
  _DoBench(i);

end;

{************************************************}
procedure TForm1.ALButton33Click(Sender: TObject);

    procedure _DoBench(Count: integer);
    Var aLst: TALAVLStringList;
        aStopWatch: TstopWatch;
        i,j: integer;
    begin
      aLst := TALAVLStringList.create;
      try
        aLst.CaseSensitive := false;
        aLst.duplicates := DupIgnore;
        aStopWatch := TstopWatch.Create;
        for I := 1 to 1000 do begin
          aLst.Clear;
          for j := 1 to Count do
            aLst.Add(AlRandomStr(Random(30)+1) + '=' + AlRandomStr(50));
          aStopWatch.start;
          aLst.Values[AlRandomStr(Random(30)+1)];
          aStopWatch.Stop;
        end;
        chart1.Series[3].Addxy(Count, aStopWatch.Elapsed.TotalMilliseconds , '', clBlack);
        application.ProcessMessages;
      finally
        aLst.Free;
      end;
    end;

Var i: integer;

begin

chart1.Series[3].Clear;
for I := 1 to 50 do
  _DoBench(i);

end;

{$IFDEF DEBUG}
initialization
  ReportMemoryleaksOnSHutdown := True;
  SetMultiByteConversionCodePage(CP_UTF8);
{$ENDIF}

end.
