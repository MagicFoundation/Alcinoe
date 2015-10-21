unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  cxLabel, Shellapi, System.Generics.Collections, system.Diagnostics;

type
  TForm1 = class(TForm)
    ALButton1: TButton;
    Panel2: TPanel;
    ALButton2: TButton;
    ALButton5: TButton;
    ALButton6: TButton;
    ALButton10: TButton;
    ALButton11: TButton;
    ALButton33: TButton;
    ALButton34: TButton;
    cxLabel1: TcxLabel;
    cxLabel2: TcxLabel;
    cxWwwArkadiaComLabel: TcxLabel;
    cxLabel18: TcxLabel;
    cxLabel17: TcxLabel;
    Memo1: TMemo;
    Button1: TButton;
    procedure FormClick(Sender: TObject);
    procedure ALButton3Click(Sender: TObject);
    procedure ALButton4Click(Sender: TObject);
    procedure ALButton5Click(Sender: TObject);
    procedure ALButton6Click(Sender: TObject);
    procedure ALButton11Click(Sender: TObject);
    procedure ALButton10Click(Sender: TObject);
    procedure ALButton33Click(Sender: TObject);
    procedure cxWwwArkadiaComLabelClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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

{***********************************************}
procedure TForm1.ALButton3Click(Sender: TObject);
Var aStringKeyAVLBinaryTree: TALStringKeyAVLBinaryTree;
    aStringKeyAVLBinaryTreeNode: TALStringKeyAVLBinaryTreeNode;
    aStopWatch: TstopWatch;
    MemoryUsage: DWORD;
    i: integer;
begin
  IF Memo1.Lines.Count > 0 then Memo1.Lines.Add('');
  Memo1.Lines.Add('Benchmark TALStringKeyAVLBinaryTree');

  aStringKeyAVLBinaryTree := TALStringKeyAVLBinaryTree.create;
  try

    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    aStopWatch := TstopWatch.startNew;
    for I := 1 to 1000000 do begin
      aStringKeyAVLBinaryTreeNode := TALStringKeyAVLBinaryTreeNode.Create;
      aStringKeyAVLBinaryTreeNode.ID := AlRandomStr(10);
      if not aStringKeyAVLBinaryTree.AddNode(aStringKeyAVLBinaryTreeNode) then aStringKeyAVLBinaryTreeNode.Free;
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Add 1,000,000 nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms - Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 100000 do begin
      aStringKeyAVLBinaryTree.FindNode(AlRandomStr(10));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 100,000 nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (indexed search)');

  finally
    aStringKeyAVLBinaryTree.free;
  end;
  Memo1.Lines.Add('Finished');
end;

{***********************************************}
procedure TForm1.ALButton4Click(Sender: TObject);
Var aLst: TStringList;
    aStopWatch: TstopWatch;
    MemoryUsage: DWORD;
    i: integer;
begin
  IF Memo1.Lines.Count > 0 then Memo1.Lines.Add('');
  Memo1.Lines.Add('Benchmark TStringList');

  aLst := TstringList.create;
  try
    aLst.Sorted := False;

    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    aStopWatch := TstopWatch.startNew;
    for I := 1 to 1000000 do
      aLst.Add(AlRandomStrU(10));
    aStopWatch.Stop;
    Memo1.Lines.Add('Add 1,000,000 nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms - Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 10 do begin
      aLst.IndexOf(AlRandomStrU(10));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 10 nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (full scan search)');

    aStopWatch := TstopWatch.startNew;
    aLst.Duplicates := DupIgnore;
    aLst.Sorted := True;
    aStopWatch.Stop;
    Memo1.Lines.Add('Sort 1,000,000 nodes nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 100000 do begin
      aLst.IndexOf(AlRandomStrU(10));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 100,000 nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (indexed search)');


  finally
    aLst.free;
  end;
  Memo1.Lines.Add('Finished');
end;

{***********************************************}
procedure TForm1.ALButton5Click(Sender: TObject);
Var aLst: TALInt64AVLList;
    aStopWatch: TstopWatch;
    MemoryUsage: DWORD;
    i: integer;
begin
  IF Memo1.Lines.Count > 0 then Memo1.Lines.Add('');
  Memo1.Lines.Add('Benchmark TALInt64AVLList');

  aLst := TALInt64AVLList.Create;
  try

    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    aStopWatch := TstopWatch.startNew;
    aLst.Duplicates := DupIgnore;
    for I := 1 to 5000000 do
      aLst.Add(Random(MaxInt) * Random(MaxInt));
    aStopWatch.Stop;
    Memo1.Lines.Add('Add 5,000,000 nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms - Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 100000 do begin
      aLst.IndexOf(Random(MaxInt) * Random(MaxInt));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 100,000 nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (indexed search)');

  finally
    aLst.Free;
  end;
  Memo1.Lines.Add('Finished');
end;

{***********************************************}
procedure TForm1.ALButton6Click(Sender: TObject);
Var aLst: TALInt64List;
    aStopWatch: TstopWatch;
    MemoryUsage: DWORD;
    i: integer;
begin
  IF Memo1.Lines.Count > 0 then Memo1.Lines.Add('');
  Memo1.Lines.Add('Benchmark TALInt64List');

  aLst := TALInt64List.Create;
  try

    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    aStopWatch := TstopWatch.startNew;
    for I := 1 to 5000000 do
      aLst.Add(Random(MaxInt) * Random(MaxInt));
    aStopWatch.Stop;
    Memo1.Lines.Add('Add 5,000,000 nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms - Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 10 do begin
      aLst.IndexOf(Random(MaxInt) * Random(MaxInt));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 10 nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (full scan search)');

    aStopWatch := TstopWatch.startNew;
    aLst.sorted := True;
    aStopWatch.Stop;
    Memo1.Lines.Add('Sort 5,000,000 nodes nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 100000 do begin
      aLst.IndexOf(Random(MaxInt) * Random(MaxInt));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 100,000 nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (indexed search)');

  finally
    aLst.Free;
  end;
  Memo1.Lines.Add('Finished');
end;

{*********************************************}
procedure TForm1.Button1Click(Sender: TObject);
Var dict : TDictionary<ansiString, AnsiString>;
    S1: ansiString;
    aStopWatch: TstopWatch;
    MemoryUsage: DWORD;
    i: integer;
begin
  IF Memo1.Lines.Count > 0 then Memo1.Lines.Add('');
  Memo1.Lines.Add('Benchmark TALStringList');

  dict := TDictionary<ansiString, ansiString>.Create;
  try

    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    aStopWatch := TstopWatch.startNew;
    for I := 1 to 1000000 do
      dict.Add(AlRandomStr(10), AlRandomStr(10));
    aStopWatch.Stop;
    Memo1.Lines.Add('Add 1,000,000 nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms - Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 100000 do begin
      dict.TryGetValue(AlRandomStr(10),S1);
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 100,000 (via TryGetValue) nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (indexed search)');

  finally
    dict.free;
  end;
  Memo1.Lines.Add('Finished');
end;


{**********************************************************}
procedure TForm1.cxWwwArkadiaComLabelClick(Sender: TObject);
begin
  ShellExecute(Application.Handle,'open','http://www.arkadia.com',nil,nil, SW_SHOWNORMAL);
end;

{************************************************}
procedure TForm1.ALButton10Click(Sender: TObject);
Var aLst: TALStringList;
    aStopWatch: TstopWatch;
    MemoryUsage: DWORD;
    i: integer;
begin
  IF Memo1.Lines.Count > 0 then Memo1.Lines.Add('');
  Memo1.Lines.Add('Benchmark TALStringList');

  aLst := TALStringList.create;
  try
    aLst.Sorted := False;

    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    aStopWatch := TstopWatch.startNew;
    for I := 1 to 1000000 do
      aLst.Add(AlRandomStr(10) + '=' + AlRandomStr(10));
    aStopWatch.Stop;
    Memo1.Lines.Add('Add 1,000,000 nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms - Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 10 do begin
      aLst.IndexOf(AlRandomStr(10) + '=' + AlRandomStr(10));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 10 nodes (via IndexOf) in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (full scan search)');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 10 do begin
      aLst.IndexOfName(AlRandomStr(10));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 10 nodes (via IndexOfName) in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (full scan search)');

    aStopWatch := TstopWatch.startNew;
    aLst.Duplicates := DupIgnore;
    aLst.Sorted := True;
    aStopWatch.Stop;
    Memo1.Lines.Add('Sort 1,000,000 nodes nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 100000 do begin
      aLst.IndexOf(AlRandomStr(10));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 100,000 (via IndexOf) nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (indexed search)');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 100000 do begin
      aLst.IndexOfName(AlRandomStr(10));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 100,000 nodes (via IndexOfName) in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (indexed search)');

  finally
    aLst.free;
  end;
  Memo1.Lines.Add('Finished');
end;

{************************************************}
procedure TForm1.ALButton11Click(Sender: TObject);
Var aLst: TStringList;
    aStopWatch: TstopWatch;
    MemoryUsage: DWORD;
    i: integer;
begin
  IF Memo1.Lines.Count > 0 then Memo1.Lines.Add('');
  Memo1.Lines.Add('Benchmark TStringList');

  aLst := TstringList.create;
  try
    aLst.Sorted := False;

    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    aStopWatch := TstopWatch.startNew;
    for I := 1 to 1000000 do
      aLst.Add(AlRandomStrU(10) + '=' + AlRandomStrU(10));
    aStopWatch.Stop;
    Memo1.Lines.Add('Add 1,000,000 nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms - Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 10 do begin
      aLst.IndexOf(AlRandomStrU(10) + '=' + AlRandomStrU(10));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 10 nodes (via IndexOf) in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (full scan search)');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 10 do begin
      aLst.IndexOfName(AlRandomStrU(10));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 10 nodes (via IndexOfName) in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (full scan search)');

    aStopWatch := TstopWatch.startNew;
    aLst.Duplicates := DupIgnore;
    aLst.Sorted := True;
    aStopWatch.Stop;
    Memo1.Lines.Add('Sort 1,000,000 nodes nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 100000 do begin
      aLst.IndexOf(AlRandomStrU(10));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 100,000 (via IndexOf) nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (indexed search)');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 10 do begin
      aLst.IndexOfName(AlRandomStrU(10));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 10 nodes (via IndexOfName) in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (full scan search)');

  finally
    aLst.free;
  end;
  Memo1.Lines.Add('Finished');
end;

{************************************************}
procedure TForm1.ALButton33Click(Sender: TObject);
Var aLst: TALAVLStringList;
    aStopWatch: TstopWatch;
    MemoryUsage: DWORD;
    i: integer;
begin
  IF Memo1.Lines.Count > 0 then Memo1.Lines.Add('');
  Memo1.Lines.Add('Benchmark TALAVLStringList');

  aLst := TALAVLStringList.create;
  try

    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    aStopWatch := TstopWatch.startNew;
    for I := 1 to 1000000 do
      aLst.Add(AlRandomStr(10) + '=' + AlRandomStr(10));
    aStopWatch.Stop;
    Memo1.Lines.Add('Add 1,000,000 nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms - Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 100000 do begin
      aLst.IndexOf(AlRandomStr(10));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 100,000 (via IndexOf) nodes in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (indexed search)');

    aStopWatch := TstopWatch.startNew;
    for I := 1 to 100000 do begin
      aLst.IndexOfName(AlRandomStr(10));
    end;
    aStopWatch.Stop;
    Memo1.Lines.Add('Search 100,000 nodes (via IndexOfName) in ' + FormatFloat('0.00',aStopWatch.Elapsed.TotalMilliseconds) + ' ms (indexed search)');

  finally
    aLst.free;
  end;
  Memo1.Lines.Add('Finished');
end;

{$IFDEF DEBUG}
initialization
  ReportMemoryleaksOnSHutdown := True;
  SetMultiByteConversionCodePage(CP_UTF8);
{$ENDIF}

end.
