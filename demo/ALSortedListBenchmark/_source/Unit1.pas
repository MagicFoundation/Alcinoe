unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, OleCtrls, SHDocVw, ComObj;

type
  TForm1 = class(TForm)
    ALButton1: TButton;
    Panel2: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Panel3: TPanel;
    PanelWebBrowser: TPanel;
    ALButton2: TButton;
    ALButton5: TButton;
    ALButton6: TButton;
    ALButton10: TButton;
    ALButton11: TButton;
    ALButton33: TButton;
    ALButton34: TButton;
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ALButton3Click(Sender: TObject);
    procedure ALButton4Click(Sender: TObject);
    procedure ALButton5Click(Sender: TObject);
    procedure ALButton6Click(Sender: TObject);
    procedure ALButton11Click(Sender: TObject);
    procedure ALButton10Click(Sender: TObject);
    procedure ALButton33Click(Sender: TObject);
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
    StartDate: DWORD;
    MemoryUsage: DWORD;
    i: integer;
begin
  aStringKeyAVLBinaryTree := TALStringKeyAVLBinaryTree.create;
  try

    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    StartDate := GetTickCount;
    for I := 1 to 1000000 do begin
      aStringKeyAVLBinaryTreeNode := TALStringKeyAVLBinaryTreeNode.Create;
      aStringKeyAVLBinaryTreeNode.ID := AlRandomStr(10);
      if not aStringKeyAVLBinaryTree.AddNode(aStringKeyAVLBinaryTreeNode) then aStringKeyAVLBinaryTreeNode.Free;
    end;

    Showmessage('Add 1,000,000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms - Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');

    StartDate := GetTickCount;
    for I := 1 to 100000 do begin
      aStringKeyAVLBinaryTree.FindNode(AlRandomStr(10));
    end;

    Showmessage('Search 100,000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms (indexed search)');

  finally
    aStringKeyAVLBinaryTree.free;
  end;
end;

{***********************************************}
procedure TForm1.ALButton4Click(Sender: TObject);
Var aLst: TStringList;
    StartDate: DWORD;
    MemoryUsage: DWORD;
    i: integer;
begin
  aLst := TstringList.create;
  try
    aLst.Sorted := False;

    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    StartDate := GetTickCount;
    for I := 1 to 1000000 do
      aLst.Add(AlRandomStrU(10));
    Showmessage('Add 1,000,000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms - Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');

    StartDate := GetTickCount;
    for I := 1 to 10 do begin
      aLst.IndexOf(AlRandomStrU(10));
    end;
    Showmessage('Search 10 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms (full scan search)');

    StartDate := GetTickCount;
    aLst.Duplicates := DupIgnore;
    aLst.Sorted := True;
    Showmessage('Sort 1,000,000 nodes nodes in ' + inttostr(GetTickCount - StartDate) + ' ms');

    StartDate := GetTickCount;
    for I := 1 to 100000 do begin
      aLst.IndexOf(AlRandomStrU(10));
    end;
    Showmessage('Search 100,000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms (indexed search)');


  finally
    aLst.free;
  end;
end;

{***********************************************}
procedure TForm1.ALButton5Click(Sender: TObject);
Var aLst: TALInt64AVLList;
    StartDate: DWORD;
    MemoryUsage: DWORD;
    i: integer;
begin
  aLst := TALInt64AVLList.Create;
  try

    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    StartDate := GetTickCount;
    aLst.Duplicates := DupIgnore;
    for I := 1 to 5000000 do
      aLst.Add(Random(MaxInt) * Random(MaxInt));
    Showmessage('Add 5,000,000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms - Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');

    StartDate := GetTickCount;
    for I := 1 to 100000 do begin
      aLst.IndexOf(Random(MaxInt) * Random(MaxInt));
    end;
    Showmessage('Search 100,000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms (indexed search)');

  finally
    aLst.Free;
  end;

end;

{***********************************************}
procedure TForm1.ALButton6Click(Sender: TObject);
Var aLst: TALInt64List;
    StartDate: DWORD;
    MemoryUsage: DWORD;
    i: integer;
begin
  aLst := TALInt64List.Create;
  try

    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    StartDate := GetTickCount;
    for I := 1 to 5000000 do
      aLst.Add(Random(MaxInt) * Random(MaxInt));
    Showmessage('Add 5,000,000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms - Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');

    StartDate := GetTickCount;
    for I := 1 to 10 do begin
      aLst.IndexOf(Random(MaxInt) * Random(MaxInt));
    end;
    Showmessage('Search 10 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms (full scan search)');

    StartDate := GetTickCount;
    aLst.sorted := True;
    Showmessage('Sort 5,000,000 nodes nodes in ' + inttostr(GetTickCount - StartDate) + ' ms');

    StartDate := GetTickCount;
    for I := 1 to 100000 do begin
      aLst.IndexOf(Random(MaxInt) * Random(MaxInt));
    end;
    Showmessage('Search 100,000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms (indexed search)');

  finally
    aLst.Free;
  end;

end;

{************************************************}
procedure TForm1.ALButton10Click(Sender: TObject);
Var aLst: TALStringList;
    StartDate: DWORD;
    MemoryUsage: DWORD;
    i: integer;
begin
  aLst := TALStringList.create;
  try
    aLst.Sorted := False;

    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    StartDate := GetTickCount;
    for I := 1 to 1000000 do
      aLst.Add(AlRandomStr(10) + '=' + AlRandomStr(10));
    Showmessage('Add 1,000,000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms - Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');

    StartDate := GetTickCount;
    for I := 1 to 10 do begin
      aLst.IndexOf(AlRandomStr(10) + '=' + AlRandomStr(10));
    end;
    Showmessage('Search 10 nodes (via IndexOf) in ' + inttostr(GetTickCount - StartDate) + ' ms (full scan search)');

    StartDate := GetTickCount;
    for I := 1 to 10 do begin
      aLst.IndexOfName(AlRandomStr(10));
    end;
    Showmessage('Search 10 nodes (via IndexOfName) in ' + inttostr(GetTickCount - StartDate) + ' ms (full scan search)');

    StartDate := GetTickCount;
    aLst.Duplicates := DupIgnore;
    aLst.Sorted := True;
    Showmessage('Sort 1,000,000 nodes nodes in ' + inttostr(GetTickCount - StartDate) + ' ms');

    StartDate := GetTickCount;
    for I := 1 to 100000 do begin
      aLst.IndexOf(AlRandomStr(10));
    end;
    Showmessage('Search 100,000 (via IndexOf) nodes in ' + inttostr(GetTickCount - StartDate) + ' ms (indexed search)');

    StartDate := GetTickCount;
    for I := 1 to 100000 do begin
      aLst.IndexOfName(AlRandomStr(10));
    end;
    Showmessage('Search 100,000 nodes (via IndexOfName) in ' + inttostr(GetTickCount - StartDate) + ' ms (indexed search)');

  finally
    aLst.free;
  end;
end;

{************************************************}
procedure TForm1.ALButton11Click(Sender: TObject);
Var aLst: TStringList;
    StartDate: DWORD;
    MemoryUsage: DWORD;
    i: integer;
begin
  aLst := TstringList.create;
  try
    aLst.Sorted := False;

    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    StartDate := GetTickCount;
    for I := 1 to 1000000 do
      aLst.Add(AlRandomStrU(10) + '=' + AlRandomStrU(10));
    Showmessage('Add 1,000,000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms - Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');

    StartDate := GetTickCount;
    for I := 1 to 10 do begin
      aLst.IndexOf(AlRandomStrU(10) + '=' + AlRandomStrU(10));
    end;
    Showmessage('Search 10 nodes (via IndexOf) in ' + inttostr(GetTickCount - StartDate) + ' ms (full scan search)');

    StartDate := GetTickCount;
    for I := 1 to 10 do begin
      aLst.IndexOfName(AlRandomStrU(10));
    end;
    Showmessage('Search 10 nodes (via IndexOfName) in ' + inttostr(GetTickCount - StartDate) + ' ms (full scan search)');

    StartDate := GetTickCount;
    aLst.Duplicates := DupIgnore;
    aLst.Sorted := True;
    Showmessage('Sort 1,000,000 nodes nodes in ' + inttostr(GetTickCount - StartDate) + ' ms');

    StartDate := GetTickCount;
    for I := 1 to 100000 do begin
      aLst.IndexOf(AlRandomStrU(10));
    end;
    Showmessage('Search 100,000 (via IndexOf) nodes in ' + inttostr(GetTickCount - StartDate) + ' ms (indexed search)');

    StartDate := GetTickCount;
    for I := 1 to 10 do begin
      aLst.IndexOfName(AlRandomStrU(10));
    end;
    Showmessage('Search 10 nodes (via IndexOfName) in ' + inttostr(GetTickCount - StartDate) + ' ms (full scan search)');

  finally
    aLst.free;
  end;
end;

{************************************************}
procedure TForm1.ALButton33Click(Sender: TObject);
Var aLst: TALAVLStringList;
    StartDate: DWORD;
    MemoryUsage: DWORD;
    i: integer;
begin
  aLst := TALAVLStringList.create;
  try

    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    StartDate := GetTickCount;
    for I := 1 to 1000000 do
      aLst.Add(AlRandomStr(10) + '=' + AlRandomStr(10));
    Showmessage('Add 1,000,000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms - Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');

    StartDate := GetTickCount;
    for I := 1 to 100000 do begin
      aLst.IndexOf(AlRandomStr(10));
    end;
    Showmessage('Search 100,000 (via IndexOf) nodes in ' + inttostr(GetTickCount - StartDate) + ' ms (indexed search)');

    StartDate := GetTickCount;
    for I := 1 to 100000 do begin
      aLst.IndexOfName(AlRandomStr(10));
    end;
    Showmessage('Search 100,000 nodes (via IndexOfName) in ' + inttostr(GetTickCount - StartDate) + ' ms (indexed search)');

  finally
    aLst.free;
  end;
end;

{-------------------}
var ie: IWebBrowser2;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
var Url, Flags, TargetFrameName, PostData, Headers: OleVariant;
begin
  ie := CreateOleObject('InternetExplorer.Application') as IWebBrowser2;
  SetWindowLong(ie.hwnd, GWL_STYLE, GetWindowLong(ie.hwnd, GWL_STYLE) and not WS_BORDER and not WS_SIZEBOX and not WS_DLGFRAME );
  SetWindowPos(ie.hwnd, HWND_TOP, Left, Top, Width, Height, SWP_FRAMECHANGED);
  windows.setparent(ie.hwnd, PanelWebBrowser.handle);
  ie.Left := maxint; // don't understand why it's look impossible to setup the position
  ie.Top  := maxint; // don't understand why it's look impossible to setup the position
  ie.MenuBar := false;
  ie.AddressBar := false;
  ie.Resizable := false;
  ie.StatusBar := false;
  ie.ToolBar := 0;
  ie.Width := 100;
  ie.Height := 300;
  Url := 'http://static.arkadia.com/html/alcinoe_like.html';
  ie.Navigate2(Url,Flags,TargetFrameName,PostData,Headers);
  ie.Visible := true;
end;

{********************************************************************}
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    ie.quit;
  except
  end;
  sleep(500);
end;

{$IFDEF DEBUG}
initialization
  ReportMemoryleaksOnSHutdown := True;
  SetMultiByteConversionCodePage(CP_UTF8);
{$ENDIF}

end.
