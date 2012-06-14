unit Unit1;

interface

uses Windows,
     Messages,
     SysUtils,
     Variants,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     Contnrs,
     StdCtrls,
     xmldom,
     XMLIntf,
     msxmldom,
     XMLDoc,
     AlFcnString,
     AlXmlDoc,
     ExtCtrls,
     OleCtrls,
     SHDocVw,
     ComObj;

{------------------------------------}
Const WM_XmlFullyLoaded = WM_user + 1;

type

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

  TForm1 = class(TForm)
    ButtonLoadXmlWithALXmlDocument: TButton;
    ButtonLoadXmlWithXmlDocument: TButton;
    MemoLoadXmlWithALXmlDocument: TMemo;
    MemoLoadXmlWithXmlDocument: TMemo;
    MainOpenDialog: TOpenDialog;
    ButtonGenerate100000NodeWithALXmlDocument: TButton;
    MemoGenerate100000NodeWithALXmlDocument: TMemo;
    MemoGenerate100000NodeWithXmlDocument: TMemo;
    ButtonGenerate100000NodeWithXmlDocument: TButton;
    ALXMLDocumentSaxMode: TALXMLDocument;
    ButtonParseXMLWithALXmlDocumentInSaxMode: TButton;
    MemoParseXmlWithALXmlDocumentInSaxMode: TMemo;
    Panel1: TPanel;
    Label8: TLabel;
    Label12: TLabel;
    Panel2: TPanel;
    PanelWebBrowser: TPanel;
    procedure ButtonLoadXmlWithALXmlDocumentClick(Sender: TObject);
    procedure ButtonLoadXmlWithXmlDocumentClick(Sender: TObject);
    procedure ButtonGenerate100000NodeWithALXmlDocumentClick(Sender: TObject);
    procedure ButtonGenerate100000NodeWithXmlDocumentClick(Sender: TObject);
    procedure ALXMLDocumentSaxModeParseComment(Sender: TObject;const str: {$IF CompilerVersion >= 20}TALXMLString{$ELSE}STRING{$IFEND});
    procedure ALXMLDocumentSaxModeParseProcessingInstruction(Sender: TObject; const Target, Data: {$IF CompilerVersion >= 20}TALXMLString{$ELSE}STRING{$IFEND});
    procedure ALXMLDocumentSaxModeParseStartElement(Sender: TObject; const Name: {$IF CompilerVersion >= 20}TALXMLString{$ELSE}STRING{$IFEND}; const Attributes: TStrings);
    procedure ALXMLDocumentSaxModeParseText(Sender: TObject; const str: {$IF CompilerVersion >= 20}TALXMLString{$ELSE}STRING{$IFEND});
    procedure ButtonParseXMLWithALXmlDocumentInSaxModeClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FStartDate: Cardinal;
    FnodeCount: Integer;
    Function scrollAllNode(aNode: iXmlNode): Integer; overload;
    Function scrollAllNode(aNode: TalXmlNode): Integer; overload;
    procedure XmlFullyLoaded(var Message: Tmessage); message WM_XmlFullyLoaded;
  public
    { Public declarations }
  end;

function GetProcessMemoryInfo(Process : THandle; var MemoryCounters : TProcessMemoryCounters; cb : DWORD) : BOOL; stdcall;
function ProcessMemoryUsage(ProcessID : DWORD): DWORD;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{**************************************************}
function GetProcessMemoryInfo; external 'psapi.dll';

{****************************************************}
function ProcessMemoryUsage(ProcessID : DWORD): DWORD;
var ProcessHandle : THandle;
    MemCounters   : TProcessMemoryCounters;
begin
  Result := 0;
  ProcessHandle := OpenProcess(
                               PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
                               false,
                               ProcessID
                              );
  try
    if GetProcessMemoryInfo(
                            ProcessHandle,
                            MemCounters,
                            sizeof(MemCounters)
                           )
    then Result := MemCounters.WorkingSetSize;
  finally
    CloseHandle(ProcessHandle);
  end;
end;

{************************************************************}
Function CreateEmptyXMLDocument(Rootname:AnsiString):IXMLDocument;
Var aXmlDoc: TXMLDocument;
begin
  aXmlDoc := TXMLDocument.Create(nil);
  Result := aXmlDoc;
  with result do begin
    Options := [];
    ParseOptions := [];
    FileName := '';
  end;
End;

{******************************************************}
function TForm1.scrollAllNode(aNode: iXmlNode): Integer;
Var aStack: Tstack;
    i: integer;
begin
  Result := 0;
  aStack := Tstack.Create;
  try

      For i := 0 to aNode.ChildNodes.Count - 1 do
        aStack.Push(pointer(ANode.ChildNodes[i]));

      While astack.Count > 0 do begin
        inc(result);
        aNode := ixmlNode(astack.Pop);
        For i := 0 to ANode.ChildNodes.Count - 1 do
          aStack.Push(pointer(ANode.ChildNodes[i]));

        For i := 0 to ANode.AttributeNodes.Count - 1 do
          aStack.Push(pointer(ANode.AttributeNodes[i]));
      end;


  finally
    aStack.Free;
  end;

end;

{********************************************************}
function TForm1.scrollAllNode(aNode: TalXmlNode): Integer;
Var aStack: Tstack;
    i: integer;
begin
  Result := 0;
  aStack := Tstack.Create;
  try

      For i := 0 to aNode.ChildNodes.Count - 1 do
        aStack.Push(pointer(ANode.ChildNodes[i]));

      While astack.Count > 0 do begin
        inc(result);
        aNode := TalxmlNode(astack.Pop);
        If assigned(ANode.ChildNodes) then
          For i := 0 to ANode.ChildNodes.Count - 1 do
            aStack.Push(pointer(ANode.ChildNodes[i]));

        If assigned(ANode.AttributeNodes) then
          For i := 0 to ANode.attributeNodes.Count - 1 do
            aStack.Push(pointer(ANode.AttributeNodes[i]));
      end;


  finally
    aStack.Free;
  end;

end;

{*****************************************************}
procedure TForm1.XmlFullyLoaded(var Message: Tmessage);
begin
  Case message.WParam of
    1: MemoLoadXmlWithALXmlDocument.Lines.Add('Time to load and scroll all nodes: ' + inttostr(GetTickCount - fStartDate) + ' ms');
    2: MemoLoadXmlWithXmlDocument.Lines.Add('Time to load and scroll all nodes: ' + inttostr(GetTickCount - fStartDate) + ' ms');
    3: MemoGenerate100000NodeWithAlXmlDocument.Lines.Add('Time to create all nodes: ' + inttostr(GetTickCount - fStartDate) + ' ms');
    4: MemoGenerate100000NodeWithXmlDocument.Lines.Add('Time to create all nodes: ' + inttostr(GetTickCount - fStartDate) + ' ms');
    5: MemoParseXmlWithALXmlDocumentInSaxMode.Lines.Add('Time to scroll all nodes: ' + inttostr(GetTickCount - fStartDate) + ' ms');
  end;
end;

{********************************************************************}
procedure TForm1.ButtonLoadXmlWithALXmlDocumentClick(Sender: TObject);
Var aALXMLDocument: TALXmlDocument;
    aNodeCount: Integer;
begin
  If MainOpenDialog.Execute then begin

    MemoLoadXmlWithALXmlDocument.Lines.Clear;
    MemoLoadXmlWithALXmlDocument.Lines.Add('Memory usage before the load: ' + inttostr(ProcessMemoryUsage(GetCurrentProcessID)));

    Try

      aALXMLDocument:= TALXmlDocument.Create(nil);
      Try
        FStartDate := GetTickCount;
        aALXMLDocument.LoadFromFile(MainOpenDialog.FileName);
        aNodeCount := scrollAllNode(aALXMLDocument.Node);
        MemoLoadXmlWithALXmlDocument.Lines.Add('Memory usage after the load: ' + inttostr(ProcessMemoryUsage(GetCurrentProcessID)));
        MemoLoadXmlWithALXmlDocument.Lines.Add('Number of nodes created: ' + inttostr(aNodeCount));
        postMessage(handle,WM_XmlFullyLoaded,1,0);
      finally
        aALXMLDocument.Free;
      end;

    except
      on E: Exception do
        MemoLoadXmlWithALXmlDocument.Lines.Add('Error: ' + E.Message);
    end;

  end;

end;

{******************************************************************}
procedure TForm1.ButtonLoadXmlWithXmlDocumentClick(Sender: TObject);
Var aXMLDocument: iXmlDocument;
    aNodeCount: Integer;
begin
  If MainOpenDialog.Execute then begin

    MemoLoadXmlWithXmlDocument.Lines.Clear;
    MemoLoadXmlWithXmlDocument.Lines.Add('Memory usage before the load: ' + inttostr(ProcessMemoryUsage(GetCurrentProcessID)));

    Try

      aXMLDocument:= CreateEmptyXMLDocument('root');
      FStartDate := GetTickCount;
      aXMLDocument.LoadFromFile(MainOpenDialog.FileName);
      aNodeCount := scrollAllNode(aXMLDocument.node);
      MemoLoadXmlWithXmlDocument.Lines.Add('Memory usage after the load: ' + inttostr(ProcessMemoryUsage(GetCurrentProcessID)));
      MemoLoadXmlWithXmlDocument.Lines.Add('Number of nodes created: ' + inttostr(aNodeCount));
      postMessage(handle,WM_XmlFullyLoaded,2,0);

    except
      on E: Exception do
        MemoLoadXmlWithXmlDocument.Lines.Add('Error: ' + E.Message);
    end;

  end;

end;

{********************************************************************************}
procedure TForm1.ButtonGenerate100000NodeWithALXmlDocumentClick(Sender: TObject);
Var aALXMLDocument: TALXmlDocument;
    aNewRec, aValueRec: TalXmlNode;
    k,i: integer;
begin
  MemoGenerate100000NodeWithALXmlDocument.Lines.Clear;
  MemoGenerate100000NodeWithALXmlDocument.Lines.Add('Memory usage before the process: ' + inttostr(ProcessMemoryUsage(GetCurrentProcessID)));

  Try

    aALXMLDocument:= TALXmlDocument.Create(nil);
    Try
      FStartDate := GetTickCount;
      aALXMLDocument.Active := True;
      aALXMLDocument.AddChild('root');
      For k := 1 to 1000 do begin
        aNewRec := aALXMLDocument.DocumentElement.AddChild(alRandomStr(8));
        aNewrec.Attributes[alRandomStr(8)] := alRandomStr(25);
        aNewrec.Attributes[alRandomStr(8)] := alRandomStr(25);
        For i := 1 to 100 do begin
          aValueRec := aNewRec.AddChild(alRandomStr(8));
          aValueRec.Text := alRandomStr(25);
        end;
      end;

      MemoGenerate100000NodeWithALXmlDocument.Lines.Add('Memory usage after the process: ' + inttostr(ProcessMemoryUsage(GetCurrentProcessID)));
      postMessage(handle,WM_XmlFullyLoaded,3,0);
    finally
      aALXMLDocument.Free;
    end;

  except
    on E: Exception do
      MemoGenerate100000NodeWithALXmlDocument.Lines.Add('Error: ' + E.Message);
  end;
end;

{******************************************************************************}
procedure TForm1.ButtonGenerate100000NodeWithXmlDocumentClick(Sender: TObject);
Var aXMLDocument: iXmlDocument;
    aNewRec, aValueRec: iXmlNode;
    k,i: integer;
begin
  MemoGenerate100000NodeWithXmlDocument.Lines.Clear;
  MemoGenerate100000NodeWithXmlDocument.Lines.Add('Memory usage before the process: ' + inttostr(ProcessMemoryUsage(GetCurrentProcessID)));

  Try

      aXMLDocument:= CreateEmptyXMLDocument('root');
      FStartDate := GetTickCount;
      aXMLDocument.Active := true;
      aXMLDocument.AddChild('root');
      For k := 1 to 1000 do begin
        aNewRec := aXMLDocument.DocumentElement.AddChild(alRandomStr(8));
        aNewrec.Attributes[alRandomStr(8)] := alRandomStr(25);
        aNewrec.Attributes[alRandomStr(8)] := alRandomStr(25);
        For i := 1 to 100 do begin
          aValueRec := aNewRec.AddChild(alRandomStr(8));
          aValueRec.Text := alRandomStr(25);
        end;
      end;
      MemoGenerate100000NodeWithXmlDocument.Lines.Add('Memory usage after the process: ' + inttostr(ProcessMemoryUsage(GetCurrentProcessID)));
      postMessage(handle,WM_XmlFullyLoaded,4,0);

  except
    on E: Exception do
      MemoGenerate100000NodeWithXmlDocument.Lines.Add('Error: ' + E.Message);
  end;
end;

{******************************************************************************************************************************************}
procedure TForm1.ALXMLDocumentSaxModeParseComment(Sender: TObject; const str: {$IF CompilerVersion >= 20}TALXMLString{$ELSE}STRING{$IFEND});
begin
  inc(FNodeCount);
end;

{*****************************************************************************************************************************************************************}
procedure TForm1.ALXMLDocumentSaxModeParseProcessingInstruction(Sender: TObject; const Target, Data: {$IF CompilerVersion >= 20}TALXMLString{$ELSE}STRING{$IFEND});
begin
  inc(FNodeCount);
end;

{****************************************************************************************************************************************************************************}
procedure TForm1.ALXMLDocumentSaxModeParseStartElement(Sender: TObject; const Name: {$IF CompilerVersion >= 20}TALXMLString{$ELSE}STRING{$IFEND}; const Attributes: TStrings);
begin
  FNodeCount := FNodeCount + 2 * (Attributes.Count) + 1;
end;

{***************************************************************************************************************************************}
procedure TForm1.ALXMLDocumentSaxModeParseText(Sender: TObject; const str: {$IF CompilerVersion >= 20}TALXMLString{$ELSE}STRING{$IFEND});
begin
  inc(FNodeCount);
end;

{*******************************************************************************}
procedure TForm1.ButtonParseXMLWithALXmlDocumentInSaxModeClick(Sender: TObject);
begin
  If MainOpenDialog.Execute then begin

    MemoParseXmlWithALXmlDocumentInSaxMode.Lines.Clear;

    Try

      FnodeCount := 0;
      ALXMLDocumentSaxMode.filename := MainOpenDialog.FileName;
      FStartDate := GetTickCount;
      ALXMLDocumentSaxMode.parseXML;
      MemoParseXmlWithALXmlDocumentInSaxMode.Lines.Add('Number of nodes crawled: ' + inttostr(FNodeCount));
      postMessage(handle,WM_XmlFullyLoaded,5,0);

    except
      on E: Exception do
        MemoParseXmlWithALXmlDocumentInSaxMode.Lines.Add('Error: ' + E.Message);
    end;

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
  ie.Width := 100;
  ie.Height := 300;
  ie.MenuBar := false;
  ie.AddressBar := false;
  ie.Resizable := false;
  ie.StatusBar := false;
  ie.ToolBar := 0;
  Url := 'http://www.arkadia.com/html/alcinoe_like.html';
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
{$ENDIF}

end.
