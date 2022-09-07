unit Unit1;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
     Forms, Dialogs, Contnrs, StdCtrls, xmldom, XMLIntf, msxmldom,
     XMLDoc, ALString, AlXmlDoc, ExtCtrls, ALFiles,
     ALStringList, Shellapi;

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
    ButtonParseXMLWithALXmlDocumentInSaxMode: TButton;
    MemoParseXmlWithALXmlDocumentInSaxMode: TMemo;
    procedure ButtonLoadXmlWithALXmlDocumentClick(Sender: TObject);
    procedure ButtonLoadXmlWithXmlDocumentClick(Sender: TObject);
    procedure ButtonGenerate100000NodeWithALXmlDocumentClick(Sender: TObject);
    procedure ButtonGenerate100000NodeWithXmlDocumentClick(Sender: TObject);
    procedure ALXMLDocumentSaxModeParseComment(Sender: TObject; const Path, Str: AnsiString);
    procedure ALXMLDocumentSaxModeParseProcessingInstruction(Sender: TObject; const Path, Target, Data: AnsiString);
    procedure ALXMLDocumentSaxModeParseStartElement(Sender: TObject; const Path, Name: AnsiString; Attributes: TALStrings);
    procedure ALXMLDocumentSaxModeParseText(Sender: TObject; const Path, Str: AnsiString);
    procedure ButtonParseXMLWithALXmlDocumentInSaxModeClick(Sender: TObject);
  private
    FnodeCount: Integer;
    Function scrollAllNode(aNode: iXmlNode): Integer; overload;
    Function scrollAllNode(aNode: TalXmlNode): Integer; overload;
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

{****************************************************************}
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
      If assigned(ANode.ChildNodes) then
        For i := 0 to ANode.ChildNodes.Count - 1 do
          aStack.Push(pointer(ANode.ChildNodes[i]));

      If assigned(ANode.AttributeNodes) then
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

{********************************************************************}
procedure TForm1.ButtonLoadXmlWithALXmlDocumentClick(Sender: TObject);
Var aALXMLDocument: TALXmlDocument;
    aNodeCount: Integer;
    MemoryUsage: DWORD;
    aStartDate: cardinal;
begin
  If MainOpenDialog.Execute then begin

    MemoLoadXmlWithALXmlDocument.Lines.Clear;
    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    Try

      aALXMLDocument:= TALXmlDocument.Create;
      Try
        aStartDate := GetTickCount;
        aALXMLDocument.LoadFromFile(AnsiString(MainOpenDialog.FileName));
        aNodeCount := scrollAllNode(aALXMLDocument.Node);
        MemoLoadXmlWithALXmlDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');
        MemoLoadXmlWithALXmlDocument.Lines.Add('Number of nodes created: ' + IntToStr(aNodeCount));
        MemoLoadXmlWithALXmlDocument.Lines.Add('Time to load and scroll all nodes: ' + IntToStr(GetTickCount - aStartDate) + ' ms');
        aStartDate := GetTickCount;
        aALXMLDocument.SaveToFile(ALGetModulePath + 'sample.xml');
        MemoLoadXmlWithALXmlDocument.Lines.Add('Time to save the xml to disk: ' + IntToStr(GetTickCount - aStartDate) + ' ms');
        ALDeleteFile(ALGetModulePath + 'sample.xml');
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
    MemoryUsage: DWORD;
    aStartDate: cardinal;
begin
  If MainOpenDialog.Execute then begin

    MemoLoadXmlWithXmlDocument.Lines.Clear;
    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);

    Try

      aXMLDocument:= CreateEmptyXMLDocument('root');
      aStartDate := GetTickCount;
      aXMLDocument.LoadFromFile(MainOpenDialog.FileName);
      aNodeCount := scrollAllNode(aXMLDocument.node);
      MemoLoadXmlWithXmlDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');
      MemoLoadXmlWithXmlDocument.Lines.Add('Number of nodes created: ' + IntToStr(aNodeCount));
      MemoLoadXmlWithXmlDocument.Lines.Add('Time to load and scroll all nodes: ' + IntToStr(GetTickCount - aStartDate) + ' ms');
      aStartDate := GetTickCount;
      aXMLDocument.SaveToFile(String(ALGetModulePath + 'sample.xml'));
      MemoLoadXmlWithXmlDocument.Lines.Add('Time to save the xml to disk: ' + IntToStr(GetTickCount - aStartDate) + ' ms');
      ALDeleteFile(ALGetModulePath + 'sample.xml');

    except
      on E: Exception do
        MemoLoadXmlWithXmlDocument.Lines.Add('Error: ' + E.Message);
    end;

  end;

end;

{*******************************************************************************}
procedure TForm1.ButtonGenerate100000NodeWithALXmlDocumentClick(Sender: TObject);
Var aALXMLDocument: TALXmlDocument;
    aNewRec, aValueRec: TalXmlNode;
    MemoryUsage: DWORD;
    aStartDate: cardinal;
    k,i: integer;
begin
  MemoGenerate100000NodeWithALXmlDocument.Lines.Clear;
  MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);

  Try

    aALXMLDocument:= TALXmlDocument.Create;
    Try
      aStartDate := GetTickCount;
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

      MemoGenerate100000NodeWithALXmlDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');
      MemoGenerate100000NodeWithAlXmlDocument.Lines.Add('Time to create all nodes: ' + IntToStr(GetTickCount - aStartDate) + ' ms');

      aStartDate := GetTickCount;
      aALXMLDocument.SaveToFile(ALGetModulePath + 'sample.xml');
      MemoGenerate100000NodeWithAlXmlDocument.Lines.Add('Time to save the xml to disk: ' + IntToStr(GetTickCount - aStartDate) + ' ms');
      ALDeleteFile(ALGetModulePath + 'sample.xml');

    finally
      aALXMLDocument.Free;
    end;

  except
    on E: Exception do
      MemoGenerate100000NodeWithALXmlDocument.Lines.Add('Error: ' + E.Message);
  end;
end;

{*****************************************************************************}
procedure TForm1.ButtonGenerate100000NodeWithXmlDocumentClick(Sender: TObject);
Var aXMLDocument: iXmlDocument;
    aNewRec, aValueRec: iXmlNode;
    MemoryUsage: DWORD;
    aStartDate: cardinal;
    k,i: integer;
begin
  MemoGenerate100000NodeWithXmlDocument.Lines.Clear;
  MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);

  Try

      aXMLDocument:= CreateEmptyXMLDocument('root');
      aStartDate := GetTickCount;
      aXMLDocument.Active := true;
      aXMLDocument.AddChild('root');
      For k := 1 to 1000 do begin
        aNewRec := aXMLDocument.DocumentElement.AddChild(alRandomStrU(8));
        aNewrec.Attributes[alRandomStrU(8)] := alRandomStrU(25);
        aNewrec.Attributes[alRandomStrU(8)] := alRandomStrU(25);
        For i := 1 to 100 do begin
          aValueRec := aNewRec.AddChild(alRandomStrU(8));
          aValueRec.Text := alRandomStrU(25);
        end;
      end;
      MemoGenerate100000NodeWithXmlDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');
      MemoGenerate100000NodeWithXmlDocument.Lines.Add('Time to create all nodes: ' + IntToStr(GetTickCount - aStartDate) + ' ms');

      aStartDate := GetTickCount;
      aXMLDocument.SaveToFile(string(ALGetModulePath + 'sample.xml'));
      MemoGenerate100000NodeWithXmlDocument.Lines.Add('Time to save the xml to disk: ' + IntToStr(GetTickCount - aStartDate) + ' ms');
      ALDeleteFile(ALGetModulePath + 'sample.xml');

  except
    on E: Exception do
      MemoGenerate100000NodeWithXmlDocument.Lines.Add('Error: ' + E.Message);
  end;
end;

{**********************************************************************************************}
procedure TForm1.ALXMLDocumentSaxModeParseComment(Sender: TObject; const Path, Str: AnsiString);
begin
  inc(FNodeCount);
end;

{*********************************************************************************************************************}
procedure TForm1.ALXMLDocumentSaxModeParseProcessingInstruction(Sender: TObject; const Path, Target, Data: AnsiString);
begin
  inc(FNodeCount);
end;

{****************************************************************************************************************************}
procedure TForm1.ALXMLDocumentSaxModeParseStartElement(Sender: TObject; const Path, Name: AnsiString; Attributes: TALStrings);
begin
  FNodeCount := FNodeCount + 2 * (Attributes.Count) + 1;
end;

{*******************************************************************************************}
procedure TForm1.ALXMLDocumentSaxModeParseText(Sender: TObject; const Path, Str: AnsiString);
begin
  inc(FNodeCount);
end;

{******************************************************************************}
procedure TForm1.ButtonParseXMLWithALXmlDocumentInSaxModeClick(Sender: TObject);
var aStartDate: cardinal;
    aXMLDocument: TalXmlDocument;
begin
  If MainOpenDialog.Execute then begin

    MemoParseXmlWithALXmlDocumentInSaxMode.Lines.Clear;

    aXMLDocument := TALXmlDocument.create('root');
    try

      Try

        aXMLDocument.OnParseProcessingInstruction := ALXMLDocumentSaxModeParseProcessingInstruction;
        aXMLDocument.OnParseStartElement := ALXMLDocumentSaxModeParseStartElement;
        aXMLDocument.OnParseText := ALXMLDocumentSaxModeParseText;
        aXMLDocument.OnParseComment := ALXMLDocumentSaxModeParseComment;

        FnodeCount := 0;
        aStartDate := GetTickCount;
        aXMLDocument.LoadFromFile(AnsiString(MainOpenDialog.FileName), True);
        MemoParseXmlWithALXmlDocumentInSaxMode.Lines.Add('Number of nodes crawled: ' + IntToStr(FNodeCount));
        MemoParseXmlWithALXmlDocumentInSaxMode.Lines.Add('Time to scroll all nodes: ' + IntToStr(GetTickCount - aStartDate) + ' ms');

      except
        on E: Exception do
          MemoParseXmlWithALXmlDocumentInSaxMode.Lines.Add('Error: ' + E.Message);
      end;

    finally
      aXMLDocument.Free;
    end;

  end;

end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
