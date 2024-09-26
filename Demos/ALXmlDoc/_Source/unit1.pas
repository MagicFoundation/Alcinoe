unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, Contnrs, StdCtrls, xmldom, XMLIntf, msxmldom,
  XMLDoc, Alcinoe.StringUtils, Alcinoe.XMLDoc, ExtCtrls, Alcinoe.Files,
  Alcinoe.StringList, Shellapi, system.IOUtils;

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
    procedure ALXMLDocumentSaxModeParseStartElement(Sender: TObject; const Path, Name: AnsiString; Attributes: TALStringsA);
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
  ProcessHandle := OpenProcess(
                     PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
                     false,
                     ProcessID);
  try
    if GetProcessMemoryInfo(
         ProcessHandle,
         MemCounters,
         sizeof(MemCounters))
    then Result := MemCounters.WorkingSetSize;
  finally
    CloseHandle(ProcessHandle);
  end;
end;

{****************************************************************}
Function CreateEmptyXMLDocument(Rootname:AnsiString):IXMLDocument;
Var LXmlDoc: TXMLDocument;
begin
  LXmlDoc := TXMLDocument.Create(nil);
  Result := LXmlDoc;
  with result do begin
    Options := [];
    ParseOptions := [];
    FileName := '';
  end;
End;

{******************************************************}
function TForm1.scrollAllNode(aNode: iXmlNode): Integer;
Var LStack: Tstack;
    I: integer;
begin
  Result := 0;
  LStack := Tstack.Create;
  try

    For I := 0 to aNode.ChildNodes.Count - 1 do
      LStack.Push(pointer(ANode.ChildNodes[I]));

    While LStack.Count > 0 do begin
      inc(result);
      aNode := ixmlNode(LStack.Pop);
      If assigned(ANode.ChildNodes) then
        For I := 0 to ANode.ChildNodes.Count - 1 do
          LStack.Push(pointer(ANode.ChildNodes[I]));

      If assigned(ANode.AttributeNodes) then
        For I := 0 to ANode.AttributeNodes.Count - 1 do
          LStack.Push(pointer(ANode.AttributeNodes[I]));
    end;

  finally
    LStack.Free;
  end;
end;

{********************************************************}
function TForm1.scrollAllNode(aNode: TalXmlNode): Integer;
Var LStack: Tstack;
    I: integer;
begin
  Result := 0;
  LStack := Tstack.Create;
  try

    For I := 0 to aNode.ChildNodes.Count - 1 do
      LStack.Push(pointer(ANode.ChildNodes[I]));

    While LStack.Count > 0 do begin
      inc(result);
      aNode := TalxmlNode(LStack.Pop);
      If assigned(ANode.ChildNodes) then
        For I := 0 to ANode.ChildNodes.Count - 1 do
          LStack.Push(pointer(ANode.ChildNodes[I]));

      If assigned(ANode.AttributeNodes) then
        For I := 0 to ANode.attributeNodes.Count - 1 do
          LStack.Push(pointer(ANode.AttributeNodes[I]));
    end;

  finally
    LStack.Free;
  end;

end;

{********************************************************************}
procedure TForm1.ButtonLoadXmlWithALXmlDocumentClick(Sender: TObject);
Var LXMLDocument: TALXmlDocument;
    LNodeCount: Integer;
    LMemoryUsage: DWORD;
    LStartDate: cardinal;
begin
  If MainOpenDialog.Execute then begin

    MemoLoadXmlWithALXmlDocument.Lines.Clear;
    LMemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    Try

      LXMLDocument:= TALXmlDocument.Create;
      Try
        LStartDate := GetTickCount;
        LXMLDocument.LoadFromFile(AnsiString(MainOpenDialog.FileName));
        LNodeCount := scrollAllNode(LXMLDocument.Node);
        MemoLoadXmlWithALXmlDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - LMemoryUsage)) + ' bytes');
        MemoLoadXmlWithALXmlDocument.Lines.Add('Number of nodes created: ' + IntToStr(LNodeCount));
        MemoLoadXmlWithALXmlDocument.Lines.Add('Time to load and scroll all nodes: ' + IntToStr(GetTickCount - LStartDate) + ' ms');
        LStartDate := GetTickCount;
        LXMLDocument.SaveToFile(ALGetModulePathA + 'sample.xml');
        MemoLoadXmlWithALXmlDocument.Lines.Add('Time to save the xml to disk: ' + IntToStr(GetTickCount - LStartDate) + ' ms');
        Tfile.delete(String(ALGetModulePathA + 'sample.xml'));
      finally
        LXMLDocument.Free;
      end;

    except
      on E: Exception do
        MemoLoadXmlWithALXmlDocument.Lines.Add('Error: ' + E.Message);
    end;

  end;
end;

{******************************************************************}
procedure TForm1.ButtonLoadXmlWithXmlDocumentClick(Sender: TObject);
Var LXMLDocument: iXmlDocument;
    LNodeCount: Integer;
    LMemoryUsage: DWORD;
    LStartDate: cardinal;
begin
  If MainOpenDialog.Execute then begin

    MemoLoadXmlWithXmlDocument.Lines.Clear;
    LMemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);

    Try

      LXMLDocument:= CreateEmptyXMLDocument('root');
      LStartDate := GetTickCount;
      LXMLDocument.LoadFromFile(MainOpenDialog.FileName);
      LNodeCount := scrollAllNode(LXMLDocument.node);
      MemoLoadXmlWithXmlDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - LMemoryUsage)) + ' bytes');
      MemoLoadXmlWithXmlDocument.Lines.Add('Number of nodes created: ' + IntToStr(LNodeCount));
      MemoLoadXmlWithXmlDocument.Lines.Add('Time to load and scroll all nodes: ' + IntToStr(GetTickCount - LStartDate) + ' ms');
      LStartDate := GetTickCount;
      LXMLDocument.SaveToFile(String(ALGetModulePathA + 'sample.xml'));
      MemoLoadXmlWithXmlDocument.Lines.Add('Time to save the xml to disk: ' + IntToStr(GetTickCount - LStartDate) + ' ms');
      Tfile.delete(String(ALGetModulePathA + 'sample.xml'));

    except
      on E: Exception do
        MemoLoadXmlWithXmlDocument.Lines.Add('Error: ' + E.Message);
    end;

  end;

end;

{*******************************************************************************}
procedure TForm1.ButtonGenerate100000NodeWithALXmlDocumentClick(Sender: TObject);
Var LXMLDocument: TALXmlDocument;
    LNewRec, LValueRec: TalXmlNode;
    LMemoryUsage: DWORD;
    LStartDate: cardinal;
    K,I: integer;
begin
  MemoGenerate100000NodeWithALXmlDocument.Lines.Clear;
  LMemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);

  Try

    LXMLDocument:= TALXmlDocument.Create;
    Try
      LStartDate := GetTickCount;
      LXMLDocument.Active := True;
      LXMLDocument.AddChild('root');
      For K := 1 to 1000 do begin
        LNewRec := LXMLDocument.DocumentElement.AddChild(ALRandomStrA(8));
        LNewRec.Attributes[ALRandomStrA(8)] := ALRandomStrA(25);
        LNewRec.Attributes[ALRandomStrA(8)] := ALRandomStrA(25);
        For I := 1 to 100 do begin
          LValueRec := LNewRec.AddChild(ALRandomStrA(8));
          LValueRec.Text := ALRandomStrA(25);
        end;
      end;

      MemoGenerate100000NodeWithALXmlDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - LMemoryUsage)) + ' bytes');
      MemoGenerate100000NodeWithAlXmlDocument.Lines.Add('Time to create all nodes: ' + IntToStr(GetTickCount - LStartDate) + ' ms');

      LStartDate := GetTickCount;
      LXMLDocument.SaveToFile(ALGetModulePathA + 'sample.xml');
      MemoGenerate100000NodeWithAlXmlDocument.Lines.Add('Time to save the xml to disk: ' + IntToStr(GetTickCount - LStartDate) + ' ms');
      Tfile.delete(String(ALGetModulePathA + 'sample.xml'));

    finally
      LXMLDocument.Free;
    end;

  except
    on E: Exception do
      MemoGenerate100000NodeWithALXmlDocument.Lines.Add('Error: ' + E.Message);
  end;
end;

{*****************************************************************************}
procedure TForm1.ButtonGenerate100000NodeWithXmlDocumentClick(Sender: TObject);
Var LXMLDocument: iXmlDocument;
    LNewRec, LValueRec: iXmlNode;
    LMemoryUsage: DWORD;
    LStartDate: cardinal;
    K,I: integer;
begin
  MemoGenerate100000NodeWithXmlDocument.Lines.Clear;
  LMemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);

  Try

      LXMLDocument:= CreateEmptyXMLDocument('root');
      LStartDate := GetTickCount;
      LXMLDocument.Active := true;
      LXMLDocument.AddChild('root');
      For K := 1 to 1000 do begin
        LNewRec := LXMLDocument.DocumentElement.AddChild(ALRandomStrW(8));
        LNewRec.Attributes[ALRandomStrW(8)] := ALRandomStrW(25);
        LNewRec.Attributes[ALRandomStrW(8)] := ALRandomStrW(25);
        For I := 1 to 100 do begin
          LValueRec := LNewRec.AddChild(ALRandomStrW(8));
          LValueRec.Text := ALRandomStrW(25);
        end;
      end;
      MemoGenerate100000NodeWithXmlDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - LMemoryUsage)) + ' bytes');
      MemoGenerate100000NodeWithXmlDocument.Lines.Add('Time to create all nodes: ' + IntToStr(GetTickCount - LStartDate) + ' ms');

      LStartDate := GetTickCount;
      LXMLDocument.SaveToFile(string(ALGetModulePathA + 'sample.xml'));
      MemoGenerate100000NodeWithXmlDocument.Lines.Add('Time to save the xml to disk: ' + IntToStr(GetTickCount - LStartDate) + ' ms');
      Tfile.delete(String(ALGetModulePathA + 'sample.xml'));

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

{*****************************************************************************************************************************}
procedure TForm1.ALXMLDocumentSaxModeParseStartElement(Sender: TObject; const Path, Name: AnsiString; Attributes: TALStringsA);
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
var LStartDate: cardinal;
    LXMLDocument: TalXmlDocument;
begin
  If MainOpenDialog.Execute then begin

    MemoParseXmlWithALXmlDocumentInSaxMode.Lines.Clear;

    LXMLDocument := TALXmlDocument.create('root');
    try

      Try

        LXMLDocument.OnParseProcessingInstruction := ALXMLDocumentSaxModeParseProcessingInstruction;
        LXMLDocument.OnParseStartElement := ALXMLDocumentSaxModeParseStartElement;
        LXMLDocument.OnParseText := ALXMLDocumentSaxModeParseText;
        LXMLDocument.OnParseComment := ALXMLDocumentSaxModeParseComment;

        FnodeCount := 0;
        LStartDate := GetTickCount;
        LXMLDocument.LoadFromFile(AnsiString(MainOpenDialog.FileName), True);
        MemoParseXmlWithALXmlDocumentInSaxMode.Lines.Add('Number of nodes crawled: ' + IntToStr(FNodeCount));
        MemoParseXmlWithALXmlDocumentInSaxMode.Lines.Add('Time to scroll all nodes: ' + IntToStr(GetTickCount - LStartDate) + ' ms');

      except
        on E: Exception do
          MemoParseXmlWithALXmlDocumentInSaxMode.Lines.Add('Error: ' + E.Message);
      end;

    finally
      LXMLDocument.Free;
    end;

  end;

end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
