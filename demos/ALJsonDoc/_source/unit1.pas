unit Unit1;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
     StdCtrls, ALString, ALJsonDoc, ExtCtrls,
     ALStringList, Shellapi, cxGraphics, cxControls, cxLookAndFeels,
     cxLookAndFeelPainters, cxContainer, cxEdit, cxLabel, ALMime, Vcl.Dialogs,
     Contnrs, alFiles, diagnostics, superobject, DBXJSON, DBXplatform, IOUtils,
     dwsJSON;

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
    MemoLoadJsonDocument: TMemo;
    ButtonCreateDynamicallyJsonDocument: TButton;
    ButtonSaveToBson: TButton;
    MemoLoadJsonDocumentSAXMODEResult: TMemo;
    MemoCreateDynamicallyJsonDocument: TMemo;
    Label1: TLabel;
    MemoBSON: TMemo;
    Button1: TButton;
    Button3: TButton;
    MainOpenDialog: TOpenDialog;
    Button2: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure ButtonLoadXmlWithALXmlDocumentClick(Sender: TObject);
    procedure ButtonCreateDynamicallyJsonDocumentClick(Sender: TObject);
    procedure ButtonSaveToBsonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cxWwwArkadiaComLabelClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
  public
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

{**************************************************}
function scrollAllNode(aNode: TalJsonNode): Integer; overload;
Var aStack: Tstack;
    i: integer;
begin
  Result := 0;
  aStack := Tstack.Create;
  try

    if aNode.NodeType in [ntArray, ntObject] then
      For i := 0 to aNode.ChildNodes.Count - 1 do
        aStack.Push(pointer(ANode.ChildNodes[i]));

    While astack.Count > 0 do begin
      inc(result);
      aNode := TalJsonNode(astack.Pop);
      if aNode.NodeType in [ntArray, ntObject] then
        For i := 0 to ANode.ChildNodes.Count - 1 do
          aStack.Push(pointer(ANode.ChildNodes[i]));
    end;

  finally
    aStack.Free;
  end;

end;

{***************************************************}
function scrollAllNode(aNode: ISuperObject): Integer; overload;
Var aStack: Tstack;
    item: ISuperObject;
begin
  Result := 0;
  aStack := Tstack.Create;
  try

    for item in aNode do
      aStack.Push(pointer(item));

    While astack.Count > 0 do begin
      inc(result);
      aNode := ISuperObject(astack.Pop);
        For item in ANode do
          aStack.Push(pointer(item));
    end;

  finally
    aStack.Free;
  end;

end;

{*************************************************}
function scrollAllNode(aNode: TJSONValue): Integer; overload;
Var aStack: Tstack;
    item: TJSONValue;
    pair: TjsonPair;
begin
  Result := 0;
  aStack := Tstack.Create;
  try

    if aNode is TJSONArray then
      for item in (aNode as TJSONArray) do
        aStack.Push(pointer(item))
    else  if aNode is TJSONObject then
      for pair in (aNode as TJSONObject) do
         aStack.Push(pointer(pair.JsonValue));

    While astack.Count > 0 do begin
      inc(result);
      aNode := TJSONValue(astack.Pop);
      if aNode is TJSONArray then
        For item in (aNode as TJSONArray) do
          aStack.Push(pointer(item))
        else  if aNode is TJSONObject then
          for pair in (aNode as TJSONObject) do
             aStack.Push(pointer(pair.JsonValue))
    end;

  finally
    aStack.Free;
  end;

end;

{****************************************************}
function scrollAllNode(aNode: TdwsJSONValue): Integer; overload;
Var aStack: Tstack;
    item: TdwsJSONValue;
begin
  Result := 0;
  aStack := Tstack.Create;
  try

    if aNode is TdwsJSONArray then
      for item in (aNode as TdwsJSONArray) do
        aStack.Push(pointer(item))
    else  if aNode is TdwsJSONObject then
      for item in (aNode as TdwsJSONObject) do
         aStack.Push(pointer(item));

    While astack.Count > 0 do begin
      inc(result);
      aNode := TdwsJSONValue(astack.Pop);
      if aNode is TdwsJSONArray then
        For item in (aNode as TdwsJSONArray) do
          aStack.Push(pointer(item))
        else  if aNode is TdwsJSONObject then
          for item in (aNode as TdwsJSONObject) do
             aStack.Push(pointer(item))
    end;

  finally
    aStack.Free;
  end;

end;

{********************************************************************}
procedure TForm1.ButtonLoadXmlWithALXmlDocumentClick(Sender: TObject);
Var aALJsonDocument: TALJsonDocument;
begin

  //clear MemoLoadJsonDocumentSAXMODEResult
  MemoLoadJsonDocumentSAXMODEResult.Lines.Clear;

  //exemple 1 load the JSON doc in memory
  aALJsonDocument := TALJsonDocument.Create;
  try
    aALJsonDocument.LoadFromJSONString(AnsiString(MemoLoadJsonDocument.Lines.Text));
    aALJsonDocument.Options := [doNodeAutoIndent];
    MemoLoadJsonDocument.Lines.Text := String(aALJsonDocument.JSON);
  finally
    aALJsonDocument.Free;
  end;

  //exemple 2 load the JSON doc in SAX MODE
  aALJsonDocument := TALJsonDocument.Create;
  try
    aALJsonDocument.onParseText := procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType)
                                   begin
                                     case NodeSubType of
                                       nstFloat: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + string(ALFloatToStr(Args[0].VExtended^, ALDefaultFormatSettings)));
                                       nstText: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
                                       nstObject: ;
                                       nstArray: ;
                                       nstObjectID: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + 'ObjectId("'+string(ALBinToHex(ansiString(Args[0].VAnsiString)))+'")');
                                       nstBoolean: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + String(ALBoolToStr(Args[0].VBoolean,'true','false')));
                                       nstDateTime: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + string(ALFormatDateTime('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Args[0].VExtended^, ALDefaultFormatSettings)));
                                       nstNull: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + 'null');
                                       nstRegEx: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + '/' + string(ansiString(Args[0].VAnsiString)));
                                       nstBinary: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + 'BinData('+inttostr(Args[1].VInteger)+', "'+string(ansiString(ALMimeBase64EncodeStringNoCRLF(ansiString(Args[0].VAnsiString))))+'")');
                                       nstJavascript: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
                                       nstInt32: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + 'NumberInt('+inttostr(Args[0].VInteger)+')');
                                       nstTimestamp: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + 'Timestamp('+inttostr(int64(cardinal(Args[0].VInteger)))+', '+inttostr(int64(cardinal(Args[1].VInteger)))+')');
                                       nstInt64: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
                                     end;
                                   end;

    aALJsonDocument.LoadFromJSONString(AnsiString(MemoLoadJsonDocument.Lines.Text), true{saxMode});
  finally
    aALJsonDocument.Free;
  end;

end;

{*********************************************}
procedure TForm1.Button2Click(Sender: TObject);
var obj: ISuperObject;
    aNodeCount: Integer;
    MemoryUsage: DWORD;
    aStopWatch: TstopWatch;
begin
  If MainOpenDialog.Execute then begin

    MemoCreateDynamicallyJsonDocument.Lines.Clear;
    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    Try

      aStopWatch := TstopWatch.StartNew;
      obj := TSuperObject.ParseFile(MainOpenDialog.FileName, false);
      aStopWatch.Stop;
      MemoCreateDynamicallyJsonDocument.Lines.Add('Time to load all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
      aStopWatch := TstopWatch.StartNew;
      aNodeCount := scrollAllNode(obj);
      aStopWatch.Stop;
      MemoCreateDynamicallyJsonDocument.Lines.Add('Time scroll all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
      MemoCreateDynamicallyJsonDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');
      MemoCreateDynamicallyJsonDocument.Lines.Add('Number of nodes created: ' + FormatFloat('0,',aNodeCount));
      aStopWatch := TStopWatch.StartNew;
      obj.SaveTo(string(ALGetModulePath) + 'sample.txt');
      aStopWatch.Stop;
      MemoCreateDynamicallyJsonDocument.Lines.Add('Time to save the xml to disk: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
      ALDeleteFile(ALGetModulePath + 'sample.txt');

    except
      on E: Exception do
        MemoCreateDynamicallyJsonDocument.Lines.Add('Error: ' + E.Message);
    end;

  end;
end;

{*********************************************}
procedure TForm1.Button3Click(Sender: TObject);
Var aALJsonDocument: TALJsonDocument;
    aNodeCount: Integer;
    MemoryUsage: DWORD;
    aStopWatch: TstopWatch;
begin
  If MainOpenDialog.Execute then begin

    MemoCreateDynamicallyJsonDocument.Lines.Clear;
    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    Try

      aALJsonDocument:= TALJsonDocument.Create;
      Try
        aStopWatch := TstopWatch.StartNew;
        aALJsonDocument.LoadFromJsonFile(AnsiString(MainOpenDialog.FileName));
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('Time to load all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        aStopWatch := TstopWatch.StartNew;
        aNodeCount := scrollAllNode(aALJsonDocument.Node);
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('Time scroll all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        MemoCreateDynamicallyJsonDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');
        MemoCreateDynamicallyJsonDocument.Lines.Add('Number of nodes created: ' + FormatFloat('0,',aNodeCount));
        aStopWatch := TStopWatch.StartNew;
        aALJsonDocument.SaveToJsonFile(ALGetModulePath + 'sample.txt');
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('Time to save the xml to disk: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        ALDeleteFile(ALGetModulePath + 'sample.txt');
      finally
        aALJsonDocument.Free;
      end;

    except
      on E: Exception do
        MemoCreateDynamicallyJsonDocument.Lines.Add('Error: ' + E.Message);
    end;

  end;
end;

{*********************************************}
procedure TForm1.Button4Click(Sender: TObject);
Var aNodeCount: Integer;
    MemoryUsage: DWORD;
    JSONValue: TJSONValue;
    aStopWatch: TstopWatch;
begin
  If MainOpenDialog.Execute then begin

    MemoCreateDynamicallyJsonDocument.Lines.Clear;
    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    Try

      aStopWatch := TstopWatch.StartNew;
      JSONValue:= TJSONObject.ParseJSONValue(TFile.ReadAllText(MainOpenDialog.FileName));
      Try
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('Time to load all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        aStopWatch := TstopWatch.StartNew;
        aNodeCount := scrollAllNode(JSONValue);
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('Time scroll all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        MemoCreateDynamicallyJsonDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');
        MemoCreateDynamicallyJsonDocument.Lines.Add('Number of nodes created: ' + FormatFloat('0,',aNodeCount));
      finally
        JSONValue.Free;
      end;

    except
      on E: Exception do
        MemoCreateDynamicallyJsonDocument.Lines.Add('Error: ' + E.Message);
    end;

  end;
end;

{*********************************************}
procedure TForm1.Button5Click(Sender: TObject);
Var aNodeCount: Integer;
    MemoryUsage: DWORD;
    JSONValue: TdwsJSONValue;
    aStopWatch: TstopWatch;
begin
  If MainOpenDialog.Execute then begin

    MemoCreateDynamicallyJsonDocument.Lines.Clear;
    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    Try

      aStopWatch := TstopWatch.StartNew;
      JSONValue:= TdwsJSONValue.ParseFile(MainOpenDialog.FileName);
      Try
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('Time to load all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        aStopWatch := TstopWatch.StartNew;
        aNodeCount := scrollAllNode(JSONValue);
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('Time scroll all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        MemoCreateDynamicallyJsonDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');
        MemoCreateDynamicallyJsonDocument.Lines.Add('Number of nodes created: ' + FormatFloat('0,',aNodeCount));
      finally
        JSONValue.Free;
      end;

    except
      on E: Exception do
        MemoCreateDynamicallyJsonDocument.Lines.Add('Error: ' + E.Message);
    end;

  end;
end;

{*********************************************}
procedure TForm1.Button6Click(Sender: TObject);
Var aALJsonDocument: TALJsonDocument;
    aNodeCount: Integer;
    MemoryUsage: DWORD;
    aStopWatch: TstopWatch;
begin
  If MainOpenDialog.Execute then begin

    MemoCreateDynamicallyJsonDocument.Lines.Clear;
    MemoryUsage := ProcessMemoryUsage(GetCurrentProcessID);
    Try

      aALJsonDocument:= TALJsonDocument.Create;
      Try
        aStopWatch := TstopWatch.StartNew;
        aALJsonDocument.LoadFrombsonFile(AnsiString(MainOpenDialog.FileName));
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('Time to load all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        aStopWatch := TstopWatch.StartNew;
        aNodeCount := scrollAllNode(aALJsonDocument.Node);
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('Time scroll all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        MemoCreateDynamicallyJsonDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(ProcessMemoryUsage(GetCurrentProcessID) - MemoryUsage)) + ' bytes');
        MemoCreateDynamicallyJsonDocument.Lines.Add('Number of nodes created: ' + FormatFloat('0,',aNodeCount));
        aStopWatch := TStopWatch.StartNew;
        aALJsonDocument.SaveTobsonFile(ALGetModulePath + 'sample.txt');
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('Time to save the xml to disk: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        ALDeleteFile(ALGetModulePath + 'sample.txt');
      finally
        aALJsonDocument.Free;
      end;

    except
      on E: Exception do
        MemoCreateDynamicallyJsonDocument.Lines.Add('Error: ' + E.Message);
    end;

  end;
end;

{*************************************************************************}
procedure TForm1.ButtonCreateDynamicallyJsonDocumentClick(Sender: TObject);
Var aALJsonDocument: TALJsonDocument;
begin

  aALJsonDocument:= TALJsonDocument.Create(true);
  Try

    aALJsonDocument.addchild('_id').float := 1.32;
    with aALJsonDocument.addchild('name', ntObject) do begin
      addchild('first').text := 'John';
      addchild('last').text := 'Backus';
    end;
    aALJsonDocument.addchild('birth').datetime := Now;
    with aALJsonDocument.addchild('contribs', ntArray) do begin
      addchild.text := 'Fortran';
      addchild.text := 'ALGOL';
      addchild.text := 'Backus-Naur Form';
      addchild.text := 'FP';
    end;
    with aALJsonDocument.addchild('awards', ntArray) do begin
      with addchild(ntObject) do begin
        addchild('award').text := 'National Medal of Science';
        addchild('year').int32 := 1975;
        addchild('by').text := 'National Science Foundation';
      end;
      with addchild(ntObject) do begin
        addchild('award').text := 'Turing Award';
        addchild('year').int32 := 1977;
        addchild('by').text := 'ACM';
      end;
    end;
    aALJsonDocument.addchild('spouse');
    aALJsonDocument.addchild('address', ntObject);
    aALJsonDocument.addchild('phones', ntArray);

    aALJsonDocument.Options := [doNodeAutoIndent];
    MemoCreateDynamicallyJsonDocument.Lines.Text := String(aALJsonDocument.JSON);

  finally
    aALJsonDocument.Free;
  end;

end;

{******************************************************}
procedure TForm1.ButtonSaveToBsonClick(Sender: TObject);
Var aALJsonDocument: TALJsonDocument;
    aBsonStr: AnsiString;
    i: integer;
begin

  //clear MemoLoadJsonDocumentSAXMODEResult
  MemoCreateDynamicallyJsonDocument.Lines.Clear;
  MemoBSON.Lines.Clear;

  aALJsonDocument:= TALJsonDocument.Create(true);
  Try

    aALJsonDocument.addchild('_id').float := 1.32;
    with aALJsonDocument.addchild('name', ntObject) do begin
      addchild('first').text := 'John';
      addchild('last').text := 'Backus';
    end;
    aALJsonDocument.addchild('birth').datetime := Now;
    with aALJsonDocument.addchild('contribs', ntArray) do begin
      addchild.text := 'Fortran';
      addchild.text := 'ALGOL';
      addchild.text := 'Backus-Naur Form';
      addchild.text := 'FP';
    end;
    with aALJsonDocument.addchild('awards', ntArray) do begin
      with addchild(ntObject) do begin
        addchild('award').text := 'National Medal of Science';
        addchild('year').int32 := 1975;
        addchild('by').text := 'National Science Foundation';
      end;
      with addchild(ntObject) do begin
        addchild('award').text := 'Turing Award';
        addchild('year').int32 := 1977;
        addchild('by').text := 'ACM';
      end;
    end;
    aALJsonDocument.addchild('spouse');
    aALJsonDocument.addchild('address', ntObject);
    aALJsonDocument.addchild('phones', ntArray);

    aALJsonDocument.Options := [doNodeAutoIndent];
    MemoCreateDynamicallyJsonDocument.Lines.Text := String(aALJsonDocument.JSON);
    aBsonStr := aALJsonDocument.BSON;
    for I := 1 to length(aBsonStr) do
      MemoBSON.Lines.add(Inttostr(ord(aBsonStr[i])));

  finally
    aALJsonDocument.Free;
  end;

end;

{**********************************************************}
procedure TForm1.cxWwwArkadiaComLabelClick(Sender: TObject);
begin
  ShellExecute(Application.Handle,'open','http://www.arkadia.com',nil,nil, SW_SHOWNORMAL);
end;

{*********************************************}
procedure TForm1.Button1Click(Sender: TObject);
var aBsonStr: AnsiString;
    aALJsonDocument: TALJsonDocument;
    i: integer;
begin
  aBsonStr := '';
  for I := 0 to MemoBSON.Lines.Count - 1 do
    aBsonStr := aBsonStr + AnsiChar(StrToInt(MemoBSON.Lines[i]));
  MemoCreateDynamicallyJsonDocument.Lines.Clear;

  //exemple 1 load the JSON doc in memory
  aALJsonDocument := TALJsonDocument.Create;
  try
    aALJsonDocument.LoadFromBSONString(aBsonStr);
    aALJsonDocument.Options := [doNodeAutoIndent];
    MemoCreateDynamicallyJsonDocument.Lines.Text := String(aALJsonDocument.JSON);
  finally
    aALJsonDocument.Free;
  end;

  //exemple 2 load the JSON doc in SAX MODE
  MemoLoadJsonDocumentSAXMODEResult.Lines.Clear;
  aALJsonDocument := TALJsonDocument.Create;
  try
    aALJsonDocument.onParseText := procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType)
                                   begin
                                     case NodeSubType of
                                       nstFloat: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + string(ALFloatToStr(Args[0].VExtended^, ALDefaultFormatSettings)));
                                       nstText: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
                                       nstObject: ;
                                       nstArray: ;
                                       nstObjectID: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + 'ObjectId("'+string(ALBinToHex(ansiString(Args[0].VAnsiString)))+'")');
                                       nstBoolean: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + String(ALBoolToStr(Args[0].VBoolean,'true','false')));
                                       nstDateTime: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + string(ALFormatDateTime('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Args[0].VExtended^, ALDefaultFormatSettings)));
                                       nstNull: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + 'null');
                                       nstRegEx: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + '/' + string(ansiString(Args[0].VAnsiString)));
                                       nstBinary: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + 'BinData('+inttostr(Args[1].VInteger)+', "'+string(ansiString(ALMimeBase64EncodeStringNoCRLF(ansiString(Args[0].VAnsiString))))+'")');
                                       nstJavascript: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
                                       nstInt32: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + 'NumberInt('+inttostr(Args[0].VInteger)+')');
                                       nstTimestamp: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + 'Timestamp('+inttostr(int64(cardinal(Args[0].VInteger)))+', '+inttostr(int64(cardinal(Args[1].VInteger)))+')');
                                       nstInt64: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
                                     end;
                                   end;
    aALJsonDocument.LoadFromBSONString(aBsonStr, true{saxMode});
  finally
    aALJsonDocument.Free;
  end;

end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
