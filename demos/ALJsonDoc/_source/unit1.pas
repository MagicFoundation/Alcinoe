unit Unit1;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
     StdCtrls, ALString, ALJsonDoc, ExtCtrls,
     ALStringList, Shellapi, Vcl.Dialogs,
     Contnrs, alFiles, diagnostics, superobject, DBXJSON, DBXplatform, IOUtils,
     dwsJSON, Execute.JSON, system.Generics.collections,
     system.UITypes, system.JSON;

type

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
    Button7: TButton;
    procedure ButtonLoadXmlWithALXmlDocumentClick(Sender: TObject);
    procedure ButtonCreateDynamicallyJsonDocumentClick(Sender: TObject);
    procedure ButtonSaveToBsonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{$WARNINGS OFF}

{**************************************}
function GetTotalMemoryAllocated: int64;
var aMemoryState: TMemoryManagerState;
    i: Integer;
begin

  // get memory manager state
  GetMemoryManagerState(aMemoryState);

  // take the allocated size
  Result := 0;
  with aMemoryState do begin
    // small blocks
    for i := Low(SmallBlockTypeStates) to High(SmallBlockTypeStates) do
      Inc(Result,
          SmallBlockTypeStates[i].AllocatedBlockCount *
          SmallBlockTypeStates[i].UseableBlockSize);

    // medium blocks
    Inc(Result, TotalAllocatedMediumBlockSize);

    // large blocks
    Inc(Result, TotalAllocatedLargeBlockSize);
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
function scrollAllNode(aNode: TalJsonNodeU): Integer; overload;
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
      aNode := TalJsonNodeU(astack.Pop);
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
    aALJsonDocumentU: TALJsonDocumentU;
begin

  //clear MemoLoadJsonDocumentSAXMODEResult
  MemoLoadJsonDocumentSAXMODEResult.Lines.Clear;

  if messageDlg('Use unicode version of TalJsonDoc (ie: TalJsonDocU)?', mtConfirmation, [TMsgDlgBtn.mbNo, TMsgDlgBtn.mbYes], 0) = MrNo then begin

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
                                         nstRegEx: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
                                         nstBinary: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + 'BinData('+inttostr(Args[1].VInteger)+', "'+string(ansiString(ALBase64EncodeString(ansiString(Args[0].VAnsiString))))+'")');
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

  end
  else begin

    //exemple 1 load the JSON doc in memory
    aALJsonDocumentU := TALJsonDocumentU.Create;
    try
      aALJsonDocumentU.LoadFromJSONString(MemoLoadJsonDocument.Lines.Text);
      aALJsonDocumentU.Options := [doNodeAutoIndent];
      MemoLoadJsonDocument.Lines.Text := aALJsonDocumentU.JSON;
    finally
      aALJsonDocumentU.Free;
    end;

    //exemple 2 load the JSON doc in SAX MODE
    aALJsonDocumentU := TALJsonDocumentU.Create;
    try
      aALJsonDocumentU.onParseText := procedure (Sender: TObject; const Path: String; const name: String; const Args: array of const; NodeSubType: TALJSONNodeSubType)
                                     begin
                                       case NodeSubType of
                                         nstFloat: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + ALFloatToStrU(Args[0].VExtended^, ALDefaultFormatSettingsU));
                                         nstText: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + String(Args[0].VUnicodeString));
                                         nstObject: ;
                                         nstArray: ;
                                         nstObjectID: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + 'ObjectId("'+string(Args[0].VUnicodeString)+'")');
                                         nstBoolean: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + ALBoolToStrU(Args[0].VBoolean,'true','false'));
                                         nstDateTime: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + ALFormatDateTimeU('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Args[0].VExtended^, ALDefaultFormatSettingsU));
                                         nstNull: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + 'null');
                                         nstRegEx: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + String(Args[0].VUnicodeString));
                                         nstBinary: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + 'BinData('+ALinttostrU(Args[1].VInteger)+', "'+String(Args[0].VunicodeString)+'")');
                                         nstJavascript: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + String(Args[0].VUnicodeString));
                                         nstInt32: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + 'NumberInt('+alinttostrU(Args[0].VInteger)+')');
                                         nstTimestamp: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + 'Timestamp('+alinttostrU(int64(cardinal(Args[0].VInteger)))+', '+alinttostrU(int64(cardinal(Args[1].VInteger)))+')');
                                         nstInt64: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
                                       end;
                                     end;

      aALJsonDocumentU.LoadFromJSONString(MemoLoadJsonDocument.Lines.Text, true{saxMode});
    finally
      aALJsonDocumentU.Free;
    end;

  end;

end;

{*********************************************}
procedure TForm1.Button2Click(Sender: TObject);
var obj: ISuperObject;
    aNodeCount: Integer;
    MemoryUsage: Int64;
    aStopWatch: TstopWatch;
begin
  If MainOpenDialog.Execute then begin

    MemoCreateDynamicallyJsonDocument.Lines.Clear;
    MemoryUsage := GetTotalMemoryAllocated;
    Try

      aStopWatch := TstopWatch.StartNew;
      obj := TSuperObject.ParseFile(MainOpenDialog.FileName, false);
      aStopWatch.Stop;
      MemoCreateDynamicallyJsonDocument.Lines.Add('Time to load all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
      aStopWatch := TstopWatch.StartNew;
      aNodeCount := scrollAllNode(obj);
      aStopWatch.Stop;
      MemoCreateDynamicallyJsonDocument.Lines.Add('Time scroll all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
      MemoCreateDynamicallyJsonDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(GetTotalMemoryAllocated - MemoryUsage)) + ' bytes');
      MemoCreateDynamicallyJsonDocument.Lines.Add('Number of nodes created: ' + FormatFloat('0,',aNodeCount));
      aStopWatch := TStopWatch.StartNew;
      obj.SaveTo(string(ALGetModulePath) + 'sample.txt');
      aStopWatch.Stop;
      MemoCreateDynamicallyJsonDocument.Lines.Add('Time to save the JSON to disk: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
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
    aALJsonDocumentU: TALJsonDocumentU;
    aNodeCount: Integer;
    MemoryUsage: int64;
    aStopWatch: TstopWatch;
begin
  If MainOpenDialog.Execute then begin

    screen.Cursor := CrHourglass;
    MemoCreateDynamicallyJsonDocument.Lines.Clear;
    MemoryUsage := GetTotalMemoryAllocated;
    Try

      aALJsonDocument:= TALJsonDocument.Create;
      Try
        aStopWatch := TstopWatch.StartNew;
        aALJsonDocument.LoadFromJsonFile(AnsiString(MainOpenDialog.FileName));
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDoc: Time to load all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        aStopWatch := TstopWatch.StartNew;
        aNodeCount := scrollAllNode(aALJsonDocument.Node);
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDoc: Time scroll all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDoc: Memory used: ' + FormatFloat('0,',(GetTotalMemoryAllocated - MemoryUsage)) + ' bytes');
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDoc: Number of nodes created: ' + FormatFloat('0,',aNodeCount));
        aStopWatch := TStopWatch.StartNew;
        aALJsonDocument.SaveToJsonFile(ALGetModulePath + 'sample.txt');
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDoc: Time to save the JSON to disk: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        ALDeleteFile(ALGetModulePath + 'sample.txt');
      finally
        aALJsonDocument.Free;
      end;

      MemoCreateDynamicallyJsonDocument.Lines.Add('');
      MemoCreateDynamicallyJsonDocument.Lines.Add('');

      aALJsonDocumentU:= TALJsonDocumentU.Create;
      Try
        aStopWatch := TstopWatch.StartNew;
        aALJsonDocumentU.LoadFromJsonFile(MainOpenDialog.FileName);
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDocU: Time to load all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        aStopWatch := TstopWatch.StartNew;
        aNodeCount := scrollAllNode(aALJsonDocumentU.Node);
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDocU: Time scroll all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDocU: Memory used: ' + FormatFloat('0,',(GetTotalMemoryAllocated - MemoryUsage)) + ' bytes');
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDocU: Number of nodes created: ' + FormatFloat('0,',aNodeCount));
        aStopWatch := TStopWatch.StartNew;
        aALJsonDocumentU.SaveToJsonFile(String(ALGetModulePath + 'sample.txt'));
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDocU: Time to save the JSON to disk: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        ALDeleteFile(ALGetModulePath + 'sample.txt');
      finally
        aALJsonDocumentU.Free;
      end;

    except
      on E: Exception do
        MemoCreateDynamicallyJsonDocument.Lines.Add('Error: ' + E.Message);
    end;
    screen.Cursor := CrDefault;

  end;
end;

{*********************************************}
procedure TForm1.Button4Click(Sender: TObject);
Var aNodeCount: Integer;
    MemoryUsage: int64;
    JSONValue: TJSONValue;
    aStopWatch: TstopWatch;
begin
  If MainOpenDialog.Execute then begin

    MemoCreateDynamicallyJsonDocument.Lines.Clear;
    MemoryUsage := GetTotalMemoryAllocated;
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
        MemoCreateDynamicallyJsonDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(GetTotalMemoryAllocated - MemoryUsage)) + ' bytes');
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
    MemoryUsage: int64;
    JSONValue: TdwsJSONValue;
    aStopWatch: TstopWatch;
begin
  If MainOpenDialog.Execute then begin

    MemoCreateDynamicallyJsonDocument.Lines.Clear;
    MemoryUsage := GetTotalMemoryAllocated;
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
        MemoCreateDynamicallyJsonDocument.Lines.Add('Memory used: ' + FormatFloat('0,',(GetTotalMemoryAllocated - MemoryUsage)) + ' bytes');
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
    aALJsonDocumentU: TALJsonDocumentU;
    aNodeCount: Integer;
    MemoryUsage: int64;
    aStopWatch: TstopWatch;
begin
  If MainOpenDialog.Execute then begin

    screen.Cursor := crHourglass;
    MemoCreateDynamicallyJsonDocument.Lines.Clear;
    MemoryUsage := GetTotalMemoryAllocated;
    Try

      aALJsonDocument:= TALJsonDocument.Create;
      Try
        aStopWatch := TstopWatch.StartNew;
        aALJsonDocument.LoadFrombsonFile(AnsiString(MainOpenDialog.FileName));
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDoc: Time to load all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        aStopWatch := TstopWatch.StartNew;
        aNodeCount := scrollAllNode(aALJsonDocument.Node);
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDoc: Time scroll all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDoc: Memory used: ' + FormatFloat('0,',(GetTotalMemoryAllocated - MemoryUsage)) + ' bytes');
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDoc: Number of nodes created: ' + FormatFloat('0,',aNodeCount));
        aStopWatch := TStopWatch.StartNew;
        aALJsonDocument.SaveTobsonFile(ALGetModulePath + 'sample.txt');
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDoc: Time to save the JSON to disk: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        ALDeleteFile(ALGetModulePath + 'sample.txt');
      finally
        aALJsonDocument.Free;
      end;

      MemoCreateDynamicallyJsonDocument.Lines.Add('');
      MemoCreateDynamicallyJsonDocument.Lines.Add('');

      aALJsonDocumentU:= TALJsonDocumentU.Create;
      Try
        aStopWatch := TstopWatch.StartNew;
        aALJsonDocumentU.LoadFrombsonFile(MainOpenDialog.FileName);
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDocU: Time to load all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        aStopWatch := TstopWatch.StartNew;
        aNodeCount := scrollAllNode(aALJsonDocumentU.Node);
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDocU: Time scroll all nodes: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDocU: Memory used: ' + FormatFloat('0,',(GetTotalMemoryAllocated - MemoryUsage)) + ' bytes');
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDocU: Number of nodes created: ' + FormatFloat('0,',aNodeCount));
        aStopWatch := TStopWatch.StartNew;
        aALJsonDocumentU.SaveTobsonFile(string(ALGetModulePath) + 'sample.txt');
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('TALJSONDocU: Time to save the JSON to disk: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');
        ALDeleteFile(ALGetModulePath + 'sample.txt');
      finally
        aALJsonDocumentU.Free;
      end;

    except
      on E: Exception do
        MemoCreateDynamicallyJsonDocument.Lines.Add('Error: ' + E.Message);
    end;
    screen.Cursor := crdefault;

  end;
end;

type
  TMyObject = class
    Name: string;
    Date: TDateTime;
    Nums: array of Integer;
    Plus: string;
  end;

procedure TForm1.Button7Click(Sender: TObject);
  var
    t: TMyObject;
    s: string;
    i: integer;
    aStopWatch: TstopWatch;
    aALJsonDocument: TALJsonDocument;
    S2: ansiString;

begin

    MemoCreateDynamicallyJsonDocument.Lines.Clear;
    Try

      aStopWatch := TstopWatch.StartNew;
      for I := 0 to 100000 do begin
        t := TMyObject.Create;
        t.Name := 'Paul';
        t.Date := Now();
        setlength(t.Nums, 3);
        t.Nums[0] := 1;
        t.Nums[2] := 2;
        t.Nums[3] := 3;
        t.Plus := 'plus';
        s := t.toJSON(); // {"Name":"Paul","Date":"2015-11-23T19:33:25","Nums":[1,0,2],"Plus":"plus"}
        t.Free;
        t := TMyObject.Create;
        t.FromJSON(s);
        t.Free;
      end;
      aStopWatch.Stop;
      MemoCreateDynamicallyJsonDocument.Lines.Add('Time to save/load 100000 objects with EXECUTE.json: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');

        aStopWatch := TstopWatch.StartNew;
        for I := 0 to 100000 do begin
          aALJsonDocument:= TALJsonDocument.Create;
          aALJsonDocument.addchild('Name').Text := 'Stephane';
          aALJsonDocument.addchild('Date').DateTime := Now;
          with aALJsonDocument.addchild('Nums', ntArray) do begin
            addchild.Float := 1;
            addchild.Float := 2;
            addchild.Float := 3;
          end;
          aALJsonDocument.addchild('Plus').Text := 'plus';
          aALJsonDocument.SaveToBSONString(S2); // {"Name":"Stephane","Date":ISODate("2015-11-23T19:44:53.057Z"),"Nums":[1,2,3],"Plus":"plus"}
          aALJsonDocument.Free;
          aALJsonDocument:= TALJsonDocument.Create;
          aALJsonDocument.LoadFromBSONString(S2);
          aALJsonDocument.Free;
        end;
        aStopWatch.Stop;
        MemoCreateDynamicallyJsonDocument.Lines.Add('Time to save/load 100000 objects with TalJsonDoc: ' + FormatFloat('0,',aStopWatch.Elapsed.TotalMilliseconds) + ' ms');

    except
      on E: Exception do
        MemoCreateDynamicallyJsonDocument.Lines.Add('Error: ' + E.Message);
    end;

end;

{*************************************************************************}
procedure TForm1.ButtonCreateDynamicallyJsonDocumentClick(Sender: TObject);
Var aALJsonDocument: TALJsonDocument;
    aALJsonDocumentU: TALJsonDocumentU;
    aBytes: Tbytes;
begin

  if messageDlg('Use unicode version of TalJsonDoc (ie: TalJsonDocU)?', mtConfirmation, [TMsgDlgBtn.mbNo, TMsgDlgBtn.mbYes], 0) = MrNo then begin

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
      with aALJsonDocument.AddChild('regex') do begin
        RegEx := '<TAG\b[^>]*>(.*?)</TAG>';
        RegExOptions := [preMultiLine, preCaseLess];
      end;
      with aALJsonDocument.AddChild('binary') do begin
        binary := #1#2#3#4#5;
        BinarySubType := 0;
      end;
      aALJsonDocument.AddChild('ObjectId').ObjectId := #1#2#3#4#5#6#7#8#9#0#1#2;

      aALJsonDocument.Options := [doNodeAutoIndent];
      MemoCreateDynamicallyJsonDocument.Lines.Text := String(aALJsonDocument.JSON);

    finally
      aALJsonDocument.Free;
    end;

  end
  else begin

    aALJsonDocumentU:= TALJsonDocumentU.Create(true);
    Try

      aALJsonDocumentU.addchild('_id').float := 1.32;
      with aALJsonDocumentU.addchild('name', ntObject) do begin
        addchild('first').text := 'John';
        addchild('last').text := 'Backus';
      end;
      aALJsonDocumentU.addchild('birth').datetime := Now;
      with aALJsonDocumentU.addchild('contribs', ntArray) do begin
        addchild.text := 'Fortran';
        addchild.text := 'ALGOL';
        addchild.text := 'Backus-Naur Form';
        addchild.text := 'FP';
      end;
      with aALJsonDocumentU.addchild('awards', ntArray) do begin
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
      aALJsonDocumentU.addchild('spouse');
      aALJsonDocumentU.addchild('address', ntObject);
      aALJsonDocumentU.addchild('phones', ntArray);
      with aALJsonDocumentU.AddChild('regex') do begin
        RegEx := '<TAG\b[^>]*>(.*?)</TAG>';
        RegExOptions := [preMultiLine, preCaseLess];
      end;
      with aALJsonDocumentU.AddChild('binary') do begin
        setlength(aBytes, 5);
        Abytes[0] := 1;
        Abytes[1] := 2;
        Abytes[2] := 3;
        Abytes[3] := 4;
        Abytes[4] := 5;
        binary := ALBase64EncodeBytesU(Abytes);
        BinarySubType := 0;
      end;
      setlength(aBytes, 12);
      Abytes[0] := 1;
      Abytes[1] := 2;
      Abytes[2] := 3;
      Abytes[3] := 4;
      Abytes[4] := 5;
      Abytes[5] := 6;
      Abytes[6] := 7;
      Abytes[7] := 8;
      Abytes[8] := 9;
      Abytes[9] := 0;
      Abytes[10] := 1;
      Abytes[11] := 2;
      aALJsonDocumentU.AddChild('ObjectId').ObjectId := albinToHexU(Abytes);

      aALJsonDocumentU.Options := [doNodeAutoIndent];
      MemoCreateDynamicallyJsonDocument.Lines.Text := aALJsonDocumentU.JSON;

    finally
      aALJsonDocumentU.Free;
    end;

  end;

end;

{******************************************************}
procedure TForm1.ButtonSaveToBsonClick(Sender: TObject);
Var aALJsonDocument: TALJsonDocument;
    aALJsonDocumentU: TALJsonDocumentU;
    aBsonStr: AnsiString;
    aBsonBytes: Tbytes;
    aBytes: Tbytes;
    i: integer;
begin

  //clear MemoLoadJsonDocumentSAXMODEResult
  MemoCreateDynamicallyJsonDocument.Lines.Clear;
  MemoBSON.Lines.Clear;

 if messageDlg('Use unicode version of TalJsonDoc (ie: TalJsonDocU)?', mtConfirmation, [TMsgDlgBtn.mbNo, TMsgDlgBtn.mbYes], 0) = MrNo then begin

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
      with aALJsonDocument.AddChild('regex') do begin
        RegEx := '<TAG\b[^>]*>(.*?)</TAG>';
        RegExOptions := [preMultiLine, preCaseLess];
      end;
      with aALJsonDocument.AddChild('binary') do begin
        binary := #1#2#3#4#5;
        BinarySubType := 0;
      end;
      aALJsonDocument.AddChild('ObjectId').ObjectId := #1#2#3#4#5#6#7#8#9#0#1#2;

      aALJsonDocument.Options := [doNodeAutoIndent];
      MemoCreateDynamicallyJsonDocument.Lines.Text := String(aALJsonDocument.JSON);
      aBsonStr := aALJsonDocument.BSON;
      for I := 1 to length(aBsonStr) do
        MemoBSON.Lines.add(Inttostr(ord(aBsonStr[i])));

    finally
      aALJsonDocument.Free;
    end;

   end
   else begin

      aALJsonDocumentU:= TALJsonDocumentU.Create(true);
      Try

        aALJsonDocumentU.addchild('_id').float := 1.32;
        with aALJsonDocumentU.addchild('name', ntObject) do begin
          addchild('first').text := 'John';
          addchild('last').text := 'Backus';
        end;
        aALJsonDocumentU.addchild('birth').datetime := Now;
        with aALJsonDocumentU.addchild('contribs', ntArray) do begin
          addchild.text := 'Fortran';
          addchild.text := 'ALGOL';
          addchild.text := 'Backus-Naur Form';
          addchild.text := 'FP';
        end;
        with aALJsonDocumentU.addchild('awards', ntArray) do begin
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
        aALJsonDocumentU.addchild('spouse');
        aALJsonDocumentU.addchild('address', ntObject);
        aALJsonDocumentU.addchild('phones', ntArray);
        with aALJsonDocumentU.AddChild('regex') do begin
          RegEx := '<TAG\b[^>]*>(.*?)</TAG>';
          RegExOptions := [preMultiLine, preCaseLess];
        end;
        with aALJsonDocumentU.AddChild('binary') do begin
          setlength(aBytes, 5);
          Abytes[0] := 1;
          Abytes[1] := 2;
          Abytes[2] := 3;
          Abytes[3] := 4;
          Abytes[4] := 5;
          binary := ALBase64EncodeBytesU(Abytes);
          BinarySubType := 0;
        end;
        setlength(aBytes, 12);
        Abytes[0] := 1;
        Abytes[1] := 2;
        Abytes[2] := 3;
        Abytes[3] := 4;
        Abytes[4] := 5;
        Abytes[5] := 6;
        Abytes[6] := 7;
        Abytes[7] := 8;
        Abytes[8] := 9;
        Abytes[9] := 0;
        Abytes[10] := 1;
        Abytes[11] := 2;
        aALJsonDocumentU.AddChild('ObjectId').ObjectId := albinToHexU(Abytes);

        aALJsonDocumentU.Options := [doNodeAutoIndent];
        MemoCreateDynamicallyJsonDocument.Lines.Text := aALJsonDocumentU.JSON;
        aBsonBytes := aALJsonDocumentU.BSON;
        for I := 0 to length(aBsonBytes) - 1 do
          MemoBSON.Lines.add(Inttostr(aBsonBytes[i]));

      finally
        aALJsonDocumentU.Free;
      end;

   end;
end;

{*********************************************}
procedure TForm1.Button1Click(Sender: TObject);
var aBsonStr: AnsiString;
    aALJsonDocument: TALJsonDocument;
    aALJsonDocumentU: TALJsonDocumentU;
    i: integer;
begin
  aBsonStr := '';
  for I := 0 to MemoBSON.Lines.Count - 1 do
    aBsonStr := aBsonStr + AnsiChar(StrToInt(MemoBSON.Lines[i]));
  MemoCreateDynamicallyJsonDocument.Lines.Clear;

  if messageDlg('Use unicode version of TalJsonDoc (ie: TalJsonDocU)?', mtConfirmation, [TMsgDlgBtn.mbNo, TMsgDlgBtn.mbYes], 0) = MrNo then begin

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
                                         nstRegEx: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
                                         nstBinary: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(String(Path) + '=' + 'BinData('+inttostr(Args[1].VInteger)+', "'+string(ansiString(ALBase64EncodeString(ansiString(Args[0].VAnsiString))))+'")');
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

  end
  else begin


    //exemple 1 load the JSON doc in memory
    aALJsonDocumentU := TALJsonDocumentU.Create;
    try
      aALJsonDocumentU.LoadFromBSONBytes(BytesOf(aBsonStr));
      aALJsonDocumentU.Options := [doNodeAutoIndent];
      MemoCreateDynamicallyJsonDocument.Lines.Text := String(aALJsonDocumentU.JSON);
    finally
      aALJsonDocumentU.Free;
    end;

    //exemple 2 load the JSON doc in SAX MODE
    MemoLoadJsonDocumentSAXMODEResult.Lines.Clear;
    aALJsonDocumentU := TALJsonDocumentU.Create;
    try
      aALJsonDocumentU.onParseText := procedure (Sender: TObject; const Path: String; const name: String; const Args: array of const; NodeSubType: TALJSONNodeSubType)
                                       begin
                                         case NodeSubType of
                                           nstFloat: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + ALFloatToStrU(Args[0].VExtended^, ALDefaultFormatSettingsU));
                                           nstText: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + String(Args[0].VUnicodeString));
                                           nstObject: ;
                                           nstArray: ;
                                           nstObjectID: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + 'ObjectId("'+String(Args[0].VUnicodeString)+'")');
                                           nstBoolean: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + ALBoolToStrU(Args[0].VBoolean,'true','false'));
                                           nstDateTime: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + ALFormatDateTimeU('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Args[0].VExtended^, ALDefaultFormatSettingsU));
                                           nstNull: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + 'null');
                                           nstRegEx: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + String(Args[0].VUnicodeString));
                                           nstBinary: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + 'BinData('+ALinttostrU(Args[1].VInteger)+', "'+String(Args[0].VunicodeString)+'")');
                                           nstJavascript: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + String(Args[0].VUnicodeString));
                                           nstInt32: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + 'NumberInt('+alinttostrU(Args[0].VInteger)+')');
                                           nstTimestamp: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + 'Timestamp('+alinttostrU(int64(cardinal(Args[0].VInteger)))+', '+alinttostrU(int64(cardinal(Args[1].VInteger)))+')');
                                           nstInt64: MemoLoadJsonDocumentSAXMODEResult.Lines.Add(Path + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
                                         end;
                                       end;
      aALJsonDocumentU.LoadFromBSONBytes(BytesOf(aBsonStr), true{saxMode});
    finally
      aALJsonDocumentU.Free;
    end;

  end;

end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
