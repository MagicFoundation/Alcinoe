unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Alcinoe.StringUtils, Alcinoe.JSONDoc, ExtCtrls,
  Alcinoe.StringList, Shellapi, Vcl.Dialogs,
  Contnrs, Alcinoe.Files, diagnostics, superobject, DBXplatform, IOUtils,
  dwsJSON, system.Generics.collections,
  system.UITypes, system.JSON, Vcl.ComCtrls, VclTee.TeeGDIPlus,
  Vcl.Samples.Spin, VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs,
  VCLTee.Chart, System.AnsiStrings;

type

  TForm1 = class(TForm)
    MainOpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    ButtonLoadXmlWithALXmlDocument: TButton;
    MemoJson: TMemo;
    ButtonCreateDynamicallyJsonDocument: TButton;
    MemoSaxModeEvents: TMemo;
    MemoBSON: TMemo;
    Button1: TButton;
    Label2: TLabel;
    Chart1: TChart;
    Series1: TBarSeries;
    Series2: TBarSeries;
    Series3: TBarSeries;
    Series4: TBarSeries;
    Series5: TBarSeries;
    BtnRunBenchmark: TButton;
    SpinEditNbItems: TSpinEdit;
    CheckBoxTALJsonDocJSON: TCheckBox;
    CheckBoxTALJsonDocUJSON: TCheckBox;
    CheckBoxSuperObject: TCheckBox;
    CheckBoxSystemJSON: TCheckBox;
    CheckBoxDwsJSON: TCheckBox;
    procedure ButtonLoadXmlWithALXmlDocumentClick(Sender: TObject);
    procedure ButtonCreateDynamicallyJsonDocumentClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BtnRunBenchmarkClick(Sender: TObject);
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
      Inc(
        Result,
        SmallBlockTypeStates[i].AllocatedBlockCount *
        SmallBlockTypeStates[i].UseableBlockSize);

    // medium blocks
    Inc(Result, TotalAllocatedMediumBlockSize);

    // large blocks
    Inc(Result, TotalAllocatedLargeBlockSize);
  end;

end;

{********************************************************************}
procedure TForm1.ButtonLoadXmlWithALXmlDocumentClick(Sender: TObject);
begin

  //clear MemoJSON
  MemoSaxModeEvents.Lines.Clear;
  MemoBSON.lines.Clear;
  MemoSaxModeEvents.Lines.Clear;

  if messageDlg('Use unicode version of TalJsonDoc (ie: TalJsonDocU)?', mtConfirmation, [TMsgDlgBtn.mbNo, TMsgDlgBtn.mbYes], 0) = MrNo then begin

    //exemple 1 load the JSON doc in memory
    var LALJsonDocument := TALJSONDocumentA.Create;
    try
      LALJsonDocument.LoadFromJSONString(AnsiString(MemoJSON.Lines.Text));
      LALJsonDocument.Options := [doNodeAutoIndent];
      MemoJSON.Lines.Text := String(LALJsonDocument.JSON);
      var LBsonStr := LALJsonDocument.BSON;
      for var I := 1 to length(LBsonStr) do
        MemoBSON.Lines.add(Inttostr(ord(LBsonStr[i])));
    finally
      LALJsonDocument.Free;
    end;

    //exemple 2 load the JSON doc in SAX MODE
    LALJsonDocument := TALJSONDocumentA.Create;
    try
      LALJsonDocument.onParseText := procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType)
                                     begin
                                       case NodeSubType of
                                         nstFloat: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + string(ALFloatToStrA(Args[0].VExtended^, ALDefaultFormatSettingsA)));
                                         nstText: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
                                         nstObject: ;
                                         nstArray: ;
                                         nstObjectID: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + 'ObjectId("'+string(ALBinToHexA(ansiString(Args[0].VAnsiString)))+'")');
                                         nstBoolean: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + String(ALBoolToStrA(Args[0].VBoolean,'true','false')));
                                         nstDateTime: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + string(ALFormatDateTimeA('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Args[0].VExtended^, ALDefaultFormatSettingsA)));
                                         nstNull: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + 'null');
                                         nstRegEx: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
                                         nstBinary: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + 'BinData('+inttostr(Args[1].VInteger)+', "'+string(ansiString(ALBase64EncodeString(ansiString(Args[0].VAnsiString))))+'")');
                                         nstJavascript: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
                                         nstInt32: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + 'NumberInt('+inttostr(Args[0].VInteger)+')');
                                         nstTimestamp: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + 'Timestamp('+inttostr(int64(cardinal(Args[0].VInteger)))+', '+inttostr(int64(cardinal(Args[1].VInteger)))+')');
                                         nstInt64: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
                                       end;
                                     end;

      LALJsonDocument.LoadFromJSONString(AnsiString(MemoJSON.Lines.Text), true{saxMode});
    finally
      LALJsonDocument.Free;
    end;

  end
  else begin

    //exemple 1 load the JSON doc in memory
    var LALJsonDocumentU := TALJSONDocumentW.Create;
    try
      LALJsonDocumentU.LoadFromJSONString(MemoJSON.Lines.Text);
      LALJsonDocumentU.Options := [doNodeAutoIndent];
      MemoJSON.Lines.Text := LALJsonDocumentU.JSON;
      var LBsonBytes := LALJsonDocumentU.BSON;
      for var I := 0 to length(LBsonBytes) - 1 do
        MemoBSON.Lines.add(Inttostr(LBsonBytes[i]));
    finally
      LALJsonDocumentU.Free;
    end;

    //exemple 2 load the JSON doc in SAX MODE
    LALJsonDocumentU := TALJSONDocumentW.Create;
    try
      LALJsonDocumentU.onParseText := procedure (Sender: TObject; const Path: String; const name: String; const Args: array of const; NodeSubType: TALJSONNodeSubType)
                                      begin
                                        case NodeSubType of
                                          nstFloat: MemoSaxModeEvents.Lines.Add(Path + '=' + ALFloatToStrW(Args[0].VExtended^, ALDefaultFormatSettingsW));
                                          nstText: MemoSaxModeEvents.Lines.Add(Path + '=' + String(Args[0].VUnicodeString));
                                          nstObject: ;
                                          nstArray: ;
                                          nstObjectID: MemoSaxModeEvents.Lines.Add(Path + '=' + 'ObjectId("'+string(Args[0].VUnicodeString)+'")');
                                          nstBoolean: MemoSaxModeEvents.Lines.Add(Path + '=' + ALBoolToStrW(Args[0].VBoolean,'true','false'));
                                          nstDateTime: MemoSaxModeEvents.Lines.Add(Path + '=' + ALFormatDateTimeW('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Args[0].VExtended^, ALDefaultFormatSettingsW));
                                          nstNull: MemoSaxModeEvents.Lines.Add(Path + '=' + 'null');
                                          nstRegEx: MemoSaxModeEvents.Lines.Add(Path + '=' + String(Args[0].VUnicodeString));
                                          nstBinary: MemoSaxModeEvents.Lines.Add(Path + '=' + 'BinData('+ALIntToStrW(Args[1].VInteger)+', "'+String(Args[0].VunicodeString)+'")');
                                          nstJavascript: MemoSaxModeEvents.Lines.Add(Path + '=' + String(Args[0].VUnicodeString));
                                          nstInt32: MemoSaxModeEvents.Lines.Add(Path + '=' + 'NumberInt('+ALIntToStrW(Args[0].VInteger)+')');
                                          nstTimestamp: MemoSaxModeEvents.Lines.Add(Path + '=' + 'Timestamp('+ALIntToStrW(int64(cardinal(Args[0].VInteger)))+', '+ALIntToStrW(int64(cardinal(Args[1].VInteger)))+')');
                                          nstInt64: MemoSaxModeEvents.Lines.Add(Path + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
                                        end;
                                      end;

      LALJsonDocumentU.LoadFromJSONString(MemoJSON.Lines.Text, true{saxMode});
    finally
      LALJsonDocumentU.Free;
    end;

  end;

end;

{*************************************************************************}
procedure TForm1.ButtonCreateDynamicallyJsonDocumentClick(Sender: TObject);
Var LALJsonDocument: TALJSONDocumentA;
    LALJsonDocumentU: TALJSONDocumentW;
    LBsonStr: AnsiString;
    LBsonBytes: Tbytes;
    LBytes: Tbytes;
    i: integer;
begin

  //clear MemoJSON
  MemoJSON.Lines.Clear;
  MemoBSON.Lines.Clear;
  MemoSaxModeEvents.Lines.Clear;

  if messageDlg('Use unicode version of TalJsonDoc (ie: TalJsonDocU)?', mtConfirmation, [TMsgDlgBtn.mbNo, TMsgDlgBtn.mbYes], 0) = MrNo then begin

    LALJsonDocument:= TALJSONDocumentA.Create(true);
    Try

      LALJsonDocument.addchild('_id').float := 1.32;
      with LALJsonDocument.addchild('name', ntObject) do begin
        addchild('first').text := 'John';
        addchild('last').text := 'Backus';
      end;
      LALJsonDocument.addchild('birth').datetime := Now;
      with LALJsonDocument.addchild('contribs', ntArray) do begin
        addchild.text := 'Fortran';
        addchild.text := 'ALGOL';
        addchild.text := 'Backus-Naur Form';
        addchild.text := 'FP';
      end;
      with LALJsonDocument.addchild('awards', ntArray) do begin
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
      LALJsonDocument.addchild('spouse');
      LALJsonDocument.addchild('address', ntObject);
      LALJsonDocument.addchild('phones', ntArray);
      with LALJsonDocument.AddChild('regex') do begin
        RegEx := '<TAG\b[^>]*>(.*?)</TAG>';
        RegExOptions := [preMultiLine, preCaseLess];
      end;
      with LALJsonDocument.AddChild('binary') do begin
        binary := #1#2#3#4#5;
        BinarySubType := 0;
      end;
      LALJsonDocument.AddChild('ObjectId').ObjectId := #1#2#3#4#5#6#7#8#9#0#1#2;

      LALJsonDocument.Options := [doNodeAutoIndent];
      MemoJSON.Lines.Text := String(LALJsonDocument.JSON);
      LBsonStr := LALJsonDocument.BSON;
      for I := 1 to length(LBsonStr) do
        MemoBSON.Lines.add(Inttostr(ord(LBsonStr[i])));

    finally
      LALJsonDocument.Free;
    end;

   end
   else begin

      LALJsonDocumentU:= TALJSONDocumentW.Create(true);
      Try

        LALJsonDocumentU.addchild('_id').float := 1.32;
        with LALJsonDocumentU.addchild('name', ntObject) do begin
          addchild('first').text := 'John';
          addchild('last').text := 'Backus';
        end;
        LALJsonDocumentU.addchild('birth').datetime := Now;
        with LALJsonDocumentU.addchild('contribs', ntArray) do begin
          addchild.text := 'Fortran';
          addchild.text := 'ALGOL';
          addchild.text := 'Backus-Naur Form';
          addchild.text := 'FP';
        end;
        with LALJsonDocumentU.addchild('awards', ntArray) do begin
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
        LALJsonDocumentU.addchild('spouse');
        LALJsonDocumentU.addchild('address', ntObject);
        LALJsonDocumentU.addchild('phones', ntArray);
        with LALJsonDocumentU.AddChild('regex') do begin
          RegEx := '<TAG\b[^>]*>(.*?)</TAG>';
          RegExOptions := [preMultiLine, preCaseLess];
        end;
        with LALJsonDocumentU.AddChild('binary') do begin
          setlength(LBytes, 5);
          LBytes[0] := 1;
          LBytes[1] := 2;
          LBytes[2] := 3;
          LBytes[3] := 4;
          LBytes[4] := 5;
          binary := ALBase64EncodeBytesW(LBytes);
          BinarySubType := 0;
        end;
        setlength(LBytes, 12);
        LBytes[0] := 1;
        LBytes[1] := 2;
        LBytes[2] := 3;
        LBytes[3] := 4;
        LBytes[4] := 5;
        LBytes[5] := 6;
        LBytes[6] := 7;
        LBytes[7] := 8;
        LBytes[8] := 9;
        LBytes[9] := 0;
        LBytes[10] := 1;
        LBytes[11] := 2;
        LALJsonDocumentU.AddChild('ObjectId').ObjectId := ALBinToHexW(LBytes);

        LALJsonDocumentU.Options := [doNodeAutoIndent];
        MemoJSON.Lines.Text := LALJsonDocumentU.JSON;
        LBsonBytes := LALJsonDocumentU.BSON;
        for I := 0 to length(LBsonBytes) - 1 do
          MemoBSON.Lines.add(Inttostr(LBsonBytes[i]));

      finally
        LALJsonDocumentU.Free;
      end;

   end;
end;

{*****************************************************}
procedure TForm1.BtnRunBenchmarkClick(Sender: TObject);

  const
    _iterationcount: integer = 1000000;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _scrollAllNode(aNode: TALJSONNodeA): Integer; overload;
  Var LStack: Tstack;
      i: integer;
  begin
    Result := 0;
    LStack := Tstack.Create;
    try

      if aNode.NodeType in [ntArray, ntObject] then
        For i := 0 to aNode.ChildNodes.Count - 1 do
          LStack.Push(pointer(ANode.ChildNodes[i]));

      While LStack.Count > 0 do begin
        inc(result);
        aNode := TALJSONNodeA(LStack.Pop);
        if aNode.NodeType in [ntArray, ntObject] then
          For i := 0 to ANode.ChildNodes.Count - 1 do
            LStack.Push(pointer(ANode.ChildNodes[i]));
      end;

    finally
      LStack.Free;
    end;

  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _scrollAllNode(aNode: TALJSONNodeW): Integer; overload;
  Var LStack: Tstack;
      i: integer;
  begin
    Result := 0;
    LStack := Tstack.Create;
    try

      if aNode.NodeType in [ntArray, ntObject] then
        For i := 0 to aNode.ChildNodes.Count - 1 do
          LStack.Push(pointer(ANode.ChildNodes[i]));

      While LStack.Count > 0 do begin
        inc(result);
        aNode := TALJSONNodeW(LStack.Pop);
        if aNode.NodeType in [ntArray, ntObject] then
          For i := 0 to ANode.ChildNodes.Count - 1 do
            LStack.Push(pointer(ANode.ChildNodes[i]));
      end;

    finally
      LStack.Free;
    end;

  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _scrollAllNode(aNode: ISuperObject): Integer; overload;
  Var LStack: Tstack;
      Litem: ISuperObject;
  begin
    Result := 0;
    LStack := Tstack.Create;
    try

      for Litem in aNode do
        LStack.Push(pointer(Litem));

      While LStack.Count > 0 do begin
        inc(result);
        aNode := ISuperObject(LStack.Pop);
          For Litem in ANode do
            LStack.Push(pointer(Litem));
      end;

    finally
      LStack.Free;
    end;

  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _scrollAllNode(aNode: TJSONValue): Integer; overload;
  Var LStack: Tstack;
      Litem: TJSONValue;
      Lpair: TjsonPair;
  begin
    Result := 0;
    LStack := Tstack.Create;
    try

      if aNode is TJSONArray then
        for Litem in (aNode as TJSONArray) do
          LStack.Push(pointer(Litem))
      else  if aNode is TJSONObject then
        for Lpair in (aNode as TJSONObject) do
           LStack.Push(pointer(Lpair.JsonValue));

      While LStack.Count > 0 do begin
        inc(result);
        aNode := TJSONValue(LStack.Pop);
        if aNode is TJSONArray then
          For Litem in (aNode as TJSONArray) do
            LStack.Push(pointer(Litem))
          else  if aNode is TJSONObject then
            for Lpair in (aNode as TJSONObject) do
               LStack.Push(pointer(Lpair.JsonValue))
      end;

    finally
      LStack.Free;
    end;

  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _scrollAllNode(aNode: TdwsJSONValue): Integer; overload;
  Var LStack: Tstack;
      Litem: TdwsJSONValue;
  begin
    Result := 0;
    LStack := Tstack.Create;
    try

      if aNode is TdwsJSONArray then
        for Litem in (aNode as TdwsJSONArray) do
          LStack.Push(pointer(Litem))
      else  if aNode is TdwsJSONObject then
        for Litem in (aNode as TdwsJSONObject) do
           LStack.Push(pointer(Litem));

      While LStack.Count > 0 do begin
        inc(result);
        aNode := TdwsJSONValue(LStack.Pop);
        if aNode is TdwsJSONArray then
          For Litem in (aNode as TdwsJSONArray) do
            LStack.Push(pointer(Litem))
          else  if aNode is TdwsJSONObject then
            for Litem in (aNode as TdwsJSONObject) do
               LStack.Push(pointer(Litem))
      end;

    finally
      LStack.Free;
    end;

  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoTALJsonDocJSONBench(Count: integer);
  begin
    //var LMemoryUsage := GetTotalMemoryAllocated;
    var LALJsonDocument:= TALJSONDocumentA.Create;
    Try
      var LStopWatch := TstopWatch.StartNew;
      LALJsonDocument.LoadFromJsonFile('..\sample.json');
      LStopWatch.Stop;
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Load (JSON)');
      application.ProcessMessages;
      //----
      LALJsonDocument.Clear;
      LStopWatch := TstopWatch.StartNew;
      LALJsonDocument.LoadFromBsonFile('..\sample.bson');
      LStopWatch.Stop;
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Load (BSON)');
      application.ProcessMessages;
      //----
      LStopWatch := TstopWatch.StartNew;
      _scrollAllNode(LALJsonDocument.Node);
      LStopWatch.Stop;
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Scroll all nodes');
      application.ProcessMessages;
      //----
      //MemoMemoryUsed.Lines.Add('TALJSONDocumentA: ' + FormatFloat('0,',(GetTotalMemoryAllocated - LMemoryUsage)) + ' bytes');
      //application.ProcessMessages;
      //----
      for var I := 1 to count - 1 do
        LALJsonDocument.Node.AddChild(ALLowerCase(ALRandomStrA(7)));
      //----
      LStopWatch := TstopWatch.StartNew;
      for var i := 1 to _iterationcount do
        LALJsonDocument.Node.ChildNodes.FindNode(ALLowerCase(ALRandomStrA(7)));
      LStopWatch.Stop;
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Find (unsorted)');
      application.ProcessMessages;
      //----
      LALJsonDocument.Options := LALJsonDocument.Options + [doSorted];
      LStopWatch := TstopWatch.StartNew;
      for var i := 1 to _iterationcount do
        LALJsonDocument.Node.ChildNodes.FindNode(ALLowerCase(ALRandomStrA(7)));
      LStopWatch.Stop;
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Find (sorted)');
      application.ProcessMessages;
      //----
      LStopWatch := TStopWatch.StartNew;
      LALJsonDocument.SaveToJsonFile(ALGetModulePathA + '~tmp.json');
      LStopWatch.Stop;
      ALDeleteFile(ALGetModulePathA + '~tmp.json');
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Save (JSON)');
      application.ProcessMessages;
      //----
      LStopWatch := TStopWatch.StartNew;
      LALJsonDocument.SaveToBsonFile(ALGetModulePathA + '~tmp.bson');
      LStopWatch.Stop;
      ALDeleteFile(ALGetModulePathA + '~tmp.bson');
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Save (BSON)');
      application.ProcessMessages;
    finally
      LALJsonDocument.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoTALJsonDocUJSONBench(Count: integer);
  begin
    //var LMemoryUsage := GetTotalMemoryAllocated;
    var LALJsonDocument:= TALJSONDocumentW.Create;
    Try
      var LStopWatch := TstopWatch.StartNew;
      LALJsonDocument.LoadFromJsonFile('..\sample.json');
      LStopWatch.Stop;
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Load (JSON)');
      application.ProcessMessages;
      //----
      LALJsonDocument.Clear;
      LStopWatch := TstopWatch.StartNew;
      LALJsonDocument.LoadFromBsonFile('..\sample.bson');
      LStopWatch.Stop;
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Load (BSON)');
      application.ProcessMessages;
      //----
      LStopWatch := TstopWatch.StartNew;
      _scrollAllNode(LALJsonDocument.Node);
      LStopWatch.Stop;
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Scroll all nodes');
      application.ProcessMessages;
      //----
      //MemoMemoryUsed.Lines.Add('TALJSONDocumentW: ' + FormatFloat('0,',(GetTotalMemoryAllocated - LMemoryUsage)) + ' bytes');
      //application.ProcessMessages;
      //----
      for var I := 1 to count - 1 do
        LALJsonDocument.Node.AddChild(ALLowerCase(ALRandomStrA(7)));
      //----
      LStopWatch := TstopWatch.StartNew;
      for var i := 1 to _iterationcount do
        LALJsonDocument.Node.ChildNodes.FindNode(ALLowerCase(ALRandomStrA(7)));
      LStopWatch.Stop;
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Find (unsorted)');
      application.ProcessMessages;
      //----
      LALJsonDocument.Options := LALJsonDocument.Options + [doSorted];
      LStopWatch := TstopWatch.StartNew;
      for var i := 1 to _iterationcount do
        LALJsonDocument.Node.ChildNodes.FindNode(ALLowerCase(ALRandomStrA(7)));
      LStopWatch.Stop;
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Find (sorted)');
      application.ProcessMessages;
      //----
      LStopWatch := TStopWatch.StartNew;
      LALJsonDocument.SaveToJsonFile(ALGetModulePathA + '~tmp.json');
      LStopWatch.Stop;
      ALDeleteFile(ALGetModulePathA + '~tmp.json');
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Save (JSON)');
      application.ProcessMessages;
      //----
      LStopWatch := TStopWatch.StartNew;
      LALJsonDocument.SaveToBsonFile(ALGetModulePathA + '~tmp.bson');
      LStopWatch.Stop;
      ALDeleteFile(ALGetModulePathA + '~tmp.bson');
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Save (BSON)');
      application.ProcessMessages;
    finally
      LALJsonDocument.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoSystemJSONBench(Count: integer);
  begin
    //var LMemoryUsage := GetTotalMemoryAllocated;
    var LStopWatch := TstopWatch.StartNew;
    var LJSONValue:= TJSONObject.ParseJSONValue(TFile.ReadAllText('..\sample.json'));
    Try
      LStopWatch.Stop;
      chart1.Series[2].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Load (JSON)');
      application.ProcessMessages;
      //-----
      chart1.Series[2].AddY(0, 'Load (BSON)');
      //-----
      LStopWatch := TstopWatch.StartNew;
      _scrollAllNode(LJSONValue);
      LStopWatch.Stop;
      chart1.Series[2].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Scroll all nodes');
      application.ProcessMessages;
      //----
      //MemoMemoryUsed.Lines.Add('System.JSON: ' + FormatFloat('0,',(GetTotalMemoryAllocated - LMemoryUsage)) + ' bytes');
      //application.ProcessMessages;
      //----
      for var I := 1 to count - 1 do
        TJSONObject(LJSONValue).AddPair(ALLowerCase(ALRandomStrA(7)), '');
      //----
      LStopWatch := TstopWatch.StartNew;
      for var i := 1 to _iterationcount do
        LJSONValue.FindValue(AlLowerCase(ALRandomStrW(7)));
      LStopWatch.Stop;
      chart1.Series[2].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Find (unsorted)');
      application.ProcessMessages;
      //----
      chart1.Series[2].AddY(0, 'Find (sorted)');
      //-----
      LStopWatch := TStopWatch.StartNew;
      TFile.WriteAllText(ALGetModulePathA + '~tmp.json', LJSONValue.ToString);
      LStopWatch.Stop;
      ALDeleteFile(ALGetModulePathA + '~tmp.json');
      chart1.Series[2].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Save (JSON)');
      application.ProcessMessages;
      //----
      chart1.Series[2].AddY(0, 'Save (BSON)');
      application.ProcessMessages;
    finally
      LJSONValue.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DodwsJSONBench(Count: integer);
  begin
    //var LMemoryUsage := GetTotalMemoryAllocated;
    var LStopWatch := TstopWatch.StartNew;
    var LJSONValue:= TdwsJSONValue.ParseFile('..\sample.json');
    Try
      LStopWatch.Stop;
      chart1.Series[3].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Load (JSON)');
      application.ProcessMessages;
      //-----
      chart1.Series[3].AddY(0, 'Load (BSON)');
      //-----
      LStopWatch := TstopWatch.StartNew;
      _scrollAllNode(LJSONValue);
      LStopWatch.Stop;
      chart1.Series[3].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Scroll all nodes');
      application.ProcessMessages;
      //----
      //MemoMemoryUsed.Lines.Add('dwsJSON: ' + FormatFloat('0,',(GetTotalMemoryAllocated - LMemoryUsage)) + ' bytes');
      //application.ProcessMessages;
      //----
      for var I := 1 to count - 1 do
        TdwsJSONObject(LJSONValue).AddValue(ALLowerCase(ALRandomStrA(7)), '');
      //----
      LStopWatch := TstopWatch.StartNew;
      for var i := 1 to _iterationcount do
        LJSONValue.Items[AlLowerCase(ALRandomStrW(7))];
      LStopWatch.Stop;
      chart1.Series[3].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Find (unsorted)');
      application.ProcessMessages;
      //----
      chart1.Series[3].AddY(0, 'Find (sorted)');
      //----
      LStopWatch := TStopWatch.StartNew;
      TFile.WriteAllText(ALGetModulePathA + '~tmp.json', LJSONValue.ToString);
      LStopWatch.Stop;
      ALDeleteFile(ALGetModulePathA + '~tmp.json');
      chart1.Series[3].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Save (JSON)');
      application.ProcessMessages;
      //----
      chart1.Series[3].AddY(0, 'Save (BSON)');
      application.ProcessMessages;
    finally
      LJSONValue.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoSuperObjectBench(Count: integer);
  begin
    //var LMemoryUsage := GetTotalMemoryAllocated;
    var LStopWatch := TstopWatch.StartNew;
    var Lobj := TSuperObject.ParseFile('..\sample.json', false);
    LStopWatch.Stop;
    chart1.Series[4].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Load (JSON)');
    application.ProcessMessages;
    //-----
    chart1.Series[4].AddY(0, 'Load (BSON)');
    //-----
    LStopWatch := TstopWatch.StartNew;
    _scrollAllNode(Lobj);
    LStopWatch.Stop;
    chart1.Series[4].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'scroll all nodes');
    application.ProcessMessages;
    //----
    //MemoMemoryUsed.Lines.Add('SuperObject: ' + FormatFloat('0,',(GetTotalMemoryAllocated - LMemoryUsage)) + ' bytes');
    //application.ProcessMessages;
    //----
    for var I := 1 to count - 1 do
      Lobj.S[ALLowerCase(ALRandomStrA(7))] := '';
    //----
    LStopWatch := TstopWatch.StartNew;
    for var i := 1 to _iterationcount do
      Lobj[AlLowerCase(ALRandomStrW(7))];
    LStopWatch.Stop;
    chart1.Series[4].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Find (unsorted)');
    application.ProcessMessages;
    //----
    chart1.Series[4].AddY(0, 'Find (sorted)');
    //----
    LStopWatch := TStopWatch.StartNew;
    Lobj.SaveTo(string(ALGetModulePathA) + '~tmp.json');
    LStopWatch.Stop;
    ALDeleteFile(ALGetModulePathA + '~tmp.json');
    chart1.Series[4].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Save (JSON)');
    application.ProcessMessages;
    //----
    chart1.Series[4].AddY(0, 'Save (BSON)');
    application.ProcessMessages;
  end;

begin

  chart1.Series[0].Clear;
  chart1.Series[1].Clear;
  chart1.Series[2].Clear;
  chart1.Series[3].Clear;
  chart1.Series[4].Clear;
  if CheckBoxTALJsonDocJSON.Checked then _DoTALJsonDocJSONBench(StrToInt(SpinEditNbItems.Text));
  if CheckBoxTALJsonDocUJSON.Checked then _DoTALJsonDocUJSONBench(StrToInt(SpinEditNbItems.Text));
  if CheckBoxSystemJSON.Checked then _DoSystemJSONBench(StrToInt(SpinEditNbItems.Text));
  if CheckBoxdwsJSON.Checked then _DodwsJSONBench(StrToInt(SpinEditNbItems.Text));
  if CheckBoxSuperObject.Checked then _DoSuperObjectBench(StrToInt(SpinEditNbItems.Text));

end;

{*********************************************}
procedure TForm1.Button1Click(Sender: TObject);
var LBsonStr: AnsiString;
    LALJsonDocument: TALJSONDocumentA;
    LALJsonDocumentU: TALJSONDocumentW;
    i: integer;
begin
  LBsonStr := '';
  for I := 0 to MemoBSON.Lines.Count - 1 do
    LBsonStr := LBsonStr + AnsiChar(StrToInt(MemoBSON.Lines[i]));
  MemoJSON.Lines.Clear;
  MemoSaxModeEvents.lines.clear;

  if messageDlg('Use unicode version of TalJsonDoc (ie: TalJsonDocU)?', mtConfirmation, [TMsgDlgBtn.mbNo, TMsgDlgBtn.mbYes], 0) = MrNo then begin

    //exemple 1 load the JSON doc in memory
    LALJsonDocument := TALJSONDocumentA.Create;
    try
      LALJsonDocument.LoadFromBSONString(LBsonStr);
      LALJsonDocument.Options := [doNodeAutoIndent];
      MemoJSON.Lines.Text := String(LALJsonDocument.JSON);
    finally
      LALJsonDocument.Free;
    end;

    //exemple 2 load the JSON doc in SAX MODE
    LALJsonDocument := TALJSONDocumentA.Create;
    try
      LALJsonDocument.onParseText := procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType)
                                     begin
                                       case NodeSubType of
                                         nstFloat: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + string(ALFloatToStrA(Args[0].VExtended^, ALDefaultFormatSettingsA)));
                                         nstText: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
                                         nstObject: ;
                                         nstArray: ;
                                         nstObjectID: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + 'ObjectId("'+string(ALBinToHexA(ansiString(Args[0].VAnsiString)))+'")');
                                         nstBoolean: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + String(ALBoolToStrA(Args[0].VBoolean,'true','false')));
                                         nstDateTime: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + string(ALFormatDateTimeA('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Args[0].VExtended^, ALDefaultFormatSettingsA)));
                                         nstNull: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + 'null');
                                         nstRegEx: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
                                         nstBinary: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + 'BinData('+inttostr(Args[1].VInteger)+', "'+string(ansiString(ALBase64EncodeString(ansiString(Args[0].VAnsiString))))+'")');
                                         nstJavascript: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
                                         nstInt32: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + 'NumberInt('+inttostr(Args[0].VInteger)+')');
                                         nstTimestamp: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + 'Timestamp('+inttostr(int64(cardinal(Args[0].VInteger)))+', '+inttostr(int64(cardinal(Args[1].VInteger)))+')');
                                         nstInt64: MemoSaxModeEvents.Lines.Add(String(Path) + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
                                       end;
                                     end;
      LALJsonDocument.LoadFromBSONString(LBsonStr, true{saxMode});
    finally
      LALJsonDocument.Free;
    end;

  end
  else begin


    //exemple 1 load the JSON doc in memory
    LALJsonDocumentU := TALJSONDocumentW.Create;
    try
      LALJsonDocumentU.LoadFromBSONBytes(BytesOf(LBsonStr));
      LALJsonDocumentU.Options := [doNodeAutoIndent];
      MemoJSON.Lines.Text := String(LALJsonDocumentU.JSON);
    finally
      LALJsonDocumentU.Free;
    end;

    //exemple 2 load the JSON doc in SAX MODE
    LALJsonDocumentU := TALJSONDocumentW.Create;
    try
      LALJsonDocumentU.onParseText := procedure (Sender: TObject; const Path: String; const name: String; const Args: array of const; NodeSubType: TALJSONNodeSubType)
                                       begin
                                         case NodeSubType of
                                           nstFloat: MemoSaxModeEvents.Lines.Add(Path + '=' + ALFloatToStrW(Args[0].VExtended^, ALDefaultFormatSettingsW));
                                           nstText: MemoSaxModeEvents.Lines.Add(Path + '=' + String(Args[0].VUnicodeString));
                                           nstObject: ;
                                           nstArray: ;
                                           nstObjectID: MemoSaxModeEvents.Lines.Add(Path + '=' + 'ObjectId("'+String(Args[0].VUnicodeString)+'")');
                                           nstBoolean: MemoSaxModeEvents.Lines.Add(Path + '=' + ALBoolToStrW(Args[0].VBoolean,'true','false'));
                                           nstDateTime: MemoSaxModeEvents.Lines.Add(Path + '=' + ALFormatDateTimeW('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Args[0].VExtended^, ALDefaultFormatSettingsW));
                                           nstNull: MemoSaxModeEvents.Lines.Add(Path + '=' + 'null');
                                           nstRegEx: MemoSaxModeEvents.Lines.Add(Path + '=' + String(Args[0].VUnicodeString));
                                           nstBinary: MemoSaxModeEvents.Lines.Add(Path + '=' + 'BinData('+ALIntToStrW(Args[1].VInteger)+', "'+String(Args[0].VunicodeString)+'")');
                                           nstJavascript: MemoSaxModeEvents.Lines.Add(Path + '=' + String(Args[0].VUnicodeString));
                                           nstInt32: MemoSaxModeEvents.Lines.Add(Path + '=' + 'NumberInt('+ALIntToStrW(Args[0].VInteger)+')');
                                           nstTimestamp: MemoSaxModeEvents.Lines.Add(Path + '=' + 'Timestamp('+ALIntToStrW(int64(cardinal(Args[0].VInteger)))+', '+ALIntToStrW(int64(cardinal(Args[1].VInteger)))+')');
                                           nstInt64: MemoSaxModeEvents.Lines.Add(Path + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
                                         end;
                                       end;
      LALJsonDocumentU.LoadFromBSONBytes(BytesOf(LBsonStr), true{saxMode});
    finally
      LALJsonDocumentU.Free;
    end;

  end;

end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
