unit Unit1;

interface

uses
  Windows, system.Types, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Alcinoe.StringUtils, Alcinoe.JSONDoc, ExtCtrls,
  Alcinoe.StringList, Shellapi, Vcl.Dialogs,
  Contnrs, Alcinoe.Files, diagnostics, DBXplatform, IOUtils,
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
    ButtonLoadJsonDocument: TButton;
    MemoJson: TMemo;
    ButtonCreateDynamicallyJsonDocument: TButton;
    MemoSaxModeEvents: TMemo;
    MemoBSON: TMemo;
    ButtonLoadJsonFromBson: TButton;
    Label2: TLabel;
    Chart1: TChart;
    Series1: TBarSeries;
    Series2: TBarSeries;
    Series3: TBarSeries;
    Series4: TBarSeries;
    BtnRunBenchmark: TButton;
    SpinEditNbItems: TSpinEdit;
    CheckBoxTALJsonDocJSON: TCheckBox;
    CheckBoxTALJsonDocWJSON: TCheckBox;
    CheckBoxSystemJSON: TCheckBox;
    CheckBoxDwsJSON: TCheckBox;
    Label3: TLabel;
    Label4: TLabel;
    CheckBoxUseTALJsonDocumentW: TCheckBox;
    Label5: TLabel;
    BtnMemoryConsumption: TButton;
    procedure ButtonLoadJsonDocumentClick(Sender: TObject);
    procedure ButtonCreateDynamicallyJsonDocumentClick(Sender: TObject);
    procedure ButtonLoadJsonFromBsonClick(Sender: TObject);
    procedure BtnRunBenchmarkClick(Sender: TObject);
    procedure BtnMemoryConsumptionClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{**************************************}
function GetTotalMemoryAllocated: int64;
var LMemoryState: TMemoryManagerState;
    i: Integer;
begin

  // get memory manager state
  {$WARNINGS OFF}
  GetMemoryManagerState(LMemoryState);
  {$WARNINGS ON}

  // take the allocated size
  Result := 0;
  with LMemoryState do begin
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

{************************************************************}
procedure TForm1.ButtonLoadJsonDocumentClick(Sender: TObject);
begin

  //clear Memos
  MemoSaxModeEvents.Lines.Clear;
  MemoBSON.lines.Clear;
  MemoSaxModeEvents.Lines.Clear;

  //Use the ansiString version
  if not CheckBoxUseTALJsonDocumentW.Checked then begin

    //exemple 1 load the JSON doc in memory
    var LALJsonDocumentA := TALJSONDocumentA.CreateFromJSONString(AnsiString(MemoJSON.Lines.Text));
    try
      var LJsonStr: AnsiString;
      LALJsonDocumentA.SaveToJSONString(LJsonStr, [soNodeAutoIndent]);
      MemoJSON.Lines.Text := String(LJSONStr);

      var LBsonStr := LALJsonDocumentA.BSON;
      if LBsonStr <> '' then
        MemoBSON.Lines.Text := String(ALBinToHexA(LALJsonDocumentA.BSON));
    finally
      LALJsonDocumentA.Free;
    end;

    //exemple 2 load the JSON doc in SAX MODE
    TALJSONDocumentA.ParseJSONString(
      AnsiString(MemoJSON.Lines.Text),
      //--
      procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType)
      begin
        case NodeSubType of
          nstFloat:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + string(ALFloatToStrA(Args[0].VExtended^, ALDefaultFormatSettingsA)));
          nstText:       MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
          nstObject: ;
          nstArray: ;
          nstObjectID:   MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + 'ObjectId("'+string(ALBinToHexA(ansiString(Args[0].VAnsiString)))+'")');
          nstBoolean:    MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + String(ALBoolToStrA(Args[0].VBoolean,'true','false')));
          nstDateTime:   MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + string(ALFormatDateTimeA('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Args[0].VExtended^, ALDefaultFormatSettingsA)));
          nstNull:       MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + 'null');
          nstRegEx:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
          nstBinary:     MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + 'BinData('+inttostr(Args[1].VInteger)+', "'+string(ansiString(ALBase64EncodeString(ansiString(Args[0].VAnsiString))))+'")');
          nstJavascript: MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
          nstInt32:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + 'NumberInt('+inttostr(Args[0].VInteger)+')');
          nstTimestamp:  MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + 'Timestamp('+inttostr(int64(cardinal(Args[0].VInteger)))+', '+inttostr(int64(cardinal(Args[1].VInteger)))+')');
          nstInt64:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
        end;
      end{onParseText},
      //--
      procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
      begin
        MemoSaxModeEvents.Lines.Add('STARTOBJECT  =>  ' + String(Name));
      end{onParseStartObject},
      //--
      procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
      begin
        MemoSaxModeEvents.Lines.Add('ENDOBJECT    =>  ' + String(Name));
      end{onParseEndObject},
      //--
      procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
      begin
        MemoSaxModeEvents.Lines.Add('STARTARRAY   =>  ' + String(Name));
      end{onParseStartArray},
      //--
      procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
      begin
        MemoSaxModeEvents.Lines.Add('ENDARRAY     =>  ' + String(Name));
      end{onParseEndArray});

  end

  //Use the unicode String version
  else begin

    //exemple 1 load the JSON doc in memory
    var LALJsonDocumentW := TALJSONDocumentW.CreateFromJSONString(MemoJSON.Lines.Text);
    try
      var LJsonStr: String;
      LALJsonDocumentW.SaveToJSONString(LJsonStr, [soNodeAutoIndent]);
      MemoJSON.Lines.Text := LJSONStr;

      var LBsonBytes := LALJsonDocumentW.BSON;
      if length(LBsonBytes) > 0 then
        MemoBSON.Lines.Text := ALBinToHexW(LALJsonDocumentW.BSON);
    finally
      LALJsonDocumentW.Free;
    end;

    //exemple 2 load the JSON doc in SAX MODE
    TALJSONDocumentW.ParseJSONString(
      MemoJSON.Lines.Text,
      //--
      procedure (Sender: TObject; const Path: String; const name: String; const Args: array of const; NodeSubType: TALJSONNodeSubType)
      begin
        case NodeSubType of
          nstFloat:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + ALFloatToStrW(Args[0].VExtended^, ALDefaultFormatSettingsW));
          nstText:       MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + String(Args[0].VUnicodeString));
          nstObject: ;
          nstArray: ;
          nstObjectID:   MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'ObjectId("'+string(Args[0].VUnicodeString)+'")');
          nstBoolean:    MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + ALBoolToStrW(Args[0].VBoolean,'true','false'));
          nstDateTime:   MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + ALFormatDateTimeW('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Args[0].VExtended^, ALDefaultFormatSettingsW));
          nstNull:       MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'null');
          nstRegEx:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + String(Args[0].VUnicodeString));
          nstBinary:     MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'BinData('+ALIntToStrW(Args[1].VInteger)+', "'+String(Args[0].VunicodeString)+'")');
          nstJavascript: MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + String(Args[0].VUnicodeString));
          nstInt32:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'NumberInt('+ALIntToStrW(Args[0].VInteger)+')');
          nstTimestamp:  MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'Timestamp('+ALIntToStrW(int64(cardinal(Args[0].VInteger)))+', '+ALIntToStrW(int64(cardinal(Args[1].VInteger)))+')');
          nstInt64:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
        end;
      end{onParseText},
      //--
      procedure (Sender: TObject; const Path: String; const Name: String)
      begin
        MemoSaxModeEvents.Lines.Add('STARTOBJECT  =>  ' + Name);
      end{onParseStartObject},
      //--
      procedure (Sender: TObject; const Path: String; const Name: String)
      begin
        MemoSaxModeEvents.Lines.Add('ENDOBJECT    =>  ' + Name);
      end{onParseEndObject},
      //--
      procedure (Sender: TObject; const Path: String; const Name: String)
      begin
        MemoSaxModeEvents.Lines.Add('STARTARRAY   =>  ' + Name);
      end{onParseStartArray},
      //--
      procedure (Sender: TObject; const Path: String; const Name: String)
      begin
        MemoSaxModeEvents.Lines.Add('ENDARRAY     =>  ' + Name);
      end{onParseEndArray});

  end;

end;

{*************************************************************************}
procedure TForm1.ButtonCreateDynamicallyJsonDocumentClick(Sender: TObject);
begin

  //clear Memos
  MemoJSON.Lines.Clear;
  MemoBSON.Lines.Clear;
  MemoSaxModeEvents.Lines.Clear;

  //Use the ansiString version
  if not CheckBoxUseTALJsonDocumentW.Checked then begin

    var LALJsonDocumentA:= TALJSONDocumentA.Create;
    Try
      LALJsonDocumentA.addchild('_id').float := 1.32;
      with LALJsonDocumentA.addchild('name', ntObject) do begin
        addchild('first').text := 'John';
        addchild('last').text := 'Backus';
      end;
      LALJsonDocumentA.addchild('birth').datetime := Now;
      with LALJsonDocumentA.addchild('contribs', ntArray) do begin
        addchild.text := 'Fortran';
        addchild.text := 'ALGOL';
        addchild.text := 'Backus-Naur Form';
        addchild.text := 'FP';
      end;
      with LALJsonDocumentA.addchild('awards', ntArray) do begin
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
      LALJsonDocumentA.addchild('spouse');
      LALJsonDocumentA.addchild('address', ntObject);
      LALJsonDocumentA.addchild('phones', ntArray);
      with LALJsonDocumentA.AddChild('regex') do begin
        RegEx := '<TAG\b[^>]*>(.*?)</TAG>';
        RegExOptions := [preMultiLine, preCaseLess];
      end;
      with LALJsonDocumentA.AddChild('binary') do begin
        binary := #1#2#3#4#5;
        BinarySubType := 0;
      end;
      LALJsonDocumentA.AddChild('ObjectId').ObjectId := #1#2#3#4#5#6#7#8#9#0#1#2;

      var LJsonStr: AnsiString;
      LALJsonDocumentA.SaveToJSONString(LJsonStr, [soNodeAutoIndent]);
      MemoJSON.Lines.Text := String(LJSONStr);

      MemoBSON.Lines.Text := String(ALBinToHexA(LALJsonDocumentA.BSON));
    finally
      LALJsonDocumentA.Free;
    end;

  end

  //Use the unicode String version
  else begin

    var LALJsonDocumentW:= TALJSONDocumentW.Create;
    Try
      LALJsonDocumentW.addchild('_id').float := 1.32;
      with LALJsonDocumentW.addchild('name', ntObject) do begin
        addchild('first').text := 'John';
        addchild('last').text := 'Backus';
      end;
      LALJsonDocumentW.addchild('birth').datetime := Now;
      with LALJsonDocumentW.addchild('contribs', ntArray) do begin
        addchild.text := 'Fortran';
        addchild.text := 'ALGOL';
        addchild.text := 'Backus-Naur Form';
        addchild.text := 'FP';
      end;
      with LALJsonDocumentW.addchild('awards', ntArray) do begin
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
      LALJsonDocumentW.addchild('spouse');
      LALJsonDocumentW.addchild('address', ntObject);
      LALJsonDocumentW.addchild('phones', ntArray);
      with LALJsonDocumentW.AddChild('regex') do begin
        RegEx := '<TAG\b[^>]*>(.*?)</TAG>';
        RegExOptions := [preMultiLine, preCaseLess];
      end;
      var LBytes: Tbytes;
      with LALJsonDocumentW.AddChild('binary') do begin
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
      LALJsonDocumentW.AddChild('ObjectId').ObjectId := ALBinToHexW(LBytes);

      var LJsonStr: String;
      LALJsonDocumentW.SaveToJSONString(LJsonStr, [soNodeAutoIndent]);
      MemoJSON.Lines.Text := LJSONStr;

      MemoBSON.Lines.Text := ALBinToHexW(LALJsonDocumentW.BSON);
    finally
      LALJsonDocumentW.Free;
    end;

  end;
end;

{************************************************************}
procedure TForm1.ButtonLoadJsonFromBsonClick(Sender: TObject);
begin

  //init LBsonStr
  var LBsonStr: AnsiString := ALHexToBin(ansiString(ALTrim(MemoBSON.Text)));

  //clear Memos
  MemoJSON.Lines.Clear;
  MemoSaxModeEvents.lines.clear;

  //Use the ansiString version
  if not CheckBoxUseTALJsonDocumentW.Checked then begin

    //exemple 1 load the JSON doc in memory
    var LALJsonDocumentA := TALJSONDocumentA.Create;
    try
      LALJsonDocumentA.LoadFromBSONString(LBsonStr);

      var LJsonStr: AnsiString;
      LALJsonDocumentA.SaveToJSONString(LJsonStr, [soNodeAutoIndent]);
      MemoJSON.Lines.Text := String(LJSONStr);
    finally
      LALJsonDocumentA.Free;
    end;

    //exemple 2 load the JSON doc in SAX MODE
    TALJSONDocumentA.ParseBSONString(
      LBsonStr,
      //--
      procedure (Sender: TObject; const Path: AnsiString; const name: AnsiString; const Args: array of const; NodeSubType: TALJSONNodeSubType)
      begin
        case NodeSubType of
          nstFloat:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + string(ALFloatToStrA(Args[0].VExtended^, ALDefaultFormatSettingsA)));
          nstText:       MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
          nstObject: ;
          nstArray: ;
          nstObjectID:   MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + 'ObjectId("'+string(ALBinToHexA(ansiString(Args[0].VAnsiString)))+'")');
          nstBoolean:    MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + String(ALBoolToStrA(Args[0].VBoolean,'true','false')));
          nstDateTime:   MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + string(ALFormatDateTimeA('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Args[0].VExtended^, ALDefaultFormatSettingsA)));
          nstNull:       MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + 'null');
          nstRegEx:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
          nstBinary:     MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + 'BinData('+inttostr(Args[1].VInteger)+', "'+string(ansiString(ALBase64EncodeString(ansiString(Args[0].VAnsiString))))+'")');
          nstJavascript: MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + string(ansiString(Args[0].VAnsiString)));
          nstInt32:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + 'NumberInt('+inttostr(Args[0].VInteger)+')');
          nstTimestamp:  MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + 'Timestamp('+inttostr(int64(cardinal(Args[0].VInteger)))+', '+inttostr(int64(cardinal(Args[1].VInteger)))+')');
          nstInt64:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + String(Path) + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
        end;
      end{onParseText},
      //--
      procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
      begin
        MemoSaxModeEvents.Lines.Add('STARTOBJECT  =>  ' + String(Name));
      end{onParseStartObject},
      //--
      procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
      begin
        MemoSaxModeEvents.Lines.Add('ENDOBJECT    =>  ' + String(Name));
      end{onParseEndObject},
      //--
      procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
      begin
        MemoSaxModeEvents.Lines.Add('STARTARRAY   =>  ' + String(Name));
      end{onParseStartArray},
      //--
      procedure (Sender: TObject; const Path: AnsiString; const Name: AnsiString)
      begin
        MemoSaxModeEvents.Lines.Add('ENDARRAY     =>  ' + String(Name));
      end{onParseEndArray});

  end

  //Use the unicode String version
  else begin

    //exemple 1 load the JSON doc in memory
    var LALJsonDocumentW := TALJSONDocumentW.Create;
    try
      LALJsonDocumentW.LoadFromBSONBytes(BytesOf(LBsonStr));

      var LJsonStr: String;
      LALJsonDocumentW.SaveToJSONString(LJsonStr, [soNodeAutoIndent]);
      MemoJSON.Lines.Text := String(LJSONStr);
    finally
      LALJsonDocumentW.Free;
    end;

    //exemple 2 load the JSON doc in SAX MODE
    TALJSONDocumentW.ParseBSONBytes(
      BytesOf(LBsonStr),
      //--
      procedure (Sender: TObject; const Path: String; const name: String; const Args: array of const; NodeSubType: TALJSONNodeSubType)
      begin
        case NodeSubType of
          nstFloat:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + ALFloatToStrW(Args[0].VExtended^, ALDefaultFormatSettingsW));
          nstText:       MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + String(Args[0].VUnicodeString));
          nstObject: ;
          nstArray: ;
          nstObjectID:   MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'ObjectId("'+string(Args[0].VUnicodeString)+'")');
          nstBoolean:    MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + ALBoolToStrW(Args[0].VBoolean,'true','false'));
          nstDateTime:   MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + ALFormatDateTimeW('''ISODate("''yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z")''', Args[0].VExtended^, ALDefaultFormatSettingsW));
          nstNull:       MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'null');
          nstRegEx:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + String(Args[0].VUnicodeString));
          nstBinary:     MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'BinData('+ALIntToStrW(Args[1].VInteger)+', "'+String(Args[0].VunicodeString)+'")');
          nstJavascript: MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + String(Args[0].VUnicodeString));
          nstInt32:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'NumberInt('+ALIntToStrW(Args[0].VInteger)+')');
          nstTimestamp:  MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'Timestamp('+ALIntToStrW(int64(cardinal(Args[0].VInteger)))+', '+ALIntToStrW(int64(cardinal(Args[1].VInteger)))+')');
          nstInt64:      MemoSaxModeEvents.Lines.Add('TEXT         =>  ' + Path + '=' + 'NumberLong('+inttostr(Args[0].VInt64^)+')');
        end;
      end{onParseText},
      //--
      procedure (Sender: TObject; const Path: String; const Name: String)
      begin
        MemoSaxModeEvents.Lines.Add('STARTOBJECT  =>  ' + Name);
      end{onParseStartObject},
      //--
      procedure (Sender: TObject; const Path: String; const Name: String)
      begin
        MemoSaxModeEvents.Lines.Add('ENDOBJECT    =>  ' + Name);
      end{onParseEndObject},
      //--
      procedure (Sender: TObject; const Path: String; const Name: String)
      begin
        MemoSaxModeEvents.Lines.Add('STARTARRAY   =>  ' + Name);
      end{onParseStartArray},
      //--
      procedure (Sender: TObject; const Path: String; const Name: String)
      begin
        MemoSaxModeEvents.Lines.Add('ENDARRAY     =>  ' + Name);
      end{onParseEndArray});

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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoTALJsonDocABench(Count: integer);
  begin
    //var LMemoryUsage := GetTotalMemoryAllocated;
    var LALJsonDocumentA:= TALJSONDocumentA.Create;
    Try
      var LStopWatch := TstopWatch.StartNew;
      LALJsonDocumentA.LoadFromJsonFile('..\..\sample.json');
      LStopWatch.Stop;
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Load (JSON)');
      application.ProcessMessages;
      //----
      LALJsonDocumentA.ChildNodes.Clear;
      LStopWatch := TstopWatch.StartNew;
      LALJsonDocumentA.LoadFromBsonFile('..\..\sample.bson');
      LStopWatch.Stop;
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Load (BSON)');
      application.ProcessMessages;
      //----
      LStopWatch := TstopWatch.StartNew;
      _scrollAllNode(LALJsonDocumentA);
      LStopWatch.Stop;
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Scroll all nodes');
      application.ProcessMessages;
      //----
      //MemoMemoryUsed.Lines.Add('TALJSONDocumentA: ' + FormatFloat('0,',(GetTotalMemoryAllocated - LMemoryUsage)) + ' bytes');
      //application.ProcessMessages;
      //----
      for var I := 1 to count - 1 do
        LALJsonDocumentA.AddChild(ALLowerCase(ALRandomStrA(7)));
      //----
      LStopWatch := TstopWatch.StartNew;
      for var i := 1 to _iterationcount do
        LALJsonDocumentA.ChildNodes.FindNode(ALLowerCase(ALRandomStrA(7)));
      LStopWatch.Stop;
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Find (unsorted)');
      application.ProcessMessages;
      //----
      LALJsonDocumentA.ChildNodes.SetSorted(True{Value}, true{Recurse});
      LStopWatch := TstopWatch.StartNew;
      for var i := 1 to _iterationcount do
        LALJsonDocumentA.ChildNodes.FindNode(ALLowerCase(ALRandomStrA(7)));
      LStopWatch.Stop;
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Find (sorted)');
      application.ProcessMessages;
      //----
      LStopWatch := TStopWatch.StartNew;
      LALJsonDocumentA.SaveToJsonFile(ALGetModulePathW + '~tmp.json');
      LStopWatch.Stop;
      Tfile.Delete(ALGetModulePathW + '~tmp.json');
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Save (JSON)');
      application.ProcessMessages;
      //----
      LStopWatch := TStopWatch.StartNew;
      LALJsonDocumentA.SaveToBsonFile(ALGetModulePathW + '~tmp.bson');
      LStopWatch.Stop;
      Tfile.Delete(ALGetModulePathW + '~tmp.bson');
      chart1.Series[0].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Save (BSON)');
      application.ProcessMessages;
    finally
      LALJsonDocumentA.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoTALJsonDocWBench(Count: integer);
  begin
    //var LMemoryUsage := GetTotalMemoryAllocated;
    var LALJsonDocumentW:= TALJSONDocumentW.Create;
    Try
      var LStopWatch := TstopWatch.StartNew;
      LALJsonDocumentW.LoadFromJsonFile('..\..\sample.json');
      LStopWatch.Stop;
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Load (JSON)');
      application.ProcessMessages;
      //----
      LALJsonDocumentW.ChildNodes.Clear;
      LStopWatch := TstopWatch.StartNew;
      LALJsonDocumentW.LoadFromBsonFile('..\..\sample.bson');
      LStopWatch.Stop;
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Load (BSON)');
      application.ProcessMessages;
      //----
      LStopWatch := TstopWatch.StartNew;
      _scrollAllNode(LALJsonDocumentW);
      LStopWatch.Stop;
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Scroll all nodes');
      application.ProcessMessages;
      //----
      //MemoMemoryUsed.Lines.Add('TALJSONDocumentW: ' + FormatFloat('0,',(GetTotalMemoryAllocated - LMemoryUsage)) + ' bytes');
      //application.ProcessMessages;
      //----
      for var I := 1 to count - 1 do
        LALJsonDocumentW.AddChild(ALLowerCase(ALRandomStrW(7)));
      //----
      LStopWatch := TstopWatch.StartNew;
      for var i := 1 to _iterationcount do
        LALJsonDocumentW.ChildNodes.FindNode(ALLowerCase(ALRandomStrW(7)));
      LStopWatch.Stop;
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Find (unsorted)');
      application.ProcessMessages;
      //----
      LALJsonDocumentW.ChildNodes.SetSorted(True{Value}, true{Recurse});
      LStopWatch := TstopWatch.StartNew;
      for var i := 1 to _iterationcount do
        LALJsonDocumentW.ChildNodes.FindNode(ALLowerCase(ALRandomStrW(7)));
      LStopWatch.Stop;
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Find (sorted)');
      application.ProcessMessages;
      //----
      LStopWatch := TStopWatch.StartNew;
      LALJsonDocumentW.SaveToJsonFile(ALGetModulePathW + '~tmp.json');
      LStopWatch.Stop;
      Tfile.Delete(ALGetModulePathW + '~tmp.json');
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Save (JSON)');
      application.ProcessMessages;
      //----
      LStopWatch := TStopWatch.StartNew;
      LALJsonDocumentW.SaveToBsonFile(ALGetModulePathW + '~tmp.bson');
      LStopWatch.Stop;
      Tfile.Delete(ALGetModulePathW + '~tmp.bson');
      chart1.Series[1].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Save (BSON)');
      application.ProcessMessages;
    finally
      LALJsonDocumentW.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoSystemJSONBench(Count: integer);
  begin
    //var LMemoryUsage := GetTotalMemoryAllocated;
    var LStopWatch := TstopWatch.StartNew;
    var LJSONValue:= TJSONObject.ParseJSONValue(TFile.ReadAllText('..\..\sample.json'));
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
        TJSONObject(LJSONValue).AddPair(ALLowerCase(ALRandomStrW(7)), '');
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
      TFile.WriteAllText(ALGetModulePathW + '~tmp.json', LJSONValue.ToString);
      LStopWatch.Stop;
      Tfile.Delete(ALGetModulePathW + '~tmp.json');
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
    var LJSONValue:= TdwsJSONValue.ParseFile('..\..\sample.json');
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
        TdwsJSONObject(LJSONValue).AddValue(ALLowerCase(ALRandomStrW(7)), '');
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
      TFile.WriteAllText(ALGetModulePathW + '~tmp.json', LJSONValue.ToString);
      LStopWatch.Stop;
      Tfile.Delete(ALGetModulePathW + '~tmp.json');
      chart1.Series[3].AddY(LStopWatch.Elapsed.TotalMilliseconds, 'Save (JSON)');
      application.ProcessMessages;
      //----
      chart1.Series[3].AddY(0, 'Save (BSON)');
      application.ProcessMessages;
    finally
      LJSONValue.Free;
    end;
  end;

begin

  chart1.Series[0].Clear;
  chart1.Series[1].Clear;
  chart1.Series[2].Clear;
  chart1.Series[3].Clear;
  if CheckBoxTALJsonDocJSON.Checked then _DoTALJsonDocABench(StrToInt(SpinEditNbItems.Text));
  if CheckBoxTALJsonDocWJSON.Checked then _DoTALJsonDocWBench(StrToInt(SpinEditNbItems.Text));
  if CheckBoxSystemJSON.Checked then _DoSystemJSONBench(StrToInt(SpinEditNbItems.Text));
  if CheckBoxdwsJSON.Checked then _DodwsJSONBench(StrToInt(SpinEditNbItems.Text));

end;

{**********************************************************}
procedure TForm1.BtnMemoryConsumptionClick(Sender: TObject);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoTALJsonDocABench(Count: integer);
  begin
    var LMemoryUsage := GetTotalMemoryAllocated;
    var LALJsonDocumentA:= TALJSONDocumentA.Create;
    Try
      LALJsonDocumentA.LoadFromJsonFile('..\..\sample.json');
      for var i := 1 to 2000000 do
        LALJsonDocumentA.AddChild('qsdkqlskdjqsdlkqs').Text := 'sdoijsodifsqsmdlqsdmlqmsdlqmsldmqsld';
      chart1.Series[0].AddY(GetTotalMemoryAllocated - LMemoryUsage, 'Memory consumption');
      application.ProcessMessages;
    finally
      LALJsonDocumentA.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoTALJsonDocWBench(Count: integer);
  begin
    var LMemoryUsage := GetTotalMemoryAllocated;
    var LALJsonDocumentW:= TALJSONDocumentW.Create;
    Try
      LALJsonDocumentW.LoadFromJsonFile('..\..\sample.json');
      for var i := 1 to 2000000 do
        LALJsonDocumentW.AddChild('qsdkqlskdjqsdlkqs').Text := 'sdoijsodifsqsmdlqsdmlqmsdlqmsldmqsld';
      chart1.Series[1].AddY(GetTotalMemoryAllocated - LMemoryUsage, 'Memory consumption');
      application.ProcessMessages;
    finally
      LALJsonDocumentW.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DoSystemJSONBench(Count: integer);
  begin
    var LMemoryUsage := GetTotalMemoryAllocated;
    var LJSONValue:= TJSONObject.ParseJSONValue(TFile.ReadAllText('..\..\sample.json')) as TJSONObject;
    Try
      for var i := 1 to 2000000 do
        LJSONValue.AddPair('qsdkqlskdjqsdlkqs', 'sdoijsodifsqsmdlqsdmlqmsdlqmsldmqsld');
      chart1.Series[2].AddY(GetTotalMemoryAllocated - LMemoryUsage, 'Memory consumption');
      application.ProcessMessages;
    finally
      LJSONValue.Free;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DodwsJSONBench(Count: integer);
  begin
    var LMemoryUsage := GetTotalMemoryAllocated;
    var LJSONValue:= TdwsJSONValue.ParseFile('..\..\sample.json') as TdwsJSONObject;
    Try
      chart1.Series[3].AddY(GetTotalMemoryAllocated - LMemoryUsage, 'Memory consumption');
      for var i := 1 to 2000000 do
        LJSONValue.AddValue('qsdkqlskdjqsdlkqs', 'sdoijsodifsqsmdlqsdmlqmsdlqmsldmqsld');
      application.ProcessMessages;
    finally
      LJSONValue.Free;
    end;
  end;

begin

  chart1.Series[0].Clear;
  chart1.Series[1].Clear;
  chart1.Series[2].Clear;
  chart1.Series[3].Clear;
  if CheckBoxTALJsonDocJSON.Checked then _DoTALJsonDocABench(StrToInt(SpinEditNbItems.Text));
  if CheckBoxTALJsonDocWJSON.Checked then _DoTALJsonDocWBench(StrToInt(SpinEditNbItems.Text));
  if CheckBoxSystemJSON.Checked then _DoSystemJSONBench(StrToInt(SpinEditNbItems.Text));
  if CheckBoxdwsJSON.Checked then _DodwsJSONBench(StrToInt(SpinEditNbItems.Text));

end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
