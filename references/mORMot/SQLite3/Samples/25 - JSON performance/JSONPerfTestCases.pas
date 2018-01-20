/// benchmarks of JSON process, using several librairies (including mORMot)
unit JSONPerfTestCases;

interface

{$I Synopse.inc}

// standard slow-as-hell JSON library as part of the Delphi RTL
{$define USEDBXJSON}

// download from https://code.google.com/p/superobject
{.$define USESUPEROBJECT}

// download from https://code.google.com/p/x-superobject
{.$define USEXSUPEROBJECT}

// download from http://sourceforge.net/projects/qdac3
{.$define USEQDAC}

// download from https://code.google.com/p/dwscript
{.$define USEDWSJSON}

// download from https://github.com/ahausladen/JsonDataObjects
{.$define USEJDO}


{$ifdef USEXSUPEROBJECT}
  {$undef USESUPEROBJECT} // both libraries are exclusive! :(
{$endif}
{$ifdef CPU64}
  {$undef USESUPEROBJECT}   // SuperObject  just explodes under Win64 :(
  {$undef USEXSUPEROBJECT}  // XSuperObject just explodes under Win64 :(
{$endif}
{$ifndef ISDELPHI2010}      // libraries not available before Delphi 2010
  {$undef USEXSUPEROBJECT}
  {$undef USEQDAC}
  {$undef USEDWSJSON}
  {$undef USEJDO}
{$endif}
{$ifndef ISDELPHIXE}
  {$undef USEDBXJSON}  // Delphi 2010 DBXJSON is just buggy and not workable
{$endif}

{$define TESTBSON}

{$ifdef ISDELPHI2010}
  // undefine to test our text-based RTTI
  {$define USEENHANCEDRTTIFORRECORDS}
{$endif}

uses
  {$I SynDprUses.inc} // link FastMM4 for older versions of Delphi
  Windows,
  SysUtils,
  Classes,
  Variants,
  {$ifdef ISDELPHI2010}
  Generics.Collections,
  {$else}
  Contnrs,
  {$endif}
  {$ifdef USEXSUPEROBJECT}
  xsuperobject,
  {$endif}
  {$ifdef USESUPEROBJECT}
  superobject,
  {$endif}
  {$ifdef USEDWSJSON}
  dwsJSON,
  {$endif}
  {$ifdef USEJDO}
  JsonDataObjects,
  {$endif}
  {$ifdef USEDBXJSON}
  {$ifdef ISDELPHIXE6}
  JSON,
  {$else}
  DBXJSON,
  {$endif}
  {$endif}
  {$ifdef USEQDAC}
  qjson,
  {$endif}
  SynCrtSock,
  SynZip,
  {$ifdef TESTBSON}
  SynMongoDB,
  {$endif}
  mORMot,
  SynCommons,
  SynTests,
  SynCrossPlatformJSON;


const
  // number of iterations for TTestJSONBenchmarking.SmallContent
  SAMPLE_JSON_1_COUNT = 50000;

type
  TTestJSONBenchmarking = class(TSynTestsLogged)
  published
    procedure SmallContent;
    procedure BigContent;
  end;

  TTestSynopseRecord = class(TSynTestCase)
  published
    procedure Read;
    procedure Access;
    procedure Write;
  end;

  TTestSynopseVariant = class(TSynTestCase)
  published
    procedure Read;
    procedure AccessDirect;
    procedure AccessLateBinding;
    procedure Write;
  end;

  TTestSynopseCrossPlatformVariant = class(TSynTestCase)
  published
    procedure Read;
    procedure AccessDirect;
    procedure AccessLateBinding;
    procedure Write;
  end;

  {$ifdef USESUPEROBJECT}
  {$ifdef ISDELPHI2010} // TSuperRttiContext expects enhanced RTTI
  TTestSuperObjectRecord = class(TSynTestCase)
  published
    procedure Read;
    procedure Access;
    procedure Write;
  end;
  {$endif}
  TTestSuperObjectProperties = class(TSynTestCase)
  published
    procedure Read;
    procedure Access;
    procedure Write;
  end;
  {$endif USESUPEROBJECT}

  {$ifdef USEXSUPEROBJECT}
  {$ifdef ISDELPHI2010} // TSuperRttiContext expects enhanced RTTI
  TTestXSuperObjectRecord = class(TSynTestCase)
  published
    procedure Read;
    procedure Access;
    procedure Write;
  end;
  {$endif}
  TTestXSuperObjectProperties = class(TSynTestCase)
  published
    procedure Read;
    procedure Access;
    procedure Write;
  end;
  {$endif USEXSUPEROBJECT}

  {$ifdef USEDWSJSON}
  TTestdwsJSON = class(TSynTestCase)
  published
    procedure Read;
    procedure Access;
    procedure Write;
  end;
  {$endif}

  {$ifdef USEJDO}
  TTestJsonDataObjects = class(TSynTestCase)
  published
    procedure Read;
    procedure Access;
    procedure Write;
  end;
  {$endif}

  {$ifdef USEQDAC}
  TTestQDAC = class(TSynTestCase)
  published
    procedure Read;
    procedure Access;
    procedure Write;
  end;
  {$endif}

  {$ifdef USEDBXJSON}
  TTestDBXJSON = class(TSynTestCase)
  published
    procedure Read;
    procedure Access;
    procedure Write;
  end;
  {$endif}

  TTestBigContentRead = class(TSynTestCase)
  protected
    fFileName, fZipFileName: TFileName;
    fDownloadURI: RawByteString;
    fMemoryAtStart: Cardinal;
    procedure DownloadFilesIfNecessary; virtual;
  published
  end;

  TTestDepthContent = class(TTestBigContentRead)
  protected
  published
    procedure DownloadFilesIfNecessary; override;
    procedure SynopseReadVariant;
    {$ifdef TESTBSON}
    procedure SynopseReadToBSON;
    {$endif}
    procedure SynopseCrossPlatform;
    {$ifdef USESUPEROBJECT}
    procedure SuperObjectRead;
    {$endif}
    {$ifdef USEDWSJSON}
    procedure dwsJSONRead;
    {$endif}
    {$ifdef USEDBXJSON}
    procedure DBXJSONRead;
    {$endif}
    {$ifdef USEJDO}
    procedure JsonDataObjectsRead;
    {$endif}
    {$ifdef USEQDAC}
    procedure QDACRead;
    {$endif}
  end;

  TTestTableContent = class(TTestBigContentRead)
  protected
  published
    procedure DownloadFilesIfNecessary; override;
    procedure SynopseParse;
    procedure SynopseTableCached;
    procedure SynopseTableIndex;
    procedure SynopseTableLoop;
    procedure SynopseTableVariant;
    procedure SynopseORMLoop;
    procedure SynopseORMList;
    procedure SynopseDocVariant;
    procedure SynopseLateBinding;
    procedure SynopseCrossORM;
    procedure SynopseCrossDirect;
    procedure SynopseCrossVariant;
    {$ifdef TESTBSON}
    procedure SynopseToBSON;
    {$endif}
    {$ifdef USESUPEROBJECT}
    procedure SuperObjectProps;
    procedure SuperObjectRecord;
    {$endif}
    {$ifdef USEDWSJSON}
    procedure dwsJSON;
    {$endif}
    {$ifdef USEDBXJSON}
    procedure DBXJSON;
    {$endif}
    {$ifdef USEJDO}
    procedure _JsonDataObjects;
    {$endif}
    {$ifdef USEQDAC}
    procedure QDAC;
    {$endif}
  end;

  TTestHugeContent = class(TTestBigContentRead)
  protected
    procedure GeoJSONCoordWriter(const aWriter: TTextWriter; const aValue);
    function GeoJSONCoordReader(P: PUTF8Char; var aValue; out aValid: Boolean): PUTF8Char;
  published
    procedure DownloadFilesIfNecessary; override;
    procedure SynopseBeautifier;
    procedure SynopseReadRecord;
    procedure SynopseReadVariant;
    procedure SynopseCrossPlatform;
    {$ifdef TESTBSON}
    procedure SynopseReadToBSON;
    {$endif}
    {$ifdef USESUPEROBJECT}
    procedure SuperObjectRead;
    {$endif}
    {$ifdef USEDWSJSON}
    procedure dwsJSONRead;
    {$endif}
    {$ifdef USEDBXJSON}
    procedure DBXJSONRead;
    {$endif}
    {$ifdef USEJDO}
    procedure JsonDataObjectsRead;
    procedure JsonDataObjectsBeautifier;
    {$endif}
    {$ifdef USEQDAC}
    procedure QDACRead;
    {$endif}
  end;


implementation


{ TTestJSONBenchmarking }

procedure TTestJSONBenchmarking.SmallContent;
begin
  fRunConsoleOccurenceNumber := SAMPLE_JSON_1_COUNT;
  AddCase([TTestSynopseRecord,TTestSynopseVariant, TTestSynopseCrossPlatformVariant
  {$ifdef USESUPEROBJECT},
    {$ifdef ISDELPHI2010}
    TTestSuperObjectRecord,
    {$endif}
    TTestSuperObjectProperties
  {$endif}
  {$ifdef USEXSUPEROBJECT},
    {$ifdef ISDELPHI2010}
    TTestXSuperObjectRecord,
    {$endif}
    TTestXSuperObjectProperties
  {$endif}
  {$ifdef USEDWSJSON},
    TTestdwsJSON
  {$endif}
  {$ifdef USEDBXJSON},
    TTestDBXJSON
  {$endif}
  {$ifdef USEQDAC},
    TTestQDAC
  {$endif}
  {$ifdef USEJDO},
  TTestJsonDataObjects
  {$endif}
  ]);
end;

procedure TTestJSONBenchmarking.BigContent;
begin
  AddCase([TTestDepthContent,TTestTableContent]);
  AddCase(TTestHugeContent);
end;



function MemoryUsed: cardinal; // directly from FastMM4
{$ifndef FPC}
var st: TMemoryManagerState;
    sb: Integer;
begin
  GetMemoryManagerState(st);
  result := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
  for sb := Low(st.SmallBlockTypeStates) to High(st.SmallBlockTypeStates) do
  with st.SmallBlockTypeStates[sb] do
      result := result + UseableBlockSize * AllocatedBlockCount;
end;
{$else}
begin
  result := MaxInt;
end;
{$endif}

{ TTestSynopseRecord }

const
  SAMPLE_JSON_1 = // from http://json.org/example.html
    '{' + #13#10 + 
    '"glossary": {' + #13#10 + 
        '"title": "example glossary",' + #13#10 + 
        '		"GlossDiv": {' + #13#10 + 
            '"title": "S",' + #13#10 +
            '			"GlossList": {' + #13#10 +
                '"GlossEntry": {' + #13#10 + 
                    '"ID": "SGML",' + #13#10 + 
                    '					"SortAs": "SGML",' + #13#10 +
                    '					"GlossTerm": "Standard Generalized Markup Language",' + #13#10 +
                    '					"Acronym": "SGML",' + #13#10 +
                    '					"Abbrev": "ISO 8879:1986",' + #13#10 +
                    '					"GlossDef": {' + #13#10 +
                        '"para": "A meta-markup language, used to create markup languages such as DocBook.",' + #13#10 +
                        '						"GlossSeeAlso": ["GML", "XML"]' + #13#10 +
                    '},' + #13#10 +
                    '					"GlossSee": "markup"' + #13#10 +
                '}' + #13#10 +
            '}' + #13#10 +
        '}' + #13#10 +
    '}' + #13#10 +
    '}';

{$ifdef USEENHANCEDRTTIFORRECORDS}
  {$RTTI EXPLICIT FIELDS([vcPublic])} // needed e.g. on Delphi XE4
{$endif}

type
  TGlossary = packed record
    glossary: record
      title: string;
      GlossDiv: record
        title: string;
        GlossList: record
          GlossEntry: record
            ID, SortAs, GlossTerm, Acronym, Abbrev: string;
            GlossDef: record
              para: string;
              GlossSeeAlso: array of string;
            end;
            GlossSee: string;
           end;
        end;
      end;
    end;
  end;

const // if we don't have Enhanced RTTI
  __TGlossary = 'glossary{title string;GlossDiv{title string;GlossList{'+
    'GlossEntry{ID,SortAs,GlossTerm,Acronym,Abbrev string;GlossDef{'+
    'para string;GlossSeeAlso array of string}GlossSee string}}}}}';

procedure TTestSynopseRecord.Read;
var gloss: TGlossary;
    i: integer;
    json: RawUTF8;
begin
  {$ifndef USEENHANCEDRTTIFORRECORDS}
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TGlossary),__TGlossary);
  {$endif}
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    json := SAMPLE_JSON_1;
    RecordLoadJSON(gloss,@json[1],TypeInfo(TGlossary));
    Check(gloss.glossary.title='example glossary');
  end;
end;

procedure TTestSynopseRecord.Access;
var gloss: TGlossary;
    i: integer;
    json: RawUTF8;
begin
  json := SAMPLE_JSON_1;
  RecordLoadJSON(gloss,@json[1],TypeInfo(TGlossary));
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    Check(gloss.glossary.title='example glossary');
    Check(gloss.glossary.GlossDiv.GlossList.GlossEntry.GlossDef.GlossSeeAlso[0]='GML');
  end;
end;

procedure TTestSynopseRecord.Write;
var gloss: TGlossary;
    i: Integer;
    json: RawUTF8;
begin
  json := SAMPLE_JSON_1;
  RecordLoadJSON(gloss,@json[1],TypeInfo(TGlossary));
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    json := RecordSaveJSON(gloss,TypeInfo(TGlossary));
    check(Hash32(json)=$293BAAA1);
  end;
end;


{ TTestSynopseVariant }

procedure TTestSynopseVariant.Read;
var doc: variant;
    i: integer;
begin
   for i := 1 to SAMPLE_JSON_1_COUNT do begin
     doc := _JsonFast(SAMPLE_JSON_1);
    Check(TDocVariantData(doc).GetValueByPath(['glossary','title'])='example glossary');
   end;
end;

procedure TTestSynopseVariant.AccessDirect;
var doc: TDocVariantData;
    i: integer;
begin
  doc.InitJSON(SAMPLE_JSON_1,JSON_OPTIONS_FAST_STRICTJSON);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    Check(doc.GetValueByPath(['glossary','title'])='example glossary');
    Check(DocVariantData(doc.GetValueByPath([
      'glossary','GlossDiv','GlossList','GlossEntry','GlossDef','GlossSeeAlso'])).Value[0]='GML');
  end;
end;

procedure TTestSynopseVariant.AccessLateBinding;
var doc: variant;
    i: integer;
begin
  doc := _JsonFast(SAMPLE_JSON_1);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    Check(doc.glossary.title='example glossary');
    Check(doc.glossary.GlossDiv.GlossList.GlossEntry.GlossDef.GlossSeeAlso._(0)='GML');
  end;
end;

procedure TTestSynopseVariant.Write;
var doc: variant;
    i: integer;
    json: RawUTF8;
begin
  doc := _JsonFast(SAMPLE_JSON_1);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    json := VariantToUTF8(doc._json);
    check(Hash32(json)=$293BAAA1);
  end;
end;


{ TTestSynopseCrossPlatformVariant }

procedure TTestSynopseCrossPlatformVariant.Read;
var doc: variant;
    i: integer;
begin
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    doc := JSONVariant(SAMPLE_JSON_1);
    Check(doc.glossary.title='example glossary');
  end;
end;

procedure TTestSynopseCrossPlatformVariant.AccessDirect;
var doc: TJSONVariantData;
    i: integer;
begin
  doc.Init(SAMPLE_JSON_1);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    Check(doc.Data('glossary').Value['title']='example glossary');
    Check(doc.Data('glossary').Data('GlossDiv').Data('GlossList').
      Data('GlossEntry').Data('GlossDef').Data('GlossSeeAlso').Values[0]='GML');
  end;
end;

procedure TTestSynopseCrossPlatformVariant.AccessLateBinding;
var doc: variant;
    i: integer;
begin
  doc := JSONVariant(SAMPLE_JSON_1);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    Check(doc.glossary.title='example glossary');
    Check(JSONVariantData(
      doc.glossary.GlossDiv.GlossList.GlossEntry.GlossDef.GlossSeeAlso).Values[0]='GML');
  end;
end;

procedure TTestSynopseCrossPlatformVariant.Write;
var doc: variant;
    i: integer;
    json: RawUTF8;
begin
  doc := JSONVariant(SAMPLE_JSON_1);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    json := StringToUTF8(doc);
    check(Hash32(json)=$293BAAA1);
  end;
end;


{$ifdef USESUPEROBJECT}

{ TTestSuperObjectProperties }

procedure TTestSuperObjectProperties.Read;
var obj: superobject.ISuperObject;
    i: integer;
begin
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    obj := superobject.SO(SAMPLE_JSON_1);
    check(obj['glossary.title'].AsString='example glossary');
  end;
end;

procedure TTestSuperObjectProperties.Access;
var obj: superobject.ISuperObject;
    i: integer;
begin
  obj := superobject.SO(SAMPLE_JSON_1);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    check(obj['glossary.title'].AsString='example glossary');
    check(obj['glossary.GlossDiv.GlossList.GlossEntry.GlossDef.GlossSeeAlso[0]'].AsString='GML');
// slower: check(obj['glossary']['GlossDiv']['GlossList']['GlossEntry']['GlossDef']['GlossSeeAlso'].AsArray[0].AsString='GML');
  end;
end;

procedure TTestSuperObjectProperties.Write;
var obj: superobject.ISuperObject;
    i: integer;
    json: RawUTF8;
begin
  obj := superobject.SO(SAMPLE_JSON_1);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    json := StringToUTF8(obj.AsJSon);
    check(Hash32(json)=$B9D3630E);
  end;
end;

{$ifdef ISDELPHI2010}

{ TTestSuperObjectRecord }

procedure TTestSuperObjectRecord.Access;
var obj: superobject.ISuperObject;
    ctx: superobject.TSuperRttiContext;
    gloss: TGlossary;
    i: integer;
begin
  ctx := superobject.TSuperRttiContext.Create;
  try
    obj := superobject.SO(SAMPLE_JSON_1);
    gloss := ctx.AsType<TGlossary>(obj);
  finally
    ctx.Free;
  end;
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    Check(gloss.glossary.title='example glossary');
    Check(gloss.glossary.GlossDiv.GlossList.GlossEntry.GlossDef.GlossSeeAlso[0]='GML');
  end;
end;

procedure TTestSuperObjectRecord.Read;
var obj: superobject.ISuperObject;
    ctx: superobject.TSuperRttiContext;
    gloss: TGlossary;
    i: integer;
begin
  ctx := superobject.TSuperRttiContext.Create;
  try
    for i := 1 to SAMPLE_JSON_1_COUNT do begin
      obj := superobject.SO(SAMPLE_JSON_1);
      gloss := ctx.AsType<TGlossary>(obj);
      Check(gloss.glossary.title='example glossary');
    end;
  finally
    ctx.Free;
  end;
end;

procedure TTestSuperObjectRecord.Write;
var obj: superobject.ISuperObject;
    ctx: superobject.TSuperRttiContext;
    gloss: TGlossary;
    json: RawUTF8;
    i: integer;
begin
  ctx := superobject.TSuperRttiContext.Create;
  try
    obj := superobject.SO(SAMPLE_JSON_1);
    gloss := ctx.AsType<TGlossary>(obj);
    Owner.TestTimer.Start;
    for i := 1 to SAMPLE_JSON_1_COUNT do begin
      obj := ctx.AsJson<TGlossary>(gloss);
      json := SynUnicodeToUtf8(obj.AsJSon);
      check(Hash32(json)=$B9D3630E); // SuperObject does change the ordering! :(
    end;
  finally
    ctx.Free;
  end;
end;

{$endif ISDELPHI2010}

{$endif USESUPEROBJECT}


{$ifdef USEXSUPEROBJECT}

{ TTestSuperObjectProperties }

procedure TTestXSuperObjectProperties.Read;
var obj: xsuperobject.ISuperObject;
    i: integer;
begin
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    obj := xsuperobject.SO(SAMPLE_JSON_1);
    check(obj['glossary.title'].AsString='example glossary');
  end;
end;

procedure TTestXSuperObjectProperties.Access;
var obj: xsuperobject.ISuperObject;
    i: integer;
begin
  obj := xsuperobject.SO(SAMPLE_JSON_1);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    check(obj['glossary.title'].AsString='example glossary');
    check(obj['glossary.GlossDiv.GlossList.GlossEntry.GlossDef.GlossSeeAlso[0]'].AsString='GML');
  end;
end;

procedure TTestXSuperObjectProperties.Write;
var obj: xsuperobject.ISuperObject;
    i: integer;
    json: RawUTF8;
begin
  obj := xsuperobject.SO(SAMPLE_JSON_1);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    json := StringToUTF8(obj.AsJSon);
    check(Hash32(json)=$293BAAA1);
  end;
end;

{$ifdef ISDELPHI2010}

{ TTestXSuperObjectRecord }

procedure TTestXSuperObjectRecord.Read;
var gloss: TGlossary;
    i: integer;
begin
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    gloss := TSuperRecord<TGlossary>.FromJSON(SAMPLE_JSON_1);
    Check(gloss.glossary.title='example glossary');
  end;
end;

procedure TTestXSuperObjectRecord.Access;
var gloss: TGlossary;
    i: integer;
begin
  gloss := TSuperRecord<TGlossary>.FromJSON(SAMPLE_JSON_1);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    Check(gloss.glossary.title='example glossary');
    Check(gloss.glossary.GlossDiv.GlossList.GlossEntry.GlossDef.GlossSeeAlso[0]='GML');
  end;
end;

procedure TTestXSuperObjectRecord.Write;
var gloss: TGlossary;
    i: Integer;
    json: RawUTF8;
begin
  gloss := TSuperRecord<TGlossary>.FromJSON(SAMPLE_JSON_1);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    StringToUTF8(TSuperRecord<TGlossary>.AsJSON(gloss),json);
    check(Hash32(json)=$293BAAA1);
  end;
end;

{$endif ISDELPHI2010}

{$endif USEXSUPEROBJECT}


{$ifdef USEDWSJSON}

{ TTestdwsJSON }

procedure TTestdwsJSON.Read;
var obj: TdwsJSONValue;
    i: integer;
begin
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    obj := TdwsJSONValue.ParseString(SAMPLE_JSON_1);
    check(obj['glossary']['title'].AsString='example glossary');
    obj.Free;
  end;
end;

procedure TTestdwsJSON.Access;
var obj: TdwsJSONValue;
    i: integer;
begin
  obj := TdwsJSONValue.ParseString(SAMPLE_JSON_1);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    check(obj['glossary']['title'].AsString='example glossary');
    check(obj['glossary']['GlossDiv']['GlossList']['GlossEntry']['GlossDef']['GlossSeeAlso'][0].AsString='GML');
  end;
  obj.Free;
end;

procedure TTestdwsJSON.Write;
var obj: TdwsJSONValue;
    i: integer;
    json: RawUTF8;
begin
  obj := TdwsJSONValue.ParseString(SAMPLE_JSON_1);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    StringToUTF8(obj.ToString,json);
    check(Hash32(json)=$293BAAA1);
  end;
  obj.Free;
end;

{$endif USEDWSJSON}

{$ifdef USEQDAC}

{ TTestQDAC }

procedure TTestQDAC.Read;
var obj: TQJson;
    i: integer;
begin
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    obj := TQJson.Create;
    obj.Parse(SAMPLE_JSON_1);
    check(obj.ItemByPath('glossary.title').AsString='example glossary');
    obj.Free;
  end;
end;

procedure TTestQDAC.Access;
var obj: TQJson;
    i: integer;
begin
  obj := TQJson.Create;
  obj.Parse(SAMPLE_JSON_1);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    check(obj.ItemByPath('glossary.title').AsString='example glossary');
    check(obj.ItemByPath('glossary.GlossDiv.GlossList.GlossEntry.GlossDef.GlossSeeAlso')[0].AsString='GML');
  end;
  obj.Free;
end;

procedure TTestQDAC.Write;
var obj: TQJson;
    i: integer;
    json: RawUTF8;
begin
  obj := TQJson.Create;
  obj.Parse(SAMPLE_JSON_1);
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    StringToUTF8(obj.Encode(False),json); // (false)=no format
    check(Hash32(json)=$293BAAA1);
  end;
  obj.Free;
end;

{$endif USEQDAC}

{$ifdef USEJDO}
procedure TTestJsonDataObjects.Read;
var obj: JsonDataObjects.TJsonBaseObject;
    i: integer;
begin
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    obj := TJsonBaseObject.ParseUtf8(SAMPLE_JSON_1);
    check((obj as JsonDataObjects.TJsonObject).O['glossary'].S['title']='example glossary');
    obj.Free;
  end;
end;

procedure TTestJsonDataObjects.Access;
var obj: JsonDataObjects.TJsonBaseObject;
    doc: JsonDataObjects.TJsonObject;
    i: integer;
begin
  obj := TJsonBaseObject.ParseUtf8(SAMPLE_JSON_1);
  doc := obj as JsonDataObjects.TJsonObject;
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    Check(doc.O['glossary'].S['title']='example glossary');
    Check(doc.O['glossary'].O['GlossDiv'].O['GlossList'].
      O['GlossEntry'].O['GlossDef'].A['GlossSeeAlso'].S[0]='GML');
  end;
  obj.Free;
end;

procedure TTestJsonDataObjects.Write;
var obj: JsonDataObjects.TJsonBaseObject;
    doc: JsonDataObjects.TJsonObject;
    i: integer;
    json: RawUTF8;
begin
  obj := TJsonBaseObject.ParseUtf8(SAMPLE_JSON_1);
  doc := obj as JsonDataObjects.TJsonObject;
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    StringToUTF8(doc.ToJSON,json); // 91 ms
    //json := doc.ToUtf8JSON; // 107 ms
    check(Hash32(json)=$293BAAA1);
  end;
  obj.Free;
end;
{$endif}


{$ifdef USEDBXJSON}

{ TTestDBXJSON }

{$ifndef ISDELPHIXE6}
type // DBXJSON is indeed an half-backed library!
  TJSONObjectHook = class helper for DBXJSON.TJSONObject
  public
    function GetValue(const Name: string): TJSONValue;
    {$ifndef ISDELPHIXE}
    class function ParseJSONValue(const Data: UTF8String): TJSONValue;
    {$endif}
  end;
  TJSONArrayHook = class helper for DBXJSON.TJSONArray
  public // Size and Get() are marked as deprecated since XE6
    function Count: integer;
    function GetItem(const Index: Integer): TJSONValue;
    property Items[const Index: Integer]: TJSONValue read GetItem;
  end;

{$ifndef ISDELPHIXE}
class function TJSONObjectHook.ParseJSONValue(const Data: UTF8String): TJSONValue;
var DataBytes: TBytes;
    len: integer;
begin
  len := Length(Data);
  SetLength(DataBytes,len);
  Move(pointer(Data)^,pointer(DataBytes)^,len);
  Result := inherited ParseJSONValue(DataBytes, 0, len);
  assert(Result<>nil);
end;
{$endif}

function TJSONObjectHook.GetValue(const Name: string): TJSONValue;
var i: integer;
begin
  for i := 0 to Size-1 do
  with Get(i) do
    if JsonString.Value=Name then
      exit(JsonValue);
  result := nil;
end;

function TJSONArrayHook.GetItem(const Index: Integer): TJSONValue;
begin
  result := Get(Index);
end;

function TJSONArrayHook.Count: integer;
begin
  result := Size;
end;
{$endif ISDELPHIXE6}

procedure TTestDBXJSON.Read;
var obj: TJSONObject;
    i: integer;
begin
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    obj := TJSONObject.ParseJSONValue(UTF8String(SAMPLE_JSON_1)) as TJSONObject;
    check(
      (obj.GetValue('glossary') as TJSONObject).
           GetValue('title').Value='example glossary');
    obj.Free;
  end;
end;

procedure TTestDBXJSON.Access;
var obj: TJSONObject;
    i: integer;
begin
  obj := TJSONObject.ParseJSONValue(UTF8String(SAMPLE_JSON_1)) as TJSONObject;
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    check(
      (obj.GetValue('glossary') as TJSONObject).
           GetValue('title').Value='example glossary');
    check(((((((obj.GetValue('glossary') as TJSONObject).
                    GetValue('GlossDiv') as TJSONObject).
                    GetValue('GlossList') as TJSONObject).
                    GetValue('GlossEntry') as TJSONObject).
                    GetValue('GlossDef') as TJSONObject).
                    GetValue('GlossSeeAlso') as TJSONArray).Items[0].Value='GML');
  end;
  obj.Free;
end;

procedure TTestDBXJSON.Write;
var obj: TJSONObject;
    i: integer;
    json: RawUTF8;
begin
  obj := TJSONObject.ParseJSONValue(UTF8String(SAMPLE_JSON_1)) as TJSONObject;
  Owner.TestTimer.Start;
  for i := 1 to SAMPLE_JSON_1_COUNT do begin
    StringToUTF8(obj.ToString,json);
    check(Hash32(json)=$293BAAA1);
  end;
  obj.Free;
end;

{$endif USEDBXJSON}

{ TTestBigContentRead }

procedure TTestBigContentRead.DownloadFilesIfNecessary;
var download: RawByteString;
begin // overriden method should have been set fFileName+fZipFileName+fDownloadURI
  fRunConsoleOccurenceNumber := 0;
  fMemoryAtStart := MemoryUsed;
  fFileName := ExeVersion.ProgramFilePath+fFileName;
  if not FileExists(fFileName) then begin
    download := TWinINet.Get(fDownloadURI);
    if not CheckFailed(download<>'') then begin
      with TZipRead.Create(pointer(download),length(download)) do
      try
        UnZip(fZipFileName,fFileName,true);
      finally
        Free;
      end;
    end;
    if not FileExists(fFileName) then
      MessageBox(0,Pointer('Impossible to find '+fFileName+
        #13#13'Please download it from '+string(fDownloadURI)),nil,MB_ICONEXCLAMATION);
  end;
end;


{ TTestHugeContent }

procedure TTestHugeContent.DownloadFilesIfNecessary;
begin
  fFileName := 'citylots.json';
  fDownloadURI := 'https://github.com/zemirco/sf-city-lots-json/archive/master.zip';
  fZipFileName := 'sf-city-lots-json-master\citylots.json';
  inherited; // perform the download
end;

type
  TGeoJSONCoord = packed record
    x,y,z: double;
  end;
  // only handle "Polygon" and "MultiPolygon" yet
  TGeoJSONCoords = packed record
    values: array of array of TGeoJSONCoord;
    multipolygon: boolean;
  end;
  TGeoJSONObjectType = (
    Point,MultiPoint,LineString,MultiLineString,
    Polygon,MultiPolygon,GeometryCollection,
    Feature,FeatureCollection);
  {$ifdef USEENHANCEDRTTIFORRECORDS}
  TCity = packed record
    &type: TGeoJSONObjectType;
    features: array of record
      &type: TGeoJSONObjectType;
      properties: record // we may define a variant here (TDocVariant)
        MAPBLKLOT, BLKLOT, BLOCK_NUM, LOT_NUM: RawUTF8;
        FROM_ST, TO_ST, STREET, ST_TYPE, ODD_EVEN: RawUTF8;
      end;
      geometry: record
        &type: TGeoJSONObjectType;
        coordinates: array of TGeoJSONCoords;
      end;
    end;
  end;
  {$else} // &type not allowed? not a problem, since we use text-based definition
  TCity = packed record
    _type: TGeoJSONObjectType;
    features: array of record
      _type: TGeoJSONObjectType;
      properties: record
        MAPBLKLOT, BLKLOT, BLOCK_NUM, LOT_NUM: RawUTF8;
        FROM_ST, TO_ST, STREET, ST_TYPE, ODD_EVEN: RawUTF8;
      end;
      geometry: record
        _type: TGeoJSONObjectType;
        coordinates: array of TGeoJSONCoords;
      end;
    end;
  end;
  {$endif}

const
  __TCity = 'type TGeoJSONObjectType features[type TGeoJSONObjectType '+
    ' properties{MAPBLKLOT, BLKLOT, BLOCK_NUM, LOT_NUM: RawUTF8;'+
    'FROM_ST, TO_ST, STREET, ST_TYPE, ODD_EVEN: RawUTF8}'+
    'geometry{type TGeoJSONObjectType coordinates array of TGeoJSONCoords}]';

function TTestHugeContent.GeoJSONCoordReader(P: PUTF8Char; var aValue;
  out aValid: Boolean): PUTF8Char;
var V: TGeoJSONCoords absolute aValue;
    i1,i2,n: integer;
begin // '[ [ -122.420540559229593, 37.805963600244901, 0.0 ], ... ]'
  aValid := false;
  result := nil;
  if (P=nil) or (P^<>'[') then
    exit;
  P := GotoNextNotSpace(P+1);
  if P^<>'[' then
    exit;
  if GotoNextNotSpace(P+1)^='[' then begin // MultiPolygon '[ [ [ -122.461...'
    SetLength(V.values,JSONArrayCount(P));
    P := GotoNextNotSpace(P+1);
    V.multipolygon := true;
  end else begin
    SetLength(V.values,1);
    V.multipolygon := false;
  end;
  for i1 := 0 to high(V.values) do begin
    n := JSONArrayCount(P);
    SetLength(V.values[i1],n);
    for i2 := 0 to n-1 do begin
      P := GotoNextNotSpace(P);
      if P^<>'[' then
        exit;
      inc(P);
      V.values[i1,i2].x := GetNextItemDouble(P);
      V.values[i1,i2].y := GetNextItemDouble(P);
      V.values[i1,i2].z := GetNextItemDouble(P,']');
      if P=nil then
        exit;
      if P^=',' then
        inc(P);
    end;
    if V.multipolygon then begin
      P := GotoNextNotSpace(P);
      if P^<>']' then
        exit;
      P := GotoNextNotSpace(P+1);
      if P^=',' then begin
        P := GotoNextNotSpace(P+1);
        if P^<>'[' then
          exit;
        P := GotoNextNotSpace(P+1);
      end;
    end;
  end;
  if P=nil then
    exit;
  P := GotoNextNotSpace(P);
  if P^<>']' then
    exit;
  result := GotoNextNotSpace(P+1);
  aValid := true;
end;

procedure TTestHugeContent.GeoJSONCoordWriter(const aWriter: TTextWriter;
  const aValue);
var i1,i2: integer;
begin // '[ [ [ -122.420540559229593, 37.805963600244901, 0.0 ], ... ] ]'
  aWriter.Add('[','[');
  with TGeoJSONCoords(aValue) do begin
    for i1 := 0 to high(values) do begin
      if multipolygon then
        aWriter.Add('[');
      for i2 := 0 to high(values[i1]) do
        with values[i1,i2] do
          aWriter.Add('[%,%,%],',[x,y,z]);
      if multipolygon then begin
        aWriter.CancelLastComma;
        aWriter.Add(']',',');
      end;
    end;
  end;
  aWriter.CancelLastComma;
  aWriter.Add(']',']');
end;

procedure TTestHugeContent.SynopseBeautifier;
var json: RawUTF8;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  check(JSONBufferReformatToFile(pointer(json),'testsynopse.json',jsonHumanReadable));
  json := '';
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
end;

procedure TTestHugeContent.SynopseReadRecord;
var json: RawUTF8;
    data: TCity;
begin
  TTextWriter.RegisterCustomJSONSerializer(TypeInfo(TGeoJSONCoords),GeoJSONCoordReader,GeoJSONCoordWriter);
  {$ifndef USEENHANCEDRTTIFORRECORDS}
  TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TGeoJSONObjectType));
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TCity),__TCity);
  {$endif}
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  RecordLoadJSON(data,pointer(JSON),TypeInfo(TCity));
  json := '';
  {$ifdef USEENHANCEDRTTIFORRECORDS}
  check(data.&type=FeatureCollection);
  {$else}
  check(data._type=FeatureCollection);
  {$endif}
  fRunConsoleOccurenceNumber := length(data.features);
  if data.features<>nil then
    with data.features[high(data.features)] do begin
      check(properties.MAPBLKLOT='VACSTWIL');
      checksame(geometry.coordinates[0].values[0,0].x,-122.424,1E-1);
    end;
  check(fRunConsoleOccurenceNumber=206560);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
end;

{$ifdef TESTBSON}
procedure TTestHugeContent.SynopseReadToBSON;
var json: RawUTF8;
    docs: TBSONDocumentDynArray;
    start: integer;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  start := PosEx('[',json);
  if CheckFailed(start>0) then
    exit;
  Check(JSONBufferToBSONArray(@json[start],docs,true));
  fRunConsoleOccurenceNumber := length(docs);
  check(fRunConsoleOccurenceNumber=206560);
  json := '';
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
end;
{$endif}

procedure TTestHugeContent.SynopseReadVariant;
var json: RawUTF8;
    doc: TDocVariantData;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  doc.InitJSONInPlace(Pointer(json),JSON_OPTIONS_FAST_STRICTJSON);
  json := '';
  check(doc.Value['type']='FeatureCollection');
  fRunConsoleOccurenceNumber := doc.Value['features']._count;
  check(fRunConsoleOccurenceNumber=206560);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
end;

procedure TTestHugeContent.SynopseCrossPlatform;
var json: string;
    doc: TJSONVariantData;
begin
  json := AnyTextFileToString(fFileName,true);
  Owner.TestTimer.Start;
  doc.Init(json);
  json := '';
  check(doc.Value['type']='FeatureCollection');
  fRunConsoleOccurenceNumber := doc.Data('features').Count;
  check(fRunConsoleOccurenceNumber=206560);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
end;

{$ifdef USESUPEROBJECT}
procedure TTestHugeContent.SuperObjectRead;
var f: TStream;
    obj: ISuperObject;
begin
  f := TFileStream.Create(fFileName,fmOpenRead);
  Owner.TestTimer.Start;
  obj := TSuperObject.ParseStream(f, False);
  f.Free;
  check(obj['type'].AsString='FeatureCollection');
  fRunConsoleOccurenceNumber := obj['features'].AsArray.Length;
  check(fRunConsoleOccurenceNumber=206560);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
end;
{$endif}

{$ifdef USEDWSJSON}
procedure TTestHugeContent.dwsJSONRead;
var obj: TdwsJSONValue;
begin
  obj := TdwsJSONValue.ParseFile(fFileName);
  check(obj['type'].AsString='FeatureCollection');
  fRunConsoleOccurenceNumber := obj['features'].ElementCount;
  check(fRunConsoleOccurenceNumber=206560);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  obj.Free;
end;
{$endif}

{$ifdef USEQDAC}
procedure TTestHugeContent.QDACRead;
var obj: TQJson;
begin
  obj := TQJson.Create;
  obj.LoadFromFile(fFileName);
  check(obj.ItemByName('type').AsString='FeatureCollection');
  fRunConsoleOccurenceNumber := obj.ItemByName('features').Count;
  check(fRunConsoleOccurenceNumber=206560);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  obj.Free;
end;
{$endif}

{$ifdef USEDBXJSON}
procedure TTestHugeContent.DBXJSONRead;
var json: UTF8String;
    obj: TJSONObject;
begin
  {$ifndef CPU64}
  fRunConsole := 'DBXJSON will raise EOutOfMemory for 185 MB JSON in Win32 -> skip';
  exit;
  {$endif}
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  obj := TJSONObject.ParseJSONValue(json) as TJSONObject;
  check(obj.GetValue('type').Value='FeatureCollection');
  fRunConsoleOccurenceNumber := (obj.GetValue('features') as TJSONArray).Count;
  check(fRunConsoleOccurenceNumber=206560);
  json := '';
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  obj.Free;
end;
{$endif}

{$ifdef USEJDO}
procedure TTestHugeContent.JsonDataObjectsRead;
var json: UTF8String;
    obj: JsonDataObjects.TJsonObject;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  obj := JsonDataObjects.TJsonBaseObject.ParseUTF8(json) as JsonDataObjects.TJsonObject;
  Check(obj.S['type']='FeatureCollection');
  fRunConsoleOccurenceNumber := obj.A['features'].Count;
  check(fRunConsoleOccurenceNumber=206560);
  json := '';
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  obj.Free;
end;

procedure TTestHugeContent.JsonDataObjectsBeautifier;
var json: UTF8String;
//    output: string; new: RawUTF8;
    obj: JsonDataObjects.TJsonObject;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  obj := JsonDataObjects.TJsonBaseObject.ParseUTF8(json) as JsonDataObjects.TJsonObject;
{  output := obj.ToJSON(false); // is raising an OutOfMemory under Win32 -> file
  StringToUTF8(output,new);
  check(length(new)>length(json));
  output := ''; }
  obj.SaveToFile('testjdo.json',false);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  obj.Free;
end;

{$endif}

{ TTestDepthContent }

procedure TTestDepthContent.DownloadFilesIfNecessary;
begin
  fRunConsoleOccurenceNumber := 3315; // line numbers in file
  fFileName := 'sample.json';
  fDownloadURI := 'https://json-test-suite.googlecode.com/files/sample.zip';
  fZipFileName := 'sample.json';
  inherited; // perform the download
end;

{$ifdef TESTBSON}
procedure TTestDepthContent.SynopseReadToBSON;
var json: RawUTF8;
    doc: TBSONDocument;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  Check(JSONBufferToBSONDocument(pointer(json),doc,true)=betDoc);
  Check(length(doc)>150000);
  json := '';
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
end;
{$endif}

procedure TTestDepthContent.SynopseReadVariant;
var json: RawUTF8;
    doc: TDocVariantData;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  doc.InitJSONInPlace(Pointer(json),JSON_OPTIONS_FAST_STRICTJSON);
  json := '';
  check(doc.GetValueByPath('a.obj.key')='wrong value');
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
end;

procedure TTestDepthContent.SynopseCrossPlatform;
var json: string;
    doc: TJSONVariantData;
begin
  json := AnyTextFileToString(fFileName,true);
  Owner.TestTimer.Start;
  doc.Init(json);
  json := '';
  check(doc.Data('a').Data('obj').Value['key']='wrong value');
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
end;

{$ifdef USESUPEROBJECT}
procedure TTestDepthContent.SuperObjectRead;
var f: TStream;
    obj: ISuperObject;
begin
  // by default, SuperObject is NOT able to handle such a depth
  // you have to set SUPER_TOKENER_MAX_DEPTH = 1000 in the libary source! 
  f := TFileStream.Create(fFileName,fmOpenRead);
  Owner.TestTimer.Start;
  obj := TSuperObject.ParseStream(f, False);
  f.Free;
  if CheckFailed(obj<>nil,'please set SUPER_TOKENER_MAX_DEPTH = 1000 in superobject.pas') then
    exit;
  check(obj['a.obj.key'].AsString='wrong value');
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
end;
{$endif}

{$ifdef USEDWSJSON}
procedure TTestDepthContent.dwsJSONRead;
var obj: TdwsJSONValue;
begin
  obj := TdwsJSONValue.ParseFile(fFileName);
  check(obj['a']['obj']['key'].AsString='wrong value');
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  obj.Free;
end;
{$endif}

{$ifdef USEQDAC}
procedure TTestDepthContent.QDACRead;
var obj: TQJson;
begin
  obj := TQJson.Create;
  obj.LoadFromFile(fFileName);
  check(obj.ItemByPath('a.obj.key').AsString='wrong value');
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  obj.Free;
end;
{$endif}

{$ifdef USEDBXJSON}
procedure TTestDepthContent.DBXJSONRead;
var json: UTF8String;
    obj: TJSONObject;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  obj := TJSONObject.ParseJSONValue(json) as TJSONObject;
  check(((obj.GetValue('a') as TJSONObject).
           GetValue('obj') as TJSONObject).
           GetValue('key').Value='wrong value');
  json := '';
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  obj.Free;
end;
{$endif}

{$ifdef USEJDO}
procedure TTestDepthContent.JsonDataObjectsRead;
var json: UTF8String;
    obj: JsonDataObjects.TJsonObject;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  obj := JsonDataObjects.TJsonBaseObject.ParseUTF8(json) as JsonDataObjects.TJsonObject;
  check(obj.O['a'].O['obj'].S['key']='wrong value');
  json := '';
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  obj.Free;
end;
{$endif}

{ TTestTableContent }

type
  TSQLRecordPeople = class(TSQLRecord)
  private
    fData: TSQLRawBlob;
    fFirstName: RawUTF8;
    fLastName: RawUTF8;
    fYearOfBirth: integer;
    fYearOfDeath: word;
  published
    property FirstName: RawUTF8 read fFirstName write fFirstName;
    property LastName: RawUTF8 read fLastName write fLastName;
    property Data: TSQLRawBlob read fData write fData;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
  end;

  TSQLRecordPeoplePersistent = class(TPersistent)
  private
    fRowID: integer;
    fData: TByteDynArray;
    fFirstName: string;
    fLastName: string;
    fYearOfBirth: integer;
    fYearOfDeath: word;
  published
    property RowID: integer read fRowID write fRowID;
    property FirstName: string read fFirstName write fFirstName;
    property LastName: string read fLastName write fLastName;
    property Data: TByteDynArray read fData write fData;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
  end;

procedure TTestTableContent.DownloadFilesIfNecessary;
var i: integer;
begin
  fMemoryAtStart := MemoryUsed;
  fFileName := ExeVersion.ProgramFilePath;
  i := pos('\Samples\',fFileName);
  if i>0 then begin
    Setlength(fFileName,i);
    if FileExists(fFileName+'exe\people.json') then
      fFileName := fFileName+'exe\people.json' else
      fFileName  := fFileName+'people.json'
  end;
  fRunConsoleOccurenceNumber := 8228; // row numbers in file
  if not FileExists(fFileName) then
    MessageBox(0,Pointer('Impossible to find '+fFileName+
      #13#13'Please run at least once TestSQL3.dpr'),nil,MB_ICONEXCLAMATION);
end;

procedure TTestTableContent.SynopseParse;
var json: RawUTF8;
    list: TSQLTableJSON;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  list := TSQLTableJSON.Create('',pointer(json),length(json));
  fRunConsoleOccurenceNumber := list.RowCount;
  check(fRunConsoleOccurenceNumber>8000);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  list.Free;
end;

procedure TTestTableContent.SynopseTableVariant;
var json: RawUTF8;
    people: variant;
    list: TSQLTableJSON;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  list := TSQLTableJSON.Create('',pointer(json),length(json));
  while list.Step(false,@people) do begin
    Check(people.FirstName<>'');
    Check(people.LastName<>'');
    Check(people.YearOfBirth<10000);
    Check((people.YearOfDeath>1400)and(people.YearOfDeath<2000));
    Check((people.ID>11011) or (people.Data<>''));
  end;
  fRunConsoleOccurenceNumber := list.RowCount;
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  list.Free;
end;

procedure TTestTableContent.SynopseTableIndex;
var json: RawUTF8;
    list: TSQLTableJSON;
    i: Integer;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  list := TSQLTableJSON.Create('',pointer(json),length(json));
  for i := 1 to list.RowCount do begin
    Check(list.Get(i,'FirstName')<>nil);
    Check(list.Get(i,'LastName')<>nil);
    Check(list.GetAsInteger(i,'YearOfBirth')<10000);
    Check((list.GetAsInteger(i,'YearOfDeath')>1400)and(list.GetAsInteger(i,'YearOfDeath')<2000));
    Check((list.GetAsInteger(i,'RowID')>11011) or (list.Get(i,'Data')<>nil));
  end;
  fRunConsoleOccurenceNumber := list.RowCount;
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  list.Free;
end;

procedure TTestTableContent.SynopseTableCached;
var json: RawUTF8;
    list: TSQLTableJSON;
    i,FirstName,LastName,YearOfBirth,YearOfDeath,RowID,Data: Integer;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  list := TSQLTableJSON.Create('',pointer(json),length(json));
  list.FieldIndexExisting(
    ['FirstName','LastName','YearOfBirth','YearOfDeath','RowID','Data'],
    [@FirstName,@LastName,@YearOfBirth,@YearOfDeath,@RowID,@Data]);
  for i := 1 to list.RowCount do begin
    Check(list.Get(i,FirstName)<>nil);
    Check(list.Get(i,LastName)<>nil);
    Check(list.GetAsInteger(i,YearOfBirth)<10000);
    Check((list.GetAsInteger(i,YearOfDeath)>1400)and(list.GetAsInteger(i,YearOfDeath)<2000));
    Check((list.GetAsInteger(i,RowID)>11011) or (list.Get(i,Data)<>nil));
  end;
  fRunConsoleOccurenceNumber := list.RowCount;
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  list.Free;
end;

procedure TTestTableContent.SynopseTableLoop;
var json: RawUTF8;
    list: TSQLTableJSON;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  list := TSQLTableJSON.Create('',pointer(json),length(json));
  while list.Step do begin
    Check(list.FieldBuffer('FirstName')<>nil);
    Check(list.FieldBuffer('LastName')<>nil);
    Check(list.FieldAsInteger('YearOfBirth')<10000);
    Check((list.FieldAsInteger('YearOfDeath')>1400)and(list.FieldAsInteger('YearOfDeath')<2000));
    Check((list.FieldAsInteger('RowID')>11011) or (list.FieldBuffer('Data')<>nil));
  end;
  fRunConsoleOccurenceNumber := list.RowCount;
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  list.Free;
end;

procedure TTestTableContent.SynopseCrossORM;
var json: string;
    table: TJSONTableObject;
    people: TSQLRecordPeoplePersistent;
begin
  json := AnyTextFileToString(fFileName,true);
  people := TSQLRecordPeoplePersistent.Create;
  Owner.TestTimer.Start;
  table := TJSONTableObject.Create(json);
  fRunConsoleOccurenceNumber := 0;
  while table.StepObject(people) do begin
    Check(people.FirstName<>'');
    Check(people.LastName<>'');
    Check(people.YearOfBirth<10000);
    Check((people.YearOfDeath>1400)and(people.YearOfDeath<2000));
    Check((people.RowID>11011) or (people.Data<>nil));
    inc(fRunConsoleOccurenceNumber);
  end;
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  table.Free;
  people.Free;
end;

procedure TTestTableContent.SynopseCrossDirect;
var json: string;
    table: TJSONTable;
begin
  json := AnyTextFileToString(fFileName,true);
  Owner.TestTimer.Start;
  table := TJSONTable.Create(json);
  fRunConsoleOccurenceNumber := 0;
  while table.Step do begin
    Check(table['FirstName']<>'');
    Check(table['LastName']<>'');
    Check(table['YearOfBirth']<10000);
    Check((table['YearOfDeath']>1400)and(table['YearOfDeath']<2000));
    Check((table['RowID']>11011) or (table['Data']<>null));
    inc(fRunConsoleOccurenceNumber);
  end;
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  table.Free;
end;
procedure TTestTableContent.SynopseCrossVariant;
var json: string;
    table: TJSONTable;
    obj: variant;
begin
  json := AnyTextFileToString(fFileName,true);
  Owner.TestTimer.Start;
  table := TJSONTable.Create(json);
  fRunConsoleOccurenceNumber := 0;
  while table.StepValue(obj) do begin
    Check(obj.FirstName<>'');
    Check(obj.LastName<>'');
    Check(obj.YearOfBirth<10000);
    Check((obj.YearOfDeath>1400)and(obj.YearOfDeath<2000));
    Check((obj.RowID>11011) or (obj.Data<>null));
    inc(fRunConsoleOccurenceNumber);
  end;
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  table.Free;
end;

procedure TTestTableContent.SynopseORMLoop;
var people: TSQLRecordPeople;
    json: RawUTF8;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  people := TSQLRecordPeople.CreateAndFillPrepare(json);
  json := '';
  while people.FillOne do begin
    Check(people.FirstName<>'');
    Check(people.LastName<>'');
    Check(people.YearOfBirth<10000);
    Check((people.YearOfDeath>1400)and(people.YearOfDeath<2000));
    Check((people.ID>11011) or (people.Data<>''));
  end;
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  fRunConsoleOccurenceNumber := people.FillTable.RowCount;
  people.Free;
end;

procedure TTestTableContent.SynopseORMList;
{$ifdef ISDELPHI2010} // use generic syntax, just for fun ;)
var json: RawUTF8;
    people: TSQLRecordPeople;
    table: TSQLTableJSON;
    list: TObjectList<TSQLRecordPeople>;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  table := TSQLTableJSON.Create('',pointer(json),length(json));
  list := table.ToObjectList<TSQLRecordPeople>;
  table.Free;
  json := '';
  for people in list do begin
    Check(people.FirstName<>'');
    Check(people.LastName<>'');
    Check(people.YearOfBirth<10000);
    Check((people.YearOfDeath>1400)and(people.YearOfDeath<2000));
    Check((people.ID>11011) or (people.Data<>''));
  end;
  fRunConsoleOccurenceNumber := list.Count;
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  list.Free;
end;
{$else}
var json: RawUTF8;
    i: integer;
    list: TSQLTableJSON;
    doc: TObjectList;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  list := TSQLTableJSON.Create('',pointer(json),length(json));
  doc := list.ToObjectList(TSQLRecordPeople);
  list.Free;
  json := '';
  for i := 0 to doc.Count-1 do
    with TSQLRecordPeople(doc.List[i]) do begin
      Check(FirstName<>'');
      Check(LastName<>'');
      Check(YearOfBirth<10000);
      Check((YearOfDeath>1400)and(YearOfDeath<2000));
      Check((ID>11011) or (Data<>''));
    end;
  fRunConsoleOccurenceNumber := doc.Count;
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  doc.Free;
end;
{$endif}

{$ifdef TESTBSON}
procedure TTestTableContent.SynopseToBSON;
var json: RawUTF8;
    docs: TBSONDocumentDynArray;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  Check(JSONBufferToBSONArray(pointer(json),docs,true));
  json := '';
  fRunConsoleOccurenceNumber := Length(docs);
  check(fRunConsoleOccurenceNumber>8000);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
end;
{$endif}

procedure TTestTableContent.SynopseDocVariant;
var json: RawUTF8;
    doc: TDocVariantData;
    i: integer;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  doc.InitJSONInPlace(Pointer(json),JSON_OPTIONS_FAST_STRICTJSON);
  json := '';
  check(doc.Kind=dvArray);
  for i := 0 to doc.Count-1 do
  with DocVariantData(doc.Value[i])^ do begin
    Check(Value['FirstName']<>'');
    Check(Value['LastName']<>'');
    Check(Value['YearOfBirth']<10000);
    Check((Value['YearOfDeath']>1400)and(Value['YearOfDeath']<2000));
    Check((Value['RowID']>11011) or (Value['Data']<>null));
  end;
  fRunConsoleOccurenceNumber := doc.Count;
  check(fRunConsoleOccurenceNumber>8000);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
end;

procedure TTestTableContent.SynopseLateBinding;
var json: RawUTF8;
    doc: TDocVariantData;
    i: integer;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  doc.InitJSONInPlace(Pointer(json),JSON_OPTIONS_FAST_STRICTJSON);
  json := '';
  check(doc.Kind=dvArray);
  for i := 0 to doc.Count-1 do begin
    Check(doc.Values[i].FirstName<>'');
    Check(doc.Values[i].LastName<>'');
    Check(doc.Values[i].YearOfBirth<10000);
    Check((doc.Values[i].YearOfDeath>1400)and(doc.Values[i].YearOfDeath<2000));
    Check((doc.Values[i].RowID>11011) or (doc.Values[i].Data<>''));
  end;
  fRunConsoleOccurenceNumber := doc.Count;
  check(fRunConsoleOccurenceNumber>8000);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
end;

{$ifdef USESUPEROBJECT}
procedure TTestTableContent.SuperObjectProps;
var f: TStream;
    obj: ISuperObject;
    ndx: integer;
begin
  f := TFileStream.Create(fFileName,fmOpenRead);
  Owner.TestTimer.Start;
  obj := TSuperObject.ParseStream(f, False);
  with obj.AsArray do
    for ndx := 0 to Length-1 do
    with O[ndx] do begin
      Check(S['FirstName']<>'');
      Check(S['LastName']<>'');
      Check(I['YearOfBirth']<10000);
      Check((I['YearOfDeath']>1400)and(I['YearOfDeath']<2000));
      Check((I['RowID']>11011) or (S['Data']<>''));
    end;
  f.Free;
  fRunConsoleOccurenceNumber := obj.AsArray.Length;
  check(fRunConsoleOccurenceNumber>8000);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
end;

type // SuperObject does not work as expected with TSQLRecord
  TPeople = record
    RowID: integer;
    FirstName: RawUTF8;
    LastName: RawUTF8;
    Data: TSQLRawBlob;
    YearOfBirth: integer;
    YearOfDeath: word;
  end;

procedure TTestTableContent.SuperObjectRecord;
var f: TStream;
    obj: ISuperObject;
    ndx: integer;
    ctx: superobject.TSuperRttiContext ;
    people: TPeople;
begin
  f := TFileStream.Create(fFileName,fmOpenRead);
  Owner.TestTimer.Start;
  obj := TSuperObject.ParseStream(f, False);
  f.Free;
  ctx := superobject.TSuperRttiContext.Create;
  try
    with obj.AsArray do
    for ndx := 0 to Length-1 do begin
      people := ctx.AsType<TPeople>(O[ndx]);
      Check(people.FirstName<>'');
      Check(people.LastName<>'');
      Check(people.YearOfBirth<10000);
      Check((people.YearOfDeath>1400)and(people.YearOfDeath<2000));
      //Check((people.RowID>11011) or (people.Data<>'')); SO has issue with Data
      Check(people.RowID>0);
    end;
  finally
    ctx.Free;
  end;
  fRunConsoleOccurenceNumber := obj.AsArray.Length;
  check(fRunConsoleOccurenceNumber>8000);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
end;

{$endif}

{$ifdef USEDWSJSON}
procedure TTestTableContent.dwsJSON;
var obj: TdwsJSONValue;
    i: integer;
begin
  obj := TdwsJSONValue.ParseFile(fFileName);
  fRunConsoleOccurenceNumber := obj.ElementCount;
  for i := 0 to fRunConsoleOccurenceNumber-1 do
    with obj.Elements[i] do begin
      Check(Values['FirstName'].AsString<>'');
      Check(Values['LastName'].AsString<>'');
      Check(Values['YearOfBirth'].AsInteger<10000);
      Check((Values['YearOfDeath'].AsInteger>1400)and(Values['YearOfDeath'].AsInteger<2000));
      Check((Values['RowID'].AsInteger>11011) or (Values['Data'].AsString<>''));
    end;
  check(fRunConsoleOccurenceNumber>8000);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  obj.Free;
end;
{$endif}

{$ifdef USEQDAC}
procedure TTestTableContent.QDAC;
var obj: TQJson;
    i: integer;
begin
  obj := TQJson.Create;
  obj.LoadFromFile(fFileName);
  fRunConsoleOccurenceNumber := obj.Count;
  for i := 0 to fRunConsoleOccurenceNumber-1 do
    with obj.Items[i] do begin
      Check(ItemByName('FirstName').AsString<>'');
      Check(ItemByName('LastName').AsString<>'');
      Check(ItemByName('YearOfBirth').AsInteger<10000);
      Check((ItemByName('YearOfDeath').AsInteger>1400)and(ItemByName('YearOfDeath').AsInteger<2000));
      Check((ItemByName('RowID').AsInteger>11011) or (ItemByName('Data').AsString<>''));
    end;
  check(fRunConsoleOccurenceNumber>8000);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  obj.Free;
end;
{$endif}

{$ifdef USEJDO}
procedure TTestTableContent._JsonDataObjects;
var arr: JsonDataObjects.TJsonArray;
    i: integer;
begin
  arr := JsonDataObjects.TJsonBaseObject.ParseFromFile(fFileName) as JsonDataObjects.TJsonArray;
  fRunConsoleOccurenceNumber := arr.Count;
  for i := 0 to fRunConsoleOccurenceNumber-1 do
    with arr.O[i] do begin
      Check(S['FirstName']<>'');
      Check(S['LastName']<>'');
      Check(I['YearOfBirth']<10000);
      Check((I['YearOfDeath']>1400)and(I['YearOfDeath']<2000));
      Check((I['RowID']>11011) or (S['Data']<>''));
    end;
  check(fRunConsoleOccurenceNumber>8000);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  arr.Free;
end;
{$endif}

{$ifdef USEDBXJSON}
procedure TTestTableContent.DBXJSON;
var json: UTF8String;
    obj: TJSONArray;
    i: integer;
begin
  json := StringFromFile(fFileName);
  Owner.TestTimer.Start;
  obj := TJSONObject.ParseJSONValue(json) as TJSONArray;
  json := '';
  for i := 0 to obj.Count-1 do
  with obj.Items[i] as TJSONObject do begin
    Check(GetValue('FirstName').Value<>'');
    Check(GetValue('LastName').Value<>'');
    Check(StrToInt(GetValue('YearOfBirth').Value)<10000);
    Check((StrToInt(GetValue('YearOfDeath').Value)>1400)and
          (StrToInt(GetValue('YearOfDeath').Value)<2000));
    Check((StrToInt(GetValue('RowID').Value)>11011) or
        (GetValue('Data').Value<>''));
  end;
  fRunConsoleOccurenceNumber := obj.Count;
  check(fRunConsoleOccurenceNumber>8000);
  fRunConsoleMemoryUsed := MemoryUsed-fMemoryAtStart;
  obj.Free;
end;
{$endif}

end.
