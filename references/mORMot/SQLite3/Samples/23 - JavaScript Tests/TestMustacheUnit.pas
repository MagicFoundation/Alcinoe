unit TestMustacheUnit;

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SynCrtSock,
  {$ifndef CPU64} // SpiderMonkey library is not available yet in 64 bit
  SynSM, SynSMAPI,
  {$endif}
  SynCommons, SynMustache;


type
  TMainForm = class(TForm)
    mmoTemplate: TMemo;
    lblTemplate: TLabel;
    mmoContext: TMemo;
    lblContext: TLabel;
    btnExecSynMustache: TButton;
    btnExecSpiderMonkey: TButton;
    mmoResult: TMemo;
    btnOpenBrowser: TButton;
    lblIteration: TLabel;
    edtIteration: TEdit;
    procedure Render(Sender: TObject);
    procedure btnOpenBrowserClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    {$ifndef CPU64} // SpiderMonkey library is not available yet in 64 bit
    fEngineManager: TSMEngineManager;
    fEngine: TSMEngine;
    {$endif}
  end;

var
  MainForm: TMainForm;

implementation

uses
  ShellAPI;

{$R *.dfm}

{$ifndef FPC}
{$R Vista.res}
{$endif}

procedure TMainForm.Render(Sender: TObject);
var Template, Context, Result: RawUTF8;
    data: variant;
    i,n: Integer;
    Timer: TPrecisionTimer;
    SynMustacheTemplate: TSynMustache;
    {$ifndef CPU64} // SpiderMonkey library is not available yet in 64 bit
    person: RawUTF8;
    partial: variant;
    nn: Variant;
    {$endif}
begin
  Template := StringToUTF8(mmoTemplate.Lines.Text);
  Context := StringToUTF8(mmoContext.Lines.Text);
  data := _JsonFast(Context);
  n := StrToIntDef(edtIteration.Text,1000);
  if Sender=btnExecSynMustache then begin
    SynMustacheTemplate := TSynMustache.Parse(Template);
    Timer.Start;
    for i := 1 to n do
      result := SynMustacheTemplate.Render(data);
  end else
  {$ifndef CPU64} // SpiderMonkey library is not available yet in 64 bit
  if Sender=btnExecSpiderMonkey then begin
    fEngine.MaybeGarbageCollect;
    i := PosEx('{{<person}}',Template);
    if i>0 then begin // extract any inlined partial (should be at the end)
      person := Copy(Template,i+12,maxInt);
      SetLength(Template,i-1);
      i := PosEx('{{/person}}',person);
      if i>0 then
        SetLength(person,i-1);
      partial := _ObjFast(['person',person]);
    end;
    fEngine.Global.testMustache(Template, data, partial, 1); //compile template
    Timer.Start;
    nn := n;
    result := VariantToUTF8(fEngine.Global.testMustache(Template, data, partial, nn));
    // test below slow because of 2 things:
//  1) marsalling argumnets between Delphi & JS
//  2) we measure "prepare" time 
//    for i := 1 to n do
//      result := VariantToUTF8(fEngine.Global.Mustache.render(Template, data, partial));
  end else
  {$endif}
    exit;
  mmoResult.Lines.Text :=
    Format('Rendered %d times in %s (%d/sec):'#13#10#13#10'%s',
      [n,Timer.Stop,Timer.PerSec(n),result]);
  FileFromString(Result,ChangeFileExt(ExeVersion.ProgramFileName,'.html'));
end;

procedure TMainForm.btnOpenBrowserClick(Sender: TObject);
begin
  ShellExecute(0,'open',Pointer(ChangeFileExt(ExeVersion.ProgramFileName,'.html')),nil,nil,SW_SHOWNORMAL);
end;

procedure TMainForm.FormShow(Sender: TObject);
{$ifdef CPU64} // SpiderMonkey library is not available yet in 64 bit
begin
  btnExecSpiderMonkey.Hide;
end;
{$else}
const
  testMustacheFunc =
    'function testMustache(template, data, partial, iterCount){ ' +
    '  var result = ""; ' +
    '  for(var i=0; i<iterCount; i++){ ' +
    '    result = Mustache.render(template, data, partial) ' +
    '  }' +
    ' return result; ' +
    '}'#10'1;';

var mustacheFN: TFileName;
    mSource: SynUnicode;
    mustache: RawByteString;
    i: integer;
begin
  fEngineManager := TSMEngineManager.Create;
  fEngine := fEngineManager.ThreadSafeEngine;
  mustacheFN := ExeVersion.ProgramFilePath + 'js\mustache.js';
  mSource := AnyTextFileToSynUnicode(mustacheFN);
  if mSource='' then begin
    mustache := TWinINet.Get('https://github.com/janl/mustache.js/raw/master/mustache.js');
    if PosEx('return send(result);',mustache)=0 then begin
      i := PosEx('send(result);',mustache);
      if i>0 then
        insert('return ',mustache,i); // fix syntax error in official libary! :)
    end;
    FileFromString(mustache,mustacheFN);
    mSource := SynUnicode(mustache);
  end;
  fEngine.Evaluate(mSource,'mustache.js');
  fEngine.Evaluate(testMustacheFunc,'testMustacheFunc.js');
end;
{$endif}

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  {$ifndef CPU64} // SpiderMonkey library is not available yet in 64 bit
  fEngineManager.Free;
  {$endif}
end;

end.
