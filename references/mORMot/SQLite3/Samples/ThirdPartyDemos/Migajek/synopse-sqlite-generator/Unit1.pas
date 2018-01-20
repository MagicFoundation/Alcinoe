unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  regexpr;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    generate: TButton;
    Edit1: TEdit;
    Memo2: TMemo;
    parse: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    procedure generateClick(Sender: TObject);
    procedure parseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  tstrpair = record
    sFrom: string;
    sTo: string;
  end;

const
  conversionTypes: array [0..1] of tstrpair = (
    (sFrom: 'str'; sTo: 'RawUTF8'),
    (sFrom: 'int'; sTo: 'integer')
  );

function aliasToType(s: string) : string ;
var i: integer;
begin
  result:= s;
  s:= lowercase(s);
  for i:= low(conversionTypes) to high(conversionTypes) do
    if conversionTypes[i].sFrom = s then
      begin
        result:= conversionTypes[i].sTo;
        break;
      end;
end;

function typeToAlias(s: string) : string ;
var i: integer;
begin
  result:= s;
  s:= lowercase(s);
  for i:= low(conversionTypes) to high(conversionTypes) do
    if lowercase(conversionTypes[i].sTo) = s then
      begin
        result:= conversionTypes[i].sFrom;
        break;
      end;
end;

procedure TForm1.generateClick(Sender: TObject);
var
 r: TRegExpr;
 clsname,
 priv, publ,
 functs, impl,
 stype, sname,
 fwrite, fread: string;
begin
clsname:= 'TSQL'+edit1.Text;

r:= TRegExpr.Create;
r.Expression:= '(\s+)?(\w+)(\s+)?:(\s+)?(\w+)(\s+)?(\;r)?(\;w)?';
if r.Exec(memo1.Text) then
  repeat
    stype:= aliasToType( r.match[5] );
    sname:= r.match[2];

    if (length(r.Match[7]) > 0) then
      begin
        fread := 'Get'+sname;
        functs:= functs + #13#10#9#9 + format('function Get%s():%s;'#13#10, [sname, stype]);
        impl:= impl + #13#10 + format('function %s.Get%s():%s;'#13#10'begin'#13#10#9'result:= f%s;'#13#10'end;'#13#10, [clsname, sname, stype, sname]);
      end
    else
      fRead:= 'f'+sname;

    if (length(r.Match[8]) > 0) then
      begin
        fwrite := 'Set'+sname;
        functs:= functs + #13#10#9#9 + format('procedure Set%s(const AValue: %s);'#13#10, [sname, stype]);
        impl:= impl + #13#10 + format('procedure %s.Set%s(const AValue: %s);'#13#10'begin'#13#10#9'f%s:= AValue;'#13#10'end;'#13#10, [clsname, sname, stype, sname]);
      end
    else
      fWrite:= 'f'+sname;

    priv:= priv + #9#9 + format('f%s: %s;'#13#10, [sname, stype]);
    publ:= publ + #9#9 + format('property %s: %s read %s write %s;'#13#10, [sname, stype, fread, fwrite]);
  until not r.ExecNext;
memo2.Text:=
format(
  #9'%s = class(TSQLRecord)'#13#10 +
  #9'private'#13#10'%s'#13#10'%s'+
  #9'published'#13#10'%s'#13#10+
  #9'end;'#13#10#13#10#13#10'%s',
  [clsname, priv, functs, publ, impl]);
end;

procedure TForm1.parseClick(Sender: TObject);
var
 r: TRegExpr;
 def: string;
begin
r:= TRegExpr.Create;

r.Expression:= '\s+?TSQL(\w+)\s*=\s*class';
if r.Exec(memo2.Text) then
  edit1.Text:= r.Match[1];

r.Expression:= 'property\s+(\w+)(\s+)?:(\s+)?(\w+)\s*(read (\w+))?\s*(write (\w+))?';
//r.Expression:= '(\w+):(\w+)';
//r.ModifierM
if r.Exec(memo2.Text) then
  repeat
    def:= def + r.Match[1] + ':'+ typeToAlias( r.Match[4] );
    //showmessage(r.Match[6]);
    if Copy(r.match[6], 1, 3) = 'Get' then
      def:= def + ';r';
    if Copy(r.match[8], 1, 3) = 'Set' then
      def:= def + ';w';

    def:= def + #13#10;
  until not r.ExecNext;
memo1.Text:=def;
end;

procedure TForm1.Button1Click(Sender: TObject);
const
  sHelp =
    'This simple application speeds up defining SQL Records classes for Synopse mORMot Framework'#13#10+
    'Since some versions of Delphi (including mine) does not have class completion, declaring properties,'#13#10+
    'their setters / getters and so on is a huge waste of time.'#13#10+
    'Thus I''ve written this simple tool which introduces so-called meta-language for defining SQL Records'#13#10+
    'which is later converted to Delphi code.'#13#10+
    'It is simply list of field declarations we want to have in our SQL Record.'#13#10#13#10+
    'Syntax:'#13#10+
    'each line is a declaration of one field. It consists of field name and it''s type'#13#10+
    'the type might be shortened using aliases (see below).'#13#10+
    'additionally, each field can be marked with ";r" and/or ";w" flags'#13#10+
    'which will make the code generator to use respectively getter and/or setter for the specific field'#13#10+
    'The tool also makes it possible to parse back from Delphi class declaration to meta-code'#13#10+
    'Please note this will only work if the Delphi class follows naming pattern used in meta2pas generatorion'#13#10+
    #13#10'List of available aliases:'#13#10'%s'#13#10+
    '================'#13#10+
    'enjoy!'#13#10+
    'Michal migajek Gajek'#13#10'http://migajek.com'#13#10'migajek@gmail.com';
var
  i: integer;
  tmp: string;
begin
  for i:= low(conversionTypes) to high(conversionTypes) do
    tmp:= tmp + conversionTypes[i].sFrom + ' = ' + conversionTypes[i].sTo + #13#10;

  MessageBox(handle, PChar(Format(sHelp, [tmp])), 'Help', MB_ICONINFORMATION);
end;

end.
