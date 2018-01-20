program JSONconsole;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  SynCrossPlatformJSON;

type

  { TJSONConsole }

  TJSONConsole = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TJSONConsole }

procedure TJSONConsole.DoRun;
var ErrorMsg: String;
    doc: variant;
    json: string;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  {$ASSERTIONS ON}
  writeln('starting');
  doc := JSONVariant('{"test":1234,"name":"Joh\"n\r","zero":0.0}');
  assert(doc.test=1234);
  assert(doc.name='Joh"n'#13);
  assert(doc.name2=null);
  assert(doc.zero=0);
  json := doc;
  writeln(json);
  assert(json='{"test":1234,"name":"Joh\"n\r","zero":0}');
  TJSONVariantData(doc)['name2'] := 3.1415926;
  TJSONVariantData(doc)['name'] := 'John';
  assert(doc.test=1234);
  assert(doc.name='John',doc.name);
  assert(doc.name2>3);
  assert(doc.zero=0);
  json := doc;
  assert(json='{"test":1234,"name":"John","zero":0,"name2":3.1415926}');
  writeln(json);
  readln;

  // stop program loop
  Terminate;
end;

constructor TJSONConsole.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TJSONConsole.Destroy;
begin
  inherited Destroy;
end;

procedure TJSONConsole.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TJSONConsole;
begin
  Application:=TJSONConsole.Create(nil);
  Application.Title:='JSON Console';
  Application.Run;
  Application.Free;
end.

