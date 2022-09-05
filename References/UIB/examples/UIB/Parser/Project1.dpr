program Project1;

{%File 'uibSQLLexer.l'}
{%File 'uibSQLParser.y'}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uibSQLParser in 'uibSQLParser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
