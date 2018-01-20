program mongoproj;

uses
  Forms,
  mormotMongo in 'mormotMongo.pas'{FrmServidor},
  Form2 in 'Form2.pas';
  
{$R *.res}
{$R 'btn.res'}
begin
  Application.Initialize;
  Application.CreateForm(TFrmServidor, FrmServidor);
  Application.CreateForm(TItems, Items);
  Application.Run;
end.
