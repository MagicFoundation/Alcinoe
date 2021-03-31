program ErrorReportingSample;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMain in 'FMain.pas' {FormMain},
  FReport in 'FReport.pas' {FormReport},
  Grijjy.ErrorReporting in '..\Grijjy.ErrorReporting.pas',
  Grijjy.SymbolTranslator in '..\Grijjy.SymbolTranslator.pas',
  UFoo in 'UFoo.pas',
  UThreads in 'UThreads.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
