program ArchiveDemo;

uses
  Forms,
  UMain in 'UMain.pas' {FormMain},
  UProperties in 'UProperties.pas' {FormArchiveSettings};

{$R *.res}
{$R ..\..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
