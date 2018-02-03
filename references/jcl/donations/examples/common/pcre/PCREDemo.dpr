program PCREDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JclPCRE Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
