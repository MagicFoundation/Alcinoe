program ClrDemo;

{$I jcl.inc}

uses
  Forms,
  ClrDemoMain in 'ClrDemoMain.pas' {frmMain},
  ClrDemoAbstractFrame in 'ClrDemoAbstractFrame.pas' {frmAbstract: TFrame},
  ClrDemoMetaDataFrame in 'ClrDemoMetaDataFrame.pas' {frmMetadata: TFrame},
  ClrDemoStringsForm in 'ClrDemoStringsForm.pas' {frmStrings},
  ClrDemoGuidForm in 'ClrDemoGuidForm.pas' {frmGuid},
  ClrDemoBlobForm in 'ClrDemoBlobForm.pas' {frmBlobs},
  ClrDemoTableForm in 'ClrDemoTableForm.pas' {frmTable},
  ClrDemoUserStringsForm in 'ClrDemoUserStringsForm.pas' {frmUserStrings},
  ClrDemoCLRFrame in 'ClrDemoCLRFrame.pas' {frmCLR: TFrame};

{$R *.RES}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
