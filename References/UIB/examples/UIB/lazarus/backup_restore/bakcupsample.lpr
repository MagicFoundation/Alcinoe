program bakcupsample;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms,
  frm_bakcupsample;

begin
  Application.Initialize;
  Application.CreateForm(TfrmBakcup, frmBakcup);
  Application.Run;
end.
