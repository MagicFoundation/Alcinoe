program FBReporter;

uses
  Forms,
  frxClass,
  frxDesgn,
  PNGImage,
  frxUIBComponents;

{$R *.res}
var
  Report: TfrxReport;

begin
  Application.Initialize;

  Report := TfrxReport.Create(nil);
  try
    Report.DesignReport;
  finally
    Report.Free;
  end;
end.
