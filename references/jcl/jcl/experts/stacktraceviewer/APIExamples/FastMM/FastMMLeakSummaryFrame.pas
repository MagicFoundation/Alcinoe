unit FastMMLeakSummaryFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, FastMMParser;

type
  TfrmLeakSummary = class(TFrame)
    memSummary: TMemo;
  private
    { Private-Deklarationen }
    FReport: TFastMMReport;
    procedure SetReport(const Value: TFastMMReport);
  public
    { Public-Deklarationen }
    property Report: TFastMMReport write SetReport;
  end;

implementation

{$R *.dfm}

{ TfrmLeakSummary }

procedure TfrmLeakSummary.SetReport(const Value: TFastMMReport);
begin
  FReport := Value;
  if Assigned(FReport) then
    memSummary.Lines.Assign(FReport.LeakSummary)
  else
    memSummary.Lines.Clear;
end;

end.
