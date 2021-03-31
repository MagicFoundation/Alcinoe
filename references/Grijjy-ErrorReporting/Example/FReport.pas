unit FReport;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TFormReport = class(TForm)
    ToolBar: TToolBar;
    LabelReport: TLabel;
    ButtonBack: TSpeedButton;
    Memo: TMemo;
    procedure ButtonBackClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowReport(const AReport: String);
  end;

var
  FormReport: TFormReport = nil;

procedure ShowReport(const AReport: String);

implementation

{$R *.fmx}

procedure ShowReport(const AReport: String);
begin
  if (FormReport = nil) then
    FormReport := TFormReport.Create(Application);
  FormReport.ShowReport(AReport);
end;

{ TFormReport }

procedure TFormReport.ButtonBackClick(Sender: TObject);
begin
  Hide;
end;

procedure TFormReport.ShowReport(const AReport: String);
begin
  Memo.Text := AReport;
  Show;
end;

end.
