unit UnitDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs;

type
  TDemoForm = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DemoForm: TDemoForm;

implementation

{$R *.fmx}

procedure TDemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action := TcloseAction.caFree;
end;

end.
