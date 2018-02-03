unit MapWriteForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TWriteForm = class(TForm)
    Panel1: TPanel;
    mmWrite: TMemo;
    cmdWrite: TButton;
    procedure cmdWriteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WriteForm: TWriteForm;

implementation

{$R *.DFM}

procedure TWriteForm.cmdWriteClick(Sender: TObject);
begin
  Close;
end;

end.
