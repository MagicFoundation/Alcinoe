unit SingleInstDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    DialogBtn: TButton;
    procedure DialogBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

// See Project file source (SingleInstExample.dpr) for added single instance
// checking code.

procedure TForm1.DialogBtnClick(Sender: TObject);
begin
  ShowMessage('This is a modal dialog.');
end;

end.
