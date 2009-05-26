unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, AlScrollBar, ALMemo;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    ALMemo1: TALMemo;
    Panel1: TPanel;
    Label5: TLabel;
    procedure ALMemo1Paint(Sender: TObject; var continue: Boolean);
    procedure ALMemo1PaintScrollBar(Sender: TObject; var continue: Boolean;
      Area: TALScrollbarArea);
  private
  public
  end;

var Form1: TForm1;

implementation

uses alFcnSkin;

{$R *.dfm}


procedure TForm1.ALMemo1Paint(Sender: TObject; var continue: Boolean);
begin
  paintAlMemoBlueSkin(sender, Continue);
end;

procedure TForm1.ALMemo1PaintScrollBar(Sender: TObject;
  var continue: Boolean; Area: TALScrollbarArea);
begin
  paintAlMemoScrollBarBlueSkin(sender, Continue, area);
end;

end.
