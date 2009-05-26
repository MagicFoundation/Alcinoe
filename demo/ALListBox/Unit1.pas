unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ALListBox, StdCtrls, AlScrollBar;

type
  TForm1 = class(TForm)
    ALListBox1: TALListBox;
    ListBox2: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Label5: TLabel;
    procedure ALListBox1Paint(Sender: TObject; var continue: Boolean);
    procedure ALListBox1PaintScrollBar(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
  private
  public
  end;

var Form1: TForm1;

implementation

uses alFcnSkin;

{$R *.dfm}

procedure TForm1.ALListBox1Paint(Sender: TObject; var continue: Boolean);
begin
  paintAlListBoxBlueSkin(sender, Continue);
end;

procedure TForm1.ALListBox1PaintScrollBar(Sender: TObject;
  var continue: Boolean; Area: TALScrollbarArea);
begin
  paintAlListBoxScrollBarBlueSkin(sender, Continue, area);
end;

end.
