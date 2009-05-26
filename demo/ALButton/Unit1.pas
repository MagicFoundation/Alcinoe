unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ALButton, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Main_TopPanel: TPanel;
    BtnNavPrior: TALGraphicButton;
    BtnNavNext: TALGraphicButton;
    btnNavHome: TALGraphicButton;
    TopBtn_ImageList: TImageList;
    Button1: TButton;
    ALButton1: TALButton;
    ALRadioButton1: TALRadioButton;
    ALCheckBox1: TALCheckBox;
    Image1: TImage;
    RadioButton1: TRadioButton;
    CheckBox1: TCheckBox;
    Panel1: TPanel;
    Label5: TLabel;
    procedure ALButton1Paint(Sender: TObject; var continue: Boolean);
    procedure ALRadioButton1Paint(Sender: TObject; var continue: Boolean);
    procedure ALCheckBox1Paint(Sender: TObject; var continue: Boolean);
    procedure btnNavHomePaint(Sender: TObject; var continue: Boolean);
    procedure BtnNavPriorClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses alfcnSkin;

procedure TForm1.ALButton1Paint(Sender: TObject; var continue: Boolean);
begin
  PaintAlButtonBlueSkin(Sender, Continue);
end;

procedure TForm1.ALRadioButton1Paint(Sender: TObject;
  var continue: Boolean);
begin
  PaintAlRadioButtonBlueSkin(Sender, Continue);
end;

procedure TForm1.ALCheckBox1Paint(Sender: TObject; var continue: Boolean);
begin
  PaintAlCheckBoxBlueSkin(Sender, Continue);
end;

procedure TForm1.btnNavHomePaint(Sender: TObject; var continue: Boolean);
Var OldFontColor: Tcolor;
begin
  With Sender As TalGraphicButton Do begin
    OldFontColor := Font.Color;

    {Ctrl Enabled}
    if Enabled then begin
       {CTRL down}
       If State = cbchecked then begin
         GlyphIndex := 3*tag + 1;
         Font.Color := $007BDFE1
       end

       {CTRL UP}
       else if State = cbunchecked then begin
         {ctrl pushed or MouseIn}
         If MouseIsDown or
            KeyIsDown or
            MouseInControl then begin
           GlyphIndex := 3*tag + 1;
           Font.Color := $007BDFE1;

         end
         {ctrl MouseOut}
         else begin
           GlyphIndex := 3*tag + 0;
           Font.Color := $00F0FFFF;
         end;
       end
    end

    {Button Disabled}
    else begin
      GlyphIndex := 3*tag + 2;
      Font.Color := $00AFAFAF;
    end;

    Continue := OldFontColor = Font.Color;    
  end;
end;


procedure TForm1.BtnNavPriorClick(Sender: TObject);
begin
  Showmessage('Onclick Event');
end;

end.
