unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ALButton, ExtCtrls, StdCtrls, OleCtrls, SHDocVw, ComObj;

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
    Panel2: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Panel3: TPanel;
    PanelWebBrowser: TPanel;
    procedure ALButton1Paint(Sender: TObject; var continue: Boolean);
    procedure ALRadioButton1Paint(Sender: TObject; var continue: Boolean);
    procedure ALCheckBox1Paint(Sender: TObject; var continue: Boolean);
    procedure btnNavHomePaint(Sender: TObject; var continue: Boolean);
    procedure BtnNavPriorClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
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



{-------------------}
var ie: IWebBrowser2;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
var Url, Flags, TargetFrameName, PostData, Headers: OleVariant;
begin
  ie := CreateOleObject('InternetExplorer.Application') as IWebBrowser2;
  SetWindowLong(ie.hwnd, GWL_STYLE, GetWindowLong(ie.hwnd, GWL_STYLE) and not WS_BORDER and not WS_SIZEBOX and not WS_DLGFRAME );
  SetWindowPos(ie.hwnd, HWND_TOP, Left, Top, Width, Height, SWP_FRAMECHANGED);
  windows.setparent(ie.hwnd, PanelWebBrowser.handle);
  ie.Left := maxint; // don't understand why it's look impossible to setup the position
  ie.Top  := maxint; // don't understand why it's look impossible to setup the position
  ie.MenuBar := false;
  ie.AddressBar := false;
  ie.Resizable := false;
  ie.StatusBar := false;
  ie.ToolBar := 0;
  ie.Width := 100;
  ie.Height := 300;
  Url := 'http://static.arkadia.com/html/alcinoe_like.html';
  ie.Navigate2(Url,Flags,TargetFrameName,PostData,Headers);
  ie.Visible := true;
end;

{********************************************************************}
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    ie.quit;
  except
  end;
  sleep(500);
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
