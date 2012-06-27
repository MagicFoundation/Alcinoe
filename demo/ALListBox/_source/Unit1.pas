unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ALListBox, StdCtrls, AlScrollBar, OleCtrls, SHDocVw,
  ComObj;

type
  TForm1 = class(TForm)
    ALListBox1: TALListBox;
    ListBox2: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel2: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Panel3: TPanel;
    PanelWebBrowser: TPanel;
    procedure ALListBox1Paint(Sender: TObject; var continue: Boolean);
    procedure ALListBox1PaintScrollBar(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
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
  ie.Width := 100;
  ie.Height := 300;
  ie.MenuBar := false;
  ie.AddressBar := false;
  ie.Resizable := false;
  ie.StatusBar := false;
  ie.ToolBar := 0;
  Url := 'http://www.arkadia.com/html/alcinoe_like.html';
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
