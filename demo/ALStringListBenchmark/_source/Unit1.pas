unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, AlScrollBar, ALMemo, ALButton,
  OleCtrls, SHDocVw, ComObj;

type
  TForm1 = class(TForm)
    ALButton1: TALButton;
    ALButton2: TALButton;
    ALButton3: TALButton;
    ALButton4: TALButton;
    Panel2: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Panel3: TPanel;
    PanelWebBrowser: TPanel;
    procedure ALButtonPaint(Sender: TObject; var continue: Boolean);
    procedure FormClick(Sender: TObject);
    procedure ALButton1Click(Sender: TObject);
    procedure ALButton2Click(Sender: TObject);
    procedure ALButton3Click(Sender: TObject);
    procedure ALButton4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
  end;

var Form1: TForm1;

implementation

uses alFcnSkin,
     alStringList,
     alFcnString;

{$R *.dfm}

{*********************************************************************}
procedure TForm1.ALButtonPaint(Sender: TObject; var continue: Boolean);
begin
  paintAlButtonBlueSkin(sender, Continue);
end;

{******************************************}
procedure TForm1.FormClick(Sender: TObject);
begin
  Windows.SetFocus(0);
end;

{***********************************************}
procedure TForm1.ALButton1Click(Sender: TObject);
Var Lst1: TALStringList;
    StartDate: DWORD;
    i: integer;
begin
  StartDate := GetTickCount;
  Lst1 := TALStringList.create;
  try
    Lst1.Sorted := True;
    Lst1.Duplicates := DupIgnore;

    for I := 1 to 10000 do
      Lst1.Values[AlRandomStr(5)] := AlRandomStr(5);

  finally
    Lst1.free;
  end;
  Showmessage('Process 10.000 Values[random(x)]:=random(y) in ' + inttostr(GetTickCount - StartDate) + ' ms');
end;

{**********************************************}
procedure TForm1.ALButton2Click(Sender: TObject);
Var Lst1: TStringList;
    StartDate: DWORD;
    i: integer;
begin
  Showmessage('You can go to take a coffee during the process!');
  StartDate := GetTickCount;
  Lst1 := TStringList.create;
  try
    Lst1.Sorted := False;

    for I := 1 to 10000 do
      Lst1.Values[AlRandomStr(5)] := AlRandomStr(5);

  finally
    Lst1.free;
  end;
  Showmessage('Process 10.000 Values[random(x)]:=random(y) in ' + inttostr(GetTickCount - StartDate) + ' ms');
end;


{***********************************************}
procedure TForm1.ALButton3Click(Sender: TObject);
Var Lst1: TALAVLStringList;
    StartDate: DWORD;
    i: integer;
begin
  StartDate := GetTickCount;
  Lst1 := TALAVLStringList.create;
  try

    for I := 1 to 100000 do
      Lst1.Values[AlRandomStr(5)] := AlRandomStr(5);

  finally
    Lst1.free;
  end;
  Showmessage('Process 100.000 Values[random(x)]:=random(y) in ' + inttostr(GetTickCount - StartDate) + ' ms');
end;


{***********************************************}
procedure TForm1.ALButton4Click(Sender: TObject);
Var Lst1: TStringList;
    StartDate: DWORD;
    i: integer;
begin
  Showmessage('You can go to in holidays during the process!');
  StartDate := GetTickCount;
  Lst1 := TStringList.create;
  try
    Lst1.Sorted := False;

    for I := 1 to 100000 do
      Lst1.Values[AlRandomStr(5)] := AlRandomStr(5);

  finally
    Lst1.free;
  end;
  Showmessage('Process 100.000 Values[random(x)]:=random(y) in ' + inttostr(GetTickCount - StartDate) + ' ms');
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

{$IFDEF DEBUG}
initialization
  ReportMemoryleaksOnSHutdown := True;
{$ENDIF}

end.
