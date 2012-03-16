unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, ExtCtrls, StdCtrls, AlScrollBar, ALMemo, ALButton,
  OleCtrls, SHDocVw, ComObj;

type
  TForm1 = class(TForm)
    ALButton1: TALButton;
    ALButton3: TALButton;
    Panel2: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Panel3: TPanel;
    PanelWebBrowser: TPanel;
    ALButton2: TALButton;
    ALButton4: TALButton;
    procedure ALButtonPaint(Sender: TObject; var continue: Boolean);
    procedure FormClick(Sender: TObject);
    procedure ALButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ALButton2Click(Sender: TObject);
    procedure ALButton3Click(Sender: TObject);
    procedure ALButton4Click(Sender: TObject);
  private
  public
  end;

var Form1: TForm1;

implementation

uses alFcnSkin,
     AlFcnString,
     alavlBinaryTree;

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
Var aStringKeyAVLBinaryTree: TALStringKeyAVLBinaryTree;
    aStringKeyAVLBinaryTreeNode: TALStringKeyAVLBinaryTreeNode;
    StartDate: DWORD;
    i: integer;
begin
  aStringKeyAVLBinaryTree := TALStringKeyAVLBinaryTree.create;
  try

    StartDate := GetTickCount;
    for I := 1 to 1000000 do begin
      aStringKeyAVLBinaryTreeNode := TALStringKeyAVLBinaryTreeNode.Create;
      aStringKeyAVLBinaryTreeNode.ID := AlRandomStr(10);
      if not aStringKeyAVLBinaryTree.AddNode(aStringKeyAVLBinaryTreeNode) then aStringKeyAVLBinaryTreeNode.Free;
    end;
    Showmessage('Add 1.000.000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms');

  finally
    aStringKeyAVLBinaryTree.free;
  end;
end;

{***********************************************}
procedure TForm1.ALButton2Click(Sender: TObject);
Var aLst: TStringList;
    StartDate: DWORD;
    i: integer;
begin
  aLst := TstringList.create;
  try
    aLst.Sorted := False;

    StartDate := GetTickCount;
    for I := 1 to 1000000 do
      aLst.Add(AlRandomStr(10));
    aLst.Duplicates := DupIgnore;
    aLst.Sorted := True;
    Showmessage('Add 1.000.000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms');

  finally
    aLst.free;
  end;
end;

{***********************************************}
procedure TForm1.ALButton3Click(Sender: TObject);
Var aStringKeyAVLBinaryTree: TALStringKeyAVLBinaryTree;
    aStringKeyAVLBinaryTreeNode: TALStringKeyAVLBinaryTreeNode;
    StartDate: DWORD;
    i: integer;
begin
  aStringKeyAVLBinaryTree := TALStringKeyAVLBinaryTree.create;
  try

    StartDate := GetTickCount;
    for I := 1 to 1000000 do begin
      aStringKeyAVLBinaryTreeNode := TALStringKeyAVLBinaryTreeNode.Create;
      aStringKeyAVLBinaryTreeNode.ID := AlRandomStr(10);
      if not aStringKeyAVLBinaryTree.AddNode(aStringKeyAVLBinaryTreeNode) then aStringKeyAVLBinaryTreeNode.Free;
    end;

    Showmessage('Add 1.000.000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms');

    StartDate := GetTickCount;
    for I := 1 to 100000 do begin
      aStringKeyAVLBinaryTree.FindNode(AlRandomStr(10));
    end;

    Showmessage('Search 100.000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms');

  finally
    aStringKeyAVLBinaryTree.free;
  end;
end;

{***********************************************}
procedure TForm1.ALButton4Click(Sender: TObject);
Var aLst: TStringList;
    StartDate: DWORD;
    i: integer;
begin
  aLst := TstringList.create;
  try
    aLst.Sorted := False;

    StartDate := GetTickCount;
    for I := 1 to 1000000 do
      aLst.Add(AlRandomStr(10));
    aLst.Duplicates := DupIgnore;
    aLst.Sorted := True;
    Showmessage('Add 1.000.000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms');

    StartDate := GetTickCount;
    for I := 1 to 100000 do begin
      aLst.IndexOf(AlRandomStr(10));
    end;
    Showmessage('Search 100.000 nodes in ' + inttostr(GetTickCount - StartDate) + ' ms');

  finally
    aLst.free;
  end;
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
