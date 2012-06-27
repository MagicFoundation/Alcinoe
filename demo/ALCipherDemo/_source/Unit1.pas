unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ALListBox, StdCtrls, AlScrollBar, ALButton, ComCtrls,
  ALMemo, ALEdit, OleCtrls, SHDocVw, ComObj;

type
  TForm1 = class(TForm)
    ALButton1: TALButton;
    StatusBar1: TStatusBar;
    ALButton3: TALButton;
    ALButton4: TALButton;
    Label1: TLabel;
    EditKey: TALEdit;
    ALMemoDecryptedData: TALMemo;
    Label2: TLabel;
    ALMemoCryptedData: TALMemo;
    Label3: TLabel;
    ALButton2: TALButton;
    ALButton5: TALButton;
    ALButton6: TALButton;
    ALButton7: TALButton;
    ALButton8: TALButton;
    ALButton9: TALButton;
    Panel2: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Panel3: TPanel;
    PanelWebBrowser: TPanel;
    procedure ALButton1Click(Sender: TObject);
    procedure ALButton1Paint(Sender: TObject; var continue: Boolean);
    procedure ALButton3Click(Sender: TObject);
    procedure ALButton4Click(Sender: TObject);
    procedure ALButton2Click(Sender: TObject);
    procedure ALButton5Click(Sender: TObject);
    procedure ALMemoDecryptedDataPaint(Sender: TObject; var continue: Boolean);
    procedure ALMemoDecryptedDataPaintScrollBar(Sender: TObject; var continue: Boolean;
      Area: TALScrollbarArea);
    procedure EditKeyPaint(Sender: TObject; var continue: Boolean);
    procedure ALButton6Click(Sender: TObject);
    procedure ALButton7Click(Sender: TObject);
    procedure ALButton8Click(Sender: TObject);
    procedure ALButton9Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  public
  end;

var Form1: TForm1;

implementation

uses alFcnSkin,
     alFcnMisc,
     AlAVLBinaryTree,
     alFcnString,
     alFcnMime,
     alCipher;

{$R *.dfm}

{*******************************************************}
function internalRandomStr(aLength: Longint): AnsiString;
var X: Longint;
begin
  if aLength <= 0 then exit;
  SetLength(Result, aLength);
  for X:=1 to aLength do Result[X] := ansiChar(Random(256));
end;

{***********************************************}
procedure TForm1.ALButton1Click(Sender: TObject);
Var Key: AnsiString;
    Data: AnsiString;
    Str1, Str2: AnsiString;
    Counter: integer;
    DataCount: longint;
    StartTime: DWORD;
    RemoveTime: DWORD;
begin
  if (Sender as TALButton).Tag = 1 then begin
    (Sender as TALButton).Tag := 0;
    (Sender as TALButton).Caption := 'Bench AES (EBC)';
    exit;
  end;
  (Sender as TALButton).Tag := 1;
  (Sender as TALButton).Caption := 'Stop';
  randomize;
  Counter := 0;
  DataCount := 0;
  StartTime := GetTickCount;
  StatusBar1.Panels[0].Text := '';
  ALMemoDecryptedData.SetFocus;
  while True do begin
    RemoveTime := GetTickCount;
    if (Sender as TALButton).Tag = 0 then break;
    Data := internalRandomStr(random(32768));
    datacount := datacount + length(data);
    Key := AlRandomStr(Random(40));
    StartTime := StartTime + (GetTickCount - RemoveTime);
    ALRDLEncryptString(Data, Str1, Key, True);
    ALRDLEncryptString(Str1, Str2, Key, false);
    if str2 <> Data then Raise Exception.Create('!!Abnormal Error!!');
    inc(counter);
    if counter mod 100 = 0 then begin
      StatusBar1.Panels[0].Text := 'AES (EBC): ' + IntToStr(round((datacount / 1000) / ((GetTickCount - StartTime) / 1000))) + ' ko/s';
      application.ProcessMessages;
    end;
  end;
end;

{***********************************************}
procedure TForm1.ALButton3Click(Sender: TObject);
Var Key: AnsiString;
    Data: AnsiString;
    Str1, Str2: AnsiString;
    Counter: integer;
    DataCount: longint;
    StartTime: DWORD;
    RemoveTime: DWORD;
begin
  if (Sender as TALButton).Tag = 1 then begin
    (Sender as TALButton).Tag := 0;
    (Sender as TALButton).Caption := 'Bench AES (CBC)';
    exit;
  end;
  (Sender as TALButton).Tag := 1;
  (Sender as TALButton).Caption := 'Stop';
  randomize;
  Counter := 0;
  DataCount := 0;
  StartTime := GetTickCount;
  StatusBar1.Panels[1].Text := '';
  ALMemoDecryptedData.SetFocus;
  while True do begin
    RemoveTime := GetTickCount;
    if (Sender as TALButton).Tag = 0 then break;
    Data := internalRandomStr(random(32768));
    datacount := datacount + length(data);
    Key := AlRandomStr(Random(40));
    StartTime := StartTime + (GetTickCount - RemoveTime);
    ALRDLEncryptStringCBC(Data, Str1, Key, True);
    ALRDLEncryptStringCBC(Str1, Str2, Key, false);
    if str2 <> Data then Raise Exception.Create('!!Abnormal Error!!');
    inc(counter);
    if counter mod 100 = 0 then begin
      StatusBar1.Panels[1].Text := 'AES (CBC): ' + IntToStr(round((datacount / 1000) / ((GetTickCount - StartTime) / 1000))) + ' ko/s';
      application.ProcessMessages;
    end;
  end;
end;

{***********************************************}
procedure TForm1.ALButton4Click(Sender: TObject);
Var Key: AnsiString;
    Data: AnsiString;
    Str1, Str2: AnsiString;
    Counter: integer;
    DataCount: longint;
    StartTime: DWORD;
    RemoveTime: DWORD;
begin
  if (Sender as TALButton).Tag = 1 then begin
    (Sender as TALButton).Tag := 0;
    (Sender as TALButton).Caption := 'Bench Blowfish (EBC)';
    exit;
  end;
  (Sender as TALButton).Tag := 1;
  (Sender as TALButton).Caption := 'Stop';
  randomize;
  Counter := 0;
  DataCount := 0;
  StartTime := GetTickCount;
  StatusBar1.Panels[2].Text := '';
  ALMemoDecryptedData.SetFocus;
  while True do begin
    RemoveTime := GetTickCount;
    if (Sender as TALButton).Tag = 0 then break;
    Data := internalRandomStr(random(32768));
    datacount := datacount + length(data);
    Key := AlRandomStr(Random(40));
    StartTime := StartTime + (GetTickCount - RemoveTime);
    ALBFEncryptString(Data, Str1, Key, True);
    ALBFEncryptString(Str1, Str2, Key, false);
    if str2 <> Data then Raise Exception.Create('!!Abnormal Error!!');
    inc(counter);
    if counter mod 100 = 0 then begin
      StatusBar1.Panels[2].Text := 'BF (EBC): ' + IntToStr(round((datacount / 1000) / ((GetTickCount - StartTime) / 1000))) + ' ko/s';
      application.ProcessMessages;
    end;
  end;
end;

{***********************************************}
procedure TForm1.ALButton5Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALBFEncryptStringCBC(ALMimeBase64DecodeString(ALTrim(AnsiString(ALMemocryptedData.Lines.Text))), outString, AnsiString(EditKey.Text), False);
  ALMemoDeCryptedData.Lines.Text := String(outString);
end;

{***********************************************}
procedure TForm1.ALButton6Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALRDLEncryptStringCBC(AnsiString(ALMemoDecryptedData.Lines.Text), outString, AnsiString(EditKey.Text), True);
  ALMemoCryptedData.Lines.Text := String(ALMimeBase64EncodeStringNoCRLF(outString));
end;

{***********************************************}
procedure TForm1.ALButton7Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALRDLEncryptStringCBC(ALMimeBase64DecodeString(ALTrim(ansiString(ALMemocryptedData.Lines.Text))), outString, ansiString(EditKey.Text), False);
  ALMemoDeCryptedData.Lines.Text := string(outString);
end;

{***********************************************}
procedure TForm1.ALButton8Click(Sender: TObject);
Var aBinTree: TALStringKeyAVLBinaryTree;
    aNode: TALStringKeyAVLBinaryTreeNode;
    Counter: Integer;
begin
  if (Sender as TALButton).Tag = 1 then begin
    (Sender as TALButton).Tag := 0;
    (Sender as TALButton).Caption := 'Check SHA1 Hash';
    exit;
  end;
  (Sender as TALButton).Tag := 1;
  (Sender as TALButton).Caption := 'Stop';

  StatusBar1.Panels[0].Text := '';
  aBinTree := TALStringKeyAVLBinaryTree.Create;
  counter := 0;
  while true do begin
    if (Sender as TALButton).Tag = 0 then break;
    aNode := TALStringKeyAVLBinaryTreeNode.Create;
    ANode.ID := ALStringHashSHA1(ALMakeKeyStrByGUID + ' ' + internalRandomStr(random(8192)));
    if not aBinTree.AddNode(aNode) then begin
      Showmessage('1 Collision Found!');
      aNode.free;
      Exit;
    end;
    inc(counter);
    if (counter mod 1000 = 0) then begin
      StatusBar1.Panels[0].Text := IntToStr(counter) + ' items generated';
      application.ProcessMessages;
    end;
  end;
  aBinTree.free;
end;

{***********************************************}
procedure TForm1.ALButton9Click(Sender: TObject);
Var aBinTree: TALStringKeyAVLBinaryTree;
    aNode: TALStringKeyAVLBinaryTreeNode;
    Counter: Integer;
begin
  if (Sender as TALButton).Tag = 1 then begin
    (Sender as TALButton).Tag := 0;
    (Sender as TALButton).Caption := 'Check MD5 Hash';
    exit;
  end;
  (Sender as TALButton).Tag := 1;
  (Sender as TALButton).Caption := 'Stop';

  aBinTree := TALStringKeyAVLBinaryTree.Create;
  counter := 0;
  StatusBar1.Panels[1].Text := '';
  while true do begin
    if (Sender as TALButton).Tag = 0 then break;
    aNode := TALStringKeyAVLBinaryTreeNode.Create;
    ANode.ID := ALStringHashMD5(ALMakeKeyStrByGUID + ' ' + internalRandomStr(random(2048)));
    if not aBinTree.AddNode(aNode) then begin
      Showmessage('1 Collision Found!');
      aNode.free;
      Exit;
    end;
    inc(counter);
    if (counter mod 1000 = 0) then begin
      StatusBar1.Panels[1].Text := IntToStr(counter) + ' items generated';
      application.ProcessMessages;
    end;
  end;
  aBinTree.free;
end;

{********************************************************************************}
procedure TForm1.ALMemoDecryptedDataPaint(Sender: TObject; var continue: Boolean);
begin
  paintAlMemoBlueSkin(sender, Continue);
end;

{****************************************************************************************}
procedure TForm1.ALMemoDecryptedDataPaintScrollBar(Sender: TObject; var continue: Boolean;
  Area: TALScrollbarArea);
begin
  paintAlMemoScrollBarBlueSkin(sender, Continue, area);
end;

{********************************************************************}
procedure TForm1.EditKeyPaint(Sender: TObject; var continue: Boolean);
begin
  PaintAlEditBlueSkin(Sender, Continue);
end;

{***********************************************}
procedure TForm1.ALButton2Click(Sender: TObject);
Var outString: AnsiString;
begin
  ALBFEncryptStringCBC(AnsiString(ALMemoDecryptedData.Lines.Text), outString, AnsiString(EditKey.Text), True);
  ALMemoCryptedData.Lines.Text := String(ALMimeBase64EncodeStringNoCRLF(outString));
end;

{**********************************************************************}
procedure TForm1.ALButton1Paint(Sender: TObject; var continue: Boolean);
begin
  PaintAlButtonBlueSkin(Sender, Continue);
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
