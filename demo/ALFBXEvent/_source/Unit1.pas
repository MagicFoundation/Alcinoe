unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ALComboBox, ALEdit, ALMemo, ALButton, ExtCtrls, AlScrollBar,
  ALFBXclient, OleCtrls, SHDocVw, ComObj;

type

  TMyEventThread = class(TALFBXEventThread)
  Private
    FMsg: AnsiString;
  protected
    Procedure UpdateMemoResults;
    procedure DoException(Error: Exception); override;
    procedure DoEvent(const EventName: AnsiString; Count: Integer); override;
  end;

  TForm1 = class(TForm)
    Label3: TLabel;
    Label6: TLabel;
    ALButton1: TALButton;
    ALButton2: TALButton;
    ALComboBoxFirebirdapiVer: TALComboBox;
    ALMemoResult: TALMemo;
    OpenDialog1: TOpenDialog;
    Label2: TLabel;
    Label4: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    Label30: TLabel;
    ALEditFirebirdLogin: TALEdit;
    ALEditFirebirdPassword: TALEdit;
    ALEditFirebirdCharset: TALEdit;
    ALEditFirebirdLib: TALEdit;
    ALEditFirebirdDatabase: TALEdit;
    ALEditFireBirdNum_buffers: TALEdit;
    ALMemoFireBirdEventName: TALMemo;
    Label1: TLabel;
    OpenDialog2: TOpenDialog;
    Panel2: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Panel3: TPanel;
    PanelWebBrowser: TPanel;
    procedure ALButton1Click(Sender: TObject);
    procedure ALComboBoxPaint(Sender: TObject; var continue: Boolean);
    procedure ALEditPaint(Sender: TObject; var continue: Boolean);
    procedure ALMemoPaint(Sender: TObject; var continue: Boolean);
    procedure ALMemoPaintScrollBar(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
    procedure ALButtonPaint(Sender: TObject; var continue: Boolean);
    procedure ALButton2Click(Sender: TObject);
    procedure ALEditFirebirdLibButtonClick(Sender: TObject);
    procedure ALEditFirebirdDatabaseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    fEventThread: TALFBXEventThread;
  public
  end;

var
  Form1: TForm1;

implementation

uses ALFcnString,
     ALFcnSkin,
     ALFbxBase;

{$R *.dfm}

{***********************************************}
procedure TForm1.ALButton1Click(Sender: TObject);
var aFBAPiVersion: TALFBXVersion_API;
begin
  if Trim(ALMemoFireBirdEventName.text) = '' then raise Exception.Create('You must listen at least one event!');

  if assigned(fEventThread) then exit;

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

  fEventThread := TMyEventThread.Create(AnsiString(ALEditFirebirdDatabase.text),
                                        AnsiString(ALEditFirebirdLogin.text),
                                        AnsiString(ALEditFirebirdPassword.text),
                                        AnsiString(ALEditFirebirdCharset.text),
                                        ALStringReplace(ALTrim(AnsiString(ALMemoFireBirdEventName.lines.text)),#13#10,';',[RfReplaceALL]),
                                        aFBAPiVersion,
                                        AnsiString(ALEditFirebirdLib.text),
                                        -1,
                                        StrToInt(ALEditFireBirdNum_buffers.text),
                                        '');
end;

{***********************************************}
procedure TForm1.ALButton2Click(Sender: TObject);
begin
  if not assigned(fEventThread) then exit;
  FEventThread.Free;
  fEventThread := nil;
end;

{*****************************************}
procedure TMyEventThread.UpdateMemoResults;
begin
  Form1.ALMemoResult.Lines.Add(string(fMsg));
end;

{************************************************************************}
procedure TMyEventThread.DoEvent(const EventName: AnsiString; Count: Integer);
begin
  FMsg := 'Event fired: ' + EventName;
  synchronize(UpdateMemoResults);
end;

{*****************************************************}
procedure TMyEventThread.DoException(Error: Exception);
begin
  FMsg := 'Error detected: ' + AnsiString(Error.Message);
  synchronize(UpdateMemoResults);
end;

{*************************************************************}
procedure TForm1.ALEditFirebirdLibButtonClick(Sender: TObject);
begin
  If OpenDialog1.Execute then (Sender as TALEdit).Text := OpenDialog1.FileName;
end;

{******************************************************************}
procedure TForm1.ALEditFirebirdDatabaseButtonClick(Sender: TObject);
begin
  If OpenDialog2.Execute then (Sender as TALEdit).Text := OpenDialog2.FileName;
end;

{*********************************************************************}
procedure TForm1.ALButtonPaint(Sender: TObject; var continue: Boolean);
begin
  paintAlButtonBlueSkin(sender, Continue);
end;

{*******************************************************************}
procedure TForm1.ALEditPaint(Sender: TObject; var continue: Boolean);
begin
  PaintAlEditBlueSkin(Sender, Continue);
end;

{*******************************************************************}
procedure TForm1.ALMemoPaint(Sender: TObject; var continue: Boolean);
begin
  PaintALMemoBlueSkin(Sender, continue);
end;

{****************************************************************************************************}
procedure TForm1.ALMemoPaintScrollBar(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
begin
  PaintALMemoScrollBarBlueSkin(Sender, continue, Area);
end;

{***********************************************************************}
procedure TForm1.ALComboBoxPaint(Sender: TObject; var continue: Boolean);
begin
  paintAlComboBoxBlueSkin(sender, Continue);
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
