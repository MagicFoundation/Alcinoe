unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ALComboBox, ALEdit, ALMemo, ALButton, ExtCtrls, AlScrollBar,
  ALFBXclient;

type

  TMyEventThread = class(TALFBXEventThread)
  Private
    FMsg: String;
  protected
    Procedure UpdateMemoResults;
    procedure DoException(Error: Exception); override;
    procedure DoEvent(const EventName: string; Count: Integer); override;
  end;

  TForm1 = class(TForm)
    Label3: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    Label5: TLabel;
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
    procedure ALButton1Click(Sender: TObject);
    procedure ALComboBoxPaint(Sender: TObject; var continue: Boolean);
    procedure ALEditPaint(Sender: TObject; var continue: Boolean);
    procedure ALMemoPaint(Sender: TObject; var continue: Boolean);
    procedure ALMemoPaintScrollBar(Sender: TObject; var continue: Boolean; Area: TALScrollbarArea);
    procedure ALButtonPaint(Sender: TObject; var continue: Boolean);
    procedure ALButton2Click(Sender: TObject);
    procedure ALEditFirebirdLibButtonClick(Sender: TObject);
    procedure ALEditFirebirdDatabaseButtonClick(Sender: TObject);
  private
    { Private declarations }
    fEventThread: TALFBXEventThread;
  public
  end;

var
  Form1: TForm1;

implementation

uses ALFcnSkin,
     ALFbxBase;

{$R *.dfm}

{***********************************************}
procedure TForm1.ALButton1Click(Sender: TObject);
var aFBAPiVersion: TALFBXVersion_API;
begin
  if assigned(fEventThread) then exit;

  case ALComboBoxFirebirdapiVer.ItemIndex of
    1: aFBAPiVersion := FB103;
    2: aFBAPiVersion := FB15;
    3: aFBAPiVersion := FB20;
    4: aFBAPiVersion := FB25;
    else aFBAPiVersion := FB102;
  end;

  fEventThread := TMyEventThread.Create(ALEditFirebirdDatabase.text,
                                        ALEditFirebirdLogin.text,
                                        ALEditFirebirdPassword.text,
                                        ALEditFirebirdCharset.text,
                                        StringReplace(Trim(ALMemoFireBirdEventName.lines.text),#13#10,';',[RfReplaceALL]),
                                        aFBAPiVersion,
                                        ALEditFirebirdLib.text,
                                        -1,
                                        StrtoInt(ALEditFireBirdNum_buffers.text),
                                        '');
end;

{***********************************************}
procedure TForm1.ALButton2Click(Sender: TObject);
begin
  if not assigned(fEventThread) then exit;
  FEventThread.Free;
  fEventThread := nil;
end;



//////////////////////////
///// TMyEventThread /////
//////////////////////////

{*****************************************}
procedure TMyEventThread.UpdateMemoResults;
begin
  Form1.ALMemoResult.Lines.Add(fMsg);
end;

{************************************************************************}
procedure TMyEventThread.DoEvent(const EventName: string; Count: Integer);
begin
  FMsg := 'Event fired: ' + EventName;
  synchronize(UpdateMemoResults);
end;

{*****************************************************}
procedure TMyEventThread.DoException(Error: Exception);
begin
  FMsg := 'Error detected: ' + Error.Message;
  synchronize(UpdateMemoResults);
end;



//////////////////
///// DESIGN /////
//////////////////

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

initialization
  ReportMemoryLeaksOnShutdown := True;
  
end.
