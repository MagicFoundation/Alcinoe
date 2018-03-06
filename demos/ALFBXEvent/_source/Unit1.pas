unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ALFBXclient, Shellapi;

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
    ALButton1: TButton;
    ALButton2: TButton;
    ALComboBoxFirebirdapiVer: TCombobox;
    ALMemoResult: TMemo;
    OpenDialog1: TOpenDialog;
    Label2: TLabel;
    Label4: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    Label30: TLabel;
    ALEditFirebirdLogin: TEdit;
    ALEditFirebirdPassword: TEdit;
    ALEditFirebirdCharset: TEdit;
    ALEditFirebirdLib: TEdit;
    ALEditFirebirdDatabase: TEdit;
    ALEditFireBirdNum_buffers: TEdit;
    ALMemoFireBirdEventName: TMemo;
    Label1: TLabel;
    OpenDialog2: TOpenDialog;
    Button2: TButton;
    Button1: TButton;
    procedure ALButton1Click(Sender: TObject);
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

uses ALString,
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
  If OpenDialog1.Execute then (Sender as TEdit).Text := OpenDialog1.FileName;
end;

{******************************************************************}
procedure TForm1.ALEditFirebirdDatabaseButtonClick(Sender: TObject);
begin
  If OpenDialog2.Execute then (Sender as TEdit).Text := OpenDialog2.FileName;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
