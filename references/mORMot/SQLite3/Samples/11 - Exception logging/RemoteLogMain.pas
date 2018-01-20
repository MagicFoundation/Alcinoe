unit RemoteLogMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SynCommons, SynLog, mORMot, mORMotHttpClient;

type
  TMainForm = class(TForm)
    grpEvent: TGroupBox;
    cbbEvent: TComboBox;
    edtText: TEdit;
    btnEventSend: TButton;
    grpConnection: TGroupBox;
    edtServer: TEdit;
    lblServer: TLabel;
    lblPort: TLabel;
    edtPort: TEdit;
    lblInfoConnect: TLabel;
    btnConnect: TButton;
    btnDisconnect: TButton;
    procedure btnConnectClick(Sender: TObject);
    procedure btnEventSendClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fNumber: integer;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R Vista.res}

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PTypeInfo(TypeInfo(TSynLogInfo))^.EnumBaseType^.AddCaptionStrings(cbbEvent.Items);
  SQLite3Log.Family.Level := LOG_VERBOSE;
end;

procedure TMainForm.btnConnectClick(Sender: TObject);
begin
  try
    Screen.Cursor := crHourGlass;
    try
      TSQLHttpClient.CreateForRemoteLogging(
        AnsiString(edtServer.Text),SQLite3Log,StrToInt(edtPort.Text));
    finally
      Screen.Cursor := crDefault;
    end;
  except
    on E: Exception do begin
      MessageDlg(E.Message,mtError,[mbOk],0);
      exit;
    end;
  end;
  grpConnection.Enabled := false;
  btnConnect.Enabled := false;
  cbbEvent.ItemIndex := Ord(sllInfo);
  grpEvent.Show;
  btnEventSend.SetFocus;
end;

procedure TMainForm.btnEventSendClick(Sender: TObject);
begin
  SQLite3Log.Add.Log(TSynLogInfo(cbbEvent.ItemIndex),
    FormatUTF8('% - %',[edtText.Text,fNumber]));
  inc(fNumber);
end;

procedure TMainForm.btnDisconnectClick(Sender: TObject);
begin
  SQLite3Log.Family.EchoRemoteStop;
  grpConnection.Enabled := true;
  btnConnect.Enabled := true;
  cbbEvent.ItemIndex := Ord(sllInfo);
  grpEvent.Hide;
  btnConnect.SetFocus;
end;

end.
