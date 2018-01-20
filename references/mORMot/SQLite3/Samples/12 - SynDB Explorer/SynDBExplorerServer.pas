unit SynDBExplorerServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SynCommons, SynDB, SynDBRemote;

type
  THTTPServerForm = class(TForm)
    lbledtPort: TLabeledEdit;
    lbledtDatabase: TLabeledEdit;
    lbledtUser: TLabeledEdit;
    lbledtPassword: TLabeledEdit;
    btnConnect: TButton;
    btnDisconnect: TButton;
    procedure lbledtPortKeyPress(Sender: TObject; var Key: Char);
    procedure lbledtDatabaseKeyPress(Sender: TObject; var Key: Char);
    procedure btnConnectClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
  private
  public
    Props: TSQLDBConnectionProperties;
    Server: TSQLDBServerHttpApi;
  end;

var
  HTTPServerForm: THTTPServerForm;

implementation

uses
  mORMotUILogin; // for ShowMessage()
  
{$R *.dfm}

procedure THTTPServerForm.lbledtPortKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (AnsiChar(Key) in ['0'..'9']) then
    Key := #0
end;

procedure THTTPServerForm.lbledtDatabaseKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (AnsiChar(Key) in ['A'..'Z','a'..'z','_']) then
    Key := #0
end;

procedure THTTPServerForm.btnConnectClick(Sender: TObject);
begin
  if (Server<>nil) or (Props=nil) then
    exit;
  try
    Server := TSQLDBServerHttpApi.Create(Props,StringToUTF8(lbledtDatabase.Text),
      StringToUTF8(lbledtPort.Text),StringToUTF8(lbledtUser.Text),
      StringToUTF8(lbledtPassword.Text));
  except
    on E: Exception do
      ShowMessage(E.ClassName,E.Message,true);
  end;
  btnConnect.Enabled := false;
  btnDisconnect.Enabled := true;
end;

procedure THTTPServerForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Server);
end;

procedure THTTPServerForm.btnDisconnectClick(Sender: TObject);
begin
  if Server<>nil then
    FreeAndNil(Server);
  Props := nil;
  btnDisconnect.Enabled := false;
  btnConnect.Enabled := true;
end;

end.
