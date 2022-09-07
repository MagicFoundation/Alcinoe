unit Unit1;

interface

uses
  winapi.WinHTTP,
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  shellapi,
  ExtCtrls,
  ComCtrls,
  Menus,
  cxPCdxBarPopupMenu,
  cxGraphics,
  cxControls,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  cxContainer,
  cxEdit,
  cxLabel,
  cxPC,
  cxRadioGroup,
  cxCheckBox,
  cxButtons,
  cxMemo,
  cxTextEdit,
  cxGroupBox,
  dxSkinsForm,
  dxSkinsCore,
  dxSkinFoggy,
  dxSkinscxPCPainter,
  cxSplitter,
  cxClasses,
  dxBarBuiltInMenu,
  ALString,
  AlWinHttpWebSocketClient;

type
  TForm1 = class(TForm)
    PageControl1: TcxPageControl;
    TabSheet1: TcxTabSheet;
    TabSheet2: TcxTabSheet;
    GroupBox3: TcxGroupBox;
    Label18: TcxLabel;
    Label19: TcxLabel;
    EditUserName: TcxTextEdit;
    EditPassword: TcxTextEdit;
    GroupBox4: TcxGroupBox;
    Label14: TcxLabel;
    Label17: TcxLabel;
    Label20: TcxLabel;
    EditSendTimeout: TcxTextEdit;
    EditReceiveTimeout: TcxTextEdit;
    EditConnectTimeout: TcxTextEdit;
    GroupBox6: TcxGroupBox;
    GroupBox7: TcxGroupBox;
    GroupBox2: TcxGroupBox;
    RadioButtonAccessType_NAMED_PROXY: TcxRadioButton;
    RadioButtonAccessType_NO_PROXY: TcxRadioButton;
    RadioButtonAccessType_DEFAULT_PROXY: TcxRadioButton;
    GroupBox1: TcxGroupBox;
    Label15: TcxLabel;
    Label12: TcxLabel;
    Label11: TcxLabel;
    Label16: TcxLabel;
    Label13: TcxLabel;
    EdProxyPort: TcxTextEdit;
    EdProxyUserName: TcxTextEdit;
    EdProxyServer: TcxTextEdit;
    EdProxyPassword: TcxTextEdit;
    EdProxyBypass: TcxTextEdit;
    GroupBox5: TcxGroupBox;
    EditBufferSize: TcxTextEdit;
    CheckBoxInternetOption_BYPASS_PROXY_CACHE: TcxCheckBox;
    CheckBoxInternetOption_ESCAPE_DISABLE: TcxCheckBox;
    CheckBoxInternetOption_REFRESH: TcxCheckBox;
    CheckBoxInternetOption_SECURE: TcxCheckBox;
    CheckBoxInternetOption_ESCAPE_PERCENT: TcxCheckBox;
    CheckBoxInternetOption_NULL_CODEPAGE: TcxCheckBox;
    CheckBoxInternetOption_ESCAPE_DISABLE_QUERY: TcxCheckBox;
    GroupBox8: TcxGroupBox;
    MemoRequestRawHeader: TcxMemo;
    Label8: TcxLabel;
    RadioButtonProtocolVersion1_0: TcxRadioButton;
    RadioButtonProtocolVersion1_1: TcxRadioButton;
    GroupBox9: TcxGroupBox;
    editURL: TcxTextEdit;
    Label4: TcxLabel;
    MemoMessage: TcxMemo;
    Label1: TcxLabel;
    ButtonSend: TcxButton;
    ButtonConnect: TcxButton;
    CheckBoxInternetOption_KEEP_CONNECTION: TcxCheckBox;
    CheckBoxInternetOption_NO_COOKIES: TcxCheckBox;
    CheckBoxInternetOption_NO_AUTO_REDIRECT: TcxCheckBox;
    dxSkinController1: TdxSkinController;
    Panel4: TPanel;
    cxSplitter2: TcxSplitter;
    GroupBox10: TcxGroupBox;
    Panel5: TPanel;
    Label2: TcxLabel;
    MemoLogStatus: TcxMemo;
    Panel6: TPanel;
    Label3: TcxLabel;
    MemoLogMsgReceived: TcxMemo;
    cxSplitter1: TcxSplitter;
    Label24: TcxLabel;
    procedure ButtonConnectClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
  private
    FWinHttpWebSocketClient: TalWinHttpWebSocketClient;
    procedure initWinHTTPWebSocketClient;
  public
    procedure OnWebSocketClientStatus(Sender: Tobject;
                                      InternetStatus: DWord;
                                      StatusInformation: Pointer;
                                      StatusInformationLength: DWord);
    procedure OnWebSocketClientError(sender: Tobject; const &Message: AnsiString);
    procedure OnWebSocketClientReceive(sender: Tobject; const Data: AnsiString; const IsUTF8: Boolean);
  end;

var
  Form1: TForm1;

implementation

Uses
  system.AnsiStrings,
  DateUtils,
  HttpApp,
  ALMultiPartParser,
  AlFiles,
  AlCommon,
  ALMime,
  ALWinHttpClient,
  ALStringList,
  AlHttpClient;

{$R *.dfm}

{******************************************}
procedure TForm1.initWinHTTPWebSocketClient;
Begin
  With FWinHttpWebSocketClient do begin
    UserName := AnsiString(EditUserName.Text);
    Password := AnsiString(EditPassword.Text);

    if AlIsInteger(AnsiString(EditConnectTimeout.Text)) then ConnectTimeout := StrToInt(EditConnectTimeout.Text);
    if AlIsInteger(AnsiString(EditsendTimeout.Text)) then SendTimeout := StrToInt(EditSendTimeout.Text);
    if AlIsInteger(AnsiString(EditReceiveTimeout.Text)) then ReceiveTimeout := StrToInt(EditReceiveTimeout.Text);

    if RadioButtonProtocolVersion1_0.Checked then ProtocolVersion := HTTPpv_1_0
    else ProtocolVersion := HTTPpv_1_1;

    if AlIsInteger(AnsiString(EditBufferSize.Text)) then BufferSize := StrToInt(EditBufferSize.Text);

    ProxyParams.ProxyServer := AnsiString(EdProxyServer.Text);
    ProxyParams.ProxyPort := StrToInt(EdProxyPort.Text);
    ProxyParams.ProxyUserName := AnsiString(EdProxyUserName.Text);
    ProxyParams.ProxyPassword := AnsiString(EdProxyPassword.Text);
    ProxyParams.ProxyBypass := AnsiString(EdProxyBypass.Text);

    if RadioButtonAccessType_NO_PROXY.Checked then AccessType := wHttpAt_NO_PROXY
    else if RadioButtonAccessType_NAMED_PROXY.Checked then AccessType := wHttpAt_NAMED_PROXY
    else if RadioButtonAccessType_DEFAULT_PROXY.Checked then AccessType := wHttpAt_DEFAULT_PROXY;

    InternetOptions := [];
    If CheckBoxInternetOption_BYPASS_PROXY_CACHE.checked then InternetOptions := InternetOptions + [wHttpIo_BYPASS_PROXY_CACHE];
    If CheckBoxInternetOption_ESCAPE_DISABLE.checked then InternetOptions := InternetOptions + [wHttpIo_ESCAPE_DISABLE];
    If CheckBoxInternetOption_ESCAPE_DISABLE_QUERY.checked then InternetOptions := InternetOptions + [wHttpIo_ESCAPE_DISABLE_QUERY];
    If CheckBoxInternetOption_ESCAPE_PERCENT.checked then InternetOptions := InternetOptions + [wHttpIo_ESCAPE_PERCENT];
    If CheckBoxInternetOption_NULL_CODEPAGE.checked then InternetOptions := InternetOptions + [wHttpIo_NULL_CODEPAGE];
    If CheckBoxInternetOption_REFRESH.checked then InternetOptions := InternetOptions + [wHttpIo_REFRESH];
    If CheckBoxInternetOption_SECURE.checked then InternetOptions := InternetOptions + [wHttpIo_SECURE];
    If CheckBoxInternetOption_NO_COOKIES.checked then InternetOptions := InternetOptions + [wHttpIo_NO_COOKIES];
    If CheckBoxInternetOption_KEEP_CONNECTION.checked then InternetOptions := InternetOptions + [wHttpIo_KEEP_CONNECTION];
    If CheckBoxInternetOption_NO_AUTO_REDIRECT.checked then InternetOptions := InternetOptions + [wHttpIo_NO_AUTO_REDIRECT];

    RequestHeader.RawHeaderText := AnsiString(MemoRequestRawHeader.Text);
  end;
end;

{*******************************************************}
procedure TForm1.OnWebSocketClientStatus(Sender: Tobject;
                                         InternetStatus: DWord;
                                         StatusInformation: Pointer;
                                         StatusInformationLength: DWord);
var StatusStr: AnsiString;
begin
  case InternetStatus of
    WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION: StatusStr := 'Closing the connection to the server';
    WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER: StatusStr := 'Successfully connected to the server';
    WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER: StatusStr := 'Connecting to the server';
    WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED: StatusStr := 'Successfully closed the connection to the server';
    WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE: StatusStr := 'Data is available to be retrieved with WinHttpReadData';
    WINHTTP_CALLBACK_STATUS_HANDLE_CREATED: StatusStr := 'An HINTERNET handle has been created';
    WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING: StatusStr := 'This handle value has been terminated';
    WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE: StatusStr := 'The response header has been received and is available with WinHttpQueryHeaders';
    WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE: StatusStr := 'Received an intermediate (100 level) status code message from the server';
    WINHTTP_CALLBACK_STATUS_NAME_RESOLVED: StatusStr := 'Successfully found the IP address of the server';
    WINHTTP_CALLBACK_STATUS_READ_COMPLETE: StatusStr := 'Data was successfully read from the server';
    WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE: StatusStr := 'Waiting for the server to respond to a request';
    WINHTTP_CALLBACK_STATUS_REDIRECT: StatusStr := 'An HTTP request is about to automatically redirect the request';
    WINHTTP_CALLBACK_STATUS_REQUEST_ERROR: StatusStr := 'An error occurred while sending an HTTP request';
    WINHTTP_CALLBACK_STATUS_REQUEST_SENT: StatusStr := 'Successfully sent the information request to the server';
    WINHTTP_CALLBACK_STATUS_RESOLVING_NAME: StatusStr := 'Looking up the IP address of a server name';
    WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED: StatusStr := 'Successfully received a response from the server';
    WINHTTP_CALLBACK_STATUS_SECURE_FAILURE: StatusStr := 'One or more errors were encountered while retrieving a Secure Sockets Layer (SSL) certificate from the server';
    WINHTTP_CALLBACK_STATUS_SENDING_REQUEST: StatusStr := 'Sending the information request to the server';
    WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE: StatusStr := 'The request completed successfully';
    WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE: StatusStr := 'Data was successfully written to the server';
    else
      StatusStr := 'Unknown status: ' + ALIntToStr(InternetStatus);
  end;
  MemoLogStatus.Lines.Add(String(StatusStr));
end;

{***********************************************************************************}
procedure TForm1.OnWebSocketClientError(sender: Tobject; const &Message: AnsiString);
begin
  MemoLogStatus.Lines.Add(String(&Message));
end;

{********************************************************************************************************}
procedure TForm1.OnWebSocketClientReceive(sender: Tobject; const Data: AnsiString; const IsUTF8: Boolean);
begin
  MemoLogMsgReceived.Lines.Add(String(Data));
end;

{***************************************************}
procedure TForm1.ButtonConnectClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    if ButtonConnect.tag = 0 then begin
      initWinHTTPWebSocketClient;
      MemoLogMsgReceived.Lines.Clear;
      MemoLogStatus.Lines.Clear;
      FWinHttpWebSocketClient.Connect(AnsiString(editURL.Text));
      ButtonConnect.Caption := 'Disconnect';
      ButtonConnect.Tag := 1;
      ButtonSend.Enabled := True;
    end
    else begin
      FWinHttpWebSocketClient.Disconnect;
      ButtonConnect.Caption := 'Connect';
      ButtonConnect.Tag := 0;
      ButtonSend.Enabled := False;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

{************************************************}
procedure TForm1.ButtonSendClick(Sender: TObject);
begin
  FWinHttpWebSocketClient.Send(AnsiString(MemoMessage.Text));
end;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  FWinHttpWebSocketClient := TaLWinHttpWebSocketClient.Create;
  with FWinHttpWebSocketClient do begin
    AccessType := wHttpAt_NO_PROXY;
    InternetOptions := [];
    OnStatus := OnWebSocketClientStatus;
    OnError := OnWebSocketClientError;
    OnReceive := OnWebSocketClientReceive;
    MemoRequestRawHeader.Text := String(RequestHeader.RawHeaderText);
  end;
  MemoLogStatus.Height := MemoLogStatus.Parent.Height - MemoLogStatus.top - 6;
  MemoLogMsgReceived.Height := MemoLogMsgReceived.Parent.Height - MemoLogMsgReceived.top - 6;
  MemoMessage.lines.Text := ALTrimU(MemoMessage.lines.Text);
end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  FWinHttpWebSocketClient.Free;
end;

initialization
  {$IFDEF DEBUG}
  ReporTMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
