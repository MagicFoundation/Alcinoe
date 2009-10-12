unit Unit1;

interface

uses Windows,
     Messages,
     SysUtils,
     Variants,
     Classes,
     Graphics,
     Controls,
     Forms,
     Dialogs,
     StdCtrls,
     WinInet,
     shellapi,
     ExtCtrls,
     ComCtrls,
     AlWininetHttpClient;

type
  TForm1 = class(TForm)
    MainStatusBar: TStatusBar;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBox3: TGroupBox;
    Label18: TLabel;
    Label19: TLabel;
    EditUserName: TEdit;
    EditPassword: TEdit;
    GroupBox4: TGroupBox;
    Label14: TLabel;
    Label17: TLabel;
    Label20: TLabel;
    EditSendTimeout: TEdit;
    EditReceiveTimeout: TEdit;
    EditConnectTimeout: TEdit;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox2: TGroupBox;
    RadioButtonAccessType_Direct: TRadioButton;
    RadioButtonAccessType_Preconfig: TRadioButton;
    RadioButtonAccessType_Preconfig_with_no_autoproxy: TRadioButton;
    RadioButtonAccessType_Proxy: TRadioButton;
    GroupBox1: TGroupBox;
    Label15: TLabel;
    Label12: TLabel;
    Label11: TLabel;
    Label16: TLabel;
    Label13: TLabel;
    EdProxyPort: TEdit;
    EdProxyUserName: TEdit;
    EdProxyServer: TEdit;
    EdProxyPassword: TEdit;
    EdProxyBypass: TEdit;
    GroupBox5: TGroupBox;
    Label24: TLabel;
    EditBufferUploadSize: TEdit;
    CheckBoxInternetOption_From_Cache: TCheckBox;
    CheckBoxInternetOption_Offline: TCheckBox;
    CheckBoxInternetOption_Keep_connection: TCheckBox;
    CheckBoxInternetOption_No_auto_redirect: TCheckBox;
    CheckBoxInternetOption_Ignore_redirect_to_https: TCheckBox;
    CheckBoxInternetOption_No_auth: TCheckBox;
    CheckBoxInternetOption_Ignore_cert_date_invalid: TCheckBox;
    CheckBoxInternetOption_Need_file: TCheckBox;
    CheckBoxInternetOption_Ignore_redirect_to_http: TCheckBox;
    CheckBoxInternetOption_Hyperlink: TCheckBox;
    CheckBoxInternetOption_Ignore_cert_cn_invalid: TCheckBox;
    CheckBoxInternetOption_Cache_if_net_fail: TCheckBox;
    CheckBoxInternetOption_No_cache_write: TCheckBox;
    CheckBoxInternetOption_Resynchronize: TCheckBox;
    CheckBoxInternetOption_No_cookies: TCheckBox;
    CheckBoxInternetOption_Pragma_nocache: TCheckBox;
    CheckBoxInternetOption_Reload: TCheckBox;
    CheckBoxInternetOption_No_ui: TCheckBox;
    CheckBoxInternetOption_Secure: TCheckBox;
    GroupBox8: TGroupBox;
    MemoRequestRawHeader: TMemo;
    Label8: TLabel;
    RadioButtonProtocolVersion1_0: TRadioButton;
    RadioButtonProtocolVersion1_1: TRadioButton;
    GroupBox9: TGroupBox;
    editURL: TEdit;
    Label4: TLabel;
    Label6: TLabel;
    MemoPostDataStrings: TMemo;
    MemoPostDataFiles: TMemo;
    Label7: TLabel;
    Label5: TLabel;
    GroupBox10: TGroupBox;
    Label2: TLabel;
    MemoResponseRawHeader: TMemo;
    MemoContentBody: TMemo;
    Label3: TLabel;
    Label1: TLabel;
    ButtonPost: TButton;
    ButtonGet: TButton;
    ButtonOpenInExplorer: TButton;
    Panel1: TPanel;
    Label9: TLabel;
    CheckBoxEncodeParams: TCheckBox;
    procedure ButtonGetClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonOpenInExplorerClick(Sender: TObject);
    procedure ButtonPostClick(Sender: TObject);
  private
    FWinInetHttpClient: TalWinInetHttpClient;
    FDownloadSpeedStartTime: TdateTime;
    FDownloadSpeedBytesRead: Integer;
    FDownloadSpeedBytesNotRead: Integer;
    procedure initWinInetHTTP;
  public
    procedure OnHttpClientStatusChange(sender: Tobject; InternetStatus: DWord; StatusInformation: Pointer; StatusInformationLength: DWord);
    procedure OnHttpDownloadProgress(sender: Tobject; Read: Integer; Total: Integer);
    procedure OnHttpUploadProgress(sender: Tobject; Sent: Integer; Total: Integer);    
  end;

var
  Form1: TForm1;

implementation

Uses DateUtils,
     ALMultiPartFormDataParser,
     AlFcnMisc,
     AlFcnFile,
     AlFcnMime,
     AlHTTPCommon;

{$R *.dfm}

{*******************************}
procedure TForm1.initWinInetHTTP;
Begin
  With FWinInetHTTPClient do begin
    UserName := EditUserName.Text;
    Password := EditPassword.Text;

    if AlIsInteger(EditConnectTimeout.Text) then ConnectTimeout := strtoint(EditConnectTimeout.Text);
    if AlIsInteger(EditsendTimeout.Text) then SendTimeout := strtoint(EditSendTimeout.Text);
    if AlIsInteger(EditReceiveTimeout.Text) then ReceiveTimeout := strtoint(EditReceiveTimeout.Text);

    if RadioButtonProtocolVersion1_0.Checked then ProtocolVersion := HTTPpv_1_0
    else ProtocolVersion := HTTPpv_1_1;

    if AlIsInteger(EditBufferUploadSize.Text) then UploadBufferSize := strtoint(EditBufferUploadSize.Text);

    ProxyParams.ProxyServer := EdProxyServer.Text;
    ProxyParams.ProxyPort := strToInt(EdProxyPort.Text);
    ProxyParams.ProxyUserName := EdProxyUserName.Text;
    ProxyParams.ProxyPassword := EdProxyPassword.Text;
    ProxyParams.ProxyBypass := EdProxyBypass.Text;

    if RadioButtonAccessType_Direct.Checked then AccessType := wHttpAt_Direct
    else if RadioButtonAccessType_Preconfig.Checked then AccessType := wHttpAt_Preconfig
    else if RadioButtonAccessType_Preconfig_with_no_autoproxy.Checked then AccessType := wHttpAt_Preconfig_with_no_autoproxy
    else if RadioButtonAccessType_Proxy.Checked then AccessType := wHttpAt_Proxy;

    InternetOptions := [];
    If CheckBoxInternetOption_From_Cache.checked then InternetOptions := InternetOptions + [wHttpIo_From_Cache];
    If CheckBoxInternetOption_Offline.checked then InternetOptions := InternetOptions + [wHttpIo_Offline];
    If CheckBoxInternetOption_Cache_if_net_fail.checked then InternetOptions := InternetOptions + [wHttpIo_Cache_if_net_fail];
    If CheckBoxInternetOption_Hyperlink.checked then InternetOptions := InternetOptions + [wHttpIo_Hyperlink];
    If CheckBoxInternetOption_Ignore_cert_cn_invalid.checked then InternetOptions := InternetOptions + [wHttpIo_Ignore_cert_cn_invalid];
    If CheckBoxInternetOption_Ignore_cert_date_invalid.checked then InternetOptions := InternetOptions + [wHttpIo_Ignore_cert_date_invalid];
    If CheckBoxInternetOption_Ignore_redirect_to_http.checked then InternetOptions := InternetOptions + [wHttpIo_Ignore_redirect_to_http];
    If CheckBoxInternetOption_Ignore_redirect_to_https.checked then InternetOptions := InternetOptions + [wHttpIo_Ignore_redirect_to_https];
    If CheckBoxInternetOption_Keep_connection.checked then InternetOptions := InternetOptions + [wHttpIo_Keep_connection];
    If CheckBoxInternetOption_Need_file.checked then InternetOptions := InternetOptions + [wHttpIo_Need_file];
    If CheckBoxInternetOption_No_auth.checked then InternetOptions := InternetOptions + [wHttpIo_No_auth];
    If CheckBoxInternetOption_No_auto_redirect.checked then InternetOptions := InternetOptions + [wHttpIo_No_auto_redirect];
    If CheckBoxInternetOption_No_cache_write.checked then InternetOptions := InternetOptions + [wHttpIo_No_cache_write];
    If CheckBoxInternetOption_No_cookies.checked then InternetOptions := InternetOptions + [wHttpIo_No_cookies];
    If CheckBoxInternetOption_No_ui.checked then InternetOptions := InternetOptions + [wHttpIo_No_ui];
    If CheckBoxInternetOption_Pragma_nocache.checked then InternetOptions := InternetOptions + [wHttpIo_Pragma_nocache];
    If CheckBoxInternetOption_Reload.checked then InternetOptions := InternetOptions + [wHttpIo_Reload];
    If CheckBoxInternetOption_Resynchronize.checked then InternetOptions := InternetOptions + [wHttpIo_Resynchronize];
    If CheckBoxInternetOption_Secure.checked then InternetOptions := InternetOptions + [wHttpIo_Secure];

    RequestHeader.RawHeaderText := MemoRequestRawHeader.Text;
  end;
end;

{********************************************************}
procedure TForm1.OnHttpClientStatusChange(Sender: Tobject;
                                          InternetStatus: DWord;
                                          StatusInformation: Pointer;
                                          StatusInformationLength: DWord);
var StatusStr: String;
begin
  case InternetStatus of
    INTERNET_STATUS_RESOLVING_NAME: StatusStr := 'Resolving name';
    INTERNET_STATUS_NAME_RESOLVED: StatusStr := 'Name resolved';
    INTERNET_STATUS_CONNECTING_TO_SERVER: StatusStr := 'Connecting to server';
    INTERNET_STATUS_CONNECTED_TO_SERVER: StatusStr := 'Connected';
    INTERNET_STATUS_SENDING_REQUEST: StatusStr := 'Sending Request';
    INTERNET_STATUS_REQUEST_SENT: StatusStr := 'Request sent';
    INTERNET_STATUS_RECEIVING_RESPONSE: StatusStr := 'Receiving response';
    INTERNET_STATUS_RESPONSE_RECEIVED: StatusStr := 'Response received';
    INTERNET_STATUS_CTL_RESPONSE_RECEIVED: StatusStr := 'CTL Response received';
    INTERNET_STATUS_PREFETCH: StatusStr := 'Prefetch';
    INTERNET_STATUS_CLOSING_CONNECTION: StatusStr := 'Closing connection';
    INTERNET_STATUS_CONNECTION_CLOSED: StatusStr := 'Connection closed';
    INTERNET_STATUS_HANDLE_CREATED: StatusStr := 'Handle created';
    INTERNET_STATUS_HANDLE_CLOSING: StatusStr := 'Handle closing';
    INTERNET_STATUS_REQUEST_COMPLETE: StatusStr := 'Request complete';
    INTERNET_STATUS_REDIRECT: StatusStr := 'Redirect';
    INTERNET_STATUS_INTERMEDIATE_RESPONSE: StatusStr := 'Intermediate response';
    INTERNET_STATUS_STATE_CHANGE: StatusStr := 'State change';
    INTERNET_STATUS_COOKIE_SENT: StatusStr := 'COOKIE SENT';
    INTERNET_STATUS_COOKIE_RECEIVED: StatusStr := 'COOKIE RECEIVED';
    INTERNET_STATUS_PRIVACY_IMPACTED: StatusStr := 'PRIVACY IMPACTED';
    INTERNET_STATUS_P3P_HEADER: StatusStr := 'P3P HEADER';
    INTERNET_STATUS_P3P_POLICYREF: StatusStr := 'P3P POLICYREF';
    INTERNET_STATUS_COOKIE_HISTORY: StatusStr := 'COOKIE HISTORY';
    else
      StatusStr := 'Unknown status: ' + inttostr(InternetStatus);
   end;

 MainStatusBar.Panels[0].Text := StatusStr;
 application.ProcessMessages;
end;

{*****************************************************************************}
procedure TForm1.OnHttpDownloadProgress(sender: Tobject; Read, Total: Integer);
Var In1, In2: integer;
begin
 if MainStatusBar.Panels[1].Text = '' then Begin
   FDownloadSpeedStartTime := now;
   FDownloadSpeedBytesNotRead := Read;
 End;
 FDownloadSpeedBytesRead := Read;

 MainStatusBar.Panels[1].Text := 'Read '+inttostr(read) + ' bytes of '+inttostr(total) + ' bytes';

 in1 := FDownloadSpeedBytesRead - FDownloadSpeedBytesNotRead;
 in2 := MillisecondsBetween(now, FDownloadSpeedStartTime);
 if (in1 > 0) and (in2 > 0) then MainStatusBar.Panels[2].Text := 'Download speed: '+ Inttostr(Round((in1 / 1000) / (in2 / 1000))) +'kbps';

 application.ProcessMessages;
end;

{***************************************************************************}
procedure TForm1.OnHttpUploadProgress(sender: Tobject; Sent, Total: Integer);
begin
 MainStatusBar.Panels[1].Text := 'Send '+inttostr(sent) + ' bytes of '+inttostr(total) + ' bytes';
 application.ProcessMessages;
end;

{***********************************************}
procedure TForm1.ButtonGetClick(Sender: TObject);
Var AHTTPResponseHeader: TALHTTPResponseHeader;
    AHTTPResponseStream: TStringStream;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinInetHTTP;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  AHTTPResponseStream := TstringStream.Create('');
  try
    try
      FWinInetHttpClient.Get(editURL.Text, AHTTPResponseStream, AHTTPResponseHeader);
      MemoContentBody.Lines.Text := AHTTPResponseStream.DataString;
      MemoResponseRawHeader.Lines.Text := AHTTPResponseHeader.RawHeaderText;
    except
      MemoContentBody.Lines.Text := AHTTPResponseStream.DataString;
      MemoResponseRawHeader.Lines.Text := AHTTPResponseHeader.RawHeaderText;
      Raise;
    end;
  finally
    AHTTPResponseHeader.Free;
    AHTTPResponseStream.Free;
  end;
end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  FWinInetHttpClient.Free;
end;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  FWinInetHttpClient := TaLWinInetHttpClient.Create(self);
  with FWinInetHttpClient do begin
    AccessType := wHttpAt_Preconfig;
    InternetOptions := [wHttpIo_Keep_connection];
    OnStatusChange := OnHttpClientStatusChange;
    OnDownloadProgress := OnHttpDownloadProgress;
    OnUploadProgress := OnHttpUploadProgress;
    MemoRequestRawHeader.Text := RequestHeader.RawHeaderText;
  end;
end;

{**********************************************************}
procedure TForm1.ButtonOpenInExplorerClick(Sender: TObject);
Var AFullPath: String;
begin
  AFullPath := ALGetModulePath + '~tmp.html';
  MemoContentBody.Lines.SaveToFile(AFullPath);
  ShellExecute(0,'OPEN',Pchar(AFullPath),nil,nil,SW_SHOW)
end;

{************************************************}
procedure TForm1.ButtonPostClick(Sender: TObject);
Var AHTTPResponseHeader: TALHTTPResponseHeader;
    AHTTPResponseStream: TStringStream;
    AMultiPartFormDataFile: TALMultiPartFormDataContent;
    AMultiPartFormDataFiles: TALMultiPartFormDataContents;
    aTmpPostDataString: TStrings;
    i: Integer;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinInetHTTP;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  AHTTPResponseStream := TstringStream.Create('');
  AMultiPartFormDataFiles := TALMultiPartFormDataContents.Create(true);
  aTmpPostDataString := TstringList.Create;
  try
    Try

      aTmpPostDataString.Assign(MemoPostDataStrings.lines);

      For I := 0 To MemoPostDataFiles.Lines.Count - 1 do
        if MemoPostDataFiles.Lines[i] <> '' then begin
          AMultiPartFormDataFile := TALMultiPartFormDataContent.Create;
          TmemoryStream(AMultiPartFormDataFile.DataStream).LoadFromFile(MemoPostDataFiles.Lines.ValueFromIndex[i]);
          AMultiPartFormDataFile.ContentDisposition := 'form-data; name="'+MemoPostDataFiles.Lines.Names[i]+'"; filename="'+MemoPostDataFiles.Lines.ValueFromIndex[i]+'"';
          AMultiPartFormDataFile.ContentType := ALGetDefaultMIMEContentTypeFromExt(ExtractFileExt(MemoPostDataFiles.Lines.ValueFromIndex[i]));
          AMultiPartFormDataFiles.Add(AMultiPartFormDataFile);
        end;

      if AMultiPartFormDataFiles.Count > 0 then
        FWinInetHttpClient.PostMultiPartFormData(
                                                 editURL.Text,
                                                 aTmpPostDataString,
                                                 AMultiPartFormDataFiles,
                                                 AHTTPResponseStream,
                                                 AHTTPResponseHeader
                                                )

      else if aTmpPostDataString.Count > 0 then
        FWinInetHttpClient.PostURLEncoded(
                                          editURL.Text,
                                          aTmpPostDataString,
                                          AHTTPResponseStream,
                                          AHTTPResponseHeader,
                                          CheckBoxEncodeParams.Checked
                                         )

      else FWinInetHttpClient.Post(
                                   editURL.Text,
                                   AHTTPResponseStream,
                                   AHTTPResponseHeader
                                  );

      MemoContentBody.Lines.Text := AHTTPResponseStream.DataString;
      MemoResponseRawHeader.Lines.Text := AHTTPResponseHeader.RawHeaderText;
    Except
      MemoContentBody.Lines.Text := AHTTPResponseStream.DataString;
      MemoResponseRawHeader.Lines.Text := AHTTPResponseHeader.RawHeaderText;
      Raise;
    end;
  finally
    AHTTPResponseHeader.Free;
    AHTTPResponseStream.Free;
    AMultiPartFormDataFiles.Free;
    aTmpPostDataString.free;
  end;
end;

end.
