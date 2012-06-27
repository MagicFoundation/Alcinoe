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
     shellapi,
     ExtCtrls,
     ComCtrls,
     AlWinHttpClient,
     AlWinHttpWrapper,
     OleCtrls,
     SHDocVw,
     ComObj, cxPCdxBarPopupMenu, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxLabel, cxPC, Menus,
  cxRadioGroup, cxCheckBox, cxButtons, cxMemo, cxTextEdit, cxGroupBox,
  dxSkinsForm, dxSkinsCore, dxSkinFoggy, dxSkinscxPCPainter, cxSplitter;

type
  TForm1 = class(TForm)
    MainStatusBar: TStatusBar;
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
    Label24: TcxLabel;
    EditBufferUploadSize: TcxTextEdit;
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
    Label6: TcxLabel;
    MemoPostDataStrings: TcxMemo;
    MemoPostDataFiles: TcxMemo;
    Label7: TcxLabel;
    Label5: TcxLabel;
    Label1: TcxLabel;
    ButtonPost: TcxButton;
    ButtonGet: TcxButton;
    CheckBoxInternetOption_KEEP_CONNECTION: TcxCheckBox;
    CheckBoxInternetOption_NO_COOKIES: TcxCheckBox;
    CheckBoxInternetOption_NO_AUTO_REDIRECT: TcxCheckBox;
    CheckBoxHttpEncodePostData: TcxCheckBox;
    ButtonHead: TcxButton;
    CheckBoxUrlEncodePostData: TcxCheckBox;
    Panel1: TPanel;
    Label9: TcxLabel;
    Label10: TcxLabel;
    Panel2: TPanel;
    PanelWebBrowser: TPanel;
    ButtonTrace: TcxButton;
    dxSkinController1: TdxSkinController;
    Panel3: TPanel;
    Panel4: TPanel;
    GroupBox10: TcxGroupBox;
    Panel5: TPanel;
    Label2: TcxLabel;
    MemoResponseRawHeader: TcxMemo;
    Panel6: TPanel;
    Label3: TcxLabel;
    MemoContentBody: TcxMemo;
    cxSplitter1: TcxSplitter;
    procedure ButtonGetClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonPostClick(Sender: TObject);
    procedure ButtonHeadClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonTraceClick(Sender: TObject);
    procedure OnCfgEditChange(Sender: TObject);
    procedure OnCfgEditKeyPress(Sender: TObject; var Key: Char);
  private
    FWinHttpClient: TalWinHttpClient;
    FDownloadSpeedStartTime: TdateTime;
    FDownloadSpeedBytesRead: Integer;
    FDownloadSpeedBytesNotRead: Integer;
    fMustInitWinHTTP: Boolean;
    procedure initWinHTTP;
    function AnsiStrTo8bitUnicodeString(s: AnsiString): String;
  public
    procedure OnHttpClientStatusChange(sender: Tobject; InternetStatus: DWord; StatusInformation: Pointer; StatusInformationLength: DWord);
    procedure OnHttpDownloadProgress(sender: Tobject; Read: Integer; Total: Integer);
    procedure OnHttpUploadProgress(sender: Tobject; Sent: Integer; Total: Integer);
  end;

var
  Form1: TForm1;

implementation

Uses DateUtils,
     HttpApp,
     ALMultiPartFormDataParser,
     AlFcnFile,
     AlFcnMisc,
     AlFcnMime,
     ALFcnString,
     ALStringList,
     AlHttpCommon;

{$R *.dfm}

{***************************}
procedure TForm1.initWinHTTP;
Begin
  if not fMustInitWinHTTP then Exit;
  fMustInitWinHTTP := False;
  With FWinHTTPClient do begin
    UserName := AnsiString(EditUserName.Text);
    Password := AnsiString(EditPassword.Text);

    if AlIsInteger(AnsiString(EditConnectTimeout.Text)) then ConnectTimeout := StrToInt(EditConnectTimeout.Text);
    if AlIsInteger(AnsiString(EditsendTimeout.Text)) then SendTimeout := StrToInt(EditSendTimeout.Text);
    if AlIsInteger(AnsiString(EditReceiveTimeout.Text)) then ReceiveTimeout := StrToInt(EditReceiveTimeout.Text);

    if RadioButtonProtocolVersion1_0.Checked then ProtocolVersion := HTTPpv_1_0
    else ProtocolVersion := HTTPpv_1_1;

    if AlIsInteger(AnsiString(EditBufferUploadSize.Text)) then UploadBufferSize := StrToInt(EditBufferUploadSize.Text);

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

{********************************************************}
procedure TForm1.OnHttpClientStatusChange(Sender: Tobject;
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

 MainStatusBar.Panels[0].Text := String(StatusStr);
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

 MainStatusBar.Panels[1].Text := 'Read '+IntToStr(read) + ' bytes of '+IntToStr(total) + ' bytes';

 in1 := FDownloadSpeedBytesRead - FDownloadSpeedBytesNotRead;
 in2 := MillisecondsBetween(now, FDownloadSpeedStartTime);
 if (in1 > 0) and (in2 > 0) then MainStatusBar.Panels[2].Text := 'Download speed: '+ IntToStr(Round((in1 / 1000) / (in2 / 1000))) +'kbps';

 application.ProcessMessages;
end;

{***************************************************************************}
procedure TForm1.OnHttpUploadProgress(sender: Tobject; Sent, Total: Integer);
begin
 MainStatusBar.Panels[1].Text := 'Send '+IntToStr(sent) + ' bytes of '+IntToStr(total) + ' bytes';
 application.ProcessMessages;
end;

{****************************************************************}
function TForm1.AnsiStrTo8bitUnicodeString(s: AnsiString): String;
var i: integer;
begin
  Setlength(Result, length(s));
  for I := 1 to length(s) do
    result[I] := Char(s[i]);
end;

{***********************************************}
procedure TForm1.ButtonGetClick(Sender: TObject);
Var AHTTPResponseHeader: TALHTTPResponseHeader;
    AHTTPResponseStream: TALStringStream;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTP;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  AHTTPResponseStream := TALStringStream.Create('');
  try
    try
      FWinHttpClient.Get(AnsiString(editURL.Text), AHTTPResponseStream, AHTTPResponseHeader);
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
    except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
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
  FWinHttpClient.Free;
end;

{************************************************}
procedure TForm1.ButtonHeadClick(Sender: TObject);
Var AHTTPResponseHeader: TALHTTPResponseHeader;
    AHTTPResponseStream: TALStringStream;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTP;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  AHTTPResponseStream := TALStringStream.Create('');
  try
    try
      FWinHttpClient.Head(AnsiString(editURL.Text), AHTTPResponseStream, AHTTPResponseHeader);
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
    except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    AHTTPResponseHeader.Free;
    AHTTPResponseStream.Free;
  end;
end;

{*************************************************}
procedure TForm1.ButtonTraceClick(Sender: TObject);
Var AHTTPResponseHeader: TALHTTPResponseHeader;
    AHTTPResponseStream: TALStringStream;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTP;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  AHTTPResponseStream := TALStringStream.Create('');
  try
    try
      FWinHttpClient.Trace(AnsiString(editURL.Text), AHTTPResponseStream, AHTTPResponseHeader);
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
    except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    AHTTPResponseHeader.Free;
    AHTTPResponseStream.Free;
  end;
end;

{************************************************}
procedure TForm1.ButtonPostClick(Sender: TObject);
Var AHTTPResponseHeader: TALHTTPResponseHeader;
    AHTTPResponseStream: TALStringStream;
    ARawPostDatastream: TALStringStream;
    AMultiPartFormDataFile: TALMultiPartFormDataContent;
    AMultiPartFormDataFiles: TALMultiPartFormDataContents;
    aTmpPostDataString: TALStrings;
    i: Integer;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTP;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  AHTTPResponseStream := TALStringStream.Create('');
  AMultiPartFormDataFiles := TALMultiPartFormDataContents.Create(true);
  aTmpPostDataString := TALStringList.Create;
  try
    Try

      aTmpPostDataString.Assign(MemoPostDataStrings.lines);

      For I := 0 To MemoPostDataFiles.Lines.Count - 1 do
        if MemoPostDataFiles.Lines[i] <> '' then begin
          AMultiPartFormDataFile := TALMultiPartFormDataContent.Create;
          TMemoryStream(AMultiPartFormDataFile.DataStream).LoadFromFile(MemoPostDataFiles.Lines.ValueFromIndex[i]);
          AMultiPartFormDataFile.ContentDisposition := 'form-data; name="'+AnsiString(MemoPostDataFiles.Lines.Names[i])+'"; filename="'+AnsiString(MemoPostDataFiles.Lines.ValueFromIndex[i])+'"';
          AMultiPartFormDataFile.ContentType := ALGetDefaultMIMEContentTypeFromExt(ALExtractFileExt(AnsiString(MemoPostDataFiles.Lines.ValueFromIndex[i])));
          AMultiPartFormDataFiles.Add(AMultiPartFormDataFile);
        end;

      if AMultiPartFormDataFiles.Count > 0 then
        FWinHttpClient.PostMultiPartFormData(AnsiString(editURL.Text),
                                             aTmpPostDataString,
                                             AMultiPartFormDataFiles,
                                             AHTTPResponseStream,
                                             AHTTPResponseHeader)

      else if aTmpPostDataString.Count > 0 then begin
        if CheckBoxUrlEncodePostData.Checked then FWinHttpClient.PostURLEncoded(AnsiString(editURL.Text),
                                                                                aTmpPostDataString,
                                                                                AHTTPResponseStream,
                                                                                AHTTPResponseHeader,
                                                                                CheckBoxHTTPEncodePostData.Checked)
        else begin

          if CheckBoxHTTPEncodePostData.Checked then ARawPostDatastream := TALStringStream.create(HTTPEncode(aTmpPostDataString.text))
          else ARawPostDatastream := TALStringStream.create(aTmpPostDataString.text);
          try

            FWinHttpClient.post(AnsiString(editURL.Text),
                                ARawPostDatastream,
                                AHTTPResponseStream,
                                AHTTPResponseHeader);

          finally
            ARawPostDatastream.free;
          end;

        end;
      end

      else FWinHttpClient.Post(AnsiString(editURL.Text),
                               AHTTPResponseStream,
                               AHTTPResponseHeader);

      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
    Except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    AHTTPResponseHeader.Free;
    AHTTPResponseStream.Free;
    AMultiPartFormDataFiles.Free;
    aTmpPostDataString.free;
  end;
end;

{************************************************}
procedure TForm1.OnCfgEditChange(Sender: TObject);
begin
  fMustInitWinHTTP := True;
end;

{*****************************************************************}
procedure TForm1.OnCfgEditKeyPress(Sender: TObject; var Key: Char);
begin
  fMustInitWinHTTP := True;
end;


{-------------------}
var ie: IWebBrowser2;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
var Url, Flags, TargetFrameName, PostData, Headers: OleVariant;
begin
  fMustInitWinHTTP := True;
  FWinHttpClient := TaLWinHttpClient.Create(self);
  with FWinHttpClient do begin
    AccessType := wHttpAt_NO_PROXY;
    InternetOptions := [];
    OnStatusChange := OnHttpClientStatusChange;
    OnDownloadProgress := OnHttpDownloadProgress;
    OnUploadProgress := OnHttpUploadProgress;
    MemoRequestRawHeader.Text := String(RequestHeader.RawHeaderText);
  end;

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
  ReporTMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
