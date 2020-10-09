unit Unit1;

interface

uses
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
  AlWinHttpClient,
  AlWinHttpWrapper;

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
    ButtonHead: TcxButton;
    CheckBoxUrlEncodePostData: TcxCheckBox;
    ButtonTrace: TcxButton;
    dxSkinController1: TdxSkinController;
    Panel4: TPanel;
    GroupBox10: TcxGroupBox;
    Panel5: TPanel;
    Label2: TcxLabel;
    MemoResponseRawHeader: TcxMemo;
    Panel6: TPanel;
    Label3: TcxLabel;
    MemoContentBody: TcxMemo;
    cxSplitter1: TcxSplitter;
    cxSplitter2: TcxSplitter;
    ButtonOptions: TcxButton;
    ButtonPut: TcxButton;
    ButtonDelete: TcxButton;
    cxSplitter3: TcxSplitter;
    ButtonSaveToFile: TcxButton;
    procedure ButtonGetClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonPostClick(Sender: TObject);
    procedure ButtonHeadClick(Sender: TObject);
    procedure ButtonTraceClick(Sender: TObject);
    procedure OnCfgEditChange(Sender: TObject);
    procedure OnCfgEditKeyPress(Sender: TObject; var Key: Char);
    procedure ButtonOptionsClick(Sender: TObject);
    procedure ButtonPutClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonSaveToFileClick(Sender: TObject);
  private
    FWinHttpClient: TalWinHttpClient;
    FDownloadSpeedStartTime: TdateTime;
    FDownloadSpeedBytesRead: Integer;
    FDownloadSpeedBytesNotRead: Integer;
    fMustInitWinHTTP: Boolean;
    FHTTPResponseStream: TALStringStream;
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

Uses
  system.AnsiStrings,
  DateUtils,
  HttpApp,
  ALMultiPartParser,
  AlFiles,
  AlCommon,
  ALMime,
  ALStringList,
  AlHttpClient;

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

{**************************************************}
procedure TForm1.ButtonDeleteClick(Sender: TObject);
Var AHTTPResponseHeader: TALHTTPResponseHeader;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTP;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  try
    fHTTPResponseStream.Size := 0;
    try
      FWinHttpClient.Delete(AnsiString(editURL.Text), fHTTPResponseStream, AHTTPResponseHeader);
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
    except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    AHTTPResponseHeader.Free;
  end;
end;

{***********************************************}
procedure TForm1.ButtonGetClick(Sender: TObject);
Var AHTTPResponseHeader: TALHTTPResponseHeader;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTP;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  try
    fHTTPResponseStream.Size := 0;
    try
      FWinHttpClient.Get(AnsiString(editURL.Text), FHTTPResponseStream, AHTTPResponseHeader);
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
    except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    AHTTPResponseHeader.Free;
  end;
end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  FWinHttpClient.Free;
  fHTTPResponseStream.Free;
end;

{************************************************}
procedure TForm1.ButtonHeadClick(Sender: TObject);
Var AHTTPResponseHeader: TALHTTPResponseHeader;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTP;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  try
    fHTTPResponseStream.Size := 0;
    try
      FWinHttpClient.Head(AnsiString(editURL.Text), FHTTPResponseStream, AHTTPResponseHeader);
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
    except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    AHTTPResponseHeader.Free;
  end;
end;

{***************************************************}
procedure TForm1.ButtonOptionsClick(Sender: TObject);
Var AHTTPResponseHeader: TALHTTPResponseHeader;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTP;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  try
    fHTTPResponseStream.Size := 0;
    try
      FWinHttpClient.Options(AnsiString(editURL.Text), FHTTPResponseStream, AHTTPResponseHeader);
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
    except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    AHTTPResponseHeader.Free;
  end;
end;

{*************************************************}
procedure TForm1.ButtonTraceClick(Sender: TObject);
Var AHTTPResponseHeader: TALHTTPResponseHeader;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTP;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  try
    fHTTPResponseStream.Size := 0;
    try
      FWinHttpClient.Trace(AnsiString(editURL.Text), FHTTPResponseStream, AHTTPResponseHeader);
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
    except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    AHTTPResponseHeader.Free;
  end;
end;

{************************************************}
procedure TForm1.ButtonPostClick(Sender: TObject);
Var AHTTPResponseHeader: TALHTTPResponseHeader;
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
  AMultiPartFormDataFiles := TALMultiPartFormDataContents.Create(true);
  aTmpPostDataString := TALStringList.Create;
  try
    fHTTPResponseStream.Size := 0;
    Try

      aTmpPostDataString.Assign(MemoPostDataStrings.lines);
      aTmpPostDataString.Text := ansiString(MemoPostDataStrings.Text); // << I don't know yet why but MemoPostDataStrings.lines.count split very long line in severals lines of around 1000 chars

      For I := 0 To MemoPostDataFiles.Lines.Count - 1 do
        if MemoPostDataFiles.Lines[i] <> '' then begin
          AMultiPartFormDataFile := TALMultiPartFormDataContent.Create;
          TMemoryStream(AMultiPartFormDataFile.DataStream).LoadFromFile(MemoPostDataFiles.Lines.ValueFromIndex[i]);
          AMultiPartFormDataFile.ContentDisposition := 'form-data; name="'+AnsiString(MemoPostDataFiles.Lines.Names[i])+'"; filename="'+AnsiString(MemoPostDataFiles.Lines.ValueFromIndex[i])+'"';
          AMultiPartFormDataFile.ContentType := ALGetDefaultMIMEContentTypeFromExt(ALExtractFileExt(AnsiString(MemoPostDataFiles.Lines.ValueFromIndex[i])));
          AMultiPartFormDataFiles.Add(AMultiPartFormDataFile);
        end;

      if (AMultiPartFormDataFiles.Count > 0) and (CheckBoxURLEncodePostData.Checked) then
        FWinHttpClient.PostMultiPartFormData(AnsiString(editURL.Text),
                                             aTmpPostDataString,
                                             AMultiPartFormDataFiles,
                                             FHTTPResponseStream,
                                             AHTTPResponseHeader)

      else if (AMultiPartFormDataFiles.Count > 0) then begin

        FWinHttpClient.post(AnsiString(editURL.Text),
                            AMultiPartFormDataFiles.Items[0].DataStream,
                            FHTTPResponseStream,
                            AHTTPResponseHeader);

      end

      else if aTmpPostDataString.Count > 0 then begin
        if CheckBoxUrlEncodePostData.Checked then FWinHttpClient.PostURLEncoded(AnsiString(editURL.Text),
                                                                                aTmpPostDataString,
                                                                                FHTTPResponseStream,
                                                                                AHTTPResponseHeader,
                                                                                TALNameValueArray.Create(),
                                                                                True)
        else begin

          ARawPostDatastream := TALStringStream.create(aTmpPostDataString.text);
          try

            FWinHttpClient.post(AnsiString(editURL.Text),
                                ARawPostDatastream,
                                FHTTPResponseStream,
                                AHTTPResponseHeader);

          finally
            ARawPostDatastream.free;
          end;

        end;
      end

      else FWinHttpClient.Post(AnsiString(editURL.Text),
                               FHTTPResponseStream,
                               AHTTPResponseHeader);

      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
    Except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    AHTTPResponseHeader.Free;
    AMultiPartFormDataFiles.Free;
    aTmpPostDataString.free;
  end;
end;

{***********************************************}
procedure TForm1.ButtonPutClick(Sender: TObject);
Var AHTTPResponseHeader: TALHTTPResponseHeader;
    ARawPutDatastream: TALStringStream;
    AMultiPartFormDataFile: TALMultiPartFormDataContent;
    AMultiPartFormDataFiles: TALMultiPartFormDataContents;
    aTmpPutDataString: TALStrings;
    i: Integer;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTP;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  AMultiPartFormDataFiles := TALMultiPartFormDataContents.Create(true);
  aTmpPutDataString := TALStringList.Create;
  try
    fHTTPResponseStream.Size := 0;
    Try

      aTmpPutDataString.Assign(MemoPostDataStrings.lines);
      aTmpPutDataString.Text := ansiString(MemoPostDataStrings.Text); // << I don't know yet why but MemoPostDataStrings.lines.count split very long line in severals lines of around 1000 chars

      For I := 0 To MemoPostDataFiles.Lines.Count - 1 do
        if MemoPostDataFiles.Lines[i] <> '' then begin
          AMultiPartFormDataFile := TALMultiPartFormDataContent.Create;
          TMemoryStream(AMultiPartFormDataFile.DataStream).LoadFromFile(MemoPostDataFiles.Lines.ValueFromIndex[i]);
          AMultiPartFormDataFile.ContentDisposition := 'form-data; name="'+AnsiString(MemoPostDataFiles.Lines.Names[i])+'"; filename="'+AnsiString(MemoPostDataFiles.Lines.ValueFromIndex[i])+'"';
          AMultiPartFormDataFile.ContentType := ALGetDefaultMIMEContentTypeFromExt(ALExtractFileExt(AnsiString(MemoPostDataFiles.Lines.ValueFromIndex[i])));
          AMultiPartFormDataFiles.Add(AMultiPartFormDataFile);
        end;

      //if (AMultiPartFormDataFiles.Count > 0) and (CheckBoxURLEncodePostData.Checked) then
      //  FWinHttpClient.PutMultiPartFormData(AnsiString(editURL.Text),
      //                                      aTmpPutDataString,
      //                                      AMultiPartFormDataFiles,
      //                                      FHTTPResponseStream,
      //                                      AHTTPResponseHeader)

      if (AMultiPartFormDataFiles.Count > 0) then begin

        FWinHttpClient.Put(AnsiString(editURL.Text),
                           AMultiPartFormDataFiles.Items[0].DataStream,
                           FHTTPResponseStream,
                           AHTTPResponseHeader);

      end

      else if aTmpPutDataString.Count > 0 then begin
        //if CheckBoxUrlEncodePostData.Checked then FWinHttpClient.PutURLEncoded(AnsiString(editURL.Text),
        //                                                                       aTmpPutDataString,
        //                                                                       FHTTPResponseStream,
        //                                                                       AHTTPResponseHeader,
        //                                                                       TALNameValueArray.Create(),
        //                                                                       True)
        //else begin

          ARawPutDatastream := TALStringStream.create(aTmpPutDataString.text);
          try

            FWinHttpClient.Put(AnsiString(editURL.Text),
                               ARawPutDatastream,
                               FHTTPResponseStream,
                               AHTTPResponseHeader);

          finally
            ARawPutDatastream.free;
          end;

        //end;
      end

      else FWinHttpClient.Put(AnsiString(editURL.Text),
                              nil,
                              FHTTPResponseStream,
                              AHTTPResponseHeader);

      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
    Except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(AHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    AHTTPResponseHeader.Free;
    AMultiPartFormDataFiles.Free;
    aTmpPutDataString.free;
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

{******************************************************}
procedure TForm1.ButtonSaveToFileClick(Sender: TObject);
Var LsaveDialog: TsaveDialog;
begin
  LsaveDialog := TSaveDialog.Create(self);
  Try
    if LsaveDialog.Execute then
      ALSaveStringToFile(fHTTPResponseStream.DataString, AnsiString(LsaveDialog.FileName));
  Finally
    LsaveDialog.Free;
  End;
end;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  fMustInitWinHTTP := True;
  FWinHttpClient := TaLWinHttpClient.Create;
  fHTTPResponseStream := TALStringStream.Create('');
  with FWinHttpClient do begin
    AccessType := wHttpAt_NO_PROXY;
    InternetOptions := [];
    OnStatusChange := OnHttpClientStatusChange;
    OnDownloadProgress := OnHttpDownloadProgress;
    OnUploadProgress := OnHttpUploadProgress;
    MemoRequestRawHeader.Text := String(RequestHeader.RawHeaderText);
  end;
  MemoResponseRawHeader.Height := MemoResponseRawHeader.Parent.Height - MemoResponseRawHeader.top - 6;
  MemoContentBody.Height := MemoContentBody.Parent.Height - MemoContentBody.top - 6;
end;

initialization
  {$IFDEF DEBUG}
  ReporTMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
