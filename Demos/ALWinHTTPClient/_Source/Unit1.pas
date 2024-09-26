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
  Alcinoe.StringUtils,
  Alcinoe.HTTP.Client.WinHTTP,
  dxCore,
  dxUIAClasses;

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
    fMustinitWinHTTPClient: Boolean;
    FHTTPResponseStream: TALStringStreamA;
    procedure initWinHTTPClient;
    function AnsiStrTo8bitUnicodeString(s: AnsiString): String;
  public
    procedure OnHttpClientStatus(sender: Tobject; InternetStatus: DWord; StatusInformation: Pointer; StatusInformationLength: DWord);
    procedure OnHttpClientDownloadProgress(sender: Tobject; Read: Integer; Total: Integer);
    procedure OnHttpClientUploadProgress(sender: Tobject; Sent: Integer; Total: Integer);
  end;

var
  Form1: TForm1;

implementation

Uses
  Winapi.WinHTTP,
  system.AnsiStrings,
  DateUtils,
  HttpApp,
  Alcinoe.MultiPartParser,
  Alcinoe.Files,
  Alcinoe.Common,
  Alcinoe.Mime,
  Alcinoe.StringList,
  Alcinoe.HTTP.Client;

{$R *.dfm}

{*********************************}
procedure TForm1.initWinHTTPClient;
Begin
  if not fMustinitWinHTTPClient then Exit;
  fMustinitWinHTTPClient := False;
  With FWinHTTPClient do begin
    UserName := AnsiString(EditUserName.Text);
    Password := AnsiString(EditPassword.Text);

    if AlIsInteger(AnsiString(EditConnectTimeout.Text)) then ConnectTimeout := StrToInt(EditConnectTimeout.Text);
    if AlIsInteger(AnsiString(EditsendTimeout.Text)) then SendTimeout := StrToInt(EditSendTimeout.Text);
    if AlIsInteger(AnsiString(EditReceiveTimeout.Text)) then ReceiveTimeout := StrToInt(EditReceiveTimeout.Text);

    if RadioButtonProtocolVersion1_0.Checked then ProtocolVersion := TALHTTPProtocolVersion.v1_0
    else ProtocolVersion := TALHTTPProtocolVersion.v1_1;

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

{**********************************}
procedure TForm1.OnHttpClientStatus(
            Sender: Tobject;
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
      StatusStr := 'Unknown status: ' + ALIntToStrA(InternetStatus);
   end;

 MainStatusBar.Panels[0].Text := String(StatusStr);
 application.ProcessMessages;
end;

{***********************************************************************************}
procedure TForm1.OnHttpClientDownloadProgress(sender: Tobject; Read, Total: Integer);
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

{*********************************************************************************}
procedure TForm1.OnHttpClientUploadProgress(sender: Tobject; Sent, Total: Integer);
begin
 MainStatusBar.Panels[1].Text := 'Send '+IntToStr(sent) + ' bytes of '+IntToStr(total) + ' bytes';
 application.ProcessMessages;
end;

{****************************************************************}
function TForm1.AnsiStrTo8bitUnicodeString(s: AnsiString): String;
var I: integer;
begin
  Setlength(Result, length(s));
  for I := 1 to length(s) do
    result[I] := Char(s[I]);
end;

{**************************************************}
procedure TForm1.ButtonDeleteClick(Sender: TObject);
Var LHTTPResponseHeader: TALHTTPResponseHeader;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  LHTTPResponseHeader := TALHTTPResponseHeader.Create;
  try
    fHTTPResponseStream.Size := 0;
    try
      FWinHttpClient.Delete(AnsiString(editURL.Text), fHTTPResponseStream, LHTTPResponseHeader);
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPResponseHeader.RawHeaderText);
    except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    LHTTPResponseHeader.Free;
  end;
end;

{***********************************************}
procedure TForm1.ButtonGetClick(Sender: TObject);
Var LHTTPResponseHeader: TALHTTPResponseHeader;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  LHTTPResponseHeader := TALHTTPResponseHeader.Create;
  try
    fHTTPResponseStream.Size := 0;
    try
      FWinHttpClient.Get(AnsiString(editURL.Text), FHTTPResponseStream, LHTTPResponseHeader);
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPResponseHeader.RawHeaderText);
    except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    LHTTPResponseHeader.Free;
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
Var LHTTPResponseHeader: TALHTTPResponseHeader;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  LHTTPResponseHeader := TALHTTPResponseHeader.Create;
  try
    fHTTPResponseStream.Size := 0;
    try
      FWinHttpClient.Head(AnsiString(editURL.Text), FHTTPResponseStream, LHTTPResponseHeader);
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPResponseHeader.RawHeaderText);
    except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    LHTTPResponseHeader.Free;
  end;
end;

{***************************************************}
procedure TForm1.ButtonOptionsClick(Sender: TObject);
Var LHTTPResponseHeader: TALHTTPResponseHeader;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  LHTTPResponseHeader := TALHTTPResponseHeader.Create;
  try
    fHTTPResponseStream.Size := 0;
    try
      FWinHttpClient.Options(AnsiString(editURL.Text), FHTTPResponseStream, LHTTPResponseHeader);
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPResponseHeader.RawHeaderText);
    except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    LHTTPResponseHeader.Free;
  end;
end;

{*************************************************}
procedure TForm1.ButtonTraceClick(Sender: TObject);
Var LHTTPResponseHeader: TALHTTPResponseHeader;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  LHTTPResponseHeader := TALHTTPResponseHeader.Create;
  try
    fHTTPResponseStream.Size := 0;
    try
      FWinHttpClient.Trace(AnsiString(editURL.Text), FHTTPResponseStream, LHTTPResponseHeader);
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPResponseHeader.RawHeaderText);
    except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    LHTTPResponseHeader.Free;
  end;
end;

{************************************************}
procedure TForm1.ButtonPostClick(Sender: TObject);
Var LHTTPResponseHeader: TALHTTPResponseHeader;
    LRawPostDatastream: TALStringStreamA;
    LMultiPartFormDataFile: TALMultiPartFormDataContent;
    LMultiPartFormDataFiles: TALMultiPartFormDataContents;
    LTmpPostDataString: TALStringsA;
    I: Integer;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  LHTTPResponseHeader := TALHTTPResponseHeader.Create;
  LMultiPartFormDataFiles := TALMultiPartFormDataContents.Create(true);
  LTmpPostDataString := TALStringListA.Create;
  try
    fHTTPResponseStream.Size := 0;
    Try

      LTmpPostDataString.Assign(MemoPostDataStrings.lines);
      LTmpPostDataString.Text := ansiString(MemoPostDataStrings.Text); // << I don't know yet why but MemoPostDataStrings.lines.count split very long line in severals lines of around 1000 chars

      For I := 0 To MemoPostDataFiles.Lines.Count - 1 do
        if MemoPostDataFiles.Lines[I] <> '' then begin
          LMultiPartFormDataFile := TALMultiPartFormDataContent.Create;
          TMemoryStream(LMultiPartFormDataFile.DataStream).LoadFromFile(MemoPostDataFiles.Lines.ValueFromIndex[I]);
          LMultiPartFormDataFile.ContentDisposition := 'form-data; name="'+AnsiString(MemoPostDataFiles.Lines.Names[I])+'"; filename="'+AnsiString(MemoPostDataFiles.Lines.ValueFromIndex[I])+'"';
          LMultiPartFormDataFile.ContentType := ALGetDefaultMIMEContentTypeFromExt(ALExtractFileExt(AnsiString(MemoPostDataFiles.Lines.ValueFromIndex[I])));
          LMultiPartFormDataFiles.Add(LMultiPartFormDataFile);
        end;

      if (LMultiPartFormDataFiles.Count > 0) and (CheckBoxURLEncodePostData.Checked) then
        FWinHttpClient.PostMultiPartFormData(
          AnsiString(editURL.Text),
          LTmpPostDataString,
          LMultiPartFormDataFiles,
          FHTTPResponseStream,
          LHTTPResponseHeader)

      else if (LMultiPartFormDataFiles.Count > 0) then begin

        FWinHttpClient.post(
          AnsiString(editURL.Text),
          LMultiPartFormDataFiles.Items[0].DataStream,
          FHTTPResponseStream,
          LHTTPResponseHeader);

      end

      else if LTmpPostDataString.Count > 0 then begin
        if CheckBoxUrlEncodePostData.Checked then FWinHttpClient.PostURLEncoded(
                                                    AnsiString(editURL.Text),
                                                    LTmpPostDataString,
                                                    FHTTPResponseStream,
                                                    LHTTPResponseHeader,
                                                    TALNameValueArray.Create(),
                                                    True)
        else begin

          LRawPostDatastream := TALStringStreamA.create(LTmpPostDataString.text);
          try

            FWinHttpClient.post(
              AnsiString(editURL.Text),
              LRawPostDatastream,
              FHTTPResponseStream,
              LHTTPResponseHeader);

          finally
            LRawPostDatastream.free;
          end;

        end;
      end

      else FWinHttpClient.Post(
             AnsiString(editURL.Text),
             FHTTPResponseStream,
             LHTTPResponseHeader);

      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPResponseHeader.RawHeaderText);
    Except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    LHTTPResponseHeader.Free;
    LMultiPartFormDataFiles.Free;
    LTmpPostDataString.free;
  end;
end;

{***********************************************}
procedure TForm1.ButtonPutClick(Sender: TObject);
Var LHTTPResponseHeader: TALHTTPResponseHeader;
    LRawPutDatastream: TALStringStreamA;
    LMultiPartFormDataFile: TALMultiPartFormDataContent;
    LMultiPartFormDataFiles: TALMultiPartFormDataContents;
    LTmpPutDataString: TALStringsA;
    I: Integer;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  LHTTPResponseHeader := TALHTTPResponseHeader.Create;
  LMultiPartFormDataFiles := TALMultiPartFormDataContents.Create(true);
  LTmpPutDataString := TALStringListA.Create;
  try
    fHTTPResponseStream.Size := 0;
    Try

      LTmpPutDataString.Assign(MemoPostDataStrings.lines);
      LTmpPutDataString.Text := ansiString(MemoPostDataStrings.Text); // << I don't know yet why but MemoPostDataStrings.lines.count split very long line in severals lines of around 1000 chars

      For I := 0 To MemoPostDataFiles.Lines.Count - 1 do
        if MemoPostDataFiles.Lines[I] <> '' then begin
          LMultiPartFormDataFile := TALMultiPartFormDataContent.Create;
          TMemoryStream(LMultiPartFormDataFile.DataStream).LoadFromFile(MemoPostDataFiles.Lines.ValueFromIndex[I]);
          LMultiPartFormDataFile.ContentDisposition := 'form-data; name="'+AnsiString(MemoPostDataFiles.Lines.Names[I])+'"; filename="'+AnsiString(MemoPostDataFiles.Lines.ValueFromIndex[I])+'"';
          LMultiPartFormDataFile.ContentType := ALGetDefaultMIMEContentTypeFromExt(ALExtractFileExt(AnsiString(MemoPostDataFiles.Lines.ValueFromIndex[I])));
          LMultiPartFormDataFiles.Add(LMultiPartFormDataFile);
        end;

      //if (AMultiPartFormDataFiles.Count > 0) and (CheckBoxURLEncodePostData.Checked) then
      //  FWinHttpClient.PutMultiPartFormData(AnsiString(editURL.Text),
      //                                      aTmpPutDataString,
      //                                      AMultiPartFormDataFiles,
      //                                      FHTTPResponseStream,
      //                                      AHTTPResponseHeader)

      if (LMultiPartFormDataFiles.Count > 0) then begin

        FWinHttpClient.Put(
          AnsiString(editURL.Text),
          LMultiPartFormDataFiles.Items[0].DataStream,
          FHTTPResponseStream,
          LHTTPResponseHeader);

      end

      else if LTmpPutDataString.Count > 0 then begin
        //if CheckBoxUrlEncodePostData.Checked then FWinHttpClient.PutURLEncoded(AnsiString(editURL.Text),
        //                                                                       aTmpPutDataString,
        //                                                                       FHTTPResponseStream,
        //                                                                       AHTTPResponseHeader,
        //                                                                       TALNameValueArray.Create(),
        //                                                                       True)
        //else begin

          LRawPutDatastream := TALStringStreamA.create(LTmpPutDataString.text);
          try

            FWinHttpClient.Put(
              AnsiString(editURL.Text),
              LRawPutDatastream,
              FHTTPResponseStream,
              LHTTPResponseHeader);

          finally
            LRawPutDatastream.free;
          end;

        //end;
      end

      else FWinHttpClient.Put(
             AnsiString(editURL.Text),
             nil,
             FHTTPResponseStream,
             LHTTPResponseHeader);

      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPResponseHeader.RawHeaderText);
    Except
      MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(FHTTPResponseStream.DataString);
      MemoResponseRawHeader.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPResponseHeader.RawHeaderText);
      Raise;
    end;
  finally
    LHTTPResponseHeader.Free;
    LMultiPartFormDataFiles.Free;
    LTmpPutDataString.free;
  end;
end;

{************************************************}
procedure TForm1.OnCfgEditChange(Sender: TObject);
begin
  fMustinitWinHTTPClient := True;
end;

{*****************************************************************}
procedure TForm1.OnCfgEditKeyPress(Sender: TObject; var Key: Char);
begin
  fMustinitWinHTTPClient := True;
end;

{******************************************************}
procedure TForm1.ButtonSaveToFileClick(Sender: TObject);
Var LSaveDialog: TsaveDialog;
begin
  LSaveDialog := TSaveDialog.Create(self);
  Try
    if LSaveDialog.Execute then
      ALSaveStringToFile(fHTTPResponseStream.DataString, AnsiString(LSaveDialog.FileName));
  Finally
    LSaveDialog.Free;
  End;
end;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  fMustinitWinHTTPClient := True;
  FWinHttpClient := TaLWinHttpClient.Create;
  fHTTPResponseStream := TALStringStreamA.Create('');
  with FWinHttpClient do begin
    AccessType := wHttpAt_NO_PROXY;
    InternetOptions := [];
    OnStatus := OnHttpClientStatus;
    OnDownloadProgress := OnHttpClientDownloadProgress;
    OnUploadProgress := OnHttpClientUploadProgress;
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
