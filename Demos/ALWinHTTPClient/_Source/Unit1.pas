unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, shellapi, ExtCtrls, ComCtrls, Menus, cxPCdxBarPopupMenu,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, cxLabel, cxPC, cxRadioGroup, cxCheckBox, cxButtons, cxMemo, cxTextEdit,
  cxGroupBox, dxSkinsForm, dxSkinsCore, dxSkinFoggy, dxSkinscxPCPainter,
  cxSplitter, cxClasses, dxBarBuiltInMenu, Alcinoe.http.Client, Alcinoe.StringUtils,
  Alcinoe.HTTP.Client.WinHTTP, dxCore, dxUIAClasses, cxMaskEdit, cxDropDownEdit;

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
    CheckBoxHttpOption_REFRESH: TcxCheckBox;
    GroupBox8: TcxGroupBox;
    MemoRequestRawHeader: TcxMemo;
    Label8: TcxLabel;
    RadioButtonProtocolVersion1_0: TcxRadioButton;
    RadioButtonProtocolVersion1_1: TcxRadioButton;
    GroupBox9: TcxGroupBox;
    editURL: TcxComboBox;
    Label4: TcxLabel;
    MemoPostDataStrings: TcxMemo;
    MemoPostDataFiles: TcxMemo;
    Label7: TcxLabel;
    Label5: TcxLabel;
    Label1: TcxLabel;
    ButtonPost: TcxButton;
    ButtonGet: TcxButton;
    CheckBoxHttpOption_KEEP_CONNECTION: TcxCheckBox;
    CheckBoxHttpOption_NO_COOKIES: TcxCheckBox;
    CheckBoxHttpOption_NO_AUTO_REDIRECT: TcxCheckBox;
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
    RadioButtonProtocolVersion2: TcxRadioButton;
    RadioButtonProtocolVersion3: TcxRadioButton;
    CheckBoxHttpOption_DECOMPRESSION: TcxCheckBox;
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
    function GetStatusLine(const AHttpClientResponse: TALHttpClientResponseA): String;
  private
    FWinHttpClient: TalWinHttpClient;
    FDownloadSpeedStartTime: TdateTime;
    FDownloadSpeedBytesRead: Integer;
    FDownloadSpeedBytesNotRead: Integer;
    fMustinitWinHTTPClient: Boolean;
    procedure initWinHTTPClient;
    function AnsiStrTo8bitUnicodeString(s: AnsiString): String;
  public
    procedure OnHttpClientStatus(sender: Tobject; InternetStatus: DWord; StatusInformation: Pointer; StatusInformationLength: DWord);
    procedure OnHttpClientDownloadProgress(sender: Tobject; Read: Int64; Total: Int64);
    procedure OnHttpClientUploadProgress(sender: Tobject; Sent: Int64; Total: Int64);
  end;

var
  Form1: TForm1;

implementation

Uses
  Winapi.WinHTTP,
  system.AnsiStrings,
  DateUtils,
  HttpApp,
  Alcinoe.Mime.MultiPart,
  Alcinoe.Files,
  Alcinoe.Common,
  Alcinoe.Mime.ContentTypes,
  Alcinoe.StringList,
  Alcinoe.HTTP;

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

    if RadioButtonProtocolVersion1_0.Checked then ProtocolVersion := TALHttpVersion.v1_0
    else if RadioButtonProtocolVersion2.Checked then ProtocolVersion := TALHttpVersion.v2
    else if RadioButtonProtocolVersion3.Checked then ProtocolVersion := TALHttpVersion.v3
    else ProtocolVersion := TALHttpVersion.v1_1;

    ProxyParams.ProxyServer := AnsiString(EdProxyServer.Text);
    ProxyParams.ProxyPort := StrToInt(EdProxyPort.Text);
    ProxyParams.ProxyUserName := AnsiString(EdProxyUserName.Text);
    ProxyParams.ProxyPassword := AnsiString(EdProxyPassword.Text);
    ProxyParams.ProxyBypass := AnsiString(EdProxyBypass.Text);

    if RadioButtonAccessType_NO_PROXY.Checked then AccessType := TAccessType.NO_PROXY
    else if RadioButtonAccessType_NAMED_PROXY.Checked then AccessType := TAccessType.NAMED_PROXY
    else if RadioButtonAccessType_DEFAULT_PROXY.Checked then AccessType := TAccessType.DEFAULT_PROXY;

    HttpOptions := [];
    If CheckBoxHttpOption_REFRESH.checked then HttpOptions := HttpOptions + [THttpOption.Refresh];
    If CheckBoxHttpOption_NO_COOKIES.checked then HttpOptions := HttpOptions + [THttpOption.NO_COOKIES];
    If CheckBoxHttpOption_KEEP_CONNECTION.checked then HttpOptions := HttpOptions + [THttpOption.KEEP_CONNECTION];
    If CheckBoxHttpOption_NO_AUTO_REDIRECT.checked then HttpOptions := HttpOptions + [THttpOption.NO_AUTO_REDIRECT];
    If CheckBoxHttpOption_DECOMPRESSION.checked then HttpOptions := HttpOptions + [THttpOption.DECOMPRESSION];

    RequestHeaders.RawHeaderText := AnsiString(MemoRequestRawHeader.Text);
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

{****************************************************************************************}
procedure TForm1.OnHttpClientDownloadProgress(sender: Tobject; Read: Int64; Total: Int64);
Var In1, In2: integer;
begin
  if FDownloadSpeedStartTime = 0 then Begin
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

{**************************************************************************************}
procedure TForm1.OnHttpClientUploadProgress(sender: Tobject; Sent: Int64; Total: Int64);
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
begin
  FDownloadSpeedStartTime := 0;
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  var LHTTPClientResponse := FWinHttpClient.Delete(AnsiString(editURL.Text), nil);
  try
    MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPClientResponse.BodyString);
    MemoResponseRawHeader.Lines.Text := GetStatusLine(LHTTPClientResponse) + #13#10 + AnsiStrTo8bitUnicodeString(LHTTPClientResponse.Headers.RawHeaderText);
  finally
    ALFreeAndNil(LHTTPClientResponse);
  end;
end;

{***********************************************}
procedure TForm1.ButtonGetClick(Sender: TObject);
begin
  FDownloadSpeedStartTime := 0;
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  var LHTTPClientResponse := FWinHttpClient.Get(AnsiString(editURL.Text));
  try
    MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPClientResponse.BodyString);
    MemoResponseRawHeader.Lines.Text := GetStatusLine(LHTTPClientResponse) + #13#10 + AnsiStrTo8bitUnicodeString(LHTTPClientResponse.Headers.RawHeaderText);
  finally
    ALFreeAndNil(LHTTPClientResponse);
  end;
end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  AlfreeAndNil(FWinHttpClient);
end;

{************************************************}
procedure TForm1.ButtonHeadClick(Sender: TObject);
begin
  FDownloadSpeedStartTime := 0;
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  var LHTTPClientResponse := FWinHttpClient.Head(AnsiString(editURL.Text));
  try
    MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPClientResponse.BodyString);
    MemoResponseRawHeader.Lines.Text := GetStatusLine(LHTTPClientResponse) + #13#10 + AnsiStrTo8bitUnicodeString(LHTTPClientResponse.Headers.RawHeaderText);
  finally
    ALFreeAndNil(LHTTPClientResponse);
  end;
end;

{***************************************************}
procedure TForm1.ButtonOptionsClick(Sender: TObject);
begin
  FDownloadSpeedStartTime := 0;
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  var LHTTPClientResponse := FWinHttpClient.Options(AnsiString(editURL.Text));
  try
    MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPClientResponse.BodyString);
    MemoResponseRawHeader.Lines.Text := GetStatusLine(LHTTPClientResponse) + #13#10 + AnsiStrTo8bitUnicodeString(LHTTPClientResponse.Headers.RawHeaderText);
  finally
    ALFreeAndNil(LHTTPClientResponse);
  end;
end;

{*************************************************}
procedure TForm1.ButtonTraceClick(Sender: TObject);
begin
  FDownloadSpeedStartTime := 0;
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  var LHTTPClientResponse := FWinHttpClient.Trace(AnsiString(editURL.Text));
  try
    MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPClientResponse.BodyString);
    MemoResponseRawHeader.Lines.Text := GetStatusLine(LHTTPClientResponse) + #13#10 + AnsiStrTo8bitUnicodeString(LHTTPClientResponse.Headers.RawHeaderText);
  finally
    ALFreeAndNil(LHTTPClientResponse);
  end;
end;

{************************************************}
procedure TForm1.ButtonPostClick(Sender: TObject);
begin
  FDownloadSpeedStartTime := 0;
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  var LHTTPClientResponse: TALHttpClientResponseA := nil;
  try

    if (MemoPostDataFiles.Lines.Count > 0) and (CheckBoxURLEncodePostData.Checked) then begin
      var LMultiPartFormDataEncoder := TALMultipartFormDataEncoderA.Create;
      try
        For var I := 0 To MemoPostDataFiles.Lines.Count - 1 do
          if MemoPostDataFiles.Lines[I] <> '' then
            LMultiPartFormDataEncoder.AddFile(
              AnsiString(MemoPostDataFiles.Lines.Names[I]), // const AField: AnsiString;
              MemoPostDataFiles.Lines.ValueFromIndex[I]); // const AFilePath: String;
        For var I := 0 To MemoPostDataStrings.Lines.Count - 1 do
          if MemoPostDataStrings.Lines[I] <> '' then
            LMultiPartFormDataEncoder.AddField(
              AnsiString(MemoPostDataStrings.Lines.Names[I]), // const AField, AValue: AnsiString;
              AnsiString(MemoPostDataStrings.Lines.ValueFromIndex[I])); // const AField, AValue: AnsiString;
        LHTTPClientResponse := FWinHttpClient.PostMultiPartFormData(
                                 AnsiString(editURL.Text),
                                 LMultiPartFormDataEncoder);
      finally
        ALFreeAndNil(LMultiPartFormDataEncoder);
      end;
    end

    else if (MemoPostDataFiles.Lines.Count > 0) then begin
      var LBodyStream := TFileStream.create(MemoPostDataFiles.Lines[0], fmOpenRead or fmShareDenyWrite);
      try
        LHTTPClientResponse := FWinHttpClient.post(
                                 AnsiString(editURL.Text),
                                 LBodyStream);
      finally
        ALFreeAndNil(LBodyStream);
      end;
    end

    else begin
      if CheckBoxUrlEncodePostData.Checked then begin
        var LFormParams := TALStringListA.Create;
        try
          LFormParams.Text := ansiString(MemoPostDataStrings.Text);
          LHTTPClientResponse := FWinHttpClient.PostFormURLEncoded(
                                   AnsiString(editURL.Text),
                                   LFormParams,
                                   [],
                                   True)
        finally
          ALFreeAndNil(LFormParams);
        end;
      end
      else begin
        var LBodyStream := TALStringStreamA.create(AnsiString(MemoPostDataStrings.lines.Text));
        try
          LHTTPClientResponse := FWinHttpClient.post(
                                   AnsiString(editURL.Text),
                                   LBodyStream);
        finally
          ALFreeAndNil(LBodyStream);
        end;
      end;
    end;

    MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPClientResponse.BodyString);
    MemoResponseRawHeader.Lines.Text := GetStatusLine(LHTTPClientResponse) + #13#10 + AnsiStrTo8bitUnicodeString(LHTTPClientResponse.Headers.RawHeaderText);

  finally
    ALFreeAndNil(LHTTPClientResponse);
  end;
end;

{***********************************************}
procedure TForm1.ButtonPutClick(Sender: TObject);
begin
  FDownloadSpeedStartTime := 0;
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  var LHTTPClientResponse: TALHttpClientResponseA := nil;
  try

    if (MemoPostDataFiles.Lines.Count > 0) and (CheckBoxURLEncodePostData.Checked) then begin
      var LMultiPartFormDataEncoder := TALMultipartFormDataEncoderA.Create;
      try
        For var I := 0 To MemoPostDataFiles.Lines.Count - 1 do
          if MemoPostDataFiles.Lines[I] <> '' then
            LMultiPartFormDataEncoder.AddFile(
              AnsiString(MemoPostDataFiles.Lines.Names[I]), // const AField: AnsiString;
              MemoPostDataFiles.Lines.ValueFromIndex[I]); // const AFilePath: String;
        For var I := 0 To MemoPostDataStrings.Lines.Count - 1 do
          if MemoPostDataStrings.Lines[I] <> '' then
            LMultiPartFormDataEncoder.AddField(
              AnsiString(MemoPostDataStrings.Lines.Names[I]), // const AField, AValue: AnsiString;
              AnsiString(MemoPostDataStrings.Lines.ValueFromIndex[I])); // const AField, AValue: AnsiString;
        LHTTPClientResponse := FWinHttpClient.PutMultiPartFormData(
                                 AnsiString(editURL.Text),
                                 LMultiPartFormDataEncoder);
      finally
        ALFreeAndNil(LMultiPartFormDataEncoder);
      end;
    end

    else if (MemoPostDataFiles.Lines.Count > 0) then begin
      var LBodyStream := TFileStream.create(MemoPostDataFiles.Lines[0], fmOpenRead or fmShareDenyWrite);
      try
        LHTTPClientResponse := FWinHttpClient.Put(
                                 AnsiString(editURL.Text),
                                 LBodyStream);
      finally
        ALFreeAndNil(LBodyStream);
      end;
    end

    else begin
      if CheckBoxUrlEncodePostData.Checked then begin
        var LFormParams := TALStringListA.Create;
        try
          LFormParams.Text := ansiString(MemoPostDataStrings.Text);
          LHTTPClientResponse := FWinHttpClient.PutFormURLEncoded(
                                   AnsiString(editURL.Text),
                                   LFormParams,
                                   [],
                                   True)
        finally
          ALFreeAndNil(LFormParams);
        end;
      end
      else begin
        var LBodyStream := TALStringStreamA.create(AnsiString(MemoPostDataStrings.lines.Text));
        try
          LHTTPClientResponse := FWinHttpClient.Put(
                                   AnsiString(editURL.Text),
                                   LBodyStream);
        finally
          ALFreeAndNil(LBodyStream);
        end;
      end;
    end;

    MemoContentBody.Lines.Text := AnsiStrTo8bitUnicodeString(LHTTPClientResponse.BodyString);
    MemoResponseRawHeader.Lines.Text := GetStatusLine(LHTTPClientResponse) + #13#10 + AnsiStrTo8bitUnicodeString(LHTTPClientResponse.Headers.RawHeaderText);

  finally
    ALFreeAndNil(LHTTPClientResponse);
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

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  fMustinitWinHTTPClient := True;
  FWinHttpClient := TaLWinHttpClient.Create;
  FWinHttpClient.OnStatus := OnHttpClientStatus;
  FWinHttpClient.OnDownloadProgress := OnHttpClientDownloadProgress;
  FWinHttpClient.OnUploadProgress := OnHttpClientUploadProgress;
  MemoRequestRawHeader.Text := String(FWinHttpClient.RequestHeaders.RawHeaderText);
  MemoResponseRawHeader.Height := MemoResponseRawHeader.Parent.Height - MemoResponseRawHeader.top - 6;
  MemoContentBody.Height := MemoContentBody.Parent.Height - MemoContentBody.top - 6;
end;

{***************************************************************************************}
function TForm1.GetStatusLine(const AHttpClientResponse: TALHttpClientResponseA): String;
begin
  case AHttpClientResponse.Version of
    TALHttpVersion.Unspecified: Result := 'Unspecified ';
    TALHttpVersion.v0_9: Result := 'HTTP/0.9 ';
    TALHttpVersion.v1_0: Result := 'HTTP/1.0 ';
    TALHttpVersion.v1_1: Result := 'HTTP/1.1 ';
    TALHttpVersion.v2: Result := 'HTTP/2 ';
    TALHttpVersion.v3: Result := 'HTTP/3 ';
  end;
  Result := Result + ALInttoStrW(AHttpClientResponse.StatusCode);
  Result := Result + ' ' + string(AHttpClientResponse.Reason);
end;

initialization
  {$IFDEF DEBUG}
  ReporTMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.