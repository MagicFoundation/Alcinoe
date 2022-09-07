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
  WinInet,
  shellapi,
  ExtCtrls,
  ComCtrls,
  AlWininetHttpClient,
  cxPCdxBarPopupMenu,
  cxGraphics,
  cxControls,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  cxContainer,
  cxEdit,
  Menus,
  cxRadioGroup,
  cxCheckBox,
  cxButtons,
  cxMemo,
  cxTextEdit,
  cxLabel,
  cxGroupBox,
  cxPC,
  dxSkinsCore,
  dxSkinFoggy,
  dxSkinscxPCPainter,
  dxSkinsForm,
  cxSplitter,
  cxClasses,
  dxBarBuiltInMenu,
  ALString,
  system.AnsiStrings;

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
    RadioButtonAccessType_Direct: TcxRadioButton;
    RadioButtonAccessType_Preconfig: TcxRadioButton;
    RadioButtonAccessType_Preconfig_with_no_autoproxy: TcxRadioButton;
    RadioButtonAccessType_Proxy: TcxRadioButton;
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
    CheckBoxInternetOption_From_Cache: TcxCheckBox;
    CheckBoxInternetOption_Offline: TcxCheckBox;
    CheckBoxInternetOption_Keep_connection: TcxCheckBox;
    CheckBoxInternetOption_No_auto_redirect: TcxCheckBox;
    CheckBoxInternetOption_Ignore_redirect_to_https: TcxCheckBox;
    CheckBoxInternetOption_No_auth: TcxCheckBox;
    CheckBoxInternetOption_Ignore_cert_date_invalid: TcxCheckBox;
    CheckBoxInternetOption_Need_file: TcxCheckBox;
    CheckBoxInternetOption_Ignore_redirect_to_http: TcxCheckBox;
    CheckBoxInternetOption_Hyperlink: TcxCheckBox;
    CheckBoxInternetOption_Ignore_cert_cn_invalid: TcxCheckBox;
    CheckBoxInternetOption_Cache_if_net_fail: TcxCheckBox;
    CheckBoxInternetOption_No_cache_write: TcxCheckBox;
    CheckBoxInternetOption_Resynchronize: TcxCheckBox;
    CheckBoxInternetOption_No_cookies: TcxCheckBox;
    CheckBoxInternetOption_Pragma_nocache: TcxCheckBox;
    CheckBoxInternetOption_Reload: TcxCheckBox;
    CheckBoxInternetOption_No_ui: TcxCheckBox;
    CheckBoxInternetOption_Secure: TcxCheckBox;
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
    GroupBox10: TcxGroupBox;
    Label1: TcxLabel;
    ButtonPost: TcxButton;
    ButtonGet: TcxButton;
    ButtonHead: TcxButton;
    CheckBoxUrlEncodePostData: TcxCheckBox;
    ButtonTrace: TcxButton;
    dxSkinController1: TdxSkinController;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Label2: TcxLabel;
    MemoResponseRawHeader: TcxMemo;
    cxSplitter1: TcxSplitter;
    Label3: TcxLabel;
    MemoContentBody: TcxMemo;
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
    procedure OnCfgEditCHange(Sender: TObject);
    procedure OnCfgEditKeyPress(Sender: TObject; var Key: Char);
    procedure ButtonOptionsClick(Sender: TObject);
    procedure ButtonPutClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonSaveToFileClick(Sender: TObject);
  private
    FWinInetHttpClient: TalWinInetHttpClient;
    FDownloadSpeedStartTime: TdateTime;
    FDownloadSpeedBytesRead: Integer;
    FDownloadSpeedBytesNotRead: Integer;
    fMustInitWinHTTP: Boolean;
    FHTTPResponseStream: TALStringStream;
    procedure initWinInetHTTPClient;
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
  DateUtils,
  HttpApp,
  ALMultiPartParser,
  AlCommon,
  AlFiles,
  ALMime,
  AlStringList,
  AlHTTPClient;

{$R *.dfm}

{*************************************}
procedure TForm1.initWinInetHTTPClient;
Begin
  if not fMustInitWinHTTP then exit;
  fMustInitWinHTTP := False;
  With FWinInetHTTPClient do begin
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

    RequestHeader.RawHeaderText := AnsiString(MemoRequestRawHeader.Text);
  end;
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
procedure TForm1.OnHttpClientStatus(Sender: Tobject;
                                    InternetStatus: DWord;
                                    StatusInformation: Pointer;
                                    StatusInformationLength: DWord);
var StatusStr: AnsiString;
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
      StatusStr := 'Unknown status: ' + ALIntToStr(InternetStatus);
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

{**************************************************}
procedure TForm1.ButtonDeleteClick(Sender: TObject);
Var AHTTPResponseHeader: TALHTTPResponseHeader;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinInetHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  try
    fHTTPResponseStream.Size := 0;
    try
      FWinInetHttpClient.Delete(AnsiString(editURL.Text), FHTTPResponseStream, AHTTPResponseHeader);
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
  initWinInetHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  try
    fHTTPResponseStream.Size := 0;
    try
      FWinInetHttpClient.Get(AnsiString(editURL.Text), FHTTPResponseStream, AHTTPResponseHeader);
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
  FWinInetHttpClient.Free;
  fHTTPResponseStream.Free;
end;

{************************************************}
procedure TForm1.ButtonHeadClick(Sender: TObject);
Var AHTTPResponseHeader: TALHTTPResponseHeader;
begin
  MainStatusBar.Panels[0].Text := '';
  MainStatusBar.Panels[1].Text := '';
  MainStatusBar.Panels[2].Text := '';
  initWinInetHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  try
    fHTTPResponseStream.Size := 0;
    try
      FWinInetHttpClient.Head(AnsiString(editURL.Text), FHTTPResponseStream, AHTTPResponseHeader);
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
  initWinInetHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  try
    fHTTPResponseStream.Size := 0;
    try
      FWinInetHttpClient.Options(AnsiString(editURL.Text), FHTTPResponseStream, AHTTPResponseHeader);
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
  initWinInetHTTPClient;
  MemoContentBody.Lines.Clear;
  MemoResponseRawHeader.Lines.Clear;
  AHTTPResponseHeader := TALHTTPResponseHeader.Create;
  try
    fHTTPResponseStream.Size := 0;
    try
      FWinInetHttpClient.Trace(AnsiString(editURL.Text), FHTTPResponseStream, AHTTPResponseHeader);
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
procedure TForm1.OnCfgEditCHange(Sender: TObject);
begin
  fMustInitWinHTTP := True;
end;

{*****************************************************************}
procedure TForm1.OnCfgEditKeyPress(Sender: TObject; var Key: Char);
begin
  fMustInitWinHTTP := True;
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
  initWinInetHTTPClient;
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
        FWinInetHttpClient.PostMultiPartFormData(AnsiString(editURL.Text),
                                                 aTmpPostDataString,
                                                 AMultiPartFormDataFiles,
                                                 FHTTPResponseStream,
                                                 AHTTPResponseHeader)

      else if (AMultiPartFormDataFiles.Count > 0) then begin

        FWinInetHttpClient.post(AnsiString(editURL.Text),
                                AMultiPartFormDataFiles.Items[0].DataStream,
                                FHTTPResponseStream,
                                AHTTPResponseHeader);

      end

      else if aTmpPostDataString.Count > 0 then begin
        if CheckBoxURLEncodePostData.Checked then FWinInetHttpClient.PostURLEncoded(AnsiString(editURL.Text),
                                                                                    aTmpPostDataString,
                                                                                    FHTTPResponseStream,
                                                                                    AHTTPResponseHeader,
                                                                                    TALNameValueArray.Create(),
                                                                                    True)
        else begin

          ARawPostDatastream := TALStringStream.create(aTmpPostDataString.text);
          try

            FWinInetHttpClient.post(AnsiString(editURL.Text),
                                    ARawPostDatastream,
                                    FHTTPResponseStream,
                                    AHTTPResponseHeader);

          finally
            ARawPostDatastream.free;
          end;

        end
      end

      else FWinInetHttpClient.Post(AnsiString(editURL.Text),
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
  initWinInetHTTPClient;
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
      //  FWinInetHttpClient.PutMultiPartFormData(AnsiString(editURL.Text),
      //                                          aTmpPutDataString,
      //                                          AMultiPartFormDataFiles,
      //                                          FHTTPResponseStream,
      //                                          AHTTPResponseHeader)

      if (AMultiPartFormDataFiles.Count > 0) then begin

        FWinInetHttpClient.Put(AnsiString(editURL.Text),
                               AMultiPartFormDataFiles.Items[0].DataStream,
                               FHTTPResponseStream,
                               AHTTPResponseHeader);

      end

      else if aTmpPutDataString.Count > 0 then begin
        //if CheckBoxURLEncodePostData.Checked then FWinInetHttpClient.PutURLEncoded(AnsiString(editURL.Text),
        //                                                                           aTmpPutDataString,
        //                                                                           FHTTPResponseStream,
        //                                                                           AHTTPResponseHeader,
        //                                                                           TALNameValueArray.Create(),
        //                                                                           True)
        //else begin

          ARawPutDatastream := TALStringStream.create(aTmpPutDataString.text);
          try

            FWinInetHttpClient.Put(AnsiString(editURL.Text),
                                   ARawPutDatastream,
                                   FHTTPResponseStream,
                                   AHTTPResponseHeader);

          finally
            ARawPutDatastream.free;
          end;

        //end
      end

      else FWinInetHttpClient.Put(AnsiString(editURL.Text),
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

{******************************************************}
procedure TForm1.ButtonSaveToFileClick(Sender: TObject);
Var LsaveDialog: TSaveDialog;
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
  FWinInetHttpClient := TaLWinInetHttpClient.Create;
  fHTTPResponseStream := TALStringStream.Create('');
  with FWinInetHttpClient do begin
    AccessType := wHttpAt_Preconfig;
    InternetOptions := [wHttpIo_Keep_connection];
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
