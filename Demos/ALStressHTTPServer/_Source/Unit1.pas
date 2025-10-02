unit Unit1;

interface

uses
  Windows, winapi.winhttp, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, shellapi, ExtCtrls, ComCtrls,
  Alcinoe.HTTP.Client, Alcinoe.HTTP.Client.WinHTTP, Alcinoe.StringList,
  cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxEdit,
  cxDropDownEdit, cxImageComboBox, cxSpinEdit, cxGridLevel,
  cxGridCustomTableView, cxGridTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, Spin, dxSkinsCore, dxSkinFoggy, dxSkinscxPCPainter,
  cxPCdxBarPopupMenu, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, Menus,
  dxSkinsForm, cxRadioGroup, cxGroupBox, cxButtons, cxTextEdit, cxMaskEdit,
  cxCheckBox, cxMemo, cxLabel, cxPC, uiTypes, cxNavigator, dxBarBuiltInMenu,
  dxDateRanges, dxScrollbarAnnotations, dxCore, dxUIAClasses, Alcinoe.HTTP,
  system.Diagnostics, system.Generics.Collections, dxShellDialogs,
  Alcinoe.StringUtils;

type

  THttpRequestMetrics = record
  public
    &On: Boolean;
    Url: AnsiString;
    RequestCount: Integer;
    StatusCode: Integer;
    TotalBytesSent: Int64;
    TotalBytesRead: Int64;
    AverageDNSTimeTaken: Double;
    AverageConnectTimeTaken: Double;
    TotalSendTimeTaken: Double;
    AverageSendTimeTaken: Double;
    AverageWaitTimeTaken: Double;
    TotalReceiveTimeTaken: Double;
    AverageReceiveTimeTaken: Double;
  End;

  TWorkerThread = Class(Tthread)
  private
    FBytesSent: Int64;
    FBytesRead: Int64;
    FDNSStopWatch: TStopWatch;
    FConnectStopWatch: TStopWatch;
    FSendStopWatch: TStopWatch;
    FWaitStopWatch: TStopWatch;
    FReceiveStopWatch: TStopWatch;
    FStopOnError: Boolean;
    FSimulateSlowClient: Integer;
    FDelayBetweenEachCall: integer;
    FUrls: TALStringListA;
    FMaxHttpRequest: Integer;
    FIndex: integer;
    FBodyStream: TALStringStreamA;
    procedure OnHttpDownloadProgress(sender: Tobject; Read: Int64; Total: Int64);
    procedure OnHttpUploadProgress(Sender: Tobject; Sent: Int64; Total: Int64);
    procedure OnHttpStatus(
                sender: Tobject;
                InternetStatus: DWord;
                StatusInformation: Pointer;
                StatusInformationLength: DWord);
  protected
    procedure Execute; override;
  Public
    constructor Create(CreateSuspended: Boolean; aIndex: integer);
    destructor Destroy; override;
  End;

  TForm1 = class(TForm)
    PageControl1: TcxPageControl;
    TabSheet1: TcxTabSheet;
    TabSheet2: TcxTabSheet;
    Label4: TcxLabel;
    MemoLstUrl: TcxMemo;
    Label1: TcxLabel;
    Label2: TcxLabel;
    GridThread: TcxGrid;
    TableViewThread: TcxGridTableView;
    TableViewThreadNumber: TcxGridColumn;
    TableViewThreadUrl: TcxGridColumn;
    TableViewThreadHttpStatus: TcxGridColumn;
    TableViewThreadBytesReceived: TcxGridColumn;
    TableViewThreadDownloadSpeed: TcxGridColumn;
    levelThread: TcxGridLevel;
    TableViewThreadRequestCount: TcxGridColumn;
    CheckBoxStopOnError: TcxCheckBox;
    MainStatusBar: TStatusBar;
    EditMaxHttpRequest: TcxSpinEdit;
    EditNbThread: TcxSpinEdit;
    Label3: TcxLabel;
    EditSendDelayBetweenEachSend: tcxtextedit;
    Label5: TcxLabel;
    TableViewThreadDNS: TcxGridColumn;
    TableViewThreadConnect: TcxGridColumn;
    TableViewThreadReceive: TcxGridColumn;
    TableViewThreadSend: TcxGridColumn;
    TableViewThreadWait: TcxGridColumn;
    ButtonStart: TcxButton;
    dxSkinController1: TdxSkinController;
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
    RadioButtonProtocolVersion1_0: TcxRadioButton;
    RadioButtonProtocolVersion1_1: TcxRadioButton;
    RadioButtonProtocolVersion2: TcxRadioButton;
    RadioButtonProtocolVersion3: TcxRadioButton;
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
    GroupBox2: TcxGroupBox;
    RadioButtonAccessType_NAMED_PROXY: TcxRadioButton;
    RadioButtonAccessType_NO_PROXY: TcxRadioButton;
    RadioButtonAccessType_DEFAULT_PROXY: TcxRadioButton;
    GroupBox7: TcxGroupBox;
    CheckBoxHttpOption_REFRESH: TcxCheckBox;
    CheckBoxHttpOption_KEEP_CONNECTION: TcxCheckBox;
    CheckBoxHttpOption_NO_COOKIES: TcxCheckBox;
    CheckBoxHttpOption_NO_AUTO_REDIRECT: TcxCheckBox;
    CheckBoxHttpOption_DECOMPRESSION: TcxCheckBox;
    GroupBox8: TcxGroupBox;
    Label8: TcxLabel;
    MemoRequestRawHeader: TcxMemo;
    UpdateGuiTimer: TTimer;
    EditFileToUpload: TcxTextEdit;
    cxLabel1: TcxLabel;
    ButtonSelectFileToUpload: TcxButton;
    MainFileOpenDialog: TFileOpenDialog;
    TableViewThreadBytesSent: TcxGridColumn;
    TableViewThreadUploadSpeed: TcxGridColumn;
    ComboBoxSimulateSlowClient: TcxComboBox;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
    procedure ButtonStartClick(Sender: TObject);
    procedure TableViewThreadBytesGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: string);
    procedure TableViewThreadSpeedGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: String);
    procedure TableViewThreadTimeTakenGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: String);
    procedure TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItemsSpeedGetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
    procedure TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItemsBytesGetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: string);
    procedure UpdateGuiTimerTimer(Sender: TObject);
    procedure ButtonSelectFileToUploadClick(Sender: TObject);
    procedure MainFileOpenDialogFileOkClick(Sender: TObject; var CanClose: Boolean);
  private
    FHttpClientUserName: AnsiString;
    FHttpClientPassword: AnsiString;
    FHttpClientConnectTimeout: integer;
    FHttpClientSendTimeout: integer;
    FHttpClientReceiveTimeout: integer;
    FHttpClientProtocolVersion: TALHTTPVersion;
    FHttpClientProxyServer: AnsiString;
    FHttpClientProxyPort: integer;
    FHttpClientProxyUserName: AnsiString;
    FHttpClientProxyPassword: AnsiString;
    FHttpClientProxyBypass: AnsiString;
    FHttpClientAccessType: TALWinHttpClient.TAccessType;
    FHttpClientHttpOptions: TALWinHttpClient.THttpOptionSet;
    FHttpClientRawHeaderText: ansiString;
    FStartStopWatch: TStopWatch;
    FHttpRequestMetrics: TArray<THttpRequestMetrics>;
    FBodyString: AnsiString;
    procedure UpdateGUI;
    procedure initHttpClientParams;
  end;

var
  Form1: TForm1;

implementation

Uses
  System.AnsiStrings,
  Math,
  DateUtils,
  Alcinoe.Mime.Multipart,
  Alcinoe.Files,
  Alcinoe.Common,
  Alcinoe.WinApi.windows,
  Alcinoe.HTML,
  Alcinoe.Cipher,
  Alcinoe.Localization;

{$R *.dfm}

{************************************}
procedure TForm1.initHttpClientParams;
Begin
  FHttpClientUserName := AnsiString(EditUserName.Text);
  FHttpClientPassword := AnsiString(EditPassword.Text);

  FHttpClientConnectTimeout := ALStrToInt(EditConnectTimeout.Text);
  FHttpClientSendTimeout := ALStrToInt(EditSendTimeout.Text);
  FHttpClientReceiveTimeout := ALStrToInt(EditReceiveTimeout.Text);

  if RadioButtonProtocolVersion1_0.Checked then FHttpClientProtocolVersion := TALHttpVersion.v1_0
  else if RadioButtonProtocolVersion2.Checked then FHttpClientProtocolVersion := TALHttpVersion.v2
  else if RadioButtonProtocolVersion3.Checked then FHttpClientProtocolVersion := TALHttpVersion.v3
  else FHttpClientProtocolVersion := TALHttpVersion.v1_1;

  FHttpClientProxyServer := AnsiString(EdProxyServer.Text);
  FHttpClientProxyPort := ALStrToInt(EdProxyPort.Text);
  FHttpClientProxyUserName := AnsiString(EdProxyUserName.Text);
  FHttpClientProxyPassword := AnsiString(EdProxyPassword.Text);
  FHttpClientProxyBypass := AnsiString(EdProxyBypass.Text);

  if RadioButtonAccessType_NO_PROXY.Checked then FHttpClientAccessType := TALWinHttpClient.TAccessType.NO_PROXY
  else if RadioButtonAccessType_NAMED_PROXY.Checked then FHttpClientAccessType := TALWinHttpClient.TAccessType.NAMED_PROXY
  else if RadioButtonAccessType_DEFAULT_PROXY.Checked then FHttpClientAccessType := TALWinHttpClient.TAccessType.DEFAULT_PROXY;

  FHttpClientHttpOptions := [];
  If CheckBoxHttpOption_REFRESH.checked then FHttpClientHttpOptions := FHttpClientHttpOptions + [TALWinHttpClient.THttpOption.Refresh];
  If CheckBoxHttpOption_NO_COOKIES.checked then FHttpClientHttpOptions := FHttpClientHttpOptions + [TALWinHttpClient.THttpOption.NO_COOKIES];
  If CheckBoxHttpOption_KEEP_CONNECTION.checked then FHttpClientHttpOptions := FHttpClientHttpOptions + [TALWinHttpClient.THttpOption.KEEP_CONNECTION];
  If CheckBoxHttpOption_NO_AUTO_REDIRECT.checked then FHttpClientHttpOptions := FHttpClientHttpOptions + [TALWinHttpClient.THttpOption.NO_AUTO_REDIRECT];
  If CheckBoxHttpOption_DECOMPRESSION.checked then FHttpClientHttpOptions := FHttpClientHttpOptions + [TALWinHttpClient.THttpOption.DECOMPRESSION];

  FHttpClientRawHeaderText := AnsiString(MemoRequestRawHeader.Text);
end;

{*******************************************************************************************************************************************}
procedure TForm1.MainFileOpenDialogFileOkClick(Sender: TObject; var CanClose: Boolean);
begin
  EditFileToUpload.Text := MainFileOpenDialog.FileName;
end;

{**************************************************************************************************************************************************************************************}
procedure TForm1.TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItemsSpeedGetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
begin
  var LBytes: Int64;
  If Not ALTryStrToInt64(aText, LBytes) then LBytes := 0;
  if LBytes > 1_000_000_000 then aText := ALFloatToStrW(SimpleRoundTo(LBytes / 1_000_000_000, -2)) + ' GB/s'
  else if LBytes > 1_000_000 then aText := ALFloatToStrW(SimpleRoundTo(LBytes / 1_000_000, -2)) + ' MB/s'
  else if LBytes > 1000 then aText := ALFloatToStrW(SimpleRoundTo(LBytes / 1000, -2)) + ' KB/s'
  else aText := ALIntToStrW(LBytes) + ' B/s';
end;

{**************************************************************************************************************************************************************************************}
procedure TForm1.TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItemsBytesGetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: string);
begin
  var LBytes: Int64;
  If Not ALTryStrToInt64(aText, LBytes) then LBytes := 0;
  if LBytes > 1_000_000_000 then aText := ALFloatToStrW(SimpleRoundTo(LBytes / 1_000_000_000, -2)) + ' GB'
  else if LBytes > 1_000_000 then aText := ALFloatToStrW(SimpleRoundTo(LBytes / 1_000_000, -2)) + ' MB'
  else if LBytes > 1000 then aText := ALFloatToStrW(SimpleRoundTo(LBytes / 1000, -2)) + ' KB'
  else aText := ALIntToStrW(LBytes) + ' B';
end;

{*******************************************************************************************************************************************}
procedure TForm1.TableViewThreadBytesGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: string);
begin
  var LBytes: Int64;
  If Not ALTryStrToInt64(aText, LBytes) then LBytes := 0;
  if LBytes > 1_000_000_000 then aText := ALFloatToStrW(SimpleRoundTo(LBytes / 1_000_000_000, -2)) + ' GB'
  else if LBytes > 1_000_000 then aText := ALFloatToStrW(SimpleRoundTo(LBytes / 1_000_000, -2)) + ' MB'
  else if LBytes > 1000 then aText := ALFloatToStrW(SimpleRoundTo(LBytes / 1000, -2)) + ' KB'
  else aText := ALIntToStrW(LBytes) + ' B';
end;

{************************************************}
procedure TForm1.TableViewThreadSpeedGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: String);
begin
  var LBytes: Int64;
  If Not ALTryStrToInt64(aText, LBytes) then LBytes := 0;
  if LBytes > 1_000_000_000 then aText := ALFloatToStrW(SimpleRoundTo(LBytes / 1_000_000_000, -2)) + ' GB/s'
  else if LBytes > 1_000_000 then aText := ALFloatToStrW(SimpleRoundTo(LBytes / 1_000_000, -2)) + ' MB/s'
  else if LBytes > 1000 then aText := ALFloatToStrW(SimpleRoundTo(LBytes / 1000, -2)) + ' KB/s'
  else aText := ALIntToStrW(LBytes) + ' B/s';
end;

{***************************************************************************************************************************************}
procedure TForm1.TableViewThreadTimeTakenGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: String);
begin
  if AText <> '' then AText := AText + ' ms';
end;

{********************************************}
procedure TForm1.UpdateGuiTimerTimer(Sender: TObject);
begin
  UpdateGUI;
end;

{*************************}
procedure TForm1.UpdateGUI;
begin

  var LNBActiveThread := length(FHttpRequestMetrics);
  TableViewThread.BeginUpdate;
  try

    For var I := low(FHttpRequestMetrics) to high(FHttpRequestMetrics) do begin
      with FHttpRequestMetrics[i] do begin
        if not On then begin
          dec(LNBActiveThread);
          TableViewThread.DataController.SetValue(I, TableViewThreadNumber.Index, ALIntToStrW(I+1) + ' (off)');
        end;
        TableViewThread.DataController.SetValue(I, TableViewThreadRequestCount.Index, RequestCount);
        TableViewThread.DataController.SetValue(I, TableViewThreadUrl.Index, Url);
        if StatusCode >= 0 then TableViewThread.DataController.SetValue(I, TableViewThreadHttpStatus.Index, StatusCode)
        else TableViewThread.DataController.SetValue(I, TableViewThreadHttpStatus.Index, null);
        TableViewThread.DataController.SetValue(I, TableViewThreadDNS.Index, SimpleRoundTo(AverageDNSTimeTaken, -2));
        TableViewThread.DataController.SetValue(I, TableViewThreadConnect.Index, SimpleRoundTo(AverageConnectTimeTaken, -2));
        TableViewThread.DataController.SetValue(I, TableViewThreadSend.Index, SimpleRoundTo(AverageSendTimeTaken, -2));
        TableViewThread.DataController.SetValue(I, TableViewThreadWait.Index, SimpleRoundTo(AverageWaitTimeTaken, -2));
        TableViewThread.DataController.SetValue(I, TableViewThreadReceive.Index, SimpleRoundTo(AverageReceiveTimeTaken, -2));
        TableViewThread.DataController.SetValue(I, TableViewThreadBytesSent.Index, TotalBytesSent);
        TableViewThread.DataController.SetValue(I, TableViewThreadBytesReceived.Index, TotalBytesRead);
        if (TotalBytesSent > 0) and (TotalSendTimeTaken > 0) then
          TableViewThread.DataController.SetValue(I, TableViewThreadUploadSpeed.Index, Round(TotalBytesSent / (TotalSendTimeTaken / 1000)))
        else
          TableViewThread.DataController.SetValue(I, TableViewThreadUploadSpeed.Index, null);
        if (TotalBytesRead > 0) and (TotalReceiveTimeTaken > 0) then
          TableViewThread.DataController.SetValue(I, TableViewThreadDownloadSpeed.Index, Round(TotalBytesRead / (TotalReceiveTimeTaken / 1000)))
        else
          TableViewThread.DataController.SetValue(I, TableViewThreadDownloadSpeed.Index, null);
      end;
    end;

  finally
    TableViewThread.EndUpdate;
  end;

  MainStatusBar.Panels[0].Text := '# Threads: ' + ALIntToStrW(LNBActiveThread);
  if LNBActiveThread = 0 then begin
    ButtonStart.Tag := 0;
    ButtonStart.Caption := 'Start';
    UpdateGuiTimer.Enabled := False;
    FStartStopWatch.Stop;
    Setlength(FHttpRequestMetrics, 0);
  end;

  var LTotalRequestCount: Integer := 0;
  for var I := 0 to TableViewThread.DataController.RecordCount - 1 do
    LTotalRequestCount := LTotalRequestCount + TableViewThread.DataController.GetValue(I, TableViewThreadRequestCount.Index);
  MainStatusBar.Panels[1].Text := ALIntToStrW(LTotalRequestCount) +
                               ' Requests in ' +
                               ALFormatFloatW('0.##', FStartStopWatch.Elapsed.TotalMilliseconds / 1000) +
                               ' seconds (' +
                               ALFormatFloatW('0.##', LTotalRequestCount / (Max(FStartStopWatch.Elapsed.TotalMilliseconds, 1) / 1000)) +
                               ' Request/s)';

end;

{*************************************************}
procedure TForm1.ButtonSelectFileToUploadClick(Sender: TObject);
begin
  MainFileOpenDialog.Execute;
end;

{*************************************************}
procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  If ButtonStart.tag = 0 then begin
    if length(FHttpRequestMetrics) > 0 then begin
      messageDlg('Busy! Please wait a moment.',mtError,[mbok],0);
      exit;
    end;
    if MemoLstUrl.Lines.Count = 0 then  begin
      messageDlg('Please write some URL!',mtError,[mbok],0);
      exit;
    end;
    ButtonStart.Tag := 1;
    ButtonStart.Caption := 'Stop';
  end
  else If ButtonStart.tag = 1 then begin
    ButtonStart.Tag := 0;
    ButtonStart.Caption := 'Start';
    exit;
  end;

  initHttpClientParams;
  if EditFileToUpload.Text <> '' then FBodyString := ALGetStringFromFile(EditFileToUpload.Text)
  else FBodyString := '';
  Setlength(FHttpRequestMetrics, ALStrToInt(EditNbThread.Text));
  For var I := low(FHttpRequestMetrics) to high(FHttpRequestMetrics) do begin
    FHttpRequestMetrics[I].&On := True;
    FHttpRequestMetrics[I].Url := '';
    FHttpRequestMetrics[I].RequestCount := 0;
    FHttpRequestMetrics[I].StatusCode := -1;
    FHttpRequestMetrics[I].TotalBytesSent := 0;
    FHttpRequestMetrics[I].TotalBytesRead := 0;
    FHttpRequestMetrics[I].AverageDNSTimeTaken := 0;
    FHttpRequestMetrics[I].AverageConnectTimeTaken := 0;
    FHttpRequestMetrics[I].totalSendTimeTaken := 0;
    FHttpRequestMetrics[I].AverageSendTimeTaken := 0;
    FHttpRequestMetrics[I].AverageWaitTimeTaken := 0;
    FHttpRequestMetrics[I].totalReceiveTimeTaken := 0;
    FHttpRequestMetrics[I].AverageReceiveTimeTaken := 0;
  end;
  TableViewThread.DataController.RecordCount := length(FHttpRequestMetrics);
  FStartStopWatch := TStopWatch.StartNew;

  MainStatusBar.Panels[1].Text := '';
  for var I := 0 to ALStrToInt(EditNbThread.Text) - 1 do begin
    TableViewThread.DataController.SetValue(I,TableViewThreadNumber.Index,ALIntToStrW(I+1) + ' (on)');
    TableViewThread.DataController.SetValue(I,TableViewThreadRequestCount.Index,0);
    var LWorkerThread := TWorkerThread.Create(True, I);
    for var J := 0 to MemoLstUrl.Lines.Count - 1 do
      if Trim(MemoLstUrl.Lines[J]) <> '' then
        LWorkerThread.FUrls.Add(AnsiString(MemoLstUrl.Lines[J]));
    LWorkerThread.FMaxHttpRequest := ALStrToInt(EditMaxHttpRequest.Text);
    LWorkerThread.FDelayBetweenEachCall := ALStrToInt(EditSendDelayBetweenEachSend.text);
    LWorkerThread.FStopOnError := CheckBoxStopOnError.Checked;
    LWorkerThread.FSimulateSlowClient := ALStrToIntDef(ComboBoxSimulateSlowClient.Text, 0);
    if FBodyString <> '' then LWorkerThread.FBodyStream.DataString := FBodyString;
    MainStatusBar.Panels[0].Text := '# Threads: ' + ALIntToStrW(length(FHttpRequestMetrics));
    LWorkerThread.FreeOnTerminate := True;
    LWorkerThread.Start;
  end;
  UpdateGuiTimer.Enabled := True;
end;

{**************************************************************************}
constructor TWorkerThread.Create(CreateSuspended: Boolean; AIndex: integer);
begin
  inherited Create(CreateSuspended);
  FBytesSent := 0;
  FBytesRead := 0;
  //FDNSStopWatch
  //FConnectStopWatch
  //FSendStopWatch
  //FWaitStopWatch
  //FReceiveStopWatch
  FStopOnError := False;
  FSimulateSlowClient := 0;
  FDelayBetweenEachCall := 0;
  FUrls := TALStringListA.Create;
  FMaxHttpRequest := 0;
  FIndex := AIndex;
  FBodyStream := TALStringStreamA.Create('');
end;

{***********************************}
destructor TWorkerThread.Destroy;
begin
  ALFreeAndNil(FUrls);
  ALFreeAndNil(FBodyStream);
  inherited;
end;

{**********************************}
procedure TWorkerThread.Execute;
begin

  var LUrl: AnsiString := '';
  var LStatusCode: Integer := 0;
  var LRequestCount: Integer := 0;
  var LTotalBytesSent: Int64 := 0;
  var LTotalBytesRead: Int64 := 0;
  var LDNSCount: Integer := 0;
  var LDNSTimeTaken: Double := 0;
  var LConnectCount: Integer := 0;
  var LConnectTimeTaken: Double := 0;
  var LSendCount: Integer := 0;
  var LSendTimeTaken: Double := 0;
  var LWaitCount: Integer := 0;
  var LWaitTimeTaken: Double := 0;
  var LReceiveCount: Integer := 0;
  var LReceiveTimeTaken: Double := 0;

  var LHttpClient := TaLWinHttpClient.Create;
  try

    with LHttpClient do begin
      OnUploadProgress := OnHttpUploadProgress;
      OnDownloadProgress := OnHttpDownloadProgress;
      OnStatus := OnHttpStatus;
      UserName := Form1.FHttpClientUserName;
      Password := Form1.FHttpClientPassword;
      ConnectTimeout := Form1.FHttpClientConnectTimeout;
      SendTimeout := Form1.FHttpClientSendTimeout;
      ReceiveTimeout := Form1.FHttpClientReceiveTimeout;
      ProtocolVersion := Form1.FHttpClientProtocolVersion;
      ProxyParams.ProxyServer := Form1.FHttpClientProxyServer;
      ProxyParams.ProxyPort := Form1.FHttpClientProxyPort;
      ProxyParams.ProxyUserName := Form1.FHttpClientProxyUserName;
      ProxyParams.ProxyPassword := Form1.FHttpClientProxyPassword;
      ProxyParams.ProxyBypass := Form1.FHttpClientProxyBypass;
      AccessType := Form1.FHttpClientAccessType;
      HttpOptions := Form1.FHttpClientHttpOptions;
      RequestHeaders.RawHeaderText := Form1.FHttpClientRawHeaderText;
    end;

    for var I := 1 to FMaxHttpRequest do begin
      try

        if FUrls.Count = 0 then break;
        LUrl := FUrls[AlRandom32(FUrls.Count)];
        FBytesSent := 0;
        FBytesRead := 0;
        FDNSStopWatch := TStopWatch.Create;
        FConnectStopWatch := TStopWatch.Create;
        FSendStopWatch := TStopWatch.Create;
        FWaitStopWatch := TStopWatch.Create;
        FReceiveStopWatch := TStopWatch.Create;
        var LHttpclientResponse: TALHTTPClientResponse;
        if FBodyStream.Size > 0 then begin
          FBytesSent := FBodyStream.Size;
          LHttpclientResponse := LHttpClient.Post(LUrl, FBodyStream)
        end
        else LHttpclientResponse := LHttpClient.Get(LUrl);
        try
          LStatusCode := LHttpclientResponse.StatusCode;
          if (LStatusCode >= 400) and FStopOnError then break;

          inc(LRequestCount);
          LTotalBytesSent := LTotalBytesSent + FBytesSent;
          LTotalBytesRead := LTotalBytesRead + FBytesRead;
          if FDNSStopWatch.Elapsed.TotalMilliseconds > 0 then begin
            inc(LDNSCount);
            LDNSTimeTaken := LDNSTimeTaken + FDNSStopWatch.Elapsed.TotalMilliseconds;
          end;
          if FConnectStopWatch.Elapsed.TotalMilliseconds > 0 then begin
            inc(LConnectCount);
            LConnectTimeTaken := LConnectTimeTaken + FConnectStopWatch.Elapsed.TotalMilliseconds;
          end;
          if FSendStopWatch.Elapsed.TotalMilliseconds > 0 then begin
            inc(LSendCount);
            LSendTimeTaken := LSendTimeTaken + FSendStopWatch.Elapsed.TotalMilliseconds;
          end;
          if FWaitStopWatch.Elapsed.TotalMilliseconds > 0 then begin
            inc(LWaitCount);
            LWaitTimeTaken := LWaitTimeTaken + FWaitStopWatch.Elapsed.TotalMilliseconds;
          end;
          if FReceiveStopWatch.Elapsed.TotalMilliseconds > 0 then begin
            inc(LReceiveCount);
            LReceiveTimeTaken := LReceiveTimeTaken + FReceiveStopWatch.Elapsed.TotalMilliseconds;
          end;

          If Form1.ButtonStart.tag = 0 then Break;

          Form1.FHttpRequestMetrics[FIndex].RequestCount := LRequestCount;
          Form1.FHttpRequestMetrics[FIndex].TotalBytesSent := LTotalBytesSent;
          Form1.FHttpRequestMetrics[FIndex].TotalBytesRead := LTotalBytesRead;
          Form1.FHttpRequestMetrics[FIndex].AverageDNSTimeTaken := LDNSTimeTaken / Max(1, LDNSCount);
          Form1.FHttpRequestMetrics[FIndex].AverageConnectTimeTaken := LConnectTimeTaken / Max(1, LConnectCount);
          Form1.FHttpRequestMetrics[FIndex].TotalSendTimeTaken := LSendTimeTaken;
          Form1.FHttpRequestMetrics[FIndex].AverageSendTimeTaken := LSendTimeTaken / Max(1, LSendCount);
          Form1.FHttpRequestMetrics[FIndex].AverageWaitTimeTaken := LWaitTimeTaken / Max(1, LWaitCount);
          Form1.FHttpRequestMetrics[FIndex].totalReceiveTimeTaken := LReceiveTimeTaken;
          Form1.FHttpRequestMetrics[FIndex].AverageReceiveTimeTaken := LReceiveTimeTaken / Max(1, LReceiveCount);

        finally
          AlFreeAndNil(LHttpclientResponse);
        end;
        if FDelayBetweenEachCall > 0 then sleep(FDelayBetweenEachCall);
      Except
        on e: Exception do begin
          LStatusCode := 0;
          if FStopOnError then Break;
        end;
      end;
    end;

    TThread.Synchronize(nil,
    procedure
    begin
      Form1.FHttpRequestMetrics[FIndex].&On := False;
      Form1.FHttpRequestMetrics[FIndex].Url := LUrl;
      Form1.FHttpRequestMetrics[FIndex].RequestCount := LRequestCount;
      Form1.FHttpRequestMetrics[FIndex].StatusCode := LStatusCode;
      Form1.FHttpRequestMetrics[FIndex].TotalBytesSent := LTotalBytesSent;
      Form1.FHttpRequestMetrics[FIndex].TotalBytesRead := LTotalBytesRead;
      Form1.FHttpRequestMetrics[FIndex].AverageDNSTimeTaken := LDNSTimeTaken / Max(1, LDNSCount);
      Form1.FHttpRequestMetrics[FIndex].AverageConnectTimeTaken := LConnectTimeTaken / Max(1, LConnectCount);
      Form1.FHttpRequestMetrics[FIndex].TotalSendTimeTaken := LSendTimeTaken;
      Form1.FHttpRequestMetrics[FIndex].AverageSendTimeTaken := LSendTimeTaken / Max(1, LSendCount);
      Form1.FHttpRequestMetrics[FIndex].AverageWaitTimeTaken := LWaitTimeTaken / Max(1, LWaitCount);
      Form1.FHttpRequestMetrics[FIndex].totalReceiveTimeTaken := LReceiveTimeTaken;
      Form1.FHttpRequestMetrics[FIndex].AverageReceiveTimeTaken := LReceiveTimeTaken / Max(1, LReceiveCount);
    end);

  finally
    ALFreeAndNil(LHttpClient);
  end;

end;

{********************************************************************************************}
procedure TWorkerThread.OnHttpDownloadProgress(sender: Tobject; Read: Int64; Total: Int64);
begin
  FBytesRead := Read;
  if FSimulateSlowClient > 0 then begin
    // How long we *should* have spent to read `Read` bytes at TARGET_BPS
    var LTargetS := Read / (FSimulateSlowClient * 1000);

    // How long we've actually spent so far
    var LActualS := FReceiveStopWatch.Elapsed.TotalSeconds;

    // If we're ahead of schedule, pause to let time catch up
    if LTargetS > LActualS then begin
      var LBehindMs := (LTargetS - LActualS) * 1000.0;

      // Avoid very long sleeps in a single callback; let subsequent callbacks refine it
      if LBehindMs > 250 then LBehindMs := 250;

      var LSleepMs := Ceil(LBehindMs);
      if LSleepMs > 0 then Sleep(LSleepMs);
    end;
  end;
end;

{********************************************************************************************}
procedure TWorkerThread.OnHttpUploadProgress(Sender: Tobject; Sent: Int64; Total: Int64);
begin
  FBytesSent := Sent;
  if FSimulateSlowClient > 0 then begin
    // How long we *should* have spent to send `Sent` bytes at TARGET_BPS
    var LTargetS := Sent / (FSimulateSlowClient * 1000);

    // How long we've actually spent so far
    var LActualS := FSendStopWatch.Elapsed.TotalSeconds;

    // If we're ahead of schedule, pause to let time catch up
    if LTargetS > LActualS then begin
      var LBehindMs := (LTargetS - LActualS) * 1000.0;

      // Avoid very long sleeps in a single callback; let subsequent callbacks refine it
      if LBehindMs > 250 then LBehindMs := 250;

      var LSleepMs := Ceil(LBehindMs);
      if LSleepMs > 0 then Sleep(LSleepMs);
    end;
  end;
end;

{***************************************}
procedure TWorkerThread.OnHttpStatus(
            sender: Tobject;
            InternetStatus: DWord;
            StatusInformation: Pointer;
            StatusInformationLength: DWord);
begin
       if InternetStatus = WINHTTP_CALLBACK_STATUS_RESOLVING_NAME then FDNSStopWatch.Start
  else if InternetStatus = WINHTTP_CALLBACK_STATUS_NAME_RESOLVED then FDNSStopWatch.Stop

  else if InternetStatus = WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER then FConnectStopWatch.Start
  else if InternetStatus = WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER then FConnectStopWatch.Stop

  else if InternetStatus = WINHTTP_CALLBACK_STATUS_SENDING_REQUEST then FSendStopWatch.Start
  else if InternetStatus = WINHTTP_CALLBACK_STATUS_REQUEST_SENT then FSendStopWatch.Stop

  else if InternetStatus = WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE then FWaitStopWatch.Start
  else if InternetStatus = WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED then begin
    FWaitStopWatch.Stop;
    FReceiveStopWatch.Start;
  end

  else if InternetStatus = WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING then FReceiveStopWatch.Stop;
end;

{*******************************************}
initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
