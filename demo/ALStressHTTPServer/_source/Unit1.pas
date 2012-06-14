unit Unit1;

interface

uses Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
     Dialogs, StdCtrls, shellapi, ExtCtrls, ComCtrls, AlHttpCommon, AlWinHttpClient,
     AlStringList, AlWinHttpWrapper, cxStyles, cxCustomData, cxGraphics, cxFilter,
     cxData, cxDataStorage, cxEdit, cxDropDownEdit, cxImageComboBox, cxSpinEdit,
     cxGridLevel, cxGridCustomTableView, cxGridTableView, cxClasses, cxControls,
     cxGridCustomView, cxGrid, Spin, OleCtrls, SHDocVw,
     ComObj, cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, dxSkinBlack,
     dxSkinBlue, dxSkinBlueprint, dxSkinCaramel, dxSkinCoffee, dxSkinDarkRoom,
     dxSkinDarkSide, dxSkinDevExpressDarkStyle, dxSkinDevExpressStyle, dxSkinFoggy,
     dxSkinGlassOceans, dxSkinHighContrast, dxSkiniMaginary, dxSkinLilian,
     dxSkinLiquidSky, dxSkinLondonLiquidSky, dxSkinMcSkin, dxSkinMoneyTwins,
     dxSkinOffice2007Black, dxSkinOffice2007Blue, dxSkinOffice2007Green,
     dxSkinOffice2007Pink, dxSkinOffice2007Silver, dxSkinOffice2010Black,
     dxSkinOffice2010Blue, dxSkinOffice2010Silver, dxSkinPumpkin, dxSkinSeven,
     dxSkinSevenClassic, dxSkinSharp, dxSkinSharpPlus, dxSkinSilver,
     dxSkinSpringTime, dxSkinStardust, dxSkinSummer2008, dxSkinTheAsphaltWorld,
     dxSkinsDefaultPainters, dxSkinValentine, dxSkinVS2010, dxSkinWhiteprint,
     dxSkinXmas2008Blue, dxSkinscxPCPainter, cxButtons, cxContainer,
     cxTextEdit, cxMemo, cxLabel, cxGroupBox, cxCheckBox, cxPCdxBarPopupMenu, cxPC,
     cxRadioGroup, cxMaskEdit, dxSkinsForm, Menus;

Const
  WM_UpdateGUI = WM_User + 1;

type
  TForm1 = class(TForm)
    PageControl1: TcxPageControl;
    TabSheet1: TcxTabSheet;
    TabSheet2: TcxTabSheet;
    GroupBox3: TcxGroupBox;
    Label18: TcxLabel;
    Label19: TcxLabel;
    EditUserName: tcxtextedit;
    EditPassword: tcxtextedit;
    GroupBox4: TcxGroupBox;
    Label14: TcxLabel;
    Label17: TcxLabel;
    Label20: TcxLabel;
    EditSendTimeout: tcxtextedit;
    EditReceiveTimeout: tcxtextedit;
    EditConnectTimeout: tcxtextedit;
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
    EdProxyPort: tcxtextedit;
    EdProxyUserName: tcxtextedit;
    EdProxyServer: tcxtextedit;
    EdProxyPassword: tcxtextedit;
    EdProxyBypass: tcxtextedit;
    GroupBox5: TcxGroupBox;
    Label24: TcxLabel;
    EditBufferUploadSize: tcxtextedit;
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
    CheckBoxInternetOption_KEEP_CONNECTION: TcxCheckBox;
    CheckBoxInternetOption_NO_COOKIES: TcxCheckBox;
    CheckBoxInternetOption_NO_AUTO_REDIRECT: TcxCheckBox;
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
    CheckBoxDoLikeSpider: TcxCheckBox;
    CheckBoxStopOnError: TcxCheckBox;
    StatusBar1: TStatusBar;
    EditMaxHttpRequest: TcxSpinEdit;
    EditNbThread: TcxSpinEdit;
    Label3: TcxLabel;
    EditSendDelayBetweenEachSend: tcxtextedit;
    Label5: TcxLabel;
    Panel2: TPanel;
    Label7: TcxLabel;
    Label6: TcxLabel;
    Panel3: TPanel;
    PanelWebBrowser: TPanel;
    TableViewThreadDNS: TcxGridColumn;
    TableViewThreadConnect: TcxGridColumn;
    TableViewThreadReceive: TcxGridColumn;
    TableViewThreadSend: TcxGridColumn;
    TableViewThreadWait: TcxGridColumn;
    ButtonStart: TcxButton;
    dxSkinController1: TdxSkinController;
    cxStyleRepository1: TcxStyleRepository;
    cxStyle1: TcxStyle;
    procedure ButtonStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TableViewThreadDownloadSpeedGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: String);
    procedure TableViewThreadTimeTakenGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: String);
    procedure TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems1GetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
    procedure TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems2GetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
    procedure TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems6GetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
    procedure TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems4GetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
    procedure TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems5GetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
    procedure TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems3GetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
    procedure TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems0GetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
  private
    procedure WMUpdateGUI(var Msg: TMessage); message WM_UpdateGUI;
    procedure initHttpClientParams;
  public
    HttpClientUserName: AnsiString;
    HttpClientPassword: AnsiString;
    HttpClientConnectTimeout: integer;
    HttpClientSendTimeout: integer;
    HttpClientReceiveTimeout: integer;
    HttpClientProtocolVersion: TALHTTPProtocolVersion;
    HttpClientUploadBufferSize: integer;
    HttpClientProxyServer: AnsiString;
    HttpClientProxyPort: integer;
    HttpClientProxyUserName: AnsiString;
    HttpClientProxyPassword: AnsiString;
    HttpClientProxyBypass: AnsiString;
    HttpClientAccessType: TALWinHttpClientInternetOpenAccessType;
    HttpClientInternetOptions: TALWinHttpClientInternetOptionSet;
    HttpClientRawHeaderText: ansiString;
    NBActiveThread: Integer;
    LastUpdateStatusBar: int64;
    StartTime: int64;
    ToTalBytesRead: Int64;
    TotalDNScount: integer;
    TotalDNSTimeTaken: Int64;
    TotalConnectCount: integer;
    TotalConnectTimeTaken: Int64;
    TotalSendCount: integer;
    TotalSendTimeTaken: Int64;
    TotalWaitCount: integer;
    TotalWaitTimeTaken: Int64;
    TotalReceiveCount: int64;
    TotalReceiveTimeTaken: int64;
  end;

  TStressHttpThreadVarContainer = Class(TObject)
    On: Boolean;
    Rank: integer;
    Url: AnsiString;
    RequestCount: Integer;
    RequestStatus: AnsiString;
    BytesRead: Integer;
    DNSTimeTaken: Integer;
    ConnectTimeTaken: Integer;
    SendTimeTaken: Integer;
    WaitTimeTaken: Integer;
    ReceiveTimeTaken: Integer;
  End;

  TStressHttpThread = Class(Tthread)
  private
    FBytesRead: Integer;
    fUrl: AnsiString;
    fRequestCount: Integer;
    FRequestStatus: AnsiString;
    FDNSTimeTaken: Integer;
    FConnectTimeTaken: Integer;
    FSendTimeTaken: Integer;
    FWaitTimeTaken: Integer;
    FReceiveTimeTaken: Integer;
    FHttpStatusStartTime: int64;
  protected
    StopOnError: Boolean;
    DoLikeaSpider: Boolean;
    DelayBetweenEachCall: integer;
    LstUrl: TAlAVLstringList;
    MaxHttpRequest: Integer;
    Rank: integer;
    procedure Execute; override;
    procedure OnHttpDownloadProgress(sender: Tobject; Read: Integer; Total: Integer);
    procedure OnHttpStatusChange(sender: Tobject;
                                 InternetStatus: DWord;
                                 StatusInformation: Pointer;
                                 StatusInformationLength: DWord);
  Public
    constructor Create(CreateSuspended: Boolean; aRank: integer);
    destructor Destroy; override;
  End;


var
  Form1: TForm1;

implementation

Uses Math,
     DateUtils,
     ALMultiPartFormDataParser,
     AlFcnFile,
     AlFcnMisc,
     ALWindows,
     AlfcnHtml,
     AlFcnMime,
     AlFcnString;

{$R *.dfm}

{************************************}
procedure TForm1.initHttpClientParams;
Begin
  HttpClientUserName := EditUserName.Text;
  HttpClientPassword := EditPassword.Text;

  HttpClientConnectTimeout := strtoint(EditConnectTimeout.Text);
  HttpClientSendTimeout := strtoint(EditSendTimeout.Text);
  HttpClientReceiveTimeout := strtoint(EditReceiveTimeout.Text);

  if RadioButtonProtocolVersion1_0.Checked then HttpClientProtocolVersion := HTTPpv_1_0
  else HttpClientProtocolVersion := HTTPpv_1_1;

  HttpClientUploadBufferSize := strtoint(EditBufferUploadSize.Text);

  HttpClientProxyServer := EdProxyServer.Text;
  HttpClientProxyPort := strToInt(EdProxyPort.Text);
  HttpClientProxyUserName := EdProxyUserName.Text;
  HttpClientProxyPassword := EdProxyPassword.Text;
  HttpClientProxyBypass := EdProxyBypass.Text;

  if RadioButtonAccessType_NO_PROXY.Checked then HttpClientAccessType := wHttpAt_NO_PROXY
  else if RadioButtonAccessType_NAMED_PROXY.Checked then HttpClientAccessType := wHttpAt_NAMED_PROXY
  else if RadioButtonAccessType_DEFAULT_PROXY.Checked then HttpClientAccessType := wHttpAt_DEFAULT_PROXY;

  HttpClientInternetOptions := [];
  If CheckBoxInternetOption_BYPASS_PROXY_CACHE.checked then HttpClientInternetOptions := HttpClientInternetOptions + [wHttpIo_BYPASS_PROXY_CACHE];
  If CheckBoxInternetOption_ESCAPE_DISABLE.checked then HttpClientInternetOptions := HttpClientInternetOptions + [wHttpIo_ESCAPE_DISABLE];
  If CheckBoxInternetOption_ESCAPE_DISABLE_QUERY.checked then HttpClientInternetOptions := HttpClientInternetOptions + [wHttpIo_ESCAPE_DISABLE_QUERY];
  If CheckBoxInternetOption_ESCAPE_PERCENT.checked then HttpClientInternetOptions := HttpClientInternetOptions + [wHttpIo_ESCAPE_PERCENT];
  If CheckBoxInternetOption_NULL_CODEPAGE.checked then HttpClientInternetOptions := HttpClientInternetOptions + [wHttpIo_NULL_CODEPAGE];
  If CheckBoxInternetOption_REFRESH.checked then HttpClientInternetOptions := HttpClientInternetOptions + [wHttpIo_REFRESH];
  If CheckBoxInternetOption_SECURE.checked then HttpClientInternetOptions := HttpClientInternetOptions + [wHttpIo_SECURE];
  If CheckBoxInternetOption_NO_COOKIES.checked then HttpClientInternetOptions := HttpClientInternetOptions + [wHttpIo_NO_COOKIES];
  If CheckBoxInternetOption_KEEP_CONNECTION.checked then HttpClientInternetOptions := HttpClientInternetOptions + [wHttpIo_KEEP_CONNECTION];
  If CheckBoxInternetOption_NO_AUTO_REDIRECT.checked then HttpClientInternetOptions := HttpClientInternetOptions + [wHttpIo_NO_AUTO_REDIRECT];

  HttpClientRawHeaderText := MemoRequestRawHeader.Text;
end;

{*******************************************************************************************************************************************}
procedure TForm1.TableViewThreadDownloadSpeedGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: String);
begin
  if AText <> '' then AText := AText + ' KB/s';
end;

{***************************************************************************************************************************************}
procedure TForm1.TableViewThreadTimeTakenGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: String);
begin
  if AText <> '' then AText := AText + ' ms';
end;

{**************************************************************************************************************************************************************************************}
procedure TForm1.TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems0GetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
begin
  if ToTalBytesRead > 107374182400 then aText := inttostr(round(ToTalBytesRead / 107374182400)) + ' GB'
  else if ToTalBytesRead > 1048576 then aText := inttostr(round(ToTalBytesRead / 1048576)) + ' MB'
  else if ToTalBytesRead > 1024 then aText := inttostr(round(ToTalBytesRead / 1024)) + ' KB';
end;

{**************************************************************************************************************************************************************************************}
procedure TForm1.TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems1GetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
begin
  if (TotalReceiveCount > 0) and (TotalReceiveTimeTaken > 0) then AText := inttostr(Round((totalBytesRead / 1024) /  ((TotalReceiveTimeTaken / 1000)))) + ' KB/s'
  else aText := '';
end;

{**************************************************************************************************************************************************************************************}
procedure TForm1.TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems2GetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
begin
  if TotalDNSCount > 0 then AText := inttostr(Round(TotalDNSTimeTaken / TotalDNSCount)) + ' ms'
  else AText := '';
end;

{**************************************************************************************************************************************************************************************}
procedure TForm1.TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems3GetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
begin
  if TotalReceiveCount > 0 then AText := inttostr(Round(TotalReceiveTimeTaken / TotalReceiveCount)) + ' ms'
  else AText := '';
end;

{**************************************************************************************************************************************************************************************}
procedure TForm1.TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems4GetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
begin
  if TotalSendCount > 0 then AText := inttostr(Round(TotalSendTimeTaken / TotalSendCount)) + ' ms'
  else AText := '';
end;

{**************************************************************************************************************************************************************************************}
procedure TForm1.TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems5GetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
begin
  if TotalWaitCount > 0 then AText := inttostr(Round(TotalWaitTimeTaken / TotalWaitCount)) + ' ms'
  else AText := '';
end;

{**************************************************************************************************************************************************************************************}
procedure TForm1.TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems6GetText(Sender: TcxDataSummaryItem; const AValue: Variant; AIsFooter: Boolean; var AText: String);
begin
  if TotalConnectCount > 0 then AText := inttostr(Round(TotalConnectTimeTaken / TotalConnectCount)) + ' ms'
  else AText := '';
end;

{**********************************************}
procedure TForm1.WMUpdateGUI(var Msg: TMessage);

  {----------------------------------------------------}
  function internalInttoVariant(aInt: Integer): Variant;
  begin
    if aInt >= 0 then result := aint
    else result := null;
  end;

Var aTimeElapsed: int64;
    aTotalRequestCount: Integer;
    i: integer;

begin
  with TStressHttpThreadVarContainer(pointer(Msg.WParam)) do begin

    TableViewThread.BeginUpdate;
    try

      if RequestCount - TableViewThread.DataController.GetValue(Rank-1,TableViewThreadRequestCount.Index) = 1 then begin

        if DNSTimeTaken >= 0 then begin
          inc(TotalDNSCount);
          TotalDNSTimeTaken := TotalDNSTimeTaken + int64(DNSTimeTaken);
        end;
        if ConnectTimeTaken >= 0 then begin
          inc(TotalConnectCount);
          TotalConnectTimeTaken := TotalConnectTimeTaken + int64(ConnectTimeTaken);
        end;
        if SendTimeTaken >= 0 then begin
          inc(TotalSendCount);
          TotalSendTimeTaken := TotalSendTimeTaken + int64(SendTimeTaken);
        end;
        if WaitTimeTaken >= 0 then begin
          inc(TotalWaitCount);
          TotalWaitTimeTaken := TotalWaitTimeTaken + int64(WaitTimeTaken);
        end;
        if (BytesRead >= 0) and (ReceiveTimeTaken >= 0) then begin
          inc(TotalReceiveCount);
          TotalReceiveTimeTaken := TotalReceiveTimeTaken + int64(ReceiveTimeTaken);
          ToTalBytesRead := ToTalBytesRead + int64(BytesRead);
        end;

      end;

      if not On then begin
        dec(NBActiveThread);
        StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(NBActiveThread);
        TableViewThread.DataController.SetValue(rank-1,TableViewThreadNumber.Index,inttostr(rank) + ' (off)');
        if NBActiveThread = 0 then begin
          ButtonStart.Tag := 0;
          ButtonStart.Caption := 'Start';
        end;
      end;
      TableViewThread.DataController.SetValue(Rank-1,TableViewThreadUrl.Index,Url);
      TableViewThread.DataController.SetValue(Rank-1,TableViewThreadRequestCount.Index,RequestCount);
      TableViewThread.DataController.SetValue(Rank-1,TableViewThreadHttpStatus.Index,RequestStatus);
      TableViewThread.DataController.SetValue(Rank-1,TableViewThreadBytesReceived.Index,internalInttoVariant(BytesRead));
      TableViewThread.DataController.SetValue(Rank-1,TableViewThreadDNS.Index,internalInttoVariant(DNSTimeTaken));
      TableViewThread.DataController.SetValue(Rank-1,TableViewThreadConnect.Index,internalInttoVariant(ConnectTimeTaken));
      TableViewThread.DataController.SetValue(Rank-1,TableViewThreadSend.Index,internalInttoVariant(SendTimeTaken));
      TableViewThread.DataController.SetValue(Rank-1,TableViewThreadWait.Index,internalInttoVariant(WaitTimeTaken));
      TableViewThread.DataController.SetValue(Rank-1,TableViewThreadReceive.Index,internalInttoVariant(ReceiveTimeTaken));
      if (BytesRead > 0) and (ReceiveTimeTaken > 0) then TableViewThread.DataController.SetValue(Rank-1,TableViewThreadDownloadSpeed.Index,Round((BytesRead / 1024) /  (ReceiveTimeTaken / 1000)))
      else  TableViewThread.DataController.SetValue(Rank-1,TableViewThreadDownloadSpeed.Index,null);

      if ALGetTickCount64 - LastUpdateStatusBar > 1000  then begin
        LastUpdateStatusBar := ALGetTickCount64;
        aTimeElapsed := ALGetTickCount64 - StartTime;
        aTotalRequestCount := 0;
        for I := 0 to TableViewThread.DataController.RecordCount - 1 do
          aTotalRequestCount := aTotalRequestCount + TableViewThread.DataController.GetValue(I,TableViewThreadRequestCount.Index);
          StatusBar1.Panels[1].Text := inttostr(aTotalRequestCount) + ' Requests in ' + inttostr(round(aTimeElapsed / 1000)) + ' seconds (' + FormatFloat('0.##',aTotalRequestCount / (Max(aTimeElapsed,1) / 1000)) + ' Request/s | '+
                                       FormatFloat('0.##',((ToTalBytesRead / 1024) / (max(1,aTimeElapsed) / 1000)))+' KB/s)';
      end;

    finally
      TableViewThread.EndUpdate;
    end;

    free;

  end;

end;

{*************************************************}
procedure TForm1.ButtonStartClick(Sender: TObject);
Var i: integer;
    j: integer;
    aStressHttpThread: TStressHttpThread;
begin
  If ButtonStart.tag = 0 then begin
    if NBActiveThread > 0 then begin
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

  TableViewThread.DataController.RecordCount := strtoint(EditNbThread.Text);
  initHttpClientParams;
  NBActiveThread := 0;
  LastUpdateStatusBar := ALGetTickCount64;
  StartTime := ALGetTickCount64;
  ToTalBytesRead := 0;
  TotalDNScount := 0;
  TotalDNSTimeTaken := 0;
  TotalConnectCount := 0;
  TotalConnectTimeTaken := 0;
  TotalSendCount := 0;
  TotalSendTimeTaken := 0;
  TotalWaitCount := 0;
  TotalWaitTimeTaken := 0;
  TotalReceiveCount := 0;
  TotalReceiveTimeTaken := 0;

  StatusBar1.Panels[1].Text := '';
  for i := 1 to strtoint(EditNbThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,inttostr(i) + ' (on)');
    TableViewThread.DataController.SetValue(i-1,TableViewThreadRequestCount.Index,0);
    aStressHttpThread := TStressHttpThread.Create(True, i);
    aStressHttpThread.lstUrl.NameValueSeparator := #1;
    for J := 0 to MemoLstUrl.Lines.Count - 1 do
      if trim(MemoLstUrl.Lines[j]) <> '' then
        aStressHttpThread.LstUrl.Add(MemoLstUrl.Lines[j]);
    aStressHttpThread.MaxHttpRequest := strtoint(EditMaxHttpRequest.Text);
    aStressHttpThread.FreeOnTerminate := True;
    aStressHttpThread.DoLikeaSpider := CheckBoxDoLikeSpider.Checked;
    aStressHttpThread.DelayBetweenEachCall := strtoint(EditSendDelayBetweenEachSend.text);
    aStressHttpThread.StopOnError := CheckBoxStopOnError.Checked;
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(NBActiveThread);
    StatusBar1.Repaint;
    aStressHttpThread.Resume;
  end;
end;

{*****************************************************************************}
constructor TStressHttpThread.Create(CreateSuspended: Boolean; aRank: integer);
begin
  inherited Create(CreateSuspended);
  doLikeASpider := False;
  StopOnError := False;
  DelayBetweenEachCall := 0;
  LstUrl := TALAVLstringList.Create;
  LstUrl.Duplicates := DupIgnore;
  lstUrl.NameValueSeparator := #1;
  MaxHttpRequest := 0;
  Rank := aRank;
  fUrl := '';
  fRequestCount := 0;
  FRequestStatus := '';
  FBytesRead := 0;
  FDNSTimeTaken := 0;
  FConnectTimeTaken := 0;
  FSendTimeTaken := 0;
  FWaitTimeTaken := 0;
  FReceiveTimeTaken := 0;
  FHttpStatusStartTime := AlGetTickCount64;
end;

{***********************************}
destructor TStressHttpThread.Destroy;
var aVarContainer: TStressHttpThreadVarContainer;
begin
  aVarContainer := TStressHttpThreadVarContainer.Create;
  With aVarContainer do begin
    On := False;
    Rank := Self.Rank;
    Url := fUrl;
    RequestCount := fRequestCount;
    RequestStatus := fRequestStatus;
    BytesRead := fBytesRead;
    DNSTimeTaken := fDNSTimeTaken;
    ConnectTimeTaken := fConnectTimeTaken;
    SendTimeTaken := fSendTimeTaken;
    WaitTimeTaken := fWaitTimeTaken;
    ReceiveTimeTaken := fReceiveTimeTaken;
  end;
  PostMessage(Form1.Handle,WM_UpdateGUI, integer(aVarContainer), 0);
  LstUrl.free;
  inherited;
end;

{**********************************}
procedure TStressHttpThread.Execute;

Var atmpUrl: AnsiString;
    aBody: AnsiString;
    aHostName: AnsiString;
    aLowerCaseBody: AnsiString;
    aResponseContentStream: TStream;
    aResponseContentHeader: TALHTTPResponseHeader;
    aHttpClient: TALWinHttpClient;
    aVarContainer: TStressHttpThreadVarContainer;
    P1, P2: integer;
    i: integer;

begin

  for I := 1 to MaxHttpRequest do begin
    if LstUrl.Count = 0 then break;
    fUrl := LstUrl[random(LstUrl.Count)];
    inc(FRequestCount);
    FRequestStatus := '';
    FBytesRead := -1;
    FDNSTimeTaken := -1;
    FConnectTimeTaken := -1;
    FSendTimeTaken := -1;
    FWaitTimeTaken := -1;
    FReceiveTimeTaken := -1;
    FHttpStatusStartTime := AlGetTickCount64;
    try

      aHttpClient := TaLWinHttpClient.Create(nil);
      try

        with aHttpClient do begin
          OnDownloadProgress := OnHttpDownloadProgress;
          OnStatusChange := OnHttpStatusChange;
          UserName := Form1.HttpClientUserName;
          Password := Form1.HttpClientPassword;
          ConnectTimeout := Form1.HttpClientConnectTimeout;
          SendTimeout := Form1.HttpClientSendTimeout;
          ReceiveTimeout := Form1.HttpClientReceiveTimeout;
          ProtocolVersion := Form1.HttpClientProtocolVersion;
          UploadBufferSize := Form1.HttpClientUploadBufferSize;
          ProxyParams.ProxyServer := Form1.HttpClientProxyServer;
          ProxyParams.ProxyPort := Form1.HttpClientProxyPort;
          ProxyParams.ProxyUserName := Form1.HttpClientProxyUserName;
          ProxyParams.ProxyPassword := Form1.HttpClientProxyPassword;
          ProxyParams.ProxyBypass := Form1.HttpClientProxyBypass;
          AccessType := Form1.HttpClientAccessType;
          InternetOptions := Form1.HttpClientInternetOptions;
          RequestHeader.RawHeaderText := Form1.HttpClientRawHeaderText;
        end;

        aResponseContentStream:= TStringStream.create('');
        aResponseContentHeader := TALHTTPResponseHeader.Create;
        try
          aHttpClient.Get(fUrl, aResponseContentStream, aResponseContentHeader);
          aBody := TStringStream(aResponseContentStream).datastring;
          FRequestStatus := aResponseContentHeader.StatusCode;
        finally
          aResponseContentStream.free;
          aResponseContentHeader.free;
        end;

        if DelayBetweenEachCall > 0 then sleep(DelayBetweenEachCall);

        if dolikeaspider then begin
          aLowerCaseBody := AlLowerCase(aBody);
          aHostName := 'http://' + AlLowerCase(AlExtractHostNameFromUrl(fUrl));

          P1 := Alpos('href=''http://',aLowerCaseBody);
          while P1 > 0 do begin
            inc(p1,5);
            P2 := AlPosEx('''',aLowerCaseBody, P1 + 1);
            if P2 > P1 then atmpurl := ALUTF8HTMLDecode(AlCopyStr(aBody, P1+1, P2 - P1 - 1))
            else break;
            if (AlPos(aHostName, alLowerCase(atmpUrl))=1) then LstUrl.Add(atmpUrl);
            P1 := AlposEx('href=''http://',aLowerCaseBody, P2+ 1);
          end;

          P1 := Alpos('href="http://',aLowerCaseBody);
          while P1 > 0 do begin
            inc(p1,5);
            P2 := AlPosEx('"',aLowerCaseBody, P1 + 1);
            if P2 > P1 then atmpurl := ALUTF8HTMLDecode(AlCopyStr(aBody, P1+1, P2 - P1 - 1))
            else break;
            if (AlPos(aHostName, alLowerCase(atmpUrl))=1) then LstUrl.Add(atmpUrl);
            P1 := AlposEx('href="http://',aLowerCaseBody, P2+ 1);
          end;

          P1 := Alpos('href=''/',aLowerCaseBody);
          while P1 > 0 do begin
            inc(p1,5);
            P2 := AlPosEx('''',aLowerCaseBody, P1 + 1);
            if P2 > P1+2 then atmpurl := aHostName + ALUTF8HTMLDecode(AlCopyStr(aBody, P1+1, P2 - P1 - 1))
            else break;
            if (AlPos(aHostName, alLowerCase(atmpUrl))=1) then LstUrl.Add(atmpUrl);
            P1 := AlposEx('href=''/',aLowerCaseBody, P2+ 1);
          end;

          P1 := Alpos('href="/',aLowerCaseBody);
          while P1 > 0 do begin
            inc(p1,5);
            P2 := AlPosEx('"',aLowerCaseBody, P1 + 1);
            if P2 > P1+2 then atmpurl := aHostName + ALUTF8HTMLDecode(AlCopyStr(aBody, P1+1, P2 - P1 - 1))
            else break;
            if (AlPos(aHostName, alLowerCase(atmpUrl))=1) then LstUrl.Add(atmpUrl);
            P1 := AlposEx('href="/',aLowerCaseBody, P2+ 1);
          end;

        end;

      finally
        aHttpClient.free;
      end;

    Except
      on e: Exception do begin
        FRequestStatus := E.message;
        if StopOnError then Exit;
      end;
    end;

    if (fRequestStatus <> '200') then begin
      FBytesRead := -1;
      FDNSTimeTaken := -1;
      FConnectTimeTaken := -1;
      FSendTimeTaken := -1;
      FWaitTimeTaken := -1;
      FReceiveTimeTaken := -1;
    end;

    aVarContainer := TStressHttpThreadVarContainer.Create;
    With aVarContainer do begin
      On := True;
      Rank := Self.Rank;
      Url := fUrl;
      RequestCount := fRequestCount;
      RequestStatus := fRequestStatus;
      BytesRead := fBytesRead;
      DNSTimeTaken := fDNSTimeTaken;
      ConnectTimeTaken := fConnectTimeTaken;
      SendTimeTaken := fSendTimeTaken;
      WaitTimeTaken := fWaitTimeTaken;
      ReceiveTimeTaken := fReceiveTimeTaken;
    end;
    PostMessage(Form1.Handle,WM_UpdateGUI, Integer(aVarContainer), 0);
    If Form1.ButtonStart.tag = 0 then Break;
  end;

end;

{****************************************************************************************}
procedure TStressHttpThread.OnHttpDownloadProgress(sender: Tobject; Read, Total: Integer);
begin
  FBytesRead := Read;
end;

{*************************************************************}
procedure TStressHttpThread.OnHttpStatusChange(sender: Tobject;
                                               InternetStatus: DWord;
                                               StatusInformation: Pointer;
                                               StatusInformationLength: DWord);
begin
  if InternetStatus = WINHTTP_CALLBACK_STATUS_RESOLVING_NAME then FHttpStatusStartTime := ALGetTickCount64
  else if InternetStatus = WINHTTP_CALLBACK_STATUS_NAME_RESOLVED then begin
    FDNSTimeTaken := ALGetTickCount64 - FHttpStatusStartTime;
    FHttpStatusStartTime := ALGetTickCount64;
  end

  else if InternetStatus = WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER then FHttpStatusStartTime := ALGetTickCount64
  else if InternetStatus = WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER then begin
    FConnectTimeTaken := ALGetTickCount64 - FHttpStatusStartTime;
    FHttpStatusStartTime := ALGetTickCount64;
  end

  else if InternetStatus = WINHTTP_CALLBACK_STATUS_SENDING_REQUEST then FHttpStatusStartTime := ALGetTickCount64
  else if InternetStatus = WINHTTP_CALLBACK_STATUS_REQUEST_SENT then begin
    FSendTimeTaken := ALGetTickCount64 - FHttpStatusStartTime;
    FHttpStatusStartTime := ALGetTickCount64;
  end

  else if InternetStatus = WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE then FHttpStatusStartTime := ALGetTickCount64
  else if InternetStatus = WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED then begin
    FWaitTimeTaken := ALGetTickCount64 - FHttpStatusStartTime;
    FHttpStatusStartTime := ALGetTickCount64;
  end

  else if InternetStatus = WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING then FReceiveTimeTaken := ALGetTickCount64 - FHttpStatusStartTime;
end;



{-------------------}
var ie: IWebBrowser2;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
var Url, Flags, TargetFrameName, PostData, Headers: OleVariant;
begin
  NBActiveThread := 0;

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

{$IFDEF DEBUG}
initialization
  ReportMemoryleaksOnSHutdown := True;
{$ENDIF}

end.
