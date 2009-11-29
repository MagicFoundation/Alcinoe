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
     AlStringList,
     AlWinHttpWrapper,
     cxStyles,
     cxCustomData,
     cxGraphics,
     cxFilter,
     cxData,
     cxDataStorage,
     cxEdit,
     cxDropDownEdit,
     cxImageComboBox,
     cxSpinEdit,
     cxGridLevel,
     cxGridCustomTableView,
     cxGridTableView,
     cxClasses,
     cxControls,
     cxGridCustomView,
     cxGrid, Spin;

type
  TForm1 = class(TForm)
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
    RadioButtonAccessType_NAMED_PROXY: TRadioButton;
    RadioButtonAccessType_NO_PROXY: TRadioButton;
    RadioButtonAccessType_DEFAULT_PROXY: TRadioButton;
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
    CheckBoxInternetOption_BYPASS_PROXY_CACHE: TCheckBox;
    CheckBoxInternetOption_ESCAPE_DISABLE: TCheckBox;
    CheckBoxInternetOption_REFRESH: TCheckBox;
    CheckBoxInternetOption_SECURE: TCheckBox;
    CheckBoxInternetOption_ESCAPE_PERCENT: TCheckBox;
    CheckBoxInternetOption_NULL_CODEPAGE: TCheckBox;
    CheckBoxInternetOption_ESCAPE_DISABLE_QUERY: TCheckBox;
    GroupBox8: TGroupBox;
    MemoRequestRawHeader: TMemo;
    Label8: TLabel;
    RadioButtonProtocolVersion1_0: TRadioButton;
    RadioButtonProtocolVersion1_1: TRadioButton;
    CheckBoxInternetOption_KEEP_CONNECTION: TCheckBox;
    CheckBoxInternetOption_NO_COOKIES: TCheckBox;
    CheckBoxInternetOption_NO_AUTO_REDIRECT: TCheckBox;
    ButtonStart: TButton;
    Label4: TLabel;
    MemoLstUrl: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    GridThread: TcxGrid;
    TableViewThread: TcxGridTableView;
    TableViewThreadNumber: TcxGridColumn;
    TableViewThreadUrl: TcxGridColumn;
    TableViewThreadHttpStatus: TcxGridColumn;
    TableViewThreadBytesReceived: TcxGridColumn;
    TableViewThreadDownloadSpeed: TcxGridColumn;
    levelThread: TcxGridLevel;
    TableViewThreadRequestCount: TcxGridColumn;
    CheckBoxDoLikeSpider: TCheckBox;
    CheckBoxStopOnError: TCheckBox;
    StatusBar1: TStatusBar;
    EditMaxHttpRequest: TSpinEdit;
    EditNbThread: TSpinEdit;
    procedure ButtonStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure initWinHTTP(aHttpClient: TAlWinHttpClient);
  public
    NBActiveThread: Integer;
    StartTime: DWOrd;
    ToTalBytesReceived: Integer;
  end;

  TStressHttpThread = Class(Tthread)
  private
    fOn: Boolean;
    FDownloadSpeedStartTime: DWord;
    FBytesRead: Integer;
    fUrl: String;
    fRequestCount: Integer;
    FRequestStatus: String;
    Procedure UpdateGUI;
  protected
    StopOnError: Boolean;
    DoLikeaSpider: Boolean;
    HttpClient: TalWinHttpClient;
    Owner: TWinControl;
    LstUrl: TAlAVLstringList;
    MaxHttpRequest: Integer;
    Rank: integer;
    procedure Execute; override;
    procedure OnHttpDownloadProgress(sender: Tobject; Read: Integer; Total: Integer);
  Public
    constructor Create(CreateSuspended: Boolean; AOwner: TwinControl; aRank: integer);
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
     AlfcnHtml,
     AlFcnMime,
     AlFcnString,
     AlHttpCommon;

{$R *.dfm}

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  NBActiveThread := 0;
end;

{**********************************************************}
procedure TForm1.initWinHTTP(aHttpClient: TAlWinHttpClient);
Begin
  With aHTTPClient do begin
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

    RequestHeader.RawHeaderText := MemoRequestRawHeader.Text;
  end;
end;

{*************************************************}
procedure TForm1.ButtonStartClick(Sender: TObject);
Var i: integer;
    aStressHttpThread: TStressHttpThread;
begin
  {init button action}
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
  StartTime := GetTickCount;
  ToTalBytesReceived := 0;

  for i := 1 to strtoint(EditNbThread.Text) do begin
    TableViewThread.DataController.SetValue(i-1,TableViewThreadNumber.Index,inttostr(i) + ' (on)');
    aStressHttpThread := TStressHttpThread.Create(True, self, i);
    initWinHTTP(aStressHttpThread.HttpClient);
    aStressHttpThread.LstUrl.Assign(MemoLstUrl.Lines);
    aStressHttpThread.lstUrl.NameValueSeparator := #1;
    aStressHttpThread.MaxHttpRequest := strtoint(EditMaxHttpRequest.Text);
    aStressHttpThread.FreeOnTerminate := True;
    aStressHttpThread.DoLikeaSpider := CheckBoxDoLikeSpider.Checked;
    aStressHttpThread.StopOnError := CheckBoxStopOnError.Checked;
    aStressHttpThread.Resume;
    inc(NBActiveThread);
    StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(NBActiveThread);
    StatusBar1.Repaint;
  end;
end;

///////////////////////////////////////
////////// TStressHttpThread //////////
///////////////////////////////////////

{**************************************************************************************************}
constructor TStressHttpThread.Create(CreateSuspended: Boolean; AOwner: TWinControl; aRank: integer);
begin
  inherited Create(CreateSuspended);
  fOn := True;
  doLikeASpider := False;
  Owner := AOwner;
  HttpClient := TaLWinHttpClient.Create(nil);
  with HttpClient do begin
    AccessType := wHttpAt_NO_PROXY;
    InternetOptions := [];
    OnDownloadProgress := OnHttpDownloadProgress;
  end;
  LstUrl := TALAVLstringList.Create;
  lstUrl.NameValueSeparator := #1;
  FDownloadSpeedStartTime:= 0;
  FBytesRead := 0;
  MaxHttpRequest := 0;
  Rank := aRank;
  fUrl := '';
  fRequestCount := 0;
  FRequestStatus := '';
end;

{***********************************}
destructor TStressHttpThread.Destroy;
begin
  HttpClient.Free;
  fOn := False;
  Synchronize(UpdateGUI);
  LstUrl.free;
  inherited;
end;

{**********************************}
procedure TStressHttpThread.Execute;
Var i: integer;
    atmpUrl: string;
    aBody: String;
    aHostName: String;
    aLowerCaseBody: String;
    P1, P2: integer;
    aResponseContentStream: TStream;
    aResponseContentHeader: TALHTTPResponseHeader;
begin
  for I := 1 to MaxHttpRequest do begin
    if LstUrl.Count = 0 then break;
    fUrl := LstUrl[random(LstUrl.Count)];
    inc(FRequestCount);
    FDownloadSpeedStartTime := GetTickCount;
    FBytesRead := 0;
    try

      aResponseContentStream:= TStringStream.create('');
      aResponseContentHeader := TALHTTPResponseHeader.Create;
      try
        HttpClient.Get(fUrl, aResponseContentStream, aResponseContentHeader);
        aBody := TStringStream(aResponseContentStream).datastring;
        FRequestStatus := aResponseContentHeader.StatusCode;
      finally
        aResponseContentStream.free;
        aResponseContentHeader.free;
      end;

      if dolikeaspider then begin
        aLowerCaseBody := AlLowerCase(aBody);
        aHostName := 'http://' + AlLowerCase(AlExtractHostNameFromUrl(fUrl));

        P1 := Alpos('href=''http://',aLowerCaseBody);
        while P1 > 0 do begin
          inc(p1,5);
          P2 := AlPosEx('''',aLowerCaseBody, P1 + 1);
          if P2 > P1 then atmpurl := ALUTF8HTMLDecode(AlCopyStr(aBody, P1+1, P2 - P1 - 1))
          else break;
          if (AlPos(aHostName, alLowerCase(atmpUrl))=1) and (LstUrl.IndexOf(atmpUrl) < 0) then LstUrl.Add(atmpUrl);
          P1 := AlposEx('href=''http://',aLowerCaseBody, P2+ 1);
        end;

        P1 := Alpos('href="http://',aLowerCaseBody);
        while P1 > 0 do begin
          inc(p1,5);
          P2 := AlPosEx('"',aLowerCaseBody, P1 + 1);
          if P2 > P1 then atmpurl := ALUTF8HTMLDecode(AlCopyStr(aBody, P1+1, P2 - P1 - 1))
          else break;
          if (AlPos(aHostName, alLowerCase(atmpUrl))=1) and (LstUrl.IndexOf(atmpUrl) < 0) then LstUrl.Add(atmpUrl);
          P1 := AlposEx('href="http://',aLowerCaseBody, P2+ 1);
        end;

        P1 := Alpos('href=''/',aLowerCaseBody);
        while P1 > 0 do begin
          inc(p1,5);
          P2 := AlPosEx('''',aLowerCaseBody, P1 + 1);
          if P2 > P1+2 then atmpurl := aHostName + ALUTF8HTMLDecode(AlCopyStr(aBody, P1+1, P2 - P1 - 1))
          else break;
          if (AlPos(aHostName, alLowerCase(atmpUrl))=1) and (LstUrl.IndexOf(atmpUrl) < 0) then LstUrl.Add(atmpUrl);
          P1 := AlposEx('href=''/',aLowerCaseBody, P2+ 1);
        end;

        P1 := Alpos('href="/',aLowerCaseBody);
        while P1 > 0 do begin
          inc(p1,5);
          P2 := AlPosEx('"',aLowerCaseBody, P1 + 1);
          if P2 > P1+2 then atmpurl := aHostName + ALUTF8HTMLDecode(AlCopyStr(aBody, P1+1, P2 - P1 - 1))
          else break;
          if (AlPos(aHostName, alLowerCase(atmpUrl))=1) and (LstUrl.IndexOf(atmpUrl) < 0) then LstUrl.Add(atmpUrl);
          P1 := AlposEx('href="/',aLowerCaseBody, P2+ 1);
        end;

      end;
    Except
      on e: Exception do begin
        FRequestStatus := E.message;
        Synchronize(UpdateGUI);
        if StopOnError then Exit;
      end;
    end;
    Synchronize(UpdateGUI);
    If Tform1(Owner).ButtonStart.tag = 0 then Break;
  end;
end;

{****************************************************************************************}
procedure TStressHttpThread.OnHttpDownloadProgress(sender: Tobject; Read, Total: Integer);
begin
  FBytesRead := Read;
end;

{************************************}
procedure TStressHttpThread.UpdateGUI;
Var timeElapsed: DWord;
    RequestCount: Integer;
    i: integer;
begin
  TForm1(Owner).TableViewThread.BeginUpdate;
  try
    TForm1(Owner).ToTalBytesReceived := TForm1(Owner).ToTalBytesReceived + FBytesRead;
    if not fOn then begin
      dec(TForm1(Owner).NBActiveThread);
      TForm1(Owner).StatusBar1.Panels[0].Text := '# Threads: ' + inttostr(TForm1(Owner).NBActiveThread);
      TForm1(Owner).TableViewThread.DataController.SetValue(rank-1,TForm1(Owner).TableViewThreadNumber.Index,inttostr(rank) + ' (off)');
      if TForm1(Owner).NBActiveThread = 0 then begin
        timeElapsed := GetTickCount - TForm1(Owner).StartTime;
        RequestCount := 0;
        for I := 0 to TForm1(Owner).TableViewThread.DataController.RecordCount - 1 do
          RequestCount := RequestCount + TForm1(Owner).TableViewThread.DataController.GetValue(I,Tform1(Owner).TableViewThreadRequestCount.Index);
        TForm1(Owner).StatusBar1.Panels[1].Text := inttostr(RequestCount) + ' Requests in ' + inttostr(round(timeElapsed / 1000)) + ' seconds (' + FormatFloat('0.##',RequestCount / (timeElapsed / 1000)) + ' Request/seconds | '+FormatFloat('0.##',(TForm1(Owner).ToTalBytesReceived / 1000) / (timeElapsed / 1000))+' KB/seconds)';
        TForm1(Owner).ButtonStart.Tag := 0;
        TForm1(Owner).ButtonStart.Caption := 'Start';
      end;
    end;
    TForm1(Owner).TableViewThread.DataController.SetValue(Rank-1,Tform1(Owner).TableViewThreadUrl.Index,fUrl);
    TForm1(Owner).TableViewThread.DataController.SetValue(Rank-1,Tform1(Owner).TableViewThreadRequestCount.Index,FRequestCount);
    TForm1(Owner).TableViewThread.DataController.SetValue(Rank-1,Tform1(Owner).TableViewThreadHttpStatus.Index,FRequestStatus);
    TForm1(Owner).TableViewThread.DataController.SetValue(Rank-1,Tform1(Owner).TableViewThreadBytesReceived.Index,FBytesRead);
    TForm1(Owner).TableViewThread.DataController.SetValue(Rank-1,Tform1(Owner).TableViewThreadDownloadSpeed.Index,Inttostr(Round((FBytesRead / 1000) /  ((max(GetTickCount - FDownloadSpeedStartTime,1) / 1000)))) +' KB/s');
  finally
    TForm1(Owner).TableViewThread.EndUpdate;
  end;
end;

{$IFDEF DEBUG}
initialization
  ReportMemoryleaksOnSHutdown := True;
{$ENDIF}

end.
