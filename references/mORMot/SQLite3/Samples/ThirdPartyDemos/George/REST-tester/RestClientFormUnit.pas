unit RestClientFormUnit;

// mORMot RESTful API test case 1.02

interface

uses
  // RTL
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$ifdef FPC}
  LCLType,
  {$endif}
  Messages,
  SysUtils,
  Classes,
  Generics.Collections,
  Forms,
  Dialogs,
  Controls,
  StdCtrls,
  ExtCtrls,
  // mORMot
  mORMot,
  SynLog,
  SynCommons,
  mORMotHttpClient, // tmp
  // Custom
  RestClientUnit,
  RestMethodsInterfaceUnit;

type

  TCustomRecord = record helper for rCustomRecord
    procedure FillFromClient();
  end;

  lClientAction = (Auto, Start, Stop, Restart);

  TForm1 = class(TForm)
    EditServerAdress: TEdit;
    EditServerPort: TEdit;
    ButtonStartStop: TButton;
    TimerRefreshLogMemo: TTimer;
    GroupBoxIRestMethods: TGroupBox;
    ButtonMethHelloWorld: TButton;
    ButtonMethSum: TButton;
    ButtonGetCustomRecord: TButton;
    LabelAuthenticationMode: TLabel;
    ComboBoxAuthentication: TComboBox;
    ButtonMethSendCustomRecord: TButton;
    MemoLog: TMemo;
    ButtonCLS: TButton;
    CheckBoxAutoScroll: TCheckBox;
    CheckBoxDisableLog: TCheckBox;
    ButtonMethSendMultipleCustomRecords: TButton;
    LabelProtocol: TLabel;
    ComboBoxProtocol: TComboBox;
    EditUserLogin: TEdit;
    EditUserPassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ButtonMethGetMethodCustomResult: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonStartStopClick(Sender: TObject);
    procedure ButtonCLSClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerRefreshLogMemoTimer(Sender: TObject);
    procedure ButtonMethHelloWorldClick(Sender: TObject);
    procedure ButtonMethSumClick(Sender: TObject);
    procedure ButtonGetCustomRecordClick(Sender: TObject);
    procedure ComboBoxAuthenticationChange(Sender: TObject);
    procedure ButtonMethSendCustomRecordClick(Sender: TObject);
    procedure CheckBoxDisableLogClick(Sender: TObject);
    procedure ButtonMethSendMultipleCustomRecordsClick(Sender: TObject);
    procedure ComboBoxProtocolChange(Sender: TObject);
    procedure ButtonMethGetMethodCustomResultClick(Sender: TObject);
  private
    { Private declarations }
    function LogEvent(Sender: TTextWriter; Level: TSynLogInfo; const Text: RawUTF8): boolean;
    procedure StartStopClient(ClientAction: lClientAction = Auto);
  public
    { Public declarations }
  end;

  TLocalLog = class
    Level: TSynLogInfo;
    Text: RawUTF8;
  end;

var
  Form1: TForm1;
  {$ifdef FPC}
  LogThreadSafeList: TList<TLocalLog>;
  {$else}
  LogThreadSafeList: TThreadList<TLocalLog>;
  {$endif}

implementation

{$ifdef FPC}
{$R *.lfm}
{$else}
{$R *.dfm}
{$endif}
{ TCustomResult }

procedure TCustomRecord.FillFromClient();
var
  i: Integer;
begin
  ResultCode := 200;
  ResultStr := 'Awesome';
  ResultTimeStamp := Now();
  SetLength(ResultArray, 3);
  for i := 0 to 2 do
    ResultArray[i] := 'str_' + i.ToString();
end;

{ Form1 }

// On Form1 create
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create thread safe List with log data class
  {$ifdef FPC}
  LogThreadSafeList := TList<TLocalLog>.Create();
  {$else}
  LogThreadSafeList := TThreadList<TLocalLog>.Create();
  {$endif}
// Enable logging
  with TSQLLog.Family do
    begin
      Level := LOG_VERBOSE;
      EchoCustom := LogEvent;
      NoFile := True;
    end;
end;

// On Form1 destory
procedure TForm1.FormDestroy(Sender: TObject);
var
  i: Integer;
  List: TList<TLocalLog>;
begin
  // Clear and destroy LogThreadSafeList
  {$ifdef FPC}
  List := LogThreadSafeList;
  {$else}
  List := LogThreadSafeList.LockList();
  {$endif}
  for i := 0 to List.Count - 1 do
    List.Items[i].Free;
  List.Clear;
  {$ifndef FPC}
  LogThreadSafeList.UnlockList();
  {$endif}
FreeAndNil(LogThreadSafeList);
end;

{ CLIENT EVENTS, START / STOP }

// Depends on the client status, start or stop client (create or destroy objects)
procedure TForm1.StartStopClient(ClientAction: lClientAction = Auto);
var
  pClientCreated: boolean;
  ClientSettings: rClientSettings;
begin
  pClientCreated := RestClient.Initialized;
  // Unload current client if required
  RestClient.DeInitialize();
  // Create client if required
  if ((ClientAction = lClientAction.Auto) and not pClientCreated) or ((ClientAction = lClientAction.Restart) and pClientCreated) or (ClientAction = lClientAction.Start) then
    begin
      ClientSettings.Protocol := lProtocol(ComboBoxProtocol.ItemIndex);
      ClientSettings.AuthMode := lAuthenticationMode(ComboBoxAuthentication.ItemIndex);;
      ClientSettings.HostOrIP := EditServerAdress.Text;
      ClientSettings.Port := EditServerPort.Text;
      ClientSettings.UserLogin := StringToUTF8(EditUserLogin.Text);
      ClientSettings.UserPassword := StringToUTF8(EditUserPassword.Text);
      RestClient.Initialize(ClientSettings);
    end;
end;

// Processing mORMot log event
function TForm1.LogEvent(Sender: TTextWriter; Level: TSynLogInfo; const Text: RawUTF8): boolean;
var
  List: TList<TLocalLog>;
  LogEventData: TLocalLog;
begin
  Result := False;
  if Assigned(LogThreadSafeList) then
    begin
      {$ifdef FPC}
      List := LogThreadSafeList;
      {$else}
      List := LogThreadSafeList.LockList;
      {$endif}
      try
        LogEventData := TLocalLog.Create();
        LogEventData.Level := Level;
        LogEventData.Text := Text;
        List.Add(LogEventData);
        Result := True;
      finally
        {$ifndef FPC}
        LogThreadSafeList.UnlockList();
        {$endif}
      end;
    end;
end;

{ UI }

// Grabbing new events from thread safe list
procedure TForm1.TimerRefreshLogMemoTimer(Sender: TObject);
var
  List: TList<TLocalLog>;
  i: Integer;
begin
  if Assigned(LogThreadSafeList) then
    begin
      {$ifdef FPC}
      List := LogThreadSafeList;
      {$else}
      List := LogThreadSafeList.LockList;
      {$endif}
      try
        if Assigned(Form1) and not Application.Terminated and (List.Count > 0) then
          begin
            for i := 0 to List.Count - 1 do
              begin
                Form1.MemoLog.Lines.BeginUpdate();
                Form1.MemoLog.Lines.Add(string(List.Items[i].Text));
                Form1.MemoLog.Lines.EndUpdate();
                List.Items[i].Free;
              end;
            List.Clear();
            {$IFDEF MSWINDOWS}
            if CheckBoxAutoScroll.Checked then
              SendMessage(Form1.MemoLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
            {$ENDIF}
          end;
      finally
        {$ifndef FPC}
        LogThreadSafeList.UnlockList();
        {$endif}
      end;
    end;
  if RestClient.Initialized then
    ButtonStartStop.Caption := 'Stop client'
  else
    ButtonStartStop.Caption := 'Start client';
end;

// Changing client protocol
procedure TForm1.ComboBoxProtocolChange(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  EditServerPort.Enabled := lProtocol(ComboBoxProtocol.ItemIndex) <> lProtocol.NamedPipe;
  {$endif}
  StartStopClient(Restart);
end;

// Changing client authentication mode
procedure TForm1.ComboBoxAuthenticationChange(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  if lAuthenticationMode(ComboBoxAuthentication.ItemIndex) = lAuthenticationMode.SSPI then
    begin
      EditUserLogin.Text := '';
      EditUserPassword.Text := '';
    end;
  {$endif}
  StartStopClient(Restart);
end;

// Clears log memo
procedure TForm1.ButtonCLSClick(Sender: TObject);
begin
  MemoLog.Clear;
end;

// Button start stop client
procedure TForm1.ButtonStartStopClick(Sender: TObject);
begin
  StartStopClient();
end;

// Checkbox Enable/Disable logging to memo (slow down performance when enabled)
procedure TForm1.CheckBoxDisableLogClick(Sender: TObject);
begin
  if not CheckBoxDisableLog.Checked then
    TSQLLog.Family.Level := LOG_VERBOSE
  else
    TSQLLog.Family.Level := [];
end;

{ IRestMethods execution }

procedure TForm1.ButtonMethHelloWorldClick(Sender: TObject);
begin
  if Assigned(RestClient.RestMethods) then
    RestClient.RestMethods.HelloWorld();
end;

procedure TForm1.ButtonMethSendCustomRecordClick(Sender: TObject);
var
  CustomResult: rCustomRecord;
begin
  if Assigned(RestClient.RestMethods) then
    begin
      CustomResult.FillFromClient();
      RestClient.RestMethods.SendCustomRecord(CustomResult);
    end;
end;

procedure TForm1.ButtonMethSumClick(Sender: TObject);
begin
  if Assigned(RestClient.RestMethods) then
    RestClient.RestMethods.Sum(Random(100) + 0.6, Random(100) + 0.3);
end;

procedure TForm1.ButtonGetCustomRecordClick(Sender: TObject);
var
  CustomResult: rCustomRecord;
begin
  if Assigned(RestClient.RestMethods) then
    CustomResult := RestClient.RestMethods.GetCustomRecord();
end;

procedure TForm1.ButtonMethSendMultipleCustomRecordsClick(Sender: TObject);
var
  cr: rCustomRecord;
  ccr: rCustomComplicatedRecord;
begin
  if Assigned(RestClient.RestMethods) then
    begin
      cr.FillFromClient();
      ccr.SimpleString := 'Simple string, Простая строка, 単純な文字列';
      ccr.SimpleInteger := 100500;
      ccr.AnotherRecord := cr;
      RestClient.RestMethods.SendMultipleCustomRecords(cr, ccr);
    end;
end;

procedure TForm1.ButtonMethGetMethodCustomResultClick(Sender: TObject);
var
  ServiceCustomAnswer: TServiceCustomAnswer;
begin
  if Assigned(RestClient.RestMethods) then
    ServiceCustomAnswer := RestClient.RestMethods.GetMethodCustomResult();
end;

end.
