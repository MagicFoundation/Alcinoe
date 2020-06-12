unit ClientForm;

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  Messages,
  Graphics,
  {$endif}
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls,
  SynCommons,
  SynLog,
  mORMot,
  mORMotHttpClient,
  SampleData, ComCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    lblA: TLabel;
    lblB: TLabel;
    lblResult: TLabel;
    QuestionMemo: TMemo;
    NameEdit: TEdit;
    AddButton: TButton;
    QuitButton: TButton;
    FindButton: TButton;
    edtA: TEdit;
    edtB: TEdit;
    btnCall: TButton;
    Button1: TButton;
    Edit1: TEdit;
    ListView1: TListView;
    Panel2: TPanel;
    txtUser: TEdit;
    txtPassword: TEdit;
    btnLogin: TButton;
    procedure AddButtonClick(Sender: TObject);
    procedure FindButtonClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
    procedure btnCallClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ClientFailed(Sender: TSQLRestClientURI; E: Exception;
    Call: PSQLRestURIParams);
  public
    { public declarations }
    Port: String;
    Client: TSQLRest;
    Model: TSQLModel;
    svcClient: TSQLRestClientURI;
    svcModel: TSQLModel;
  end;

var
  Form1: TForm1;

implementation

{$ifdef FPC}
{$R *.lfm}
{$else}
{$R *.dfm}
{$endif}

{ TForm1 }

uses
  CalcInterface,  FileCollect;

var
  startTime64, endTime64, frequency64: Int64;
  elapsedSeconds: single;

procedure EnableControls(const ctrl: TWinControl; enbl: Boolean);
var
  i: Integer;
begin
  for i:=0 to ctrl.ControlCount -1 do
    TWinControl(ctrl.Controls[i]).Enabled := enbl;
end;

procedure TForm1.btnLoginClick(Sender: TObject);
begin
  EnableControls(Panel2, False);
  try
    if TSQLHttpClient(Client).SetUser(txtUser.Text, txtPassword.Text) then
    begin
      if svcClient.SetUser(txtUser.Text, txtPassword.Text) then
      begin
        svcClient.ServiceDefine([ICalculator],sicShared);
        StatusBar1.SimpleText := 'Connected.';
        EnableControls(Panel1, True);
      end;
    end;
  except
    on E: Exception do
    begin
       EnableControls(Panel2, True);
       ShowMessage(E.Message);
    end;
  end;

end;

procedure TForm1.AddButtonClick(Sender: TObject);
var Rec: TSQLSampleRecord;
begin
  Rec := TSQLSampleRecord.Create;
  try
    // we use explicit StringToUTF8() for conversion below
    // a real application should use TLanguageFile.StringToUTF8() in mORMoti18n
    Rec.Name := StringToUTF8(NameEdit.Text);
    Rec.Question := StringToUTF8(QuestionMemo.Text);
    if Client.Add(Rec,true)=0 then
      ShowMessage('Error adding the data') else begin
      NameEdit.Text := '';
      QuestionMemo.Text := '';
      NameEdit.SetFocus;
    end;
  finally
    Rec.Free;
  end;
end;

procedure TForm1.FindButtonClick(Sender: TObject);
var Rec: TSQLSampleRecord;
begin
  QueryPerformanceCounter(startTime64);
  Rec := TSQLSampleRecord.Create(Client,'Name=?',[StringToUTF8(NameEdit.Text)]);
  try
    if Rec.ID=0 then
      QuestionMemo.Text := 'Not found'
    else
      QuestionMemo.Text := UTF8ToString(Rec.Question);
  finally
    Rec.Free;
  end;
  QueryPerformanceCounter(endTime64);
  elapsedSeconds := (endTime64 - startTime64) / frequency64;
  StatusBar1.SimpleText :=  FormatFloat('0.000 msec.', 1000*elapsedSeconds);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Client.Free;
  Model.Free;

  svcClient.Free;
  svcModel.Free;
end;

procedure TForm1.QuitButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.btnCallClick(Sender: TObject);
var a,b: integer;
    err: integer;
    I: ICalculator;
begin
  QueryPerformanceCounter(startTime64);
  val(edtA.Text,a,err);
  if err<>0 then begin
    edtA.SetFocus;
    exit;
  end;
  val(edtB.Text,b,err);
  if err<>0 then begin
    edtB.SetFocus;
    exit;
  end;

  if svcClient.Services['Calculator'].Get(I) then
      lblResult.Caption := 'A + B = ' +IntToStr(I.Add(a,b));

  QueryPerformanceCounter(endTime64);
  elapsedSeconds := (endTime64 - startTime64) / frequency64;
  StatusBar1.SimpleText :=  FormatFloat('0.000 msec.', 1000*elapsedSeconds);
end;

procedure TForm1.ClientFailed(Sender: TSQLRestClientURI; E: Exception;
  Call: PSQLRestURIParams);
var
  values: TPUtf8CharDynArray;
begin

  if (E<>nil) then
    StatusBar1.SimpleText := 'HTTP Error: '+e.Message
  else if (Call<>nil) then
  begin
    JSONDecode(Call.OutBody,['errorCode', 'errorText'],@values,false);
    StatusBar1.SimpleText := 'HTTP Error: '+values[0]+', '+values[1];
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  calc: ICalculator;
  lst: TFlileCollection;
  i: Integer;
begin
   QueryPerformanceCounter(startTime64);
  if svcClient.Services['Calculator'].Get(calc) then
  begin
    lst:= TFlileCollection.Create;
    try
      if calc.GetFileList(Edit1.Text, lst) then
      begin
        ListView1.Items.Clear;
        for i:=0 to lst.Count-1 do
          with ListView1.Items.Add do
          begin
            Caption := lst.Items[i].Name;
            SubItems.Add(IntToStr(lst.Items[i].Size));
            SubItems.Add(DateTimeToStr(lst.Items[i].ModificationDate));
            SubItems.Add(lst.Items[i].Version);
          end;
      end
      else
        ShowMessage('Empty or invalid path!');
    finally
      lst.Free;
    end;
  end;
  QueryPerformanceCounter(endTime64);
  elapsedSeconds := (endTime64 - startTime64) / frequency64;
  StatusBar1.SimpleText :=  FormatFloat('0.000 msec.', 1000*elapsedSeconds);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Server: AnsiString;
  aHttps: Boolean;
begin
  EnableControls(Panel1, False);
  QueryPerformanceFrequency(frequency64);

  
  {with TSQLLog.Family do
  begin
    Level := LOG_VERBOSE;
    EchoToConsole := LOG_VERBOSE; // log all events to the console
  end;
  }

  Caption := ' Sample HTTP Client';
  Port := '8080';

  if ParamCount=0 then
    Server := 'localhost'
  else
    Server := AnsiString(Paramstr(1));

  aHttps := (ParamCount>1) and (AnsiLowerCase(Paramstr(2))='ssl');
  if aHttps then
  begin
    Caption := Caption + ' SSL';
    Port := '8443';
  end;

  Model := CreateSampleModel; // from SampleData unit
  Client := TSQLHttpsClient.Create(Server,Port,Model,aHttps);
  TSQLHttpClient(Client).OnFailed := ClientFailed;

  svcModel := TSQLModel.Create([],SERVICE_NAME);
  svcClient := TSQLHttpsClient.Create(Server,Port,svcModel,aHttps);
  svcClient.OnFailed := ClientFailed
end;

end.

