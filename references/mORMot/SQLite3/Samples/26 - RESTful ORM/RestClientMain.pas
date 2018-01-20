unit RestClientMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  SynCommons,
  mORMot,
  mORMotHttpClient,
  RESTData, StdCtrls; // data model unit, shared between server and client

type
  TMainForm = class(TForm)
    btnConnect: TButton;
    btnGet: TButton;
    btnNew: TButton;
    lblNewID: TLabel;
    btnSet: TButton;
    mmoSet: TMemo;
    mmoGet: TMemo;
    edtGetID: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnGetClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnSetClick(Sender: TObject);
  protected
    fModel: TSQLModel;
    fClient: TSQLHttpClient;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fModel := DataModel('root');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fClient);
  FreeAndNil(fModel);
end;

procedure TMainForm.btnConnectClick(Sender: TObject);
begin
  fClient := TSQLHttpClientWinHTTP.Create('localhost','888',fModel);
  try
    if not fClient.ServerTimeStampSynchronize then
      exit;
    btnConnect.Enabled := false;
    btnNew.Enabled := true;
    btnGet.Enabled := true;
    btnSet.Enabled := true;
    mmoGet.Enabled := true;
    mmoSet.Enabled := true;
    edtGetID.Enabled := true;
  except
    FreeAndNil(fClient);
  end;
end;

procedure TMainForm.btnGetClick(Sender: TObject);
var ID: TID;
    resp: RawUTF8;
begin
  if fClient=nil then
    exit;
  if not TryStrToInt64(edtGetID.Text,Int64(ID)) then
    exit;
  if fClient.CallBackGet('blob',[],resp,TSQLNoteFile,ID)=HTTP_SUCCESS then
    mmoGet.Text := UTF8ToString(resp) else
    mmoGet.Text := '? not found';
end;

procedure TMainForm.btnNewClick(Sender: TObject);
var Note: TSQLNoteFile;
begin
  if fClient=nil then
    exit;
  Note := TSQLNoteFile.Create;
  try
    Note.FileName := 'Test'+UInt32ToUTF8(GetTickCount64);
    Note.MetaData := _ObjFast(['timestamp',GetTickCount64]);
    Tag := fClient.Add(Note,true);
    lblNewID.Caption := Format('Current ID=%d',[Tag]);
    mmoSet.Text := Format('Bla bla %d'#13#10'%d',[Tag,GetTickCount64]);
    edtGetID.Text := IntToStr(Tag);
  finally
    Note.Free;
  end;
end;

procedure TMainForm.btnSetClick(Sender: TObject);
var resp: RawUTF8;
begin
  if fClient=nil then
    exit;
  if fClient.CallBackPut('blob',StringToUTF8(mmoSet.Text),resp,TSQLNoteFile,Tag)=HTTP_CREATED then
    mmoSet.Text := '.. saved ..';
end;

end.
