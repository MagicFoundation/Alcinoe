unit fMain;

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  Messages,
  {$endif}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SynCommons, mORMot, mORMotSQLite3, SynSQLite3Static,
  mORMotHttpServer, SampleData;

type
  TfrmMain = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
    Model: TSQLModel;
    DB: TSQLRestServerDB;
    Server: TSQLHttpServer;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  mORMotDB;

{$R *.dfm}

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Model := CreateSampleModel;
  DB := TSQLRestServerDB.Create(Model, 'Project19Server.db3', False);
  DB.CreateMissingTables;
  Server := TSQLHttpServer.Create('8080',[DB],'+',HTTP_DEFAULT_MODE);
  Server.AccessControlAllowOrigin := '*'; // allow cross-site AJAX queries
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Server.Free;
  DB.Free;
  Model.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  Label1.Caption := Caption;
end;

end.
