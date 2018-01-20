unit Ffactwin;

{ This application shows how to display TSynRestDataset style memo and graphic
  fields in a form.

  - This application use TWebBrowser for display the image from Project19Server.db3.
  - Removed display of image because is need convert the Project19Server.db3 field image to base64 or any suggest.
  - fixed memory leak (by houdw2006)
}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, StdCtrls, DBCtrls, DBGrids, DB, DBTables, Buttons, Grids, ExtCtrls,
  SynRestMidasVCL, DBClient,
  SynCommons, mORMot, OleCtrls, Dialogs, ExtDlgs,
  SynGdiPlus;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    DBLabel1: TDBText;
    DBMemo1: TDBMemo;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    dbnvgr1: TDBNavigator;
    btnUpload: TButton;
    dlgOpenPic1: TOpenPictureDialog;
    img: TImage;
    procedure FormCreate(Sender: TObject);
    procedure dbnvgr1Click(Sender: TObject; Button: TNavigateBtn);
    procedure btnUploadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure DoOnAfterScroll(Dataset: TDataset);
  public
    { Public declarations }
    SynRestDataset: TSynRestDataset;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses SampleData;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  SynRestDataset := TSynRestDataset.Create(Nil);
  SynRestDataset.DataSet.SQLModel := TSQLModel.Create([TSQLBioLife]);
  SynRestDataset.CommandText := 'http://LocalHost:8080/root/BioLife?select=Species_No,Category,Common_Name,Species_Name,Length_cm,Length_in,Graphic,Notes,Som&sort=Species_No';
  SynRestDataset.Open;
  SynRestDataset.AfterScroll := DoOnAfterScroll;
  DataSource1.DataSet := SynRestDataset;
  // show the first record image
  DoOnAfterScroll(Nil);
  // hide blob fields in the grid
  for I := 0 to DBGrid1.Columns.Count-1 do
    if (DBGrid1.Columns[I].Field.DataType = DB.ftBlob) then
      DBGrid1.Columns[I].Visible := False;
end;

procedure TForm1.dbnvgr1Click(Sender: TObject; Button: TNavigateBtn);
begin
  case Button of
    nbDelete, nbPost: SynRestDataset.ApplyUpdates(0);
  end;
end;

procedure TForm1.btnUploadClick(Sender: TObject);
begin
  // I don't know as encode this :(
  if not (SynRestDataset.State in [dsEdit, dsInsert]) then
    SynRestDataset.Edit;
  if dlgOpenPic1.Execute then
    TBlobField(SynRestDataset.FieldByName('Graphic')).LoadFromFile(dlgOpenPic1.FileName);
end;

procedure TForm1.DoOnAfterScroll(Dataset: TDataset);
begin
  //img.Picture :=
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SynRestDataset.Dataset.SQLModel.Free;
  SynRestDataset.Dataset.SQLModel := nil;
  FreeAndNil(SynRestDataset);
end;

end.
