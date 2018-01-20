unit mormotMongo;

interface

uses
  SysUtils, Variants, Controls, Forms, Dialogs,
  mORMot, mORMotUI, SynCommons, SynMongoDB, jpeg,
  Grids, Buttons, ToolWin, ExtDlgs, ComCtrls, ExtCtrls, StdCtrls, Classes;

type
  TFrmServidor = class(TForm)
    Memo1: TMemo;
    txtname: TLabeledEdit;
    txtgrapes: TLabeledEdit;
    txtcountry: TLabeledEdit;
    txtregion: TLabeledEdit;
    txtyear: TLabeledEdit;
    txtphoto: TLabeledEdit;
    Panel1: TPanel;
    photo: TImage;
    txtdescription: TMemo;
    txtID: TLabeledEdit;
    bmp1: TImage;
    Label1: TLabel;
    title: TLabel;
    txtItems: TLabel;
    txtitem1: TEdit;
    txtitem2: TEdit;
    txtitem3: TEdit;
    StringGrid1: TStringGrid;
    OpenPictureDialog1: TOpenPictureDialog;
    txtsearch: TEdit;
    Label2: TLabel;
    status: TLabel;
    bmp2: TImage;
    ToolBar1: TToolBar;
    btnClear: TBitBtn;
    btnSave: TBitBtn;
    btnDelete: TBitBtn;
    btnPrev: TBitBtn;
    pag: TLabel;
    btnNext: TBitBtn;
    L1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    btnItems: TBitBtn;

    procedure FormCreate(Sender: TObject);
    procedure photoClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnItemsClick(Sender: TObject);
  protected
  private
    { Private declarations }
    fClient: TMongoClient;
    fDB: TMongoDatabase;
    fCollection : TMongoCollection;
    fGrid: TSQLTableToGrid;
    procedure InicializaMongo;
    procedure ShowRecord(b : Variant);
    procedure OnGridDblClick(Sender: TObject);
  public
    { Public declarations }
  published
  end;

const
  NoConnectMsg = 'Unable to connect to a MongoDB server running on localhost';

var
  FrmServidor: TFrmServidor;
  page: Integer;
  str : string;

implementation

{$R *.dfm}
uses form2;

procedure InsertRow(VGrid: TStringGrid);
var
I: Integer;
begin
VGrid.RowCount:= VGrid.RowCount + 1;
for I:= 0 to VGrid.ColCount - 1 do
VGrid.Cells[I, VGrid.RowCount - 1]:= '';
VGrid.Row:= VGrid.RowCount - 1;
end;

procedure DeleteRow(ARowIndex: Integer; AGrid: TStringGrid);
var
  i, j: Integer;
begin
  with AGrid do
  begin
    if (ARowIndex = RowCount) then
      RowCount := RowCount - 1
    else
    begin
      for i := ARowIndex to RowCount do
        for j := 0 to ColCount do
          Cells[j, i] := Cells[j, i + 1];
      RowCount := RowCount - 1;
    end;
  end;
end;


procedure MoveRowUp(vgrid: TStringGrid; vrow:integer);
var
s: string;
i:integer;
begin
if vrow=1 then exit;
for i:=0 to vgrid.colcount-1 do
begin
s:=vgrid.cells[i,vrow-1];
vgrid.cells[i,vrow-1]:=vgrid.cells[i,vrow];
vgrid.cells[i,vrow]:=s;
end;
vgrid.row:= vrow -1;
vgrid.repaint;
end;

procedure MoveRowDown(vgrid: TStringGrid; vrow:integer);
var
s: string;
i:integer;
begin
if vrow= vgrid.rowcount-1 then exit;
for i:=0 to vgrid.colcount-1 do
begin
s:=vgrid.cells[i,vrow+1];
vgrid.cells[i,vrow+1]:=vgrid.cells[i,vrow];
vgrid.cells[i,vrow]:=s;
end;
vgrid.row:=vrow +1;
vgrid.repaint;
end;

procedure ClearStringGrid(const Grid: TStringGrid);
var
  c : Integer;
begin
  for c := 0 to Pred(Grid.RowCount-1) do
      Grid.Cols[c].Clear;
      Grid.RowCount := 1;
end;

procedure TFrmServidor.FormCreate(Sender: TObject);
begin
bmp1.Picture.Bitmap.LoadFromResourceName(HInstance, 'BMP1');
bmp2.Picture.Bitmap.LoadFromResourceName(HInstance, 'BMP2');
btnPrev.Glyph.LoadFromResourceName(HInstance, 'BMP3');
btnNext.Glyph.LoadFromResourceName(HInstance, 'BMP4');
btnClear.Glyph.LoadFromResourceName(HInstance, 'BMP5');
btnDelete.Glyph.LoadFromResourceName(HInstance, 'BMP6');
btnSave.Glyph.LoadFromResourceName(HInstance, 'BMP7');
btnItems.Glyph.LoadFromResourceName(HInstance, 'BMP8');

InicializaMongo;
end;

procedure TFrmServidor.photoClick(Sender: TObject);
begin
OpenPictureDialog1.Execute();
photo.Picture.LoadFromFile(ExtractFileName(OpenPictureDialog1.FileName));
txtphoto.Text :=  ExtractFileName(OpenPictureDialog1.FileName);
if txtphoto.Text <> '' then btnSave.Enabled := true;
end;

procedure TFrmServidor.btnClearClick(Sender: TObject);
begin
ClearStringGrid(StringGrid1);
  txtID .Text := ObjectId();
  txtname.Text := '';
  txtyear.Text := '';
  txtgrapes.Text := '';
  txtcountry.Text := '';
  txtregion.Text := '';
  txtdescription.Lines.Clear;
  txtphoto.Text := 'warley.jpg';
  txtItems.Caption := '[{"sku":"","quantity":0,"price":0.0}]';
  photo.Picture.Assign(nil);

end;

procedure TFrmServidor.btnSaveClick(Sender: TObject);
var
 DBObject : variant;
begin
TDocVariant.New(DBObject);
 begin
  DBObject.Clear;
  DBObject._id           := ObjectId(txtID.Text);
  DBObject.name          := txtname.Text;
  DBObject.year          := txtyear.Text;
  DBObject.grapes        := txtgrapes.Text;
  DBObject.country       := txtcountry.Text;
  DBObject.region        := txtregion.Text;
  DBObject.description   := txtdescription.Lines.Text;
  DBObject.picture       := txtphoto.Text;
  DBObject.items         := _JSON(txtItems.Caption);
  if txtID.Text = '' then DBObject._id := ObjectId() else DBObject._id := ObjectId(txtID.Text);
  if  (MessageDlg('Save this record?', mtWarning, [mbYes, MbNo], 0) = mrYes) then
  begin
     fCollection.Save(DBObject);
     btnPrev.Click;
     btnDelete.Enabled := true;
  end;
 end;
end;

procedure TFrmServidor.btnDeleteClick(Sender: TObject);
begin
  if txtID.Text <>'' then
  if MessageDlg('Are you sure delete this record?', mtWarning, [mbYes, MbNo], 0) = mrYes then
  begin
  fCollection.RemoveOne(ObjectId(txtID.Text));
  btnDelete.Enabled := false;
  btnClear.Click;
  end;
    btnPrev.Click; btnDelete.Enabled := true;
end;

procedure TFrmServidor.InicializaMongo;
var
 DBObject   : variant;
begin
  fClient := TMongoClient.Create('localhost',27017);
  fDB := fClient.Database['winedb'];
  fCollection := fDB.Collection['wines'];
  if fCollection.Count <> 0 then
  page := 0;
  pag.Caption := IntToStr(page);
  btnSearchClick(self);
  DBObject := fCollection.FindJSON(null, null);
end;

procedure TFrmServidor.ShowRecord(b : Variant);
var
 fTableJSON : RawUTF8;
 aTable :TSQLTable;
 begin
    txtID.Text := ( b._id );
    txtname.Text := VariantToUTF8( b.name );
    txtyear.Text := string(VariantToUTF8(b.year ));
    txtgrapes.Text := string(VariantToUTF8(b.grapes ));
    txtcountry.Text := string(VariantToUTF8(b.country ));
    txtregion.Text := string(VariantToUTF8(b.region ));
    txtdescription.Text := string( VariantToUTF8(b.description )) ;
    photo.Picture.LoadFromFile({GetCurrentDir+}'E:\mormot\SQLite3\Samples\projMongo\srv\pics\'+ b.picture );
    txtphoto.Text := string(VariantToUTF8(b.picture ));
if not b.Exists('items')then ClearStringGrid(StringGrid1) else
fTableJSON := b.items;
StringGrid1.width := 290;
txtItems.Caption :=   fTableJSON;
aTable := TSQLTableJSON.Create('',fTableJSON);
fGrid := TSQLTableToGrid.Create(StringGrid1, aTable, nil);
StringGrid1.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect];
//              fGrid.SetAlignedByType(sftCurrency,alRight);
//              fGrid.OnValueText := OnText;
              fGrid.SetFieldFixedWidth(70);
              fGrid.FieldTitleTruncatedNotShownAsHint := true;
//              StringGrid1.Options := StringGrid1.Options-[goRowSelect];
              StringGrid1.OnDblClick := self.OnGridDblClick;
end;

procedure TFrmServidor.btnPrevClick(Sender: TObject);
var DBObject : variant;
begin
if fCollection.Count = 0 then
   begin btnClearClick(self);
         exit;
   end;
dec(page);
if page < 0 then page := 0;
pag.caption := IntToStr(page+1);
str := txtsearch.Text;

DBObject := fCollection.FindDoc('{name:?}',[BSONVariant('{"$regex": "'+str+'", $options: "i"}')],1,page) ;
ShowRecord(DBObject);
end;

procedure TFrmServidor.btnNextClick(Sender: TObject);
var DBObject : variant;
cnt : integer;
begin
str := txtsearch.Text;
cnt := fCollection.FindCount('{name:?}',[],[BSONVariant('{"$regex": "'+str+'", $options: "i"}')]) ;

if fCollection.Count = 0 then exit;
if page < cnt-1 then inc(page);
pag.caption := IntToStr(page+1);

DBObject := fCollection.FindDoc('{name:?}',[BSONVariant('{"$regex": "'+str+'", $options: "i"}')],1,page) ;
ShowRecord(DBObject);
end;

procedure TFrmServidor.OnGridDblClick(Sender: TObject);
var
    doc : Variant;
    i   : integer;
    s : string;
    csv : TStringList;
begin
csv:= TStringList.create;
   s:= ',';
for i := 1 to StringGrid1.RowCount-1 do begin
fGrid.Table.ToDocVariant(i,doc);
 csv.add (doc);
end;
ShowMessage(csv.GetText);
//txtItem1.Text :=   fGrid.Table.GetU(StringGrid1.Row,0);
//txtItem2.Text :=   fGrid.Table.GetU(StringGrid1.Row,1);
//txtItem3.Text :=   fGrid.Table.GetU(StringGrid1.Row,2);
end;

procedure TFrmServidor.btnSearchClick(Sender: TObject);
var
cnt : integer;
begin
str := txtsearch.Text;
cnt := fCollection.FindCount('{name:?}',[],[BSONVariant('{"$regex": "'+str+'", $options: "i"}')]) ;
page := 0;
pag.Caption := '0';
status.Caption := 'Found: '+ IntToStr(cnt) + ' docs';
if cnt <> 0 then btnPrev.Click;
end;

procedure TFrmServidor.btnItemsClick(Sender: TObject);
begin
Items.show;;
end;

end.
