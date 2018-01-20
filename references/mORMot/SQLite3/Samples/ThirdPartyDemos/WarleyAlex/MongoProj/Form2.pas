unit Form2;
interface

uses
 Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
 Dialogs, Grids, StdCtrls, Mask;

type
 TItems = class(TForm)
   StringGrid: TStringGrid;
   edtName: TEdit;
   btnAdd: TButton;
   Label1: TLabel;
   Label2: TLabel;
   Label3: TLabel;
   btnDelete: TButton;
    Button1: TButton;
    edtCity: TMaskEdit;
    edtEmail: TMaskEdit;
    Button2: TButton;
   procedure FormCreate(Sender: TObject);
   procedure btnAddClick(Sender: TObject);
   procedure btnDeleteClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
 private
   procedure DeleteStringGridRow(n: Integer; var Grid: TStringGrid);
 public
 end;

var
 Items: TItems;

implementation

{$R *.dfm}

procedure TItems.FormCreate(Sender: TObject);
begin
 StringGrid.ColCount := 4;
 StringGrid.RowCount := 2;
 StringGrid.FixedRows := 1;
 StringGrid.ColWidths[0] := 50;
 StringGrid.ColWidths[1] := 100;
 StringGrid.ColWidths[2] := 50;
 StringGrid.ColWidths[3] := 50;

 StringGrid.DefaultRowHeight := StringGrid.Canvas.TextHeight('X') + 8;

 StringGrid.Cells[0, 0] := 'Row';
 StringGrid.Cells[1, 0] := 'SKU';
 StringGrid.Cells[2, 0] := 'Quantity';
 StringGrid.Cells[3, 0] := 'Price';
end;

procedure TItems.btnAddClick(Sender: TObject);
begin
 if Trim(edtName.Text) = '' then begin
   ShowMessage('Name Required');
   Exit;
 end;

 if (StringGrid.RowCount > 2) or (StringGrid.Cells[1, StringGrid.RowCount-1] > '') then
   StringGrid.RowCount := StringGrid.RowCount + 1;

 StringGrid.Cells[0, StringGrid.RowCount-1] := IntToStr(StringGrid.RowCount-1);
 StringGrid.Cells[1, StringGrid.RowCount-1] := edtName.Text;
 StringGrid.Cells[2, StringGrid.RowCount-1] := edtCity.Text;
 StringGrid.Cells[3, StringGrid.RowCount-1] := edtEmail.Text;

 edtName.Text := '';
 edtCity.Text := '';
 edtEmail.Text := '';

 ActiveControl := edtName;
end;


procedure TItems.btnDeleteClick(Sender: TObject);
begin
 if (StringGrid.Row > 0)
 and (StringGrid.Cells[1, StringGrid.RowCount-1] > '')
 and (MessageDlg(Format('Delete %s?', [StringGrid.Cells[1, StringGrid.Row]]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then begin
   DeleteStringGridRow(StringGrid.Row, StringGrid);
   if StringGrid.RowCount < 2 then
     StringGrid.RowCount := 2;
 end;

 ActiveControl := StringGrid;
end;

procedure TItems.DeleteStringGridRow(n: Integer; var Grid: TStringGrid);
var
 i: integer;
begin
 if Grid.RowCount > Grid.FixedRows + 1 then begin
   for i := (n + 1) to Grid.RowCount do
     Grid.Rows[i-1] := Grid.Rows[i];
   Grid.RowCount := Grid.RowCount -1;
 end else
   Grid.Rows[n].Delete(Grid.Row);
end;
procedure TItems.Button1Click(Sender: TObject);
begin
close;
end;

type
TTableData = record
  header: String[25];   //the header of the column (row 0)
  value : String[25];   //the string value of the data
  number: Integer;      //the y-pos of the data in the table
end;

procedure TItems.Button2Click(Sender: TObject);
var
  i, j: Integer;
  arrData : TTableData;
  tableData : array of TTableData;
  Total_Firma:array of array[0..3] of string;
  CSV : TStrings;
begin
end;
//  for i:= 0 to StringGrid1.RowCount - 1 do begin
//    for j:=0 to StringGrid1.ColCount - 1 do begin
//        ShowMessage(tableData[i,j].header+': '+tableData[i,j].value);
//    end;
//  end;
//end;

end.
