unit mainclient;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids;

type
  TMainForm = class(TForm)
    getdata: TButton;
    Grid: TStringGrid;
    table: TEdit;
    procedure getdataClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation
uses msxml, superobject;
{$R *.dfm}

var
  cnx: IXMLHttpRequest = nil;

procedure TMainForm.getdataClick(Sender: TObject);
var
  params, dataset, meta, data: ISuperObject;
  i, j: integer;
begin
  if cnx = nil then
    cnx := CoXMLHTTPRequest.Create;
  cnx.open('POST', 'http://localhost:81/', false, 'user', 'pass');
  cnx.setRequestHeader('Accept', 'application/json');

  params := SO;
  params.S['controller'] := 'application';
  params.S['action'] := 'getdata';
  params.S['id'] := table.Text;

  cnx.send(string(params.AsString));

  dataset := SO(string(cnx.responseText));

  meta := dataset['meta'];
  data := dataset['data'];

  grid.ColCount := meta.AsArray.Length+1;
  grid.RowCount := data.AsArray.Length+1;

  for i := 0 to meta.AsArray.Length - 1 do
  begin
    Grid.Cells[i+1, 0] := meta.AsArray[i].S['name'];
    for j := 0 to data.AsArray.Length - 1 do
      Grid.Cells[i+1, j+1] := data.AsArray[j].AsArray.S[i];
  end;

end;

end.
