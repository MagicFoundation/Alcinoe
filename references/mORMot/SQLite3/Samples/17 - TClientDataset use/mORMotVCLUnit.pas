unit mORMotVCLUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Grids, DBGrids,
  SynCommons, mORMot, mORMotMidasVCL, mORMotVCL,
  SynDB, SynDBSQLite3, SynSQLite3Static, SynDBRemote,
  SynVirtualDataset, SynDBMidasVCL, SynDBVCL,
  DB, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    dbgrdData: TDBGrid;
    ds1: TDataSource;
    pnl1: TPanel;
    chkViaTClientDataSet: TCheckBox;
    lblTiming: TLabel;
    cbbDataSource: TComboBox;
    lblFrom: TLabel;
    btnRefresh: TButton;
    btnApply: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure chkFromSQLClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
  private
    fJSON: RawUTF8;
    fDBFileName: TFileName;
    fProps: TSQLDBConnectionProperties;
    fServer: TSQLDBServerAbstract;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

const
  SERVER_CLASS: TSQLDBServerClass =
    {$ifdef MSWINDOWS}TSQLDBServerHttpApi{$else}TSQLDBServerSockets{$endif};
  SERVER_PORT = {$ifdef MSWINDOWS}'888'{$else}'8888'{$endif};
  SERVER_NAME = 'root';
  SERVER_ADDR = 'localhost:'+SERVER_PORT;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fJSON := StringFromFile('..\..\exe\People.json');
  if fJSON='' then
    fJSON := StringFromFile('..\..\People.json');
  if fJSON='' then
    fJSON := StringFromFile('..\..\..\exe\People.json');
  if fJSON='' then
    raise Exception.Create('No People.json');
  fDBFileName :=  '..\..\exe\test.db3';
  if not FileExists(fDBFileName) then
    fDBFileName :=  '..\..\test.db3';
  if not FileExists(fDBFileName) then
    fDBFileName :=  '..\..\..\exe\test.db3';
  if not FileExists(fDBFileName) then
    raise Exception.Create('No test.db3');
  fProps := TSQLDBSQLite3ConnectionProperties.Create(StringToUTF8(fDBFileName),'','','');
  fServer := SERVER_CLASS.Create(fProps,SERVER_NAME,SERVER_PORT,'user','pass');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  fServer.Free;
  fProps.Free;
end;

procedure TForm1.chkFromSQLClick(Sender: TObject);
const SQL_PEOPLE = 'select * from People';
var proxy: TSQLDBConnectionProperties;
    stmt: TSQLDBStatement;
    values: TDocVariantData;
    Timer: TPrecisionTimer;
begin
  ds1.DataSet.Free;
  chkViaTClientDataSet.Enabled := not (cbbDataSource.ItemIndex in [1]);
  Timer.Start;
  case cbbDataSource.ItemIndex of
  0: // test TSynSQLTableDataSet: reading from JSON content
    if chkViaTClientDataSet.Checked then
      ds1.DataSet := JSONToClientDataSet(self,fJSON) else
      ds1.DataSet := JSONToDataSet(self,fJSON, // demo client-side column definition
        [sftInteger,sftUTF8Text,sftUTF8Text,sftBlob,sftInteger,sftInteger]);
  1: begin // no TClientDataSet yet for dynamic array of TDocVariant
    values.InitJSON(fJSON,JSON_OPTIONS[true]);
    ds1.DataSet := ToDataSet(self,values.Values,[],[]);
  end;
  2..7: begin
    // test TSynSQLStatementDataSet: reading from SynDB database
    proxy := fProps;
    try
      case cbbDataSource.ItemIndex of
      2: ; // source is directly the SQLite3 engine
      3: proxy := TSQLDBRemoteConnectionPropertiesTest.Create(
          fProps,'user','pass',TSQLDBProxyConnectionProtocol);
      4: proxy := TSQLDBRemoteConnectionPropertiesTest.Create(
         fProps,'user','pass',TSQLDBRemoteConnectionProtocol);
      5: proxy := TSQLDBWinHTTPConnectionProperties.Create(
          SERVER_ADDR,SERVER_NAME,'user','pass');
      6: proxy := TSQLDBWinINetConnectionProperties.Create(
          SERVER_ADDR,SERVER_NAME,'user','pass');
      7: proxy := TSQLDBSocketConnectionProperties.Create(
          SERVER_ADDR,SERVER_NAME,'user','pass');
      end;
      stmt := proxy.NewThreadSafeStatement;
      try
        stmt.Execute(SQL_PEOPLE,true);
        if chkViaTClientDataSet.Checked then
          ds1.DataSet := ToClientDataSet(self,stmt) else 
          ds1.DataSet := ToDataSet(self,stmt);
      finally
        stmt.Free;
      end;
    finally
      if proxy<>fProps then
        proxy.Free;
    end;
  end;
  8: // test TSynDBSQLDataSet / TSynDBDataSet
    if chkViaTClientDataSet.Checked then begin
      ds1.DataSet := TSynDBDataSet.Create(self);
      TSynDBDataSet(ds1.DataSet).Connection := fProps;
      TSynDBDataSet(ds1.DataSet).CommandText := SQL_PEOPLE;
      TSynDBDataSet(ds1.DataSet).IgnoreColumnDataSize := true;
      ds1.DataSet.Open;
    end else begin
      ds1.DataSet := TSynDBSQLDataSet.Create(self);
      TSynDBSQLDataSet(ds1.DataSet).Connection := fProps;
      TSynDBSQLDataSet(ds1.DataSet).CommandText := SQL_PEOPLE;
      ds1.DataSet.Open;
    end;
  end;
  lblTiming.Caption := 'Processed in '+Ansi7ToString(Timer.Stop);
end;



procedure TForm1.btnApplyClick(Sender: TObject);
begin
  if ds1.DataSet is TSynDBDataSet then
    TSynDBDataSet(ds1.DataSet).ApplyUpdates(0);
end;

end.
