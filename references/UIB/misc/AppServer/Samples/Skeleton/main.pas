unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TestLib_Client, StdCtrls, WinSock, DB, kbmMemTable, Grids,
  DBGrids, kbUIBLoader, ExtCtrls, DBCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    DataSource: TDataSource;
    DBGrid1: TDBGrid;
    Button3: TButton;
    Edit1: TEdit;
    DataSource1: TDataSource;
    DBGrid2: TDBGrid;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    Employee, Department: TkbUIBLoader;
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation
uses uiblib, PDGUtils;

{$R *.dfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  o: TMyObjectClient;
  stream: TMemoryStream;
  qr: TSQLResult;
begin
  o := TMyObjectClient.Create('localhost', 33000);
  stream := TMemoryStream.Create;
  qr := TSQLResult.Create;
  Employee.DisableControls;
  Department.DisableControls;
  try
    o.ExecuteScript('select * from department; select * from employee;', stream);

    Employee.MasterSource := nil;
    Employee.MasterFields := '';
    Employee.DetailFields := '';

    qr.LoadFromStream(stream);
    Department.LoadFromSQLResult(qr);
    Department.Commit;
    Department.CheckPoint;

    qr.LoadFromStream(stream);
    Employee.LoadFromSQLResult(qr);
    Employee.Commit;
    Employee.CheckPoint;

    Employee.MasterSource := DataSource1;
    Employee.MasterFields := 'DEPT_NO';
    Employee.DetailFields := 'DEPT_NO';
  finally
    Employee.EnableControls;
    Department.EnableControls;
    stream.Free;
    o.free;
    qr.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Employee := TkbUIBLoader.Create(self);
  Employee.EnableVersioning := true;
  Employee.VersioningMode := mtvmAllSinceCheckPoint;
  DataSource.DataSet := Employee;

  Department := TkbUIBLoader.Create(self);
  Department.EnableVersioning := true;
  Department.VersioningMode := mtvmAllSinceCheckPoint;
  DataSource1.DataSet := Department;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  o: TMyObject2Client;
begin
  o := TMyObject2Client.Create('localhost', 33000);
  try
    Edit1.Text := o.GetString;
  finally
    o.free;
  end;
end;

end.
