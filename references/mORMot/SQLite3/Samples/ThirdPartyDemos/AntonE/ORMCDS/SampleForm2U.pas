unit SampleForm2U;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.DBGrids, JvExDBGrids,
  JvDBGrid, JvDBUltimGrid, Vcl.StdCtrls,DBClient, Data.DB;

type
  TSampleForm2 = class(TForm)
    Button2: TButton;
    BtnApply: TButton;
    JvDBUltimGrid1: TJvDBUltimGrid;
    JvDBUltimGrid3: TJvDBUltimGrid;
    DSPerson: TDataSource;
    DSPhones: TDataSource;
    DSChildren: TDataSource;
    JvDBUltimGrid2: TJvDBUltimGrid;
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    TmpCDS:TClientdataset;
  end;

var
  SampleForm2: TSampleForm2;

implementation

{$R *.dfm}

uses RTTI,mORMot,MAinFormU,ORMCDS;

procedure TSampleForm2.BtnApplyClick(Sender: TObject);
var Value : TValue;
begin
 Value.From(TSQLRecord);
 Value:=TSQLRecord;
 ORM_SaveCDSFields(MAinForm.DB,TmpCDS,'Person',Value,False);
 Close;
end;

procedure TSampleForm2.Button2Click(Sender: TObject);
var Person     : TSQLPerson;
    Ctx        : TRttiContext;
begin
 TmpCDS:=TClientdataset.Create(self);

 Ctx:=TRttiContext.Create;
 ORM_CreateCDSFields(TmpCDS   ,'Person'  ,Ctx.GetType(TSQLPerson).Handle);
 ORM_AddSubField    (TmpCDS   ,'Children','Parent',TSQLChild);

 TmpCDS.CreateDataSet;
 TmpCDS.LogChanges:=False;
 TmpCDS.SaveToFile('FileX.xml');

 Person:=TSQLPerson.Create;
 MainForm.DB.Retrieve(1,Person);
 ORM_LoadCDSFields(MainForm.DB,TmpCDS,'Person',Person);
 MainForm.DB.Retrieve(2,Person);
 ORM_LoadCDSFields(MainForm.DB,TmpCDS,'Person',Person);
 Person.Free;

 TmpCDS.LogChanges:=True;
 TmpCDS.MergeChangeLog;
 TmpCDS.SaveToFile('FileY.xml');

 (*Link Datasources to Tmp TClientdatasets*)
 DSPerson.DataSet:=TmpCDS;
 DSPhones.DataSet:=TORMCDSinfo(TmpCDS.FieldByName('Phones').Tag).CDS;
 DSPhones  .DataSet:=TORMCDSinfo(TmpCDS.FieldByName('Phones'  ).Tag).CDS;
 DSChildren.DataSet:=TORMCDSinfo(TmpCDS.FieldByName('Children').Tag).CDS;
end;

procedure TSampleForm2.FormDestroy(Sender: TObject);
begin
 if Assigned(TmpCDS)
    then ORM_FreeCDSInfo(TmpCDS,True);
end;

end.
