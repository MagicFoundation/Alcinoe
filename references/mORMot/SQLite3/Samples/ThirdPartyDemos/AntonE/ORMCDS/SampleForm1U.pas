unit SampleForm1U;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.DBGrids, JvExDBGrids,
  JvDBGrid, JvDBUltimGrid, Vcl.StdCtrls, Data.DB, Datasnap.DBClient;

type
  TSampleForm1 = class(TForm)
    DSPerson: TDataSource;
    DSPhones: TDataSource;
    DSChildren: TDataSource;
    cdsChildren: TClientDataSet;
    cdsChildrenID: TLargeintField;
    cdsChildrenParent: TLargeintField;
    cdsChildrenChildName: TStringField;
    cdsChildrenChildGender: TIntegerField;
    cdsPhones: TClientDataSet;
    cdsPhonesNumber: TStringField;
    cdsPhonesPType_ptWork: TBooleanField;
    cdsPhonesPType_ptHome: TBooleanField;
    cdsPhonesPType_ptFax: TBooleanField;
    cdsPhonesPType_ptSMS: TBooleanField;
    cdsPerson: TClientDataSet;
    cdsPersonID: TLargeintField;
    cdsPersonName: TStringField;
    cdsPersonInt: TIntegerField;
    cdsPersonPhones: TDataSetField;
    cdsPersonGender: TIntegerField;
    cdsPersonChildren: TDataSetField;
    Button2: TButton;
    JvDBUltimGrid1: TJvDBUltimGrid;
    JvDBUltimGrid2: TJvDBUltimGrid;
    JvDBUltimGrid3: TJvDBUltimGrid;
    BtnApply: TButton;
    Label1: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JvDBUltimGrid1GetCellParams(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor; Highlight: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SampleForm1: TSampleForm1;

implementation

{$R *.dfm}

uses MainFormU,ORMCDS,mORMot,RTTI,Provider;

procedure TSampleForm1.BtnApplyClick(Sender: TObject);
var Value : TValue;
begin
 Value.From(TSQLRecord);
 Value:=TSQLRecord;
 ORM_SaveCDSFields(MainForm.DB,cdsPerson,'Person',Value,False);
 Close;
end;

procedure TSampleForm1.Button2Click(Sender: TObject);
var Person     : TSQLPerson;
begin
 ORM_LinkCDS(cdsPerson  ,TypeInfo(TSQLPerson),'');
 ORM_LinkCDS(cdsPhones  ,TypeInfo(TPhoneArr ),'');
 ORM_LinkCDS(cdsChildren,TypeInfo(TSQLChild ),'Parent');

 cdsPerson.CreateDataSet;
 cdsPerson.LogChanges:=False;
 cdsPerson.SaveToFile('FileX.xml');

 Person:=TSQLPerson.Create;
 MAinForm.DB.Retrieve(1,Person);
 ORM_LoadCDSFields(MainForm.DB,cdsPerson,'Person',Person);
 MainForm.DB.Retrieve(2,Person);
 ORM_LoadCDSFields(MainForm.DB,cdsPerson,'Person',Person);
 Person.Free;
 cdsPerson.MergeChangeLog;
 cdsPerson.LogChanges:=True;
 cdsPerson.SaveToFile('FileY.xml');
end;

procedure TSampleForm1.FormDestroy(Sender: TObject);
begin
 ORM_FreeCDSInfo(cdsPerson,False);
end;

procedure TSampleForm1.JvDBUltimGrid1GetCellParams(Sender: TObject;
  Field: TField; AFont: TFont; var Background: TColor; Highlight: Boolean);
begin
 case TClientDataset(Field.DataSet).UpdateStatus of
  usUnmodified : Background:=clWhite;
  usModified   : BAckground:=clMoneyGreen;
  usInserted   : Background:=clSkyBlue;
  usDeleted    : Background:=clRed
 else raise Exception.Create('Error Message');
 end;
end;

end.
