unit MAinFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,mORMot,mORMotSQLite3, SynSQLite3Static,SynCommons,
  Vcl.StdCtrls, Data.DB, Datasnap.DBClient,Contnrs, Vcl.Grids, Vcl.DBGrids,
  Xml.xmldom, Datasnap.Provider,
  Xmlxform;

type
  TMainForm = class(TForm)
    BtnSample1: TButton;
    BtnSample2: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnSample1Click(Sender: TObject);
    procedure BtnSample2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Model           : TSQLModel;
    DB              : TSQLRestServerDB;
    procedure CreateDefaults;
  end;

type TPhoneType     = (ptWork, ptHome,ptFax,ptSMS);
     TPhoneTypeSet  = set of TPhoneType;

type TPhone     =  packed record
                    Number : RawUTF8;
                    PType  : TPhoneTypeSet;
                   end;
     TPhoneArr  =  Array of TPhone;
type TGender    = (gnUnknown,gnMale,gnFemale);

type TSQLPerson = class(TSQLRecord)
                  private
                   fName    : RawUTF8;
                   fInt     : Integer;
                   fGender  : TGender;
                   fPhones  : TPhoneArr;
                  public
                  protected
                  published
                   property Name    : RawUTF8             read fName     write fName;
                   property Int     : Integer             read fInt      write fInt;
                   property Phones  : TPhoneArr           read fPhones   write fPhones;
                   property Gender  : TGender             read fGender   write fGender;
                  end;

type TSQLChild  = class(TSQLRecord)
                       private
                        fParent      : TID;
                        fChildName   : RawUTF8;
                        fChildGender : TGender;
                       published
                        property Parent       : TID        read fParent      write fParent;
                        property ChildName    : RawUTF8    read fChildName   write fChildName;
                        property ChildGender  : TGender    read fChildGender write fChildGender;
                      end;

var MainForm: TMainForm;

implementation

{$R *.dfm}

uses SysUtils,RTTI, TypInfo, ORMCDS, SampleForm1U, SampleForm2U;


procedure TMainForm.BtnSample1Click(Sender: TObject);
begin
 Application.CreateForm(TSampleForm1,SampleForm1);
 SampleForm1.ShowModal;
 SampleForm1.Release;
end;

procedure TMainForm.BtnSample2Click(Sender: TObject);
begin
 Application.CreateForm(TSampleForm1,SampleForm1);
 SampleForm1.ShowModal;
 SampleForm1.Release;
end;

procedure TMainForm.CreateDefaults;
var Person : TSQLPerson;
    DA     : TDynArray;
    Ph     : TPhone;
    Ch     : TSQLChild;
begin
 Person:=TSQLPerson.Create;
 Person.Name:='Guy';
 Person.Int :=1;
 DA:=Person.DynArray('Phones');
 Ph.Number:='1234';
 Ph.PType :=[ptWork];
 DA.Add(Ph);
 Ph.Number:='5678';
 Ph.PType :=[ptHome,ptSMS];
 DA.Add(Ph);
 DB.Add(Person,True);

 Ch:=TSQLChild.Create;
 Ch.Parent   :=Person.ID;
 Ch.ChildName:='Boy1';
 Ch.ChildGender:=gnMale;
 DB.Add(Ch,True);
 Ch.ChildName:='Boy2';
 Ch.ChildGender:=gnMale;
 DB.Add(Ch,True);
 Person.Free;
 Ch.Free;

 Person:=TSQLPerson.Create;
 Person.Name:='Gal';
 Person.Int :=1;
 DA:=Person.DynArray('Phones');
 Ph.Number:='AA1234';
 Ph.PType :=[ptHome];
 DA.Add(Ph);
 Ph.Number:='BB5678';
 Ph.PType :=[ptSMS];
 DA.Add(Ph);
 DB.Add(Person,True);

 Ch:=TSQLChild.Create;
 Ch.Parent   :=Person.ID;
 Ch.ChildName:='GirlA';
 Ch.ChildGender:=gnFeMale;
 DB.Add(Ch,True);
 Ch.ChildName:='ChildB';
 Ch.ChildGender:=gnUnknown;
 DB.Add(Ch,True);
 Person.Free;
 Ch.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var FN    : String;
    IsNew : Boolean;
begin
  FN:=ChangeFileExt(paramstr(0),'.db3');
  IsNew:=not FileExists(FN);
  {  DeleteFile(FN);}
  Model := TSQLModel.Create([TSQLChild,TSQLPerson]);
  DB    := TSQLRestServerDB.Create(Model,FN);
  TSQLRestServerDB(DB).CreateMissingTables(0);
  if IsNew
     then CreateDefaults;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
 DB.Free;
 Model.Free;
end;

end.

