TSynRestDataset
===============

By *EMartin* (Esteban Martin).


# Presentation

Migrating from *RemObjects* to *mORMot* I had to implement a GUI functionality that *RemObjects* has, an editable dataset connected through URL (RO version 3 use SOAP and other components adapters, etc.).

My implementation is basic and the most probably is not the best, but works for me, the same use RESTful URL for get and update data, also get data from a *mORMot* interface based services returning a *mORMot* JSON array but cannot update because the table not exists.

In this folder there are two units: `SynRestVCL.pas` and `SynRestMidasVCL.pas`, both have some duplicated code from its counterpart (`SynDBVCL.pas` and `SynDBMidasVCL.pas`) and the others, but the rest are modifications with use of RESTful instead of the `TSQLDBConnection` (this require the database client installed in the client machine).

A `TSQLModel` is required because the `TSynRestDataset` get the fields definition column type and size from this. Also is used from the `TSQLRecord` the defined validations (I used `InternalDefineModel`) and the `ComputeFieldsBeforeWrite` (I used this for default values).

This was developed with Delphi 7 on Windows 7 and probably (almost sure) is not cross platform.

If this serves for others may be the best option will be that *ab* integrate this in the framework and make this code more *mORMot*. Meanwhile I will update on the google drive.
I hope this is helpful to someone.

# Example 1: from a table

    // defining the table
    TSQLRecordTest = class(TSQLRecord)
    private
      fDecimal: Double;
      fNumber: Double;
      fTestID: Integer;
      fText: RawUTF8;
      fDateTime: TDateTime;
    protected
      class procedure InternalDefineModel(Props: TSQLRecordProperties); override;
    public
      procedure ComputeFieldsBeforeWrite(aRest: TSQLRest; aOccasion: TSQLEvent); override;
    published
      property Test_ID: Integer read fTestID write fTestID;
      property Text: RawUTF8 index 255 read fText write fText;
      property Date_Time: TDateTime read fDateTime write fDateTime;
      property Number: Double read fNumber write fNumber;
      property Decimal_: Double read fDecimal write fDecimal;
    end;

    ...

    { TSQLRecordTest }

    procedure TSQLRecordTest.ComputeFieldsBeforeWrite(aRest: TSQLRest; aOccasion: TSQLEvent);
    begin
      inherited;
      fDateTime := Now;
    end;

    class procedure TSQLRecordTest.InternalDefineModel(Props: TSQLRecordProperties);
    begin
      AddFilterNotVoidText(['Text']);
      AddFilterOrValidate('Text', TSynValidateNonNull.Create);
    end;

    // client
    type
      TForm3 = class(TForm)
        DBGrid1: TDBGrid;
        DBNavigator1: TDBNavigator;
        btnOpen: TButton;
        edtURL: TEdit;
        dsRest: TDataSource;
        procedure FormCreate(Sender: TObject);
        procedure btnOpenClick(Sender: TObject);
        procedure DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
      private
        { Private declarations }
        fRestDS: TSynRestDataset;
      public
        { Public declarations }
      end;

    ...

    procedure TForm3.FormCreate(Sender: TObject);
    begin
      fRestDS := TSynRestDataset.Create(Self);
      fRestDS.Dataset.SQLModel := TSQLModel.Create([TSQLRecordTest], 'root');
      dsRest.Dataset := fRestDS;
    end;

    procedure TForm3.btnOpenClick(Sender: TObject);
    begin
      fRestDS.Close;
      fRestDS.CommandText := edtURL.Text; // edtURL.Text = 'http://localhost:8888/root/Test/select=*
      fRestDS.Open;
      // you can filter by
      // where: fRestDS.CommandText := edtURL.Text; // edtURL.Text = 'http://localhost:8888/root/Test/select=*&where=CONDITION
      // fRestDS.Open;
      // named parameter: fRestDS.CommandText := edtURL.Text; // edtURL.Text = 'http://localhost:8888/root/Test/select=*&where=:PARAMNAME
      // fRestDS.Params.ParamByName('PARAMNAME').Value := XXX
      // fRestDS.Open;
    end;

    procedure TForm3.DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
    begin
      if (Button = nbPost) then
        fRestDS.ApplyUpdates(0);
    end;

# Example 2: from a service

     // defining the table, the service name and operation name are required
    TSQLRecordServiceName_OperationName = class(TSQLRecord)
    private
      fText: RawUTF8;
    published
      property Text: RawUTF8 index 255 read fText write fText;
    end;

    ...

    // server (the implementation)

    TServiceName =class(TInterfacedObjectWithCustomCreate, IServiceName)
    public
      ...
      // this function can also be function OperationName(const aParamName: RawUTF8): RawUTF8;
      function OperationName(const aParamName: RawUTF8; out aData: RawUTF8): Integer;
      ...
    end;

    ...

    function TServiceName.OperationName(const aParamName: RawUTF8; out aData: RawUTF8): Integer;
    begin
       Result := OK;
       aData := '[{"text":"test"},{"text":"test1"}]';    
    end;

    ...

    // client
    type
      TForm3 = class(TForm)
        DBGrid1: TDBGrid;
        DBNavigator1: TDBNavigator;
        btnOpen: TButton;
        edtURL: TEdit;
        dsRest: TDataSource;
        procedure FormCreate(Sender: TObject);
        procedure btnOpenClick(Sender: TObject);
        procedure DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
      private
        { Private declarations }
        fRestDS: TSynRestDataset;
      public
        { Public declarations }
      end;

    ...

    procedure TForm3.FormCreate(Sender: TObject);
    begin
      fRestDS := TSynRestDataset.Create(Self);
      fRestDS.Dataset.SQLModel := TSQLModel.Create([TSQLRecordServiceName_OperationName], 'root');
      dsRest.Dataset := fRestDS;
    end;

    procedure TForm3.btnOpenClick(Sender: TObject);
    begin
      fRestDS.Close;
      fRestDS.CommandText := edtURL.Text; // edtURL.Text = 'http://localhost:8888/root/ServiceName.OperationName?aParamName=XXX
      fRestDS.Open;
      // you can filter by named parameter:
      // fRestDS.CommandText := edtURL.Text; // 'http://localhost:8888/root/ServiceName.OperationName?aParamName=:aParamName
      // fRestDS.Params.ParamByName('aParamName').Value := XXX
      // fRestDS.Open;
    end;

    procedure TForm3.DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
    begin
      if (Button = nbPost) then
        fRestDS.ApplyUpdates(0); // raise an error "Cannot update data from a service"
    end;


# Forum Thread

See http://synopse.info/forum/viewtopic.php?id=2712

# License

Feel free to use and/or append to Lib and extend if needed.
