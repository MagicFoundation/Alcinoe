unit RESTModel;

interface

uses
  SynCommons,
  SynTable, // for TSynValidateText
  mORMot;

type
  TPerson = class(TSQLRecord) // TSQLRecord has already ID: integer primary key
  private
    fName: RawUTF8;
  published
    /// ORM will create a NAME VARCHAR(80) column
    property Name: RawUTF8 index 80 read fName write fName; 
  end;

function DataModel: TSQLModel;

const
  SERVER_ROOT = 'root';
  SERVER_PORT = '888';


implementation

function DataModel: TSQLModel;
begin
  result := TSQLModel.Create([TPerson],SERVER_ROOT);
  TPerson.AddFilterOrValidate('Name',TSynValidateText.Create); // ensure exists
end;


end.
