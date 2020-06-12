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

const
  SERVER_ROOT = 'root';
  SERVER_PORT = '888';


implementation



end.
