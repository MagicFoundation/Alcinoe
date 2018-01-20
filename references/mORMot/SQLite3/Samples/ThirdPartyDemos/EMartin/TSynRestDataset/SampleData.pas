/// it's a good practice to put all data definition into a stand-alone unit
// - this unit will be shared between client and server
unit SampleData;

interface

uses
  SynCommons,
  mORMot;

type
  /// here we declare the class containing the data
  // - it just has to inherits from TSQLRecord, and the published
  // properties will be used for the ORM (and all SQL creation)
  // - the beginning of the class name must be 'TSQL' for proper table naming
  // in client/server environnment
  TSQLBiolife = class(TSQLRecord)
  private
    fSpecies_No: Integer;
    fCategory: RawUTF8;
    fCommon_Name: RawUTF8;
    fSpecies_Name: RawUTF8;
    fLength_cm: double;
    fLength_in: double;
    fNotes: TSQLRawBlob;
    fGraphic: TSQLRawBlob;
    fSom: TSQLRawBlob;
  published
    property Species_No: Integer read fSpecies_No write fSpecies_No;
    property Category: RawUTF8 index 15 read fCategory write fCategory;
    property Common_Name: RawUTF8 index 30 read fCommon_Name write fCommon_Name;
    property Species_Name: RawUTF8 index 40 read fSpecies_Name write fSpecies_Name;
    property Length_cm: Double read fLength_Cm write fLength_Cm;
    property Length_In: Double read fLength_In write fLength_In;
    property Notes: TSQLRawBlob read fNotes write fNotes;
    property Graphic: TSQLRawBlob read fGraphic write fGraphic;
    property Som: TSQLRawBlob read fSom write fSom;
  end;

/// an easy way to create a database model for client and server
function CreateSampleModel: TSQLModel;


implementation

function CreateSampleModel: TSQLModel;
begin
  result := TSQLModel.Create([TSQLBioLife]);
end;

end.
