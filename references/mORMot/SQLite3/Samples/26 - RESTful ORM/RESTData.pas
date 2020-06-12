unit RESTData;

{$I Synopse.inc} // define HASINLINE and some FPC-specific options

interface

uses
  SynCommons,
  SynTable, // for TSynValidate
  mORMot;

type
  TSQLRecordWithModTimeAndMetaData = class(TSQLRecord)
  protected
    fCreated: TCreateTime;
    fModified: TModTime;
    fMetaData: variant;
  published
    property Modified: TModTime read fModified write fModified;
    property Created: TCreateTime read fCreated write fCreated;
    property MetaData: variant read fMetaData write fMetaData;
  end;

  TSQLNoteKind = class(TSQLRecordWithModTimeAndMetaData)
  protected
    fName: RawUTF8;
  public
    class procedure InitializeTable(Server: TSQLRestServer;
      const FieldName: RawUTF8; Options: TSQLInitializeTableOptions); override;
  published
    property Name: RawUTF8 read fName write fName stored AS_UNIQUE;
  end;

  TSQLNote = class(TSQLRecordWithModTimeAndMetaData)
  protected
    fIdent: RawUTF8;
    fKind: TSQLNoteKind;
    fParent: TSQLNote;
  published
    property Ident: RawUTF8 read fIdent write fIdent;
    property Kind: TSQLNoteKind read fKind write fKind;
    property Parent: TSQLNote read fParent write fParent;
  end;

  TSQLNoteFile = class(TSQLRecordWithModTimeAndMetaData)
  protected
    fFileName: RawUTF8;
    fNote: TSQLNote;
  published
    property FileName: RawUTF8 read fFileName write fFileName;
    property Note: TSQLNote read fNote write fNote;
  end;

  TSQLUser = class(TSQLAuthUser)
  protected
    fMetaData: variant;
  published
    property MetaData: variant read fMetaData write fMetaData;
  end;


function DataModel(const RootURI: RawUTF8): TSQLModel;

const
  HTTP_PORT = '888'; 

  
implementation

function DataModel(const RootURI: RawUTF8): TSQLModel;
begin
  result := TSQLModel.Create(
    [TSQLAuthGroup,TSQLUser,TSQLNoteKind,TSQLNote,TSQLNoteFile],
    RootURI);
  TSQLNoteKind.AddFilterOrValidate('Name',TSynValidateText.Create('{MinLength:3}'));
  TSQLNote.AddFilterOrValidate('Ident',TSynValidateText.Create('{MinLength:3}'));
  TSQLNoteFile.AddFilterOrValidate('FileName',TSynValidateNonVoidText.Create);
  TSQLNoteFile.AddFilterOrValidate('FileName',TSynValidateText.Create('{MaxPunctCount:0}'));
end;

{ TSQLNoteKind }

class procedure TSQLNoteKind.InitializeTable(Server: TSQLRestServer;
  const FieldName: RawUTF8; Options: TSQLInitializeTableOptions);
var Kind: TSQLNoteKind;
begin
  inherited;
  Kind := TSQLNoteKind.Create;
  Kind.Name := 'PostIt';
  Server.Add(Kind,true);
  Kind.Name := 'Todo';
  Server.Add(Kind,true);
  Kind.Free;
end;

end.