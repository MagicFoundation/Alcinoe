/// Booking server implementation
unit ServBookMain;

interface

uses
  SysUtils,
  Classes,
  SynCommons,
  mORMot,
  SynSQLite3,
  mORMotSQLite3,
  mORMotDB,
  dddInfraApps,
  DomConferenceTypes,
  DomConferenceInterfaces,
  DomConferenceServices,
  DomConferenceDepend,
  InfraConferenceRepository;

type
  TBookProcessSettings = class(TSynAutoCreateFields)
  private
    fStore: TSynConnectionDefinition;
  public
    constructor Create; override;
  published
    property Store: TSynConnectionDefinition read fStore;
  end;

  TBookProcess = class(TSynPersistent)
  protected
    fSettings: TBookProcessSettings;
    fRest: TSQLRest;
    fBooking: IConferenceBooking;
  public
    constructor Create(aSettings: TBookProcessSettings); reintroduce;
    destructor Destroy; override;
    property Booking: IConferenceBooking read fBooking;
    property Settings: TBookProcessSettings read fSettings;
  end;


implementation

{ TBookProcessSettings }

constructor TBookProcessSettings.Create;
begin
  inherited;
  // use a local SQlite3 database file by default
  fStore.Kind := 'TSQLRestServerDB'; // change Kind to switch to another engine
  fStore.ServerName := ChangeFileExt(ExeVersion.ProgramFileName, '.db');
end;


{ TBookProcess }

constructor TBookProcess.Create(aSettings: TBookProcessSettings);
begin
  inherited Create;
  fSettings := aSettings;
  fRest := TSQLRestExternalDBCreate(
    TSQLModel.Create([TSQLBooking], 'book'), fSettings.Store, false, []);
  fRest.Model.Owner := fRest;
  if fRest is TSQLRestServerDB then
    with TSQLRestServerDB(fRest) do begin // may be a client in settings :)
      DB.Synchronous := smOff; // faster exclusive access to the file
      DB.LockingMode := lmExclusive;
      CreateMissingTables;     // will create the Booking table, if necessary
    end;
  fBooking := TConferenceBooking.Create(TORMBookingRepository.Create(fRest));
end;

destructor TBookProcess.Destroy;
begin
  inherited;
  fBooking := nil; // before fRest
  fRest.Free;
end;

initialization
end.