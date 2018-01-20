/// Booking server implementation
unit ServBookMain;

interface

uses
  SysUtils,
  Classes,
  SynCommons,
  mORMot,
  DomConferenceTypes,
  DomConferenceInterfaces,
  DomConferenceServices,
  InfraConferenceRepository;

type
  TBookProcessSettings = class(TSynAutoCreateFields)
  published
  end;

  TBookProcess = class(TSynPersistent)
  protected
    fSettings: TBookProcessSettings;
  public
    constructor Create(aSettings: TBookProcessSettings); reintroduce;
    property Settings: TBookProcessSettings read fSettings;
  end;

implementation

{ TBookProcess }

constructor TBookProcess.Create(aSettings: TBookProcessSettings);
begin
  inherited Create;
  fSettings := aSettings;
end;

initialization
end.