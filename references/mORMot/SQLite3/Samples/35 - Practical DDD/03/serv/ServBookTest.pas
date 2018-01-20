/// unit tests for the Booking server
unit ServBookTest;

interface

uses
  SysUtils,
  Classes,
  SynCommons,
  SynTests,
  mORMot,
  DomConferenceTypes,
  DomConferenceInterfaces,
  DomConferenceServices,
  InfraConferenceRepository,
  ServBookMain;

type
  TTestBookingApplication = class(TSynTestCase)
  published
    procedure RunService;
    procedure ApplicationTest;
    procedure ShutdownService;
  end;

implementation

{ TTestBookingApplication }

procedure TTestBookingApplication.RunService;
begin

end;

procedure TTestBookingApplication.ApplicationTest;
begin

end;

procedure TTestBookingApplication.ShutdownService;
begin

end;

initialization
end.