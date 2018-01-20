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
  protected
    fSettings: TBookProcessSettings;
    fProcess: TBookProcess;
  published
    procedure RunService;
    procedure ApplicationTest;
    procedure ShutdownService;
  end;


implementation

{ TTestBookingApplication }

procedure TTestBookingApplication.RunService;
begin
  fSettings := TBookProcessSettings.Create;
  fProcess := TBookProcess.Create(fSettings);
end;

procedure TTestBookingApplication.ApplicationTest;
var
  a: TAttendee;
  days: TSessionDays;
  res: TRegisterAttendee;
begin
  a := TAttendee.Create;
  try
    days := TSessionDay.From([0, 1, 2]);
    res := fProcess.Booking.RegisterAttendee('abc', ' def', days, a);
    if res = raAlreadyRegistered then // would works only first time
      res := fProcess.Booking.RegisterAttendee(CardinalToHex(UnixTimeUTC),
        RandomIdentifier(10), days, a);
    check(res = raSuccess);
  finally
    a.Free;
    ObjArrayClear(days);
  end;
end;

procedure TTestBookingApplication.ShutdownService;
begin
  FreeAndNil(fProcess);
  FreeAndNil(fSettings);
end;

initialization
end.