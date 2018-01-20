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
  a, b: TAttendee;
  days1, days2: TSessionDays;
  res: TRegisterAttendee;
begin
  a := TAttendee.Create;
  b := TAttendee.Create;
  try
    days1 := TSessionDay.From([0, 1, 2]);
    res := fProcess.Booking.RegisterAttendee('abc', ' def', days1, a);
    if res = raAlreadyRegistered then // works only first time (blank DB)
      res := fProcess.Booking.RegisterAttendee(CardinalToHex(UnixTimeUTC),
        RandomIdentifier(10), days1, a);
    check(res = raSuccess);
    check(a.Name <> '');
    check(length(a.FirstName) = 10);
    check(fProcess.Booking.SearchRegistration(a.Name, a.FirstName, days2, b) = srFound);
    check(a.Name = b.Name);
    check(a.RegistrationNumber = b.RegistrationNumber);
    check(DynArrayEquals(TypeInfo(TSessionDays), days1, days2));
  finally
    a.Free;
    b.Free;
    ObjArrayClear(days1);
    ObjArrayClear(days2);
  end;
end;

procedure TTestBookingApplication.ShutdownService;
begin
  FreeAndNil(fProcess);
  FreeAndNil(fSettings);
end;

initialization
end.