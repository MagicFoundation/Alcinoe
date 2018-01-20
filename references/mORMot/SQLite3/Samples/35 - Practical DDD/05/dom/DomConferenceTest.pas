/// Conference Domain unit tests
unit DomConferenceTest;

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
  DomConferenceDepend;

type
  TTestConference = class(TSynTestCase)
  protected
  published
    procedure DomainTypes;
    procedure DomainBooking;
  end;


implementation

{ TTestConference }

procedure TTestConference.DomainTypes;
var
  a: TAttendee;
begin
  a := TAttendee.Create;
  try
    a.FirstName := ' abc';
    a.Name := 'def ';
    a.CleanupName;
    Check(a.FirstName = 'abc');
    Check(a.Name = 'def');
  finally
    a.Free;
  end;
end;

procedure TTestConference.DomainBooking;
var
  book: IConferenceBooking;
  a: TAttendee;
  days: TSessionDays;
  res: TRegisterAttendee;
  repo: IBookingRepository;
begin
  TInterfaceStub.Create(IBookingRepository, repo);
  book := TConferenceBooking.Create(repo);
  a := TAttendee.Create;
  try
    res := book.RegisterAttendee('', '', nil, a);
    check(res = raMissingField);
  finally
    a.Free;
  end;
  a := TAttendee.Create;
  try
    res := book.RegisterAttendee('abc', ' def', nil, a);
    check(res = raMissingField);
  finally
    a.Free;
  end;
  a := TAttendee.Create;
  try
    days := TSessionDay.From([0, 1, 2]);
    res := book.RegisterAttendee('abc', ' def', days, a);
    check(res = raSuccess);
    check(a.Name = 'abc');
    check(a.FirstName = 'def');
  finally
    a.Free;
    ObjArrayClear(days);
  end;
  a := TAttendee.Create;
  try // IBookingRepository.RetrieveRegistration stub will return default false
    check(book.SearchRegistration('abc', 'def', days, a) = srNotFound);
  finally
    a.Free;
    ObjArrayClear(days);
  end;
end;

initialization
end.