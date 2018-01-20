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
  res: TRegisterAttendee;
  repo: IBookingRepository;
begin
  TInterfaceStub.Create(TypeInfo(IBookingRepository), repo);
  book := TConferenceBooking.Create(repo);
  a := TAttendee.Create;
  try
    res := book.RegisterAttendee(a, nil);
    check(res = raMissingField);
  finally
    a.Free;
  end;
  a := TAttendee.Create;
  try
    a.FirstName := 'abc';
    a.Name := ' def';
    res := book.RegisterAttendee(a, nil);
    check(res = raSuccess);
  finally
    a.Free;
  end;
end;

initialization
end.