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
  DomConferenceServices;

type
  TTestConference = class(TSynTestCase)
  protected
  published
    procedure DomainTypes;
    procedure DomainBooking;
  end;


implementation

{ TConferenceTest }

procedure TTestConference.DomainTypes;
begin

end;

procedure TTestConference.DomainBooking;
begin

end;

initialization
end.