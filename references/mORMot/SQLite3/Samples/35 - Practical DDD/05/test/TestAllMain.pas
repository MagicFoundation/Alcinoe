unit TestAllMain;

interface

uses
  SynCommons,
  SynTests,
  DomConferenceTest,
  ServBookTest;

type
  TTestEkon = class(TSynTestsLogged)
  published
    procedure Infrastructure;
    procedure Domain;
    procedure Applications;
  end;

implementation

{ TTestEkon }

procedure TTestEkon.Infrastructure;
begin

end;

procedure TTestEkon.Domain;
begin
  AddCase([TTestConference]);
end;

procedure TTestEkon.Applications;
begin
  AddCase([TTestBookingApplication]);
end;

end.