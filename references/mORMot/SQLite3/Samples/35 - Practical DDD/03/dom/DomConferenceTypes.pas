/// entities, values, aggregates for the Conference domain
unit DomConferenceTypes;

interface

uses
  SysUtils,
  Classes,
  SynCommons,
  mORMot;


{ Conference Domain Objects }

type

  TAttendeeName = type RawUTF8;
  TAttendeeFirstName = type RawUTF8;
  TAttendeeRegistrationNumber = type cardinal;
  TSessionDate = type TDateTime;

  TAttendee = class(TPersistent)
  private
    fName: TAttendeeName;
    fFirstName: TAttendeeFirstName;
    fRegistrationNumber: TAttendeeRegistrationNumber;
  public
    procedure CleanupName;
  published
    property RegistrationNumber: TAttendeeRegistrationNumber
      read fRegistrationNumber write fRegistrationNumber;
    property Name: TAttendeeName read fName write fName;
    property FirstName: TAttendeeFirstName read fFirstName write fFirstName;
  end;

  TSessionDay = class;
  TSessionDays = array of TSessionDay;
  TSessionDay = class(TPersistent)
  private
    fDay: TSessionDate;
  public
constructor Create(aDay: TSessionDate); overload; virtual;
    class function From(const Days: array of TSessionDate): TSessionDays;
  published
    property Day: TSessionDate read fDay write fDay;
  end;



implementation

{ TAttendee }

procedure TAttendee.CleanupName;
begin
  fName := Trim(fName);
  fFirstName := Trim(fFirstName);
end;

{ TSessionDay }

constructor TSessionDay.Create(aDay: TSessionDate);
begin
  inherited Create;
  fDay := aDay;
end;

class function TSessionDay.From(
  const Days: array of TSessionDate): TSessionDays;
var
  i, n: integer;
begin
  n := length(Days);
  SetLength(result, n);
  for i := 0 to n - 1 do
    result[i] := TSessionDay.Create(Days[i]);
end;

initialization
  TJSONSerializer.RegisterObjArrayForJSON([
    TypeInfo(TSessionDays), TSessionDay]);
end.
