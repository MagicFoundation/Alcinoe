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
    property RegistrationNumber: TAttendeeRegistrationNumber read fRegistrationNumber
      write fRegistrationNumber;
    property Name: TAttendeeName read fName write fName;
    property FirstName: TAttendeeFirstName read fFirstName write fFirstName;
  end;

  TSessionDay = class(TPersistent)
  private
    fDay: TSessionDate;
  published
    property Day: TSessionDate read fDay write fDay;
  end;
  TSessionDays = array of TSessionDay;



implementation

{ TAttendee }

procedure TAttendee.CleanupName;
begin
  fName := Trim(fName);
  fFirstName := Trim(fFirstName);
end;

initialization
  TJSONSerializer.RegisterObjArrayForJSON([
    TypeInfo(TSessionDays), TSessionDay]);
end.
