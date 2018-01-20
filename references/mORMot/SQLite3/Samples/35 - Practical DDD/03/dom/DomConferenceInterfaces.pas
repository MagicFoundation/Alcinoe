/// Conference Domain services interfaces definition
unit DomConferenceInterfaces;

interface

uses
  SysUtils,
  Classes,
  SynCommons,
  mORMot,
  DomConferenceTypes;

{ Conference Domain Services }

type
  TRegisterAttendee = (raSuccess, raMissingField, raInvalidField,
    raAlreadyRegistered, raPersistenceError);

  IConferenceBooking = interface(IInvokable)
    ['{0A128982-38E3-406E-B7B4-7FE212552BBF}']
    function RegisterAttendee(const Name: TAttendeeName;
      FirstName: TAttendeeFirstName; const Days: TSessionDays;
      out Attendee: TAttendee): TRegisterAttendee;
  end;


implementation

initialization
  TJSONSerializer.RegisterObjArrayForJSON([
    ]);
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IConferenceBooking)
    ]);
end.
