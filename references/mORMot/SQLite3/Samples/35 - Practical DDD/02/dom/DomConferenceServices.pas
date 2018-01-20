/// Conference Domain services implementation
unit DomConferenceServices;

interface

uses
  SysUtils,
  Classes,
  SynCommons,
  mORMot,
  DomConferenceTypes,
  DomConferenceInterfaces,
  DomConferenceDepend;

type
  TConferenceBooking = class(TInterfacedObject, IConferenceBooking)
  protected
    fRepository: IBookingRepository;
  public
    constructor Create(aRepository: IBookingRepository); reintroduce;
    // IConferenceBooking methods below
    function RegisterAttendee(var Attendee: TAttendee;
      const Days: TSessionDays): TRegisterAttendee;
  end;

implementation

{ TConferenceBooking }

constructor TConferenceBooking.Create(aRepository: IBookingRepository);
begin
  inherited Create;
  fRepository := aRepository;
end;

function TConferenceBooking.RegisterAttendee(var Attendee: TAttendee;
  const Days: TSessionDays): TRegisterAttendee;
var
  number: TAttendeeRegistrationNumber;
  res: TBookingRepositoryError;
begin
  Attendee.CleanupName;
  if (Attendee.Name = '') or (Attendee.FirstName = '') then
    exit(raMissingField);
  //if length(Days) = 0 then
  //  exit(raMissingField);
  res := fRepository.SaveNewRegistration(Attendee, number);
  if res = brSuccess then begin
    Attendee.RegistrationNumber := number;
    result := raSuccess;
  end
  else
    result := raPersistenceError;
end;

initialization
end.
