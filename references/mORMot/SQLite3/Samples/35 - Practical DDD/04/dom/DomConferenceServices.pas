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
    function RegisterAttendee(const Name: TAttendeeName;
      FirstName: TAttendeeFirstName; const Days: TSessionDays;
      out Attendee: TAttendee): TRegisterAttendee;
  end;

implementation

{ TConferenceBooking }

constructor TConferenceBooking.Create(aRepository: IBookingRepository);
begin
  inherited Create;
  fRepository := aRepository;
end;

function TConferenceBooking.RegisterAttendee(const Name: TAttendeeName;
  FirstName: TAttendeeFirstName; const Days: TSessionDays;
  out Attendee: TAttendee): TRegisterAttendee;
const
  // this kind of structures won't compile any more if you add items to
  // TBookingRepositoryError: so it is a safe way to enforce error coverage
  RepoToDomain: array[TBookingRepositoryError] of TRegisterAttendee =
    // brSuccess, brDuplicatedInfo, brWriteFailure
    (raSuccess, raAlreadyRegistered, raPersistenceError);
var
  number: TAttendeeRegistrationNumber;
  res: TBookingRepositoryError;
begin
  result := raMissingField;
  if Days = nil then
    exit;
  Attendee.Name := Name;
  Attendee.FirstName := FirstName;
  Attendee.CleanupName;
  if (Attendee.Name = '') or (Attendee.FirstName = '') then
    exit;
  res := fRepository.SaveNewRegistration(Attendee, Days, number);
  result := RepoToDomain[res];
  if result = raSuccess then
    Attendee.RegistrationNumber := number;
end;

initialization
end.
