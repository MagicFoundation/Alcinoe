/// Conference Domain dependencies interface definition
unit DomConferenceDepend;

interface

uses
  SysUtils,
  Classes,
  SynCommons,
  mORMot,
  DomConferenceTypes;

type
  TBookingRepositoryError = (brSuccess, brWriteFailure);

  IBookingRepository = interface(IInvokable)
    ['{8E121C97-7E53-4208-BE05-1660EAD8AB43}']
    function SaveNewRegistration(const Attendee: TAttendee;
      out RegistrationNumber: TAttendeeRegistrationNumber): TBookingRepositoryError;
  end;
{
  TAPIError = (apiSuccess, apiError);

const
  DomainToAPI: array[TBookingRepositoryError] of TAPIError = (apiSuccess, apiError);
}

implementation

initialization
  TJSONSerializer.RegisterObjArrayForJSON([
    ]);
  TInterfaceFactory.RegisterInterfaces([
    ]);
end.
