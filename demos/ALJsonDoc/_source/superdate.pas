unit superdate;

interface

uses
  supertypes, supertimezone;

function JavaToDelphiDateTime(const dt: Int64; const TimeZone: SOString = ''): TDateTime;
function DelphiToJavaDateTime(const dt: TDateTime; const TimeZone: SOString = ''): Int64;
function JavaDateTimeToISO8601Date(const dt: Int64; const TimeZone: SOString = ''): SOString;
function DelphiDateTimeToISO8601Date(const dt: TDateTime; const TimeZone: SOString = ''): SOString;
function ISO8601DateToJavaDateTime(const str: SOString; var ms: Int64; const TimeZone: SOString = ''): Boolean;
function ISO8601DateToDelphiDateTime(const str: SOString; var dt: TDateTime; const TimeZone: SOString = ''): Boolean;

implementation

function JavaToDelphiDateTime(const dt: Int64; const TimeZone: SOString = ''): TDateTime;
begin
  Result := TSuperTimeZone.Zone[TimeZone].JavaToDelphi(dt);
end;

function DelphiToJavaDateTime(const dt: TDateTime; const TimeZone: SOString = ''): Int64;
begin
  Result := TSuperTimeZone.Zone[TimeZone].DelphiToJava(dt);
end;

function JavaDateTimeToISO8601Date(const dt: Int64; const TimeZone: SOString = ''): SOString;
begin
  Result := TSuperTimeZone.Zone[TimeZone].JavaToISO8601(dt);
end;

function DelphiDateTimeToISO8601Date(const dt: TDateTime; const TimeZone: SOString = ''): SOString;
begin
  Result := TSuperTimeZone.Zone[TimeZone].DelphiToISO8601(dt);
end;

function ISO8601DateToJavaDateTime(const str: SOString; var ms: Int64; const TimeZone: SOString = ''): Boolean;
begin
  Result := TSuperTimeZone.Zone[TimeZone].ISO8601ToJava(str, ms);
end;

function ISO8601DateToDelphiDateTime(const str: SOString; var dt: TDateTime; const TimeZone: SOString = ''): Boolean;
begin
  Result := TSuperTimeZone.Zone[TimeZone].ISO8601ToDelphi(str, dt);
end;

end.
