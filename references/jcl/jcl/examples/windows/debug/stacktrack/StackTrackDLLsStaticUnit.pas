unit StackTrackDLLsStaticUnit;

interface

procedure Error1; stdcall;
procedure Error2; stdcall;

implementation

uses
  SysUtils;

procedure Error1_1;
begin
  StrToInt('x');
end;

procedure Error1; stdcall;
begin
  Error1_1;
end;

procedure Error2; stdcall;
begin
  raise Exception.Create('Exception from StaticLibrary.dll');
end;

end.
