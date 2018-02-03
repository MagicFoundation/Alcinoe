library StackTrackDLLsStaticLibrary;

uses
  SysUtils,
  JclHookExcept,
  StackTrackDLLsStaticUnit in 'StackTrackDLLsStaticUnit.pas';

{$R *.res}

exports
  Error1, Error2;

begin
  JclInitializeLibrariesHookExcept;
end.
