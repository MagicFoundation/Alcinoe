library StackTrackDLLsDynamicLibrary;

uses
  SysUtils,
  JclHookExcept,
  StackTrackDLLsDynamicUnit in 'StackTrackDLLsDynamicUnit.pas';

{$R *.res}

exports
  Error1, Error2;

begin
  JclInitializeLibrariesHookExcept;
end.
