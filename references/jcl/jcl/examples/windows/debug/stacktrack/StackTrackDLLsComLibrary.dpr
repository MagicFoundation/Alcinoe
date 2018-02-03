library StackTrackDLLsComLibrary;

uses
  ComServ,
  JclHookExcept,
  StackTrackDLLsComLibrary_TLB in 'StackTrackDLLsComLibrary_TLB.pas',
  StackTrackDLLsComUnit in 'StackTrackDLLsComUnit.pas' {StackTrackDllsTest: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
  JclInitializeLibrariesHookExcept;
end.
