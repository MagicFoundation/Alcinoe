unit StackTraceViewerDummyExampleReg;

interface

procedure Register;

implementation

uses
  JclStackTraceViewerAPI, DummyLineNumberTranslator, DummyRevisionProvider;

var
  LineNumberTranslatorIndex: Integer = -1;
  RevisionProviderIndex: Integer = -1;

procedure Register;
begin
  RegisterLineNumberTranslator(TDummyLineNumberTranslator.Create);
  RegisterRevisionProvider(TDummyRevisionProvider.Create);
end;

initialization

finalization
  if LineNumberTranslatorIndex <> -1 then
    UnregisterLineNumberTranslator(LineNumberTranslatorIndex);
  if RevisionProviderIndex <> -1 then
    UnregisterRevisionProvider(RevisionProviderIndex);

end.