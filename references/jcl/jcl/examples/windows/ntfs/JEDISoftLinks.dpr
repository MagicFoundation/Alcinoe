{$R JEDISoftLinks.TLB}

library JEDISoftLinks;

uses
  ComServ,
  SoftLinkDragDropHandler in 'SoftLinkDragDropHandler.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin
end.
