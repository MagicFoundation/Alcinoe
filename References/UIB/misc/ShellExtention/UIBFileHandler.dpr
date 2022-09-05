library UIBFileHandler;

uses
  SysUtils,
  Classes,
  ComServ,
  main in 'main.pas',
  Backup in 'Backup.pas' {BackupForm},
  Restore in 'Restore.pas' {RestoreForm},
  Infos in 'Infos.pas' {InfosForm},
  Shutdown in 'Shutdown.pas' {ShutDownForm},
  Clone in 'Clone.pas' {CloneForm},
  Options in 'Options.pas' {OptionsForm},
  Pump in 'Pump.pas' {PumpForm};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin
end.

