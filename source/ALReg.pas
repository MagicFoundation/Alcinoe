unit ALReg;

interface

procedure Register;

implementation

uses DesignIntf;

procedure Register;
begin
  //This forces the package to load at startup to allow designer support
  ForceDemandLoadState(dlDisable);
end;

end.
