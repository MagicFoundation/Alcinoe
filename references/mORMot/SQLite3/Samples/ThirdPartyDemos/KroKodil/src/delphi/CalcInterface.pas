unit CalcInterface;

interface

uses FileCollect;

/// some common definitions shared by both client and server side

type
  ICalculator = interface(IInvokable)
    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
    function Add(n1,n2: integer): integer;
    function GetFileList(path: String; out lst: TFlileCollection): Boolean;
  end;

const
  ROOT_NAME = 'root';
  PORT_HTTP = '8080';
  PORT_HTTPS = '8443';
  APPLICATION_NAME = 'RestService';
  SERVICE_NAME = 'service';

implementation

uses
  mORMot;

initialization
  // so that we could use directly ICalculator instead of TypeInfo(ICalculator)
  TInterfaceFactory.RegisterInterfaces([TypeInfo(ICalculator)]);
end.
