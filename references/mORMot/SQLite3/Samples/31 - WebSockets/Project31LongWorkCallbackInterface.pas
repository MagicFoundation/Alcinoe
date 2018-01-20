unit Project31LongWorkCallbackInterface;

interface

uses
  SysUtils,
  SynCommons,
  mORMot;

type
  ILongWorkCallback = interface(IInvokable)
    ['{425BF199-19C7-4B2B-B1A4-A5BE7A9A4748}']
    procedure WorkFinished(const workName: string; timeTaken: integer);
    procedure WorkFailed(const workName, error: string);
  end;
  
  ILongWorkService = interface(IInvokable)
    ['{09FDFCEF-86E5-4077-80D8-661801A9224A}']
    procedure StartWork(const workName: string; const onFinish: ILongWorkCallback);
    function TotalWorkCount: Integer;
  end;

const
  PROJECT31_TRANSMISSION_KEY = 'longwork_privatekey';

  
implementation

initialization
  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(ILongWorkService),TypeInfo(ILongWorkCallback)]);
end.
