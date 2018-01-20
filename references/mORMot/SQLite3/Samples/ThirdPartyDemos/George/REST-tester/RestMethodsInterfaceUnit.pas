unit RestMethodsInterfaceUnit;

interface

uses
  mORMot;

type
  rCustomRecord = record
    ResultCode: integer;
    ResultStr: string;
    ResultArray: array of string;
    ResultTimeStamp: TDateTime;
  end;

  rCustomComplicatedRecord = record
    SimpleString: string;
    SimpleInteger: integer;
    AnotherRecord: rCustomRecord;
  end;

  IRestMethods = interface(IInvokable)
    ['{4EB49814-A4A9-40D2-B85A-137DDF43C72C}']
    function HelloWorld(): string;
    function Sum(val1, val2: Double): Double;
    function GetCustomRecord(): rCustomRecord;
    function SendCustomRecord(const CustomResult: rCustomRecord): Boolean;
    function SendMultipleCustomRecords(const CustomResult: rCustomRecord; const CustomComplicatedRecord: rCustomComplicatedRecord): Boolean;
    function GetMethodCustomResult(): TServiceCustomAnswer; // without default {result:[]}
    // function ProperErrorHanding(): boolean;
  end;

const
  ROOT_NAME = 'service';
  SERVICE_INSTANCE_IMPLEMENTATION = TServiceInstanceImplementation.sicSingle;
  NAMED_PIPE_NAME = 'Rest_Test.0001';
  CONNECTION_TIMEOUT = 3000; // Default mORMot value

implementation

initialization

TInterfaceFactory.RegisterInterfaces([TypeInfo(IRestMethods)]); // to use directly IRestMethods instead of TypeInfo(IRestMethods)

end.
