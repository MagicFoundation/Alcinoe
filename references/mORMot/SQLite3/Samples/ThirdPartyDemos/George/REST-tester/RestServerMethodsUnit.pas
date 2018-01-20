unit RestServerMethodsUnit;

interface

uses
  // RTL
  SysUtils,
  Classes,
  StrUtils,
  Forms,
  Dialogs,
  Controls,
  StdCtrls,
  ExtCtrls,
  // mORMot
  mORMot,
  mORMotHttpServer,
  SynCommons,
  // Custom
  RestMethodsInterfaceUnit;

type

  TCustomRecord = record helper for rCustomRecord
    procedure FillResultFromServer();
  end;

  TRestMethods = class(TInjectableObjectRest, IRestMethods)
  public
    function HelloWorld(): string;
    function Sum(val1, val2: Double): Double;
    function GetCustomRecord(): rCustomRecord;
    function SendCustomRecord(const CustomResult: rCustomRecord): Boolean;
    function SendMultipleCustomRecords(const CustomResult: rCustomRecord; const CustomComplicatedRecord: rCustomComplicatedRecord): Boolean;
    function GetMethodCustomResult(): TServiceCustomAnswer; // without default {result:[]}
  end;

implementation

{ TCustomResultSrv }

procedure TCustomRecord.FillResultFromServer();
var
  i: Integer;
begin
  ResultCode := 200;
  ResultStr := 'Awesome';
  ResultTimeStamp := Now();
  SetLength(ResultArray, 3);
  for i := 0 to 2 do
    ResultArray[i] := 'str_' + i.ToString();
end;

{ TServiceServer }

// [!] ServiceContext can be used from any method to access low level request data

function TRestMethods.HelloWorld(): string;
begin
  Result := 'Hello world';
end;

function TRestMethods.Sum(val1, val2: Double): Double;
begin
  Result := val1 + val2;
end;

function TRestMethods.GetCustomRecord(): rCustomRecord;
begin
  Result.FillResultFromServer();
end;

function TRestMethods.SendCustomRecord(const CustomResult: rCustomRecord): Boolean;
begin
  Result := True;
end;

function TRestMethods.SendMultipleCustomRecords(const CustomResult: rCustomRecord; const CustomComplicatedRecord: rCustomComplicatedRecord): Boolean;
begin
  Result := True;
end;

function TRestMethods.GetMethodCustomResult(): TServiceCustomAnswer;
begin
  Result.Header := 'Content-type: UTF-8';
  Result.Content := 'I am custom result, no "result:[]" used.';
  Result.Status := 200;
end;

end.
