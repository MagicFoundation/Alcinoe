unit Alcinoe.FMX.ErrorReporting;

interface

{$I Alcinoe.inc}

uses
  system.SysUtils,
  system.Messaging,
  Alcinoe.Common;

type

  {~~~~~~~~~~~~~~~~~~~~~~~}
  TALErrorReporting = class
  private
    class function CreateInstance: TALErrorReporting;
    class function GetInstance: TALErrorReporting; static;
  protected
    class var FInstance: TALErrorReporting;
  public
    type
      TCreateInstanceFunc = function: TALErrorReporting;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALErrorReporting read GetInstance;
    class function HasInstance: Boolean; inline;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure HandleExceptionReportMessage(const Sender: TObject; const M: TMessage); virtual;
    procedure CustomLogException(Const Tag: String; Const E: Exception; const &Type: TalLogType); virtual;
  end;

implementation

uses
  FMX.Forms,
  Grijjy.ErrorReporting,
  Alcinoe.StringUtils;

{***********************************}
constructor TALErrorReporting.Create;
begin
  inherited Create;
  TgoExceptionReporter.MaxCallStackDepth := 50;
  Application.OnException := TgoExceptionReporter.ExceptionHandler;
  TMessageManager.DefaultManager.SubscribeToMessage(TgoExceptionReportMessage, HandleExceptionReportMessage);
  ALCustomLogExceptionProc := CustomLogException;
end;

{***********************************}
destructor TALErrorReporting.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TgoExceptionReportMessage, HandleExceptionReportMessage);
  ALCustomLogExceptionProc := nil;
  inherited Destroy;
end;

{*****************************************************************}
class function TALErrorReporting.CreateInstance: TALErrorReporting;
begin
  result := TALErrorReporting.Create;
end;

{**************************************************************}
class function TALErrorReporting.GetInstance: TALErrorReporting;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance)
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALErrorReporting.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{*************************************************************************************************}
procedure TALErrorReporting.HandleExceptionReportMessage(const Sender: TObject; const M: TMessage);
begin
  var LExceptionReport := TgoExceptionReportMessage(M).Report;
  ALLog('Unhandled Exception', ALTrim(LExceptionReport.Report), TALLogType.ERROR);
end;

{*************************************************************************************************************}
procedure TALErrorReporting.CustomLogException(Const Tag: String; Const E: Exception; const &Type: TalLogType);
begin
  Var LExceptionReport := TgoExceptionReporter.BuildExceptionReport(E, ExceptAddr);
  if LExceptionReport = nil then
    ALLog(Tag, E.message, &Type)
  else
    ALLog(Tag, LExceptionReport.Report, &Type)
end;

initialization
  TALErrorReporting.FInstance := nil;
  TALErrorReporting.CreateInstanceFunc := @TALErrorReporting.CreateInstance;

finalization
  ALFreeAndNil(TALErrorReporting.FInstance);

end.
