unit Alcinoe.FMX.ErrorReporting;

interface

{$I Alcinoe.inc}

uses
  system.SysUtils,
  system.Messaging,
  Alcinoe.Common;

type

  {~~~~~~~~~~~~~~~~~~~~~~}
  TALErrorReporter = class
  private
    class function CreateInstance: TALErrorReporter;
    class function GetInstance: TALErrorReporter; static;
  protected
    class var FInstance: TALErrorReporter;
  public
    type
      TCreateInstanceFunc = function: TALErrorReporter;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALErrorReporter read GetInstance;
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

{**********************************}
constructor TALErrorReporter.Create;
begin
  inherited Create;
  TgoExceptionReporter.MaxCallStackDepth := 50;
  Application.OnException := TgoExceptionReporter.ExceptionHandler;
  TMessageManager.DefaultManager.SubscribeToMessage(TgoExceptionReportMessage, HandleExceptionReportMessage);
  ALCustomLogExceptionProc := CustomLogException;
end;

{**********************************}
destructor TALErrorReporter.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TgoExceptionReportMessage, HandleExceptionReportMessage);
  ALCustomLogExceptionProc := nil;
  inherited Destroy;
end;

{***************************************************************}
class function TALErrorReporter.CreateInstance: TALErrorReporter;
begin
  result := TALErrorReporter.Create;
end;

{************************************************************}
class function TALErrorReporter.GetInstance: TALErrorReporter;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance)
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALErrorReporter.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{************************************************************************************************}
procedure TALErrorReporter.HandleExceptionReportMessage(const Sender: TObject; const M: TMessage);
begin
  var LExceptionReport := TgoExceptionReportMessage(M).Report;
  ALLog('Unhandled Exception', ALAdjustLineBreaks(LExceptionReport.Report), TALLogType.ERROR);
end;

{************************************************************************************************************}
procedure TALErrorReporter.CustomLogException(Const Tag: String; Const E: Exception; const &Type: TalLogType);
begin
  Var LExceptionReport := TgoExceptionReporter.BuildExceptionReport(E, ExceptAddr);
  if LExceptionReport = nil then
    ALLog(Tag, E.message, &Type)
  else
    ALLog(Tag, ALAdjustLineBreaks(LExceptionReport.Report), &Type);
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.ErrorReporting','initialization');
  {$ENDIF}
  TALErrorReporter.FInstance := nil;
  TALErrorReporter.CreateInstanceFunc := @TALErrorReporter.CreateInstance;

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.FMX.ErrorReporting','finalization');
  {$ENDIF}
  ALFreeAndNil(TALErrorReporter.FInstance);

end.