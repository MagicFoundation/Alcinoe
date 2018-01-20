unit TimeLogVisualizer;

// http://edn.embarcadero.com/article/40268

interface

procedure Register;

implementation

uses
  { Standard Stuff }
  System.SysUtils, System.DateUtils, SysTem.Math,
  { OTA }
  ToolsAPI,
  { mORMot }
  SynCommons;


type
  TTimeLogVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer, IOTADebuggerVisualizerValueReplacer)
  public
    function GetReplacementValue(const Expression: string; const TypeName: string; const EvalResult: string): string;
    procedure GetSupportedType(Index: Integer; var TypeName: string; var AllDescendents: Boolean);
    function GetSupportedTypeCount: Integer;
    function GetVisualizerDescription: string;
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
  end;


type
  TTypeLang = (tlDelphi, tlCpp);

  TDateTimeVisualizerType = record
    TypeName: string;
    TypeLang: TTypeLang;
  end;


const
  // If later need CPP support
  TimeLogVisualizerTypes: array[0..5] of TDateTimeVisualizerType =
  (
    (TypeName: 'TTimeLog'; TypeLang: tlDelphi),
    (TypeName: 'TModTime'; TypeLang: tlDelphi),
    (TypeName: 'TCreateTime'; TypeLang: tlDelphi),
    (TypeName: 'function: TTimeLog'; TypeLang: tlDelphi),
    (TypeName: 'function: TModTime'; TypeLang: tlDelphi),
    (TypeName: 'function: TCreateTime'; TypeLang: tlDelphi)
  );


{ TTimeLogVisualizer }

function TTimeLogVisualizer.GetReplacementValue(const Expression, TypeName, EvalResult: string): string;
var
  I : Integer;
  LLang: TTypeLang;
  LValue: TTimeLog;
  LDateTime: TDateTime;
begin
  LLang := TTypeLang(-1);
  for I := Low(TimeLogVisualizerTypes) to High(TimeLogVisualizerTypes) do
  begin
    if TypeName = TimeLogVisualizerTypes[I].TypeName then
    begin
      LLang := TimeLogVisualizerTypes[I].TypeLang;
      Break;
    end;
  end;

  if LLang = tlDelphi then
  begin
    LValue := StrToInt64Def(EvalResult, 0);

    LDateTime := PTimeLogBits(@LValue)^.ToDateTime;

    // Only time
    if IsZero(Trunc(LDateTime)) then
      Result := TimeToStr(LDateTime)
    else if not IsZero(LDateTime)  then
      Result := DateTimeToStr(LDateTime);
  end
  else if LLang = tlCpp then
  begin
    Result := EvalResult;
  end;
end;

procedure TTimeLogVisualizer.GetSupportedType(Index: Integer; var TypeName: string; var AllDescendents: Boolean);
begin
  AllDescendents := False;
  TypeName := TimeLogVisualizerTypes[Index].TypeName;
end;

function TTimeLogVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := Length(TimeLogVisualizerTypes);
end;

function TTimeLogVisualizer.GetVisualizerDescription: string;
begin
  Result := 'Shows mORMot proprietary TTimeLog Date/Time format in humanreadable format';
end;

function TTimeLogVisualizer.GetVisualizerIdentifier: string;
begin
  Result := ClassName;
end;

function TTimeLogVisualizer.GetVisualizerName: string;
begin
  Result := 'mORMot TTimeLog visualizer';
end;

var
  TimeLogVis: IOTADebuggerVisualizer;


procedure Register;
begin
  TimeLogVis := TTimeLogVisualizer.Create;
  (BorlandIDEServices as IOTADebuggerServices).RegisterDebugVisualizer(TimeLogVis);
end;

procedure RemoveVisualizer;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    DebuggerServices.UnregisterDebugVisualizer(TimeLogVis);
    TimeLogVis := nil;
  end;
end;

initialization

finalization
  RemoveVisualizer;

end.
