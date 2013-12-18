{******************************************}
{                                          }
{             FastReport v4.x              }
{          UIB components RTTI             }
{                                          }
{         Copyright (c) 2005-2007          }
{            by Pierre Yager.              }
{                                          }
{******************************************}

unit frxUIBRTTI;

interface

{$I frx.inc}

implementation

uses
  Windows, Classes, fs_iinterpreter, frxUIBComponents
{$IFDEF Delphi6}
, Variants
{$ENDIF};


type
  TFunctions = class(TfsRTTIModule)
  private
    function CallMethod(Instance: TObject; ClassType: TClass;
      const MethodName: String; Caller: TfsMethodHelper): Variant;
    function GetProp(Instance: TObject; ClassType: TClass;
      const PropName: String): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

{ TFunctions }

constructor TFunctions.Create(AScript: TfsScript);
begin
  inherited Create(AScript);
  with AScript do  
  begin
    with AddClass(TfrxUIBDatabase, 'TfrxCustomDatabase') do
    begin
      AddProperty('Database', 'TUIBDatabase', GetProp, nil);
    end;

    with AddClass(TfrxUIBTransaction, 'TfrxDialogComponent') do
    begin
      AddProperty('Transaction', 'TUIBTransaction', GetProp, nil);
    end;

    with AddClass(TfrxUIBQuery, 'TfrxCustomQuery') do
    begin
      AddMethod('procedure ExecSQL', CallMethod);
      AddProperty('Query', 'TUIBQuery', GetProp, nil);
    end;
  end;
end;

function TFunctions.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  Result := 0;

  if ClassType = TfrxUIBQuery then
  begin
    if MethodName = 'EXECSQL' then
      TfrxUIBQuery(Instance).Query.ExecSQL
  end
end;

function TFunctions.GetProp(Instance: TObject; ClassType: TClass;
  const PropName: String): Variant;
begin
  Result := 0;

  if ClassType = TfrxUIBDatabase then
  begin
    if PropName = 'DATABASE' then
      Result := Integer(TfrxUIBDatabase(Instance).Database)
  end
  else if ClassType = TfrxUIBTransaction then
  begin
    if PropName = 'TRANSACTION' then
      Result := Integer(TfrxUIBTransaction(Instance).Transaction)
  end
  else if ClassType = TfrxUIBQuery then
  begin
    if PropName = 'QUERY' then
      Result := Integer(TfrxUIBQuery(Instance).Query)
  end
end;

initialization
  fsRTTIModules.Add(TFunctions);

end.
