{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Variables classes and interfaces            }
{                                                         }
{           Originally written by Sergey Seroukhov        }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZFunctionsOther;

interface

{$I ZCore.inc}

uses
  SysUtils, ZClasses, ZFunctions, ZExpression, ZVariant;

{** Other functions}

type

  {** Implements a EMPTY function. }
  TZEmptyFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MIN function. }
  TZMinFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MAX function. }
  TZMaxFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SUM function. }
  TZSumFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a IIF function. }
  TZIIFFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a CASEF function. }
  TZCASEFFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

procedure AddOtherFunctions(Functions : TZFunctionsList);

implementation

uses
  ZMessages;

{ TZEmptyFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZEmptyFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  Value: TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Value := Stack.GetParameter(1);
  VariantManager.SetAsBoolean(Result, VariantManager.IsNull(Value));
end;

{ TZMinFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZMinFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  I, ParamsCount: Integer;
  Value: TZVariant;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if ParamsCount < 2 then
    raise TZExpressionError.Create(SExpectedMoreParams);

  Result := Stack.GetParameter(ParamsCount);
  for I := 1 to ParamsCount - 1 do
  begin
    Value := Stack.GetParameter(I);
    if VariantManager.Compare(Result, Value) > 0 then
      Result := Value;
  end;
end;

{ TZMaxFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZMaxFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  I, ParamsCount: Integer;
  Value: TZVariant;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if ParamsCount < 2 then
    raise TZExpressionError.Create(SExpectedMoreParams);

  Result := Stack.GetParameter(ParamsCount);
  for I := 1 to ParamsCount - 1 do
  begin
    Value := Stack.GetParameter(I);
    if VariantManager.Compare(Result, Value) < 0 then
      Result := Value;
  end;
end;

{ TZSumFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZSumFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  I, ParamsCount: Integer;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if ParamsCount < 2 then
    raise TZExpressionError.Create(SExpectedMoreParams);

  Result := Stack.GetParameter(ParamsCount);
  for I := ParamsCount - 1 downto 1 do
    Result := VariantManager.OpAdd(Result, Stack.GetParameter(I));
end;

{ TZIIFFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZIIFFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 3);
  if VariantManager.GetAsBoolean(Stack.GetParameter(3)) then
     Result := Stack.GetParameter(2)
   else
     Result := Stack.GetParameter(1);
end;

{ TZCASEFFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZCASEFFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  ParamsCount, Index : Integer;

begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if ParamsCount < 2 then
    raise TZExpressionError.Create(SExpectedMoreParams);
  Index := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount));
  if ParamsCount < (Index+2) then
    raise TZExpressionError.Create(SExpectedMoreParams);
  Result := Stack.GetParameter(ParamsCount-Index-1)
end;

procedure AddOtherFunctions(Functions : TZFunctionsList);
begin
  Functions.Add(TZEmptyFunction.Create('EMPTY'));
  Functions.Add(TZMinFunction.Create('MIN'));
  Functions.Add(TZMaxFunction.Create('MAX'));
  Functions.Add(TZSumFunction.Create('SUM'));
  Functions.Add(TZIIFFunction.Create('IIF'));
  Functions.Add(TZCASEFFunction.Create('CASEF'));
end;

end.
