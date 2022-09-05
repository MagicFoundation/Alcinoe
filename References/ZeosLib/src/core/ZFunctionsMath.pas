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

unit ZFunctionsMath;

interface

{$I ZCore.inc}

uses
  SysUtils, ZClasses, ZFunctions, ZExpression, ZVariant;

{**  Math functions }

type
  {** Implements a E function. }
  TZEFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a PI function. }
  TZPIFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a RND function. }
  TZRndFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ABS function. }
  TZAbsFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

{** Trigonometric }
  {** Implements a COS function. }
  TZCosFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a COT function. }
  TZCotFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SIN function. }
  TZSinFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a TAN function. }
  TZTanFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ACOS function. }
  TZAcosFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ASIN function. }
  TZAsinFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ATAN function. }
  TZAtanFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

{** Rounding }
  {** Implements a ROUND function. }
  TZRoundFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a TRUNC function. }
  TZTruncFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a INT function. }
  TZIntFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a FRAC function. }
  TZFracFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a CEIL function. }
  TZCeilFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a FLOOR function. }
  TZFloorFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

{** Logarithmic }
  {** Implements a EXP function. }
  TZExpFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LOG function. }
  TZLogFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LOG10 function. }
  TZLog10Function = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SQR function. }
  TZSqrFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

procedure AddMathFunctions(Functions : TZFunctionsList);

implementation

uses
  Math;

{ TZEFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZEFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  VariantManager.SetAsFloat(Result, Exp(1));
end;

{ TZPIFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZPIFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  VariantManager.SetAsFloat(Result, PI);
end;

{ TZRndFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZRndFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  VariantManager.SetAsFloat(Result, Random);
end;

{ TZAbsFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZAbsFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  Value: TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Value := Stack.GetParameter(1);
  if Value.VType = vtInteger then
    VariantManager.SetAsInteger(Result, Abs(Value.VInteger))
  else if Value.VType = vtFloat then
    VariantManager.SetAsFloat(Result, Abs(Value.VFloat))
  else
    Result := Value;
end;

{ TZExpFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZExpFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Exp(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZLogFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZLogFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Ln(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZLog10Function }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZLog10Function.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Log10(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZCosFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZCosFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Cos(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZCotFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZCotFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Cotan(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZSinFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZSinFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Sin(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZTanFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZTanFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Tan(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZAcosFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZAcosFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, ArcCos(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZAsinFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZAsinFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, ArcSin(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZAtanFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZAtanFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, ArcTan(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZCeilFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZCeilFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, Ceil(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZFloorFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZFloorFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, Floor(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZRoundFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZRoundFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, Round(VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZTruncFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZTruncFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, Trunc(VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZIntFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZIntFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Int(VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZFracFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZFracFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Frac(VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

{ TZSqrFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZSqrFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, Sqrt(
    VariantManager.GetAsFloat(Stack.GetParameter(1))));
end;

procedure AddMathFunctions(Functions : TZFunctionsList);
begin
  Functions.Add(TZEFunction.Create('E'));
  Functions.Add(TZPIFunction.Create('PI'));
  Functions.Add(TZRndFunction.Create('RND'));
  Functions.Add(TZAbsFunction.Create('ABS'));
  Functions.Add(TZExpFunction.Create('EXP'));
  Functions.Add(TZLogFunction.Create('LOG'));
  Functions.Add(TZLog10Function.Create('LOG10'));
  Functions.Add(TZCosFunction.Create('COS'));
  Functions.Add(TZSinFunction.Create('SIN'));
  Functions.Add(TZTanFunction.Create('TAN'));
  Functions.Add(TZCotFunction.Create('COT'));
  Functions.Add(TZAcosFunction.Create('ACOS'));
  Functions.Add(TZAsinFunction.Create('ASIN'));
  Functions.Add(TZAtanFunction.Create('ATAN'));
  Functions.Add(TZRoundFunction.Create('ROUND'));
  Functions.Add(TZCeilFunction.Create('CEIL'));
  Functions.Add(TZFloorFunction.Create('FLOOR'));
  Functions.Add(TZIntFunction.Create('INT'));
  Functions.Add(TZTruncFunction.Create('TRUNC'));
  Functions.Add(TZFracFunction.Create('FRAC'));
  Functions.Add(TZSqrFunction.Create('SQR'));
  Functions.Add(TZSqrFunction.Create('SQRT'));
end;

end.
