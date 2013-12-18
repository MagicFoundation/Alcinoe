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

unit ZFunctions;

interface

{$I ZCore.inc}

uses SysUtils, Classes, ZClasses, ZCollections, ZCompatibility, ZVariant,
  ZExpression;

type

  {** Implements a list of functions. }

  { TZFunctionsList }

  TZFunctionsList = class (TInterfacedObject, IZFunctionsList)
  private
    FFunctions: IZCollection;
    FCapacity : Integer;
    FKeys     : Array of LongInt;

    procedure SetKeyCapacity(const NewCapacity : Integer);
    procedure SetKey(const aKey : LongInt; const aPosition : Integer);
    procedure RegenerateKey(const aPosition : Integer);
    procedure RegenerateKeys;
  protected
    property Functions: IZCollection read FFunctions write FFunctions;
    function FindByKeyAndName(const aKey : LongInt; const aName: string): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function GetCount: Integer;
    function GetName(Index: Integer): string;
    function GetFunction(Index: Integer): IZFunction;

    procedure Add(Func: IZFunction);
    procedure Remove(const Name: string);
    function FindByName(const Name: string): Integer;

    procedure Clear;
  end;

  {** Implements an abstract function. }

  { TZAbstractFunction }

  TZAbstractFunction = class (TInterfacedObject, IZFunction)
  private
    FName: string;
  protected
    function GetName: string;
    function CheckParamsCount(Stack: TZExecutionStack;
      ExpectedCount: Integer): Integer;
  public
    constructor Create(aName : string);
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; virtual; abstract;

    property Name: string read GetName;
  end;

  {** Implements a default function list. }
  TZDefaultFunctionsList = class (TZFunctionsList)
  public
    constructor Create;
  end;

implementation

uses ZMessages, ZFunctionsMath, ZFunctionsDateTime, ZFunctionsStrings,
     ZFunctionsConvert, ZFunctionsOther;

{ TZFunctionsList }

{**
  Constructs this object.
}
constructor TZFunctionsList.Create;
begin
  FFunctions := TZCollection.Create;
  SetKeyCapacity(0);
end;

{**
  Destroys this object and cleanup the memory.
}
destructor TZFunctionsList.Destroy;
begin
  SetKeyCapacity(0);
  FFunctions := nil;
  inherited Destroy;
end;

{**
  Sets the capacity of the internal Keystorage.
}
procedure TZFunctionsList.SetKeyCapacity(const NewCapacity : Integer);
begin
  if NewCapacity <> FCapacity then
  begin
    SetLength(FKeys, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{**
  Sets a key to the Keystorage
}
procedure TZFunctionsList.SetKey(const aKey : LongInt; const aPosition : Integer);
begin
  if aPosition >= FCapacity then
    SetKeyCapacity(FCapacity+16);
  FKeys[aPosition] := aKey
end;

{**
  Regenerates a given key
}
procedure TZFunctionsList.RegenerateKey(const aPosition : Integer);

begin
  SetKey(Hash((FFunctions[aPosition] as IZFunction).Name), aPosition);
end;

{**
  Regenerates all keys
}
procedure TZFunctionsList.RegenerateKeys;

var
  I       : Integer;

begin
  SetKeyCapacity(0);
  for I := 0 to FFunctions.Count - 1 do
    RegenerateKey(i);
end;

{**
  Finds a function reference by its Name and Hashkey
}
function TZFunctionsList.FindByKeyAndName(const aKey : LongInt; const aName: string): Integer;

var
  I: Integer;

begin
  Result := -1;
  for I := 0 to FFunctions.Count - 1 do
  begin
    if aKey = FKeys[i] then
    begin
      if aName = (FFunctions[I] as IZFunction).Name then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
end;

{**
  Finds a function reference
}
function TZFunctionsList.FindByName(const Name: string): Integer;
var
  aName: string;

begin
  aName := Uppercase(Name);
  Result := FindByKeyAndName(Hash(aName), aName);
end;

{**
  Adds a new function to this list.
  @param Func a function reference.
}
procedure TZFunctionsList.Add(Func: IZFunction);
var
  Index: Integer;
  aKey : LongInt;
  aName: string;

begin
  aName := Uppercase(Func.Name);
  aKey  := Hash(aName);
  Index := FindByKeyAndName(aKey, aName);
  if Index < 0 then
  begin
    FFunctions.Add(Func);
    SetKey(aKey, FFunctions.Count-1);
  end
  else
    raise TZExpressionError.Create('Function '+Func.Name+' already defined!');
end;

{**
  Removes a reference to a function by it's name.
  @param Name a name of the function to be removed.
}
procedure TZFunctionsList.Remove(const Name: string);
var
  Index: Integer;
begin
  Index := FindByName(Name);
  if Index >= 0 then
  begin
    FFunctions.Delete(Index);
    RegenerateKeys;
  end;
end;

{**
  Cleans the list of registered functions.
}
procedure TZFunctionsList.Clear;
begin
  FFunctions.Clear;
  SetKeyCapacity(0);
end;

{**
  Gets a number of registered functions.
  @returns a number of registered functions.
}
function TZFunctionsList.GetCount: Integer;
begin
  Result := FFunctions.Count;
end;

{**
  Gets a function reference by it's index.
  @param Index a function index.
  @returns a function reference.
}
function TZFunctionsList.GetFunction(Index: Integer): IZFunction;
begin
  Result := FFunctions[Index] as IZFunction;
end;

{**
  Gets a name of the functions by it's index.
  @param Index a functon index.
  @returns a name of the function.
}
function TZFunctionsList.GetName(Index: Integer): string;
begin
  Result := (FFunctions[Index] as IZFunction).Name;
end;

{ TZDefaultFunctionsList }

{**
  Constructs a default functions list and adds all available
  standard functions.
}
constructor TZDefaultFunctionsList.Create;
begin
  inherited Create;
  AddMathFunctions(Self);
  AddStringFunctions(Self);
  AddConvertFunctions(Self);
  AddOtherFunctions(Self);
  AddDateTimeFunctions(Self);
end;

{ TZAbstractFunction }

{**
  Creates the function with a user defined name.
}
constructor TZAbstractFunction.Create(aName : string);
begin
  inherited Create;
  FName := UpperCase(aName);
end;

{**
  Gets the assigned function name.
  @returns the assigned function name.
}
function TZAbstractFunction.GetName: string;
begin
  Result := FName;
end;

{**
  Checks the function parameter count number.
  @param Stack a stack object.
  @param ExpectedCount a number of expected parameters.
  @returns a real number of parameters.
}
function TZAbstractFunction.CheckParamsCount(Stack: TZExecutionStack;
  ExpectedCount: Integer): Integer;
begin
  Result := DefVarManager.GetAsInteger(Stack.GetParameter(0));
  if Result <> ExpectedCount then
  begin
    raise TZExpressionError.Create(Format(SParametersError,
      [ExpectedCount, Result]));
  end;
end;

end.

