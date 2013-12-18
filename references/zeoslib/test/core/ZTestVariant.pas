{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                Test Case for Variants                   }
{                                                         }
{         Originally written by Sergey Seroukhov          }
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

unit ZTestVariant;

interface

{$I ZCore.inc}

uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZTestDefinitions, SysUtils, Classes, ZVariant;

type

  {** Implements a test case for Utilities. }
  TZTestVariantCase = class(TZCoreGenericTestCase)
  private
    FManager: IZVariantManager;
  protected
    property Manager: IZVariantManager read FManager write FManager;

    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNull;
    procedure TestBoolean;
    procedure TestInteger;
    procedure TestDouble;
    procedure TestString;
{$IFNDEF VER130BELOW}
    procedure TestArray;
{$ENDIF}
  end;

implementation

{ TZTestVariantCase }

{**
  Sets up the test environment before tests.
}
procedure TZTestVariantCase.SetUp;
begin
  Manager := DefVarManager;
end;

{**
  Cleans up the test environment after tests.
}
procedure TZTestVariantCase.TearDown;
begin
  Manager := nil;
end;

{**
  Runs a test for null variants.
}
procedure TZTestVariantCase.TestNull;
var
  Value: TZVariant;
begin
  Value := NullVariant;

  Check(Manager.IsNull(Value));
  CheckEquals(Ord(vtNull), Ord(Value.VType));

  CheckEquals(False, Manager.GetAsBoolean(Value));
  CheckEquals('', Manager.GetAsString(Value));
  CheckEquals(0, Manager.GetAsInteger(Value));
  CheckEquals(0.0, Manager.GetAsFloat(Value), 0.001);
end;

{**
  Runs a test for boolean variants.
}
procedure TZTestVariantCase.TestBoolean;
var
  Value: TZVariant;
begin
  Manager.SetAsBoolean(Value, True);
  CheckEquals(True, Value.VBoolean);
  CheckEquals(Ord(vtBoolean), Ord(Value.VType));

  Check(Manager.IsNull(Value) = False);
  CheckEquals(True, Manager.GetAsBoolean(Value));
  try
    Manager.GetAsString(Value);
    Fail('Incorrect getString operation behaviour.');
  except
  end;
  try
    Manager.GetAsInteger(Value);
    Fail('Incorrect getInt operation behaviour.');
  except
  end;
  try
    Manager.GetAsFloat(Value);
    Fail('Incorrect getDouble operation behaviour.');
  except
  end;
end;

{**
  Runs a test for integer variants.
}
procedure TZTestVariantCase.TestInteger;
var
  Value: TZVariant;
begin
  Manager.SetAsInteger(Value, 123);

  CheckEquals(123, Value.VInteger);
  CheckEquals(Ord(vtInteger), Ord(Value.VType));

  Check(Manager.IsNull(Value) = False);
  try
    Manager.GetAsBoolean(Value);
    Fail('Incorrect getBoolean operation behaviour.');
  except
  end;
  try
    Manager.GetAsString(Value);
    Fail('Incorrect getString operation behaviour.');
  except
  end;
  CheckEquals(123, Manager.GetAsInteger(Value));
  CheckEquals(123, Manager.GetAsFloat(Value), 0.1);
end;

{**
  Runs a test for string variants.
}
procedure TZTestVariantCase.TestString;
var
  Value: TZVariant;
begin
  Manager.SetAsString(Value, 'ABC');
  CheckEquals('ABC', Value.VString);
  CheckEquals(Ord(vtString), Ord(Value.VType));

  Manager.SetAsString(Value, '123');
  CheckEquals('123', Manager.GetAsString(Value));
  CheckEquals(Ord(vtString), Ord(Value.VType));

  Check(Manager.IsNull(Value) = False);
  try
    Manager.GetAsBoolean(Value);
    Fail('Incorrect getString operation behaviour.');
  except
  end;
  try
    Manager.GetAsInteger(Value);
    Fail('Incorrect getInt operation behaviour.');
  except
  end;
  try
    Manager.GetAsFloat(Value);
    Fail('Incorrect getDouble operation behaviour.');
  except
  end;
end;

{**
  Runs a test for double variants.
}
procedure TZTestVariantCase.TestDouble;
var
  Value: TZVariant;
begin
  Manager.SetAsFloat(Value, 123.456);

  CheckEquals(123.456, Value.VFloat, 0.001);
  CheckEquals(Ord(vtFloat), Ord(Value.VType));

  Check(Manager.IsNull(Value) = False);
  try
    Manager.GetAsBoolean(Value);
    Fail('Incorrect getBoolean operation behaviour.');
  except
  end;
  try
    Manager.GetAsString(Value);
    Fail('Incorrect getString operation behaviour.');
  except
  end;
  try
    Manager.GetAsInteger(Value);
    Fail('Incorrect getInteger operation behaviour.');
  except
  end;
  CheckEquals(123.456, Manager.GetAsFloat(Value), 0.001);
end;

{**
  Runs a test for variant arrays.
}
{$IFNDEF VER130BELOW}
procedure TZTestVariantCase.TestArray;
const
  MAX_ITEM_COUNT = 10;
var
  I: Integer;
  MaxInt64: Int64;
  ArrayValue: TZVariantDynArray;
  ArrayItem: TZVariant;
  Value: Variant;
begin
  MaxInt64 := High(Int64);
  SetLength(ArrayValue, MAX_ITEM_COUNT);
  for I := 0 to MAX_ITEM_COUNT - 1 do
  begin
    if I < MAX_ITEM_COUNT div 2 then
      Manager.SetAsInteger(ArrayItem, MaxInt64 - I)
    else
      Manager.SetAsInteger(ArrayItem, I);
    ArrayValue[I] := ArrayItem;
  end;
  Value := EncodeVariantArray(ArrayValue);
  for I := 0 to MAX_ITEM_COUNT - 1 do
  begin
    if I < MAX_ITEM_COUNT div 2 then
      Check((MaxInt64 - I) = Value[I], 'Incorrect Int64 array processing')
    else
      Check(I = Value[I], 'Incorrect Int64 array processing');
  end;
end;
{$ENDIF}

initialization
  RegisterTest('core',TZTestVariantCase.Suite);
end.
