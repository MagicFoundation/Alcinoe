{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for Utility Functions              }
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

unit ZTestList;

interface

{$I ZCore.inc}

uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZTestDefinitions, SysUtils, Classes;

type

  {** Implements a test case for Utilities. }
  TZTestListCase = class(TZCoreGenericTestCase)
  published
    procedure TestPerformanceStandardList;
    procedure TestPerformanceItemList;
  end;

implementation

uses ZCompatibility;

const
  ITEM_COUNT = 10000;
  ITEM_SIZE = 512;

{ TZTestListCase }

{**
  Runs a performance test for TZItemList
}
procedure TZTestListCase.TestPerformanceItemList;
var
  Buffer: Pointer;
  StartTicks: Cardinal;
begin
  StartTicks := GetTickCount;
  Buffer := AllocMem(ITEM_COUNT * ITEM_SIZE);
  PrintLn(Format('Creating buffer, Time: %d', [GetTickCount - StartTicks]));

  StartTicks := GetTickCount;
  FreeMem(Buffer, ITEM_COUNT * ITEM_SIZE);
  PrintLn(Format('Removing buffer, Time: %d', [GetTickCount - StartTicks]));
end;

{**
  Runs a performance test for standard TList
}
procedure TZTestListCase.TestPerformanceStandardList;
var
  I: Integer;
  Buffer: Pointer;
  StartTicks: Cardinal;
  List: TList;
begin
  List := TList.Create;

  { Fills the list }
  StartTicks := GetTickCount;
  for I := 0 to ITEM_COUNT - 1 do
  begin
    Buffer := AllocMem(ITEM_SIZE);
    List.Add(Buffer);
  end;
  PrintLn(Format('Creating standard list, Time: %d',
    [GetTickCount - StartTicks]));

  { Removes the list }
  StartTicks := GetTickCount;
  for I := 0 to ITEM_COUNT - 1 do
  begin
    Buffer := List[I];
    FreeMem(Buffer, ITEM_SIZE);
  end;
  List.Clear;
  PrintLn(Format('Removing standard list, Time: %d',
    [GetTickCount - StartTicks]));

  List.Free;
end;

initialization
  RegisterTest('core',TZTestListCase.Suite);
end.
