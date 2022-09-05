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

unit ZFunctionsStrings;

interface

{$I ZCore.inc}

uses
  SysUtils, ZClasses, ZFunctions, ZExpression, ZVariant;

{**  String functions}

type
  {** Implements a CONCAT function. }
  TZConcatFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SUBSTR function. }
  TZSubStrFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LEFT function. }
  TZLeftFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a RIGHT function. }
  TZRightFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a STRPOS function. }
  TZStrPosFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LENGTH function. }
  TZLengthFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a UPPER function. }
  TZUpperFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LOWER function. }
  TZLowerFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a CAPITALIZE function. }
  TZCapitalizeFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a TRIM function. }
  TZTrimFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LTRIM function. }
  TZLTrimFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a RTRIM function. }
  TZRTrimFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SOUNDEX function. }
  TZSoundexFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a LEVENSHTEINDIST function. }
  TZLevenshteinDistanceFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

Function Capitalize(const s:string; Delims : string = '') : string;
Function LevenshteinDistance(const s1, s2: string; const DoUpcase : BOOLEAN = TRUE): Integer;
procedure AddStringFunctions(Functions : TZFunctionsList);

{$IFNDEF FPC}
{$ENDIF}

implementation

uses
  Math, StrUtils, ZMessages, ZCompatibility;

Function Capitalize(const s:string; Delims : string = '') : string;
var
  sDelims : set of ansichar;
  i : integer;
begin
  if Delims = '' then
    sDelims := StdWordDelims
  else
  begin
    sDelims := [];
    for i:=1 to Length(Delims) do
      Include(sDelims,AnsiChar(Delims[i]))
  end;
  Result := AnsiProperCase(s, sDelims);
end;

Function LevenshteinDistance(const s1, s2: string; const DoUpcase : BOOLEAN = TRUE): Integer;

var
  d      : array of array of Integer;
  s,t    : string;
  Start,
  Len1,
  Len2,
  i, j,
  Cost : Integer;

begin
  Len1 := Length(s1);
  Len2 := Length(s2);

  if Len1 = 0 then
  begin
    Result := Len2;
    Exit;
  end;
  if Len2 = 0 then
  begin
    Result := Len1;
    Exit;
  end;
  if DoUpcase then
  begin
    s := Uppercase(s1);
    t := Uppercase(s2);
  end
  else
  begin
    s := s1;
    t := s2;
  end;
  start := 1;
//  trim off the matching items at the beginning
  while (start <= Len1) and (start <= Len2) and (s[start] = t[start]) do
      INC(start);
//  trim off the matching items at the end
  while (start <= Len1) and (start <= Len2) and (s[Len1] = t[Len2]) do
  begin
    DEC(Len1);
    DEC(Len2);
  end;

  DEC(Start);

  DEC(Len1, Start);
  DEC(Len2, Start);

  if Len1 = 0 then
  begin
    Result := Len2;
    Exit;
  end;
  if Len2 = 0 then
  begin
    Result := Len1;
    Exit;
  end;

  setlength(d, Len1 + 1, Len2 + 1);
  for i := 0 to Len1 do
    d[i, 0] := i;
  for j := 0 to Len2 do
    d[0, j] := j;

//  only loop over the items that are different
  for i := 1 to Len1 do
  begin
    for j := 1 to Len2 do
    begin
      Cost := ABS(ORD(s[i+start] <> t[j+start]));
      d[i, j] := Min(
                     Min(d[i-1,j]+1,          // deletion
                         d[i,j-1]+1),         // insertion
                         d[i-1,j-1]+Cost);    // substitution
    end;
  end;
  Result := d[Len1, Len2];
end;

{****  This is the original not optimized version
Function LevenshteinDistance(const s1, s2: string; const DoUpcase : BOOLEAN = TRUE): Integer;

var
  d      : array of array of Integer;
  s,t    : string;
  Len1,
  Len2,
  i, j,
  Cost : Integer;
begin
  Len1 := Length(s1);
  Len2 := Length(s2);
  if Len1 = 0 then
  begin
    Result := Len2;
    Exit;
  end;
  if Len2 = 0 then
  begin
    Result := Len1;
    Exit;
  end;
  if DoUpcase then
  begin
    s := Upcase(s1);
    t := Upcase(s2);
  end
  else
  begin
    s := s1;
    t := s2;
  end;
  setlength(d, Len1 + 1, Len2 + 1);
  for i := 0 to Len1 do
    d[i, 0] := i;
  for j := 0 to Len2 do
    d[0, j] := j;
  for i := 1 to Len1 do
  begin
    for j := 1 to Len2 do
    begin
      Cost := ABS(ORD(s[i] <> t[j]));
      d[i, j] := Min(
                     Min(d[i-1,j]+1,
                         d[i,j-1]+1),
                         d[i-1,j-1]+Cost);
    end;
  end;
  Result := d[Len1, Len2];
end;
******}

{ TZConcatFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZConcatFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  I, ParamsCount: Integer;
  Temp: string;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if ParamsCount < 2 then
    raise TZExpressionError.Create(SExpectedMoreParams);

  Temp := VariantManager.GetAsString(Stack.GetParameter(ParamsCount));
  for I := ParamsCount - 1 downto 1 do
    Temp := Temp + VariantManager.GetAsString(Stack.GetParameter(I));
  VariantManager.SetAsString(Result, Temp);
end;

{ TZSubStrFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZSubStrFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 3);
  VariantManager.SetAsString(Result, Copy(
    VariantManager.GetAsString(Stack.GetParameter(3)),
    VariantManager.GetAsInteger(Stack.GetParameter(2)),
    VariantManager.GetAsInteger(Stack.GetParameter(1))));
end;

{ TZLeftFunction }
function TZLeftFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  Value1, Value2: TZVariant;
begin
  CheckParamsCount(Stack, 2);
  Value1 := Stack.GetParameter(2);
  Value2 := Stack.GetParameter(1);
  VariantManager.SetAsString(Result, LeftStr(Value1.VString, Value2.VInteger));
end;

{ TZRightFunction }
function TZRightFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  Value1, Value2: TZVariant;
begin
  CheckParamsCount(Stack, 2);
  Value1 := Stack.GetParameter(2);
  Value2 := Stack.GetParameter(1);
  VariantManager.SetAsString(Result, RightStr(Value1.VString, Value2.VInteger));
end;

{ TZStrPosFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZStrPosFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, Pos(
    VariantManager.GetAsString(Stack.GetParameter(2)),
    VariantManager.GetAsString(Stack.GetParameter(1))));
end;

{ TZLengthFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZLengthFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, Length(VariantManager.GetAsString(Stack.GetParameter(1))));
end;

{ TZLowerFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZLowerFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsString(Result, AnsiLowerCase(
    VariantManager.GetAsString(Stack.GetParameter(1))));
end;

{ TZUpperFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZUpperFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsString(Result, AnsiUpperCase(
    VariantManager.GetAsString(Stack.GetParameter(1))));
end;

{ TZCapitalizeFunction }

function TZCapitalizeFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  ParamsCount: Integer;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if (ParamsCount < 1) then
    raise TZExpressionError.Create(SExpectedMoreParams);
  if (ParamsCount < 2) then
    VariantManager.SetAsString(Result, Capitalize(
      VariantManager.GetAsString(Stack.GetParameter(1))))
  else
    VariantManager.SetAsString(Result, Capitalize(
      VariantManager.GetAsString(Stack.GetParameter(2)),
      VariantManager.GetAsString(Stack.GetParameter(1))))
end;

{ TZTrimFunction }

function TZTrimFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  Value: TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Value := Stack.GetParameter(1);
  VariantManager.SetAsString(Result, Trim(Value.VString));
end;

{ TZLTrimFunction }

function TZLTrimFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  Value: TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Value := Stack.GetParameter(1);
  VariantManager.SetAsString(Result, TrimLeft(Value.VString));
end;

{ TZRTrimFunction }

function TZRTrimFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  Value: TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Value := Stack.GetParameter(1);
  VariantManager.SetAsString(Result, TrimRight(Value.VString));
end;

{ TZSoundexFunction }

function TZSoundexFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  ParamsCount: Integer;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if (ParamsCount < 1) then
    raise TZExpressionError.Create(SExpectedMoreParams);
  if (ParamsCount < 2) then
    VariantManager.SetAsString(Result, Soundex(
      VariantManager.GetAsString(Stack.GetParameter(1))))
  else
    VariantManager.SetAsString(Result, Soundex(
      VariantManager.GetAsString(Stack.GetParameter(2)),
      VariantManager.GetAsInteger(Stack.GetParameter(1))))
end;

function TZLevenshteinDistanceFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;

var
  ParamsCount: Integer;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));
  if (ParamsCount < 2) then
    raise TZExpressionError.Create(SExpectedMoreParams);
  if (ParamsCount < 3) then
    VariantManager.SetAsInteger(Result,
                                  LevenshteinDistance(
                                    VariantManager.GetAsString(Stack.GetParameter(2)),
                                    VariantManager.GetAsString(Stack.GetParameter(1))))
  else
    VariantManager.SetAsInteger(Result,
                                LevenshteinDistance(
                                  VariantManager.GetAsString(Stack.GetParameter(3)),
                                  VariantManager.GetAsString(Stack.GetParameter(2)),
                                  VariantManager.GetAsBoolean(Stack.GetParameter(1))))
end;

procedure AddStringFunctions(Functions : TZFunctionsList);
begin
  Functions.Add(TZConcatFunction.Create('CONCAT'));
  Functions.Add(TZSubStrFunction.Create('SUBSTR'));
  Functions.Add(TZLeftFunction.Create('LEFT'));
  Functions.Add(TZRightFunction.Create('RIGHT'));
  Functions.Add(TZStrPosFunction.Create('STRPOS'));
  Functions.Add(TZLengthFunction.Create('LENGTH'));

  Functions.Add(TZUpperFunction.Create('UPPER'));
  Functions.Add(TZLowerFunction.Create('LOWER'));
  Functions.Add(TZCapitalizeFunction.Create('CAP'));
  Functions.Add(TZCapitalizeFunction.Create('CAPITALIZE'));

  Functions.Add(TZTrimFunction.Create('TRIM'));
  Functions.Add(TZLTrimFunction.Create('LTRIM'));
  Functions.Add(TZRTrimFunction.Create('RTRIM'));

  Functions.Add(TZSoundexFunction.Create('SOUNDEX'));
  Functions.Add(TZLevenshteinDistanceFunction.Create('LEVDIST'));
  Functions.Add(TZLevenshteinDistanceFunction.Create('LEVENSHTEINDISTANCE'));
end;

end.

