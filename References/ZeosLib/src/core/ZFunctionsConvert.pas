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

unit ZFunctionsConvert;

interface

{$I ZCore.inc}

uses
  SysUtils, ZClasses, ZFunctions, ZExpression, ZVariant;

{**  Conversion functions }

type

{**  Str <> Float}
  {** Implements a VAL function. }
  TZValFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

{**  Str <> Date}
  {** Implements a CTOD function. }
  TZCtodFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a DTOS function. }
  TZDtosFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a DTOS function. }
  TZFormatDateTimeFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

procedure AddConvertFunctions(Functions : TZFunctionsList);

implementation

var
  InternalDefaultFormatSettings : TFormatSettings;

{*******
    DefaultFormatSettings : TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'd/m/y';
    LongDateFormat: 'dd" "mmmm" "yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );
******}

 { TZValFunction }

function TZValFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsFloat(Result, StrToFloatDef(Stack.GetParameter(1).VString, 0, InternalDefaultFormatSettings));
end;

{ TZCtodFunction }

function TZCtodFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  Value: TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Value := Stack.GetParameter(1);
  VariantManager.SetAsDateTime(Result, StrToDateDef(Value.VString, 0));
end;

{ TZDtosFunction }

function TZDtosFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  Value: TZVariant;
begin
  CheckParamsCount(Stack, 1);
  Value := Stack.GetParameter(1);
  VariantManager.SetAsString(Result, FormatDateTime('yyyymmdd', Value.VDateTime));
end;

{ TZFormatDateTimeFunction }

function TZFormatDateTimeFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsString(Result, FormatDateTime(Stack.GetParameter(2).VString, Stack.GetParameter(1).VDateTime));
end;

procedure AddConvertFunctions(Functions : TZFunctionsList);
begin
  Functions.Add(TZValFunction.Create('VAL'));
  Functions.Add(TZDtosFunction.Create('DTOS'));
  Functions.Add(TZCtodFunction.Create('CTOD'));
  Functions.Add(TZFormatDateTimeFunction.Create('FORMATDATETIME'));
end;

initialization
//  InternalDefaultFormatSettings := DefaultFormatSettings;
  InternalDefaultFormatSettings.ThousandSeparator   := ',';
  InternalDefaultFormatSettings.DecimalSeparator    := '.';
end.
