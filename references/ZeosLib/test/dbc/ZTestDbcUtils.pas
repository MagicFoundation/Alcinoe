{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       Test Case for Database Connectivity Classes       }
{                                                         }
{ Originally written by Sergey Seroukhov, Sergey Merkuriev}
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

unit ZTestDbcUtils;

interface
{$I ZDbc.inc}
uses {$IFDEF FPC}testregistry,fpcunit{$ELSE}TestFramework{$ENDIF}, ZDbcIntfs, ZDbcConnection, SysUtils, ZCompatibility;

type

  {** Implements a test case for class TZAbstractDriver and Utilities. }
  TZTestDbcUtilsCase = class(TTestCase)
  published
    procedure TestResolveUrl;
    procedure TestCheckConvertion;
    procedure TestDefineColumnTypeName;
    procedure TestAnsiSqlDateToDateTime;
  end;

implementation

uses Classes, ZSysUtils, ZDbcUtils;

{ TZTestDbcUtilsCase }

{**
  Test for function AnsiSqlDateToDateTime
}
procedure TZTestDbcUtilsCase.TestAnsiSqlDateToDateTime;
var
  Value: string;
  Default: TDateTime;
begin
  Value := '';
  CheckEquals(0, AnsiSqlDateToDateTime(Value), 0, Value);

  { Tests normal time. }
  Value := '22:01:31';
  Default := EncodeTime(22, 01, 31, 0);
  CheckEquals(Default, AnsiSqlDateToDateTime(Value), 0, Value);

  { Tests incorrect time. }
  Value := '99:77:99';
  CheckEquals(0, AnsiSqlDateToDateTime(Value), 0, Value);

  { Tests normal date. }
  Value := '1999-01-31';
  Default := EncodeDate(1999, 01, 31);
  CheckEquals(Default, AnsiSqlDateToDateTime(Value), 0, Value);

  { Tests normal 2k date. }
  Value := '2001-07-22';
  Default := EncodeDate(2001, 07, 22);
  CheckEquals(Default, AnsiSqlDateToDateTime(Value), 0, Value);

  { Test normal datetime time. }
  Value := '1999-01-31 02:20:42';
  Default := EncodeDate(1999, 01, 31) + EncodeTime(02, 20, 42, 0);
  CheckEquals(Default, AnsiSqlDateToDateTime(Value), 0, Value);

  { Tests normal 2k datetimetime. }
  Value := '2001-07-22 02:20:42';
  Default := EncodeDate(2001, 07, 22) + EncodeTime(02, 20, 42, 0);
  CheckEquals(Default, AnsiSqlDateToDateTime(Value), 0, Value);
end;

{**
  Tests function CheckConvertion.
}
procedure TZTestDbcUtilsCase.TestCheckConvertion;
begin
  {check Boolean convertion}
  Check(CheckConvertion(stBoolean,stBoolean), 'stBoolean to stBoolean');
  Check(CheckConvertion(stBoolean,stByte), 'stBoolean to stByte');
  Check(CheckConvertion(stBoolean,stShort), 'stBoolean to stShort');
  Check(CheckConvertion(stBoolean,stInteger), 'stBoolean to stInteger');
  Check(CheckConvertion(stBoolean,stLong), 'stBoolean to stLong');
  Check(CheckConvertion(stBoolean,stFloat), 'stBoolean to stFloat');
  Check(CheckConvertion(stBoolean,stDouble), 'stBoolean to stDouble');
  Check(CheckConvertion(stBoolean,stBigDecimal), 'stBoolean to stBigDecimal');
  Check(CheckConvertion(stBoolean,stString), 'stBoolean to stString');
  Check(not CheckConvertion(stBoolean,stBytes), 'stBoolean to stBytes');
  Check(not CheckConvertion(stBoolean,stAsciiStream), 'stBoolean to stAsciiStream');
  Check(not CheckConvertion(stBoolean,stBinaryStream), 'stBoolean to stBinaryStream');
  Check(not CheckConvertion(stBoolean,stDate), 'stBoolean to stDate');
  Check(not CheckConvertion(stBoolean,stTime), 'stBoolean to stTime');
  Check(not CheckConvertion(stBoolean,stTimestamp), 'stBoolean to stTimestamp');
  Check(not CheckConvertion(stBoolean,stUnicodeStream), 'stBoolean to stUnicodeStream');
  {check Byte convertion}
  Check(CheckConvertion(stByte,stBoolean), 'stByte to stBoolean');
  Check(CheckConvertion(stByte,stByte), 'stByte to stByte');
  Check(CheckConvertion(stByte,stShort), 'stByte to stShort');
  Check(CheckConvertion(stByte,stInteger), 'stByte to stInteger');
  Check(CheckConvertion(stByte,stLong), 'stByte to stLong');
  Check(CheckConvertion(stByte,stFloat), 'stByte to stFloat');
  Check(CheckConvertion(stByte,stDouble), 'stByte to stDouble');
  Check(CheckConvertion(stByte,stBigDecimal), 'stByte to stBigDecimal');
  Check(CheckConvertion(stByte,stString), 'stByte,stString');
  Check(not CheckConvertion(stByte,stBytes), 'stByte to stBytes');
  Check(not CheckConvertion(stByte,stAsciiStream), 'stByte to stAsciiStream');
  Check(not CheckConvertion(stByte,stBinaryStream), 'stByte to stBinaryStream');
  Check(not CheckConvertion(stByte,stDate), 'stByte to stDate');
  Check(not CheckConvertion(stByte,stTime), 'stByte to stTime');
  Check(not CheckConvertion(stByte,stTimestamp), 'stByte to stTimestamp');
  Check(not CheckConvertion(stByte,stUnicodeStream), 'stByte to stUnicodeStream');
  {check Short convertion}
  Check(CheckConvertion(stShort,stBoolean), 'stShort to stBoolean');
  Check(CheckConvertion(stShort,stShort), 'stShort to stShort');
  Check(CheckConvertion(stShort,stByte), 'stShort to stByte');
  Check(CheckConvertion(stShort,stInteger), 'stShort to stInteger');
  Check(CheckConvertion(stShort,stLong), 'stShort to stLong');
  Check(CheckConvertion(stShort,stFloat), 'stShort to stFloat');
  Check(CheckConvertion(stShort,stDouble), 'stShort to stDouble');
  Check(CheckConvertion(stShort,stBigDecimal), 'stShort to stBigDecimal');
  Check(CheckConvertion(stShort,stString), 'stShort,stString');
  Check(not CheckConvertion(stShort,stBytes), 'stShort to stBytes');
  Check(not CheckConvertion(stShort,stAsciiStream), 'stShort to stAsciiStream');
  Check(not CheckConvertion(stShort,stBinaryStream), 'stShort to stBinaryStream');
  Check(not CheckConvertion(stShort,stDate), 'stShort to stDate');
  Check(not CheckConvertion(stShort,stTime), 'stShort to stTime');
  Check(not CheckConvertion(stShort,stTimestamp), 'stShort to stTimestamp');
  Check(not CheckConvertion(stBytes,stUnicodeStream), 'stBytes to stUnicodeStream');
  {check Integer convertion}
  Check(CheckConvertion(stInteger,stBoolean), 'stInteger to stBoolean');
  Check(CheckConvertion(stInteger,stByte), 'stInteger to stByte');
  Check(CheckConvertion(stInteger,stShort), 'stInteger to stShort');
  Check(CheckConvertion(stInteger,stInteger), 'stInteger to stInteger');
  Check(CheckConvertion(stInteger,stLong), 'stInteger to stLong');
  Check(CheckConvertion(stInteger,stFloat), 'stInteger to stFloat');
  Check(CheckConvertion(stInteger,stDouble), 'stInteger to stDouble');
  Check(CheckConvertion(stInteger,stBigDecimal), 'stInteger to stBigDecimal');
  Check(CheckConvertion(stInteger,stString), 'stInteger to stString');
  Check(not CheckConvertion(stInteger,stBytes), 'stInteger to stBytes');
  Check(not CheckConvertion(stInteger,stAsciiStream), 'stInteger to stAsciiStream');
  Check(not CheckConvertion(stInteger,stBinaryStream), 'stInteger to stBinaryStream');
  Check(not CheckConvertion(stInteger,stDate), 'stInteger to stDate');
  Check(not CheckConvertion(stInteger,stTime), 'stInteger to stTime');
  Check(not CheckConvertion(stInteger,stTimestamp), 'stInteger to stTimestamp');
  Check(not CheckConvertion(stInteger,stUnicodeStream), 'stInteger to stUnicodeStream');
  {check Long convertion}
  Check(CheckConvertion(stLong,stBoolean), 'stLong to stTimestamp');
  Check(CheckConvertion(stLong,stByte), 'stLong to stByte');
  Check(CheckConvertion(stLong,stShort), 'stLong to stShort');
  Check(CheckConvertion(stLong,stInteger), 'stLong to stInteger');
  Check(CheckConvertion(stLong,stLong), 'stLong to stLong');
  Check(CheckConvertion(stLong,stFloat), 'stLong to stFloat');
  Check(CheckConvertion(stLong,stDouble), 'stLong,stDouble');
  Check(CheckConvertion(stLong,stBigDecimal), 'stLong to stBigDecimal');
  Check(CheckConvertion(stLong,stString), 'stLong,stString');
  Check(not CheckConvertion(stLong,stBytes), 'stLong to stBytes');
  Check(not CheckConvertion(stLong,stAsciiStream), 'stLong to stAsciiStream');
  Check(not CheckConvertion(stLong,stBinaryStream), 'stLong to stBinaryStream');
  Check(not CheckConvertion(stLong,stDate), 'stLong,stDate');
  Check(not CheckConvertion(stLong,stTime), 'stLong,stTime');
  Check(not CheckConvertion(stLong,stTimestamp), 'stLong to stTimestamp');
  Check(not CheckConvertion(stLong,stUnicodeStream), 'stLong to stUnicodeStream');
  {check Float convertion}
  Check(CheckConvertion(stFloat,stBoolean), 'stFloat to stBoolean');
  Check(CheckConvertion(stFloat,stByte), 'stFloat to stByte');
  Check(CheckConvertion(stFloat,stShort), 'stFloat to stShort');
  Check(CheckConvertion(stFloat,stInteger), 'stFloat to stInteger');
  Check(CheckConvertion(stFloat,stLong), 'stFloat to stLong');
  Check(CheckConvertion(stFloat,stFloat), 'stFloat to stFloat');
  Check(CheckConvertion(stFloat,stDouble), 'stFloat to stDouble');
  Check(CheckConvertion(stFloat,stBigDecimal), 'stFloat to stBigDecimal');
  Check(CheckConvertion(stFloat,stString), 'stFloat to stString');
  Check(not CheckConvertion(stFloat,stBytes), 'stFloat to stBytes');
  Check(not CheckConvertion(stFloat,stAsciiStream), 'stFloat to stAsciiStream');
  Check(not CheckConvertion(stFloat,stBinaryStream), 'stFloat to stBinaryStream');
  Check(not CheckConvertion(stFloat,stDate), 'stFloat to stDate');
  Check(not CheckConvertion(stFloat,stTime), 'stFloat to stTime');
  Check(not CheckConvertion(stFloat,stTimestamp), 'stFloat to stTimestamp');
  Check(not CheckConvertion(stFloat,stUnicodeStream), 'stFloat to stUnicodeStream');
  {check Double convertion}
  Check(CheckConvertion(stDouble,stBoolean), 'stDouble to stBoolean');
  Check(CheckConvertion(stDouble,stByte), 'stDouble to stByte');
  Check(CheckConvertion(stDouble,stShort), 'stDouble to stShort');
  Check(CheckConvertion(stDouble,stInteger), 'stDouble to stInteger');
  Check(CheckConvertion(stDouble,stLong), 'stDouble to stLong');
  Check(CheckConvertion(stDouble,stFloat), 'stDouble to stFloat');
  Check(CheckConvertion(stDouble,stDouble), 'stDouble to stDouble');
  Check(CheckConvertion(stDouble,stBigDecimal), 'stDouble to stBigDecimal');
  Check(CheckConvertion(stDouble,stString), 'stDouble to stString');
  Check(not CheckConvertion(stDouble,stBytes), 'stDouble to stBytes');
  Check(not CheckConvertion(stDouble,stAsciiStream), 'stDouble to stAsciiStream');
  Check(not CheckConvertion(stDouble,stBinaryStream), 'stDouble to stBinaryStream');
  Check(not CheckConvertion(stDouble,stDate), 'stDouble to stDate');
  Check(not CheckConvertion(stDouble,stTime), 'stDouble to stTime');
  Check(not CheckConvertion(stDouble,stTimestamp), 'stDouble to stTimestamp');
  Check(not CheckConvertion(stDouble,stUnicodeStream), 'stDouble to stUnicodeStream');
  {check BigDecimal convertion}
  Check(CheckConvertion(stBigDecimal,stBoolean), 'stBigDecimal to stBoolean');
  Check(CheckConvertion(stBigDecimal,stByte), 'stBigDecimal to stByte');
  Check(CheckConvertion(stBigDecimal,stShort), 'stBigDecimal to stShort');
  Check(CheckConvertion(stBigDecimal,stInteger), 'stBigDecimal to stInteger');
  Check(CheckConvertion(stBigDecimal,stLong), 'stBigDecimal to stLong');
  Check(CheckConvertion(stBigDecimal,stFloat), 'stBigDecimal to stFloat');
  Check(CheckConvertion(stBigDecimal,stDouble), 'stBigDecimal to stDouble');
  Check(CheckConvertion(stBigDecimal,stBigDecimal), 'stBigDecimal to stBigDecimal');
  Check(CheckConvertion(stBigDecimal,stString), 'stBigDecimal to stString');
  Check(not CheckConvertion(stBigDecimal,stBytes), 'stBigDecimal to stBytes');
  Check(not CheckConvertion(stBigDecimal,stAsciiStream), 'stBigDecimal to stAsciiStream');
  Check(not CheckConvertion(stBigDecimal,stBinaryStream), 'stBigDecimal to stBinaryStream');
  Check(not CheckConvertion(stBigDecimal,stDate), 'stBigDecimal to stDate');
  Check(not CheckConvertion(stBigDecimal,stTime), 'stBigDecimal to stTime');
  Check(not CheckConvertion(stBigDecimal,stTimestamp), 'stBigDecimal to stTimestamp');
  Check(not CheckConvertion(stBigDecimal,stUnicodeStream), 'stBigDecimal to stUnicodeStream');
  {check String convertion}
  Check(CheckConvertion(stString,stBoolean), 'stString to stBoolean');
  Check(CheckConvertion(stString,stByte), 'stString to stByte');
  Check(CheckConvertion(stString,stShort), 'stString to stShort');
  Check(CheckConvertion(stString,stInteger), 'stString to stInteger');
  Check(CheckConvertion(stString,stLong), 'stString to stLong');
  Check(CheckConvertion(stString,stFloat), 'stString to stFloat');
  Check(CheckConvertion(stString,stDouble), 'stString to stDouble');
  Check(CheckConvertion(stString,stBigDecimal), 'stString to stBigDecimal');
  Check(CheckConvertion(stString,stString), 'stString to stString');
  Check(CheckConvertion(stString,stBytes), 'stString to stBytes');
  Check(not CheckConvertion(stString,stAsciiStream), 'stString to stAsciiStream');
  Check(not CheckConvertion(stString,stBinaryStream), 'stString to stBinaryStream');
  Check(CheckConvertion(stString,stDate), 'stString to stDate');
  Check(CheckConvertion(stString,stTime), 'stString to stTime');
  Check(CheckConvertion(stString,stTimestamp), 'stString to stTimestamp');
  Check(not CheckConvertion(stString,stUnicodeStream), 'stString to stUnicodeStream');
  {check Bytes convertion}
  Check(not CheckConvertion(stBytes,stBoolean), 'stBytes to stBoolean');
  Check(not CheckConvertion(stBytes,stByte), 'stBytes to stByte');
  Check(not CheckConvertion(stBytes,stShort), 'stBytes to stShort');
  Check(not CheckConvertion(stBytes,stInteger), 'stBytes to stInteger');
  Check(not CheckConvertion(stBytes,stLong), 'stBytes to stLong');
  Check(not CheckConvertion(stBytes,stFloat), 'stBytes to stFloat');
  Check(not CheckConvertion(stBytes,stDouble), 'stBytes to stDouble');
  Check(not CheckConvertion(stBytes,stBigDecimal), 'stBytes to stBigDecimal');
  Check(CheckConvertion(stBytes,stString), 'stBytes to stString');
  Check(CheckConvertion(stBytes,stBytes), 'stBytes to stBytes');
  Check(not CheckConvertion(stBytes,stAsciiStream), 'stBytes to stAsciiStream');
  Check(not CheckConvertion(stBytes,stBinaryStream), 'stBytes to stBinaryStream');
  Check(not CheckConvertion(stBytes,stDate), 'stBytes to stDate');
  Check(not CheckConvertion(stBytes,stTime), 'stBytes to stTime');
  Check(not CheckConvertion(stBytes,stTimestamp), 'stBytes to stTimestamp');
  Check(not CheckConvertion(stBytes,stUnicodeStream), 'stBytes to stUnicodeStream');
  {check Date convertion}
  Check(not CheckConvertion(stDate,stBoolean), 'stDate to stBoolean');
  Check(not CheckConvertion(stDate,stByte), 'stDate,stByte');
  Check(not CheckConvertion(stDate,stShort), 'stDate to stShort');
  Check(not CheckConvertion(stDate,stInteger), 'stDate to stInteger');
  Check(not CheckConvertion(stDate,stLong), 'stDate to stLong');
  Check(not CheckConvertion(stDate,stFloat), 'stDate to stFloat');
  Check(not CheckConvertion(stDate,stDouble), 'stDate to stDouble');
  Check(not CheckConvertion(stDate,stBigDecimal), 'stDate to stBigDecimal');
  Check(CheckConvertion(stDate,stString), 'stDate to stString');
  Check(not CheckConvertion(stDate,stBytes), 'stDate to stBytes');
  Check(not CheckConvertion(stDate,stAsciiStream), 'stDate to stAsciiStream');
  Check(not CheckConvertion(stDate,stBinaryStream), 'stDate to stBinaryStream');
  Check(CheckConvertion(stDate,stDate), 'stDate to stDate');
  Check(not CheckConvertion(stDate,stTime), 'stDate,stTime');
  Check(CheckConvertion(stDate,stTimestamp), 'stDate to stTimestamp');
  Check(not CheckConvertion(stDate,stUnicodeStream), 'stDate to stUnicodeStream');
  {check Time convertion}
  Check(not CheckConvertion(stTime,stBoolean), 'stTime to stBoolean');
  Check(not CheckConvertion(stTime,stByte), 'stTime to stByte');
  Check(not CheckConvertion(stTime,stShort), 'stTime to stShort');
  Check(not CheckConvertion(stTime,stInteger), 'stTime to stInteger');
  Check(not CheckConvertion(stTime,stLong), 'stTime to stLong');
  Check(not CheckConvertion(stTime,stFloat), 'stTime to stFloat');
  Check(not CheckConvertion(stTime,stDouble), 'stTime to stDouble');
  Check(not CheckConvertion(stTime,stBigDecimal), 'stTime to stBigDecimal');
  Check(CheckConvertion(stTime,stString), 'stTime,stString');
  Check(not CheckConvertion(stTime,stBytes), 'stTime to stBytes');
  Check(not CheckConvertion(stTime,stAsciiStream), 'stTime to stAsciiStream');
  Check(not CheckConvertion(stTime,stBinaryStream), 'stTime to stBinaryStream');
  Check(not CheckConvertion(stTime,stDate), 'stTime to stDate');
  Check(CheckConvertion(stTime,stTime), 'stTime to stTime');
  Check(CheckConvertion(stTime,stTimestamp), 'stTime,stTimestamp');
  Check(not CheckConvertion(stTime,stUnicodeStream), 'stTime to stUnicodeStream');
  {check TimeStamp convertion}
  Check(not CheckConvertion(stTimestamp,stBoolean), 'stTimestamp to stBoolean');
  Check(not CheckConvertion(stTimestamp,stByte), 'stTimestamp to stByte');
  Check(not CheckConvertion(stTimestamp,stShort), 'stTimestamp to stShort');
  Check(not CheckConvertion(stTimestamp,stInteger), 'stTimestamp to stInteger');
  Check(not CheckConvertion(stTimestamp,stLong), 'stTimestamp to stLong');
  Check(not CheckConvertion(stTimestamp,stFloat), 'stTimestamp to stFloat');
  Check(not CheckConvertion(stTimestamp,stDouble), 'stTimestamp to stDouble');
  Check(not CheckConvertion(stTimestamp,stBigDecimal), 'stTimestamp to stBigDecimal');
  Check(CheckConvertion(stTimestamp,stString), 'stTimestamp to stString');
  Check(not CheckConvertion(stTimestamp,stBytes), 'stTimestamp to stBytes');
  Check(not CheckConvertion(stTimestamp,stAsciiStream), 'stTimestamp to stAsciiStream');
  Check(not CheckConvertion(stTimestamp,stBinaryStream), 'stTimestamp to stBinaryStream');
  Check(CheckConvertion(stTimestamp,stDate), 'stTimestamp to stDate');
  Check(CheckConvertion(stTimestamp,stTime), 'stTimestamp to stTime');
  Check(CheckConvertion(stTimestamp,stTimestamp), 'stTimestamp to stTimestamp');
  Check(not CheckConvertion(stTimestamp,stUnicodeStream), 'stTimestamp to stUnicodeStream');
  {check AsciiStream convertion}
  Check(not CheckConvertion(stAsciiStream,stBoolean), 'stAsciiStream to stBoolean');
  Check(not CheckConvertion(stAsciiStream,stByte), 'stAsciiStream to stByte');
  Check(not CheckConvertion(stAsciiStream,stShort), 'stAsciiStream to stShort');
  Check(not CheckConvertion(stAsciiStream,stInteger), 'stAsciiStream to stInteger');
  Check(not CheckConvertion(stAsciiStream,stLong), 'stAsciiStream to stLong');
  Check(not CheckConvertion(stAsciiStream,stFloat), 'stAsciiStream to stFloat');
  Check(not CheckConvertion(stAsciiStream,stDouble), 'stAsciiStream to stDouble');
  Check(not CheckConvertion(stAsciiStream,stBigDecimal), 'stAsciiStream to stBigDecimal');
  Check(CheckConvertion(stAsciiStream,stString), 'stAsciiStream to stString');
  Check(CheckConvertion(stAsciiStream,stBytes), 'stAsciiStream to stBytes');
  Check(CheckConvertion(stAsciiStream,stAsciiStream), 'stAsciiStream to stAsciiStream');
  Check(not CheckConvertion(stAsciiStream,stBinaryStream), 'stAsciiStream to stBinaryStream');
  Check(not CheckConvertion(stAsciiStream,stDate), 'stAsciiStream to stDate');
  Check(not CheckConvertion(stAsciiStream,stTime), 'stAsciiStream to stTime');
  Check(not CheckConvertion(stAsciiStream,stTimestamp), 'stAsciiStream to stTimestamp');
  Check(not CheckConvertion(stAsciiStream, stUnicodeStream), 'stAsciiStream to  stUnicodeStream');
  {check BinaryStream convertion}
  Check(not CheckConvertion(stBinaryStream,stBoolean), 'stBinaryStream to stBoolean');
  Check(not CheckConvertion(stBinaryStream,stByte), 'stBinaryStream to stByte');
  Check(not CheckConvertion(stBinaryStream,stShort), 'stBinaryStream to stShort');
  Check(not CheckConvertion(stBinaryStream,stInteger), 'stBinaryStream to stInteger');
  Check(not CheckConvertion(stBinaryStream,stLong), 'stBinaryStream to stLong');
  Check(not CheckConvertion(stBinaryStream,stFloat), 'stBinaryStream to stFloat');
  Check(not CheckConvertion(stBinaryStream,stDouble), 'stBinaryStream to stDouble');
  Check(not CheckConvertion(stBinaryStream,stBigDecimal), 'stBinaryStream to stBigDecimal');
  Check(CheckConvertion(stBinaryStream,stString), 'stBinaryStream to stString');
  Check(CheckConvertion(stBinaryStream,stBytes), 'stBinaryStream to stBytes');
  Check(not CheckConvertion(stBinaryStream,stAsciiStream), 'stBinaryStream to stAsciiStream');
  Check(CheckConvertion(stBinaryStream,stBinaryStream), 'stBinaryStream to stBinaryStream');
  Check(not CheckConvertion(stBinaryStream,stDate), 'stBinaryStream to stDate');
  Check(not CheckConvertion(stBinaryStream,stTime), 'stBinaryStream to stTime');
  Check(not CheckConvertion(stBinaryStream,stTimestamp), 'stBinaryStream to stTimestamp');
  Check(not CheckConvertion(stBinaryStream,stUnicodeStream), 'stBinaryStream to stUnicodeStream');
  {check unknown convertion}
  Check(not CheckConvertion(stUnknown,stBoolean), 'stUnknown to stBoolean');
  Check(not CheckConvertion(stUnknown,stByte), 'stUnknown to stByte');
  Check(not CheckConvertion(stUnknown,stShort), 'stUnknown to stShort');
  Check(not CheckConvertion(stUnknown,stInteger), 'stUnknown to stInteger');
  Check(not CheckConvertion(stUnknown,stLong), 'stUnknown to stLong');
  Check(not CheckConvertion(stUnknown,stFloat), 'stUnknown to stFloat');
  Check(not CheckConvertion(stUnknown,stDouble), 'stUnknown to stDouble');
  Check(not CheckConvertion(stUnknown,stBigDecimal), 'stUnknown to stBigDecimal');
  Check(CheckConvertion(stUnknown,stString), 'stUnknown to stString');
  Check(not CheckConvertion(stUnknown,stBytes), 'stUnknown to stBytes');
  Check(not CheckConvertion(stUnknown,stAsciiStream), 'stUnknown to stAsciiStream');
  Check(not CheckConvertion(stUnknown,stBinaryStream), 'stUnknown to stBinaryStream');
  Check(not CheckConvertion(stUnknown,stDate), 'stUnknown to stDate');
  Check(not CheckConvertion(stUnknown,stTime), 'stUnknown to stTime');
  Check(not CheckConvertion(stUnknown,stTimestamp), 'stUnknown to stTimestamp');
  Check(not CheckConvertion(stUnknown,stUnknown), 'stUnknown to stUnknown');
  {check UnicodeStream convertion}
  Check(not CheckConvertion(stUnicodeStream,stBoolean), 'stUnicodeStream to stBoolean');
  Check(not CheckConvertion(stUnicodeStream,stByte), 'stUnicodeStream to stByte');
  Check(not CheckConvertion(stUnicodeStream,stShort), 'stUnicodeStream to stShort');
  Check(not CheckConvertion(stUnicodeStream,stInteger), 'stUnicodeStream to stInteger');
  Check(not CheckConvertion(stUnicodeStream,stLong), 'stUnicodeStream to stLong');
  Check(not CheckConvertion(stUnicodeStream,stFloat), 'stUnicodeStream to stFloat');
  Check(not CheckConvertion(stUnicodeStream,stDouble), 'stUnicodeStream to stDouble');
  Check(not CheckConvertion(stUnicodeStream,stBigDecimal), 'stUnicodeStream to stBigDecimal');
  Check(CheckConvertion(stUnicodeStream,stString), 'stUnicodeStream to stString');
  Check(CheckConvertion(stUnicodeStream,stBytes), 'stUnicodeStream to stBytes');
  Check(not CheckConvertion(stUnicodeStream,stAsciiStream), 'stUnicodeStream to stAsciiStream');
  Check(not CheckConvertion(stUnicodeStream,stBinaryStream), 'stUnicodeStream to stBinaryStream');
  Check(not CheckConvertion(stUnicodeStream,stDate), 'stUnicodeStream to stDate');
  Check(not CheckConvertion(stUnicodeStream,stTime), 'stUnicodeStream to stTime');
  Check(not CheckConvertion(stUnicodeStream,stTimestamp), 'stUnicodeStream to stTimestamp');
  Check(not CheckConvertion(stUnicodeStream,stUnknown), 'stUnicodeStream to stUnknown');
  Check(CheckConvertion(stUnicodeStream,stUnicodeStream), 'stUnicodeStream to stUnicodeStream');
end;

{**
  Test for function DefineColumnTypeName
}
procedure TZTestDbcUtilsCase.TestDefineColumnTypeName;
begin
  CheckEquals('Boolean', DefineColumnTypeName(stBoolean));
  CheckEquals('Byte', DefineColumnTypeName(stByte));
  CheckEquals('Short', DefineColumnTypeName(stShort));
  CheckEquals('Integer', DefineColumnTypeName(stInteger));
  CheckEquals('Long', DefineColumnTypeName(stLong));
  CheckEquals('Float', DefineColumnTypeName(stFloat));
  CheckEquals('Double', DefineColumnTypeName(stDouble));
  CheckEquals('BigDecimal', DefineColumnTypeName(stBigDecimal));
  CheckEquals('String', DefineColumnTypeName(stString));
  CheckEquals('Bytes', DefineColumnTypeName(stBytes));
  CheckEquals('AsciiStream', DefineColumnTypeName(stAsciiStream));
  CheckEquals('BinaryStream', DefineColumnTypeName(stBinaryStream));
  CheckEquals('Date', DefineColumnTypeName(stDate));
  CheckEquals('Time', DefineColumnTypeName(stTime));
  CheckEquals('Timestamp', DefineColumnTypeName(stTimestamp));
  CheckEquals('Unknown', DefineColumnTypeName(stUnknown));
  CheckEquals('UnicodeStream', DefineColumnTypeName(stUnicodeStream));
end;

{**
  Test for function ResolveUrl
}
procedure TZTestDbcUtilsCase.TestResolveUrl;
var
  Url, HostName, Database, UserName, Password: string;
  Info, ResultInfo: TStrings;
  Port: Integer;
begin
  Info := SplitString('trace=true;username=scott', ';');
  ResultInfo := TStringList.Create;
  try
    CheckEquals('true', Info.Values['trace']);
    CheckEquals('true', Info.Values['TRACE']);
    CheckEquals('scott', Info.Values['username']);
    CheckEquals('', Info.Values['unknownparam']);

    Url := 'zdbc:mysql://192.168.0.1:33600/test?UID=admin;PWD=none';
    ResolveDatabaseUrl(Url, Info, HostName, Port, Database,
      UserName, Password, ResultInfo);
    CheckEquals('192.168.0.1', HostName);
    CheckEquals(33600, Port);
    CheckEquals('test', Database);
    CheckEquals('admin', UserName);
    CheckEquals('none', Password);
    CheckEquals('true', ResultInfo.Values['trace']);

    Url := 'zdbc:mysql://192.168.0.1/test?PWD=none';
    ResolveDatabaseUrl(Url, Info, HostName, Port, Database,
      UserName, Password, ResultInfo);
    CheckEquals('192.168.0.1', HostName);
    CheckEquals(0, Port);
    CheckEquals('test', Database);
    CheckEquals('scott', UserName);
    CheckEquals('none', Password);
    CheckEquals('true', ResultInfo.Values['trace']);

    Url := 'zdbc:mysql:test?UID=admin;PWD=none';
    ResolveDatabaseUrl(Url, Info, HostName, Port, Database,
      UserName, Password, ResultInfo);
    CheckEquals('localhost', HostName);
    CheckEquals(0, Port);
    CheckEquals('test', Database);
    CheckEquals('admin', UserName);
    CheckEquals('none', Password);
    CheckEquals('true', ResultInfo.Values['trace']);

    Url := 'zdbc:mysql:test';
    ResolveDatabaseUrl(Url, Info, HostName, Port, Database,
      UserName, Password, ResultInfo);
    CheckEquals('localhost', HostName);
    CheckEquals(0, Port);
    CheckEquals('test', Database);
    CheckEquals('scott', UserName);
    CheckEquals('', Password);
    CheckEquals('true', ResultInfo.Values['trace']);
  finally
    Info.Free;
    ResultInfo.Free;
  end;
end;

initialization
  RegisterTest('dbc',TZTestDbcUtilsCase.Suite);
end.
