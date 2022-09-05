{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Test Case for Caching Classes               }
{                                                         }
{         Originally written by Sergey Merkuriev          }
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

unit ZTestData;

interface

{$I ZComponent.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZTestCase,ZDataset, ZConnection, Classes, SysUtils;

type
  {** Implements a abstract test case for TZDataset. }
  TZAbstractQueryCase = class(TZAbstractTestCase)
  private
    FQuery: TZQuery;

    {procedures for test modify data}
    procedure SetIntegerValues;
    procedure SetStringValues;
    {$IFNDEF FPC}
    procedure SetBlobValues;
    {$ENDIF}

    procedure CheckIntegerValuesEx;
    procedure CheckStringValuesEx;
    {$IFNDEF FPC}
    procedure CheckBlobValuesEx;
    {$ENDIF}

    function CompareStreams(Stream1, Stream2: TStream): boolean;
  protected
    {procedures for test select data}
    procedure TestQuery; virtual;
    procedure CheckIntegerValues; virtual;
    procedure CheckStringValues; virtual;
    {$IFNDEF FPC}
    procedure CheckBlobValues; virtual;
    {$ENDIF}

    {procedures for test select data}
    procedure TestAddEditDeleteRecords; virtual;
    procedure TestAddRecords; virtual;
    procedure TestEditRecords; virtual;
    procedure TestDeleteRecords; virtual;

    procedure TestExecuteQuery; virtual;
    procedure TestTransactions; virtual;
    procedure TestStoredProcedures; virtual;
    procedure TestDataBaseMetadata; virtual;
    procedure TestResultSetMetadata; virtual;
    procedure TestSort; virtual;
    procedure TestFilter; virtual;
  public
    property Query: TZQuery read FQuery write FQuery;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  Db, {$IFNDEF FPC} DbTables, {$ENDIF} ZAbstractRODataset;


{ TZAbstractQueryCase }

{**
   Comapre data of two streams
   @param The First stream
   @param The Second stream
   @return true if data equally and false if not equally
}
function TZAbstractQueryCase.CompareStreams(Stream1,
  Stream2: TStream): boolean;
var
  Size: integer;
  Buffer1, Buffer2: Pointer;
begin
  Size := Stream1.Size;
  GetMem(Buffer1, Size);
  GetMem(Buffer2, Size);

  FillChar(Buffer1^, Size, 1);
  FillChar(Buffer2^, Size, 1);

  Stream1.Position := 0;
  Stream2.Position := 0;
  Stream1.ReadBuffer(Buffer1^, Size);
  Stream2.ReadBuffer(Buffer2^, Size);

  Result := CompareMem(Buffer1, Buffer2, Size);

  FreeMem(Buffer1, Size);
  FreeMem(Buffer2, Size);
end;

//======================================================================
// Methods for test query records
//======================================================================

{**
   Test simple queries to database such as "select * from table"
}
procedure TZAbstractQueryCase.TestQuery;
begin
  if FQuery <> nil then
  begin
    CheckIntegerValues;
    CheckStringValues;
    {$IFNDEF FPC}
    CheckBlobValues;
    {$ENDIF}
  end;
end;

{$IFNDEF FPC}
{**
   Test for select data from table with blob values and test what returned
   data correct
}
procedure TZAbstractQueryCase.CheckBlobValues;
var
  FieldStream: TBlobStream;
  AsciiStream: TFileStream;
  BinaryStream: TFileStream;
begin
  AsciiStream := TFileStream.Create('..\database\lgpl.txt', fmOpenRead);
  BinaryStream := TFileStream.Create('..\database\horse.jpg', fmOpenRead);
  FQuery.SQL.Text := 'SELECT * FROM blob_values';
  FQuery.Open;
  FQuery.First;
  with FQuery do
  begin
    CheckEquals(1, Fields[1].AsInteger, 'The row index');
    CheckEquals(AsciiStream.Size, Fields[2].Size, 'Sizes Ascii data');
    CheckEquals(BinaryStream.Size, Fields[3].Size, 'Sizes Binary data');

    FieldStream := TBlobStream.Create(Fields[2] as TBlobField, bmRead);
    Check(CompareStreams(FieldStream, AsciiStream), 'Compare field and file data');
    FieldStream.Free;

    FieldStream := TBlobStream.Create(Fields[3] as TBlobField, bmRead);
    Check(CompareStreams(FieldStream, AsciiStream), 'Compare file and dfiel data');
    FieldStream.Free;
  end;
  AsciiStream.Free;
  BinaryStream.Free;
end;
{$ENDIF}

{**
   Test for select data from table with integer values and test what returned
   data correct
}
procedure TZAbstractQueryCase.CheckIntegerValues;
begin
  FQuery.SQL.Text := 'SELECT * FROM number_values';
  FQuery.Open;
  with FQuery do
  begin
    CheckEquals(1, Fields[0].AsInteger, 'The row index');
    CheckEquals(-128, Fields[1].AsInteger, 'The Byte Field');
    CheckEquals(-32768, Fields[2].AsInteger, 'The Short Field');
{$IFNDEF VER130BELOW}
//    CheckEquals(-2147483648, Fields[3].AsInteger, 'The Interger Field');
{$ENDIF}
    //CheckEquals(-9223372036854775807, Fields[4].Value, 'The BigDecimal Field');
    //CheckEquals(-9223372036854775807, Fields[5].AsInteger, 'The Numeric Field');
    CheckEquals(-3.402823466E+38, Fields[6].AsFloat, 1, 'The Float Field');
    CheckEquals(-1.7976931348623157E+308, Fields[7].AsFloat, 1,'The Real Field');
    CheckEquals(-1.7976931348623157E+308, Fields[8].AsFloat, 1,'The Double Field');
    CheckEquals(-3.402823466E+38, Fields[9].AsCurrency, 1, 'The Money Field');
    CheckEquals(EncodeDate(1000, 1, 1), Fields[10].AsDateTime, 1, 'The Date Field');
    CheckEquals(EncodeTime(0, 0, 0, 0), Fields[11].AsDateTime, 1, 'The Time ');
    CheckEquals(EncodeDate(1000, 1, 1) + EncodeTime(0, 0, 0, 0),
        Fields[12].AsDateTime, 1, 'The DateTime Field');
    CheckEquals(EncodeDate(1000, 1, 1) + EncodeTime(0, 0, 0, 0),
        Fields[13].AsDateTime, 1, 'The Timestamp Field');

    Next;
    CheckEquals(2, Fields[0].AsInteger, 'The row index');
    CheckEquals(-128, Fields[1].AsInteger, 'The Byte Field');
    CheckEquals(-32768, Fields[2].AsInteger, 'The Short Field');
{$IFNDEF VER130BELOW}
//    CheckEquals(-2147483648, Fields[3].AsInteger, 'The Interger Field');
{$ENDIF}
    //CheckEquals(-9223372036854775807, Fields[4].Value, 'The BigDecimal Field');
    //CheckEquals(-9223372036854775807, Fields[5].AsInteger, 'The Numeric Field');
    CheckEquals(-1.175494351E-38, Fields[6].AsFloat, 1, 'The Float Field');
    CheckEquals(-2.2250738585072014E-308, Fields[7].AsFloat, 1,'The Real Field');
    CheckEquals(-2.2250738585072014E-308, Fields[8].AsFloat, 1,'The Double Field');
    CheckEquals(-1.175494351E-38, Fields[9].AsCurrency, 1, 'The Money Field');
    CheckEquals(EncodeDate(1000, 1, 1), Fields[10].AsDateTime, 1, 'The Date Field');
    CheckEquals(EncodeTime(0, 0, 0, 0), Fields[11].AsDateTime, 1, 'The Time ');
    CheckEquals(EncodeDate(1000, 1, 1) + EncodeTime(0, 0, 0, 0),
        Fields[12].AsDateTime, 1, 'The DateTime Field');
    CheckEquals(EncodeDate(1000, 1, 1) + EncodeTime(0, 0, 0, 0),
        Fields[13].AsDateTime, 1, 'The Timestamp Field');

    Next;
    CheckEquals(3, Fields[0].AsInteger, 'The row index');
    CheckEquals(0, Fields[1].AsInteger, 'The Byte Field');
    CheckEquals(0, Fields[2].AsInteger, 'The Short Field');
    CheckEquals(0, Fields[3].AsInteger, 'The Interger Field');
    CheckEquals(0, Fields[4].Value, 'The BigDecimal Field');
    CheckEquals(0, Fields[5].AsInteger, 'The Numeric Field');
    CheckEquals(0, Fields[6].AsFloat, 1, 'The Float Field');
    CheckEquals(0, Fields[7].AsFloat, 1,'The Real Field');
    CheckEquals(0, Fields[8].AsFloat, 1,'The Double Field');
    CheckEquals(0, Fields[9].AsCurrency, 1, 'The Money Field');
    CheckEquals(EncodeDate(2002, 12, 29), Fields[10].AsDateTime, 1, 'The Date Field');
    CheckEquals(EncodeTime(12, 0, 0, 0), Fields[11].AsDateTime, 1, 'The Time ');
    CheckEquals(EncodeDate(2002, 12, 29) + EncodeTime(12, 0, 0, 0),
        Fields[12].AsDateTime, 1, 'The DateTime Field');
    CheckEquals(EncodeDate(2002, 12, 29) + EncodeTime(12, 0, 0, 0),
        Fields[13].AsDateTime, 1, 'The Timestamp Field');

    Next;
    CheckEquals(4, Fields[0].AsInteger, 'The row index');
    CheckEquals(128, Fields[1].AsInteger, 'The Byte Field');
    CheckEquals(32767, Fields[2].AsInteger, 'The Short Field');
    CheckEquals(2147483647, Fields[3].AsInteger, 'The Interger Field');
    //CheckEquals(-9223372036854775807, Fields[4].Value, 'The BigDecimal Field');
    //CheckEquals(-9223372036854775807, Fields[5].AsInteger, 'The Numeric Field');
    CheckEquals(3.402823466E+38, Fields[6].AsFloat, 1, 'The Float Field');
    CheckEquals(1.7976931348623157E+308, Fields[7].AsFloat, 1,'The Real Field');
    CheckEquals(1.7976931348623157E+308, Fields[8].AsFloat, 1,'The Double Field');
    CheckEquals(3.402823466E+38, Fields[9].AsCurrency, 1, 'The Money Field');
    CheckEquals(EncodeDate(9999, 12, 31), Fields[10].AsDateTime, 1, 'The Date Field');
    CheckEquals(EncodeTime(12, 59, 59, 0), Fields[11].AsDateTime, 1, 'The Time ');
    CheckEquals(EncodeDate(9999, 12, 31) + EncodeTime(12, 59, 59, 0),
        Fields[12].AsDateTime, 1, 'The DateTime Field');
    CheckEquals(EncodeDate(9999, 12, 31) + EncodeTime(12, 59, 59, 0),
        Fields[13].AsDateTime, 1, 'The Timestamp Field');

    Next;
    CheckEquals(5, Fields[1].AsInteger, 'The row index');
    CheckEquals(128, Fields[2].AsInteger, 'The Byte Field');
    CheckEquals(32767, Fields[3].AsInteger, 'The Short Field');
    CheckEquals(2147483647, Fields[4].AsInteger, 'The Interger Field');
    //CheckEquals(-9223372036854775807, Fields[5].Value, 'The BigDecimal Field');
    //CheckEquals(-9223372036854775807, Fields[6].AsInteger, 'The Numeric Field');
    CheckEquals(1.175494351E-38, Fields[7].AsFloat, 1, 'The Float Field');
    CheckEquals(2.2250738585072014E-308, Fields[8].AsFloat, 1,'The Real Field');
    CheckEquals(2.2250738585072014E-308, Fields[9].AsFloat, 1,'The Double Field');
    CheckEquals(1.175494351E-38, Fields[10].AsCurrency, 1, 'The Money Field');
        CheckEquals(EncodeDate(9999, 12, 31), Fields[11].AsDateTime, 1, 'The Date Field');
    CheckEquals(EncodeTime(12, 59, 59, 0), Fields[12].AsDateTime, 1, 'The Time ');
    CheckEquals(EncodeDate(9999, 12, 31) + EncodeTime(12, 59, 59, 0),
        Fields[13].AsDateTime, 1, 'The DateTime Field');
    CheckEquals(EncodeDate(9999, 12, 31) + EncodeTime(12, 59, 59, 0),
        Fields[14].AsDateTime, 1, 'The Timestamp Field');

    Next;
    CheckEquals(6, Fields[1].AsInteger, 'The row index');
    CheckEquals(15, Fields[2].AsInteger, 'The Byte Field');
    CheckEquals(3457, Fields[3].AsInteger, 'The Short Field');
    CheckEquals(1968754, Fields[4].AsInteger, 'The Interger Field');
    CheckEquals(645397181, Fields[5].Value, 'The BigDecimal Field');
    CheckEquals(645397181, Fields[6].AsInteger, 'The Numeric Field');
    CheckEquals(178.345123, Fields[7].AsFloat, 1, 'The Float Field');
    CheckEquals(132489.45612098, Fields[8].AsFloat, 1,'The Real Field');
    CheckEquals(98467345.3412563, Fields[9].AsFloat, 1,'The Double Field');
    CheckEquals(178.345123, Fields[10].AsCurrency, 1, 'The Money Field');
        CheckEquals(EncodeDate(1979, 07, 07), Fields[11].AsDateTime, 1, 'The Date Field');
    CheckEquals(EncodeTime(11, 34, 12, 0), Fields[12].AsDateTime, 1, 'The Time ');
    CheckEquals(EncodeDate(1979, 07, 07) + EncodeTime(11, 34, 12, 0),
        Fields[13].AsDateTime, 1, 'The DateTime Field');
    CheckEquals(EncodeDate(1979, 07, 07) + EncodeTime(11, 34, 12, 0),
        Fields[14].AsDateTime, 1, 'The Timestamp Field');
  end;
  FQuery.Close;
end;

{**
   Test for select data from table with string values and test what returned
   data correct
}
procedure TZAbstractQueryCase.CheckStringValues;
const
  Str1 = 'This license, the Lesser General Public License, applies to some specially designated software packages--typically libraries--of the Free Software Foundation and other authors who decide to use it.  You can use it too, but we suggest you first think ...';
  Str2 = 'Одной из наиболее тривиальных задач, решаемых многими коллективами программистов, является построение информационной системы для автоматизации бизнес-деятельности предприятия. Все архитектурные компоненты (базы данных, сервера приложений, клиентское ...';
var
  Str: string;
begin
  FQuery.SQL.Text := 'SELECT * FROM string_values';
  FQuery.Open;
  FQuery.First;
  with FQuery do
  begin
    CheckEquals(1, Fields[0].AsInteger, 'The row index');
    CheckEquals('', Fields[1].AsString, 'The Char Field');
    CheckEquals('', Fields[2].AsString, 'The VarChar Field');
    CheckEquals('', Fields[3].AsString, 'The NChar Field');
    CheckEquals('', Fields[4].AsString, 'The NVarChar Field');
    CheckEquals('', Fields[5].AsString, 'The Bytes Field');
    CheckEquals('', Fields[6].AsString, 'The VarBytes Field');

    Next;
    CheckEquals(2, Fields[0].AsInteger, 'The row index');
    CheckEquals('Test string', Fields[1].AsString, 'The Char Field');
    CheckEquals('Test string', Fields[2].AsString, 'The VarChar Field');
    CheckEquals('Тестовая строка', Fields[3].AsString, 'The NChar Field');
    CheckEquals('Тестовая строка', Fields[4].AsString, 'The NVarChar Field');
    CheckEquals('0123456789', Fields[5].AsString, 'The Bytes Field');
    CheckEquals('01234567890123456789', Fields[6].AsString, 'The VarBytes Field');

    Next;
    while Length(Str) < 255 do
      Str := Str + '1';
    CheckEquals(3, Fields[0].AsInteger, 'The row index');
    CheckEquals(Str1, Fields[1].AsString, 'The Char Field');
    CheckEquals(Str1, Fields[2].AsString, 'The VarChar Field');
    CheckEquals(Str2, Fields[3].AsString, 'The NChar Field');
    CheckEquals(Str2, Fields[4].AsString, 'The NVarChar Field');
    CheckEquals(Str, Fields[5].AsString, 'The Bytes Field');
    CheckEquals(Str, Fields[6].AsString, 'The VarBytes Field');
  end;
end;

{**
   Test operations with transactions
}
procedure TZAbstractQueryCase.TestTransactions;
begin

end;


//======================================================================
// Methods for test add edit delete records
//======================================================================

{$IFNDEF FPC}
{**
   Test added or updated blob values
}
procedure TZAbstractQueryCase.CheckBlobValuesEx;
var
  FieldStream: TBlobStream;
  AsciiStream: TFileStream;
  BinaryStream: TFileStream;
begin
  AsciiStream := TFileStream.Create('..\database\gnu.txt', fmOpenRead);
  BinaryStream := TFileStream.Create('..\database\horse.jpg', fmOpenRead);
  with FQuery do
  begin
    CheckEquals(AsciiStream.Size, Fields[2].Size, 'Sizes Ascii data');
    CheckEquals(BinaryStream.Size, Fields[3].Size, 'Sizes Binary data');

    FieldStream := TBlobStream.Create(Fields[2] as TBlobField, bmRead);
    Check(CompareStreams(FieldStream, AsciiStream), 'Compare field and file data');
    FieldStream.Free;

    FieldStream := TBlobStream.Create(Fields[3] as TBlobField, bmRead);
    Check(CompareStreams(FieldStream, AsciiStream), 'Compare file and dfiel data');
    FieldStream.Free;
  end;
  AsciiStream.Free;
  BinaryStream.Free;
end;
{$ENDIF}

{**
   Test added or updated Integer values
}
procedure TZAbstractQueryCase.CheckIntegerValuesEx;
begin
  with Query do
  begin
    CheckEquals(15, Fields[2].AsInteger, 'The Byte Field');
    CheckEquals(3457, Fields[3].AsInteger, 'The Short Field');
    CheckEquals(1968754, Fields[4].AsInteger, 'The Interger Field');
    CheckEquals(645397181, Fields[5].AsInteger, 'The BigDecimal Field');
    CheckEquals(645397182, Fields[6].AsInteger, 'The Numeric Field');
    CheckEquals(178.345123, Fields[7].AsInteger, 'The Float Field');
    CheckEquals(132489.45612098, Fields[8].AsInteger, 'The Real Field');
    CheckEquals(98467345.3412563, Fields[9].AsInteger, 'The Double Field');
    CheckEquals(178.345123, Fields[10].AsInteger, 'The Money Field');
    CheckEquals(EncodeDate(1979, 07, 07), Fields[11].AsInteger, 'The Date Field');
    CheckEquals(EncodeTime(11, 34, 12, 0), Fields[12].AsInteger, 'The Time Field');
    CheckEquals(EncodeDate(1979, 07, 07) + EncodeTime(11, 34, 12, 0),
      Fields[13].AsInteger, 'The DateTime Field');
    CheckEquals(EncodeDate(1979, 03, 03) + EncodeTime(7, 7, 0, 0),
      Fields[14].AsInteger, 'The Timestamp Field');
 end;
end;

{**
   Test added or updated string values
}
procedure TZAbstractQueryCase.CheckStringValuesEx;
begin
  with Query do
  begin
    CheckEquals('The test string', Fields[2].AsString, 'The Char Field');
    CheckEquals('The some test string', Fields[3].AsString, 'The VarChar Field');
    CheckEquals('Тестовая трока', Fields[4].AsString, 'The NChar Field');
    CheckEquals('Просто тестовая строка', Fields[5].AsString, 'The NVarChar Field');
    CheckEquals('1234567890', Fields[6].AsString, 'The Bytes Field');
    CheckEquals('12345678901234567890', Fields[7].AsString, 'The VarBytes Field');
 end;
end;

{$IFNDEF FPC}
procedure TZAbstractQueryCase.SetBlobValues;
var
  Stream: TFileStream;
  BlobStream: TBlobStream;
begin
  with Query do
  begin
    Fields[1].AsInteger := 100;

    Stream := TFileStream.Create('..\gnu.txt', fmOpenRead);
    BlobStream := TBlobStream.Create(Fields[2] as TBlobField, bmReadWrite);
    BlobStream.Position := 0;
    BlobStream.CopyFrom(Stream, Stream.Size);

    Stream := TFileStream.Create('..\gnu.txt', fmOpenRead);
    BlobStream := TBlobStream.Create(Fields[2] as TBlobField, bmReadWrite);
    BlobStream.Position := 0;
    BlobStream.CopyFrom(Stream, Stream.Size);
 end;
end;
{$ENDIF}

{**
   Set the integer values for added or edited record
}
procedure TZAbstractQueryCase.SetIntegerValues;
begin
  with Query do
  begin
    Fields[1].AsInteger := 100;
    Fields[2].AsInteger := 15;
    Fields[3].AsInteger := 3457;
    Fields[4].AsInteger := 1968754;
    Fields[5].AsInteger := 645397181;
    Fields[6].AsInteger := 645397182;
    Fields[7].AsFloat := 178.345123;
    Fields[8].AsFloat := 132489.45612098;
    Fields[9].AsFloat := 98467345.3412563;
    Fields[10].AsCurrency := 178.345123;
    Fields[11].AsDateTime := EncodeDate(1979, 07, 07);
    Fields[12].AsDateTime := EncodeTime(11, 34, 12, 0);
    Fields[13].AsDateTime := EncodeDate(1979, 07, 07) + EncodeTime(11, 34, 12, 0);
    Fields[14].AsDateTime := EncodeDate(1979, 03, 03) + EncodeTime(7, 7, 0, 0);
 end;
end;

{**
   Set the string values for added or edited record
}
procedure TZAbstractQueryCase.SetStringValues;
begin
  with Query do
  begin
    Fields[1].AsInteger := 100;
    Fields[2].AsString := 'The test string';
    Fields[3].AsString := 'The some test string';
    Fields[4].AsString := 'Тестовая трока';
    Fields[5].AsString := 'Просто тестовая строка';
    Fields[6].AsString := '1234567890';
    Fields[7].AsString := '12345678901234567890';
 end;
end;

{**
    Test add, edit and deleting by TZDataset
}
procedure TZAbstractQueryCase.TestAddEditDeleteRecords;
begin
  if FQuery <> nil then
  begin
    TestAddRecords;
    TestEditRecords;
    TestDeleteRecords;
  end;
end;

{**
    Test add records by TZDataset
}
procedure TZAbstractQueryCase.TestAddRecords;
begin
  FQuery.SQL.Text := 'SELECT * FROM number_values';
  FQuery.Open;
  FQuery.Append;
  SetIntegerValues;
  CheckIntegerValuesEx;
  FQuery.Post;
  FQuery.Close;

  FQuery.SQL.Text := 'SELECT * FROM string_values';
  FQuery.Open;
  FQuery.Append;
  {$IFNDEF FPC}
  SetBlobValues;
  {$ENDIF}
  CheckStringValuesEx;
  FQuery.Post;
  FQuery.Close;

  FQuery.SQL.Text := 'SELECT * FROM blob_values';
  FQuery.Open;
  FQuery.Append;
  {$IFNDEF FPC}
  SetBlobValues;
  CheckBlobValuesEx;
  {$ENDIF}
  FQuery.Post;
  FQuery.Close;

  FQuery.SQL.Text := 'SELECT * FROM number_values WHERE n_id = 100';
  FQuery.Open;
  CheckIntegerValuesEx;
  FQuery.Close;

  FQuery.SQL.Text := 'SELECT * FROM string_values WHERE s_id = 100';
  FQuery.Open;
  CheckStringValuesEx;
  FQuery.Close;

  {$IFNDEF FPC}
  FQuery.SQL.Text := 'SELECT * FROM blob_values WHERE b_id = 100';
  FQuery.Open;
  CheckBlobValuesEx;
  FQuery.Close;
  {$ENDIF}
end;

{**
    Test delete records by TZDataset
}
procedure TZAbstractQueryCase.TestDeleteRecords;
begin
  with Query do
  begin
    SQL.Text := 'SELECT * FROM number_values WHERE n_id = 100';
    Open;
    Check(not IsEmpty, '1. Dataset empty');
    Delete;
    Check(IsEmpty, '2. Dataset empty');
    Close;

    SQL.Text := 'SELECT * FROM string_values WHERE s_id = 100';
    Open;
    Check(not IsEmpty, '3. Dataset empty');
    Delete;
    Check(IsEmpty, '4. Dataset empty');
    Close;

    SQL.Text := 'SELECT * FROM blob_values WHERE b_id = 100';
    Open;
    Check(not IsEmpty, '5. Dataset empty');
    Delete;
    Check(IsEmpty, '6. Dataset empty');
    Close;

    SQL.Text := 'SELECT * FROM number_values n_id = 100';
    Open;
    Check(IsEmpty, '7. Dataset empty');
    Close;

    SQL.Text := 'SELECT * FROM string_values s_id = 100';
    Open;
    Check(IsEmpty, '8. Dataset empty');
    Close;

    SQL.Text := 'SELECT * FROM blob_values b_id = 100';
    Open;
    Check(IsEmpty, '9. Dataset empty');
    Close;
  end;
end;

{**
    Test edit records by TZDataset
}
procedure TZAbstractQueryCase.TestEditRecords;
begin
  with Query do
  begin
    SQL.Text := 'SELECT * FROM number_values n_id = 50';
    Open;
    Edit;
    SetIntegerValues;
    Post;
    Close;

    SQL.Text := 'SELECT * FROM string_values s_id = 50';
    Open;
    Edit;
    SetStringValues;
    Post;
    Close;

    {$IFNDEF FPC}
    SQL.Text := 'SELECT * FROM blob_values b_id = 50';
    Open;
    Edit;
    SetBlobValues;
    Post;
    Close;
    {$ENDIF}

    SQL.Text := 'SELECT * FROM number_values n_id = 50';
    Open;
    CheckIntegerValuesEx;
    Close;

    SQL.Text := 'SELECT * FROM string_values s_id = 50';
    Open;
    CheckStringValuesEx;
    Close;

    {$IFNDEF FPC}
    SQL.Text := 'SELECT * FROM blob_values b_id = 50';
    Open;
    CheckBlobValuesEx;
    Close;
    {$ENDIF}
  end;
end;

//======================================================================
// Methods for test resutset and database metadata
//======================================================================

{**
    Test database metadata
}
procedure TZAbstractQueryCase.TestDataBaseMetadata;
begin

end;

{**
    Test resultset metadata
}
procedure TZAbstractQueryCase.TestResultSetMetadata;
begin

end;

//======================================================================
// Methods for test execute query
//======================================================================

{**
    Test execute queries what do not return data
}
procedure TZAbstractQueryCase.TestExecuteQuery;
begin
  with FQuery do
  begin
    SQL.Text := 'DELETE FROM number_values WHERE n_id > 10';
    ExecSQL;

    SQL.Text := 'DELETE FROM string_values WHERE s_id > 10';
    ExecSQL;

    SQL.Text := 'DELETE FROM blob_values WHERE b_id > 10';
    ExecSQL;

    SQL.Text := 'SELECT * FROM number_values n_id = 100';
    Open;
    Check(IsEmpty, '1. Dataset empty');
    Close;

    SQL.Text := 'SELECT * FROM string_values s_id = 100';
    Open;
    Check(IsEmpty, '2. Dataset empty');
    Close;

    SQL.Text := 'SELECT * FROM blob_values b_id = 100';
    Open;
    Check(IsEmpty, '3. Dataset empty');
    Close;
  end;
end;

{**
    Test execute stored procedures and check values if procedure return it
}
procedure TZAbstractQueryCase.TestStoredProcedures;
begin
  with FQuery do
  begin
    SQL.Text := 'DELETE FROM number_values WHERE n_id > 10';
    Open;
  end;  
end;

//======================================================================
// Methods for test addithional functionality
//======================================================================

{**
    Test filtering recods in TZDataset
}
procedure TZAbstractQueryCase.TestFilter;
begin

end;

{**
    Test sorting recods in TZDataset
}
procedure TZAbstractQueryCase.TestSort;
begin

end;

end.
