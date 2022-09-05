{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Test Case for Caching Classes               }
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

unit ZTestDbcCachedResultSet;

interface
{$I ZDbc.inc}
uses
  Types, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZDbcCachedResultSet, ZClasses, ZCollections, ZDbcIntfs,
  ZSysUtils, ZDbcResultSet, ZDbcCache, Classes, ZDbcResultSetMetadata,
  Contnrs, ZCompatibility, ZTestConsts, ZDbcMetadata, ZTestDefinitions;

type

 {** Implements a test case for TZRowAccessor. }
  TZTestCachedResultSetCase = class(TZDbcGenericTestCase)
  private
    FBoolean: Boolean;
    FByte: ShortInt;
    FShort: SmallInt;
    FInt: Integer;
    FLong: LongInt;
    FFloat: Single;
    FDouble: Double;
    FBigDecimal: Int64;
    FString: string;
    FDate: TDateTime;
    FTime: TDateTime;
    FTimeStamp: TDateTime;
    FAsciiStream: TStream;
    FUnicodeStream: TStream;
    FBinaryStream: TStream;
    FByteArray: TByteDynArray;
    FAsciiStreamData: string;
    FUnicodeStreamData: WideString;
    FBinaryStreamData: Pointer;
    FResultSet: IZCachedResultSet;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    function GetColumnsInfo(Index: Integer; ColumnType: TZSqlType;
      Nullable: TZColumnNullableType; ReadOnly: Boolean;
      Writable: Boolean): TZColumnInfo;
    function GetColumnsInfoCollection: TObjectList;
    function CompareArrays(Array1, Array2: TByteDynArray): Boolean;
    procedure FillResultSet(ResultSet: IZResultSet; RecCount: integer);
    function CompareStreams(Stream1, Stream2: TStream): boolean; overload;

  published
    procedure TestTraversalAndPositioning;
    procedure TestInsert;
    procedure TestUpdate;
    procedure TestDelete;
    procedure TestOtherFunctions;
    procedure TestFillMaxValues;
    procedure TestCachedUpdates;
  end;

  {** Defines an empty resolver class. }
  TZEmptyResolver = class(TInterfacedObject, IZCachedResolver)
  public
    procedure CalculateDefaults(Sender: IZCachedResultSet;
      RowAccessor: TZRowAccessor);
    procedure PostUpdates(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor);
    {BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    procedure UpdateAutoIncrementFields(Sender: IZCachedResultSet;
      UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor; Resolver: IZCachedResolver); virtual;
    {END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    procedure RefreshCurrentRow(Sender: IZCachedResultSet;RowAccessor: TZRowAccessor);
  end;

implementation

uses SysUtils;

{ TZTestCachedResultSetCase }

{**
  Compares two byte arrays
  @param Array1 the first array to compare.
  @param Array2 the second array to compare.
  @return <code>True</code> if arrays are equal.
}
function TZTestCachedResultSetCase.CompareArrays(Array1,
  Array2: TByteDynArray): Boolean;
var
  I: Integer;
begin
  Result := False;
  if High(Array2) <> High(Array1) then Exit;
  for I := 0 to High(Array1) do
    if Array1[I] <> Array2[I] then Exit;
  Result := True;
end;

procedure TZTestCachedResultSetCase.FillResultSet(
  ResultSet: IZResultSet; RecCount: Integer);
var
  I: Integer;
begin
 with ResultSet do
   for I := 0 to RecCount-1 do
   begin
     MoveToInsertRow;
     UpdateBoolean(1, FBoolean);
     UpdateByte(2, FByte);
     UpdateShort(3, FShort);
     UpdateInt(4, FInt);
     UpdateLong(5, FLong);
     UpdateFloat(6, FFloat);
     UpdateDouble(7, FDouble);
     UpdateBigDecimal(8, FBigDecimal);
     UpdateString(9, FString);
     UpdateBytes(10, FByteArray);
     UpdateDate(11, FDate);
     UpdateTime(12, FTime);
     UpdateTimestamp(13, FTimestamp);
     UpdateAsciiStream(14, FAsciiStream);
     UpdateUnicodeStream(15, FUnicodeStream);
     UpdateBinaryStream(16, FBinaryStream);
     InsertRow;
   end;
end;

function TZTestCachedResultSetCase.GetColumnsInfo(Index: Integer;
  ColumnType: TZSqlType; Nullable: TZColumnNullableType; ReadOnly,
  Writable: Boolean): TZColumnInfo;
begin
  Result := TZColumnInfo.Create;

  Result.AutoIncrement := True;
  Result.CaseSensitive := True;
  Result.Searchable := True;
  Result.Currency := True;
  Result.Nullable := Nullable;
  Result.Signed := True;
  Result.ColumnDisplaySize := 32;
  Result.ColumnLabel := 'Column'+IntToStr(Index);
  Result.ColumnName := 'Column'+IntToStr(Index);
  Result.SchemaName := 'TestSchemaName';
  case ColumnType of
    stString: Result.Precision := 255;
    stBytes: Result.Precision := 5;
  else
    Result.Precision := 0;
  end;
  Result.Scale := 5;
  Result.TableName := 'TestTableName';
  Result.CatalogName := 'TestCatalogName';
  Result.ColumnType := ColumnType;
  Result.ReadOnly := ReadOnly;
  Result.Writable := Writable;
  Result.DefinitelyWritable := Writable;
end;

{**
  Create IZCollection and fill it by ZColumnInfo objects
  @return the ColumnInfo object
}
function TZTestCachedResultSetCase.GetColumnsInfoCollection: TObjectList;
begin
  Result := TObjectList.Create;
  with Result do
  begin
    Add(GetColumnsInfo(1, stBoolean, ntNullable, False, True));
    Add(GetColumnsInfo(2, stByte, ntNullable, False, True));
    Add(GetColumnsInfo(3, stShort, ntNullable, False, True));
    Add(GetColumnsInfo(4, stInteger, ntNullable, False, True));
    Add(GetColumnsInfo(5, stLong, ntNullable, False, True));
    Add(GetColumnsInfo(6, stFloat, ntNullable, False, True));
    Add(GetColumnsInfo(7, stDouble, ntNullable, False, True));
    Add(GetColumnsInfo(8, stBigDecimal, ntNullable, False, True));
    Add(GetColumnsInfo(9, stString, ntNullable, False, True));
    Add(GetColumnsInfo(10, stBytes, ntNullable, False, True));
    Add(GetColumnsInfo(11, stDate, ntNullable, False, True));
    Add(GetColumnsInfo(12, stTime, ntNullable, False, True));
    Add(GetColumnsInfo(13, stTimestamp, ntNullable, False, True));
    Add(GetColumnsInfo(14, stAsciiStream, ntNullable, False, True));
    Add(GetColumnsInfo(15, stUnicodeStream, ntNullable, False, True));
    Add(GetColumnsInfo(16, stBinaryStream, ntNullable, False, True));
  end;
end;

{**
  Setup paramters for test such as variables, stream datas and streams
}
procedure TZTestCachedResultSetCase.SetUp;
var
  BufferChar: PChar;
  BufferWideChar: PWideChar;
begin
  FDate := SysUtils.Date;
  FTime := SysUtils.Time;
  FTimeStamp := SysUtils.Now;

  FAsciiStream := TMemoryStream.Create;
  FAsciiStreamData := 'Test Ascii Stream Data';
  BufferChar := PChar(FAsciiStreamData);
  FAsciiStream.Write(BufferChar^, Length(FAsciiStreamData));

  FUnicodeStream := TMemoryStream.Create;
  FUnicodeStreamData := 'Test Unikode Stream Data';
  BufferWideChar := PWideChar(FUnicodeStreamData);
  FUnicodeStream.Write(BufferWideChar^, Length(FUnicodeStreamData) * 2);

  FBinaryStream := TMemoryStream.Create;
  FBinaryStreamData := AllocMem(BINARY_BUFFER_SIZE);
  FBinaryStream.Write(FBinaryStreamData^, BINARY_BUFFER_SIZE);

  FBoolean := true;
  FByte := 127;
  FShort := 32767;
  FInt := 2147483647;
  FLong := 1147483647;
  FFloat := 3.4E-38;
  FDouble := 1.7E-308;
  FBigDecimal := 9223372036854775807;
  FString := '0123456789';

  SetLength(FByteArray, 5);
  FByteArray[0] := 0;
  FByteArray[1] := 1;
  FByteArray[2] := 2;
  FByteArray[3] := 3;
  FByteArray[4] := 4;
end;

{**
  Free paramters for test such as stream datas and streams
}
procedure TZTestCachedResultSetCase.TearDown;
begin
  FAsciiStream.Free;
  FUnicodeStream.Free;
  FBinaryStream.Free;
  FreeMem(FBinaryStreamData);
end;

{**
  Test Delete values in ResultSet
}
procedure TZTestCachedResultSetCase.TestDelete;
var
  I: Integer;
  Warnings: EZSQLWarning;
  Collection: TObjectList;
  CachedResultSet: TZAbstractCachedResultSet;
begin
  Collection := GetColumnsInfoCollection;
  try
    CachedResultSet := TZAbstractCachedResultSet.CreateWithColumns(
      Collection, '');
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetType(rtScrollInsensitive);

    FResultSet := CachedResultSet;
    FResultSet.SetResolver(TZEmptyResolver.Create);

    with FResultSet do
    begin
     for I := 0 to MAX_POS_ELEMENT-1 do
     begin
       MoveToInsertRow;
       InsertRow;
     end;

    { Delete rows }
     First;
     DeleteRow;
     Check(RowDeleted, 'The row is deleted');
     while Next do
     begin
       DeleteRow;
       Check(RowDeleted, 'The row is deleted');
     end;

     { Delete rows }
     First;
     while Next do
     try
      UpdateNull(1);
  //!!    Fail('Row deleted and can''t be edited');
     except
     end;


     CheckEquals('', GetCursorName, 'GetCursorName');
     { Check Warnings working}
     Warnings := GetWarnings;
     CheckNull(Warnings);
     ClearWarnings;

     { Check FetchSize }
     SetFetchSize(MAX_ELEMENT-100);
     CheckEquals(MAX_ELEMENT-100, GetFetchSize, 'GetFetchSize');
    end;
    FResultSet := nil;
  finally
    Collection.Free;
  end;
end;

{**
  Test set and get data row function and additional function for
  TZCachedResultSet, such as UpdateFloat,  UpdateRow, MoveToInsertRow
  and etc.
}
procedure TZTestCachedResultSetCase.TestTraversalAndPositioning;
var
  I: Integer;
  Collection: TObjectList;
  CachedResultSet: TZAbstractCachedResultSet;
  Successful: Boolean;
begin
  Collection := GetColumnsInfoCollection;
  try
    CachedResultSet := TZAbstractCachedResultSet.CreateWithColumns(
      Collection, '');
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetType(rtScrollInsensitive);
    FResultSet := CachedResultSet;
    FResultSet.SetResolver(TZEmptyResolver.Create);

    with FResultSet do
    begin
       { Fill ResultSet }
       for I := 0 to MAX_POS_ELEMENT-1 do
       begin
         MoveToInsertRow;
         UpdateInt(4, I);
         InsertRow;
       end;

      { Test First Method }
      Check(First, 'Move to first record');
      CheckEquals(1, GetRow, '1.GetRow');

      { Test Last method }
      Check(Last, 'Test to move to last record');;
      CheckEquals(MAX_POS_ELEMENT, GetRow, '2.GetRow');

      { Test MoveAbsolute }
      Check(MoveAbsolute(1), 'Move to first record');
      Check(IsFirst, 'IsFirst');

      Check(MoveAbsolute(MAX_POS_ELEMENT), 'Move to last record');
      Check(IsLast, 'IsLast');

      Check(MoveAbsolute(100), 'Move to 100th record');
      CheckEquals(100, GetRow, 'GetRow must be 100');
      Check(MoveAbsolute(533), 'Move to 533th record');
      CheckEquals(533, GetRow, 'GetRow must be 533');
      Check(MoveAbsolute(50), 'Move to 50th record');
      CheckEquals(50, GetRow, 'GetRow must be 50');
      Check(not MoveAbsolute(1533), 'Move to 1533th record');
      CheckEquals(50, GetRow, 'GetRow must be 50');
  //    Check(not MoveAbsolute(-1533), 'Move to -1533th record');
  //    CheckEquals(50, GetRow, 'GetRow must be 50');

      Check(not MoveAbsolute(0), 'Move to last record');
      Check(IsBeforeFirst, 'Record position is last');
      Check(not MoveAbsolute(MAX_POS_ELEMENT + 1), 'Move to record after last');
      Check(IsAfterLast, 'Record position is after last');

      { Test MoveRelative }
      MoveAbsolute(100);
      CheckEquals(100, GetRow, 'GetRow must be 100');
      Check(MoveRelative(50), 'Increase position on 50');
      CheckEquals(150, GetRow, 'GetRow must be 150');
      Check(MoveRelative(-100), 'Deccrease position on 100');
      CheckEquals(50, GetRow, 'GetRow must be 50');
  //    Check(not MoveRelative(-10000), 'Deccrease position on 10000');
  //    CheckEquals(50, GetRow, 'GetRow must be 50');
      Check(not MoveRelative(10000), 'Increase position on 10000');
      CheckEquals(50, GetRow, 'GetRow must be 50');

      { Test BeforeFirst }
      Check(not IsBeforeFirst, 'not IsBeforeFirst');
      BeforeFirst;
      Check(IsBeforeFirst, 'IsBeforeFirst');

      { Test BeforeFirst }
      Check(not IsFirst, 'not IsFirst');
      First;
      Check(IsFirst, 'IsFirst');

      { Test BeforeFirst }
      Check(not IsLast, 'not IsLast');
      Last;
      Check(IsLast, 'IsLast');

      { Test BeforeFirst }
      Check(not IsAfterLast, 'not IsAfterLast');
      AfterLast;
      Check(IsAfterLast, 'IsAfterLast');

      { Test IsBeforeFirst, IsFirst, IsLast, IsAfterLast }
      Check(MoveAbsolute(1), '2.Go to first row');
      Check(not IsBeforeFirst, '2.IsBeforeFirst row');
      Check(IsFirst, '2.IsFirst row');
      Check(not IsLast, '2.IsLast row');
      Check(not IsAfterLast, '2.IsAfterLast row');

      Check(MoveAbsolute(MAX_POS_ELEMENT), '3.Go to MAX_ELEMENT row');
      Check(not IsBeforeFirst, '3.IsBeforeFirst row');
      Check(not IsFirst, '3.IsFirst row');
      Check(IsLast, '3.IsLast row');
      Check(not IsAfterLast, '3.IsAfterLast row');

      Check(MoveAbsolute(MAX_POS_ELEMENT div 2), 'Go to MAX_ELEMENT div 2 row');
      Check(MoveAbsolute(MAX_POS_ELEMENT div 2), 'Go to MAX_ELEMENT div 2 row');
      Check(not IsBeforeFirst, '5.IsBeforeFirst row');
      Check(not IsFirst, '5.IsFirst row');
      Check(not IsLast, '5.IsLast row');
      Check(not IsAfterLast, '5.IsAfterLast row');

      { Test Next method }
      First;
      while not IsLast do
        Next;

      { Test Previous method }
      Last;
      while not IsFirst do
        Previous;

      Successful := True;
    end;
    FResultSet := nil;

    if not Successful then
      Fail('Test for traversal and positioning failed');
  finally
    Collection.Free;
  end;
end;

{**
 Test Inset values to the ResultSet
}
procedure TZTestCachedResultSetCase.TestInsert;
var
  I: integer;
  Blob: IZBlob;
  Stream: TStream;
  Collection: TObjectList;
  ByteArray: TByteDynArray;
  CachedResultSet: TZAbstractCachedResultSet;
begin
  Collection := GetColumnsInfoCollection;
  try
    CachedResultSet := TZAbstractCachedResultSet.CreateWithColumns(
      Collection, '');
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetType(rtScrollInsensitive);
    CachedResultSet.SetCachedUpdates(True);

    FResultSet := CachedResultSet;
    FResultSet.SetResolver(TZEmptyResolver.Create);

    with FResultSet do
    begin
      { Test FindColumn }
      CheckEquals(1,  FindColumn('Column1'), 'FindColumn with name Column1');
      CheckEquals(8,  FindColumn('Column8'), 'FindColumn with name Column8');
      CheckEquals(16,  FindColumn('Column16'), 'FindColumn with name Column16');

      First;
      for I := 0 to ROWS_COUNT do
      begin
        {Test Insert Record}
        MoveToInsertRow;
        UpdateBoolean(1, FBoolean);
        UpdateByte(2, FByte);
        UpdateShort(3, FShort);
        UpdateInt(4, FInt);
        UpdateLong(5, FLong);
        UpdateFloat(6, FFloat);
        UpdateDouble(7, FDouble);
        UpdateBigDecimal(8, FBigDecimal);
        UpdateString(9, FString);
        UpdateBytes(10, FByteArray);
        UpdateDate(11, FDate);
        UpdateTime(12, FTime);
        UpdateTimestamp(13, FTimestamp);
        UpdateAsciiStream(14, FAsciiStream);
        UpdateUnicodeStream(15, FUnicodeStream);
        UpdateBinaryStream(16, FBinaryStream);
        InsertRow;
        Check(RowInserted);
      end;

      First;
      for i := 0 to ROWS_COUNT do
      begin
        CheckEquals(FBoolean, GetBoolean(1), 'GetBoolean');
        CheckEquals(FByte, GetByte(2), 'GetByte');
        CheckEquals(FShort, GetShort(3), 'GetShort');
        CheckEquals(FInt, GetInt(4), 'GetInt');
        CheckEquals(FLong, GetLong(5), 'GetLong');
        CheckEquals(FFloat, GetFloat(6), 0, 'GetFloat');
        CheckEquals(FDouble, GetDouble(7), 0, 'GetDouble');
        CheckEquals(FBigDecimal, GetBigDecimal(8), 0, 'GetBigDecimal');
        CheckEquals(FString, GetString(9), 'GetString');
        CheckEquals(FDate, GetDate(11), 0, 'GetDate');
        CheckEquals(FTime, GetTime(12), 0, 'GetTime');
        CheckEquals(FTimeStamp, GetTimestamp(13), 0, 'GetTimestamp');

        ByteArray := GetBytes(10);
        Check(CompareArrays(FByteArray, ByteArray));

        CheckEquals(False, FResultSet.WasNull, 'WasNull');

        { GetBlob }
        Blob := GetBlob(14);
        if (Blob = nil) or Blob.IsEmpty then
          Fail('asciistream emty');
        Blob := nil;

        { AciiStream check }
        try
          Stream := GetAsciiStream(14);
          Check(CompareStreams(Stream, FAsciiStream), 'AsciiStream');
          Stream.Free;
        except
          Fail('Incorrect GetBinaryStream method behavior');
        end;

        { UnicodeStream check }
        try
          Stream := GetUnicodeStream(15);
          Check(CompareStreams(Stream, FUnicodeStream), 'UnicodeStream');
          Stream.Free;
        except
          Fail('Incorrect GetUnicodeStream method behavior');
        end;

        { BinaryStream check }
        try
          Stream := GetBinaryStream(16);
          Check(CompareStreams(Stream, FBinaryStream), 'BinaryStream');
          Stream.Free;
        except
          Fail('Incorrect GetBinaryStream method behavior');
        end;
      end;

      for i := 0 to ROWS_COUNT do
      begin
        { Check MoveToCurrentRow and MoveToInsertRow }
        MoveToInsertRow;
        Check(IsNull(1), '1. IsNull column number 1');
        Check(IsNull(2), '1. IsNull column number 2');
        Check(IsNull(3), '1. IsNull column number 3');
        MoveToCurrentRow;
        CheckEquals(FBoolean, GetBoolean(1), 'GetBoolean to current row');
        CheckEquals(FByte, GetByte(2), 'GetByte to current row');
        CheckEquals(FShort, GetShort(3), 'GetShort to current row');
        MoveToInsertRow;
        Check(IsNull(1), '2. IsNull column number 1');
        Check(IsNull(2), '2. IsNull column number 2');
        Check(IsNull(3), '2. IsNull column number 3');
      end;
    end;
    FResultSet := nil;
  finally
    Collection.Free;
  end;
end;

{**
 Test Update values in the ResultSet
}
procedure TZTestCachedResultSetCase.TestUpdate;
var
  I: integer;
  Blob: IZBlob;
  Collection: TObjectList;
  CachedResultSet: TZAbstractCachedResultSet;
begin
  Collection := GetColumnsInfoCollection;
  try
    CachedResultSet := TZAbstractCachedResultSet.CreateWithColumns(
      Collection, '');
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetType(rtScrollInsensitive);
    CachedResultSet.SetCachedUpdates(True);

    FResultSet := CachedResultSet;
    FResultSet.SetResolver(TZEmptyResolver.Create);
    FillResultSet(FResultSet, ROWS_COUNT);

    with FResultSet do
    begin
      { Update record values to null}
      First;
      while Next do
      begin
        for I := 1 to 16 do
         UpdateNull(I);
        UpdateRow;
        Check(RowUpdated or RowInserted, 'Row updated with null fields');
      end;
      { Check what updated record values to null}
      First;
      while Next do
      begin
        for I := 1 to 16 do
          Check(IsNull(I), 'The field '+IntToStr(I)+' did not set to null');
      end;

      { Set row values }
      First;
      while Next do
      begin
        UpdateBooleanByName('Column1', FBoolean);
        UpdateByteByName('Column2', FByte);
        UpdateShortByName('Column3', FShort);
        UpdateIntByName('Column4', FInt);
        UpdateLongByName('Column5', FLong);
        UpdateFloatByName('Column6', FFloat);
        UpdateDoubleByName('Column7', FDouble);
        UpdateBigDecimalByName('Column8', FBigDecimal);
        UpdateStringByName('Column9', FString);
        UpdateBytesByName('Column10', FByteArray);
        UpdateDateByName('Column11', FDate);
        UpdateTimeByName('Column12', FTime);
        UpdateTimestampByName('Column13', FTimestamp);
        UpdateAsciiStreamByName('Column14', FAsciiStream);
        UpdateUnicodeStreamByName('Column15', FUnicodeStream);
        UpdateBinaryStreamByName('Column16', FBinaryStream);
        UpdateRow;
        Check(RowUpdated or RowInserted, 'RowUpdated');
      end;
      { Test what set row values}
      First;
      while Next do
      begin
        { Check what fields changed in previous operation }
        CheckEquals(FBoolean, GetBoolean(1), 'Field changed; GetBoolean.');
        CheckEquals(FByte, GetByte(2), 'Field changed; GetByte');
        CheckEquals(FShort, GetShort(3), 'Field changed; GetShort');
        CheckEquals(FInt, GetInt(4), 'Field changed; GetInt');
        CheckEquals(FLong, GetLong(5), 'Field changed; GetLong');
        CheckEquals(FFloat, GetFloat(6), 0, 'Field changed; GetFloat');
        CheckEquals(FDouble, GetDouble(7), 0, 'Field changed; GetDouble');
        CheckEquals(FBigDecimal, GetBigDecimal(8), 0, 'Field changed; GetBigDecimal');
        CheckEquals(FString, GetString(9), 'Field changed; GetString');
        CheckEquals(FDate, GetDate(11), 0, 'Field changed; GetDate');
        CheckEquals(FTime, GetTime(12), 0, 'Field changed; GetTime');
        CheckEquals(FTimeStamp, GetTimestamp(13), 0, 'Field changed; GetTimestamp');
        { GetBlob }
        Blob := GetBlob(14);
        if (Blob = nil) or Blob.IsEmpty then
          Fail('asciistream emty');
        Blob := nil;

        Blob := GetBlob(15);
        if (Blob = nil) or Blob.IsEmpty then
          Fail('unicodestream emty');
        Blob := nil;

        Blob := GetBlob(16);
        if (Blob = nil) or Blob.IsEmpty then
          Fail('binarystream emty');
        Blob := nil;
      end;

      { Clear values }
      First;
      while Next do
      begin
        for I := 1 to 16 do
         UpdateNull(I);
        UpdateRow;
      end;

      { Set values. And call CancelRowUpdates
        for test what original values shall return }
      First;
      while Next do
      begin
        for i := 1 to 16 do
          UpdateNull(I);
        UpdateRow;
        { Test CancelRowUpdates and update row columns by it names}
        UpdateBooleanByName('Column1', FBoolean);
        UpdateByteByName('Column2', FByte);
        UpdateShortByName('Column3', FShort);
        UpdateIntByName('Column4', FInt);
        UpdateLongByName('Column5', FLong);
        UpdateFloatByName('Column6', FFloat);
        UpdateDoubleByName('Column7', FDouble);
        UpdateBigDecimalByName('Column8', FBigDecimal);
        UpdateStringByName('Column9', FString);
        UpdateBytesByName('Column10', FByteArray);
        UpdateDateByName('Column11', FDate);
        UpdateTimeByName('Column12', FTime);
        UpdateTimestampByName('Column13', FTimestamp);
        UpdateAsciiStreamByName('Column14', FAsciiStream);
        UpdateUnicodeStreamByName('Column15', FUnicodeStream);
        UpdateBinaryStreamByName('Column16', FBinaryStream);
        CancelRowUpdates;
        Check(RowUpdated or RowInserted, 'The row did not updated');
      end;
      { Check what fields still set to null}
      First;
      while Next do
      begin
        for i := 1 to 16 do
         Check(IsNull(I), 'The field '+IntToStr(I)+' did not still equals null');
      end;
    end;
    FResultSet := nil;
  finally
    Collection.Free;
  end;
end;

{**
 Test frunctions such as SetFetchDirection, GetWarnings, SetFetchSize etc.
}
procedure TZTestCachedResultSetCase.TestOtherFunctions;
var
  I: Integer;
  Collection: TObjectList;
  Warnings: EZSQLWarning;
  CachedResultSet: TZVirtualResultSet;
begin
  Collection := GetColumnsInfoCollection;
  try
    CachedResultSet := TZVirtualResultSet.CreateWithColumns(
      Collection, '');
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetType(rtScrollInsensitive);

    FResultSet := CachedResultSet;

    with FResultSet do
    begin
  {
      try
        SetFetchDirection(fdForward);
      except
        Fail('Incorrect SetFetchDirection fdForward behavior');
      end;
      try
        SetFetchDirection(fdReverse);
        Fail('Incorrect SetFetchDirection fdReverse behavior');
      except
      end;
      try
        SetFetchDirection(fdUnknown);
        Fail('Incorrect SetFetchDirection fdUnknown behavior');
      except
      end;
  }

      for I := 0 to MAX_POS_ELEMENT-1 do
      begin
        MoveToInsertRow;
        InsertRow;
      end;

      CheckEquals('', GetCursorName, 'GetCursorName');

      { Check Warnings working}
      Warnings := GetWarnings;
      CheckNull(Warnings);
      ClearWarnings;

      { Check FetchSize }
      SetFetchSize(MAX_ELEMENT-100);
      CheckEquals(MAX_ELEMENT-100, GetFetchSize, 'GetFetchSize');
    end;
    FResultSet := nil;
  finally
    Collection.Free;
  end;
end;

{**
  Compare two streams.
  @param the first stream
  @param the second stream
  @result if two streams equals then result true otherwise false
}
function TZTestCachedResultSetCase.CompareStreams(Stream1, Stream2: TStream):
  Boolean;
var
  Buffer1, Buffer2: array[0..1024] of Char;
  ReadNum1, ReadNum2: Integer;
begin
  CheckNotNull(Stream1, 'Stream #1 is null');
  CheckNotNull(Stream2, 'Stream #2 is null');
  CheckEquals(Stream1.Size, Stream2.Size, 'Stream sizes are not equal');

  Stream1.Position := 0;
  ReadNum1 := Stream1.Read(Buffer1, 1024);
  Stream2.Position := 0;
  ReadNum2 := Stream2.Read(Buffer2, 1024);

  CheckEquals(ReadNum1, ReadNum2, 'Read sizes are not equal.');
  Result := CompareMem(@Buffer1, @Buffer2, ReadNum1);

  Stream1.Position := 0;
  Stream2.Position := 0;
end;

{**
  Test class for store much rows and work with they.
}
procedure TZTestCachedResultSetCase.TestFillMaxValues;
var
  I: integer;
  Collection: TObjectList;
  TimeStart, TimeEnd: TDateTime;
  CachedResultSet: TZAbstractCachedResultSet;
begin
  PrintLn;
  PrintLn('Test work with ' + IntToStr(MAX_ELEMENT) + ' elements');

  Collection := GetColumnsInfoCollection;
  try
    CachedResultSet := TZAbstractCachedResultSet.CreateWithColumns(
      Collection, '');
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetType(rtScrollInsensitive);

    FResultSet := CachedResultSet;
    FResultSet.SetResolver(TZEmptyResolver.Create);

    { Test for fill ResultSet speed }
    TimeStart := Now;
    FillResultSet(FResultSet, MAX_ELEMENT);
    TimeEnd := Now;
    PrintLn('Fill for ' + TimeToStr(TimeStart - TimeEnd));

    with FResultSet do
    begin
      { Test for set to null rows speed }
      First;
      TimeStart := Now;
      while Next do
      begin
        for I := 1 to 16 do
          UpdateNull(I);
        UpdateRow;
      end;
      TimeEnd := Now;
      PrintLn('Fields all resords set to null for '
        + TimeToStr(TimeStart-TimeEnd));

      { Test for set values speed }
      First;
      TimeStart := Now;
      while Next do
      begin
        { Test Insert Record }
        MoveToInsertRow;
        UpdateBoolean(1, FBoolean);
        UpdateByte(2, FByte);
        UpdateShort(3, FShort);
        UpdateInt(4, FInt);
        UpdateLong(5, FLong);
        UpdateFloat(6, FFloat);
        UpdateDouble(7, FDouble);
        UpdateBigDecimal(8, FBigDecimal);
        UpdateString(9, FString);
        UpdateBytes(10, FByteArray);
        UpdateDate(11, FDate);
        UpdateTime(12, FTime);
        UpdateTimestamp(13, FTimestamp);
        //UpdateAsciiStream(14, FAsciiStream);
        //UpdateUnicodeStream(15, FUnicodeStream);
        //UpdateBinaryStream(16, FBinaryStream);
        InsertRow;
      end;
      TimeEnd := Now;
      PrintLn('Set records values for'+TimeToStr(TimeStart-TimeEnd));

      { Test for delete rows speed }
      First;
      TimeStart := Now;
      DeleteRow;
      while Next do
        DeleteRow;
      TimeEnd := Now;
      PrintLn('Deleted for '+TimeToStr(TimeStart-TimeEnd));
    end;
    FResultSet := nil;
  finally
    Collection.Free;
  end;
end;

{**
  Test class for cached updates.
}
procedure TZTestCachedResultSetCase.TestCachedUpdates;
var
  Collection: TObjectList;
  CachedResultSet: TZAbstractCachedResultSet;
begin
  Collection := GetColumnsInfoCollection;
  try
    CachedResultSet := TZAbstractCachedResultSet.CreateWithColumns(
      Collection, '');
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetType(rtScrollInsensitive);
    CachedResultSet.SetCachedUpdates(True);

    FResultSet := CachedResultSet;
    FResultSet.SetResolver(TZEmptyResolver.Create);

    { Tests cancel updates. }
    CachedResultSet.MoveToInsertRow;
    CachedResultSet.UpdateBoolean(1, True);
    CachedResultSet.InsertRow;
    CheckEquals(1, CachedResultSet.GetRow);

    CachedResultSet.MoveToInsertRow;
    CachedResultSet.UpdateBoolean(1, True);
    CachedResultSet.InsertRow;
    CheckEquals(2, CachedResultSet.GetRow);

    CachedResultSet.MoveToInsertRow;
    CachedResultSet.UpdateBoolean(1, True);
    CachedResultSet.InsertRow;
    CheckEquals(3, CachedResultSet.GetRow);

    CachedResultSet.DeleteRow;
    Check(CachedResultSet.RowDeleted);

    CachedResultSet.MoveAbsolute(2);
    CachedResultSet.RevertRecord;
    Check(CachedResultSet.RowDeleted);

    CachedResultSet.CancelUpdates;
    CachedResultSet.MoveAbsolute(1);
    Check(CachedResultSet.RowDeleted);
  finally
    Collection.Free;
  end;
end;

{ TZEmptyResolver }

{**
  Calculate default values for the fields.
  @param Sender a cached result set object.
  @param RowAccessor an accessor object to column values.
}
procedure TZEmptyResolver.CalculateDefaults(Sender: IZCachedResultSet;
  RowAccessor: TZRowAccessor);
begin
end;

{**
  Posts cached updates.
  @param Sender a sender CachedResultSet object.
  @param UpdateType a type of posted updates.
  @param OldRowAccessor a row accessor which contains old column values.
  @param NewRowAccessor a row accessor which contains new column values.
}
procedure TZEmptyResolver.PostUpdates(Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; OldRowAccessor,
  NewRowAccessor: TZRowAccessor);
begin
//  Check(UpdateType <> utUnmodified);
(*
  if NewRowAccessor.RowBuffer.Index < 10 then
  begin
    PrintLn('Posted updates for row# ', NewRowAccessor.RowBuffer.Index,
      ' code# ', Ord(UpdateType));
  end;
*)
end;

{BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
procedure TZEmptyResolver.UpdateAutoIncrementFields(
  Sender: IZCachedResultSet; UpdateType: TZRowUpdateType; OldRowAccessor,
  NewRowAccessor: TZRowAccessor; Resolver: IZCachedResolver);
begin
 //Should be implemented at Specific database Level Cached resolver
end;
{END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }

procedure TZEmptyResolver.RefreshCurrentRow(Sender: IZCachedResultSet;RowAccessor: TZRowAccessor);
begin
 //Should be implemented at Specific database Level Cached resolver
end;


initialization
  RegisterTest('dbc',TZTestCachedResultSetCase.Suite);
end.
