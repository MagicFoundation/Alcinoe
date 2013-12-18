{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Test Suite for Analysing SQL Classes           }
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

unit ZTestSqlAnalyser;

interface
{$I ZParseSql.inc}
uses Contnrs, ZClasses, {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZTokenizer, ZGenericSqlAnalyser,
  ZSelectSchema, ZTestDefinitions;

type

  {** Implements a test case for Generic SQL Analyser classes. }
  TZTestStatementAnalyser = class(TZParseSQLGenericTestCase)
  protected
    FAnalyser: IZStatementAnalyser;
    FTokenizer: IZTokenizer;

    function SchemaToString(SelectSchema: IZSelectSchema): string;

    procedure TearDown; override;
    procedure RunPerformanceTest(Query: string);
  published
    procedure TestSplitting; virtual;
    procedure TestComposing; virtual;
    procedure TestSelectParser; virtual;
  end;

  {** Implements a test case for Generic SQL analyser classes. }
  TZTestGenericStatementAnalyser = class(TZTestStatementAnalyser)
  protected
    procedure SetUp; override;
  published
    procedure TestPerformance;
    procedure TestPerformance1;
  end;

{$IFDEF ENABLE_DBLIB}
  {** Implements a test case for Sybase SQL analyser classes. }
  TZTestSybaseStatementAnalyser = class(TZTestStatementAnalyser)
  protected
    procedure SetUp; override;
  end;
{$ENDIF}
{$IFDEF ENABLE_INTERBASE}
  {** Implements a test case for Interbase SQL analyser classes. }
  TZTestInterbaseStatementAnalyser = class(TZTestStatementAnalyser)
  protected
    procedure SetUp; override;
  end;
{$ENDIF}

implementation

uses Classes, SysUtils, ZCollections, ZGenericSqlToken,
{$IFDEF ENABLE_DBLIB}ZSybaseToken, ZSybaseAnalyser, {$ENDIF}
{$IFDEF ENABLE_INTERBASE}ZInterbaseToken, ZInterbaseAnalyser, {$ENDIF}
ZCompatibility;

{ TZTestStatementAnalyser }

{**
  Gets a string representation for this object.
  @return a string representation for this object.
}
function TZTestStatementAnalyser.SchemaToString(
  SelectSchema: IZSelectSchema): string;
var
  I: Integer;
  Temp: string;
  FieldRef: TZFieldRef;
  TableRef: TZTableRef;
begin
  Result := 'SS:[';

  { Composes field references. }
  for I := 0 to SelectSchema.FieldCount - 1 do
  begin
    FieldRef := SelectSchema.Fields[I];
    Temp := FieldRef.Field;
    if FieldRef.Table <> '' then
      Temp := FieldRef.Table + '.' + Temp;
    if FieldRef.Schema <> '' then
      Temp := FieldRef.Schema + '.' + Temp;
    if FieldRef.Catalog <> '' then
      Temp := FieldRef.Catalog + '.' + Temp;
    if FieldRef.Alias <> '' then
      Temp := Temp + '/' + FieldRef.Alias;

    if I > 0 then
      Result := Result + ',';
    Result := Result + 'FR:' + Temp;
  end;
  Result := Result + '][';

  { Composes table references. }
  for I := 0 to SelectSchema.TableCount - 1 do
  begin
    TableRef := SelectSchema.Tables[I];
    Temp := TableRef.Table;
    if TableRef.Schema <> '' then
      Temp := TableRef.Schema + '.' + Temp;
    if TableRef.Catalog <> '' then
      Temp := TableRef.Catalog + '.' + Temp;
    if TableRef.Alias <> '' then
      Temp := Temp + '/' + TableRef.Alias;

    if I > 0 then
      Result := Result + ',';
    Result := Result + 'TR:' + Temp;
  end;
  Result := Result + ']';
end;

{**
  Cleans up the test environment after tests.
}
procedure TZTestStatementAnalyser.TearDown;
begin
  FAnalyser := nil;
  FTokenizer := nil;
end;

{**
  Runs a test for splitting procedures.
}
procedure TZTestStatementAnalyser.TestSplitting;
var
  Tokens: TStrings;
  Sections: TObjectList;
  Section: TZStatementSection;
begin
  Tokens := FAnalyser.TokenizeQuery(FTokenizer,
    'select * from Table order by Id', False);
  try
    CheckEquals(13, Tokens.Count);

    Sections := FAnalyser.SplitSections(Tokens);
    try
      CheckEquals(3, Sections.Count);

      Section := TZStatementSection(Sections[0]);
      CheckEquals('SELECT', Section.Name);
      CheckEquals(4, Section.Tokens.Count);

      Section := TZStatementSection(Sections[1]);
      CheckEquals('FROM', Section.Name);
      CheckEquals(4, Section.Tokens.Count);

      Section := TZStatementSection(Sections[2]);
      CheckEquals('ORDER*BY', Section.Name);
      CheckEquals(5, Section.Tokens.Count);
    finally
      Sections.Free;
    end;
  finally
    Tokens.Free;
  end;

  Tokens := FAnalyser.TokenizeQuery(FTokenizer,
    'select "select" ((select))', True);
  try
    CheckEquals(9, Tokens.Count);

    Sections := FAnalyser.SplitSections(Tokens);
    try
      CheckEquals(1, Sections.Count);
    finally
      Sections.Free;
    end;
  finally
    Tokens.Free;
  end;
end;

{**
  Runs a test for composing procedures.
}
procedure TZTestStatementAnalyser.TestComposing;
var
  Tokens: TStrings;
  Sections: TObjectList;
begin
  Tokens := FAnalyser.TokenizeQuery(FTokenizer, 'select * from Table order by Id',
    False);
  try
    CheckEquals(13, Tokens.Count);
    CheckEquals('select * from Table order by Id',
      FAnalyser.ComposeTokens(Tokens));
  finally
    Tokens.Free;
  end;

  Tokens := FAnalyser.TokenizeQuery(FTokenizer, ' select Order from Table'
    + ' Where Id in (select * from Items)', False);
  Sections := FAnalyser.SplitSections(Tokens);
  try
    CheckEquals(4, Sections.Count);
    CheckEquals(' select Order from Table Where Id in (select * from Items)',
      FAnalyser.ComposeSections(Sections));
  finally
    Tokens.Free;
    Sections.Free;
  end;
end;

{**
  Runs a performance tests for the specified query.
  @paramm Query a query to run the test.
}
procedure TZTestStatementAnalyser.RunPerformanceTest(Query: string);
var
  StartTicks: Cardinal;
  Tokens: TStrings;
  SelectSchema: IZSelectSchema;
begin
  { Runs tokenizing test. }
  StartTicks := GetTickCount;
  Tokens := FAnalyser.TokenizeQuery(FTokenizer, Query, False);
  try
    CheckNotNull(Tokens);
  finally
    Tokens.Free;
  end;
  PrintLn(Format('Subtest: "Tokenizing" (2), Time: %d',
    [GetTickCount - StartTicks]));

  { Runs overall performance test. }
  StartTicks := GetTickCount;
  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer, Query);
  CheckNotNull(SelectSchema);
  PrintLn(Format('Subtest: "Overall Performance (2)", Time: %d',
    [GetTickCount - StartTicks]));
end;

{**
  Runs a test for select parser.
}
procedure TZTestStatementAnalyser.TestSelectParser;
var
  SelectSchema: IZSelectSchema;
begin
  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT Field FROM Table');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:Field][TR:Table]', SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT * FROM Table');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:*][TR:Table]', SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT Table.* FROM Schema.Table');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:Table.*][TR:Schema.Table]', SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT Schema.Table.Field AS Alias FROM Schema.Table AS Alias');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:Schema.Table.Field/Alias][TR:Schema.Table/Alias]',
    SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT Schema.Table.Field Alias FROM Schema.Table Alias');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:Schema.Table.Field/Alias][TR:Schema.Table/Alias]',
    SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT Field Alias1 FROM Table Alias2');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:Field/Alias1][TR:Table/Alias2]',
    SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT Field1, 1 + MAX(1,FROM+2) Alias1, Field2 Alias2 FROM Table1, Table2');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:Field1,FR:/Alias1,FR:Field2/Alias2][TR:Table1,TR:Table2]',
    SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT * FROM Table');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:*][TR:Table]', SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT ALL * FROM Table');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:*][TR:Table]', SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT ALL * FROM Table WHERE 0<>0');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:*][TR:Table]', SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT DISTINCT *, A AS B, MAX(Sum) AS C FROM Table AS T WHERE 0<>0');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:*,FR:A/B,FR:/C][TR:Table/T]',
    SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT DISTINCT *, A AS B, Somefunction(Sum,2,re) AS C FROM Table AS T WHERE 0<>0');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:*,FR:A/B,FR:/C][TR:Table/T]',
    SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT * FROM D1.A1 AS T1 LEFT OUTER JOIN A2 AS T2 ON A1.K=T2.K');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:*][TR:D1.A1/T1,TR:A2/T2]',
    SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT * FROM D1.A1 AS T1 LEFT OUTER JOIN A2 AS T2 ON A1.K=T2.K JOIN T3 ON T2.K=T3.K');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:*][TR:D1.A1/T1,TR:A2/T2,TR:T3]',
    SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT D1.T1.A1 AS A, T2.A1 AS B, MAX(Sum) AS C'
    + ' FROM D1.A1 AS T1 LEFT OUTER JOIN A2 AS T2 ON A1.K=T2.K'
    + ' WHERE 0<>0 FOR UPDATE');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:D1.T1.A1/A,FR:T2.A1/B,FR:/C][TR:D1.A1/T1,TR:A2/T2]',
    SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT DISTINCT *, A AS B, MAX(Sum) AS C FROM Table AS T WHERE 0<>0'
    + ' GROUP BY F, G ORDER BY I, H');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:*,FR:A/B,FR:/C][TR:Table/T]',
    SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT 2+2 AS C FROM Table AS T');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:/C][TR:Table/T]',
    SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT a.eq_id as id FROM equipment a');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:a.eq_id/id][TR:equipment/a]',
    SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT a.eq_id as id, a.eq_name as name, a.eq_type as type1,'
    + ' a.eq_cost + 10 as cost FROM equipment a where a.eq_id = 34767');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:a.eq_id/id,FR:a.eq_name/name,FR:a.eq_type/type1,FR:/cost]'
    + '[TR:equipment/a]', SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT f1, f2, t1.f3 from t2 inner join t1 on (t2.f3 = t1.f3)');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:f1,FR:f2,FR:t1.f3][TR:t2,TR:t1]',
    SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'select * from prd, bmi left join bom on if(bmi_prd_bom_ID is NULL,'
    + ' bom_ID=prd_bom_ID, bom_ID=bmi_prd_bom_ID) where prd_ID=bmi_prd_ID'
    + ' and bmi_bom_ID=10 order by bmi_INDEX');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:*][TR:prd,TR:bmi,TR:bom]',
    SchemaToString(SelectSchema));

  SelectSchema := FAnalyser.DefineSelectSchemaFromQuery(FTokenizer,
    'SELECT f1, f2, t1.f3 from t2 inner join t1 on t2.f3 = t1.f3, t3');
  CheckNotNull(SelectSchema);
  CheckEquals('SS:[FR:f1,FR:f2,FR:t1.f3][TR:t2,TR:t1,TR:t3]',
    SchemaToString(SelectSchema));
end;

{ TZTestGenericStatementAnalyser }

{**
  Sets up the test environment before tests.
}
procedure TZTestGenericStatementAnalyser.SetUp;
begin
  FAnalyser := TZGenericStatementAnalyser.Create;
  FTokenizer := TZGenericSQLTokenizer.Create;
end;

{**
  Runs a performance tests.
}
procedure TZTestGenericStatementAnalyser.TestPerformance;
var
  Query: string;
begin
  PrintLn('');
  Query := 'select * from SomeTable';
  PrintLn(Format('*** Testing Parser: %s, Test #%d', [Query, 1]));
  RunPerformanceTest(Query);

  PrintLn('');
  Query := 'select * from SomeTable where col1 = 1';
  PrintLn(Format('*** Testing Parser: %s, Test #%d', [Query, 2]));
  RunPerformanceTest(Query);

  PrintLn('');
  Query := 'select * from SomeTable where col1 = 1 and col2 = 2 and col3 = 3'
    + ' and col4 = 4';
  PrintLn(Format('*** Testing Parser: %s, Test #%d', [Query, 4]));
  RunPerformanceTest(Query);

  PrintLn('');
  Query := 'select id, cast(num || '' '' ||'
    + ' get_room_type_name(room_type) as varchar(255)) as name'
    + ' from room where hotel=1 and room_type=2 and'
    + ' (select count(*) from reservation_day where reservation_day.room=room.id'
    + ' and reservation_day.r_date between ''2003-04-09'' and ''2003-04-21'')=0'
    + ' ORDER BY name';
  PrintLn(Format('*** Testing Parser: %s, Test #%d', [Query, 6]));
  RunPerformanceTest(Query);

  PrintLn('');
  Query := 'select idpozycjatyp, format(wartosc,2) a, wartoscind, slowo,'
    + ' kolejnosc from wywiadownia.tsprpozycja p, slowniki.tslownik s'
    + ' where idsprawozdanie=425228 and p.idpozycjatyp=s.idslownik'
    + ' and s.idjezyktyp=3';
  PrintLn(Format('*** Testing Parser: %s, Test #%d', [Query, 1]));
  RunPerformanceTest(Query);

  PrintLn('');
  Query := 'select idzdarzenietyp, miasto, nrref,'
    + ' substring(date_format(publikacjidt,"%d.%m.%Y"), 11-publikacjidttyp,'
    + ' publikacjidttyp) publikacji, cb1,substring(date_format(dt1,"%d.%m.%Y"),'
    + ' 11-dt1dttyp, dt1dttyp) dt1, cb2,substring(date_format(dt2,"%d.%m.%Y"),'
    + ' 11-dt2dttyp, dt2dttyp) dt2, cb3,substring(date_format(dt3,"%d.%m.%Y"),'
    + ' 11-dt3dttyp, dt3dttyp) dt3, idzdarzenieinfo, idzdarzenie'
    + ' from wywiadownia.tzdarzenie where idzrodlatyp<>379 and idfirma=2800'
    + ' and widocznosc=1 order by idzdarzenietyp';
  PrintLn(Format('*** Testing Parser: %s, Test #%d', [Query, 3]));
  RunPerformanceTest(Query);

  PrintLn('');
  Query := 'SELECT UserName, FullName, Office, o.Name OfficeName,'
    + ' c.Name CityName, c.Region, r.Name RegionName'
    + ' FROM Users u LEFT OUTER JOIN offices o ON u.Office = o.Code'
    + ' LEFT OUTER JOIN cities c ON o.City = c.PostalCode'
    + ' LEFT OUTER JOIN regions r ON c.Region = r.Code ORDER BY 1';
  PrintLn(Format('*** Testing Parser: %s, Test #%d', [Query, 4]));
  RunPerformanceTest(Query);
end;

procedure TZTestGenericStatementAnalyser.TestPerformance1;
var
  I: Integer;
  Query: string;
  StartTicks: Cardinal;
begin
  StartTicks := GetTickCount;
  for I := 1 to 1000 do
  begin
    Query := 'select idzdarzenietyp, miasto, nrref,'
      + ' substring(date_format(publikacjidt,"%d.%m.%Y"), 11-publikacjidttyp,'
      + ' publikacjidttyp) publikacji, cb1,substring(date_format(dt1,"%d.%m.%Y"),'
      + ' 11-dt1dttyp, dt1dttyp) dt1, cb2,substring(date_format(dt2,"%d.%m.%Y"),'
      + ' 11-dt2dttyp, dt2dttyp) dt2, cb3,substring(date_format(dt3,"%d.%m.%Y"),'
      + ' 11-dt3dttyp, dt3dttyp) dt3, idzdarzenieinfo, idzdarzenie'
      + ' from wywiadownia.tzdarzenie where idzrodlatyp<>379 and idfirma=2800'
      + ' and widocznosc=1 order by idzdarzenietyp';
    FAnalyser.DefineSelectSchemaFromQuery(FTokenizer, Query);
  end;
  PrintLn(Format('Subtest: "Overall Performance (3)", Time: %d',
    [GetTickCount - StartTicks]));
end;

{$IFDEF ENABLE_DBLIB}
{ TZTestSybaseStatementAnalyser }

{**
  Sets up the test environment before tests.
}
procedure TZTestSybaseStatementAnalyser.SetUp;
begin
  FAnalyser := TZSybaseStatementAnalyser.Create;
  FTokenizer := TZSybaseTokenizer.Create;
end;
{$ENDIF}

{$IFDEF ENABLE_INTERBASE}
{ TZTestInterbaseStatementAnalyser }

{**
  Sets up the test environment before tests.
}
procedure TZTestInterbaseStatementAnalyser.SetUp;
begin
  FAnalyser := TZInterbaseStatementAnalyser.Create;
  FTokenizer := TZInterbaseTokenizer.Create;
end;
{$ENDIF}

initialization
  RegisterTest('parsesql',TZTestGenericStatementAnalyser.Suite);
{$IFDEF ENABLE_DBLIB}
  RegisterTest('parsesql',TZTestSybaseStatementAnalyser.Suite);
{$ENDIF}
{$IFDEF ENABLE_INTERBASE}
  RegisterTest('parsesql',TZTestInterbaseStatementAnalyser.Suite);
{$ENDIF}
end.

