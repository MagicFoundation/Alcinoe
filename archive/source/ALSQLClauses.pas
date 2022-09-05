{*******************************************************************************
SQL function to create easily sql string without
take care if it's an update or insert sql Statement.
just add the value in a tstringList like
fieldname=value and at the end contruct the sql string
*******************************************************************************}

unit ALSQLClauses;

interface

uses
  System.Contnrs,
  AlStringList,
  ALFbxClient;

Type

  {------------------------------------------------------------------------}
  TALSQLClauseUpdateKind = (alDelete, alUpdate, AlInsert, AlUpdateOrInsert);
  TALSQLClauseServerType = (alFirebird, AlSphinx, AlMySql);

  {---------------------------------}
  TAlSelectSQLClause = Class(Tobject)
  public
    ServerType: TALSQLClauseServerType;
    First: integer;
    Skip: Integer;
    Distinct: Boolean;
    Select: TALStrings;
    Where: TALStrings;
    From: TALStrings;
    Join: TALStrings;
    GroupBy: TALStrings;
    Having: TALStrings;
    Plan: ansiString; // dedicated to Firebird
    OrderBy: TALStrings;
    Custom: TALStrings;
    FBXClientSQLParams: TALFBXClientSQLParams; // dedicated to Firebird
    Constructor Create; Virtual;
    Destructor Destroy; override;
    Procedure clear; virtual;
    procedure Assign(Source: TAlSelectSQLClause); virtual;
    function SQLText: AnsiString; virtual;
    function FBXClientSelectDataSQL(const ViewTag, RowTag: AnsiString): TALFBXClientSelectDataQUERY; overload;
    function FBXClientSelectDataSQL(const RowTag: AnsiString): TALFBXClientSelectDataQUERY; overload;
    function FBXClientSelectDataSQL: TALFBXClientSelectDataQUERY; overload;
  end;

  {--------------------------------------}
  TAlSelectSQLClauses = class(TObjectList)
  private
  protected
    function GetItems(Index: Integer): TAlSelectSQLClause;
    procedure SetItems(Index: Integer; aSelectSQLClause: TAlSelectSQLClause);
  public
    function Add(aSelectSQLClause: TAlSelectSQLClause): Integer;
    function Extract(Item: TAlSelectSQLClause): TAlSelectSQLClause;
    function Remove(aSelectSQLClause: TAlSelectSQLClause): Integer;
    function IndexOf(aSelectSQLClause: TAlSelectSQLClause): Integer;
    function First: TAlSelectSQLClause;
    function Last: TAlSelectSQLClause;
    procedure Insert(Index: Integer; aSelectSQLClause: TAlSelectSQLClause);
    property Items[Index: Integer]: TAlSelectSQLClause read GetItems write SetItems; default;
    function FBXClientSelectDataQUERIES(const ViewTag, RowTag: AnsiString): TALFBXClientSelectDataQUERIES; overload;
    function FBXClientSelectDataQUERIES(const RowTag: AnsiString): TALFBXClientSelectDataQUERIES; overload;
    function FBXClientSelectDataQUERIES: TALFBXClientSelectDataQUERIES; overload;
  end;

  {---------------------------------}
  TALUpdateSQLClause = Class(Tobject)
    ServerType: TALSQLClauseServerType;
    Kind: TALSQLClauseUpdateKind;
    Table: ansiString;
    Value: TALStrings;
    Where: TALStrings;
    Custom: TALStrings;
    FBXClientSQLParams: TALFBXClientSQLParams; // dedicated to Firebird
    Constructor Create; Virtual;
    Destructor Destroy; override;
    Procedure clear; virtual;
    procedure Assign(Source: TALUpdateSQLClause); virtual;
    function SQLText: AnsiString; virtual;
    function FBXClientUpdateDataSQL(const ViewTag, RowTag: AnsiString): TALFBXClientUpdateDataQUERY; overload;
    function FBXClientUpdateDataSQL(const RowTag: AnsiString): TALFBXClientUpdateDataQUERY; overload;
    function FBXClientUpdateDataSQL: TALFBXClientUpdateDataQUERY; overload;
  end;

  {--------------------------------------}
  TAlUpdateSQLClauses = class(TObjectList)
  private
  protected
    function GetItems(Index: Integer): TAlUpdateSQLClause;
    procedure SetItems(Index: Integer; aUpdateSQLClause: TAlUpdateSQLClause);
  public
    function Add(aUpdateSQLClause: TAlUpdateSQLClause): Integer;
    function Extract(Item: TAlUpdateSQLClause): TAlUpdateSQLClause;
    function Remove(aUpdateSQLClause: TAlUpdateSQLClause): Integer;
    function IndexOf(aUpdateSQLClause: TAlUpdateSQLClause): Integer;
    function First: TAlUpdateSQLClause;
    function Last: TAlUpdateSQLClause;
    procedure Insert(Index: Integer; aUpdateSQLClause: TAlUpdateSQLClause);
    property Items[Index: Integer]: TAlUpdateSQLClause read GetItems write SetItems; default;
    function FBXClientUpdateDataQUERIES(const ViewTag, RowTag: AnsiString): TALFBXClientUpdateDataQUERIES; overload;
    function FBXClientUpdateDataQUERIES(const RowTag: AnsiString): TALFBXClientUpdateDataQUERIES; overload;
    function FBXClientUpdateDataQUERIES: TALFBXClientUpdateDataQUERIES; overload;
  end;

implementation

uses
  System.Classes,
  System.Types,
  AlString;

{************************************}
constructor TAlSelectSQLClause.Create;
Begin
  ServerType := alFirebird;
  First := -1;
  Skip := -1;
  Distinct := False;
  Select:= TALStringList.create;
  Where:= TALStringList.create;
  From:= TALStringList.create;
  Join:= TALStringList.create;
  GroupBy:= TALStringList.create;
  Having:= TALStringList.create;
  Plan := '';
  OrderBy := TALStringList.create;
  Custom := TALStringList.create;
  setlength(FBXClientSQLParams, 0);
end;

{************************************}
destructor TAlSelectSQLClause.Destroy;
begin
  Select.free;
  Where.free;
  From.free;
  Join.free;
  GroupBy.free;
  Having.free;
  OrderBy.free;
  Custom.free;
  inherited;
end;

{**************************************************************}
procedure TAlSelectSQLClause.Assign(Source: TAlSelectSQLClause);
begin
  ServerType := Source.ServerType;
  First := Source.First;
  Skip := Source.Skip;
  Distinct := source.Distinct;
  Select.Assign(source.Select);
  Where.Assign(source.Where);
  From.Assign(source.From);
  Join.Assign(source.Join);
  GroupBy.Assign(source.GroupBy);
  Having.Assign(source.Having);
  Plan := source.Plan;
  OrderBy.Assign(source.OrderBy);
  Custom.Assign(source.Custom);
  FBXClientSQLParams := Source.FBXClientSQLParams;
end;

{**********************************************}
function TAlSelectSQLClause.SQLText: AnsiString;
Var Flag: Boolean;
    i: integer;
    S: ansiString;
Begin

  //start
  Result := 'Select ';

  //first + skip (if server type = ALFirebird)
  if ServerType = alFirebird then begin
    if First >= 0 then Result := result + 'first ' + ALIntToStr(First) + ' ';
    if skip >= 0 then Result := result + 'skip ' + ALIntToStr(skip) + ' ';
  end;

  //distinct
  If Distinct then Result := result + 'distinct ';

  //Select
  Flag := False;
  For i := 0 to Select.Count - 1 do begin
    If ALTrim(Select[i]) <> '' then begin
      Flag := True;
      Result := Result + ALTrim(Select[i]) + ', ';
    end;
  end;
  IF not flag then result := result + '*'
  else Delete(Result,length(result)-1,2);

  //From
  Result := Result + ' From ';
  For i := 0 to From.Count - 1 do
    If ALTrim(From[i]) <> '' then Result := Result + ALTrim(From[i]) + ', ';
  Delete(Result,length(result)-1,2);

  //join
  For i := 0 to join.Count - 1 do
    If ALTrim(join[i]) <> '' then Result := Result + ' ' + ALTrim(join[i]);

  //Where
  If where.Count > 0 then begin
    S := '';
    For i := 0 to where.Count - 1 do
      If ALTrim(where[i]) <> '' then begin
        if ServerType <> AlSphinx then S := S + '(' + ALTrim(where[i]) + ') and '
        else S := S + ALTrim(where[i]) + ' and ';
      end;
    If s <> '' then begin
      delete(S,length(S)-4,5);
      Result := Result + ' Where ' + S;
    end;
  end;

  //group by
  If groupby.Count > 0 then begin
    S := '';
    For i := 0 to groupby.Count - 1 do
      If ALTrim(groupby[i]) <> '' then S := S + ALTrim(groupby[i]) + ' and ';
    If s <> '' then begin
      Delete(S,length(S)-4,5);
      Result := Result + ' Group by ' + S;
    end;
  end;

  //Having
  If having.Count > 0 then begin
    S := '';
    For i := 0 to having.Count - 1 do
      If ALTrim(having[i]) <> '' then S := S + ALTrim(having[i]) + ' and ';
    If s <> '' then begin
      Delete(S,length(S)-4,5);
      Result := Result + ' Having ' + S;
    end;
  end;

  //Plan
  if (ServerType = alFirebird) then begin
    If ALTrim(Plan) <> '' then
      Result := Result + ' ' + ALTrim(Plan);
  end;

  //order by
  If orderby.Count > 0 then begin
    S := '';
    For i := 0 to orderby.Count - 1 do
      If ALTrim(orderby[i]) <> '' then  S := S + ALTrim(orderby[i]) + ', ';
    If s <> '' then begin
      Delete(S,length(S)-1,2);
      Result := Result + ' Order by ' + S;
    end;
  end;

  //first + skip (if server type = ALSphinx)
  if (ServerType in [alSphinx, alMySql]) then begin
    if (First >= 0) and (skip >= 0) then Result := result + ' Limit ' + ALIntToStr(skip) + ', ' + ALIntToStr(First) // With two arguments, the first argument specifies the offset of the first row to return, and the second specifies the maximum number of rows to return
    else if (skip >= 0) then             Result := result + ' Limit ' + ALIntToStr(skip) + ', ' + ALIntToStr(Maxint) // To retrieve all rows from a certain offset up to the end of the result set, you can use some large number for the second parameter.
    else if (First >= 0) then            Result := result + ' Limit 0, '                        + ALIntToStr(First)
  end;

End;

{*****************************************************************************************************************}
function TAlSelectSQLClause.FBXClientSelectDataSQL(const ViewTag, RowTag: AnsiString): TALFBXClientSelectDataQUERY;
begin
  result.SQL := SQLText;
  result.Params := FBXClientSQLParams;
  result.RowTag := RowTag;
  result.ViewTag := ViewTag;
  result.Skip := -1; // because it's added in the SQL
  result.First := -1; // because it's added in the SQL
  result.CacheThreshold := 0;
end;

{********************************************************************************************************}
function TAlSelectSQLClause.FBXClientSelectDataSQL(const RowTag: AnsiString): TALFBXClientSelectDataQUERY;
begin
  result := FBXClientSelectDataSQL('', RowTag);
end;

{******************************************************************************}
function TAlSelectSQLClause.FBXClientSelectDataSQL: TALFBXClientSelectDataQUERY;
begin
  result := FBXClientSelectDataSQL('', '');
end;

{*********************************}
procedure TAlSelectSQLClause.Clear;
Begin
  ServerType:= alFirebird;
  First:= -1;
  Skip:= -1;
  Distinct:= False;
  Select.clear;
  Where.clear;
  From.clear;
  Join.clear;
  GroupBy.clear;
  Having.clear;
  Plan := '';
  OrderBy.clear;
  Custom.clear;
  setlength(FBXClientSQLParams,0);
End;

{******************************************************************************}
function TAlSelectSQLClauses.Add(aSelectSQLClause: TAlSelectSQLClause): Integer;
begin
  Result := inherited Add(aSelectSQLClause);
end;

{*********************************************************************************}
function TAlSelectSQLClauses.Extract(Item: TAlSelectSQLClause): TAlSelectSQLClause;
begin
  Result := TAlSelectSQLClause(inherited Extract(Item));
end;

{*****************************************************}
function TAlSelectSQLClauses.First: TAlSelectSQLClause;
begin
  Result := TAlSelectSQLClause(inherited First);
end;

{************************************************************************}
function TAlSelectSQLClauses.GetItems(Index: Integer): TAlSelectSQLClause;
begin
  Result := TAlSelectSQLClause(inherited Items[Index]);
end;

{**********************************************************************************}
function TAlSelectSQLClauses.IndexOf(aSelectSQLClause: TAlSelectSQLClause): Integer;
begin
  Result := inherited IndexOf(aSelectSQLClause);
end;

{*****************************************************************************************}
procedure TAlSelectSQLClauses.Insert(Index: Integer; aSelectSQLClause: TAlSelectSQLClause);
begin
  inherited Insert(Index, aSelectSQLClause);
end;

{****************************************************}
function TAlSelectSQLClauses.Last: TAlSelectSQLClause;
begin
  Result := TAlSelectSQLClause(inherited Last);
end;

{*********************************************************************************}
function TAlSelectSQLClauses.Remove(aSelectSQLClause: TAlSelectSQLClause): Integer;
begin
  Result := inherited Remove(aSelectSQLClause);
end;

{*******************************************************************************************}
procedure TAlSelectSQLClauses.SetItems(Index: Integer; aSelectSQLClause: TAlSelectSQLClause);
begin
  inherited Items[Index] := aSelectSQLClause;
end;

{************************************************************************************************************************}
function TAlSelectSQLClauses.FBXClientSelectDataQUERIES(const ViewTag, RowTag: AnsiString): TALFBXClientSelectDataQUERIES;
var i: integer;
begin
  setlength(Result, count);
  for I := 0 to count-1 do
    result[i] := GetItems(i).FBXClientSelectDataSQL(ViewTag, RowTag);
end;

{***************************************************************************************************************}
function TAlSelectSQLClauses.FBXClientSelectDataQUERIES(const RowTag: AnsiString): TALFBXClientSelectDataQUERIES;
begin
  result := FBXClientSelectDataQUERIES('', RowTag);
end;

{*************************************************************************************}
function TAlSelectSQLClauses.FBXClientSelectDataQUERIES: TALFBXClientSelectDataQUERIES;
begin
  result := FBXClientSelectDataQUERIES('', '');
end;

{************************************}
constructor TALUpdateSQLClause.Create;
Begin
  ServerType := ALFirebird;
  Kind := AlInsert;
  table:= '';
  Value:= TALStringList.create;
  Where:= TALStringList.create;
  Custom := TALStringList.create;
  setlength(FBXClientSQLParams, 0);
end;

{************************************}
destructor TALUpdateSQLClause.Destroy;
begin
  Value.free;
  Where.free;
  Custom.free;
  inherited;
end;

{**************************************************************}
procedure TALUpdateSQLClause.Assign(Source: TALUpdateSQLClause);
begin
  ServerType := Source.ServerType;
  Kind := Source.Kind;
  table:= source.Table;
  Value.Assign(source.Value);
  Where.Assign(source.Where);
  Custom.Assign(source.Custom);
  FBXClientSQLParams := Source.FBXClientSQLParams;
end;

{**********************************************}
function TALUpdateSQLClause.SQLText: AnsiString;
var i: integer;
    S1, S2: ansiString;
Begin

  //empty result if Value.Count = 0
  if (Kind <> AlDelete) and
     (Value.Count = 0) then begin
    result := '';
    exit;
  end;

  //AlUpdate
  If Kind = AlUpdate then Begin
    Result := '';
    for i := 0 to Value.Count- 1 do
      If ALTrim(Value[i]) <> '' then Result := Result + ALTrim(Value[i]) + ', ';
    delete(Result,length(result)-1,2);

    Result := 'Update ' + Table + ' Set ' + result;

    If where.Count > 0 then begin
      Result := Result + ' Where ';
      For i := 0 to where.Count - 1 do
        If ALTrim(where[i]) <> '' then Result := Result + '(' + ALTrim(where[i]) + ') and ';
      delete(Result,length(result)-4,5);
    end;
  end

  //AlDelete
  else If Kind = AlDelete then Begin
    Result := 'delete from ' + Table;

    If where.Count > 0 then begin
      Result := Result + ' Where ';
      For i := 0 to where.Count - 1 do
        If ALTrim(where[i]) <> '' then Result := Result + '(' + ALTrim(where[i]) + ') and ';
      delete(Result,length(result)-4,5);
    end;
  end

  //AlInsert, AlUpdateOrInsert
  else Begin
    S1 := '';
    S2 :='';

    for i := 0 to Value.Count-1 do
      If ALTrim(Value[i]) <> '' then begin
        S1 := S1 + ALTrim(Value.Names[i]) + ', ';
        S2 := S2 + ALTrim(Value.ValueFromIndex[i]) + ', ';
      end;
    delete(S1,length(S1)-1,2);
    delete(S2,length(S2)-1,2);

    if kind = ALInsert then Result := 'Insert into '
    else Result := 'Update or Insert into ';
    Result := Result + Table + ' (' + S1 + ') Values (' + s2 + ')';
  end;

end;

{*****************************************************************************************************************}
function TAlUpdateSQLClause.FBXClientUpdateDataSQL(const ViewTag, RowTag: AnsiString): TALFBXClientUpdateDataQUERY;
begin
  result.SQL := SQLText;
  result.Params := FBXClientSQLParams;
end;

{********************************************************************************************************}
function TAlUpdateSQLClause.FBXClientUpdateDataSQL(const RowTag: AnsiString): TALFBXClientUpdateDataQUERY;
begin
  result := FBXClientUpdateDataSQL('', RowTag);
end;

{******************************************************************************}
function TAlUpdateSQLClause.FBXClientUpdateDataSQL: TALFBXClientUpdateDataQUERY;
begin
  result := FBXClientUpdateDataSQL('', '');
end;

{*********************************}
procedure TALUpdateSQLClause.clear;
begin
  ServerType:= ALFirebird;
  Kind:= AlInsert;
  Table:= '';
  Value.clear;
  Where.clear;
  Custom.clear;
  setlength(FBXClientSQLParams, 0);
end;

{******************************************************************************}
function TAlUpdateSQLClauses.Add(aUpdateSQLClause: TAlUpdateSQLClause): Integer;
begin
  Result := inherited Add(aUpdateSQLClause);
end;

{*********************************************************************************}
function TAlUpdateSQLClauses.Extract(Item: TAlUpdateSQLClause): TAlUpdateSQLClause;
begin
  Result := TAlUpdateSQLClause(inherited Extract(Item));
end;

{*****************************************************}
function TAlUpdateSQLClauses.First: TAlUpdateSQLClause;
begin
  Result := TAlUpdateSQLClause(inherited First);
end;

{************************************************************************}
function TAlUpdateSQLClauses.GetItems(Index: Integer): TAlUpdateSQLClause;
begin
  Result := TAlUpdateSQLClause(inherited Items[Index]);
end;

{**********************************************************************************}
function TAlUpdateSQLClauses.IndexOf(aUpdateSQLClause: TAlUpdateSQLClause): Integer;
begin
  Result := inherited IndexOf(aUpdateSQLClause);
end;

{*****************************************************************************************}
procedure TAlUpdateSQLClauses.Insert(Index: Integer; aUpdateSQLClause: TAlUpdateSQLClause);
begin
  inherited Insert(Index, aUpdateSQLClause);
end;

{****************************************************}
function TAlUpdateSQLClauses.Last: TAlUpdateSQLClause;
begin
  Result := TAlUpdateSQLClause(inherited Last);
end;

{*********************************************************************************}
function TAlUpdateSQLClauses.Remove(aUpdateSQLClause: TAlUpdateSQLClause): Integer;
begin
  Result := inherited Remove(aUpdateSQLClause);
end;

{*******************************************************************************************}
procedure TAlUpdateSQLClauses.SetItems(Index: Integer; aUpdateSQLClause: TAlUpdateSQLClause);
begin
  inherited Items[Index] := aUpdateSQLClause;
end;

{************************************************************************************************************************}
function TAlUpdateSQLClauses.FBXClientUpdateDataQUERIES(const ViewTag, RowTag: AnsiString): TALFBXClientUpdateDataQUERIES;
var i: integer;
begin
  setlength(Result, count);
  for I := 0 to count-1 do
    result[i] := GetItems(i).FBXClientUpdateDataSQL(ViewTag, RowTag);
end;

{***************************************************************************************************************}
function TAlUpdateSQLClauses.FBXClientUpdateDataQUERIES(const RowTag: AnsiString): TALFBXClientUpdateDataQUERIES;
begin
  result := FBXClientUpdateDataQUERIES('', RowTag);
end;

{*************************************************************************************}
function TAlUpdateSQLClauses.FBXClientUpdateDataQUERIES: TALFBXClientUpdateDataQUERIES;
begin
  result := FBXClientUpdateDataQUERIES('', '');
end;

end.
