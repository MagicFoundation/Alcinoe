{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      SQL Parser Functions
Version:      3.50

Description:  SQL function to create easily sql string without
              take care if it's an update or insert sql Statement.
              just add the value in a tstringList like
              fieldname=value and at the end contruct the sql string

Legal issues: Copyright (C) 1999-2010 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :     20/04/2006: Add plan property

Link :

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  voting on http://www.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALFcnSQL;

interface

uses classes;

Type

  {--------------------------------------------------------------}
  TALSQLClauseUpdateKind = (alUpdate, AlInsert, AlUpdateOrInsert);

  {----------------------------}
  TAlStrSelectSQLClause = record
    First: integer;
    Skip: Integer;
    Distinct : Boolean;
    Select: String;
    Where: string;
    From: string;
    Join: string;
    GroupBy: string;
    Having: string;
    Plan: String;
    OrderBy: String;
  end;
  TALStrUpdateSQLClause = record
    Kind: TALSQLClauseUpdateKind;
    Table: string;
    Value: string;
    Where: string;
  end;
  TAlLstSelectSQLClause = Class(Tobject)
  public
    First: integer;
    Skip: Integer;
    Distinct: Boolean;
    Select: TStrings;
    Where: Tstrings;
    From: Tstrings;
    Join: Tstrings;
    GroupBy: Tstrings;
    Having: Tstrings;
    Plan: String;
    OrderBy: TStrings;
    Constructor Create; Virtual;
    Destructor Destroy; override;
    procedure Assign(Source: TAlLstSelectSQLClause); virtual;
  end;
  TALLstUpdateSQLClause = Class(Tobject)
    Kind: TALSQLClauseUpdateKind;
    Table: String;
    Value: Tstrings;
    Where: Tstrings;
    Constructor Create; Virtual;
    Destructor Destroy; override;
    procedure Assign(Source: TALLstUpdateSQLClause); virtual;
  end;

{---------------------------------------------------------}
Function  AlEmptyStrUpdateSqlClause: TAlStrUpdateSQLClause;
Function  AlEmptyStrSelectSqlClause: TAlStrSelectSQLClause;
Function  AlSQLFromStrSelectSqlClause(SqlClause: TAlStrSelectSQLClause) :String;
Function  AlSQLFromLstSelectSqlClause(SqlClause: TAlLstSelectSQLClause) :String;
Function  AlSQLFromStrUpdateSqlClause(SqlClause: TAlStrUpdateSQLClause): String;
Function  AlSQLFromLstUpdateSqlClause(SqlClause: TAlLstUpdateSQLClause): String;

implementation

uses sysutils;

{********************************************************}
Function ALEmptyStrSelectSqlClause: TAlStrSelectSQLClause;
Begin
  with result do begin
    Distinct := False;
    First := -1;
    Skip := -1;
    Select:= '';
    Where:= '';
    From:= '';
    Join:= '';
    GroupBy:= '';
    Having:= '';
    Plan := '';
    OrderBy:= '';
  end;
end;

{********************************************************}
Function ALEmptyStrUpdateSqlClause: TAlStrUpdateSQLClause;
Begin
  with result do begin
    Kind := AlInsert;
    table:= '';
    Value:= '';
    Where:= '';
  end;
end;

{*****************************************************************************}
Function ALSQLFromStrSelectSqlClause(SqlClause: TAlStrSelectSQLClause) :String;
Var Lst : TstringList;
    i : integer;
    Flag : Boolean;
    S : String;
Begin

  Result := 'Select ';
  if SqlClause.First >= 0 then Result := result + 'first ' + inttostr(SqlClause.First) + ' ';
  if SqlClause.skip >= 0 then Result := result + 'skip ' + inttostr(SqlClause.skip) + ' ';  
  If SqlClause.Distinct then Result := result + 'distinct ';

  Lst := TstringList.Create;
  Try
    Lst.Delimiter := ';';
    Lst.QuoteChar := '''';

    {------------}
    Flag := False;
    Lst.DelimitedText := trim(SqlClause.Select);
    For i := 0 to lst.Count - 1 do
      If trim(lst[i]) <> '' then begin
        Flag := True;
        Result := Result + trim(Lst[i]) + ', ';
    end;
    IF not flag then result := result + '*'
    else Delete(Result,length(result)-1,2);
    {---------------------------}
    Result := Result + ' From ';
    Lst.DelimitedText := trim(SqlClause.From);
    For i := 0 to lst.Count - 1 do
      If trim(lst[i]) <> '' then Result := Result + trim(Lst[i]) + ', ';
    Delete(Result,length(result)-1,2);
    {----------------------------------------}
    Lst.DelimitedText := trim(SqlClause.join);
    For i := 0 to lst.Count - 1 do
      If trim(lst[i]) <> '' then Result := Result + ' ' + trim(Lst[i]);
    {---------------------------------------}
    If trim(SqlClause.where) <> '' then begin
      Lst.DelimitedText := trim(SqlClause.where);
      S := '';
      If Lst.Count > 0 then begin
        For i := 0 to lst.Count - 1 do
          If trim(lst[i]) <> '' then S := S + '(' + trim(Lst[i]) + ') and ';
        If s <> '' then begin
          delete(S,length(S)-4,5);
          Result := Result + ' Where ' + S;
        end;
      end;
    end;
    {-----------------------------------------}
    If trim(SqlClause.groupby) <> '' then begin
      Lst.DelimitedText := trim(SqlClause.groupby);
      S := '';
      If Lst.Count > 0 then begin
        For i := 0 to lst.Count - 1 do
          If trim(lst[i]) <> '' then S := S + trim(Lst[i]) + ' and ';
        If s <> '' then begin
          Delete(S,length(S)-4,5);
          Result := Result + ' group by ' + S;
        end;
      end;
    end;
    {----------------------------------------}
    If trim(SqlClause.having) <> '' then begin
      Lst.DelimitedText := trim(SqlClause.having);
      If Lst.Count > 0 then begin
        S := '';
        For i := 0 to lst.Count - 1 do
          If trim(lst[i]) <> '' then S := S + trim(Lst[i]) + ' and ';
        If s <> '' then begin
          Delete(S,length(S)-4,5);
          Result := Result + ' Having ' + S;
        end;
      end;
    end;
    {--------------------------------}
    If trim(SqlClause.Plan) <> '' then
      Result := Result + ' ' + trim(SqlClause.Plan);
    {-----------------------------------------}
    If trim(SqlClause.orderby) <> '' then begin
      Lst.DelimitedText := trim(SqlClause.orderby);
      If Lst.Count > 0 then begin
        S := '';
        For i := 0 to lst.Count - 1 do
          If trim(lst[i]) <> '' then  S := S + trim(Lst[i]) + ', ';
        If s <> '' then begin
          Delete(S,length(S)-1,2);
          Result := Result + ' order by ' + S;
        end;
      end;
    end;

  finally
    Lst.Free;
  end;

end;

{*****************************************************************************}
Function ALSQLFromStrUpdateSqlClause(SqlClause: TAlStrUpdateSQLClause): String;
Var Lst : TstringList;
    i : integer;
    S1, S2 : String;
Begin

  Lst := TstringList.Create;
  Try
    Lst.Delimiter := ';';
    Lst.QuoteChar := '''';
    Lst.DelimitedText := trim(SqlClause.Value);

    {----Update---------------------------}
    If SqlClause.Kind = AlUpdate then Begin
      Result := '';
      for i := 0 to Lst.Count- 1 do
        If Trim(Lst[i]) <> '' then Result := Result + trim(Lst[i]) + ', ';
      delete(Result,length(result)-1,2);

      if result = '' then exit;
      Result := 'Update ' + SqlClause.Table + ' Set ' + result;

      If trim(SqlClause.Where) <> '' then begin
        Lst.DelimitedText := trim(SqlClause.where);
        If Lst.Count > 0 then begin
          Result := Result + ' Where ';
          For i := 0 to lst.Count - 1 do
            If trim(lst[i]) <> '' then Result := Result + '(' + trim(Lst[i]) + ') and ';
          delete(Result,length(result)-4,5);
        end;
      end;
    end

    {-Insert-}
    else Begin
      S1 := '';
      S2 :='';

      for i := 0 to Lst.Count-1 do
        If Trim(Lst[i]) <> '' then begin
          S1 := S1 + trim(Lst.Names[i]) + ', ';
          S2 := S2 + trim(Lst.ValueFromIndex[i]) + ', ';
        end;
      delete(S1,length(S1)-1,2);
      delete(S2,length(S2)-1,2);

      If (S1='') or (S2='') then begin
        result := '';
        exit;
      end;

      if SqlClause.kind = ALInsert then Result := 'Insert into '
      else Result := 'Update or Insert into ';
      Result := result + SqlClause.Table + ' (' + S1 + ') Values (' + s2 + ')';
    end;

  finally
    Lst.Free;
  end;

end;

{*****************************************************************************}
Function ALSQLFromLstSelectSqlClause(SqlClause: TAlLstSelectSQLClause) :String;
Var i : integer;
    Flag : Boolean;
    S : String;
Begin

  Result := 'Select ';
  if SqlClause.First >= 0 then Result := result + 'first ' + inttostr(SqlClause.First) + ' ';
  if SqlClause.skip >= 0 then Result := result + 'skip ' + inttostr(SqlClause.skip) + ' ';
  If SqlClause.Distinct then Result := result + 'distinct ';

  {------------}
  Flag := False;
  For i := 0 to SqlClause.Select.Count - 1 do
    If trim(SqlClause.Select[i]) <> '' then begin
      Flag := True;
      Result := Result + trim(SqlClause.Select[i]) + ', ';
  end;
  IF not flag then result := result + '*'
  else Delete(Result,length(result)-1,2);
  {--------------------------}
  Result := Result + ' From ';
  For i := 0 to SqlClause.From.Count - 1 do
    If trim(SqlClause.From[i]) <> '' then Result := Result + trim(SqlClause.From[i]) + ', ';
  Delete(Result,length(result)-1,2);
  {---------------------------------------}
  For i := 0 to SqlClause.join.Count - 1 do
    If trim(SqlClause.join[i]) <> '' then Result := Result + ' ' + trim(SqlClause.join[i]);
  {--------------------------------------}
  If SqlClause.where.Count > 0 then begin
    S := '';
    For i := 0 to SqlClause.where.Count - 1 do
      If trim(SqlClause.where[i]) <> '' then S := S + '(' + trim(SqlClause.where[i]) + ') and ';
    If s <> '' then begin
      delete(S,length(S)-4,5);
      Result := Result + ' Where ' + S;
    end;
  end;
  {-----------------------------------------}
  If SqlClause.groupby.Count > 0 then begin
    S := '';
    For i := 0 to SqlClause.groupby.Count - 1 do
      If trim(SqlClause.groupby[i]) <> '' then S := S + trim(SqlClause.groupby[i]) + ' and ';
    If s <> '' then begin
      Delete(S,length(S)-4,5);
      Result := Result + ' group by ' + S;
    end;
  end;
  {----------------------------------------}
  If SqlClause.having.Count > 0 then begin
    S := '';
    For i := 0 to SqlClause.having.Count - 1 do
      If trim(SqlClause.having[i]) <> '' then S := S + trim(SqlClause.having[i]) + ' and ';
    If s <> '' then begin
      Delete(S,length(S)-4,5);
      Result := Result + ' Having ' + S;
    end;
  end;
  {--------------------------------}
  If trim(SqlClause.Plan) <> '' then
    Result := Result + ' ' + trim(SqlClause.Plan);
  {-----------------------------------------}
  If SqlClause.orderby.Count > 0 then begin
    S := '';
    For i := 0 to SqlClause.orderby.Count - 1 do
      If trim(SqlClause.orderby[i]) <> '' then  S := S + trim(SqlClause.orderby[i]) + ', ';
    If s <> '' then begin
      Delete(S,length(S)-1,2);
      Result := Result + ' order by ' + S;
    end;
  end;

end;

{******************************************************************************}
Function ALSQLFromLstUpdateSqlClause(SqlClause: TAlLstUpdateSQLClause): String;
var i : integer;
    S1, S2 : String;
Begin

  {----Update---------------------------}
  If SqlClause.Kind = AlUpdate then Begin
    Result := '';
    for i := 0 to SqlClause.Value.Count- 1 do
      If Trim(SqlClause.Value[i]) <> '' then Result := Result + trim(SqlClause.Value[i]) + ', ';
    delete(Result,length(result)-1,2);

    if result = '' then exit;
    Result := 'Update ' + SqlClause.Table + ' Set ' + result;

    If SqlClause.where.Count > 0 then begin
      Result := Result + ' Where ';
      For i := 0 to SqlClause.where.Count - 1 do
        If trim(SqlClause.where[i]) <> '' then Result := Result + '(' + trim(SqlClause.where[i]) + ') and ';
      delete(Result,length(result)-4,5);
    end;
  end

  {-Insert-}
  else Begin
    S1 := '';
    S2 :='';

    for i := 0 to SqlClause.Value.Count-1 do
      If Trim(SqlClause.Value[i]) <> '' then begin
        S1 := S1 + trim(SqlClause.Value.Names[i]) + ', ';
        S2 := S2 + trim(SqlClause.Value.ValueFromIndex[i]) + ', ';
      end;
    delete(S1,length(S1)-1,2);
    delete(S2,length(S2)-1,2);

    If (S1='') or (S2='') then begin
      result := '';
      exit;
    end;

    if SqlClause.kind = ALInsert then Result := 'Insert into '
    else Result := 'Update or Insert into ';
    Result := Result + SqlClause.Table + ' (' + S1 + ') Values (' + s2 + ')';
  end;

end;


/////////////////////////////////
///// TAlLstSelectSQLClause /////
/////////////////////////////////

{***************************************}
constructor TAlLstSelectSQLClause.Create;

  {-----------------------------------}
  Function InternalCreateLst: Tstrings;
  Begin
    Result := TstringList.create;
    with result as TstringList do begin
      Delimiter := ';';
      QuoteChar := '''';
    end;
  end;

Begin
  First := -1;
  Skip := -1;
  Distinct := False;
  Select:= InternalCreateLst;
  Where:= InternalCreateLst;
  From:= InternalCreateLst;
  Join:= InternalCreateLst;
  GroupBy:= InternalCreateLst;
  Having:= InternalCreateLst;
  Plan := '';
  OrderBy:= InternalCreateLst;
end;

{***************************************}
destructor TAlLstSelectSQLClause.Destroy;
begin
  Select.free;
  Where.free;
  From.free;
  Join.free;
  GroupBy.free;
  Having.free;
  OrderBy.free;
  inherited;
end;

{********************************************************************}
procedure TAlLstSelectSQLClause.Assign(Source: TAlLstSelectSQLClause);
begin
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
end;



/////////////////////////////////
///// TALLstUpdateSQLClause /////
/////////////////////////////////

{***************************************}
constructor TALLstUpdateSQLClause.Create;

  {-----------------------------------}
  Function InternalCreateLst: Tstrings;
  Begin
    Result := TstringList.create;
    with result as TstringList do begin
      Delimiter := ';';
      QuoteChar := '''';
    end;
  end;

Begin
  Kind := AlInsert;
  table:= '';
  Value:= InternalCreateLst;
  Where:= InternalCreateLst;
end;

{***************************************}
destructor TALLstUpdateSQLClause.Destroy;
begin
  Value.free;
  Where.free;
  inherited;
end;

{********************************************************************}
procedure TALLstUpdateSQLClause.Assign(Source: TALLstUpdateSQLClause);
begin
  Kind := Source.Kind;
  table:= source.Table;
  Value.Assign(source.Value);
  Where.Assign(source.Where);
end;

end.
