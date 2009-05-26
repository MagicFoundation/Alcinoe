{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      SQL Parser Functions
Version:      3.50

Description:  SQL function to create easily sql string without
              take care if it's an update or insert sql Statement.
              just add the value in a tstringList like
              fieldname=value and at the end contruct the sql string

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

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

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALFcnSQL;

interface

uses classes;

Type

  {----------------------------}
  TAlStrSelectSQLClause = record
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
    Update: Boolean;
    Table: string;
    Value: string;
    Where: string;
  end;
  TAlLstSelectSQLClause = record
    Distinct : Boolean;
    Select: TStrings;
    Where: Tstrings;
    From: Tstrings;
    Join: Tstrings;
    GroupBy: Tstrings;
    Having: Tstrings;
    Plan: String;
    OrderBy: TStrings;
  end;
  TALLstUpdateSQLClause = record
    Update: Boolean;
    Table: String;
    Value: Tstrings;
    Where: Tstrings;
  end;

{---------------------------------------------------------}
Function  AlEmptyStrUpdateSqlClause: TAlStrUpdateSQLClause;
Function  AlEmptyLstUpdateSqlClause: TAlLstUpdateSQLClause;
Function  AlEmptyStrSelectSqlClause: TAlStrSelectSQLClause;
Function  AlEmptyLstSelectSqlClause: TAlLstSelectSQLClause;
Procedure ALFreeLstUpdateSqlClause(Var aLstUpdateSQLClause: TAlLstUpdateSQLClause);
Procedure ALFreeLstSelectSqlClause(Var aLstSelectSQLClause: TAlLstSelectSQLClause);
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
    Update := False;
    table:= '';
    Value:= '';
    Where:= '';
  end;
end;

{********************************************************}
Function ALEmptyLstSelectSqlClause: TAlLstSelectSQLClause;

  {------------------------}
  Function alg001: Tstrings;
  Begin
    Result := TstringList.create;
    with result as TstringList do begin
      Sorted := True;
      Duplicates := dupIgnore;
      Delimiter := ';';
      QuoteChar := '''';
    end;
  end;

Begin
  with result do begin
    Distinct := False;
    Select:= alg001;
    Where:= alg001;
    From:= alg001;
    Join:= alg001;
    GroupBy:= alg001;
    Having:= alg001;
    Plan := '';
    OrderBy:= alg001;
  end;
end;

{********************************************************}
Function ALEmptyLstUpdateSqlClause: TAlLstUpdateSQLClause;

  {------------------------}
  Function alg001: Tstrings;
  Begin
    Result := TstringList.create;
    with result as TstringList do begin
      Sorted := True;
      Duplicates := dupIgnore;
      Delimiter := ';';
      QuoteChar := '''';
    end;
  end;

Begin
  with result do begin
    Update := False;
    table:= '';
    Value:= alg001;
    Where:= alg001;
  end;
end;

{*********************************************************************************}
Procedure ALFreeLstSelectSqlClause(Var aLstSelectSQLClause: TAlLstSelectSQLClause);
Begin
  with aLstSelectSQLClause do begin
    Select.free;
    Where.free;
    From.free;
    Join.free;
    GroupBy.free;
    Having.free;
    OrderBy.free;
  end;
end;

{*********************************************************************************}
Procedure ALFreeLstUpdateSqlClause(Var aLstUpdateSQLClause: TAlLstUpdateSQLClause);
Begin
  with aLstUpdateSQLClause do begin
    Value.free;
    Where.free;
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
  If SqlClause.Distinct then Result := result + 'distinct ';

  Lst := TstringList.Create;
  Try
    Lst.Sorted := True;
    Lst.Duplicates := dupIgnore;
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
    Lst.Sorted := True;
    Lst.Duplicates := dupIgnore;
    Lst.Delimiter := ';';
    Lst.QuoteChar := '''';
    Lst.DelimitedText := trim(SqlClause.Value);

    {----Update------------------}
    If SqlClause.Update then Begin
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

      Result := 'Insert into ' + SqlClause.Table + ' (' + S1 + ') Values (' + s2 + ')';
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
  {----Update------------------}
  If SqlClause.Update then Begin
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

    Result := 'Insert into ' + SqlClause.Table + ' (' + S1 + ') Values (' + s2 + ')';
  end;
end;

end.
