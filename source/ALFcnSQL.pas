{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      SQL Parser Functions
Version:      4.00

Description:  SQL function to create easily sql string without
              take care if it's an update or insert sql Statement.
              just add the value in a tstringList like
              fieldname=value and at the end contruct the sql string

Legal issues: Copyright (C) 1999-2012 by Arkadia Software Engineering

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
              26/06/2012: Add xe2 support

Link :

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://www.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALFcnSQL;

interface

uses AlStringList;

Type

  {--------------------------------------------------------------}
  TALSQLClauseUpdateKind = (alUpdate, AlInsert, AlUpdateOrInsert);
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
    Plan: ansiString;
    OrderBy: TALStrings;
    Custom: TALStrings;
    Constructor Create; Virtual;
    Destructor Destroy; override;
    procedure Assign(Source: TAlSelectSQLClause); virtual;
    function SQLText: AnsiString; virtual;
    Procedure clear; virtual;
  end;
  TALUpdateSQLClause = Class(Tobject)
    ServerType: TALSQLClauseServerType;
    Kind: TALSQLClauseUpdateKind;
    Table: ansiString;
    Value: TALStrings;
    Where: TALStrings;
    Custom: TALStrings;
    Constructor Create; Virtual;
    Destructor Destroy; override;
    procedure Assign(Source: TALUpdateSQLClause); virtual;
    function SQLText: AnsiString; virtual;
    Procedure clear; virtual;
  end;

implementation

uses AlFcnString;

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
    if (First >= 0) and
       (skip >= 0) then Result := result + ' Limit ' + ALIntToStr(skip) + ', ' + ALIntToStr(First)
    else if (First >= 0) then Result := result + ' Limit 0, ' + ALIntToStr(First)
  end;

End;

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
End;

{************************************}
constructor TALUpdateSQLClause.Create;
Begin
  ServerType := ALFirebird;
  Kind := AlInsert;
  table:= '';
  Value:= TALStringList.create;
  Where:= TALStringList.create;
  Custom := TALStringList.create;
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
end;

{**********************************************}
function TALUpdateSQLClause.SQLText: AnsiString;
var i: integer;
    S1, S2: ansiString;
Begin

  //empty result if Value.Count = 0
  if Value.Count = 0 then begin
    result := '';
    exit;
  end;

  //Update
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

  //Insert
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

{*********************************}
procedure TALUpdateSQLClause.clear;
begin
  ServerType:= ALFirebird;
  Kind:= AlInsert;
  Table:= '';
  Value.clear;
  Where.clear;
  Custom.clear;
end;


end.
