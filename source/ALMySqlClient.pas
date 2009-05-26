{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALMySqlClient
Version:      3.53

Description:  An Object to query MySql Server Version 5 and get
              the Result In Xml Stream

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

Know bug :    30/01/2008: Correct memory leak bug.

History :

Link :        http://dev.mysql.com/doc/refman/5.0/en/
              http://dev.mysql.com/doc/refman/5.0/en/string-syntax.html

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit AlMySqlClient;

interface

uses SysUtils,
     AlXmlDoc,
     AlMySqlWrapper;

Type

  {-----------------------------}
  TalMySqlClient = Class(Tobject)
  Private
    fLibrary: TALMySqlLibrary;
    fMySQL: PMYSQL;
    fconnected: Boolean;
    fdefaultFormatSettings: TformatSettings;
    fNullString: String;
    procedure Fail(aMsg: string = '');
    Function  FormatFieldValue(aMySqlFormatedFieldValue: Pchar; aFieldType: cardinal; aformatSettings: TformatSettings): String;
  Protected
  Public
    Constructor Create(ApiVer: TALMySqlVersion_API; const lib: string = 'libmysql.dll'); virtual;
    Destructor Destroy; Override;
    Procedure Connect(HostName, UserName, Password, DatabaseName: String; Port: Integer; CharacterSet: String; Const ClientFlag: Cardinal = 0);
    Procedure Disconnect;
    Procedure SelectData(Query: string; XMLDATA: TalXMLNode; FormatSettings: TformatSettings; RowTag: String; Const Skip: Integer = -1; Const First: Integer = -1); overload;
    Procedure SelectData(Query: string; XMLDATA: TalXMLNode; RowTag: String; Const Skip: Integer = -1; Const First: Integer = -1); overload;
    procedure UpdateData(Query: string);
    function  insert_id(Query: string): Int64;
    Property Connected: Boolean Read Fconnected;
    Property NullString: String Read fNullString Write fNullString;
  end;

Function AlMySqlClientSlashedStr(Const Str: String): String;

implementation

Uses AlFcnString;

{**********************************************************}
Function AlMySqlClientSlashedStr(Const Str: String): String;
var I: Integer;
begin
  Result := Str;
  for I := Length(Result) downto 1 do
    if Result[I] in ['''','"','\',#0] then Insert('\', Result, I);
  Result := '''' + Result + '''';
end;


{*************************************************************************************************}
constructor TalMySqlClient.Create(ApiVer: TALMySqlVersion_API; const lib: string = 'libmysql.dll');
begin
  fconnected := False;
  fMySQL := nil;
  fLibrary := TALMySqlLibrary.Create(ApiVer);
  fLibrary.Load(lib);
  GetLocaleFormatSettings(1033, fdefaultFormatSettings);
  fdefaultFormatSettings.DecimalSeparator := '.';
  fdefaultFormatSettings.DateSeparator := '-';
  fdefaultFormatSettings.TimeSeparator := ':';
  fdefaultFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  fdefaultFormatSettings.ShortTimeFormat := 'hh:nn:ss';
  fNullString := '';
end;

{********************************}
destructor TalMySqlClient.Destroy;
begin
  If Connected then disconnect;
  fLibrary.Free;
  inherited;
end;

{***********************************************}
procedure TalMySqlClient.Fail(aMsg: string = '');
Begin
	if aMsg = '' then aMsg := Format('%d - %s', [fLibrary.mysql_errno(fMySQL), fLibrary.mysql_error(fMySQL)]);
	raise Exception.Create(aMsg);
end;

{***********************************************************************}
function TalMySqlClient.FormatFieldValue(aMySqlFormatedFieldValue: Pchar;
                                         aFieldType: cardinal;
                                         aformatSettings: TformatSettings): String;
begin
  IF aMySqlFormatedFieldValue = nil then result := fNullString
  else begin
    Case aFieldType of
      FIELD_TYPE_DECIMAL,
      FIELD_TYPE_NEWDECIMAL,
      FIELD_TYPE_FLOAT,
      FIELD_TYPE_DOUBLE: result := floattostr(strtofloat(aMySqlFormatedFieldValue,fdefaultFormatSettings),aformatSettings);
      FIELD_TYPE_DATETIME: Result := DateTimeToStr(StrToDateTime(aMySqlFormatedFieldValue,fdefaultFormatSettings),aformatSettings);
      FIELD_TYPE_DATE,
      FIELD_TYPE_NEWDATE: Result := DateToStr(StrToDate(aMySqlFormatedFieldValue,fdefaultFormatSettings),aformatSettings);
      FIELD_TYPE_TIME: Result := TimeToStr(StrToTime(aMySqlFormatedFieldValue,fdefaultFormatSettings),aformatSettings);
      FIELD_TYPE_TIMESTAMP: Result := DateTimeToStr(StrToDateTime(aMySqlFormatedFieldValue,fdefaultFormatSettings),aformatSettings);
      Else result := aMySqlFormatedFieldValue;
    end;
  end;
end;

{****************************************}
procedure TalMySqlClient.Connect(HostName,
                                 UserName,
                                 Password,
                                 DatabaseName: String;
                                 Port: Integer;
                                 CharacterSet: String;
                                 Const ClientFlag: Cardinal = 0);
begin
	fMySQL := fLibrary.mysql_init(nil);
	if fMySQL = nil then Fail('Couldn''t init PMYSQL object');
	try

    Fconnected := True;
    If (CharacterSet <> '') and (fLibrary.mysql_options(fMySQL, MYSQL_SET_CHARSET_NAME, Pchar(CharacterSet)) <> 0) then fail;
		if fLibrary.mysql_real_connect(
                                   fMySQL,
                                   pChar(HostName),
                                   pChar(UserName),
                                   pChar(Password),
                                   Pchar(DatabaseName),
                                   Port,
                                   nil,
                                   ClientFlag
                                  ) = nil then Fail;

  Except
		Disconnect;
    Raise;
	end;
end;

{**********************************}
procedure TalMySqlClient.Disconnect;
begin
  If not Fconnected then Fail('Not connected');
  fLibrary.mysql_close(fMySQL);
  FMySql := Nil;
  Fconnected := False;
end;

{************************************************}
procedure TalMySqlClient.SelectData(Query: string;
                                    XMLDATA: TalXMLNode;
                                    FormatSettings: TformatSettings;
                                    RowTag: String;
                                    Const Skip: Integer = -1;
                                    Const First: Integer = -1);
var aMySqlRes: PMYSQL_RES;
    aMySqlRow: PMYSQL_ROW;
    aMySqlField: PMYSQL_FIELD;
    aFieldCount: cardinal;
    aRecIndex: integer;
    aRecAdded: Integer;
    NewRec: TalXMLNode;
    ValueRec: TalXMLNode;
    i: Integer;
begin
  {Text if we are connected}
  If not Fconnected then Fail('Not connected');

  {exec the query}
	if fLibrary.mysql_query(fMySQL, pChar(Query)) <> 0 then Fail;
	aMySqlRes := fLibrary.mysql_use_result(fMySQL);
	if aMySqlRes = nil then Fail;
  Try
    aFieldCount := fLibrary.mysql_field_count(fMySQL);

    {loop on all the rec}
    aRecIndex := 0;
    aRecAdded := 0;
    aMySqlRow := fLibrary.mysql_fetch_row(aMySqlRes);
    while aMySqlRow <> nil do begin
      inc(aRecIndex);
      If aRecIndex > Skip then begin
        if RowTag <> '' then NewRec := XMLDATA.AddChild(RowTag)
        Else NewRec := XMLDATA;
        for i := 0 to aFieldCount - 1 do begin
          aMySqlField := fLibrary.mysql_fetch_field_direct(aMySqlRes, i);
          ValueRec := NewRec.AddChild(ALlowercase(aMySqlField.name));
          ValueRec.Text := FormatFieldValue(
                                            aMySqlRow[i],
                                            aMySqlField._type,
                                            FormatSettings
                                           );
        end;
        inc(aRecAdded);
        If (First >= 0) and (aRecAdded >= First) then Break;
      end;
      aMySqlRow := fLibrary.mysql_fetch_row(aMySqlRes);
    end
  finally
    fLibrary.mysql_free_result(aMySqlRes);
  end;
end;

{************************************************}
procedure TalMySqlClient.SelectData(Query: string;
                                    XMLDATA: TalXMLNode;
                                    RowTag: String;
                                    Const Skip: Integer = -1;
                                    Const First: Integer = -1);
begin
  SelectData(
             Query,
             XMLDATA,
             fdefaultFormatSettings,
             RowTag,
             Skip,
             First
            );
end;

{*************************************************}
procedure TalMySqlClient.UpdateData(Query: string);
begin
  {Text if we are connected}
  If not Fconnected then Fail('Not connected');

  {Execute the Query}
	if fLibrary.mysql_query(fMySQL, pChar(Query)) <> 0 then Fail;
end;

{******************************************************}
function TalMySqlClient.insert_id(Query: string): Int64;
Begin
  {Execute the Query}
  UpdateData(Query);

  {Get The ID}
  Result := fLibrary.mysql_insert_id(fMySQL);
end;

end.
