/// BDE access classes for SynDB units
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynDBBDE;

{
  This file is part of Synopse framework.

  Synopse framework. Copyright (C) 2021 Arnaud Bouchez
  Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):

  
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Todo:
  - use BDE metadata to retrieve table names and field definitions from
    any supported database (not only our SynDB.TSQLDBDefinition list)
}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface


uses
  Windows, SysUtils,
  {$IFNDEF DELPHI5OROLDER}
  Variants,
  {$ENDIF}
  Classes, Contnrs,
  SynCommons,
  SynLog,
  SynDB,
  DBTables,
  SynDBDataset;


{ -------------- BDE database engine connection }

type
  /// Exception type associated to the direct BDE connection
  ESQLDBBDE = class(ESQLDBDataset);


  /// implement properties shared by BDE connections
  TSQLDBBDEConnectionProperties = class(TSQLDBDatasetConnectionProperties)
  protected
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - do nothing by now (BDE metadata may be used in the future)
    procedure GetForeignKeys; override;
    /// this overridden method will retrieve the kind of DBMS from the main connection
    function GetDBMS: TSQLDBDefinition; override;
  public
    /// initialize the properties to connect to the BDE engine
    // - aServerName shall contain the BDE Alias name
    // - aDatabaseName is ignored
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSQLDBBDEConnection instance
    function NewConnection: TSQLDBConnection; override;
  end;


  /// implements a direct connection via the BDE access layer
  TSQLDBBDEConnection = class(TSQLDBConnectionThreadSafe)
  protected
    fDatabase: TDatabase;
    fSession: TSession;
    fDBMS: TSQLDBDefinition;
    fDBMSName: RawUTF8;
  public
    /// prepare a connection to a specified BDE database server
    constructor Create(aProperties: TSQLDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
    /// connect to the specified BDE server
    // - should raise an ESQLDBBDE on error
    procedure Connect; override;
    /// stop connection to the specified BDE database server
    // - should raise an ESQLDBBDE on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// create a new statement instance
    function NewStatement: TSQLDBStatement; override;
    /// begin a Transaction for this connection
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// access to the associated BDE connection instance
    property Database: TDatabase read fDatabase;
  published
    /// the remote DBMS name, as retrieved at BDE connection creation
    property DBMSName: RawUTF8 read fDBMSName;
    /// the remote DBMS type, as retrieved at BDE connection creation
    property DBMS: TSQLDBDefinition read fDBMS;
  end;

  /// implements a statement via a BDE connection
  TSQLDBBDEStatement = class(TSQLDBDatasetStatement)
  protected
    /// initialize and set fQuery internal field as expected
    procedure DatasetCreate; override;
    /// set fQueryParams internal field as expected
    function DatasetPrepare(const aSQL: string): boolean; override;
    /// execute underlying TQuery.ExecSQL
    procedure DatasetExecSQL; override;
  public
  end;


implementation


{ TSQLDBBDEConnectionProperties }

constructor TSQLDBBDEConnectionProperties.Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  inherited Create(aServerName,aDatabaseName,aUserID,aPassWord);
  {$ifndef UNICODE}
  fForceInt64AsFloat := true; // BDE is old and deprecated :(
  {$endif}
end;

procedure TSQLDBBDEConnectionProperties.GetForeignKeys;
begin
  { TODO : get FOREIGN KEYS from BDE metadata ? }
end;

function TSQLDBBDEConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TSQLDBBDEConnection.Create(self);
end;

function TSQLDBBDEConnectionProperties.GetDBMS: TSQLDBDefinition;
begin
  if fDBMS=dUnknown then // retrieve DBMS type from alias driver name
    fDBMS := (MainConnection as TSQLDBBDEConnection).DBMS;
  result := fDBMS;
end;


{ TSQLDBBDEConnection }

procedure TSQLDBBDEConnection.Commit;
begin
  inherited Commit;
  try
    fDatabase.Commit;
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

var
  BDEConnectionCount: integer = 0;

constructor TSQLDBBDEConnection.Create(aProperties: TSQLDBConnectionProperties);
const
  PCHARS: array[0..2] of PAnsiChar = (
    'ORACLE','MSSQL','MSACCESS');
  TYPES: array[-1..high(PCHARS)] of TSQLDBDefinition = (
    dDefault,dOracle,dMSSQL,dJet);
var alias: string;
begin
  inherited Create(aProperties);
  fDatabase := TDatabase.Create(nil);
  fSession := TSession.Create(nil);
  fSession.AutoSessionName := true;
  fDatabase.SessionName := fSession.SessionName;
  fDatabase.LoginPrompt := false;
  inc(BDEConnectionCount);
  alias := UTF8ToString(fProperties.ServerName);
  fDatabase.DatabaseName := 'SynDB'+alias+IntToStr(BDEConnectionCount);
  fDatabase.AliasName := alias;
  fDatabase.Params.Text := Format('USER NAME=%s'#13#10'PASSWORD=%s',
    [UTF8ToString(fProperties.UserID),UTF8ToString(fProperties.PassWord)]);
  fDBMSName := StringToUTF8(fSession.GetAliasDriverName(alias));
  fDBMS := TYPES[IdemPCharArray(pointer(fDBMSName),PCHARS)];
end;

procedure TSQLDBBDEConnection.Connect;
var Log: ISynLog;
begin
  if (fSession=nil) or (fDatabase=nil) then
    raise ESQLDBBDE.CreateUTF8('%.Connect() on % failed: Database=nil',
      [self,fProperties.ServerName]);
  Log := SynDBLog.Enter('Connect to Alias=%',[fDatabase.AliasName],self);
  try
    fSession.Open;
    fDatabase.Open;
    inherited Connect; // notify any re-connection 
  except
    on E: Exception do begin
      Log.Log(sllError,E);
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

destructor TSQLDBBDEConnection.Destroy;
begin
  try
   Disconnect;
  except
    on Exception do
  end;
  inherited;
  FreeAndNil(fDatabase);
  FreeAndNil(fSession);
end;

procedure TSQLDBBDEConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statements 
  finally
    if fDatabase<>nil then
      fDatabase.Close;
    if (fSession<>nil) and fSession.Active then
      fSession.Close;
  end;
end;

function TSQLDBBDEConnection.IsConnected: boolean;
begin
  result := Assigned(fDatabase) and fDatabase.Connected;
end;

function TSQLDBBDEConnection.NewStatement: TSQLDBStatement;
begin
  result := TSQLDBBDEStatement.Create(self);
end;

procedure TSQLDBBDEConnection.Rollback;
begin
  inherited Rollback;
  fDatabase.Rollback;
end;

procedure TSQLDBBDEConnection.StartTransaction;
begin
  inherited StartTransaction;
  fDatabase.StartTransaction;
end;


{ TSQLDBBDEStatement }

procedure TSQLDBBDEStatement.DatasetCreate;
begin
  fQuery := DBTables.TQuery.Create(nil);
  with DBTables.TQuery(fQuery) do begin
    DatabaseName := (fConnection as TSQLDBBDEConnection).Database.DatabaseName;
    SessionName := TSQLDBBDEConnection(fConnection).Database.Session.SessionName;
  end;
end;

function TSQLDBBDEStatement.DatasetPrepare(const aSQL: string): boolean;
begin
  (fQuery as DBTables.TQuery).SQL.Text := aSQL;
  fQueryParams := DBTables.TQuery(fQuery).Params;
  result := fQueryParams<>nil;
end;

procedure TSQLDBBDEStatement.DatasetExecSQL;
begin
  (fQuery as DBTables.TQuery).ExecSQL;
end;

initialization
  TSQLDBBDEConnectionProperties.RegisterClassNameForDefinition;
end.
