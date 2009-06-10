{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALFBXClient (Alcinoe FireBird Express Client)
Version:      3.50

Description:  Retrieving Data as XML from Firebird Server.

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

History :

Link :        http://www.progdigy.com/modules.php?name=UIB

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALFBXClient;

interface

uses classes,
     SysUtils,
     AlXmlDoc,
     ALFBXLib,
     ALFBXBase,
     Windows;

Type

  {---------------------------}
  TAlFBXClient = Class(Tobject)
  Private
    FSQLDIALECT: word;
    FLibrary: TALFBXLibrary;
    fDBHandle: IscDbHandle;
    fTraHandle: IscTrHandle;
    fdefaultFormatSettings: TformatSettings;
    fNullString: String;
    function GetConnected: Boolean;
    function GetInTransaction: Boolean;
  Protected
  Public
    Constructor Create(ApiVer: TALFBXVersion_API; const lib: string = GDS32DLL); virtual;
    Destructor Destroy; Override;
    function GetFieldValue(aSQLDA:TALFBXSQLResult;
                           aDBHandle: IscDbHandle;
                           aTraHandle: IscTrHandle;
                           aIndex: Integer;
                           aFormatSettings: TformatSettings): String;
    procedure connect(aDataBaseName,
                      aDataBaseLogin,
                      aDataBasePwd,
                      aDataBaseCharSet: String);
    procedure Disconnect;
    Procedure TransactionStart(Readonly: Boolean; const TPB: string = '');
    Procedure TransactionCommit;
    Procedure TransactionRollback;
    Procedure SelectData(SQL: String;
                         RowTag: String;
                         XMLDATA : TalXMLNode;
                         Const FormatSettings : TformatSettings); overload;
    Function  SelectData(SQL: String;
                         RowTag: String;
                         Const FormatSettings : TformatSettings): String; overload;
    Procedure SelectData(SQL: string;
                         XMLDATA : TalXMLNode;
                         Const FormatSettings : TformatSettings); overload;
    Function  SelectData(SQL: string;
                         Const FormatSettings : TformatSettings): string; overload;
    procedure UpdateData(SQL: string; const Blobs: array of Tstream);
    Property Connected: Boolean Read GetConnected;
    Property InTransaction: Boolean read GetInTransaction;
    Property NullString: String Read fNullString Write fNullString;
  end;

implementation

uses AlFcnString,
     AlFcnHTML;

{***************************************************************************************}
constructor TAlFBXClient.Create(ApiVer: TALFBXVersion_API; const lib: string = GDS32DLL);
begin
  fDBHandle := nil;
  fTraHandle := nil;
  FLibrary := TALFBXLibrary.Create(ApiVer);
  fLibrary.Load(lib);
  GetLocaleFormatSettings(1033, fdefaultFormatSettings);
  fNullString := '';
  FSQLDIALECT := 3;
end;

{******************************}
destructor TAlFBXClient.Destroy;
begin
  if Connected then disconnect;
  FLibrary.Free;
  inherited;
end;

{****************************************************************************************************}
procedure TAlFBXClient.connect(aDataBaseName, aDataBaseLogin, aDataBasePwd, aDataBaseCharSet: String);
begin
  if connected then raise Exception.Create('Already connected!');
  FLibrary.AttachDatabase(
                          aDataBaseName,
                          fDBHandle,
                          'user_name = '+aDataBaseLogin+'; '+
                          'password = '+aDataBasePwd+'; '+
                          'lc_ctype = '+aDataBaseCharSet
                         );
end;

{********************************}
procedure TAlFBXClient.Disconnect;
begin
  if not Connected then raise Exception.Create('Not connected!');
  if InTransaction then TransactionRollback;
  Flibrary.DetachDatabase(fDBHandle);
  fDBHandle := nil;
end;

{******************************************}
function TAlFBXClient.GetConnected: Boolean;
begin
  result := assigned(fDBHandle)
end;

{**********************************************}
function TAlFBXClient.GetInTransaction: Boolean;
begin
  result := assigned(fTraHandle)
end;

{****************************************************************************}
procedure TAlFBXClient.TransactionStart(Readonly: Boolean; const TPB: string);
begin
  if InTransaction then raise Exception.Create('Another transaction is active!');

  If TPB = '' then begin

    if Readonly then begin

      Flibrary.TransactionStart(
                                fTraHandle,
                                fDBHandle,

                                isc_tpb_version3 + //Transaction version number is used internally by the InterBase engine. It is always be
                                                   //the first attribute specified in the TPB, and must always be set to isc_tpb_version3.
                                                   //isc_tpb_version3 = InterBase version 3 transaction

                                isc_tpb_read +     //Access mode describes the actions that can be performed by the functions associated with
                                                   //the transaction. Valid access modes are:
                                                   //  * isc_tpb_read: Read-only access mode that allows a transaction only to select data
                                                   //                  from tables
                                                   //  * isc_tpb_write: Read-write access mode of that allows a transaction to select, insert,
                                                   //                   update, and delete table data [Default]

                                isc_tpb_read_committed + isc_tpb_rec_version +
                                                   // Isolation level describes the view of the database given a transaction as it relates to
                                                   // actions performed by other simultaneously occurring transactions.
                                                   // Valid isolation levels are:
                                                   //  * isc_tpb_concurrency: High throughput, high concurrency transaction with acceptable
                                                   //                         consistency; use of this parameter takes full advantage of the InterBase
                                                   //                         multi-generational transaction model [Default]
                                                   //                         By default, after a transaction starts it cannot access committed changes
                                                   //                         to a table made by other simultaneous transactions, even though it shares
                                                   //                         access to the table with them. Such a transaction has an isolation level of
                                                   //                         isc_tpb_concurrency, meaning it can have concurrent access to tables also
                                                   //                         accessed simultaneously by other transactions.
                                                   //  * isc_tpb_consistency: Table-locking transaction model
                                                   //                         InterBase also supports a restrictive isolation level. isc_tpb_consistency
                                                   //                         prevents a transaction from accessing tables if they are written to by other
                                                   //                         transactions; it also prevents other transactions from writing to a table
                                                   //                         once this transaction writes to it. This isolation level is designed to
                                                   //                         guarantee that if a transaction writes to a table before other simultaneous
                                                   //                         read and write transactions, then only it can change a table?s data. Because
                                                   //                         it essentially restricts (and often prevents) shared access to tables,
                                                   //                         isc_tpb_consistency should be used with care.
                                                   //  * isc_tpb_read_committed, isc_tpb_rec_version: High throughput, high concurrency transaction
                                                   //                                                 that can read changes committed by other concurrent
                                                   //                                                 transactions. Use of this parameter takes full advantage
                                                   //                                                 of the InterBase multi-generational transaction model.
                                                   //                                                 * isc_tpb_rec_version: Enables an isc_tpb_read_committed
                                                   //                                                   transaction to read the most recently
                                                   //                                                   committed version of a record even if
                                                   //                                                   other, uncommitted versions are pending.
                                                   //                                                 -------
                                                   //                                                 isc_tpb_read_committed, offers all the advantages of the
                                                   //                                                 isc_tpb_concurrency isolation level and additionally enables
                                                   //                                                 a transaction to access changes committed by other
                                                   //                                                 simultaneous transactions. Two other parameters,
                                                   //                                                 isc_tpb_rec_version, and isc_tpb_no_rec_version, should be
                                                   //                                                 used with the isc_tpb_read_committed parameter. They offer
                                                   //                                                 refined control over the committed changes a transaction is
                                                   //                                                 permitted to access:
                                                   //                                                 * isc_tpb_rec_version specifies that a transaction can read
                                                   //                                                   the latest committed version of a row, even if a more recent
                                                   //                                                   uncommitted version is pending.

                                                   //  * isc_tpb_read_committed, isc_tpb_no_rec_version: High throughput, high concurrency transaction
                                                   //                                                    that can read changes committed by other concurrent
                                                   //                                                    transactions. Use of this parameter takes full advantage
                                                   //                                                    of the InterBase multi-generational transaction model.
                                                   //                                                    * isc_tpb_no_rec_version: Enables an isc_tpb_read_committed
                                                   //                                                      transaction to read only the latest committed version of
                                                   //                                                      a record. If an uncommitted version of a record is
                                                   //                                                      pending and isc_tpb_wait is also specified, then the
                                                   //                                                      transaction waits for the pending record to be committed
                                                   //                                                      or rolled back before proceeding. Otherwise, a lock
                                                   //                                                      conflict error is reported at once.
                                                   //                                                    -------
                                                   //                                                    isc_tpb_read_committed, offers all the advantages of the
                                                   //                                                    isc_tpb_concurrency isolation level and additionally enables
                                                   //                                                    a transaction to access changes committed by other
                                                   //                                                    simultaneous transactions. Two other parameters,
                                                   //                                                    isc_tpb_rec_version, and isc_tpb_no_rec_version, should be
                                                   //                                                    used with the isc_tpb_read_committed parameter. They offer
                                                   //                                                    refined control over the committed changes a transaction is
                                                   //                                                    permitted to access:
                                                   //                                                    * isc_tpb_no_rec_version, the default refinement,
                                                   //                                                      specifies that a transaction can only read the latest
                                                   //                                                      version of a row. If a change to a row is pending, but
                                                   //                                                      not yet committed, the row cannot be read.

                                isc_tpb_nowait     // Lock resolution describes how a transaction should react if a lock conflict occurs. Valid
                                                   // lock resolutions are:
                                                   // * isc_tpb_wait: Lock resolution specifies that the transaction is to wait until locked
                                                   //                 resources are released before retrying an operation [Default]
                                                   // * isc_tpb_nowait: Lock resolution specifies that the transaction is not to wait for locks to be
                                                   //                   released, but instead, a lock conflict error should be returned immediately
                               )
    end

    else begin

      Flibrary.TransactionStart(
                                fTraHandle,
                                fDBHandle,

                                isc_tpb_version3 + //Transaction version number is used internally by the InterBase engine. It is always be
                                                   //the first attribute specified in the TPB, and must always be set to isc_tpb_version3.
                                                   //isc_tpb_version3 = InterBase version 3 transaction

                                isc_tpb_write +    //Access mode describes the actions that can be performed by the functions associated with
                                                   //the transaction. Valid access modes are:
                                                   //  * isc_tpb_read: Read-only access mode that allows a transaction only to select data
                                                   //                  from tables
                                                   //  * isc_tpb_write: Read-write access mode of that allows a transaction to select, insert,
                                                   //                   update, and delete table data [Default]

                                isc_tpb_read_committed + isc_tpb_no_rec_version +
                                                   // Isolation level describes the view of the database given a transaction as it relates to
                                                   // actions performed by other simultaneously occurring transactions.
                                                   // Valid isolation levels are:
                                                   //  * isc_tpb_concurrency: High throughput, high concurrency transaction with acceptable
                                                   //                         consistency; use of this parameter takes full advantage of the InterBase
                                                   //                         multi-generational transaction model [Default]
                                                   //                         By default, after a transaction starts it cannot access committed changes
                                                   //                         to a table made by other simultaneous transactions, even though it shares
                                                   //                         access to the table with them. Such a transaction has an isolation level of
                                                   //                         isc_tpb_concurrency, meaning it can have concurrent access to tables also
                                                   //                         accessed simultaneously by other transactions.
                                                   //  * isc_tpb_consistency: Table-locking transaction model
                                                   //                         InterBase also supports a restrictive isolation level. isc_tpb_consistency
                                                   //                         prevents a transaction from accessing tables if they are written to by other
                                                   //                         transactions; it also prevents other transactions from writing to a table
                                                   //                         once this transaction writes to it. This isolation level is designed to
                                                   //                         guarantee that if a transaction writes to a table before other simultaneous
                                                   //                         read and write transactions, then only it can change a table?s data. Because
                                                   //                         it essentially restricts (and often prevents) shared access to tables,
                                                   //                         isc_tpb_consistency should be used with care.
                                                   //  * isc_tpb_read_committed, isc_tpb_rec_version: High throughput, high concurrency transaction
                                                   //                                                 that can read changes committed by other concurrent
                                                   //                                                 transactions. Use of this parameter takes full advantage
                                                   //                                                 of the InterBase multi-generational transaction model.
                                                   //                                                 * isc_tpb_rec_version: Enables an isc_tpb_read_committed
                                                   //                                                   transaction to read the most recently
                                                   //                                                   committed version of a record even if
                                                   //                                                   other, uncommitted versions are pending.
                                                   //                                                 -------
                                                   //                                                 isc_tpb_read_committed, offers all the advantages of the
                                                   //                                                 isc_tpb_concurrency isolation level and additionally enables
                                                   //                                                 a transaction to access changes committed by other
                                                   //                                                 simultaneous transactions. Two other parameters,
                                                   //                                                 isc_tpb_rec_version, and isc_tpb_no_rec_version, should be
                                                   //                                                 used with the isc_tpb_read_committed parameter. They offer
                                                   //                                                 refined control over the committed changes a transaction is
                                                   //                                                 permitted to access:
                                                   //                                                 * isc_tpb_rec_version specifies that a transaction can read
                                                   //                                                   the latest committed version of a row, even if a more recent
                                                   //                                                   uncommitted version is pending.

                                                   //  * isc_tpb_read_committed, isc_tpb_no_rec_version: High throughput, high concurrency transaction
                                                   //                                                    that can read changes committed by other concurrent
                                                   //                                                    transactions. Use of this parameter takes full advantage
                                                   //                                                    of the InterBase multi-generational transaction model.
                                                   //                                                    * isc_tpb_no_rec_version: Enables an isc_tpb_read_committed
                                                   //                                                      transaction to read only the latest committed version of
                                                   //                                                      a record. If an uncommitted version of a record is
                                                   //                                                      pending and isc_tpb_wait is also specified, then the
                                                   //                                                      transaction waits for the pending record to be committed
                                                   //                                                      or rolled back before proceeding. Otherwise, a lock
                                                   //                                                      conflict error is reported at once.
                                                   //                                                    -------
                                                   //                                                    isc_tpb_read_committed, offers all the advantages of the
                                                   //                                                    isc_tpb_concurrency isolation level and additionally enables
                                                   //                                                    a transaction to access changes committed by other
                                                   //                                                    simultaneous transactions. Two other parameters,
                                                   //                                                    isc_tpb_rec_version, and isc_tpb_no_rec_version, should be
                                                   //                                                    used with the isc_tpb_read_committed parameter. They offer
                                                   //                                                    refined control over the committed changes a transaction is
                                                   //                                                    permitted to access:
                                                   //                                                    * isc_tpb_no_rec_version, the default refinement,
                                                   //                                                      specifies that a transaction can only read the latest
                                                   //                                                      version of a row. If a change to a row is pending, but
                                                   //                                                      not yet committed, the row cannot be read.

                                isc_tpb_nowait     // Lock resolution describes how a transaction should react if a lock conflict occurs. Valid
                                                   // lock resolutions are:
                                                   // * isc_tpb_wait: Lock resolution specifies that the transaction is to wait until locked
                                                   //                 resources are released before retrying an operation [Default]
                                                   // * isc_tpb_nowait: Lock resolution specifies that the transaction is not to wait for locks to be
                                                   //
                               )

    end

  end

  else begin

    Flibrary.TransactionStart(
                              fTraHandle,
                              fDBHandle,
                              TPB
                             );

  end;

end;

{***************************************}
procedure TAlFBXClient.TransactionCommit;
begin
  if not InTransaction then raise Exception.Create('No active transaction to commit!');

  try
    Flibrary.TransactionCommit(fTraHandle);
  finally
    fTraHandle := nil;
  end;
end;

{*****************************************}
procedure TAlFBXClient.TransactionRollback;
begin
  if not InTransaction then raise Exception.Create('No active transaction to rollback!');

  try
    Flibrary.TransactionRollback(fTraHandle);
  finally
    fTraHandle := nil;
  end;
end;

{*********************************************************}
function TAlFBXClient.GetFieldValue(aSQLDA:TALFBXSQLResult;
                                    aDBHandle: IscDbHandle;
                                    aTraHandle: IscTrHandle;
                                    aIndex: Integer;
                                    aFormatSettings: TformatSettings): String;
  {-------------------------}
  Procedure InternalReadBlob;
  var BlobHandle: IscBlobHandle;
  begin
    with FLibrary do begin
      Result := '';
      BlobHandle := nil;
      BlobOpen(aDBHandle, aTraHandle, BlobHandle, aSQLDA.AsQuad[aIndex]);
      try
        BlobReadString(BlobHandle, Result);
      finally
        BlobClose(BlobHandle);
      end;
    end;
  end;

Begin
  If not aSQLDA.IsNull[aIndex] then
    //SQL_VARYING     =    448;
    //SQL_TEXT        =    452;
    //SQL_DOUBLE      =    480;
    //SQL_FLOAT       =    482;
    //SQL_LONG        =    496;
    //SQL_SHORT       =    500;
    //SQL_TIMESTAMP   =    510;
    //SQL_BLOB        =    520;
    //SQL_D_FLOAT     =    530;
    //SQL_ARRAY       =    540;
    //SQL_QUAD        =    550;
    //SQL_TYPE_TIME   =    560;
    //SQL_TYPE_DATE   =    570;
    //SQL_INT64       =    580;
    //SQL_DATE        =    SQL_TIMESTAMP;}
    Case aSQLDA.SQLType[aIndex] of
      SQL_TIMESTAMP : Result := datetostr(aSQLDA.AsDateTime[aIndex], aFormatSettings);
      SQL_TYPE_TIME : Result := Timetostr(aSQLDA.AsTime[aIndex], aFormatSettings);
      SQL_TYPE_DATE : Result := Datetostr(aSQLDA.AsDate[aIndex], aFormatSettings);
      SQL_DOUBLE    : Result := Floattostr(aSQLDA.AsDouble[aIndex], aFormatSettings);
      SQL_FLOAT,
      SQL_D_FLOAT   : Result := Floattostr(aSQLDA.AsSingle[aIndex], aFormatSettings);
      SQL_INT64,
      SQL_LONG,
      SQL_SHORT     : Result := FloatToStr(aSQLDA.asDouble[Aindex],aFormatSettings);
      SQL_BLOB: InternalReadBlob;
      else Result := aSQLDA.AsString[Aindex];
    end
    else result := fNullString;
end;

{************************************}
procedure TAlFBXClient.SelectData(SQL,
                                  RowTag: String;
                                  XMLDATA: TalXMLNode;
                                  const FormatSettings: TformatSettings);
Var NewRec: TalXMLNode;
    ValueRec: TalXMLNode;
    ViewRec: TalXMLNode;
    i: integer;
    aStmtHandle: IscStmtHandle;
    aSqlda: TALFBXSQLResult;
begin
  {init}
  XMLDATA.ChildNodes.Clear;
  ViewRec := XMLdata;

  {load the data}
  aSqlda := TALFBXSQLResult.Create;
  Try
    aStmtHandle := nil;
    Flibrary.DSQLAllocateStatement(fDBHandle, aStmtHandle);
    try
      Flibrary.DSQLPrepare(fDBHandle, fTraHandle, aStmtHandle, Sql, FSQLDIALECT, aSqlda);
      FLibrary.DSQLExecute(fTraHandle, aStmtHandle, FSQLDIALECT, nil);
      while Flibrary.DSQLFetch(fDBHandle, fTraHandle, aStmtHandle, FSQLDIALECT, asqlda) do begin
        if RowTag <> '' then NewRec := ViewRec.AddChild(RowTag)
        Else NewRec := ViewRec;
        For i := 0 to asqlda.FieldCount - 1 do begin
          ValueRec := NewRec.AddChild(ALlowercase(asqlda.AliasName[i]));
          if (aSQLDA.SQLType[i] = SQL_BLOB) then valueRec.ChildNodes.Add(
                                                                         valueRec.OwnerDocument.CreateNode(
                                                                                                           GetFieldValue(
                                                                                                                         asqlda,
                                                                                                                         fDBHandle,
                                                                                                                         fTRAHandle,
                                                                                                                         i,
                                                                                                                         FormatSettings
                                                                                                                        ),
                                                                                                           ntCData
                                                                                                          )
                                                                        )
          else ValueRec.Text := GetFieldValue(asqlda, fDBHandle, fTRAHandle, i, FormatSettings);
        end;
      end;
    finally
      Flibrary.DSQLFreeStatement(aStmtHandle, DSQL_drop);
    end;
  finally
    aSqlda.free;
  end;
end;

{***********************************}
function TAlFBXClient.SelectData(SQL,
                                 RowTag: String;
                                 const FormatSettings: TformatSettings): String;
Const buffSize: integer = 16384;
Var ResultCurrentPos: Integer;
    CurFieldTag : String;
    ResultCurrentLength: integer;

    {-------------------------------------------}
    Procedure InternalMoveStr2Result(Src:String);
    Var l : integer;
    Begin
      L := Length(Src);
      If L+ResultCurrentPos-1>ResultCurrentLength Then begin
        ResultCurrentLength := L+ResultCurrentPos-1 + BuffSize;
        SetLength(Result,ResultCurrentLength);
      end;
      ALMove(Src[1],Result[ResultCurrentPos],L);
      ResultCurrentPos := ResultCurrentPos + L;
    end;

    {--------------------------}
    Procedure InternalExecQuery;
    Var i: integer;
        aStmtHandle: IscStmtHandle;
        aSqlda: TALFBXSQLResult;
    Begin
      aSqlda := TALFBXSQLResult.Create;
      Try
        aStmtHandle := nil;
        Flibrary.DSQLAllocateStatement(fDBHandle, aStmtHandle);
        try
          Flibrary.DSQLPrepare(fDBHandle, fTraHandle, aStmtHandle, Sql, FSQLDIALECT, aSqlda);
          FLibrary.DSQLExecute(fTraHandle, aStmtHandle, FSQLDIALECT, nil);
          while Flibrary.DSQLFetch(fDBHandle, fTraHandle, aStmtHandle, FSQLDIALECT, asqlda) do begin
            if RowTag <> '' then InternalMoveStr2Result('<'+RowTag+'>');
            For i := 0 to asqlda.FieldCount - 1 do begin
              CurFieldTag := ALlowercase(asqlda.AliasName[i]);
              if (aSQLDA.SQLType[i] = SQL_BLOB) then InternalMoveStr2Result('<'+CurFieldTag+'><![CDATA[' + AlStringReplace(
                                                                                                                           GetFieldValue(asqlda, fDBHandle, fTRAHandle, i, FormatSettings),
                                                                                                                           ']]>',
                                                                                                                           ']]]]><![CDATA[>',
                                                                                                                           [RfReplaceAll]
                                                                                                                          ) + ']]></'+CurFieldTag+'>')
              else InternalMoveStr2Result('<'+CurFieldTag+'>' + ALXMLTextElementEncode(GetFieldValue(asqlda, fDBHandle, fTRAHandle, i, FormatSettings)) + '</'+CurFieldTag+'>');
            end;
            if RowTag <> '' then InternalMoveStr2Result('</'+RowTag+'>');
          end;
        finally
          Flibrary.DSQLFreeStatement(aStmtHandle, DSQL_drop);
        end;
      finally
        aSqlda.free;
      end;
    end;

begin
  ResultCurrentLength := BuffSize;
  SetLength(Result,ResultCurrentLength);
  ResultCurrentPos := 1;
  InternalMoveStr2Result('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'#13#10'<root>');
  InternalExecQuery;
  InternalMoveStr2Result('</root>');
  SetLength(Result,ResultCurrentPos-1);
end;

{********************************************}
procedure TAlFBXClient.SelectData(SQL: string;
                                  XMLDATA: TalXMLNode;
                                  const FormatSettings: TformatSettings);
begin
  SelectData(SQL, 'rec', XMLDATA, FormatSettings);
end;

{*******************************************}
function TAlFBXClient.SelectData(SQL: string;
                                 const FormatSettings: TformatSettings): string;
begin
  Result := SelectData(SQL, 'rec', FormatSettings);
end;

{****************************************************************************}
procedure TAlFBXClient.UpdateData(SQL: string; const Blobs: array of Tstream);
Var aSqlpa: TALFBXSQLParams;
    aBlobhandle: IscBlobHandle;
    CurReqBlobsIndex: integer;
begin
  if not Connected then raise exception.Create('Not connected!');
  if not assigned(fTraHandle) then raise exception.Create('No active transaction!');
  if SQL = '' then raise exception.Create('Empty SQL in UpdateData!');
  if length(Blobs) > 0 then begin
    aSqlpa := TALFBXSQLParams.Create;
    try
      for CurReqBlobsIndex := 0 to length(Blobs) - 1 do begin
        aSqlpa.AddFieldType('',uftBlob);
        aBlobhandle := nil;
        aSqlpa.AsQuad[CurReqBlobsIndex] := Flibrary.BlobCreate(fDBHandle,fTraHandle,aBlobHandle);
        Try
          FLibrary.BlobWriteStream(aBlobHandle,Blobs[CurReqBlobsIndex]);
        Finally
          FLibrary.BlobClose(aBlobHandle);
        End;
      end;
      Flibrary.DSQLExecuteImmediate(fDBHandle, fTraHandle, SQL, FSQLDIALECT, aSqlpa);
    finally
      asqlpa.free;
    end;
  end
  else Flibrary.DSQLExecuteImmediate(fDBHandle, fTraHandle, SQL, FSQLDIALECT, nil);
end;

end.

