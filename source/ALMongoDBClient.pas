//rename TALFBXClientSelectDataSQL to TALFBXClientSelectDataQuery etc..
//   add       class function Create: TALMongoDBClientSelectDataQUERYFlags; static; {$IF CompilerVersion >= 17.0}inline;{$IFEND} to them
// create          Procedure ALMongoDBClientInitSelectDataQUERY(var aSelectDataQUERY: TALMongoDBClientSelectDataQUERY);
//  remove   ==>  XMLDATA.ChildNodes.Clear; from TALFBXClient.SelectData
// add formatsettings to TALJSONDOC

{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALMongoDBClient
Version:      4.00

Description:  Delphi Client for MongoDB database.

              What is MongoDB?  Free & open source, high-performance,
              distributed memory object caching system, generic in
              nature, but intended for use in speeding up dynamic web
              applications by alleviating database load.

              MongoDB is an in-memory key-value store for small chunks
              of arbitrary data (strings, objects) from results of
              database calls, API calls, or page rendering.

              MongoDB is simple yet powerful. Its simple design promotes
              quick deployment, ease of development, and solves many
              problems facing large data caches.

Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

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

Link :

* Please send all your feedback to alcinoe@arkadia.com
* If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALMongoDBClient;

interface

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     WinSock2,
     {$ELSE}
     WinSock,
     {$IFEND}
     Contnrs,
     SyncObjs,
     ALString,
     ALStringList,
     ALJSONDoc;

const MONGO_OP_REPLY = 1; //Reply to a client request. responseTo is set
      MONGO_OP_MSG = 1000; //generic msg command followed by a string
      MONGO_OP_UPDATE = 2001; //update document
      MONGO_OP_INSERT = 2002; //insert new document
      MONGO_RESERVED = 2003; //formerly used for OP_GET_BY_OID
      MONGO_OP_QUERY = 2004; //query a collection
      MONGO_OP_GET_MORE = 2005; //Get more data from a query. See Cursors
      MONGO_OP_DELETE = 2006; //Delete documents
      MONGO_OP_KILL_CURSORS = 2007; //Tell database client is done with a cursor

type

    {-------------------------------------------------------------------------------}
    TALMongoDBClientSelectDataOnNewRowFunct = Procedure(JSONRowData: TALJSONNode;
                                                        ViewTag: AnsiString;
                                                        ExtData: Pointer;
                                                        Var Continue: Boolean);

    {---------------------------------------}
    //bit num     name            description
    // 0	      Reserved	        Must be set to 0.
    // 1	      TailableCursor	  Tailable means cursor is not closed when the last data is retrieved. Rather, the cursor marks the final
    //                            object’s position. You can resume using the cursor later, from where it was located, if more data were received.
    //                            Like any “latent cursor”, the cursor may become invalid at some point (CursorNotFound) – for example if
    //                            the final object it references were deleted.
    // 2	      SlaveOk	          Allow query of replica slave. Normally these return an error except for namespace “local”.
    // 3	      OplogReplay	      Internal replication use only - driver should not set
    // 4	      NoCursorTimeout	  The server normally times out idle cursors after an inactivity period (10 minutes) to prevent excess memory use. Set this option to prevent that.
    // 5	      AwaitData	        Use with TailableCursor. If we are at the end of the data, block for a while rather than returning no data. After a timeout period, we do return as normal.
    // 6	      Exhaust         	Stream the data down full blast in multiple “more” packages, on the assumption that the client will fully read all data queried.
    //                            Faster when you are pulling a lot of data and know you want to pull it all down. Note: the client is not allowed to not
    //                            read all the data unless it closes the connection.
    // 7	      Partial	          Get partial results from a mongos if some shards are down (instead of throwing an error)
    // 8-31	                      Reserved	Must be set to 0.
    TALMongoDBClientSelectDataQUERYFlags = record
      TailMonitoring: boolean;  // it's TailableCursor + AwaitData option. If we are at the end of the data, wait for new data rather than returning no data
                                // a good explanation can be found here: http://docs.mongodb.org/manual/tutorial/create-tailable-cursor/
                                //                                       http://shtylman.com/post/the-tail-of-mongodb/
                                // use only with TALMongoDBClientSelectDataOnNewRowFunct
      SlaveOk: boolean;         // Allow query of replica slave.
                                // Note that you should use this flag even if you do not use the automatic routing to secondaries.
                                // If you connect directly to a secondary in a replica set, you still need to set this flag, which basically tells
                                // the database that you are aware that you might be getting older data and you're okay with that.
                                // If you do not call this, you'll get "not master" errors when you try to query.
      NoCursorTimeout: boolean; // The server normally times out idle cursors after an inactivity period (10 minutes) to prevent excess memory use.
                                // Set this option to prevent that.
      Partial: boolean;         // Get partial results from a mongos if some shards are down (instead of throwing an error)
      class function Create: TALMongoDBClientSelectDataQUERYFlags; static; {$IF CompilerVersion >= 17.0}inline;{$IFEND}
    end;

    {--------------------------------------}
    TALMongoDBClientSelectDataQUERY = record
      FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                      // name with the collection name, using a . for the concatenation. For example, for the database
                                      // foo and the collection bar, the full collection name is foo.bar
      Query: AnsiString; // BSON document that represents the query. The query will contain one or more elements,
                         // all of which must match for a document to be included in the result set. Possible elements
                         // include $query, $orderby, $hint, $explain, and $snapshot.
      ReturnFieldsSelector: AnsiString; // Optional. BSON document that limits the fields in the returned documents.
                                        // The returnFieldsSelector contains one or more elements, each of which is the name
                                        // of a field that should be returned, and and the integer value 1. In JSON notation,
                                        // a returnFieldsSelector to limit to the fields a, b and c would be:
                                        // { a : 1, b : 1, c : 1}
      flags: TALMongoDBClientSelectDataQUERYFlags; // Options (see TALMongoDBClientSelectDataQUERYFlags)
      RowTag: AnsiString; // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
      ViewTag: AnsiString; // the node name under with all records will be stored in the JSON/XML result document.
      Skip: integer; // Sets the number of documents to omit
      First: Integer; // Limits the number of documents to retrieve
      CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                               // cache or not. Values <= 0 deactivate the cache
      class function Create: TALMongoDBClientSelectDataQUERY; static; {$IF CompilerVersion >= 17.0}inline;{$IFEND}
    end;
    TALMongoDBClientSelectDataQUERIES = array of TALMongoDBClientSelectDataQUERY;

    {-------------------------------------------}
    TALMongoDBClientUpdateDataQUERYFlags = record
      Upsert: Boolean; // If set, the database will insert the supplied object into the collection if no matching document is found.
      MultiUpdate: Boolean; // If set, the database will update all matching objects in the collection. Otherwise only updates first matching doc.
      class function Create: TALMongoDBClientUpdateDataQUERYFlags; static; {$IF CompilerVersion >= 17.0}inline;{$IFEND}
    end;

    {--------------------------------------}
    TALMongoDBClientUpdateDataQUERY = record
      FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                      // name with the collection name, using a . for the concatenation. For example, for the database
                                      // foo and the collection bar, the full collection name is foo.bar
      selector: AnsiString; // BSON document that specifies the query for selection of the document to update.
      update: AnsiString; // BSON document that specifies the update to be performed. For information on specifying updates see
                          // http://docs.mongodb.org/manual/tutorial/modify-documents/
      flags: TALMongoDBClientUpdateDataQUERYFlags; // Options (see TALMongoDBClientUpdateDataQUERYFlags)
      class function Create: TALMongoDBClientUpdateDataQUERY; static; {$IF CompilerVersion >= 17.0}inline;{$IFEND}
    end;
    TALMongoDBClientUpdateDataQUERIES = array of TALMongoDBClientUpdateDataQUERY;

    {-------------------------------------------}
    TALMongoDBClientInsertDataQUERYFlags = record
      ContinueOnError: Boolean; // If set, the database will not stop processing a bulk insert if one fails (eg due to duplicate IDs).
                                // This makes bulk insert behave similarly to a series of single inserts, except lastError will be set
                                // if any insert fails, not just the last one. If multiple errors occur, only the most recent will be
                                // reported by getLastError. (new in 1.9.1)
      class function Create: TALMongoDBClientInsertDataQUERYFlags; static; {$IF CompilerVersion >= 17.0}inline;{$IFEND}
    end;

    {--------------------------------------}
    TALMongoDBClientInsertDataQUERY = record
      FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                      // name with the collection name, using a . for the concatenation. For example, for the database
                                      // foo and the collection bar, the full collection name is foo.bar
      documents: AnsiString; // One or more documents to insert into the collection. If there are more than one, they are written in sequence, one after another.
      flags: TALMongoDBClientInsertDataQUERYFlags; // Options (see TALMongoDBClientInsertDataQUERYFlags)
      class function Create: TALMongoDBClientInsertDataQUERY; static; {$IF CompilerVersion >= 17.0}inline;{$IFEND}
    end;
    TALMongoDBClientInsertDataQUERIES = array of TALMongoDBClientInsertDataQUERY;

    {-------------------------------------------}
    TALMongoDBClientDeleteDataQUERYFlags = record
      SingleRemove: Boolean; // If set, the database will remove only the first matching document in the
                             // collection. Otherwise all matching documents will be removed.
      class function Create: TALMongoDBClientDeleteDataQUERYFlags; static; {$IF CompilerVersion >= 17.0}inline;{$IFEND}
    end;

    {--------------------------------------}
    TALMongoDBClientDeleteDataQUERY = record
      FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                      // name with the collection name, using a . for the concatenation. For example, for the database
                                      // foo and the collection bar, the full collection name is foo.bar
      selector: AnsiString; // BSON document that represent the query used to select the documents to be removed
                            // The selector will contain one or more elements, all of which must match for a document
                            // to be removed from the collection
      flags: TALMongoDBClientDeleteDataQUERYFlags; // Options (see TALMongoDBClientDeleteDataQUERYFlags)
      class function Create: TALMongoDBClientDeleteDataQUERY; static; {$IF CompilerVersion >= 17.0}inline;{$IFEND}
    end;
    TALMongoDBClientDeleteDataQUERIES = array of TALMongoDBClientDeleteDataQUERY;

    {---------------------------------------------}
    EAlMongoDBClientException = class(EALException)
    private
      FCloseConnection: Boolean;
    public
      constructor Create(const aMsg: AnsiString; const aCloseConnection: Boolean = False);
      property CloseConnection: Boolean read FCloseConnection write FCloseConnection;
    end;

    {-----------------------------------}
    TAlBaseMongoDBClient = class(TObject)
    Private
      FSendTimeout: Integer;
      FReceiveTimeout: Integer;
      fKeepAlive: Boolean;
      fTCPNoDelay: Boolean;
    protected
      procedure DoSetSendTimeout(aSocketDescriptor: TSocket; const Value: integer); virtual;
      procedure DoSetReceiveTimeout(aSocketDescriptor: TSocket; const Value: integer); virtual;
      procedure DoSetKeepAlive(aSocketDescriptor: TSocket; const Value: boolean); virtual;
      procedure DoSetTCPNoDelay(aSocketDescriptor: TSocket; const Value: boolean); virtual;
      procedure SetSendTimeout(const Value: integer); virtual;
      procedure SetReceiveTimeout(const Value: integer); virtual;
      procedure SetKeepAlive(const Value: boolean); virtual;
      procedure SetTCPNoDelay(const Value: boolean); virtual;
      procedure CheckError(Error: Boolean);
      procedure DoConnect(var aSocketDescriptor: TSocket;
                          const aHost: AnsiString;
                          const APort: integer;
                          const aSendTimeout: Integer;
                          const aReceiveTimeout: Integer;
                          const aKeepAlive: Boolean;
                          const aTCPNoDelay: Boolean);
      Procedure DoDisconnect(var aSocketDescriptor: TSocket); virtual;
      Function SocketWrite(aSocketDescriptor: TSocket; {$IF CompilerVersion >= 23}const{$ELSE}var{$IFEND} Buf; len: Integer): Integer; Virtual;
      Function SocketRead(aSocketDescriptor: TSocket; var buf; len: Integer): Integer; Virtual;
      Function SendCmd(aSocketDescriptor: TSocket;
                       const aCmd: AnsiString;
                       const aGetResponse: boolean): AnsiString;
      Function GetResponse(aSocketDescriptor: TSocket): AnsiString; virtual;
      Procedure CheckLastError(aSocketDescriptor: TSocket);
      Function BuildOPKILLCURSORSMessage(const requestID: integer;                       // identifier for this message
                                         const responseTo: integer;                      // requestID from the original request (used in reponses from db)
                                         const cursorIDs: array of int64): ansiString;   // cursorIDs to be close
      Function BuildOPQUERYMessage(const requestID: integer;                             // identifier for this message
                                   const responseTo: integer;                            // requestID from the original request (used in reponses from db)
                                   const flags: integer;                                 // bit vector of query options.
                                   const fullCollectionName: ansiString;                 // "dbname.collectionname"
                                   const numberToSkip: integer;                          // number of documents to skip
                                   const numberToReturn: integer;                        // number of documents to return in the first OP_REPLY batch
                                   const query: ansiString;                              // query object
                                   const returnFieldsSelector: ansiString): AnsiString;  // Optional. Selector indicating the fields to return
      Procedure ParseOPREPLYMessage(const OpReplyMsg: AnsiString;  // the OP_REPLY message body
                                    var requestID: integer;        // identifier for this message
                                    var responseTo: integer;       // requestID from the original request (used in reponses from db)
                                    var responseFlags: integer;    // bit vector
                                    var cursorID: int64;           // cursor id if client needs to do get more's
                                    var startingFrom: integer;     // where in the cursor this reply is starting
                                    var numberReturned: integer;   // number of documents in the reply
                                    documents: TALJSONNode;        // documents
                                    OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                    ExtData: Pointer;
                                    FormatSettings: TALFormatSettings;
                                    const RowTag: AnsiString;      // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                                    const ViewTag: AnsiString;     // the node name under with all records will be stored in the JSON/XML result document.
                                    var continue: boolean);
      Function BuildOPGETMOREMessage(const requestID: integer;              // identifier for this message
                                     const responseTo: integer;             // requestID from the original request (used in reponses from db)
                                     const fullCollectionName: ansiString;  // "dbname.collectionname"
                                     const numberToReturn: integer;         // number of documents to return in the first OP_REPLY batch
                                     const cursorID: int64): AnsiString;    // cursorID from the OP_REPLY
      Procedure OP_KILL_CURSORS(aSocketDescriptor: TSocket;
                                const cursorIDs: array of int64); // cursorIDs to be close
      Procedure OP_QUERY(aSocketDescriptor: TSocket;
                         const flags: integer;                    // bit vector of query options.
                         const fullCollectionName: ansiString;    // "dbname.collectionname"
                         const numberToSkip: integer;             // number of documents to skip
                         const numberToReturn: integer;           // number of documents to return in the first OP_REPLY batch
                         const query: ansiString;                 // query object.
                         const returnFieldsSelector: ansiString;  // Optional. Selector indicating the fields to return.
                         var responseFlags: integer;              // bit vector
                         var cursorID: int64;                     // cursor id if client needs to do get more's
                         var startingFrom: integer;               // where in the cursor this reply is starting
                         var numberReturned: integer;             // number of documents in the reply
                         documents: TALJSONNode;                  // documents
                         OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                         ExtData: Pointer;
                         FormatSettings: TALFormatSettings;
                         const RowTag: AnsiString;                // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                         const ViewTag: AnsiString;               // the node name under with all records will be stored in the JSON/XML result document.
                         var continue: boolean);
      Procedure OP_GET_MORE(aSocketDescriptor: TSocket;
                            const fullCollectionName: ansiString;   // "dbname.collectionname"
                            const numberToReturn: integer;          // number of documents to return
                            var cursorID: int64;                    // cursorID from the OP_REPLY
                            var responseFlags: integer;             // bit vector
                            var startingFrom: integer;              // where in the cursor this reply is starting
                            var numberReturned: integer;            // number of documents in the reply
                            documents: TALJSONNode;                 // documents
                            OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                            ExtData: Pointer;
                            FormatSettings: TALFormatSettings;
                            const RowTag: AnsiString;               // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                            const ViewTag: AnsiString;              // the node name under with all records will be stored in the JSON/XML result document.
                            var continue: boolean);
    public
      constructor Create; virtual;
      destructor Destroy; override;
      property SendTimeout: Integer read FSendTimeout write SetSendTimeout;
      property ReceiveTimeout: Integer read FReceiveTimeout write SetReceiveTimeout;
      property KeepAlive: Boolean read fKeepAlive write SetKeepAlive;
      property TcpNoDelay: Boolean read fTCPNoDelay write fTCPNoDelay;
    end;

    {--------------------------------------------}
    TAlMongoDBClient = class(TAlBaseMongoDBClient)
    private
      Fconnected: Boolean;
      FSocketDescriptor: TSocket;
    protected
      procedure SetSendTimeout(const Value: integer); override;
      procedure SetReceiveTimeout(const Value: integer); override;
      procedure SetKeepAlive(const Value: boolean); override;
      procedure SetTCPNoDelay(const Value: boolean); override;
    public
      constructor Create; override;
      destructor Destroy; override;
      Procedure Connect(const aHost: AnsiString; const APort: integer); virtual;
      Procedure Disconnect; virtual;

      Procedure SelectData(Queries: TALMongoDBClientSelectDataQUERIES;
                           JSONDATA: TALJSONNode;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer;
                           FormatSettings: TALFormatSettings); overload;
      Procedure SelectData(Query: TALMongoDBClientSelectDataQUERY;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer;
                           FormatSettings: TALFormatSettings); overload;
      Procedure SelectData(FullCollectionName: AnsiString;
                           Query: AnsiString;
                           ReturnFieldsSelector: AnsiString;
                           flags: TALMongoDBClientSelectDataQUERYFlags;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer;
                           FormatSettings: TALFormatSettings); overload;
      Procedure SelectData(FullCollectionName: AnsiString;
                           Query: AnsiString;
                           ReturnFieldsSelector: AnsiString;
                           flags: TALMongoDBClientSelectDataQUERYFlags;
                           Skip: integer;
                           First: Integer;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer;
                           FormatSettings: TALFormatSettings); overload;
      Procedure SelectData(SQL: AnsiString;
                           flags: TALMongoDBClientSelectDataQUERYFlags;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer;
                           FormatSettings: TALFormatSettings); overload;
      Procedure SelectData(SQL: AnsiString;
                           flags: TALMongoDBClientSelectDataQUERYFlags;
                           Skip: integer;
                           First: Integer;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer;
                           FormatSettings: TALFormatSettings); overload;
      Procedure SelectData(Queries: TALMongoDBClientSelectDataQUERIES;
                           JSONDATA: TALJSONNode); overload;
      Procedure SelectData(Query: TALMongoDBClientSelectDataQUERY;
                           JSONDATA: TALJSONNode); overload;
      Procedure SelectData(FullCollectionName: AnsiString;
                           Query: AnsiString;
                           ReturnFieldsSelector: AnsiString;
                           flags: TALMongoDBClientSelectDataQUERYFlags;
                           JSONDATA: TALJSONNode); overload;
      Procedure SelectData(FullCollectionName: AnsiString;
                           Query: AnsiString;
                           ReturnFieldsSelector: AnsiString;
                           flags: TALMongoDBClientSelectDataQUERYFlags;
                           RowTag: AnsiString;
                           JSONDATA: TALJSONNode); overload;
      Procedure SelectData(FullCollectionName: AnsiString;
                           Query: AnsiString;
                           ReturnFieldsSelector: AnsiString;
                           flags: TALMongoDBClientSelectDataQUERYFlags;
                           RowTag: AnsiString;
                           Skip: integer;
                           First: Integer;
                           JSONDATA: TALJSONNode); overload;
      Procedure SelectData(SQL: AnsiString;
                           flags: TALMongoDBClientSelectDataQUERYFlags;
                           JSONDATA: TALJSONNode); overload;
      Procedure SelectData(SQL: AnsiString;
                           flags: TALMongoDBClientSelectDataQUERYFlags;
                           RowTag: AnsiString;
                           JSONDATA: TALJSONNode); overload;
      Procedure SelectData(SQL: AnsiString;
                           flags: TALMongoDBClientSelectDataQUERYFlags;
                           RowTag: AnsiString;
                           Skip: integer;
                           First: Integer;
                           JSONDATA: TALJSONNode); overload;

      procedure UpdateData(Queries: TALMongoDBClientUpdateDataQUERIES); overload;
      procedure UpdateData(Query: TALMongoDBClientUpdateDataQUERY); overload;
      procedure UpdateData(FullCollectionName: AnsiString;
                           selector: AnsiString;
                           update: AnsiString;
                           flags: TALMongoDBClientUpdateDataQUERYFlags); overload;
      procedure UpdateData(SQLs: TALStrings); overload;
      procedure UpdateData(SQL: AnsiString); overload;
      procedure UpdateData(SQLs: array of AnsiString); overload;

      procedure InsertData(Queries: TALMongoDBClientInsertDataQUERIES); overload;
      procedure InsertData(Query: TALMongoDBClientInsertDataQUERY); overload;
      procedure InsertData(FullCollectionName: AnsiString;
                           documents: AnsiString;
                           flags: TALMongoDBClientInsertDataQUERYFlags); overload;

      procedure DeleteData(Queries: TALMongoDBClientDeleteDataQUERIES); overload;
      procedure DeleteData(Query: TALMongoDBClientDeleteDataQUERY); overload;
      procedure DeleteData(FullCollectionName: AnsiString;
                           selector: AnsiString;
                           flags: TALMongoDBClientDeleteDataQUERYFlags); overload;

      property Connected: Boolean read FConnected;
    end;

    {------------------------------------------------}
    TAlMongoDBConnectionPoolContainer = Class(TObject)
      SocketDescriptor: TSocket;
      LastAccessDate: int64;
    End;

    {----------------------------------------------------------}
    TAlMongoDBConnectionPoolClient = class(TAlBaseMongoDBClient)
    private
      FConnectionPool: TObjectList;
      FConnectionPoolCS: TCriticalSection;
      FWorkingConnectionCount: Integer;
      FReleasingAllconnections: Boolean;
      FLastConnectionGarbage: Int64;
      FConnectionMaxIdleTime: integer;
      fHost: AnsiString;
      fPort: integer;
    protected
      procedure SetSendTimeout(const Value: integer); override;
      procedure SetReceiveTimeout(const Value: integer); override;
      procedure SetKeepAlive(const Value: boolean); override;
      procedure SetTCPNoDelay(const Value: boolean); override;
      Function  AcquireConnection: TSocket; virtual;
      Procedure ReleaseConnection(var SocketDescriptor: TSocket;
                                  const CloseConnection: Boolean = False); virtual;
    public
      constructor Create(const aHost: AnsiString; const APort: integer); reintroduce;
      destructor Destroy; override;
      Procedure ReleaseAllConnections(Const WaitWorkingConnections: Boolean = True); virtual;
      property Host: ansiString read fHost;
      property Port: integer read fPort;
    end;

implementation

Uses Windows,
     SysUtils,
     AlWinsock,
     ALWindows;

{***********************************************************************************************}
class function TALMongoDBClientSelectDataQUERYFlags.Create: TALMongoDBClientSelectDataQUERYFlags;
begin
  with result do begin
    TailMonitoring := False;
    SlaveOk := False;
    NoCursorTimeout := False;
    Partial := False;
  end;
end;

{*************************************************************************************}
class function TALMongoDBClientSelectDataQUERY.Create: TALMongoDBClientSelectDataQUERY;
begin
  with result do begin
    FullCollectionName := '';
    Query := '';
    ReturnFieldsSelector := '';
    flags := TALMongoDBClientSelectDataQUERYFlags.Create;
    RowTag := '';
    ViewTag := '';
    Skip := -1;
    First := -1;
    CacheThreshold := -1;
  end;
end;

{***********************************************************************************************}
class function TALMongoDBClientUpdateDataQUERYFlags.Create: TALMongoDBClientUpdateDataQUERYFlags;
begin
  with result do begin
    Upsert := False;
    MultiUpdate := True;
  end;
end;

{*************************************************************************************}
class function TALMongoDBClientUpdateDataQUERY.Create: TALMongoDBClientUpdateDataQUERY;
begin
  with result do begin
    FullCollectionName := '';
    selector := '';
    update := '';
    flags := TALMongoDBClientUpdateDataQUERYFlags.Create;
  end;
end;

{***********************************************************************************************}
class function TALMongoDBClientInsertDataQUERYFlags.Create: TALMongoDBClientInsertDataQUERYFlags;
begin
  with result do begin
    ContinueOnError := False;
  end;
end;

{*************************************************************************************}
class function TALMongoDBClientInsertDataQUERY.Create: TALMongoDBClientInsertDataQUERY;
begin
  with result do begin
    FullCollectionName := '';
    documents := '';
    flags := TALMongoDBClientInsertDataQUERYFlags.Create;
  end;
end;

{***********************************************************************************************}
class function TALMongoDBClientDeleteDataQUERYFlags.Create: TALMongoDBClientDeleteDataQUERYFlags;
begin
  with result do begin
    SingleRemove := false;
  end;
end;

{*************************************************************************************}
class function TALMongoDBClientDeleteDataQUERY.Create: TALMongoDBClientDeleteDataQUERY;
begin
  with result do begin
    FullCollectionName := '';
    selector := '';
    flags := TALMongoDBClientDeleteDataQUERYFlags.Create;
  end;
end;

{************************************************************************************************************}
constructor EAlMongoDBClientException.Create(const aMsg: AnsiString; const aCloseConnection: Boolean = False);
begin
  fCloseConnection := aCloseConnection;
  inherited create(aMsg);
end;

{**************************************}
constructor TAlBaseMongoDBClient.Create;
var aWSAData: TWSAData;
begin
  CheckError(WSAStartup(MAKEWORD(2,2), aWSAData) <> 0);
  FSendTimeout := 10000; // 10 seconds
  FReceiveTimeout := 10000; // 10 seconds
  FKeepAlive := True;
  fTCPNoDelay := True;
end;

{**************************************}
destructor TAlBaseMongoDBClient.Destroy;
begin
  WSACleanup;
  inherited;
end;

{********************************************************}
procedure TAlBaseMongoDBClient.CheckError(Error: Boolean);
begin
  if Error then RaiseLastOSError;
end;

{**********************************************************************}
procedure TAlBaseMongoDBClient.DoConnect(var aSocketDescriptor: TSocket;
                                         const aHost: AnsiString;
                                         const APort: integer;
                                         const aSendTimeout: Integer;
                                         const aReceiveTimeout: Integer;
                                         const aKeepAlive: Boolean;
                                         const aTCPNoDelay: Boolean);

  {--------------------------------------------------------------}
  procedure _CallServer(const Server:AnsiString; const Port:word);
  var SockAddr:Sockaddr_in;
      IP: AnsiString;
  begin
    aSocketDescriptor:=Socket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
    CheckError(aSocketDescriptor=INVALID_SOCKET);
    FillChar(SockAddr,SizeOf(SockAddr),0);
    SockAddr.sin_family:=AF_INET;
    SockAddr.sin_port:=swap(Port);
    SockAddr.sin_addr.S_addr:=inet_addr(PAnsiChar(Server));
    {$IF CompilerVersion >= 23} {Delphi XE2}
    If SockAddr.sin_addr.S_addr = INADDR_NONE then begin
    {$ELSE}
    If SockAddr.sin_addr.S_addr = integer(INADDR_NONE) then begin
    {$IFEND}
      checkError(not ALHostToIP(Server, IP));
      SockAddr.sin_addr.S_addr:=inet_addr(PAnsiChar(IP));
    end;
    {$IF CompilerVersion >= 23} {Delphi XE2}
    CheckError(WinSock2.Connect(aSocketDescriptor,TSockAddr(SockAddr),SizeOf(SockAddr))=SOCKET_ERROR);
    {$ELSE}
    CheckError(WinSock.Connect(aSocketDescriptor,SockAddr,SizeOf(SockAddr))=SOCKET_ERROR);
    {$IFEND}

  end;

begin

  Try

    _CallServer(aHost,aPort);
    DoSetSendTimeout(aSocketDescriptor, aSendTimeout);
    DoSetReceiveTimeout(aSocketDescriptor, aReceiveTimeout);
    DoSetKeepAlive(aSocketDescriptor, aKeepAlive);
    DoSetTCPNoDelay(aSocketDescriptor, aTCPNoDelay);

  Except
    DoDisconnect(aSocketDescriptor);
    raise;
  end;

end;

{**************************************************************************}
procedure TAlBaseMongoDBClient.DoDisconnect(var aSocketDescriptor: TSocket);
begin
  if aSocketDescriptor <> INVALID_SOCKET then begin
    ShutDown(aSocketDescriptor,SD_BOTH);
    CloseSocket(aSocketDescriptor);
    aSocketDescriptor := INVALID_SOCKET;
  end;
end;

{***************************************************************}
Function TAlBaseMongoDBClient.SendCmd(aSocketDescriptor: TSocket;
                                      const aCmd: AnsiString;
                                      const aGetResponse: boolean): AnsiString;
Var P: PAnsiChar;
    L: Integer;
    ByteSent: integer;
begin
  p:=@aCmd[1]; // pchar
  l:=length(aCmd);
  while l>0 do begin
    ByteSent:=SocketWrite(aSocketDescriptor, p^,l);
    if ByteSent<=0 then raise EALException.Create('Connection close gracefully!');
    inc(p,ByteSent);
    dec(l,ByteSent);
  end;
  if aGetResponse then Result := GetResponse(aSocketDescriptor)
  else result := '';
end;

{********************************************************************************}
function TAlBaseMongoDBClient.GetResponse(aSocketDescriptor: TSocket): AnsiString;
Var aBytesReceived: Integer;
    aResultPos: Integer;
    aMessageLength: Integer;
const aBuffSize: integer = 8192;
begin

  //init local var
  Setlength(Result,aBuffSize);
  aResultPos := 0;
  aMessageLength := 0;

  //loop still we receive the full answer
  While True do begin

    //expnd the buffer
    if aResultPos = length(Result) then setlength(Result, length(Result) + aBuffSize);

    //read string from socket
    aBytesReceived := SocketRead(aSocketDescriptor, Result[aResultPos+1], length(Result) - aResultPos);
    If aBytesReceived <= 0 then raise EALException.Create('Connection close gracefully!');
    aResultPos := aResultPos + aBytesReceived;

    //the first 4 bytes contain the length of the message
    if (aMessageLength = 0) and (aResultPos >= sizeof(aMessageLength)) then begin
      ALMove(Result[1], aMessageLength, sizeof(aMessageLength));
      if aMessageLength < aResultPos then raise EALException.Create('Wrong socket response');
      setlength(Result, aMessageLength);
    end;

    //break if we are at the end of the message
    if aResultPos = aMessageLength then break;

  end;

end;

{************************************************************************}
Procedure TAlBaseMongoDBClient.CheckLastError(aSocketDescriptor: TSocket);
begin

end;

{*******************************************************************************}
Function TAlBaseMongoDBClient.BuildOPKILLCURSORSMessage(const requestID: integer;                       // identifier for this message
                                                        const responseTo: integer;                      // requestID from the original request (used in reponses from db)
                                                        const cursorIDs: array of int64): ansiString;   // cursorIDs to be close
var aCurrPos: integer;
    aMessageLength: Integer;
    aOPCode: integer;
    aZERO: integer;
    anumberOfCursorIDs: integer;
    i: integer;
begin

  //
  //  messageLength     requestID       responseTo        opCode                 aZERO
  //  [0][1][2][3]     [4][5][6][7]    [8][9][10][11]   [12][13][14][15]    [16][17][18][19]
  //
  //  numberOfCursorIDs             cursorIDs
  //  [20][21][22][23]   [24][25][26][27][28][29][30][31]....
  //

  //exit if no cursorID
  anumberOfCursorIDs := length(cursorIDs);
  if anumberOfCursorIDs <= 0 then exit;

  // init aMessageLength
  aMessageLength := sizeof(aMessageLength) +
                    sizeof(requestID) +
                    sizeof(responseTo) +
                    sizeof(aOPCode) +
                    sizeof(aZERO) +
                    sizeof(anumberOfCursorIDs) +
                    anumberOfCursorIDs * sizeof(cursorIDs[low(cursorIDs)]);

  //init the length of the result
  setlength(result, amessageLength);
  aCurrPos := 1;

  // messageLength
  ALMove(aMessageLength, Result[aCurrPos], sizeof(aMessageLength));
  inc(aCurrPos,sizeof(aMessageLength));

  //requestID
  ALMove(requestID, Result[aCurrPos], sizeof(requestID));
  inc(aCurrPos,sizeof(requestID));

  //responseTo
  ALMove(responseTo, Result[aCurrPos], sizeof(responseTo));
  inc(aCurrPos,sizeof(responseTo));

  //opCode
  aOPCode := MONGO_OP_KILL_CURSORS;
  ALMove(aOPCode, Result[aCurrPos], sizeof(aOPCode));
  inc(aCurrPos,sizeof(aOPCode));

  //aZERO
  aZERO := 0;
  ALMove(aZERO, Result[aCurrPos], sizeof(aZERO));
  inc(aCurrPos,sizeof(aZERO));

  //anumberOfCursorIDs
  ALMove(anumberOfCursorIDs, Result[aCurrPos], sizeof(anumberOfCursorIDs));
  inc(aCurrPos,sizeof(anumberOfCursorIDs));

  //cursorIDs
  for i := low(cursorIDs) to High(cursorIDs) do
    ALMove(cursorIDs[i], Result[aCurrPos], sizeof(cursorIDs[i]));

end;

{*************************************************************************}
Function TAlBaseMongoDBClient.BuildOPQUERYMessage(const requestID: integer;                             // identifier for this message
                                                  const responseTo: integer;                            // requestID from the original request (used in reponses from db)
                                                  const flags: integer;                                 // bit vector of query options.
                                                  const fullCollectionName: ansiString;                 // "dbname.collectionname"
                                                  const numberToSkip: integer;                          // number of documents to skip
                                                  const numberToReturn: integer;                        // number of documents to return in the first OP_REPLY batch
                                                  const query: ansiString;                              // query object
                                                  const returnFieldsSelector: ansiString): AnsiString;  // Optional. Selector indicating the fields to return
var aCurrPos: integer;
    aMessageLength: Integer;
    aOPCode: integer;
    aBsonQuery: ansiString;
    aBSONReturnFieldsSelector: ansiString;
    aJsonDocument: TALJsonDocument;
begin

  //
  //  messageLength     requestID       responseTo        opCode                 flags
  //  [0][1][2][3]     [4][5][6][7]    [8][9][10][11]   [12][13][14][15]    [16][17][18][19]
  //
  //  fullCollectionName       numberToSkip        numberToReturn          query     returnFieldsSelector
  //  [20]...[23]            [24][25][26][27]     [28][29][30][31]      [32]...[50]    [51]...[70]
  //

  //init aBsonQuery
  if (length(query) > sizeof(aMessageLength)) then begin
    ALMove(query[1], aMessageLength, sizeof(aMessageLength));
    if aMessageLength <> length(query) then begin // yes but aMessageLength can = length(query) and be not a BSON string ... i thing this will be too much rare
      aJsonDocument := TALJsonDocument.Create;
      try
        aJsonDocument.LoadFromJSON(query);
        aJsonDocument.SaveToBSON(aBsonQuery);
      finally
        aJsonDocument.Free;
      end;
    end
    else aBsonQuery := query;
  end
  else aBsonQuery := query;

  //init aBSONReturnFieldsSelector
  if (length(returnFieldsSelector) > sizeof(aMessageLength)) then begin
    ALMove(returnFieldsSelector[1], aMessageLength, sizeof(aMessageLength));
    if aMessageLength <> length(returnFieldsSelector) then begin // yes but aMessageLength can = length(query) and be not a BSON string ... i thing this will be too much rare
      aJsonDocument := TALJsonDocument.Create;
      try
        aJsonDocument.LoadFromJSON(returnFieldsSelector);
        aJsonDocument.SaveToBSON(aBSONReturnFieldsSelector);
      finally
        aJsonDocument.Free;
      end;
    end
    else aBSONReturnFieldsSelector := returnFieldsSelector;
  end
  else aBSONReturnFieldsSelector := returnFieldsSelector;

  // init aMessageLength
  aMessageLength := sizeof(aMessageLength) +
                    sizeof(requestID) +
                    sizeof(responseTo) +
                    sizeof(aOPCode) +
                    sizeof(flags) +
                    length(fullCollectionName) + 1{the trailing #0} +
                    sizeof(numberToSkip) +
                    sizeof(numberToReturn) +
                    ALIfThen(length(aBsonQuery) <= 0, 5{length of an empty BSON string}, length(aBsonQuery)) +
                    length(aBSONReturnFieldsSelector);

  //init the length of the result
  setlength(result, amessageLength);
  aCurrPos := 1;

  // messageLength
  ALMove(aMessageLength, Result[aCurrPos], sizeof(aMessageLength));
  inc(aCurrPos,sizeof(aMessageLength));

  //requestID
  ALMove(requestID, Result[aCurrPos], sizeof(requestID));
  inc(aCurrPos,sizeof(requestID));

  //responseTo
  ALMove(responseTo, Result[aCurrPos], sizeof(responseTo));
  inc(aCurrPos,sizeof(responseTo));

  //opCode
  aOPCode := MONGO_OP_QUERY;
  ALMove(aOPCode, Result[aCurrPos], sizeof(aOPCode));
  inc(aCurrPos,sizeof(aOPCode));

  //flags
  ALMove(flags, Result[aCurrPos], sizeof(flags));
  inc(aCurrPos,sizeof(flags));

  //fullCollectionName
  if length(fullCollectionName) <= 0 then raise Exception.Create('FullCollectionName must not be empty');
  ALMove(fullCollectionName[1], result[aCurrPos], length(fullCollectionName));
  inc(aCurrPos,length(fullCollectionName));
  result[aCurrPos] := #0;
  inc(aCurrPos);

  //numberToSkip
  ALMove(numberToSkip, Result[aCurrPos], sizeof(numberToSkip));
  inc(aCurrPos,sizeof(numberToSkip));

  //numberToReturn
  ALMove(numberToReturn, Result[aCurrPos], sizeof(numberToReturn));
  inc(aCurrPos,sizeof(numberToReturn));

  //aBsonQuery
  if length(aBsonQuery) <= 0 then begin
    result[aCurrPos]     := #5;
    result[aCurrPos + 1] := #0;
    result[aCurrPos + 2] := #0;
    result[aCurrPos + 3] := #0;
    result[aCurrPos + 4] := #0;
    inc(aCurrPos,5);
  end
  else begin
    ALMove(aBsonQuery[1], result[aCurrPos], length(aBsonQuery));
    inc(aCurrPos,length(aBsonQuery));
  end;

  //aBSONReturnFieldsSelector
  if length(aBSONReturnFieldsSelector) > 0 then
    ALMove(aBSONReturnFieldsSelector[1], result[aCurrPos], length(aBSONReturnFieldsSelector));

end;

{******************************************************************************}
Procedure TAlBaseMongoDBClient.ParseOPREPLYMessage(const OpReplyMsg: AnsiString;  // the OP_REPLY message body
                                                   var requestID: integer;        // identifier for this message
                                                   var responseTo: integer;       // requestID from the original request (used in reponses from db)
                                                   var responseFlags: integer;    // bit vector
                                                   var cursorID: int64;           // cursor id if client needs to do get more's
                                                   var startingFrom: integer;     // where in the cursor this reply is starting
                                                   var numberReturned: integer;   // number of documents in the reply
                                                   documents: TALJSONNode;        // documents
                                                   OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                                   ExtData: Pointer;
                                                   FormatSettings: TALFormatSettings;
                                                   const RowTag: AnsiString;      // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                                                   const ViewTag: AnsiString;     // the node name under with all records will be stored in the JSON/XML result document.
                                                   var continue: boolean);
Var aMessageLength: integer;
    aOPCode: integer;
    aCurrPos: integer;
    aDocumentStr: AnsiString;
    aNumberOfDocumentsFounded: Integer;
    aTmpRowTag: ansiString;
    aUpdateRowTagByFieldValue: Boolean;
    aJsonNode1: TALJSONNode;
    aJsonNode2: TALJSONNode;
begin

  //
  //  messageLength     requestID       responseTo        opCode              responseFlags
  //  [0][1][2][3]     [4][5][6][7]    [8][9][10][11]   [12][13][14][15]    [16][17][18][19]
  //
  //            cursorID                  startingFrom         numberReturned        documents
  //  [20][22][23][24][25][26][27]       [24][25][26][27]     [28][29][30][31]      [32]...[50]
  //

  // some checkc
  if length(OpReplyMsg) < sizeof(aMessageLength) +
                          sizeof(requestID) +
                          sizeof(responseTo) +
                          sizeof(aOPCode) +
                          sizeof(responseFlags) +
                          sizeof(cursorID) +
                          sizeof(startingFrom) +
                          sizeof(numberReturned) then raise Exception.Create('Wrong OP_REPLY message');

  // init aCurrPos
  aCurrPos := 1;

  // messageLength
  ALMove(OpReplyMsg[aCurrPos], aMessageLength, sizeof(aMessageLength));
  inc(aCurrPos,sizeof(aMessageLength));
  if aMessageLength <> length(OpReplyMsg) then raise Exception.Create('Wrong OP_REPLY message');

  // requestID
  ALMove(OpReplyMsg[aCurrPos], requestID, sizeof(requestID));
  inc(aCurrPos,sizeof(requestID));

  // responseTo
  ALMove(OpReplyMsg[aCurrPos], responseTo, sizeof(responseTo));
  inc(aCurrPos,sizeof(responseTo));

  // OPCode
  ALMove(OpReplyMsg[aCurrPos], aOPCode, sizeof(aOPCode));
  if aOPCode <> MONGO_OP_REPLY then raise Exception.Create('Wrong OP_REPLY message');
  inc(aCurrPos,sizeof(aOPCode));

  // responseFlags
  ALMove(OpReplyMsg[aCurrPos], responseFlags, sizeof(responseFlags));
  inc(aCurrPos,sizeof(responseFlags));

  //bit num	  name	            description
  //0	        CursorNotFound	  Set when getMore is called but the cursor id is not valid at the server. Returned with zero results.
  //1	        QueryFailure	    Set when query failed. Results consist of one document containing an “$err” field describing the failure.
  //2	        ShardConfigStale	Drivers should ignore this. Only mongos will ever see this set, in which case, it needs to update config from the server.
  //3        	AwaitCapable	    Set when the server supports the AwaitData Query option. If it doesn’t, a client should sleep a little between
  //                            getMore’s of a Tailable cursor. Mongod version 1.6 supports AwaitData and thus always sets AwaitCapable.
  //4-31    	Reserved	        Ignore
  if responseFlags and (1 shl 0) <> 0 then raise Exception.Create('Cursor Not Found');
  if responseFlags and (1 shl 1) <> 0 then raise Exception.Create('Query Failure');

  // cursorID
  ALMove(OpReplyMsg[aCurrPos], cursorID, sizeof(cursorID));
  inc(aCurrPos,sizeof(cursorID));

  // startingFrom
  ALMove(OpReplyMsg[aCurrPos], startingFrom, sizeof(startingFrom));
  inc(aCurrPos,sizeof(startingFrom));

  // numberReturned
  ALMove(OpReplyMsg[aCurrPos], numberReturned, sizeof(numberReturned));
  inc(aCurrPos,sizeof(numberReturned));

  // documents
  aNumberOfDocumentsFounded := 0;
  if aCurrPos <= aMessageLength then begin

    //init aUpdateRowTagByFieldValue and aTmpRowTag
    if AlPos('&>',RowTag) = 1 then begin
      aTmpRowTag := AlcopyStr(RowTag, 3, maxint);
      aUpdateRowTagByFieldValue := aTmpRowTag <> '';
    end
    else begin
      aTmpRowTag := RowTag;
      aUpdateRowTagByFieldValue := False;
    end;

    //loop on all node to add
    while True do begin
      if aCurrPos > length(OpReplyMsg) then break;
      if aCurrPos > length(OpReplyMsg) - sizeof(aMessageLength) + 1 then raise Exception.Create('Wrong OP_REPLY message');
      ALMove(OpReplyMsg[aCurrPos], aMessageLength, sizeof(aMessageLength));
      if aCurrPos + aMessageLength - 1 > length(OpReplyMsg) then raise Exception.Create('Wrong OP_REPLY message');
      setlength(adocumentStr, aMessageLength);
      ALMove(OpReplyMsg[aCurrPos], adocumentStr[1], aMessageLength);
      inc(aCurrPos,aMessageLength);
      if aTmpRowTag <> '' then aJsonNode1 := documents.AddChild(aTmpRowTag, ntobject)
      else aJsonNode1 := documents;
      aJsonNode1.LoadFromBSON(adocumentStr, False{ClearChildNodes});
      if aUpdateRowTagByFieldValue then begin
        aJsonNode2 := aJsonNode1.ChildNodes.FindNode(aTmpRowTag);
        if assigned(aJsonNode2) then aJsonNode1.NodeName := aJsonNode2.Text;
      end;
      if assigned(OnNewRowFunct) then begin
        Continue := True;
        OnNewRowFunct(aJsonNode1, ViewTag, ExtData, Continue);
        documents.ChildNodes.Clear;
        if Not Continue then exit;
      end;
      inc(aNumberOfDocumentsFounded);
    end;
  end;
  if aNumberOfDocumentsFounded <> numberReturned then raise Exception.Create('Wrong OP_REPLY message');

end;

{***************************************************************************}
Function TAlBaseMongoDBClient.BuildOPGETMOREMessage(const requestID: integer;              // identifier for this message
                                                    const responseTo: integer;             // requestID from the original request (used in reponses from db)
                                                    const fullCollectionName: ansiString;  // "dbname.collectionname"
                                                    const numberToReturn: integer;         // number of documents to return in the first OP_REPLY batch
                                                    const cursorID: int64): AnsiString;
var aCurrPos: integer;
    aMessageLength: Integer;
    aOPCode: integer;
    aZero: integer;
begin

  //
  //  messageLength     requestID       responseTo        opCode                 ZERO
  //  [0][1][2][3]     [4][5][6][7]    [8][9][10][11]   [12][13][14][15]    [16][17][18][19]
  //
  //  fullCollectionName      numberToReturn                cursorID
  //  [20]...[23]            [24][25][26][27]     [28][29][30][31][32][33][34][35]
  //

  // init aMessageLength
  aMessageLength := sizeof(aMessageLength) +
                    sizeof(requestID) +
                    sizeof(responseTo) +
                    sizeof(aOPCode) +
                    sizeof(aZero) +
                    length(fullCollectionName) + 1{the trailing #0} +
                    sizeof(numberToReturn) +
                    sizeof(cursorID);

  //init the length of the result
  setlength(result, amessageLength);
  aCurrPos := 1;

  // messageLength
  ALMove(aMessageLength, Result[aCurrPos], sizeof(aMessageLength));
  inc(aCurrPos,sizeof(aMessageLength));

  //requestID
  ALMove(requestID, Result[aCurrPos], sizeof(requestID));
  inc(aCurrPos,sizeof(requestID));

  //responseTo
  ALMove(responseTo, Result[aCurrPos], sizeof(responseTo));
  inc(aCurrPos,sizeof(responseTo));

  //opCode
  aOPCode := MONGO_OP_GET_MORE;
  ALMove(aOPCode, Result[aCurrPos], sizeof(aOPCode));
  inc(aCurrPos,sizeof(aOPCode));

  //Zero
  aZero := 0;
  ALMove(aZero, Result[aCurrPos], sizeof(aZero));
  inc(aCurrPos,sizeof(aZero));

  //fullCollectionName
  if length(fullCollectionName) <= 0 then raise Exception.Create('FullCollectionName must not be empty');
  ALMove(fullCollectionName[1], result[aCurrPos], length(fullCollectionName));
  inc(aCurrPos,length(fullCollectionName));
  result[aCurrPos] := #0;
  inc(aCurrPos);

  //numberToReturn
  ALMove(numberToReturn, Result[aCurrPos], sizeof(numberToReturn));
  inc(aCurrPos,sizeof(numberToReturn));

  //cursorID
  ALMove(cursorID, Result[aCurrPos], sizeof(cursorID));

end;

{************************************************************************}
Procedure TAlBaseMongoDBClient.OP_KILL_CURSORS(aSocketDescriptor: TSocket;
                                               const cursorIDs: array of int64); // cursorIDs to be close
begin
  SendCmd(aSocketDescriptor,
          BuildOPKILLCURSORSMessage(0, // requestID
                                    0, // responseTo
                                    cursorIDs),
          False); // aGetResponse
end;

{*****************************************************************}
Procedure TAlBaseMongoDBClient.OP_QUERY(aSocketDescriptor: TSocket;
                                        const flags: integer;                    // bit vector of query options.
                                        const fullCollectionName: ansiString;    // "dbname.collectionname"
                                        const numberToSkip: integer;             // number of documents to skip
                                        const numberToReturn: integer;           // number of documents to return in the first OP_REPLY batch
                                        const query: ansiString;                 // query object.
                                        const returnFieldsSelector: ansiString;  // Optional. Selector indicating the fields to return.
                                        var responseFlags: integer;              // bit vector
                                        var cursorID: int64;                     // cursor id if client needs to do get more's
                                        var startingFrom: integer;               // where in the cursor this reply is starting
                                        var numberReturned: integer;             // number of documents in the reply
                                        documents: TALJSONNode;                  // documents
                                        OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                        ExtData: Pointer;
                                        FormatSettings: TALFormatSettings;
                                        const RowTag: AnsiString;                // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                                        const ViewTag: AnsiString;               // the node name under with all records will be stored in the JSON/XML result document.
                                        var continue: boolean);
var aRequestID: integer;
    aResponseTo: integer;
begin
  ParseOPREPLYMessage(SendCmd(aSocketDescriptor,
                              BuildOPQUERYMessage(0, //const requestID: integer;
                                                  0, //const responseTo: integer;
                                                  flags,
                                                  fullCollectionName,
                                                  numberToSkip,
                                                  numberToReturn,
                                                  query,
                                                  returnFieldsSelector),
                      True), // aGetResponse
                      arequestID,
                      aresponseTo,
                      responseFlags,
                      cursorID,
                      startingFrom,
                      numberReturned,
                      documents,
                      OnNewRowFunct,
                      ExtData,
                      FormatSettings,
                      RowTag,
                      ViewTag,
                      continue);
end;

{********************************************************************}
Procedure TAlBaseMongoDBClient.OP_GET_MORE(aSocketDescriptor: TSocket;
                                           const fullCollectionName: ansiString;   // "dbname.collectionname"
                                           const numberToReturn: integer;          // number of documents to return
                                           var cursorID: int64;                    // cursorID from the OP_REPLY
                                           var responseFlags: integer;             // bit vector
                                           var startingFrom: integer;              // where in the cursor this reply is starting
                                           var numberReturned: integer;            // number of documents in the reply
                                           documents: TALJSONNode;                 // documents
                                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                           ExtData: Pointer;
                                           FormatSettings: TALFormatSettings;
                                           const RowTag: AnsiString;               // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                                           const ViewTag: AnsiString;              // the node name under with all records will be stored in the JSON/XML result document.
                                           var continue: boolean);
var aRequestID: integer;
    aResponseTo: integer;
begin
  ParseOPREPLYMessage(SendCmd(aSocketDescriptor,
                              BuildOPGETMOREMessage(0, //const requestID: integer;
                                                    0, //const responseTo: integer;
                                                    fullCollectionName,
                                                    numberToReturn,
                                                    cursorID),
                      True), // aGetResponse
                      arequestID,
                      aresponseTo,
                      responseFlags,
                      cursorID,
                      startingFrom,
                      numberReturned,
                      documents,
                      OnNewRowFunct,
                      ExtData,
                      FormatSettings,
                      RowTag,
                      ViewTag,
                      continue);
end;

{***************************************************************************************************************************************************}
Function TAlBaseMongoDBClient.SocketWrite(aSocketDescriptor: TSocket; {$IF CompilerVersion >= 23}const{$ELSE}var{$IFEND} Buf; len: Integer): Integer;
begin
  Result := Send(aSocketDescriptor,Buf,len,0);
  CheckError(Result =  SOCKET_ERROR);
end;

{***************************************************************************************************}
function TAlBaseMongoDBClient.SocketRead(aSocketDescriptor: TSocket; var buf; len: Integer): Integer;
begin
  Result := Recv(aSocketDescriptor,buf,len,0);
  CheckError(Result = SOCKET_ERROR);
end;

{************************************************************************************************}
procedure TAlBaseMongoDBClient.DoSetSendTimeout(aSocketDescriptor: TSocket; const Value: integer);
begin
  CheckError(setsockopt(aSocketDescriptor,SOL_SOCKET,SO_SNDTIMEO,PAnsiChar(@Value),SizeOf(Value))=SOCKET_ERROR);
end;

{***************************************************************************************************}
procedure TAlBaseMongoDBClient.DoSetReceiveTimeout(aSocketDescriptor: TSocket; const Value: integer);
begin
  CheckError(setsockopt(aSocketDescriptor,SOL_SOCKET,SO_RCVTIMEO,PAnsiChar(@Value),SizeOf(Value))=SOCKET_ERROR);
end;

{***********************************************************************************************************************}
// // http://blogs.technet.com/b/nettracer/archive/2010/06/03/things-that-you-may-want-to-know-about-tcp-keepalives.aspx
procedure TAlBaseMongoDBClient.DoSetKeepAlive(aSocketDescriptor: TSocket; const Value: boolean);
var aIntBool: integer;
begin
  // warning the winsock seam buggy because the getSockOpt return optlen = 1 (byte) intead of 4 (dword)
  // so the getSockOpt work only if aIntBool = byte ! (i see this on windows vista)
  // but this is only for getSockOpt, for setsockopt it's seam to work OK so i leave it like this
  if Value then aIntBool := 1
  else aIntBool := 0;
  CheckError(setsockopt(aSocketDescriptor,SOL_SOCKET,SO_KEEPALIVE,PAnsiChar(@aIntBool),SizeOf(aIntBool))=SOCKET_ERROR);
end;

{***************************************************************************************************************************************************************************************************************}
// https://access.redhat.com/site/documentation/en-US/Red_Hat_Enterprise_MRG/1.1/html/Realtime_Tuning_Guide/sect-Realtime_Tuning_Guide-Application_Tuning_and_Deployment-TCP_NODELAY_and_Small_Buffer_Writes.html
procedure TAlBaseMongoDBClient.DoSetTCPNoDelay(aSocketDescriptor: TSocket; const Value: boolean);
var aIntBool: integer;
begin
  // warning the winsock seam buggy because the getSockOpt return optlen = 1 (byte) intead of 4 (dword)
  // so the getSockOpt work only if aIntBool = byte ! (i see this on windows vista)
  // but this is only for getSockOpt, for setsockopt it's seam to work OK so i leave it like this
  if Value then aIntBool := 1
  else aIntBool := 0;
  CheckError(setsockopt(aSocketDescriptor,SOL_SOCKET,TCP_NODELAY,PAnsiChar(@aIntBool),SizeOf(aIntBool))=SOCKET_ERROR);
end;

{******************************************************************}
procedure TAlBaseMongoDBClient.SetSendTimeout(const Value: integer);
begin
  FSendTimeout := Value;
end;

{*********************************************************************}
procedure TAlBaseMongoDBClient.SetReceiveTimeout(const Value: integer);
begin
  FReceiveTimeout := Value;
end;

{****************************************************************}
procedure TAlBaseMongoDBClient.SetKeepAlive(const Value: boolean);
begin
  FKeepAlive := Value;
end;

{*****************************************************************}
procedure TAlBaseMongoDBClient.SetTCPNoDelay(const Value: boolean);
begin
  fTCPNoDelay := Value;
end;

{**************************************************************}
procedure TAlMongoDBClient.SetSendTimeout(const Value: integer);
begin
  inherited SetSendTimeout(Value);
  if FConnected then DoSetSendTimeout(fSocketDescriptor, Value);
end;

{*****************************************************************}
procedure TAlMongoDBClient.SetReceiveTimeout(const Value: integer);
begin
  inherited SetReceiveTimeout(Value);
  if FConnected then DoSetReceiveTimeout(fSocketDescriptor, Value);
end;

{************************************************************}
procedure TAlMongoDBClient.SetKeepAlive(const Value: boolean);
begin
  inherited SetKeepAlive(Value);
  if FConnected then DoSetKeepAlive(fSocketDescriptor, Value);
end;

{*************************************************************}
procedure TAlMongoDBClient.SetTCPNoDelay(const Value: boolean);
begin
  inherited SetTCPNoDelay(Value);
  if FConnected then DoSetTCPNoDelay(fSocketDescriptor, Value);
end;

{**********************************}
constructor TAlMongoDBClient.Create;
begin
  inherited;
  Fconnected:= False;
  FSocketDescriptor:= INVALID_SOCKET;
end;

{**********************************}
destructor TAlMongoDBClient.Destroy;
begin
  If Fconnected then Disconnect;
  inherited;
end;

{********************************************************************************}
procedure TAlMongoDBClient.Connect(const aHost: AnsiString; const APort: integer);
begin

  if FConnected then raise EALException.Create('MongoDB component already connected');
  DoConnect(fSocketDescriptor,
            aHost,
            APort,
            fSendTimeout,
            fReceiveTimeout,
            fKeepAlive,
            fTCPNoDelay);
  fConnected := True;

end;

{************************************}
procedure TAlMongoDBClient.Disconnect;
begin
  If Fconnected then begin
    doDisconnect(FSocketDescriptor);
    Fconnected := False;
  end;
end;

{***********************************************************************************}
Procedure TAlMongoDBClient.SelectData(Queries: TALMongoDBClientSelectDataQUERIES;
                                      JSONDATA: TALJSONNode;
                                      OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer;
                                      FormatSettings: TALFormatSettings);

Var aSQLsindex: integer;
    aViewRec: TalJSONNode;
    aJSONDocument: TalJSONDocument;
    aResponseFlags: integer;
    aCursorID: int64;
    aStartingFrom: integer;
    aNumberReturned: integer;
    aFlags: integer;
    aRecAdded: integer;
    aContinue: boolean;

begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //only OnNewRowFunct / JSONDATA can be used
  if assigned(OnNewRowFunct) then JSONDATA := nil;

  //clear the JSONDATA
  if assigned(JSONDATA) then aJSONDocument := Nil
  else begin
    aJSONDocument := TALJSONDocument.create;
    JSONDATA := aJSONDocument.Node;
  end;

  Try

    //loop on all the SQL
    For aSQLsindex := 0 to length(Queries) - 1 do begin

      //build the flag integer
      //bit num	   name	            description
      //0	         Reserved	        Must be set to 0.
      //1	         TailableCursor	  Tailable means cursor is not closed when the last data is retrieved. Rather, the cursor marks the final object’s
      //                            position. You can resume using the cursor later, from where it was located, if more data were received. Like any
      //                            “latent cursor”, the cursor may become invalid at some point (CursorNotFound) – for example if the final object
      //                            it references were deleted.
      //2	         SlaveOk	        Allow query of replica slave. Normally these return an error except for namespace “local”.
      //3	         OplogReplay	    Internal replication use only - driver should not set
      //4	         NoCursorTimeout	The server normally times out idle cursors after an inactivity period (10 minutes) to prevent excess memory use. Set this
      //                            option to prevent that.
      //5	         AwaitData	      Use with TailableCursor. If we are at the end of the data, block for a while rather than returning no data. After a
      //                            timeout period, we do return as normal.
      //6	         Exhaust	        Stream the data down full blast in multiple “more” packages, on the assumption that the client will fully read all data queried.
      //                            Faster when you are pulling a lot of data and know you want to pull it all down. Note: the client is not allowed to not read all
      //                            the data unless it closes the connection.
      //                            Caveats:
      //                            * Exhaust and limit are not compatible.
      //                            * Exhaust cursors are not supported by mongos and can not be used with a sharded cluster.
      //                            * Exhaust cursors require an exclusive socket connection to MongoDB. If the Cursor is discarded
      //                              without being completely iterated the underlying socket connection will be closed and discarded
      //                              without being returned to the connection pool.
      //                            * If you create an exhaust cursor in a request (http://api.mongodb.org/python/current/examples/requests.html),
      //                              you must completely iterate the Cursor before executing any other operation.
      //                            * The per-query network_timeout option is ignored when using an exhaust cursor.
      //                            Mostly because Exhaust is not compatible with mongos we will not use it
      //7	         Partial	        Get partial results from a mongos if some shards are down (instead of throwing an error)
      //8-31	     Reserved	        Must be set to 0.
      aFlags := 0;
      if Queries[aSQLsindex].flags.TailMonitoring and assigned(OnNewRowFunct) then begin
        aFlags := aFlags or (1 shl 1); // TailableCursor
        aFlags := aFlags or (1 shl 5); // AwaitData
      end;
      if Queries[aSQLsindex].flags.SlaveOk then aFlags := aFlags or (1 shl 2);
      if Queries[aSQLsindex].flags.NoCursorTimeout then aFlags := aFlags or (1 shl 4);
      if Queries[aSQLsindex].flags.Partial then aFlags := aFlags or (1 shl 7);

      //init the aViewRec
      if (Queries[aSQLsindex].ViewTag <> '') and (not assigned(aJSONDocument)) then aViewRec := JSONdata.AddChild(Queries[aSQLsindex].ViewTag, ntobject)
      else aViewRec := JSONdata;

      //do the First query
      aContinue := true;
      OP_QUERY(fSocketDescriptor,
               aFlags,
               Queries[aSQLsindex].fullCollectionName,
               ALIfThen(Queries[aSQLsindex].Skip >= 0, Queries[aSQLsindex].Skip, 0),
               ALIfThen(Queries[aSQLsindex].First >= 0, Queries[aSQLsindex].First, 0), // The MongoDB server returns the query results in batches. Batch size will not exceed
                                                                                       // the maximum BSON document size. For most queries, the first batch returns 101
                                                                                       // documents or just enough documents to exceed 1 megabyte. Subsequent batch size is
                                                                                       // 4 megabytes. To override the default size of the batch, see batchSize() and limit().
               Queries[aSQLsindex].query,
               Queries[aSQLsindex].ReturnFieldsSelector,
               aResponseFlags,
               aCursorID,
               aStartingFrom, // where in the cursor this reply is starting
               aNumberReturned, // number of documents in the reply
               aViewRec,
               OnNewRowFunct,
               ExtData,
               FormatSettings,
               Queries[aSQLsindex].RowTag,
               Queries[aSQLsindex].ViewTag,
               aContinue);

      try

        //loop still the cursorID > 0
        aRecAdded := aNumberReturned;
        while (aContinue) and
              (aCursorID <> 0) and
              ((Queries[aSQLsindex].First <= 0) or
               (aRecAdded < Queries[aSQLsindex].First)) do begin

          //Get more data
          OP_GET_MORE(fSocketDescriptor,
                      Queries[aSQLsindex].fullCollectionName,
                      ALIfThen(Queries[aSQLsindex].First > 0, Queries[aSQLsindex].First - aRecAdded, 0),
                      aCursorID,
                      aResponseFlags,
                      aStartingFrom,
                      anumberReturned,
                      aViewRec,
                      OnNewRowFunct,
                      ExtData,
                      FormatSettings,
                      Queries[aSQLsindex].RowTag,
                      Queries[aSQLsindex].ViewTag,
                      aContinue);

          //init aRecAdded
          aRecAdded := aRecAdded + aNumberReturned;

        end;

      finally

        //close the curson
        if aCursorId <> 0 then OP_KILL_CURSORS(fSocketDescriptor, [aCursorID]);

      end;

    End;

  Finally
    if assigned(aJSONDocument) then aJSONDocument.free;
  End;

end;

{*******************************************************************************}
Procedure TAlMongoDBClient.SelectData(Query: TALMongoDBClientSelectDataQUERY;
                                      OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer;
                                      FormatSettings: TALFormatSettings);
begin

end;

{***********************************************************************}
Procedure TAlMongoDBClient.SelectData(FullCollectionName: AnsiString;
                                      Query: AnsiString;
                                      ReturnFieldsSelector: AnsiString;
                                      flags: TALMongoDBClientSelectDataQUERYFlags;
                                      OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer;
                                      FormatSettings: TALFormatSettings);
begin

end;

{***********************************************************************}
Procedure TAlMongoDBClient.SelectData(FullCollectionName: AnsiString;
                                      Query: AnsiString;
                                      ReturnFieldsSelector: AnsiString;
                                      flags: TALMongoDBClientSelectDataQUERYFlags;
                                      Skip: integer;
                                      First: Integer;
                                      OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer;
                                      FormatSettings: TALFormatSettings);
begin

end;

{********************************************************}
Procedure TAlMongoDBClient.SelectData(SQL: AnsiString;
                                      flags: TALMongoDBClientSelectDataQUERYFlags;
                                      OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer;
                                      FormatSettings: TALFormatSettings);
begin

end;

{********************************************************}
Procedure TAlMongoDBClient.SelectData(SQL: AnsiString;
                                      flags: TALMongoDBClientSelectDataQUERYFlags;
                                      Skip: integer;
                                      First: Integer;
                                      OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer;
                                      FormatSettings: TALFormatSettings);
begin

end;

{***********************************************************************************}
Procedure TAlMongoDBClient.SelectData(Queries: TALMongoDBClientSelectDataQUERIES;
                                      JSONDATA: TALJSONNode);
begin
  SelectData(Queries,
             JSONDATA,
             nil, // OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
             nil, // ExtData: Pointer
             ALDefaultFormatSettings); // we don't care it's just use for the OnNewRowFunct
end;

{*******************************************************************************}
Procedure TAlMongoDBClient.SelectData(Query: TALMongoDBClientSelectDataQUERY;
                                      JSONDATA: TALJSONNode);
var aSelectDataQueries: TALMongoDBClientSelectDataQUERIES;
begin
  setlength(aSelectDataQueries,1);
  aSelectDataQueries[0] := Query;
  SelectData(aSelectDataQueries,
             JSONDATA,
             nil, // OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
             nil, // ExtData: Pointer
             ALDefaultFormatSettings); // we don't care it's just use for the OnNewRowFunct
end;


{***********************************************************************}
Procedure TAlMongoDBClient.SelectData(FullCollectionName: AnsiString;
                                      Query: AnsiString;
                                      ReturnFieldsSelector: AnsiString;
                                      flags: TALMongoDBClientSelectDataQUERYFlags;
                                      JSONDATA: TALJSONNode);
begin

end;

{***********************************************************************}
Procedure TAlMongoDBClient.SelectData(FullCollectionName: AnsiString;
                                      Query: AnsiString;
                                      ReturnFieldsSelector: AnsiString;
                                      flags: TALMongoDBClientSelectDataQUERYFlags;
                                      RowTag: AnsiString;
                                      JSONDATA: TALJSONNode);
begin

end;

{***********************************************************************}
Procedure TAlMongoDBClient.SelectData(FullCollectionName: AnsiString;
                                      Query: AnsiString;
                                      ReturnFieldsSelector: AnsiString;
                                      flags: TALMongoDBClientSelectDataQUERYFlags;
                                      RowTag: AnsiString;
                                      Skip: integer;
                                      First: Integer;
                                      JSONDATA: TALJSONNode);
begin

end;

{********************************************************}
Procedure TAlMongoDBClient.SelectData(SQL: AnsiString;
                                          flags: TALMongoDBClientSelectDataQUERYFlags;
                                          JSONDATA: TALJSONNode);
begin

end;

{********************************************************}
Procedure TAlMongoDBClient.SelectData(SQL: AnsiString;
                                      flags: TALMongoDBClientSelectDataQUERYFlags;
                                      RowTag: AnsiString;
                                      JSONDATA: TALJSONNode);
begin

end;

{********************************************************}
Procedure TAlMongoDBClient.SelectData(SQL: AnsiString;
                                      flags: TALMongoDBClientSelectDataQUERYFlags;
                                      RowTag: AnsiString;
                                      Skip: integer;
                                      First: Integer;
                                      JSONDATA: TALJSONNode);
begin

end;

{************************************************************************************}
Procedure TAlMongoDBClient.UpdateData(Queries: TALMongoDBClientUpdateDataQUERIES);
begin

end;

{********************************************************************************}
Procedure TAlMongoDBClient.UpdateData(Query: TALMongoDBClientUpdateDataQUERY);
begin

end;

{***********************************************************************}
Procedure TAlMongoDBClient.UpdateData(FullCollectionName: AnsiString;
                                      selector: AnsiString;
                                      update: AnsiString;
                                      flags: TALMongoDBClientUpdateDataQUERYFlags);
begin

end;

{**********************************************************}
Procedure TAlMongoDBClient.UpdateData(SQLs: TALStrings);
begin

end;

{*********************************************************}
Procedure TAlMongoDBClient.UpdateData(SQL: AnsiString);
begin

end;

{*******************************************************************}
Procedure TAlMongoDBClient.UpdateData(SQLs: array of AnsiString);
begin

end;

{************************************************************************************}
Procedure TAlMongoDBClient.InsertData(Queries: TALMongoDBClientInsertDataQUERIES);
begin

end;

{********************************************************************************}
Procedure TAlMongoDBClient.InsertData(Query: TALMongoDBClientInsertDataQUERY);
begin

end;

{***********************************************************************}
Procedure TAlMongoDBClient.InsertData(FullCollectionName: AnsiString;
                                      documents: AnsiString;
                                      flags: TALMongoDBClientInsertDataQUERYFlags);
begin

end;

{************************************************************************************}
Procedure TAlMongoDBClient.DeleteData(Queries: TALMongoDBClientDeleteDataQUERIES);
begin

end;

{********************************************************************************}
Procedure TAlMongoDBClient.DeleteData(Query: TALMongoDBClientDeleteDataQUERY);
begin

end;

{***********************************************************************}
Procedure TAlMongoDBClient.DeleteData(FullCollectionName: AnsiString;
                                      selector: AnsiString;
                                      flags: TALMongoDBClientDeleteDataQUERYFlags);
begin

end;

{****************************************************************************}
procedure TAlMongoDBConnectionPoolClient.SetSendTimeout(const Value: integer);
begin
  FConnectionPoolCS.Acquire;
  Try
    inherited SetSendTimeout(Value);
  finally
    FConnectionPoolCS.Release;
  end;
  ReleaseAllConnections;
end;

{*******************************************************************************}
procedure TAlMongoDBConnectionPoolClient.SetReceiveTimeout(const Value: integer);
begin
  FConnectionPoolCS.Acquire;
  Try
    inherited SetReceiveTimeout(Value);
  finally
    FConnectionPoolCS.Release;
  end;
  ReleaseAllConnections;
end;

{**************************************************************************}
procedure TAlMongoDBConnectionPoolClient.SetKeepAlive(const Value: boolean);
begin
  FConnectionPoolCS.Acquire;
  Try
    inherited SetKeepAlive(Value);
  finally
    FConnectionPoolCS.Release;
  end;
  ReleaseAllConnections;
end;

{***************************************************************************}
procedure TAlMongoDBConnectionPoolClient.SetTCPNoDelay(const Value: boolean);
begin
  FConnectionPoolCS.Acquire;
  Try
    inherited SetTCPNoDelay(Value);
  finally
    FConnectionPoolCS.Release;
  end;
  ReleaseAllConnections;
end;

{*****************************************************************}
function TAlMongoDBConnectionPoolClient.AcquireConnection: TSocket;
Var aConnectionPoolContainer: TAlMongoDBConnectionPoolContainer;
    aTickCount: int64;
Begin

  //synchronize the code
  FConnectionPoolCS.Acquire;
  Try

    //raise an exception if currently realeasing all connection
    if FReleasingAllconnections then raise exception.Create('Can not acquire connection: currently releasing all connections');

    //delete the old unused connection
    aTickCount := ALGetTickCount64;
    if aTickCount - fLastConnectionGarbage > (60000 {every minutes})  then begin
      while FConnectionPool.Count > 0 do begin
        aConnectionPoolContainer := TAlMongoDBConnectionPoolContainer(FConnectionPool[0]);
        if aTickCount - aConnectionPoolContainer.Lastaccessdate > FConnectionMaxIdleTime then begin
          Try
            DoDisconnect(aConnectionPoolContainer.SocketDescriptor);
          Except
            //Disconnect must be a "safe" procedure because it's mostly called in
            //finalization part of the code that it is not protected
          End;
          FConnectionPool.Delete(0); // must be delete here because FConnectionPool free the object also
        end
        else break;
      end;
      FLastConnectionGarbage := aTickCount;
    end;

    //acquire the new connection from the pool
    If FConnectionPool.Count > 0 then begin
      aConnectionPoolContainer := TAlMongoDBConnectionPoolContainer(FConnectionPool[FConnectionPool.count - 1]);
      Result := aConnectionPoolContainer.SocketDescriptor;
      FConnectionPool.Delete(FConnectionPool.count - 1);
    end

    //create a new connection
    else begin
      Doconnect(result,
                fHost,//aHost,
                fPort,//APort,
                fSendTimeout,
                fReceiveTimeout,
                fKeepAlive,
                fTCPNoDelay);
    end;

    //increase the connection count
    inc(FWorkingConnectionCount);

  //get out of the synchronization
  finally
    FConnectionPoolCS.Release;
  end;

End;

{***************************************************************************************}
procedure TAlMongoDBConnectionPoolClient.ReleaseConnection(var SocketDescriptor: TSocket;
                                                           const CloseConnection: Boolean = False);
Var aConnectionPoolContainer: TAlMongoDBConnectionPoolContainer;
begin

  //security check
  if SocketDescriptor = INVALID_SOCKET then raise exception.Create('Connection handle can not be INVALID_SOCKET');

  //release the connection
  FConnectionPoolCS.Acquire;
  Try

    //add the connection to the pool
    If (not CloseConnection) and (not FReleasingAllconnections) then begin
      aConnectionPoolContainer := TAlMongoDBConnectionPoolContainer.Create;
      aConnectionPoolContainer.SocketDescriptor := SocketDescriptor;
      aConnectionPoolContainer.LastAccessDate := ALGetTickCount64;
      FConnectionPool.add(aConnectionPoolContainer);
    end

    //close the connection
    else begin
      try
        doDisconnect(SocketDescriptor);
      Except
        //Disconnect must be a "safe" procedure because it's mostly called in
        //finalization part of the code that it is not protected
      end;
    end;

    //set the connectionhandle to nil
    SocketDescriptor := INVALID_SOCKET;

    //dec the WorkingConnectionCount
    Dec(FWorkingConnectionCount);

  finally
    FConnectionPoolCS.Release;
  end;

end;

{***********************************************************************************************}
constructor TAlMongoDBConnectionPoolClient.Create(const aHost: AnsiString; const APort: integer);
begin
  inherited create;
  fHost:= aHost;
  fPort:= APort;
  FConnectionPool:= TObjectList.Create(True);
  FConnectionPoolCS:= TCriticalSection.create;
  FWorkingConnectionCount:= 0;
  FReleasingAllconnections := False;
  FLastConnectionGarbage := ALGettickCount64;
  FConnectionMaxIdleTime := 1200000; // 1000 * 60 * 20 = 20 min
end;

{************************************************}
destructor TAlMongoDBConnectionPoolClient.Destroy;
begin
  ReleaseAllConnections;
  FConnectionPool.free;
  FConnectionPoolCS.free;
  inherited;
end;

{***********************************************************************************************************}
procedure TAlMongoDBConnectionPoolClient.ReleaseAllConnections(Const WaitWorkingConnections: Boolean = True);
Var aConnectionPoolContainer: TAlMongoDBConnectionPoolContainer;
begin

  {we do this to forbid any new thread to create a new transaction}
  FReleasingAllconnections := True;
  Try

    //wait that all transaction are finished
    if WaitWorkingConnections then
      while true do begin
        FConnectionPoolCS.Acquire;
        Try
          if FWorkingConnectionCount <= 0 then break;
        finally
          FConnectionPoolCS.Release;
        end;
        sleep(1);
      end;

    {free all database}
    FConnectionPoolCS.Acquire;
    Try
      while FConnectionPool.Count > 0 do begin
        aConnectionPoolContainer := TAlMongoDBConnectionPoolContainer(FConnectionPool[FConnectionPool.count - 1]);
        Try
          DoDisconnect(aConnectionPoolContainer.SocketDescriptor);
        Except
          //Disconnect must be a "safe" procedure because it's mostly called in
          //finalization part of the code that it is not protected
        End;
        FConnectionPool.Delete(FConnectionPool.count - 1); // must be delete here because FConnectionPool free the object also
      end;
      FLastConnectionGarbage := ALGetTickCount64;
    finally
      FConnectionPoolCS.Release;
    end;

  finally
    //Do not forbid anymore new thread to create a new transaction
    FReleasingAllconnections := False;
  End;

end;

end.
