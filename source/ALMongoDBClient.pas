(*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    Stéphane Vander Clock (alcinoe@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALMongoDBClient
Version:      4.00

Description:  Delphi Client for MongoDB database.
              A Delphi driver (with connection pool) to access a
              mongoDB server. a connection pool is a cache of database
              connections maintained so that the connections can be reused
              when future requests to the database are required.
              In connection pooling, after a connection is created,
              it is placed in the pool and it is used over again so that a
              new connection does not have to be established. If all the
              connections are being used, a new connection is made and is
              added to the pool. Connection pooling also cuts down on the
              amount of time a user must wait to establish a connection
              to the database.


              ---------
              Exemple :

              aJSONDoc := TALJSONDocument.create;
              aMongoDBClient := TAlMongoDBClient.create;
              try
                aMongoDBClient.Connect('', 0);
                aMongoDBClient.SelectData('test.exemple',
                                          '{fieldA:123}', // the query
                                          '{fieldA:1, fieldB:1}', // the return fields selector
                                          aJSONDoc.node);
                aMongoDBClient.disconnect;
                for i := 0 to aJSONDoc.node.childnodes.count - 1 do
                  with aJSONDoc.node.childnodes[i] do
                    writeln(aJSONDoc.node.childnodes[i].nodename + '=' + aJSONDoc.node.childnodes[i].text)
              finally
                aMongoDBClient.free;
                aJSONDoc.free;
              end;


              ------------------------------
              Exemple with connection pool :

              aMongoDBConnectionPoolClient := TAlMongoDBConnectionPoolClient.create(aDBHost, aDBPort);
              try

                ::Thread1::
                aMongoDBConnectionPoolClient.SelectData('test.exemple',
                                                        '{fieldA:123}', // the query
                                                        '{fieldA:1, fieldB:1}', // the return fields selector
                                                        aLocalVarJSONDOC.node);

                ::Thread2::
                aMongoDBConnectionPoolClient.SelectData('test.exemple',
                                                        '{fieldA:999}', // the query
                                                        '{fieldA:1, fieldB:1}', // the return fields selector
                                                        aLocalVarJSONDOC.node);

              finally
                aMongoDBClient.free;
              end;


              -------------------------
              Exemple tail monitoring :

              aMongoDBTailMonitoringThread := TAlMongoDBTailMonitoringThread.Create(aDBHost,
                                                                                    aDBPort,
                                                                                    'test.cappedCollectionExemple'
                                                                                    '{}', // the query
                                                                                    '{fieldA:1, fieldB:1}', // the return fields selector

                                                                                    Procedure (Sender: TObject; JSONRowData: TALJSONNode)
                                                                                    begin
                                                                                      writeln('New item added in cappedCollectionExemple: ' + JSONRowData.childnodes['fieldA'].text);
                                                                                    end,

                                                                                    procedure (Sender: TObject; Error: Exception)
                                                                                    begin
                                                                                      writeln(Error.message);
                                                                                    end);
              ....
              aMongoDBTailMonitoringThread.free;



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
**************************************************************)
unit ALMongoDBClient;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     winapi.WinSock2,
     system.Contnrs,
     system.Classes,
     System.SyncObjs,
     System.SysUtils,
     {$ELSE}
     WinSock,
     Contnrs,
     Classes,
     SyncObjs,
     SysUtils,
     {$IFEND}
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

    {--------------------------------------}
    {$IF CompilerVersion >= 23} {Delphi XE2}
    TALMongoDBClientSelectDataOnNewRowFunct = reference to Procedure(JSONRowData: TALJSONNode;
                                                                     ViewTag: AnsiString;
                                                                     ExtData: Pointer;
                                                                     Var Continue: Boolean);
    {$ELSE}
    TALMongoDBClientSelectDataOnNewRowFunct = Procedure(JSONRowData: TALJSONNode;
                                                        ViewTag: AnsiString;
                                                        ExtData: Pointer;
                                                        Var Continue: Boolean);
    {$IFEND}

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
      FErrorCode: Integer;
      FCloseConnection: Boolean;
    public
      constructor Create(const aMsg: AnsiString; const aErrorCode: integer; const aCloseConnection: Boolean = False);
      property CloseConnection: Boolean read FCloseConnection write FCloseConnection;
      property ErrorCode: Integer read FErrorCode write FErrorCode;
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
      Procedure CheckServerLastError(aSocketDescriptor: TSocket;
                                     var NumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed, if the preceding operation was an update or remove operation.
                                     var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                     var upserted: TALJSONObjectID); overload; // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
      Procedure CheckServerLastError(aSocketDescriptor: TSocket); overload;
      Function BuildOPKILLCURSORSMessage(const requestID: integer;                       // identifier for this message
                                         const responseTo: integer;                      // requestID from the original request (used in reponses from db)
                                         const cursorIDs: array of int64): ansiString;   // cursorIDs to be close
      Function BuildOPINSERTMessage(const requestID: integer;                            // identifier for this message
                                    const responseTo: integer;                           // requestID from the original request (used in reponses from db)
                                    const flags: integer;                                // bit vector
                                    const fullCollectionName: ansiString;                // "dbname.collectionname"
                                    const documents: ansiString): ansiString;            // one or more documents to insert into the collection
      Function BuildOPUPDATEMessage(const requestID: integer;                            // identifier for this message
                                    const responseTo: integer;                           // requestID from the original request (used in reponses from db)
                                    const flags: integer;                                // bit vector
                                    const fullCollectionName: ansiString;                // "dbname.collectionname"
                                    const selector: ansiString;                          // the query to select the document
                                    const update: ansiString): ansiString;               // specification of the update to perform
      Function BuildOPDELETEMessage(const requestID: integer;                            // identifier for this message
                                    const responseTo: integer;                           // requestID from the original request (used in reponses from db)
                                    const flags: integer;                                // bit vector
                                    const fullCollectionName: ansiString;                // "dbname.collectionname"
                                    const selector: ansiString): ansiString;             // query object.
      Function BuildOPQUERYMessage(const requestID: integer;                             // identifier for this message
                                   const responseTo: integer;                            // requestID from the original request (used in reponses from db)
                                   const flags: integer;                                 // bit vector of query options.
                                   const fullCollectionName: ansiString;                 // "dbname.collectionname"
                                   const numberToSkip: integer;                          // number of documents to skip
                                   const numberToReturn: integer;                        // number of documents to return in the first OP_REPLY batch
                                   const query: ansiString;                              // query object
                                   const returnFieldsSelector: ansiString): AnsiString;  // Optional. Selector indicating the fields to return
      Function BuildOPGETMOREMessage(const requestID: integer;              // identifier for this message
                                     const responseTo: integer;             // requestID from the original request (used in reponses from db)
                                     const fullCollectionName: ansiString;  // "dbname.collectionname"
                                     const numberToReturn: integer;         // number of documents to return in the first OP_REPLY batch
                                     const cursorID: int64): AnsiString;    // cursorID from the OP_REPLY
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
                                    const RowTag: AnsiString;      // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                                    const ViewTag: AnsiString;     // the node name under with all records will be stored in the JSON/XML result document.
                                    var continue: boolean);
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
                            const RowTag: AnsiString;               // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                            const ViewTag: AnsiString;              // the node name under with all records will be stored in the JSON/XML result document.
                            var continue: boolean);
      Procedure OP_INSERT(aSocketDescriptor: TSocket;
                          const flags: integer;                  // bit vector
                          const fullCollectionName: ansiString;  // "dbname.collectionname"
                          const documents: ansiString);          // one or more documents to insert into the collection
      Procedure OP_UPDATE(aSocketDescriptor: TSocket;
                          const flags: integer;                   // bit vector
                          const fullCollectionName: ansiString;   // "dbname.collectionname"
                          const selector: ansiString;             // the query to select the document
                          const update: ansiString;               // specification of the update to perform
                          var NumberOfDocumentsUpdated: integer;  // reports the number of documents updated or removed, if the preceding operation was an update or remove operation.
                          var updatedExisting: boolean;           // is true when an update affects at least one document and does not result in an upsert.
                          var upserted: TALJSONObjectID);         // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
      Procedure OP_DELETE(aSocketDescriptor: TSocket;
                          const flags: integer;                   // bit vector
                          const fullCollectionName: ansiString;   // "dbname.collectionname"
                          const selector: ansiString;             // query object.
                          var NumberOfDocumentsRemoved: integer); // reports the number of documents updated or removed, if the preceding operation was an update or remove operation.
      procedure OnSelectDataDone(Query: TALMongoDBClientSelectDataQUERY;
                                 TimeTaken: Integer); virtual;
      procedure OnUpdateDataDone(Query: TALMongoDBClientUpdateDataQUERY;
                                 TimeTaken: Integer); virtual;
      procedure OnDeleteDataDone(Query: TALMongoDBClientDeleteDataQUERY;
                                 TimeTaken: Integer); virtual;
      procedure OnInsertDataDone(Query: TALMongoDBClientInsertDataQUERY;
                                 TimeTaken: Integer); virtual;
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
      fStopTailMonitoring: Boolean;
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
                           ExtData: Pointer); overload; virtual;
      Procedure SelectData(Query: TALMongoDBClientSelectDataQUERY;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer); overload; virtual;
      Procedure SelectData(FullCollectionName: AnsiString;
                           Query: AnsiString;
                           ReturnFieldsSelector: AnsiString;
                           Skip: integer;
                           First: Integer;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer); overload; virtual;
      Procedure SelectData(FullCollectionName: AnsiString;
                           Query: AnsiString;
                           ReturnFieldsSelector: AnsiString;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer); overload; virtual;
      Procedure SelectData(Queries: TALMongoDBClientSelectDataQUERIES;
                           JSONDATA: TALJSONNode); overload; virtual;
      Procedure SelectData(Query: TALMongoDBClientSelectDataQUERY;
                           JSONDATA: TALJSONNode); overload; virtual;
      Procedure SelectData(FullCollectionName: AnsiString;
                           Query: AnsiString;
                           ReturnFieldsSelector: AnsiString;
                           RowTag: AnsiString;
                           Skip: integer;
                           First: Integer;
                           JSONDATA: TALJSONNode); overload; virtual;
      Procedure SelectData(FullCollectionName: AnsiString;
                           Query: AnsiString;
                           ReturnFieldsSelector: AnsiString;
                           RowTag: AnsiString;
                           JSONDATA: TALJSONNode); overload; virtual;
      Procedure SelectData(FullCollectionName: AnsiString;
                           Query: AnsiString;
                           ReturnFieldsSelector: AnsiString;
                           JSONDATA: TALJSONNode); overload; virtual;

      procedure UpdateData(Queries: TALMongoDBClientUpdateDataQUERIES); overload; virtual;
      procedure UpdateData(Query: TALMongoDBClientUpdateDataQUERY;
                           var NumberOfDocumentsUpdated: integer; // reports the number of documents updated
                           var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                           var ObjectID: TALJSONObjectID); overload; virtual; // an ObjectId that corresponds to the upserted document if the update resulted in an insert.
      procedure UpdateData(Query: TALMongoDBClientUpdateDataQUERY); overload; virtual;
      procedure UpdateData(FullCollectionName: AnsiString;
                           Selector: AnsiString;
                           Update: AnsiString;
                           InsertIfNotFound: Boolean;
                           MultiUpdate: Boolean;
                           var NumberOfDocumentsUpdated: integer; // reports the number of documents updated
                           var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                           var ObjectID: TALJSONObjectID); overload; virtual; // an ObjectId that corresponds to the upserted document if the update resulted in an insert.
      procedure UpdateData(FullCollectionName: AnsiString;
                           Selector: AnsiString;
                           Update: AnsiString;
                           InsertIfNotFound: Boolean;
                           MultiUpdate: Boolean); overload; virtual;

      procedure InsertData(Queries: TALMongoDBClientInsertDataQUERIES); overload; virtual;
      procedure InsertData(Query: TALMongoDBClientInsertDataQUERY); overload; virtual;
      procedure InsertData(FullCollectionName: AnsiString;
                           Documents: AnsiString;
                           ContinueOnError: Boolean); overload; virtual;

      procedure DeleteData(Queries: TALMongoDBClientDeleteDataQUERIES); overload; virtual;
      procedure DeleteData(Query: TALMongoDBClientDeleteDataQUERY;
                           var NumberOfDocumentsRemoved: integer); overload; virtual;
      procedure DeleteData(Query: TALMongoDBClientDeleteDataQUERY); overload; virtual;
      procedure DeleteData(FullCollectionName: AnsiString;
                           Selector: AnsiString;
                           SingleRemove: Boolean;
                           var NumberOfDocumentsRemoved: integer); overload; virtual;
      procedure DeleteData(FullCollectionName: AnsiString;
                           Selector: AnsiString;
                           SingleRemove: Boolean); overload; virtual;

      property Connected: Boolean read FConnected;
      property StopTailMonitoring: boolean read fStopTailMonitoring write fStopTailMonitoring;
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

      Procedure SelectData(Queries: TALMongoDBClientSelectDataQUERIES;
                           JSONDATA: TALJSONNode;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(Query: TALMongoDBClientSelectDataQUERY;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(FullCollectionName: AnsiString;
                           Query: AnsiString;
                           ReturnFieldsSelector: AnsiString;
                           Skip: integer;
                           First: Integer;
                           NoCursorTimeout: Boolean;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(FullCollectionName: AnsiString;
                           Query: AnsiString;
                           ReturnFieldsSelector: AnsiString;
                           NoCursorTimeout: Boolean;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(Queries: TALMongoDBClientSelectDataQUERIES;
                           JSONDATA: TALJSONNode;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(Query: TALMongoDBClientSelectDataQUERY;
                           JSONDATA: TALJSONNode;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(FullCollectionName: AnsiString;
                           Query: AnsiString;
                           ReturnFieldsSelector: AnsiString;
                           RowTag: AnsiString;
                           Skip: integer;
                           First: Integer;
                           JSONDATA: TALJSONNode;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(FullCollectionName: AnsiString;
                           Query: AnsiString;
                           ReturnFieldsSelector: AnsiString;
                           RowTag: AnsiString;
                           JSONDATA: TALJSONNode;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(FullCollectionName: AnsiString;
                           Query: AnsiString;
                           ReturnFieldsSelector: AnsiString;
                           JSONDATA: TALJSONNode;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;

      procedure UpdateData(Queries: TALMongoDBClientUpdateDataQUERIES;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      procedure UpdateData(Query: TALMongoDBClientUpdateDataQUERY;
                           var NumberOfDocumentsUpdated: integer; // reports the number of documents updated
                           var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                           var ObjectID: TALJSONObjectID; // an ObjectId that corresponds to the upserted document if the update resulted in an insert.
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      procedure UpdateData(Query: TALMongoDBClientUpdateDataQUERY;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      procedure UpdateData(FullCollectionName: AnsiString;
                           Selector: AnsiString;
                           Update: AnsiString;
                           InsertIfNotFound: Boolean;
                           MultiUpdate: Boolean;
                           var NumberOfDocumentsUpdated: integer; // reports the number of documents updated
                           var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                           var ObjectID: TALJSONObjectID; // an ObjectId that corresponds to the upserted document if the update resulted in an insert.
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      procedure UpdateData(FullCollectionName: AnsiString;
                           Selector: AnsiString;
                           Update: AnsiString;
                           InsertIfNotFound: Boolean;
                           MultiUpdate: Boolean;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;

      procedure InsertData(Queries: TALMongoDBClientInsertDataQUERIES;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      procedure InsertData(Query: TALMongoDBClientInsertDataQUERY;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      procedure InsertData(FullCollectionName: AnsiString;
                           Documents: AnsiString;
                           ContinueOnError: Boolean;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;

      procedure DeleteData(Queries: TALMongoDBClientDeleteDataQUERIES;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      procedure DeleteData(Query: TALMongoDBClientDeleteDataQUERY;
                           var NumberOfDocumentsRemoved: integer;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      procedure DeleteData(Query: TALMongoDBClientDeleteDataQUERY;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      procedure DeleteData(FullCollectionName: AnsiString;
                           Selector: AnsiString;
                           SingleRemove: Boolean;
                           var NumberOfDocumentsRemoved: integer;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      procedure DeleteData(FullCollectionName: AnsiString;
                           Selector: AnsiString;
                           SingleRemove: Boolean;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;

      property Host: ansiString read fHost;
      property Port: integer read fPort;
    end;

    {$IF CompilerVersion >= 23} {Delphi XE2}
    TAlMongoDBTailMonitoringThreadEvent = reference to Procedure (Sender: TObject; JSONRowData: TALJSONNode);
    TAlMongoDBTailMonitoringThreadException = reference to procedure (Sender: TObject; Error: Exception);
    {$ELSE}
    TAlMongoDBTailMonitoringThreadEvent = Procedure (Sender: TObject; JSONRowData: TALJSONNode) of object;
    TAlMongoDBTailMonitoringThreadException = procedure (Sender: TObject; Error: Exception) of object;
    {$IFEND}

    {---------------------------------------------}
    TAlMongoDBTailMonitoringThread = class(TThread)
    private
      fMongoDBClient: TALMongoDBClient;
      FHost: AnsiString;
      fPort: integer;
      fFullCollectionName: AnsiString;
      fQuery: AnsiString;
      fReturnFieldsSelector: AnsiString;
      FStarted: Boolean;
      FCompleted: Boolean;
      fOnEvent: TAlMongoDBTailMonitoringThreadEvent;
      fOnException: TAlMongoDBTailMonitoringThreadException;
    protected
      Procedure DoEvent(JSONRowData: TALJSONNode); virtual;
      procedure DoException(Error: Exception); virtual;
    public
      constructor Create(const aHost: AnsiString;
                         const aPort: integer;
                         const aFullCollectionName: AnsiString;
                         const aQuery: AnsiString;
                         const aReturnFieldsSelector: AnsiString;
                         aOnEvent: TAlMongoDBTailMonitoringThreadEvent;
                         aOnError: TAlMongoDBTailMonitoringThreadException); virtual;
      procedure AfterConstruction; override;
      Destructor Destroy; override;
      procedure Execute; override;
      property OnEvent: TAlMongoDBTailMonitoringThreadEvent read fOnEvent write fOnEvent;
      property OnException: TAlMongoDBTailMonitoringThreadException read fOnException write fOnException;
    end;

implementation

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     Winapi.Windows,
     System.Diagnostics,
     {$ELSE}
     Windows,
     Diagnostics,
     {$IFEND}
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
    MultiUpdate := False;
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

{***************************************************************************************************************************************}
constructor EAlMongoDBClientException.Create(const aMsg: AnsiString; const aErrorCode: integer; const aCloseConnection: Boolean = False);
begin
  fErrorCode := aErrorCode;
  fCloseConnection := aCloseConnection;
  inherited create(aMsg);
end;

{**************************************}
constructor TAlBaseMongoDBClient.Create;
var aWSAData: TWSAData;
begin
  CheckError(WSAStartup(MAKEWORD(2,2), aWSAData) <> 0);
  FSendTimeout := 60000; // 60 seconds
  FReceiveTimeout := 60000; // 60 seconds
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
    CheckError(Winapi.WinSock2.Connect(aSocketDescriptor,TSockAddr(SockAddr),SizeOf(SockAddr))=SOCKET_ERROR);
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

{*****************************************************************************}
Procedure TAlBaseMongoDBClient.CheckServerLastError(aSocketDescriptor: TSocket;
                                                    var NumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed, if the preceding operation was an update or remove operation.
                                                    var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                                    var upserted: TALJSONObjectID); // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.

Var aResponseFlags: integer;
    aCursorID: int64;
    aStartingFrom: integer;
    aNumberReturned: integer;
    aContinue: boolean;
    aJSONDoc: TALJSONDocument;
    aNode: TALJSONNode;

begin
  aJSONDoc := TALJSONDocument.Create;
  try

    //do the First query
    OP_QUERY(aSocketDescriptor,
             0, // flags
             'db.$cmd',
             0, // skip
             1, // First
             '{getLastError:1}', // query
             '', // ReturnFieldsSelector,
             aResponseFlags,
             aCursorID,
             aStartingFrom,
             aNumberReturned,
             aJSONDoc.Node,
             nil,
             nil,
             '',
             '',
             aContinue);
    try

      aJSONDoc.Options := [doNodeAutoCreate];

      if aJSONDoc.Node.ChildNodes['ok'].Text <> '1' then raise Exception.Create('Error calling getLastError');

      aNode := aJSONDoc.Node.ChildNodes.FindNode('err');
      if assigned(aNode) and (not anode.Null) then raise EAlMongoDBClientException.Create(aNode.Text,
                                                                                          ALIfThen(ALIsInteger(aJSONDoc.Node.ChildNodes['code'].Text),
                                                                                                   ALStrToInt(aJSONDoc.Node.ChildNodes['code'].Text),
                                                                                                   0));

      aNode := aJSONDoc.Node.ChildNodes.FindNode('code');
      if assigned(aNode) then raise EAlMongoDBClientException.Create('Misc Error',aNode.int32);

      aNode := aJSONDoc.Node.ChildNodes.FindNode('n');
      if assigned(aNode) then NumberOfDocumentsUpdatedOrRemoved := aNode.int32
      else NumberOfDocumentsUpdatedOrRemoved := 0;


      aNode := aJSONDoc.Node.ChildNodes.FindNode('updatedExisting');
      if assigned(aNode) then updatedExisting := aNode.bool
      else updatedExisting := False;

      aNode := aJSONDoc.Node.ChildNodes.FindNode('upserted');
      if assigned(aNode) then upserted := aNode.ObjectID
      else fillchar(upserted[1],length(upserted),0);

    finally

      //close the curson
      if aCursorId <> 0 then OP_KILL_CURSORS(aSocketDescriptor, [aCursorID]);

    end;

  finally
    aJSONDoc.free;
  end;
end;

{******************************************************************************}
Procedure TAlBaseMongoDBClient.CheckServerLastError(aSocketDescriptor: TSocket);
var aNumberOfDocumentsUpdatedOrRemoved: integer;
    updatedExisting: boolean;
    upserted: TALJSONObjectID;
begin
  CheckServerLastError(aSocketDescriptor,
                       aNumberOfDocumentsUpdatedOrRemoved,
                       updatedExisting,
                       upserted);
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

{**************************************************************************}
Function TAlBaseMongoDBClient.BuildOPINSERTMessage(const requestID: integer;                            // identifier for this message
                                                   const responseTo: integer;                           // requestID from the original request (used in reponses from db)
                                                   const flags: integer;                                // bit vector
                                                   const fullCollectionName: ansiString;                // "dbname.collectionname"
                                                   const documents: ansiString): ansiString;            // one or more documents to insert into the collection
var aCurrPos: integer;
    aMessageLength: Integer;
    aTmpInt: integer;
    aOPCode: integer;
    aBsonDocuments: ansiString;
    aJsonDocument: TALJsonDocument;
    i: integer;
begin

  //
  //  messageLength     requestID       responseTo        opCode                 flags
  //  [0][1][2][3]     [4][5][6][7]    [8][9][10][11]   [12][13][14][15]    [16][17][18][19]
  //
  //  fullCollectionName       documents
  //  [20]...[23]            [24]...
  //

  //init aBsonQuery
  if (length(documents) > sizeof(aMessageLength)) then ALMove(documents[1], aMessageLength, sizeof(aMessageLength))
  else aMessageLength := 0;
  while (aMessageLength > 0) and
        (aMessageLength <= length(documents) - sizeof(aTmpInt)) do begin
    ALMove(documents[aMessageLength+1], aTmpInt, sizeof(aTmpInt));
    if aTmpInt <= 0 then break;
    aMessageLength := aMessageLength + aTmpInt;
  end;
  if (aMessageLength <> length(documents)) or
     ((length(documents) > 0) and
      (documents[length(documents)] <> #0)) then begin
    aJsonDocument := TALJsonDocument.Create;
    try
      aJsonDocument.LoadFromJSON('{"documents":' + documents + '}'); // { .... }                      => {"documents":{ .... } }
                                                                     // [{ ... }, { ... }, { ... }]   => {"documents":[{ ... }, { ... }, { ... }] }
      aBsonDocuments := '';
      with aJsonDocument.Node.ChildNodes['documents'] do begin
        if nodeType = ntObject then aBsonDocuments := BSON  // {"documents":{ .... } }
        else begin  // {"documents":[{ ... }, { ... }, { ... }] }
          for I := 0 to ChildNodes.Count - 1 do
            aBsonDocuments := aBsonDocuments + ChildNodes[i].BSON;
        end;
      end;
    finally
      aJsonDocument.Free;
    end;
  end
  else aBsonDocuments := documents;

  // init aMessageLength
  aMessageLength := sizeof(aMessageLength) +
                    sizeof(requestID) +
                    sizeof(responseTo) +
                    sizeof(aOPCode) +
                    sizeof(flags) +
                    length(fullCollectionName) + 1{the trailing #0} +
                    length(aBsonDocuments);

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
  aOPCode := MONGO_OP_INSERT;
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

  //aBsonDocuments
  if length(aBsonDocuments) <= 0 then raise Exception.Create('documents must not be empty');
  ALMove(aBsonDocuments[1], result[aCurrPos], length(aBsonDocuments));

end;

{**************************************************************************}
Function TAlBaseMongoDBClient.BuildOPUPDATEMessage(const requestID: integer;                            // identifier for this message
                                                   const responseTo: integer;                           // requestID from the original request (used in reponses from db)
                                                   const flags: integer;                                // bit vector
                                                   const fullCollectionName: ansiString;                // "dbname.collectionname"
                                                   const selector: ansiString;                          // the query to select the document
                                                   const update: ansiString): ansiString;               // specification of the update to perform
var aCurrPos: integer;
    aMessageLength: Integer;
    aOPCode: integer;
    aZERO: integer;
    aBsonselector: ansiString;
    aBsonupdate: AnsiString;
    aJSONDocument: TalJSONDocument;
begin

  //
  //  messageLength     requestID       responseTo        opCode                 ZERO
  //  [0][1][2][3]     [4][5][6][7]    [8][9][10][11]   [12][13][14][15]    [16][17][18][19]
  //
  //  fullCollectionName       flags              selector       update
  //  [20]...[23]            [24][25][26][27]   [27]...[35]    [36]...[39]
  //

  //init aBsonselector
  if (length(Selector) > sizeof(aMessageLength)) then ALMove(Selector[1], aMessageLength, sizeof(aMessageLength))
  else aMessageLength := 0;
    if (aMessageLength <> length(Selector)) or
     ((length(Selector) > 0) and
      (Selector[length(Selector)] <> #0)) then begin
    aJsonDocument := TALJsonDocument.Create;
    try
      aJsonDocument.LoadFromJSON(Selector);
      aJsonDocument.SaveToBSON(aBsonSelector);
    finally
      aJsonDocument.Free;
    end;
  end
  else begin
    if Selector = '' then aBsonSelector := #5#0#0#0#0 // empty BSON
    else aBsonSelector := Selector;
  end;

  //init aBSONUpdate
  if (length(Update) > sizeof(aMessageLength)) then ALMove(Update[1], aMessageLength, sizeof(aMessageLength))
  else aMessageLength := 0;
  if (aMessageLength <> length(Update)) or
     ((length(Update) > 0) and
      (Update[length(Update)] <> #0)) then begin
    aJsonDocument := TALJsonDocument.Create;
    try
      aJsonDocument.LoadFromJSON(Update);
      aJsonDocument.SaveToBSON(aBsonUpdate);
    finally
      aJsonDocument.Free;
    end;
  end
  else begin
    if Update = '' then aBsonUpdate := #5#0#0#0#0 // empty BSON
    else aBsonUpdate := Update;
  end;

  // init aMessageLength
  aMessageLength := sizeof(aMessageLength) +
                    sizeof(requestID) +
                    sizeof(responseTo) +
                    sizeof(aOPCode) +
                    sizeof(aZERO) +
                    length(fullCollectionName) + 1{the trailing #0} +
                    sizeof(flags) +
                    length(aBsonselector) +
                    length(aBSONUpdate);

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
  aOPCode := MONGO_OP_UPDATE;
  ALMove(aOPCode, Result[aCurrPos], sizeof(aOPCode));
  inc(aCurrPos,sizeof(aOPCode));

  //ZERO
  aZERO := 0;
  ALMove(aZERO, Result[aCurrPos], sizeof(aZERO));
  inc(aCurrPos,sizeof(aZERO));

  //fullCollectionName
  if length(fullCollectionName) <= 0 then raise Exception.Create('FullCollectionName must not be empty');
  ALMove(fullCollectionName[1], result[aCurrPos], length(fullCollectionName));
  inc(aCurrPos,length(fullCollectionName));
  result[aCurrPos] := #0;
  inc(aCurrPos);

  //flags
  ALMove(flags, Result[aCurrPos], sizeof(flags));
  inc(aCurrPos,sizeof(flags));

  //aBsonselector
  ALMove(aBsonselector[1], result[aCurrPos], length(aBsonselector));
  inc(aCurrPos,length(aBsonselector));

  //aBSONUpdate
  ALMove(aBSONUpdate[1], result[aCurrPos], length(aBSONUpdate));

end;

{**************************************************************************}
Function TAlBaseMongoDBClient.BuildOPDELETEMessage(const requestID: integer;                            // identifier for this message
                                                   const responseTo: integer;                           // requestID from the original request (used in reponses from db)
                                                   const flags: integer;                                // bit vector
                                                   const fullCollectionName: ansiString;                // "dbname.collectionname"
                                                   const selector: ansiString): ansiString;             // query object.
var aCurrPos: integer;
    aMessageLength: Integer;
    aOPCode: integer;
    aZERO: integer;
    aBsonselector: ansiString;
    aJSONDocument: TalJSONDocument;
begin

  //
  //  messageLength     requestID       responseTo        opCode                 ZERO
  //  [0][1][2][3]     [4][5][6][7]    [8][9][10][11]   [12][13][14][15]    [16][17][18][19]
  //
  //  fullCollectionName       flags              selector
  //  [20]...[23]            [24][25][26][27]   [27]...[35]
  //

  //init aBsonselector
  if (length(Selector) > sizeof(aMessageLength)) then ALMove(Selector[1], aMessageLength, sizeof(aMessageLength))
  else aMessageLength := 0;
  if (aMessageLength <> length(Selector)) or
     ((length(Selector) > 0) and
      (Selector[length(Selector)] <> #0)) then begin
    aJsonDocument := TALJsonDocument.Create;
    try
      aJsonDocument.LoadFromJSON(Selector);
      aJsonDocument.SaveToBSON(aBsonSelector);
    finally
      aJsonDocument.Free;
    end;
  end
  else begin
    if Selector = '' then aBsonSelector := #5#0#0#0#0 // empty BSON
    else aBsonSelector := Selector;
  end;

  // init aMessageLength
  aMessageLength := sizeof(aMessageLength) +
                    sizeof(requestID) +
                    sizeof(responseTo) +
                    sizeof(aOPCode) +
                    sizeof(aZERO) +
                    length(fullCollectionName) + 1{the trailing #0} +
                    sizeof(flags) +
                    length(aBsonselector);

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
  aOPCode := MONGO_OP_DELETE;
  ALMove(aOPCode, Result[aCurrPos], sizeof(aOPCode));
  inc(aCurrPos,sizeof(aOPCode));

  //ZERO
  aZERO := 0;
  ALMove(aZERO, Result[aCurrPos], sizeof(aZERO));
  inc(aCurrPos,sizeof(aZERO));

  //fullCollectionName
  if length(fullCollectionName) <= 0 then raise Exception.Create('FullCollectionName must not be empty');
  ALMove(fullCollectionName[1], result[aCurrPos], length(fullCollectionName));
  inc(aCurrPos,length(fullCollectionName));
  result[aCurrPos] := #0;
  inc(aCurrPos);

  //flags
  ALMove(flags, Result[aCurrPos], sizeof(flags));
  inc(aCurrPos,sizeof(flags));

  //aBsonselector
  ALMove(aBsonselector[1], result[aCurrPos], length(aBsonselector));

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
  if (length(Query) > sizeof(aMessageLength)) then ALMove(Query[1], aMessageLength, sizeof(aMessageLength))
  else aMessageLength := 0;
  if (aMessageLength <> length(Query)) or
     ((length(Query) > 0) and
      (Query[length(Query)] <> #0)) then begin
    aJsonDocument := TALJsonDocument.Create;
    try
      aJsonDocument.LoadFromJSON(Query);
      aJsonDocument.SaveToBSON(aBsonQuery);
    finally
      aJsonDocument.Free;
    end;
  end
  else begin
    if Query = '' then aBsonQuery := #5#0#0#0#0 // empty BSON
    else aBsonQuery := Query;
  end;

  //init aBSONReturnFieldsSelector
  if (length(ReturnFieldsSelector) > sizeof(aMessageLength)) then ALMove(ReturnFieldsSelector[1], aMessageLength, sizeof(aMessageLength))
  else aMessageLength := 0;
  if (aMessageLength <> length(ReturnFieldsSelector)) or
     ((length(ReturnFieldsSelector) > 0) and
      (ReturnFieldsSelector[length(ReturnFieldsSelector)] <> #0)) then begin
    aJsonDocument := TALJsonDocument.Create;
    try
      aJsonDocument.LoadFromJSON(ReturnFieldsSelector);
      aJsonDocument.SaveToBSON(aBsonReturnFieldsSelector);
    finally
      aJsonDocument.Free;
    end;
  end
  else aBsonReturnFieldsSelector := ReturnFieldsSelector;

  // init aMessageLength
  aMessageLength := sizeof(aMessageLength) +
                    sizeof(requestID) +
                    sizeof(responseTo) +
                    sizeof(aOPCode) +
                    sizeof(flags) +
                    length(fullCollectionName) + 1{the trailing #0} +
                    sizeof(numberToSkip) +
                    sizeof(numberToReturn) +
                    length(aBsonQuery) +
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
  ALMove(aBsonQuery[1], result[aCurrPos], length(aBsonQuery));
  inc(aCurrPos,length(aBsonQuery));

  //aBSONReturnFieldsSelector
  if length(aBSONReturnFieldsSelector) > 0 then
    ALMove(aBSONReturnFieldsSelector[1], result[aCurrPos], length(aBSONReturnFieldsSelector));

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
      if (aTmpRowTag <> '') or
         (documents.NodeType = ntarray) then aJsonNode1 := documents.AddChild(aTmpRowTag, ntobject)
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
                      RowTag,
                      ViewTag,
                      continue);
end;

{******************************************************************}
Procedure TAlBaseMongoDBClient.OP_INSERT(aSocketDescriptor: TSocket;
                                         const flags: integer;                  // bit vector
                                         const fullCollectionName: ansiString;  // "dbname.collectionname"
                                         const documents: ansiString);          // one or more documents to insert into the collection
begin
  SendCmd(aSocketDescriptor,
          BuildOPINSERTMessage(0, //const requestID: integer;
                               0, //const responseTo: integer;
                               flags,
                               fullCollectionName,
                               documents),
          false);
  CheckServerLastError(aSocketDescriptor);
end;

{******************************************************************}
Procedure TAlBaseMongoDBClient.OP_UPDATE(aSocketDescriptor: TSocket;
                                         const flags: integer;                   // bit vector
                                         const fullCollectionName: ansiString;   // "dbname.collectionname"
                                         const selector: ansiString;             // the query to select the document
                                         const update: ansiString;               // specification of the update to perform
                                         var NumberOfDocumentsUpdated: integer;  // reports the number of documents updated or removed, if the preceding operation was an update or remove operation.
                                         var updatedExisting: boolean;           // is true when an update affects at least one document and does not result in an upsert.
                                         var upserted: TALJSONObjectID);         // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
begin
  SendCmd(aSocketDescriptor,
          BuildOPUPDATEMessage(0, //const requestID: integer;
                               0, //const responseTo: integer;
                               flags,
                               fullCollectionName,
                               selector,
                               Update),
          false);
  CheckServerLastError(aSocketDescriptor,
                       NumberOfDocumentsUpdated,
                       updatedExisting,
                       upserted);
end;

{******************************************************************}
Procedure TAlBaseMongoDBClient.OP_DELETE(aSocketDescriptor: TSocket;
                                         const flags: integer;                   // bit vector
                                         const fullCollectionName: ansiString;   // "dbname.collectionname"
                                         const selector: ansiString;             // query object.
                                         var NumberOfDocumentsRemoved: integer); // reports the number of documents updated or removed, if the preceding operation was an update or remove operation.
var updatedExisting: boolean;
    upserted: TALJSONObjectID;
begin
  SendCmd(aSocketDescriptor,
          BuildOPDELETEMessage(0, //const requestID: integer;
                               0, //const responseTo: integer;
                               flags,
                               fullCollectionName,
                               selector),
          false);
  CheckServerLastError(aSocketDescriptor,
                       NumberOfDocumentsRemoved,
                       updatedExisting,
                       upserted);
end;

{*************************************************************************************}
procedure TAlBaseMongoDBClient.OnSelectDataDone(Query: TALMongoDBClientSelectDataQUERY;
                                                TimeTaken: Integer);
begin
  // virtual
end;

{*************************************************************************************}
procedure TAlBaseMongoDBClient.OnUpdateDataDone(Query: TALMongoDBClientUpdateDataQUERY;
                                                TimeTaken: Integer);
begin
  // virtual
end;

{*************************************************************************************}
procedure TAlBaseMongoDBClient.OnDeleteDataDone(Query: TALMongoDBClientDeleteDataQUERY;
                                                TimeTaken: Integer);
begin
  // virtual
end;

{*************************************************************************************}
procedure TAlBaseMongoDBClient.OnInsertDataDone(Query: TALMongoDBClientInsertDataQUERY;
                                                TimeTaken: Integer);
begin
  // virtual
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

{*******************************************************************************************************************}
// http://blogs.technet.com/b/nettracer/archive/2010/06/03/things-that-you-may-want-to-know-about-tcp-keepalives.aspx
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
  fStopTailMonitoring := False;
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

{*******************************************************************************}
Procedure TAlMongoDBClient.SelectData(Queries: TALMongoDBClientSelectDataQUERIES;
                                      JSONDATA: TALJSONNode;
                                      OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer);

Var aQueriesIndex: integer;
    aViewRec: TalJSONNode;
    aJSONDocument: TalJSONDocument;
    aResponseFlags: integer;
    aCursorID: int64;
    aStartingFrom: integer;
    aNumberReturned: integer;
    aFlags: integer;
    aRecAdded: integer;
    aContinue: boolean;
    aStopWatch: TStopWatch;

begin

  //exit if no query
  if length(Queries) = 0 then Exit;

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

    //init the TstopWatch
    aStopWatch := TstopWatch.Create;

    //loop on all the Queries
    For aQueriesIndex := 0 to length(Queries) - 1 do begin

      //start the TstopWatch
      aStopWatch.Reset;
      aStopWatch.Start;

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
      if Queries[aQueriesIndex].flags.TailMonitoring then begin
        if not assigned(OnNewRowFunct) then raise Exception.Create('OnNewRowFunct is mandatory for tail monitoring');
        aFlags := aFlags or (1 shl 1); // TailableCursor
        aFlags := aFlags or (1 shl 5); // AwaitData
      end;
      if Queries[aQueriesIndex].flags.SlaveOk then aFlags := aFlags or (1 shl 2);
      if Queries[aQueriesIndex].flags.NoCursorTimeout then aFlags := aFlags or (1 shl 4);
      if Queries[aQueriesIndex].flags.Partial then aFlags := aFlags or (1 shl 7);

      //init the aViewRec
      if (Queries[aQueriesIndex].ViewTag <> '') and (not assigned(aJSONDocument)) then aViewRec := JSONdata.AddChild(Queries[aQueriesIndex].ViewTag, ntobject)
      else aViewRec := JSONdata;

      //init aRecAdded
      aRecAdded := 0;

      //for the TailMonitoring
      repeat

        //do the First query
        aContinue := true;
        OP_QUERY(fSocketDescriptor,
                 aFlags,
                 Queries[aQueriesIndex].fullCollectionName,
                 ALIfThen(Queries[aQueriesIndex].Skip >= 0, Queries[aQueriesIndex].Skip, 0),
                 ALIfThen(Queries[aQueriesIndex].First >= 0, Queries[aQueriesIndex].First, 0), // The MongoDB server returns the query results in batches. Batch size will not exceed
                                                                                               // the maximum BSON document size. For most queries, the first batch returns 101
                                                                                               // documents or just enough documents to exceed 1 megabyte. Subsequent batch size is
                                                                                               // 4 megabytes. To override the default size of the batch, see batchSize() and limit().
                 Queries[aQueriesIndex].query,
                 Queries[aQueriesIndex].ReturnFieldsSelector,
                 aResponseFlags,
                 aCursorID,
                 aStartingFrom, // where in the cursor this reply is starting
                 aNumberReturned, // number of documents in the reply
                 aViewRec,
                 OnNewRowFunct,
                 ExtData,
                 Queries[aQueriesIndex].RowTag,
                 Queries[aQueriesIndex].ViewTag,
                 aContinue);

        try

          //init aRecAdded
          aRecAdded := aRecAdded + aNumberReturned;

          //loop still the cursorID > 0
          while ((not Queries[aQueriesIndex].flags.TailMonitoring) or
                 (not fStopTailMonitoring)) and
                (aContinue) and
                (aCursorID <> 0) and
                ((Queries[aQueriesIndex].First <= 0) or
                 (aRecAdded < Queries[aQueriesIndex].First)) do begin

            //Get more data
            OP_GET_MORE(fSocketDescriptor,
                        Queries[aQueriesIndex].fullCollectionName,
                        ALIfThen(Queries[aQueriesIndex].First > 0, Queries[aQueriesIndex].First - aRecAdded, 0),
                        aCursorID,
                        aResponseFlags,
                        aStartingFrom,
                        anumberReturned,
                        aViewRec,
                        OnNewRowFunct,
                        ExtData,
                        Queries[aQueriesIndex].RowTag,
                        Queries[aQueriesIndex].ViewTag,
                        aContinue);

            //init aRecAdded
            aRecAdded := aRecAdded + aNumberReturned;

          end;

        finally

          //close the curson
          if aCursorId <> 0 then OP_KILL_CURSORS(fSocketDescriptor, [aCursorID]);

        end;

        //sleep for TailMonitoring to not use 100% CPU
        if (Queries[aQueriesIndex].flags.TailMonitoring) and
           (not fStopTailMonitoring) and
           ((Queries[aQueriesIndex].First <= 0) or
            (aRecAdded < Queries[aQueriesIndex].First)) then sleep(1);

      //loop for the TailMonitoring
      until (not Queries[aQueriesIndex].flags.TailMonitoring) or
            (fStopTailMonitoring) or
            ((Queries[aQueriesIndex].First > 0) and
             (aRecAdded >= Queries[aQueriesIndex].First));

      //do the OnSelectDataDone
      aStopWatch.Stop;
      OnSelectDataDone(Queries[aQueriesIndex],
                       aStopWatch.ElapsedMilliseconds);

    End;

  Finally
    if assigned(aJSONDocument) then aJSONDocument.free;
  End;

end;

{***************************************************************************}
Procedure TAlMongoDBClient.SelectData(Query: TALMongoDBClientSelectDataQUERY;
                                      OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer);
var aSelectDataQueries: TALMongoDBClientSelectDataQUERIES;
begin
  setlength(aSelectDataQueries,1);
  aSelectDataQueries[0] := Query;
  SelectData(aSelectDataQueries,
             nil, // JSONDATA
             OnNewRowFunct,
             ExtData);
end;

{*******************************************************************}
Procedure TAlMongoDBClient.SelectData(FullCollectionName: AnsiString;
                                      Query: AnsiString;
                                      ReturnFieldsSelector: AnsiString;
                                      Skip: integer;
                                      First: Integer;
                                      OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer);
var aQuery: TALMongoDBClientSelectDataQUERY;
begin
  aQuery := TALMongoDBClientSelectDataQUERY.Create;
  aQuery.FullCollectionName := FullCollectionName;
  aQuery.Query := Query;
  aQuery.ReturnFieldsSelector := ReturnFieldsSelector;
  aQuery.Skip := Skip;
  aQuery.First := First;
  SelectData(aQuery,
             OnNewRowFunct,
             ExtData);
end;

{*******************************************************************}
Procedure TAlMongoDBClient.SelectData(FullCollectionName: AnsiString;
                                      Query: AnsiString;
                                      ReturnFieldsSelector: AnsiString;
                                      OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             0, // skip
             0, // first
             OnNewRowFunct,
             ExtData);
end;

{*******************************************************************************}
Procedure TAlMongoDBClient.SelectData(Queries: TALMongoDBClientSelectDataQUERIES;
                                      JSONDATA: TALJSONNode);
begin
  SelectData(Queries,
             JSONDATA,
             nil, // OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
             nil); // ExtData: Pointer
end;

{***************************************************************************}
Procedure TAlMongoDBClient.SelectData(Query: TALMongoDBClientSelectDataQUERY;
                                      JSONDATA: TALJSONNode);
var aSelectDataQueries: TALMongoDBClientSelectDataQUERIES;
begin
  setlength(aSelectDataQueries,1);
  aSelectDataQueries[0] := Query;
  SelectData(aSelectDataQueries,
             JSONDATA,
             nil, // OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
             nil); // ExtData: Pointer
end;

{*******************************************************************}
Procedure TAlMongoDBClient.SelectData(FullCollectionName: AnsiString;
                                      Query: AnsiString;
                                      ReturnFieldsSelector: AnsiString;
                                      RowTag: AnsiString;
                                      Skip: integer;
                                      First: Integer;
                                      JSONDATA: TALJSONNode);
var aQuery: TALMongoDBClientSelectDataQUERY;
begin
  aQuery := TALMongoDBClientSelectDataQUERY.Create;
  aQuery.FullCollectionName := FullCollectionName;
  aQuery.Query := Query;
  aQuery.ReturnFieldsSelector := ReturnFieldsSelector;
  aQuery.RowTag := RowTag;
  aQuery.Skip := Skip;
  aQuery.First := First;
  SelectData(aQuery,
             JSONDATA);
end;

{*******************************************************************}
Procedure TAlMongoDBClient.SelectData(FullCollectionName: AnsiString;
                                      Query: AnsiString;
                                      ReturnFieldsSelector: AnsiString;
                                      RowTag: AnsiString;
                                      JSONDATA: TALJSONNode);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             RowTag,
             0, // Skip
             0, //First
             JSONDATA);
end;

{*******************************************************************}
Procedure TAlMongoDBClient.SelectData(FullCollectionName: AnsiString;
                                      Query: AnsiString;
                                      ReturnFieldsSelector: AnsiString;
                                      JSONDATA: TALJSONNode);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             '', // RowTag,
             0, // Skip
             0, //First
             JSONDATA);
end;

{********************************************************************************}
Procedure TAlMongoDBClient.UpdateData(Queries: TALMongoDBClientUpdateDataQUERIES);
Var aQueriesIndex: integer;
    aFlags: integer;
    aNumberOfDocumentsUpdated: integer;
    aupdatedExisting: boolean;
    aObjectID: TALJSONObjectID;
    aStopWatch: TStopWatch;
begin

  //exit if no query
  if length(Queries) = 0 then Exit;

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //init the TstopWatch
  aStopWatch := TstopWatch.Create;

  //loop on all the Queries
  For aQueriesIndex := 0 to length(Queries) - 1 do begin

    //start the TstopWatch
    aStopWatch.Reset;
    aStopWatch.Start;

    //bit num	  name	        description
    //0	        Upsert	      If set, the database will insert the supplied object into the collection if no matching document is found.
    //1	        MultiUpdate	  If set, the database will update all matching objects in the collection. Otherwise only updates first matching doc.
    //2-31    	Reserved	    Must be set to 0.
    aFlags := 0;
    if Queries[aQueriesIndex].flags.Upsert then aFlags := aFlags or (1 shl 0);
    if Queries[aQueriesIndex].flags.MultiUpdate then aFlags := aFlags or (1 shl 1);

    OP_UPDATE(fSocketDescriptor,
              aflags,
              Queries[aQueriesIndex].FullCollectionName,
              Queries[aQueriesIndex].selector,
              Queries[aQueriesIndex].Update,
              aNumberOfDocumentsUpdated,
              aupdatedExisting,
              aObjectID);

    //do the OnUpdateDataDone
    aStopWatch.Stop;
    OnUpdateDataDone(Queries[aQueriesIndex],
                     aStopWatch.ElapsedMilliseconds);

  end;

end;

{***************************************************************************}
procedure TAlMongoDBClient.UpdateData(Query: TALMongoDBClientUpdateDataQUERY;
                                      var NumberOfDocumentsUpdated: integer; // reports the number of documents updated
                                      var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                      var ObjectID: TALJSONObjectID); // an ObjectId that corresponds to the upserted document if the update resulted in an insert.
Var aFlags: integer;
    aStopWatch: TStopWatch;
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //init the TstopWatch
  aStopWatch := TstopWatch.StartNew;

  //bit num	  name	        description
  //0	        Upsert	      If set, the database will insert the supplied object into the collection if no matching document is found.
  //1	        MultiUpdate	  If set, the database will update all matching objects in the collection. Otherwise only updates first matching doc.
  //2-31    	Reserved	    Must be set to 0.
  aFlags := 0;
  if Query.flags.Upsert then aFlags := aFlags or (1 shl 0);
  if Query.flags.MultiUpdate then aFlags := aFlags or (1 shl 1);

  OP_UPDATE(fSocketDescriptor,
            aflags,
            Query.FullCollectionName,
            Query.selector,
            Query.Update,
            NumberOfDocumentsUpdated,
            updatedExisting,
            ObjectID);

  //do the OnUpdateDataDone
  aStopWatch.Stop;
  OnUpdateDataDone(Query,
                   aStopWatch.ElapsedMilliseconds);

end;

{****************************************************************************}
Procedure TAlMongoDBClient.UpdateData(Query: TALMongoDBClientUpdateDataQUERY);
var aNumberOfDocumentsUpdated: integer;
    aupdatedExisting: boolean;
    aObjectID: TALJSONObjectID;
begin
  UpdateData(Query,
             aNumberOfDocumentsUpdated,
             aupdatedExisting,
             aObjectID);
end;

{*******************************************************************}
procedure TAlMongoDBClient.UpdateData(FullCollectionName: AnsiString;
                                      Selector: AnsiString;
                                      Update: AnsiString;
                                      InsertIfNotFound: Boolean;
                                      MultiUpdate: Boolean;
                                      var NumberOfDocumentsUpdated: integer; // reports the number of documents updated
                                      var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                      var ObjectID: TALJSONObjectID); // an ObjectId that corresponds to the upserted document if the update resulted in an insert.
var aQuery: TALMongoDBClientUpdateDataQUERY;
begin
  aQuery := TALMongoDBClientUpdateDataQUERY.Create;
  aQuery.FullCollectionName := FullCollectionName;
  aQuery.selector := selector;
  aQuery.update := update;
  aQuery.flags.Upsert := InsertIfNotFound;
  aQuery.flags.MultiUpdate := MultiUpdate;
  UpdateData(aQuery,
             NumberOfDocumentsUpdated,
             updatedExisting,
             ObjectID);
end;

{*******************************************************************}
Procedure TAlMongoDBClient.UpdateData(FullCollectionName: AnsiString;
                                      Selector: AnsiString;
                                      Update: AnsiString;
                                      InsertIfNotFound: Boolean;
                                      MultiUpdate: Boolean);
var aNumberOfDocumentsUpdated: integer;
    aupdatedExisting: boolean;
    aObjectID: TALJSONObjectID;
begin
  UpdateData(FullCollectionName,
             Selector,
             Update,
             InsertIfNotFound,
             MultiUpdate,
             aNumberOfDocumentsUpdated,
             aupdatedExisting,
             aObjectID);
end;

{********************************************************************************}
Procedure TAlMongoDBClient.InsertData(Queries: TALMongoDBClientInsertDataQUERIES);
Var aQueriesIndex: integer;
    aFlags: integer;
    aStopWatch: TStopWatch;
begin

  //exit if no Query
  if length(Queries) = 0 then Exit;

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //init the TstopWatch
  aStopWatch := TstopWatch.Create;

  //loop on all the queries
  For aQueriesIndex := 0 to length(Queries) - 1 do begin

    //start the TstopWatch
    aStopWatch.Reset;
    aStopWatch.Start;

    //bit num	 name	            description
    //0	       ContinueOnError	If set, the database will not stop processing a bulk insert if one fails (eg due to duplicate IDs).
    //                          This makes bulk insert behave similarly to a series of single inserts, except lastError will be set if
    //                          any insert fails, not just the last one. If multiple errors occur, only the most recent will be
    //                          reported by getLastError. (new in 1.9.1)
    //1-31	   Reserved	        Must be set to 0.
    aFlags := 0;
    if Queries[aQueriesIndex].flags.ContinueOnError then aFlags := aFlags or (1 shl 0);

    OP_INSERT(fSocketDescriptor,
              aflags,
              Queries[aQueriesIndex].FullCollectionName,
              Queries[aQueriesIndex].documents);

    //do the OnInsertDataDone
    aStopWatch.Stop;
    OnInsertDataDone(Queries[aQueriesIndex],
                     aStopWatch.ElapsedMilliseconds);

  end;

end;

{****************************************************************************}
Procedure TAlMongoDBClient.InsertData(Query: TALMongoDBClientInsertDataQUERY);
var aInsertDataQueries: TALMongoDBClientInsertDataQUERIES;
begin
  setlength(aInsertDataQueries,1);
  aInsertDataQueries[0] := Query;
  InsertData(aInsertDataQueries);
end;

{*******************************************************************}
Procedure TAlMongoDBClient.InsertData(FullCollectionName: AnsiString;
                                      Documents: AnsiString;
                                      ContinueOnError: Boolean);
var aQuery: TALMongoDBClientInsertDataQUERY;
begin
  aQuery := TALMongoDBClientInsertDataQUERY.Create;
  aQuery.FullCollectionName := FullCollectionName;
  aQuery.documents := Documents;
  aQuery.flags.ContinueOnError := ContinueOnError;
  InsertData(aQuery);
end;

{********************************************************************************}
Procedure TAlMongoDBClient.DeleteData(Queries: TALMongoDBClientDeleteDataQUERIES);
Var aQueriesIndex: integer;
    aFlags: integer;
    aNumberOfDocumentsRemoved: integer;
    aStopWatch: TStopWatch;
begin

  //exit if no query
  if length(Queries) = 0 then Exit;

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //init the TstopWatch
  aStopWatch := TstopWatch.Create;

  //loop on all the Queries
  For aQueriesIndex := 0 to length(Queries) - 1 do begin

    //start the TstopWatch
    aStopWatch.Reset;
    aStopWatch.Start;

    //bit num	   name	          description
    //0	         SingleRemove	  If set, the database will remove only the first matching document in the collection. Otherwise all matching documents will be removed.
    //1-31	     Reserved	      Must be set to 0.
    aFlags := 0;
    if Queries[aQueriesIndex].flags.SingleRemove then aFlags := aFlags or (1 shl 0);

    OP_DELETE(fSocketDescriptor,
              aflags,
              Queries[aQueriesIndex].FullCollectionName,
              Queries[aQueriesIndex].selector,
              aNumberOfDocumentsRemoved);

    //do the OnDeleteDataDone
    aStopWatch.Stop;
    OnDeleteDataDone(Queries[aQueriesIndex],
                     aStopWatch.ElapsedMilliseconds);

  end;

end;

{***************************************************************************}
Procedure TAlMongoDBClient.DeleteData(Query: TALMongoDBClientDeleteDataQUERY;
                                      var NumberOfDocumentsRemoved: integer);
Var aFlags: integer;
    aStopWatch: TStopWatch;
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //init the TstopWatch
  aStopWatch := TstopWatch.StartNew;

  //bit num	   name	          description
  //0	         SingleRemove	  If set, the database will remove only the first matching document in the collection. Otherwise all matching documents will be removed.
  //1-31	     Reserved	      Must be set to 0.
  aFlags := 0;
  if Query.flags.SingleRemove then aFlags := aFlags or (1 shl 0);

  OP_DELETE(fSocketDescriptor,
            aflags,
            Query.FullCollectionName,
            Query.selector,
            NumberOfDocumentsRemoved);

  //do the OnDeleteDataDone
  aStopWatch.Stop;
  OnDeleteDataDone(Query,
                   aStopWatch.ElapsedMilliseconds);

end;

{****************************************************************************}
Procedure TAlMongoDBClient.DeleteData(Query: TALMongoDBClientDeleteDataQUERY);
var aNumberOfDocumentsRemoved: integer;
begin
  DeleteData(Query, aNumberOfDocumentsRemoved);
end;

{*******************************************************************}
Procedure TAlMongoDBClient.DeleteData(FullCollectionName: AnsiString;
                                      Selector: AnsiString;
                                      SingleRemove: Boolean;
                                      var NumberOfDocumentsRemoved: integer);
var aQuery: TALMongoDBClientDeleteDataQUERY;
begin
  aQuery := TALMongoDBClientDeleteDataQUERY.Create;
  aQuery.FullCollectionName := FullCollectionName;
  aQuery.selector := Selector;
  aQuery.flags.SingleRemove := SingleRemove;
  DeleteData(aQuery, NumberOfDocumentsRemoved);
end;

{*******************************************************************}
Procedure TAlMongoDBClient.DeleteData(FullCollectionName: AnsiString;
                                      Selector: AnsiString;
                                      SingleRemove: Boolean);
var aNumberOfDocumentsRemoved: integer;
begin
  DeleteData(FullCollectionName,
             Selector,
             SingleRemove,
             aNumberOfDocumentsRemoved);
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

{*********************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(Queries: TALMongoDBClientSelectDataQUERIES;
                                                    JSONDATA: TALJSONNode;
                                                    OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);

Var aQueriesIndex: integer;
    aViewRec: TalJSONNode;
    aJSONDocument: TalJSONDocument;
    aResponseFlags: integer;
    aCursorID: int64;
    aStartingFrom: integer;
    aNumberReturned: integer;
    aFlags: integer;
    aRecAdded: integer;
    aContinue: boolean;
    aTMPConnectionSocket: TSocket;
    aOwnConnection: Boolean;
    aStopWatch: TStopWatch;

begin

  //exit if no query
  if length(Queries) = 0 then Exit;

  //acquire a connection
  if ConnectionSocket = INVALID_SOCKET then begin
    aTMPConnectionSocket := AcquireConnection;
    aOwnConnection := True;
  end
  else begin
    aTMPConnectionSocket := ConnectionSocket;
    aOwnConnection := False;
  end;

  try

    //only OnNewRowFunct / JSONDATA can be used
    if assigned(OnNewRowFunct) then JSONDATA := nil;

    //clear the JSONDATA
    if assigned(JSONDATA) then aJSONDocument := Nil
    else begin
      aJSONDocument := TALJSONDocument.create;
      JSONDATA := aJSONDocument.Node;
    end;

    Try

      //init the TstopWatch
      aStopWatch := TstopWatch.Create;

      //loop on all the Queries
      For aQueriesIndex := 0 to length(Queries) - 1 do begin

        //start the TstopWatch
        aStopWatch.Reset;
        aStopWatch.Start;

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
        if Queries[aQueriesIndex].flags.TailMonitoring then
          raise EAlMongoDBClientException.Create('Tail monitoring work only with TAlMongoDBClient', 0 {aErrorCode}, false {aCloseConnection});
        if Queries[aQueriesIndex].flags.SlaveOk then aFlags := aFlags or (1 shl 2);
        if Queries[aQueriesIndex].flags.NoCursorTimeout then aFlags := aFlags or (1 shl 4);
        if Queries[aQueriesIndex].flags.Partial then aFlags := aFlags or (1 shl 7);

        //init the aViewRec
        if (Queries[aQueriesIndex].ViewTag <> '') and (not assigned(aJSONDocument)) then aViewRec := JSONdata.AddChild(Queries[aQueriesIndex].ViewTag, ntobject)
        else aViewRec := JSONdata;

        //init aRecAdded
        aRecAdded := 0;

        //do the First query
        aContinue := true;
        OP_QUERY(aTMPConnectionSocket,
                 aFlags,
                 Queries[aQueriesIndex].fullCollectionName,
                 ALIfThen(Queries[aQueriesIndex].Skip >= 0, Queries[aQueriesIndex].Skip, 0),
                 ALIfThen(Queries[aQueriesIndex].First >= 0, Queries[aQueriesIndex].First, 0), // The MongoDB server returns the query results in batches. Batch size will not exceed
                                                                                         // the maximum BSON document size. For most queries, the first batch returns 101
                                                                                         // documents or just enough documents to exceed 1 megabyte. Subsequent batch size is
                                                                                         // 4 megabytes. To override the default size of the batch, see batchSize() and limit().
                 Queries[aQueriesIndex].query,
                 Queries[aQueriesIndex].ReturnFieldsSelector,
                 aResponseFlags,
                 aCursorID,
                 aStartingFrom, // where in the cursor this reply is starting
                 aNumberReturned, // number of documents in the reply
                 aViewRec,
                 OnNewRowFunct,
                 ExtData,
                 Queries[aQueriesIndex].RowTag,
                 Queries[aQueriesIndex].ViewTag,
                 aContinue);

        try

          //init aRecAdded
          aRecAdded := aRecAdded + aNumberReturned;

          //loop still the cursorID > 0
          while (aContinue) and
                (aCursorID <> 0) and
                ((Queries[aQueriesIndex].First <= 0) or
                 (aRecAdded < Queries[aQueriesIndex].First)) do begin

            //Get more data
            OP_GET_MORE(aTMPConnectionSocket,
                        Queries[aQueriesIndex].fullCollectionName,
                        ALIfThen(Queries[aQueriesIndex].First > 0, Queries[aQueriesIndex].First - aRecAdded, 0),
                        aCursorID,
                        aResponseFlags,
                        aStartingFrom,
                        anumberReturned,
                        aViewRec,
                        OnNewRowFunct,
                        ExtData,
                        Queries[aQueriesIndex].RowTag,
                        Queries[aQueriesIndex].ViewTag,
                        aContinue);

            //init aRecAdded
            aRecAdded := aRecAdded + aNumberReturned;

          end;

        finally

          //close the curson
          if aCursorId <> 0 then OP_KILL_CURSORS(aTMPConnectionSocket, [aCursorID]);

        end;

        //do the OnSelectDataDone
        aStopWatch.Stop;
        OnSelectDataDone(Queries[aQueriesIndex],
                         aStopWatch.ElapsedMilliseconds);

      End;

    Finally
      if assigned(aJSONDocument) then aJSONDocument.free;
    End;

    //Release the Connection
    if aOwnConnection then ReleaseConnection(aTMPConnectionSocket);

  except
    On E: Exception do begin
      if aOwnConnection then ReleaseConnection(aTMPConnectionSocket,
                                               (not (E Is EAlMongoDBClientException)) or
                                               (E as EAlMongoDBClientException).CloseConnection);
      raise;
    end;
  end;

end;

{*****************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(Query: TALMongoDBClientSelectDataQUERY;
                                                    OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aSelectDataQueries: TALMongoDBClientSelectDataQUERIES;
begin
  setlength(aSelectDataQueries,1);
  aSelectDataQueries[0] := Query;
  SelectData(aSelectDataQueries,
             nil, // JSONDATA
             OnNewRowFunct,
             ExtData,
             ConnectionSocket);
end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(FullCollectionName: AnsiString;
                                                    Query: AnsiString;
                                                    ReturnFieldsSelector: AnsiString;
                                                    Skip: integer;
                                                    First: Integer;
                                                    NoCursorTimeout: Boolean;
                                                    OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aQuery: TALMongoDBClientSelectDataQUERY;
begin
  aQuery := TALMongoDBClientSelectDataQUERY.Create;
  aQuery.FullCollectionName := FullCollectionName;
  aQuery.Query := Query;
  aQuery.ReturnFieldsSelector := ReturnFieldsSelector;
  aQuery.Skip := Skip;
  aQuery.First := First;
  aQuery.flags.NoCursorTimeout := NoCursorTimeout;
  SelectData(aQuery,
             OnNewRowFunct,
             ExtData,
             ConnectionSocket);
end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(FullCollectionName: AnsiString;
                                                    Query: AnsiString;
                                                    ReturnFieldsSelector: AnsiString;
                                                    NoCursorTimeout: Boolean;
                                                    OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             0, // skip
             0, // first
             NoCursorTimeout,
             OnNewRowFunct,
             ExtData,
             ConnectionSocket);
end;

{*********************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(Queries: TALMongoDBClientSelectDataQUERIES;
                                                    JSONDATA: TALJSONNode;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  SelectData(Queries,
             JSONDATA,
             nil, // OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
             nil, // ExtData: Pointer
             ConnectionSocket);
end;

{*****************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(Query: TALMongoDBClientSelectDataQUERY;
                                                    JSONDATA: TALJSONNode;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aSelectDataQueries: TALMongoDBClientSelectDataQUERIES;
begin
  setlength(aSelectDataQueries,1);
  aSelectDataQueries[0] := Query;
  SelectData(aSelectDataQueries,
             JSONDATA,
             nil, // OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
             nil, // ExtData: Pointer
             ConnectionSocket);
end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(FullCollectionName: AnsiString;
                                                    Query: AnsiString;
                                                    ReturnFieldsSelector: AnsiString;
                                                    RowTag: AnsiString;
                                                    Skip: integer;
                                                    First: Integer;
                                                    JSONDATA: TALJSONNode;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aQuery: TALMongoDBClientSelectDataQUERY;
begin
  aQuery := TALMongoDBClientSelectDataQUERY.Create;
  aQuery.FullCollectionName := FullCollectionName;
  aQuery.Query := Query;
  aQuery.ReturnFieldsSelector := ReturnFieldsSelector;
  aQuery.RowTag := RowTag;
  aQuery.Skip := Skip;
  aQuery.First := First;
  SelectData(aQuery,
             JSONDATA,
             ConnectionSocket);
end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(FullCollectionName: AnsiString;
                                                    Query: AnsiString;
                                                    ReturnFieldsSelector: AnsiString;
                                                    RowTag: AnsiString;
                                                    JSONDATA: TALJSONNode;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             RowTag,
             0, // Skip
             0, //First
             JSONDATA,
             ConnectionSocket);
end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(FullCollectionName: AnsiString;
                                                    Query: AnsiString;
                                                    ReturnFieldsSelector: AnsiString;
                                                    JSONDATA: TALJSONNode;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             '', // RowTag,
             0, // Skip
             0, //First
             JSONDATA,
             ConnectionSocket);
end;

{*********************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.UpdateData(Queries: TALMongoDBClientUpdateDataQUERIES;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
Var aQueriesIndex: integer;
    aFlags: integer;
    aNumberOfDocumentsUpdated: integer;
    aupdatedExisting: boolean;
    aObjectID: TALJSONObjectID;
    aTMPConnectionSocket: TSocket;
    aOwnConnection: Boolean;
    aStopWatch: TStopWatch;
begin

  //exit if no query
  if length(Queries) = 0 then Exit;

  //acquire a connection
  if ConnectionSocket = INVALID_SOCKET then begin
    aTMPConnectionSocket := AcquireConnection;
    aOwnConnection := True;
  end
  else begin
    aTMPConnectionSocket := ConnectionSocket;
    aOwnConnection := False;
  end;

  try

    //init the TstopWatch
    aStopWatch := TstopWatch.Create;

    //loop on all the Queries
    For aQueriesIndex := 0 to length(Queries) - 1 do begin

      //start the TstopWatch
      aStopWatch.Reset;
      aStopWatch.Start;

      //bit num	  name	        description
      //0	        Upsert	      If set, the database will insert the supplied object into the collection if no matching document is found.
      //1	        MultiUpdate	  If set, the database will update all matching objects in the collection. Otherwise only updates first matching doc.
      //2-31    	Reserved	    Must be set to 0.
      aFlags := 0;
      if Queries[aQueriesIndex].flags.Upsert then aFlags := aFlags or (1 shl 0);
      if Queries[aQueriesIndex].flags.MultiUpdate then aFlags := aFlags or (1 shl 1);

      OP_UPDATE(aTMPConnectionSocket,
                aflags,
                Queries[aQueriesIndex].FullCollectionName,
                Queries[aQueriesIndex].selector,
                Queries[aQueriesIndex].Update,
                aNumberOfDocumentsUpdated,
                aupdatedExisting,
                aObjectID);

      //do the OnUpdateDataDone
      aStopWatch.Stop;
      OnUpdateDataDone(Queries[aQueriesIndex],
                       aStopWatch.ElapsedMilliseconds);

    end;

    //Release the Connection
    if aOwnConnection then ReleaseConnection(aTMPConnectionSocket);

  except
    On E: Exception do begin
      if aOwnConnection then ReleaseConnection(aTMPConnectionSocket,
                                               (not (E Is EAlMongoDBClientException)) or
                                               (E as EAlMongoDBClientException).CloseConnection);
      raise;
    end;
  end;

end;

{*****************************************************************************************}
procedure TAlMongoDBConnectionPoolClient.UpdateData(Query: TALMongoDBClientUpdateDataQUERY;
                                                    var NumberOfDocumentsUpdated: integer; // reports the number of documents updated
                                                    var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                                    var ObjectID: TALJSONObjectID; // an ObjectId that corresponds to the upserted document if the update resulted in an insert.
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
Var aFlags: integer;
    aTMPConnectionSocket: TSocket;
    aOwnConnection: Boolean;
    aStopWatch: TStopWatch;
begin

  //acquire a connection
  if ConnectionSocket = INVALID_SOCKET then begin
    aTMPConnectionSocket := AcquireConnection;
    aOwnConnection := True;
  end
  else begin
    aTMPConnectionSocket := ConnectionSocket;
    aOwnConnection := False;
  end;

  try

    //init the TstopWatch
    aStopWatch := TstopWatch.StartNew;

    //bit num	  name	        description
    //0	        Upsert	      If set, the database will insert the supplied object into the collection if no matching document is found.
    //1	        MultiUpdate	  If set, the database will update all matching objects in the collection. Otherwise only updates first matching doc.
    //2-31    	Reserved	    Must be set to 0.
    aFlags := 0;
    if Query.flags.Upsert then aFlags := aFlags or (1 shl 0);
    if Query.flags.MultiUpdate then aFlags := aFlags or (1 shl 1);

    OP_UPDATE(aTMPConnectionSocket,
              aflags,
              Query.FullCollectionName,
              Query.selector,
              Query.Update,
              NumberOfDocumentsUpdated,
              updatedExisting,
              ObjectID);

    //do the OnUpdateDataDone
    aStopWatch.Stop;
    OnUpdateDataDone(Query,
                     aStopWatch.ElapsedMilliseconds);

    //Release the Connection
    if aOwnConnection then ReleaseConnection(aTMPConnectionSocket);

  except
    On E: Exception do begin
      if aOwnConnection then ReleaseConnection(aTMPConnectionSocket,
                                               (not (E Is EAlMongoDBClientException)) or
                                               (E as EAlMongoDBClientException).CloseConnection);
      raise;
    end;
  end;

end;

{*****************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.UpdateData(Query: TALMongoDBClientUpdateDataQUERY;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aNumberOfDocumentsUpdated: integer;
    aupdatedExisting: boolean;
    aObjectID: TALJSONObjectID;
begin
  UpdateData(Query,
             aNumberOfDocumentsUpdated,
             aupdatedExisting,
             aObjectID,
             ConnectionSocket);
end;

{*********************************************************************************}
procedure TAlMongoDBConnectionPoolClient.UpdateData(FullCollectionName: AnsiString;
                                                    Selector: AnsiString;
                                                    Update: AnsiString;
                                                    InsertIfNotFound: Boolean;
                                                    MultiUpdate: Boolean;
                                                    var NumberOfDocumentsUpdated: integer; // reports the number of documents updated
                                                    var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                                    var ObjectID: TALJSONObjectID; // an ObjectId that corresponds to the upserted document if the update resulted in an insert.
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aQuery: TALMongoDBClientUpdateDataQUERY;
begin
  aQuery := TALMongoDBClientUpdateDataQUERY.Create;
  aQuery.FullCollectionName := FullCollectionName;
  aQuery.selector := selector;
  aQuery.update := update;
  aQuery.flags.Upsert := InsertIfNotFound;
  aQuery.flags.MultiUpdate := MultiUpdate;
  UpdateData(aQuery,
             NumberOfDocumentsUpdated,
             updatedExisting,
             ObjectID,
             ConnectionSocket);
end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.UpdateData(FullCollectionName: AnsiString;
                                                    Selector: AnsiString;
                                                    Update: AnsiString;
                                                    InsertIfNotFound: Boolean;
                                                    MultiUpdate: Boolean;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aNumberOfDocumentsUpdated: integer;
    aupdatedExisting: boolean;
    aObjectID: TALJSONObjectID;
begin
  UpdateData(FullCollectionName,
             Selector,
             Update,
             InsertIfNotFound,
             MultiUpdate,
             aNumberOfDocumentsUpdated,
             aupdatedExisting,
             aObjectID,
             ConnectionSocket);
end;

{*********************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.InsertData(Queries: TALMongoDBClientInsertDataQUERIES;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
Var aQueriesIndex: integer;
    aFlags: integer;
    aTMPConnectionSocket: TSocket;
    aOwnConnection: Boolean;
    aStopWatch: TStopWatch;
begin

  //exit if no Query
  if length(Queries) = 0 then Exit;

  //acquire a connection
  if ConnectionSocket = INVALID_SOCKET then begin
    aTMPConnectionSocket := AcquireConnection;
    aOwnConnection := True;
  end
  else begin
    aTMPConnectionSocket := ConnectionSocket;
    aOwnConnection := False;
  end;

  try

    //init the TstopWatch
    aStopWatch := TstopWatch.Create;

    //loop on all the queries
    For aQueriesIndex := 0 to length(Queries) - 1 do begin

      //start the TstopWatch
      aStopWatch.Reset;
      aStopWatch.Start;

      //bit num	 name	            description
      //0	       ContinueOnError	If set, the database will not stop processing a bulk insert if one fails (eg due to duplicate IDs).
      //                          This makes bulk insert behave similarly to a series of single inserts, except lastError will be set if
      //                          any insert fails, not just the last one. If multiple errors occur, only the most recent will be
      //                          reported by getLastError. (new in 1.9.1)
      //1-31	   Reserved	        Must be set to 0.
      aFlags := 0;
      if Queries[aQueriesIndex].flags.ContinueOnError then aFlags := aFlags or (1 shl 0);

      OP_INSERT(aTMPConnectionSocket,
                aflags,
                Queries[aQueriesIndex].FullCollectionName,
                Queries[aQueriesIndex].documents);

      //do the OnInsertDataDone
      aStopWatch.Stop;
      OnInsertDataDone(Queries[aQueriesIndex],
                       aStopWatch.ElapsedMilliseconds);

    end;

    //Release the Connection
    if aOwnConnection then ReleaseConnection(aTMPConnectionSocket);

  except
    On E: Exception do begin
      if aOwnConnection then ReleaseConnection(aTMPConnectionSocket,
                                               (not (E Is EAlMongoDBClientException)) or
                                               (E as EAlMongoDBClientException).CloseConnection);
      raise;
    end;
  end;

end;

{*****************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.InsertData(Query: TALMongoDBClientInsertDataQUERY;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aInsertDataQueries: TALMongoDBClientInsertDataQUERIES;
begin
  setlength(aInsertDataQueries,1);
  aInsertDataQueries[0] := Query;
  InsertData(aInsertDataQueries,
             ConnectionSocket);
end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.InsertData(FullCollectionName: AnsiString;
                                                    Documents: AnsiString;
                                                    ContinueOnError: Boolean;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aQuery: TALMongoDBClientInsertDataQUERY;
begin
  aQuery := TALMongoDBClientInsertDataQUERY.Create;
  aQuery.FullCollectionName := FullCollectionName;
  aQuery.documents := Documents;
  aQuery.flags.ContinueOnError := ContinueOnError;
  InsertData(aQuery,
             ConnectionSocket);
end;

{*********************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.DeleteData(Queries: TALMongoDBClientDeleteDataQUERIES;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
Var aQueriesIndex: integer;
    aFlags: integer;
    aNumberOfDocumentsRemoved: integer;
    aTMPConnectionSocket: TSocket;
    aOwnConnection: Boolean;
    aStopWatch: TStopWatch;
begin

  //exit if no query
  if length(Queries) = 0 then Exit;

  //acquire a connection
  if ConnectionSocket = INVALID_SOCKET then begin
    aTMPConnectionSocket := AcquireConnection;
    aOwnConnection := True;
  end
  else begin
    aTMPConnectionSocket := ConnectionSocket;
    aOwnConnection := False;
  end;

  try

    //init the TstopWatch
    aStopWatch := TstopWatch.Create;

    //loop on all the Queries
    For aQueriesIndex := 0 to length(Queries) - 1 do begin

      //start the TstopWatch
      aStopWatch.Reset;
      aStopWatch.Start;

      //bit num	   name	          description
      //0	         SingleRemove	  If set, the database will remove only the first matching document in the collection. Otherwise all matching documents will be removed.
      //1-31	     Reserved	      Must be set to 0.
      aFlags := 0;
      if Queries[aQueriesIndex].flags.SingleRemove then aFlags := aFlags or (1 shl 0);

      OP_DELETE(aTMPConnectionSocket,
                aflags,
                Queries[aQueriesIndex].FullCollectionName,
                Queries[aQueriesIndex].selector,
                aNumberOfDocumentsRemoved);

      //do the OnDeleteDataDone
      aStopWatch.Stop;
      OnDeleteDataDone(Queries[aQueriesIndex],
                       aStopWatch.ElapsedMilliseconds);

    end;

    //Release the Connection
    if aOwnConnection then ReleaseConnection(aTMPConnectionSocket);

  except
    On E: Exception do begin
      if aOwnConnection then ReleaseConnection(aTMPConnectionSocket,
                                               (not (E Is EAlMongoDBClientException)) or
                                               (E as EAlMongoDBClientException).CloseConnection);
      raise;
    end;
  end;

end;

{*****************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.DeleteData(Query: TALMongoDBClientDeleteDataQUERY;
                                                    var NumberOfDocumentsRemoved: integer;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
Var aFlags: integer;
    aTMPConnectionSocket: TSocket;
    aOwnConnection: Boolean;
    aStopWatch: TStopWatch;
begin

  //acquire a connection
  if ConnectionSocket = INVALID_SOCKET then begin
    aTMPConnectionSocket := AcquireConnection;
    aOwnConnection := True;
  end
  else begin
    aTMPConnectionSocket := ConnectionSocket;
    aOwnConnection := False;
  end;

  try

    //init the TstopWatch
    aStopWatch := TstopWatch.StartNew;

    //bit num	   name	          description
    //0	         SingleRemove	  If set, the database will remove only the first matching document in the collection. Otherwise all matching documents will be removed.
    //1-31	     Reserved	      Must be set to 0.
    aFlags := 0;
    if Query.flags.SingleRemove then aFlags := aFlags or (1 shl 0);

    OP_DELETE(aTMPConnectionSocket,
              aflags,
              Query.FullCollectionName,
              Query.selector,
              NumberOfDocumentsRemoved);

    //do the OnDeleteDataDone
    aStopWatch.Stop;
    OnDeleteDataDone(Query,
                     aStopWatch.ElapsedMilliseconds);

    //Release the Connection
    if aOwnConnection then ReleaseConnection(aTMPConnectionSocket);

  except
    On E: Exception do begin
      if aOwnConnection then ReleaseConnection(aTMPConnectionSocket,
                                               (not (E Is EAlMongoDBClientException)) or
                                               (E as EAlMongoDBClientException).CloseConnection);
      raise;
    end;
  end;

end;

{*****************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.DeleteData(Query: TALMongoDBClientDeleteDataQUERY;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aNumberOfDocumentsRemoved: integer;
begin
  DeleteData(Query,
             aNumberOfDocumentsRemoved,
             ConnectionSocket);
end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.DeleteData(FullCollectionName: AnsiString;
                                                    Selector: AnsiString;
                                                    SingleRemove: Boolean;
                                                    var NumberOfDocumentsRemoved: integer;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aQuery: TALMongoDBClientDeleteDataQUERY;
begin
  aQuery := TALMongoDBClientDeleteDataQUERY.Create;
  aQuery.FullCollectionName := FullCollectionName;
  aQuery.selector := Selector;
  aQuery.flags.SingleRemove := SingleRemove;
  DeleteData(aQuery,
             NumberOfDocumentsRemoved,
             ConnectionSocket);
end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.DeleteData(FullCollectionName: AnsiString;
                                                    Selector: AnsiString;
                                                    SingleRemove: Boolean;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aNumberOfDocumentsRemoved: integer;
begin
  DeleteData(FullCollectionName,
             Selector,
             SingleRemove,
             aNumberOfDocumentsRemoved,
             ConnectionSocket);
end;

{************************************************************************}
constructor TAlMongoDBTailMonitoringThread.Create(const aHost: AnsiString;
                                                  const aPort: integer;
                                                  const aFullCollectionName: AnsiString;
                                                  const aQuery: AnsiString;
                                                  const aReturnFieldsSelector: AnsiString;
                                                  aOnEvent: TAlMongoDBTailMonitoringThreadEvent;
                                                  aOnError: TAlMongoDBTailMonitoringThreadException);
begin
  fMongoDBClient := TalMongoDBClient.Create;
  FHost := aHost;
  fPort := aPort;
  fFullCollectionName := aFullCollectionName;
  fQuery := aQuery;
  fReturnFieldsSelector := aReturnFieldsSelector;
  fStarted := False;
  fcompleted := False;
  fOnEvent := aOnEvent;
  fOnException := aOnError;
  inherited Create(False); // see http://www.gerixsoft.com/blog/delphi/fixing-symbol-resume-deprecated-warning-delphi-2010
end;

{*********************************************************}
procedure TAlMongoDBTailMonitoringThread.AfterConstruction;
begin
  inherited;
  while (not fStarted) do sleep(1);
end;

{************************************************}
destructor TAlMongoDBTailMonitoringThread.Destroy;
begin

  //first set terminated to true
  If not Terminated then Terminate;

  //in case the execute in waiting fire the Fsignal
  fMongoDBClient.StopTailMonitoring := True;
  while (not fCompleted) do sleep(1);

  //free the fMongoDBClient
  fMongoDBClient.Free;

  //destroy the object
  inherited;

end;

{*************************************************************************}
Procedure TAlMongoDBTailMonitoringThread.DoEvent(JSONRowData: TALJSONNode);
begin
  if assigned(fOnEvent) then fOnEvent(self, JSONRowData);
end;

{*********************************************************************}
procedure TAlMongoDBTailMonitoringThread.DoException(Error: Exception);
begin
  if assigned(fonException) then fonException(self, Error);
end;

{***********************************************}
procedure TAlMongoDBTailMonitoringThread.Execute;
var aExtData: Pointer;
    aQuery: TALMongoDBClientSelectDataQUERY;
begin
  //to be sure that the thread was stated
  fStarted := True;

  //init aQuery
  aQuery := TALMongoDBClientSelectDataQUERY.Create;
  aQuery.FullCollectionName := fFullCollectionName;
  aQuery.Query := fQuery;
  aQuery.ReturnFieldsSelector := fReturnFieldsSelector;
  aQuery.flags.TailMonitoring := True;

  //loop still not terminated
  while not Terminated do begin
    Try

      //if terminated then exit;
      if Terminated then Break;

      //disconnect
      fMongoDBClient.Disconnect;

      //connect
      fMongoDBClient.Connect(fHost, fPort);

      //select the data
      aExtData := nil;
      fMongoDBClient.SelectData(aQuery,
                                Procedure (JSONRowData: TALJSONNode;
                                           ViewTag: AnsiString;
                                           ExtData: Pointer;
                                           Var Continue: Boolean)
                                begin
                                  doEvent(JSONRowData);
                                end,
                                aExtData);

    Except
      on E: Exception do begin
        DoException(E);
        sleep(1);
      end;
    End;
  end;

  //set completed to true
  //we need to to this because i don't know why
  //but on isapi the waitfor (call in thread.free)
  //never return.
  //but i don't remenbered if the free was call in the initialization
  //section of the ISAPI DLL (and that bad to do something like this
  //in initialization or finalization).
  fcompleted := True;
end;

end.
