(*******************************************************************************
Delphi Client for MongoDB database.
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
*******************************************************************************)

unit ALMongoDBClient;

interface

uses
  winapi.WinSock2,
  system.Contnrs,
  system.Classes,
  System.SyncObjs,
  System.SysUtils,
  ALCommon,
  ALStringList,
  ALJSONDoc;

const
  MONGO_OP_REPLY = 1; //Reply to a client request. responseTo is set
  MONGO_OP_MSG = 1000; //generic msg command followed by a string
  MONGO_OP_UPDATE = 2001; //update document
  MONGO_OP_INSERT = 2002; //insert new document
  MONGO_RESERVED = 2003; //formerly used for OP_GET_BY_OID
  MONGO_OP_QUERY = 2004; //query a collection
  MONGO_OP_GET_MORE = 2005; //Get more data from a query. See Cursors
  MONGO_OP_DELETE = 2006; //Delete documents
  MONGO_OP_KILL_CURSORS = 2007; //Tell database client is done with a cursor

type

    {----------------------------------------------------------------------------------------}
    TALMongoDBClientSelectDataOnNewRowFunct = reference to Procedure(JSONRowData: TALJSONNode;
                                                                     const ViewTag: AnsiString;
                                                                     ExtData: Pointer;
                                                                     Var Continue: Boolean);

    {-------------------------------------------------------------------------------------}
    TALMongoDBClientRunCommandOnNewBatchRowFunct = TALMongoDBClientSelectDataOnNewRowFunct;

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
    TALMongoDBClientSelectDataFlags = set of (sfTailMonitoring,  // it's TailableCursor + AwaitData option. If we are at the end of the data, wait for new data rather than returning no data
                                                                 // a good explanation can be found here: http://docs.mongodb.org/manual/tutorial/create-tailable-cursor/
                                                                 //                                       http://shtylman.com/post/the-tail-of-mongodb/
                                                                 // use only with TALMongoDBClientSelectDataOnNewRowFunct
                                              sfSlaveOk,         // Allow query of replica slave.
                                                                 // Note that you should use this flag even if you do not use the automatic routing to secondaries.
                                                                 // If you connect directly to a secondary in a replica set, you still need to set this flag, which basically tells
                                                                 // the database that you are aware that you might be getting older data and you're okay with that.
                                                                 // If you do not call this, you'll get "not master" errors when you try to query.
                                              sfNoCursorTimeout, // The server normally times out idle cursors after an inactivity period (10 minutes) to prevent excess memory use.
                                                                 // Set this option to prevent that.
                                              sfPartial);        // Get partial results from a mongos if some shards are down (instead of throwing an error)

    {--------------------------------------------------}
    TALMongoDBClientRunCommandFlags = set of (cfSlaveOk,         // Allow query of replica slave.
                                                                 // Note that you should use this flag even if you do not use the automatic routing to secondaries.
                                                                 // If you connect directly to a secondary in a replica set, you still need to set this flag, which basically tells
                                                                 // the database that you are aware that you might be getting older data and you're okay with that.
                                                                 // If you do not call this, you'll get "not master" errors when you try to query.
                                              cfNoCursorTimeout, // The server normally times out idle cursors after an inactivity period (10 minutes) to prevent excess memory use.
                                                                 // Set this option to prevent that.
                                              cfPartial);        // Get partial results from a mongos if some shards are down (instead of throwing an error)

    {-------------------------------------------------}
    TALMongoDBClientUpdateDataFlags = set of (ufUpsert,       // If set, the database will insert the supplied object into the collection if no matching document is found.
                                              ufMultiUpdate); // If set, the database will update all matching objects in the collection. Otherwise only updates first matching doc.

    {-----------------------------------------------------------}
    TALMongoDBClientInsertDataFlags = set of (ifContinueOnError); // If set, the database will not stop processing a bulk insert if one fails (eg due to duplicate IDs).
                                                                  // This makes bulk insert behave similarly to a series of single inserts, except lastError will be set
                                                                  // if any insert fails, not just the last one. If multiple errors occur, only the most recent will be
                                                                  // reported by getLastError. (new in 1.9.1)

    {--------------------------------------------------------}
    TALMongoDBClientDeleteDataFlags = set of (dfSingleRemove); // If set, the database will remove only the first matching document in the
                                                               // collection. Otherwise all matching documents will be removed.

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
      function loadCachedData(const Key: AnsiString;
                              var DataStr: AnsiString): Boolean; virtual;
      Procedure SaveDataToCache(const Key: ansiString;
                                const CacheThreshold: integer;
                                const DataStr: ansiString); virtual;
      procedure DoSetSendTimeout(aSocketDescriptor: TSocket; const Value: integer); virtual;
      procedure DoSetReceiveTimeout(aSocketDescriptor: TSocket; const Value: integer); virtual;
      procedure DoSetKeepAlive(aSocketDescriptor: TSocket; const Value: boolean); virtual;
      procedure DoSetTCPNoDelay(aSocketDescriptor: TSocket; const Value: boolean); virtual;
      procedure SetSendTimeout(const Value: integer); virtual;
      procedure SetReceiveTimeout(const Value: integer); virtual;
      procedure SetKeepAlive(const Value: boolean); virtual;
      procedure SetTCPNoDelay(const Value: boolean); virtual;
      procedure CheckOSError(Error: Boolean);
      procedure DoConnect(var aSocketDescriptor: TSocket;
                          const aHost: AnsiString;
                          const APort: integer;
                          const aSendTimeout: Integer;
                          const aReceiveTimeout: Integer;
                          const aKeepAlive: Boolean;
                          const aTCPNoDelay: Boolean); virtual;
      Procedure DoDisconnect(var aSocketDescriptor: TSocket); virtual;
      Function SocketWrite(aSocketDescriptor: TSocket; const Buf; len: Integer): Integer; Virtual;
      Function SocketRead(aSocketDescriptor: TSocket; var buf; len: Integer): Integer; Virtual;
      Function SendCmd(aSocketDescriptor: TSocket;
                       const aCmd: AnsiString;
                       const aGetResponse: boolean): AnsiString;
      Function GetResponse(aSocketDescriptor: TSocket): AnsiString; virtual;
      Procedure CheckRunCommandResponse(aResponseNode: TalJsonNode); virtual;
      Procedure CheckServerLastError(aSocketDescriptor: TSocket;
                                     var NumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed, if the preceding operation was an update or remove operation.
                                     var UpdatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                     var Upserted: ansiString); overload; // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
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
                          var upserted: ansiString);              // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
      Procedure OP_DELETE(aSocketDescriptor: TSocket;
                          const flags: integer;                   // bit vector
                          const fullCollectionName: ansiString;   // "dbname.collectionname"
                          const selector: ansiString;             // query object.
                          var NumberOfDocumentsRemoved: integer); // reports the number of documents updated or removed, if the preceding operation was an update or remove operation.
      procedure OnSelectDataDone(const FullCollectionName: AnsiString;
                                 const Query: AnsiString;
                                 const ReturnFieldsSelector: AnsiString;
                                 flags: TALMongoDBClientSelectDataFlags;
                                 const RowTag: AnsiString;
                                 const ViewTag: AnsiString;
                                 Skip: integer;
                                 First: Integer;
                                 CacheThreshold: Integer;
                                 TimeTaken: double); virtual;
      procedure OnRunCommandDone(const DatabaseName: AnsiString;
                                 const Command: AnsiString;
                                 flags: TALMongoDBClientRunCommandFlags;
                                 const RowTag: AnsiString;
                                 const ViewTag: AnsiString;
                                 CacheThreshold: Integer;
                                 TimeTaken: double); virtual;
      procedure OnUpdateDataDone(const FullCollectionName: AnsiString;
                                 const selector: AnsiString;
                                 const update: AnsiString;
                                 flags: TALMongoDBClientUpdateDataFlags;
                                 TimeTaken: double); virtual;
      procedure OnDeleteDataDone(const FullCollectionName: AnsiString;
                                 const selector: AnsiString;
                                 flags: TALMongoDBClientDeleteDataFlags;
                                 TimeTaken: double); virtual;
      procedure OnInsertDataDone(const FullCollectionName: AnsiString;
                                 const documents: AnsiString;
                                 flags: TALMongoDBClientInsertDataFlags;
                                 TimeTaken: double); virtual;
      procedure OnFindAndModifyDataDone(const FullCollectionName: AnsiString;
                                        const query: AnsiString;
                                        const sort: AnsiString;
                                        remove: boolean;
                                        const update: AnsiString;
                                        new: boolean;
                                        const ReturnFieldsSelector: AnsiString;
                                        InsertIfNotFound: boolean;
                                        RowTag: AnsiString;
                                        ViewTag: AnsiString;
                                        TimeTaken: double); virtual;
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

      Procedure SelectData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                 // name with the collection name, using a . for the concatenation. For example, for the database
                                                                 // foo and the collection bar, the full collection name is foo.bar
                           const Query: AnsiString; // BSON document that represents the query. The query will contain one or more elements,
                                                    // all of which must match for a document to be included in the result set. Possible elements
                                                    // include $query, $orderby, $hint, $explain, and $snapshot.
                           const ReturnFieldsSelector: AnsiString; // Optional. BSON document that limits the fields in the returned documents.
                                                                   // The returnFieldsSelector contains one or more elements, each of which is the name
                                                                   // of a field that should be returned, and and the integer value 1. In JSON notation,
                                                                   // a returnFieldsSelector to limit to the fields a, b and c would be:
                                                                   // { a : 1, b : 1, c : 1}
                           flags: TALMongoDBClientSelectDataFlags; // Options (see TALMongoDBClientSelectDataFlags)
                           const RowTag: AnsiString; // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                           const ViewTag: AnsiString; // the node name under with all records will be stored in the JSON/XML result document.
                           Skip: integer; // Sets the number of documents to omit
                           First: Integer; // Limits the number of documents to retrieve
                           CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                    // cache or not. Values <= 0 deactivate the cache
                           JSONDATA: TALJSONNode;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           flags: TALMongoDBClientSelectDataFlags;
                           Skip: integer;
                           First: Integer;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           flags: TALMongoDBClientSelectDataFlags;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           flags: TALMongoDBClientSelectDataFlags;
                           const RowTag: AnsiString;
                           Skip: integer;
                           First: Integer;
                           JSONDATA: TALJSONNode); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           flags: TALMongoDBClientSelectDataFlags;
                           const RowTag: AnsiString;
                           JSONDATA: TALJSONNode); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           flags: TALMongoDBClientSelectDataFlags;
                           JSONDATA: TALJSONNode); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           Skip: integer;
                           First: Integer;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           const RowTag: AnsiString;
                           Skip: integer;
                           First: Integer;
                           JSONDATA: TALJSONNode); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           const RowTag: AnsiString;
                           JSONDATA: TALJSONNode); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           JSONDATA: TALJSONNode); overload; virtual;

      Procedure RunCommand(const DatabaseName: AnsiString;
                           const Command: AnsiString; // BSON document that represents the Command
                           flags: TALMongoDBClientRunCommandFlags; // Options (see TALMongoDBClientRunCommandFlags)
                           const RowTag: AnsiString; // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                           const ViewTag: AnsiString; // the node name under with all records will be stored in the JSON/XML result document.
                           CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                    // cache or not. Values <= 0 deactivate the cache
                           JSONDATA: TALJSONNode;
                           OnNewBatchRowFunct: TALMongoDBClientRunCommandOnNewBatchRowFunct;
                           ExtData: Pointer); overload; virtual;
      Procedure RunCommand(const DatabaseName: AnsiString;
                           const Command: AnsiString;
                           flags: TALMongoDBClientRunCommandFlags;
                           OnNewBatchRowFunct: TALMongoDBClientRunCommandOnNewBatchRowFunct;
                           ExtData: Pointer); overload; virtual;
      Procedure RunCommand(const DatabaseName: AnsiString;
                           const Command: AnsiString;
                           flags: TALMongoDBClientRunCommandFlags;
                           const RowTag: AnsiString;
                           JSONDATA: TALJSONNode); overload; virtual;
      Procedure RunCommand(const DatabaseName: AnsiString;
                           const Command: AnsiString;
                           flags: TALMongoDBClientRunCommandFlags;
                           JSONDATA: TALJSONNode); overload; virtual;
      Procedure RunCommand(const DatabaseName: AnsiString;
                           const Command: AnsiString;
                           OnNewBatchRowFunct: TALMongoDBClientRunCommandOnNewBatchRowFunct;
                           ExtData: Pointer); overload; virtual;
      Procedure RunCommand(const DatabaseName: AnsiString;
                           const Command: AnsiString;
                           const RowTag: AnsiString;
                           JSONDATA: TALJSONNode); overload; virtual;
      Procedure RunCommand(const DatabaseName: AnsiString;
                           const Command: AnsiString;
                           JSONDATA: TALJSONNode); overload; virtual;

      procedure UpdateData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                 // name with the collection name, using a . for the concatenation. For example, for the database
                                                                 // foo and the collection bar, the full collection name is foo.bar
                           const selector: AnsiString; // BSON document that specifies the query for selection of the document to update.
                           const update: AnsiString; // BSON document that specifies the update to be performed. For information on specifying updates see
                                                     // http://docs.mongodb.org/manual/tutorial/modify-documents/
                           flags: TALMongoDBClientUpdateDataFlags; // Options (see TALMongoDBClientUpdateDataFlags)
                           var NumberOfDocumentsUpdated: integer; // reports the number of documents updated
                           var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                           var ObjectID: ansiString); overload; virtual; // an ObjectId that corresponds to the upserted document if the update resulted in an insert.
      procedure UpdateData(const FullCollectionName: AnsiString;
                           const Selector: AnsiString;
                           const Update: AnsiString;
                           flags: TALMongoDBClientUpdateDataFlags); overload; virtual;
      procedure UpdateData(const FullCollectionName: AnsiString;
                           const Selector: AnsiString;
                           const Update: AnsiString); overload; virtual;

      procedure InsertData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                 // name with the collection name, using a . for the concatenation. For example, for the database
                                                                 // foo and the collection bar, the full collection name is foo.bar
                           const documents: AnsiString; // One or more documents to insert into the collection. If there are more than one, they are written in sequence, one after another.
                           flags: TALMongoDBClientInsertDataFlags); overload; virtual; // Options (see TALMongoDBClientInsertDataFlags)
      procedure InsertData(const FullCollectionName: AnsiString;
                           const documents: AnsiString); overload; virtual;

      procedure DeleteData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                 // name with the collection name, using a . for the concatenation. For example, for the database
                                                                 // foo and the collection bar, the full collection name is foo.bar
                           const selector: AnsiString; // BSON document that represent the query used to select the documents to be removed
                                                       // The selector will contain one or more elements, all of which must match for a document
                                                       // to be removed from the collection
                           flags: TALMongoDBClientDeleteDataFlags; // Options (see TALMongoDBClientDeleteDataFlags)
                           var NumberOfDocumentsRemoved: integer); overload; virtual;
      procedure DeleteData(const FullCollectionName: AnsiString;
                           const Selector: AnsiString;
                           flags: TALMongoDBClientDeleteDataFlags); overload; virtual;
      procedure DeleteData(const FullCollectionName: AnsiString;
                           const Selector: AnsiString); overload; virtual;

      Procedure FindAndModifyData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                        // name with the collection name, using a . for the concatenation. For example, for the database
                                                                        // foo and the collection bar, the full collection name is foo.bar
                                  const query: AnsiString; // Optional. The selection criteria for the modification. The query field employs the same query selectors as used in the
                                                           // db.collection.find() method. Although the query may match multiple documents, findAndModify will select only
                                                           // one document to modify.
                                  const sort: AnsiString; // Optional. Determines which document the operation modifies if the query selects multiple
                                                          // documents. findAndModify modifies the first document in the sort order specified by this argument.
                                  remove: boolean; // Must specify either the remove or the update field. Removes the document specified in the query field.
                                                   // Set this to true to remove the selected document . The default is false.
                                  const update: AnsiString; // Must specify either the remove or the update field. Performs an update of the selected document.
                                                            // The update field employs the same update operators or field: value specifications to modify the selected document.
                                  new: boolean; // Optional. When true, returns the modified document rather than the original. The findAndModify method
                                                // ignores the new option for remove operations. The default is false.
                                  const ReturnFieldsSelector: AnsiString; // Optional. A subset of fields to return. The fields document specifies an inclusion of a
                                                                          // field with 1, as in: fields: { <field1>: 1, <field2>: 1, ... }.
                                  InsertIfNotFound: boolean;  // Optional. Used in conjunction with the update field. When true, findAndModify
                                                              // creates a new document if no document matches the query, or if documents match the query,
                                                              // findAndModify performs an update. The default is false.
                                  const RowTag: AnsiString; // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                                  const ViewTag: AnsiString; // the node name under with all records will be stored in the JSON/XML result document.
                                  JSONDATA: TALJSONNode;
                                  var NumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed
                                  var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                  var ObjectID: AnsiString); overload; virtual; // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
      Procedure FindAndModifyData(const FullCollectionName: AnsiString;
                                  const query: AnsiString;
                                  const sort: AnsiString;
                                  remove: boolean;
                                  const update: AnsiString;
                                  new: boolean;
                                  const ReturnFieldsSelector: AnsiString;
                                  InsertIfNotFound: boolean;
                                  JSONDATA: TALJSONNode;
                                  var NumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed
                                  var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                  var ObjectID: AnsiString); overload; virtual; // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
      Procedure FindAndModifyData(const FullCollectionName: AnsiString;
                                  const query: AnsiString;
                                  const sort: AnsiString;
                                  remove: boolean;
                                  const update: AnsiString;
                                  new: boolean;
                                  const ReturnFieldsSelector: AnsiString;
                                  InsertIfNotFound: boolean;
                                  JSONDATA: TALJSONNode); overload; virtual;

      property Connected: Boolean read FConnected;
      property StopTailMonitoring: boolean read fStopTailMonitoring write fStopTailMonitoring;
    end;

    {----------------------------------------}
    TAlMongoDBConnectionPoolContainer = record
      SocketDescriptor: TSocket;
      LastAccessDate: int64;
    End;
    TAlMongoDBConnectionPool = array of TAlMongoDBConnectionPoolContainer;

    {----------------------------------------------------------}
    TAlMongoDBConnectionPoolClient = class(TAlBaseMongoDBClient)
    private
      FConnectionPool: TAlMongoDBConnectionPool;
      FConnectionPoolCount: integer;
      FConnectionPoolCapacity: integer;
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

      Procedure SelectData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                 // name with the collection name, using a . for the concatenation. For example, for the database
                                                                 // foo and the collection bar, the full collection name is foo.bar
                           const Query: AnsiString; // BSON document that represents the query. The query will contain one or more elements,
                                                    // all of which must match for a document to be included in the result set. Possible elements
                                                    // include $query, $orderby, $hint, $explain, and $snapshot.
                           const ReturnFieldsSelector: AnsiString; // Optional. BSON document that limits the fields in the returned documents.
                                                                   // The returnFieldsSelector contains one or more elements, each of which is the name
                                                                   // of a field that should be returned, and and the integer value 1. In JSON notation,
                                                                   // a returnFieldsSelector to limit to the fields a, b and c would be:
                                                                   // { a : 1, b : 1, c : 1}
                           flags: TALMongoDBClientSelectDataFlags; // Options (see TALMongoDBClientSelectDataFlags)
                           const RowTag: AnsiString; // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                           const ViewTag: AnsiString; // the node name under with all records will be stored in the JSON/XML result document.
                           Skip: integer; // Sets the number of documents to omit
                           First: Integer; // Limits the number of documents to retrieve
                           CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                    // cache or not. Values <= 0 deactivate the cache
                           JSONDATA: TALJSONNode;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           flags: TALMongoDBClientSelectDataFlags;
                           Skip: integer;
                           First: Integer;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           flags: TALMongoDBClientSelectDataFlags;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           flags: TALMongoDBClientSelectDataFlags;
                           const RowTag: AnsiString;
                           Skip: integer;
                           First: Integer;
                           JSONDATA: TALJSONNode;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           flags: TALMongoDBClientSelectDataFlags;
                           const RowTag: AnsiString;
                           JSONDATA: TALJSONNode;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           flags: TALMongoDBClientSelectDataFlags;
                           JSONDATA: TALJSONNode;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           Skip: integer;
                           First: Integer;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                           ExtData: Pointer;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           const RowTag: AnsiString;
                           Skip: integer;
                           First: Integer;
                           JSONDATA: TALJSONNode;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           const RowTag: AnsiString;
                           JSONDATA: TALJSONNode;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure SelectData(const FullCollectionName: AnsiString;
                           const Query: AnsiString;
                           const ReturnFieldsSelector: AnsiString;
                           JSONDATA: TALJSONNode;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;

      Procedure RunCommand(const DatabaseName: AnsiString;
                           const Command: AnsiString; // BSON document that represents the Command
                           flags: TALMongoDBClientRunCommandFlags; // Options (see TALMongoDBClientRunCommandFlags)
                           const RowTag: AnsiString; // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                           const ViewTag: AnsiString; // the node name under with all records will be stored in the JSON/XML result document.
                           CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                    // cache or not. Values <= 0 deactivate the cache
                           JSONDATA: TALJSONNode;
                           OnNewBatchRowFunct: TALMongoDBClientRunCommandOnNewBatchRowFunct;
                           ExtData: Pointer;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure RunCommand(const DatabaseName: AnsiString;
                           const Command: AnsiString;
                           flags: TALMongoDBClientRunCommandFlags;
                           OnNewBatchRowFunct: TALMongoDBClientRunCommandOnNewBatchRowFunct;
                           ExtData: Pointer;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure RunCommand(const DatabaseName: AnsiString;
                           const Command: AnsiString;
                           flags: TALMongoDBClientRunCommandFlags;
                           const RowTag: AnsiString;
                           JSONDATA: TALJSONNode;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure RunCommand(const DatabaseName: AnsiString;
                           const Command: AnsiString;
                           flags: TALMongoDBClientRunCommandFlags;
                           JSONDATA: TALJSONNode;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure RunCommand(const DatabaseName: AnsiString;
                           const Command: AnsiString;
                           OnNewBatchRowFunct: TALMongoDBClientRunCommandOnNewBatchRowFunct;
                           ExtData: Pointer;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure RunCommand(const DatabaseName: AnsiString;
                           const Command: AnsiString;
                           const RowTag: AnsiString;
                           JSONDATA: TALJSONNode;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure RunCommand(const DatabaseName: AnsiString;
                           const Command: AnsiString;
                           JSONDATA: TALJSONNode;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;

      procedure UpdateData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                 // name with the collection name, using a . for the concatenation. For example, for the database
                                                                 // foo and the collection bar, the full collection name is foo.bar
                           const selector: AnsiString; // BSON document that specifies the query for selection of the document to update.
                           const update: AnsiString; // BSON document that specifies the update to be performed. For information on specifying updates see
                                                     // http://docs.mongodb.org/manual/tutorial/modify-documents/
                           flags: TALMongoDBClientUpdateDataFlags; // Options (see TALMongoDBClientUpdateDataFlags)
                           var NumberOfDocumentsUpdated: integer; // reports the number of documents updated
                           var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                           var ObjectID: ansiString; // an ObjectId that corresponds to the upserted document if the update resulted in an insert.
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      procedure UpdateData(const FullCollectionName: AnsiString;
                           const Selector: AnsiString;
                           const Update: AnsiString;
                           flags: TALMongoDBClientUpdateDataFlags;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      procedure UpdateData(const FullCollectionName: AnsiString;
                           const Selector: AnsiString;
                           const Update: AnsiString;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;

      procedure InsertData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                 // name with the collection name, using a . for the concatenation. For example, for the database
                                                                 // foo and the collection bar, the full collection name is foo.bar
                           const documents: AnsiString; // One or more documents to insert into the collection. If there are more than one, they are written in sequence, one after another.
                           flags: TALMongoDBClientInsertDataFlags;// Options (see TALMongoDBClientInsertDataFlags)
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      procedure InsertData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                 // name with the collection name, using a . for the concatenation. For example, for the database
                                                                 // foo and the collection bar, the full collection name is foo.bar
                           const documents: AnsiString; // One or more documents to insert into the collection. If there are more than one, they are written in sequence, one after another.
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;


      procedure DeleteData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                 // name with the collection name, using a . for the concatenation. For example, for the database
                                                                 // foo and the collection bar, the full collection name is foo.bar
                           const selector: AnsiString; // BSON document that represent the query used to select the documents to be removed
                                                       // The selector will contain one or more elements, all of which must match for a document
                                                       // to be removed from the collection
                           flags: TALMongoDBClientDeleteDataFlags; // Options (see TALMongoDBClientDeleteDataFlags)
                           var NumberOfDocumentsRemoved: integer;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      procedure DeleteData(const FullCollectionName: AnsiString;
                           const Selector: AnsiString;
                           flags: TALMongoDBClientDeleteDataFlags;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      procedure DeleteData(const FullCollectionName: AnsiString;
                           const Selector: AnsiString;
                           const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;

      Procedure FindAndModifyData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                        // name with the collection name, using a . for the concatenation. For example, for the database
                                                                        // foo and the collection bar, the full collection name is foo.bar
                                  const query: AnsiString; // Optional. The selection criteria for the modification. The query field employs the same query selectors as used in the
                                                           // db.collection.find() method. Although the query may match multiple documents, findAndModify will select only
                                                           // one document to modify.
                                  const sort: AnsiString; // Optional. Determines which document the operation modifies if the query selects multiple
                                                          // documents. findAndModify modifies the first document in the sort order specified by this argument.
                                  remove: boolean; // Must specify either the remove or the update field. Removes the document specified in the query field.
                                                   // Set this to true to remove the selected document . The default is false.
                                  const update: AnsiString; // Must specify either the remove or the update field. Performs an update of the selected document.
                                                            // The update field employs the same update operators or field: value specifications to modify the selected document.
                                  new: boolean; // Optional. When true, returns the modified document rather than the original. The findAndModify method
                                                // ignores the new option for remove operations. The default is false.
                                  const ReturnFieldsSelector: AnsiString; // Optional. A subset of fields to return. The fields document specifies an inclusion of a
                                                                          // field with 1, as in: fields: { <field1>: 1, <field2>: 1, ... }.
                                  InsertIfNotFound: boolean;  // Optional. Used in conjunction with the update field. When true, findAndModify
                                                              // creates a new document if no document matches the query, or if documents match the query,
                                                              // findAndModify performs an update. The default is false.
                                  const RowTag: AnsiString; // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                                  const ViewTag: AnsiString; // the node name under with all records will be stored in the JSON/XML result document.
                                  JSONDATA: TALJSONNode;
                                  var NumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed
                                  var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                  var ObjectID: AnsiString; // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
                                  const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure FindAndModifyData(const FullCollectionName: AnsiString;
                                  const query: AnsiString;
                                  const sort: AnsiString;
                                  remove: boolean;
                                  const update: AnsiString;
                                  new: boolean;
                                  const ReturnFieldsSelector: AnsiString;
                                  InsertIfNotFound: boolean;
                                  JSONDATA: TALJSONNode;
                                  var NumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed
                                  var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                  var ObjectID: AnsiString; // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
                                  const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;
      Procedure FindAndModifyData(const FullCollectionName: AnsiString;
                                  const query: AnsiString;
                                  const sort: AnsiString;
                                  remove: boolean;
                                  const update: AnsiString;
                                  new: boolean;
                                  const ReturnFieldsSelector: AnsiString;
                                  InsertIfNotFound: boolean;
                                  JSONDATA: TALJSONNode;
                                  const ConnectionSocket: TSocket = INVALID_SOCKET); overload; virtual;

      property Host: ansiString read fHost;
      property Port: integer read fPort;
    end;

    {-------------------------------------------------------------------------------------------------------}
    TAlMongoDBTailMonitoringThreadEvent = reference to Procedure (Sender: TObject; JSONRowData: TALJSONNode);
    TAlMongoDBTailMonitoringThreadException = reference to procedure (Sender: TObject; Error: Exception);

    {---------------------------------------------}
    TAlMongoDBTailMonitoringThread = class(TThread)
    private
      fMongoDBClient: TALMongoDBClient;
      FHost: AnsiString;
      fPort: integer;
      fFullCollectionName: AnsiString;
      fQuery: AnsiString;
      fReturnFieldsSelector: AnsiString;
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
      Destructor Destroy; override;
      procedure Execute; override;
      property OnEvent: TAlMongoDBTailMonitoringThreadEvent read fOnEvent write fOnEvent;
      property OnException: TAlMongoDBTailMonitoringThreadException read fOnException write fOnException;
    end;

implementation

uses
  Winapi.Windows,
  System.Diagnostics,
  System.math,
  ALCipher,
  AlWinsock,
  ALWindows,
  ALString;

{***************************************************************************************************************************************}
constructor EAlMongoDBClientException.Create(const aMsg: AnsiString; const aErrorCode: integer; const aCloseConnection: Boolean = False);
begin
  fErrorCode := aErrorCode;
  fCloseConnection := aCloseConnection;
  inherited create(aMsg);
end;

{**************************************}
constructor TAlBaseMongoDBClient.Create;
var LWSAData: TWSAData;
begin
  CheckOSError(WSAStartup(MAKEWORD(2,2), LWSAData) <> 0);
  FSendTimeout := 2{h}*60{min}*60{s}*1000{ms}; // 2 hours
  FReceiveTimeout := 2{h}*60{min}*60{s}*1000{ms}; // 2 hours
  FKeepAlive := True;
  fTCPNoDelay := True;
end;

{**************************************}
destructor TAlBaseMongoDBClient.Destroy;
begin
  WSACleanup;
  inherited;
end;

{**********************************************************}
procedure TAlBaseMongoDBClient.CheckOSError(Error: Boolean);
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
  var LSockAddr:Sockaddr_in;
      LIP: AnsiString;
  begin
    aSocketDescriptor:=Socket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
    CheckOSError(aSocketDescriptor=INVALID_SOCKET);
    FillChar(LSockAddr,SizeOf(LSockAddr),0);
    LSockAddr.sin_family:=AF_INET;
    LSockAddr.sin_port:=swap(Port);
    LSockAddr.sin_addr.S_addr:=inet_addr(PAnsiChar(Server));
    If LSockAddr.sin_addr.S_addr = INADDR_NONE then begin
      CheckOSError(not ALHostToIP(Server, LIP));
      LSockAddr.sin_addr.S_addr:=inet_addr(PAnsiChar(LIP));
    end;
    CheckOSError(Winapi.WinSock2.Connect(aSocketDescriptor,TSockAddr(LSockAddr),SizeOf(LSockAddr))=SOCKET_ERROR);
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
Var LByteSent: integer;
    P: PAnsiChar;
    L: Integer;
begin
  p:=@aCmd[1]; // pchar
  l:=length(aCmd);
  while l>0 do begin
    LByteSent:=SocketWrite(aSocketDescriptor, p^,l);
    if LByteSent<=0 then raise EALException.Create('Connection close gracefully!');
    inc(p,LByteSent);
    dec(l,LByteSent);
  end;
  if aGetResponse then Result := GetResponse(aSocketDescriptor)
  else result := '';
end;

{********************************************************************************}
function TAlBaseMongoDBClient.GetResponse(aSocketDescriptor: TSocket): AnsiString;
Var LBytesReceived: Integer;
    LResultPos: Integer;
    LMessageLength: Integer;
const LBuffSize: integer = 8192;
begin

  //init local var
  Setlength(Result,LBuffSize);
  LResultPos := 0;
  LMessageLength := 0;

  //loop still we receive the full answer
  While True do begin

    //expnd the buffer
    if LResultPos = length(Result) then setlength(Result, length(Result) + LBuffSize);

    //read string from socket
    LBytesReceived := SocketRead(aSocketDescriptor, Result[LResultPos+1], length(Result) - LResultPos);
    If LBytesReceived <= 0 then raise EALException.Create('Connection close gracefully!');
    LResultPos := LResultPos + LBytesReceived;

    //the first 4 bytes contain the length of the message
    if (LMessageLength = 0) and (LResultPos >= sizeof(LMessageLength)) then begin
      ALMove(Result[1], LMessageLength, sizeof(LMessageLength));
      if LMessageLength < LResultPos then raise EALException.Create('Wrong socket response');
      setlength(Result, LMessageLength);
    end;

    //break if we are at the end of the message
    if LResultPos = LMessageLength then break;

  end;

end;

{*********************************************************************************}
Procedure TAlBaseMongoDBClient.CheckRunCommandResponse(aResponseNode: TalJsonNode);
begin

  //
  //{
  //  "ok" : 0.0,
  //  "errmsg" : "cursor id 379994841580103813 not found",
  //  "code" : 43.0,
  //  "codeName" : "CursorNotFound"
  //}
  //

  if not sameValue(aResponseNode.GetChildNodeValueFloat('ok', 0), 1) then
    raise EAlMongoDBClientException.Create(aResponseNode.GetChildNodeValueText('errmsg', 'Misc Error'), // const aMsg: AnsiString;
                                           aResponseNode.GetChildNodeValueInt32('code', 0)); // const aErrorCode: integer;

end;

{*****************************************************************************}
Procedure TAlBaseMongoDBClient.CheckServerLastError(aSocketDescriptor: TSocket;
                                                    var NumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed, if the preceding operation was an update or remove operation.
                                                    var UpdatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                                    var Upserted: ansiString); // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.

Var LResponseFlags: integer;
    LCursorID: int64;
    LStartingFrom: integer;
    LNumberReturned: integer;
    LContinue: boolean;
    LJSONDoc: TALJSONDocument;
    LCodeNode: TALJSONNode;
    LCodeInt: int32;
    LNode: TALJSONNode;

begin
  LJSONDoc := TALJSONDocument.Create;
  try

    //do the First query
    OP_QUERY(aSocketDescriptor,
             0, // flags
             'db.$cmd', // fullCollectionName
             0, // skip
             1, // First
             '{getLastError:1}', // query
             '', // ReturnFieldsSelector,
             LResponseFlags,
             LCursorID,
             LStartingFrom,
             LNumberReturned,
             LJSONDoc.Node,
             nil,
             nil,
             '',
             '',
             LContinue);
    try

      CheckRunCommandResponse(LJSONDoc.Node);

      LCodeNode := LJSONDoc.Node.ChildNodes.FindNode('code');
      if assigned(LCodeNode) and (not LCodeNode.Null) then LCodeInt := LCodeNode.int32
      else LCodeInt := 0;

      //err is null unless an error occurs. When there was an error with the preceding
      //operation, err contains a string identifying the error.
      LNode := LJSONDoc.Node.ChildNodes.FindNode('err');
      if assigned(LNode) and (not LNode.Null) then
        raise EAlMongoDBClientException.Create(LNode.Text, LCodeInt);

      //errmsg contains the description of the error. errmsg only appears
      //if there was an error with the preceding operation.
      LNode := LJSONDoc.Node.ChildNodes.FindNode('errmsg');
      if assigned(LNode) and (not LNode.Null) then
        raise EAlMongoDBClientException.Create(LNode.Text, LCodeInt);

      //code reports the preceding operation's error code.
      if assigned(LCodeNode) and (not LCodeNode.Null) then
        raise EAlMongoDBClientException.Create('Misc Error',LCodeInt);

      //If the preceding operation was an update or a remove operation, but not a findAndModify
      //operation, n reports the number of documents matched by the update or remove operation.
      //
      //For a remove operation, the number of matched documents will equal the number removed.
      //
      //For an update operation, if the operation results in no change to the document, such as setting
      //the value of the field to its current value, the number of matched documents may be smaller than
      //the number of documents actually modified. If the update includes the upsert:true option and
      //results in the creation of a new document, n returns the number of documents inserted.
      //
      //n is 0 if reporting on an update or remove that occurs through a findAndModify operation.
      LNode := LJSONDoc.Node.ChildNodes.FindNode('n');
      if assigned(LNode) then NumberOfDocumentsUpdatedOrRemoved := LNode.int32
      else NumberOfDocumentsUpdatedOrRemoved := 0;

      //updatedExisting is true when an update affects at least one document and
      //does not result in an upsert.
      LNode := LJSONDoc.Node.ChildNodes.FindNode('updatedExisting');
      if assigned(LNode) then UpdatedExisting := LNode.bool
      else UpdatedExisting := False;

      //If the update results in an insert, upserted is the value of _id field of the document.
      LNode := LJSONDoc.Node.ChildNodes.FindNode('upserted');
      if assigned(LNode) then Upserted := LNode.text
      else Upserted := '';

    finally

      //close the curson
      if LCursorID <> 0 then OP_KILL_CURSORS(aSocketDescriptor, [LCursorID]);

    end;

  finally
    LJSONDoc.free;
  end;
end;

{******************************************************************************}
Procedure TAlBaseMongoDBClient.CheckServerLastError(aSocketDescriptor: TSocket);
var LNumberOfDocumentsUpdatedOrRemoved: integer;
    LUpdatedExisting: boolean;
    LUpserted: ansiString;
begin
  CheckServerLastError(aSocketDescriptor,
                       LNumberOfDocumentsUpdatedOrRemoved,
                       LUpdatedExisting,
                       LUpserted);
end;

{*******************************************************************************}
Function TAlBaseMongoDBClient.BuildOPKILLCURSORSMessage(const requestID: integer;                       // identifier for this message
                                                        const responseTo: integer;                      // requestID from the original request (used in reponses from db)
                                                        const cursorIDs: array of int64): ansiString;   // cursorIDs to be close
var LCurrPos: integer;
    LMessageLength: Integer;
    LOPCode: integer;
    LZERO: integer;
    LNumberOfCursorIDs: integer;
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
  LNumberOfCursorIDs := length(cursorIDs);
  if LNumberOfCursorIDs <= 0 then exit;

  // init aMessageLength
  LMessageLength := sizeof(LMessageLength) +
                    sizeof(requestID) +
                    sizeof(responseTo) +
                    sizeof(LOPCode) +
                    sizeof(LZERO) +
                    sizeof(LNumberOfCursorIDs) +
                    LNumberOfCursorIDs * sizeof(cursorIDs[low(cursorIDs)]);

  //init the length of the result
  setlength(result, LMessageLength);
  LCurrPos := 1;

  // messageLength
  ALMove(LMessageLength, Result[LCurrPos], sizeof(LMessageLength));
  inc(LCurrPos,sizeof(LMessageLength));

  //requestID
  ALMove(requestID, Result[LCurrPos], sizeof(requestID));
  inc(LCurrPos,sizeof(requestID));

  //responseTo
  ALMove(responseTo, Result[LCurrPos], sizeof(responseTo));
  inc(LCurrPos,sizeof(responseTo));

  //opCode
  LOPCode := MONGO_OP_KILL_CURSORS;
  ALMove(LOPCode, Result[LCurrPos], sizeof(LOPCode));
  inc(LCurrPos,sizeof(LOPCode));

  //aZERO
  LZERO := 0;
  ALMove(LZERO, Result[LCurrPos], sizeof(LZERO));
  inc(LCurrPos,sizeof(LZERO));

  //anumberOfCursorIDs
  ALMove(LNumberOfCursorIDs, Result[LCurrPos], sizeof(LNumberOfCursorIDs));
  inc(LCurrPos,sizeof(LNumberOfCursorIDs));

  //cursorIDs
  for i := low(cursorIDs) to High(cursorIDs) do
    ALMove(cursorIDs[i], Result[LCurrPos], sizeof(cursorIDs[i]));

end;

{**************************************************************************}
Function TAlBaseMongoDBClient.BuildOPINSERTMessage(const requestID: integer;                            // identifier for this message
                                                   const responseTo: integer;                           // requestID from the original request (used in reponses from db)
                                                   const flags: integer;                                // bit vector
                                                   const fullCollectionName: ansiString;                // "dbname.collectionname"
                                                   const documents: ansiString): ansiString;            // one or more documents to insert into the collection
var LCurrPos: integer;
    LMessageLength: Integer;
    LTmpInt: integer;
    LOPCode: integer;
    LBsonDocuments: ansiString;
    LJsonDocument: TALJsonDocument;
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
  if (length(documents) > sizeof(LMessageLength)) then ALMove(documents[1], LMessageLength, sizeof(LMessageLength))
  else LMessageLength := 0;
  while (LMessageLength > 0) and
        (LMessageLength <= length(documents) - sizeof(LTmpInt)) do begin
    ALMove(documents[LMessageLength+1], LTmpInt, sizeof(LTmpInt));
    if LTmpInt <= 0 then break;
    LMessageLength := LMessageLength + LTmpInt;
  end;
  if (LMessageLength <> length(documents)) or
     ((length(documents) > 0) and
      (documents[length(documents)] <> #0)) then begin
    LJsonDocument := TALJsonDocument.Create;
    try
      LJsonDocument.LoadFromJSONString('{"documents":' + documents + '}'); // { .... }                      => {"documents":{ .... } }
                                                                           // [{ ... }, { ... }, { ... }]   => {"documents":[{ ... }, { ... }, { ... }] }
      LBsonDocuments := '';
      with LJsonDocument.Node.ChildNodes['documents'] do begin
        if nodeType = ntObject then LBsonDocuments := BSON  // {"documents":{ .... } }
        else begin  // {"documents":[{ ... }, { ... }, { ... }] }
          for I := 0 to ChildNodes.Count - 1 do
            LBsonDocuments := LBsonDocuments + ChildNodes[i].BSON;
        end;
      end;
    finally
      LJsonDocument.Free;
    end;
  end
  else LBsonDocuments := documents;

  // init aMessageLength
  LMessageLength := sizeof(LMessageLength) +
                    sizeof(requestID) +
                    sizeof(responseTo) +
                    sizeof(LOPCode) +
                    sizeof(flags) +
                    length(fullCollectionName) + 1{the trailing #0} +
                    length(LBsonDocuments);

  //init the length of the result
  setlength(result, LMessageLength);
  LCurrPos := 1;

  // messageLength
  ALMove(LMessageLength, Result[LCurrPos], sizeof(LMessageLength));
  inc(LCurrPos,sizeof(LMessageLength));

  //requestID
  ALMove(requestID, Result[LCurrPos], sizeof(requestID));
  inc(LCurrPos,sizeof(requestID));

  //responseTo
  ALMove(responseTo, Result[LCurrPos], sizeof(responseTo));
  inc(LCurrPos,sizeof(responseTo));

  //opCode
  LOPCode := MONGO_OP_INSERT;
  ALMove(LOPCode, Result[LCurrPos], sizeof(LOPCode));
  inc(LCurrPos,sizeof(LOPCode));

  //flags
  ALMove(flags, Result[LCurrPos], sizeof(flags));
  inc(LCurrPos,sizeof(flags));

  //fullCollectionName
  if length(fullCollectionName) <= 0 then raise EALException.Create('FullCollectionName must not be empty');
  ALMove(fullCollectionName[1], result[LCurrPos], length(fullCollectionName));
  inc(LCurrPos,length(fullCollectionName));
  result[LCurrPos] := #0;
  inc(LCurrPos);

  //aBsonDocuments
  if length(LBsonDocuments) <= 0 then raise EALException.Create('documents must not be empty');
  ALMove(LBsonDocuments[1], result[LCurrPos], length(LBsonDocuments));

end;

{**************************************************************************}
Function TAlBaseMongoDBClient.BuildOPUPDATEMessage(const requestID: integer;                            // identifier for this message
                                                   const responseTo: integer;                           // requestID from the original request (used in reponses from db)
                                                   const flags: integer;                                // bit vector
                                                   const fullCollectionName: ansiString;                // "dbname.collectionname"
                                                   const selector: ansiString;                          // the query to select the document
                                                   const update: ansiString): ansiString;               // specification of the update to perform
var LCurrPos: integer;
    LMessageLength: Integer;
    LOPCode: integer;
    LZERO: integer;
    LBsonSelector: ansiString;
    LBsonUpdate: AnsiString;
    LJSONDocument: TalJSONDocument;
begin

  //
  //  messageLength     requestID       responseTo        opCode                 ZERO
  //  [0][1][2][3]     [4][5][6][7]    [8][9][10][11]   [12][13][14][15]    [16][17][18][19]
  //
  //  fullCollectionName       flags              selector       update
  //  [20]...[23]            [24][25][26][27]   [27]...[35]    [36]...[39]
  //

  //init aBsonselector
  if (length(Selector) > sizeof(LMessageLength)) then ALMove(Selector[1], LMessageLength, sizeof(LMessageLength))
  else LMessageLength := 0;
  if (LMessageLength <> length(Selector)) or
   ((length(Selector) > 0) and
    (Selector[length(Selector)] <> #0)) then begin
    LJSONDocument := TALJsonDocument.Create;
    try
      LJSONDocument.LoadFromJSONString(Selector);
      LJSONDocument.SaveToBSONString(LBsonSelector);
    finally
      LJSONDocument.Free;
    end;
  end
  else begin
    if Selector = '' then LBsonSelector := #5#0#0#0#0 // empty BSON
    else LBsonSelector := Selector;
  end;

  //init aBSONUpdate
  if (length(Update) > sizeof(LMessageLength)) then ALMove(Update[1], LMessageLength, sizeof(LMessageLength))
  else LMessageLength := 0;
  if (LMessageLength <> length(Update)) or
     ((length(Update) > 0) and
      (Update[length(Update)] <> #0)) then begin
    LJSONDocument := TALJsonDocument.Create;
    try
      LJSONDocument.LoadFromJSONString(Update);
      LJSONDocument.SaveToBSONString(LBsonUpdate);
    finally
      LJSONDocument.Free;
    end;
  end
  else begin
    if Update = '' then LBsonUpdate := #5#0#0#0#0 // empty BSON
    else LBsonUpdate := Update;
  end;

  // init aMessageLength
  LMessageLength := sizeof(LMessageLength) +
                    sizeof(requestID) +
                    sizeof(responseTo) +
                    sizeof(LOPCode) +
                    sizeof(LZERO) +
                    length(fullCollectionName) + 1{the trailing #0} +
                    sizeof(flags) +
                    length(LBsonSelector) +
                    length(LBsonUpdate);

  //init the length of the result
  setlength(result, LMessageLength);
  LCurrPos := 1;

  // messageLength
  ALMove(LMessageLength, Result[LCurrPos], sizeof(LMessageLength));
  inc(LCurrPos,sizeof(LMessageLength));

  //requestID
  ALMove(requestID, Result[LCurrPos], sizeof(requestID));
  inc(LCurrPos,sizeof(requestID));

  //responseTo
  ALMove(responseTo, Result[LCurrPos], sizeof(responseTo));
  inc(LCurrPos,sizeof(responseTo));

  //opCode
  LOPCode := MONGO_OP_UPDATE;
  ALMove(LOPCode, Result[LCurrPos], sizeof(LOPCode));
  inc(LCurrPos,sizeof(LOPCode));

  //ZERO
  LZERO := 0;
  ALMove(LZERO, Result[LCurrPos], sizeof(LZERO));
  inc(LCurrPos,sizeof(LZERO));

  //fullCollectionName
  if length(fullCollectionName) <= 0 then raise EALException.Create('FullCollectionName must not be empty');
  ALMove(fullCollectionName[1], result[LCurrPos], length(fullCollectionName));
  inc(LCurrPos,length(fullCollectionName));
  result[LCurrPos] := #0;
  inc(LCurrPos);

  //flags
  ALMove(flags, Result[LCurrPos], sizeof(flags));
  inc(LCurrPos,sizeof(flags));

  //aBsonselector
  ALMove(LBsonSelector[1], result[LCurrPos], length(LBsonSelector));
  inc(LCurrPos,length(LBsonSelector));

  //aBSONUpdate
  ALMove(LBsonUpdate[1], result[LCurrPos], length(LBsonUpdate));

end;

{**************************************************************************}
Function TAlBaseMongoDBClient.BuildOPDELETEMessage(const requestID: integer;                            // identifier for this message
                                                   const responseTo: integer;                           // requestID from the original request (used in reponses from db)
                                                   const flags: integer;                                // bit vector
                                                   const fullCollectionName: ansiString;                // "dbname.collectionname"
                                                   const selector: ansiString): ansiString;             // query object.
var LCurrPos: integer;
    LMessageLength: Integer;
    LOPCode: integer;
    LZERO: integer;
    LBsonSelector: ansiString;
    LJSONDocument: TalJSONDocument;
begin

  //
  //  messageLength     requestID       responseTo        opCode                 ZERO
  //  [0][1][2][3]     [4][5][6][7]    [8][9][10][11]   [12][13][14][15]    [16][17][18][19]
  //
  //  fullCollectionName       flags              selector
  //  [20]...[23]            [24][25][26][27]   [27]...[35]
  //

  //init aBsonselector
  if (length(Selector) > sizeof(LMessageLength)) then ALMove(Selector[1], LMessageLength, sizeof(LMessageLength))
  else LMessageLength := 0;
  if (LMessageLength <> length(Selector)) or
     ((length(Selector) > 0) and
      (Selector[length(Selector)] <> #0)) then begin
    LJSONDocument := TALJsonDocument.Create;
    try
      LJSONDocument.LoadFromJSONString(Selector);
      LJSONDocument.SaveToBSONString(LBsonSelector);
    finally
      LJSONDocument.Free;
    end;
  end
  else begin
    if Selector = '' then LBsonSelector := #5#0#0#0#0 // empty BSON
    else LBsonSelector := Selector;
  end;

  // init aMessageLength
  LMessageLength := sizeof(LMessageLength) +
                    sizeof(requestID) +
                    sizeof(responseTo) +
                    sizeof(LOPCode) +
                    sizeof(LZERO) +
                    length(fullCollectionName) + 1{the trailing #0} +
                    sizeof(flags) +
                    length(LBsonSelector);

  //init the length of the result
  setlength(result, LMessageLength);
  LCurrPos := 1;

  // messageLength
  ALMove(LMessageLength, Result[LCurrPos], sizeof(LMessageLength));
  inc(LCurrPos,sizeof(LMessageLength));

  //requestID
  ALMove(requestID, Result[LCurrPos], sizeof(requestID));
  inc(LCurrPos,sizeof(requestID));

  //responseTo
  ALMove(responseTo, Result[LCurrPos], sizeof(responseTo));
  inc(LCurrPos,sizeof(responseTo));

  //opCode
  LOPCode := MONGO_OP_DELETE;
  ALMove(LOPCode, Result[LCurrPos], sizeof(LOPCode));
  inc(LCurrPos,sizeof(LOPCode));

  //ZERO
  LZERO := 0;
  ALMove(LZERO, Result[LCurrPos], sizeof(LZERO));
  inc(LCurrPos,sizeof(LZERO));

  //fullCollectionName
  if length(fullCollectionName) <= 0 then raise EALException.Create('FullCollectionName must not be empty');
  ALMove(fullCollectionName[1], result[LCurrPos], length(fullCollectionName));
  inc(LCurrPos,length(fullCollectionName));
  result[LCurrPos] := #0;
  inc(LCurrPos);

  //flags
  ALMove(flags, Result[LCurrPos], sizeof(flags));
  inc(LCurrPos,sizeof(flags));

  //aBsonselector
  ALMove(LBsonSelector[1], result[LCurrPos], length(LBsonSelector));

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
var LCurrPos: integer;
    LMessageLength: Integer;
    LOPCode: integer;
    LBsonQuery: ansiString;
    LBSONReturnFieldsSelector: ansiString;
    LJsonDocument: TALJsonDocument;
begin

  //
  //  messageLength     requestID       responseTo        opCode                 flags
  //  [0][1][2][3]     [4][5][6][7]    [8][9][10][11]   [12][13][14][15]    [16][17][18][19]
  //
  //  fullCollectionName       numberToSkip        numberToReturn          query     returnFieldsSelector
  //  [20]...[23]            [24][25][26][27]     [28][29][30][31]      [32]...[50]    [51]...[70]
  //

  //init aBsonQuery
  if (length(Query) > sizeof(LMessageLength)) then ALMove(Query[1], LMessageLength, sizeof(LMessageLength))
  else LMessageLength := 0;
  if (LMessageLength <> length(Query)) or
     ((length(Query) > 0) and
      (Query[length(Query)] <> #0)) then begin
    LJsonDocument := TALJsonDocument.Create;
    try
      LJsonDocument.LoadFromJSONString(Query);
      LJsonDocument.SaveToBSONString(LBsonQuery);
    finally
      LJsonDocument.Free;
    end;
  end
  else begin
    if Query = '' then LBsonQuery := #5#0#0#0#0 // empty BSON
    else LBsonQuery := Query;
  end;

  //init aBSONReturnFieldsSelector
  if (length(ReturnFieldsSelector) > sizeof(LMessageLength)) then ALMove(ReturnFieldsSelector[1], LMessageLength, sizeof(LMessageLength))
  else LMessageLength := 0;
  if (LMessageLength <> length(ReturnFieldsSelector)) or
     ((length(ReturnFieldsSelector) > 0) and
      (ReturnFieldsSelector[length(ReturnFieldsSelector)] <> #0)) then begin
    LJsonDocument := TALJsonDocument.Create;
    try
      LJsonDocument.LoadFromJSONString(ReturnFieldsSelector);
      LJsonDocument.SaveToBSONString(LBSONReturnFieldsSelector);
    finally
      LJsonDocument.Free;
    end;
  end
  else LBSONReturnFieldsSelector := ReturnFieldsSelector;

  // init aMessageLength
  LMessageLength := sizeof(LMessageLength) +
                    sizeof(requestID) +
                    sizeof(responseTo) +
                    sizeof(LOPCode) +
                    sizeof(flags) +
                    length(fullCollectionName) + 1{the trailing #0} +
                    sizeof(numberToSkip) +
                    sizeof(numberToReturn) +
                    length(LBsonQuery) +
                    length(LBSONReturnFieldsSelector);

  //init the length of the result
  setlength(result, LMessageLength);
  LCurrPos := 1;

  // messageLength
  ALMove(LMessageLength, Result[LCurrPos], sizeof(LMessageLength));
  inc(LCurrPos,sizeof(LMessageLength));

  //requestID
  ALMove(requestID, Result[LCurrPos], sizeof(requestID));
  inc(LCurrPos,sizeof(requestID));

  //responseTo
  ALMove(responseTo, Result[LCurrPos], sizeof(responseTo));
  inc(LCurrPos,sizeof(responseTo));

  //opCode
  LOPCode := MONGO_OP_QUERY;
  ALMove(LOPCode, Result[LCurrPos], sizeof(LOPCode));
  inc(LCurrPos,sizeof(LOPCode));

  //flags
  ALMove(flags, Result[LCurrPos], sizeof(flags));
  inc(LCurrPos,sizeof(flags));

  //fullCollectionName
  if length(fullCollectionName) <= 0 then raise EALException.Create('FullCollectionName must not be empty');
  ALMove(fullCollectionName[1], result[LCurrPos], length(fullCollectionName));
  inc(LCurrPos,length(fullCollectionName));
  result[LCurrPos] := #0;
  inc(LCurrPos);

  //numberToSkip
  ALMove(numberToSkip, Result[LCurrPos], sizeof(numberToSkip));
  inc(LCurrPos,sizeof(numberToSkip));

  //numberToReturn
  ALMove(numberToReturn, Result[LCurrPos], sizeof(numberToReturn));
  inc(LCurrPos,sizeof(numberToReturn));

  //aBsonQuery
  ALMove(LBsonQuery[1], result[LCurrPos], length(LBsonQuery));
  inc(LCurrPos,length(LBsonQuery));

  //aBSONReturnFieldsSelector
  if length(LBSONReturnFieldsSelector) > 0 then
    ALMove(LBSONReturnFieldsSelector[1], result[LCurrPos], length(LBSONReturnFieldsSelector));

end;

{***************************************************************************}
Function TAlBaseMongoDBClient.BuildOPGETMOREMessage(const requestID: integer;              // identifier for this message
                                                    const responseTo: integer;             // requestID from the original request (used in reponses from db)
                                                    const fullCollectionName: ansiString;  // "dbname.collectionname"
                                                    const numberToReturn: integer;         // number of documents to return in the first OP_REPLY batch
                                                    const cursorID: int64): AnsiString;
var LCurrPos: integer;
    LMessageLength: Integer;
    LOPCode: integer;
    LZero: integer;
begin

  //
  //  messageLength     requestID       responseTo        opCode                 ZERO
  //  [0][1][2][3]     [4][5][6][7]    [8][9][10][11]   [12][13][14][15]    [16][17][18][19]
  //
  //  fullCollectionName      numberToReturn                cursorID
  //  [20]...[23]            [24][25][26][27]     [28][29][30][31][32][33][34][35]
  //

  // init aMessageLength
  LMessageLength := sizeof(LMessageLength) +
                    sizeof(requestID) +
                    sizeof(responseTo) +
                    sizeof(LOPCode) +
                    sizeof(LZero) +
                    length(fullCollectionName) + 1{the trailing #0} +
                    sizeof(numberToReturn) +
                    sizeof(cursorID);

  //init the length of the result
  setlength(result, LMessageLength);
  LCurrPos := 1;

  // messageLength
  ALMove(LMessageLength, Result[LCurrPos], sizeof(LMessageLength));
  inc(LCurrPos,sizeof(LMessageLength));

  //requestID
  ALMove(requestID, Result[LCurrPos], sizeof(requestID));
  inc(LCurrPos,sizeof(requestID));

  //responseTo
  ALMove(responseTo, Result[LCurrPos], sizeof(responseTo));
  inc(LCurrPos,sizeof(responseTo));

  //opCode
  LOPCode := MONGO_OP_GET_MORE;
  ALMove(LOPCode, Result[LCurrPos], sizeof(LOPCode));
  inc(LCurrPos,sizeof(LOPCode));

  //Zero
  LZero := 0;
  ALMove(LZero, Result[LCurrPos], sizeof(LZero));
  inc(LCurrPos,sizeof(LZero));

  //fullCollectionName
  if length(fullCollectionName) <= 0 then raise EALException.Create('FullCollectionName must not be empty');
  ALMove(fullCollectionName[1], result[LCurrPos], length(fullCollectionName));
  inc(LCurrPos,length(fullCollectionName));
  result[LCurrPos] := #0;
  inc(LCurrPos);

  //numberToReturn
  ALMove(numberToReturn, Result[LCurrPos], sizeof(numberToReturn));
  inc(LCurrPos,sizeof(numberToReturn));

  //cursorID
  ALMove(cursorID, Result[LCurrPos], sizeof(cursorID));

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
Var LMessageLength: integer;
    LOPCode: integer;
    LCurrPos: integer;
    LDocumentStr: AnsiString;
    LNumberOfDocumentsFounded: Integer;
    LTmpRowTag: ansiString;
    LUpdateRowTagByFieldValue: Boolean;
    LJsonNode1: TALJSONNode;
    LJsonNode2: TALJSONNode;
begin

  //
  //  messageLength     requestID       responseTo        opCode              responseFlags
  //  [0][1][2][3]     [4][5][6][7]    [8][9][10][11]   [12][13][14][15]    [16][17][18][19]
  //
  //            cursorID                  startingFrom         numberReturned        documents
  //  [20][22][23][24][25][26][27]       [24][25][26][27]     [28][29][30][31]      [32]...[50]
  //

  // some checkc
  if length(OpReplyMsg) < sizeof(LMessageLength) +
                          sizeof(requestID) +
                          sizeof(responseTo) +
                          sizeof(LOPCode) +
                          sizeof(responseFlags) +
                          sizeof(cursorID) +
                          sizeof(startingFrom) +
                          sizeof(numberReturned) then raise EALException.Create('Wrong OP_REPLY message');

  // init aCurrPos
  LCurrPos := 1;

  // messageLength
  ALMove(OpReplyMsg[LCurrPos], LMessageLength, sizeof(LMessageLength));
  inc(LCurrPos,sizeof(LMessageLength));
  if LMessageLength <> length(OpReplyMsg) then raise EALException.Create('Wrong OP_REPLY message');

  // requestID
  ALMove(OpReplyMsg[LCurrPos], requestID, sizeof(requestID));
  inc(LCurrPos,sizeof(requestID));

  // responseTo
  ALMove(OpReplyMsg[LCurrPos], responseTo, sizeof(responseTo));
  inc(LCurrPos,sizeof(responseTo));

  // OPCode
  ALMove(OpReplyMsg[LCurrPos], LOPCode, sizeof(LOPCode));
  if LOPCode <> MONGO_OP_REPLY then raise EALException.Create('Wrong OP_REPLY message');
  inc(LCurrPos,sizeof(LOPCode));

  // responseFlags
  ALMove(OpReplyMsg[LCurrPos], responseFlags, sizeof(responseFlags));
  inc(LCurrPos,sizeof(responseFlags));

  //bit num	  name	            description
  //0	        CursorNotFound	  Set when getMore is called but the cursor id is not valid at the server. Returned with zero results.
  //1	        QueryFailure	    Set when query failed. Results consist of one document containing an “$err” field describing the failure.
  //2	        ShardConfigStale	Drivers should ignore this. Only mongos will ever see this set, in which case, it needs to update config from the server.
  //3        	AwaitCapable	    Set when the server supports the AwaitData Query option. If it doesn’t, a client should sleep a little between
  //                            getMore’s of a Tailable cursor. Mongod version 1.6 supports AwaitData and thus always sets AwaitCapable.
  //4-31    	Reserved	        Ignore
  if responseFlags and (1 shl 0) <> 0 then raise EALException.Create('Cursor Not Found');
  if responseFlags and (1 shl 1) <> 0 then raise EALException.Create('Query Failure');

  // cursorID
  ALMove(OpReplyMsg[LCurrPos], cursorID, sizeof(cursorID));
  inc(LCurrPos,sizeof(cursorID));

  // startingFrom
  ALMove(OpReplyMsg[LCurrPos], startingFrom, sizeof(startingFrom));
  inc(LCurrPos,sizeof(startingFrom));

  // numberReturned
  ALMove(OpReplyMsg[LCurrPos], numberReturned, sizeof(numberReturned));
  inc(LCurrPos,sizeof(numberReturned));

  // documents
  LNumberOfDocumentsFounded := 0;
  if LCurrPos <= LMessageLength then begin

    //init aUpdateRowTagByFieldValue and aTmpRowTag
    if AlPos('&>',RowTag) = 1 then begin
      LTmpRowTag := AlcopyStr(RowTag, 3, maxint);
      LUpdateRowTagByFieldValue := LTmpRowTag <> '';
    end
    else begin
      LTmpRowTag := RowTag;
      LUpdateRowTagByFieldValue := False;
    end;

    //loop on all node to add
    while True do begin
      if LCurrPos > length(OpReplyMsg) then break;
      if LCurrPos > length(OpReplyMsg) - sizeof(LMessageLength) + 1 then raise EALException.Create('Wrong OP_REPLY message');
      ALMove(OpReplyMsg[LCurrPos], LMessageLength, sizeof(LMessageLength));
      if LCurrPos + LMessageLength - 1 > length(OpReplyMsg) then raise EALException.Create('Wrong OP_REPLY message');
      setlength(LDocumentStr, LMessageLength);
      ALMove(OpReplyMsg[LCurrPos], LDocumentStr[1], LMessageLength);
      inc(LCurrPos,LMessageLength);
      if (LTmpRowTag <> '') or
         (documents.NodeType = ntarray) then LJsonNode1 := documents.AddChild(LTmpRowTag, ntobject)
      else LJsonNode1 := documents;
      LJsonNode1.LoadFromBSONString(LDocumentStr, False{ClearChildNodes});
      if LUpdateRowTagByFieldValue then begin
        LJsonNode2 := LJsonNode1.ChildNodes.FindNode(LTmpRowTag);
        if assigned(LJsonNode2) then LJsonNode1.NodeName := LJsonNode2.Text;
      end;
      if assigned(OnNewRowFunct) then begin
        Continue := True;
        OnNewRowFunct(LJsonNode1, ViewTag, ExtData, Continue);
        documents.ChildNodes.Clear;
        if Not Continue then exit;
      end;
      inc(LNumberOfDocumentsFounded);
    end;
  end;
  if LNumberOfDocumentsFounded <> numberReturned then raise EALException.Create('Wrong OP_REPLY message');

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
var LRequestID: integer;
    LResponseTo: integer;
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
                      LRequestID,
                      LResponseTo,
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
var LRequestID: integer;
    LResponseTo: integer;
begin
  ParseOPREPLYMessage(SendCmd(aSocketDescriptor,
                              BuildOPGETMOREMessage(0, //const requestID: integer;
                                                    0, //const responseTo: integer;
                                                    fullCollectionName,
                                                    numberToReturn,
                                                    cursorID),
                              True), // aGetResponse
                      LRequestID,
                      LResponseTo,
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
                                         var upserted: ansiString);              // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
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
var LUpdatedExisting: boolean;
    LUpserted: ansiString;
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
                       LUpdatedExisting,
                       LUpserted);
end;

{***********************************************************************************}
procedure TAlBaseMongoDBClient.OnSelectDataDone(const FullCollectionName: AnsiString;
                                                const Query: AnsiString;
                                                const ReturnFieldsSelector: AnsiString;
                                                flags: TALMongoDBClientSelectDataFlags;
                                                const RowTag: AnsiString;
                                                const ViewTag: AnsiString;
                                                Skip: integer;
                                                First: Integer;
                                                CacheThreshold: Integer;
                                                TimeTaken: double);
begin
  // virtual
end;

{*****************************************************************************}
procedure TAlBaseMongoDBClient.OnRunCommandDone(const DatabaseName: AnsiString;
                                                const Command: AnsiString;
                                                flags: TALMongoDBClientRunCommandFlags;
                                                const RowTag: AnsiString;
                                                const ViewTag: AnsiString;
                                                CacheThreshold: Integer;
                                                TimeTaken: double);
begin
  // virtual
end;

{***********************************************************************************}
procedure TAlBaseMongoDBClient.OnUpdateDataDone(const FullCollectionName: AnsiString;
                                                const selector: AnsiString;
                                                const update: AnsiString;
                                                flags: TALMongoDBClientUpdateDataFlags;
                                                TimeTaken: double);
begin
  // virtual
end;

{***********************************************************************************}
procedure TAlBaseMongoDBClient.OnDeleteDataDone(const FullCollectionName: AnsiString;
                                                const selector: AnsiString;
                                                flags: TALMongoDBClientDeleteDataFlags;
                                                TimeTaken: double);
begin
  // virtual
end;

{***********************************************************************************}
procedure TAlBaseMongoDBClient.OnInsertDataDone(const FullCollectionName: AnsiString;
                                                const documents: AnsiString;
                                                flags: TALMongoDBClientInsertDataFlags;
                                                TimeTaken: double);
begin
  // virtual
end;

{******************************************************************************************}
procedure TAlBaseMongoDBClient.OnFindAndModifyDataDone(const FullCollectionName: AnsiString;
                                                       const query: AnsiString;
                                                       const sort: AnsiString;
                                                       remove: boolean;
                                                       const update: AnsiString;
                                                       new: boolean;
                                                       const ReturnFieldsSelector: AnsiString;
                                                       InsertIfNotFound: boolean;
                                                       RowTag: AnsiString;
                                                       ViewTag: AnsiString;
                                                       TimeTaken: double);
begin
  // virtual
end;

{******************************************************************************************************}
Function TAlBaseMongoDBClient.SocketWrite(aSocketDescriptor: TSocket; const Buf; len: Integer): Integer;
begin
  Result := Send(aSocketDescriptor,Buf,len,0);
  CheckOSError(Result =  SOCKET_ERROR);
end;

{***************************************************************************************************}
function TAlBaseMongoDBClient.SocketRead(aSocketDescriptor: TSocket; var buf; len: Integer): Integer;
begin
  Result := Recv(aSocketDescriptor,buf,len,0);
  CheckOSError(Result = SOCKET_ERROR);
end;

{*****************************************************************}
function TAlBaseMongoDBClient.loadCachedData(const Key: AnsiString;
                                             var DataStr: AnsiString): Boolean;
begin
  result := false; //virtual need to be overriden
end;

{*******************************************************************}
Procedure TAlBaseMongoDBClient.SaveDataToCache(const Key: ansiString;
                                               const CacheThreshold: integer;
                                               const DataStr: ansiString);
begin
  //virtual need to be overriden
end;

{************************************************************************************************}
procedure TAlBaseMongoDBClient.DoSetSendTimeout(aSocketDescriptor: TSocket; const Value: integer);
begin
  CheckOSError(setsockopt(aSocketDescriptor,SOL_SOCKET,SO_SNDTIMEO,PAnsiChar(@Value),SizeOf(Value))=SOCKET_ERROR);
end;

{***************************************************************************************************}
procedure TAlBaseMongoDBClient.DoSetReceiveTimeout(aSocketDescriptor: TSocket; const Value: integer);
begin
  CheckOSError(setsockopt(aSocketDescriptor,SOL_SOCKET,SO_RCVTIMEO,PAnsiChar(@Value),SizeOf(Value))=SOCKET_ERROR);
end;

{*******************************************************************************************************************}
// http://blogs.technet.com/b/nettracer/archive/2010/06/03/things-that-you-may-want-to-know-about-tcp-keepalives.aspx
procedure TAlBaseMongoDBClient.DoSetKeepAlive(aSocketDescriptor: TSocket; const Value: boolean);
var LIntBool: integer;
begin
  // warning the winsock seam buggy because the getSockOpt return optlen = 1 (byte) intead of 4 (dword)
  // so the getSockOpt work only if aIntBool = byte ! (i see this on windows vista)
  // but this is only for getSockOpt, for setsockopt it's seam to work OK so i leave it like this
  if Value then LIntBool := 1
  else LIntBool := 0;
  CheckOSError(setsockopt(aSocketDescriptor,SOL_SOCKET,SO_KEEPALIVE,PAnsiChar(@LIntBool),SizeOf(LIntBool))=SOCKET_ERROR);
end;

{***************************************************************************************************************************************************************************************************************}
// https://access.redhat.com/site/documentation/en-US/Red_Hat_Enterprise_MRG/1.1/html/Realtime_Tuning_Guide/sect-Realtime_Tuning_Guide-Application_Tuning_and_Deployment-TCP_NODELAY_and_Small_Buffer_Writes.html
procedure TAlBaseMongoDBClient.DoSetTCPNoDelay(aSocketDescriptor: TSocket; const Value: boolean);
var LIntBool: integer;
begin
  // warning the winsock seam buggy because the getSockOpt return optlen = 1 (byte) intead of 4 (dword)
  // so the getSockOpt work only if aIntBool = byte ! (i see this on windows vista)
  // but this is only for getSockOpt, for setsockopt it's seam to work OK so i leave it like this
  if Value then LIntBool := 1
  else LIntBool := 0;
  CheckOSError(setsockopt(aSocketDescriptor,SOL_SOCKET,TCP_NODELAY,PAnsiChar(@LIntBool),SizeOf(LIntBool))=SOCKET_ERROR);
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

{*************************************************************************}
Procedure TAlMongoDBClient.SelectData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                            // name with the collection name, using a . for the concatenation. For example, for the database
                                                                            // foo and the collection bar, the full collection name is foo.bar
                                      const Query: AnsiString; // BSON document that represents the query. The query will contain one or more elements,
                                                               // all of which must match for a document to be included in the result set. Possible elements
                                                               // include $query, $orderby, $hint, $explain, and $snapshot.
                                      const ReturnFieldsSelector: AnsiString; // Optional. BSON document that limits the fields in the returned documents.
                                                                              // The returnFieldsSelector contains one or more elements, each of which is the name
                                                                              // of a field that should be returned, and and the integer value 1. In JSON notation,
                                                                              // a returnFieldsSelector to limit to the fields a, b and c would be:
                                                                              // { a : 1, b : 1, c : 1}
                                      flags: TALMongoDBClientSelectDataFlags; // Options (see TALMongoDBClientSelectDataFlags)
                                      const RowTag: AnsiString; // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                                      const ViewTag: AnsiString; // the node name under with all records will be stored in the JSON/XML result document.
                                      Skip: integer; // Sets the number of documents to omit
                                      First: Integer; // Limits the number of documents to retrieve
                                      CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                               // cache or not. Values <= 0 deactivate the cache
                                      JSONDATA: TALJSONNode;
                                      OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer);

Var LViewRec: TalJSONNode;
    LJSONDocument: TalJSONDocument;
    LResponseFlags: integer;
    LCursorID: int64;
    LStartingFrom: integer;
    LNumberReturned: integer;
    LFlags: integer;
    LRecAdded: integer;
    LContinue: boolean;
    LStopWatch: TStopWatch;
    LCacheKey: ansiString;
    LCacheStr: ansiString;

begin

  //Error if we are not connected
  If not connected then raise EALException.Create('Not connected');

  //only OnNewRowFunct / JSONDATA can be used
  if assigned(OnNewRowFunct) then JSONDATA := nil;

  //clear the JSONDATA
  if assigned(JSONDATA) then LJSONDocument := Nil
  else begin
    LJSONDocument := TALJSONDocument.create;
    JSONDATA := LJSONDocument.Node;
  end;

  Try

    //init the TstopWatch
    LStopWatch := TstopWatch.Create;

    //Handle the CacheThreshold
    LCacheKey := '';
    If (CacheThreshold > 0) and
       (not assigned(LJSONDocument)) and
       (not (sfTailMonitoring in flags)) and
       ((JSONDATA.ChildNodes.Count = 0) or  // else the save will not work
        (ViewTag <> '')) then begin

      //try to load from from cache
      LCacheKey := ALStringHashSHA1(
                     ALFormat('%s#%d#%d#%s#%s#%s',
                              [RowTag,
                               Skip,
                               First,
                               FullCollectionName,
                               ReturnFieldsSelector,
                               Query]));

      if loadcachedData(LCacheKey, LCacheStr) then begin

        //init the aViewRec
        if (ViewTag <> '') then LViewRec := JSONDATA.AddChild(ViewTag, ntobject)
        else LViewRec := JSONDATA;

        //assign the tmp data to the XMLData
        LViewRec.LoadFromJsonString(LCacheStr, false{ClearChildNodes});

        //exit
        exit;

      end;

    end;

    //start the TstopWatch
    LStopWatch.Reset;
    LStopWatch.Start;

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
    LFlags := 0;
    if sfTailMonitoring in flags then begin
      if not assigned(OnNewRowFunct) then raise EALException.Create('OnNewRowFunct is mandatory for tail monitoring');
      LFlags := LFlags or (1 shl 1); // TailableCursor
      LFlags := LFlags or (1 shl 5); // AwaitData
    end;
    if sfSlaveOk in flags then LFlags := LFlags or (1 shl 2);
    if sfNoCursorTimeout in flags then LFlags := LFlags or (1 shl 4);
    if sfPartial in flags then LFlags := LFlags or (1 shl 7);

    //init the aViewRec
    if (ViewTag <> '') and (not assigned(LJSONDocument)) then LViewRec := JSONdata.AddChild(ViewTag, ntobject)
    else LViewRec := JSONdata;

    //init aRecAdded
    LRecAdded := 0;

    //for the TailMonitoring
    repeat

      //do the First query
      LContinue := true;
      OP_QUERY(fSocketDescriptor,
               LFlags,
               fullCollectionName,
               ALIfThen(Skip >= 0, Skip, 0),
               ALIfThen(First >= 0, First, 0), // The MongoDB server returns the query results in batches. Batch size will not exceed
                                               // the maximum BSON document size. For most queries, the first batch returns 101
                                               // documents or just enough documents to exceed 1 megabyte. Subsequent batch size is
                                               // 4 megabytes. To override the default size of the batch, see batchSize() and limit().
               query,
               ReturnFieldsSelector,
               LResponseFlags,
               LCursorID,
               LStartingFrom, // where in the cursor this reply is starting
               LNumberReturned, // number of documents in the reply
               LViewRec,
               OnNewRowFunct,
               ExtData,
               RowTag,
               ViewTag,
               LContinue);

      try

        //init aRecAdded
        LRecAdded := LRecAdded + LNumberReturned;

        //loop still the cursorID > 0
        while ((not (sfTailMonitoring in flags)) or
               (not fStopTailMonitoring)) and
              (LContinue) and
              (LCursorID <> 0) and
              ((First <= 0) or
               (LRecAdded < First)) do begin

          //Get more data
          OP_GET_MORE(fSocketDescriptor,
                      fullCollectionName,
                      ALIfThen(First > 0, First - LRecAdded, 0),
                      LCursorID,
                      LResponseFlags,
                      LStartingFrom,
                      LNumberReturned,
                      LViewRec,
                      OnNewRowFunct,
                      ExtData,
                      RowTag,
                      ViewTag,
                      LContinue);

          //init aRecAdded
          LRecAdded := LRecAdded + LNumberReturned;

        end;

      finally

        //close the curson
        if LCursorID <> 0 then OP_KILL_CURSORS(fSocketDescriptor, [LCursorID]);

      end;

      //sleep for TailMonitoring to not use 100% CPU
      if (sfTailMonitoring in flags) and
         (not fStopTailMonitoring) and
         ((First <= 0) or
          (LRecAdded < First)) then sleep(1);

    //loop for the TailMonitoring
    until (not (sfTailMonitoring in flags)) or
          (fStopTailMonitoring) or
          ((First > 0) and
           (LRecAdded >= First));

    //do the OnSelectDataDone
    LStopWatch.Stop;
    OnSelectDataDone(FullCollectionName,
                     Query,
                     ReturnFieldsSelector,
                     flags,
                     RowTag,
                     ViewTag,
                     Skip,
                     First,
                     CacheThreshold,
                     LStopWatch.Elapsed.TotalMilliseconds);

    //save to the cache
    If LCacheKey <> '' then begin

      //save the data
      LViewRec.SaveToJSONString(LCacheStr);
      SaveDataToCache(LCacheKey,
                      CacheThreshold,
                      LCacheStr);

    end;

  Finally
    if assigned(LJSONDocument) then LJSONDocument.free;
  End;

end;

{*************************************************************************}
Procedure TAlMongoDBClient.SelectData(const FullCollectionName: AnsiString;
                                      const Query: AnsiString;
                                      const ReturnFieldsSelector: AnsiString;
                                      flags: TALMongoDBClientSelectDataFlags;
                                      Skip: integer;
                                      First: Integer;
                                      OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             flags,
             '', // RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             nil, //JSONDATA,
             OnNewRowFunct,
             ExtData);
end;

{*************************************************************************}
Procedure TAlMongoDBClient.SelectData(const FullCollectionName: AnsiString;
                                      const Query: AnsiString;
                                      const ReturnFieldsSelector: AnsiString;
                                      flags: TALMongoDBClientSelectDataFlags;
                                      OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             flags,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             nil, // JSONDATA,
             OnNewRowFunct,
             ExtData);
end;

{*************************************************************************}
Procedure TAlMongoDBClient.SelectData(const FullCollectionName: AnsiString;
                                      const Query: AnsiString;
                                      const ReturnFieldsSelector: AnsiString;
                                      flags: TALMongoDBClientSelectDataFlags;
                                      const RowTag: AnsiString;
                                      Skip: integer;
                                      First: Integer;
                                      JSONDATA: TALJSONNode);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             flags,
             RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewRowFunct,
             nil); // ExtData
end;

{*************************************************************************}
Procedure TAlMongoDBClient.SelectData(const FullCollectionName: AnsiString;
                                      const Query: AnsiString;
                                      const ReturnFieldsSelector: AnsiString;
                                      flags: TALMongoDBClientSelectDataFlags;
                                      const RowTag: AnsiString;
                                      JSONDATA: TALJSONNode);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             flags,
             RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewRowFunct,
             nil); // ExtData
end;

{*************************************************************************}
Procedure TAlMongoDBClient.SelectData(const FullCollectionName: AnsiString;
                                      const Query: AnsiString;
                                      const ReturnFieldsSelector: AnsiString;
                                      flags: TALMongoDBClientSelectDataFlags;
                                      JSONDATA: TALJSONNode);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             flags,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewRowFunct,
             nil); // ExtData
end;

{*************************************************************************}
Procedure TAlMongoDBClient.SelectData(const FullCollectionName: AnsiString;
                                      const Query: AnsiString;
                                      const ReturnFieldsSelector: AnsiString;
                                      Skip: integer;
                                      First: Integer;
                                      OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             [],
             '', // RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             nil, //JSONDATA,
             OnNewRowFunct,
             ExtData);
end;

{*************************************************************************}
Procedure TAlMongoDBClient.SelectData(const FullCollectionName: AnsiString;
                                      const Query: AnsiString;
                                      const ReturnFieldsSelector: AnsiString;
                                      OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                      ExtData: Pointer);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             [],
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             nil, // JSONDATA,
             OnNewRowFunct,
             ExtData);
end;

{*************************************************************************}
Procedure TAlMongoDBClient.SelectData(const FullCollectionName: AnsiString;
                                      const Query: AnsiString;
                                      const ReturnFieldsSelector: AnsiString;
                                      const RowTag: AnsiString;
                                      Skip: integer;
                                      First: Integer;
                                      JSONDATA: TALJSONNode);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             [],
             RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewRowFunct,
             nil); // ExtData
end;

{*************************************************************************}
Procedure TAlMongoDBClient.SelectData(const FullCollectionName: AnsiString;
                                      const Query: AnsiString;
                                      const ReturnFieldsSelector: AnsiString;
                                      const RowTag: AnsiString;
                                      JSONDATA: TALJSONNode);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             [],
             RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewRowFunct,
             nil); // ExtData
end;

{*************************************************************************}
Procedure TAlMongoDBClient.SelectData(const FullCollectionName: AnsiString;
                                      const Query: AnsiString;
                                      const ReturnFieldsSelector: AnsiString;
                                      JSONDATA: TALJSONNode);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             [],
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewRowFunct,
             nil); // ExtData
end;

{*******************************************************************}
Procedure TAlMongoDBClient.RunCommand(const DatabaseName: AnsiString;
                                      const Command: AnsiString; // BSON document that represents the Command
                                      flags: TALMongoDBClientRunCommandFlags; // Options (see TALMongoDBClientRunCommandFlags)
                                      const RowTag: AnsiString; // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                                      const ViewTag: AnsiString; // the node name under with all records will be stored in the JSON/XML result document.
                                      CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                               // cache or not. Values <= 0 deactivate the cache
                                      JSONDATA: TALJSONNode;
                                      OnNewBatchRowFunct: TALMongoDBClientRunCommandOnNewBatchRowFunct;
                                      ExtData: Pointer);

Var LViewRec: TalJSONNode;
    LRowRec: TalJSONNode;
    LCursorRec: TalJsonNode;
    LFirstBatchRec: TalJsonNode;
    LJSONDocument: TalJSONDocument;
    LResponseFlags: integer;
    LCursorID: int64;
    LStartingFrom: integer;
    LNumberReturned: integer;
    LFlags: integer;
    LContinue: boolean;
    LStopWatch: TStopWatch;
    LCacheKey: ansiString;
    LCacheStr: ansiString;
    LFullCollectionName: ansiString;
    i: integer;

begin

  //Error if we are not connected
  If not connected then raise EALException.Create('Not connected');

  //init aFullCollectionName
  LFullCollectionName := DatabaseName + '.$cmd';

  //only OnNewBatchRowFunct / JSONDATA can be used
  if assigned(OnNewBatchRowFunct) then JSONDATA := nil;

  //clear the JSONDATA
  if assigned(JSONDATA) then LJSONDocument := Nil
  else begin
    LJSONDocument := TALJSONDocument.create;
    JSONDATA := LJSONDocument.Node;
  end;

  try

    //init the TstopWatch
    LStopWatch := TstopWatch.Create;

    //Handle the CacheThreshold
    LCacheKey := '';
    If (CacheThreshold > 0) and
       (not assigned(LJSONDocument)) and
       ((JSONDATA.ChildNodes.Count = 0) or  // else the save will not work
        (ViewTag <> '')) then begin

      //try to load from from cache
      LCacheKey := ALStringHashSHA1(
                     ALFormat('%s#%d#%d#%s#%s#%s',
                              [RowTag,
                               0,
                               1,
                               LFullCollectionName,
                               '',
                               Command]));

      if loadcachedData(LCacheKey, LCacheStr) then begin

        //init the aViewRec
        if (ViewTag <> '') then LViewRec := JSONDATA.AddChild(ViewTag, ntobject)
        else LViewRec := JSONDATA;

        //assign the tmp data to the XMLData
        LViewRec.LoadFromJsonString(LCacheStr, false{ClearChildNodes});

        //exit
        exit;

      end;

    end;

    //start the TstopWatch
    LStopWatch.Reset;
    LStopWatch.Start;

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
    LFlags := 0;
    if cfSlaveOk in flags then LFlags := LFlags or (1 shl 2);
    if cfNoCursorTimeout in flags then LFlags := LFlags or (1 shl 4);
    if cfPartial in flags then LFlags := LFlags or (1 shl 7);

    //init the aViewRec
    if (ViewTag <> '') and (not assigned(LJSONDocument)) then LViewRec := JSONdata.AddChild(ViewTag, ntobject)
    else LViewRec := JSONdata;

    //do the First query
    LContinue := true;
    OP_QUERY(fSocketDescriptor,
             LFlags,
             LFullCollectionName,
             0,
             1,
             Command,
             '', // ReturnFieldsSelector
             LResponseFlags,
             LCursorID,
             LStartingFrom, // where in the cursor this reply is starting
             LNumberReturned, // number of documents in the reply
             LViewRec,
             nil,
             ExtData,
             RowTag,
             ViewTag,
             LContinue);

    //aCursorID must be egual to 0 (we retrieve only one doc) but in case
    if LCursorID <> 0 then OP_KILL_CURSORS(fSocketDescriptor, [LCursorID]);

    //init LRowRec
    if (RowTag <> '') and (LViewRec.ChildNodes.Count > 0) then LRowRec := LViewRec.ChildNodes[LViewRec.ChildNodes.Count - 1]
    else LRowRec := LViewRec;

    //check error
    CheckRunCommandResponse(LRowRec);

    //{
    //  "cursor" : {
    //      "id" : NumberLong(3286429289449024015),
    //      "ns" : "mydatabase.$cmd.listCollections",
    //      "firstBatch" : [
    //          {
    //            ....
    //          },
    //          {
    //            ....
    //          }
    //      ]
    //  },
    //  "ok" : 1.0
    //}
    LCursorRec := LRowRec.ChildNodes.FindNode('cursor');
    if LCursorRec <> nil then begin

      //init LFirstBatchRec
      LFirstBatchRec := LCursorRec.ChildNodes.FindNode('firstBatch');
      if (LFirstBatchRec <> nil) and
         (assigned(OnNewBatchRowFunct)) and
         (LContinue) then begin
        for I := 0 to LFirstBatchRec.ChildNodes.Count - 1 do begin
          OnNewBatchRowFunct(LFirstBatchRec.ChildNodes[I], // JSONRowData: TALJSONNode;
                             ViewTag, // const ViewTag: AnsiString;
                             ExtData, // ExtData: Pointer;
                             LContinue); // Var Continue: Boolean);
          if not LContinue then break;
        end;
      end;

      //init LCursorID
      LCursorID := LCursorRec.getChildNodeValueInt64('id', 0);
      if LCursorID <> 0 then begin

        try

          //we do the job only if LFirstBatchRec exist
          if LFirstBatchRec <> nil then begin

            //init LCollectionName
            LFullCollectionName := LCursorRec.getChildNodeValueText('ns', '');

            //loop still the cursorID > 0
            while (LContinue) and
                  (LCursorID <> 0) do begin

              //Get more data
              OP_GET_MORE(fSocketDescriptor,
                          LFullCollectionName,
                          0, // numberToReturn
                          LCursorID,
                          LResponseFlags,
                          LStartingFrom,
                          LNumberReturned,
                          LFirstBatchRec, // documents
                          OnNewBatchRowFunct,
                          ExtData,
                          '', // RowTag,
                          ViewTag,
                          LContinue);

            end;

          end;

        finally

          //close the curson
          if LCursorID <> 0 then OP_KILL_CURSORS(fSocketDescriptor, [LCursorID]);

        end;

        LCursorRec.SetChildNodeValueInt64('id', 0);

      end;

    end;

    //do the OnRunCommandDone
    LStopWatch.Stop;
    OnRunCommandDone(DatabaseName,
                     Command,
                     flags,
                     RowTag,
                     ViewTag,
                     CacheThreshold,
                     LStopWatch.Elapsed.TotalMilliseconds);

    //save to the cache
    If LCacheKey <> '' then begin

      //save the data
      LViewRec.SaveToJSONString(LCacheStr);
      SaveDataToCache(LCacheKey,
                      CacheThreshold,
                      LCacheStr);

    end;

  finally
    if assigned(LJSONDocument) then LJSONDocument.free;
  end;

end;

{*******************************************************************}
Procedure TAlMongoDBClient.RunCommand(const DatabaseName: AnsiString;
                                      const Command: AnsiString;
                                      flags: TALMongoDBClientRunCommandFlags;
                                      OnNewBatchRowFunct: TALMongoDBClientRunCommandOnNewBatchRowFunct;
                                      ExtData: Pointer);
begin
  RunCommand(DatabaseName,
             Command,
             flags,
             '', // RowTag,
             '', // ViewTag,
             -1, // CacheThreshold,
             nil, //JSONDATA,
             OnNewBatchRowFunct,
             ExtData);
end;

{*******************************************************************}
Procedure TAlMongoDBClient.RunCommand(const DatabaseName: AnsiString;
                                      const Command: AnsiString;
                                      flags: TALMongoDBClientRunCommandFlags;
                                      const RowTag: AnsiString;
                                      JSONDATA: TALJSONNode);
begin
  RunCommand(DatabaseName,
             Command,
             flags,
             RowTag,
             '', // ViewTag,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewBatchRowFunct,
             nil); // ExtData
end;

{*******************************************************************}
Procedure TAlMongoDBClient.RunCommand(const DatabaseName: AnsiString;
                                      const Command: AnsiString;
                                      flags: TALMongoDBClientRunCommandFlags;
                                      JSONDATA: TALJSONNode);
begin
  RunCommand(DatabaseName,
             Command,
             flags,
             '', // RowTag,
             '', // ViewTag,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewBatchRowFunct,
             nil); // ExtData
end;

{*******************************************************************}
Procedure TAlMongoDBClient.RunCommand(const DatabaseName: AnsiString;
                                      const Command: AnsiString;
                                      OnNewBatchRowFunct: TALMongoDBClientRunCommandOnNewBatchRowFunct;
                                      ExtData: Pointer);
begin
  RunCommand(DatabaseName,
             Command,
             [],
             '', // RowTag,
             '', // ViewTag,
             -1, // CacheThreshold,
             nil, //JSONDATA,
             OnNewBatchRowFunct,
             ExtData);
end;

{*******************************************************************}
Procedure TAlMongoDBClient.RunCommand(const DatabaseName: AnsiString;
                                      const Command: AnsiString;
                                      const RowTag: AnsiString;
                                      JSONDATA: TALJSONNode);
begin
  RunCommand(DatabaseName,
             Command,
             [],
             RowTag,
             '', // ViewTag,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewBatchRowFunct,
             nil); // ExtData
end;

{*******************************************************************}
Procedure TAlMongoDBClient.RunCommand(const DatabaseName: AnsiString;
                                      const Command: AnsiString;
                                      JSONDATA: TALJSONNode);
begin
  RunCommand(DatabaseName,
             Command,
             [],
             '', // RowTag,
             '', // ViewTag,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewBatchRowFunct,
             nil); // ExtData
end;

{*************************************************************************}
Procedure TAlMongoDBClient.UpdateData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                            // name with the collection name, using a . for the concatenation. For example, for the database
                                                                            // foo and the collection bar, the full collection name is foo.bar
                                      const selector: AnsiString; // BSON document that specifies the query for selection of the document to update.
                                      const update: AnsiString; // BSON document that specifies the update to be performed. For information on specifying updates see
                                                                // http://docs.mongodb.org/manual/tutorial/modify-documents/
                                      flags: TALMongoDBClientUpdateDataFlags; // Options (see TALMongoDBClientUpdateDataFlags)
                                      var NumberOfDocumentsUpdated: integer; // reports the number of documents updated
                                      var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                      var ObjectID: ansiString); // an ObjectId that corresponds to the upserted document if the update resulted in an insert.
Var LFlags: integer;
    LStopWatch: TStopWatch;
begin

  //Error if we are not connected
  If not connected then raise EALException.Create('Not connected');

  //init the TstopWatch
  LStopWatch := TstopWatch.StartNew;

  //bit num	  name	        description
  //0	        Upsert	      If set, the database will insert the supplied object into the collection if no matching document is found.
  //1	        MultiUpdate	  If set, the database will update all matching objects in the collection. Otherwise only updates first matching doc.
  //2-31    	Reserved	    Must be set to 0.
  LFlags := 0;
  if ufUpsert in flags then LFlags := LFlags or (1 shl 0);
  if ufMultiUpdate in flags then LFlags := LFlags or (1 shl 1);

  OP_UPDATE(fSocketDescriptor,
            LFlags,
            FullCollectionName,
            selector,
            Update,
            NumberOfDocumentsUpdated,
            updatedExisting,
            ObjectID);

  //do the OnUpdateDataDone
  LStopWatch.Stop;
  OnUpdateDataDone(FullCollectionName,
                   selector,
                   update,
                   flags,
                   LStopWatch.Elapsed.TotalMilliseconds);

end;

{*************************************************************************}
Procedure TAlMongoDBClient.UpdateData(const FullCollectionName: AnsiString;
                                      const Selector: AnsiString;
                                      const Update: AnsiString;
                                      flags: TALMongoDBClientUpdateDataFlags);
var LNumberOfDocumentsUpdated: integer;
    LUpdatedExisting: boolean;
    LObjectID: ansiString;
begin
  UpdateData(FullCollectionName,
             selector,
             update,
             flags,
             LNumberOfDocumentsUpdated,
             LUpdatedExisting,
             LObjectID);
end;

{*************************************************************************}
Procedure TAlMongoDBClient.UpdateData(const FullCollectionName: AnsiString;
                                      const Selector: AnsiString;
                                      const Update: AnsiString);
var LNumberOfDocumentsUpdated: integer;
    LUpdatedExisting: boolean;
    LObjectID: ansiString;
begin
  UpdateData(FullCollectionName,
             selector,
             update,
             [],
             LNumberOfDocumentsUpdated,
             LUpdatedExisting,
             LObjectID);
end;

{*************************************************************************}
Procedure TAlMongoDBClient.InsertData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                            // name with the collection name, using a . for the concatenation. For example, for the database
                                                                            // foo and the collection bar, the full collection name is foo.bar
                                      const documents: AnsiString; // One or more documents to insert into the collection. If there are more than one, they are written in sequence, one after another.
                                      flags: TALMongoDBClientInsertDataFlags); // Options (see TALMongoDBClientInsertDataFlags)
Var LFlags: integer;
    LStopWatch: TStopWatch;
begin

  //Error if we are not connected
  If not connected then raise EALException.Create('Not connected');

  //init the TstopWatch
  LStopWatch := TstopWatch.Create;

  //start the TstopWatch
  LStopWatch.Reset;
  LStopWatch.Start;

  //bit num	 name	            description
  //0	       ContinueOnError	If set, the database will not stop processing a bulk insert if one fails (eg due to duplicate IDs).
  //                          This makes bulk insert behave similarly to a series of single inserts, except lastError will be set if
  //                          any insert fails, not just the last one. If multiple errors occur, only the most recent will be
  //                          reported by getLastError. (new in 1.9.1)
  //1-31	   Reserved	        Must be set to 0.
  LFlags := 0;
  if ifContinueOnError in flags then LFlags := LFlags or (1 shl 0);

  OP_INSERT(fSocketDescriptor,
            LFlags,
            FullCollectionName,
            documents);

  //do the OnInsertDataDone
  LStopWatch.Stop;
  OnInsertDataDone(FullCollectionName,
                   documents,
                   flags,
                   LStopWatch.Elapsed.TotalMilliseconds);

end;

{*************************************************************************}
Procedure TAlMongoDBClient.InsertData(const FullCollectionName: AnsiString;
                                      const documents: AnsiString);
begin
  InsertData(FullCollectionName,
             documents,
             []);
end;

{*************************************************************************}
Procedure TAlMongoDBClient.DeleteData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                            // name with the collection name, using a . for the concatenation. For example, for the database
                                                                            // foo and the collection bar, the full collection name is foo.bar
                                      const selector: AnsiString; // BSON document that represent the query used to select the documents to be removed
                                                                  // The selector will contain one or more elements, all of which must match for a document
                                                                  // to be removed from the collection
                                      flags: TALMongoDBClientDeleteDataFlags; // Options (see TALMongoDBClientDeleteDataFlags)
                                      var NumberOfDocumentsRemoved: integer);
Var LFlags: integer;
    LStopWatch: TStopWatch;
begin

  //Error if we are not connected
  If not connected then raise EALException.Create('Not connected');

  //init the TstopWatch
  LStopWatch := TstopWatch.StartNew;

  //bit num	   name	          description
  //0	         SingleRemove	  If set, the database will remove only the first matching document in the collection. Otherwise all matching documents will be removed.
  //1-31	     Reserved	      Must be set to 0.
  LFlags := 0;
  if dfSingleRemove in flags then LFlags := LFlags or (1 shl 0);

  OP_DELETE(fSocketDescriptor,
            LFlags,
            FullCollectionName,
            selector,
            NumberOfDocumentsRemoved);

  //do the OnDeleteDataDone
  LStopWatch.Stop;
  OnDeleteDataDone(FullCollectionName,
                   selector,
                   flags,
                   LStopWatch.Elapsed.TotalMilliseconds);

end;

{*************************************************************************}
Procedure TAlMongoDBClient.DeleteData(const FullCollectionName: AnsiString;
                                      const Selector: AnsiString;
                                      flags: TALMongoDBClientDeleteDataFlags);
var LNumberOfDocumentsRemoved: integer;
begin
  DeleteData(FullCollectionName,
             selector,
             flags,
             LNumberOfDocumentsRemoved);
end;

{*************************************************************************}
Procedure TAlMongoDBClient.DeleteData(const FullCollectionName: AnsiString;
                                      const Selector: AnsiString);
var LNumberOfDocumentsRemoved: integer;
begin
  DeleteData(FullCollectionName,
             selector,
             [],
             LNumberOfDocumentsRemoved);
end;

{********************************************************************************}
Procedure TAlMongoDBClient.FindAndModifyData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                                   // name with the collection name, using a . for the concatenation. For example, for the database
                                                                                   // foo and the collection bar, the full collection name is foo.bar
                                             const query: AnsiString; // Optional. The selection criteria for the modification. The query field employs the same query selectors as used in the
                                                                      // db.collection.find() method. Although the query may match multiple documents, findAndModify will select only
                                                                      // one document to modify.
                                             const sort: AnsiString; // Optional. Determines which document the operation modifies if the query selects multiple
                                                                     // documents. findAndModify modifies the first document in the sort order specified by this argument.
                                             remove: boolean; // Must specify either the remove or the update field. Removes the document specified in the query field.
                                                              // Set this to true to remove the selected document . The default is false.
                                             const update: AnsiString; // Must specify either the remove or the update field. Performs an update of the selected document.
                                                                       // The update field employs the same update operators or field: value specifications to modify the selected document.
                                             new: boolean; // Optional. When true, returns the modified document rather than the original. The findAndModify method
                                                           // ignores the new option for remove operations. The default is false.
                                             const ReturnFieldsSelector: AnsiString; // Optional. A subset of fields to return. The fields document specifies an inclusion of a
                                                                                     // field with 1, as in: fields: { <field1>: 1, <field2>: 1, ... }.
                                             InsertIfNotFound: boolean;  // Optional. Used in conjunction with the update field. When true, findAndModify
                                                                         // creates a new document if no document matches the query, or if documents match the query,
                                                                         // findAndModify performs an update. The default is false.
                                             const RowTag: AnsiString; // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                                             const ViewTag: AnsiString; // the node name under with all records will be stored in the JSON/XML result document.
                                             JSONDATA: TALJSONNode;
                                             var NumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed
                                             var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                             var ObjectID: AnsiString); // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.

Var LStopWatch: TStopWatch;
    LDatabaseName: AnsiString;
    LCollectionName: AnsiString;
    LViewRec: TalJSONNode;
    LResponseFlags: integer;
    LTmpQuery: ansiString;
    LCursorID: int64;
    LStartingFrom: integer;
    LNumberReturned: integer;
    LContinue: boolean;
    LTmpRowTag: ansiString;
    LUpdateRowTagByFieldValue: Boolean;
    LJSONDoc: TALJSONDocument;
    LNode1: TALJSONNode;
    LNode2: TALJSONNode;
    LLastErrorObjectNode: TALJSONNode;
    P1: integer;

begin

  //Error if we are not connected
  If not connected then raise EALException.Create('Not connected');

  //init the TstopWatch
  LStopWatch := TstopWatch.StartNew;

  //create a temp JSONDoc
  LJSONDoc := TALJSONDocument.Create;
  try

    //init aDatabaseName and aCollectionName
    P1 := AlPos('.',FullCollectionName);
    if P1 <= 0 then raise EALException.Create('The full collection name must be the concatenation of the database name with the collection name');
    LDatabaseName := ALCopyStr(FullCollectionName, 1, P1-1);
    LCollectionName := ALCopyStr(FullCollectionName, P1+1, maxint);

    //buid the query
    LTmpQuery := '{"findAndModify":' + ALJsonEncodeTextWithNodeSubTypeHelper(LCollectionName);
    if query <> '' then LTmpQuery := LTmpQuery + ',"query":'+query;
    if sort <> '' then LTmpQuery := LTmpQuery + ',"sort":'+sort;
    LTmpQuery := LTmpQuery + ',"remove":'+ALJsonEncodeBooleanWithNodeSubTypeHelper(remove);
    if update <> '' then LTmpQuery := LTmpQuery + ',"update":'+update;
    LTmpQuery := LTmpQuery + ',"new":' + ALJsonEncodeBooleanWithNodeSubTypeHelper(new);
    if ReturnFieldsSelector <> '' then LTmpQuery := LTmpQuery + ',"fields":'+ReturnFieldsSelector;
    LTmpQuery := LTmpQuery + ',"upsert":'+ALJsonEncodeBooleanWithNodeSubTypeHelper(InsertIfNotFound);
    LTmpQuery := LTmpQuery + '}';

    //do the First query
    OP_QUERY(fSocketDescriptor,
             0, // flags
             LDatabaseName  + '.$cmd', // fullCollectionName
             0, // skip
             1, // First
             LTmpQuery, // query
             '', // ReturnFieldsSelector,
             LResponseFlags,
             LCursorID,
             LStartingFrom,
             LNumberReturned,
             LJSONDoc.Node,
             nil,
             nil,
             '',
             '',
             LContinue);
    try

      // exemple of returned json result

      //{
      // "value":{"_id":2,"seq":3},
      // "lastErrorObject":{
      //   "updatedExisting":true,
      //   "n":1
      // },
      // "ok":1
      //}

      //{
      // "value":null,
      // "lastErrorObject":{
      //   "updatedExisting":false,
      //   "n":1,
      //   "upserted":2
      // },
      // "ok":1
      //}

      //{
      // "ok":0,
      // "errmsg":"remove and returnNew can\u0027t co-exist"
      //}

      //init the aViewRec
      if (ViewTag <> '') then LViewRec := JSONdata.AddChild(ViewTag, ntobject)
      else LViewRec := JSONdata;

      //check error
      CheckRunCommandResponse(LJSONDoc.Node);

      //get the value node
      LNode1 := LJSONDoc.Node.ChildNodes.FindNode('value');
      if assigned(LNode1) and (LNode1.NodeType = ntObject) then begin

        //init aUpdateRowTagByFieldValue and aTmpRowTag
        if AlPos('&>',RowTag) = 1 then begin
          LTmpRowTag := AlcopyStr(RowTag, 3, maxint);
          LUpdateRowTagByFieldValue := LTmpRowTag <> '';
        end
        else begin
          LTmpRowTag := RowTag;
          LUpdateRowTagByFieldValue := False;
        end;

        //add the row tag
        if (LTmpRowTag <> '') or
           (LViewRec.NodeType = ntarray) then LViewRec := LViewRec.AddChild(LTmpRowTag, ntobject);

        //move the node
        while LNode1.ChildNodes.Count > 0 do begin
          LNode2 := LNode1.ChildNodes.Extract(0);
          try
            LViewRec.ChildNodes.Add(LNode2);
          except
            LNode2.Free;
            raise;
          end;
        end;

        // aUpdateRowTagByFieldValue
        if LUpdateRowTagByFieldValue then begin
          LNode1 := LViewRec.ChildNodes.FindNode(LTmpRowTag);
          if assigned(LNode1) then LViewRec.NodeName := LNode1.Text;
        end;

      end;

      //init alastErrorObjectNode;
      LLastErrorObjectNode := LJSONDoc.Node.ChildNodes['lastErrorObject'];

      //NumberOfDocumentsUpdatedOrRemoved
      LNode1 := LLastErrorObjectNode.ChildNodes.FindNode('n');
      if assigned(LNode1) then NumberOfDocumentsUpdatedOrRemoved := LNode1.int32
      else NumberOfDocumentsUpdatedOrRemoved := 0;

      //updatedExisting
      LNode1 := LLastErrorObjectNode.ChildNodes.FindNode('updatedExisting');
      if assigned(LNode1) then updatedExisting := LNode1.bool
      else updatedExisting := False;

      //ObjectID
      LNode1 := LLastErrorObjectNode.ChildNodes.FindNode('upserted');
      if assigned(LNode1) then ObjectID := LNode1.text
      else ObjectID := '';

    finally

      //close the curson
      if LCursorID <> 0 then OP_KILL_CURSORS(fSocketDescriptor, [LCursorID]);

    end;

  finally
    LJSONDoc.free;
  end;

  //do the OnDeleteDataDone
  LStopWatch.Stop;
  OnFindAndModifyDataDone(FullCollectionName,
                          query,
                          sort,
                          remove,
                          update,
                          new,
                          ReturnFieldsSelector,
                          InsertIfNotFound,
                          RowTag,
                          ViewTag,
                          LStopWatch.Elapsed.TotalMilliseconds);

end;


{********************************************************************************}
Procedure TAlMongoDBClient.FindAndModifyData(const FullCollectionName: AnsiString;
                                             const query: AnsiString;
                                             const sort: AnsiString;
                                             remove: boolean;
                                             const update: AnsiString;
                                             new: boolean;
                                             const ReturnFieldsSelector: AnsiString;
                                             InsertIfNotFound: boolean;
                                             JSONDATA: TALJSONNode;
                                             var NumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed
                                             var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                             var ObjectID: AnsiString); // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
begin
  FindAndModifyData(FullCollectionName,
                    query,
                    sort,
                    remove,
                    update,
                    new,
                    ReturnFieldsSelector,
                    InsertIfNotFound,
                    '', // RowTag,
                    '', // ViewTag,
                    JSONDATA,
                    NumberOfDocumentsUpdatedOrRemoved,
                    updatedExisting,
                    ObjectID);
end;

{********************************************************************************}
Procedure TAlMongoDBClient.FindAndModifyData(const FullCollectionName: AnsiString;
                                             const query: AnsiString;
                                             const sort: AnsiString;
                                             remove: boolean;
                                             const update: AnsiString;
                                             new: boolean;
                                             const ReturnFieldsSelector: AnsiString;
                                             InsertIfNotFound: boolean;
                                             JSONDATA: TALJSONNode);
var LNumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed
    LUpdatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
    LObjectID: AnsiString; // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
begin
  FindAndModifyData(FullCollectionName,
                    query,
                    sort,
                    remove,
                    update,
                    new,
                    ReturnFieldsSelector,
                    InsertIfNotFound,
                    '', // RowTag,
                    '', // ViewTag,
                    JSONDATA,
                    LNumberOfDocumentsUpdatedOrRemoved,
                    LUpdatedExisting,
                    LObjectID);
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
Var LTickCount: int64;
Begin

  //synchronize the code
  FConnectionPoolCS.Acquire;
  Try

    //raise an exception if currently realeasing all connection
    if FReleasingAllconnections then raise EALException.Create('Can not acquire connection: currently releasing all connections');

    //delete the old unused connection
    LTickCount := GetTickCount64;
    if LTickCount - fLastConnectionGarbage > (60000 {every minutes})  then begin
      while FConnectionPoolCount > 0 do begin
        if LTickCount - FConnectionPool[0].Lastaccessdate > FConnectionMaxIdleTime then begin

          Try
            DoDisconnect(FConnectionPool[0].SocketDescriptor);
          Except
            //Disconnect must be a "safe" procedure because it's mostly called in
            //finalization part of the code that it is not protected
          End;

          Dec(FConnectionPoolCount);
          if  FConnectionPoolCount > 0 then
          begin
            System.Move(FConnectionPool[1], FConnectionPool[0],
              (FConnectionPoolCount) * SizeOf(TAlMongoDBConnectionPoolContainer));
          end;

        end
        else break;
      end;
      FLastConnectionGarbage := LTickCount;
    end;

    //acquire the new connection from the pool
    If FConnectionPoolCount > 0 then begin
      Result := FConnectionPool[FConnectionPoolCount - 1].SocketDescriptor;
      Dec(FConnectionPoolCount);
    end

    //ask to create a new connection
    else begin
      result := INVALID_SOCKET;
    end;

    //increase the connection count
    inc(FWorkingConnectionCount);

  //get out of the synchronization
  finally
    FConnectionPoolCS.Release;
  end;

  //create a new connection if pool was empty
  if result = INVALID_SOCKET then begin
    try
      Doconnect(result,
                fHost,//aHost,
                fPort,//APort,
                fSendTimeout,
                fReceiveTimeout,
                fKeepAlive,
                fTCPNoDelay);
    except
      FConnectionPoolCS.Acquire;
      dec(FWorkingConnectionCount);
      FConnectionPoolCS.Release;
      raise;
    end;
  end;

End;

{***************************************************************************************}
procedure TAlMongoDBConnectionPoolClient.ReleaseConnection(var SocketDescriptor: TSocket;
                                                           const CloseConnection: Boolean = False);
begin

  //security check
  if SocketDescriptor = INVALID_SOCKET then raise EALException.Create('Connection handle can not be INVALID_SOCKET');

  //release the connection
  FConnectionPoolCS.Acquire;
  Try

    //add the connection to the pool
    If (not CloseConnection) and (not FReleasingAllconnections) then begin
      if FConnectionPoolCount = FConnectionPoolCapacity then begin
        if FConnectionPoolCapacity > 64 then FConnectionPoolCapacity := FConnectionPoolCapacity + (FConnectionPoolCapacity div 4) else
          if FConnectionPoolCapacity > 8 then FConnectionPoolCapacity := FConnectionPoolCapacity + 16 else
            FConnectionPoolCapacity := FConnectionPoolCapacity + 4;
        SetLength(FConnectionPool, FConnectionPoolCapacity);
      end;
      FConnectionPool[FConnectionPoolCount].SocketDescriptor := SocketDescriptor;
      FConnectionPool[FConnectionPoolCount].LastAccessDate := GetTickCount64;
      Inc(FConnectionPoolCount);
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
  setlength(FConnectionPool,0);
  FConnectionPoolCount := 0;
  FConnectionPoolCapacity := 0;
  FConnectionPoolCS:= TCriticalSection.create;
  FWorkingConnectionCount:= 0;
  FReleasingAllconnections := False;
  FLastConnectionGarbage := GettickCount64;
  FConnectionMaxIdleTime := 1200000; // 1000 * 60 * 20 = 20 min
end;

{************************************************}
destructor TAlMongoDBConnectionPoolClient.Destroy;
begin
  ReleaseAllConnections;
  FConnectionPoolCS.free;
  inherited;
end;

{***********************************************************************************************************}
procedure TAlMongoDBConnectionPoolClient.ReleaseAllConnections(Const WaitWorkingConnections: Boolean = True);
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
      while FConnectionPoolCount > 0 do begin
        Try
          DoDisconnect(FConnectionPool[FConnectionPoolcount - 1].SocketDescriptor);
        Except
          //Disconnect must be a "safe" procedure because it's mostly called in
          //finalization part of the code that it is not protected
        End;
        Dec(FConnectionPoolCount);
      end;
      FLastConnectionGarbage := GetTickCount64;
    finally
      FConnectionPoolCS.Release;
    end;

  finally
    //Do not forbid anymore new thread to create a new transaction
    FReleasingAllconnections := False;
  End;

end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                                          // name with the collection name, using a . for the concatenation. For example, for the database
                                                                                          // foo and the collection bar, the full collection name is foo.bar
                                                    const Query: AnsiString; // BSON document that represents the query. The query will contain one or more elements,
                                                                             // all of which must match for a document to be included in the result set. Possible elements
                                                                             // include $query, $orderby, $hint, $explain, and $snapshot.
                                                    const ReturnFieldsSelector: AnsiString; // Optional. BSON document that limits the fields in the returned documents.
                                                                                            // The returnFieldsSelector contains one or more elements, each of which is the name
                                                                                            // of a field that should be returned, and and the integer value 1. In JSON notation,
                                                                                            // a returnFieldsSelector to limit to the fields a, b and c would be:
                                                                                            // { a : 1, b : 1, c : 1}
                                                    flags: TALMongoDBClientSelectDataFlags; // Options (see TALMongoDBClientSelectDataFlags)
                                                    const RowTag: AnsiString; // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                                                    const ViewTag: AnsiString; // the node name under with all records will be stored in the JSON/XML result document.
                                                    Skip: integer; // Sets the number of documents to omit
                                                    First: Integer; // Limits the number of documents to retrieve
                                                    CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                                             // cache or not. Values <= 0 deactivate the cache
                                                    JSONDATA: TALJSONNode;
                                                    OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);

Var LViewRec: TalJSONNode;
    LJSONDocument: TalJSONDocument;
    LResponseFlags: integer;
    LCursorID: int64;
    LStartingFrom: integer;
    LNumberReturned: integer;
    LFlags: integer;
    LRecAdded: integer;
    LContinue: boolean;
    LTMPConnectionSocket: TSocket;
    LOwnConnection: Boolean;
    LStopWatch: TStopWatch;
    LCacheKey: ansiString;
    LCacheStr: ansiString;

begin

  //only OnNewRowFunct / JSONDATA can be used
  if assigned(OnNewRowFunct) then JSONDATA := nil;

  //clear the JSONDATA
  if assigned(JSONDATA) then LJSONDocument := Nil
  else begin
    LJSONDocument := TALJSONDocument.create;
    JSONDATA := LJSONDocument.Node;
  end;

  try

    //init the TstopWatch
    LStopWatch := TstopWatch.Create;

    //Handle the CacheThreshold
    LCacheKey := '';
    If (CacheThreshold > 0) and
       (not assigned(LJSONDocument)) and
       (not (sfTailMonitoring in flags)) and
       ((JSONDATA.ChildNodes.Count = 0) or  // else the save will not work
        (ViewTag <> '')) then begin

      //try to load from from cache
      LCacheKey := ALStringHashSHA1(
                     ALFormat('%s#%d#%d#%s#%s#%s',
                              [RowTag,
                               Skip,
                               First,
                               FullCollectionName,
                               ReturnFieldsSelector,
                               Query]));

      if loadcachedData(LCacheKey, LCacheStr) then begin

        //init the aViewRec
        if (ViewTag <> '') then LViewRec := JSONDATA.AddChild(ViewTag, ntobject)
        else LViewRec := JSONDATA;

        //assign the tmp data to the XMLData
        LViewRec.LoadFromJsonString(LCacheStr, false{ClearChildNodes});

        //exit
        exit;

      end;

    end;

    //acquire a connection
    if ConnectionSocket = INVALID_SOCKET then begin
      LTMPConnectionSocket := AcquireConnection;
      LOwnConnection := True;
    end
    else begin
      LTMPConnectionSocket := ConnectionSocket;
      LOwnConnection := False;
    end;

    try

      //start the TstopWatch
      LStopWatch.Reset;
      LStopWatch.Start;

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
      LFlags := 0;
      if sfTailMonitoring in flags then
        raise EAlMongoDBClientException.Create('Tail monitoring work only with TAlMongoDBClient', 0 {aErrorCode}, false {aCloseConnection});
      if sfSlaveOk in flags then LFlags := LFlags or (1 shl 2);
      if sfNoCursorTimeout in flags then LFlags := LFlags or (1 shl 4);
      if sfPartial in flags then LFlags := LFlags or (1 shl 7);

      //init the aViewRec
      if (ViewTag <> '') and (not assigned(LJSONDocument)) then LViewRec := JSONdata.AddChild(ViewTag, ntobject)
      else LViewRec := JSONdata;

      //init aRecAdded
      LRecAdded := 0;

      //do the First query
      LContinue := true;
      OP_QUERY(LTMPConnectionSocket,
               LFlags,
               fullCollectionName,
               ALIfThen(Skip >= 0, Skip, 0),
               ALIfThen(First >= 0, First, 0), // The MongoDB server returns the query results in batches. Batch size will not exceed
                                               // the maximum BSON document size. For most queries, the first batch returns 101
                                               // documents or just enough documents to exceed 1 megabyte. Subsequent batch size is
                                               // 4 megabytes. To override the default size of the batch, see batchSize() and limit().
               query,
               ReturnFieldsSelector,
               LResponseFlags,
               LCursorID,
               LStartingFrom, // where in the cursor this reply is starting
               LNumberReturned, // number of documents in the reply
               LViewRec,
               OnNewRowFunct,
               ExtData,
               RowTag,
               ViewTag,
               LContinue);

      try

        //init aRecAdded
        LRecAdded := LRecAdded + LNumberReturned;

        //loop still the cursorID > 0
        while (LContinue) and
              (LCursorID <> 0) and
              ((First <= 0) or
               (LRecAdded < First)) do begin

          //Get more data
          OP_GET_MORE(LTMPConnectionSocket,
                      fullCollectionName,
                      ALIfThen(First > 0, First - LRecAdded, 0),
                      LCursorID,
                      LResponseFlags,
                      LStartingFrom,
                      LNumberReturned,
                      LViewRec,
                      OnNewRowFunct,
                      ExtData,
                      RowTag,
                      ViewTag,
                      LContinue);

          //init aRecAdded
          LRecAdded := LRecAdded + LNumberReturned;

        end;

      finally

        //close the curson
        if LCursorID <> 0 then OP_KILL_CURSORS(LTMPConnectionSocket, [LCursorID]);

      end;

      //do the OnSelectDataDone
      LStopWatch.Stop;
      OnSelectDataDone(FullCollectionName,
                       Query,
                       ReturnFieldsSelector,
                       flags,
                       RowTag,
                       ViewTag,
                       Skip,
                       First,
                       CacheThreshold,
                       LStopWatch.Elapsed.TotalMilliseconds);

      //save to the cache
      If LCacheKey <> '' then begin

        //save the data
        LViewRec.SaveToJSONString(LCacheStr);
        SaveDataToCache(LCacheKey,
                        CacheThreshold,
                        LCacheStr);

      end;

      //Release the Connection
      if LOwnConnection then ReleaseConnection(LTMPConnectionSocket);

    except
      On E: Exception do begin
        if LOwnConnection then ReleaseConnection(LTMPConnectionSocket,
                                                 (not (E Is EAlMongoDBClientException)) or
                                                 (E as EAlMongoDBClientException).CloseConnection);
        raise;
      end;
    end;

  finally
    if assigned(LJSONDocument) then LJSONDocument.free;
  end;

end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(const FullCollectionName: AnsiString;
                                                    const Query: AnsiString;
                                                    const ReturnFieldsSelector: AnsiString;
                                                    flags: TALMongoDBClientSelectDataFlags;
                                                    Skip: integer;
                                                    First: Integer;
                                                    OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             flags,
             '', // RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             nil, // JSONDATA,
             OnNewRowFunct,
             ExtData,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(const FullCollectionName: AnsiString;
                                                    const Query: AnsiString;
                                                    const ReturnFieldsSelector: AnsiString;
                                                    flags: TALMongoDBClientSelectDataFlags;
                                                    OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             flags,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             nil, // JSONDATA,
             OnNewRowFunct,
             ExtData,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(const FullCollectionName: AnsiString;
                                                    const Query: AnsiString;
                                                    const ReturnFieldsSelector: AnsiString;
                                                    flags: TALMongoDBClientSelectDataFlags;
                                                    const RowTag: AnsiString;
                                                    Skip: integer;
                                                    First: Integer;
                                                    JSONDATA: TALJSONNode;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             flags,
             RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(const FullCollectionName: AnsiString;
                                                    const Query: AnsiString;
                                                    const ReturnFieldsSelector: AnsiString;
                                                    flags: TALMongoDBClientSelectDataFlags;
                                                    const RowTag: AnsiString;
                                                    JSONDATA: TALJSONNode;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             flags,
             RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(const FullCollectionName: AnsiString;
                                                    const Query: AnsiString;
                                                    const ReturnFieldsSelector: AnsiString;
                                                    flags: TALMongoDBClientSelectDataFlags;
                                                    JSONDATA: TALJSONNode;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             flags,
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(const FullCollectionName: AnsiString;
                                                    const Query: AnsiString;
                                                    const ReturnFieldsSelector: AnsiString;
                                                    Skip: integer;
                                                    First: Integer;
                                                    OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             [],
             '', // RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             nil, // JSONDATA,
             OnNewRowFunct,
             ExtData,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(const FullCollectionName: AnsiString;
                                                    const Query: AnsiString;
                                                    const ReturnFieldsSelector: AnsiString;
                                                    OnNewRowFunct: TALMongoDBClientSelectDataOnNewRowFunct;
                                                    ExtData: Pointer;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             [],
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             nil, // JSONDATA,
             OnNewRowFunct,
             ExtData,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(const FullCollectionName: AnsiString;
                                                    const Query: AnsiString;
                                                    const ReturnFieldsSelector: AnsiString;
                                                    const RowTag: AnsiString;
                                                    Skip: integer;
                                                    First: Integer;
                                                    JSONDATA: TALJSONNode;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             [],
             RowTag,
             '', // ViewTag,
             Skip,
             First,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(const FullCollectionName: AnsiString;
                                                    const Query: AnsiString;
                                                    const ReturnFieldsSelector: AnsiString;
                                                    const RowTag: AnsiString;
                                                    JSONDATA: TALJSONNode;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             [],
             RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.SelectData(const FullCollectionName: AnsiString;
                                                    const Query: AnsiString;
                                                    const ReturnFieldsSelector: AnsiString;
                                                    JSONDATA: TALJSONNode;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  SelectData(FullCollectionName,
             Query,
             ReturnFieldsSelector,
             [],
             '', // RowTag,
             '', // ViewTag,
             -1, // Skip,
             -1, // First,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewRowFunct,
             nil, // ExtData,
             ConnectionSocket);
end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.RunCommand(const DatabaseName: AnsiString;
                                                    const Command: AnsiString; // BSON document that represents the Command
                                                    flags: TALMongoDBClientRunCommandFlags; // Options (see TALMongoDBClientRunCommandFlags)
                                                    const RowTag: AnsiString; // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                                                    const ViewTag: AnsiString; // the node name under with all records will be stored in the JSON/XML result document.
                                                    CacheThreshold: Integer; // The threshold value (in ms) determine whether we will use
                                                                             // cache or not. Values <= 0 deactivate the cache
                                                    JSONDATA: TALJSONNode;
                                                    OnNewBatchRowFunct: TALMongoDBClientRunCommandOnNewBatchRowFunct;
                                                    ExtData: Pointer;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);

Var LViewRec: TalJSONNode;
    LRowRec: TalJSONNode;
    LCursorRec: TalJsonNode;
    LFirstBatchRec: TalJsonNode;
    LJSONDocument: TalJSONDocument;
    LResponseFlags: integer;
    LCursorID: int64;
    LStartingFrom: integer;
    LNumberReturned: integer;
    LFlags: integer;
    LContinue: boolean;
    LTMPConnectionSocket: TSocket;
    LOwnConnection: Boolean;
    LStopWatch: TStopWatch;
    LCacheKey: ansiString;
    LCacheStr: ansiString;
    LFullCollectionName: ansiString;
    i: integer;

begin

  //init aFullCollectionName
  LFullCollectionName := DatabaseName + '.$cmd';

  //only OnNewBatchRowFunct / JSONDATA can be used
  if assigned(OnNewBatchRowFunct) then JSONDATA := nil;

  //clear the JSONDATA
  if assigned(JSONDATA) then LJSONDocument := Nil
  else begin
    LJSONDocument := TALJSONDocument.create;
    JSONDATA := LJSONDocument.Node;
  end;

  try

    //init the TstopWatch
    LStopWatch := TstopWatch.Create;

    //Handle the CacheThreshold
    LCacheKey := '';
    If (CacheThreshold > 0) and
       (not assigned(LJSONDocument)) and
       ((JSONDATA.ChildNodes.Count = 0) or  // else the save will not work
        (ViewTag <> '')) then begin

      //try to load from from cache
      LCacheKey := ALStringHashSHA1(
                     ALFormat('%s#%d#%d#%s#%s#%s',
                              [RowTag,
                               0,
                               1,
                               LFullCollectionName,
                               '',
                               Command]));

      if loadcachedData(LCacheKey, LCacheStr) then begin

        //init the aViewRec
        if (ViewTag <> '') then LViewRec := JSONDATA.AddChild(ViewTag, ntobject)
        else LViewRec := JSONDATA;

        //assign the tmp data to the XMLData
        LViewRec.LoadFromJsonString(LCacheStr, false{ClearChildNodes});

        //exit
        exit;

      end;

    end;

    //acquire a connection
    if ConnectionSocket = INVALID_SOCKET then begin
      LTMPConnectionSocket := AcquireConnection;
      LOwnConnection := True;
    end
    else begin
      LTMPConnectionSocket := ConnectionSocket;
      LOwnConnection := False;
    end;

    try

      //start the TstopWatch
      LStopWatch.Reset;
      LStopWatch.Start;

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
      LFlags := 0;
      if cfSlaveOk in flags then LFlags := LFlags or (1 shl 2);
      if cfNoCursorTimeout in flags then LFlags := LFlags or (1 shl 4);
      if cfPartial in flags then LFlags := LFlags or (1 shl 7);

      //init the aViewRec
      if (ViewTag <> '') and (not assigned(LJSONDocument)) then LViewRec := JSONdata.AddChild(ViewTag, ntobject)
      else LViewRec := JSONdata;

      //do the First query
      LContinue := true;
      OP_QUERY(LTMPConnectionSocket,
               LFlags,
               LFullCollectionName,
               0,
               1,
               Command,
               '', // ReturnFieldsSelector
               LResponseFlags,
               LCursorID,
               LStartingFrom, // where in the cursor this reply is starting
               LNumberReturned, // number of documents in the reply
               LViewRec,
               nil,
               ExtData,
               RowTag,
               ViewTag,
               LContinue);

      //aCursorID must be egual to 0 (we retrieve only one doc) but in case
      if LCursorID <> 0 then OP_KILL_CURSORS(LTMPConnectionSocket, [LCursorID]);

      //init LRowRec
      if (RowTag <> '') and (LViewRec.ChildNodes.Count > 0) then LRowRec := LViewRec.ChildNodes[LViewRec.ChildNodes.Count - 1]
      else LRowRec := LViewRec;

      //check error
      CheckRunCommandResponse(LRowRec);

      //{
      //  "cursor" : {
      //      "id" : NumberLong(3286429289449024015),
      //      "ns" : "mydatabase.$cmd.listCollections",
      //      "firstBatch" : [
      //          {
      //            ....
      //          },
      //          {
      //            ....
      //          }
      //      ]
      //  },
      //  "ok" : 1.0
      //}
      LCursorRec := LRowRec.ChildNodes.FindNode('cursor');
      if LCursorRec <> nil then begin

        //init LFirstBatchRec
        LFirstBatchRec := LCursorRec.ChildNodes.FindNode('firstBatch');
        if (LFirstBatchRec <> nil) and
           (assigned(OnNewBatchRowFunct)) and
           (LContinue) then begin
          for I := 0 to LFirstBatchRec.ChildNodes.Count - 1 do begin
            OnNewBatchRowFunct(LFirstBatchRec.ChildNodes[I], // JSONRowData: TALJSONNode;
                               ViewTag, // const ViewTag: AnsiString;
                               ExtData, // ExtData: Pointer;
                               LContinue); // Var Continue: Boolean);
            if not LContinue then break;
          end;
        end;

        //init LCursorID
        LCursorID := LCursorRec.getChildNodeValueInt64('id', 0);
        if LCursorID <> 0 then begin

          try

            //we do the job only if LFirstBatchRec exist
            if LFirstBatchRec <> nil then begin

              //init LCollectionName
              LFullCollectionName := LCursorRec.getChildNodeValueText('ns', '');

              //loop still the cursorID > 0
              while (LContinue) and
                    (LCursorID <> 0) do begin

                //Get more data
                OP_GET_MORE(LTMPConnectionSocket,
                            LFullCollectionName,
                            0, // numberToReturn
                            LCursorID,
                            LResponseFlags,
                            LStartingFrom,
                            LNumberReturned,
                            LFirstBatchRec, // documents
                            OnNewBatchRowFunct,
                            ExtData,
                            '', // RowTag,
                            ViewTag,
                            LContinue);

              end;

            end;

          finally

            //close the curson
            if LCursorID <> 0 then OP_KILL_CURSORS(LTMPConnectionSocket, [LCursorID]);

          end;

          LCursorRec.SetChildNodeValueInt64('id', 0);

        end;

      end;

      //do the OnRunCommandDone
      LStopWatch.Stop;
      OnRunCommandDone(DatabaseName,
                       Command,
                       flags,
                       RowTag,
                       ViewTag,
                       CacheThreshold,
                       LStopWatch.Elapsed.TotalMilliseconds);

      //save to the cache
      If LCacheKey <> '' then begin

        //save the data
        LViewRec.SaveToJSONString(LCacheStr);
        SaveDataToCache(LCacheKey,
                        CacheThreshold,
                        LCacheStr);

      end;

      //Release the Connection
      if LOwnConnection then ReleaseConnection(LTMPConnectionSocket);

    except
      On E: Exception do begin
        if LOwnConnection then ReleaseConnection(LTMPConnectionSocket,
                                                 (not (E Is EAlMongoDBClientException)) or
                                                 (E as EAlMongoDBClientException).CloseConnection);
        raise;
      end;
    end;

  finally
    if assigned(LJSONDocument) then LJSONDocument.free;
  end;

end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.RunCommand(const DatabaseName: AnsiString;
                                                    const Command: AnsiString;
                                                    flags: TALMongoDBClientRunCommandFlags;
                                                    OnNewBatchRowFunct: TALMongoDBClientRunCommandOnNewBatchRowFunct;
                                                    ExtData: Pointer;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  RunCommand(DatabaseName,
             Command,
             flags,
             '', // RowTag,
             '', // ViewTag,
             -1, // CacheThreshold,
             nil, // JSONDATA,
             OnNewBatchRowFunct,
             ExtData,
             ConnectionSocket);
end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.RunCommand(const DatabaseName: AnsiString;
                                                    const Command: AnsiString;
                                                    flags: TALMongoDBClientRunCommandFlags;
                                                    const RowTag: AnsiString;
                                                    JSONDATA: TALJSONNode;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  RunCommand(DatabaseName,
             Command,
             flags,
             RowTag,
             '', // ViewTag,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewBatchRowFunct,
             nil, // ExtData,
             ConnectionSocket);
end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.RunCommand(const DatabaseName: AnsiString;
                                                    const Command: AnsiString;
                                                    flags: TALMongoDBClientRunCommandFlags;
                                                    JSONDATA: TALJSONNode;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  RunCommand(DatabaseName,
             Command,
             flags,
             '', // RowTag,
             '', // ViewTag,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewBatchRowFunct,
             nil, // ExtData,
             ConnectionSocket);
end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.RunCommand(const DatabaseName: AnsiString;
                                                    const Command: AnsiString;
                                                    OnNewBatchRowFunct: TALMongoDBClientRunCommandOnNewBatchRowFunct;
                                                    ExtData: Pointer;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  RunCommand(DatabaseName,
             Command,
             [],
             '', // RowTag,
             '', // ViewTag,
             -1, // CacheThreshold,
             nil, // JSONDATA,
             OnNewBatchRowFunct,
             ExtData,
             ConnectionSocket);
end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.RunCommand(const DatabaseName: AnsiString;
                                                    const Command: AnsiString;
                                                    const RowTag: AnsiString;
                                                    JSONDATA: TALJSONNode;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  RunCommand(DatabaseName,
             Command,
             [],
             RowTag,
             '', // ViewTag,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewBatchRowFunct,
             nil, // ExtData,
             ConnectionSocket);
end;

{*********************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.RunCommand(const DatabaseName: AnsiString;
                                                    const Command: AnsiString;
                                                    JSONDATA: TALJSONNode;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  RunCommand(DatabaseName,
             Command,
             [],
             '', // RowTag,
             '', // ViewTag,
             -1, // CacheThreshold,
             JSONDATA,
             nil, // OnNewBatchRowFunct,
             nil, // ExtData,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.UpdateData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                                          // name with the collection name, using a . for the concatenation. For example, for the database
                                                                                          // foo and the collection bar, the full collection name is foo.bar
                                                    const selector: AnsiString; // BSON document that specifies the query for selection of the document to update.
                                                    const update: AnsiString; // BSON document that specifies the update to be performed. For information on specifying updates see
                                                                              // http://docs.mongodb.org/manual/tutorial/modify-documents/
                                                    flags: TALMongoDBClientUpdateDataFlags; // Options (see TALMongoDBClientUpdateDataFlags)
                                                    var NumberOfDocumentsUpdated: integer; // reports the number of documents updated
                                                    var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                                    var ObjectID: ansiString; // an ObjectId that corresponds to the upserted document if the update resulted in an insert.
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
Var LFlags: integer;
    LTMPConnectionSocket: TSocket;
    LOwnConnection: Boolean;
    LStopWatch: TStopWatch;
begin

  //acquire a connection
  if ConnectionSocket = INVALID_SOCKET then begin
    LTMPConnectionSocket := AcquireConnection;
    LOwnConnection := True;
  end
  else begin
    LTMPConnectionSocket := ConnectionSocket;
    LOwnConnection := False;
  end;

  try

    //init the TstopWatch
    LStopWatch := TstopWatch.StartNew;

    //bit num	  name	        description
    //0	        Upsert	      If set, the database will insert the supplied object into the collection if no matching document is found.
    //1	        MultiUpdate	  If set, the database will update all matching objects in the collection. Otherwise only updates first matching doc.
    //2-31    	Reserved	    Must be set to 0.
    LFlags := 0;
    if ufUpsert in flags then LFlags := LFlags or (1 shl 0);
    if ufMultiUpdate in flags then LFlags := LFlags or (1 shl 1);

    OP_UPDATE(LTMPConnectionSocket,
              LFlags,
              FullCollectionName,
              selector,
              Update,
              NumberOfDocumentsUpdated,
              updatedExisting,
              ObjectID);

    //do the OnUpdateDataDone
    LStopWatch.Stop;
    OnUpdateDataDone(FullCollectionName,
                     selector,
                     update,
                     flags,
                     LStopWatch.Elapsed.TotalMilliseconds);

    //Release the Connection
    if LOwnConnection then ReleaseConnection(LTMPConnectionSocket);

  except
    On E: Exception do begin
      if LOwnConnection then ReleaseConnection(LTMPConnectionSocket,
                                               (not (E Is EAlMongoDBClientException)) or
                                               (E as EAlMongoDBClientException).CloseConnection);
      raise;
    end;
  end;

end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.UpdateData(const FullCollectionName: AnsiString;
                                                    const Selector: AnsiString;
                                                    const Update: AnsiString;
                                                    flags: TALMongoDBClientUpdateDataFlags;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var LNumberOfDocumentsUpdated: integer;
    LUpdatedExisting: boolean;
    LObjectID: ansiString;
begin
  UpdateData(FullCollectionName,
             selector,
             update,
             flags,
             LNumberOfDocumentsUpdated,
             LUpdatedExisting,
             LObjectID,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.UpdateData(const FullCollectionName: AnsiString;
                                                    const Selector: AnsiString;
                                                    const Update: AnsiString;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var LNumberOfDocumentsUpdated: integer;
    LUpdatedExisting: boolean;
    LObjectID: ansiString;
begin
  UpdateData(FullCollectionName,
             selector,
             update,
             [],
             LNumberOfDocumentsUpdated,
             LUpdatedExisting,
             LObjectID,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.InsertData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                                          // name with the collection name, using a . for the concatenation. For example, for the database
                                                                                          // foo and the collection bar, the full collection name is foo.bar
                                                    const documents: AnsiString; // One or more documents to insert into the collection. If there are more than one, they are written in sequence, one after another.
                                                    flags: TALMongoDBClientInsertDataFlags;// Options (see TALMongoDBClientInsertDataFlags)
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);

Var LFlags: integer;
    LTMPConnectionSocket: TSocket;
    LOwnConnection: Boolean;
    LStopWatch: TStopWatch;

begin

  //acquire a connection
  if ConnectionSocket = INVALID_SOCKET then begin
    LTMPConnectionSocket := AcquireConnection;
    LOwnConnection := True;
  end
  else begin
    LTMPConnectionSocket := ConnectionSocket;
    LOwnConnection := False;
  end;

  try

    //init the TstopWatch
    LStopWatch := TstopWatch.Create;

    //start the TstopWatch
    LStopWatch.Reset;
    LStopWatch.Start;

    //bit num	 name	            description
    //0	       ContinueOnError	If set, the database will not stop processing a bulk insert if one fails (eg due to duplicate IDs).
    //                          This makes bulk insert behave similarly to a series of single inserts, except lastError will be set if
    //                          any insert fails, not just the last one. If multiple errors occur, only the most recent will be
    //                          reported by getLastError. (new in 1.9.1)
    //1-31	   Reserved	        Must be set to 0.
    LFlags := 0;
    if ifContinueOnError in flags then LFlags := LFlags or (1 shl 0);

    OP_INSERT(LTMPConnectionSocket,
              LFlags,
              FullCollectionName,
              documents);

    //do the OnInsertDataDone
    LStopWatch.Stop;
    OnInsertDataDone(FullCollectionName,
                     documents,
                     flags,
                     LStopWatch.Elapsed.TotalMilliseconds);

    //Release the Connection
    if LOwnConnection then ReleaseConnection(LTMPConnectionSocket);

  except
    On E: Exception do begin
      if LOwnConnection then ReleaseConnection(LTMPConnectionSocket,
                                               (not (E Is EAlMongoDBClientException)) or
                                               (E as EAlMongoDBClientException).CloseConnection);
      raise;
    end;
  end;

end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.InsertData(const FullCollectionName: AnsiString;
                                                    const documents: AnsiString;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  InsertData(FullCollectionName,
             documents,
             [],
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.DeleteData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                                          // name with the collection name, using a . for the concatenation. For example, for the database
                                                                                          // foo and the collection bar, the full collection name is foo.bar
                                                    const selector: AnsiString; // BSON document that represent the query used to select the documents to be removed
                                                                                // The selector will contain one or more elements, all of which must match for a document
                                                                                // to be removed from the collection
                                                    flags: TALMongoDBClientDeleteDataFlags; // Options (see TALMongoDBClientDeleteDataFlags)
                                                    var NumberOfDocumentsRemoved: integer;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);

Var LFlags: integer;
    LTMPConnectionSocket: TSocket;
    LOwnConnection: Boolean;
    LStopWatch: TStopWatch;

begin

  //acquire a connection
  if ConnectionSocket = INVALID_SOCKET then begin
    LTMPConnectionSocket := AcquireConnection;
    LOwnConnection := True;
  end
  else begin
    LTMPConnectionSocket := ConnectionSocket;
    LOwnConnection := False;
  end;

  try

    //init the TstopWatch
    LStopWatch := TstopWatch.StartNew;

    //bit num	   name	          description
    //0	         SingleRemove	  If set, the database will remove only the first matching document in the collection. Otherwise all matching documents will be removed.
    //1-31	     Reserved	      Must be set to 0.
    LFlags := 0;
    if dfSingleRemove in flags then LFlags := LFlags or (1 shl 0);

    OP_DELETE(LTMPConnectionSocket,
              LFlags,
              FullCollectionName,
              selector,
              NumberOfDocumentsRemoved);

    //do the OnDeleteDataDone
    LStopWatch.Stop;
    OnDeleteDataDone(FullCollectionName,
                     selector,
                     flags,
                     LStopWatch.Elapsed.TotalMilliseconds);

    //Release the Connection
    if LOwnConnection then ReleaseConnection(LTMPConnectionSocket);

  except
    On E: Exception do begin
      if LOwnConnection then ReleaseConnection(LTMPConnectionSocket,
                                               (not (E Is EAlMongoDBClientException)) or
                                               (E as EAlMongoDBClientException).CloseConnection);
      raise;
    end;
  end;

end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.DeleteData(const FullCollectionName: AnsiString;
                                                    const Selector: AnsiString;
                                                    flags: TALMongoDBClientDeleteDataFlags;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var LNumberOfDocumentsRemoved: integer;
begin
  DeleteData(FullCollectionName,
             selector,
             flags,
             LNumberOfDocumentsRemoved,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.DeleteData(const FullCollectionName: AnsiString;
                                                    const Selector: AnsiString;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var LNumberOfDocumentsRemoved: integer;
begin
  DeleteData(FullCollectionName,
             selector,
             [],
             LNumberOfDocumentsRemoved,
             ConnectionSocket);
end;

{**********************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.FindAndModifyData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                                                 // name with the collection name, using a . for the concatenation. For example, for the database
                                                                                                 // foo and the collection bar, the full collection name is foo.bar
                                                           const query: AnsiString; // Optional. The selection criteria for the modification. The query field employs the same query selectors as used in the
                                                                                    // db.collection.find() method. Although the query may match multiple documents, findAndModify will select only
                                                                                    // one document to modify.
                                                           const sort: AnsiString; // Optional. Determines which document the operation modifies if the query selects multiple
                                                                                   // documents. findAndModify modifies the first document in the sort order specified by this argument.
                                                           remove: boolean; // Must specify either the remove or the update field. Removes the document specified in the query field.
                                                                            // Set this to true to remove the selected document . The default is false.
                                                           const update: AnsiString; // Must specify either the remove or the update field. Performs an update of the selected document.
                                                                                     // The update field employs the same update operators or field: value specifications to modify the selected document.
                                                           new: boolean; // Optional. When true, returns the modified document rather than the original. The findAndModify method
                                                                         // ignores the new option for remove operations. The default is false.
                                                           const ReturnFieldsSelector: AnsiString; // Optional. A subset of fields to return. The fields document specifies an inclusion of a
                                                                                                   // field with 1, as in: fields: { <field1>: 1, <field2>: 1, ... }.
                                                           InsertIfNotFound: boolean;  // Optional. Used in conjunction with the update field. When true, findAndModify
                                                                                       // creates a new document if no document matches the query, or if documents match the query,
                                                                                       // findAndModify performs an update. The default is false.
                                                           const RowTag: AnsiString; // the node name under with all the fields of a single record will be stored in the JSON/XML result document.
                                                           const ViewTag: AnsiString; // the node name under with all records will be stored in the JSON/XML result document.
                                                           JSONDATA: TALJSONNode;
                                                           var NumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed
                                                           var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                                           var ObjectID: AnsiString; // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
                                                           const ConnectionSocket: TSocket = INVALID_SOCKET);

Var LTMPConnectionSocket: TSocket;
    LOwnConnection: Boolean;
    LStopWatch: TStopWatch;
    LDatabaseName: AnsiString;
    LCollectionName: AnsiString;
    LViewRec: TalJSONNode;
    LResponseFlags: integer;
    LTmpQuery: ansiString;
    LCursorID: int64;
    LStartingFrom: integer;
    LNumberReturned: integer;
    LContinue: boolean;
    LTmpRowTag: ansiString;
    LUpdateRowTagByFieldValue: Boolean;
    LJSONDoc: TALJSONDocument;
    LNode1: TALJSONNode;
    LNode2: TALJSONNode;
    LLastErrorObjectNode: TALJSONNode;
    P1: integer;

begin

  //acquire a connection
  if ConnectionSocket = INVALID_SOCKET then begin
    LTMPConnectionSocket := AcquireConnection;
    LOwnConnection := True;
  end
  else begin
    LTMPConnectionSocket := ConnectionSocket;
    LOwnConnection := False;
  end;

  try

    //init the TstopWatch
    LStopWatch := TstopWatch.StartNew;

    //create a temp JSONDoc
    LJSONDoc := TALJSONDocument.Create;
    try

      //init aDatabaseName and aCollectionName
      P1 := AlPos('.',FullCollectionName);
      if P1 <= 0 then raise EALException.Create('The full collection name must be the concatenation of the database name with the collection name');
      LDatabaseName := ALCopyStr(FullCollectionName, 1, P1-1);
      LCollectionName := ALCopyStr(FullCollectionName, P1+1, maxint);

      //buid the query
      LTmpQuery := '{"findAndModify":' + ALJsonEncodeTextWithNodeSubTypeHelper(LCollectionName);
      if query <> '' then LTmpQuery := LTmpQuery + ',"query":'+query;
      if sort <> '' then LTmpQuery := LTmpQuery + ',"sort":'+sort;
      LTmpQuery := LTmpQuery + ',"remove":'+ALJsonEncodeBooleanWithNodeSubTypeHelper(remove);
      if update <> '' then LTmpQuery := LTmpQuery + ',"update":'+update;
      LTmpQuery := LTmpQuery + ',"new":' + ALJsonEncodeBooleanWithNodeSubTypeHelper(new);
      if ReturnFieldsSelector <> '' then LTmpQuery := LTmpQuery + ',"fields":'+ReturnFieldsSelector;
      LTmpQuery := LTmpQuery + ',"upsert":'+ALJsonEncodeBooleanWithNodeSubTypeHelper(InsertIfNotFound);
      LTmpQuery := LTmpQuery + '}';

      //do the First query
      OP_QUERY(LTMPConnectionSocket,
               0, // flags
               LDatabaseName + '.$cmd', // fullCollectionName
               0, // skip
               1, // First
               LTmpQuery, // query
               '', // ReturnFieldsSelector,
               LResponseFlags,
               LCursorID,
               LStartingFrom,
               LNumberReturned,
               LJSONDoc.Node,
               nil,
               nil,
               '',
               '',
               LContinue);
      try

        // exemple of returned json result

        //{
        // "value":{"_id":2,"seq":3},
        // "lastErrorObject":{
        //   "updatedExisting":true,
        //   "n":1
        // },
        // "ok":1
        //}

        //{
        // "value":null,
        // "lastErrorObject":{
        //   "updatedExisting":false,
        //   "n":1,
        //   "upserted":2
        // },
        // "ok":1
        //}

        //{
        // "ok":0,
        // "errmsg":"remove and returnNew can\u0027t co-exist"
        //}

        //init the aViewRec
        if (ViewTag <> '') then LViewRec := JSONdata.AddChild(ViewTag, ntobject)
        else LViewRec := JSONdata;

        //check error
        CheckRunCommandResponse(LJSONDoc.Node);

        //get the value node
        LNode1 := LJSONDoc.Node.ChildNodes.FindNode('value');
        if assigned(LNode1) and (LNode1.NodeType = ntObject) then begin

          //init aUpdateRowTagByFieldValue and aTmpRowTag
          if AlPos('&>',RowTag) = 1 then begin
            LTmpRowTag := AlcopyStr(RowTag, 3, maxint);
            LUpdateRowTagByFieldValue := LTmpRowTag <> '';
          end
          else begin
            LTmpRowTag := RowTag;
            LUpdateRowTagByFieldValue := False;
          end;

          //add the row tag
          if (LTmpRowTag <> '') or
             (LViewRec.NodeType = ntarray) then LViewRec := LViewRec.AddChild(LTmpRowTag, ntobject);

          //move the node
          while LNode1.ChildNodes.Count > 0 do begin
            LNode2 := LNode1.ChildNodes.Extract(0);
            try
              LViewRec.ChildNodes.Add(LNode2);
            except
              LNode2.Free;
              raise;
            end;
          end;

          // aUpdateRowTagByFieldValue
          if LUpdateRowTagByFieldValue then begin
            LNode1 := LViewRec.ChildNodes.FindNode(LTmpRowTag);
            if assigned(LNode1) then LViewRec.NodeName := LNode1.Text;
          end;

        end;

        //init alastErrorObjectNode;
        LLastErrorObjectNode := LJSONDoc.Node.ChildNodes['lastErrorObject'];

        //NumberOfDocumentsUpdatedOrRemoved
        LNode1 := LLastErrorObjectNode.ChildNodes.FindNode('n');
        if assigned(LNode1) then NumberOfDocumentsUpdatedOrRemoved := LNode1.int32
        else NumberOfDocumentsUpdatedOrRemoved := 0;

        //updatedExisting
        LNode1 := LLastErrorObjectNode.ChildNodes.FindNode('updatedExisting');
        if assigned(LNode1) then updatedExisting := LNode1.bool
        else updatedExisting := False;

        //ObjectID
        LNode1 := LLastErrorObjectNode.ChildNodes.FindNode('upserted');
        if assigned(LNode1) then ObjectID := LNode1.text
        else ObjectID := '';

      finally

        //close the curson
        if LCursorID <> 0 then OP_KILL_CURSORS(LTMPConnectionSocket, [LCursorID]);

      end;

    finally
      LJSONDoc.free;
    end;

    //do the OnDeleteDataDone
    LStopWatch.Stop;
    OnFindAndModifyDataDone(FullCollectionName,
                            query,
                            sort,
                            remove,
                            update,
                            new,
                            ReturnFieldsSelector,
                            InsertIfNotFound,
                            RowTag,
                            ViewTag,
                            LStopWatch.Elapsed.TotalMilliseconds);

    //Release the Connection
    if LOwnConnection then ReleaseConnection(LTMPConnectionSocket);

  except
    On E: Exception do begin
      if LOwnConnection then ReleaseConnection(LTMPConnectionSocket,
                                               (not (E Is EAlMongoDBClientException)) or
                                               (E as EAlMongoDBClientException).CloseConnection);
      raise;
    end;
  end;

end;

{**********************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.FindAndModifyData(const FullCollectionName: AnsiString;
                                                           const query: AnsiString;
                                                           const sort: AnsiString;
                                                           remove: boolean;
                                                           const update: AnsiString;
                                                           new: boolean;
                                                           const ReturnFieldsSelector: AnsiString;
                                                           InsertIfNotFound: boolean;
                                                           JSONDATA: TALJSONNode;
                                                           var NumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed
                                                           var updatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
                                                           var ObjectID: AnsiString; // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
                                                           const ConnectionSocket: TSocket = INVALID_SOCKET);
begin
  FindAndModifyData(FullCollectionName,
                    query,
                    sort,
                    remove,
                    update,
                    new,
                    ReturnFieldsSelector,
                    InsertIfNotFound,
                    '', // RowTag,
                    '', // ViewTag,
                    JSONDATA,
                    NumberOfDocumentsUpdatedOrRemoved,
                    updatedExisting,
                    ObjectID,
                    ConnectionSocket);
end;

{**********************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.FindAndModifyData(const FullCollectionName: AnsiString;
                                                           const query: AnsiString;
                                                           const sort: AnsiString;
                                                           remove: boolean;
                                                           const update: AnsiString;
                                                           new: boolean;
                                                           const ReturnFieldsSelector: AnsiString;
                                                           InsertIfNotFound: boolean;
                                                           JSONDATA: TALJSONNode;
                                                           const ConnectionSocket: TSocket = INVALID_SOCKET);
var LNumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed
    LUpdatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
    LObjectID: AnsiString; // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
begin
  FindAndModifyData(FullCollectionName,
                    query,
                    sort,
                    remove,
                    update,
                    new,
                    ReturnFieldsSelector,
                    InsertIfNotFound,
                    '', // RowTag,
                    '', // ViewTag,
                    JSONDATA,
                    LNumberOfDocumentsUpdatedOrRemoved,
                    LUpdatedExisting,
                    LObjectID,
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
  fOnEvent := aOnEvent;
  fOnException := aOnError;
  inherited Create(False); // see http://www.gerixsoft.com/blog/delphi/fixing-symbol-resume-deprecated-warning-delphi-2010
end;

{************************************************}
destructor TAlMongoDBTailMonitoringThread.Destroy;
begin
  Terminate;
  fMongoDBClient.StopTailMonitoring := True;
  WaitFor;
  fMongoDBClient.Free;
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
begin

  //loop still not terminated
  while not Terminated do begin
    Try

      //disconnect
      fMongoDBClient.Disconnect;

      //connect
      fMongoDBClient.Connect(fHost, fPort);

      //select the data
      fMongoDBClient.SelectData(fFullCollectionName,
                                fQuery,
                                fReturnFieldsSelector,
                                [sfTailMonitoring, sfNoCursorTimeout], // sfNoCursorTimeout because if the doEvent is too long then the cursor will timeout
                                Procedure (JSONRowData: TALJSONNode;
                                           const ViewTag: AnsiString;
                                           ExtData: Pointer;
                                           Var Continue: Boolean)
                                begin
                                  doEvent(JSONRowData);
                                end,
                                nil);

    Except
      on E: Exception do begin
        DoException(E);
        sleep(1);
      end;
    End;
  end;
end;

end.
