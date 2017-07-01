(*************************************************************
www:          http://sourceforge.net/projects/alcinoe/
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code
Author(s):    Stéphane Vander Clock (skype/email: svanderclock@yahoo.fr)

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



Know bug :

History :

Link :

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
                                                                     const ViewTag: AnsiString;
                                                                     ExtData: Pointer;
                                                                     Var Continue: Boolean);
    {$ELSE}
    TALMongoDBClientSelectDataOnNewRowFunct = Procedure(JSONRowData: TALJSONNode;
                                                        const ViewTag: AnsiString;
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
      procedure CheckError(Error: Boolean);
      procedure DoConnect(var aSocketDescriptor: TSocket;
                          const aHost: AnsiString;
                          const APort: integer;
                          const aSendTimeout: Integer;
                          const aReceiveTimeout: Integer;
                          const aKeepAlive: Boolean;
                          const aTCPNoDelay: Boolean); virtual;
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
                                     var upserted: ansiString); overload; // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
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

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     Winapi.Windows,
     System.Diagnostics,
     {$ELSE}
     Windows,
     Diagnostics,
     {$IFEND}
     ALCipher,
     AlWinsock,
     ALWindows;

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
                                                    var upserted: ansiString); // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.

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
             'db.$cmd', // fullCollectionName
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
      if assigned(aNode) then upserted := aNode.text
      else upserted := '';

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
    upserted: ansiString;
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
      aJsonDocument.LoadFromJSONString('{"documents":' + documents + '}'); // { .... }                      => {"documents":{ .... } }
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
      aJsonDocument.LoadFromJSONString(Selector);
      aJsonDocument.SaveToBSONString(aBsonSelector);
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
      aJsonDocument.LoadFromJSONString(Update);
      aJsonDocument.SaveToBSONString(aBsonUpdate);
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
      aJsonDocument.LoadFromJSONString(Selector);
      aJsonDocument.SaveToBSONString(aBsonSelector);
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
      aJsonDocument.LoadFromJSONString(Query);
      aJsonDocument.SaveToBSONString(aBsonQuery);
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
      aJsonDocument.LoadFromJSONString(ReturnFieldsSelector);
      aJsonDocument.SaveToBSONString(aBsonReturnFieldsSelector);
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
      aJsonNode1.LoadFromBSONString(adocumentStr, False{ClearChildNodes});
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
var updatedExisting: boolean;
    upserted: ansiString;
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

Var aViewRec: TalJSONNode;
    aJSONDocument: TalJSONDocument;
    aResponseFlags: integer;
    aCursorID: int64;
    aStartingFrom: integer;
    aNumberReturned: integer;
    aFlags: integer;
    aRecAdded: integer;
    aContinue: boolean;
    aStopWatch: TStopWatch;
    aCacheKey: ansiString;
    aCacheStr: ansiString;

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

    //init the TstopWatch
    aStopWatch := TstopWatch.Create;

    //Handle the CacheThreshold
    aCacheKey := '';
    If (CacheThreshold > 0) and
       (not assigned(aJSONDocument)) and
       (not (sfTailMonitoring in flags)) and
       ((JSONDATA.ChildNodes.Count = 0) or  // else the save will not work
        (ViewTag <> '')) then begin

      //try to load from from cache
      aCacheKey := ALStringHashSHA1(RowTag + '#' +
                                    alinttostr(Skip) + '#' +
                                    alinttostr(First) + '#' +
                                    FullCollectionName + '#' +
                                    ReturnFieldsSelector + '#' +
                                    Query);

      if loadcachedData(aCacheKey, aCacheStr) then begin

        //init the aViewRec
        if (ViewTag <> '') then aViewRec := JSONDATA.AddChild(ViewTag, ntobject)
        else aViewRec := JSONDATA;

        //assign the tmp data to the XMLData
        aViewRec.LoadFromJsonString(aCacheStr, false{ClearChildNodes});

        //exit
        exit;

      end;

    end;

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
    if sfTailMonitoring in flags then begin
      if not assigned(OnNewRowFunct) then raise Exception.Create('OnNewRowFunct is mandatory for tail monitoring');
      aFlags := aFlags or (1 shl 1); // TailableCursor
      aFlags := aFlags or (1 shl 5); // AwaitData
    end;
    if sfSlaveOk in flags then aFlags := aFlags or (1 shl 2);
    if sfNoCursorTimeout in flags then aFlags := aFlags or (1 shl 4);
    if sfPartial in flags then aFlags := aFlags or (1 shl 7);

    //init the aViewRec
    if (ViewTag <> '') and (not assigned(aJSONDocument)) then aViewRec := JSONdata.AddChild(ViewTag, ntobject)
    else aViewRec := JSONdata;

    //init aRecAdded
    aRecAdded := 0;

    //for the TailMonitoring
    repeat

      //do the First query
      aContinue := true;
      OP_QUERY(fSocketDescriptor,
               aFlags,
               fullCollectionName,
               ALIfThen(Skip >= 0, Skip, 0),
               ALIfThen(First >= 0, First, 0), // The MongoDB server returns the query results in batches. Batch size will not exceed
                                               // the maximum BSON document size. For most queries, the first batch returns 101
                                               // documents or just enough documents to exceed 1 megabyte. Subsequent batch size is
                                               // 4 megabytes. To override the default size of the batch, see batchSize() and limit().
               query,
               ReturnFieldsSelector,
               aResponseFlags,
               aCursorID,
               aStartingFrom, // where in the cursor this reply is starting
               aNumberReturned, // number of documents in the reply
               aViewRec,
               OnNewRowFunct,
               ExtData,
               RowTag,
               ViewTag,
               aContinue);

      try

        //init aRecAdded
        aRecAdded := aRecAdded + aNumberReturned;

        //loop still the cursorID > 0
        while ((not (sfTailMonitoring in flags)) or
               (not fStopTailMonitoring)) and
              (aContinue) and
              (aCursorID <> 0) and
              ((First <= 0) or
               (aRecAdded < First)) do begin

          //Get more data
          OP_GET_MORE(fSocketDescriptor,
                      fullCollectionName,
                      ALIfThen(First > 0, First - aRecAdded, 0),
                      aCursorID,
                      aResponseFlags,
                      aStartingFrom,
                      anumberReturned,
                      aViewRec,
                      OnNewRowFunct,
                      ExtData,
                      RowTag,
                      ViewTag,
                      aContinue);

          //init aRecAdded
          aRecAdded := aRecAdded + aNumberReturned;

        end;

      finally

        //close the curson
        if aCursorId <> 0 then OP_KILL_CURSORS(fSocketDescriptor, [aCursorID]);

      end;

      //sleep for TailMonitoring to not use 100% CPU
      if (sfTailMonitoring in flags) and
         (not fStopTailMonitoring) and
         ((First <= 0) or
          (aRecAdded < First)) then sleep(1);

    //loop for the TailMonitoring
    until (not (sfTailMonitoring in flags)) or
          (fStopTailMonitoring) or
          ((First > 0) and
           (aRecAdded >= First));

    //do the OnSelectDataDone
    aStopWatch.Stop;
    OnSelectDataDone(FullCollectionName,
                     Query,
                     ReturnFieldsSelector,
                     flags,
                     RowTag,
                     ViewTag,
                     Skip,
                     First,
                     CacheThreshold,
                     aStopWatch.Elapsed.TotalMilliseconds);

    //save to the cache
    If aCacheKey <> '' then begin

      //save the data
      aViewRec.SaveToJSONString(aCacheStr);
      SaveDataToCache(aCacheKey,
                      CacheThreshold,
                      aCacheStr);

    end;

  Finally
    if assigned(aJSONDocument) then aJSONDocument.free;
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
  if ufUpsert in flags then aFlags := aFlags or (1 shl 0);
  if ufMultiUpdate in flags then aFlags := aFlags or (1 shl 1);

  OP_UPDATE(fSocketDescriptor,
            aflags,
            FullCollectionName,
            selector,
            Update,
            NumberOfDocumentsUpdated,
            updatedExisting,
            ObjectID);

  //do the OnUpdateDataDone
  aStopWatch.Stop;
  OnUpdateDataDone(FullCollectionName,
                   selector,
                   update,
                   flags,
                   aStopWatch.Elapsed.TotalMilliseconds);

end;

{*************************************************************************}
Procedure TAlMongoDBClient.UpdateData(const FullCollectionName: AnsiString;
                                      const Selector: AnsiString;
                                      const Update: AnsiString;
                                      flags: TALMongoDBClientUpdateDataFlags);
var aNumberOfDocumentsUpdated: integer;
    aupdatedExisting: boolean;
    aObjectID: ansiString;
begin
  UpdateData(FullCollectionName,
             selector,
             update,
             flags,
             aNumberOfDocumentsUpdated,
             aupdatedExisting,
             aObjectID);
end;

{*************************************************************************}
Procedure TAlMongoDBClient.UpdateData(const FullCollectionName: AnsiString;
                                      const Selector: AnsiString;
                                      const Update: AnsiString);
var aNumberOfDocumentsUpdated: integer;
    aupdatedExisting: boolean;
    aObjectID: ansiString;
begin
  UpdateData(FullCollectionName,
             selector,
             update,
             [],
             aNumberOfDocumentsUpdated,
             aupdatedExisting,
             aObjectID);
end;

{*************************************************************************}
Procedure TAlMongoDBClient.InsertData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                            // name with the collection name, using a . for the concatenation. For example, for the database
                                                                            // foo and the collection bar, the full collection name is foo.bar
                                      const documents: AnsiString; // One or more documents to insert into the collection. If there are more than one, they are written in sequence, one after another.
                                      flags: TALMongoDBClientInsertDataFlags); // Options (see TALMongoDBClientInsertDataFlags)
Var aFlags: integer;
    aStopWatch: TStopWatch;
begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //init the TstopWatch
  aStopWatch := TstopWatch.Create;

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
  if ifContinueOnError in flags then aFlags := aFlags or (1 shl 0);

  OP_INSERT(fSocketDescriptor,
            aflags,
            FullCollectionName,
            documents);

  //do the OnInsertDataDone
  aStopWatch.Stop;
  OnInsertDataDone(FullCollectionName,
                   documents,
                   flags,
                   aStopWatch.Elapsed.TotalMilliseconds);

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
  if dfSingleRemove in flags then aFlags := aFlags or (1 shl 0);

  OP_DELETE(fSocketDescriptor,
            aflags,
            FullCollectionName,
            selector,
            NumberOfDocumentsRemoved);

  //do the OnDeleteDataDone
  aStopWatch.Stop;
  OnDeleteDataDone(FullCollectionName,
                   selector,
                   flags,
                   aStopWatch.Elapsed.TotalMilliseconds);

end;

{*************************************************************************}
Procedure TAlMongoDBClient.DeleteData(const FullCollectionName: AnsiString;
                                      const Selector: AnsiString;
                                      flags: TALMongoDBClientDeleteDataFlags);
var aNumberOfDocumentsRemoved: integer;
begin
  DeleteData(FullCollectionName,
             selector,
             flags,
             aNumberOfDocumentsRemoved);
end;

{*************************************************************************}
Procedure TAlMongoDBClient.DeleteData(const FullCollectionName: AnsiString;
                                      const Selector: AnsiString);
var aNumberOfDocumentsRemoved: integer;
begin
  DeleteData(FullCollectionName,
             selector,
             [],
             aNumberOfDocumentsRemoved);
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

Var aStopWatch: TStopWatch;
    aDatabaseName: AnsiString;
    aCollectionName: AnsiString;
    aViewRec: TalJSONNode;
    aResponseFlags: integer;
    aTmpQuery: ansiString;
    aCursorID: int64;
    aStartingFrom: integer;
    aNumberReturned: integer;
    aContinue: boolean;
    aTmpRowTag: ansiString;
    aUpdateRowTagByFieldValue: Boolean;
    aJSONDoc: TALJSONDocument;
    aNode1: TALJSONNode;
    aNode2: TALJSONNode;
    alastErrorObjectNode: TALJSONNode;
    P1: integer;

begin

  //Error if we are not connected
  If not connected then raise Exception.Create('Not connected');

  //init the TstopWatch
  aStopWatch := TstopWatch.StartNew;

  //create a temp JSONDoc
  aJSONDoc := TALJSONDocument.Create;
  try

    //init aDatabaseName and aCollectionName
    P1 := AlPos('.',FullCollectionName);
    if P1 <= 0 then raise Exception.Create('The full collection name must be the concatenation of the database name with the collection name');
    aDatabaseName := ALCopyStr(FullCollectionName, 1, P1-1);
    aCollectionName := ALCopyStr(FullCollectionName, P1+1, maxint);

    //buid the query
    aTmpQuery := '{"findAndModify":' + ALJsonEncodeTextWithNodeSubTypeHelper(aCollectionName);
    if query <> '' then aTmpQuery := aTmpQuery + ',"query":'+query;
    if sort <> '' then aTmpQuery := aTmpQuery + ',"sort":'+sort;
    aTmpQuery := aTmpQuery + ',"remove":'+ALJsonEncodeBooleanWithNodeSubTypeHelper(remove);
    if update <> '' then aTmpQuery := aTmpQuery + ',"update":'+update;
    aTmpQuery := aTmpQuery + ',"new":' + ALJsonEncodeBooleanWithNodeSubTypeHelper(new);
    if ReturnFieldsSelector <> '' then aTmpQuery := aTmpQuery + ',"fields":'+ReturnFieldsSelector;
    aTmpQuery := aTmpQuery + ',"upsert":'+ALJsonEncodeBooleanWithNodeSubTypeHelper(InsertIfNotFound);
    aTmpQuery := aTmpQuery + '}';

    //do the First query
    OP_QUERY(fSocketDescriptor,
             0, // flags
             aDatabaseName  + '.$cmd', // fullCollectionName
             0, // skip
             1, // First
             aTmpQuery, // query
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

      //doNodeAutoCreate for the aJSONDoc
      aJSONDoc.Options := [doNodeAutoCreate];

      //init the aViewRec
      if (ViewTag <> '') then aViewRec := JSONdata.AddChild(ViewTag, ntobject)
      else aViewRec := JSONdata;

      //check error
      if aJSONDoc.Node.ChildNodes['ok'].Text <> '1' then begin
        aNode1 := aJSONDoc.Node.ChildNodes.FindNode('errmsg');
        if assigned(aNode1) and (not aNode1.Null) then raise EALException.Create(aNode1.Text)
        else raise Exception.Create('Error calling FindAndModifyData')
      end;

      //get the value node
      aNode1 := aJSONDoc.Node.ChildNodes.FindNode('value');
      if assigned(aNode1) and (aNode1.NodeType = ntObject) then begin

        //init aUpdateRowTagByFieldValue and aTmpRowTag
        if AlPos('&>',RowTag) = 1 then begin
          aTmpRowTag := AlcopyStr(RowTag, 3, maxint);
          aUpdateRowTagByFieldValue := aTmpRowTag <> '';
        end
        else begin
          aTmpRowTag := RowTag;
          aUpdateRowTagByFieldValue := False;
        end;

        //add the row tag
        if (aTmpRowTag <> '') or
           (aViewRec.NodeType = ntarray) then aViewRec := aViewRec.AddChild(aTmpRowTag, ntobject);

        //move the node
        while aNode1.ChildNodes.Count > 0 do begin
          aNode2 := aNode1.ChildNodes.Extract(0);
          try
            aViewRec.ChildNodes.Add(aNode2);
          except
            aNode2.Free;
            raise;
          end;
        end;

        // aUpdateRowTagByFieldValue
        if aUpdateRowTagByFieldValue then begin
          aNode1 := aViewRec.ChildNodes.FindNode(aTmpRowTag);
          if assigned(aNode1) then aViewRec.NodeName := aNode1.Text;
        end;

      end;

      //init alastErrorObjectNode;
      alastErrorObjectNode := aJSONDoc.Node.ChildNodes['lastErrorObject'];

      //NumberOfDocumentsUpdatedOrRemoved
      aNode1 := alastErrorObjectNode.ChildNodes.FindNode('n');
      if assigned(aNode1) then NumberOfDocumentsUpdatedOrRemoved := aNode1.int32
      else NumberOfDocumentsUpdatedOrRemoved := 0;

      //updatedExisting
      aNode1 := alastErrorObjectNode.ChildNodes.FindNode('updatedExisting');
      if assigned(aNode1) then updatedExisting := aNode1.bool
      else updatedExisting := False;

      //ObjectID
      aNode1 := alastErrorObjectNode.ChildNodes.FindNode('upserted');
      if assigned(aNode1) then ObjectID := aNode1.text
      else ObjectID := '';

    finally

      //close the curson
      if aCursorId <> 0 then OP_KILL_CURSORS(fSocketDescriptor, [aCursorID]);

    end;

  finally
    aJSONDoc.free;
  end;

  //do the OnDeleteDataDone
  aStopWatch.Stop;
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
                          aStopWatch.Elapsed.TotalMilliseconds);

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
var aNumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed
    aupdatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
    aObjectID: AnsiString; // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
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
                    aNumberOfDocumentsUpdatedOrRemoved,
                    aupdatedExisting,
                    aObjectID);
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
Var aTickCount: int64;
Begin

  //synchronize the code
  FConnectionPoolCS.Acquire;
  Try

    //raise an exception if currently realeasing all connection
    if FReleasingAllconnections then raise exception.Create('Can not acquire connection: currently releasing all connections');

    //delete the old unused connection
    aTickCount := ALGetTickCount64;
    if aTickCount - fLastConnectionGarbage > (60000 {every minutes})  then begin
      while FConnectionPoolCount > 0 do begin
        if aTickCount - FConnectionPool[0].Lastaccessdate > FConnectionMaxIdleTime then begin

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
      FLastConnectionGarbage := aTickCount;
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
  if SocketDescriptor = INVALID_SOCKET then raise exception.Create('Connection handle can not be INVALID_SOCKET');

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
      FConnectionPool[FConnectionPoolCount].LastAccessDate := ALGetTickCount64;
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
  FLastConnectionGarbage := ALGettickCount64;
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
      FLastConnectionGarbage := ALGetTickCount64;
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

Var aViewRec: TalJSONNode;
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
    aCacheKey: ansiString;
    aCacheStr: ansiString;

begin

  //only OnNewRowFunct / JSONDATA can be used
  if assigned(OnNewRowFunct) then JSONDATA := nil;

  //clear the JSONDATA
  if assigned(JSONDATA) then aJSONDocument := Nil
  else begin
    aJSONDocument := TALJSONDocument.create;
    JSONDATA := aJSONDocument.Node;
  end;

  try

    //init the TstopWatch
    aStopWatch := TstopWatch.Create;

    //Handle the CacheThreshold
    aCacheKey := '';
    If (CacheThreshold > 0) and
       (not assigned(aJSONDocument)) and
       (not (sfTailMonitoring in flags)) and
       ((JSONDATA.ChildNodes.Count = 0) or  // else the save will not work
        (ViewTag <> '')) then begin

      //try to load from from cache
      aCacheKey := ALStringHashSHA1(RowTag + '#' +
                                    alinttostr(Skip) + '#' +
                                    alinttostr(First) + '#' +
                                    FullCollectionName + '#' +
                                    ReturnFieldsSelector + '#' +
                                    Query);

      if loadcachedData(aCacheKey, aCacheStr) then begin

        //init the aViewRec
        if (ViewTag <> '') then aViewRec := JSONDATA.AddChild(ViewTag, ntobject)
        else aViewRec := JSONDATA;

        //assign the tmp data to the XMLData
        aViewRec.LoadFromJsonString(aCacheStr, false{ClearChildNodes});

        //exit
        exit;

      end;

    end;

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
      if sfTailMonitoring in flags then
        raise EAlMongoDBClientException.Create('Tail monitoring work only with TAlMongoDBClient', 0 {aErrorCode}, false {aCloseConnection});
      if sfSlaveOk in flags then aFlags := aFlags or (1 shl 2);
      if sfNoCursorTimeout in flags then aFlags := aFlags or (1 shl 4);
      if sfPartial in flags then aFlags := aFlags or (1 shl 7);

      //init the aViewRec
      if (ViewTag <> '') and (not assigned(aJSONDocument)) then aViewRec := JSONdata.AddChild(ViewTag, ntobject)
      else aViewRec := JSONdata;

      //init aRecAdded
      aRecAdded := 0;

      //do the First query
      aContinue := true;
      OP_QUERY(aTMPConnectionSocket,
               aFlags,
               fullCollectionName,
               ALIfThen(Skip >= 0, Skip, 0),
               ALIfThen(First >= 0, First, 0), // The MongoDB server returns the query results in batches. Batch size will not exceed
                                               // the maximum BSON document size. For most queries, the first batch returns 101
                                               // documents or just enough documents to exceed 1 megabyte. Subsequent batch size is
                                               // 4 megabytes. To override the default size of the batch, see batchSize() and limit().
               query,
               ReturnFieldsSelector,
               aResponseFlags,
               aCursorID,
               aStartingFrom, // where in the cursor this reply is starting
               aNumberReturned, // number of documents in the reply
               aViewRec,
               OnNewRowFunct,
               ExtData,
               RowTag,
               ViewTag,
               aContinue);

      try

        //init aRecAdded
        aRecAdded := aRecAdded + aNumberReturned;

        //loop still the cursorID > 0
        while (aContinue) and
              (aCursorID <> 0) and
              ((First <= 0) or
               (aRecAdded < First)) do begin

          //Get more data
          OP_GET_MORE(aTMPConnectionSocket,
                      fullCollectionName,
                      ALIfThen(First > 0, First - aRecAdded, 0),
                      aCursorID,
                      aResponseFlags,
                      aStartingFrom,
                      anumberReturned,
                      aViewRec,
                      OnNewRowFunct,
                      ExtData,
                      RowTag,
                      ViewTag,
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
      OnSelectDataDone(FullCollectionName,
                       Query,
                       ReturnFieldsSelector,
                       flags,
                       RowTag,
                       ViewTag,
                       Skip,
                       First,
                       CacheThreshold,
                       aStopWatch.Elapsed.TotalMilliseconds);

      //save to the cache
      If aCacheKey <> '' then begin

        //save the data
        aViewRec.SaveToJSONString(aCacheStr);
        SaveDataToCache(aCacheKey,
                        CacheThreshold,
                        aCacheStr);

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

  finally
    if assigned(aJSONDocument) then aJSONDocument.free;
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
    if ufUpsert in flags then aFlags := aFlags or (1 shl 0);
    if ufMultiUpdate in flags then aFlags := aFlags or (1 shl 1);

    OP_UPDATE(aTMPConnectionSocket,
              aflags,
              FullCollectionName,
              selector,
              Update,
              NumberOfDocumentsUpdated,
              updatedExisting,
              ObjectID);

    //do the OnUpdateDataDone
    aStopWatch.Stop;
    OnUpdateDataDone(FullCollectionName,
                     selector,
                     update,
                     flags,
                     aStopWatch.Elapsed.TotalMilliseconds);

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

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.UpdateData(const FullCollectionName: AnsiString;
                                                    const Selector: AnsiString;
                                                    const Update: AnsiString;
                                                    flags: TALMongoDBClientUpdateDataFlags;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aNumberOfDocumentsUpdated: integer;
    aupdatedExisting: boolean;
    aObjectID: ansiString;
begin
  UpdateData(FullCollectionName,
             selector,
             update,
             flags,
             aNumberOfDocumentsUpdated,
             aupdatedExisting,
             aObjectID,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.UpdateData(const FullCollectionName: AnsiString;
                                                    const Selector: AnsiString;
                                                    const Update: AnsiString;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aNumberOfDocumentsUpdated: integer;
    aupdatedExisting: boolean;
    aObjectID: ansiString;
begin
  UpdateData(FullCollectionName,
             selector,
             update,
             [],
             aNumberOfDocumentsUpdated,
             aupdatedExisting,
             aObjectID,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.InsertData(const FullCollectionName: AnsiString; // The full collection name. The full collection name is the concatenation of the database
                                                                                          // name with the collection name, using a . for the concatenation. For example, for the database
                                                                                          // foo and the collection bar, the full collection name is foo.bar
                                                    const documents: AnsiString; // One or more documents to insert into the collection. If there are more than one, they are written in sequence, one after another.
                                                    flags: TALMongoDBClientInsertDataFlags;// Options (see TALMongoDBClientInsertDataFlags)
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
    aStopWatch := TstopWatch.Create;

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
    if ifContinueOnError in flags then aFlags := aFlags or (1 shl 0);

    OP_INSERT(aTMPConnectionSocket,
              aflags,
              FullCollectionName,
              documents);

    //do the OnInsertDataDone
    aStopWatch.Stop;
    OnInsertDataDone(FullCollectionName,
                     documents,
                     flags,
                     aStopWatch.Elapsed.TotalMilliseconds);

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
    if dfSingleRemove in flags then aFlags := aFlags or (1 shl 0);

    OP_DELETE(aTMPConnectionSocket,
              aflags,
              FullCollectionName,
              selector,
              NumberOfDocumentsRemoved);

    //do the OnDeleteDataDone
    aStopWatch.Stop;
    OnDeleteDataDone(FullCollectionName,
                     selector,
                     flags,
                     aStopWatch.Elapsed.TotalMilliseconds);

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

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.DeleteData(const FullCollectionName: AnsiString;
                                                    const Selector: AnsiString;
                                                    flags: TALMongoDBClientDeleteDataFlags;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aNumberOfDocumentsRemoved: integer;
begin
  DeleteData(FullCollectionName,
             selector,
             flags,
             aNumberOfDocumentsRemoved,
             ConnectionSocket);
end;

{***************************************************************************************}
Procedure TAlMongoDBConnectionPoolClient.DeleteData(const FullCollectionName: AnsiString;
                                                    const Selector: AnsiString;
                                                    const ConnectionSocket: TSocket = INVALID_SOCKET);
var aNumberOfDocumentsRemoved: integer;
begin
  DeleteData(FullCollectionName,
             selector,
             [],
             aNumberOfDocumentsRemoved,
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

Var aTMPConnectionSocket: TSocket;
    aOwnConnection: Boolean;
    aStopWatch: TStopWatch;
    aDatabaseName: AnsiString;
    aCollectionName: AnsiString;
    aViewRec: TalJSONNode;
    aResponseFlags: integer;
    aTmpQuery: ansiString;
    aCursorID: int64;
    aStartingFrom: integer;
    aNumberReturned: integer;
    aContinue: boolean;
    aTmpRowTag: ansiString;
    aUpdateRowTagByFieldValue: Boolean;
    aJSONDoc: TALJSONDocument;
    aNode1: TALJSONNode;
    aNode2: TALJSONNode;
    alastErrorObjectNode: TALJSONNode;
    P1: integer;

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

    //create a temp JSONDoc
    aJSONDoc := TALJSONDocument.Create;
    try

      //init aDatabaseName and aCollectionName
      P1 := AlPos('.',FullCollectionName);
      if P1 <= 0 then raise Exception.Create('The full collection name must be the concatenation of the database name with the collection name');
      aDatabaseName := ALCopyStr(FullCollectionName, 1, P1-1);
      aCollectionName := ALCopyStr(FullCollectionName, P1+1, maxint);

      //buid the query
      aTmpQuery := '{"findAndModify":' + ALJsonEncodeTextWithNodeSubTypeHelper(aCollectionName);
      if query <> '' then aTmpQuery := aTmpQuery + ',"query":'+query;
      if sort <> '' then aTmpQuery := aTmpQuery + ',"sort":'+sort;
      aTmpQuery := aTmpQuery + ',"remove":'+ALJsonEncodeBooleanWithNodeSubTypeHelper(remove);
      if update <> '' then aTmpQuery := aTmpQuery + ',"update":'+update;
      aTmpQuery := aTmpQuery + ',"new":' + ALJsonEncodeBooleanWithNodeSubTypeHelper(new);
      if ReturnFieldsSelector <> '' then aTmpQuery := aTmpQuery + ',"fields":'+ReturnFieldsSelector;
      aTmpQuery := aTmpQuery + ',"upsert":'+ALJsonEncodeBooleanWithNodeSubTypeHelper(InsertIfNotFound);
      aTmpQuery := aTmpQuery + '}';

      //do the First query
      OP_QUERY(aTMPConnectionSocket,
               0, // flags
               aDatabaseName + '.$cmd', // fullCollectionName
               0, // skip
               1, // First
               aTmpQuery, // query
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

        //doNodeAutoCreate for the aJSONDoc
        aJSONDoc.Options := [doNodeAutoCreate];

        //init the aViewRec
        if (ViewTag <> '') then aViewRec := JSONdata.AddChild(ViewTag, ntobject)
        else aViewRec := JSONdata;

        //check error
        if aJSONDoc.Node.ChildNodes['ok'].Text <> '1' then begin
          aNode1 := aJSONDoc.Node.ChildNodes.FindNode('errmsg');
          if assigned(aNode1) and (not aNode1.Null) then raise EALException.Create(aNode1.Text)
          else raise Exception.Create('Error calling FindAndModifyData')
        end;

        //get the value node
        aNode1 := aJSONDoc.Node.ChildNodes.FindNode('value');
        if assigned(aNode1) and (aNode1.NodeType = ntObject) then begin

          //init aUpdateRowTagByFieldValue and aTmpRowTag
          if AlPos('&>',RowTag) = 1 then begin
            aTmpRowTag := AlcopyStr(RowTag, 3, maxint);
            aUpdateRowTagByFieldValue := aTmpRowTag <> '';
          end
          else begin
            aTmpRowTag := RowTag;
            aUpdateRowTagByFieldValue := False;
          end;

          //add the row tag
          if (aTmpRowTag <> '') or
             (aViewRec.NodeType = ntarray) then aViewRec := aViewRec.AddChild(aTmpRowTag, ntobject);

          //move the node
          while aNode1.ChildNodes.Count > 0 do begin
            aNode2 := aNode1.ChildNodes.Extract(0);
            try
              aViewRec.ChildNodes.Add(aNode2);
            except
              aNode2.Free;
              raise;
            end;
          end;

          // aUpdateRowTagByFieldValue
          if aUpdateRowTagByFieldValue then begin
            aNode1 := aViewRec.ChildNodes.FindNode(aTmpRowTag);
            if assigned(aNode1) then aViewRec.NodeName := aNode1.Text;
          end;

        end;

        //init alastErrorObjectNode;
        alastErrorObjectNode := aJSONDoc.Node.ChildNodes['lastErrorObject'];

        //NumberOfDocumentsUpdatedOrRemoved
        aNode1 := alastErrorObjectNode.ChildNodes.FindNode('n');
        if assigned(aNode1) then NumberOfDocumentsUpdatedOrRemoved := aNode1.int32
        else NumberOfDocumentsUpdatedOrRemoved := 0;

        //updatedExisting
        aNode1 := alastErrorObjectNode.ChildNodes.FindNode('updatedExisting');
        if assigned(aNode1) then updatedExisting := aNode1.bool
        else updatedExisting := False;

        //ObjectID
        aNode1 := alastErrorObjectNode.ChildNodes.FindNode('upserted');
        if assigned(aNode1) then ObjectID := aNode1.text
        else ObjectID := '';

      finally

        //close the curson
        if aCursorId <> 0 then OP_KILL_CURSORS(aTMPConnectionSocket, [aCursorID]);

      end;

    finally
      aJSONDoc.free;
    end;

    //do the OnDeleteDataDone
    aStopWatch.Stop;
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
                            aStopWatch.Elapsed.TotalMilliseconds);

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
var aNumberOfDocumentsUpdatedOrRemoved: integer; // reports the number of documents updated or removed
    aupdatedExisting: boolean; // is true when an update affects at least one document and does not result in an upsert.
    aObjectID: AnsiString; // upserted is an ObjectId that corresponds to the upserted document if the update resulted in an insert.
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
                    aNumberOfDocumentsUpdatedOrRemoved,
                    aupdatedExisting,
                    aObjectID,
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
