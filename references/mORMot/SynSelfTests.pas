/// automated tests for common units of the Synopse mORMot Framework
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynSelfTests;

{
    This file is part of Synopse mORMot framework.

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

  The Original Code is Synopse framework.

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

}

interface

{$I Synopse.inc} // define HASINLINE CPU32 CPU64

{$ifdef ISDELPHIXE}
  // since Delphi XE, we have unit System.RegularExpressionsAPI available
  {$define TEST_REGEXP}
{$else}
  // define only if you have unit PCRE.pas installed (not set by default)
  {.$define TEST_REGEXP}
{$endif}

uses
  {$ifdef MSWINDOWS}
    Windows,
  {$else}
    {$ifdef KYLIX3}
    Types,
    LibC,
    SynKylix,
    {$endif}
    {$ifdef FPC}
    SynFPCLinux,
    BaseUnix,
    {$endif}
  {$endif}
  Classes,
  SynCrtSock,
  SynTable, // for TSynTableStatement
  {$ifndef NOVARIANTS}
    SynMongoDB,
    SynMustache,
    Variants,
  {$endif}
  {$ifdef UNICODE}
    Generics.Collections,
  {$endif}
  SysUtils,
  {$ifndef LVCL}
    Contnrs,
    {$ifdef MSWINDOWS}
    SynOleDB,
    {$ifndef FPC}
      SynGdiPlus,
      SynPdf,
    {$endif}
    {$endif}
  {$endif LVCL}
  SynEcc,
  SynDB,
  SynSQLite3,
  SynSQLite3Static,
  SynDBSQLite3,
  SynDBRemote,
  SynDBODBC,
  {$ifndef DELPHI5OROLDER}
    mORMot,
    mORMotDB,
    mORMotSQLite3,
    mORMotHttpServer,
    mORMotHttpClient,
    {$ifndef NOVARIANTS}
      mORMotMongoDB,
      mORMotMVC,
    {$endif}
    SynBidirSock,
    mORMotDDD,
    dddDomCountry,
    dddDomUserTypes,
    dddDomUserInterfaces,
    dddDomAuthInterfaces,
    dddInfraEmail,
    dddInfraEmailer,
    dddInfraAuthRest,
    dddInfraRepoUser,
    ECCProcess {$ifdef FPC} in '.\SQLite3\Samples\33 - ECC\ECCProcess.pas' {$endif},
  {$endif DELPHI5OROLDER}
  mORMotService,
  SynProtoRTSPHTTP,
  SynProtoRelay,
  {$ifdef TEST_REGEXP}
    SynSQLite3RegEx,
  {$endif TEST_REGEXP}
  {$ifdef MSWINDOWS}
    {$ifdef USEZEOS}
      SynDBZeos,
    {$endif}
  {$endif}
  SynCommons,
  SynLog,
  SynTests;




{ ************ Unit-Testing classes and functions }

{$ifndef DELPHI5OROLDER}

const
  {$ifdef MSWINDOWS}
  HTTP_DEFAULTPORT = '888';

  // if this library file is available and USEZEOS conditional is set, will run
  //   TTestExternalDatabase.FirebirdEmbeddedViaODBC
  // !! download driver from http://www.firebirdsql.org/en/odbc-driver
  FIREBIRDEMBEDDEDDLL = 'd:\Dev\Lib\SQLite3\Samples\15 - External DB performance\Firebird'+
    {$ifdef CPU64}'64'+{$endif=}'\fbembed.dll';

  {$else}

  HTTP_DEFAULTPORT = '8888'; // under Linux, port<1024 needs root user

  {$endif MSWINDOWS}


type
  // a record mapping used in the test classes of the framework
  // - this class can be used for debugging purposes, with the database
  // created by TTestFileBased in mORMotSQLite3.pas
  // - this class will use 'People' as a table name
  TSQLRecordPeople = class(TSQLRecord)
  private
    fData: TSQLRawBlob;
    fFirstName: RawUTF8;
    fLastName: RawUTF8;
    fYearOfBirth: integer;
    fYearOfDeath: word;
  published
    property FirstName: RawUTF8 read fFirstName write fFirstName;
    property LastName: RawUTF8 read fLastName write fLastName;
    property Data: TSQLRawBlob read fData write fData;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
  public
    /// method used to test the Client-Side
    // ModelRoot/TableName/ID/MethodName RESTful request, i.e.
    // ModelRoot/People/ID/DataAsHex in this case
    // - this method calls the supplied TSQLRestClient to retrieve its results,
    // with the ID taken from the current TSQLRecordPeole instance ID field
    // - parameters and result types depends on the purpose of the function
    // - TSQLRestServerTest.DataAsHex published method implements the result
    // calculation on the Server-Side
    function DataAsHex(aClient: TSQLRestClientURI): RawUTF8;
    /// method used to test the Client-Side
    // ModelRoot/MethodName RESTful request, i.e. ModelRoot/Sum in this case
    // - this method calls the supplied TSQLRestClient to retrieve its results
    // - parameters and result types depends on the purpose of the function
    // - TSQLRestServerTest.Sum published method implements the result calculation
    // on the Server-Side
    // - this method doesn't expect any ID to be supplied, therefore will be
    // called as class function - normally, it should be implement in a
    // TSQLRestClient descendant, and not as a TSQLRecord, since it does't depend
    // on TSQLRecordPeople at all
    // - you could also call the same servce from the ModelRoot/People/ID/Sum URL,
    // but it won't make any difference)
    class function Sum(aClient: TSQLRestClientURI; a, b: double; Method2: boolean): double;
  end;

 TSQLRecordTest = class(TSQLRecord)
 private
    fTest: RawUTF8;
    fValfloat: double;
    fValWord: word;
    fNext: TSQLRecordTest;
    fInt: int64;
    fValDate: TDateTime;
    fData: TSQLRawBlob;
    fAnsi: WinAnsiString;
    fUnicode: RawUnicode;
    {$ifndef NOVARIANTS}
    fVariant: variant;
    {$endif}
    procedure SetInt(const Value: int64);
 public
   procedure FillWith(i: Integer);
   procedure CheckWith(test: TSynTestCase; i: Integer; offset: integer=0;
     checkblob: boolean=true);
 published
   property Int: int64 read fInt write SetInt default 12;
   property Test: RawUTF8 read fTest write fTest;
   property Unicode: RawUnicode read fUnicode write fUnicode;
   property Ansi: WinAnsiString read fAnsi  write fAnsi;
   property ValFloat: double read fValfloat write fValFloat;
   property ValWord: word read fValWord write fValWord;
   property ValDate: tdatetime read fValDate write fValDate;
   property Next: TSQLRecordTest read fNext write fNext;
   property Data: TSQLRawBlob read fData write fData;
   {$ifndef NOVARIANTS}
   property ValVariant: variant read fVariant write fVariant;
   {$endif}
 end;
{$endif}

type
  /// this test case will test most functions, classes and types defined and
  // implemented in the SynCommons unit
  TTestLowLevelCommon = class(TSynTestCase)
  protected
    {$ifndef DELPHI5OROLDER}
    a: array of TSQLRecordPeople;
    {$endif}
    fAdd,fDel: RawUTF8;
    fQuickSelectValues: TIntegerDynArray;
    function QuickSelectGT(IndexA,IndexB: PtrInt): boolean;
    procedure intadd(const Sender; Value: integer);
    procedure intdel(const Sender; Value: integer);
  published
    /// the faster CopyRecord function, enhancing the system.pas unit
    procedure SystemCopyRecord;
    /// test the TRawUTF8List class
    procedure _TRawUTF8List;
    /// test the TDynArray object and methods
    procedure _TDynArray;
    /// test the TDynArrayHashed object and methods (dictionary features)
    // - this test will create an array of 200,000 items to test speed
    procedure _TDynArrayHashed;
    /// test the TSynDictionary class
    procedure _TSynDictionary;
    /// validate the TSynQueue class
    procedure _TSynQueue;
    /// test TObjectListHashed class
    procedure _TObjectListHashed;
    /// test TObjectListSorted class
    procedure _TObjectListSorted;
    /// test TSynNameValue class
    procedure _TSynNameValue;
    /// test TRawUTF8Interning process
    procedure _TRawUTF8Interning;
    {$ifndef DELPHI5OROLDER}
    /// test TObjectDynArrayWrapper class
    procedure _TObjectDynArrayWrapper;
    /// test T*ObjArray types and the ObjArray*() wrappers
    procedure _TObjArray;
    {$endif DELPHI5OROLDER}
    {$ifdef CPUINTEL}
    /// validate our optimized MoveFast/FillCharFast functions
    procedure CustomRTL;
    {$endif CPUINTEL}
    /// test StrIComp() and AnsiIComp() functions
    procedure FastStringCompare;
    /// test IdemPropName() and IdemPropNameU() functions
    procedure _IdemPropName;
    /// test UrlEncode() and UrlDecode() functions
    procedure UrlEncoding;
    /// test our internal fast TGUID process functions
    procedure _GUID;
    /// test ParseCommandArguments() function
    procedure _ParseCommandArguments;
    /// test IsMatch() function
    procedure _IsMatch;
    /// test TExprParserMatch class
    procedure _TExprParserMatch;
    /// the Soundex search feature (i.e. TSynSoundex and all related
    // functions)
    procedure Soundex;
    /// low level fast Integer or Floating-Point to/from string conversion
    // - especially the RawUTF8 or PUTF8Char relative versions
    procedure NumericalConversions;
    /// test low-level integer/Int64 functions
    procedure Integers;
    /// test crc32c in both software and hardware (SSE4.2) implementations
    procedure _crc32c;
    /// test RDRAND Intel x86/x64 opcode if available, or fast gsl_rng_taus2
    procedure _Random32;
    /// test TSynBloomFilter class
    procedure BloomFilters;
    /// test DeltaCompress/DeltaExtract functions
    procedure _DeltaCompress;
    /// the new fast Currency to/from string conversion
    procedure Curr64;
    /// the camel-case / camel-uncase features, used for i18n from Delphi RTII
    procedure _CamelCase;
    /// the low-level bit management functions
    procedure Bits;
    /// the fast .ini file content direct access
    procedure IniFiles;
    /// test UTF-8 and Win-Ansi conversion (from or to, through RawUnicode)
    procedure _UTF8;
    /// test UrlEncode() and UrlDecode() functions
    // - this method use some ISO-8601 encoded dates and times for the testing
    procedure UrlDecoding;
    /// test ASCII Baudot encoding
    procedure BaudotCode;
    /// the ISO-8601 date and time encoding
    // - test especially the conversion to/from text
    procedure Iso8601DateAndTime;
    /// test the TSynTimeZone class and its cross-platform local time process
    procedure TimeZones;
    /// test mime types recognition
    procedure MimeTypes;
    /// validates the median computation using the "Quick Select" algorithm
    procedure QuickSelect;
    /// test TSynTable class and TSynTableVariantType new variant type
    procedure _TSynTable;
    /// test the TSynCache class
    procedure _TSynCache;
    /// low-level TSynFilter classes
    procedure _TSynFilter;
    /// low-level TSynValidate classes
    procedure _TSynValidate;
    /// low-level TSynLogFile class
    procedure _TSynLogFile;
    /// client side geniune 64 bit identifiers generation
    procedure _TSynUniqueIdentifier;
  end;

  /// this test case will test most low-level functions, classes and types
  // defined and implemented in the mORMot.pas unit
  TTestLowLevelTypes = class(TSynTestCase)
{$ifndef NOVARIANTS}
  protected
    procedure MustacheTranslate(var English: string);
    procedure MustacheHelper(const Value: variant; out result: variant);
{$endif}
  published
{$ifndef DELPHI5OROLDER}
    /// some low-level RTTI access
    // - especially the field type retrieval from published properties
    procedure RTTI;
{$endif}
    /// some low-level Url encoding from parameters
    procedure UrlEncoding;
    /// some low-level JSON encoding/decoding
    procedure EncodeDecodeJSON;
    /// HTML generation from Wiki Or Markdown syntax
    procedure WikiMarkdownToHtml;
{$ifndef NOVARIANTS}
    /// some low-level variant process
    procedure Variants;
    /// test the Mustache template rendering unit
    procedure MustacheRenderer;
{$ifndef DELPHI5OROLDER}
{$ifndef LVCL}
    /// variant-based JSON/BSON document process
    procedure _TDocVariant;
    /// low-level TDecimal128 decimal value process (as used in BSON)
    procedure _TDecimal128;
    /// BSON process (using TDocVariant)
    procedure _BSON;
{$endif LVCL}
    /// test SELECT statement parsing
    procedure _TSynTableStatement;
    /// test advanced statistics monitoring
    procedure _TSynMonitorUsage;
{$endif DELPHI5OROLDER}
{$endif NOVARIANTS}
  end;

{$ifndef DELPHI5OROLDER}

  /// this test case will test some generic classes
  // defined and implemented in the mORMot.pas unit
  TTestBasicClasses = class(TSynTestCase)
  published
    /// test the TSQLRecord class
    // - especially SQL auto generation, or JSON export/import
    procedure _TSQLRecord;
    /// test the digital signature of records
    procedure _TSQLRecordSigned;
    /// test the TSQLModel class
    procedure _TSQLModel;
    /// test a full in-memory server over Windows Messages
    // - Under Linux, URIDll will be used instead due to lack of message loop
    // - without any SQLite3 engine linked
    procedure _TSQLRestServerFullMemory;
  end;

{$endif DELPHI5OROLDER}

  /// this test case will test most functions, classes and types defined and
  // implemented in the SynZip unit
  TTestCompression = class(TSynTestCase)
  protected
    Data: RawByteString;
    M: THeapMemoryStream;
    crc0,crc1: cardinal;
  public
    procedure Setup; override;
    procedure CleanUp; override;
  published
    /// direct deflate/inflate functions
    procedure InMemoryCompression;
    /// .gzip archive handling
    procedure GZIPFormat;
    /// .zip archive handling
    procedure ZIPFormat;
    /// SynLZO internal format
    procedure _SynLZO;
    /// SynLZ internal format
    procedure _SynLZ;
    /// TAlgoCompress classes
    procedure _TAlgoCompress;
  end;

  /// this test case will test most functions, classes and types defined and
  // implemented in the SynCrypto unit
  TTestCryptographicRoutines = class(TSynTestCase)
  public
    procedure CryptData(dpapi: boolean);
  published
    /// Adler32 hashing functions
    procedure _Adler32;
    /// MD5 hashing functions
    procedure _MD5;
    /// SHA-1 hashing functions
    procedure _SHA1;
    /// SHA-256 hashing functions
    procedure _SHA256;
    /// SHA-512 hashing functions
    procedure _SHA512;
    /// SHA-3 / Keccak hashing functions
    procedure _SHA3;
    /// AES encryption/decryption functions
    procedure _AES256;
    /// AES-GCM encryption/decryption with authentication
    procedure _AES_GCM;
    /// RC4 encryption function
    procedure _RC4;
    /// Base-64 encoding/decoding functions
    procedure _Base64;
    /// CompressShaAes() using SHA-256 / AES-256-CTR algorithm over SynLZ
    procedure _CompressShaAes;
    /// AES-based pseudorandom number generator
    procedure _TAESPNRG;
    /// CryptDataForCurrentUser() function
    procedure _CryptDataForCurrentUser;
    {$ifdef MSWINDOWS}
    /// CryptDataForCurrentUserAPI() function
    procedure _CryptDataForCurrentUserAPI;
    {$endif MSWINDOWS}
    {$ifndef NOVARIANTS}
    /// JWT classes
    procedure _JWT;
    {$endif NOVARIANTS}
    /// compute some performance numbers, mostly against regression
    procedure Benchmark;
  end;

  /// this test case will test ECDH and ECDSA cryptography as implemented
  // in the SynECC unit
  TTestECCCryptography = class(TSynTestCase)
  protected
    pub: array of TECCPublicKey;
    priv: array of TECCPrivateKey;
    sign: array of TECCSignature;
    hash: TECCHash;
  published
    /// avoid regression among platforms and compilers
    procedure ReferenceVectors;
    /// ECC private/public keys generation
    procedure _ecc_make_key;
    /// ECDSA signature computation
    procedure _ecdsa_sign;
    /// ECDSA signature verification
    procedure _ecdsa_verify;
    /// ECDH key derivation
    procedure _ecdh_shared_secret;
    /// ECDSA certificates chains and digital signatures
    procedure CertificatesAndSignatures;
    {$ifndef DELPHI5OROLDER}
    /// run most commands of the ECC tool
    procedure ECCCommandLineTool;
    {$endif}
    /// ECDHE stream protocol
    procedure ECDHEStreamProtocol;
  end;

  /// this test case will validate several low-level protocols
  TTestProtocols = class(TSynTestCase)
  published
    /// RTSP over HTTP, as implemented in SynProtoRTSPHTTP unit
    procedure RTSPOverHTTP;
  end;

{$ifdef MSWINDOWS}
{$ifndef LVCL}
{$ifndef FPC}
  /// this test case will test most functions, classes and types defined and
  // implemented in the SynPDF unit
  TTestSynopsePDF = class(TSynTestCase)
  published
    /// create a PDF document, using the PDF Canvas property
    // - test font handling, especially standard font substitution
    procedure _TPdfDocument;
    /// create a PDF document, using a EMF content
    // - validates the EMF/TMetaFile enumeration, and its conversion into the
    // PDF content, including PDF-1.5 and page orientation
    // - this method will produce a .pdf file in the executable directory,
    // if you want to check out the result (it's simply a curve drawing, with
    // data from NIST)
    procedure _TPdfDocumentGDI;
  end;
{$endif}
{$endif}
{$endif}

{$ifndef DELPHI5OROLDER}

{$ifndef LVCL}
type
  TCollTest = class(TCollectionItem)
  private
    FLength: Integer;
    FColor: Integer;
    FName: RawUTF8;
  published
    property Color: Integer read FColor write FColor;
    property Length: Integer read FLength write FLength;
    property Name: RawUTF8 read FName write FName;
  end;

  TCollTestsI = class(TInterfacedCollection)
  protected
    class function GetClass: TCollectionItemClass; override;
  end;
{$endif LVCL}


type
  /// a parent test case which will test most functions, classes and types defined
  // and implemented in the mORMotSQLite3 unit, i.e. the SQLite3 engine itself
  // - it should not be called directly, but through TTestFileBased,
  // TTestMemoryBased and TTestMemoryBased children
  TTestSQLite3Engine = class(TSynTestCase)
  protected
    { these values are used internaly by the published methods below }
    BackupProgressStep: TSQLDatabaseBackupEventStep; // should be the first
    TempFileName: TFileName;
    EncryptedFile: boolean;
    Demo: TSQLDataBase;
    Req: RawUTF8;
    JS: RawUTF8;
    BackupTimer: TPrecisionTimer;
    function OnBackupProgress(Sender: TSQLDatabaseBackupThread): Boolean;
  published
    /// test direct access to the SQLite3 engine
    // - i.e. via TSQLDataBase and TSQLRequest classes
    procedure DatabaseDirectAccess;
    /// test direct access to the Virtual Table features of SQLite3
    procedure VirtualTableDirectAccess;
    /// test the TSQLTableJSON table
    // - the JSON content generated must match the original data
    // - a VACCUM is performed, for testing some low-level SQLite3 engine
    // implementation
    // - the SortField feature is also tested
    procedure _TSQLTableJSON;
    /// test the TSQLRestClientDB, i.e. a local Client/Server driven usage
    // of the framework
    // - validates TSQLModel, TSQLRestServer and TSQLRestStorage by checking
    // the coherency of the data between client and server instances, after
    // update from both sides
    // - use all RESTful commands (GET/UDPATE/POST/DELETE...)
    // - test the 'many to many' features (i.e. TSQLRecordMany) and dynamic
    // arrays published properties handling
    // - test dynamic tables
    procedure _TSQLRestClientDB;
    {$ifdef TEST_REGEXP}
    /// check the PCRE-based REGEX function
    procedure RegexpFunction;
    {$endif TEST_REGEXP}
    /// test Master/Slave replication using TRecordVersion field
    procedure _TRecordVersion;
  end;

  /// this test case will test most functions, classes and types defined and
  // implemented in the mORMotSQLite3 unit, i.e. the SQLite3 engine itself,
  // with a file-based approach
  TTestFileBased = class(TTestSQLite3Engine);

  /// this test case will test most functions, classes and types defined and
  // implemented in the mORMotSQLite3 unit, i.e. the SQLite3 engine itself,
  // with a memory-based approach
  // - this class will also test the TSQLRestStorage class, and its
  // 100% Delphi simple database engine
  TTestMemoryBased = class(TTestSQLite3Engine)
  protected
    function CreateShardDB(maxshard: Integer): TSQLRestServer;
  published
    /// test the TSQLTableWritable table
    procedure _TSQLTableWritable;
    /// validate RTREE virtual tables
    procedure _RTree;
    /// validate TSQLRestStorageShardDB add operation, with or without batch
    procedure ShardWrite;
    /// validate TSQLRestStorageShardDB reading among all sharded databases
    procedure ShardRead;
    /// validate TSQLRestStorageShardDB reading after deletion of several shards
    procedure ShardReadAfterPurge;
    /// validate TSQLRestStorageShardDB.MaxShardCount implementation
    procedure _MaxShardCount;
  end;

  /// this test case will test most functions, classes and types defined and
  // implemented in the mORMotSQLite3 unit, i.e. the SQLite3 engine itself,
  // with a file-based approach
  // - purpose of this class is to test Write-Ahead Logging for the database
  TTestFileBasedWAL = class(TTestFileBased);

  /// this test case will test most functions, classes and types defined and
  // implemented in the mORMotSQLite3 unit, i.e. the SQLite3 engine itself,
  // with a file-based approach
  // - purpose of this class is to test Memory-Mapped I/O for the database
  TTestFileBasedMemoryMap = class(TTestFileBased);

  /// this test case will test most functions, classes and types defined and
  // implemented in the mORMotSQLite3 unit, i.e. the SQLite3 engine itself,
  // used as a HTTP/1.1 server and client
  // - test a HTTP/1.1 server and client on the port 888 of the local machine
  // - require the 'test.db3' SQLite3 database file, as created by TTestFileBased
  TTestClientServerAccess = class(TSynTestCase)
  protected
    { these values are used internaly by the published methods below }
    Model: TSQLModel;
    DataBase: TSQLRestServerDB;
    Server: TSQLHttpServer;
    Client: TSQLRestClientURI;
    /// perform the tests of the current Client instance
    procedure ClientTest;
    /// release used instances (e.g. http server) and memory
    procedure CleanUp; override;
  public
    /// this could be called as administrator for THttpApiServer to work
    {$ifdef MSWINDOWS}
    class function RegisterAddUrl(OnlyDelete: boolean): string;
    {$endif}
  published
    /// initialize a TSQLHttpServer instance
    // - uses the 'test.db3' SQLite3 database file generated by TTestSQLite3Engine
    // - creates and validates a HTTP/1.1 server on the port 888 of the local
    // machine, using the THttpApiServer (using kernel mode http.sys) class
    // if available
    procedure _TSQLHttpServer;
    /// validate the HTTP/1.1 client implementation
    // - by using a request of all records data
    procedure _TSQLHttpClient;
    /// validate the HTTP/1.1 client multi-query implementation with one
    // connection for the all queries
    // - this method keep alive the HTTP connection, so is somewhat faster
    // - it runs 1000 remote SQL queries, and check the JSON data retrieved
    // - the time elapsed for this step is computed, and displayed on the report
    procedure HTTPClientKeepAlive;
    /// validate the HTTP/1.1 client multi-query implementation with one
    // connection initialized per query
    // - this method don't keep alive the HTTP connection, so is somewhat slower:
    // a new HTTP connection is created for every query
    // - it runs 1000 remote SQL queries, and check the JSON data retrieved
    // - the time elapsed for this step is computed, and displayed on the report
    procedure HTTPClientMultiConnect;
    /// validate the HTTP/1.1 client multi-query implementation with one
    // connection for the all queries and our proprietary SHA-256 / AES-256-CTR
    // encryption encoding
    // - it runs 1000 remote SQL queries, and check the JSON data retrieved
    // - the time elapsed for this step is computed, and displayed on the report
    procedure HTTPClientEncrypted;
    /// validates TSQLRest.SetCustomEncryption process with AES+SHA
    procedure HTTPClientCustomEncryptionAesSha;
    /// validates TSQLRest.SetCustomEncryption process with only AES
    procedure HTTPClientCustomEncryptionAes;
    /// validates TSQLRest.SetCustomEncryption process with only SHA
    procedure HTTPClientCustomEncryptionSha;
{
    /// validate the HTTP/1.1 client multi-query implementation with one
    // connection for all queries, and the THttpServer class instead
    // of the THttpApiServer kernel mode server
    procedure HTTPClientKeepAliveDelphi;
    /// validate the HTTP/1.1 client multi-query implementation with one
    // connection initialized per query, and the THttpServer class instead
    // of the THttpApiServer kernel mode server
    // - this method don't keep alive the HTTP connection, so is somewhat slower:
    // a new HTTP connection is created for every query
    procedure HTTPClientMultiConnectDelphi;
}
{$ifdef MSWINDOWS}
    /// validate the Named-Pipe client implementation
    // - it first launch the Server as Named-Pipe
    // - it then runs 1000 remote SQL queries, and check the JSON data retrieved
    // - the time elapsed for this step is computed, and displayed on the report
    procedure NamedPipeAccess;
    /// validate the Windows Windows Messages based client implementation
    // - it first launch the Server to handle Windows Messages
    // - it then runs 1000 remote SQL queries, and check the JSON data retrieved
    // - the time elapsed for this step is computed, and displayed on the report
    procedure LocalWindowMessages;
    /// validate the client implementation, using direct access to the server
    // - it connects directly the client to the server, therefore use the same
    // process and memory during the run: it's the fastest possible way of
    // communicating
    // - it then runs 1000 remote SQL queries, and check the JSON data retrieved
    // - the time elapsed for this step is computed, and displayed on the report
{$endif}
    procedure DirectInProcessAccess;
    /// validate HTTP/1.1 client-server with multiple TSQLRestServer instances
    procedure HTTPSeveralDBServers;
  end;

  /// this class defined two published methods of type TSQLRestServerCallBack in
  //  order to test the Server-Side ModelRoot/TableName/ID/MethodName RESTful model
  TSQLRestServerTest = class(TSQLRestServerDB)
  published
    /// test ModelRoot/People/ID/DataAsHex
    // - this method is called by TSQLRestServer.URI when a
    // ModelRoot/People/ID/DataAsHex GET request is provided
    // - Parameters values are not used here: this service only need aRecord.ID
    // - SentData is set with incoming data from a PUT method
    // - if called from ModelRoot/People/ID/DataAsHex with GET or PUT methods,
    // TSQLRestServer.URI will create a TSQLRecord instance and set its ID
    // (but won't retrieve its other field values automaticaly)
    // - if called from ModelRoot/People/DataAsHex with GET or PUT methods,
    // TSQLRestServer.URI will leave aRecord.ID=0 before launching it
    // - if called from ModelRoot/DataAsHex with GET or PUT methods,
    // TSQLRestServer.URI will leave aRecord=nil before launching it
    // - implementation must return the HTTP error code (e.g. 200 as success)
    // - Table is overloaded as TSQLRecordPeople here, and still match the
    // TSQLRestServerCallBack prototype: but you have to check the class
    // at runtime: it can be called by another similar but invalid URL, like
    // ModelRoot/OtherTableName/ID/DataAsHex
    procedure DataAsHex(Ctxt: TSQLRestServerURIContext);
    /// method used to test the Server-Side ModelRoot/Sum or
    // ModelRoot/People/Sum Requests with JSON process
    // - implementation of this method returns the sum of two floating-points,
    // named A and B, as in the public TSQLRecordPeople.Sum() method,
    // which implements the Client-Side of this service
    // - Table nor ID are never used here
    procedure Sum(Ctxt: TSQLRestServerURIContext);
    /// method used to test the Server-Side ModelRoot/Sum or
    // ModelRoot/People/Sum Requests with variant process
    procedure Sum2(Ctxt: TSQLRestServerURIContext);
  end;

  /// a test case which will test most external DB functions of the mORMotDB unit
  // - the external DB will be in fact a SynDBSQLite3 instance, expecting a
  // test.db3 SQlite3 file available in the current directory, populated with
  // some TSQLRecordPeople rows
  // - note that SQL statement caching at SQLite3 engine level makes those test
  // 2 times faster: nice proof of performance improvement
  TTestExternalDatabase = class(TSynTestCase)
  protected
    fExternalModel: TSQLModel;
    fPeopleData: TSQLTable;
    /// called by ExternalViaREST/ExternalViaVirtualTable and
    // ExternalViaRESTWithChangeTracking tests method
    procedure Test(StaticVirtualTableDirect, TrackChanges: boolean);
  public
    /// release used instances (e.g. server) and memory
    procedure CleanUp; override;
  published
    {$ifndef LVCL}
    /// test TQuery emulation class
    procedure _TQuery;
    {$endif}
    /// test SynDB connection remote access via HTTP
    procedure _SynDBRemote;
    /// test TSQLDBConnectionProperties persistent as JSON
    procedure DBPropertiesPersistence;
    /// initialize needed RESTful client (and server) instances
    // - i.e. a RESTful direct access to an external DB
    procedure ExternalRecords;
    /// check the SQL auto-adaptation features
    procedure AutoAdaptSQL;
    /// check the per-db encryption
    // - the testpass.db3-wal file is not encrypted, but the main
    // testpass.db3 file will
    procedure CryptedDatabase;
    /// test external DB implementation via faster REST calls
    // - will mostly call directly the TSQLRestStorageExternal instance,
    // bypassing the Virtual Table mechanism of SQLite3
    procedure ExternalViaREST;
    /// test external DB implementation via slower Virtual Table calls
    // - using the Virtual Table mechanism of SQLite3 is more than 2 times
    // slower than direct REST access
    procedure ExternalViaVirtualTable;
    /// test external DB implementation via faster REST calls and change tracking
    // - a TSQLRecordHistory table will be used to store record history
    procedure ExternalViaRESTWithChangeTracking;
    {$ifndef CPU64}
    {$ifndef LVCL}
    {$ifdef MSWINDOWS}
    /// test external DB using the JET engine
    procedure JETDatabase;
    {$endif MSWINDOWS}
    {$endif LVCL}
    {$endif CPU64}
    {$ifdef MSWINDOWS}
    {$ifdef USEZEOS}
    /// test external Firebird embedded engine via Zeos/ZDBC (if available)
    procedure FirebirdEmbeddedViaZDBCOverHTTP;
    {$endif USEZEOS}
    {$endif MSWINDOWS}
  end;

  /// a test case for multi-threading abilities of the framework
  // - will test all direct or remote access protocols with a growing number
  // of concurrent clients (1,2,5,10,30,50 concurent threads), to ensure
  // stability, scalibility and safety of the framework
  TTestMultiThreadProcess = class(TSynTestCase)
  protected
    fModel: TSQLModel;
    fDatabase: TSQLRestServerDB;
    fTestClass: TSQLRestClass;
    fThreads: TSynObjectList;
    fRunningThreadCount: integer;
    fHttpServer: TSQLHttpServer;
    fMinThreads: integer;
    fMaxThreads: integer;
    fOperationCount: integer;
    fClientPerThread: integer;
    fClientOnlyServerIP: RawByteString;
    fTimer: TPrecisionTimer;
    procedure DatabaseClose;
    procedure Test(aClass: TSQLRestClass; aHttp: TSQLHttpServerOptions=HTTP_DEFAULT_MODE;
      aWriteMode: TSQLRestServerAcquireMode=amLocked);
    function CreateClient: TSQLRest;
  public
    /// create the test case instance
    constructor Create(Owner: TSynTests; const Ident: string = ''); override;
    /// release used instances (e.g. server) and memory
    procedure CleanUp; override;
    /// if not '', forces the test not to initiate any server and connnect to
    // the specified server IP address
    property ClientOnlyServerIP: RawByteString read fClientOnlyServerIP write fClientOnlyServerIP;
    /// the minimum number of threads used for this test
    // - is 1 by default
    property MinThreads: integer read fMinThreads write fMinThreads;
    /// the maximum number of threads used for this test
    // - is 50 by default
    property MaxThreads: integer read fMaxThreads write fMaxThreads;
    /// how many Add() + Retrieve() operations are performed during each test
    // - is 200 by default, i.e. 200 Add() plus 200 Retrieve() globally
    property OperationCount: integer read fOperationCount write fOperationCount;
    /// how many TSQLRest instance is initialized per thread
    // - is 1 by default
    property ClientPerThread: Integer read fClientPerThread write fClientPerThread;
  published
    /// initialize fDatabase and create MaxThreads threads for clients
    procedure CreateThreadPool;
    /// direct test of its RESTful methods
    procedure _TSQLRestServerDB;
    /// test via TSQLRestClientDB instances
    procedure _TSQLRestClientDB;
    {$ifdef MSWINDOWS}
    /// test via TSQLRestClientURINamedPipe instances
    procedure _TSQLRestClientURINamedPipe;
    /// test via TSQLRestClientURIMessage instances
    procedure _TSQLRestClientURIMessage;
    {$endif}
    {$ifndef ONLYUSEHTTPSOCKET}
    /// test via TSQLHttpClientWinHTTP instances over http.sys (HTTP API) server
    procedure WindowsAPI;
    {$endif}
    /// test via TSQLHttpClientWinSock instances over OS's socket API server
    // - this test won't work within the Delphi IDE debugger
    procedure SocketAPI;
    //// test via TSQLHttpClientWebsockets instances
    procedure Websockets;
    {$ifdef USELIBCURL}
    /// test via TSQLHttpClientCurl using libcurl library
    procedure _libcurl;
    {$endif}
    /// test via TSQLRestClientDB instances with AcquireWriteMode=amLocked
    procedure Locked;
    /// test via TSQLRestClientDB instances with AcquireWriteMode=amUnlocked
    procedure Unlocked;
    {$ifndef LVCL}
    /// test via TSQLRestClientDB instances with AcquireWriteMode=amMainThread
    procedure MainThread;
    {$endif}
    /// test via TSQLRestClientDB instances with AcquireWriteMode=amBackgroundThread
    procedure BackgroundThread;
  end;

  /// SOA callback definition as expected by TTestBidirectionalRemoteConnection
  IBidirCallback = interface(IInvokable)
    ['{5C5818CC-FFBA-445C-82C1-39F45B84520C}']
    procedure AsynchEvent(a: integer);
    function Value: Integer;
  end;

  /// SOA service definition as expected by TTestBidirectionalRemoteConnection
  IBidirService = interface(IInvokable)
    ['{0984A2DA-FD1F-49D6-ACFE-4D45CF08CA1B}']
    function TestRest(a,b: integer; out c: RawUTF8): variant;
    function TestRestCustom(a: integer): TServiceCustomAnswer;
    function TestCallback(d: Integer; const callback: IBidirCallback): boolean;
    procedure LaunchAsynchCallback(a: integer);
    procedure RemoveCallback;
  end;

  TBidirServer = class(TInterfacedObject,IBidirService)
  protected
    fCallback: IBidirCallback;
    // IBidirService implementation methods
    function TestRest(a,b: integer; out c: RawUTF8): variant;
    function TestRestCustom(a: integer): TServiceCustomAnswer;
    function TestCallback(d: Integer; const callback: IBidirCallback): boolean;
    procedure LaunchAsynchCallback(a: integer);
    procedure RemoveCallback;
  public
    function LaunchSynchCallback: integer;
  end;

  /// a test case for all bidirectional remote access, e.g. WebSockets
  TTestBidirectionalRemoteConnection = class(TSynTestCase)
  protected
    fHttpServer: TSQLHttpServer;
    fServer: TSQLRestServerFullMemory;
    fBidirServer: TBidirServer;
    fPublicRelayClientsPort, fPublicRelayPort: SockString;
    fPublicRelay: TPublicRelay;
    fPrivateRelay: TPrivateRelay;
    procedure CleanUp; override;
    function NewClient(const port: SockString): TSQLHttpClientWebsockets;
    procedure WebsocketsLowLevel(protocol: TWebSocketProtocol; opcode: TWebSocketFrameOpCode);
    procedure TestRest(Rest: TSQLRest);
    procedure TestCallback(Rest: TSQLRest);
    procedure SOACallbackViaWebsockets(Ajax, Relay: boolean);
  published
    /// low-level test of our 'synopsejson' WebSockets JSON protocol
    procedure WebsocketsJSONProtocol;
    /// low-level test of our 'synopsebinary' WebSockets binary protocol
    procedure WebsocketsBinaryProtocol;
    procedure WebsocketsBinaryProtocolEncrypted;
    procedure WebsocketsBinaryProtocolCompressed;
    procedure WebsocketsBinaryProtocolCompressEncrypted;
    /// launch the WebSockets-ready HTTP server
    procedure RunHttpServer;
    /// test the callback mechanism via interface-based services on server side
    procedure SOACallbackOnServerSide;
    /// test callbacks via interface-based services over JSON WebSockets
    procedure SOACallbackViaJSONWebsockets;
    /// test callbacks via interface-based services over binary WebSockets
    procedure SOACallbackViaBinaryWebsockets;
    /// initialize SynProtoRelay tunnelling
    procedure RelayStart;
    /// test SynProtoRelay tunnelling over JSON WebSockets
    procedure RelaySOACallbackViaJSONWebsockets;
    /// verify ability to reconect from Private Relay to Public Relay
    procedure RelayConnectionRecreate;
    /// test SynProtoRelay tunnelling over binary WebSockets
    procedure RelaySOACallbackViaBinaryWebsockets;
    /// finalize SynProtoRelay tunnelling
    procedure RelayShutdown;
    /// test Master/Slave replication using TRecordVersion field over WebSockets
    procedure _TRecordVersion;
  end;

type
  // This is our simple Test data class. Will be mapped to TSQLRecordDDDTest.
  TDDDTest = class(TSynPersistent)
  private
    fDescription: RawUTF8;
  published
    property Description: RawUTF8 read fDescription write fDescription;
  end;

  TDDDTestObjArray = array of TDDDTest;

  // The corresponding TSQLRecord for TDDDTest.
  TSQLRecordDDDTest = class(TSQLRecord)
  private
    fDescription: RawUTF8;
  published
    property Description: RawUTF8 read fDescription write fDescription;
  end;

  // CQRS Query Interface fo TTest
  IDDDThreadsQuery = interface(ICQRSService)
    ['{DD402806-39C2-4921-98AA-A575DD1117D6}']
    function SelectByDescription(const aDescription: RawUTF8): TCQRSResult;
    function SelectAll: TCQRSResult;
    function Get(out aAggregate: TDDDTest): TCQRSResult;
    function GetAll(out aAggregates: TDDDTestObjArray): TCQRSResult;
    function GetNext(out aAggregate: TDDDTest): TCQRSResult;
    function GetCount: integer;
  end;

  // CQRS Command Interface for TTest
  IDDDThreadsCommand = interface(IDDDThreadsQuery)
    ['{F0E4C64C-B43A-491B-85E9-FD136843BFCB}']
    function Add(const aAggregate: TDDDTest): TCQRSResult;
    function Update(const aUpdatedAggregate: TDDDTest): TCQRSResult;
    function Delete: TCQRSResult;
    function DeleteAll: TCQRSResult;
    function Commit: TCQRSResult;
    function Rollback: TCQRSResult;
  end;

  /// a test case for all shared DDD types and services
  TTestDDDSharedUnits = class(TSynTestCase)
  protected
  published
    /// test the User modelization types, including e.g. Address
    procedure UserModel;
    /// test the Authentication modelization types, and implementation
    procedure AuthenticationModel;
    /// test the Email validation process
    procedure EmailValidationProcess;
    /// test the CQRS Repository for TUser persistence
    procedure UserCQRSRepository;
  end;

  /// a test case for aggressive multi-threaded DDD ORM test
  TTestDDDMultiThread = class(TSynTestCase)
  private
    // Rest server
    fRestServer: TSQLRestServerDB;
    // Http server
    fHttpServer: TSQLHttpServer;
    /// will create as many Clients as specified by aClient.
    // - each client will perform as many Requests as specified by aRequests.
    // - this function will wait for all Clients until finished.
    function ClientTest(const aClients, aRequests: integer): boolean;
  protected
    /// cleaning up the test
    procedure CleanUp; override;
  published
    /// delete any old Test database on start
    procedure DeleteOldDatabase;
    /// start the whole DDD Server (http and rest)
    procedure StartServer;
    /// test straight-forward access using 1 thread and 1 client
    procedure SingleClientTest;
    /// test concurrent access with multiple clients
    procedure MultiThreadedClientsTest;
  end;


  /// a test class, used by TTestServiceOrientedArchitecture
  // - to test TPersistent objects used as parameters for remote service calls
  TComplexNumber = class(TPersistent)
  private
    fReal: Double;
    fImaginary: Double;
  public
    /// create an instance to store a complex number
    constructor Create(aReal, aImaginary: double); reintroduce;
  published
    /// the real part of this complex number
    property Real: Double read fReal write fReal;
    /// the imaginary part of this complex number
    property Imaginary: Double read fImaginary write fImaginary;
  end;

  /// a record used by IComplexCalculator.EchoRecord
  TConsultaNav = packed record
    MaxRows, Row0, RowCount: int64;
    IsSQLUpdateBack, EOF: boolean;
  end;

  /// a record used by IComplexCalculator.GetCustomer
  TCustomerData = packed record
    Id: Integer;
    AccountNum: RawUTF8;
    Name: RawUTF8;
    Address: RawUTF8;
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test basic and high-level remote service calls
  ICalculator = interface(IInvokable)
    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
    /// add two signed 32 bit integers
    function Add(n1,n2: integer): integer;
    /// multiply two signed 64 bit integers
    function Multiply(n1,n2: Int64): Int64;
    /// substract two floating-point values
    function Subtract(n1,n2: double): double;
    /// convert a currency value into text
    procedure ToText(Value: Currency; var Result: RawUTF8);
    /// convert a floating-point value into text
    function ToTextFunc(Value: double): string;
    /// swap two by-reference floating-point values
    // - would validate pointer use instead of XMM1/XMM2 registers under Win64
    procedure Swap(var n1,n2: double);
    // test unaligned stack access
    function StackIntMultiply(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10: integer): Int64;
    // test float stack access
    function StackFloatMultiply(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10: double): Int64;
    /// do some work with strings, sets and enumerates parameters,
    // testing also var (in/out) parameters and set as a function result
    function SpecialCall(Txt: RawUTF8; var Int: integer; var Card: cardinal; field: TSynTableFieldTypes;
      fields: TSynTableFieldTypes; var options: TSynTableFieldOptions): TSynTableFieldTypes;
    /// test integer, strings and wide strings dynamic arrays, together with records
    function ComplexCall(const Ints: TIntegerDynArray; const Strs1: TRawUTF8DynArray;
      var Str2: TWideStringDynArray; const Rec1: TVirtualTableModuleProperties;
      var Rec2: TSQLRestCacheEntryValue; Float1: double; var Float2: double): TSQLRestCacheEntryValue;
    /// validates ArgsInputIsOctetStream raw binary upload
    function DirectCall(const Data: TSQLRawBlob): integer;
    /// validates huge RawJSON/RawUTF8
    function RepeatJsonArray(const item: RawUTF8; count: integer): RawJSON;
    function RepeatTextArray(const item: RawUTF8; count: integer): RawUTF8;
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test remote service calls with objects as parameters (its published
  // properties will be serialized as standard JSON objects)
  // - since it inherits from ICalculator interface, it will also test
  // the proper interface inheritance handling (i.e. it will test that
  // ICalculator methods are also available)
  IComplexCalculator = interface(ICalculator)
    ['{8D0F3839-056B-4488-A616-986CF8D4DEB7}']
    /// purpose of this method is to substract two complex numbers
    // - using class instances as parameters
    procedure Substract(n1,n2: TComplexNumber; out Result: TComplexNumber);
    /// purpose of this method is to check for boolean handling
    function IsNull(n: TComplexNumber): boolean;
    /// this will test the BLOB kind of remote answer
    function TestBlob(n: TComplexNumber): TServiceCustomAnswer;
    {$ifndef NOVARIANTS}
    /// test variant kind of parameters
    function TestVariants(const Text: RawUTF8; V1: Variant; var V2: variant): variant;
    {$endif}
    {$ifndef LVCL}
    /// test in/out collections
    procedure Collections(Item: TCollTest; var List: TCollTestsI; out Copy: TCollTestsI);
    {$endif}
    /// returns the thread ID running the method on server side
    function GetCurrentThreadID: PtrUInt;
    /// validate record transmission
    function GetCustomer(CustomerId: Integer; out CustomerData: TCustomerData): Boolean;
    //// validate TSQLRecord transmission
    procedure FillPeople(var People: TSQLRecordPeople);
    {$ifdef UNICODE}
    /// validate simple record transmission
    // - older Delphi versions (e.g. 6-7) do not allow records without
    // nested reference-counted types
    function EchoRecord(const Nav: TConsultaNav): TConsultaNav;
    {$endif}
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test sicClientDriven implementation pattern: data will remain on
  // the server until the IComplexNumber instance is out of scope
  IComplexNumber = interface(IInvokable)
    ['{29D753B2-E7EF-41B3-B7C3-827FEB082DC1}']
    procedure Assign(aReal, aImaginary: double);
    function GetImaginary: double;
    function GetReal: double;
    procedure SetImaginary(const Value: double);
    procedure SetReal(const Value: double);
    procedure Add(aReal, aImaginary: double);
    property Real: double read GetReal write SetReal;
    property Imaginary: double read GetImaginary write SetImaginary;
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test sicPerUser implementation pattern
  ITestUser = interface(IInvokable)
    ['{EABB42BF-FD08-444A-BF9C-6B73FA4C4788}']
    function GetContextSessionID: integer;
    function GetContextSessionUser: integer;
    function GetContextSessionGroup: integer;
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test sicPerGroup implementation pattern
  ITestGroup = interface(ITestUser)
    ['{DCBA5A38-62CC-4A52-8639-E709B31DDCE1}']
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test sicPerSession implementation pattern
  ITestSession = interface(ITestUser)
    ['{5237A687-C0B2-46BA-9F39-BEEA7C3AA6A9}']
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test threading implementation pattern
  ITestPerThread = interface(IInvokable)
    ['{202B6C9F-FCCB-488D-A425-5472554FD9B1}']
    function GetContextServiceInstanceID: PtrUInt;
    function GetThreadIDAtCreation: PtrUInt;
    function GetCurrentThreadID: PtrUInt;
    function GetCurrentRunningThreadID: PtrUInt;
  end;

  /// a test value object, used by IUserRepository/ISmsSender interfaces
  // - to test stubing/mocking implementation pattern
  TUser = record
    Name: RawUTF8;
    Password: RawUTF8;
    MobilePhoneNumber: RawUTF8;
    ID: Integer;
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test stubing/mocking implementation pattern
  IUserRepository = interface(IInvokable)
    ['{B21E5B21-28F4-4874-8446-BD0B06DAA07F}']
    function GetUserByName(const Name: RawUTF8): TUser;
    procedure Save(const User: TUser);
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test stubing/mocking implementation pattern
  ISmsSender = interface(IInvokable)
    ['{8F87CB56-5E2F-437E-B2E6-B3020835DC61}']
    function Send(const Text, Number: RawUTF8): boolean;
  end;


const
  IID_ICalculator: TGUID = '{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}';

type
  TTestServiceInstances = record
    I: ICalculator;
    CC: IComplexCalculator;
    CN: IComplexNumber;
    CU: ITestUser;
    CG: ITestGroup;
    CS: ITestSession;
    CT: ITestPerThread;
    ExpectedSessionID: integer;
    ExpectedUserID: integer;
    ExpectedGroupID: integer;
  end;

  /// a test case which will test the interface-based SOA implementation of
  // the mORMot framework
  TTestServiceOrientedArchitecture = class(TSynTestCase)
  protected
    fModel: TSQLModel;
    fClient: TSQLRestClientDB;
    procedure Test(const Inst: TTestServiceInstances; Iterations: Cardinal=700);
    procedure ClientTest(aRouting: TSQLRestServerURIContextClass;
      aAsJSONObject: boolean; {$ifndef LVCL}aRunInOtherThread: boolean=false;{$endif}
      aOptions: TServiceMethodOptions=[]);
    procedure ClientAlgo(algo: TSQLRestServerAuthenticationSignedURIAlgo);
    class function CustomReader(P: PUTF8Char; var aValue; out aValid: Boolean{$ifndef NOVARIANTS};
      CustomVariantOptions: PDocVariantOptions{$endif}): PUTF8Char;
    class procedure CustomWriter(const aWriter: TTextWriter; const aValue);
    procedure SetOptions(aAsJSONObject: boolean;
      aOptions: TServiceMethodOptions);
    procedure IntSubtractJSON(Ctxt: TOnInterfaceStubExecuteParamsJSON);
    {$ifndef NOVARIANTS}
    procedure IntSubtractVariant(Ctxt: TOnInterfaceStubExecuteParamsVariant);
    procedure IntSubtractVariantVoid(Ctxt: TOnInterfaceStubExecuteParamsVariant);
    {$endif}
    /// release used instances (e.g. http server) and memory
    procedure CleanUp; override;
  public
  published
    /// test the SetWeak/SetWeakZero weak interface functions
    procedure WeakInterfaces;
    /// initialize the SOA implementation
    procedure ServiceInitialization;
    /// test direct call to the class instance
    procedure DirectCall;
    /// test the server-side implementation
    procedure ServerSide;
    /// test the client-side implementation in RESTful mode
    procedure ClientSideREST;
    /// test the client-side in RESTful mode with values transmitted as JSON objects
    procedure ClientSideRESTAsJSONObject;
    /// test the client-side in RESTful mode with full session statistics
    procedure ClientSideRESTSessionsStats;
    /// test the client-side implementation of optExecLockedPerInterface
    procedure ClientSideRESTLocked;
    {$ifndef LVCL}
    /// test the client-side implementation of opt*InMainThread option
    procedure ClientSideRESTMainThread;
    /// test the client-side implementation of opt*InPerInterfaceThread option
    procedure ClientSideRESTBackgroundThread;
    {$endif}
    /// test the client-side implementation with crc32c URI signature
    procedure ClientSideRESTSignWithCrc32c;
    /// test the client-side implementation with xxHash32 URI signature
    procedure ClientSideRESTSignWithXxhash;
    /// test the client-side implementation with MD5 URI signature
    procedure ClientSideRESTSignWithMd5;
    /// test the client-side implementation with SHA256 URI signature
    procedure ClientSideRESTSignWithSha256;
    /// test the client-side implementation with SHA512 URI signature
    procedure ClientSideRESTSignWithSha512;
    /// test the client-side implementation using TSQLRestServerAuthenticationNone
    procedure ClientSideRESTWeakAuthentication;
    /// test the client-side implementation using TSQLRestServerAuthenticationHttpBasic
    procedure ClientSideRESTBasicAuthentication;
    /// test the custom record JSON serialization
    procedure ClientSideRESTCustomRecordLayout;
    /// test the client-side in RESTful mode with all calls logged in a table
    procedure ClientSideRESTServiceLogToDB;
    /// test the client-side implementation in JSON-RPC mode
    procedure ClientSideJSONRPC;
    /// test REStful mode using HTTP client/server communication
    procedure TestOverHTTP;
    /// test the security features
    procedure Security;
    /// test interface stubbing / mocking
    procedure MocksAndStubs;
  end;

{$endif DELPHI5OROLDER}


implementation

uses
{$ifndef DELPHI5OROLDER}
  TestSQL3FPCInterfaces,
{$endif}
{$ifndef LVCL}
  SyncObjs,
{$endif}
{$ifdef MSWINDOWS}
  PasZip,
{$ifndef FPC}
{$ifdef ISDELPHIXE2}
  VCL.Graphics,
{$else}
  Graphics,
{$endif}
{$endif FPC}
{$endif MSWINDOWS}
  SynCrypto,
  SynZip,
  SynLZO,
  SynLZ,
  SynLizard;


{ TTestLowLevelCommon }

procedure TTestLowLevelCommon._CamelCase;
var v: RawUTF8;
begin
  v := UnCamelCase('On'); Check(v='On');
  v := UnCamelCase('ON'); Check(v='ON');
  v := UnCamelCase('OnLine'); Check(v='On line');
  v := UnCamelCase('OnLINE'); Check(v='On LINE');
  v := UnCamelCase('OnMyLINE'); Check(v='On my LINE');
  v := UnCamelCase('On_MyLINE'); Check(v='On - My LINE');
  v := UnCamelCase('On__MyLINE'); Check(v='On: My LINE');
  v := UnCamelCase('Email1'); Check(v='Email 1');
  v := UnCamelCase('Email12'); Check(v='Email 12');
  v := UnCamelCase('KLMFlightNumber'); Check(v='KLM flight number');
  v := UnCamelCase('GoodBBCProgram'); Check(v='Good BBC program');
end;

function GetBitsCount64(const Bits; Count: PtrInt): PtrInt;
begin // reference implementation
  result := 0;
  while Count>0 do begin
    dec(Count);
    if Count in TBits64(Bits) then // bt dword[rdi],edx is slow in such a loop
      inc(result);                 // ... but correct :)
  end;
end;

function GetBitsCountPurePascal(value: PtrInt): PtrInt;
begin
  result := value;
  {$ifdef CPU64}
  result := result-((result shr 1) and $5555555555555555);
  result := (result and $3333333333333333)+((result shr 2) and $3333333333333333);
  result := (result+(result shr 4)) and $0f0f0f0f0f0f0f0f;
  inc(result,result shr 8); // avoid slow multiplication
  inc(result,result shr 16);
  inc(result,result shr 32);
  result := result and $7f;
  {$else}
  result := result-((result shr 1) and $55555555);
  result := (result and $33333333)+((result shr 2) and $33333333);
  result := (result+(result shr 4)) and $0f0f0f0f;
  inc(result,result shr 8);
  inc(result,result shr 16);
  result := result and $3f;
  {$endif CPU64}
end;

procedure TTestLowLevelCommon.Bits;
const N = 1000000;
  procedure TestPopCnt(const ctxt: string);
  var timer: TPrecisionTimer;
      i,c: integer;
      v: QWord;
  begin
    CheckEqual(GetBitsCountPtrInt(0),0);
    CheckEqual(GetBitsCountPtrInt($f),4);
    CheckEqual(GetBitsCountPtrInt($ff),8);
    CheckEqual(GetBitsCountPtrInt($fff),12);
    CheckEqual(GetBitsCountPtrInt($ffff),16);
    CheckEqual(GetBitsCountPtrInt(-1),POINTERBITS);
    v := PtrUInt(-1);
    CheckEqual(GetBitsCount(v,0),0);
    CheckEqual(GetBitsCount64(v,0),0);
    for i := 0 to POINTERBITS-1 do begin
      CheckEqual(GetBitsCountPtrInt(PtrInt(1) shl i),1);
      if i<POINTERBITS-1 then begin
        CheckEqual(GetBitsCountPtrInt(PtrInt(3) shl i),2);
        CheckEqual(GetBitsCountPtrInt((PtrInt(1) shl (i+1))-1),i+1);
      end;
      if i<POINTERBITS-2 then
        CheckEqual(GetBitsCountPtrInt(PtrInt(7) shl i),3);
      if i<POINTERBITS-3 then
        CheckEqual(GetBitsCountPtrInt(PtrInt(15) shl i),4);
      CheckEqual(GetBitsCount64(v,i+1),i+1);
      CheckEqual(GetBitsCount(v,i+1),i+1);
    end;
    for i := 1 to 32 do begin
      v := ALLBITS_CARDINAL[i];
      CheckEqual(GetBitsCountPtrInt(v),i);
      CheckEqual(GetBitsCount(v,POINTERBITS),i);
      CheckEqual(GetBitsCount(v,i),i);
    end;
    for i := 1 to 1000 do begin
      v := i;
      c := GetBitsCount64(v,POINTERBITS);
      CheckEqual(GetBitsCountPtrInt(v),c);
      CheckEqual(GetBitsCount(v,POINTERBITS),c);
      {$ifdef FPC}CheckEqual(popcnt(v),c);{$endif}
      v := v*v*19;
      c := GetBitsCount64(v,POINTERBITS);
      CheckEqual(GetBitsCountPtrInt(v),c);
      {$ifdef FPC}CheckEqual(popcnt(v),c);{$endif}
      v := random32gsl{$ifdef CPU64}or (PtrUInt(random32gsl) shl 32){$endif};
      c := GetBitsCount64(v,POINTERBITS);
      CheckEqual(GetBitsCountPtrInt(v),c);
      CheckEqual(GetBitsCount(v,POINTERBITS),c);
      {$ifdef FPC}CheckEqual(popcnt(v),c);{$endif}
    end;
    timer.Start;
    for i := 1 to N do
      GetBitsCountPtrInt(i);
    NotifyTestSpeed(ctxt,N,N shl POINTERSHR,@timer,{onlylog=}true);
  end;
var Bits: array[byte] of byte;
    Bits64: Int64 absolute Bits;
    Si,i: integer;
    c: cardinal;
    {$ifdef FPC}
    u: {$ifdef CPU64}QWord{$else}DWord{$endif};
    timer: TPrecisionTimer;
    {$endif FPC}
begin
  {$ifdef CPUINTEL}
  GetBitsCountPtrInt := @GetBitsCountPurePascal;
  TestPopCnt('pas');
  GetBitsCountPtrInt := @GetBitsCountPas; // x86/x86_64 assembly
  TestPopCnt('asm');
  {$ifndef ABSOLUTEPASCAL}
  if cfPOPCNT in CpuFeatures then begin
    GetBitsCountPtrInt := @GetBitsCountSSE42;
    TestPopCnt('sse4.2');
  end;
  {$endif ABSOLUTEPASCAL}
  {$else}
  TestPopCnt('pas');
  {$endif CPUINTEL}
  {$ifdef FPC}
  timer.Start;
  for u := 1 to N do
    i := popcnt(u);
  NotifyTestSpeed('FPC',N,N shl POINTERSHR,@timer,{onlylog=}true);
  {$endif FPC}
  FillcharFast(Bits,sizeof(Bits),0);
  for i := 0 to high(Bits)*8+7 do begin
    Check(not GetBit(Bits,i));
    Check(not GetBitPtr(@Bits,i));
  end;
  RandSeed := 10; // will reproduce the same Random() values
  for i := 1 to 100 do begin
    Si := Random(high(Bits));
    SetBit(Bits,Si);
    Check(GetBit(Bits,Si));
    Check(GetBitPtr(@Bits,Si));
  end;
  RandSeed := 10;
  for i := 1 to 100 do
    Check(GetBit(Bits,Random(high(Bits))));
  RandSeed := 10;
  for i := 1 to 100 do begin
    Si := Random(high(Bits));
    UnSetBit(Bits,Si);
    Check(not GetBit(Bits,Si));
    Check(not GetBitPtr(@Bits,Si));
  end;
  for i := 0 to high(Bits)*8+7 do
    Check(not GetBit(Bits,i));
  for i := 0 to 63 do
    Check(not GetBit64(Bits64,i));
  RandSeed := 10;
  for i := 1 to 30 do begin
    Si := Random(63);
    SetBit64(Bits64,Si);
    Check(GetBit64(Bits64,Si));
  end;
  RandSeed := 10;
  for i := 1 to 30 do
    Check(GetBit64(Bits64,Random(63)));
  RandSeed := 10;
  for i := 1 to 30 do begin
    Si := Random(63);
    UnSetBit64(Bits64,Si);
    Check(not GetBit64(Bits64,Si));
  end;
  for i := 0 to 63 do
    Check(not GetBit64(Bits64,i));
  c := 1;
  for i := 1 to 32 do begin
    Check(GetAllBits($ffffffff,i));
    Check(not GetAllBits(0,i));
    Check(GetAllBits(c,i));
    Check(not GetAllBits(c and -2,i));
    Check(GetAllBits(ALLBITS_CARDINAL[i],i));
    c := c or (1 shl i);
  end;
  Randomize; // we fixed the RandSeed value above -> get true random now
end;

procedure TTestLowLevelCommon.Curr64;
var tmp: string[63];
    i, err: Integer;
    V1: currency;
    V2: TSynExtended;
    i64: Int64;
    v: RawUTF8;
begin
  Check(TruncTo2Digits(1)=1);
  Check(TruncTo2Digits(1.05)=1.05);
  Check(TruncTo2Digits(1.051)=1.05);
  Check(TruncTo2Digits(1.0599)=1.05);
  Check(TruncTo2Digits(-1)=-1);
  Check(TruncTo2Digits(-1.05)=-1.05);
  Check(TruncTo2Digits(-1.051)=-1.05);
  Check(TruncTo2Digits(-1.0599)=-1.05);
  Check(SimpleRoundTo2Digits(1)=1);
  Check(SimpleRoundTo2Digits(1.05)=1.05);
  Check(SimpleRoundTo2Digits(1.051)=1.05);
  Check(SimpleRoundTo2Digits(1.0549)=1.05);
  Check(SimpleRoundTo2Digits(1.0550)=1.05);
  Check(SimpleRoundTo2Digits(1.0551)=1.06);
  Check(SimpleRoundTo2Digits(1.0599)=1.06);
  Check(SimpleRoundTo2Digits(-1)=-1);
  Check(SimpleRoundTo2Digits(-1.05)=-1.05);
  Check(SimpleRoundTo2Digits(-1.051)=-1.05);
  Check(SimpleRoundTo2Digits(-1.0549)=-1.05);
  Check(SimpleRoundTo2Digits(-1.0550)=-1.05);
  Check(SimpleRoundTo2Digits(-1.0551)=-1.06);
  Check(SimpleRoundTo2Digits(-1.0599)=-1.06);
  Check(StrToCurr64('.5')=5000);
  Check(StrToCurr64('.05')=500);
  Check(StrToCurr64('.005')=50);
  Check(StrToCurr64('.0005')=5);
  Check(StrToCurr64('.00005')=0);
  Check(StrToCurr64('0.5')=5000);
  Check(StrToCurr64('0.05')=500);
  Check(StrToCurr64('0.005')=50);
  Check(StrToCurr64('0.0005')=5);
  Check(StrToCurr64('0.00005')=0);
  Check(StrToCurr64('1.5')=15000);
  Check(StrToCurr64('1.05')=10500);
  Check(StrToCurr64('1.005')=10050);
  Check(StrToCurr64('1.0005')=10005);
  Check(StrToCurr64('1.00005')=10000);
  Check(StrToCurr64(pointer(Curr64ToStr(1)))=1);
  Check(StrToCurr64(pointer(Curr64ToStr(12)))=12);
  Check(StrToCurr64(pointer(Curr64ToStr(123)))=123);
  Check(StrToCurr64(pointer(Curr64ToStr(1234)))=1234);
  Check(StrToCurr64(pointer(Curr64ToStr(12345)))=12345);
  Check(StrToCurr64(pointer(Curr64ToStr(123456)))=123456);
  Check(StrToCurr64(pointer(Curr64ToStr(12340000)))=12340000);
  Check(StrToCurr64(pointer(Curr64ToStr(12345000)))=12345000);
  Check(StrToCurr64(pointer(Curr64ToStr(12345600)))=12345600);
  Check(StrToCurr64(pointer(Curr64ToStr(12345670)))=12345670);
  Check(StrToCurr64(pointer(Curr64ToStr(12345678)))=12345678);
  tmp[0] := AnsiChar(Curr64ToPChar(1,@tmp[1])); Check(tmp='0.0001');
  tmp[0] := AnsiChar(Curr64ToPChar(12,@tmp[1])); Check(tmp='0.0012');
  tmp[0] := AnsiChar(Curr64ToPChar(123,@tmp[1])); Check(tmp='0.0123');
  tmp[0] := AnsiChar(Curr64ToPChar(1234,@tmp[1])); Check(tmp='0.1234');
  for i := 0 to 5000 do begin
    if i<500 then
      V1 := i*3 else
      V1 := Random*(Int64(MaxInt)*10);
    if Random(10)<4 then
      V1 := -V1;
    v := Curr64ToStr(PInt64(@V1)^);
    tmp[0] := AnsiChar(Curr64ToPChar(PInt64(@V1)^,@tmp[1]));
    Check(RawUTF8(tmp)=v);
    V2 := GetExtended(pointer(v),err);
    Check(err=0);
    CheckSame(V1,V2,1E-4);
    i64 := StrToCurr64(pointer(v));
    Check(PInt64(@V1)^=i64);
  end;
end;

procedure TTestLowLevelCommon.FastStringCompare;
begin
  Check(CompareText('','')=0);
  Check(CompareText('abcd','')>0);
  Check(CompareText('','abcd')<0);
  Check(StrCompFast(nil,nil)=0);
  Check(StrCompFast(PAnsiChar('abcD'),nil)=1);
  Check(StrCompFast(nil,PAnsiChar('ABcd'))=-1);
  Check(StrCompFast(PAnsiChar('ABCD'),PAnsiChar('ABCD'))=0);
  Check(StrCompFast(PAnsiChar('ABCD'),PAnsiChar('ABCE'))=-1);
  Check(StrCompFast(PAnsiChar('ABCD'),PAnsiChar('ABCC'))=1);
  Check(StrIComp(nil,nil)=0);
  Check(StrIComp(PAnsiChar('abcD'),nil)=1);
  Check(StrIComp(nil,PAnsiChar('ABcd'))=-1);
  Check(StrIComp(PAnsiChar('abcD'),PAnsiChar('ABcd'))=0);
  Check(StrIComp(PAnsiChar('abcD'),PAnsiChar('ABcF'))=
    StrComp(PAnsiChar('ABCD'),PAnsiChar('ABCF')));
  Check(StrComp(PAnsiChar('ABCD'),PAnsiChar('ABCE'))=-1);
  Check(StrComp(PAnsiChar('ABCD'),PAnsiChar('ABCC'))=1);
  Check(StrComp(nil,nil)=0);
  Check(StrComp(PAnsiChar('ABCD'),PAnsiChar('ABCD'))=0);
  Check(StrComp(PAnsiChar('ABCD'),PAnsiChar('ABCE'))=-1);
  Check(StrComp(PAnsiChar('ABCD'),PAnsiChar('ABCC'))=1);
  Check(SynCommons.AnsiIComp(pointer(PAnsiChar('abcD')),pointer(PAnsiChar('ABcd')))=0);
  Check(SynCommons.AnsiIComp(pointer(PAnsiChar('abcD')),pointer(PAnsiChar('ABcF')))=
    StrComp(PAnsiChar('ABCD'),PAnsiChar('ABCF')));
  Check(StrIComp(PAnsiChar('abcD'),PAnsiChar('ABcd'))=
    SynCommons.AnsiIComp(PAnsiChar('abcD'),PAnsiChar('ABcd')));
  Check(StrIComp(PAnsiChar('abcD'),PAnsiChar('ABcF'))=
    SynCommons.AnsiIComp(PAnsiChar('ABCD'),PAnsiChar('ABCF')));
  Check(strcspn(PAnsiChar('ab'),PAnsiChar('a'#0))=0);
  Check(strcspn(PAnsiChar('ab'),PAnsiChar('b'#0))=1);
  Check(strcspn(PAnsiChar('1234ab'),PAnsiChar('a'#0))=4);
  Check(strcspn(PAnsiChar('12345ab'),PAnsiChar('a'#0))=5);
  Check(strcspn(PAnsiChar('123456ab'),PAnsiChar('a'#0))=6);
  Check(strcspn(PAnsiChar('1234567ab'),PAnsiChar('a'#0))=7);
  Check(strcspn(PAnsiChar('12345678ab'),PAnsiChar('a'#0))=8);
  Check(strcspn(PAnsiChar('1234ab'),PAnsiChar('c'#0))=6);
  Check(strcspnpas(PAnsiChar('ab'),PAnsiChar('a'#0))=0);
  Check(strcspnpas(PAnsiChar('ab'),PAnsiChar('b'#0))=1);
  Check(strcspnpas(PAnsiChar('1234ab'),PAnsiChar('a'#0))=4);
  Check(strcspnpas(PAnsiChar('12345ab'),PAnsiChar('a'#0))=5);
  Check(strcspnpas(PAnsiChar('123456ab'),PAnsiChar('a'#0))=6);
  Check(strcspnpas(PAnsiChar('1234567ab'),PAnsiChar('a'#0))=7);
  Check(strcspnpas(PAnsiChar('12345678ab'),PAnsiChar('a'#0))=8);
  Check(strcspnpas(PAnsiChar('1234ab'),PAnsiChar('c'#0))=6);
  Check(strcspnpas(PAnsiChar('12345678901234567ab'),PAnsiChar('cccccccccccccccccccd'))=19);
  Assert(strspn(PAnsiChar('abcdef'),PAnsiChar('debca'))=5);
  Assert(strspn(PAnsiChar('baabbaabcd'),PAnsiChar('ab'))=8);
  Assert(strspnpas(PAnsiChar('abcdef'),PAnsiChar('g'#0))=0);
  Assert(strspnpas(PAnsiChar('abcdef'),PAnsiChar('a'#0))=1);
  Assert(strspnpas(PAnsiChar('bbcdef'),PAnsiChar('b'#0))=2);
  Assert(strspnpas(PAnsiChar('bbcdef'),PAnsiChar('bf'))=2);
  Assert(strspnpas(PAnsiChar('bcbdef'),PAnsiChar('cb'))=3);
  Assert(strspnpas(PAnsiChar('baabcd'),PAnsiChar('ab'))=4);
  Assert(strspnpas(PAnsiChar('abcdef'),PAnsiChar('debca'))=5);
  Assert(strspnpas(PAnsiChar('baabbaabcd'),PAnsiChar('ab'))=8);
  Assert(strspnpas(PAnsiChar('baabbaabbaabcd'),PAnsiChar('ab'))=12);
  Assert(strspnpas(PAnsiChar('baabbaabbaabbabcd'),PAnsiChar('ab'))=15);
  Assert(strspnpas(PAnsiChar('baabbaabbaabbaabcd'),PAnsiChar('ab'))=16);
  Assert(strspnpas(PAnsiChar('baabbaabbaababaabcd'),PAnsiChar('ab'))=17);
  {$ifndef ABSOLUTEPASCAL}
  {$ifdef CPUINTEL}
  if cfSSE42 in CpuFeatures then begin
    Check(strcspnsse42(PAnsiChar('ab'),PAnsiChar('a'#0))=0);
    Check(strcspnsse42(PAnsiChar('ab'),PAnsiChar('b'#0))=1);
    Check(strcspnsse42(PAnsiChar('1234ab'),PAnsiChar('a'#0))=4);
    Check(strcspnsse42(PAnsiChar('12345ab'),PAnsiChar('a'#0))=5);
    Check(strcspnsse42(PAnsiChar('123456ab'),PAnsiChar('a'#0))=6);
    Check(strcspnsse42(PAnsiChar('1234567ab'),PAnsiChar('a'#0))=7);
    Check(strcspnsse42(PAnsiChar('12345678ab'),PAnsiChar('a'#0))=8);
    Check(strcspnsse42(PAnsiChar('123456789ab'),PAnsiChar('a'#0))=9);
    Check(strcspnsse42(PAnsiChar('1234ab'),PAnsiChar('c'#0))=6);
    Check(strcspnsse42(PAnsiChar('123456789012345ab'),PAnsiChar('a'#0))=15);
    Check(strcspnsse42(PAnsiChar('1234567890123456ab'),PAnsiChar('a'#0))=16);
    Check(strcspnsse42(PAnsiChar('12345678901234567ab'),PAnsiChar('a'#0))=17);
    Check(strcspnsse42(PAnsiChar('12345678901234567ab'),PAnsiChar('cccccccccccccca'))=17);
    Check(strcspnsse42(PAnsiChar('12345678901234567ab'),PAnsiChar('ccccccccccccccca'))=17);
    Check(strcspnsse42(PAnsiChar('12345678901234567ab'),PAnsiChar('cccccccccccccccca'))=17);
    Check(strcspnsse42(PAnsiChar('12345678901234567ab'),PAnsiChar('ccccccccccccccccca'))=17);
    Check(strcspnsse42(PAnsiChar('12345678901234567ab'),PAnsiChar('ccccccccccccccccccca'))=17);
    Check(strcspnsse42(PAnsiChar('12345678901234567ab'),PAnsiChar('cccccccccccccccccccd'))=19);
    Check(strspnsse42(PAnsiChar('abcdef'),PAnsiChar('g'#0))=0);
    Check(strspnsse42(PAnsiChar('abcdef'),PAnsiChar('a'#0))=1);
    Check(strspnsse42(PAnsiChar('bbcdef'),PAnsiChar('b'#0))=2);
    Check(strspnsse42(PAnsiChar('bbcdef'),PAnsiChar('bf'))=2);
    Check(strspnsse42(PAnsiChar('bcbdef'),PAnsiChar('cb'))=3);
    Check(strspnsse42(PAnsiChar('baabcd'),PAnsiChar('ab'))=4);
    Check(strspnsse42(PAnsiChar('abcdef'),PAnsiChar('debca'))=5);
    Check(strspnsse42(PAnsiChar('baabbaabcd'),PAnsiChar('ab'))=8);
    Check(strspnsse42(PAnsiChar('baabbaabbaabcd'),PAnsiChar('ab'))=12);
    Check(strspnsse42(PAnsiChar('baabbaabbaabbabcd'),PAnsiChar('ab'))=15);
    Check(strspnsse42(PAnsiChar('baabbaabbaabbaabcd'),PAnsiChar('ab'))=16);
    Check(strspnsse42(PAnsiChar('baabbaabbaababaabcd'),PAnsiChar('ab'))=17);
  end;
  {$endif CPUINTEL}
  {$endif ABSOLUTEPASCAL}
end;

procedure TTestLowLevelCommon.IniFiles;
var Content,S,N,V: RawUTF8;
    Si,Ni,Vi,i,j: integer;
    P: PUTF8Char;
begin
  Content := '';
  Randomize;
  //RandSeed := 10;
  for i := 1 to 1000 do begin
    Si := Random(20);
    Ni := Random(50);
    Vi := Si*Ni+Ni;
    if Si=0 then
      S := '' else
      S := 'Section'+{$ifndef ENHANCEDRTL}Int32ToUtf8{$else}IntToStr{$endif}(Si);
    N := {$ifndef ENHANCEDRTL}Int32ToUtf8{$else}IntToStr{$endif}(Ni);
    V := {$ifndef ENHANCEDRTL}Int32ToUtf8{$else}IntToStr{$endif}(Vi);
    UpdateIniEntry(Content,S,N,V);
    for j := 1 to 5 do
      Check(FindIniEntry(Content,S,N)=V,'FindIniEntry');
    Check(FindIniEntry(Content,S,'no')='');
    Check(FindIniEntry(Content,'no',N)='');
  end;
  Check(FileFromString(Content,'test.ini'),'test.ini');
  Check(FileSynLZ('test.ini','test.ini.synlz',$ABA51051),'synLZ');
  if CheckFailed(FileUnSynLZ('test.ini.synlz','test2.ini',$ABA51051),'unSynLZ') then
    exit;
  S := StringFromFile('test2.ini');
  Check(S=Content,'test2.ini');
  Content := 'abc'#13#10'def'#10'ghijkl'#13'1234567890';
  P := pointer(Content);
  Check(GetNextLine(P,P)='abc');
  Check(GetNextLine(P,P)='def');
  Check(GetNextLine(P,P)='ghijkl');
  Check(GetNextLine(P,P)='1234567890');
  Check(P=nil);
  Check(FindNameValue(pointer(Content),'A')^ = 'b');
  Check(FindNameValue(pointer(Content),'AB')^ = 'c');
  Check(FindNameValue(pointer(Content),'D')^ = 'e');
  Check(FindNameValue(pointer(Content),'1')^ = '2');
  Check(FindNameValue(pointer(Content),'GHIJK')^ = 'l');
  Check(FindNameValue(pointer(Content),'B') = nil);
  Check(FindNameValue(pointer(Content),'L') = nil);
  Check(FindNameValue(pointer(Content),'2') = nil);
  Check(FindNameValue(pointer(Content),'TOTO') = nil);
  Check(FindNameValue(Content,'AB',S));
  Check(S='c');
  Check(FindNameValue(Content,'DEF',S));
  Check(S='');
  Check(FindNameValue(Content,'G',S));
  Check(S='hijkl');
  Check(FindNameValue(Content,'1234',S));
  Check(S='567890');
  Check(not FindNameValue(Content,'H',S));
  Check(S='');
end;

procedure TTestLowLevelCommon.Soundex;
var e: cardinal;
    PC: PAnsiChar;
    Soundex: TSynSoundEx;
    s: WinAnsiString;
begin
  Check(SoundExAnsi(PAnsiChar(' 120 '))=0);
  if SOUNDEX_BITS=8 then
    e := $2050206 else
    e := $2526;
  Check(SoundExAnsi(PAnsiChar('bonjour'))=e);
  Check(SoundExAnsi(PAnsiChar(' 123 bonjour.  m'),@PC)=e);
  Check((PC<>nil) and (PC^='.'));
  s := ' 123 bonjourtreslongmotquidepasse  m';
  s[15] := #232;
  s[28] := #233;
  Check(SoundExAnsi(pointer(s),@PC)<>0);
  Check((PC<>nil) and (PC^=' '));
  Check(SoundExAnsi(PAnsiChar('BOnjour'))=e);
  Check(SoundExAnsi(PAnsiChar('Bnjr'))=e);
  Check(SoundExAnsi(PAnsiChar('bonchour'))=e);
  Check(SoundExAnsi(PAnsiChar('mohammad'))=SoundExAnsi(PAnsiChar('mohhhammeeet')));
  if SOUNDEX_BITS=8 then
    e := $2050206 else
    e := $25262;
  Check(SoundExAnsi(PAnsiChar('bonjours'))=e);
  Check(SoundExAnsi(PAnsiChar('BOnjours'))=e);
  Check(SoundExAnsi(PAnsiChar('Bnjrs'))=e);
  Check(SoundExAnsi(PAnsiChar(' 120 '))=0);
  if SOUNDEX_BITS=8 then
    e := $2050206 else
    e := $2526;
  Check(SoundExUTF8('bonjour')=e);
  Check(SoundExUTF8(' 123 bonjour.  m',@PC)=e);
  Check((PC<>nil) and (PC^='m'));
  Check(SoundExUTF8(Pointer(WinAnsiToUTF8(s)),@PC)<>0);
  Check((PC<>nil) and (PC^='m'));
  Check(SoundExUTF8('BOnjour')=e);
  Check(SoundExUTF8('Bnjr')=e);
  Check(SoundExUTF8('bonchour')=e);
  Check(SoundExUTF8('mohammad')=SoundExUTF8('mohhhammeeet'));
  if SOUNDEX_BITS=8 then
    e := $2050206 else
    e := $25262;
  Check(SoundExUTF8('bonjours')=e);
  Check(SoundExUTF8('BOnjours')=e);
  Check(SoundExUTF8('Bnjrs')=e);
  Check(Soundex.Prepare(PAnsiChar('mohamad'),sndxEnglish));
  Check(Soundex.Ansi('moi rechercher mohammed ici'));
  Check(Soundex.UTF8('moi rechercher mohammed ici'));
  Check(Soundex.Ansi('moi mohammed'));
  Check(Soundex.UTF8('moi mohammed'));
  Check(not Soundex.Ansi('moi rechercher mouette ici'));
  Check(not Soundex.UTF8('moi rechercher mouette ici'));
  Check(not Soundex.Ansi('moi rechercher mouette'));
  Check(not Soundex.UTF8('moi rechercher mouette'));
end;

procedure TTestLowLevelCommon._TRawUTF8List;
const MAX=20000;
var i,n: integer;
    L: TRawUTF8List;
    C: TComponent;
    Rec: TSynFilterOrValidate;
    s: RawUTF8;
begin
  L := TRawUTF8List.Create([fObjectsOwned]);
  try // no hash table involved
    for i := 0 to MAX do begin
      C := TComponent.Create(nil);
      C.Tag := i;
      Check(L.AddObject(UInt32ToUtf8(i),C)=i);
    end;
    Check(L.Count=MAX+1);
    for i := 0 to MAX do
      Check(GetInteger(Pointer(L[i]))=i);
    for i := 0 to MAX do
      Check(TComponent(L.Objects[i]).Tag=i);
    Check(L.IndexOf('')<0);
    Check(L.IndexOf('5')=5);
    Check(L.IndexOf('999')=999);
    for i := MAX downto 0 do
      if i and 1=0 then
        L.Delete(i); // delete half the array
    Check(L.Count=MAX div 2);
    for i := 0 to L.Count-1 do
      Check(GetInteger(Pointer(L[i]))=TComponent(L.Objects[i]).Tag);
    Check(L.IndexOf('5')=2);
    Check(L.IndexOf('6')<0);
  finally
    L.Free;
  end;
  L := TRawUTF8List.Create([fObjectsOwned,fNoDuplicate,fCaseSensitive]);
  try // with hash table
    for i := 1 to MAX do begin
     Rec := TSynFilterOrValidate.create;
     Rec.Parameters := Int32ToUTF8(i);
     Check(L.AddObject(Rec.Parameters,Rec)=i-1);
     Check(L.IndexOf(Rec.Parameters)=i-1);
    end;
    Check(L.IndexOf('')<0);
    Check(L.IndexOf('abcd')<0);
    Check(L.Count=MAX);
    n := 0;
    for i := 1 to MAX do begin
      UInt32ToUTF8(i,s);
      Check(L.IndexOf(s)=n);
      Check(TSynFilterOrValidate(L.Objects[n]).Parameters=s);
      if i and 127=0 then
        Check(L.Delete(s)=n) else
        inc(n);
    end;
    Check(L.Count=n);
    for i := 1 to MAX do begin
      UInt32ToUTF8(i,s);
      Check((L.IndexOf(s)>=0)=(i and 127<>0));
    end;
    L.SaveToFile('utf8list.txt');
    L.Clear;
    Check(L.Count=0);
    L.LoadFromFile('utf8list.txt');
    Check(L.Count=n);
    for i := 1 to MAX do begin
      UInt32ToUTF8(i,s);
      Check((L.IndexOf(s)>=0)=(i and 127<>0));
    end;
    DeleteFile('utf8list.txt');
  finally
    L.Free;
  end;
end;

type
  TCity = record
    Name: string;
    Country: string;
    Latitude: double;
    Longitude: double;
  end;
  TCityDynArray = array of TCity;

  TAmount = packed record
    firmID: integer;
    amount: RawUTF8;
  end;
  TAmountCollection = array of TAmount;
  TAmountI = packed record
    firmID: integer;
    amount: integer;
  end;
  TAmountICollection = array of TAmountI;

procedure TTestLowLevelCommon._TDynArrayHashed;
var ACities: TDynArrayHashed;
    Cities: TCityDynArray;
    CitiesCount: integer;
    City: TCity;
    added: boolean;
    N: string;
    i,j: integer;
    A: TAmount;
    AI: TAmountI;
    AmountCollection: TAmountCollection;
    AmountICollection: TAmountICollection;
    AmountDA,AmountIDA1,AmountIDA2: TDynArrayHashed;
const CITIES_MAX=200000;
begin
  // default Init() will hash and compare binary content before string, i.e. firmID
  AmountDA.Init(TypeInfo(TAmountCollection), AmountCollection);
  Check(AmountDA.KnownType=djInteger);
  Check(@AmountDA.HashElement=@HashInteger);
  for i := 1 to 100 do begin
    A.firmID := i;
    A.amount := UInt32ToUTF8(i);
    Check(AmountDA.Add(A)=i-1);
  end;
  AmountDA.ReHash;
  for i := 1 to length(AmountCollection) do
    Check(AmountDA.FindHashed(i)=i-1);
  // default Init() will hash and compare the WHOLE binary content, i.e. 8 bytes
  AmountIDA1.Init(TypeInfo(TAmountICollection), AmountICollection);
  Check(AmountIDA1.KnownType=djInt64);
  Check(@AmountIDA1.HashElement=@HashInt64);
  for i := 1 to 100 do begin
    AI.firmID := i;
    AI.amount := i*2;
    Check(AmountIDA1.Add(AI)=i-1);
  end;
  AmountIDA1.ReHash;
  for i := 1 to length(AmountICollection) do begin
    AI.firmID := i;
    AI.amount := i*2;
    Check(AmountIDA1.FindHashed(AI)=i-1);
  end;
  AmountIDA1.Clear;
  // specific hash & compare of the firmID integer first field
  AmountIDA2.InitSpecific(TypeInfo(TAmountICollection), AmountICollection, djInteger);
  Check(AmountIDA2.KnownType=djInteger);
  Check(@AmountIDA2.HashElement=@HashInteger);
  for i := 1 to 100 do begin
    AI.firmID := i;
    AI.amount := i*2;
    Check(AmountIDA2.Add(AI)=i-1);
  end;
  AmountIDA2.ReHash;
  for i := 1 to length(AmountICollection) do
    Check(AmountIDA2.FindHashed(i)>=0);
  // valide generic-like features
  // see http://docwiki.embarcadero.com/CodeExamples/en/Generics_Collections_TDictionary_(Delphi)
  ACities.Init(TypeInfo(TCityDynArray),Cities,nil,nil,nil,@CitiesCount);
  City.Name := 'Iasi';
  City.Country := 'Romania';
  City.Latitude := 47.16;
  City.Longitude := 27.58;
  ACities.Add(City);
  City.Name := 'London';
  City.Country := 'United Kingdom';
  City.Latitude := 51.5;
  City.Longitude := -0.17;
  ACities.Add(City);
  City.Name := 'Buenos Aires';
  City.Country := 'Argentina';
  City.Latitude := 0;
  City.Longitude := 0;
  ACities.Add(City);
  Check(ACities.Count=3);
  ACities.ReHash; // will use default hash, and search by Name = 1st field
  City.Name := 'Iasi';
  Check(ACities.FindHashedAndFill(City)=0);
  Check(City.Name='Iasi');
  Check(City.Country='Romania');
  CheckSame(City.Latitude,47.16);
  CheckSame(City.Longitude,27.58);
  Check(ACities.FindHashedAndDelete(City)=0);
  Check(City.Name='Iasi');
  Check(ACities.Scan(City)<0);
  Check(ACities.FindHashed(City)<0);
  City.Name := 'Buenos Aires';
  City.Country := 'Argentina';
  City.Latitude := -34.6;
  City.Longitude := -58.45;
  Check(ACities.FindHashedAndUpdate(City,{addifnotexisting=}false)>=0);
  City.Latitude := 0;
  City.Longitude := 0;
  Check(City.Name='Buenos Aires');
  Check(ACities.FindHashedAndFill(City)>=0);
  CheckSame(City.Latitude,-34.6);
  CheckSame(City.Longitude,-58.45);
  Check(ACities.FindHashedForAdding(City,added)>=0);
  Check(not added);
  City.Name := 'Iasi';
  City.Country := 'Romania';
  City.Latitude := 47.16;
  City.Longitude := 27.58;
  i := ACities.FindHashedForAdding(City,added);
  Check(added);
  Check(i>0);
  if i>0 then begin
    Check(Cities[i].Name=''); // FindHashedForAdding left void content
    Cities[i] := City; // should fill Cities[i] content by hand
  end;
  Check(ACities.Count=3);
  Check(City.Name='Iasi');
  Check(ACities.FindHashed(City)>=0);
  // add CITIES_MAX items
  for i := 1 to 2000 do begin
    City.Name := IntToString(i);
    City.Latitude := i*3.14;
    City.Longitude := i*6.13;
    Check(ACities.FindHashedAndUpdate(City,true)=i+2,'multiple ReHash');
    Check(ACities.FindHashed(City)=i+2);
  end;
  ACities.Capacity := CITIES_MAX+30; // will trigger HASH_PO2
  for i := 2001 to CITIES_MAX do begin
    City.Name := IntToString(i);
    City.Latitude := i*3.14;
    City.Longitude := i*6.13;
    if i=8703 then
      City.Latitude := i*3.14;
    Check(ACities.FindHashedAndUpdate(City,true)=i+2);
    Check(ACities.FindHashed(City.Name)=i+2);
  end;
  for i := 1 to CITIES_MAX do begin
    N := IntToString(i);
    Check(ACities.FindHashed(N)=i+2);
  end;
  for i := 1 to CITIES_MAX do begin
    N := IntToString(i);
    j := ACities.FindHashed(N);
    Check(j>=0);
    if i and 127=0 then begin
      Check(ACities.FindHashedAndDelete(N)>=0,'delete');
      j := ACities.FindHashed(N);
      Check(j<0);
    end;
  end;
  for i := 1 to CITIES_MAX do begin
    N := IntToString(i);
    j := ACities.FindHashed(N);
    if i and 127=0 then
      Check(j<0,'deteled') else
      if not CheckFailed(j>=0,N) then begin
        Check(Cities[j].Name=N);
        CheckSame(Cities[j].Latitude,i*3.14);
        CheckSame(Cities[j].Longitude,i*6.13);
      end;
  end;
end;

type
  TRec = packed record A: integer; B: byte; C: double; D: Currency; end;
  TRecs = array of TRec;
  TProvince = record
    Name: RawUTF8;
    Comment: RawUTF8;
    Year: cardinal;
    Cities: TCityDynArray;
  end;
  TFV = packed record
    Major, Minor, Release, Build: integer;
    Main, Detailed: string;
    BuildDateTime: TDateTime;
    BuildYear: integer;
  end;
  TFVs = array of TFV;
  TFV2 = packed record
    V1: TFV;
    Value: integer;
    V2: TFV;
    Text: string;
  end;
  TFV2s = array of TFV2;
  TSynValidates = array of TSynValidate;
  TDataItem = record
    Modified: TDateTime;
    Data: string;
  end;
  TDataItems = array of TDataItem;

  TRawUTF8DynArray1 = type TRawUTF8DynArray;
  TRawUTF8DynArray2 = array of RawUTF8;

function FVSort(const A,B): integer;
begin
  result := SysUtils.StrComp(PChar(pointer(TFV(A).Detailed)),PChar(pointer(TFV(B).Detailed)));
end;

procedure TTestLowLevelCommon._TDynArray;
var AI, AI2: TIntegerDynArray;
    AU: TRawUTF8DynArray;
    AR: TRecs;
    AF: TFVs;
    AF2: TFV2s;
    h: cardinal;
    i,j,k,Len, count,AIcount: integer;
    U,U2: RawUTF8;
    P: PUTF8Char;
    PI: PIntegerArray;
    AB: TBooleanDynArray;
    R: TRec;
    F, F1: TFV;
    F2: TFV2;
    City: TCity;
    Province: TProvince;
    AV: TSynValidates;
    V: TSynValidate;
    AIP, AI2P, AUP, ARP, AFP, ACities, AVP, dyn1,dyn2: TDynArray;
    dyniter: TDynArrayLoadFrom;
    B: boolean;
    dp: TDataItem;
    dyn1Array,dyn2Array: TDataItems;
    Test, Test2: RawByteString;
    ST: TCustomMemoryStream;
    Index: TIntegerDynArray;
    W: TTextWriter;
    JSON_BASE64_MAGIC_UTF8: RawUTF8;
const MAGIC: array[0..1] of word = (34,$fff0);
procedure Fill(var F: TFV; i: integer);
begin
  F.Major := i;
  F.Minor := i+1;
  F.Release := i+2;
  F.Build := i+3;
  F.Main := IntToString(i+1000);
  F.Detailed := IntToString(2000-i);
  F.BuildDateTime := 36215.12;
  F.BuildYear := i+2011;
end;
procedure TestAF2;
var i: integer;
    F1,F2: TFV;
begin
  for i := 0 to AFP.Count-1 do begin
    Check(AF2[i].Value=i);
    Check(AF2[i].Text=IntToString(i));
    Fill(F1,i*2);
    Fill(F2,i*2+1);
    Check(RecordEquals(F1,AF2[i].V1,TypeInfo(TFV)));
    Check(RecordEquals(F2,AF2[i].V2,TypeInfo(TFV)));
  end;
end;
procedure Test64K;
var i, E, n: integer;
    D: TDynArray;
    IA: TIntegerDynArray;
begin
  D.Init(TypeInfo(TIntegerDynArray),IA,@n);
  D.Capacity := 16300;
  for i := 0 to 16256 do begin
    E := i*5;
    Check(D.Add(E)=i);
    Check(IA[i]=i*5);
  end;
  Check(D.Count=16257);
  Check(D.Capacity=16300);
  Check(length(IA)=D.Capacity);
  for i := 0 to 16256 do
    Check(IA[i]=i*5);
  Check(Hash32(D.SaveTo)=$36937D84);
end;
procedure TestCities;
var i: integer;
begin
  for i := 0 to ACities.Count-1 do
  with Province.Cities[i] do begin
    {$ifdef UNICODE}
    Check(StrToInt(Name)=i);
    {$else}
    Check(GetInteger(pointer(Name))=i);
    {$endif}
    CheckSame(Latitude,i*3.14);
    CheckSame(Longitude,i*6.13);
  end;
end;
begin
  h := TypeInfoToHash(TypeInfo(TAmount));
  Check(h=$9032161B,'TypeInfoToHash(TAmount)');
  h := TypeInfoToHash(TypeInfo(TAmountCollection));
  Check(h=$887ED692,'TypeInfoToHash(TAmountCollection)');
  h := TypeInfoToHash(TypeInfo(TAmountICollection));
  Check(h=$4051BAC,'TypeInfoToHash(TAmountICollection)');
  Check(not IsRawUTF8DynArray(nil),'IsRawUTF8DynArray0');
  Check(IsRawUTF8DynArray(TypeInfo(TRawUTF8DynArray)),'IsRawUTF8DynArray1');
  Check(IsRawUTF8DynArray(TypeInfo(TRawUTF8DynArray1)),'IsRawUTF8DynArray11');
  Check(IsRawUTF8DynArray(TypeInfo(TRawUTF8DynArray2)),'IsRawUTF8DynArray12');
  Check(not IsRawUTF8DynArray(TypeInfo(TAmount)),'IsRawUTF8DynArray2');
  Check(not IsRawUTF8DynArray(TypeInfo(TIntegerDynArray)),'IsRawUTF8DynArray2');
  Check(not IsRawUTF8DynArray(TypeInfo(TPointerDynArray)),'IsRawUTF8DynArray3');
  Check(not IsRawUTF8DynArray(TypeInfo(TAmountCollection)),'IsRawUTF8DynArray4');
  W := TTextWriter.CreateOwnedStream;
  // validate TBooleanDynArray
  dyn1.Init(TypeInfo(TBooleanDynArray),AB);
  SetLength(AB,4);
  for i := 0 to 3 do
    AB[i] := i and 1=1;
  test := dyn1.SaveToJSON;
  check(test='[false,true,false,true]');
  Check(AB<>nil);
  dyn1.Clear;
  Check(AB=nil);
  Check(dyn1.Count=0);
  Check(dyn1.LoadFromJSON(pointer(test))<>nil);
  Check(length(AB)=4);
  Check(dyn1.Count=4);
  for i := 0 to 3 do
    Check(AB[i]=(i and 1=1));
  Test := dyn1.SaveTo;
  dyn1.Clear;
  Check(AB=nil);
  Check(dyn1.LoadFrom(pointer(test))<>nil);
  Check(dyn1.Count=4);
  for i := 0 to 3 do
    Check(AB[i]=(i and 1=1));
  dyn1.Clear;
  Check(AB=nil);
  Check(dyn1.LoadFromBinary(test));
  Check(dyn1.Count=4);
  for i := 0 to 3 do
    Check(AB[i]=(i and 1=1));
  Check(dyniter.Init(TypeInfo(TBooleanDynArray),test));
  Check(dyniter.Count=4);
  for i := 0 to 3 do begin
    Check(dyniter.FirstField(B));
    Check(B=(i and 1=1));
    B := not B;
    Check(dyniter.Step(B));
    Check(B=(i and 1=1));
  end;
  Check(not dyniter.Step(B));
  Check(not dyniter.FirstField(B));
  Check(dyniter.CheckHash,'checkhash');
  // validate TIntegerDynArray
  Test64K;
  AIP.Init(TypeInfo(TIntegerDynArray),AI);
  for i := 0 to 1000 do begin
    Check(AIP.Count=i);
    Check(AIP.Add(i)=i);
    Check(AIP.Count=i+1);
    Check(AI[i]=i);
  end;
  for i := 0 to 1000 do
    Check(AIP.IndexOf(i)=i);
  for i := 0 to 1000 do begin
    Check(IntegerScanExists(Pointer(AI),i+1,i));
    Check(IntegerScanExists(Pointer(AI),AIP.Count,i));
    Check(not IntegerScanExists(Pointer(AI),AIP.Count,i+2000));
  end;
  Test := AIP.SaveTo;
  Check(Hash32(Test)=$924462C);
  PI := IntegerDynArrayLoadFrom(pointer(Test),AIcount);
  Check(AIcount=1001);
  Check(PI<>nil);
  for i := 0 to 1000 do
    Check(PI[i]=i);
  W.AddDynArrayJSON(AIP);
  U := W.Text;
  P := pointer(U);
  for i := 0 to 1000 do
    Check(GetNextItemCardinal(P)=cardinal(i));
  Check(Hash32(U)=$CBDFDAFC,'hash32a');
  for i := 0 to 1000 do begin
    Test2 := AIP.ElemSave(i);
    Check(length(Test2)=4);
    k := 0;
    AIP.ElemLoad(pointer(Test2),k);
    Check(k=i);
    Check(AIP.ElemLoadFind(pointer(Test2))=i);
  end;
  AIP.Reverse;
  for i := 0 to 1000 do
    Check(AI[i]=1000-i);
  AIP.Clear;
  Check(AIP.LoadFrom(pointer(Test))<>nil);
  for i := 0 to 1000 do
    Check(AIP.IndexOf(i)=i);
  AIP.Clear;
  Check(AIP.LoadFromBinary(Test));
  for i := 0 to 1000 do
    Check(AIP.IndexOf(i)=i);
  for i := 1000 downto 0 do
    if i and 3=0 then
      AIP.Delete(i);
  Check(AIP.Count=750);
  for i := 0 to 1000 do
    if i and 3=0 then
      Check(AIP.IndexOf(i)<0) else
      Check(AIP.IndexOf(i)>=0);
  AIP.Clear;
  Check(AIP.LoadFromJSON(pointer(U))<>nil);
  for i := 0 to 1000 do
    Check(AI[i]=i);
  AIP.Init(TypeInfo(TIntegerDynArray),AI,@AIcount);
  for i := 0 to 50000 do begin
    Check(AIP.Count=i,'use of AIcount should reset it to zero');
    Check(AIP.Add(i)=i);
    Check(AIP.Count=i+1);
    Check(AI[i]=i);
  end;
  AIP.Compare := SortDynArrayInteger;
  AIP.Sort;
  Check(AIP.Count=50001);
  for i := 0 to AIP.Count-1 do
    Check(AIP.Find(i)=i);
  Test := AIP.SaveTo;
  Check(Hash32(Test)=$B9F2502A,'hash32b');
  AIP.Reverse;
  for i := 0 to 50000 do
    Check(AI[i]=50000-i);
  SetLength(AI,AIcount);
  AIP.Init(TypeInfo(TIntegerDynArray),AI);
  AIP.Compare := SortDynArrayInteger;
  AIP.Sort;
  Test := AIP.SaveTo;
  Check(Hash32(Test)=$B9F2502A,'hash32c');
  AIP.Reverse;
  AIP.Slice(AI2,2000,1000);
  Check(length(AI2)=2000);
  for i := 0 to 1999 do
    Check(AI2[i]=49000-i);
  AIP.AddArray(AI2,1000,2000);
  Check(AIP.Count=51001);
  for i := 0 to 50000 do
    Check(AI[i]=50000-i);
  for i := 0 to 999 do
    Check(AI[i+50001]=48000-i);
  AIP.Count := 50001;
  AIP.AddArray(AI2);
  Check(AIP.Count=52001);
  for i := 0 to 50000 do
    Check(AI[i]=50000-i);
  for i := 0 to 1999 do
    Check(AI[i+50001]=49000-i);
  AIP.Clear;
  with DynArray(TypeInfo(TIntegerDynArray),AI) do begin
    Check(LoadFrom(pointer(Test))<>nil);
    for i := 0 to Count-1 do
      Check(AI[i]=i);
  end;
  Check(AIP.Count=50001);
  {$ifndef DELPHI5OROLDER}
  AI2P.Init(TypeInfo(TIntegerDynArray),AI2);
  AIP.AddDynArray(AI2P);
  Check(AIP.Count=52001);
  for i := 0 to 50000 do
    Check(AI[i]=i);
  for i := 0 to 1999 do
    Check(AI[i+50001]=49000-i);
  {$endif}
  // validate TSynValidates (an array of classes is an array of PtrInt)
  AVP.Init(TypeInfo(TSynValidates),AV);
  for i := 0 to 1000 do begin
    Check(AVP.Count=i);
    PtrInt(V) := i;
    Check(AVP.Add(V)=i);
    Check(AVP.Count=i+1);
    Check(AV[i]=V);
  end;
  Check(length(AV)=1001);
  Check(AVP.Count=1001);
  for i := 0 to 1000 do begin
    // untyped const must be the same exact type !
    PtrInt(V) := i;
    Check(AVP.IndexOf(V)=i);
  end;
  Test := AVP.SaveTo;
  Check(Hash32(Test)={$ifdef CPU64}$31484630{$else}$924462C{$endif},'hash32d');
  // validate TRawUTF8DynArray
  AUP.Init(TypeInfo(TRawUTF8DynArray),AU);
  for i := 0 to 1000 do begin
    Check(AUP.Count=i);
    U := UInt32ToUtf8(i+1000);
    Check(AUP.Add(U)=i);
    Check(AUP.Count=i+1);
    Check(AU[i]=U);
  end;
  for i := 0 to 1000 do begin
    U := Int32ToUtf8(i+1000);
    Check(AUP.IndexOf(U)=i);
  end;
  Test := AUP.SaveTo;
  Check(Hash32(@Test[2],length(Test)-1)=$D9359F89,'hash32e'); // trim Test[1]=ElemSize
  for i := 0 to 1000 do begin
    U := Int32ToUtf8(i+1000);
    Check(RawUTF8DynArrayLoadFromContains(pointer(Test),pointer(U),length(U),false)=i);
    Check(RawUTF8DynArrayLoadFromContains(pointer(Test),pointer(U),length(U),true)=i);
  end;
  for i := 0 to 1000 do begin
    U := UInt32ToUtf8(i+1000);
    Test2 := AUP.ElemSave(U);
    Check(length(Test2)>4);
    U := '';
    AUP.ElemLoad(pointer(Test2),U);
    Check(GetInteger(pointer(U))=i+1000);
    Check(AUP.ElemLoadFind(pointer(Test2))=i);
  end;
  W.CancelAll;
  W.AddDynArrayJSON(AUP);
  W.SetText(U);
  Check(Hash32(U)=$1D682EF8,'hash32f');
  P := pointer(U);
  if not CheckFailed(P^='[') then inc(P);
  for i := 0 to 1000 do begin
    Check(P^='"'); inc(P);
    Check(GetNextItemCardinal(P)=cardinal(i+1000));
    if P=nil then
      break;
  end;
  Check(P=nil);
  AUP.Clear;
  Check(AUP.LoadFrom(pointer(Test))-pointer(Test)=length(Test));
  for i := 0 to 1000 do
    Check(GetInteger(pointer(AU[i]))=i+1000);
  AUP.Clear;
  Check(AUP.LoadFromBinary(Test));
  for i := 0 to 1000 do
    Check(GetInteger(pointer(AU[i]))=i+1000);
  Check(dyniter.Init(TypeInfo(TRawUTF8DynArray),pointer(test)));
  Check(dyniter.Count=1001);
  for i := 0 to 1000 do begin
    Check(dyniter.FirstField(U2));
    Check(GetInteger(pointer(U2))=i+1000);
    U2 := '';
    Check(dyniter.Step(U2));
    Check(GetInteger(pointer(U2))=i+1000);
  end;
  Check(not dyniter.Step(U2));
  Check(not dyniter.FirstField(U2));
  Check(dyniter.CheckHash);
  AUP.Clear;
  Check(AUP.LoadFromJSON(pointer(U))<>nil);
  for i := 0 to 1000 do
    Check(GetInteger(pointer(AU[i]))=i+1000);
  for i := 0 to 1000 do begin
    U := Int32ToUtf8(i+1000);
    Check(AUP.IndexOf(U)=i);
  end;
  for i := 1000 downto 0 do
    if i and 3=0 then
      AUP.Delete(i);
  Check(AUP.Count=750);
  for i := 0 to 1000 do begin
    U := Int32ToUtf8(i+1000);
    if i and 3=0 then
      Check(AUP.IndexOf(U)<0) else
      Check(AUP.IndexOf(U)>=0);
  end;
  U := 'inserted';
  AUP.Insert(500,U);
  Check(AUP.IndexOf(U)=500);
  j := 0;
  for i := 0 to AUP.Count-1 do
    if i<>500 then begin
      U := Int32ToUtf8(j+1000);
      if j and 3=0 then
        Check(AUP.IndexOf(U)<0) else
        Check(AUP.IndexOf(U)>=0);
      inc(j);
  end;
  AUP.CreateOrderedIndex(Index,SortDynArrayAnsiString);
  Check(StrComp(pointer(AU[Index[750]]),pointer(AU[Index[749]]))>0);
  for i := 1 to AUP.Count-1 do
    Check(AU[Index[i]]>AU[Index[i-1]]);
  AUP.Compare := SortDynArrayAnsiString;
  AUP.Sort;
  Check(AUP.Sorted);
  Check(AU[AUP.Count-1]='inserted');
  for i := 1 to AUP.Count-1 do
    Check(AU[i]>AU[i-1]);
  j := 0;
  for i := 0 to AUP.Count-1 do
    if i<>500 then begin
      U := Int32ToUtf8(j+1000);
      if j and 3=0 then
        Check(AUP.Find(U)<0) else
        Check(AUP.Find(U)>=0);
      inc(j);
  end;
  AUP.Sorted := false;
  j := 0;
  for i := 0 to AUP.Count-1 do
    if i<>500 then begin
      U := Int32ToUtf8(j+1000);
      if j and 3=0 then
        Check(AUP.Find(U)<0) else
        Check(AUP.Find(U)>=0);
      inc(j);
  end;
  // validate packed binary record (no string inside)
  ARP.Init(TypeInfo(TRecs),AR);
  for i := 0 to 1000 do begin
    Check(ARP.Count=i);
    R.A := i;
    R.B := i+1;
    R.C := i*2.2;
    R.D := i*3.25;
    Check(ARP.Add(R)=i);
    Check(ARP.Count=i+1);
  end;
  for i := 0 to 1000 do begin
    with AR[i] do begin
      Check(A=i);
      Check(B=byte(i+1));
      CheckSame(C,i*2.2);
      CheckSame(D,i*3.25);
    end;
    R.A := i;
    R.B := i+1;
    R.C := i*2.2;
    R.D := i*3.25;
    Check(ARP.IndexOf(R)=i); // will work (packed + no ref-counted types inside)
  end;
  W.CancelAll;
  W.AddDynArrayJSON(ARP);
  U := W.Text;
  // no check(Hash32(U)) since it is very platform-dependent: LoadFromJSON is enough
  P := pointer(U);
  JSON_BASE64_MAGIC_UTF8 := RawUnicodeToUtf8(@MAGIC,2);
  U2 := RawUTF8('[')+JSON_BASE64_MAGIC_UTF8+RawUTF8(BinToBase64(ARP.SaveTo))+RawUTF8('"]');
  Check(U=U2);
  ARP.Clear;
  Check(ARP.LoadFromJSON(pointer(U))<>nil);
  if not CheckFailed(ARP.Count=1001) then
    for i := 0 to 1000 do
    with AR[i] do begin
      Check(A=i);
      Check(B=byte(i+1));
      CheckSame(C,i*2.2);
      CheckSame(D,i*3.25);
    end;
  // validate packed record with strings inside
  AFP.Init(TypeInfo(TFVs),AF);
  for i := 0 to 1000 do begin
    Check(AFP.Count=i);
    Fill(F,i);
    Check(AFP.Add(F)=i);
    Check(AFP.Count=i+1);
  end;
  Fill(F,100);
  Check(RecordEquals(F,AF[100],TypeInfo(TFV)));
  Len := RecordSaveLength(F,TypeInfo(TFV));
  Check(Len=38{$ifdef UNICODE}+length(F.Main)+length(F.Detailed){$endif});
  SetLength(Test,Len);
  Check(RecordSave(F,pointer(Test),TypeInfo(TFV))-pointer(Test)=Len);
  Fill(F,0);
  Check(RecordLoad(F,pointer(Test),TypeInfo(TFV))-pointer(Test)=Len);
  Check(RecordEquals(F,AF[100],TypeInfo(TFV)));
  Fill(F,0);
  Check(RecordLoad(F,Test,TypeInfo(TFV)));
  Check(RecordEquals(F,AF[100],TypeInfo(TFV)));
  Test := RecordSaveBase64(F,TypeInfo(TFV));
  Check(Test<>'');
  Fill(F,0);
  Check(RecordLoadBase64(pointer(Test),length(Test),F,TypeInfo(TFV)));
  Check(RecordEquals(F,AF[100],TypeInfo(TFV)));
  Test := RecordSaveBase64(F,TypeInfo(TFV),true);
  Check(Test<>'');
  Fill(F,0);
  Check(RecordLoadBase64(pointer(Test),length(Test),F,TypeInfo(TFV),true));
  Check(RecordEquals(F,AF[100],TypeInfo(TFV)));
  for i := 0 to 1000 do
    with AF[i] do begin
      Check(Major=i);
      Check(Minor=i+1);
      Check(Release=i+2);
      Check(Build=i+3);
      Check(Main=IntToString(i+1000));
      Check(Detailed=IntToString(2000-i));
      CheckSame(BuildDateTime,36215.12);
      Check(BuildYear=i+2011);
    end;
  for i := 0 to 1000 do begin
    Fill(F,i);
    Check(AFP.IndexOf(F)=i);
  end;
  Test := AFP.SaveTo;
  Check(Hash32(Test)={$ifdef CPU64}{$ifdef FPC}$3DE22166{$else}$A29C10E{$endif}{$else}
    {$ifdef UNICODE}$62F9C106{$else}$6AA2215E{$endif}{$endif},'hash32h');
  for i := 0 to 1000 do begin
    Fill(F,i);
    AFP.ElemCopy(F,F1);
    Check(AFP.ElemEquals(F,F1));
    Test2 := AFP.ElemSave(F);
    Check(length(Test2)>4);
    AFP.ElemClear(F);
    AFP.ElemLoad(pointer(Test2),F);
    Check(AFP.ElemEquals(F,F1));
    Check(AFP.ElemLoadFind(pointer(Test2))=i);
    Check(AFP.ElemLoadFind(pointer(Test2),PAnsiChar(Test2)+length(Test2))=i);
  end;
  W.CancelAll;
  W.AddDynArrayJSON(AFP);
  // note: error? ensure TTestLowLevelCommon run after TTestLowLevelTypes
  // -> otherwise custom serialization is still active with no Build* fields
  U := W.Text;
  {$ifdef ISDELPHI2010} // thanks to enhanced RTTI
  Check(IdemPChar(pointer(U),'[{"MAJOR":0,"MINOR":1,"RELEASE":2,"BUILD":3,'+
    '"MAIN":"1000","DETAILED":"2000","BUILDDATETIME":"1999-02-24T02:52:48",'+
    '"BUILDYEAR":2011},{"MAJOR":1,"MINOR":2,"RELEASE":3,"BUILD":4,'));
  Check(Hash32(U)=$74523E0F,'hash32i');
  {$else}
  Check(U='['+JSON_BASE64_MAGIC_UTF8+BinToBase64(Test)+'"]');
  {$endif}
  AFP.Clear;
  Check(AFP.LoadFrom(pointer(Test))-pointer(Test)=length(Test));
  for i := 0 to 1000 do begin
    Fill(F,i);
    Check(AFP.IndexOf(F)=i);
  end;
  Check(dyniter.Init(TypeInfo(TFVs),pointer(test)));
  Check(dyniter.Count=1001);
  for i := 0 to 1000 do begin
    Check(dyniter.Step(F1));
    Fill(F,i);
    Check(AFP.ElemEquals(F,F1));
  end;
  Check(not dyniter.Step(F1));
  Check(dyniter.CheckHash);
  ST := THeapMemoryStream.Create;
  AFP.SaveToStream(ST);
  AFP.Clear;
  ST.Position := 0;
  AFP.LoadFromStream(ST);
  Check(ST.Position=length(Test));
  for i := 0 to 1000 do begin
    Fill(F,i);
    Check(AFP.IndexOf(F)=i);
  end;
  ST.Free;
  AFP.Clear;
  Check(AFP.LoadFromJSON(pointer(U))<>nil);
  for i := 0 to 1000 do begin
    Fill(F,i);
    Check(RecordEquals(F,AF[i],AFP.ElemType));
  end;
  for i := 0 to 1000 do begin
    Fill(F,i);
    F.BuildYear := 10;
    Check(AFP.IndexOf(F)<0);
    F.BuildYear := i+2011;
    F.Detailed := '??';
    Check(AFP.IndexOf(F)<0);
  end;
  for i := 1000 downto 0 do
    if i and 3=0 then
      AFP.Delete(i);
  Check(AFP.Count=750);
  for i := 0 to 1000 do begin
    Fill(F,i);
    if i and 3=0 then
      Check(AFP.IndexOf(F)<0) else
      Check(AFP.IndexOf(F)>=0);
  end;
  Fill(F,5000);
  AFP.Insert(500,F);
  Check(AFP.IndexOf(F)=500);
  j := 0;
  for i := 0 to AFP.Count-1 do
    if i<>500 then begin
      Fill(F,j);
      if j and 3=0 then
        Check(AFP.IndexOf(F)<0) else
        Check(AFP.IndexOf(F)>=0);
      inc(j);
  end;
  Finalize(Index);
  AFP.CreateOrderedIndex(Index,FVSort);
  for i := 1 to AUP.Count-1 do
    Check(AF[Index[i]].Detailed>AF[Index[i-1]].Detailed);
  AFP.Compare := FVSort;
  AFP.Sort;
  for i := 1 to AUP.Count-1 do
    Check(AF[i].Detailed>AF[i-1].Detailed);
  j := 0;
  for i := 0 to AFP.Count-1 do
    if i<>500 then begin
      Fill(F,j);
      if j and 3=0 then
        Check(AFP.Find(F)<0) else
        Check(AFP.Find(F)>=0);
      inc(j);
  end;
  W.Free;
  // validate packed record with records of strings inside
  AFP.Init(Typeinfo(TFV2s),AF2);
  for i := 0 to 1000 do begin
    Fill(F2.V1,i*2);
    F2.Value := i;
    Fill(F2.V2,i*2+1);
    F2.Text := IntToString(i);
    Check(AFP.Add(F2)=i);
  end;
  Check(AFP.Count=1001);
  TestAF2;
  Test := AFP.SaveTo;
  AFP.Clear;
  Check(AFP.Count=0);
  Check(AFP.LoadFromBinary(Test));
  Check(AFP.Count=1001);
  TestAF2;
  // validate https://synopse.info/forum/viewtopic.php?pid=16581#p16581
  DP.Modified := Now;
  DP.Data := '1';
  dyn1.Init(TypeInfo(TDataItems),dyn1Array);
  dyn1.Add(DP);
  DP.Modified := Now;
  DP.Data := '2';
  dyn2.Init(TypeInfo(TDataItems),dyn2Array);
  check(dyn2.count=0);
  dyn2.Add(DP);
  check(length(dyn2Array)=1);
  check(dyn2.count=1);
  dyn2.AddArray(dyn1Array);
  check(dyn2.count=2);
  check(dyn2.ElemEquals(dyn2Array[0],DP));
  check(dyn2.ElemEquals(dyn2Array[1],dyn1Array[0]));
  {$ifndef DELPHI5OROLDER}
  dyn2.AddDynArray(dyn1);
  check(dyn2.count=3);
  check(dyn2.ElemEquals(dyn2Array[0],DP));
  check(dyn2.ElemEquals(dyn2Array[1],dyn1Array[0]));
  check(dyn2.ElemEquals(dyn2Array[2],dyn1Array[0]));
  {$endif}
  // valide generic-like features
  // see http://docwiki.embarcadero.com/CodeExamples/en/Generics_Collections_TDictionary_(Delphi)
  ACities.Init(TypeInfo(TCityDynArray),Province.Cities);
  City.Name := 'Iasi';
  City.Country := 'Romania';
  City.Latitude := 47.16;
  City.Longitude := 27.58;
  ACities.Add(City);
  City.Name := 'London';
  City.Country := 'United Kingdom';
  City.Latitude := 51.5;
  City.Longitude := -0.17;
  ACities.Add(City);
  City.Name := 'Buenos Aires';
  City.Country := 'Argentina';
  City.Latitude := 0;
  City.Longitude := 0;
  ACities.Add(City);
  Check(ACities.Count=3);
  ACities.Compare := SortDynArrayString; // will search by Name = 1st field
  City.Name := 'Iasi';
  Check(ACities.FindAndFill(City)=0);
  Check(City.Name='Iasi');
  Check(City.Country='Romania');
  CheckSame(City.Latitude,47.16);
  CheckSame(City.Longitude,27.58);
  Check(ACities.FindAndDelete(City)=0);
  Check(City.Name='Iasi');
  Check(ACities.Find(City)<0);
  City.Name := 'Buenos Aires';
  City.Country := 'Argentina';
  City.Latitude := -34.6;
  City.Longitude := -58.45;
  Check(ACities.FindAndUpdate(City)>=0);
  City.Latitude := 0;
  City.Longitude := 0;
  Check(City.Name='Buenos Aires');
  Check(ACities.FindAndFill(City)>=0);
  CheckSame(City.Latitude,-34.6);
  CheckSame(City.Longitude,-58.45);
  Check(ACities.FindAndAddIfNotExisting(City)>=0);
  City.Name := 'Iasi';
  City.Country := 'Romania';
  City.Latitude := 47.16;
  City.Longitude := 27.58;
  Check(ACities.FindAndAddIfNotExisting(City)<0);
  Check(City.Name='Iasi');
  Check(ACities.FindAndUpdate(City)>=0);
  ACities.Sort;
  for i := 1 to high(Province.Cities) do
    Check(Province.Cities[i].Name>Province.Cities[i-1].Name);
  Check(ACities.Count=3);
  // complex record test
  Province.Name := 'Test';
  Province.Comment := 'comment';
  Province.Year := 1000;
  Test := RecordSave(Province,TypeInfo(TProvince));
  RecordClear(Province,TypeInfo(TProvince));
  Check(Province.Name='');
  Check(Province.Comment='');
  Check(length(Province.Cities)=0);
  Check(ACities.Count=0);
  Province.Year := 0;
  Check(RecordLoad(Province,pointer(Test),TypeInfo(TProvince))^=#0);
  Check(Province.Name='Test');
  Check(Province.Comment='comment');
  Check(Province.Year=1000);
  Check(length(Province.Cities)=3);
  Check(ACities.Count=3);
  for i := 1 to high(Province.Cities) do
    Check(Province.Cities[i].Name>Province.Cities[i-1].Name);
  Province.Cities := nil;
  Test := RecordSave(Province,TypeInfo(TProvince));
  RecordClear(Province,TypeInfo(TProvince));
  Check(Province.Name='');
  Check(Province.Comment='');
  Check(length(Province.Cities)=0);
  Check(ACities.Count=0);
  Check(RecordLoad(Province,pointer(Test),TypeInfo(TProvince))^=#0);
  Check(Province.Name='Test');
  Check(Province.Comment='comment');
  Check(Province.Year=1000);
  Check(length(Province.Cities)=0);
  Check(ACities.Count=0);
  // big array test
  ACities.Init(TypeInfo(TCityDynArray),Province.Cities);
  ACities.Clear;
  for i := 0 to 10000 do begin
    City.Name := IntToString(i);
    City.Latitude := i*3.14;
    City.Longitude := i*6.13;
    Check(ACities.Add(City)=i);
  end;
  Check(ACities.Count=Length(Province.Cities));
  Check(ACities.Count=10001);
  TestCities;
  ACities.Init(TypeInfo(TCityDynArray),Province.Cities,@count);
  ACities.Clear;
  for i := 0 to 100000 do begin
    City.Name := IntToString(i);
    City.Latitude := i*3.14;
    City.Longitude := i*6.13;
    Check(ACities.Add(City)=i);
  end;
  Check(ACities.Count=count);
  TestCities;
end;

{$ifdef CPUINTEL}
function BufEquals(P, n, b: PtrInt): boolean;
begin // slower than FillChar, faster than for loop, but fast enough for testing
  result := false;
  b := b*{$ifdef CPU32}$01010101{$else}$0101010101010101{$endif};
  inc(n,P-SizeOf(P));
  if n>=P then
    repeat
      if PPtrInt(P)^<>b then
        exit;
      inc(PPtrInt(P));
    until n<P;
  inc(n,SizeOf(P));
  if P<n then
    repeat
      if PByte(P)^<>byte(b) then
        exit;
      inc(P);
    until P>=n;
  result := true;
end;

function IsBufIncreasing(P: PByteArray; n: PtrInt; b: byte): boolean;
var i: PtrInt;
begin
  result := false;
  for i := 0 to n-1 do
    if P[i]<>b then
      exit else
      inc(b);
  result := true;
end;

{$ifndef ABSOLUTEPASCAL}
{$ifdef CPUX64} // will define its own self-dispatched SSE2/AVX functions
  {$define HASCPUIDX64}
{$endif}
{$endif}

procedure TTestLowLevelCommon.CustomRTL;
// note: FPC uses the RTL for FillCharFast/MoveFast
var buf: RawByteString;
   procedure Validate(rtl: boolean=false);
   var i,len,filled,moved: PtrInt;
       b1,b2: byte;
       timer: TPrecisionTimer;
       P: PByteArray;
       msg: string;
       cpu: RawUTF8;
       elapsed: Int64;
   begin
     // first validate FillCharFast
     filled := 0;
     b1 := 0;
     len := 1;
     repeat
       b2 := (b1+1) and 255;
       buf[len+1] := AnsiChar(b1);
       if rtl then
         FillChar(pointer(buf)^,len,b2) else
         FillCharFast(pointer(buf)^,len,b2);
       inc(filled,len);
       Check(BufEquals(PtrInt(buf),len,b2));
       Check(ord(buf[len+1])=b1);
       b1 := b2;
       if len<16384 then
         inc(len) else
         inc(len,777+len shr 4);
     until len>=length(buf);
     // small len makes timer.Resume/Pause unreliable -> single shot measure
     b1 := 0;
     len := 1;
     timer.Start;
     repeat
       b2 := (b1+1) and 255;
       if rtl then
         FillChar(pointer(buf)^,len,b2) else
         FillCharFast(pointer(buf)^,len,b2);
       b1 := b2;
       if len<16384 then
         inc(len) else
         inc(len,777+len shr 4);
     until len>=length(buf);
     timer.Stop;
     {$ifdef HASCPUIDX64}
     cpu := GetSetName(TypeInfo(TX64CpuFeatures),CPUIDX64);
     {$endif}
     if rtl then
       msg := 'FillChar' else
       FormatString('FillCharFast [%]',[cpu],msg);
     NotifyTestSpeed(msg,1,filled,@timer);
     // validates overlapping forward Move/MoveFast
     if rtl then
       msg := 'Move' else
       FormatString('MoveFast [%]',[cpu],msg);
     P := pointer(buf);
     for i := 0 to length(buf)-1 do
       P[i] := i; // fills with 0,1,2,...
     Check(IsBufIncreasing(p,length(buf),0));
     len := 1;
     moved := 0;
     timer.Start;
     repeat
       if rtl then
         Move(P[moved+1],P[moved],len) else
         MoveFast(p[moved+1],p[moved],len);
       inc(moved,len);
       Check(p[moved]=p[moved-1]);
       inc(len);
     until moved+len>=length(buf);
     NotifyTestSpeed(msg,1,moved,@timer);
     Check(IsBufIncreasing(p,moved,1));
     checkEqual(Hash32(buf),2284147540);
     // forward and backward overlapped moves on small buffers
     elapsed := 0;
     moved := 0;
     for len := 1 to 48 do begin
       timer.Start;
       if rtl then
         for i := 1 to 10000 do begin
           Move(P[100],P[i],len);
           Move(P[i],P[100],len);
         end else
         for i := 1 to 10000 do begin
           MoveFast(P[100],P[i],len);
           MoveFast(P[i],P[100],len);
         end;
       inc(moved,20000*len);
       inc(elapsed,NotifyTestSpeed('%b %',[len,msg],1,20000*len,@timer,{onlylog=}true));
     end;
     timer.FromExternalMicroSeconds(elapsed);
     NotifyTestSpeed('small %',[msg],1,moved,@timer);
     checkEqual(Hash32(buf),1635609040);
     // forward and backward non-overlapped moves on big buffers
     len := (length(buf)-3200) shr 1;
     timer.Start;
     for i := 1 to 25 do
       if rtl then begin
         Move(P[len],P[i],len-i*10);
         Move(P[i],P[len],len-i*10);
       end else begin
         MoveFast(p[len],p[i],len-i*10);
         MoveFast(P[i],P[len],len-i*10);
       end;
     NotifyTestSpeed('big %',[msg],1,50*len,@timer);
     checkEqual(Hash32(buf),818419281);
     // forward and backward overlapped moves on big buffers
     len := length(buf)-3200;
     for i := 1 to 3 do
       if rtl then begin
         Move(P[3100],P[i],len-i);
         Move(P[i],P[3200],len-i);
       end else begin
         MoveFast(p[3100],p[i],len-i);
         MoveFast(P[i],P[3200],len-i);
       end;
     checkEqual(Hash32(buf),1646145792);
   end;
{$ifdef HASCPUIDX64} var cpu: TX64CpuFeatures; {$endif}
begin
  SetLength(buf,16 shl 20); // 16MB
  {$ifdef HASCPUIDX64} // activate and validate SSE2 + AVX branches
  cpu := CPUIDX64;
  CPUIDX64 := []; // default SSE2 128-bit process
  Validate;
  {$ifdef FPC} // Delphi doesn't support AVX asm
  if cpuAvx in cpu then begin
    CPUIDX64 := [cpuAvx]; // AVX 256-bit process
    Validate;
  end;
  {$endif FPC}
  CPUIDX64 := cpu; // there is no AVX2 move/fillchar (still 256-bit wide)
  if (cpu<>[]) and (cpu<>[cpuAvx]) and (cpu<>[cpuAvx,cpuAvx2]) then
    Validate;
  // no Validate(true): RedirectCode(@System.FillChar,@FillcharFast)
  {$else}
  Validate({rtl=}true);
  Validate(false);
  {$endif HASCPUIDX64}
end;
{$endif CPUINTEL}

procedure TTestLowLevelCommon.SystemCopyRecord;
type TR = record
       One: integer;
       S1: AnsiString;
       Three: byte;
       S2: WideString;
       Five: boolean;
       {$ifndef NOVARIANTS}
       V: Variant;
       {$endif}
       R: Int64Rec;
       Arr: array[0..10] of AnsiString;
       Dyn: array of integer;
       Bulk: array[0..19] of byte;
     end;
var A,B,C: TR;
    i: integer;
begin
  FillCharFast(A,sizeof(A),0);
  for i := 0 to High(A.Bulk) do
    A.Bulk[i] := i;
  A.S1 := 'one';
  A.S2 := 'two';
  A.Five := true;
  A.Three := $33;
  {$ifndef NOVARIANTS}
  A.V := 'One Two';
  {$endif}
  A.R.Lo := 10;
  A.R.Hi := 20;
  A.Arr[5] := 'five';
  SetLength(A.Dyn,10);
  A.Dyn[9] := 9;
  B := A;
  Check(A.One=B.One);
  Check(A.S1=B.S1);
  Check(A.Three=B.Three);
  Check(A.S2=B.S2);
  Check(A.Five=B.Five);
  {$ifndef NOVARIANTS}
  Check(A.V=B.V);
  {$endif}
  Check(Int64(A.R)=Int64(B.R));
  Check(A.Arr[5]=B.Arr[5]);
  Check(A.Arr[0]=B.Arr[0]);
  Check(A.Dyn[9]=B.Dyn[9]);
  Check(A.Dyn[0]=0);
  for i := 0 to High(B.Bulk) do
    Check(B.Bulk[i]=i);
  for i := 0 to High(B.Bulk) do
    Check(CompareMem(@A.Bulk,@B.Bulk,i));
  for i := 0 to High(B.Bulk) do
    Check(CompareMemSmall(@A.Bulk,@B.Bulk,i));
  for i := 0 to High(B.Bulk) do
    Check(CompareMemFixed(@A.Bulk,@B.Bulk,i));
  FillCharFast(A.Bulk,sizeof(A.Bulk),255);
  for i := 0 to High(B.Bulk) do
    Check(CompareMem(@A.Bulk,@B.Bulk,i)=(i=0));
  for i := 0 to High(B.Bulk) do
    Check(CompareMemSmall(@A.Bulk,@B.Bulk,i)=(i=0));
  for i := 0 to High(B.Bulk) do
    Check(CompareMemFixed(@A.Bulk,@B.Bulk,i)=(i=0));
  B.Three := 3;
  B.Dyn[0] := 10;
  C := B;
  Check(A.One=C.One);
  Check(A.S1=C.S1);
  Check(C.Three=3);
  Check(A.S2=C.S2);
  Check(A.Five=C.Five);
  {$ifndef NOVARIANTS}
  Check(A.V=C.V);
  {$endif}
  Check(Int64(A.R)=Int64(C.R));
  Check(A.Arr[5]=C.Arr[5]);
  Check(A.Arr[0]=C.Arr[0]);
  Check(A.Dyn[9]=C.Dyn[9]);
  {Check(A.Dyn[0]=0) bug in original VCL?}
  Check(C.Dyn[0]=10);
end;

procedure TTestLowLevelCommon.UrlEncoding;
var i,j: integer;
    s: RawByteString;
    name,value,utf: RawUTF8;
    str: string;
    P: PUTF8Char;
    GUID2: TGUID;
    U: TURI;
const GUID: TGUID = '{c9a646d3-9c61-4cb7-bfcd-ee2522c8f633}';
procedure Test(const decoded,encoded: RawUTF8);
begin
  Check(UrlEncode(decoded)=encoded);
  Check(UrlDecode(encoded)=decoded);
  Check(UrlDecode(PUTF8Char(encoded))=decoded);
end;
begin
  str := UTF8ToString(UrlEncode(StringToUTF8('https://test3.diavgeia.gov.gr/doc/')));
  check(str='https%3A%2F%2Ftest3.diavgeia.gov.gr%2Fdoc%2F');
  Test('abcdef','abcdef');
  Test('abcdefyzABCDYZ01239_-.~ ','abcdefyzABCDYZ01239_-.%7E+');
  Test('"Aardvarks lurk, OK?"','%22Aardvarks+lurk%2C+OK%3F%22');
  Test('"Aardvarks lurk, OK%"','%22Aardvarks+lurk%2C+OK%25%22');
  Test('where=name like :(''Arnaud%'')','where%3Dname+like+%3A%28%27Arnaud%25%27%29');
  Check(UrlDecode('where=name%20like%20:(%27Arnaud%%27):')=
    'where=name like :(''Arnaud%''):','URI from browser');
  P := UrlDecodeNextNameValue('where=name+like+%3A%28%27Arnaud%25%27%29%3A',
    name,value);
  Check(P<>nil);
  Check(P^=#0);
  Check(name='where');
  Check(value='name like :(''Arnaud%''):');
  P := UrlDecodeNextNameValue('where%3Dname+like+%3A%28%27Arnaud%25%27%29%3A',
    name,value);
  Check(P<>nil);
  Check(P^=#0);
  Check(name='where');
  Check(value='name like :(''Arnaud%''):');
  P := UrlDecodeNextNameValue('where%3Dname+like+%3A%28%27Arnaud%%27%29%3A',
    name,value);
  Check(P<>nil);
  Check(P^=#0);
  Check(name='where');
  Check(value='name like :(''Arnaud%''):','URI from browser');
  P := UrlDecodeNextNameValue('name%2Ccom+plex=value',name,value);
  Check(P<>nil);
  Check(P^=#0);
  Check(name='name,com plex');
  Check(value='value');
  P := UrlDecodeNextNameValue('name%2Ccomplex%3Dvalue',name,value);
  Check(P<>nil);
  Check(P^=#0);
  Check(name='name,complex');
  Check(value='value');
  for i := 0 to 100 do begin
    j := i*5; // circumvent weird FPC code generation bug in -O2 mode
    s := RandomString(j);
    Check(UrlDecode(UrlEncode(s))=s,string(s));
  end;
  utf := BinToBase64URI(@GUID,sizeof(GUID));
  Check(utf='00amyWGct0y_ze4lIsj2Mw');
  FillCharFast(GUID2,sizeof(GUID2),0);
  Check(Base64uriToBin(utf,@GUID2,SizeOf(GUID2)));
  Check(IsEqualGUID(GUID2,GUID));
  Check(IsEqualGUID(@GUID2,@GUID));
  Check(U.From('toto.com'));
  Check(U.URI='http://toto.com/');
  Check(U.From('toto.com:123'));
  Check(U.URI='http://toto.com:123/');
  Check(U.From('https://toto.com:123/tata/titi'));
  Check(U.URI='https://toto.com:123/tata/titi');
  Check(U.From('https://toto.com:123/tata/tutu:tete'));
  Check(U.URI='https://toto.com:123/tata/tutu:tete');
  Check(U.From('toto.com/tata/tutu:tete'));
  Check(U.URI='http://toto.com/tata/tutu:tete');
end;

procedure TTestLowLevelCommon._GUID;
var i: integer;
    s: RawByteString;
    st: string;
    g,g2: TGUID;
const GUID: TGUID = '{c9a646d3-9c61-4cb7-bfcd-ee2522c8f633}';
begin
  s := GUIDToRawUTF8(GUID);
  Check(s='{C9A646D3-9C61-4CB7-BFCD-EE2522C8F633}');
  Check(TextToGUID(@s[2],@g2)^='}');
  Check(IsEqualGUID(g2,GUID));
  Check(GUIDToString(GUID)='{C9A646D3-9C61-4CB7-BFCD-EE2522C8F633}');
  Check(IsEqualGUID(RawUTF8ToGUID(s),GUID));
  for i := 1 to 1000 do begin
    g.D1 := Random(maxInt);
    g.D2 := Random(65535);
    g.D3 := Random(65535);
    Int64(g.D4) := Int64(Random(maxInt))*Random(maxInt);
    st := GUIDToString(g);
    {$ifndef DELPHI5OROLDER}
    Check(st=SysUtils.GUIDToString(g));
    {$endif}
    Check(IsEqualGUID(StringToGUID(st),g));
    s := GUIDToRawUTF8(g);
    Check(st=UTF8ToString(s));
    st[Random(38)+1] := ' ';
    g2 := StringToGUID(st);
    Check(IsZero(@g2,sizeof(g2)));
    Check(TextToGUID(@s[2],@g2)^='}');
    Check(IsEqualGUID(g2,g));
    Check(IsEqualGUID(@g2,@g));
    Check(IsEqualGUID(RawUTF8ToGUID(s),g));
    inc(g.D1);
    Check(not IsEqualGUID(g2,g));
    Check(not IsEqualGUID(RawUTF8ToGUID(s),g));
  end;
  {$ifdef ISDELPHI2010}
  s := RecordSaveJSON(g,TypeInfo(TGUID));
  FillCharFast(g2,sizeof(g2),0);
  Check(RecordLoadJSON(g2,pointer(s),TypeInfo(TGUID))<>nil);
  Check(IsEqualGUID(g2,g));
  {$endif}
end;

procedure TTestLowLevelCommon._ParseCommandArguments;
  procedure Test(const cmd: RawUTF8; const expected: array of RawUTF8;
    const flags: TParseCommands = []; posix: boolean=true);
  var tmp: RawUTF8;
      n, i: integer;
      a: TParseCommandsArgs;
  begin
    if checkfailed(ParseCommandArgs(cmd, nil, nil, nil, posix) = flags) then
      exit;
    FillcharFast(a, SizeOf(a), 255);
    check(ParseCommandArgs(cmd, @a, @n, @tmp, posix) = flags);
    if (flags = []) and not CheckFailed(n = length(expected)) then begin
      for i := 0 to high(expected) do
        check(StrComp(pointer(a[i]), pointer(expected[i])) = 0);
      check(a[n] = nil);
    end;
  end;
begin
  Test('', [], [pcInvalidCommand]);
  Test('one', ['one']);
  Test('one two', ['one', 'two']);
  Test('    one     two    ', ['one', 'two']);
  Test('"one" two', ['one', 'two']);
  Test('one "two"', ['one', 'two']);
  Test('one     "two"', ['one', 'two']);
  Test('one " two"', ['one', ' two']);
  Test('" one" two', [' one', 'two']);
  Test(''' one'' two', [' one', 'two']);
  Test('"one one" two', ['one one', 'two']);
  Test('one "two two"', ['one', 'two two']);
  Test('"1  2"    "3    4"', ['1  2', '3    4']);
  Test('"1 '' 2"    "3    4"', ['1 '' 2', '3    4']);
  Test('''1  2''    "3    4"', ['1  2', '3    4']);
  Test('1 ( "3    4"', [], [pcHasParenthesis]);
  Test('1 "3  "  4"', [], [pcUnbalancedDoubleQuote]);
  Test(''' "3  4"', [], [pcUnbalancedSingleQuote]);
  Test('one|two', [], [pcHasRedirection]);
  Test('one\|two', ['one|two'], []);
  Test('"one|two"', ['one|two']);
  Test('one>two', [], [pcHasRedirection]);
  Test('one\>two', ['one>two'], []);
  Test('"one>two"', ['one>two']);
  Test('one&two', [], [pcHasJobControl]);
  Test('one\&two', ['one&two'], []);
  Test('"one&two"', ['one&two']);
  Test('one`two', [], [pcHasSubCommand]);
  Test('''one`two''', ['one`two']);
  Test('one$two', [], [pcHasShellVariable]);
  Test('''one$two''', ['one$two']);
  Test('one$(two)', [], [pcHasSubCommand, pcHasParenthesis]);
  Test('one\$two', ['one$two'], []);
  Test('''one$(two)''', ['one$(two)']);
  Test('one*two', [], [pcHasWildcard]);
  Test('"one*two"', ['one*two']);
  Test('one*two', [], [pcHasWildcard]);
  Test('''one*two''', ['one*two']);
  Test('one\ two', ['one two'], []);
  Test('one\\two', ['one\two'], []);
  Test('one\\\\\\two', ['one\\\two'], []);
  Test('one|two', [], [pcHasRedirection], {posix=}false);
  Test('one&two', ['one&two'], [], false);
  Test(''' one'' two', ['''', 'one''', 'two'], [], false);
  Test('"one" two', ['one', 'two'], [], false);
  Test('one "two"', ['one', 'two'], [], false);
  Test('one     "two"', ['one', 'two'], [], false);
  Test('one " two"', ['one', ' two'], [], false);
  Test('" one" two', [' one', 'two'], [], false);
  Test('"one one" two', ['one one', 'two'], [], false);
end;

procedure TTestLowLevelCommon._IsMatch;
var i,j: integer;
    V, cont: RawUTF8;
    match: TMatch;
    reuse,isword: boolean;

  procedure Contains;
  begin
    check(match.Match('12'));
    check(match.Match('12e'));
    check(match.Match('12er'));
    check(match.Match('a12'));
    check(match.Match('a12e'));
    check(match.Match('ab12'));
    check(match.Match('ab12er'));
    check(not match.Match('1'));
    check(not match.Match('a1'));
    check(not match.Match('a1b2'));
    check(not match.Match('1a2'));
  end;

  function GL(a,b: PAnsiChar; const c: RawUTF8): boolean;
  begin // avoid Delphi compiler complains about PUTF8Char/PAnsiChar types
    result := GetLineContains(pointer(a), pointer(b), pointer(c));
  end;

begin
  V := '123456789ABC'#10'DEF0zxy';
  Check(GL(@V[1],nil,'1'));
  Check(GL(@V[1],nil,'C'));
  Check(GL(@V[1],nil,'89'));
  Check(not GL(@V[1],nil,'ZX'));
  Check(GL(@V[14],nil,'ZXY'));
  Check(not GL(@V[1],nil,'890'));
  Check(GL(@V[1],@V[21],'89'));
  Check(GL(@V[14],@V[21],'ZX'));
  Check(not GL(@V[1],@V[21],'ZX'));
  Check(GL(@V[14],@V[21],'ZXY'));
  Check(not GL(@V[1],@V[5],'89'));
  Check(not GL(@V[1],@V[15],'ZXY'));
  Check(not GL(@V[14],@V[17],'ZXY'));
  V := '1234567890123456'#13'1234567890123456789';
  for j := 1 to 16 do begin
    for i := j to 16 do begin
      CheckEqual(BufferLineLength(@V[j],@V[i]),i-j);
      CheckEqual(GetLineSize(@V[j],@V[i]),i-j);
    end;
    for i := 17 to 34 do begin
      CheckEqual(BufferLineLength(@V[j],@V[i]),17-j);
      CheckEqual(GetLineSize(@V[j],@V[i]),17-j);
    end;
    CheckEqual(GetLineSize(@V[j],nil),17-j);
  end;
  V := '12345678901234561234567890123456'#10'1234567890123456789';
  for j := 1 to 32 do begin
    for i := j to 32 do begin
      CheckEqual(BufferLineLength(@V[j],@V[i]),i-j);
      CheckEqual(GetLineSize(@V[j],@V[i]),i-j);
    end;
    for i := 33 to 50 do begin
      CheckEqual(BufferLineLength(@V[j],@V[i]),33-j);
      CheckEqual(GetLineSize(@V[j],@V[i]),33-j);
    end;
    CheckEqual(GetLineSize(@V[j],nil),33-j);
  end;
  Check(IsMatch('','',true));
  Check(not IsMatch('','toto',true));
  Check(not IsMatch('Bidule.pas','',true));
  Check(IsMatch('Bidule.pas','Bidule.pas',true));
  Check(IsMatch('Bidule.pas','BIDULE.pas',true));
  Check(IsMatch('Bidule.pas','Bidule.paS',true));
  Check(IsMatch('Bidule.pas','Bidule.pas',false));
  Check(not IsMatch('Bidule.pas','bidule.pas',false));
  Check(not IsMatch('bidule.pas','bidulE.pas',false));
  Check(not IsMatch('bidule.pas','bidule.paS',false));
  Check(not IsMatch('bidule.pas','bidule.pa',false));
  for i := 0 to 200 do begin
    V := Int32ToUtf8(i);
    Check(IsMatch(V,V,false)=IsMatch(V,V,true));
  end;
  Check(IsMatch('test*','test',false));
  Check(IsMatch('test*','test',true));
  Check(IsMatch('test*','teste',false));
  Check(IsMatch('test*','teste',true));
  Check(IsMatch('test*','tester',false));
  Check(IsMatch('test*','tester',true));
  Check(IsMatch('a*','anything',true));
  Check(IsMatch('a*','a',true));
  Check(IsMatch('*','anything',true));
  Check(IsMatch('*.pas','Bidule.pas',true));
  Check(IsMatch('*.pas','Bidule.pas',false));
  Check(IsMatch('*.PAS','Bidule.pas',true));
  Check(not IsMatch('*.PAS','Bidule.pas',false));
  Check(IsMatch('*.p?s','Bidule.pas',true));
  Check(IsMatch('*.p*S','Bidule.pas',true));
  Check(IsMatch('B*.PAS','bidule.pas',true));
  Check(IsMatch('*.p?s','bidule.pas',false));
  Check(IsMatch('*.p*s','bidule.pas',false));
  Check(IsMatch('b*.pas','bidule.pas',false));
  Check(not IsMatch('B*.das','Bidule.pas',true));
  Check(IsMatch('bidule.*','Bidule.pas',true));
  Check(IsMatch('ma?ch.*','match.exe',false));
  Check(IsMatch('ma?ch.*','mavch.dat',false));
  Check(IsMatch('ma?ch.*','march.on',false));
  Check(IsMatch('ma?ch.*','march.',false));
  Check(IsMatch('ab*.exyz', 'ab.exyz',true));
  Check(IsMatch('ab[ef]xyz', 'abexyz',false));
  Check(IsMatch('ab[ef]xyz', 'abexyz',true));
  Check(IsMatch('ab*.[ef]xyz', 'abcd.exyz',true));
  Check(IsMatch('ab*.[ef]xyz', 'ab.exyz',true));
  Check(IsMatch('ab*.[ef]xyz', 'abcd.exyz',true));
  Check(IsMatch('ab*.[ef]xyz', 'ab.fxyz',true));
  Check(IsMatch('ab*.[ef]xyz', 'abcd.fxyz',true));
  check(not IsMatch('ab[cd]e','abdde',false));
  check(not IsMatch('ab[cd]ex','abddex',false));
  check(not IsMatch('ab*.[cd]e','ab.dde',false));
  check(not IsMatch('ab*.[cd]ex','ab.ddex',false));
  V := 'this [e-n]s a [!zy]est';
  check(not IsMatch(V,V,false));
  Check(IsMatch(V,'this is a test',false));
  Check(IsMatch(V,'this is a rest',false));
  Check(not IsMatch(V,'this is a zest',false));
  Check(not IsMatch(V,'this as a test',false));
  Check(not IsMatch(V,'this as a rest',false));
  for reuse := false to true do begin  // ensure very same behavior
    match.Prepare(V, false, reuse);
    Check(not match.Match(V));
    Check(match.Match('this is a test'));
    Check(match.Match('this is a rest'));
    Check(not match.Match('this is a zest'));
    match.Prepare('test', false, reuse);
    check(match.Match('test'));
    check(not match.Match('tes'));
    check(not match.Match('tests'));
    check(not match.Match('tesT'));
    match.Prepare('teST', true, reuse);
    check(match.Match('test'));
    check(match.Match('test'));
    match.Prepare('*', false, reuse);
    check(match.Match('test'));
    check(match.Match('tests'));
    match.Prepare('*', true, reuse);
    check(match.Match('test'));
    check(match.Match('tests'));
    match.Prepare('**', false, reuse);
    check(match.Match('test'));
    check(match.Match('tests'));
    match.Prepare('****', false, reuse);
    check(match.Match('test'));
    check(match.Match('tests'));
    match.Prepare('*.*', false, reuse);
    check(match.Match('te.st'));
    check(match.Match('te.st.'));
    check(match.Match('test.'));
    check(match.Match('.test'));
    check(match.Match('.'));
    check(not match.Match('test'));
    match.Prepare('*.*', true, reuse);
    check(match.Match('te.st'));
    check(match.Match('te.st.'));
    check(match.Match('test.'));
    check(match.Match('.test'));
    check(not match.Match('test'));
    check(match.Match('.'));
    match.Prepare('test*', false, reuse);
    check(match.Match('test'));
    check(match.Match('tests'));
    check(match.Match('tester'));
    check(not match.Match('atest'));
    check(not match.Match('tes'));
    check(not match.Match('tEst'));
    check(not match.Match('tesT'));
    check(not match.Match('t'));
    match.Prepare('*test', false, reuse);
    check(match.Match('test'));
    check(match.Match('stest'));
    check(match.Match('attest'));
    check(not match.Match('est'));
    check(not match.Match('testa'));
    check(not match.Match('tes'));
    check(not match.Match('tEst'));
    check(not match.Match('tesT'));
    check(not match.Match('t'));
    match.Prepare('*t', false, reuse);
    check(match.Match('t'));
    check(match.Match('st'));
    check(match.Match('tt'));
    check(match.Match('att'));
    check(not match.Match('s'));
    check(not match.Match('es'));
    check(not match.Match('ts'));
    match.Prepare('**', false, reuse);
    check(match.Match('') = reuse);
    check(match.Match('test'));
    match.Prepare('*test*', false, reuse);
    check(match.Match('test'));
    check(match.Match('tests'));
    check(match.Match('tester'));
    check(match.Match('atest'));
    check(match.Match('ateste'));
    check(match.Match('abtest'));
    check(match.Match('abtester'));
    check(not match.Match('tes'));
    check(not match.Match('ates'));
    check(not match.Match('tesates'));
    check(not match.Match('tesT'));
    check(not match.Match('Teste'));
    check(not match.Match('TEster'));
    check(not match.Match('atEst'));
    check(not match.Match('ateSTe'));
    match.Prepare('*12*', false, reuse);
    Contains;
    if reuse then begin
      cont := '12';
      match.PrepareContains(cont, false);
      Contains;
      cont := '12';
      match.PrepareContains(cont, true);
      Contains;
    end;
    match.Prepare('*teSt*', true, reuse);
    check(match.Match('test'));
    check(match.Match('teste'));
    check(match.Match('tester'));
    check(match.Match('atest'));
    check(match.Match('ateste'));
    check(match.Match('abtest'));
    check(match.Match('abtester'));
    check(match.Match('tesT'));
    check(match.Match('Teste'));
    check(match.Match('TEster'));
    check(match.Match('atEst'));
    check(match.Match('ateSTe'));
    check(match.Match('abteST'));
    check(match.Match('abtEster'));
    check(not match.Match('tes'));
    check(not match.Match('ates'));
    check(not match.Match('tesates'));
    match.Prepare('*te?t*', true, reuse);
    check(match.Match('test'));
    check(match.Match('tezt'));
    check(match.Match('teste'));
    check(match.Match('tezte'));
    check(match.Match('tester'));
    check(match.Match('atest'));
    check(match.Match('ateste'));
    check(not match.Match('tes'));
    check(not match.Match('tet'));
    check(not match.Match('ates'));
    check(not match.Match('tesates'));
    match.Prepare('?est*', true, reuse);
    check(match.Match('test'));
    check(match.Match('test'));
    check(match.Match('teste'));
    check(match.Match('tester'));
    check(not match.Match('tezte'));
    check(not match.Match('atest'));
    check(not match.Match('est'));
    check(not match.Match('este'));
    check(not match.Match('tes'));
    check(not match.Match('tet'));
    check(not match.Match('ates'));
    check(not match.Match('tesates'));
    match.Prepare('a*bx*cy*d', false, reuse);
    check(match.Match('abxcyd'));
    check(match.Match('a1bxcyd'));
    check(match.Match('a12bxcyd'));
    check(match.Match('a123bxcyd'));
    check(match.Match('abx1cyd'));
    check(match.Match('abx12cyd'));
    check(match.Match('abxcy1d'));
    check(match.Match('abxcy12d'));
    check(match.Match('abxcy123d'));
    check(not match.Match('abcyd'));
    check(not match.Match('abxcyde'));
    match.Prepare('************************************************'+
         '************************************************'+
         '**************************************************.*', false, reuse);
    check(match.MatchThreadSafe('abxcyd.'));
    check(match.MatchThreadSafe('abxc.yd'));
    check(match.MatchThreadSafe('abxcy.d'));
    check(match.MatchThreadSafe('.'));
    check(match.MatchThreadSafe('.a'));
    check(match.MatchThreadSafe('.abxcyd'));
    check(not match.MatchThreadSafe('abxcyd'));
  end;
  for i := 32 to 127 do begin
    SetLength(V,1);
    V[1] := AnsiChar(i);
    isword := (tcWord in TEXT_BYTES[i]);
    Check(IsMatch('[A-Za-z0-9]',V)=isword);
    Check(IsMatch('[01-456a-zA-Z789]',V)=isword);
    SetLength(V,3);
    V[1] := AnsiChar(i);
    V[2] := AnsiChar(i);
    V[3] := AnsiChar(i);
    Check(IsMatch('[A-Za-z0-9]?[A-Za-z0-9]',V)=isword);
    Check(IsMatch('[A-Za-z0-9]*',V)=isword);
    Check(IsMatch('[a-z0-9]?[A-Z0-9]',V,true)=isword);
    Check(IsMatch('[A-Z0-9]*',V,true)=isword);
  end;
end;

procedure TTestLowLevelCommon._TExprParserMatch;
var
  s: TExprParserMatch;

  procedure Test(const expression: RawUTF8; const ok, nok: array of RawUTF8);
  var i: integer;
  begin
    Check(s.Parse(expression) = eprSuccess);
    for i := 0 to high(ok) do
      Check(s.Search(ok[i]));
    for i := 0 to high(nok) do
      Check(not s.Search(nok[i]));
  end;
  
begin
  s := TExprParserMatch.Create({casesensitive=}true);
  try // &=AND -=WITHOUT +=OR 
    check(s.Parse('') = eprNoExpression);
    check(s.Parse('  ') = eprNoExpression);
    check(s.Parse('1+ ') = eprMissingFinalWord);
    Test('1', ['1', '1 2 3', '2 1'], ['2', '13', '2 3']);
    Test('   1   ', ['1', '1 2 3', '2 1'], ['2', '13', '2 3']);
    Test('1+4', ['1', '1 2 3', '2 1', '2 4 3'], ['2', '13', '2 3', '41']);
    Test(' 1 + 4 ', ['1', '1 2 3', '2 1', '2 4 3'], ['2', '13', '2 3', '41']);
    Test('1+4+5', ['1', '1 2 3', '2 1', '2 4 3'], ['2', '13', '2 3', '41']);
    Test('1+(4+5)', ['1', '1 2 3', '2 1', '2 4 3'], ['2', '13', '2 3', '41']);
    Test('1+4*+5', ['1', '1 2 3', '2 1', '2 4 3', '41'], ['2', '13', '2 3']);
    Test('1+(4&555)', ['4 555 3', '555 4', '1', '1 2 3', '2 1'], ['2', '13', '2 3', '41', '4 3', '3 555']);
    Test('1+(4 555)', ['4 555 3', '555 4', '1', '1 2 3', '2 1'], ['2', '13', '2 3', '41', '4 3', '3 555']);
    Test('1-4', ['1', '1 2 3', '2 1', '2 1 3'], ['1 4', '4 2 1', '2', '13', '2 3', '41']);
    Test('1-(4&5)', ['1', '1 2 3', '2 1', '1 4', '1 5'],
       ['2', '5 2 3 4 1', '2 3', '41', '4 3', '3 5', '1 4 5']);
    Test('1-(4&(5+6))', ['1', '1 2 3', '2 1', '1 4', '1 5', '1 6'],
       ['2', '5 2 3 4 1', '2 3', '41', '4 3', '3 5', '1 4 5', '1 4 6']);
    Test('1 - ( 4 & ( 57 + 6 ) )', ['1', '1 2 3', '2 1', '1 4', '1 57', '1 6'],
       ['2', '57 2 3 4 1', '2 3', '41', '4 3', '3 5"7', '1 4 57', '1 4 6']);
  finally
    s.Free;
  end;
end;

procedure TTestLowLevelCommon._Random32;
var i: integer;
    c: array[0..1000] of cardinal;
begin
  for i := 0 to high(c) do
    c[i] := Random32;
  QuickSortInteger(@c,0,high(c));
  for i := 1 to high(c) do
    Check(c[i+1]<>c[i],'unique Random32');
  Check(Random32(0)=0);
  for i := 1 to 100000 do
    Check(Random32(i)<cardinal(i));
  for i := 0 to 100000 do
    Check(Random32(maxInt-i)<cardinal(maxInt-i));
end;

procedure TTestLowLevelCommon._TRawUTF8Interning;
var int: TRawUTF8Interning;
    i,v: integer;
    tmp: RawUTF8;
    vs: TRawUTF8DynArray;
    timer: TPrecisionTimer;
const MAX=500000;
      DIRSIZE = 16*(MAX+1); // assume each SmallUInt32UTF8[] uses 16 heap bytes
      INTSIZE = 512*16;
begin
  {$ifndef HASINLINE} // inlining induces optimizations which trigger Clean
  int := TRawUTF8Interning.Create(1);
  try
    check(int.Count=0);
    check(int.Unique('test')='test');
    check(int.Count=1);
    check(int.Unique('test')='test');
    check(int.Count=1);
    check(int.Clean=0);
    check(int.Unique('single')='single');
    check(int.Count=2);
    check(int.Clean=1);
    check(int.Count=1);
    check(int.Clean=0);
    check(int.Count=1);
    check(int.Unique('single1')='single1');
    check(int.Count=2);
    check(int.Unique('test2')='test2');
    check(int.Count=3);
    check(int.Unique('test2')='test2');
    check(int.Count=3);
    check(int.Unique('single2')='single2');
    check(int.Count=4);
    check(int.Clean=2);
    check(int.Count=2);
    int.Clear;
    check(int.Count=0);
    check(int.Clean=0);
    check(int.Count=0);
  finally
    int.Free;
  end;
  {$endif HASINLINE}
  int := TRawUTF8Interning.Create(16);
  try
    for i := 0 to MAX do begin
      v := i and 511;
      int.Unique(tmp,SmallUInt32UTF8[v]);
      check(UTF8ToInteger(tmp)=v);
    end;
    checkEqual(int.Count,512);
    checkEqual(int.Clean,0);
    checkEqual(int.Count,512);
  finally
    int.Free;
  end;
  int := TRawUTF8Interning.Create(4);
  try
    SetLength(vs,MAX+1);
    timer.Start;
    for i := 0 to MAX do begin
      v := i and 511;
      int.Unique(vs[i],pointer(SmallUInt32UTF8[v]),length(SmallUInt32UTF8[v]));
    end;
    NotifyTestSpeed('interning %',[KB(INTSIZE)],MAX,DIRSIZE,@timer);
    for i := 0 to MAX do
      check(UTF8ToInteger(vs[i])=i and 511);
    check(int.Count=512);
    check(int.Clean=0);
    check(int.Count=512);
    for i := 0 to MAX do
      check(UTF8ToInteger(vs[i])=i and 511);
    vs := nil;
    check(int.Count=512);
    check(int.Clean=512);
    check(int.Count=0);
  finally
    int.Free;
  end;
  SetLength(vs,MAX+1);
  timer.Start;
  for i := 0 to MAX do begin
    v := i and 511;
    FastSetString(vs[i],pointer(SmallUInt32UTF8[v]),length(SmallUInt32UTF8[v]));
  end;
  NotifyTestSpeed('direct %',[KB(DIRSIZE)],MAX,DIRSIZE,@timer);
  for i := 0 to MAX do
    check(UTF8ToInteger(vs[i])=i and 511);
end;

function kr32reference(buf: PAnsiChar; len: cardinal): cardinal;
var i: integer;
begin
  result := 0;
  for i := 0 to len-1 do
    result := result*31+ord(buf[i]);
end;

function fnv32reference(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
var i: integer;
begin
  for i := 0 to len-1 do
    crc := (crc xor ord(buf[i]))*16777619;
  result := crc;
end;

function crc32creference(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
begin
  result := not crc;
  if buf<>nil then
    while len>0 do begin
      result := crc32ctab[0,byte(result xor ord(buf^))] xor (result shr 8);
      dec(len);
      inc(buf);
    end;
  result := not result;
end;

function Hash32Reference(Data: PCardinal; Len: integer): cardinal;
var s1,s2: cardinal;
    i: integer;
begin
  if Data<>nil then begin
    s1 := 0;
    s2 := 0;
    for i := 1 to Len shr 2 do begin // 4 bytes (DWORD) by loop
      inc(s1,Data^);
      inc(s2,s1);
      inc(Data);
    end;
    case Len and 3 of // remaining 0..3 bytes
    1: inc(s1,PByte(Data)^);
    2: inc(s1,PWord(Data)^);
    3: inc(s1,PWord(Data)^ or (PByteArray(Data)^[2] shl 16));
    end;
    inc(s2,s1);
    result := s1 xor (s2 shl 16);
  end else
    result := 0;
end;

{$ifndef FPC} // RolDWord is an intrinsic function under FPC :)
function RolDWord(value: cardinal; count: integer): cardinal;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := (value shl count) or (value shr (32-count));
end;
{$endif FPC}

function xxHash32reference(P: PAnsiChar; len: integer; seed: cardinal = 0): cardinal;
const
  PRIME32_1 = 2654435761;
  PRIME32_2 = 2246822519;
  PRIME32_3 = 3266489917;
  PRIME32_4 = 668265263;
  PRIME32_5 = 374761393;
var c1, c2, c3, c4: cardinal;
    PLimit, PEnd: PAnsiChar;
begin
  PEnd := P + len;
  if len >= 16 then
    begin
      PLimit := PEnd - 16;
      c1 := seed + PRIME32_1 + PRIME32_2;
      c2 := seed + PRIME32_2;
      c3 := seed;
      c4 := seed - PRIME32_1;
      repeat
        c1 := PRIME32_1 * RolDWord(c1 + PRIME32_2 * PCardinal(P)^, 13);
        c2 := PRIME32_1 * RolDWord(c2 + PRIME32_2 * PCardinal(P+4)^, 13);
        c3 := PRIME32_1 * RolDWord(c3 + PRIME32_2 * PCardinal(P+8)^, 13);
        c4 := PRIME32_1 * RolDWord(c4 + PRIME32_2 * PCardinal(P+12)^, 13);
        inc(P, 16);
      until not (P <= PLimit);
      result := RolDWord(c1, 1) + RolDWord(c2, 7) + RolDWord(c3, 12) + RolDWord(c4, 18);
    end else
    result := seed + PRIME32_5;
  inc(result, len);
  while P <= PEnd - 4 do begin
    inc(result, PCardinal(P)^ * PRIME32_3);
    result := RolDWord(result, 17) * PRIME32_4;
    inc(P, 4);
  end;
  while P < PEnd do begin
    inc(result, PByte(P)^ * PRIME32_5);
    result := RolDWord(result, 11) * PRIME32_1;
    inc(P);
  end;
  result := result xor (result shr 15);
  result := result * PRIME32_2;
  result := result xor (result shr 13);
  result := result * PRIME32_3;
  result := result xor (result shr 16);
end;

procedure crcblockreference(crc128, data128: PBlock128);
var c: cardinal;
begin
  c := crc128^[0] xor data128^[0];
  crc128^[0] := crc32ctab[3,byte(c)] xor crc32ctab[2,byte(c shr 8)]
            xor crc32ctab[1,byte(c shr 16)] xor crc32ctab[0,c shr 24];
  c := crc128^[1] xor data128^[1];
  crc128^[1] := crc32ctab[3,byte(c)] xor crc32ctab[2,byte(c shr 8)]
            xor crc32ctab[1,byte(c shr 16)] xor crc32ctab[0,c shr 24];
  c := crc128^[2] xor data128^[2];
  crc128^[2] := crc32ctab[3,byte(c)] xor crc32ctab[2,byte(c shr 8)]
            xor crc32ctab[1,byte(c shr 16)] xor crc32ctab[0,c shr 24];
  c := crc128^[3] xor data128^[3];
  crc128^[3] := crc32ctab[3,byte(c)] xor crc32ctab[2,byte(c shr 8)]
            xor crc32ctab[1,byte(c shr 16)] xor crc32ctab[0,c shr 24];
end;

procedure TTestLowLevelCommon._crc32c;
var crc: array[0..10000] of record
      s: RawByteString;
      crc: cardinal;
    end;
    totallen: Cardinal;
    s2: RawByteString;
procedure Test(hash: THasher; const name: string);
var i: Integer;
    Timer: TPrecisionTimer;
    a: string[10];
begin
  Timer.Start;
  a := '123456789';
  Check(hash(0,@a,0)=0);
  Check(hash(0,@a,1)=$2ACF889D);
  Check(hash(0,@a,2)=$BD5FE6AF);
  Check(hash(0,@a,3)=$7F40BC73);
  Check(hash(0,@a,4)=$13790E51);
  Check(crc32cBy4(cardinal(not 0),PCardinal(@a)^)=cardinal(not $13790E51),'crc32cBy4');
  Check(hash(0,@a,5)=$659AD21);
  Check(hash(0,@a,6)=$85BF5A8C);
  Check(hash(0,@a,7)=$8B0FB6FA);
  Check(hash(0,@a,8)=$2E5336F0);
  for i := 0 to High(crc) do
    with crc[i] do
      Check(hash(0,pointer(s),length(s))=crc);
  fRunConsole := format('%s %s %s/s',[fRunConsole,name,KB(Timer.PerSec(totallen))]);
end;
procedure test16(const text: RawUTF8; expected: cardinal);
begin
  Check(crc16(pointer(text),length(text))=expected);
end;
var i,j: integer;
    Timer: TPrecisionTimer;
    c1,c2: cardinal;
    crc1,crc2: THash128;
    crcs: THash512Rec;
    digest: THash256;
    tmp: RawByteString;
    hmac32: THMAC_CRC32C;
//    hmac256: THMAC_CRC256C;
begin
  test16('',$ffff);
  test16('a',$9d77);
  test16('ab',$69f0);
  test16('toto',$e2ca);
  test16('123456789',$29b1);
  test16('123456789123456789',$a86d);
  totallen := 36;
  tmp := '123456789123456789';
  c2 := $12345678;
  c1 := HMAC_CRC32C(@c2,pointer(tmp),4,length(tmp));
  check(c1=$1C3C4B51);
  hmac32.Init(@c2,4);
  hmac32.Update(pointer(tmp),length(tmp));
  check(hmac32.Done=c1);
  c2 := $12345678;
  HMAC_CRC256C(@c2,pointer(tmp),4,length(tmp),digest);
  check(SHA256DigestToString(digest)='46da01fb9f4a97b5f8ba2c70512bc22aa'+
    'a9b57e5030ced9f5c7c825ab5ec1715');
  FillZero(crc2);
  crcblock(@crc2,PBlock128(PAnsiChar('0123456789012345')));
  check(not IsZero(crc2));
  check(TBlock128(crc2)[0]=1314793854);
  check(TBlock128(crc2)[1]=582109780);
  check(TBlock128(crc2)[2]=1177891908);
  check(TBlock128(crc2)[3]=4047040040);
  FillZero(crc1);
  crcblockreference(@crc1,PBlock128(PAnsiChar('0123456789012345')));
  check(not IsZero(crc1));
  check(IsEqual(crc1,crc2));
  FillZero(crc1);
  crcblocks(@crc1,PBlock128(PAnsiChar('0123456789012345')),1);
  check(not IsZero(crc1));
  check(IsEqual(crc1,crc2),'crcblocks');
  {$ifdef CPUINTEL}
  FillZero(crc1);
  crcblockNoSSE42(@crc1,PBlock128(PAnsiChar('0123456789012345')));
  check(not IsZero(crc1));
  check(IsEqual(crc1,crc2));
  {$endif}
  for i := 0 to high(crcs.b) do
    crcs.b[i] := i;
  for j := 1 to 4 do begin
    FillZero(crc2);
    crcblockreference(@crc2,@crcs.h0);
    if j>1 then crcblockreference(@crc2,@crcs.h1);
    if j>2 then crcblockreference(@crc2,@crcs.h2);
    if j>3 then crcblockreference(@crc2,@crcs.h3);
    FillZero(crc1);
    crcblocks(@crc1,@crcs.h0,j);
    check(not IsZero(crc1));
    check(IsEqual(crc1,crc2),'crcblocks4');
    FillZero(crc1);
    crcblocksfast(@crc1,@crcs.h0,j);
    check(not IsZero(crc1));
    check(IsEqual(crc1,crc2),'crcblocksfast4');
    CheckEqual(Hash128Index(@crcs,4,@crcs.r[j-1]),j-1);
    check(Hash128Index(@crcs,j-1,@crcs.r[j-1])<0);
  end;
  CheckEqual(Hash256Index(@crcs,2,@crcs.r[0]),0);
  check(Hash256Index(@crcs,2,@crcs.r[1])<0);
  CheckEqual(Hash256Index(@crcs,2,@crcs.r[2]),1);
  check(Hash256Index(@crcs,2,@crcs.r[3])<0);
  for i := 0 to 50000 do begin
    FillZero(crc1);
    crcblock(@crc1,@digest);
    check(not IsZero(crc1));
    {$ifdef CPUINTEL}
    FillZero(crc2);
    crcblockreference(@crc2,@digest);
    check(not IsZero(crc2));
    check(IsEqual(crc1,crc2));
    FillZero(crc2);
    crcblockNoSSE42(@crc2,@digest);
    check(not IsZero(crc2));
    check(IsEqual(crc1,crc2));
    {$endif}
    for j := 0 to high(digest) do
      inc(digest[j]);
  end;
  for i := 0 to High(crc) do
  with crc[i] do begin
    j := i shr 3+1; // circumvent weird FPC code generation bug in -O2 mode
    s := RandomString(j);
    crc := crc32creference(0,pointer(s),length(s));
    inc(totallen,length(s));
    c2 := HMAC_CRC32C(@c1,pointer(s),4,length(s));
    hmac32.Init(@c1,4);
    hmac32.Update(pointer(s),length(s));
    check(hmac32.Done=c2);
    s2 := s;
    SymmetricEncrypt(i, s2);
    check(s2 <> s);
    SymmetricEncrypt(i, s2);
    check(s2 = s);
  end;
  Test(crc32creference,'pas');
  Test(crc32cfast,'fast');
  {$ifdef CPUINTEL}
  {$ifndef Darwin}
  // Not [yet] working on Darwin
  if cfSSE42 in CpuFeatures then
    Test(crc32csse42,'sse42');
  {$endif}
  {$ifdef CPUX64}
  if (cfSSE42 in CpuFeatures) and (cfAesNi in CpuFeatures) then
    Test(crc32c,'sse42+aesni'); // use SSE4.2+pclmulqdq instructions on x64
  {$endif}
  {$endif}
  exit; // code below is speed informative only, without any test
  Timer.Start;
  for i := 0 to high(crc) do
    with crc[i] do
      fnv32(0,pointer(s),length(s));
  fRunConsole := format('%s fnv32 %s %s/s',[fRunConsole,Timer.Stop,
    KB(Timer.PerSec(totallen))]);
end;

procedure TTestLowLevelCommon.intadd(const Sender; Value: integer);
begin
  AddToCSV(UInt32ToUtf8(Value),fAdd);
end;

procedure TTestLowLevelCommon.intdel(const Sender; Value: integer);
begin
  AddToCSV(UInt32ToUtf8(Value),fDel);
end;

procedure TTestLowLevelCommon.Integers;
  procedure changes(const old,new,added,deleted: RawUTF8);
  var o,n: TIntegerDynArray;
  begin
    CSVToIntegerDynArray(Pointer(old),o);
    CSVToIntegerDynArray(Pointer(new),n);
    fAdd := '';
    fDel := '';
    NotifySortedIntegerChanges(pointer(o),pointer(n),length(o),length(n),intadd,intdel,self);
    Check(fAdd = added, 'added');
    Check(fDel = deleted, 'deleted');
  end;
  procedure includes(const values, includes, excludes, included, excluded: RawUTF8);
    procedure includes32;
    var v, i, e: TIntegerDynArray;
    begin
      CSVToIntegerDynArray(Pointer(values),v);
      CSVToIntegerDynArray(Pointer(excludes),e);
      ExcludeInteger(v, e, 32); // no sort
      Check(IntegerDynArrayToCSV(v) = excluded);
      v := nil;
      e := nil;
      CSVToIntegerDynArray(Pointer(values),v);
      CSVToIntegerDynArray(Pointer(excludes),e);
      ExcludeInteger(v, e, 2); // sort
      Check(IntegerDynArrayToCSV(v) = excluded);
      v := nil;
      e := nil;
      CSVToIntegerDynArray(Pointer(values),v);
      CSVToIntegerDynArray(Pointer(includes),i);
      IncludeInteger(v, i, 32); // no sort
      Check(IntegerDynArrayToCSV(v) = included);
      v := nil;
      e := nil;
      CSVToIntegerDynArray(Pointer(values),v);
      CSVToIntegerDynArray(Pointer(includes),i);
      IncludeInteger(v, i, 2); // sort
      Check(IntegerDynArrayToCSV(v) = included);
    end;
    procedure includes64;
    var v, i, e: TInt64DynArray;
    begin
      CSVToInt64DynArray(Pointer(values),v);
      CSVToInt64DynArray(Pointer(excludes),e);
      ExcludeInt64(v, e, 32); // no sort
      Check(Int64DynArrayToCSV(v) = excluded);
      v := nil;
      e := nil;
      CSVToInt64DynArray(Pointer(values),v);
      CSVToInt64DynArray(Pointer(excludes),e);
      ExcludeInt64(v, e, 2); // sort
      Check(Int64DynArrayToCSV(v) = excluded);
      v := nil;
      e := nil;
      CSVToInt64DynArray(Pointer(values),v);
      CSVToInt64DynArray(Pointer(includes),i);
      IncludeInt64(v, i, 32); // no sort
      Check(Int64DynArrayToCSV(v) = included);
      v := nil;
      e := nil;
      CSVToInt64DynArray(Pointer(values),v);
      CSVToInt64DynArray(Pointer(includes),i);
      IncludeInt64(v, i, 2); // sort
      Check(Int64DynArrayToCSV(v) = included);
    end;
  begin
    Includes32;
    Includes64;
  end;
var i32: TIntegerDynArray;
    i64: TInt64DynArray;
    i,n: integer;
begin
  check(i32=nil);
  DeduplicateInteger(i32);
  check(i32=nil);
  SetLength(i32,2);
  i32[0] := 1;
  QuickSortInteger(i32);
  check(i32[0]=0);
  check(i32[1]=1);
  DeduplicateInteger(i32);
  check(length(i32)=2);
  check(i32[0]=0);
  check(i32[1]=1);
  i32[0] := 1;
  DeduplicateInteger(i32);
  check(length(i32)=1);
  check(i32[0]=1);
  SetLength(i32,6);
  i32[4] := 1;
  i32[5] := 2;
  DeduplicateInteger(i32); // (1, 0, 0, 0, 1, 2)
  check(length(i32)=3);
  check(i32[0]=0);
  check(i32[1]=1);
  check(i32[2]=2);
  SetLength(i32,6);
  i32[4] := 3;
  i32[5] := 3;
  DeduplicateInteger(i32); // (0, 1, 2, 0, 3, 3)
  check(length(i32)=4);
  check(i32[0]=0);
  check(i32[1]=1);
  check(i32[2]=2);
  check(i32[3]=3);
  for n := 1 to 1000 do begin
    SetLength(i32,n);
    for i := 0 to n - 1 do
      i32[i] := i and 15;
    DeduplicateInteger(i32);
    if n < 16 then
      check(Length(i32) = n) else
      check(Length(i32) = 16);
    for i := 0 to high(i32) do
      check(i32[i] = i);
  end;
  changes('','','','');
  changes('1','1','','');
  changes('','1','1','');
  changes('1','','','1');
  changes('1,2','1,3','3','2');
  changes('2','1,3','1,3','2');
  changes('','1,3','1,3','');
  changes('1,2,3,4','1,2,3,4','','');
  changes('1,2,3,4','1,2,3,4,5','5','');
  changes('1,2,3,4','1,3,4','','2');
  changes('1,2,3,4','3,4','','1,2');
  changes('1,2,3,4','1,4','','2,3');
  changes('1,2,3,4','','','1,2,3,4');
  changes('1,2,3,4','5,6','5,6','1,2,3,4');
  changes('1,2,4','1,3,5,6','3,5,6','2,4');
  changes('1,2,4','3,5,6','3,5,6','1,2,4');
  includes('1,2,3', '2', '2', '2', '1,3');
  includes('1,2,3', '2,3', '2,3', '2,3', '1');
  includes('1,2,3', '1,2,3', '1,2,3', '1,2,3', '');
  includes('1,2,3', '3,1,2', '3,1,2', '1,2,3', '');
  check(i64=nil);
  DeduplicateInt64(i64);
  check(i64=nil);
  SetLength(i64,2);
  i64[0] := 1;
  QuickSortInt64(pointer(i64),0,1);
  check(i64[0]=0);
  check(i64[1]=1);
  DeduplicateInt64(i64);
  check(length(i64)=2);
  check(i64[0]=0);
  check(i64[1]=1);
  i64[0] := 1;
  DeduplicateInt64(i64);
  check(length(i64)=1);
  check(i64[0]=1);
  SetLength(i64,6);
  i64[4] := 1;
  i64[5] := 2;
  DeduplicateInt64(i64); // (1, 0, 0, 0, 1, 2)
  check(length(i64)=3);
  check(i64[0]=0);
  check(i64[1]=1);
  check(i64[2]=2);
  SetLength(i64,6);
  i64[4] := 3;
  i64[5] := 3;
  DeduplicateInt64(i64); // (0, 1, 2, 0, 3, 3)
  check(length(i64)=4);
  check(i64[0]=0);
  check(i64[1]=1);
  check(i64[2]=2);
  check(i64[3]=3);
  for n := 1 to 1000 do begin
    SetLength(i64,n);
    for i := 0 to n - 1 do
      i64[i] := i and 15;
    DeduplicateInt64(i64);
    if n < 16 then
      check(Length(i64) = n) else
      check(Length(i64) = 16);
    for i := 0 to high(i64) do
      check(i64[i] = i);
  end;
end;

function TestAddFloatStr(const str: RawUTF8): RawUTF8;
var tmp: TTextWriterStackBuffer;
begin
  with TTextWriter.CreateOwnedStream(tmp) do
    try
      AddFloatStr(pointer(str));
      SetText(result);
    finally
      Free;
    end;
end;

procedure TTestLowLevelCommon.NumericalConversions;

  procedure CheckDoubleToShort(v: double; const expected: ShortString);
  var a: ShortString;
      d: double;
      err: integer;
  begin
    ExtendedToShort(a,v,DOUBLE_PRECISION);
    CheckEqual(a,expected,'ExtendedToShort');
    DoubleToShort(a,v);
    CheckEqual(a,expected,'DoubleToShort');
    a[ord(a[0])+1] := #0;
    d := GetExtended(@a[1],err);
    CheckEqual(err,0);
    CheckSame(v,d);
  end;

  procedure CheckDoubleToShortSame(v: double);
  var s: string;
      u: RawUTF8;
      err: integer;
      d: double;
  begin
    s := DoubleToString(v);
    val(s,d,err);
    Check(err=0);
    CheckSame(d,v);
    StringToUTF8(s,u);
    d := GetExtended(pointer(u),err);
    Check(err=0);
    CheckSame(d,v);
  end;

var i, j, b, err: integer;
    juint: cardinal absolute j;
    k,l: Int64;
    q: QWord;
    s,s2: RawUTF8;
    d,e: double;
    f: extended;
    sd,se: single;
    {$ifndef DELPHI5OROLDER}
    c: currency;
    ident: TRawUTF8DynArray;
    {$endif}
    {$ifndef NOVARIANTS}
    vj, vs: variant;
    {$endif}
    a,a2: shortstring;
    u: string;
    varint: array[0..255] of byte;
    st: TFastReader;
    PB,PC: PByte;
    P: PUTF8Char;
    crc, u32, n: cardinal;
    Timer: TPrecisionTimer;
begin
  n := 100000;
  Timer.Start;
  crc := 0;
  d := 3.141592653 / 1.0573623912;
  for i := 1 to n do begin
    f  := d;
    j := FloatToText(PChar(@varint),f,{$ifndef FPC}fvExtended,{$endif}
      ffGeneral,DOUBLE_PRECISION,0);
    PChar(@varint)[j] := #0;
    inc(crc,j);
    d := d * 1.0038265263;
  end;
  NotifyTestSpeed('FloatToText ', [Pchar(@varint)], n, crc, @timer);
  Timer.Start;
  crc := 0;
  d := 3.141592653 / 1.0573623912;
  for i := 1 to n do begin
    Str(d,a);
    inc(crc,ord(a[0]));
    d := d * 1.0038265263;
  end;
  NotifyTestSpeed('str ', [a], n, crc, @timer);
  //  a[ord(a[0])+1] := #0; Check(SameValue(GetExtended(pointer(@a[1])),d,0));
  Timer.Start;
  crc := 0;
  d := 3.141592653 / 1.0573623912;
  for i := 1 to n do begin
    DoubleToShort(a,d);
    inc(crc,ord(a[0]));
    d := d * 1.0038265263;
  end;
  NotifyTestSpeed('DoubleToShort ', [a], n, crc, @timer);
  a[ord(a[0])+1] := #0;
  //  a[ord(a[0])+1] := #0; Check(SameValue(GetExtended(pointer(@a[1])),d,0));
  {$ifdef DOUBLETOSHORT_USEGRISU}
  Timer.Start;
  crc := 0;
  d := 3.141592653 / 1.0573623912;
  for i := 1 to n do begin
    DoubleToAscii(C_NO_MIN_WIDTH,-1,d,@a);
    inc(crc,ord(a[0]));
    d := d * 1.0038265263;
  end;
  NotifyTestSpeed('DoubleToAscii ', [a], n, crc, @timer);
  //  a[ord(a[0])+1] := #0; Check(SameValue(GetExtended(pointer(@a[1])),d,0));
  d := 0;
  DoubleToAscii(C_NO_MIN_WIDTH,-1,d,@a);
  Check(a='0');
  DoubleToAscii(0,DOUBLE_PRECISION,d,@a);
  Check(a='0');
  {$endif DOUBLETOSHORT_USEGRISU}
  CheckEqual(TestAddFloatStr(''),'0');
  CheckEqual(TestAddFloatStr(' 123'),'123');
  CheckEqual(TestAddFloatStr(' 1a23'),'1');
  CheckEqual(TestAddFloatStr(' 123z'),'123');
  CheckEqual(TestAddFloatStr(' 12.3'),'12.3');
  CheckEqual(TestAddFloatStr('12.'),'12.');
  CheckEqual(TestAddFloatStr(' +12.3'),'+12.3');
  CheckEqual(TestAddFloatStr(' -12.3'),'-12.3');
  CheckEqual(TestAddFloatStr('12.3e230'),'12.3e230');
  CheckEqual(TestAddFloatStr('12.3E230'),'12.3E230');
  CheckEqual(TestAddFloatStr('12.3e-230'),'12.3e-230');
  CheckEqual(TestAddFloatStr('12.3E-230'),'12.3E-230');
  CheckEqual(TestAddFloatStr('12.3e 230'),'12.3e');
  CheckEqual(TestAddFloatStr('12.3f230'),'12.3');
  CheckEqual(TestAddFloatStr('12.3E23.0'),'12.3E23');
  CheckEqual(TestAddFloatStr('-.01'),'-0.01'); // ODBC numeric output
  CheckEqual(TestAddFloatStr('.0002'),'0.0002'); // ODBC numeric output
  CheckEqual(OctToBin(''),'');
  CheckEqual(OctToBin('123'),'123');
  CheckEqual(OctToBin('\\123'),'\123');
  CheckEqual(OctToBin('12\\3'),'12\3');
  CheckEqual(OctToBin('123\\'),'123\');
  CheckEqual(OctToBin('123\'),'123');
  CheckEqual(OctToBin('\041'),'!');
  CheckEqual(OctToBin('a\041'),'a!');
  CheckEqual(OctToBin('\041b'),'!b');
  CheckEqual(OctToBin('a\041b'),'a!b');
  CheckEqual(OctToBin('a\101b\102'),'aAbB');
  CheckEqual(OctToBin('a\101\102b'),'aABb');
  CheckEqual(OctToBin('a\101\\\102b'),'aA\Bb');
  CheckEqual(OctToBin('a\401b\102'),'a');
  CheckEqual(OctToBin('a\181b\102'),'a');
  CheckEqual(OctToBin('a\10ab\102'),'a');
  CheckEqual(OctToBin('a\1'),'a');
  CheckEqual(OctToBin('a\10'),'a');
  Check(Plural('row',0)='0 row');
  Check(Plural('row',1)='1 row');
  Check(Plural('row',2)='2 rows');
  Check(Plural('row',20)='20 rows');
  Check(Plural('row',200000)='200000 rows');
  Check(not SameValue(386.0, 386.1));
  Check(not SameValue(386.0, 700, 2));
  Check(IntToThousandString(0)='0');
  Check(IntToThousandString(1)='1');
  Check(IntToThousandString(10)='10');
  Check(IntToThousandString(100)='100');
  Check(IntToThousandString(1000)='1,000');
  Check(IntToThousandString(10000)='10,000');
  Check(IntToThousandString(100000)='100,000');
  Check(IntToThousandString(1000000)='1,000,000');
  Check(IntToThousandString(-1)='-1');
  Check(IntToThousandString(-10)='-10');
  Check(IntToThousandString(-100)='-100');
  Check(IntToThousandString(-1000)='-1,000');
  Check(IntToThousandString(-10000)='-10,000');
  Check(IntToThousandString(-100000)='-100,000');
  Check(IntToThousandString(-1000000)='-1,000,000');
  Check(UInt3DigitsToUTF8(1)='001');
  Check(UInt3DigitsToUTF8(12)='012');
  Check(UInt3DigitsToUTF8(123)='123');
  Check(UInt4DigitsToUTF8(1)='0001');
  Check(UInt4DigitsToUTF8(12)='0012');
  Check(UInt4DigitsToUTF8(123)='0123');
  Check(UInt4DigitsToUTF8(1234)='1234');
  Check(MicroSecToString(0)='0us');
  Check(MicroSecToString(QWord(-10))='0us');
  Check(MicroSecToString(10)='10us');
  Check(MicroSecToString(999)='999us');
  Check(MicroSecToString(1000)='1ms');
  Check(MicroSecToString(1001)='1ms');
  Check(MicroSecToString(1010)='1.01ms');
  Check(MicroSecToString(1100)='1.10ms');
  Check(MicroSecToString(999999)='999.99ms');
  Check(MicroSecToString(1000000)='1s');
  Check(MicroSecToString(1000001)='1s');
  Check(MicroSecToString(2030001)='2.03s');
  Check(MicroSecToString(200000070001)='2d');
  Check(KB(-123)='-123 B');
  Check(KB(0)='0 B');
  Check(KB(123)='123 B');
  Check(KB(1023)='1 KB');
  Check(KB(1024)='1 KB');
  Check(KB(1025)='1 KB');
  Check(KB(16383)='16 KB');
  Check(KB(16384)='16 KB');
  Check(KB(16385)='16 KB');
  Check(KB(3*1024*1024-800*1024)='2.2 MB');
  Check(KB(3*1024*1024)='3 MB');
  Check(KB(3*1024*1024+512*1024)='3.5 MB');
  Check(KB(3*1024*1024+1024)='3 MB');
  Check(KB(maxInt)='2 GB');
  Check(KB(3294963200)='3 GB');
  Check(KB(4294963200)='4 GB');
  Check(Int64ToUtf8(-maxInt)='-2147483647');
  Check(Int64ToUtf8(-1)='-1');
  Check(Int64ToUtf8(-9223372036854775807)='-9223372036854775807');
  Int64ToUtf8(-maxInt,s);
  Check(s='-2147483647');
  Int64ToUtf8(-1,s);
  Check(s='-1');
  Int64ToUtf8(100,s);
  Check(s='100');
  Int64ToUtf8(-9223372036854775807,s);
  Check(s='-9223372036854775807');
  {$ifdef HASINLINE} // bug with MinInt64 with older versions of Delphi
  Check(Int64ToUtf8(-9223372036854775808)='-9223372036854775808');
  Int64ToUtf8(-9223372036854775808,s);
  Check(s='-9223372036854775808');
  {$endif}
  Check(Int64ToUTF8(2119852951849248647)='2119852951849248647');
  Check(FormatUTF8(' % ',[2119852951849248647])=' 2119852951849248647 ');
  s := '1234';
  d := GetExtended(pointer(s));
  CheckSame(d,1234);
  s := '1234.1';
  d := GetExtended(pointer(s));
  CheckSame(d,1234.1);
  s := '12345678901234567890';
  d := GetExtended(pointer(s));
  CheckSame(d,12345678901234567890.0,0);
  s := '1234.1234567890123456789';
  d := GetExtended(pointer(s));
  CheckSame(d,1234.1234567890123456789);
  s := '.1234';
  d := GetExtended(pointer(s));
  CheckSame(d,0.1234);
  s := '.1234e';
  d := GetExtended(pointer(s),err);
  Check(err<>0);
  s := '.1234e4';
  d := GetExtended(pointer(s),err);
  Check(err=0);
  CheckSame(d,1234);
  u := DoubleToString(40640.5028819444);
  Check(u='40640.5028819444',u);
  s := '40640.5028a819444';
  GetExtended(pointer(s),err);
  Check(err>0);
  s := '40640.5028819444';
  d := GetExtended(pointer(s),err);
  Check(err=0);
  u := DoubleToString(d);
  Check(u='40640.5028819444',u);
  e := 40640.5028819444;
  CheckSame(d,e,1e-11);
  Check(IsAnsiCompatible('t'));
  Check(IsAnsiCompatible('te'));
  Check(IsAnsiCompatible('tes'));
  Check(IsAnsiCompatible('test'));
  Check(IsAnsiCompatible('teste'));
  CheckDoubleToShort(0,'0');
  CheckDoubleToShort(1,'1');
  CheckDoubleToShort(-1,'-1');
  CheckDoubleToShort(0.1,'0.1');
  CheckDoubleToShort(0.01,'0.01');
  CheckDoubleToShort(0.001,'0.001');
  CheckDoubleToShort(0.0001,'0.0001');
  CheckDoubleToShort(-0.1,'-0.1');
  CheckDoubleToShort(-0.01,'-0.01');
  CheckDoubleToShort(-0.001,'-0.001');
  CheckDoubleToShort(-0.0001,'-0.0001');
  CheckDoubleToShort(1.1,'1.1');
  CheckDoubleToShort(1.01,'1.01');
  CheckDoubleToShort(1.001,'1.001');
  CheckDoubleToShort(1.0001,'1.0001');
  CheckDoubleToShort(1.00001,'1.00001');
  CheckDoubleToShort(-1.1,'-1.1');
  CheckDoubleToShort(-1.01,'-1.01');
  CheckDoubleToShort(-1.001,'-1.001');
  CheckDoubleToShort(-1.0001,'-1.0001');
  CheckDoubleToShort(-1.00001,'-1.00001');
  CheckDoubleToShort(7,'7');
  CheckDoubleToShort(-7,'-7');
  CheckDoubleToShort(0.7,'0.7');
  CheckDoubleToShort(0.07,'0.07');
  CheckDoubleToShort(0.007,'0.007');
  CheckDoubleToShort(0.0007,'0.0007');
  CheckDoubleToShort(-0.7,'-0.7');
  CheckDoubleToShort(-0.07,'-0.07');
  CheckDoubleToShort(-0.007,'-0.007');
  CheckDoubleToShort(-0.0007,'-0.0007');
  CheckDoubleToShort(7.7,'7.7');
  CheckDoubleToShort(7.07,'7.07');
  CheckDoubleToShort(7.007,'7.007');
  CheckDoubleToShort(7.0007,'7.0007');
  CheckDoubleToShort(7.00007,'7.00007');
  CheckDoubleToShort(-7.7,'-7.7');
  CheckDoubleToShort(-7.07,'-7.07');
  CheckDoubleToShort(-7.007,'-7.007');
  CheckDoubleToShort(-7.0007,'-7.0007');
  CheckDoubleToShort(-7.00007,'-7.00007');
  {$ifdef FPC}
  CheckDoubleToShort(0.00001,'0.00001');
  CheckDoubleToShort(-0.00001,'-0.00001');
  CheckDoubleToShort(0.00007,'0.00007');
  CheckDoubleToShort(-0.00007,'-0.00007');
  {$endif FPC}
  CheckDoubleToShort(11111.1,'11111.1');
  CheckDoubleToShort(11111.01,'11111.01');
  CheckDoubleToShort(11111.001,'11111.001');
  CheckDoubleToShort(11111.0001,'11111.0001');
  CheckDoubleToShort(11111.00001,'11111.00001');
  CheckDoubleToShort(-11111.1,'-11111.1');
  CheckDoubleToShort(-11111.01,'-11111.01');
  CheckDoubleToShort(-11111.001,'-11111.001');
  CheckDoubleToShort(-11111.0001,'-11111.0001');
  CheckDoubleToShort(-11111.00001,'-11111.00001');
  CheckDoubleToShort(0.9999999999999997,'1');
  CheckDoubleToShort(-0.9999999999999997,'-1');
  CheckDoubleToShort(9.999999999999997,'10');
  CheckDoubleToShort(-9.999999999999997,'-10');
  CheckDoubleToShort(999.9999999999997,'1000');
  CheckDoubleToShort(-999.9999999999997,'-1000');
  CheckDoubleToShort(22.99999999999997,'23');
  CheckDoubleToShort(-22.99999999999997,'-23');
  CheckDoubleToShort(999.9999999999933,'999.999999999993');
  CheckDoubleToShort(-999.9999999999933,'-999.999999999993');
  CheckDoubleToShortSame(3.3495117168);
  CheckDoubleToShortSame(-3.3495117168);
  CheckDoubleToShortSame(-3.3495117168e-1);
  CheckDoubleToShortSame(3.3495117168e-1);
  CheckDoubleToShortSame(-3.3495117168e-5);
  CheckDoubleToShortSame(3.3495117168e-5);
  CheckDoubleToShortSame(-3.3495117168e-10);
  CheckDoubleToShortSame(3.3495117168e-10);
  CheckDoubleToShortSame(-3.9999617168e-14);
  CheckDoubleToShortSame(3.9999617168e-14);
  CheckDoubleToShortSame(-3.9999617168e-15);
  CheckDoubleToShortSame(3.9999617168e-15);
  CheckDoubleToShortSame(12.345678901234);
  CheckDoubleToShortSame(123.45678901234);
  CheckDoubleToShortSame(1234.5678901234);
  Check(Int32ToUtf8(1599638299)='1599638299');
  Check(UInt32ToUtf8(1599638299)='1599638299');
  Check(Int32ToUtf8(-1599638299)='-1599638299');
  Check(Int64ToUTF8(-1271083787498396012)='-1271083787498396012');
  {$ifdef FPC} // Delphi doesn't handle correctly such huge constants
  CheckDoubleToShort(1234567890123456789,'1.2345678901234568E18');
  CheckDoubleToShortSame(1234567890123456789);
  {$endif}
  s := Int64ToUTF8(242161819595454762);
  Check(s='242161819595454762');
  {$ifndef DELPHI5OROLDER}
  Check(ScanUTF8('1 2 3','  %',[@i,@j,@d])=0);
  Check(ScanUTF8('','%d%d%f',[@i,@j,@d])=0);
  Check(ScanUTF8('1 2 7','%d%d%f',[@i,@j,@d])=3);
  Check(i=1);
  Check(j=2);
  Check(d=7);
  Check(ScanUTF8('2/3/8.1','%d/%d/%f',[@i,@j,@d])=3);
  Check(i=2);
  Check(j=3);
  CheckSame(d,8.1);
  Check(ScanUTF8('5 / 6/3','%d/%d / %f',[@i,@j,@d])=3);
  Check(i=5);
  Check(j=6);
  Check(d=3);
  Check(ScanUTF8('15 25 35','%d%D',[@i,@k,@d])=2);
  Check(i=15);
  Check(k=25);
  Check(d=3);
  Check(ScanUTF8('1 21 35','%d%d%f',[@i,@j])=2);
  Check(i=1);
  Check(j=21);
  Check(d=3);
  Check(ScanUTF8(' 10  20  abc  ','%d%d%s',[@i,@j,@a])=3);
  Check(i=10);
  Check(j=20);
  Check(a='abc');
  Check(ScanUTF8('1 00000002 3.01234 ','%dtoto %x%Ftiti',[@i,@j,@c])=3);
  Check(i=1);
  Check(j=2);
  Check(c=3.0123);
  Check(ScanUTF8('10 0000000a 77.77 7','%dtoto %x%Ftiti%Uboat',[@i,@j,@c,@crc],@ident)=4);
  Check(i=10);
  Check(j=10);
  Check(c=77.77);
  Check(crc=7);
  Check(Length(ident)=4);
  Check(ident[0]='dtoto');
  Check(ident[1]='x');
  Check(ident[2]='Ftiti');
  Check(ident[3]='Uboat');
  {$endif}
  Check(xxHash32(0,'A',1)=275094093);
  Check(xxHash32(0,'ABACK',5)=314231639);
  Check(xxHash32(0,'ABBREVIATIONS',13)=3058487595);
  Check(xxHash32(0,'LORD',4)=3395586315);
  Check(xxHash32(0,'MICROINSTRUCTION''S',18)=1576115228);
  for i := -10000 to 10000 do
    Check(GetInteger(Pointer(Int32ToUtf8(i)))=i);
  for i := 0 to 10000 do begin
    j := i shr 6; // circumvent weird FPC code generation bug in -O2 mode
    s := RandomString(j);
    Check(hash32(s)=Hash32Reference(pointer(s),length(s)));
    Check(kr32(0,pointer(s),length(s))=kr32reference(pointer(s),length(s)));
    Check(fnv32(0,pointer(s),length(s))=fnv32reference(0,pointer(s),length(s)));
    crc := crc32creference(0,pointer(s),length(s));
    Check(crc32cfast(0,pointer(s),length(s))=crc);
    Check(crc32c(0,pointer(s),length(s))=crc);
    if s<>'' then
      Check(xxhash32(0,pointer(s),length(s))=xxHash32reference(pointer(s),length(s)));
    j := Random32gsl;
    str(j,a);
    s := RawUTF8(a);
    u := string(a);
    CheckEqual(OctToBin(s),s);
    CheckEqual(TestAddFloatStr(s),s);
    Check(SysUtils.IntToStr(j)=u);
    s2 := Int32ToUtf8(j);
    CheckEqual(s2,s);
    Check(format('%d',[j])=u);
    Check(GetInteger(pointer(s))=j);
{$ifndef DELPHI5OROLDER}
    CheckEqual(FormatUTF8('%',[j]),s);
    CheckEqual(FormatUTF8('?',[],[j]),':('+s+'):');
    CheckEqual(FormatUTF8('%?',[j]),s+'?');
    CheckEqual(FormatUTF8('?%',[j]),'?'+s);
    CheckEqual(FormatUTF8('?%?',[j]),'?'+s+'?');
    CheckEqual(FormatUTF8('?%%?',[j]),'?'+s+'?');
    CheckEqual(FormatUTF8('?%?%  ',[j]),'?'+s+'?  ');
    CheckEqual(FormatUTF8('?%',[],[j]),':('+s+'):');
    CheckEqual(FormatUTF8('%?',[j],[j]),s+':('+s+'):');
    CheckEqual(FormatUTF8('%?',[s],[s]),s+':('''+s+'''):');
    CheckEqual(FormatUTF8('% ',[j]),s+' ');
    CheckEqual(FormatUTF8('? ',[],[j]),':('+s+'): ');
    CheckEqual(FormatUTF8('% %',[j]),s+' ');
    CheckEqual(FormatUTF8(' % %',[j]),' '+s+' ');
    CheckEqual(FormatUTF8(' ?? ',[],[j]),' :('+s+'): ');
    CheckEqual(FormatUTF8('?',[],[j],true),s);
    CheckEqual(FormatUTF8('?%',[],[j],true),s);
    CheckEqual(FormatUTF8('? ',[],[j],true),s+' ');
    CheckEqual(FormatUTF8(' ?? ',[],[j],true),' '+s+' ');
    CheckEqual(FormatUTF8('?%',[],[s],true),'"'+s+'"');
    CheckEqual(FormatUTF8(' ?? ',[],[s],true),' "'+s+'" ');
    CheckEqual(FormatUTF8('? %',[s],[s],true),'"'+s+'" '+s);
{$ifndef NOVARIANTS}
    vj := variant(j);
    RawUTF8ToVariant(s,vs);
    CheckEqual(FormatUTF8(' ?? ',[],[vj],true),' '+s+' ');
    CheckEqual(FormatUTF8(' ?? ',[],[vj]),' :('''+s+'''): ');
    CheckEqual(FormatUTF8('% ?',[vj],[vj]),s+' :('''+s+'''):');
    CheckEqual(FormatUTF8(' ?? ',[],[vs]),' :('''+s+'''): ');
    CheckEqual(FormatUTF8('% ?',[vj],[vj]),s+' :('''+s+'''):');
    CheckEqual(FormatUTF8('? %',[vj],[vj],true),s+' '+s);
    CheckEqual(FormatUTF8(' ?? ',[],[vs],true),' "'+s+'" ');
    CheckEqual(FormatUTF8('? %',[vs],[vj],true),s+' '+s);
{$endif}
{$endif DELPHI5OROLDER}
    k := Int64(j)*Random(MaxInt);
    b := Random(64);
    s := GetBitCSV(k,b);
    l := 0;
    P := pointer(s);
    SetBitCSV(l,b,P);
    Check(P=nil);
    while b>0 do begin
      dec(b);
      Check(GetBit(l,b)=GetBit(k,b));
    end;
    str(k,a);
    s := RawUTF8(a);
    u := string(a);
    CheckEqual(TestAddFloatStr(s),s);
    Check(SysUtils.IntToStr(k)=u);
    Check(IsAnsiCompatible(s));
    Check(Int64ToUtf8(k)=s);
    Check(IntToString(k)=u);
    Check(format('%d',[k])=u);
{$ifndef DELPHI5OROLDER}
    Check(FormatUTF8('%',[k])=s);
    Check(FormatUTF8('?',[],[k])=':('+s+'):');
{$endif}
    err := 1;
    l := GetInt64(pointer(s),err);
    Check((err=0)and(l=k));
    SetInt64(pointer(s),l);
    s := s+'z';
    l := GetInt64(pointer(s),err);
    Check(err<>0);
    case i of // validate some explicit ToVarUInt32/64 boundaries
      9991: j := $00003fff;
      9992: j := $00004000;
      9993: j := $00004001;
      9994: j := $001fffff;
      9995: j := $00200000;
      9996: j := $00200001;
      9997: j := $0fffffff;
      9998: j := $10000000;
      9999: j := $10000001;
    end;
    str(j,a);
    Check(SysUtils.IntToStr(j)=string(a));
    Check(format('%d',[j])=string(a));
    Check(format('%.8x',[j])=IntToHex(j,8));
    case i of
      9990: d := 1E110;
      9991: d := 1E-110;
      9992: d := 1E210;
      9993: d := 1E-210;
      else d := Random*1E-17-Random*1E-19;
    end;
    str(d,a);
    s := RawUTF8(a);
    e := GetExtended(Pointer(s),err);
    Check(SameValue(e,d,0)); // validate str()
    s := ExtendedToStr(d,DOUBLE_PRECISION);
    e := GetExtended(Pointer(s),err);
    Check(SameValue(e,d,0));
    e := d;
    if (i < 9000) or (i > 9999) then begin
      a[0] := AnsiChar(ExtendedToShort(a,d,DOUBLE_PRECISION));
      a2[0] := AnsiChar(DoubleToShort(a2,d));
      Check(a=a2);
      a[0] := AnsiChar(ExtendedToShortNoExp(a,d,DOUBLE_PRECISION));
      a2[0] := AnsiChar(DoubleToShortNoExp(a2,d));
      Check(a=a2);
      CheckEqual(TestAddFloatStr(s),s);
      Check(not SameValue(e+1,d));
      sd := d;
      Check(d=e);
      Check(SortDynArrayDouble(d,d)=0);
      Check(SortDynArrayDouble(d,e)=0);
      se := sd;
      Check(SortDynArraySingle(sd,sd)=0);
      Check(SortDynArraySingle(sd,se)=0);
    end;
    if d<0 then
      e := e*0.9 else
      e := e*1.1;
    check(d<e);
    Check(SortDynArrayDouble(d,e)=-1);
    Check(SortDynArrayDouble(e,d)=1);
    if (i < 9000) or (i > 9999) then begin
      se := e;
      Check(SortDynArraySingle(sd,se)=-1);
      Check(SortDynArraySingle(se,sd)=1);
    end;
    PC := ToVarUInt32(juint,@varint);
    Check(PC<>nil);
    Check(PtrInt(PC)-PtrInt(@varint)=integer(ToVarUInt32Length(juint)));
    PB := @varint;
    Check(PtrUInt(FromVarUint32(PB))=juint);
    Check(PB=PC);
    PC := ToVarUInt32(i,@varint);
    Check(PC<>nil);
    PB := @varint;
    Check(PtrInt(FromVarUint32(PB))=i);
    Check(PB=PC);
    PB := FromVarUInt32Safe(@varint,PC,u32);
    Check(PtrInt(u32)=i);
    Check(PB=PC);
    PC := ToVarInt32(j,@varint);
    Check(PC<>nil);
    PB := @varint;
    Check(FromVarInt32(PB)=j);
    Check(PB=PC);
    PC := ToVarInt32(i-1,@varint);
    Check(PC<>nil);
    PB := @varint;
    Check(FromVarInt32(PB)=i-1);
    Check(PB=PC);
    PC := ToVarUInt64(juint,@varint);
    Check(PC<>nil);
    Check(PtrInt(PC)-PtrInt(@varint)=integer(ToVarUInt32Length(juint)));
    PB := @varint;
    Check(PtrUInt(FromVarUint64(PB))=juint);
    Check(PB=PC);
    PB := FromVarUInt64Safe(@varint,PC,q);
    Check(q=juint);
    Check(PB=PC);
    PC := ToVarInt64(k,@varint);
    Check(PC<>nil);
    PB := @varint;
    Check(FromVarInt64(PB)=k);
    Check(PB=PC);
    Check(FromVarInt64Value(@varint)=k);
    PC := ToVarInt64(i,@varint);
    Check(PC<>nil);
    PB := @varint;
    Check(FromVarInt64(PB)=i);
    Check(PB=PC);
    if k<0 then
      k := -k;
    PC := ToVarUInt64(k,@varint);
    Check(PC<>nil);
    PB := @varint;
    Check(FromVarUint64(PB)=k);
    Check(PB=PC);
    PC := ToVarUInt64(i,@varint);
    Check(PC<>nil);
    PB := @varint;
    Check(FromVarUint64(PB)=i);
    Check(PB=PC);
    PC := @varint;
    for n := 0 to 49 do
      PC := ToVarUInt32(juint+n,PC);
    check(PC<>nil);
    st.Init(@varint, PtrInt(PC) - PtrInt(@varint));
    check(not st.EOF);
    for n := 0 to 48 do
      check(st.VarUInt32 = cardinal(juint+n));
    check(not st.EOF);
    check(st.VarUInt32 = cardinal(juint+49));
    check(pointer(st.P) = pointer(PC));
    check(st.EOF);
    st.Init(@varint, PtrInt(PC) - PtrInt(@varint));
    check(not st.EOF);
    for n := 0 to 49 do
      check(st.VarUInt64 = cardinal(juint+n));
    check(pointer(st.P) = pointer(PC));
    check(st.EOF);
    st.Init(@varint, PtrInt(PC) - PtrInt(@varint));
    for n := 0 to 48 do
      st.VarNextInt;
    check(not st.EOF);
    check(st.VarUInt32 = cardinal(juint+49));
    check(pointer(st.P) = pointer(PC));
    check(st.EOF);
    st.Init(@varint, PtrInt(PC) - PtrInt(@varint));
    st.VarNextInt(49);
    check(not st.EOF);
    check(st.VarUInt32 = cardinal(juint+49));
    check(pointer(st.P) = pointer(PC));
    check(st.EOF);
  end;
  exit; // code below is speed informative only, without any test
  Timer.Start;
  for i := 0 to 99999 do
    SysUtils.IntToStr(Int64(7777)*Random32gsl);
  fRunConsole := format('%s SysUtils.IntToStr %s %s/s',[fRunConsole,Timer.Stop,
    IntToThousandString(Timer.PerSec(100000))]);
  Timer.Start;
  RandSeed := 10;
  for i := 0 to 99999 do
    StrInt64(@varint[31],Int64(7777)*Random32gsl);
  fRunConsole := format('%s StrInt64 %s %s/s',[fRunConsole,Timer.Stop,
    IntToThousandString(Timer.PerSec(100000))]);
end;

function LowerCaseReference(const S: RawByteString): RawByteString;
var Ch: AnsiChar;
    L: Integer;
    Source, Dest: PAnsiChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L<>0 do begin
    Ch := Source^;
    if (Ch >= 'A') and (Ch <= 'Z') then Inc(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

procedure TTestLowLevelCommon.BaudotCode;
var u: RawUTF8;
    b: RawByteString;
    i,j,k: integer;
    P: PAnsiChar absolute u;
const CHR: array[0..82] of AnsiChar =
  'abcdefghijklm nopqrstuvwx yzabcdefghijklm nopqrstuvwx yz012345 6789-''3,!:(+)$?@./; ';
begin
  b := AsciiToBaudot('');
  check(b='');
  b := AsciiToBaudot('abc');
  u := BaudotToAscii(b);
  check(u='abc');
  b := AsciiToBaudot('mORMot.net');
  check(BaudotToAscii(b)='mormot.net');
  b := b+#0#0#0;
  u := BaudotToAscii(b);
  check(u='mormot.net');
  b := AsciiToBaudot('https://synopse.info');
  u := BaudotToAscii(b);
  check(u='https://synopse.info');
  b := AsciiToBaudot('abcdef 1234 5678'#13#10'ABCD;/23u'#13#10'op @toto.#com');
  check(b<>'');
  u := BaudotToAscii(b);
  check(u='abcdef 1234 5678'#13#10'abcd;/23u'#13#10'op @toto.com');
  for i := 1 to 200 do begin
    SetLength(u,i);
    for k := 1 to 50 do begin
      for j := 0 to i-1 do
        P[j] := CHR[Random(83)];
      b := AsciiToBaudot(u);
      check(BaudotToAscii(b)=u);
    end;
  end;
end;

procedure TTestLowLevelCommon._UTF8;
  procedure Test(CP: cardinal; const W: WinAnsiString);
  var C: TSynAnsiConvert;
      A: RawByteString;
      U: RawUTF8;
  begin
    C := TSynAnsiConvert.Engine(CP);
    Check(C.CodePage=CP);
    U := C.AnsiToUTF8(W);
    A := C.UTF8ToAnsi(U);
    if W='' then
      exit;
    {$ifdef HASCODEPAGE}
    {$ifndef FPC}
    Check(StringCodePage(W)=1252);
    {$endif}
    CP := StringCodePage(A);
    Check(CP=C.CodePage);
    {$endif}
    if CP=CP_UTF16 then
      exit;
    Check(length(W)=length(A));
    {$ifdef FPC}
    Check(CompareMem(pointer(W),pointer(A),length(W)));
    {$else}
    Check(A=W);
    Check(C.RawUnicodeToAnsi(C.AnsiToRawUnicode(W))=W);
    {$endif}
  end;
  procedure CheckTrimCopy(const S: RawUTF8; start,count: PtrInt);
  var t: RawUTF8;
  begin
    trimcopy(s,start,count,t);
    checkEqual(t,trim(copy(s,start,count)));
  end;
var i, j, k, len, lenup100, CP, L: integer;
    W: WinAnsiString;
    WS: WideString;
    SU: SynUnicode;
    str: string;
    U,U2, res, Up,Up2: RawUTF8;
    arr: TRawUTF8DynArray;
    PB: PByte;
    {$ifndef DELPHI5OROLDER}
    q: RawUTF8;
    {$endif}
    Unic: RawUnicode;
    WA: Boolean;
const ROWIDS: array[0..17] of PUTF8Char = (
  'id','ID','iD','rowid','ROWid','ROWID','rowiD','ROWId', // ok
  'id2','id ','idd','i','rowi','row','ROWI','ROW','ROWIDD','ROWID ');
  IDPU: array[0..15] of PUTF8Char = (
    'anything','t','1','te','tE','TE','tes','test','TeSt','teS','tesT','testE',
    'T','T','1','teste');
  IDPA: array[0..15] of PAnsiChar = (
    nil,'T','1','TE','TE','TE','TES','TEST','TEST','TES','TEST','TESTE',
    't','U','2','TESTe');
begin
  for i := 0 to high(ROWIDS) do
    Check(isRowID(ROWIDS[i])=(i<8));
  U := 'old1,old2,old3';
  Check(not RenameInCSV('old','new',U));
  Check(RenameInCSV('old1','n1',U));
  Check(U='n1,old2,old3');
  Check(RenameInCSV('old2','n2',U));
  Check(not RenameInCSV('old2','news2',U));
  Check(RenameInCSV('old3','news3',U));
  Check(U='n1,n2,news3');
  Check(RenameInCSV(U,'1-2-3',U,'!'));
  Check(U='1-2-3');
  Check(RenameInCSV('2','bee',U,'-'));
  Check(RenameInCSV('1','ah',U,'-'));
  Check(RenameInCSV('3','see',U,'-'));
  Check(U='ah-bee-see');
  for i := 0 to High(IDPU) do
    Check(IdemPChar(IDPU[i],IDPA[i])=(i<12));
  res := '{"result":[{"000001000013":{"00100000000016":[1534510257860,103100,2000,' +
    '103108,1004,104132],"00100000000026":[1534510257860,12412,2000,12420,1004,12420],' +
    '"00100000000036":[1534510257860,1378116,2000,1378112,1004,1378112],"00100000000056":' +
    '[1534510257860,479217551,2000,479217551],"00100000000076":[1534510257860,136079943,' +
    '2000,136079943,1004,136079944],"00100000000086":[1534510257860,1648800821,2000,' +
    '1648801020,1004,1648801119],"00100000000096":[1534510257860,87877677,2000,87877678,' +
    '1004,87877678],"001000000000ec":[1534510257860,1.64,2000,1.64],"001000000000fc":[' +
    '1534510257860,1.72,2000,1.72],"0010000000010c":[1534510257860,1.64,2000,1.64],"' +
    '00100000000196":[1534510257860,0,2000,0]}}]}';
  i := SynCommons.StrLenPas(@res[1]);
  check(SynCommons.StrLen(@res[1])=i);
  res := 'one,two,three';
  Check(EndWith('three','THREE'));
  Check(EndWith(res,'E'));
  Check(EndWith(res,'THREE'));
  Check(EndWith(res,',THREE'));
  Check(not EndWith(res,',THREe'));
  Check(not EndWith(res,res));
  Check(not EndWith('t',',THREe'));
  Check(not EndWith('thre',',THREe'));
  Check(EndWithArray(res,[])<0);
  Check(EndWithArray(res,['E','F'])=0);
  Check(EndWithArray(res,['ONE','THREE'])=1);
  Check(EndWithArray(res,['ONE','three','THREE'])=2);
  Check(EndWithArray(res,['ONE','','THREE'])=1);
  Check(EndWithArray(res,['ONE','three','THREe'])<0);
  Check(split(res,',')='one');
  Check(split(res,'*')=res);
  Check(split(res,',',5)='two');
  Check(split(res,'*',6)='wo,three');
  Check(SynCommons.StrLen(nil)=0);
  for i := length(res)+1 downto 1 do
    Check(SynCommons.StrLen(Pointer(@res[i]))=length(res)-i+1);
  Check(StrLenPas(nil)=0);
  for i := length(res)+1 downto 1 do
    Check(StrLenPas(Pointer(@res[i]))=length(res)-i+1);
  CSVToRawUTF8DynArray(pointer(res),arr);
  Check(arr[0]='one');
  Check(arr[1]='two');
  Check(arr[2]='three');
  Finalize(arr);
  CSVToRawUTF8DynArray(res,',','',arr);
  Check(arr[0]='one');
  Check(arr[1]='two');
  Check(arr[2]='three');
  Finalize(arr);
  CSVToRawUTF8DynArray('one=?,two=?,three=?','=?,','=?',arr);
  Check(arr[0]='one');
  Check(arr[1]='two');
  Check(arr[2]='three');
  Finalize(arr);
  res := '-1,25,0';
  CSVToRawUTF8DynArray(pointer(res),arr);
  check(Length(arr)=3);
  Check(arr[0]='-1');
  Check(arr[1]='25');
  Check(arr[2]='0');
  Check(AddPrefixToCSV('One,Two,Three','Pre')='PreOne,PreTwo,PreThree');
  Check(CSVOfValue('?',3)='?,?,?');
{$ifndef DELPHI5OROLDER}
  Check(GetUnQuoteCSVItem('"""one,""","two "',1,',','"')='two ');
  Check(GetUnQuoteCSVItem('''''''one,''''''',0)='''one,''');
  Check(GetUnQuoteCSVItem('"""one,',0,',','"')='');
  Check(FormatUTF8('abcd',[U],[WS])='abcd');
{$endif}
  U := QuotedStr('','"');
  CheckEqual(U,'""');
  U := QuotedStr('abc','"');
  CheckEqual(U,'"abc"');
  U := QuotedStr('a"c','"');
  CheckEqual(U,'"a""c"');
  U := QuotedStr('abcd"efg','"');
  CheckEqual(U,'"abcd""efg"');
  U := QuotedStr('abcd""efg','"');
  CheckEqual(U,'"abcd""""efg"');
  U := QuotedStr('abcd"e"fg"','"');
  CheckEqual(U,'"abcd""e""fg"""');
  U := QuotedStr('"abcd"efg','"');
  CheckEqual(U,'"""abcd""efg"');
  U := QuotedStr('','#'); // also test for custom quote
  CheckEqual(U,'##');
  U := QuotedStr('abc','#');
  CheckEqual(U,'#abc#');
  U := QuotedStr('a#c','#');
  CheckEqual(U,'#a##c#');
  U := QuotedStr('abcd#efg','#');
  CheckEqual(U,'#abcd##efg#');
  U := QuotedStr('abcd##efg','#');
  CheckEqual(U,'#abcd####efg#');
  U := QuotedStr('abcd#e#fg#','#');
  CheckEqual(U,'#abcd##e##fg###');
  U := QuotedStr('#abcd#efg','#');
  CheckEqual(U,'###abcd##efg#');
  for i := 0 to 1000 do begin
    len := i*5;
    W := RandomAnsi7(len);
    Check(length(W)=len);
    lenup100 := len;
    if lenup100>100 then
      lenup100 := 100;
    str := Ansi7ToString(W); // should be fine on any code page
    if len>0 then begin
      Check(length(str)=len);
      check(PosExString(str[1],str)=1);
      if str[1]<>str[2] then begin
        check(PosExString(str[2],str)=2);
        if (str[1]<>str[2]) and (str[2]<>str[3]) and (str[1]<>str[3]) then
          check(PosExString(str[3],str)=3);
      end;
      for j := 1 to lenup100 do begin
        check(PosExString(#13,str,j)=0);
        check(PosExString(str[j],str,j)=j);
        if (j>1) and (str[j-1]<>str[j]) then
          check(PosExString(str[j],str,j-1)=j);
        k := PosExString(str[j],str);
        check((k>0) and (str[k]=str[j]));
      end;
    end else
      check(PosExString(#0,str)=0);
    for CP := 1250 to 1258 do
      Test(CP,W);
    Test(932,W);
    Test(949,W);
    Test(874,W);
    Test(CP_UTF8,W);
    L := Length(W);
    if L and 1<>0 then
      SetLength(W,L-1); // force exact UTF-16 buffer length
    Test(CP_UTF16,W);
    W := WinAnsiString(RandomString(len));
    U := WinAnsiToUtf8(W);
    if len>0 then begin
      check(PosEx(U[1],U)=1);
      check(PosExChar(U[1],U)=1);
      if (len>1) and (U[1]<>U[2]) then begin
        check(PosEx(U[2],U)=2);
        check(PosExChar(U[2],U)=2);
        if (len>2) and (U[1]<>U[2]) and (U[2]<>U[3]) and (U[1]<>U[3]) then begin
          check(PosEx(U[3],U)=3);
          check(PosExChar(U[3],U)=3);
        end;
      end;
    end;
    for j := 1 to lenup100 do begin // validates with offset parameter
      check(PosEx(#13,U,j)=0);
      check(PosEx(U[j],U,j)=j);
      if (j>1) and (U[j-1]<>U[j]) then
        check(PosEx(U[j],U,j-1)=j);
      k := PosEx(U[j],U);
      check((k>0) and (U[k]=U[j]));
      check(PosExChar(U[j],U)=k);
    end;
    Unic := Utf8DecodeToRawUnicode(U);
    {$ifndef FPC_HAS_CPSTRING} // buggy FPC
    Check(Utf8ToWinAnsi(U)=W);
    Check(WinAnsiConvert.UTF8ToAnsi(WinAnsiConvert.AnsiToUTF8(W))=W);
    Check(WinAnsiConvert.RawUnicodeToAnsi(WinAnsiConvert.AnsiToRawUnicode(W))=W);
    if CurrentAnsiConvert.InheritsFrom(TSynAnsiFixedWidth) then begin
      Check(CurrentAnsiConvert.UTF8ToAnsi(CurrentAnsiConvert.AnsiToUTF8(W))=W);
      Check(CurrentAnsiConvert.RawUnicodeToAnsi(CurrentAnsiConvert.AnsiToRawUnicode(W))=W);
    end;
    res := RawUnicodeToUtf8(Unic);
    Check(res=U);
    Check(RawUnicodeToWinAnsi(Unic)=W);
    {$endif FPC_HAS_CPSTRING}
    WS := UTF8ToWideString(U);
    Check(length(WS)=length(Unic)shr 1);
    if WS<>'' then
      Check(CompareMem(pointer(WS),pointer(Unic),length(WS)*sizeof(WideChar)));
    Check(integer(Utf8ToUnicodeLength(Pointer(U)))=length(WS));
    SU := UTF8ToSynUnicode(U);
    Check(length(SU)=length(Unic)shr 1);
    if SU<>'' then
      Check(CompareMem(pointer(SU),pointer(Unic),length(SU)));
    WA := IsWinAnsi(pointer(Unic));
    Check(IsWinAnsi(pointer(Unic),length(Unic)shr 1)=WA);
    Check(IsWinAnsiU(pointer(U))=WA);
    Up := SynCommons.UpperCase(U);
    Check(SynCommons.UpperCase(SynCommons.LowerCase(U))=Up);
    Check(UTF8IComp(pointer(U),pointer(U))=0);
    Check(UTF8IComp(pointer(U),pointer(Up))=0);
    Check(UTF8ILComp(pointer(U),pointer(U),length(U),length(U))=0);
    Check(UTF8ILComp(pointer(U),pointer(Up),length(U),length(Up))=0);
    Check(LowerCase(U)=LowerCaseReference(U));
    L := Length(U);
    SetString(Up,nil,L);
    SetString(Up2,PAnsiChar(pointer(U)),L);
    L := UTF8UpperCopy(pointer(Up),pointer(U),L)-pointer(Up);
    Check(L<=length(U));
    Check(ConvertCaseUTF8(Pointer(Up2),NormToUpperByte)=L);
    if Up<>'' then
      Check(CompareMem(Pointer(Up),pointer(Up2),L));
    if CurrentAnsiConvert.CodePage=CODEPAGE_US then
       // initial text above is WinAnsiString (CP 1252)
      Check(StringToUTF8(UTF8ToString(U))=U);
    Up := UpperCaseUnicode(U);
    Check(Up=UpperCaseUnicode(LowerCaseUnicode(U)));
    Check(kr32(0,pointer(U),length(U))=kr32reference(pointer(U),length(U)));
    if U='' then
      continue;
    U2 := QuotedStr(U,'"');
    Check(UnQuoteSQLStringVar(pointer(U2),res)<>nil);
    Check(res=U);
    Check(not IsZero(pointer(W),length(W)));
    FillCharFast(pointer(W)^,length(W),0);
    Check(IsZero(pointer(W),length(W)));
    Check(FormatUTF8(U,[])=U);
{$ifndef DELPHI5OROLDER}
    res := FormatUTF8(U,[],[]); // Delphi 5 bug with high([])>0 :(
    Check(length(res)=Length(u));
    Check(res=u);
    Check(FormatUTF8('%',[U])=U);
    Check(FormatUTF8('%',[U],[])=U);
    q := ':('+QuotedStr(U)+'):';
    Check(FormatUTF8('?',[],[U])=q);
    res := 'ab'+U;
    q := 'ab'+q;
    Check(FormatUTF8('ab%',[U])=res);
    Check(FormatUTF8('%%',['ab',U])=res);
    Check(FormatUTF8('ab%',[U],[])=res);
    Check(FormatUTF8('%%',['ab',U],[])=res);
    Check(FormatUTF8('ab?',[],[U])=q);
    Check(FormatUTF8('%?',['ab'],[U])=q);
    res := res+'cd';
    q := q+'cd';
    Check(FormatUTF8('ab%cd',[U])=res);
    Check(FormatUTF8('ab%cd',[U],[])=res);
    Check(FormatUTF8('a%%cd',['b',U])=res);
    Check(FormatUTF8('a%%cd',['b',U],[])=res);
    Check(FormatUTF8('%%%',['ab',U,'cd'])=res);
    Check(FormatUTF8('ab?cd',[],[U])=q);
    Check(FormatUTF8('%?cd',['ab'],[U])=q);
    Check(FormatUTF8('%?%',['ab','cd'],[U])=q);
    Check(FormatUTF8('%?c%',['ab','d'],[U])=q);
    Check(FormatUTF8('a%?%d',['b','c'],[U])=q);
{$endif}
  end;
  SetLength(U, 4);
  U[1] := #$F0;
  U[2] := #$A8;
  U[3] := #$B3;
  U[4] := #$92;
  SU := UTF8ToSynUnicode(U);
  if not CheckFailed(length(SU)=2) then
    Check(PCardinal(SU)^=$DCD2D863);
  Check(Utf8ToUnicodeLength(Pointer(U))=2);
  Check(Utf8FirstLineToUnicodeLength(Pointer(U))=2);
  U := SynUnicodeToUtf8(SU);
  if not CheckFailed(length(U)=4) then
    Check(PCardinal(U)^=$92b3a8f0);
  U := TSynAnsiConvert.Engine(CP_UTF8).UnicodeBufferToAnsi(pointer(SU), length(SU));
  Check(length(U)=4);
  SetLength(res,10);
  PB := pointer(res);
  PB := ToVarString(U,PB);
  check(PAnsiChar(PB)-pointer(res)=length(U)+1);
  PB := pointer(res);
  res := FromVarString(PB);
  check(res=U);
  Check(UnQuoteSQLStringVar('"one two"',U)<>nil);
  Check(U='one two');
  Check(UnQuoteSQLStringVar('one two',U)<>nil);
  Check(U='ne tw');
  Check(UnQuoteSQLStringVar('"one "" two"',U)<>nil);
  Check(U='one " two');
  Check(UnQuoteSQLStringVar('"one " two"',U)<>nil);
  Check(U='one ');
  Check(UnQuoteSQLStringVar('"one two',U)=nil);
  Check(UnQuoteSQLStringVar('"one "" two',U)=nil);
  Check(IsValidEmail('test@synopse.info'));
  Check(not IsValidEmail('test@ synopse.info'));
  Check(IsValidEmail('test_two@blog.synopse.info'));
  Check(IsValidIP4Address('192.168.1.1'));
  Check(IsValidIP4Address('192.168.001.001'));
  Check(not IsValidIP4Address('192.158.1. 1'));
  Check(not IsValidIP4Address('192.158.1.301'));
  Check(not IsValidIP4Address(' 12.158.1.01'));
  Check(not IsValidIP4Address('12.158.1.'));
  Check(not IsValidIP4Address('12.158.1'));
  {$ifdef MSWINDOWS}
  Check(FindUnicode('  ABCD DEFG','ABCD',4));
  Check(FindUnicode('  ABCD DEFG','DEFG',4));
  Check(FindUnicode('ABCD DEFG ','DEFG',4));
  Check(FindUnicode('ABCD DEFG ','ABCD',4));
  Check(FindUnicode('  abcd defg','ABCD',4));
  Check(FindUnicode('  abcd defg','DEFG',4));
  Check(FindUnicode('abcd defg ','DEFG',4));
  Check(FindUnicode('abcd defg ','ABCD',4));
  Check(FindUnicode('ABCD DEFG ','ABCD',4));
  Check(FindUnicode('  abcde defg','ABCD',4));
  Check(FindUnicode('  abcdf defg','DEFG',4));
  Check(FindUnicode('abcdg defg ','DEFG',4));
  Check(FindUnicode('abcdh defg ','ABCD',4));
  Check(FindUnicode('  abcd defg','ABC',3));
  Check(FindUnicode('  abcd defg','DEF',3));
  Check(FindUnicode('abcd defg ','DEF',3));
  Check(FindUnicode('abcd defg ','ABC',3));
  Check(not FindUnicode('  abcd defg','ABC2',4));
  Check(not FindUnicode('  abcd defg','DEF2',4));
  Check(not FindUnicode('abcd defg ','DEF1',4));
  Check(not FindUnicode('abcd defg ','ABC1',4));
  Check(UpperCaseUnicode('abcdefABCD')='ABCDEFABCD');
  Check(LowerCaseUnicode('abcdefABCD')='abcdefabcd');
  {$endif}
  Check(StringReplaceAll('abcabcabc','toto','toto')='abcabcabc');
  Check(StringReplaceAll('abcabcabc','toto','titi')='abcabcabc');
  Check(StringReplaceAll('abcabcabc','ab','AB')='ABcABcABc');
  Check(StringReplaceAll('abcabcabc','bc','')='aaa');
  Check(StringReplaceAll('abcabcabc','bc','B')='aBaBaB');
  Check(StringReplaceAll('abcabcabc','bc','bcd')='abcdabcdabcd');
  Check(StringReplaceAll('abcabcabc','c','C')='abCabCabC');
  Check(StringReplaceAll('abcabcabc',[])='abcabcabc');
  Check(StringReplaceAll('abcabcabc',['c'])='abcabcabc');
  Check(StringReplaceAll('abcabcabc',['c','C'])='abCabCabC');
  Check(StringReplaceAll('abcabcabc',['c','C','a'])='abcabcabc');
  Check(StringReplaceAll('abcabcabc',['c','C','toto','titi','ab','AB'])='ABCABCABC');
  for i := -10 to 50 do
    for j := -10 to 50 do begin
      CheckTrimCopy('',i,j);
      CheckTrimCopy('1',i,j);
      CheckTrimCopy('1 ',i,j);
      CheckTrimCopy(' 1',i,j);
      CheckTrimCopy('   1',i,j);
      CheckTrimCopy('1   ',i,j);
      CheckTrimCopy('1',i,j);
      CheckTrimCopy('12',i,j);
      CheckTrimCopy('123',i,j);
      CheckTrimCopy(' 234',i,j);
      CheckTrimCopy(' 234 ',i,j);
      CheckTrimCopy(' 2 4',i,j);
      CheckTrimCopy(' 2 4 ',i,j);
      CheckTrimCopy('  3    ',i,j);
      CheckTrimCopy('  3   7  ',i,j);
      CheckTrimCopy(' 234 6',i,j);
      CheckTrimCopy('234 67 ',i,j);
      CheckTrimCopy(' 234 67 ',i,j);
      CheckTrimCopy(' 234 67 ',i,maxInt);
    end;
end;

procedure TTestLowLevelCommon.Iso8601DateAndTime;
  procedure Test(D: TDateTime; Expanded: boolean);
  var s,t: RawUTF8;
      E,F: TDateTime;
      I,J: TTimeLogBits;
      st, s2: TSynSystemTime;
      P: PUTF8Char;
      d1, d2: TSynDate;
  begin
    s := DateTimeToIso8601(D,Expanded);
    if Expanded then
      Check(length(s)=19) else
      Check(length(s)=15);
    if Expanded then begin
      Check(Iso8601CheckAndDecode(Pointer(s),length(s),E));
      Check(Abs(D-E)<(1/SecsPerDay)); // we allow 999 ms error
    end;
    st.FromDateTime(D);
    s2.Clear;
    DecodeDate(D,s2.Year,s2.Month,s2.Day);
    DecodeTime(D,s2.Hour,s2.Minute,s2.Second,s2.MilliSecond);
    Check(abs(st.MilliSecond-s2.MilliSecond)<=1); // allow 1 ms rounding error
    st.MilliSecond := 0;
    s2.MilliSecond := 0;
    Check(st.IsEqual(s2)); // ensure conversion matches the RTL's
    t := st.ToText(Expanded);
    Check(Copy(t,1,length(s))=s);
    d1.Clear;
    check(d1.IsZero);
    d2.SetMax;
    check(not d2.IsZero);
    check(not d1.IsEqual(d2));
    check(d1.Compare(d2)<0);
    check(d2.Compare(d1)>0);
    t := d2.ToText(false);
    check(t='99991231');
    check(d2.ToText(true)='9999-12-31');
    d2.Clear;
    check(d1.IsEqual(d2));
    check(d1.Compare(d2)=0);
    check(d2.Compare(d1)=0);
    P := pointer(s);
    check(d1.ParseFromText(P));
    check(P<>nil);
    check(not d1.IsZero);
    check(st.IsDateEqual(d1));
    t := d1.ToText(Expanded);
    check(copy(s,1,length(t))=t);
    d2.Clear;
    check(d2.IsZero);
    check(not d1.IsEqual(d2));
    check(d1.Compare(d2)>0);
    check(d2.Compare(d1)<0);
    check(d2.ToText(Expanded)='');
    d2.SetMax;
    check(not d2.IsZero);
    check(not d1.IsEqual(d2));
    check(d1.Compare(d2)<0);
    check(d2.Compare(d1)>0);
    d2 := d1;
    check(d1.IsEqual(d2));
    check(d1.Compare(d2)=0);
    check(d2.Compare(d1)=0);
    E := Iso8601ToDateTime(s);
    Check(Abs(D-E)<(1/SecsPerDay)); // we allow 999 ms error
    E := Iso8601ToDateTime(s+'Z');
    Check(Abs(D-E)<(1/SecsPerDay)); // we allow 999 ms error
    I.From(D);
    Check(Iso8601ToTimeLog(s)=I.Value);
    t := s;
    t[11] := ''''; // as in SynDB VArray[] quoted parameters
    J.From(pointer(t),10);
    Check(I.Value and not(1 shl (6+6+5)-1)=J.Value);
    I.From(s);
    t := I.Text(Expanded);
    if t<>s then // we allow error on time = 00:00:00 -> I.Text = just date
      Check(I.Value and (1 shl (6+6+5)-1)=0) else
      Check(true);
    J.From(E);
    Check(Int64(I)=Int64(J));
    s := TimeToIso8601(D,Expanded);
    Check(PosEx('.',s)=0);
    Check(abs(frac(D)-Iso8601ToDateTime(s))<1/SecsPerDay);
    s := TimeToIso8601(D,Expanded,'T',true);
    Check(PosEx('.',s)>0);
    F := Iso8601ToDateTime(s);
    Check(abs(frac(D)-F)<1/MSecsPerDay,'withms1');
    s := DateToIso8601(D,Expanded);
    Check(trunc(D)=trunc(Iso8601ToDateTime(s)));
    Check(Abs(D-I.ToDateTime)<(1/SecsPerDay));
    E := TimeLogToDateTime(I.Value);
    Check(Abs(D-E)<(1/SecsPerDay));
    s := DateTimeToIso8601(D,Expanded,#0);
    if Expanded then
      Check(length(s)=18) else
      Check(length(s)=14);
    s := DateTimeToIso8601(D,Expanded,'T',true);
    Check(PosEx('.',s)>0);
    if Expanded then
      Check(length(s)=23) else
      Check(length(s)=19);
    F := Iso8601ToDateTime(s);
    Check(abs(D-F)<1/MSecsPerDay,'withms2');
    if Expanded then begin
      F := 0;
      Check(Iso8601CheckAndDecode(pointer(s),length(s),F));
      Check(abs(D-F)<1/MSecsPerDay,'withms3');
    end;
  end;
var i: integer;
    D: TDateTime;
    tmp: RawUTF8;
    b: TTimeLogBits;
begin
  for i := 1700 to 2500 do
    Check(SynCommons.IsLeapYear(i) = SysUtils.IsLeapYear(i), 'IsLeapYear');
  // this will test typically from year 1905 to 2065
  D := Now/20+Random*20; // some starting random date/time
  for i := 1 to 2000 do begin
    Test(D, true);
    Test(D, false);
    D := D+Random*57; // go further a little bit: change date/time
  end;
  b.Value := Iso8601ToTimeLog('20150504');
  Check(b.Year=2015);
  Check(b.Month=5);
  Check(b.Day=4);
  tmp := b.Text(false);
  Check(tmp='20150504');
  IntervalTextToDateTimeVar('+0 06:03:20',D);
  CheckSame(D,0.252314,1e-5);
  D := IntervalTextToDateTime('+1 06:03:20');
  CheckSame(D,1.252314,1e-5);
  CheckSame(IntervalTextToDateTime('-20 06:03:20'),-20.252314,1e-6);
  Check(DateTimeToIso8601Text(IntervalTextToDateTime('+0 06:03:20'))='T06:03:20');
  tmp := DateTimeToIso8601Text(IntervalTextToDateTime('+1 06:03:20'));
  Check(tmp='1899-12-31T06:03:20');
  tmp := DateTimeToIso8601Text(IntervalTextToDateTime('-2 06:03:20'));
  Check(tmp='1899-12-28T06:03:20');
  CheckSame(TimeLogToDateTime(135131870949),41578.477512,1e-5);
  tmp := '1982-10-30T06:03:20';
  Check(Iso8601CheckAndDecode(Pointer(tmp),length(tmp),D));
  Check(DateTimeToIso8601(D,true)=tmp);
  tmp := '1982-10-30';
  Check(Iso8601CheckAndDecode(Pointer(tmp),length(tmp),D));
  Check(DateToIso8601(D,true)=tmp);
  tmp := 'T06:03:20';
  Check(Iso8601CheckAndDecode(Pointer(tmp),length(tmp),D));
  Check(TimeToIso8601(D,true)=tmp);
  tmp := '1982-10-30 06:03:20';
  Check(not Iso8601CheckAndDecode(Pointer(tmp),length(tmp),D));
  tmp := 'T06:03:2a';
  Check(not Iso8601CheckAndDecode(Pointer(tmp),length(tmp),D));
  tmp := '1435051262-45869-63626';
  check(Iso8601ToDateTime(tmp)=0);
  check(Iso8601ToTimelog(tmp)=0);
  tmp := UnixTimePeriodToString(0);
  check(tmp='T00:00:00');
  tmp := UnixTimePeriodToString(30);
  check(tmp='T00:00:30');
  tmp := UnixTimePeriodToString(SecsPerMin);
  check(tmp='T00:01:00');
  tmp := UnixTimePeriodToString(SecsPerMin*MinsPerHour);
  check(tmp='T01:00:00');
  tmp := UnixTimePeriodToString(SecsPerDay);
  check(tmp='0000-00-01');
  tmp := UnixTimePeriodToString(SecsPerDay*15);
  check(tmp='0000-00-15');
  tmp := UnixTimePeriodToString(SecsPerDay*365);
  check(tmp='0000-12-31');
  tmp := UnixTimePeriodToString(SecsPerDay*366);
  check(tmp='0001-00-00');
  tmp := UnixTimePeriodToString(SecsPerDay*732);
  check(tmp='0002-00-00');
end;

{$ifdef FPC}
Function _LocalTimeToUniversal(LT: TDateTime;TZOffset: Integer): TDateTime;
begin
  if (TZOffset > 0) then
    Result := LT - EncodeTime(TZOffset div 60, TZOffset mod 60, 0, 0)
  else if (TZOffset < 0) then
    Result := LT + EncodeTime(Abs(TZOffset) div 60, Abs(TZOffset) mod 60, 0, 0)
  else
    Result := LT;
end;
{$endif}

procedure TTestLowLevelCommon.TimeZones;
var tz: TSynTimeZone;
    d: TTimeZoneData;
    i,bias: integer;
    hdl,reload: boolean;
    buf: RawByteString;
    dt {$ifdef MSWINDOWS},local{$endif}: TDateTime;
procedure testBias(year,expected: integer);
begin
  check(tz.GetBiasForDateTime(EncodeDate(year,10,30),'1',bias,hdl));
  check(bias=expected);
end;
begin
  tz := TSynTimeZone.Create;
  try
    check(tz.Zone=nil);
    FillCharFast(d,sizeof(d),0);
    for i := 0 to 40 do begin
      UInt32ToUTF8(i,RawUTF8(d.id));
      d.display := 'displayed '+d.id;
      d.tzi.Bias := i;
      check(tz.Zones.Add(d)=i,'add some zones');
    end;
    tz.Zones.ReHash;
    dt := nowutc;
    for reload := false to true do begin
      check(tz.Zone<>nil);
      check(tz.Zones.Count=41);
      for i := 0 to 40 do begin
        UInt32ToUTF8(i,RawUTF8(d.id));
        check(tz.GetDisplay(d.id)='displayed '+d.id);
        hdl := true;
        check(tz.GetBiasForDateTime(dt,d.id,bias,hdl));
        check(bias=i);
        check(not hdl);
      end;
      check(not tz.GetBiasForDateTime(dt,'fail',bias,hdl));
      buf := tz.SaveToBuffer;
      tz.Zones.Clear;
      check(tz.Zone=nil);
      tz.LoadFromBuffer(buf);
    end;
    with tz.Zone[1] do begin
      SetLength(dyn,4);
      dyn[0].year := 2000;
      dyn[0].tzi.bias := 3600;
      dyn[1].year := 2003;
      dyn[1].tzi.bias := 3601;
      dyn[2].year := 2005;
      dyn[2].tzi.bias := 3602;
      dyn[3].year := 2006;
      dyn[3].tzi.bias := 3603;
    end;
    testBias(1990,3600);
    testBias(2000,3600);
    testBias(2001,3600);
    testBias(2002,3600);
    testBias(2003,3601);
    testBias(2004,3601);
    testBias(2005,3602);
    testBias(2006,3603);
    testBias(2007,3603);
    testBias(2008,3603);
  finally
    tz.Free;
  end;
  dt := NowUTC;
  {$ifdef FPC}
  CheckSame(_LocalTimeToUniversal(Now(), - GetLocalTimeOffset) - dt, 0, 1E-2,
    'NowUTC should not shift or truncate time');
  {$endif}
  sleep(200);
  Check(not SameValue(dt,NowUTC), 'NowUTC should not truncate time to 5 sec resolution');
  {$ifdef MSWINDOWS}
  tz := TSynTimeZone.CreateDefault;
  try
    local := tz.UtcToLocal(dt,'UTC');
    check(SameValue(local,dt));
    check(tz.GetBiasForDateTime(dt,'UTC',bias,hdl));
    check(bias=0);
    check(not hdl);
    local := tz.UtcToLocal(dt,'Romance Standard Time');
    check(not SameValue(local,dt),'Paris never aligns with London');
    check(tz.GetBiasForDateTime(dt,'Romance Standard Time',bias,hdl));
    check(hdl);
    check(bias<0);
    buf := tz.SaveToBuffer;
  finally
    tz.Free;
  end;
  tz := TSynTimeZone.Create;
  try
    tz.LoadFromBuffer(buf);
    CheckSame(local,tz.UtcToLocal(dt,'Romance Standard Time'));
  finally
    tz.Free;
  end;
  {$endif}
end;

{$IFDEF FPC} {$PUSH} {$ENDIF} {$HINTS OFF}
// [dcc64 Hint] H2135 FOR or WHILE loop executes zero times - deleted
procedure TTestLowLevelCommon._IdemPropName;
  function IPNUSL(const s1,s2: RawUTF8; len: integer): boolean;
  begin
    result := IdemPropNameUSameLen(pointer(s1),pointer(s2),len);
  end;
const abcde: PUTF8Char = 'ABcdE';
      abcdf: PUTF8Char = 'abCDF';
      zbcde: PUTF8Char = 'zBcdE';
      edf:   PUTF8Char = '$a_bc[0]edfghij';
      eda:   PUTF8Char = '$a_bc[0]"edfghij';
var WinAnsi: WinAnsiString;
    i: integer;
begin
  Check(IdemPropName('a','A'));
  Check(not IdemPropName('a','z'));
  Check(IdemPropName('ab','AB'));
  Check(IdemPropName('abc','ABc'));
  Check(IdemPropName('abcD','ABcd'));
  Check(not IdemPropName('abcD','ABcF'));
  Check(not IdemPropName('abcD','ABcFG'));
  Check(not IdemPropName('abcDe','ABcFG'));
  Check(IdemPropName('abcDe','ABcdE'));
  Check(not IdemPropName('abcDef','ABcdEe'));
  Check(IdemPropName('abcDeF','ABcdEF'));
  Check(IdemPropName('ABCDEF','ABCDEF'));
  Check(not IdemPropName('abcD',''));
  Check(not IdemPropName('','ABcFG'));
  Check(IdemPropName('',''));
  Check(IdemPropNameU('a','A'));
  Check(not IdemPropNameU('a','z'));
  Check(IdemPropNameU('ab','AB'));
  Check(not IdemPropNameU('abc','ABz'));
  Check(not IdemPropNameU('zbc','abc'));
  Check(IdemPropNameU('abc','ABc'));
  Check(IdemPropNameU('abcD','ABcd'));
  Check(not IdemPropNameU('abcD','ABcF'));
  Check(not IdemPropNameU('abcD','ABcFG'));
  Check(not IdemPropNameU('abcDe','ABcFG'));
  Check(IdemPropNameU('abcDe','ABcdE'));
  Check(not IdemPropNameU('abcDef','ABcdEe'));
  Check(IdemPropNameU('abcDeF','ABcdEF'));
  Check(IdemPropNameU('ABCDEF','ABCDEF'));
  Check(not IdemPropNameU('abcD',''));
  Check(not IdemPropNameU('','ABcFG'));
  for i := 0 to 100 do
    Check(IdemPropNameU(RawUTF8(StringOfChar('a',i)),RawUTF8(StringOfChar('A',i))));
  Check(UpperCaseU('abcd')='ABCD');
  Check(IdemPropNameU('abcDe',abcde,5));
  Check(not IdemPropNameU('abcD',abcde,5));
  Check(not IdemPropNameU('abcDF',abcde,5));
  {$ifndef DELPHI5OROLDER}
  Check(IdemPropName(abcde,abcde,4,4));
  Check(IdemPropName(abcde,abcde,5,5));
  Check(not IdemPropName(abcde,abcde,4,5));
  Check(not IdemPropName(abcde,abcdf,5,5));
  {$endif DELPHI5OROLDER}
  Check(not IPNUSL('abcD','ABcF',4));
  Check(not IPNUSL('abcD','ABcFG',4));
  Check(IPNUSL('abcDe','ABcdE',5));
  Check(IPNUSL('ABcdE','abCDF',0));
  Check(IPNUSL('ABcdE','',0));
  Check(IPNUSL('','abCDF',0));
  Check(IdemPropNameUSameLen(abcde,abcdf,1));
  Check(IdemPropNameUSameLen(abcde,abcdf,2));
  Check(IdemPropNameUSameLen(abcde,abcdf,3));
  Check(IdemPropNameUSameLen(abcde,abcdf,4));
  Check(not IdemPropNameUSameLen(abcde,abcdf,5));
  Check(IdemPropNameUSameLen(abcde,zbcde,0));
  Check(not IdemPropNameUSameLen(abcde,zbcde,1));
  Check(not IdemPropNameUSameLen(abcde,zbcde,2));
  Check(not IdemPropNameUSameLen(abcde,zbcde,3));
  Check(not IdemPropNameUSameLen(abcde,zbcde,4));
  Check(not IdemPropNameUSameLen(abcde,zbcde,5));
  Check(FindRawUTF8(['a','bb','cc'],'a')=0);
  Check(FindRawUTF8(['a','bb','cc'],'cc')=2);
  Check(FindRawUTF8(['a','bb','cc'],'ab')=-1);
  Check(FindRawUTF8(['a','bb','cc'],'A')=-1);
  Check(FindRawUTF8(['a','bb','cc'],'A',false)=0);
  Check(FindPropName(['a','bb','cc'],'A')=0);
  Check(FindPropName(['a','bb','cc'],'cC')=2);
  Check(FindPropName(['a','bb','cc'],'ab')=-1);
  WinAnsi := 'aecD';
  WinAnsi[2] := #$E9;
  WinAnsi[3] := #$E7;
  Check(UpperCaseU(WinAnsiToUTF8(WinAnsi))='AECD');
  check(not JsonPropNameValid(nil));
  check(not JsonPropNameValid(@edf[15]));
  for i := 14 downto 0 do
    check(JsonPropNameValid(@edf[i])<>(i in [5,7]));
  for i := 15 downto 0 do
    check(JsonPropNameValid(@eda[i])=(i>8));
  Check(PosCharAny('ABC','z')=nil);
  Check(PosCharAny('ABC','A')^='A');
  Check(PosCharAny('ABC','B')^='B');
  Check(PosCharAny('ABC','C')^='C');
  Check(PosCharAny('ABC','az')=nil);
  Check(PosCharAny('ABC','aA')^='A');
  Check(PosCharAny('ABC','bB')^='B');
  Check(PosCharAny('ABC','cC')^='C');
  Check(PosExChar('z','')=0,'ABC');
  Check(PosExChar('z','A')=0,'ABC');
  Check(PosExChar('z','ABC')=0,'ABC');
  Check(PosExChar('A','A')=1,'ABC');
  Check(PosExChar('A','AB')=1,'ABC');
  Check(PosExChar('A','ABC')=1,'ABC');
  Check(PosExChar('B','ABC')=2,'ABC');
  Check(PosExChar('B','AB')=2,'ABC');
  Check(PosExChar('C','ABC')=3,'ABC');
end;
{$IFDEF FPC} {$POP} {$ELSE} {$HINTS ON} {$ENDIF}

procedure TTestLowLevelCommon._TSynTable;
var T: TSynTable;
procedure Test;
begin
  Check(T.Field[0].Name='currency');
  Check(T.Field[0].Offset=0);
  Check(T.Field[1].Name='double');
  Check(T.Field[1].Offset=8);
  Check(T.Field[2].Name='bool');
  Check(T.Field[2].Offset=16);
  Check(T.FieldVariableOffset=17);
  Check(T.FieldFromName['TEXT'].Offset=-1);
  Check(T.FieldFromName['text'].FieldNumber=3);
  Check(tfoIndex in T.Field[3].Options);
  Check(T.FieldFromName['VARint'].Name='varint');
  Check(T.FieldFromName['VARint'].Name='varint');
  Check(T.FieldFromName['VARint'].FieldNumber=4);
  Check(T.Field[4].Options=[]);
  Check(T.FieldFromName['ansi'].Offset=-3);
  Check(T.FieldFromName['ansi'].FieldNumber=5);
end;
var W: TFileBufferWriter;
    R: TFileBufferReader;
    f: THandle;
    FN: TFileName;
    {$ifndef NOVARIANTS}
    data: TSynTableData;
    rec: Variant;
    i: integer;
    V: double;
    u: SynUnicode;
    a: RawUTF8;
    {$endif NOVARIANTS}
begin
  T := TSynTable.Create('Test');
  try
    Check(T.AddField('One',tftUnknown)=nil);
    Check(T.AddField('bool',tftBoolean)<>nil);
    Check(T.AddField('bool',tftBoolean)=nil);
    Check(T.AddField('double',tftDouble)<>nil);
    Check(T.AddField('varint',tftVarUInt32)<>nil);
    Check(T.AddField('text',tftUTF8,[tfoUnique])<>nil);
    Check(T.AddField('ansi',tftWinAnsi,[])<>nil);
    Check(T.AddField('currency',tftCurrency)<>nil);
    Test;
    FN := ChangeFileExt(ExeVersion.ProgramFileName,'.syntable');
    DeleteFile(FN);
    W := TFileBufferWriter.Create(FN); // manual storage of TSynTable header
    try
      T.SaveTo(W);
      W.Flush;
    finally
      W.Free;
    end;
    T.Free;
    f := FileOpen(FN,fmOpenRead);
    R.Open(f);
    Check(R.Seek(0));
    T := TSynTable.Create('Test');
    T.LoadFrom(R);
    R.Close;
    Test;
    {$ifndef NOVARIANTS}
    try
      // test TSynTableData
      data.Init(T);
      check(data.Field['ID']=0);
      data.Field['ID'] := 1;
      check(data.Field['ID']=1);
      check(data.Field['bool']=false);
      data.Field['bool'] := 12;
      check(data.Field['bool']=true);
      check(data.Field['varint']=0);
      check(data.Field['double']=0.0);
      data.Field['varint'] := 100;
      check(data.Field['varint']=100);
      data.Field['double'] := 3.1415;
      CheckSame(data.Field['double'],3.1415);
      for i := 1 to 100 do begin
        u := RandomUnicode(i*2);
        data.Field['text'] := u;
        check(SynUnicode(data.Field['text'])=u);
        a := RandomAnsi7(i*2);
        data.Field['ansi'] := a;
        check(SynUnicode(data.Field['ansi'])=SynUnicode(a));
        // here, ansi is more efficent than text for storage size
      end;
      check(data.Field['bool']=true);
      check(data.Field['varint']=100);
      check(data.Field['ID']=1);
      CheckSame(data.Field['double'],3.1415);
      for i := 1 to 100 do begin
        data.Field['varint'] := i shl 6;
        Check(data.Field['varint']=i shl 6,'varlength');
        V := random;
        data.Field['double'] := V;
        CheckSame(data.Field['double'],V);
      end;
      check(data.Field['bool']=true);
      check(SynUnicode(data.Field['text'])=u);
      check(SynUnicode(data.Field['ansi'])=SynUnicode(a));
      check(data.Field['ID']=1);
      // test TSynTableVariantType
      rec := T.Data;
      check(rec.ID=0);
      rec.ID := 1;
      check(rec.ID=1);
      check(rec.bool=false);
      rec.bool := 12;
      check(rec.bool=true);
      rec.bool := false;
      check(rec.bool=false);
      rec.bool := true;
      check(rec.bool=true);
      check(rec.varint=0);
      check(rec.double=0.0);
      rec.varint := 100;
      check(rec.varint=100);
      rec.double := 3.141592654;
      CheckSame(rec.double,3.141592654);
      for i := 1 to 100 do begin
        a := RandomAnsi7(i*2);
        u := SynUnicode(a);
        rec.text := a;
        check(SynUnicode(rec.text)=u,'rec.text');
        rec.ansi := a;
        check(SynUnicode(rec.ansi)=u,'rec.ansi');
      end;
      check(rec.bool=true,'rec.bool');
      check(rec.varint=100);
      check(rec.ID=1);
      CheckSame(rec.double,3.141592654);
      for i := 1 to 100 do begin
        rec.varint := i shl 6;
        Check(rec.varint=i shl 6,'varlength');
        V := random;
        rec.double := V;
        CheckSame(rec.double,V);
      end;
      check(rec.bool=true);
      check(SynUnicode(rec.text)=u);
      check(SynUnicode(rec.ansi)=u);
      check(rec.ID=1);
    except
      on E: Exception do // variant error could raise exceptions
        Check(false,E.Message);
    end;
    {$endif NOVARIANTS}
    FileClose(f);
  finally
    T.Free;
  end;
end;

procedure TTestLowLevelCommon._TSynCache;
var C: TSynCache;
    s,v: RawUTF8;
    i: integer;
    Tag: PtrInt;
begin
   C := TSynCache.Create;
  try
    for i := 0 to 100 do begin
      v := {$ifndef ENHANCEDRTL}Int32ToUtf8{$else}IntToStr{$endif}(i);
      Tag := 0;
      s := C.Find(v,@Tag);
      Check(s='');
      Check(Tag=0);
      C.Add(v+v,i);
    end;
    for i := 0 to 100 do begin
      v := {$ifndef ENHANCEDRTL}Int32ToUtf8{$else}IntToStr{$endif}(i);
      Check(C.Find(v,@Tag)=v+v);
      Check(Tag=i);
    end;
  finally
    C.Free;
  end;
end;

procedure TTestLowLevelCommon._TSynFilter;
type TFilterProcess = function(const Value: RawUTF8): RawUTF8;
procedure Test(Filter: TSynFilterClass; Proc: TFilterProcess);
var V, Old: RawUTF8;
    i: integer;
begin
  with Filter.Create do
  try
    for i := 0 to 200 do begin
      V := RandomUTF8(i);
      Old := V;
      Process(0,V);
      Check(V=Proc(Old));
    end;
  finally
    Free;
  end;
end;
begin
  {$ifndef PUREPASCAL}
  {$ifndef ENHANCEDRTL}
  {$ifndef LVCL}
  {$ifndef FPC}
  Test(TSynFilterTrim,SynCommons.Trim);
  {$endif}
  {$endif}
  {$endif}
  {$endif}
  Test(TSynFilterLowerCase,LowerCase);
  Test(TSynFilterUpperCase,UpperCase);
  Test(TSynFilterLowerCaseU,LowerCaseU);
  Test(TSynFilterUpperCaseU,UpperCaseU);
end;

procedure TTestLowLevelCommon._TSynValidate;
procedure TestValidateLength(const Params: RawUTF8; aMin,aMax: cardinal);
var i: cardinal;
    V: RawUTF8;
    Msg: string;
    ok: boolean;
    valid: TSynValidateText;
begin
  valid := TSynValidateText.Create(Params);
  try
    Check(valid.MinLength=aMin);
    Check(valid.MaxLength=aMax);
    for i := 0 to 100 do begin
      V := RandomUTF8(i);
      Check(Utf8ToUnicodeLength(pointer(V))=i,'Unicode glyph=Ansi char=i');
      Msg := '';
      ok := (i>=aMin)and(i<=aMax);
      Check(valid.Process(0,V,Msg)=ok,Msg);
      Check(Msg=''=ok,Msg);
    end;
  finally
    valid.Free;
  end;
end;
var Msg: string;
begin
  with TSynValidateIPAddress.Create do
  try
    Check(Process(0,'192.168.1.1',Msg));
    Check(Msg='');
    Msg := '';
    Check(not Process(0,' 192.168.1.1',Msg));
    Check(Msg<>'');
    Msg := '';
    Check(not Process(0,'292.168.1.1',Msg));
    Check(Msg<>'');
    Msg := '';
    Check(Process(0,'192.168.001.001',Msg));
    Check(Msg='');
  finally
    Free;
  end;
  with TSynValidateEmail.Create do
  try
    Msg := '';
    Check(Process(0,'test@synopse.info',Msg));
    Check(Msg='');
    Msg := '';
    Check(not Process(0,'test@ synopse.info',Msg));
    Check(Msg<>'');
    Msg := '';
    Check(not Process(0,'test@synopse.delphi',Msg));
    Check(Msg<>'');
    Msg := '';
    Check(Process(0,'test_two@blog.synopse.info',Msg));
    Check(Msg='');
    Msg := '';
    Check(Process(0,'test_two@blog.synopse.fr',Msg));
    Check(Msg='');
  finally
    Free;
  end;
  with TSynValidateEmail.Create('{"ForbiddenDomains":"google.fr,synopse.info"}') do
  try
    Msg := '';
    Check(Process(0,'test@blog.synopse.fr',Msg));
    Check(Process(0,'test@blog.synopse.info',Msg));
    Check(not Process(0,'test@synopse.info',Msg));
    Msg := '';
    Check(Process(0,'test@blog.google.fr',Msg));
    Check(not Process(0,'test@google.fr',Msg));
  finally
    Free;
  end;
  with TSynValidateEmail.Create('{"AllowedTLD":"com,org,net","ForbiddenTLD":"net"}') do
  try
    Msg := '';
    Check(Process(0,'test@synopse.com',Msg));
    Check(Msg='');
    Msg := '';
    Check(not Process(0,'test@ synopse.com',Msg));
    Check(Msg<>'');
    Msg := '';
    Check(not Process(0,'test@synopse.info',Msg));
    Check(Msg<>'');
    Msg := '';
    Check(not Process(0,'test_two@blog.synopse.net',Msg));
    Check(Msg<>'');
    Msg := '';
    Check(not Process(0,'test_two@blog.synopse.fr',Msg));
    Check(Msg<>'');
  finally
    Free;
  end;
  with TSynValidatePattern.Create('this [e-n]s a [!zy]est') do
  try
    Msg := '';
    Check(Process(0,'this is a test',Msg));
    Check(Msg='');
    Msg := '';
    Check(Process(0,'this is a rest',Msg));
    Check(Msg='');
    Msg := '';
    Check(not Process(0,'this is a zest',Msg));
    Check(Msg<>'');
    Msg := '';
    Check(not Process(0,'this as a test',Msg));
    Check(Msg<>'');
    Msg := '';
    Check(not Process(0,'this as a rest',Msg));
    Check(Msg<>'');
  finally
    Free;
  end;
  TestValidateLength('',1,maxInt);
  TestValidateLength('{"mAXlength": 10 , "MInLENgtH" : 3 }',3,10);
  with TSynValidateText.Create do
  try
    Msg := '';
    MaxLeftTrimCount := 0;
    Check(Process(0,'one',Msg));
    Check(not Process(0,' one',Msg));
    MaxRightTrimCount := 0;
    Check(Process(0,'one',Msg));
    Check(not Process(0,' one',Msg));
    Check(not Process(0,'one ',Msg));
    Msg:= '';
    MinAlphaCount := 3;
    Check(Process(0,'one',Msg));
    Check(not Process(0,'on2',Msg));
    Msg := '';
    MinDigitCount := 2;
    Check(Process(0,'one12',Msg));
    Check(not Process(0,'one2',Msg));
    Msg := '';
    MinPunctCount := 1;
    Check(Process(0,'one12_',Msg));
    Check(Process(0,'_one12_',Msg));
    Check(Process(0,'_one12',Msg));
    Check(not Process(0,'one12',Msg));
    Msg := '';
    MinLowerCount := 3;
    Check(Process(0,'o12_ne',Msg));
    Check(not Process(0,'o12_An',Msg));
    Msg := '';
    MinUpperCount := 3;
    Check(Process(0,'o12_neABC',Msg));
    Check(not Process(0,'o12_AnBc',Msg));
    Msg := '';
    MinSpaceCount := 3;
    Check(Process(0,'o12 _ne AB C',Msg));
    Check(not Process(0,'O1 2_A neeB',Msg));
    Msg := '';
    MaxSpaceCount := 3;
    Check(Process(0,'o12 _ne AB C',Msg));
    Check(not Process(0,'o12 _ ne AB C',Msg));
  finally
    Free;
  end;
  with TSynValidatePassword.Create do
  try
    Msg := '';
    Check(Process(0,'aA3!Z',Msg));
    Check(not Process(0,'aA3!',Msg));
    Msg := '';
    Check(not Process(0,'aA 3!Z',Msg));
  finally
    Free;
  end;
end;

procedure TTestLowLevelCommon.UrlDecoding;
var i, V: integer;
    s,t,d: RawUTF8;
    U: PUTF8Char;
begin
  for i := 1 to 100 do begin
    s := DateTimeToIso8601(Now/20+Random*20,true);
    t := UrlEncode(s);
    Check(UrlDecode(t)=s);
    d := 'seleCT='+t+'&where='+
      {$ifndef ENHANCEDRTL}Int32ToUtf8{$else}IntToStr{$endif}(i);
    Check(UrlDecodeNeedParameters(pointer(d),'where,select'));
    Check(not UrlDecodeNeedParameters(pointer(d),'foo,select'));
    Check(UrlDecodeValue(pointer(d),'SELECT=',t,@U));
    Check(t=s,'UrlDecodeValue');
    Check(IdemPChar(U,'WHERE='),'Where');
    Check(UrlDecodeInteger(U,'WHERE=',V));
    Check(V=i);
    Check(not UrlDecodeValue(pointer(d),'NOTFOUND=',t,@U));
    Check(UrlDecodeInteger(U,'WHERE=',V,@U));
    Check(U=nil);
  end;
  s := '{"b":30,"a":"toto"}'; // temp read-only var for proper overload call 
  CheckEqual(UrlEncodeJsonObject('',s,[]),'?b=30&a=toto');
end;

procedure TTestLowLevelCommon.MimeTypes;
const
  MIMES: array[0..49] of TFileName = (
   'png','image/png',
   'PNg','image/png',
   'gif','image/gif',
   'tif','image/tiff',
   'tiff','image/tiff',
   'jpg','image/jpeg',
   'JPG','image/jpeg',
   'jpeg','image/jpeg',
   'bmp','image/bmp',
   'doc','application/msword',
   'docx','application/msword',
   'htm',HTML_CONTENT_TYPE,
   'html',HTML_CONTENT_TYPE,
   'HTML',HTML_CONTENT_TYPE,
   'css','text/css',
   'js','application/javascript',
   'ico','image/x-icon',
   'pdf','application/pdf',
   'PDF','application/pdf',
   'Json',JSON_CONTENT_TYPE,
   'webp','image/webp',
   'manifest','text/cache-manifest',
   'appcache','text/cache-manifest',
   'h264','video/H264',
   'ogg','video/ogg');
  BIN: array[0..1] of Cardinal = (
    $04034B50,$38464947);
  BIN_MIME: array[0..1] of RawUTF8 = (
    'application/zip','image/gif');
var i: integer;
begin
  CheckEqual(GetMimeContentType(nil,0,'toto.h264'),'video/H264');
  for i := 0 to high(MIMES)shr 1 do
    CheckEqual(GetMimeContentType(nil,0,'toto.'+MIMES[i*2]),ToUTF8(MIMES[i*2+1]));
  for i := 0 to high(BIN) do begin
    CheckEqual(GetMimeContentType(@BIN[i],34,''),BIN_MIME[i]);
    CheckEqual(GetMimeContentTypeFromBuffer(@BIN[i],34,''),BIN_MIME[i]);
  end;
end;

function TTestLowLevelCommon.QuickSelectGT(IndexA,IndexB: PtrInt): boolean;
begin
  result := fQuickSelectValues[IndexA]>fQuickSelectValues[IndexB];
end;

procedure TTestLowLevelCommon.QuickSelect;
  function Median(const CSV: RawUTF8; Expected: integer): integer;
  var IDA: TIntegerDynArray;
  begin
    CSVToIntegerDynArray(pointer(CSV),IDA);
    result := MedianQuickSelectInteger(pointer(IDA),length(IDA));
    Check(result=Expected);
  end;
var n,i,med2,med1,len: integer;
    tmp: TSynTempBuffer;
    P: PIntegerArray;
begin
  Median('',0);
  Median('2',2);
  Median('3,5,12',5);
  Median('12,3,5',5);
  Median('19,10,84,11,23',19);
  Median('1,3,3,6,7,8,9',6);
  Median('1,2,3,4,5,6,8,9',4);
  Median('3,5,7,12,13,14,21,23,23,23,23,29,39,40,56',23);
  Median('3,13,7,5,21,23,39,23,40,23,14,12,56,23,29',23);
  Median('3,5,7,12,13,14,21,23,23,23,23,29,40,56',21);
  Median('3,13,7,5,21,23,23,40,23,14,12,56,23,29',21);
  for n := 0 to 1000 do begin
    len := n*2+1;
    SetLength(fQuickSelectValues,len);
    P := pointer(fQuickSelectValues);
    FillIncreasing(P,1,len);
    med1 := MedianQuickSelect(QuickSelectGT,len,tmp);
    Check(fQuickSelectValues[med1]=n+1);
    Check(MedianQuickSelectInteger(P,len)=n+1);
    for i := 0 to high(fQuickSelectValues) do
      fQuickSelectValues[i] := Random(MaxInt);
    med1 := fQuickSelectValues[MedianQuickSelect(QuickSelectGT,len,tmp)];
    med2 := MedianQuickSelectInteger(P,len);
    Check(med1=med2);
    QuickSortInteger(P,0,len-1);
    check(med2=fQuickSelectValues[n]);
  end;
end;

procedure TTestLowLevelCommon._TSynLogFile;
procedure Test(const LOG: RawUTF8; ExpectedDate: TDateTime);
var L: TSynLogFile;
begin
  L := TSynLogFile.Create(pointer(LOG),length(LOG));
  try
    Check(L.ExecutableName='D:\Dev\lib\SQLite3\exe\TestSQL3.exe');
    Check(L.ExecutableVersion='1.2.3.4');
    if trunc(ExpectedDate)=40640 then
      Check(L.InstanceName='D:\Dev\MyLibrary.dll') else
      Check(L.InstanceName='');
    CheckSame(L.ExecutableDate,ExpectedDate,1/SecsPerDay);
    Check(L.ComputerHost='MyPC');
    Check(L.LevelUsed=[sllEnter,sllLeave,sllDebug]);
    Check(L.RunningUser='MySelf');
    Check(L.CPU='2*0-15-1027');
    {$ifdef MSWINDOWS}
    Check(L.OS=wXP);
    Check(L.ServicePack=3);
    Check(not L.Wow64);
    {$endif}
    Check(L.Freq=0);
    CheckSame(L.StartDateTime,40640.502882,1/SecsPerDay);
    if CheckFailed(L.Count=3) then
      exit;
    Check(L.EventLevel[0]=sllEnter);
    Check(L.EventLevel[1]=sllDebug);
    CheckSame(L.EventDateTime(1),L.StartDateTime,1/SecsPerDay);
    Check(L.EventLevel[2]=sllLeave);
    if CheckFailed(L.LogProcCount=1) then
      exit;
    Check(L.LogProc[0].Index=0);
    Check(L.LogProc[0].Time=10020006);
  finally
    L.Free;
  end;
end;
var tmp: array[0..512] of AnsiChar;
    msg: RawUTF8;
    len: integer;
begin
  FillcharFast(tmp,sizeof(tmp),1);
  len := SyslogMessage(sfAuth,ssCrit,'test','','',tmp,sizeof(tmp),false);
  // Check(len=65); // <-- different for every PC, due to PC name differences
  tmp[len] := #0;
  Check(IdemPChar(PUTF8Char(@tmp),PAnsiChar('<34>1 ')));
  Check(PosEx(' - - - test',tmp)=len-10);
  msg := RawUTF8(StringOfChar('+',300));
  len := SyslogMessage(sfLocal4,ssNotice,msg,'proc','msg',tmp,300,false);
  Check(IdemPChar(PUTF8Char(@tmp),PAnsiChar('<165>1 ')));
  Check(PosEx(' proc msg - ++++',tmp)>1);
  Check(len<300,'truncated to avoid buffer overflow');
  Check(tmp[len-1]='+');
  Check(tmp[len]=#1);
  Test('D:\Dev\lib\SQLite3\exe\TestSQL3.exe 1.2.3.4 (2011-04-07 11:09:06)'#13#10+
    'Host=MyPC User=MySelf CPU=2*0-15-1027 OS=2.3=5.1.2600 Wow64=0 Freq=3579545 '+
    'Instance=D:\Dev\MyLibrary.dll'#13#10+
    'TSynLog 1.15 LVCL 2011-04-07 12:04:09'#13#10#13#10+
    '20110407 12040903  +    SQLite3Commons.TSQLRestServer.URI (14163)'#13#10+
    '20110407 12040904 debug {"TObjectList(00AF8D00)":["TObjectList(00AF8D20)",'+
    '"TObjectList(00AF8D60)","TFileVersion(00ADC0B0)","TSynMapFile(00ACC990)"]}'#13#10+
    '20110407 12040915  -    SQLite3Commons.TSQLRestServer.URI (14163) 10.020.006',
    40640.464653);
  Test('D:\Dev\lib\SQLite3\exe\TestSQL3.exe 1.2.3.4 (2011-04-08 11:09:06)'#13#10+
    'Host=MyPC User=MySelf CPU=2*0-15-1027 OS=2.3=5.1.2600 Wow64=0 Freq=3579545'#13#10+
    'TSynLog 1.15 LVCL 2011-04-07 12:04:09'#13#10#13#10+
    '20110407 12040903  +    SQLite3Commons.TSQLRestServer.URI (14163)'#13#10+
    '20110407 12040904 debug {"TObjectList(00AF8D00)":["TObjectList(00AF8D20)",'+
    '"TObjectList(00AF8D60)","TFileVersion(00ADC0B0)","TSynMapFile(00ACC990)"]}'#13#10+
    '20110407 12040915  -    SQLite3Commons.TSQLRestServer.URI (14163) 10.020.006',
    40641.464653);
end;

procedure TTestLowLevelCommon._TSynNameValue;
const MAX=10000;
var nv: TSynNameValue;
    i: integer;
    tmp: TSynTempBuffer;
begin
  nv.Init(false);
  check(nv.Count=0);
  for i := 1 to MAX do
    nv.Add(UInt32ToUtf8(i),UInt32ToUtf8(i+MAX));
  check(nv.Count=MAX);
  for i := 1 to MAX do
    check(nv.Find(UInt32ToUtf8(i))=i-1);
  for i := MAX+1 to MAX*2 do
    check(nv.Find(UInt32ToUtf8(i))<0);
  for i := 1 to MAX do
    check(nv.Value(UInt32ToUtf8(i))=UInt32ToUtf8(i+MAX));
  for i := 1 to MAX do
    check(nv.Str[UInt32ToUtf8(i)]=UInt32ToUtf8(i+MAX));
  nv.InitFromNamesValues(['a','b'],['1','be']);
  check(nv.Count=2);
  check(nv.Str['a']='1');
  check(nv.Str['b']='be');
  check(nv.Str['c']='');
  check(nv.ValueInt('a')=1);
  check(nv.ValueInt('b')=0);
  check(nv.ValueInt('c')=0);
  check(nv.AsCSV('=',';')='a=1;b=be;');
  check(nv.AsJSON='{"a":"1","b":"be"}');
  tmp.Init('{a:10,b:"bee"}');
  check(nv.InitFromJSON(tmp.buf));
  check(nv.Count=2);
  check(nv.Str['a']='10');
  check(nv.Str['b']='bee');
  check(nv.Str['c']='');
  check(nv.Int['a']=10);
  check(nv.Int['b']=0);
  check(nv.Int['c']=0);
  check(nv.AsCSV('=',';')='a=10;b=bee;');
  check(nv.AsJSON='{"a":"10","b":"bee"}');
  check(nv.Delete('b'));
  check(nv.ValueInt('a')=10);
  check(nv.Str['b']='');
  check(not nv.Delete('b'));
  check(nv.DeleteByValue('10')=1);
  check(nv.ValueInt('a')=0);
  check(nv.DeleteByValue('10')=0);
  check(nv.Count=0);
  check(nv.AsCSV('=',';')='');
  tmp.Init('{"a":20,b:"bi"]');
  check(not nv.InitFromJSON(tmp.buf));
  check(nv.Count=0);
end;

procedure TTestLowLevelCommon._TObjectListHashed;
const MAX = 1000000;
var obj: TObjectListHashed;
    i: PtrInt;
    added: boolean;
begin
  obj := TObjectListHashed.Create(false);
  try
    //obj.Hash.Capacity := MAX; // we will test hash size growing abilities
    Check(obj.Count=0);
    for i := 0 to MAX do begin
      obj.Add(pointer(i),added);
      check(added);
    end;
    for i := 0 to obj.Count-1 do
      Check(obj.IndexOf(obj.List[i])=i);
    for i := obj.Count-1 downto 0 do
      if i and 255=0 then
        obj.Delete(i); // will invalidate hash, but won't rehash now
     CheckEqual(obj.IndexOf(TObject(255)),254);
     CheckEqual(obj.IndexOf(TObject(256)),-1);
     CheckEqual(obj.IndexOf(TObject(512)),-1);
    for i := 0 to obj.Count-1 do
      Check(obj.IndexOf(obj.List[i])=i); // will rehash after trigger=32
  finally
    obj.Free;
  end;
end;

type
  TSynPersistentStoreList = class(TObjectListSorted)
  protected
    function Compare(Item: TSynPersistentLock; const Value): integer; override;
    function NewItem(const Value): TSynPersistentLock; override;
  end;

function TSynPersistentStoreList.Compare(Item: TSynPersistentLock;
  const Value): integer;
begin
  result := StrComp(pointer(TSynPersistentStore(Item).Name),pointer(Value));
end;

function TSynPersistentStoreList.NewItem(const Value): TSynPersistentLock;
begin
  result := TSynPersistentStore.Create(RawUTF8(Value));
end;

procedure TTestLowLevelCommon._TObjectListSorted;
const MAX = 20000;
var obj: TSynPersistentStoreList;
    i, n: integer;
    v: RawUTF8;
    item: TSynPersistentStore;
    added: boolean;
begin
  obj := TSynPersistentStoreList.Create;
  try
    n := 0;
    Check(obj.Count=0);
    for i := 1 to MAX do begin
      UInt32ToUtf8(Random32 shr 10,v);
      item := obj.FindOrAddLocked(v,added);
      Check(item<>nil);
      Check(item.Name=v);
      if added then
        inc(n);
      item.Safe.UnLock;
    end;
    Check(obj.Count=n);
    for i := 0 to obj.Count-1 do begin
      item := obj.FindLocked(TSynPersistentStore(obj.ObjArray[i]).Name);
      Check(item<>nil);
      Check(pointer(item)=obj.ObjArray[i]);
      item.Safe.UnLock;
    end;
  finally
    obj.Free;
  end;
end;

procedure TTestLowLevelCommon._TSynUniqueIdentifier;
const JAN2015_UNIX = 1420070400;
var gen: TSynUniqueIdentifierGenerator;
    i1,i2: TSynUniqueIdentifierBits;
    i3: TSynUniqueIdentifier;
    i: integer;
    {$ifndef NOVARIANTS}json,{$endif} obfusc: RawUTF8;
begin
  gen := TSynUniqueIdentifierGenerator.Create(10,'toto');
  try
    for i := 1 to 100000 do begin
      gen.ComputeNew(i1);
      gen.ComputeNew(i2);
      check(i1.ProcessID=10);
      check(i2.ProcessID=10);
      check(i1.CreateTimeUnix>JAN2015_UNIX);
      check(i1.CreateTimeUnix<=i2.CreateTimeUnix);
      check(i1.Value<i2.Value);
      {$ifndef NOVARIANTS}
      check(not i1.Equal(i2));
      i2.From(i1.Value);
      check(i1.Equal(i2));
      json := VariantSaveJSON(i1.AsVariant);
      check(VariantSaveJSON(i2.AsVariant)=json);
      check(json=FormatUTF8('{"Created":"%","Identifier":%,"Counter":%,"Value":%,"Hex":"%"}',
        [DateTimeToIso8601Text(i1.CreateDateTime),i1.ProcessID,i1.Counter,i1.Value,Int64ToHex(i1.Value)]));
      {$endif}
      obfusc := gen.ToObfuscated(i1.Value);
      check(gen.FromObfuscated(obfusc,i3));
      check(i1.Value=i3);
      check(Length(obfusc)=24);
      inc(obfusc[12]);
      check(not gen.FromObfuscated(obfusc,i3));
      dec(obfusc[12]);
    end;
  finally
    gen.Free;
  end;
  gen := TSynUniqueIdentifierGenerator.Create(10,'toto');
  try
    i3 := 0;
    check(gen.FromObfuscated(obfusc,i3),'SharedObfuscationKey');
    check(i1.Value=i3);
  finally
    gen.Free;
  end;
end;
procedure TTestLowLevelCommon._TSynDictionary;
type tvalue = {$ifdef NOVARIANTS}integer{$else}variant{$endif};
     tvalues = {$ifdef NOVARIANTS}TIntegerDynArray{$else}TVariantDynArray{$endif};
const MAX = 10000;
var dict: TSynDictionary;
  procedure Test;
  var k: RawUTF8;
      v: tvalue;
      i: integer;
  begin
    check(dict.Count=MAX);
    for i := 1 to MAX do begin
      UInt32ToUTF8(i,k);
      v := 0;
      check(dict.Exists(k));
      check(dict.FindAndCopy(k, v));
      check(v=i);
    end;
  end;
var v: tvalue;
    s, k: RawUTF8;
    i: integer;
    exists: boolean;
begin
  dict := TSynDictionary.Create(TypeInfo(TRawUTF8DynArray),TypeInfo(tvalues));
  try
    for i := 1 to MAX do begin
      UInt32ToUTF8(i,k);
      v := i;
      dict.Add(k,v);
    end;
    Test;
    s := dict.SaveToJSON;
    check(dict.Exists(k));
    dict.DeleteAll;
    check(dict.Count=0);
    check(not dict.Exists(k));
    check(dict.LoadFromJSON(s));
    Test;
    s := dict.SaveToBinary;
  finally
    dict.Free;
  end;
  dict := TSynDictionary.Create(TypeInfo(TRawUTF8DynArray),TypeInfo(tvalues));
  try
    check(dict.LoadFromBinary(s));
    Test;
    for i := MAX downto 1 do
    if i and 127=0 then begin
      UInt32ToUTF8(i,k);
      check(dict.Delete(k)=i-1);
      check(dict.Exists(k)=false);
    end;
    for i := 1 to MAX do begin
      exists := (i and 127)<>0;
      UInt32ToUTF8(i,k);
      check(dict.Exists(k)=exists);
      if exists then begin
        v := 0;
        check(dict.FindAndCopy(k, v));
        check(v=i);
        if i<10000 then begin // FindKeyFromValue() brute force is slow
          k := '';
          check(dict.FindKeyFromValue(v,k));
          check(GetInteger(pointer(k))=i);
        end;
      end;
    end;
  finally
    dict.Free;
  end;
end;

procedure TTestLowLevelCommon._TSynQueue;
var o,i,j,k,n: integer;
    f: TSynQueue;
    u,v: RawUTF8;
    savedint: TIntegerDynArray;
    savedu: TRawUTF8DynArray;
begin
  f := TSynQueue.Create(TypeInfo(TIntegerDynArray));
  try
    for o := 1 to 1000 do begin
      check(f.Count=0);
      check(not f.Pending);
      for i := 1 to o do
        f.Push(i);
      check(f.Pending);
      check(f.Count=o);
      check(f.Capacity>=o);
      f.Save(savedint);
      check(Length(savedint)=o);
      for i := 1 to o do begin
        j := -1;
        check(f.Peek(j));
        check(j=i);
        j := -1;
        check(f.Pop(j));
        check(j=i);
      end;
      check(not f.Pending);
      check(f.Count=0);
      check(f.Capacity>0);
      f.Clear; // ensure f.Pop(j) will use leading storage
      check(not f.Pending);
      check(f.Count=0);
      check(f.Capacity=0);
      check(Length(savedint)=o);
      for i := 1 to o do
        check(savedint[i-1]=i);
      n := 0;
      for i := 1 to o do
        if i and 7=0 then begin
          j := -1;
          check(f.Pop(j));
          check(j and 7<>0);
          dec(n);
        end else begin
          f.Push(i);
          inc(n);
        end;
      check(f.Count=n);
      check(f.Pending);
      f.Save(savedint);
      check(Length(savedint)=n);
      for i := 1 to n do
        check(savedint[i-1] and 7<>0);
      for i := 1 to n do begin
        j := -1;
        check(f.Peek(j));
        k := -1;
        check(f.Pop(k));
        check(j=k);
        check(j and 7<>0);
      end;
      check(f.Count=0);
      check(f.Capacity>0);
    end;
  finally
    f.Free;
  end;
  f := TSynQueue.Create(TypeInfo(TRawUTF8DynArray));
  try
    for o := 1 to 1000 do begin
      check(not f.Pending);
      check(f.Count=0);
      f.Clear; // ensure f.Pop(j) will use leading storage
      check(f.Count=0);
      check(f.Capacity=0);
      n := 0;
      for i := 1 to o do
        if i and 7=0 then begin
          u := '7';
          check(f.Pop(u));
          check(GetInteger(pointer(u)) and 7<>0);
          dec(n);
        end else begin
          u := UInt32ToUtf8(i);
          f.Push(u);
          inc(n);
        end;
      check(f.Pending);
      check(f.Count=n);
      f.Save(savedu);
      check(Length(savedu)=n);
      for i := 1 to n do
        check(GetInteger(pointer(savedu[i-1])) and 7<>0);
      for i := 1 to n do begin
        u := '';
        check(f.Peek(u));
        v := '';
        check(f.Pop(v));
        check(u=v);
        check(GetInteger(pointer(u)) and 7<>0);
      end;
      check(not f.Pending);
      check(f.Count=0);
      check(f.Capacity>0);
    end;
    check(Length(savedu)=length(savedint));
  finally
    f.Free;
  end;
end;

procedure TTestLowLevelCommon._DeltaCompress;
var o,n,d,s: RawByteString;
    i: integer;
begin
  n := RandomTextParagraph(100);
  d := DeltaCompress(n,o);
  check(DeltaExtract(d,o,s)=dsSuccess,'delta0');
  Check(s=n);
  d := DeltaCompress(n,s);
  check(d='=');
  for i := 1 to 20 do begin
    o := n;
    s := RandomTextParagraph(100);
    case i and 7 of
      2: n := n+s;
      7: n := s+n;
      else insert(s,n,i*50);
    end;
    d := DeltaCompress(n,o);
    check(d<>'=');
    check(length(d)<length(s));
    check(DeltaExtract(d,o,s)=dsSuccess,'delta+');
    Check(s=n);
  end;
  o := n;
  delete(n,100,100);
  d := DeltaCompress(n,o);
  check(DeltaExtract(d,o,s)=dsSuccess,'delta-');
  Check(s=n);
  o := n;
  delete(n,1000,100);
  insert(RandomIdentifier(50),n,200);
  d := DeltaCompress(n,o);
  check(DeltaExtract(d,o,s)=dsSuccess,'delta-+');
  Check(s=n);
end;

procedure TTestLowLevelCommon.BloomFilters;
const SIZ=200000;
var b: TSynBloomFilter;
    d1,d2: TSynBloomFilterDiff;
    i,j,n: integer;
    falsepositive: double;
    sav1000, savSIZ: RawByteString;
begin
  b := TSynBloomFilter.Create(SIZ+5000);
  try
    CheckSame(b.FalsePositivePercent,1);
    Check(b.Size=SIZ+5000);
    Check(b.Bits>b.Size shl 3);
    Check(b.HashFunctions=7);
    Check(b.Inserted=0);
    CheckLogTimeStart;
    for i := 1 to SIZ do
      Check(not b.MayExist(@i,sizeof(i)));
    CheckLogTime(b.Inserted=0,'MayExists(%)=false',[SIZ]);
    for i := 1 to 1000 do
      b.Insert(@i,sizeof(i));
    CheckLogTime(b.Inserted=1000,'Insert(%)',[b.Inserted]);
    sav1000 := b.SaveTo;
    CheckLogTime(sav1000<>'','b.SaveTo(%) len=%',[b.Inserted,kb(sav1000)]);
    for i := 1001 to SIZ do
      b.Insert(@i,sizeof(i));
    CheckLogTime(b.Inserted=SIZ,'Insert(%)',[SIZ-1000]);
    savSIZ := b.SaveTo;
    CheckLogTime(length(savSIZ)>length(sav1000),'b.SaveTo(%) len=%',[SIZ,kb(savSIZ)]);
    for i := 1 to SIZ do
      Check(b.MayExist(@i,sizeof(i)));
    CheckLogTime(b.Inserted=SIZ,'MayExists(%)=true',[SIZ]);
    n := 0;
    for i := SIZ+1 to SIZ+SIZ shr 5 do
      if b.MayExist(@i,sizeof(i)) then
        inc(n);
    falsepositive := (n*100)/(SIZ shr 5);
    CheckLogTime(falsepositive<1,'falsepositive=%',[falsepositive]);
    b.Reset;
    CheckLogTime(b.Inserted=0,'b.Reset',[]);
    for i := 1 to SIZ do
      Check(not b.MayExist(@i,sizeof(i)));
    CheckLogTime(b.Inserted=0,'MayExists(%)=false',[SIZ]);
    CheckLogTime(b.LoadFrom(sav1000),'b.LoadFrom(%)',[1000]);
    for i := 1 to 1000 do
      Check(b.MayExist(@i,sizeof(i)));
    CheckLogTime(b.Inserted=1000,'MayExists(%)=true',[1000]);
  finally
    b.Free;
  end;
  CheckLogTime(true,'b.Free',[]);
  d1 := TSynBloomFilterDiff.Create(savSIZ);
  try
    CheckLogTime(true,'d1 := TSynBloomFilterDiff.Create(%)',[SIZ]);
    CheckSame(d1.FalsePositivePercent,1);
    Check(d1.Size=SIZ+5000);
    Check(d1.Bits>d1.Size shl 3);
    Check(d1.HashFunctions=7);
    for i := 1 to SIZ do
      Check(d1.MayExist(@i,sizeof(i)));
    CheckLogTime(d1.Inserted=SIZ,'MayExists(%)=true',[SIZ]);
    d2 := TSynBloomFilterDiff.Create;
    try
      Check(d2.Revision=0);
      n := SIZ;
      for j := 1 to 3 do begin
        savSiz := d1.SaveToDiff(d2.Revision);
        CheckLogTime(savSiz<>'','d1.SaveToDiff(%) len=%',[d2.Revision,KB(savSiz)]);
        Check(d1.DiffKnownRevision(savSIZ)=d1.Revision);
        Check((d2.Revision=d1.Revision)=(j>1));
        CheckLogTime(d2.LoadFromDiff(savSiz),'d2.LoadFromDiff(%)',[n]);
        Check(d2.Revision=d1.Revision);
        Check(d2.Size=d1.Size);
        for i := 1 to n do
          Check(d2.MayExist(@i,sizeof(i)));
        CheckLogTime(d2.Inserted=cardinal(n),'MayExists(%)=true',[n]);
        for i := n+1 to n+1000 do
          d1.Insert(@i,sizeof(i));
        CheckLogTime(d2.Revision<>d1.Revision,'d1.Insert(%)',[1000]);
        savSiz := d1.SaveToDiff(d2.Revision);
        CheckLogTime(savSiz<>'','d1.SaveToDiff(%) len=%',[d2.Revision,kb(savSiz)]);
        Check(d1.DiffKnownRevision(savSIZ)=d1.Revision);
        Check(d2.Revision<>d1.Revision);
        CheckLogTime(d2.LoadFromDiff(savSiz),'d2.LoadFromDiff(%)',[n]);
        Check(d2.Revision=d1.Revision);
        inc(n,1000);
        for i := 1 to n do
          Check(d2.MayExist(@i,sizeof(i)));
        CheckLogTime(d2.Inserted=cardinal(n),'MayExists(%)=true',[n]);
        Check(d2.Inserted=cardinal(n));
        if j=2 then begin
          d1.DiffSnapshot;
          CheckLogTime(d2.Revision=d1.Revision,'d1.DiffSnapshot',[]);
        end;
      end;
    finally
      d2.Free;
      CheckLogTime(true,'d2.Free',[]);
    end;
  finally
    d1.Free;
    CheckLogTime(true,'d1.Free',[]);
  end;
end;


{$ifndef DELPHI5OROLDER}

type
  TPersistentAutoCreateFieldsTest = class(TPersistentAutoCreateFields)
  private
    fText: RawUTF8;
    fValue1: TComplexNumber;
    fValue2: TComplexNumber;
  public
    constructor CreateFake;
  published
    property Text: RawUTF8 read fText write fText;
    property Value1: TComplexNumber read fValue1;
    property Value2: TComplexNumber read fValue2;
  end;
  TPersistentAutoCreateFieldsTestObjArray = array of TPersistentAutoCreateFieldsTest;
  TComplexNumberObjArray = array of TComplexNumber;
  TObjArrayTest = class(TPersistentAutoCreateFieldsTest)
  private
    fValues: TComplexNumberObjArray;
  published
    property Values: TComplexNumberObjArray read fValues write fValues;
  end;
  TSQLRecordArrayTest = class(TSQLRecord)
  private
    fValues: TComplexNumberObjArray;
  published
    property Values: TComplexNumberObjArray read fValues write fValues;
  end;

constructor TPersistentAutoCreateFieldsTest.CreateFake;
begin
  inherited Create;
  Text := 'text';
  Value1.Real := 1.5;
  Value1.Imaginary := 2.5;
  Value2.Real := 1.7;
  Value2.Imaginary := 2.7;
end;

procedure TTestLowLevelCommon._TObjArray;
const MAX=200;
var i: integer;
    arr: TPersistentAutoCreateFieldsTestObjArray;
    test,test2: TObjArrayTest;
    p: TPersistentAutoCreateFieldsTest;
    r1,r2: TSQLRecordArrayTest;
    tmp: RawUTF8;
    valid: boolean;
procedure CheckValues(test: TComplexNumberObjArray);
var i: integer;
begin
  Check(length(test)=MAX+1);
  for i := 0 to MAX do begin
    CheckSame(test[i].Real,0.5+i);
    CheckSame(test[i].Imaginary,0.2+i);
  end;
end;
begin
  TJSONSerializer.RegisterObjArrayForJSON(
    TypeInfo(TPersistentAutoCreateFieldsTestObjArray),TPersistentAutoCreateFieldsTest);
  try
    tmp := DynArraySaveJSON(arr,TypeInfo(TPersistentAutoCreateFieldsTestObjArray));
    check(tmp='[]');
    p := TPersistentAutoCreateFieldsTest.CreateFake;
    ObjArrayAdd(arr,p);
    tmp := DynArraySaveJSON(arr,TypeInfo(TPersistentAutoCreateFieldsTestObjArray));
    check(tmp='[{"Text":"text","Value1":{"Real":1.5,"Imaginary":2.5},'+
      '"Value2":{"Real":1.7,"Imaginary":2.7}}]');
    for i := 1 to MAX do begin
      p := TPersistentAutoCreateFieldsTest.CreateFake;
      p.Value1.Real := p.Value1.Real+i*1.0;
      Check(ObjArrayAdd(arr,p)=i);
    end;
    tmp := DynArraySaveJSON(arr,TypeInfo(TPersistentAutoCreateFieldsTestObjArray));
    ObjArrayClear(arr);
    Check(length(arr)=0);
    DynArrayLoadJSON(arr,pointer(tmp),TypeInfo(TPersistentAutoCreateFieldsTestObjArray));
    Check(length(arr)=MAX+1);
    for i := 0 to MAX do begin
      Check(arr[i].Text='text');
      CheckSame(arr[i].Value1.Real,1.5+i);
      CheckSame(arr[i].Value1.Imaginary,2.5);
      CheckSame(arr[i].Value2.Real,1.7);
      CheckSame(arr[i].Value2.Imaginary,2.7);
    end;
  finally
    ObjArrayClear(arr);
  end;
  TJSONSerializer.RegisterObjArrayForJSON(
    TypeInfo(TComplexNumberObjArray),TComplexNumber);
  test := TObjArrayTest.CreateFake;
  try
    for i := 0 to max do
      ObjArrayAdd(test.fValues,TComplexNumber.Create(0.5+i,0.2+i));
    CheckValues(test.Values);
    tmp := ObjectToJSON(test);
  finally
    test.Free;
  end;
  r1 := TSQLRecordArrayTest.CreateFrom(tmp);
  r2 := TSQLRecordArrayTest.CreateFrom(tmp);
  try
    check(r1.IDValue=0);
    check(r2.IDValue=0);
    CheckValues(r1.Values);
    CheckValues(r2.Values);
    check(r1.SameValues(r2));
  finally
    r2.Free;
    r1.Free;
  end;
  test := TObjArrayTest.CreateFake;
  test2 := TObjArrayTest.CreateFake;
  try
    check(ObjectLoadJSON(test,tmp));
    CheckValues(test.Values);
    JSONToObject(test2,pointer(tmp),valid);
    Check(valid);
    CheckValues(test2.Values);
    check(ObjectEquals(test,test2));
  finally
    test2.Free;
    test.Free;
  end;
end;

function TSQLRecordPeopleCompareByFirstName(const A,B): integer;
begin
  result := StrIComp(pointer(TSQLRecordPeople(A).FirstName),
    pointer(TSQLRecordPeople(B).FirstName));
end;

procedure TTestLowLevelCommon._TObjectDynArrayWrapper;
const MAX = 10000;
var i,j: integer;
    s: RawUTF8;
    da: IObjectDynArray; // force the interface to be defined BEFORE the array
procedure CheckItem(p: TSQLRecordPeople; i: integer);
var s: RawUTF8;
begin
  UInt32ToUtf8(i,s);
  Check(p.fID=i);
  Check(p.FirstName='FirstName'+s);
  Check(p.LastName='LastName'+s);
  Check(p.Data='');
  Check(p.YearOfBirth=i);
  Check(p.YearOfDeath=i+80);
end;
begin
  da := TObjectDynArrayWrapper.Create(a);
  for i := 1 to MAX do begin
    UInt32ToUtf8(i,s);
    Check(da.Add(TSQLRecordPeople.Create(['FirstName'+s,'LastName'+s,i,i+80],i))=i-1);
  end;
  Check(da.Count=MAX);
  for i := 0 to da.Count-1 do
    CheckItem(a[i],i+1);
  for i := da.Count-1 downto 0 do
    if i and 3=0 then
      da.Delete(i);
  j := 0;
  for i := 0 to MAX-1 do
    if i and 3<>0 then begin
      CheckItem(a[j],i+1);
      inc(j);
    end;
  Check(j=da.Count);
  da.Sort(TSQLRecordPeopleCompareByFirstName);
  for i := 0 to da.Count-1 do
    CheckItem(a[i],a[i].fID);
  for i := 1 to da.Count-1 do
    Check(a[i-1].FirstName<a[i].FirstName);
end;


{ TSQLRecordTest }

procedure TSQLRecordTest.SetInt(const Value: int64);
begin
  fInt := Value;
end;

procedure TSQLRecordTest.FillWith(i: Integer);
begin
  Int := i;
  Test := Int32ToUtf8(i);
  Ansi := WinAnsiString(Test);
  Unicode := WinAnsiToRawUnicode(Ansi);
  ValFloat := i*2.5;
  ValWord := i;
  ValDate := i+30000;
  Data := Test;
{$ifndef NOVARIANTS}
  ValVariant := _ObjFast(['id',i]);
{$endif}
end;

procedure TSQLRecordTest.CheckWith(test: TSynTestCase; i: Integer; offset: integer;
  checkblob: boolean);
begin
  test.Check(i<>0);
  test.CheckUtf8(ID=i,'id=%=%',[ID,i]);
  test.Check(Int=i);
  test.Check(self.Test=Int32ToUtf8(i));
  test.Check(Ansi=WinAnsiString(self.Test));
  test.Check(Unicode=WinAnsiToRawUnicode(Ansi));
  test.Check(ValFloat=i*2.5);
  test.Check(ValWord=(i+offset) and $ffff);
  test.Check(ValDate=i+30000);
  if checkblob then
    test.Check(Data=self.Test);
{$ifndef NOVARIANTS}
  test.Check(DocVariantType.IsOfType(ValVariant),'var1');
  test.Check(VariantSaveJson(ValVariant)='{"id":'+self.Test+'}','var2');
{$endif}
end;


{ TSQLRecordPeople }

function TSQLRecordPeople.DataAsHex(aClient: TSQLRestClientURI): RawUTF8;
begin
  Result := aClient.CallBackGetResult('DataAsHex',[],RecordClass,fID);
end;

class function TSQLRecordPeople.Sum(aClient: TSQLRestClientURI; a, b: double; Method2: boolean): double;
var err: integer;
const METHOD: array[boolean] of RawUTF8 = ('sum','sum2');
begin
  Result := GetExtended(pointer(aClient.CallBackGetResult(
    METHOD[Method2],['a',a,'b',b])),err);
end;


{ TTestLowLevelTypes }

{$ifndef NOVARIANTS}

procedure TTestLowLevelTypes.Variants;
var v: Variant;
    vd: TVarData absolute v;
    t: pointer;
    dt: TDateTime;
    ni: TNullableInteger;
    nt: TNullableUTF8Text;
begin
  t := nil; // makes the compiler happy
  ValueVarToVariant(nil,0,sftBoolean,vd,false,t);
  Check(not boolean(v));
  ValueVarToVariant('0',1,sftBoolean,vd,false,t);
  Check(not boolean(v));
  ValueVarToVariant('false',5,sftBoolean,vd,false,t);
  Check(not boolean(v));
  ValueVarToVariant('1',1,sftBoolean,vd,false,t);
  Check(boolean(v));
  ValueVarToVariant('true',4,sftBoolean,vd,false,t);
  Check(boolean(v));
  GetVariantFromJSON('0',False,v,nil);
  Check(vd.VType=varInteger);
  Check(v=0);
  GetVariantFromJSON('123',False,v,nil);
  Check(vd.VType=varInteger);
  Check(v=123);
  GetVariantFromJSON('0123',False,v,nil);
  Check(vd.VType=varString);
  GetVariantFromJSON('-123',False,v,nil);
  Check(vd.VType=varInteger);
  Check(v=-123);
  GetVariantFromJSON('123456789012345678',False,v,nil);
  Check(vd.VType=varInt64);
  Check(v=123456789012345678);
  GetVariantFromJSON('1234567890123456789',False,v,nil);
  Check(vd.VType=varInt64);
  Check(v=1234567890123456789);
  GetVariantFromJSON('12345678901234567890',False,v,nil,true);
  Check(vd.VType=varDouble);
  CheckSame(vd.VDouble,12345678901234567890.0,0);
  GetVariantFromJSON('12345678901234567890',False,v,nil,false);
  Check(vd.VType=varString);
  GetVariantFromJSON('-123.1',False,v,nil);
  Check(vd.VType=varCurrency);
  Check(v=-123.1);
  GetVariantFromJSON('-123.12',False,v,nil);
  Check(vd.VType=varCurrency);
  Check(v=-123.12);
  GetVariantFromJSON('-123.123',False,v,nil);
  Check(vd.VType=varCurrency);
  Check(v=-123.123);
  GetVariantFromJSON('123.1234',False,v,nil,false);
  Check(vd.VType=varCurrency);
  Check(v=123.1234);
  GetVariantFromJSON('123.1234',False,v,nil,true);
  Check(vd.VType=varCurrency);
  Check(v=123.1234);
  GetVariantFromJSON('-123.12345',False,v,nil,true);
  Check(vd.VType=varDouble);
  CheckSame(v,-123.12345);
  GetVariantFromJSON('-1.123e12',False,v,nil,true);
  Check(vd.VType=varDouble);
  CheckSame(v,-1.123e12);
  GetVariantFromJSON('-123.123e-2',False,v,nil,true);
  Check(vd.VType=varDouble);
  CheckSame(v,-123.123e-2);
  GetVariantFromJSON('-123.123ee2',False,v,nil,true);
  Check(vd.VType=varString);
  Check(v='-123.123ee2');
  GetVariantFromJSON('1-123.12',False,v,nil);
  Check(vd.VType=varString);
  Check(v='1-123.12');
  GetVariantFromJSON('123.',False,v,nil);
  Check(vd.VType=varString);
  Check(v='123.');
  GetVariantFromJSON('123.abc',False,v,nil);
  Check(vd.VType=varString);
  Check(v='123.abc');
  GetVariantFromJSON('123.1abc',False,v,nil);
  Check(vd.VType=varString);
  Check(v='123.1abc');
  GetVariantFromJSON('123.12a',False,v,nil);
  Check(vd.VType=varString);
  Check(v='123.12a');
  GetVariantFromJSON('123.123a',False,v,nil);
  Check(vd.VType=varString);
  Check(v='123.123a');
  GetVariantFromJSON('123.1234a',False,v,nil);
  Check(vd.VType=varString);
  Check(v='123.1234a');
  Check(VariantToDateTime('2016',dt));
  CheckSame(dt,42370);
  Check(VariantToDateTime(2016,dt));
  CheckSame(dt,42370);
  Check(VariantToDateTime('1982/10/30',dt));
  CheckSame(dt,30254);
  Check(not VariantToDateTime('201a',dt));
  ni := NullableIntegerNull;
  Check(NullableIntegerIsEmptyOrNull(ni));
  ni := NullableInteger(10);
  Check(not NullableIntegerIsEmptyOrNull(ni));
  Check(NullableIntegerToValue(ni) = 10);
  nt := NullableUTF8TextNull;
  Check(NullableUTF8TextIsEmptyOrNull(nt));
  nt := NullableUTF8Text('toto');
  Check(not NullableUTF8TextIsEmptyOrNull(nt));
  Check(NullableUTF8TextToValue(nt) = 'toto');
  {$ifndef FPC} // FPC does not allow to mix variant derivated types
  Check(ni = 10);
  Check(nt = 'toto');
  {$endif}
  JSONToVariantInPlace(v,nil);
  Check(vd.VType=varEmpty);
  v := JSONToVariant('');
  Check(vd.VType=varEmpty);
  v := JSONToVariant('null');
  Check(vd.VType=varNull);
  v := JSONToVariant('false');
  Check(not boolean(v));
  v := JSONToVariant('true');
  Check(boolean(v));
  v := JSONToVariant('invalid');
  Check(vd.VType=varNull);
  v := JSONToVariant('0');
  Check(vd.VType=varInteger);
  v := JSONToVariant('123456789012345678');
  Check(vd.VType=varInt64);
  Check(v=123456789012345678);
  v := JSONToVariant('123.1234');
  Check(vd.VType=varCurrency);
  CheckSame(v,123.1234);
  v := JSONToVariant('-1E-300',[],true);
  Check(vd.VType=varDouble);
  CheckSame(v,-1e-300);
  v := JSONToVariant('[]');
  Check(v._kind=ord(dvArray));
  Check(v._count=0);
  v := JSONToVariant('{  }');
  Check(v._kind=ord(dvObject));
  Check(v._count=0);
  v := JSONToVariant('[1,2,3]');
  Check(v._kind=ord(dvArray));
  Check(v._count=3);
  v := JSONToVariant(' {"a":10,b:20}');
  Check(v._kind=ord(dvObject));
  Check(v._count=2);
  v := JSONToVariant('{"invalid":');
  Check(vd.VType=varEmpty);
  v := JSONToVariant(' "toto\r\ntoto"');
  Check(vd.VType=varString);
  Check(v='toto'#$D#$A'toto');
end;

type
  TMustacheTest = packed record
    desc: string;
    template, expected: RawUTF8;
    data,partials: variant;
  end;
  TMustacheTests = packed record
    tests: array of TMustacheTest;
  end;

const
  __TMustacheTest = 'desc string template,expected RawUTF8 data,partials variant';
  __TMustacheTests = 'tests array of TMustacheTest';
  MUSTACHE_SPECS: array[0..4] of TFileName =
    ('interpolation','comments','sections','inverted','partials');

procedure TTestLowLevelTypes.MustacheRenderer;
var mustacheJson: RawByteString;
    mus: TMustacheTests;
    mustache: TSynMustache;
    mustacheJsonFileName: TFileName;
    doc: variant;
    html: RawUTF8;
    helpers: TSynMustacheHelpers;
    guid: TGUID;
    spec,i: integer;
begin
  // manual tests
  mustache := TSynMustache.Parse(
    'Hello {{name}}'#13#10'You have just won {{value}} dollars!');
  Check(mustache.SectionMaxCount=0);
  TDocVariant.New(doc);
  doc.name := 'Chris';
  doc.value := 10000;
  html := mustache.Render(doc);
  Check(html='Hello Chris'#13#10'You have just won 10000 dollars!');
  mustache := TSynMustache.Parse(
    '{{=<% %>=}}Hello <%name%><%={{ }}=%>'#13#10'You have just won {{& value }} dollars!');
  Check(mustache.SectionMaxCount=0);
  doc := _ObjFast(['name','Chris','value',1000]);
  html := mustache.Render(doc);
  Check(html='Hello Chris'#13#10'You have just won 1000 dollars!');
  mustache := TSynMustache.Parse(
    'Hello {{value.name}}'#13#10'You have just won {{value.value}} dollars!');
  Check(mustache.SectionMaxCount=0);
  html := mustache.RenderJSON('{value:{name:"Chris",value:10000}}');
  Check(html='Hello Chris'#13#10'You have just won 10000 dollars!');
  mustache := TSynMustache.Parse(
    '* {{name}}'#13#10'* {{age}}'#13#10'* {{company}}'#13#10'* {{{company}}}');
  Check(mustache.SectionMaxCount=0);
  html := mustache.RenderJson('{name:"Chris",company:"<b>Synopse</b>"}');
  Check(html='* Chris'#13#10'* '#13#10'* &lt;b&gt;Synopse&lt;/b&gt;'#13#10'* <b>Synopse</b>');
  mustache := TSynMustache.Parse(
    '* {{name}}'#13#10'* {{age}}'#13#10'* {{company}}'#13#10'* {{&company}}');
  Check(mustache.SectionMaxCount=0);
  html := mustache.RenderJson('{name:"Chris",company:"<b>Synopse</b>"}');
  Check(html='* Chris'#13#10'* '#13#10'* &lt;b&gt;Synopse&lt;/b&gt;'#13#10'* <b>Synopse</b>');
  mustache := TSynMustache.Parse('Shown.{{#person}}Never shown!{{/person}}end');
  Check(mustache.SectionMaxCount=1);
  html := mustache.RenderJson('{person:false}');
  Check(html='Shown.end');
  mustache := TSynMustache.Parse('Shown.{{#person}}Also shown!{{/person}}end');
  Check(mustache.SectionMaxCount=1);
  html := mustache.RenderJSON('{person:true}');
  Check(html='Shown.Also shown!end');
  html := mustache.RenderJSON('{person:"toto"}');
  Check(html='Shown.Also shown!end');
  html := mustache.RenderJSON('{person:false}');
  Check(html='Shown.end');
  mustache := TSynMustache.Parse('Shown.{{#person}}As {{name}}!{{/person}}end{{name}}');
  Check(mustache.SectionMaxCount=1);
  html := mustache.RenderJSON('{person:{age:10,name:"toto"}}');
  Check(html='Shown.As toto!end');
  mustache := TSynMustache.Parse('Shown.{{^person}}Never shown!{{/person}}end');
  Check(mustache.SectionMaxCount=1);
  html := mustache.RenderJSON('{person:true}');
  Check(html='Shown.end');
  mustache := TSynMustache.Parse('Shown.{{^person}}Never shown!{{/person}}end');
  Check(mustache.SectionMaxCount=1);
  html := mustache.RenderJSON('{person:{age:10,name:"toto"}}');
  Check(html='Shown.end');
  mustache := TSynMustache.Parse('Shown.{{^person}}Also shown!{{/person}}end');
  Check(mustache.SectionMaxCount=1);
  html := mustache.RenderJSON('{person:false}');
  Check(html='Shown.Also shown!end');
  mustache := TSynMustache.Parse('Shown.{{^person}}Also shown!{{/person}}end');
  Check(mustache.SectionMaxCount=1);
  html := mustache.RenderJSON('{person2:2}');
  Check(html='Shown.Also shown!end');
  Check(helpers=nil,'compiler initialized');
  mustache.HelperAdd(helpers, 'jsonhelper', MustacheHelper);
  mustache := TSynMustache.Parse('{{jsonhelper {a:"a",b:10}}}');
  html := mustache.RenderJSON('', nil, helpers);
  Check(html='a=a,b=10');
  mustache := TSynMustache.Parse('{{jsonhelper {a:"b",b:10} }}');
  html := mustache.RenderJSON('', nil, helpers);
  Check(html='a=b,b=10');
  mustache := TSynMustache.Parse('{{{jsonhelper {a:"a",b:1}}}}');
  html := mustache.RenderJSON('', nil, helpers);
  check(html='a=a,b=1');
  mustache := TSynMustache.Parse('{{jsonhelper {a:1,b:2} }},titi');
  html := mustache.RenderJSON('', nil, helpers);
  Check(html='a=1,b=2,titi');
  mustache := TSynMustache.Parse('{{jsonhelper {a:1,nested:{c:{d:[1,2]}},b:10}}}}toto');
  html := mustache.RenderJSON('', nil, helpers);
  Check(html='a=1,b=10}toto');
  mustache := TSynMustache.Parse('{{#a}}'#$A'{{one}}'#$A'{{/a}}'#$A);
  html := mustache.RenderJSON('{a:{one:1}}');
  Check(html='1'#$A);
  mustache := TSynMustache.Parse('{{#a}}{{one}}{{#b}}{{one}}{{two}}{{/b}}{{/a}}');
  html := mustache.RenderJSON('{a:{one:1},b:{two:2}}');
  Check(html='112');
  mustache := TSynMustache.Parse('{{>partial}}'#$A'3');
  html := mustache.RenderJSON('{}',TSynMustachePartials.CreateOwned(['partial','1'#$A'2']));
  Check(html='1'#$A'23','external partials');
  mustache := TSynMustache.Parse('{{<partial}}1'#$A'2{{name}}{{/partial}}{{>partial}}4');
  html := mustache.RenderJSON('{name:3}');
  Check(html='1'#$A'234','internal partials');
  mustache := TSynMustache.Parse(
    'My favorite things:'#$A'{{#things}}{{-index}}. {{.}}'#$A'{{/things}}');
  Check(mustache.SectionMaxCount=1);
  html := mustache.RenderJSON('{things:["Peanut butter", "Pen spinning", "Handstands"]}');
  Check(html='My favorite things:'#$A'1. Peanut butter'#$A'2. Pen spinning'#$A+
    '3. Handstands'#$A,'-index pseudo variable');
  mustache := TSynMustache.Parse('{{#things}}{{.}}{{/things}}');
  html := mustache.RenderJSON('{things:["one", "two", "three"]}');
  check(html='onetwothree');
  mustache := TSynMustache.Parse('{{#things}}{{#-first}}{{.}}{{/-first}}{{/things}} {{pi}}');
  html := mustache.RenderJSON('{things:["one", "two", "three"],pi:3.1415}');
  check(html='one 3.1415');
  mustache := TSynMustache.Parse('{{#things}}{{^-first}}, {{/-first}}{{.}}{{/things}}');
  html := mustache.RenderJSON('{things:["one", "two", "three"]}');
  check(html='one, two, three');
  mustache := TSynMustache.Parse('{{#things}}{{.}}{{^-last}}, {{/-last}}{{/things}}');
  html := mustache.RenderJSON('{things:["one", "two", "three"]}');
  check(html='one, two, three');
  mustache := TSynMustache.Parse('{{#things}}{{#-last}}{{.}}{{/-last}}{{/things}}');
  html := mustache.RenderJSON('{things:["one", "two", "three"]}');
  check(html='three');
  mustache := TSynMustache.Parse('{{#things}}{{#-odd}}{{.}}{{/-odd}}{{/things}}');
  html := mustache.RenderJSON('{things:["one", "two", "three"]}');
  check(html='onethree');
  mustache := TSynMustache.Parse(
    '{{"Hello}} {{name}}'#13#10'{{"You have just won}} {{value}} {{"dollars}}!');
  Check(mustache.SectionMaxCount=0);
  html := mustache.RenderJSON('{name:?,value:?}',[],['Chris',10000],nil,nil,MustacheTranslate);
  Check(html='Bonjour Chris'#$D#$A'Vous venez de gagner 10000 dollars!');
  mustache := TSynMustache.Parse('1+3={{tval}} - is it 4?{{#if tval=4}} yes!{{/if}}');
  html := mustache.RenderJSON('{tval:4}',nil,TSynMustache.HelpersGetStandardList);
  check(html='1+3=4 - is it 4? yes!');
  html := mustache.RenderJSON('{tval:5}',nil,TSynMustache.HelpersGetStandardList);
  check(html='1+3=5 - is it 4?');
  mustache := TSynMustache.Parse('{{newguid}}');
  html := mustache.RenderJSON('{}',nil,TSynMustache.HelpersGetStandardList);
  check((html<>'') and (TextToGUID(@html[2],@guid)<>nil));
  mustache := TSynMustache.Parse(
    '<h1>{{header}}</h1>'#$D#$A'{{#items}}'#$D#$A'{{#first}}'#$D#$A+
    '<li><strong>{{name}}</strong></li>'#$D#$A'{{/first}}'#$D#$A+
    '{{#link}}'#$D#$A'<li><a href="{{url}}">{{name}}</a></li>'#$D#$A'{{/link}}'#$D#$A+
    '{{/items}}'#$D#$A#$D#$A'{{#empty}}'#$D#$A'<p>The list is empty.</p>'#$D#$A'{{/empty}}');
  Check(mustache.SectionMaxCount=2);
  html := mustache.RenderJSON(
    '{"header":"Colors","items":[{"name":"red","first":true,"url":"#Red"},'+
    '{"name":"green","link":true,"url":"#Green"},{"name":"blue","first":true,'+
    '"link":true,"url":"#Blue"}],"empty":true}');
  Check(trim(html)=
      '<h1>Colors</h1>'#$D#$A'<li><strong>red</strong></li>'#$D#$A+
      '<li><a href="#Green">green</a></li>'#$D#$A'<li><strong>blue</strong></li>'#$D#$A+
      '<li><a href="#Blue">blue</a></li>'#$D#$A#$D#$A'<p>The list is empty.</p>');
  mustache := TSynMustache.Parse('{{#users}}'#$D#$A'{{^Connected}}'#$D#$A+
    '- {{Name}} {{Firstname}} ({{Connected}})<BR>'#$D#$A'{{/Connected}}'#$D#$A'{{/users}}');
  Check(mustache.SectionMaxCount=2);
  html := mustache.RenderJSON('{"users":['+
    '{"RowID":1,"Login":"safr","Firstname":"Frodon","Name":"Sacquet","Alias":"safr","Connected":true,"Resto":0},' + #13#10 +
    '{"RowID":2,"Login":"saga","Firstname":"Samsagace","Name":"Gamegie","Alias":"saga","Connected":false,"Resto":0},' + #13#10 +
    '{"RowID":3,"Login":"peto","Firstname":"Peregrin","Name":"Touque","Alias":"peto","Connected":false,"Resto":0},' + #13#10 +
    '{"RowID":4,"Login":"mebr","Firstname":"Meriadoc","Name":"Brandebouc","Alias":"mebr","Connected":true,"Resto":0}]}');
  check(html='- Gamegie Samsagace (false)<BR>'#$D#$A'- Touque Peregrin (false)<BR>'#$D#$A);

  // run official {{mustache}} regression tests suite
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TMustacheTest),__TMustacheTest).
    Options := [soReadIgnoreUnknownFields];
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TMustacheTests),__TMustacheTests).
    Options := [soReadIgnoreUnknownFields];
  for spec := 0 to High(MUSTACHE_SPECS) do begin
    mustacheJsonFileName := MUSTACHE_SPECS[spec]+'.json';
    mustacheJson := StringFromFile(mustacheJsonFileName);
    if mustacheJson='' then begin
      mustacheJson := HttpGet('https://raw.githubusercontent.com/mustache/spec/'+
        'master/specs/'+StringToAnsi7(mustacheJsonFileName));
      FileFromString(mustacheJson,mustacheJsonFileName);
    end;
    RecordLoadJSON(mus,pointer(mustacheJson),TypeInfo(TMustacheTests));
    Check(length(mus.tests)>5);
    for i := 0 to high(mus.tests) do
    with mus.Tests[i] do begin
      if PosEx(' {{>partial}}',template)>0 then
        continue; // we don't indent each line of the expanded partials (yet)
      mustache := TSynMustache.Parse(template);
      html := mustache.Render(data,TSynMustachePartials.CreateOwned(partials));
      Check(html=expected,desc);
    end;
  end;
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TMustacheTest),'');
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TMustacheTests),'');
end;

procedure TTestLowLevelTypes.MustacheTranslate(var English: string);
begin
  if English='Hello' then
    English := 'Bonjour' else
  if English='You have just won' then
    English := 'Vous venez de gagner';
end;

procedure TTestLowLevelTypes.MustacheHelper(const Value: variant; out result: variant);
begin
  with _Safe(Value)^ do
    result := RawUTF8ToVariant(FormatUTF8('a=%,b=%',[U['a'],I['b']]));
end;

{$endif NOVARIANTS}

{$endif DELPHI5OROLDER}


{$ifdef UNICODE}
{$WARNINGS OFF} // don't care about implicit string cast in tests
{$endif}

{$ifndef LVCL}
{$ifndef DELPHI5OROLDER}

type
  TCollTests = class(TInterfacedCollection)
  private
    function GetCollItem(Index: Integer): TCollTest;
  protected
    class function GetClass: TCollectionItemClass; override;
  public
    function Add: TCollTest;
    property Item[Index: Integer]: TCollTest read GetCollItem; default;
  end;

  TMyCollection = class(TCollection);

  TCollTst = class(TPersistent)
  private
    fColl: TCollTests;
    fTCollTest: TCollTest;
    fStr: TStringList;
    procedure SetColl(const Value: TCollTests);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property One: TCollTest read fTCollTest write fTCollTest;
    property Coll: TCollTests read fColl write SetColl;
    property Str: TStringList read fStr write fStr;
  end;

  TCollTstDynArray = class(TCollTst)
  private
    fInts: TIntegerDynArray;
    fTimeLog: TTimeLogDynArray;
    fFileVersions: TFVs;
    class function FVReader(P: PUTF8Char; var aValue; out aValid: Boolean
      {$ifndef NOVARIANTS}; CustomVariantOptions: PDocVariantOptions{$endif}): PUTF8Char;
    class procedure FVWriter(const aWriter: TTextWriter; const aValue);
    class function FVReader2(P: PUTF8Char; var aValue; out aValid: Boolean
      {$ifndef NOVARIANTS}; CustomVariantOptions: PDocVariantOptions{$endif}): PUTF8Char;
    class procedure FVWriter2(const aWriter: TTextWriter; const aValue);
    class function FVClassReader(const aValue: TObject; aFrom: PUTF8Char;
      var aValid: Boolean; aOptions: TJSONToObjectOptions): PUTF8Char;
    class procedure FVClassWriter(const aSerializer: TJSONSerializer;
      aValue: TObject; aOptions: TTextWriterWriteObjectOptions);
  published
    property Ints: TIntegerDynArray read fInts write fInts;
    property TimeLog: TTimeLogDynArray read fTimeLog write fTimeLog;
    property FileVersion: TFVs read fFileVersions write fFileVersions;
  end;


{ TCollTstDynArray}

class function TCollTstDynArray.FVReader(P: PUTF8Char; var aValue;
  out aValid: Boolean{$ifndef NOVARIANTS}; CustomVariantOptions: PDocVariantOptions{$endif}): PUTF8Char;
var V: TFV absolute aValue;
begin // '[1,2001,3001,4001,"1","1001"],[2,2002,3002,4002,"2","1002"],...'
  aValid := false;
  result := nil;
  if (P=nil) or (P^<>'[') then
    exit;
  inc(P);
  V.Major := GetNextItemCardinal(P);
  V.Minor := GetNextItemCardinal(P);
  V.Release := GetNextItemCardinal(P);
  V.Build := GetNextItemCardinal(P);
  V.Main := UTF8ToString(GetJSONField(P,P));
  V.Detailed := UTF8ToString(GetJSONField(P,P));
  if P=nil then
    exit;
  aValid := true;
  result := P; // ',' or ']' for last item of array
end;

class procedure TCollTstDynArray.FVWriter(const aWriter: TTextWriter; const aValue);
var V: TFV absolute aValue;
begin
  aWriter.Add('[%,%,%,%,"%","%"]',
    [V.Major,V.Minor,V.Release,V.Build,V.Main,V.Detailed],twJSONEscape);
end;

class function TCollTstDynArray.FVReader2(P: PUTF8Char; var aValue;
  out aValid: Boolean{$ifndef NOVARIANTS}; CustomVariantOptions: PDocVariantOptions{$endif}): PUTF8Char;
var V: TFV absolute aValue;
    Values: array[0..5] of TValuePUTF8Char;
begin // '{"Major":1,"Minor":2001,"Release":3001,"Build":4001,"Main":"1","Detailed":"1001"},..
  aValid := false;
  result := JSONDecode(P,['Major','Minor','Release','Build','Main','Detailed'],@Values);
  if result=nil then
    exit; // result^ = ',' or ']' for last item of array
  V.Major := Values[0].ToInteger;
  V.Minor := Values[1].ToInteger;
  V.Release := Values[2].ToInteger;
  V.Build := Values[3].ToInteger;
  V.Main := Values[4].ToString;
  V.Detailed := Values[5].ToString;
  aValid := true;
end;

class procedure TCollTstDynArray.FVWriter2(const aWriter: TTextWriter; const aValue);
var V: TFV absolute aValue;
begin
  aWriter.AddJSONEscape(['Major',V.Major,'Minor',V.Minor,'Release',V.Release,
    'Build',V.Build,'Main',V.Main,'Detailed',V.Detailed]);
end;

class function TCollTstDynArray.FVClassReader(const aValue: TObject; aFrom: PUTF8Char;
  var aValid: Boolean; aOptions: TJSONToObjectOptions): PUTF8Char;
var V: TFileVersion absolute aValue;
    Values: array[0..5] of TValuePUTF8Char;
begin // '{"Major":2,"Minor":2002,"Release":3002,"Build":4002,"Main":"2","BuildDateTime":"1911-03-15"}'
  result := JSONDecode(aFrom,['Major','Minor','Release','Build','Main','BuildDateTime'],@Values);
  aValid := (result<>nil);
  if aValid then begin
    V.Major := Values[0].ToInteger;
    V.Minor := Values[1].ToInteger;
    V.Release := Values[2].ToInteger;
    V.Build := Values[3].ToInteger;
    V.Main := Values[4].ToString;
    V.BuildDateTime := Iso8601ToDateTimePUTF8Char(Values[5].Value,Values[5].ValueLen);
  end;
end;

class procedure TCollTstDynArray.FVClassWriter(const aSerializer: TJSONSerializer;
  aValue: TObject; aOptions: TTextWriterWriteObjectOptions);
var V: TFileVersion absolute aValue;
begin
  aSerializer.AddJSONEscape(['Major',V.Major,'Minor',V.Minor,'Release',V.Release,
    'Build',V.Build,'Main',V.Main,'BuildDateTime',DateTimeToIso8601Text(V.BuildDateTime)]);
end;


{ TCollTests }

function TCollTests.Add: TCollTest;
begin
  result := inherited Add as TCollTest;
end;

class function TCollTests.GetClass: TCollectionItemClass;
begin
  result := TCollTest;
end;

function TCollTests.GetCollItem(Index: Integer): TCollTest;
begin
  result := Items[Index] as TCollTest;
end;


{ TCollTst }

constructor TCollTst.Create;
begin
  inherited;
  fColl := TCollTests.Create;
  fTCollTest := TCollTest.Create(nil);
end;

destructor TCollTst.Destroy;
begin
  fColl.Free;
  fTCollTest.Free;
  fStr.Free;
  inherited;
end;

procedure TCollTst.SetColl(const Value: TCollTests);
begin
  fColl.Free;
  fColl := Value;
end;

{$endif DELPHI5OROLDER}
{$endif LVCL}

type
  {$M+} // TPersistent has no RTTI for LVCL!
  TPersistentToJSON = class(TPersistent)
  protected
    fName: RawUTF8;
    fEnum: TSynBackgroundThreadProcessStep;
    fSets: TSynBackgroundThreadProcessSteps;
  published
    property Name: RawUTF8 read fName write fName;
    property Enum: TSynBackgroundThreadProcessStep read fEnum write fEnum default flagIdle;
    property Sets: TSynBackgroundThreadProcessSteps read fSets write fSets default [];
  end;
  {$M-}

{$ifdef DELPHI5OROLDER} // mORMot.pas not linked yet
  TSQLRestCacheEntryValue = packed record
    /// corresponding ID
    ID: Int64;
    /// GetTickCount64 shr 9 timestamp when this cached value was stored
    // - resulting time period has therefore a resolution of 512 ms, and
    // overflows after 70 years without computer reboot
    // - equals 0 when there is no JSON value cached
    Timestamp512: cardinal;
    /// some associated unsigned integer value
    // - not used by TSQLRestCache, but available at TSQLRestCacheEntry level
    Tag: cardinal;
    /// JSON encoded UTF-8 serialization of the record
    JSON: RawUTF8;
  end;
{$else}
  TRange = record
    Min, Max: Integer;
  end;
  TOffense = record
    Damage, AttackSpeed: TRange;
  end;
  TEnemy = class(TSynPersistent)
  private
    fEnabled: Boolean;
    fName: string;
    function GetOffense: RawJSON;
    procedure SetOffense(Value: RawJSON);
  public
    Off: TOffense;
  published
    property Enabled: Boolean read fEnabled write fEnabled;
    property Name: string read fName write fName;
    property Offense: RawJSON read GetOffense write SetOffense;
  end;

function TEnemy.GetOffense: RawJSON;
begin
  result := JSONEncode([
    'damage','{','min',Off.Damage.Min,'max',Off.Damage.Max,'}',
    'attackspeed','{','min',Off.AttackSpeed.Min,'max',Off.AttackSpeed.Max,'}']);
end;

procedure RangeFromJSON(out Range: TRange; JSON: PUTF8Char);
var V: array[0..1] of TValuePUTF8Char;
begin
  JSONDecode(JSON, ['min', 'max'],@V);
  Range.Min := V[0].ToInteger;
  Range.Max := V[1].ToInteger;
end;

procedure TEnemy.SetOffense(Value: RawJSON);
var V: array[0..1] of TValuePUTF8Char;
begin
  JSONDecode(Value,['damage','attackspeed'],@V,true);
  RangeFromJSON(Off.Damage, V[0].Value);
  RangeFromJSON(Off.AttackSpeed, V[1].Value);
end;

{$endif}

type
  TTestCustomJSONRecord = packed record
    A,B,C: integer;
    D: RawUTF8;
    E: record E1,E2: double; end;
    F: TDateTime;
  end;
  TTestCustomJSONArray = packed record
    A,B,C: byte;
    D: RawByteString;
    E: array of record E1: double; E2: string; end;
    F: TDateTime;
  end;
  TTestCustomJSONArrayWithoutF = packed record
    A,B,C: byte;
    D: RawByteString;
    E: array of record E1: double; E2: string; end;
  end;
  TTestCustomJSONArraySimpleArray = packed record
    F: RawUTF8;
    G: array of RawUTF8;
  end;
  TTestCustomJSONArraySimple = packed record
    A,B: Int64;
    C: array of TGUID;
    D: RawUTF8;
    E: array of TTestCustomJSONArraySimpleArray;
    H: RawUTF8;
  end;
  {$ifndef NOVARIANTS}
  TTestCustomJSONArrayVariant = packed record
    A,B: Int64;
    C: array of variant;
    D: RawUTF8;
  end;
  {$endif}
  TTestCustomJSONGitHub = packed record
    name: RawUTF8;
    id: cardinal;
    description: RawUTF8;
    fork: boolean;
    owner: record
      login: RawUTF8;
      id: currency;
    end;
  end;
  TTestCustomJSONGitHubs = array of TTestCustomJSONGitHub;
  TTestCustomJSON2Title = packed record
    TITYPE,TIID,TICID,TIDSC30,TIORDER,TIDEL: RawUTF8;
  end;
  TTestCustomJSON2Trans = packed record
    TRTYPE: RawUTF8;
    TRDATE: TDateTime;
    TRAA: RawUTF8;
    TRCAT1, TRCAT2, TRCAT3, TRACID: TTestCustomJSON2Title;
    TRRMK: RawUTF8;
  end;
  TTestCustomJSON2 = packed record
    Transactions: array of TTestCustomJSON2Trans;
  end;
  TTestCustomDiscogs = packed record
    pagination: record
      per_page, items, page: Integer;
    end;
    releases: array of record
      status, title, format, _label, artist: RawUTF8; // label is a keyword
      year, id: Integer;
    end;
  end;
  TSubAB = packed record
    a: RawUTF8;
    b: integer;
  end;
  TSubCD = packed record
    c: byte;
    d: RawUTF8;
  end;
  TAggregate = packed record
    abArr: array of TSubAB;
    cdArr: array of TSubCD;
  end;
  TNestedDtoObject = class(TSynAutoCreateFields)
  private
    FFieldString: RawUTF8;
    FFieldInteger: integer;
    FFieldVariant: variant;
  published
    property FieldString: RawUTF8 read FFieldString write FFieldString;
    property FieldInteger: integer read FFieldInteger write FFieldInteger;
    property FieldVariant: variant read FFieldVariant write FFieldVariant;
  end;
  TDtoObject = class(TSynAutoCreateFields)
  private
    FFieldNestedObject: TNestedDtoObject;
    FSomeField: RawUTF8;
  published
    property NestedObject: TNestedDtoObject read FFieldNestedObject;
    property SomeField: RawUTF8 read FSomeField write FSomeField;
  end;
  {$ifdef ISDELPHI2010}
  TStaticArrayOfInt = packed array[1..5] of Integer;
  TNewRTTI = record
    Number: integer;
    StaticArray: array[1..2] of record
      Name: string;
      Single: Single;
      Double: Double;
    end;
    Int: TStaticArrayOfInt;
  end;
  TBookRecord = packed record
    name: string;
    author: record
      first_name:string;
      last_name:string;
    end;
  end;
{$endif}

const // convention may be to use __ before the type name
  __TTestCustomJSONRecord = 'A,B,C integer D RawUTF8 E{E1,E2 double} F TDateTime';
  __TTestCustomJSONArray  = 'A,B,C byte D RawByteString E[E1 double E2 string] F TDateTime';
  __TTestCustomJSONArraySimple =
    'A,B Int64 C array of TGUID D RawUTF8 E [F RawUTF8 G array of RawUTF8] H RawUTF8';
  __TTestCustomJSONArrayVariant =  'A,B Int64 C array of variant D RawUTF8';
  __TTestCustomJSONGitHub = 'name RawUTF8 id cardinal description RawUTF8 '+
    'fork boolean owner{login RawUTF8 id currency}';
  __TTestCustomJSON2Title = 'TITYPE,TIID,TICID,TIDSC30,TIORDER,TIDEL RawUTF8';
  __TTestCustomJSON2 = 'Transactions [TRTYPE RawUTF8 TRDATE TDateTime TRAA RawUTF8 '+
    'TRCAT1,TRCAT2,TRCAT3,TRACID TTestCustomJSON2Title '+
    'TRRMK RawUTF8]';
  __TTestCustomDiscogs = 'pagination{per_page,items,page Integer}'+
    'releases[status,title,format,label,artist RawUTF8 year,id integer]';
  __TSQLRestCacheEntryValue = 'ID: Int64; Timestamp512,Tag: cardinal; JSON: RawUTF8';
  __TSubAB = 'a : RawUTF8; b : integer;';
  __TSubCD = 'c : byte; d : RawUTF8;';
  __TAggregate = 'abArr : array of TSubAB; cdArr : array of TSubCD;';

  zendframeworkFileName = 'zendframework.json';
  discogsFileName = 'discogs.json';

procedure TTestLowLevelTypes.EncodeDecodeJSON;
var J,U,U2: RawUTF8;
    P: PUTF8Char;
    binary,zendframeworkJson,discogsJson: RawByteString;
    V: array[0..4] of TValuePUTF8Char;
    i, a, err: integer;
    r: Double;
    Parser: TJSONRecordTextDefinition;
    JR,JR2: TTestCustomJSONRecord;
    JA,JA2: TTestCustomJSONArray;
    JAS: TTestCustomJSONArraySimple;
{$ifndef NOVARIANTS}
    JAV: TTestCustomJSONArrayVariant;
    GDtoObject: TDtoObject;
{$endif}
    Trans: TTestCustomJSON2;
    Disco: TTestCustomDiscogs;
    Cache: TSQLRestCacheEntryValue;
{$ifndef DELPHI5OROLDER}
    peop: TSQLRecordPeople;
    K: RawUTF8;
    Valid: boolean;
    RB: TSQLRawBlob;
    Enemy: TEnemy;
{$ifndef LVCL}
    Instance: TClassInstance;
    Coll, C2: TCollTst;
    MyItem: TCollTest;
    Comp: TComplexNumber;
    DA: TDynArray;
    F: TFV;
    TLNow: TTimeLog;

  procedure TestMyColl(MyColl: TMyCollection);
  begin
    if CheckFailed(MyColl<>nil) then
      exit;
    MyItem := MyColl.Add as TCollTest;
    Check(MyItem.ClassType=TCollTest);
    MyItem.Length := 10;
    MyItem.Color := 20;
    MyItem.Name := 'ABC';
    U := ObjectToJSON(MyColl);
    CheckEqual(U,'[{"Color":20,"Length":10,"Name":"ABC"}]');
    MyColl.Free;
  end;
  procedure TCollTstDynArrayTest;
  var CA: TCollTstDynArray;
      i: integer;
      tmp: RawByteString;
      pu: PUTF8Char;
  begin
    CA := TCollTstDynArray.Create;
    try
      CA.Str := TStringList.Create;
      tmp := J;
      Check(JSONToObject(CA,UniqueRawUTF8(RawUTF8(tmp)),Valid)=nil);
      Check(Valid);
      Check(CA.One.Color=2);
      Check(CA.One.Name='test2');
      if not CheckFailed(CA.Coll.Count=1) then
        Check(CA.Coll[0].Name='test');
      Check(CA.One.Length=10);
      Check(CA.Str.Count=10000);
      for i := 1 to CA.Str.Count do
        Check(CA.Str[i-1]=IntToStr(i));
      SetLength(CA.fInts,20000);
      for i := 0 to high(CA.Ints) do
        CA.Ints[i] := i;
      U := ObjectToJSON(CA);
      check(IsValidJSON(U));
    finally
      CA.Free;
    end;
    CA := TCollTstDynArray.Create;
    try
      CA.Str := TStringList.Create;
      Check(JSONToObject(CA,pointer(U),Valid)=nil);
      Check(Valid);
      Check(CA.Str.Count=10000);
      for i := 1 to CA.Str.Count do
        Check(CA.Str[i-1]=IntToStr(i));
      Check(length(CA.Ints)=20000);
      for i := 0 to high(CA.Ints) do
        CA.Ints[i] := i;
      SetLength(CA.fTimeLog,CA.Str.Count);
      TLNow := TimeLogNow and (not 63);
      for i := 0 to high(CA.TimeLog) do
        CA.TimeLog[i] := TLNow+i and 31; // and 31 to avoid min:sec rounding
      U := ObjectToJSON(CA);
      SetLength(CA.fInts,2);
      SetLength(CA.fTimeLog,2);
      Check(JSONToObject(CA,pointer(U),Valid)=nil);
      Check(Valid);
      Check(Length(CA.Ints)=20000);
      Check(Length(CA.TimeLog)=CA.Str.Count);
      for i := 0 to high(CA.Ints) do
        Check(CA.Ints[i]=i);
      for i := 0 to high(CA.TimeLog) do
        Check(CA.TimeLog[i]=TLNow+i and 31);
      DA.Init(TypeInfo(TFVs),CA.fFileVersions);
      for i := 1 to 1000 do begin
        F.Major := i;
        F.Minor := i+2000;
        F.Release := i+3000;
        F.Build := i+4000;
        str(i,F.Main);
        str(i+1000,F.Detailed);
        DA.Add(F);
      end;
      U := ObjectToJSON(CA);
      check(IsValidJSON(U));
      DA.Clear;
      Check(Length(CA.FileVersion)=0);
      pu := JSONToObject(CA,pointer(U),Valid);
      Check(pu=nil);
      Check(Valid);
      Check(Length(CA.Ints)=20000);
      Check(Length(CA.TimeLog)=CA.Str.Count);
      Check(Length(CA.FileVersion)=1000);
      for i := 1 to 1000 do
      with CA.FileVersion[i-1] do begin
        Check(Major=i);
        Check(Minor=i+2000);
        Check(Release=i+3000);
        Check(Build=i+4000);
        Check(Main=IntToStr(i));
        Check(Detailed=IntToStr(i+1000));
      end;
    finally
      CA.Free;
    end;
  end;
  procedure TFileVersionTest(Full: boolean);
  var V,F: TFileVersion;
      J: RawUTF8;
      i: integer;
      Valid: boolean;
  begin
    V := TFileVersion.Create('',0,0,0,0);
    F := TFileVersion.Create('',0,0,0,0);
    try
      for i := 1 to 1000 do begin
        if Full then begin
          V.Major := i;
          V.Minor := i+2000;
          V.Release := i+3000;
          V.Build := i+4000;
          str(i,V.Main);
        end;
        V.BuildDateTime := 4090.0+i;
        J := ObjectToJSON(V);
        check(IsValidJSON(J));
        JSONToObject(F,pointer(J),Valid);
        if CheckFailed(Valid) then
          continue;
        if Full then begin
          Check(F.Major=i);
          Check(F.Minor=V.Minor);
          Check(F.Release=V.Release);
          Check(F.Build=V.Build);
          Check(F.Main=V.Main);
        end;
        CheckSame(V.BuildDateTime,F.BuildDateTime);
      end;
    finally
      F.Free;
      V.Free;
    end;
  end;
  {$endif}
  {$endif}
  procedure ABCD;
  begin
    Check(Parser.Root.NestedProperty[0].PropertyName='A');
    Check(Parser.Root.NestedProperty[0].PropertyType=ptInteger);
    Check(Parser.Root.NestedProperty[1].PropertyName='B');
    Check(Parser.Root.NestedProperty[1].PropertyType=ptInteger);
    Check(Parser.Root.NestedProperty[2].PropertyName='C');
    Check(Parser.Root.NestedProperty[2].PropertyType=ptInteger);
    Check(Parser.Root.NestedProperty[3].PropertyName='D');
    Check(Parser.Root.NestedProperty[3].PropertyType=ptRawUTF8);
  end;
  procedure ABCDE(Typ: TJSONCustomParserRTTIType);
  begin
    ABCD;
    with Parser.Root.NestedProperty[4] do begin
      Check(PropertyName='E');
      Check(PropertyType=Typ);
      Check(length(NestedProperty)=2);
      Check(NestedProperty[0].PropertyName='E1');
      Check(NestedProperty[0].PropertyType=ptDouble);
      Check(NestedProperty[1].PropertyName='E2');
      Check(NestedProperty[1].PropertyType=ptDouble);
    end;
  end;
  procedure TestGit(Options: TJSONCustomParserSerializationOptions);
  var i: Integer;
      U: RawUTF8;
      s: RawJSON;
      git,git2: TTestCustomJSONGitHubs;
      item,value: PUTF8Char;
  begin
    if zendframeworkJson='' then
      exit; // avoid GPF e.g. on Windows XP where https is broken
    TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TTestCustomJSONGitHub),
      __TTestCustomJSONGitHub).Options := Options;
    FillCharFast(git,sizeof(git),0);
    FillCharFast(git2,sizeof(git2),0);
    U := zendframeworkJson; // need unique string for procedure re-entrance
    check(IsValidJSON(U));
    Check(DynArrayLoadJSON(git,UniqueRawUTF8(U),TypeInfo(TTestCustomJSONGitHubs))<>nil);
    U := DynArraySaveJSON(git,TypeInfo(TTestCustomJSONGitHubs));
    check(IsValidJSON(U));
    if soWriteHumanReadable in Options then
      FileFromString(U,'zendframeworkSaved.json');
    Check(length(git)>=30);
    Check(length(U)>3000);
    if git[0].id=8079771 then begin
      Check(git[0].name='Component_ZendAuthentication');
      Check(git[0].description='Authentication component from Zend Framework 2');
      Check(git[0].owner.login='zendframework');
      Check(git[0].owner.id=296074);
    end;
    for i := 0 to high(git) do
    with git[i] do begin
      item := JSONArrayItem(Pointer(U),i);
      Check(item<>nil);
      value := JsonObjectItem(item,'name');
      check(value<>nil);
      GetJSONItemAsRawJSON(value,s);
      check(IsValidJSON(s));
      check(trim(s)='"'+name+'"');
      check(GetInteger(JsonObjectByPath(item,'owner.id'))=owner.id);
      check(GetInteger(JsonObjectByPath(item,'owner.i*'))=owner.id);
      check(JsonObjectByPath(item,'owner.name')='');
      check(JsonObjectsByPath(item,'toto')='');
      check(JsonObjectsByPath(item,'toto,titi')='');
      check(JsonObjectsByPath(item,'toto,name')='{"name":"'+name+'"}');
      check(JsonObjectsByPath(item,'toto,n*')='{"name":"'+name+'"}');
      check(JsonObjectsByPath(item,'fork,toto,owner.id,name')=
        FormatUTF8('{"fork":%,"owner.id":%,"name":"%"}',
        [BOOL_STR[fork],owner.id,name]));
      check(JsonObjectsByPath(item,'owner.i*')=FormatUTF8('{"owner.id":%}',[owner.id]));
      check(JsonObjectsByPath(item,'owner.*')=FormatUTF8(
        '{"owner.login":"%","owner.id":%}',[owner.login,owner.id]));
      value := JsonObjectByPath(item,'owner');
      GetJSONItemAsRawJSON(value,s);
      check(IsValidJSON(s));
      check(JSONReformat(s,jsonCompact)=FormatUTF8(
        '{"login":"%","id":%}',[owner.login,owner.id]));
    end;
    Check(DynArrayLoadJSON(git2,pointer(U),TypeInfo(TTestCustomJSONGitHubs))<>nil);
    if not CheckFailed(length(git)=Length(git2)) then
      for i := 0 to high(git) do begin
        Check(git[i].name=git2[i].name);
        Check(git[i].id=git2[i].id);
        Check(git[i].description=git2[i].description);
        Check(git[i].fork=git2[i].fork);
        Check(git[i].owner.login=git2[i].owner.login);
        Check(git[i].owner.id=git2[i].owner.id);
      end;
    TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TTestCustomJSONGitHub),'');
  end;
  {$ifndef DELPHI5OROLDER}
  function uct(const s: RawUTF8): TSQLFieldType;
  begin
    result := UTF8ContentNumberType(pointer(s));
  end;
  var O,O2: TPersistentToJSON;
      E: TSynBackgroundThreadProcessStep;
      EndOfObject: AnsiChar;
  {$endif}
  {$ifndef NOVARIANTS}
  var Va, Vb: Variant;
      c: currency;
  {$endif}
  procedure TestJSONSerialization;
  var ab0,ab1: TSubAB;
      cd0,cd1,cd2: TSubCD;
      agg,agg2: TAggregate;
      X: RawUTF8;
      AA,AB: TRawUTF8DynArrayDynArray;
      i,a{$ifndef NOVARIANTS},v{$endif}: Integer;
  {$ifdef ISDELPHI2010}
      nav,nav2: TConsultaNav;
      nrtti,nrtti2: TNewRTTI;
      book: TBookRecord;
  {$endif}
  begin
    Finalize(JR);
    Finalize(JR2);
    Finalize(JA);
    Finalize(JA2);
    FillCharFast(JR,sizeof(JR),0);
    FillCharFast(JR2,sizeof(JR2),0);
    FillCharFast(JA,sizeof(JA),0);
    FillCharFast(JA2,sizeof(JA2),0);
    U := RecordSaveJSON(JR,TypeInfo(TTestCustomJSONRecord));
    CheckEqual(U,'{"A":0,"B":0,"C":0,"D":"","E":{"E1":0,"E2":0},"F":""}');
    check(IsValidJSON(U));
    X := JSONToXML(U,'');
    Check(X='<A>0</A><B>0</B><C>0</C><D></D><E><E1>0</E1><E2>0</E2></E><F></F>');
    J := JSONToXML(U,'',XMLUTF8_NAMESPACE);
    CheckEqual(J,XMLUTF8_NAMESPACE+X+'</contents>');
    J := RecordSaveJSON(JA,TypeInfo(TTestCustomJSONArray));
    CheckEqual(J,'{"A":0,"B":0,"C":0,"D":null,"E":[],"F":""}');
    check(IsValidJSON(J));
    X := JSONToXML(J,'');
    Check(X='<A>0</A><B>0</B><C>0</C><D>null</D><F></F>');
    JR2.A := 10;
    JR2.D := '**';
    JR2.F := 1;
    JR := JR2;
    RecordLoadJSON(JR2,pointer(U),TypeInfo(TTestCustomJSONRecord));
    Check(JR2.A=0);
    Check(JR2.D='');
    Check(JR2.F=0);
    U := RecordSaveJSON(JR2,TypeInfo(TTestCustomJSONRecord));
    CheckEqual(U,'{"A":0,"B":0,"C":0,"D":"","E":{"E1":0,"E2":0},"F":""}');
    check(IsValidJSON(U));
    U := RecordSaveJSON(JR,TypeInfo(TTestCustomJSONRecord));
    CheckEqual(U,'{"A":10,"B":0,"C":0,"D":"**","E":{"E1":0,"E2":0},"F":"1899-12-31"}');
    check(IsValidJSON(U));
    JA2.A := 10;
    JA2.D := '**';
    SetLength(JA2.E,2);
    JA2.F := 1;
    RecordLoadJSON(JA2,pointer(J),TypeInfo(TTestCustomJSONArray));
    Check(JA2.A=0);
    Check(JA2.D='');
    check(Length(JA2.E)=0);
    Check(JA2.F=0);
    J := RecordSaveJSON(JA2,TypeInfo(TTestCustomJSONArray));
    CheckEqual(J,'{"A":0,"B":0,"C":0,"D":null,"E":[],"F":""}');
    check(IsValidJSON(J));
    JA2.A := 100;
    JA2.F := 1;
    J := RecordSaveJSON(JA2,TypeInfo(TTestCustomJSONArray));
    CheckEqual(J,'{"A":100,"B":0,"C":0,"D":null,"E":[],"F":"1899-12-31"}');
    check(IsValidJSON(J));
    SetLength(JA2.E,2);
    JA2.E[0].E1 := 1;
    JA2.E[0].E2 := '2';
    JA2.E[1].E1 := 3;
    JA2.E[1].E2 := '4';
    J := RecordSaveJSON(JA2,TypeInfo(TTestCustomJSONArray));
    CheckEqual(J,'{"A":100,"B":0,"C":0,"D":null,"E":[{"E1":1,"E2":"2"},{"E1":3,"E2":"4"}],"F":"1899-12-31"}');
    check(IsValidJSON(J));
    X := JSONToXML(J,'');
    Check(X='<A>100</A><B>0</B><C>0</C><D>null</D><E><E1>1</E1><E2>2</E2></E><E><E1>3</E1><E2>4</E2></E><F>1899-12-31</F>');
    RecordLoadJSON(JA,pointer(J),TypeInfo(TTestCustomJSONArray));
    Check(RecordSave(JA,TypeInfo(TTestCustomJSONArray))=RecordSave(JA2,TypeInfo(TTestCustomJSONArray)));
    J := '{"A":0,"B":0,"C":0,"D":null,"E":[{"E1":2,"E2":"3"}],"F":""}';
    check(IsValidJSON(J));
    RecordLoadJSON(JA,UniqueRawUTF8(J),TypeInfo(TTestCustomJSONArray));
    U := RecordSaveJSON(JA,TypeInfo(TTestCustomJSONArray));
    Check(length(JA.E)=1);
    CheckEqual(U,'{"A":0,"B":0,"C":0,"D":null,"E":[{"E1":2,"E2":"3"}],"F":""}');
    check(IsValidJSON(U));
    X := JSONToXML(U,'');
    Check(X='<A>0</A><B>0</B><C>0</C><D>null</D><E><E1>2</E1><E2>3</E2></E><F></F>');
    X := JSONToXML('[1,2,"three"]');
    Check(X='<?xml version="1.0" encoding="UTF-8"?>'#$D#$A'<0>1</0><1>2</1><2>three</2>');

    SetLength(AA,100);
    for i := 0 to high(AA) do begin
      SetLength(AA[i],random(100));
      for a := 0 to high(AA[i]) do begin
        UInt32ToUtf8(i+a,AA[i,a]);
        check(IsValidJSON(AA[i,a]));
        check(IsValidJSON('    '+AA[i,a]));
        check(IsValidJSON(AA[i,a]+'  '));
      end;
    end;
    binary := DynArraySave(AA,TypeInfo(TRawUTF8DynArrayDynArray));
    Check(DynArrayLoad(AB,pointer(binary),TypeInfo(TRawUTF8DynArrayDynArray))<>nil);
    Check(length(AA)=length(AB));
    for i := 0 to high(AA) do begin
      Check(length(AA[i])=length(AB[i]));
      for a := 0 to high(AA[i]) do
        Check(AA[i,a]=AB[i,a]);
    end;
    j := DynArraySaveJSON(AA,TypeInfo(TRawUTF8DynArrayDynArray));
    check(IsValidJSON(j));
    Finalize(AB);
    Check(DynArrayLoadJSON(AB,pointer(j),TypeInfo(TRawUTF8DynArrayDynArray))<>nil);
    Check(length(AA)=length(AB));
    for i := 0 to high(AA) do begin
      Check(length(AA[i])=length(AB[i]));
      for a := 0 to high(AA[i]) do
        Check(AA[i,a]=AB[i,a]);
    end;

    ab0.a := 'AB0';
    ab0.b := 0;
    ab1.a := 'AB1';
    ab1.b := 1;
    cd0.c := 0;
    cd0.d := 'CD0';
    cd1.c := 1;
    cd1.d := 'CD1';
    cd2.c := 2;
    cd2.d := 'CD2';
    SetLength(agg.abArr,2);
    agg.abArr[0] := ab0;
    agg.abArr[1] := ab1;
    SetLength(agg.cdArr,3);
    agg.cdArr[0] := cd0;
    agg.cdArr[1] := cd1;
    agg.cdArr[2] := cd2;
    u := '{"abArr":[{"a":"AB0","b":0},{"a":"AB1","b":1}],"cdArr":[{"c":0,"d":"CD0"},'+
      '{"c":1,"d":"CD1"},{"c":2,"d":"CD2"}]}';
    Check(Hash32(u)=$E3AC9C44);
    check(IsValidJSON(u));
    Check(RecordSaveJSON(agg,TypeInfo(TAggregate))=u);
    RecordLoadJSON(agg2,UniqueRawUTF8(u),TypeInfo(TAggregate));
    j := RecordSaveJSON(agg2,TypeInfo(TAggregate));
    Check(Hash32(j)=$E3AC9C44);
    check(IsValidJSON(j));

    Finalize(JAS);
    FillCharFast(JAS,sizeof(JAS),0);
    U := RecordSaveJSON(JAS,TypeInfo(TTestCustomJSONArraySimple));
    CheckEqual(U,'{"A":0,"B":0,"C":[],"D":"","E":[],"H":""}');
    check(IsValidJSON(U));
    U := '{"a":1,"b":2,"c":["C9A646D3-9C61-4CB7-BFCD-EE2522C8F633",'+
      '"3F2504E0-4F89-11D3-9A0C-0305E82C3301"],"d":"4","e":[{"f":"f","g":["g1","g2"]}],"h":"h"}';
    J := U;
    RecordLoadJSON(JAS,UniqueRawUTF8(U),TypeInfo(TTestCustomJSONArraySimple));
    Check(JAS.A=1);
    Check(JAS.B=2);
    Check(length(JAS.C)=2);
    Check(GUIDToRawUTF8(JAS.C[0])='{C9A646D3-9C61-4CB7-BFCD-EE2522C8F633}');
    Check(GUIDToRawUTF8(JAS.C[1])='{3F2504E0-4F89-11D3-9A0C-0305E82C3301}');
    Check(JAS.D='4');
    Check(length(JAS.E)=1);
    Check(JAS.E[0].F='f');
    Check(Length(JAS.E[0].G)=2);
    Check(JAS.E[0].G[0]='g1');
    Check(JAS.E[0].G[1]='g2');
    Check(JAS.H='h');
    U := RecordSaveJSON(JAS,TypeInfo(TTestCustomJSONArraySimple));
    Check(SameTextU(J,U));
    check(IsValidJSON(U));

  {$ifndef NOVARIANTS}
    Finalize(JAV);
    FillCharFast(JAV,sizeof(JAV),0);
    U := RecordSaveJSON(JAV,TypeInfo(TTestCustomJSONArrayVariant));
    CheckEqual(U,'{"A":0,"B":0,"C":[],"D":""}');
    check(IsValidJSON(U));
    assert(DocVariantType<>nil);
    U := '{"a":1,"b":2,"c":["one",2,2.5,{four:[1,2,3,4]}],"d":"4"}';
    check(IsValidJSON(U));
    RecordLoadJSON(JAV,UniqueRawUTF8(U),TypeInfo(TTestCustomJSONArrayVariant));
    Check(JAV.A=1);
    Check(JAV.B=2);
    if not CheckFailed(length(JAV.C)=4) then begin
      Check(JAV.C[0]='one');
      Check(JAV.C[1]=2);
      CheckSame(JAV.C[2],2.5);
      Check(JAV.C[3]._Kind=ord(dvObject));
      Check(JAV.C[3]._Count=1);
      Check(JAV.C[3].Name(0)='four');
      Check(VariantSaveJSON(JAV.C[3].four)='[1,2,3,4]');
      with DocVariantData(JAV.C[3])^ do begin
        Check(Kind=dvObject);
        Check(Count=1);
        Check(Names[0]='four');
        Check(Values[0]._Kind=ord(dvArray));
        Check(Values[0]._Count=4);
        with DocVariantData(Values[0])^ do begin
          Check(Kind=dvArray);
          Check(Count=4);
          for v := 0 to Count-1 do
            Check(Values[v]=v+1);
        end;
      end;
    end;
    Check(JAV.D='4');
    GDtoObject := TDtoObject.Create;
    U := '{"SomeField":"Test"}';
    Check(ObjectLoadJSON(GDtoObject, U, nil, []),'nestedvariant1');
     J := ObjectToJSON(GDtoObject, []);
    CheckEqual(J,'{"NestedObject":{"FieldString":"","FieldInteger":0,'+
      '"FieldVariant":null},"SomeField":"Test"}');
    J := ObjectToJSON(GDtoObject, [woDontStore0]);
    CheckEqual(J,U);
    U := '{"NestedObject":{"FieldVariant":{"a":1,"b":2}},"SomeField":"Test"}';
    Check(ObjectLoadJSON(GDtoObject, U, nil, [j2oHandleCustomVariants]),'nestedvariant2');
    J := ObjectToJSON(GDtoObject, [woDontStore0,woDontStoreEmptyString]);
    CheckEqual(J,U);
    GDtoObject.Free;
  {$endif NOVARIANTS}

    Finalize(Cache);
    FillCharFast(Cache,sizeof(Cache),0);
    U := RecordSaveJSON(Cache,TypeInfo(TSQLRestCacheEntryValue));
    CheckEqual(U,'{"ID":0,"Timestamp512":0,"Tag":0,"JSON":""}');
    check(IsValidJSON(U));
    Cache.ID := 10;
    Cache.Timestamp512 := 200;
    Cache.JSON := 'test';
    Cache.Tag := 12;
    U := RecordSaveJSON(Cache,TypeInfo(TSQLRestCacheEntryValue));
    CheckEqual(U,'{"ID":10,"Timestamp512":200,"Tag":12,"JSON":"test"}');
    check(IsValidJSON(U));
    U := '{"ID":210,"Timestamp512":2200,"JSON":"test2"}';
    check(IsValidJSON(U));
    RecordLoadJSON(Cache,UniqueRawUTF8(U),TypeInfo(TSQLRestCacheEntryValue));
    Check(Cache.ID=210);
    Check(Cache.Timestamp512=2200);
    Check(Cache.JSON='test2');
    Check(Cache.Tag=12);
    U := '{ID:220,JSON:"test3",Timestamp512:2300}';
    check(IsValidJSON(U));
    RecordLoadJSON(Cache,UniqueRawUTF8(U),TypeInfo(TSQLRestCacheEntryValue));
    Check(Cache.ID=220);
    Check(Cache.Timestamp512=2300);
    Check(Cache.JSON='test3');
    Check(Cache.Tag=12);

    {$ifdef ISDELPHI2010}
    FillCharFast(nav,sizeof(nav),0);
    FillCharFast(nav2,sizeof(nav2),1);
    Check(not CompareMem(@nav,@nav2,sizeof(nav)));
    Check(nav2.MaxRows<>0);
    check(nav2.EOF);
    U := RecordSaveJSON(nav,TypeInfo(TConsultaNav));
    J := RecordSaveJSON(nav2,TypeInfo(TConsultaNav));
    Check(U<>J);
    RecordLoadJSON(nav2,UniqueRawUTF8(U),TypeInfo(TConsultaNav));
    Check(nav2.MaxRows=0);
    check(not nav2.EOF);
    J := RecordSaveJSON(nav2,TypeInfo(TConsultaNav));
    CheckEqual(J,RecordSaveJSON(nav,TypeInfo(TConsultaNav)));
    Check(CompareMem(@nav,@nav2,sizeof(nav)));
    Finalize(nrtti);
    FillCharFast(nrtti,sizeof(nrtti),0);
    U := RecordSaveJSON(nrtti,TypeInfo(TNewRTTI));
    CheckEqual(U,'{"Number":0,"StaticArray":[{"Name":"","Single":0,"Double":0},'+
       '{"Name":"","Single":0,"Double":0}],"Int":[0,0,0,0,0]}');
    Finalize(nrtti2);
    FillCharFast(nrtti2,sizeof(nrtti2),0);
    Check(RecordLoadJSON(nrtti2,pointer(U),TypeInfo(TNewRTTI))<>nil);
    J := RecordSaveJSON(nrtti2,TypeInfo(TNewRTTI));
    CheckEqual(J,RecordSaveJSON(nrtti,TypeInfo(TNewRTTI)));
    nrtti.Number := 1;
    nrtti.StaticArray[1].Name := 'one';
    nrtti.StaticArray[1].Single := 1.5;
    nrtti.StaticArray[1].Double := 1.7;
    nrtti.StaticArray[2].Name := 'two';
    nrtti.StaticArray[2].Single := 2.5;
    nrtti.StaticArray[2].Double := 2.7;
    nrtti.Int[1] := 1;
    nrtti.Int[2] := 2;
    nrtti.Int[3] := 3;
    nrtti.Int[4] := 4;
    nrtti.Int[5] := 5;
    U := RecordSaveJSON(nrtti,TypeInfo(TNewRTTI));
    CheckEqual(U,'{"Number":1,"StaticArray":[{"Name":"one","Single":1.5,"Double":1.7},'+
      '{"Name":"two","Single":2.5,"Double":2.7}],"Int":[1,2,3,4,5]}');
    Finalize(nrtti2);
    FillCharFast(nrtti2,sizeof(nrtti2),0);
    Check(RecordLoadJSON(nrtti2,pointer(U),TypeInfo(TNewRTTI))<>nil);
    J := RecordSaveJSON(nrtti2,TypeInfo(TNewRTTI));
    CheckEqual(J,RecordSaveJSON(nrtti,TypeInfo(TNewRTTI)));
    U :='{ "name": "Book the First", "author": { "first_name": "Bob", "last_name": "White" } }';
    RecordLoadJSON(Book,UniqueRawUTF8(U),TypeInfo(TBookRecord));
    check(Book.name='Book the First');
    check(Book.author.first_name='Bob');
    Check(Book.author.last_name='White');
    {$endif}
  end;
  procedure TestGetJsonField(const s,v: RawUTF8; str,error: boolean; eof,next: AnsiChar);
  var P,d: PUTF8Char;
      ws: boolean;
      e: AnsiChar;
      l: integer;
      s2: RawUTF8;
  begin
    s2 := s;
    P := UniqueRawUTF8(s2);
    P := GetJSONField(P,d,@ws,@e,@l);
    check(error=(d=nil));
    if d=nil then
      exit;
    check(str=ws);
    check(eof=e);
    check(d^=next);
    check(l=length(v));
    check(CompareMem(P,pointer(v),length(v)));
  end;

begin
  TestGetJsonField('','',false,true,#0,#0);
  TestGetJsonField('true,false','true',false,false,',','f');
  TestGetJsonField('false,1','false',false,false,',','1');
  TestGetJsonField('"true",false','true',true,false,',','f');
  TestGetJsonField('"",false','',true,false,',','f');
  TestGetJsonField('12,false','12',false,false,',','f');
  TestGetJsonField('12]','12',false,true,']',#0);
  TestGetJsonField('12],','12',false,false,']',',');
  TestGetJsonField('1.2],','1.2',false,false,']',',');
  TestGetJsonField('1.2  ],','1.2',false,false,']',',');
  TestGetJsonField('"123"},false','123',true,false,'}',',');
  TestGetJsonField('"1\\3"},false','1\3',true,false,'}',',');
  TestGetJsonField('"1\r\n"},false','1'#13#10,true,false,'}',',');
  TestGetJsonField('"\"3"},false','"3',true,false,'}',',');
  TestGetJsonField('"\u00013"},false',#1'3',true,false,'}',',');
  TestGetJsonField('"\u0020"},false',' ',true,false,'}',',');
  Check(GotoEndOfJSONString(PUTF8Char(PAnsiChar('"toto"')))='"');
  Check(GotoEndOfJSONString(PUTF8Char(PAnsiChar('"toto",')))='",');
  Check(GotoEndOfJSONString(PUTF8Char(PAnsiChar('"to'#0'to",')))^=#0);
  Check(GotoEndOfJSONString(PUTF8Char(PAnsiChar('"to\'#0'to",')))^='\');
  Check(GotoEndOfJSONString(PUTF8Char(PAnsiChar('"to\"to",')))='",');
  Check(GotoEndOfJSONString(PUTF8Char(PAnsiChar('"to\\"to",')))='"to",');
  Check(GotoEndOfJSONString(PUTF8Char(PAnsiChar('"to\\\\to",')))='",');
  Check(IsString('abc'));
  Check(IsString('NULL'));
  Check(IsString('null'));
  Check(IsString('false'));
  Check(IsString('FALSE'));
  Check(IsString('true'));
  Check(IsString('TRUE'));
  Check(not IsString('123'));
  Check(not IsString('0123'));
  Check(not IsString('0.123'));
  Check(not IsString('1E19'));
  Check(not IsString('1.23E1'));
  Check(not IsString('+0'));
  Check(IsString('1.23E'));
  Check(IsString('+'));
  Check(IsString('-'));
  Check(IsStringJSON('abc'));
  Check(IsStringJSON('NULL'));
  Check(not IsStringJSON('null'));
  Check(not IsStringJSON('false'));
  Check(IsStringJSON('FALSE'));
  Check(not IsStringJSON('true'));
  Check(IsStringJSON('TRUE'));
  Check(not IsStringJSON('123'));
  Check(IsStringJSON('0123'));
  Check(not IsStringJSON('0.123'));
  Check(not IsStringJSON('1E19'));
  Check(not IsStringJSON('1.23E1'));
  Check(not IsStringJSON('0'));
  Check(not IsStringJSON('0.1'));
  Check(not IsStringJSON('-0'));
  Check(not IsStringJSON('-0.1'));
  Check(IsStringJSON('+0'));
  Check(IsStringJSON('1.23E'));
  Check(IsStringJSON('+'));
  Check(IsStringJSON('-'));
  Check(not NeedsJsonEscape(''));
  Check(not NeedsJsonEscape('a'));
  Check(not NeedsJsonEscape('ab cd'));
  Check(not NeedsJsonEscape('13456 ds0'));
  Check(NeedsJsonEscape('"123'));
  Check(NeedsJsonEscape('123"567'));
  Check(NeedsJsonEscape('123"'));
  Check(NeedsJsonEscape('123\"'));
  Check(NeedsJsonEscape('123'#1));
  Check(NeedsJsonEscape(#10'123'));
  CheckEqual(QuotedStrJSON(''),'""');
  CheckEqual(QuotedStrJSON('a'),'"a"');
  CheckEqual(QuotedStrJSON(#30),'"\u001E"');
  CheckEqual(QuotedStrJSON('ab'),'"ab"');
  CheckEqual(QuotedStrJSON(' a'),'" a"');
  CheckEqual(QuotedStrJSON('a"'),'"a\""');
  CheckEqual(QuotedStrJSON('a""'),'"a\"\""');
  CheckEqual(QuotedStrJSON('""'),'"\"\""');
  CheckEqual(QuotedStrJSON('a"b"c'),'"a\"b\"c"');
  CheckEqual(QuotedStrJSON('a"b\c'),'"a\"b\\c"');
  CheckEqual(QuotedStrJSON('a"b'#10'c'),'"a\"b\nc"');
  CheckEqual(QuotedStrJSON('a'#13'b'#8'c'),'"a\rb\bc"');
  CheckEqual(QuotedStrJSON('a'#13'b'#1'c'),'"a\rb\u0001c"');
  CheckEqual(QuotedStrJSON('a'#13'b'#31'c'),'"a\rb\u001Fc"');
  CheckEqual(QuotedStrJSON('a'#13'b'#31),'"a\rb\u001F"');
  {$ifndef DELPHI5OROLDER}
  Check(UTF8ContentType('null')=sftUnknown);
  Check(UTF8ContentType('0')=sftInteger);
  Check(UTF8ContentType('123')=sftInteger);
  Check(UTF8ContentType('0123')=sftUTF8Text);
  Check(UTF8ContentType('-123')=sftInteger);
  Check(UTF8ContentType('123.1')=sftCurrency);
  Check(UTF8ContentType('123.12')=sftCurrency);
  Check(UTF8ContentType('123.1234')=sftCurrency);
  Check(UTF8ContentType('123.12345678')=sftFloat);
  Check(UTF8ContentType('1.13e+12')=sftFloat);
  Check(UTF8ContentType('1.13e12')=sftFloat);
  Check(UTF8ContentType('-1.13e-12')=sftFloat);
  Check(UTF8ContentType('1.13e+120')=sftFloat);
  Check(UTF8ContentType('1.13E120')=sftFloat);
  Check(UTF8ContentType('1.13E-120')=sftFloat);
  Check(UTF8ContentType('1.13E307')=sftFloat);
  Check(UTF8ContentType('1.13E-323')=sftFloat);
  Check(UTF8ContentType('1.13e+a3')=sftUTF8Text);
  Check(UTF8ContentType('1.13e+3a')=sftUTF8Text);
  Check(UTF8ContentType('1.13e+330')=sftUTF8Text);
  Check(UTF8ContentType('1.13e330')=sftUTF8Text);
  Check(UTF8ContentType('1.13e-330')=sftUTF8Text);
  Check(UTF8ContentType('420014165100E335')=sftUTF8Text);
  Check(UTF8ContentType('123.')=sftUTF8Text);
  Check(UTF8ContentType('123.a')=sftUTF8Text);
  Check(UTF8ContentType('123.1a')=sftUTF8Text);
  Check(UTF8ContentType('123.1234a')=sftUTF8Text);
  Check(UTF8ContentType('123-2')=sftUTF8Text);
  Check(uct('null')=sftUnknown);
  Check(uct('0')=sftInteger);
  Check(uct('123')=sftInteger);
  Check(uct('0123')=sftUTF8Text);
  Check(uct('-123')=sftInteger);
  Check(uct('123.1')=sftCurrency);
  Check(uct('123.12')=sftCurrency);
  Check(uct('123.12345678')=sftFloat);
  Check(uct('1.13e+12')=sftFloat);
  Check(uct('-1.13e-12')=sftFloat);
  Check(uct('123.')=sftUTF8Text);
  Check(uct('123.a')=sftUTF8Text);
  Check(uct('123.1a')=sftUTF8Text);
  Check(uct('123.1234a')=sftUTF8Text);
  Check(uct('123-2')=sftUTF8Text);
  {$endif}
  J := JSONEncode(['name','john','year',1982,'pi',3.14159]);
  CheckEqual(J,'{"name":"john","year":1982,"pi":3.14159}');
  check(IsValidJSON(J));
  JSONDecode(J,['year','pi','john','name'],@V);
  Check(V[0].Value='1982');
  Check(V[1].Value='3.14159');
  Check(V[2].Value=nil);
  Check(V[3].Value='john');
  J := '{surrogate:"\uD801\uDC00"}'; // see https://en.wikipedia.org/wiki/CESU-8
  check(IsValidJSON(J));
  JSONDecode(J,['surrogate'],@V);
  Check(V[0].ValueLen=4);
  Check(V[0].Value[0]=#$F0);
  Check(V[0].Value[1]=#$90);
  Check(V[0].Value[2]=#$90);
  Check(V[0].Value[3]=#$80);
  J := JSONEncode(['name','john','ab','[','a','b',']']);
  check(IsValidJSON(J));
  CheckEqual(J,'{"name":"john","ab":["a","b"]}');
  J := JSONEncode(['name','john','ab','[','a','b']);
  check(IsValidJSON(J));
  CheckEqual(J,'{"name":"john","ab":["a","b"]}');
  J := JSONEncode(['name','john','ab','[']);
  check(IsValidJSON(J));
  CheckEqual(J,'{"name":"john","ab":[]}');
  J := JSONEncode(['name','john','ab','{']);
  check(IsValidJSON(J));
  CheckEqual(J,'{"name":"john","ab":{}}');
  J := JSONEncode(['name','john','ab',nil]);
  check(IsValidJSON(J));
  CheckEqual(J,'{"name":"john","ab":null}');
  J := JSONEncode(['name','john','ab']);
  check(IsValidJSON(J));
  CheckEqual(J,'{"name":"john"}');
  J := JSONEncode(['name','john','{']);
  check(IsValidJSON(J));
  CheckEqual(J,'{"name":"john"}');
  J := JSONEncode(['name','john','[']);
  check(IsValidJSON(J));
  CheckEqual(J,'{"name":"john"}');
  J := JSONEncode(['name','john','ab','[','a','b',']','pi',3.14159]);
  check(IsValidJSON(J));
  CheckEqual(J,'{"name":"john","ab":["a","b"],"pi":3.14159}');
  J := JSONEncode(['doc','{','name','John','year',1982,'}','id',123]);
  check(IsValidJSON(J));
  CheckEqual(J,'{"doc":{"name":"John","year":1982},"id":123}');
  J := JSONEncode(['doc','{','name','John','abc','[','a','b','c',']','}','id',123]);
  check(IsValidJSON(J));
  CheckEqual(J,'{"doc":{"name":"John","abc":["a","b","c"]},"id":123}');
{$ifndef NOVARIANTS}
  J := JSONEncode('{%:{$in:[?,?]}}',['type'],['food','snack']);
check(IsValidJSON(J));
  CheckEqual(J,'{"type":{"$in":["food","snack"]}}');
  Check(JSONEncode('{type:{$in:?}}',[],[_Arr(['food','snack'])])=J);
  check(IsValidJSON(J));
  J := JSONEncode('{name:"John",field:{ "$regex": "acme.*corp", $options: "i" }}',[],[]);
  CheckEqual(J,'{"name":"John","field":{"$regex":"acme.*corp","$options":"i"}}');
  // the below only works if unit SynMongoDB is included in the uses list of the project
  // for virtual function TryJSONToVariant
  CheckEqual(JSONEncode('{name:?,field:/%/i}',['acme.*corp'],['John']),J);
{$endif}
{$ifndef DELPHI5OROLDER}
  peop := TSQLRecordPeople.Create;
  try
    peop.IDValue := 1234;
    peop.FirstName := 'FN';
    peop.LastName := 'LN';
    peop.YearOfBirth := 1000;
    peop.Data := #1#2#3#4;
    J := ObjectToJSON(peop,[woSQLRawBlobAsBase64]);
    check(IsValidJSON(J));
    check(J[53]=#$EF);
    check(J[54]=#$BF);
    check(J[55]=#$B0);
    J[53] := '1';
    J[54] := '2';
    J[55] := '3';
    check(IsValidJSON(J));
    CheckEqual(J,'{"ID":1234,"FirstName":"FN","LastName":"LN",'+
      '"Data":"123AQIDBA==","YearOfBirth":1000,"YearOfDeath":0}');
    J := ObjectToJSON(peop);
    check(IsValidJSON(J));
    CheckEqual(J,'{"ID":1234,"FirstName":"FN","LastName":"LN",'+
      '"Data":"","YearOfBirth":1000,"YearOfDeath":0}');
    ClearObject(peop);
    J := ObjectToJSON(peop);
    check(IsValidJSON(J));
    CheckEqual(J,'{"ID":1234,"FirstName":"","LastName":"",'+
      '"Data":"","YearOfBirth":0,"YearOfDeath":0}');
    peop.IDValue := -1234;
    J := ObjectToJSON(peop);
    check(IsValidJSON(J));
    CheckEqual(J,'{"ID":-1234,"FirstName":"","LastName":"",'+
      '"Data":"","YearOfBirth":0,"YearOfDeath":0}');
   {$ifndef NOVARIANTS}
   peop.YearOfDeath := 10;
   peop.LastName := 'john';
   TObjectVariant.New(Va,peop);
   Check(Va.id=TID(-1234));
   Check(Va.FirstName='');
   Check(Va.LastName='john');
   Check(Va.YearOfDeath=10);
   J := VariantSaveJSON(Va);
   check(IsValidJSON(J));
   CheckEqual(J,'{"ID":-1234,"FirstName":"","LastName":"john","Data":"",'+
     '"YearOfBirth":0,"YearOfDeath":10}');
   {$endif}
  finally
    peop.Free;
  end;
{$endif}
  for i := 1 to 100 do begin
    a := Random(maxInt);
    r := Random;
    U := RandomUTF8(i);
    J := JSONEncode(['a',a,'r',r,'u',U]);
    check(IsValidJSON(J));
    JSONDecode(J,['U','R','A','FOO'],@V);
    V[0].ToUTF8(U2);
    Check(U2=U);
    Check(SameValue(GetExtended(V[1].Value,err),r));
    Check(not IsString(V[2].Value));
    Check(not IsStringJSON(V[2].Value));
    Check(V[2].ToInteger=a);
    Check(V[3].Value=nil);
    J := BinToBase64WithMagic(U);
    check(PInteger(J)^ and $00ffffff=JSON_BASE64_MAGIC);
{$ifndef DELPHI5OROLDER}
    RB := BlobToTSQLRawBlob(pointer(J));
    check(length(RB)=length(U)); // RB=U is buggy under FPC :(
    check(CompareMem(pointer(RB),pointer(U),length(U)));
    Base64MagicToBlob(@J[4],K);
    RB := BlobToTSQLRawBlob(pointer(K));
    check(length(RB)=length(U)); // RB=U is buggy under FPC :(
    check(CompareMem(pointer(RB),pointer(U),length(U)));
{    J := TSQLRestServer.JSONEncodeResult([r]);
    Check(SameValue(GetExtended(pointer(JSONDecode(J)),err),r)); }
    {$ifndef NOVARIANTS}
    with TTextWriter.CreateOwnedStream do
    try
      AddVariant(a);
      Add(',');
      AddVariant(r);
      Add(',');
      PInt64(@c)^ := a;
      AddVariant(c);
      Add(',');
      U := Int32ToUTF8(a);
      AddVariant(U);
      J := Text;
      CheckEqual(J,U+','+DoubleToStr(r)+','+DoubleToStr(c)+',"'+U+'"');
      P := UniqueRawUTF8(J);
      P := VariantLoadJSON(Va,P);
      Check(P<>nil);
      Check(Va=a);
      P := VariantLoadJSON(Va,P,nil,nil,true);
      Check(P<>nil);
      CheckSame(VariantToDoubleDef(Va),r);
      P := VariantLoadJSON(Va,P);
      Check(P<>nil);
      Check(Va=c);
      P := VariantLoadJSON(Va,P);
      Check((P<>nil) and (P^=#0));
      Check(Va=U);
      Vb := VariantLoad(VariantSave(Va),@JSON_OPTIONS[true]);
      Check(Vb=U);
    finally
      Free;
    end;
    {$endif}
{$endif}
  end;
{$ifndef DELPHI5OROLDER}
  J := GetJSONObjectAsSQL('{"ID":  1 ,"Name":"Alice","Role":"User","Last Login":null,'+
    '"First Login" :   null  ,  "Department"  :  "{\"relPath\":\"317\\\\\",\"revision\":1}" } ]', false, true);
  U := ' (ID,Name,Role,Last Login,First Login,Department) VALUES '+
    '(:(1):,:(''Alice''):,:(''User''):,:(null):,:(null):,:(''{"relPath":"317\\","revision":1}''):)';
  CheckEqual(J,U);
  J := GetJSONObjectAsSQL('{ "Name":"Alice","Role":"User","Last Login":null,'+
    '"First Login" :   null  ,  "Department"  :  "{\"relPath\":\"317\\\\\",\"revision\":1}" } ]', false, true,1,true);
  CheckEqual(J,U);
  J := GetJSONObjectAsSQL('{ "Name":"Alice","Role":"User","Last Login":null,'+
    '"First Login" :   null  ,  "Department"  :  "{\"relPath\":\"317\\\\\",\"revision\":1}" } ]', false, true,1,false);
  Insert('Row',U,3);
  CheckEqual(J,U);
  Delete(U,3,3);
  J := '{"ID":  1 ,"Name":"Alice","Role":"User","Last Login":null, // comment'#13#10+
    '"First Login" : /* to be ignored */  null  ,  "Department"  :  "{\"relPath\":\"317\\\\\",\"revision\":1}" } ]';
  check(not IsValidJSON(J));
  RemoveCommentsFromJSON(UniqueRawUTF8(J));
  check(not IsValidJSON(J));
  check(IsValidJSON('['+J));
  J := GetJSONObjectAsSQL(J,false,true);
  CheckEqual(J,U);
  J := '{'#10'"httpServer": {'#10'"host": "*",'#10'"port": "8881",'#10 +
    '"serverType": "Socket",'#10'/*"reverseProxy": {'#10'"kind": "nginx",'#10 +
    '"sendFileLocationRoot": "snake-ukrpatent-local"'#10'}*/'#10'} //eol'#10'}';
  check(not IsValidJSON(J));
  RemoveCommentsFromJSON(UniqueRawUTF8(J));
  CheckUTF8(IsValidJSON(J),J);
  J := JSONReformat(J,jsonCompact);
  CheckEqual(J,'{"httpServer":{"host":"*","port":"8881","serverType":"Socket"}}');
  J := '{"RowID":  210 ,"Name":"Alice","Role":"User","Last Login":null, // comment'#13#10+
    '"First Login" : /* to be ignored */  null  ,  "Department"  :  "{\"relPath\":\"317\\\\\",\"revision\":1}" } ]';
  check(not IsValidJSON(J));
  RemoveCommentsFromJSON(UniqueRawUTF8(J));
  check(not IsValidJSON(J));
  check(IsValidJSON('['+J));
  J := GetJSONObjectAsSQL(J,false,true,1,True);
  CheckEqual(J,U);
  O := TPersistentToJSON.Create;
  O2 := TPersistentToJSON.Create;
  try
    J := ObjectToJSON(O,[]);
    check(IsValidJSON(J));
    CheckEqual(J,'{"Name":"","Enum":0,"Sets":0}');
    J := ObjectToJSON(O,[woDontStoreDefault]);
    check(IsValidJSON(J));
    CheckEqual(J,'{"Name":""}');
    J := ObjectToJSON(O,[woStoreClassName]);
    check(IsValidJSON(J));
    CheckEqual(J,'{"ClassName":"TPersistentToJSON","Name":"","Enum":0,"Sets":0}');
    J := ObjectToJSON(O,[woHumanReadable]);
    check(IsValidJSON(J));
    CheckEqual(J,'{'#$D#$A#9'"Name": "",'#$D#$A#9'"Enum": "flagIdle",'#$D#$A#9'"Sets": []'#$D#$A'}');
    with PTypeInfo(TypeInfo(TSynBackgroundThreadProcessStep))^.EnumBaseType^ do
    for E := low(E) to high(E) do begin
      O.fName := Int32ToUTF8(ord(E));
      O.fEnum := E;
      include(O.fSets,E);
      J := ObjectToJSON(O,[]);
      check(IsValidJSON(J));
      CheckEqual(J,FormatUTF8('{"Name":"%","Enum":%,"Sets":%}',[ord(E),ord(E),byte(O.fSets)]));
      JSONToObject(O2,pointer(J),valid);
      Check(Valid);
      Check(O.Name=O2.Name);
      Check(O.Enum=O2.Enum);
      Check(O.Sets=O2.Sets);
      J := ObjectToJSON(O,[woHumanReadable]);
      check(IsValidJSON(J));
      U := FormatUTF8(
        '{'#$D#$A#9'"NAME": "%",'#$D#$A#9'"ENUM": "%",'#$D#$A#9'"SETS": ["FLAGIDLE"',
        [ord(E),UpperCaseU(RawUTF8(GetEnumName(E)^))]);
      Check(IdemPChar(pointer(J),pointer(U)));
      JSONToObject(O2,pointer(J),valid);
      Check(Valid);
      Check(O.Name=O2.Name);
      Check(O.Enum=O2.Enum);
      Check(O.Sets=O2.Sets);
      Check(ObjectEquals(O,O2));
    end;
    with PTypeInfo(TypeInfo(WordBool))^.EnumBaseType^ do
      Check(SizeInStorageAsEnum=2);
    J := ObjectToJSON(O,[woHumanReadable,woHumanReadableFullSetsAsStar]);
    check(IsValidJSON(J));
    CheckEqual(J,'{'#$D#$A#9'"Name": "3",'#$D#$A#9'"Enum": "flagDestroying",'#$D#$A#9'"Sets": ["*"]'#$D#$A'}');
    J := ObjectToJSON(O,[woHumanReadable,woHumanReadableFullSetsAsStar,woHumanReadableEnumSetAsComment]);
    CheckEqual(J,'{'#$D#$A#9'"Name": "3",'#$D#$A#9'"Enum": "flagDestroying", // "flagIdle","flagStarted","flagFinished","flagDestroying"'+
      #$D#$A#9'"Sets": ["*"] // "*" or a set of "flagIdle","flagStarted","flagFinished","flagDestroying"'#$D#$A'}');
    O2.fName := '';
    O2.fEnum := low(E);
    O2.fSets := [];
    check(not IsValidJSON(J));
    RemoveCommentsFromJSON(UniqueRawUTF8(J));
    check(IsValidJSON(J));
    JSONToObject(O2,pointer(J),valid);
    Check(Valid);
    Check(O.Name=O2.Name);
    Check(O.Enum=O2.Enum);
    Check(O.Sets=O2.Sets);
    Check(ObjectEquals(O,O2));
  finally
    O2.Free;
    O.Free;
  end;
  U := '"filters":[{"name":"name1","value":"value1","comparetype":">"},'+
    '{"name":"name2","value":"value2","comparetype":"="}], "Limit":100}';
  check(not IsValidJSON(U));
  check(IsValidJSON('{'+U));
  P := UniqueRawUTF8(U);
  Check(GetJSONPropName(P)='filters');
  Check((P<>nil)and(P^='['));
  P := GotoNextJSONItem(P,1,@EndOfObject);
  Check(EndOfObject=',');
  Check(GetJSONPropName(P)='Limit');
  Check((P<>nil)and(P^='1'));
  P := GotoNextJSONItem(P,1,@EndOfObject);
  Check(P<>nil);
  Check(EndOfObject='}');
  check(IsValidJSON('null'));
  check(IsValidJSON('true'));
  check(IsValidJSON('false'));
  check(IsValidJSON(' null'));
  check(IsValidJSON(' true'));
  check(IsValidJSON(' false'));
  check(IsValidJSON('null  '));
  check(IsValidJSON('true  '));
  check(IsValidJSON('false  '));
  check(not IsValidJSON('nulle'));
  check(not IsValidJSON('trye'));
{$ifndef LVCL}
  C2 := TCollTst.Create;
  Coll := TCollTst.Create;
  try
    U := ObjectToJSON(Coll);
    check(IsValidJSON(U));
    Check(Hash32(U)=$95B54414);
    Check(ObjectToJSON(C2)=U);
    Coll.One.Name := 'test"\2';
    Coll.One.Color := 1;
    U := ObjectToJSON(Coll);
    check(IsValidJSON(U));
    Check(Hash32(U)=$CE2C2DED);
    Check(JSONToObject(C2,pointer(U),Valid)=nil);
    Check(Valid);
    U := ObjectToJSON(C2);
    check(IsValidJSON(U));
    Check(Hash32(U)=$CE2C2DED);
    Coll.Coll.Add.Color := 10;
    Coll.Coll.Add.Name := 'name';
    Check(Coll.Coll.Count=2);
    U := ObjectToJSON(Coll);
    check(IsValidJSON(U));
    Check(Hash32(U)=$36B02F0E);
    Check(JSONToObject(C2,pointer(U),Valid)=nil);
    Check(Valid);
    Check(C2.Coll.Count=2);
    U := ObjectToJSON(C2);
    check(IsValidJSON(U));
    Check(Hash32(U)=$36B02F0E);
    J := ObjectToJSON(Coll,[woHumanReadable]);
    check(IsValidJSON(U));
    Check(Hash32(J)=$9FAFF11F);
    Check(JSONReformat(J,jsonCompact)=U);
    Check(JSONReformat('{ "empty": {} }')='{'#$D#$A#9'"empty": {'#$D#$A#9#9'}'#$D#$A'}');
    U := ObjectToJSON(Coll,[woStoreClassName]);
    check(IsValidJSON(U));
    CheckEqual(U,'{"ClassName":"TCollTst","One":{"ClassName":"TCollTest","Color":1,'+
      '"Length":0,"Name":"test\"\\2"},"Coll":[{"ClassName":"TCollTest","Color":10,'+
      '"Length":0,"Name":""},{"ClassName":"TCollTest","Color":0,"Length":0,"Name":"name"}]}');
    C2.Coll.Clear;
    Check(JSONToObject(C2,pointer(U),Valid)=nil);
    Check(Valid);
    Check(C2.Coll.Count=2);
    U := ObjectToJSON(C2);
    Check(Hash32(U)=$36B02F0E);
    TJSONSerializer.RegisterClassForJSON([TComplexNumber,TCollTst]);
    J := '{"ClassName":"TComplexNumber", "Real": 10.3, "Imaginary": 7.92 }';
    P := UniqueRawUTF8(J); // make local copy of constant
    Comp := TComplexNumber(JSONToNewObject(P,Valid));
    if not CheckFailed(Comp<>nil) then begin
      Check(Valid);
      Check(Comp.ClassType=TComplexNumber);
      CheckSame(Comp.Real,10.3);
      CheckSame(Comp.Imaginary,7.92);
      U := ObjectToJSON(Comp,[woStoreClassName]);
      check(IsValidJSON(U));
      CheckEqual(U,'{"ClassName":"TComplexNumber","Real":10.3,"Imaginary":7.92}');
      Comp.Free;
    end;
    TJSONSerializer.RegisterCollectionForJSON(TMyCollection,TCollTest);
    TestMyColl(TMyCollection.Create(TCollTest));
    Instance.Init(TMyCollection);
    TestMyColl(Instance.CreateNew as TMyCollection);
    C2.Coll.Clear;
    U := ObjectToJSON(C2);
    check(IsValidJSON(U));
    Check(Hash32(U)=$CE2C2DED);
    Coll.Coll.BeginUpdate;
    for i := 1 to 10000 do
      with Coll.Coll.Add do begin
        Color := i*3;
        Length := i*5;
        Name := Int32ToUtf8(i);
      end;
    Coll.Coll.EndUpdate;
    U := ObjectToJSON(Coll.Coll);
    check(IsValidJSON(U));
    Check(Hash32(U)=$DB782098);
    C2.Coll.Clear;
    Check(JSONToObject(C2.fColl,pointer(U),Valid)=nil);
    Check(Valid);
    Check(C2.Coll.Count=Coll.Coll.Count);
    for i := 1 to C2.Coll.Count-2 do
      with C2.Coll[i+1] do begin
        Check(Color=i*3);
        Check(Length=i*5);
        Check(Name=Int32ToUtf8(i));
      end;
    U := ObjectToJSON(Coll);
    check(IsValidJSON(U));
    Check(length(U)=443103);
    Check(Hash32(U)=$7EACF12A);
    C2.One.Name := '';
    C2.Coll.Clear;
    Check(JSONToObject(C2,pointer(U),Valid)=nil);
    Check(Valid);
    Check(C2.Coll.Count=Coll.Coll.Count);
    U := ObjectToJSON(C2);
    check(IsValidJSON(U));
    Check(length(U)=443103);
    Check(Hash32(U)=$7EACF12A);
    for i := 1 to C2.Coll.Count-2 do
      with C2.Coll[i+1] do begin
        Check(Color=i*3);
        Check(Length=i*5);
        Check(Name=Int32ToUtf8(i));
      end;
    Coll.Coll.Clear;
    Coll.Str := TStringList.Create;
    Coll.Str.BeginUpdate;
    for i := 1 to 10000 do
      Check(Coll.Str.Add(IntToStr(i))=i-1);
    Coll.Str.EndUpdate;
    U := ObjectToJSON(Coll);
    check(IsValidJSON(U));
    Check(Hash32(U)=$85926050);
    J := ObjectToJSON(Coll,[woHumanReadable]);
    check(IsValidJSON(J));
    U2 := JSONReformat(J,jsonCompact);
    check(IsValidJSON(U2));
    Check(U2=U);
    C2.Str := TStringList.Create;
    Check(JSONToObject(C2,pointer(U),Valid)=nil);
    Check(Valid);
    Check(C2.Str.Count=Coll.Str.Count);
    for i := 1 to C2.Str.Count do
      Check(C2.Str[i-1]=IntToStr(i));
    J := ObjectToJSON(C2);
    check(IsValidJSON(J));
    Check(Hash32(J)=$85926050);
    C2.One.Color := 0;
    C2.One.Name := '';
    U := '{"One":{"Color":1,"Length":0,"Name":"test","Unknown":123},"Coll":[]}';
    Check(JSONToObject(C2,UniqueRawUTF8(U),Valid,nil,[j2oIgnoreUnknownProperty])=nil,'Ignore unknown');
    Check(Valid);
    Check(C2.One.Color=1);
    Check(C2.One.Name='test');
    C2.One.Color := 0;
    C2.One.Name := '';
    U := '{"One":{"Color":1,"Length":0,"wtf":{"one":1},"Name":"test","Unknown":123},"dummy":null,"Coll":[]}';
    check(IsValidJSON(U));
    Check(JSONToObject(C2,UniqueRawUTF8(U),Valid,nil,[j2oIgnoreUnknownProperty])=nil,'Ignore unknown');
    Check(Valid);
    Check(C2.One.Color=1);
    Check(C2.One.Name='test');
    U := '{"One":{"Color":1,"Length":0,"Name":"test\"\\2},"Coll":[]}';
    Check(IdemPChar(JSONToObject(C2,UniqueRawUTF8(U),Valid),'"TEST'),'invalid JSON');
    Check(not Valid);
    U := '{"One":{"Color":1,"Length":0,"Name":"test\"\\2"},"Coll":[]';
    Check(JSONToObject(C2,UniqueRawUTF8(U),Valid)<>nil);
    Check(not Valid);
    U := '{"One":{"Color":,"Length":0,"Name":"test\"\\2"},"Coll":[]';
    Check(JSONToObject(C2,UniqueRawUTF8(U),Valid)<>nil,'invalid JSON');
    Check(not Valid);
    U := '{"Coll":[{"Color":1,"Length":0,"Name":"test"}],'+
      '"One":{"Color":2,"Length":0,"Name":"test2"}}';
    Check(JSONToObject(C2,UniqueRawUTF8(U),Valid,nil,[j2oIgnoreUnknownProperty])=nil,'Ignore unknown');
    Check(Valid);
    Check(C2.One.Color=2);
    Check(C2.One.Name='test2');
    Check(C2.Coll.Count=1);
    Check(C2.Coll[0].Name='test');
    C2.One.Length := 10;
    J := ObjectToJSON(C2);
    check(IsValidJSON(J));
    Check(Hash32(J)=$41281936);
    // (custom) dynamic array serialization
    TCollTstDynArrayTest;
    TTextWriter.RegisterCustomJSONSerializer(TypeInfo(TFVs),
      TCollTstDynArray.FVReader,TCollTstDynArray.FVWriter);
    TCollTstDynArrayTest;
    TTextWriter.RegisterCustomJSONSerializer(TypeInfo(TFVs),
      TCollTstDynArray.FVReader2,TCollTstDynArray.FVWriter2);
    TCollTstDynArrayTest;
    // (custom) class serialization
    TFileVersionTest(false);
    TJSONSerializer.RegisterCustomSerializer(TFileVersion,
      TCollTstDynArray.FVClassReader,TCollTstDynArray.FVClassWriter);
    TFileVersionTest(true);
    TJSONSerializer.RegisterCustomSerializer(TFileVersion,nil,nil);
    TFileVersionTest(false);
    MyItem := TCollTest.Create(nil);
    try
      MyItem.Length := 10;
      MyItem.Color := 20;
      MyItem.Name := 'ABC';
      J := ObjectToJSON(MyItem);
      Check(IsValidJSON(J));
      CheckEqual(J,'{"Color":20,"Length":10,"Name":"ABC"}');
      TJSONSerializer.RegisterCustomSerializerFieldNames(
        TCollTest,['name','length'],['n','len']);
      J := ObjectToJSON(MyItem);
      Check(IsValidJSON(J));
      CheckEqual(J,'{"Color":20,"len":10,"n":"ABC"}');
      J := ObjectToJSON(C2);
      Check(IsValidJSON(J));
      Check(Hash32(J)=$FFBC77A,'RegisterCustomSerializerFieldNames');
      TCollTstDynArrayTest;
      TJSONSerializer.RegisterCustomSerializerFieldNames(TCollTest,[],[]);
      J := ObjectToJSON(MyItem);
      CheckEqual(J,'{"Color":20,"Length":10,"Name":"ABC"}');
      J := ObjectToJSON(C2);
      Check(IsValidJSON(J));
      Check(Hash32(J)=$41281936,'unRegisterCustomSerializerFieldNames');
      TCollTstDynArrayTest;
      TJSONSerializer.RegisterCustomSerializerFieldNames(TCollTest,['length'],['']);
      J := ObjectToJSON(MyItem);
      Check(IsValidJSON(J));
      CheckEqual(J,'{"Color":20,"Name":"ABC"}','remove field');
      TJSONSerializer.RegisterCustomSerializerFieldNames(TCollTest,[],[]);
      J := ObjectToJSON(MyItem);
      Check(IsValidJSON(J));
      CheckEqual(J,'{"Color":20,"Length":10,"Name":"ABC"}');
    finally
      MyItem.Free;
    end;
  finally
    C2.Free;
    Coll.Free;
  end;
{$endif DELPHI5OROLDER}
{$endif LVCL}
  // test TJSONRecordTextDefinition parsing
  Parser := TJSONRecordTextDefinition.FromCache(nil,'Int: double');
  Check(Length(Parser.Root.NestedProperty)=1);
  Check(Parser.Root.NestedProperty[0].PropertyName='Int');
  Check(Parser.Root.NestedProperty[0].PropertyType=ptDouble);
  Parser := TJSONRecordTextDefinition.FromCache(nil,
    'A , B,C  : integer; D: RawUTF8');
  Check(Length(Parser.Root.NestedProperty)=4);
  ABCD;
  Parser := TJSONRecordTextDefinition.FromCache(nil,
    'A,B,C: integer; D: RawUTF8; E: record E1,E2: double; end;');
  Check(Length(Parser.Root.NestedProperty)=5);
  ABCDE(ptRecord);
  Parser := TJSONRecordTextDefinition.FromCache(nil,
    'A,B: integer; C: integer; D: RawUTF8; E: array of record E1,E2: double; end;');
  Check(Length(Parser.Root.NestedProperty)=5);
  ABCDE(ptArray);
  Parser := TJSONRecordTextDefinition.FromCache(nil,
    'A,B,C integer D RawUTF8 E{E1,E2 double}');
  Check(Length(Parser.Root.NestedProperty)=5);
  ABCDE(ptRecord);
  Parser := TJSONRecordTextDefinition.FromCache(nil,
    'A,B,C integer D RawUTF8 E{E1,E2 double}');
  Check(Length(Parser.Root.NestedProperty)=5,'from cache');
  ABCDE(ptRecord);
  Parser := TJSONRecordTextDefinition.FromCache(nil,
    'A,B,C integer D RawUTF8 E[E1,E2 double]');
  Check(Length(Parser.Root.NestedProperty)=5);
  ABCDE(ptArray);
  Parser := TJSONRecordTextDefinition.FromCache(nil,
    'A,B,C integer D RawUTF8 E[E1,E2 double] F: string');
  Check(Length(Parser.Root.NestedProperty)=6);
  ABCDE(ptArray);
  Check(Parser.Root.NestedProperty[5].PropertyName='F');
  Check(Parser.Root.NestedProperty[5].PropertyType=ptString);
  Parser := TJSONRecordTextDefinition.FromCache(nil,
    'A,B,C integer D RawUTF8 E[E1,E2 double] F: array of string');
  Check(Length(Parser.Root.NestedProperty)=6);
  ABCDE(ptArray);
  Check(Parser.Root.NestedProperty[5].PropertyName='F');
  Check(Parser.Root.NestedProperty[5].PropertyType=ptArray);
  Check(length(Parser.Root.NestedProperty[5].NestedProperty)=1);
  Check(Parser.Root.NestedProperty[5].NestedProperty[0].PropertyType=ptString);
  Parser := TJSONRecordTextDefinition.FromCache(nil,
    'A,B,C integer D RawUTF8 E[E1:{E1A:integer E1B:tdatetime E1C TDatetimeMS}E2 double]');
  Check(Length(Parser.Root.NestedProperty)=5);
  ABCD;
  with Parser.Root.NestedProperty[4] do begin
    Check(PropertyName='E');
    Check(PropertyType=ptArray);
    Check(length(NestedProperty)=2);
    Check(NestedProperty[0].PropertyName='E1');
    Check(NestedProperty[0].PropertyType=ptRecord);
    with NestedProperty[0] do begin
      Check(length(NestedProperty)=3);
      Check(NestedProperty[0].PropertyName='E1A');
      Check(NestedProperty[0].PropertyType=ptInteger);
      Check(NestedProperty[1].PropertyName='E1B');
      Check(NestedProperty[1].PropertyType=ptDateTime);
      Check(NestedProperty[2].PropertyName='E1C');
      Check(NestedProperty[2].PropertyType=ptDateTimeMS);
    end;
    Check(NestedProperty[1].PropertyName='E2');
    Check(NestedProperty[1].PropertyType=ptDouble);
  end;

  {$ifdef ISDELPHI2010}
  // test JSON serialization defined by Enhanced RTTI
  TestJSONSerialization;
  {$endif}

  // test TJSONRecordTextDefinition JSON serialization
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TSubAB),__TSubAB);
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TSubCD),__TSubCD);
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TAggregate),__TAggregate);
  TTextWriter.RegisterCustomJSONSerializerFromText(
    TypeInfo(TTestCustomJSONRecord),__TTestCustomJSONRecord);
  TTextWriter.RegisterCustomJSONSerializerFromText(
    TypeInfo(TTestCustomJSONArray),__TTestCustomJSONArray);
  TTextWriter.RegisterCustomJSONSerializerFromText(
    TypeInfo(TTestCustomJSONArraySimple),__TTestCustomJSONArraySimple);
  {$ifndef NOVARIANTS}
  TTextWriter.RegisterCustomJSONSerializerFromText(
    TypeInfo(TTestCustomJSONArrayVariant),__TTestCustomJSONArrayVariant);
  {$endif}
  TTextWriter.RegisterCustomJSONSerializerFromText(
    TypeInfo(TSQLRestCacheEntryValue),__TSQLRestCacheEntryValue);
  TestJSONSerialization;
  TestJSONSerialization; // test twice for safety
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TSQLRestCacheEntryValue),'');
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TSubAB),'');
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TSubCD),'');
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TAggregate),'');
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TTestCustomJSONRecord),'');
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TTestCustomJSONArray),'');
  {$ifndef NOVARIANTS}
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TTestCustomJSONArrayVariant),'');
  {$endif}
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TTestCustomJSONArraySimple),'');

  {$ifdef ISDELPHI2010}
  // test JSON serialization defined by Enhanced RTTI
  TestJSONSerialization;
  {$endif}

  // tests parsing options
  Parser := TTextWriter.RegisterCustomJSONSerializerFromText(
    TypeInfo(TTestCustomJSONRecord),
    copy(__TTestCustomJSONRecord,1,PosEx('}',__TTestCustomJSONRecord))) as TJSONRecordTextDefinition;
  U := RecordSaveJSON(JR2,TypeInfo(TTestCustomJSONRecord));
  Check(IsValidJSON(U));
  CheckEqual(U,'{"A":0,"B":0,"C":0,"D":"","E":{"E1":0,"E2":0}}');
  U := RecordSaveJSON(JR,TypeInfo(TTestCustomJSONRecord));
  Check(IsValidJSON(U));
  CheckEqual(U,'{"A":10,"B":0,"C":0,"D":"**","E":{"E1":0,"E2":0}}');
  U := '{"B":0,"C":0,"A":10,"D":"**","E":{"E1":0,"E2":20}}';
  RecordLoadJSON(JR2,UniqueRawUTF8(U),TypeInfo(TTestCustomJSONRecord));
  Check(JR2.A=10);
  Check(JR2.D='**');
  Check(JR2.E.E2=20);
  Parser.Options := [soReadIgnoreUnknownFields];
  U := '{ "A" : 1 , "B" : 2 , "C" : 3 , "D" : "A" , "tobeignored":null,"E": '#13#10+
    '{ "E1" : 4, "E2" : 5 } , "tbi" : { "b" : 0 } }';
  RecordLoadJSON(JR2,UniqueRawUTF8(U),TypeInfo(TTestCustomJSONRecord));
  Check(JR2.A=1);
  Check(JR2.D='A');
  Check(JR2.E.E1=4);
  Check(JR2.E.E2=5);
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TTestCustomJSONRecord),'');

  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TTestCustomJSONArrayWithoutF),
    copy(__TTestCustomJSONArray,1,PosEx(']',__TTestCustomJSONArray)));
  U := RecordSaveJSON(JA2,TypeInfo(TTestCustomJSONArrayWithoutF));
  Check(IsValidJSON(U));
  CheckEqual(U,'{"A":100,"B":0,"C":0,"D":null,"E":[{"E1":1,"E2":"2"},{"E1":3,"E2":"4"}]}');
  Finalize(JA);
  FillCharFast(JA,sizeof(JA),0);
  RecordLoadJSON(JA,pointer(U),TypeInfo(TTestCustomJSONArrayWithoutF));
  Check(JA.A=100);
  Check(JA.D='');
  U := RecordSaveJSON(JA,TypeInfo(TTestCustomJSONArrayWithoutF));
  Check(IsValidJSON(U));
  Check(length(JA.E)=2);
  CheckEqual(U,'{"A":100,"B":0,"C":0,"D":null,"E":[{"E1":1,"E2":"2"},{"E1":3,"E2":"4"}]}');
  JA.D := '1234';
  U := RecordSaveJSON(JA,TypeInfo(TTestCustomJSONArrayWithoutF));
  Check(IsValidJSON(U));
  Check(length(JA.E)=2);
  Finalize(JA);
  FillCharFast(JA,sizeof(JA),0);
  RecordLoadJSON(JA,pointer(U),TypeInfo(TTestCustomJSONArrayWithoutF));
  Check(length(JA.E)=2);
  Check(JA.D='1234');
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TTestCustomJSONArrayWithoutF),'');

  discogsJson := StringFromFile(discogsFileName);
  if discogsJson='' then begin
    discogsJson := HttpGet('https://api.discogs.com/artists/45/releases?page=1&per_page=100');
    FileFromString(discogsJson,discogsFileName);
  end;
  Check(IsValidJSON(discogsJson));
  zendframeworkJson := StringFromFile(zendframeworkFileName);
  if zendframeworkJson='' then begin
    zendframeworkJson := HttpGet('https://api.github.com/users/zendframework/repos');
    FileFromString(zendframeworkJson,zendframeworkFileName);
  end;
  Check(IsValidJSON(zendframeworkJson));
  TestGit([soReadIgnoreUnknownFields]);
  TestGit([soReadIgnoreUnknownFields,soWriteHumanReadable]);
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TTestCustomJSON2Title),
    __TTestCustomJSON2Title).Options := [soWriteHumanReadable];
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TTestCustomJSON2),
    __TTestCustomJSON2).Options := [soWriteHumanReadable];
  FillCharFast(Trans,sizeof(Trans),0);
  U := RecordSaveJSON(Trans,TypeInfo(TTestCustomJSON2));
  Check(IsValidJSON(U));
  CheckEqual(U,'{'#$D#$A#9'"Transactions": []'#$D#$A'}');
  for i := 1 to 10 do begin
    U := '{"transactions":[{"TRTYPE":"INCOME","TRDATE":"2013-12-09 02:30:04","TRAA":"1.23",'+
     '"TRCAT1":{"TITYPE":"C1","TIID":"1","TICID":"","TIDSC30":"description1","TIORDER":"0","TIDEL":"false"},'+
     '"TRCAT2":{"TITYPE":"C2","TIID":"2","TICID":"","TIDSC30":"description2","TIORDER":"0","TIDEL":"false"},'+
     '"TRCAT3":{"TITYPE":"C3","TIID":"3","TICID":"","TIDSC30":"description3","TIORDER":"0","TIDEL":"false"},'+
     '"TRRMK":"Remark",'+
     '"TRACID":{"TITYPE":"AC","TIID":"4","TICID":"","TIDSC30":"account1","TIORDER":"0","TIDEL":"false"}}]}';
    Check(IsValidJSON(U));
    RecordLoadJSON(Trans,UniqueRawUTF8(U),TypeInfo(TTestCustomJSON2));
    Check(length(Trans.Transactions)=1);
    Check(Trans.Transactions[0].TRTYPE='INCOME');
    Check(Trans.Transactions[0].TRACID.TIDEL='false');
    Check(Trans.Transactions[0].TRRMK='Remark');
    U := RecordSaveJSON(Trans,TypeInfo(TTestCustomJSON2));
    Check(Hash32(U)=$CC7167FC);
  end;
  FileFromString(U,'transactions.json');
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TTestCustomJSON2Title),'');
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TTestCustomJSON2),'');

  Parser := TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TTestCustomDiscogs),
    __TTestCustomDiscogs) as TJSONRecordTextDefinition;
  Parser.Options := [soReadIgnoreUnknownFields];
  FillCharFast(Disco,sizeof(Disco),0);
  Check(PtrUInt(@Disco.releases)-PtrUInt(@Disco)=3*sizeof(integer));
  Check(sizeof(Disco.releases[0])=5*sizeof(Pointer)+2*sizeof(integer));
  Check(sizeof(Disco)=sizeof(Pointer)+3*sizeof(integer));
  U := RecordSaveJSON(Disco,TypeInfo(TTestCustomDiscogs));
  CheckEqual(U,'{"pagination":{"per_page":0,"items":0,"page":0},"releases":[]}');
  U := JSONReformat(discogsJson,jsonCompact);
  Check(IsValidJSON(U));
  Check(JSONReformat(JSONReformat(discogsJson,jsonHumanReadable),jsonCompact)=U);
  Check(JSONReformat(JSONReformat(discogsJson,jsonUnquotedPropName),jsonCompact)=U);
  Check(JSONReformat(JSONReformat(U,jsonUnquotedPropName),jsonCompact)=U);
  RecordLoadJSON(Disco,pointer(discogsJson),TypeInfo(TTestCustomDiscogs));
  Check(length(Disco.releases)<=Disco.pagination.items);
  for i := 0 to high(Disco.Releases) do
    Check(Disco.Releases[i].id>0);
  Parser.Options := [soWriteHumanReadable,soReadIgnoreUnknownFields];
  U := RecordSaveJSON(Disco,TypeInfo(TTestCustomDiscogs));
  Check(IsValidJSON(U));
  FileFromString(U,'discoExtract.json');
  Finalize(Disco);
  FillCharFast(Disco,sizeof(Disco),0);
  U := '{"pagination":{"per_page":1},"releases":[{"title":"TEST","id":10}]}';
  Check(IsValidJSON(U));
  RecordLoadJSON(Disco,UniqueRawUTF8(U),TypeInfo(TTestCustomDiscogs));
  Check(Disco.pagination.per_page=1);
  Check(Disco.pagination.page=0);
  if not CheckFailed(length(Disco.releases)=1) then begin
    Check(Disco.releases[0].title='TEST');
    Check(Disco.releases[0].id=10);
  end;
  Finalize(Disco);
  FillCharFast(Disco,sizeof(Disco),0);
  U := '{"pagination":{},"releases":[{"Id":10},{"TITle":"blabla"}]}';
  Check(IsValidJSON(U));
  RecordLoadJSON(Disco,UniqueRawUTF8(U),TypeInfo(TTestCustomDiscogs));
  Check(Disco.pagination.per_page=0);
  Check(Disco.pagination.page=0);
  if not CheckFailed(length(Disco.releases)=2) then begin
    Check(Disco.releases[0].title='');
    Check(Disco.releases[0].id=10);
    Check(Disco.releases[1].title='blabla');
    Check(Disco.releases[1].id=0);
  end;
  U := '{"pagination":{"page":1},"releases":[{"title":"abc","id":2}]}';
  Check(IsValidJSON(U));
  RecordLoadJSON(Disco,UniqueRawUTF8(U),TypeInfo(TTestCustomDiscogs));
  Check(Disco.pagination.per_page=0);
  Check(Disco.pagination.page=1);
  if not CheckFailed(length(Disco.releases)=1) then begin
    Check(Disco.releases[0].title='abc');
    Check(Disco.releases[0].id=2);
  end;
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TTestCustomDiscogs),'');
  SetString(U,PAnsiChar('true'#0'footer,'),12);
  Check(IdemPChar(GetJSONField(pointer(U),P),'TRUE'));
  Check(P=nil);
  CheckEqual(U,'true'#0'footer,','3cce80e8df');
  {$ifndef DELPHI5OROLDER}
  // validates RawJSON (custom) serialization
  Enemy := TEnemy.Create;
  try
    U := ObjectToJSON(Enemy);
    Check(IsValidJSON(U));
    CheckEqual(U,'{"Enabled":false,"Name":"","Offense":{"damage":{"min":0,"max":0},'+
      '"attackspeed":{"min":0,"max":0}}}');
    Enemy.Off.Damage.Min := 10;
    Enemy.Off.AttackSpeed.Max := 100;
    U := ObjectToJSON(Enemy);
    Check(IsValidJSON(U));
    CheckEqual(U,'{"Enabled":false,"Name":"","Offense":{"damage":{"min":10,"max":0},'+
      '"attackspeed":{"min":0,"max":100}}}');
    FillcharFast(Enemy.Off, sizeof(Enemy.Off), 0);
    check(Enemy.Off.Damage.Min = 0);
    check(Enemy.Off.AttackSpeed.Max = 0);
    JSONToObject(Enemy, pointer(U), valid);
    check(valid);
    check(Enemy.Off.Damage.Min = 10);
    check(Enemy.Off.AttackSpeed.Max = 100);
  finally
    Enemy.Free;
  end;
  {$endif}
end;

procedure TTestLowLevelTypes.WikiMarkdownToHtml;
begin
  // wiki
  CheckEqual(HtmlEscapeWiki('test'),'<p>test</p>');
  CheckEqual(HtmlEscapeWiki('te<b>st'),'<p>te&lt;b&gt;st</p>');
  CheckEqual(HtmlEscapeWiki('t *e* st'),'<p>t <em>e</em> st</p>');
  CheckEqual(HtmlEscapeWiki('t*e*st'),'<p>t<em>e</em>st</p>');
  CheckEqual(HtmlEscapeWiki('t\*e\*st'),'<p>t*e*st</p>');
  CheckEqual(HtmlEscapeWiki('t\*e*st'),'<p>t*e<em>st</em></p>');
  CheckEqual(HtmlEscapeWiki('t +e+ st'),'<p>t <strong>e</strong> st</p>');
  CheckEqual(HtmlEscapeWiki('t+e+st'),'<p>t<strong>e</strong>st</p>');
  CheckEqual(HtmlEscapeWiki('t `e` st'),'<p>t <code>e</code> st</p>');
  CheckEqual(HtmlEscapeWiki('t`e`st'),'<p>t<code>e</code>st</p>');
  CheckEqual(HtmlEscapeWiki('https://test'),'<p><a href="https://test" rel="nofollow">https://test</a></p>');
  CheckEqual(HtmlEscapeWiki('test'#13#10'click on http://coucouc.net toto'),
    '<p>test</p><p>click on <a href="http://coucouc.net" rel="nofollow">http://coucouc.net</a> toto</p>');
  CheckEqual(HtmlEscapeWiki(':test: :) joy:'),'<p>:test: '+EMOJI_UTF8[eSmiley]+' joy:</p>');
  CheckEqual(HtmlEscapeWiki(':innocent: smile'),'<p>'+EMOJI_UTF8[eInnocent]+' smile</p>');
  CheckEqual(HtmlEscapeWiki(':test: :) a:joy:'),'<p>:test: '+EMOJI_UTF8[eSmiley]+' a:joy:</p>');
  CheckEqual(HtmlEscapeWiki(':test: :)'),'<p>:test: '+EMOJI_UTF8[eSmiley]+'</p>');
  CheckEqual(HtmlEscapeWiki(':test: (:)'),'<p>:test: (:)</p>');
  CheckEqual(HtmlEscapeWiki(':test: :))'),'<p>:test: :))</p>');
  // Markdown
  CheckEqual(HtmlEscapeMarkdown('test'),'<p>test</p>');
  CheckEqual(HtmlEscapeMarkdown('test'#13#10'toto'),'<p>test toto</p>');
  CheckEqual(HtmlEscapeMarkdown('test'#13#10#13#10'toto'),'<p>test</p><p>toto</p>');
  CheckEqual(HtmlEscapeMarkdown('test'#10#10'toto'),'<p>test</p><p>toto</p>');
  CheckEqual(HtmlEscapeMarkdown('test'#10#10#10'toto'),'<p>test</p><p> toto</p>');
  CheckEqual(HtmlEscapeMarkdown('te<b>st'),'<p>te<b>st</p>');
  CheckEqual(HtmlEscapeMarkdown('te<b>st',[heHtmlEscape]),'<p>te&lt;b&gt;st</p>');
  CheckEqual(HtmlEscapeMarkdown('t *e* st'),'<p>t <em>e</em> st</p>');
  CheckEqual(HtmlEscapeMarkdown('t*e*st'),'<p>t<em>e</em>st</p>');
  CheckEqual(HtmlEscapeMarkdown('t\*e\*st'),'<p>t*e*st</p>');
  CheckEqual(HtmlEscapeMarkdown('t\*e*st'),'<p>t*e<em>st</em></p>');
  CheckEqual(HtmlEscapeMarkdown('t **e** st'),'<p>t <strong>e</strong> st</p>');
  CheckEqual(HtmlEscapeMarkdown('t**e**st'),'<p>t<strong>e</strong>st</p>');
  CheckEqual(HtmlEscapeMarkdown('t _e_ st'),'<p>t <em>e</em> st</p>');
  CheckEqual(HtmlEscapeMarkdown('t_e_st'),'<p>t<em>e</em>st</p>');
  CheckEqual(HtmlEscapeMarkdown('t\_e\_st'),'<p>t_e_st</p>');
  CheckEqual(HtmlEscapeMarkdown('t\_e_st'),'<p>t_e<em>st</em></p>');
  CheckEqual(HtmlEscapeMarkdown('t __e__ st'),'<p>t <strong>e</strong> st</p>');
  CheckEqual(HtmlEscapeMarkdown('t__e__st'),'<p>t<strong>e</strong>st</p>');
  CheckEqual(HtmlEscapeMarkdown('t `e` st'),'<p>t <code>e</code> st</p>');
  CheckEqual(HtmlEscapeMarkdown('t`e`st'),'<p>t<code>e</code>st</p>');
  CheckEqual(HtmlEscapeMarkdown('t***e***st'),'<p>t<strong><em>e</strong></em>st</p>');
  CheckEqual(HtmlEscapeMarkdown('test'#13#10'click on http://coucouc.net toto'),
    '<p>test click on <a href="http://coucouc.net" rel="nofollow">http://coucouc.net</a> toto</p>');
  CheckEqual(HtmlEscapeMarkdown('[toto](http://coucou.net) titi'),
    '<p><a href="http://coucou.net" rel="nofollow">toto</a> titi</p>');
  CheckEqual(HtmlEscapeMarkdown('blabla ![img](static/img.jpg) blibli'),
    '<p>blabla <img alt="img" src="static/img.jpg"> blibli</p>');
  CheckEqual(HtmlEscapeMarkdown('test'#13#10'    a*=10*2'#10'    b=20'#13#10'ended'),
    '<p>test</p><pre><code>a*=10*2'#$D#$A'b=20'#$D#$A'</code></pre><p>ended</p>');
  CheckEqual(HtmlEscapeMarkdown('test'#13#10'``` a*=10*2'#10'  b=20'#13#10'```ended'),
    '<p>test</p><pre><code> a*=10*2'#$D#$A'  b=20'#$D#$A'</code></pre><p>ended</p>');
  CheckEqual(HtmlEscapeMarkdown('*te*st'#13#10'* one'#13#10'* two'#13#10'end'),
    '<p><em>te</em>st</p><ul><li>one</li><li>two</li></ul><p>end</p>');
  CheckEqual(HtmlEscapeMarkdown('+test'#13#10'+ one'#13#10'- two'#13#10'end'),
    '<p>+test</p><ul><li>one</li><li>two</li></ul><p>end</p>');
  CheckEqual(HtmlEscapeMarkdown('1test'#13#10'1. one'#13#10'2. two'#13#10'end'),
    '<p>1test</p><ol><li>one</li><li>two</li></ol><p>end</p>');
  CheckEqual(HtmlEscapeMarkdown('1test'#13#10'1. one'#13#10'7. two'#13#10'3. three'#13#10'4end'),
    '<p>1test</p><ol><li>one</li><li>two</li><li>three</li></ol><p>4end</p>');
  CheckEqual(HtmlEscapeMarkdown('1test'#13#10'1. one'#13#10'2. two'#13#10'+ one'#13#10'- two'#13#10'end'),
    '<p>1test</p><ol><li>one</li><li>two</li></ol><ul><li>one</li><li>two</li></ul><p>end</p>');
  CheckEqual(HtmlEscapeMarkdown('>test'#13#10'> quote'),
    '<p>>test</p><blockquote><p>quote</p></blockquote>');
  CheckEqual(HtmlEscapeMarkdown('>test'#13#10'> quote1'#10'> quote2'#13#10'end'),
    '<p>>test</p><blockquote><p>quote1</p><p>quote2</p></blockquote><p>end</p>');
  CheckEqual(HtmlEscapeMarkdown(':test: :) joy:'),'<p>:test: '+EMOJI_UTF8[eSmiley]+' joy:</p>');
  CheckEqual(HtmlEscapeMarkdown(':innocent: :joy'),'<p>'+EMOJI_UTF8[eInnocent]+' :joy</p>');
  CheckEqual(HtmlEscapeMarkdown(':test: :)'),'<p>:test: '+EMOJI_UTF8[eSmiley]+'</p>');
  CheckEqual(HtmlEscapeMarkdown(':test: (:)'),'<p>:test: (:)</p>');
end;

{$ifndef DELPHI5OROLDER}
{$ifndef LVCL}

procedure TTestLowLevelTypes._TDecimal128;

  procedure Test(const hi,lo: QWord; const expected: RawUTF8;
    special: TDecimal128SpecialValue=dsvValue);
  var v,v2: TDecimal128;
  begin
    v.Bits.hi := hi;
    v.Bits.lo := lo;
    Check(v.ToText=expected);
    v2.SetZero;
    Check(v2.FromText(expected)=special);
    if special<>dsvValue then
      exit;
    Check(v2.Equals(v));
    Check(v2.ToText=expected);
    v2.SetZero;
    if expected[1]<>'-' then
      Check(v2.FromText('000'+LowerCase(expected))=dsvValue) else
      Check(v2.FromText(LowerCase(expected))=dsvValue);
    Check(v2.Equals(v));
  end;
  procedure Test2(const fromvalue, expected: RaWUTF8; h: QWord=0; l: QWord=0);
  var v: TDecimal128;
  begin
    Check(v.FromText(fromvalue)=dsvValue);
    Check(v.ToText=expected);
    if (h=0) and (l=0) then
      exit;
    Check(v.Bits.lo=l);
    Check(v.Bits.hi=h);
  end;

var v,v2: TDecimal128;
    s: TDecimal128SpecialValue;
    str: RawUTF8;
    i: integer;
    o: variant;
begin // see https://github.com/mongodb/libbson/blob/master/tests/test-decimal128.c
  Check(v.FromText('')=dsvError);
  Check(v.FromText('.')=dsvError);
  Check(v.FromText('.e')=dsvError);
  Check(v.FromText('i')=dsvError);
  Check(v.FromText('invalid')=dsvError);
  Check(v.FromText('1invalid')=dsvError);
  Check(v.FromText('E02')=dsvError);
  Check(v.FromText('E+02')=dsvError);
  Check(v.FromText('e+02')=dsvError);
  Check(v.FromText('1E02')=dsvValue);
  Check(v.FromText('1invalidE02')=dsvError);
  Check(v.FromText('..1')=dsvError);
  Check(v.FromText('0')=dsvZero);
  Check(v.ToText='0');
  for s := dsvNan to high(s) do begin
    v.SetSpecial(s);
    Check(v.ToText=DECIMAL128_SPECIAL_TEXT[s]);
    Check(v.IsSpecial=s);
    if s<dsvMin then begin
      v.SetZero;
      Check(v.FromText(LowerCase(DECIMAL128_SPECIAL_TEXT[s]))=s);
      Check(v.IsSpecial=s);
    end;
  end;
  v.SetZero;
  Check(v.ToText='0');
  Test(0,0,'0',dsvZero);
  Test($3040000000000000,0,'0',dsvZero);
  Test($3040000000000000,1,'1');
  Test($3040000000000000,2,'2');
  Test($b040000000000000,2,'-2');
  Test($b040000000000000,1,'-1');
  Test($b040000000000000,0,'-0');
  Test($303e000000000000,1,'0.1');
  Test($3034000000000000,$4d2,'0.001234');
  Test($3040000000000000,$1cbe991a14,'123456789012');
  Test($302a000000000000,$75aef40,'0.00123400000');
  Test($2ffc3cde6fff9732,$de825cd07e96aff2,'0.1234567890123456789012345678901234');
  Test($3040ffffffffffff,$ffffffffffffffff,'5192296858534827628530496329220095');
  Test($5ffe314dc6448d93,$38c15b0a00000000,'1.000000000000000000000000000000000E+6144');
  Test($000,$001,'1E-6176');
  Test($8000000000000000,$001,'-1E-6176');
  Test($3108000000000000,$000009184db63eb1,'9.999987654321E+112');
  Test($5fffed09bead87c0,$378d8e63ffffffff,DECIMAL128_SPECIAL_TEXT[dsvMax]);
  Test($0001ed09bead87c0,$378d8e63ffffffff,'9.999999999999999999999999999999999E-6143');
  Test($dfffed09bead87c0,$378d8e63ffffffff,DECIMAL128_SPECIAL_TEXT[dsvMin]);
  Test($304c000000000000,$41a,'1.050E+9');
  Test($3042000000000000,$41a,'1.050E+4');
  Test($3040000000000000,$069,'105');
  Test($3042000000000000,$069,'1.05E+3');
  Test($3046000000000000,$001,'1E+3');
  Test($3298000000000000,$000,'0E+300');
  Test($2b90000000000000,$000,'0E-600');
  Test2('10e0','10');
  Test2('1e1','1E+1');
  Test2('10e-1','1.0');
  Test2('1000000000000000000000000000000000000000',
    '1.000000000000000000000000000000000E+39',$304c314dc6448d93,$38c15b0a00000000);
  Test2('10000000000000000000000000000000000','1.000000000000000000000000000000000E+34',
    $3042314dc6448d93,$38c15b0a00000000);
  Test2('1000000000000000000000000000000000','1000000000000000000000000000000000',
    $3040314dc6448d93,$38c15b0a00000000);
  Test2('12345678901234567e6111','1.2345678901234567E+6127',
    $5ffe000000000000,12345678901234567);
  Test2('-100E-10','-1.00E-8',$b02c000000000000,100);
  v.SetZero;
  for i := 0 to 4000 do begin
    if i>1000 then
      inc(v.Bits.c[0],i*7) else
      v.Bits.c[0] := i;
    str := v.ToText;
    Check(str=UInt32ToUTF8(v.Bits.c[0]));
    if i=0 then
      continue;
    Check(v2.FromText(str)=dsvValue);
    Check(v2.Equals(v));
  end;
  for i := -1000 to 100 do begin
    v.FromInt32(i);
    str := v.ToText;
    Check(str=Int32ToUTF8(i));
    if i=0 then
      continue;
    Check(v2.FromText(str)=dsvValue);
    Check(v2.Equals(v));
  end;
  v.FromCurr(0);
  Check(v.ToText='0.0000');
  Check(v.ToCurr=0);
  v.FromCurr(3.14);
  Check(v.ToText='3.1400');
  for i := -160 to 160 do begin
    v.FromFloat(i/4);
    v.ToText(str);
    Check(GetExtended(pointer(str))*4=i);
    Check(v.ToFloat*4=i);
    v.FromCurr(i/16);
    v.ToText(str);
    Check(StrToCurr64(pointer(str))=i*625);
    Check(v.ToCurr*16=i);
    o := NumberDecimal(i/8);
    Check(v.FromVariant(o));
    Check(v.ToCurr*8=i);
  end;
end;

procedure TTestLowLevelTypes._BSON;
const BSONAWESOME = '{"BSON":["awesome",5.05,1986]}';
      BSONAWESOMEBIN = #$31#0#0#0#4'BSON'#0#$26#0#0#0#2'0'#0#8#0#0#0'awesome'#0+
        #1'1'#0'333333'#$14#$40#$10'2'#0#$c2#7#0#0#0#0;
      BSONID = '507F191E810C19729DE860EA';
      REGEX = '{"$regex":"acme.*corp","$options":"i"}';
      REGEX2 = '{name:"John",field:/acme.*corp/i}';
procedure CheckRegEx(o: variant);
var u,u2: RawUTF8;
begin
  u := VariantSaveMongoJSON(o,modMongoStrict);
  CheckEqual(u,'{"name":"John","field":'+REGEX+'}');
  u2 := VariantSaveMongoJSON(o,modMongoStrict);
  CheckEqual(u,u2,'call twice');
  u2 := VariantSaveJSON(o);
  CheckEqual(u,u2);
  u := VariantSaveMongoJSON(o,modMongoShell);
  CheckEqual(u,REGEX2);
end;
var o,od,o2,value: variant;
    d,d2: TDateTime;
    oid, oid2: TBSONObjectID;
    oids: array of TBSONObjectID;
    bsonDat, temp, bin: RawByteString;
    i,j: integer;
    b: PByte;
    elem, item: TBSONElement;
    iter: TBSONIterator;
    name,u,u2,u3,json: RawUTF8;
    arr: TRawUTF8DynArray;
    st: string;
    timer: TPrecisionTimer;
    dec: TDecimal128;
procedure CheckElemIsBsonArray;
var b: PByte;
begin
  Check(elem.Kind=betArray);
  Check(elem.Name='BSON');
  item.Index := -1;
  b := elem.Element;
  BSONParseLength(b,38);
  Check(b=elem.Data.DocList);
  while item.FromNext(b) do begin
    case item.Index of
    0: Check(item.ToVariant='awesome');
    1: CheckSame(item.ToVariant,5.05);
    2: Check(item.ToVariant=1986);
    else Check(false);
    end;
  end;
end;
begin
  // see http://docs.mongodb.org/manual/reference/object-id
  oid.FromText('507f191e810c19729de860ea');
  Check(oid.UnixCreateTime=bswap32($507f191e));
  u := oid.ToText;
  Check(u=BSONID);
  o := ObjectID('507f191e810c19729de860ea');
  Check(TVarData(o).VType=BSONVariantType.VarType);
  u := string(o);
  Check(u=BSONID);
  d2 := Iso8601ToDateTime('2012-10-17T20:46:22');
  od := d2;
  Check(TVarData(od).VType=varDate);
  {$ifdef FPC} // doesn't allow direct cast from varDate to double :(
  CheckSame(TVarData(od).VDate,d2);
  d := double(o);
  {$else}
  CheckSame(od,d2);
  d := o;
  {$endif}
  DateTimeToIso8601StringVar(d,'T',st);
  CheckSame(d,d2,1E-4,st);
  CheckSame(o,d2,1E-4,st);
  CheckSame(TBSONVariantData(o).VObjectID.CreateDateTime,d2,1E-4);
  o2 := o;
  Check(double(o)=double(o2));
  o := ObjectID;
  Check(Abs(NowUTC-double(o))<0.1);
  oid.FromText(string(o));
  Check(Abs(NowUTC-oid.CreateDateTime)<0.1);
  oid2.ComputeNew;
  Check(oid.MachineID.b1=oid2.MachineID.b1);
  Check(oid.MachineID.b2=oid2.MachineID.b2);
  Check(oid.MachineID.b3=oid2.MachineID.b3);
  Check(oid.ProcessID=oid2.ProcessID);
  o2 := ObjectID;
  {$ifdef FPC} // FPC bug: sysvartotdatetime doesn't handle custom variants :(
  Check(double(o2)>=double(o),o);
  {$else}
  Check(TDateTime(o2)>=TDateTime(o),o);
  {$endif}
  oid2.ComputeNew;
  j := 100000;
  timer.Start;
  for i := 1 to j do begin
    oid.ComputeNew;
    Check(not oid.Equal(oid2));
    oid2 := oid;
    Check(oid.Equal(oid2));
  end;
  NotifyTestSpeed('TBSONObjectID.ComputeNew',j,0,@timer);
  SetLength(oids,300);
  for i := 0 to high(oids) do begin
    oids[i].ComputeNew;
    for j := 0 to i-1 do
      Check(not oids[i].Equal(oids[j]),'24 bit collision');
  end;
  //Check(GetCurrentProcessId<>oid.ProcessID,'Expected overflow');
  o := _JSON('{"double_params":[-12.12345678,-9.9E-15,-9.88E-15,-9E-15]}',
     [dvoReturnNullForUnknownProperty, dvoAllowDoubleValue]);
  json := TDocVariantData(o).ToJSON;
  {$ifndef EXTENDEDTOSHORT_USESTR}
  check(json='{"double_params":[-12.12345678,-9.9E-15,-9.88E-15,-9E-15]}');
  {$endif}
  CheckSame(double(TDocVariantData(o).A['double_params'].Value[1]),-9.9E-15);
  // floats are stored as varCurrency by default in _Json()
  o := _Json('{"value":99.99}');
  d :=  _Safe(o)^.D['value'];
  CheckSame(d,99.99,DOUBLE_SAME,'99.99');
  CheckEqual(DoubleToStr(d),'99.99');
  // see http://bsonspec.org/#/specification
  o := _JSON('{"hello": "world"}');
  bsonDat := BSON(TDocVariantData(o));
  Check(bsonDat=#$16#0#0#0#2'hello'#0#6#0#0#0'world'#0#0);
  b := pointer(bsonDat);
  Check(BSONParseLength(b,$16)=length(bsonDat));
  Check(elem.FromNext(b));
  Check(elem.Kind=betString);
  Check(elem.Name='hello');
  Check(elem.Data.Text='world');
  Check(not elem.FromNext(b));
  Check(elem.Kind=betEof);
  u := BSONToJSON(pointer(bsonDat),betDoc,length(bsonDat));
  CheckEqual(u,'{"hello":"world"}');
  elem.FromDocument(bsonDat);
  Check(elem.Kind=betDoc);
  Check(elem.DocItemToVariant('hello',value));
  check(value='world');
  Check(not elem.DocItemToVariant('hello2',value));
  Check(elem.DocItemToRawUTF8('hello')='world');
  Check(elem.DocItemToRawUTF8('hello2')='');
  Check(elem.DocItemToInteger('hello',1234)=1234);
  Check(iter.Init(bsonDat));
  Check(iter.Next);
  Check(iter.Item.Kind=betString);
  Check(iter.Item.Name='hello');
  Check(iter.Item.Data.Text='world');
  Check(not iter.Next);
  b := pointer(bsonDat);
  BSONParseLength(b);
  Check(BSONParseNextElement(b,name,value));
  Check(name='hello');
  Check(value='world');
  Check(not BSONParseNextElement(b,name,value));
  o := _JSON('{"BSON": ["awesome", 5.05, 1986]}');
  bsonDat := BSON(TDocVariantData(o));
  Check(length(bsonDat)=$31);
  Check(bsonDat=BSONAWESOMEBIN);
  b := pointer(bsonDat);
  Check(BSONParseLength(b,$31)=length(bsonDat));
  Check(elem.FromNext(b));
  CheckElemIsBsonArray;
  Check(not elem.FromNext(b));
  Check(elem.Kind=betEof);
  u := BSONToJSON(pointer(bsonDat),betDoc,length(bsonDat));
  CheckEqual(u,BSONAWESOME);
  u := VariantSaveMongoJSON(o,modMongoStrict);
  CheckEqual(u,BSONAWESOME);
  u := VariantSaveJSON(o);
  CheckEqual(u,BSONAWESOME);
  Check(BSON(['BSON',_Arr(['awesome',5.05, 1986])])=bsonDat);
  o2 := BSONVariantType[bsonDat];
  Check(VariantSaveJSON(o2)=u);
  o2 := BSONVariant('{"BSON": ["awesome", 5.05, 1986]}');
  u := VariantSaveMongoJSON(o2,modMongoStrict);
  CheckEqual(u,BSONAWESOME);
  o2 := BSONVariant(['BSON',_Arr(['awesome',5.05, 1986])]);
  Check(VariantSaveMongoJSON(o2,modMongoStrict)=BSONAWESOME);
  o2 := BSONVariant(TDocVariantData(o));
  Check(VariantSaveMongoJSON(o2,modMongoStrict)=BSONAWESOME);
  o2 := BSONVariant('{%:[?,?,?]}',['BSON'],['awesome',5.05,1986]);
  Check(VariantSaveMongoJSON(o2,modMongoStrict)=BSONAWESOME);
  b := pointer(bsonDat);
  {$ifndef FPC}
  Check(o2=BSONAWESOME,'BSONVariant casted to string');
  {$endif}
  u := string(o2);
  CheckEqual(u,'{BSON:["awesome",5.05,1986]}','TBSONVariant: mongoShell syntax');
  BSONParseLength(b);
  Check(BSONParseNextElement(b,name,value,asDocVariantPerReference));
  Check(name='BSON');
  elem.FromVariant(name,value,Temp);
  CheckElemIsBsonArray;
  Check(not BSONParseNextElement(b,name,value));
  o := BSONDocumentToDoc(bsonDat);
  Check(TVarData(o).VType=DocVariantType.VarType);
  Check(DocVariantType.IsOfType(o));
  Check(o.Name(0)='BSON');
  Check(o._(0)._Kind=ord(dvArray));
  Check(o.bson._Kind=ord(dvArray));
  Check(o.bson._count=3);
  Check(o.bson._(0)='awesome');
  CheckSame(double(o.bson._(1)),5.05);
  Check(o.bson._(2)=1986);
  Check(o.dummy=null);
  Check(o.Exists('bson'));
  Check(not o.Exists('dummy'));
  Check(o.NameIndex('bson')=0);
  Check(o.NameIndex('dummy')<0);
  DocVariantData(o.bson).ToRawUTF8DynArray(arr);
  Check(length(arr)=3);
  Check(RawUTF8ArrayToCSV(arr)='awesome,5.05,1986');
  Check(DocVariantData(o.bson).ToJSON='["awesome",5.05,1986]');
  u := '{"BSON":["awesome",5.05,1986],"name":"John","one":1.2}';
  _JSON(u,o);
  Check(VariantSaveJson(BSONVariant(u))=u);
  bsonDat := BSON(TDocVariantData(o));
  b := pointer(bsonDat);
  BSONParseLength(b);
  Check(BSONParseNextElement(b,name,value));
  Check(name='BSON');
  elem.FromVariant(name,value,Temp);
  CheckElemIsBsonArray;
  Check(BSONParseNextElement(b,name,value));
  Check(name='name');
  Check(value='John');
  elem.FromVariant(name,value,Temp);
  Check(elem.name='name');
  Check(elem.Data.Text='John');
  Check(BSONParseNextElement(b,name,value));
  Check(name='one');
  CheckSame(value,1.2);
  elem.FromVariant(name,value,Temp);
  Check(elem.name='one');
  CheckSame(unaligned(PDouble(elem.Element)^),1.2);
  Check(not BSONParseNextElement(b,name,value));
  Check(BSONToJSON(pointer(bsonDat),betDoc,length(bsonDat))=u);
  elem.FromVariant('test',o,Temp);
  Check(elem.Name='test');
  Check(elem.Kind=betDoc);
  Check(VariantSaveMongoJSON(o,modMongoStrict)=u);
  Check(VariantSaveMongoJSON('test',modMongoStrict)='"test"');
  Check(VariantSaveMongoJSON(1.5,modMongoStrict)='1.5');
  Check(VariantSaveMongoJSON(_JSON('{BSON:["awesome",5.05,1986]}'),modMongoStrict)=BSONAWESOME);
  Check(VariantSaveMongoJSON(_JSONFast('{ BSON : ["awesome", 5.05, 1986] }'),modMongoStrict)=BSONAWESOME);
  Check(VariantSaveMongoJSON(_JSONFast('{ ''BSON'' : ["awesome", 5.05, 1986] } '),modMongoStrict)=BSONAWESOME);
  Check(VariantSaveJSON(o)=u);
  Check(VariantSaveJSON('test')='"test"');
  Check(VariantSaveJSON(1.5)='1.5');
  Check(VariantSaveJSON(_JSON('{BSON:["awesome",5.05,1986]}'))=BSONAWESOME);
  Check(VariantSaveJSON(_JSONFast('{ BSON : ["awesome", 5.05, 1986] }'))=BSONAWESOME);
  Check(VariantSaveJSON(_JSONFast('{ ''BSON'' : ["awesome", 5.05, 1986] } '))=BSONAWESOME);
  Check(BSON('{BSON:["awesome",5.05,1986]}',[],[])=BSONAWESOMEBIN);
  Check(BSON('{ BSON : ["awesome", 5.05, 1986] }',[],[])=BSONAWESOMEBIN);
  Check(BSON('{ ''BSON'' : ["awesome", 5.05, 1986] } ',[],[])=BSONAWESOMEBIN);
  Check(BSON('{%:[?,?,?]}',['BSON'],['awesome',5.05,1986])=BSONAWESOMEBIN);
  Check(BSON('{%:?}',['BSON'],[_Arr(['awesome',5.05,1986])])=BSONAWESOMEBIN);
  Check(BSON(['BSON','[','awesome',5.05,1986,']'])=BSONAWESOMEBIN);
  Check(BSON(['BSON','[','awesome',5.05,1986])=BSONAWESOMEBIN);
  o2 := BSONVariantType[bsonDat];
  Check(VariantSaveJSON(o2)=u);
  _Json('{BSON: ["test", 5.05, 1986]}',o);
  Check(VariantSaveMongoJSON(o,modMongoStrict)='{"BSON":["test",5.05,1986]}');
  u := VariantSaveMongoJSON(_Obj(['name','John',
    'doc',_Obj(['one',1,'two',_Arr(['one',2])])]),modMongoStrict);
  CheckEqual(u,'{"name":"John","doc":{"one":1,"two":["one",2]}}');
  Check(VariantSaveJson(BSONVariant(u))=u);
  Check(BSONDocumentToJSON(BSONFieldSelector(['a','b','c']))='{"a":1,"b":1,"c":1}');
  Check(BSONDocumentToJSON(BSONFieldSelector('a,b,c'))='{"a":1,"b":1,"c":1}');
  Check(VariantSaveMongoJSON(BSONVariantFieldSelector(['a','b','c']),modMongoShell)='{a:1,b:1,c:1}');
  Check(VariantSaveMongoJSON(BSONVariantFieldSelector('a,b,c'),modMongoShell)='{a:1,b:1,c:1}');
  o := _Obj(['id',ObjectID(BSONID),'name','John','date',variant(d2)]);
  u := VariantSaveMongoJSON(o,modNoMongo);
  u2 := FormatUTF8('{"id":"%","name":"John","date":"%"}',[BSONID,st]);
  CheckEqual(u,u2);
  u3 := VariantSaveJson(BSONVariant(u));
  Check(u3=FormatUTF8('{"id":"%","name":"John","date":{"$date":"%"}}',[BSONID,st]));
  u3 := VariantSaveMongoJSON(BSONVariant(u),modNoMongo);
  Check(u3=u);
  u := VariantSaveMongoJSON(o,modMongoShell);
  CheckEqual(u,FormatUTF8('{id:ObjectId("%"),name:"John",date:ISODate("%")}',[BSONID,st]));
  u3 := VariantSaveJson(BSONVariant(u));
  u := VariantSaveJSON(o);
  CheckEqual(u,FormatUTF8('{"id":{"$oid":"%"},"name":"John","date":"%"}',[BSONID,st]));
  u := VariantSaveMongoJSON(o,modMongoStrict);
  CheckEqual(u,FormatUTF8('{"id":{"$oid":"%"},"name":"John","date":{"$date":"%"}}',[BSONID,st]));
  Check(u3=u);
  _Json(u,o2);
  u := VariantSaveMongoJSON(o2,modMongoShell);
  CheckEqual(u,FormatUTF8('{id:ObjectId("%"),name:"John",date:ISODate("%")}',[BSONID,st]));
  _Json(u,o2);
  u := VariantSaveMongoJSON(o2,modNoMongo);
  CheckEqual(u,u2);
  o2 := _JsonFmt('{ id: objectID( "%" ) , name: "John", date: new date( "%" ) }',[BSONID,st],[]);
  u := VariantSaveMongoJSON(o2,modNoMongo);
  CheckEqual(u,u2);
  o2 := _JsonFmt('{id:objectID(?),name:?,date:ISODate(?)}',[],[BSONID,'John',st]);
  u := VariantSaveMongoJSON(o2,modNoMongo);
  CheckEqual(u,u2);
  u := VariantSaveMongoJSON(o2,modMongoShell);
  CheckEqual(u,FormatUTF8('{id:ObjectId("%"),name:"John",date:ISODate("%")}',[BSONID,st]));
  _Json(u,o2);
  u := VariantSaveMongoJSON(o2,modNoMongo);
  CheckEqual(u,u2);
  bin := VariantSave(o2);
  u := VariantSaveMongoJSON(VariantLoad(bin,@JSON_OPTIONS[true]),modNoMongo);
  CheckEqual(u,u2);
  check(VariantSaveMongoJSON(VariantLoad(bin,@JSON_OPTIONS[true]),modNoMongo)=u2,'twice to ensure bin is untouched');
  u := VariantSaveMongoJSON(_Json('{id:ObjectId(),name:"John"}'),modNoMongo);
  Check(IdemPChar(Pointer(u),'{"ID":"'),'ObjectId() constructor ');
  Check(PosEx('","name":"John"}',u)=32);
  u2 := VariantSaveMongoJSON(_Json('{id:ObjectId(),name:"John"}'),modNoMongo);
  Check(u2<>u,'should be genuine');
  o := _JSONFmt('{type:{$in:?}}',[],[_Arr(['food','snack'])]);
  u := VariantSaveJSON(o);
  CheckEqual(u,'{"type":{"$in":["food","snack"]}}');
  u := VariantSaveMongoJSON(o,modMongoShell);
  CheckEqual(u,'{type:{$in:["food","snack"]}}');
  o := _JSON('{"hello": null}');
  Check(TVarData(o).VType=DocVariantVType);
  check(string(o)='{"hello":null}');
  o := _JSON('{"hello": world}');
  Check(TVarData(o).VType=varEmpty,'invalid JSON content');
  CheckRegEx(_Json('{name:"John",field:{ "$regex": "acme.*corp", $options: "i" }}'));
  CheckRegEx(_Json(REGEX2));
  CheckRegEx(_JsonFast('{"name":"John",field:{ "$regex": "acme.*corp", $options: "i" }}'));
  CheckRegEx(_JsonFast(REGEX2));
  temp := BSON(REGEX2);
  b := pointer(temp);
  u := BSONToJSON(b,betDoc,0,modMongoStrict);
  CheckEqual(u,'{"name":"John","field":'+REGEX+'}');
  o2 := BSONVariant(REGEX2);
  Check(string(o2)='{name:"John",field:/acme.*corp/i}','MongoShell in string cast');
  Check(VariantSaveJson(o2)=u);
  temp := BSON('{name:?,field:/%/i}',['acme.*corp'],['John']);
  b := pointer(temp);
  u2 := BSONToJSON(b,betDoc,0,modMongoStrict);
  CheckEqual(u,u2);
  u := VariantSaveMongoJSON(_Json('{name:"John",date: new date() , field: /acme.*corp/i}'),modMongoStrict);
  u2 := VariantSaveMongoJSON(_Json('{name:"John",date:new date(),field:/acme.*corp/i}'),modMongoStrict);
  o := _JSON(u);
  o2 := _JSON(u2);
  Check(o.name=o2.name);
  d := TDateTime(o.date);
  d2 := TDateTime(o2.date);
  Check(d>NowUTC-1);
  Check(d2-d<0.1);
  u := VariantSaveMongoJSON(o.Field,modMongoStrict);
  u2 := VariantSaveMongoJSON(o2.Field,modMongoStrict);
  CheckEqual(u,u2);
  CheckEqual(u,REGEX);
  u := VariantSaveMongoJSON(o.Field,modMongoShell);
  u2 := VariantSaveMongoJSON(o2.Field,modMongoShell);
  CheckEqual(u,u2);
  CheckEqual(u,'/acme.*corp/i');
  u := VariantSaveMongoJSON(o.Field,modMongoStrict);
  u2 := VariantSaveMongoJSON(o2.Field,modMongoStrict);
  CheckEqual(u,u2);
  CheckEqual(u,REGEX);
  u := VariantSaveJSON(o.Field);
  u2 := VariantSaveJSON(o2.Field);
  CheckEqual(u,u2);
  CheckEqual(u,REGEX);
  o := _Json('{ tags: { $in: [ /^be/, /^st/ ] } }');
  u := VariantSaveMongoJSON(o,modMongoStrict);
  CheckEqual(u,'{"tags":{"$in":[{"$regex":"^be","$options":""},{"$regex":"^st","$options":""}]}}');
  temp := BSON(u,[],[]);
  b := pointer(temp);
  u2 := VariantSaveMongoJSON(o,modMongoShell);
  Check(u2='{tags:{$in:[/^be/,/^st/]}}');
  u := VariantSaveMongoJSON(_Json(u),modMongoShell);
  CheckEqual(u,u2);
  u2 := BSONToJSON(b,betDoc,0,modMongoShell);
  CheckEqual(u,u2);
  temp := BSON('{id:ObjectId(),doc:{name:?,date:ISODate(?)}}',[],['John',NowUTC]);
  b := pointer(temp);
  u := BSONToJSON(b,betDoc,0,modMongoShell);
  Check(IdemPChar(pointer(u),'{ID:OBJECTID("'));
  Check(PosEx('"),doc:{name:"John",date:ISODate("',u)>10);
  u := BSONDocumentToJSON(BSON(['doc','{','name','John','year',1982,'}','id',123]));
  CheckEqual(u,'{"doc":{"name":"John","year":1982},"id":123}');
  u := BSONDocumentToJSON(BSON(['doc','{','name','John','abc','[','a','b','c',']','}','id',123]));
  CheckEqual(u,'{"doc":{"name":"John","abc":["a","b","c"]},"id":123}');
  o2 := NumberDecimal('123.5600');
  u := VariantSaveJSON(o2);
  CheckEqual(u,'{"$numberDecimal":"123.5600"}');
  o := _Json('{ num: '+u+'}');
  u := VariantSaveMongoJSON(o,modMongoStrict);
  CheckEqual(u,'{"num":{"$numberDecimal":"123.5600"}}');
  u := VariantSaveMongoJSON(o,modMongoShell);
  CheckEqual(u,'{num:NumberDecimal("123.5600")}');
  o := BSONVariant(['num',o2]);
  u := VariantSaveMongoJSON(o,modMongoStrict);
  CheckEqual(u,'{"num":{"$numberDecimal":"123.5600"}}');
  u := VariantSaveMongoJSON(o,modMongoShell);
  CheckEqual(u,'{num:NumberDecimal("123.5600")}');
  o := _ObjFast(['num',o2]);
  u := VariantSaveMongoJSON(o,modMongoStrict);
  CheckEqual(u,'{"num":{"$numberDecimal":"123.5600"}}');
  o2 := _JsonFast(u);
  {$ifdef FPC} // TCustomVariantType.CompareOp not yet supported :(
  check(string(o)=string(o2),'o=o2');
  {$else}
  check(o=o2,'o=o2');
  {$endif}
  u := VariantSaveMongoJSON(o,modMongoShell);
  CheckEqual(u,'{num:NumberDecimal("123.5600")}');
  o2 := _JsonFast(u);
  {$ifdef FPC} // TCustomVariantType.CompareOp not yet supported :(
  check(string(o)=string(o2),'o=o2');
  {$else}
  check(o=o2,'o=o2');
  {$endif}
  temp := BSON(u,[],[]);
  b := pointer(temp);
  u2 := BSONToJSON(b,betDoc,0,modMongoShell);
  CheckEqual(u,u2);
  u2 := BSONToJSON(b,betDoc,0,modMongoStrict);
  check(u2='{"num":{"$numberDecimal":"123.5600"}}');
  check(dec.FromVariant(o2.num));
  check(dec.ToText='123.5600');
  o2 := dec.ToVariant;
  u := VariantSaveJSON(o2);
  CheckEqual(u,'{"$numberDecimal":"123.5600"}');
end;

procedure TTestLowLevelTypes._TDocVariant;
procedure CheckDoc(var Doc: TDocVariantData; ExpectedYear: integer=1972);
var JSON: RawUTF8;
begin
  if CheckFailed(Doc.VarType=DocVariantVType) then
    exit;
  Check(Doc.Kind=dvObject);
  Check(Doc.Count=2);
  Check(Doc.Names[0]='name');
  Check(Doc.Values[0]='John');
  Check(variant(Doc)._kind=ord(dvObject));
  Check(variant(Doc).name='John');
  Check(variant(Doc).name=Doc.Value['name']);
  Check(variant(Doc).birthYear=ExpectedYear);
  Check(variant(Doc).birthYEAR=Doc.Value['birthYear']);
  Check(variant(Doc)._Count=2);
  Check(variant(Doc).Name(0)='name');
  Check(variant(Doc).Name(1)='birthyear');
  Check(variant(Doc)._(0)='John');
  Check(variant(Doc)._(1)=ExpectedYear);
  Check(variant(Doc).Value(0)='John');
  Check(variant(Doc).Value(1)=ExpectedYear);
  JSON := '{"name":"John","birthyear":'+Int32ToUTF8(ExpectedYear)+'}';
  Check(Doc.ToJSON=JSON);
  Check(variant(Doc)._JSON=JSON);
  Check(variant(Doc)._JSON__=JSON,'pseudo methods use IdemPChar');
  Check(VariantSaveMongoJSON(variant(Doc),modMongoStrict)=JSON);
  Check(VariantToUTF8(variant(Doc))=JSON);
  Check(Doc.U['name']='John');
  Check(Doc.I['birthyear']=ExpectedYear);
end;
var discogs: RawUTF8;
procedure CheckNestedDoc(aOptions: TDocVariantOptions=[]);
var JSON, JSON2: RawUTF8;
    Doc, Doc2: TDocVariantData;
    Doc2Doc, V, Disco: variant;
    i: Integer;
begin
  V := _JSON('["one",2,3]',aOptions);
  Check(V._JSON='["one",2,3]');
  Doc.InitObject(['name','John','birthyear',1972],aOptions+[dvoReturnNullForUnknownProperty]);
  CheckDoc(Doc);
  Check(Doc.Value['toto']=null);
  Check(variant(Doc).toto=null);
  Check(Doc.Value[10]=null);
  Doc2.InitObject(['id',10,
    'doc',_Obj(['name','John','birthyear',1972],aOptions)]);
  Check(Doc2.Kind=dvObject);
  Check(variant(Doc2)._kind=ord(dvObject));
  Check(Doc2.Count=2);
  Check(Doc2.Value['id']=10);
  Check(variant(Doc2).id=10);
  Check(variant(Doc2).doc._kind=ord(dvObject));
  Doc2Doc := variant(Doc2).doc;
  CheckDoc(DocVariantData(Doc2Doc)^);
  CheckDoc(DocVariantData(variant(Doc2).doc)^);
  Doc2Doc := Doc2.GetValueOrRaiseException('doc');
  JSON := '{"id":10,"doc":{"name":"John","birthyear":1972}}';
  Check(Doc2.ToJSON=JSON);
  Check(Doc2.I['id']=10);
  Check(Doc2.O['doc'].U['name']='John');
  Check(Doc2.O['doc'].I['birthyear']=1972);
  //Doc2Doc.birthyear := 1980;
  variant(DocVariantData(Doc2Doc)^).birthyear := 1980;
  JSON2 := Doc2.ToJSON;
  if dvoValueCopiedByReference in aOptions then begin
    Check(JSON2='{"id":10,"doc":{"name":"John","birthyear":1980}}');
    Check(Doc2.O['doc'].I['birthyear']=1980);
  end else begin
    Check(JSON2=JSON);
    Check(Doc2.O['doc'].I['birthyear']=1972);
  end;
  _JSON(JSON,V,aOptions);
  Check(V._count=2);
  Check(V.id=10);
  Check(V.doc._kind=ord(dvObject));
  Check(V.doc.name='John');
  Check(V.doc.birthYear=1972);
  if discogs<>'' then begin
    FileFromString(JSONReformat(discogs),ChangeFileExt(discogsFileName,'2.json'));
    Disco := _JSON(discogs,aOptions);
    Check(Disco.releases._count<=Disco.pagination.items);
    for i := 0 to Disco.Releases._count-1 do begin
      Check(Disco.Releases._(i).id>0);
      V := Disco.Releases._(i);
      Check(V._count>0);
      Check(V.title<>'');
    end;
//    if aOptions=[] then
//      FileFromString(TDocVariantData(Disco).ToJSON,'discoVariant.json');
  end;
  _JSON('[]',V,aOptions);
  Check(V._kind=ord(dvArray));
  Check(V._count=0);
  _JSON('null',V,aOptions);
  Check(V._kind=ord(dvObject));
  Check(V._count=0);
end;
procedure DoChange(var oSeasons: variant);
var i: integer;
    oSeason: variant;
begin
  for i := 0 to oSeasons._Count-1 do begin
    oSeason := oSeasons._(i);
    oSeason.Name := 'CHANGED !';
    oSeason.Extra := 'blabla';
  end;
end;
const MAX=20000;
  TEST_DATA_1 = '['+
  '{"REC_ID":1,"CHANNEL":117,"PHONE":"5004392222,12345678","RELATION_ID":10,' +
  '"TIMESTAMP_CALL":"2017-10-26T04:48:14"},{"REC_ID":2,"CHANNEL":null,"PHONE":' +
  '"1234","RELATION_ID":11,"TIMESTAMP_CALL":"2017-10-26T04:48:14"},' +
  '{"REC_ID":3,"CHANNEL":174,"PHONE":"9149556917","RELATION_ID":12,' +
  '"TIMESTAMP_CALL":"2017-10-26T04:48:14"}]';
var Doc,Doc2: TDocVariantData;
    vr: TTVarRecDynArray;
    i,ndx: PtrInt;
    V,V1,V2: variant;
    s,j: RawUTF8;
    vd: double;
    vs: single;
    lTable: TSQLTableJSON;
    lRefreshed: Boolean;
begin
  Doc.Init;
  Check(Doc.Kind=dvUndefined);
  Check(variant(Doc)._kind=ord(dvUndefined));
  Doc.AddValue('name','Jonas');
  Doc.AddValue('birthyear',1972);
  Check(Doc.Value['name']='Jonas');
  Check(Doc.Value['birthyear']=1972);
  Check(Doc.U['name']='Jonas');
  Check(Doc.I['birthyear']=1972);
  Doc.Value['name'] := 'John';
  Check(Doc.Value['name']='John');
  CheckDoc(Doc);
  Doc.Clear;
  Doc.InitFast;
  Check(Doc.Kind=dvUndefined);
  Check(variant(Doc)._kind=ord(dvUndefined));
  Doc.AddValue('name','Jonas');
  Doc.AddValue('birthyear',1972);
  Check(Doc.Value['name']='Jonas');
  Check(Doc.Value['birthyear']=1972);
  Check(Doc.U['name']='Jonas');
  Check(Doc.I['birthyear']=1972);
  Doc.Value['name'] := 'John';
  Check(Doc.Value['name']='John');
  Check(Doc.U['name']='John');
  CheckDoc(Doc);
  Doc2.InitJSON(Doc.ToJSON);
  CheckDoc(Doc2);
  Doc.Clear;
  Doc.InitArray(['one',2,3.0]);
  Check(variant(Doc)._kind=ord(dvArray));
  Check(variant(Doc)._count=3);
  if not CheckFailed(Doc.Count=3) then begin
    Check(Doc.Values[0]='one');
    Check(Doc.Values[1]=2);
    Check(Doc.Values[2]=3.0);
    Check(Doc.Value[0]='one');
    Check(Doc.Value[1]=2);
    Check(Doc.Value[2]=3.0);
    for i := 0 to Doc.Count-1 do
      Check(VariantCompare(Doc.Values[i],Doc.Value[i])=0);
  end;
  Check(Doc.ToJSON='["one",2,3]');
  Check(Variant(Doc)._JSON='["one",2,3]');
  Doc.ToArrayOfConst(vr);
  s := FormatUTF8('[?,?,?]',[],vr,true);
  check(s='["one",2,3]');
  s := FormatUTF8('[%,%,%]',vr,[],true);
  check(s='[one,2,3]');
  s := FormatUTF8('[?,?,?]',[],Doc.ToArrayOfConst,true);
  check(s='["one",2,3]');
  s := FormatUTF8('[%,%,%]',Doc.ToArrayOfConst,[],true);
  check(s='[one,2,3]');
  V := _JSON(' [ "one" ,2,3 ]   ');
  Check(V._count=3);
  with TDocVariantData(V) do begin
    Check(Count=3);
    Check(Values[0]='one');
    Check(Values[1]=2);
    Check(Values[2]=3.0);
  end;
  for i := 0 to V._count-1 do
    Check(V._(i)=Doc.Values[i]);
  {$ifdef FPC}TDocVariantData(V).AddItem{$else}V.Add{$endif}(4);
  Check(V._count=4);
  for i := 0 to 2 do
    Check(V._(i)=Doc.Values[i]);
  Check(V._(3)=4);
  V._ := 'a5';
  Check(V._count=5);
  for i := 0 to 2 do
    Check(V._(i)=Doc.Values[i]);
  Check(V._(3)=4);
  Check(V._(4)='a5');
  Check(V{$ifdef FPC}._JSON{$endif}='["one",2,3,4,"a5"]');
  discogs := StringFromFile(discogsFileName);
  CheckNestedDoc([]);
  CheckNestedDoc([dvoValueCopiedByReference]);
  CheckNestedDoc([dvoJSONObjectParseWithinString]);
  CheckNestedDoc([dvoJSONObjectParseWithinString,dvoValueCopiedByReference]);
  V1 := _Obj(['name','John','year',1972],[dvoValueCopiedByReference]);
  V2 := V1;             // creates a reference to the V1 instance
  V2.name := 'James';   // modifies V2.name, but also V1.name
  Check(V1.name='James');
  Check(V2.name='James');
  Check(V1{$ifdef FPC}._JSON{$endif}='{"name":"James","year":1972}');
  _Unique(V1);          // change options of V1 to be by-value
  V2 := V1;             // creates a full copy of the V1 instance
  V2.name := 'John';    // modifies V2.name, but not V1.name
  Check(V1.name='James');
  Check(V2.name='John');
  V1 := _Arr(['root',V2]); // created as by-value by default, as V2 was
  Check(V1._Count=2);
  _UniqueFast(V1);      // change options of V1 to be by-reference
  V2 := V1;
  Check(V1._(1){$ifdef FPC}._JSON{$endif}='{"name":"John","year":1972}');
  {$ifdef FPC}TDocVariantData(V1).Values[1]{$else}V1._(1){$endif}.name := 'Jim';
  Check(V1{$ifdef FPC}._JSON{$endif}='["root",{"name":"Jim","year":1972}]');
  Check(V2{$ifdef FPC}._JSON{$endif}='["root",{"name":"Jim","year":1972}]');
  _UniqueFast(V2); // now V1 modifications should not affect V2
  Doc.Clear;
  Doc.Init;
  for i := 0 to MAX do begin
    UInt32ToUtf8(i,s);
    Check(Doc.AddValue(s,s)=i);
  end;
  Check(Doc.Count=MAX+1);
  for i := 0 to MAX do
    Check(GetInteger(Pointer(Doc.Names[i]))=i);
  for i := 0 to MAX do
    Check(Doc.Values[i]=i);
  Doc2.Clear;
  check(Doc2.Count=0);
  s := Doc.ToJSON;
  CheckEqual(Hash32(s),2110959969,'bigjson');
  Doc2.InitJSON(s);
  check(Doc2.Count=MAX+1);
  for i := 0 to MAX do
    Check(Doc2.Values[i]=Doc.Values[i]);
  for i := MAX downto 0 do
    if i and 1=0 then
      Doc.Delete(i);
  Check(Doc.Count=MAX div 2);
  check(Doc2.Count=MAX+1);
  for i := 0 to Doc.Count-1 do
    Check(Doc.Names[i]=Doc.Values[i]);
  s := Doc2.ToJSON;
  CheckEqual(Hash32(s),2110959969,'bigjson2');
  Check(TDocVariantData(V1)._[1].U['name']='Jim');
  Check(TDocVariantData(V1)._[1].I['year']=1972);
  {$ifdef FPC}_Safe(V1)^.AddItem{$else}V1.Add{$endif}(3.1415);
  Check(V1{$ifdef FPC}._JSON{$endif}='["root",{"name":"Jim","year":1972},3.1415]');
  {$ifdef FPC}TDocVariantData(V1)._[1]{$else}V1._(1){$endif}.Delete('year');
  Check(V1{$ifdef FPC}._JSON{$endif}='["root",{"name":"Jim"},3.1415]');
  {$ifdef FPC}TDocVariantData(V1){$else}V1{$endif}.Delete(1);
  Check(V1{$ifdef FPC}._JSON{$endif}='["root",3.1415]');
  TDocVariantData(V2).DeleteByProp('name','JIM',true);
  Check(V2{$ifdef FPC}._JSON{$endif}='["root",{"name":"Jim","year":1972}]');
  TDocVariantData(V2).DeleteByProp('name','JIM',false);
  Check(V2{$ifdef FPC}._JSON{$endif}='["root"]');
  s := '{"Url":"argentina","Seasons":[{"Name":"2011/2012","Url":"2011-2012",'+
    '"Competitions":[{"Name":"Ligue1","Url":"ligue-1"},{"Name":"Ligue2","Url":"ligue-2"}]},'+
    '{"Name":"2010/2011","Url":"2010-2011","Competitions":[{"Name":"Ligue1","Url":"ligue-1"},'+
    '{"Name":"Ligue2","Url":"ligue-2"}]}]}';
  Check(Hash32(s)=$BF60E202);
  V1 := _Json(s);
  V2 := V1.seasons;
  DoChange(V2);
  j := VariantSaveJSON(V1);
  Check(j<>s);
  Check(Hash32(j)=$6998B225,'changed');
  Check(Hash32(VariantSaveJSON(V2))=$92FEB37B);
  V1 := _Json(s);
  V2 := V1.seasons;
  _Unique(V2);
  DoChange(V2);
  Check(VariantSaveJSON(V1)=s);
  Check(Hash32(VariantSaveJSON(V2))=$92FEB37B);
  V2 := TDocVariant.NewUnique(V1.Seasons);
  DoChange(V2);
  Check(VariantSaveJSON(V1)=s);
  Check(Hash32(VariantSaveJSON(V2))=$92FEB37B);
  V2 := _copy(V1.Seasons);
  DoChange(V2);
  Check(VariantSaveJSON(V1)=s);
  Check(Hash32(VariantSaveJSON(V2))=$92FEB37B);
  s := _Safe(V1.Seasons)^.ToNonExpandedJSON;
  Check(s='{"fieldCount":3,"rowCount":2,"values":["Name","Url","Competitions",'+
    '"2011/2012","2011-2012",[{"Name":"Ligue1","Url":"ligue-1"},{"Name":"Ligue2"'+
    ',"Url":"ligue-2"}],"2010/2011","2010-2011",[{"Name":"Ligue1","Url":"ligue-1"}'+
    ',{"Name":"Ligue2","Url":"ligue-2"}]]}');
  V := _Json('{result:{data:{"1000":"D1", "1001":"D2"}}}');
  Check(V.result{$ifdef FPC}._JSON{$endif}='{"data":{"1000":"D1","1001":"D2"}}');
  Check(V.result.data.Exists('1000'));
  Check(V.result.data.Exists('1001'));
  Check(not V.result.data.Exists('1002'));
  Check(DocVariantData(V.result.data).Value['1000']='D1');
  Check(V.result.data.Value(0)='D1');
  Check(V.result.data.Value('1000')='D1');
  Check(V.result.data.Value('1001')='D2');
  V := _Obj(['Z',10,'name','John','year',1972,'a',1],[]);
  j := VariantSaveJSON(V);
  Check(j='{"Z":10,"name":"John","year":1972,"a":1}');
  TDocVariantData(V).SortByName;
  j := VariantSaveJSON(V);
  Check(j='{"a":1,"name":"John","year":1972,"Z":10}');
  TDocVariantData(V).SortByName(@StrComp);
  j := VariantSaveJSON(V);
  Check(j='{"Z":10,"a":1,"name":"John","year":1972}');
  V := _JsonFast('{"Database":"\u201d\u00c9\u00c3\u00b6\u00b1\u00a2\u00a7\u00ad\u00a5\u00a4"}');
  {$ifdef FPC}
  j := VariantToUTF8(V.Database);
  {$else}
  j := V.Database;
  {$endif}
  Check((j<>'')and(j[1]=#$E2)and(j[2]=#$80)and(j[3]=#$9D));
  v1 := _Arr([]);
  vs := 1.5;
  {$ifdef FPC}_Safe(V1)^.AddItem{$else}V1.Add{$endif}(vs);
  CheckEqual(VariantSaveJSON(v1),'[1.5]','VariantSaveJSON');
  vd := 1.7;
  {$ifdef FPC}_Safe(V1)^.AddItem{$else}V1.Add{$endif}(vd);
  CheckEqual(VariantSaveJSON(v1),'[1.5,1.7]');
  v2 := _obj(['id',1]);
  Check(VariantSaveJSON(v2)='{"id":1}');
  {$ifdef FPC}_Safe(v1)^.AddItem(v2); // FPC does not accept v1.Add(v2)
  {$else}v1.Add(v2);{$endif}
  Check(VariantSaveJSON(v1)='[1.5,1.7,{"id":1}]');
  s := 'abc';
  {$ifdef FPC}_Safe(v1)^.AddItem(s); // FPC does not accept v1.Add(s)
  {$else}v1.Add(s);{$endif}
  Check(VariantSaveJSON(v1)='[1.5,1.7,{"id":1},"abc"]');
  RawUTF8ToVariant('def',v2);
  {$ifdef FPC}_Safe(v1)^.AddItem{$else}v1.Add{$endif}(v2);
  Check(VariantSaveJSON(v1)='[1.5,1.7,{"id":1},"abc","def"]');
  Doc.Clear;
  Doc.InitObjectFromPath('name','toto');
  check(Doc.ToJSON='{"name":"toto"}');
  Doc.Clear;
  Doc.InitObjectFromPath('people.age',31);
  check(Doc.ToJSON='{"people":{"age":31}}');
  check(Doc.O['people'].ToJson='{"age":31}');
  check(Doc.O['people2'].ToJson='null');
  Doc.O_['people2'].AddValue('name','toto');
  check(Doc.ToJSON='{"people":{"age":31},"people2":{"name":"toto"}}');
  check(Doc.A['arr'].ToJson='null');
  Doc.A_['arr'].AddItems([1,2.2,'3']);
  check(Doc.ToJSON='{"people":{"age":31},"people2":{"name":"toto"},"arr":[1,2.2,"3"]}');
  Doc.Clear;
  check(Doc.A['test'].ToJson='null');
  Doc.A_['test']^.AddItems([1,2]);
  j := Doc.ToJSON;
  check(j='{"test":[1,2]}');
  check(Doc.A['test'].ToJson='[1,2]');
  Doc.A_['test']^.AddItems([3,4]);
  check(Doc.ToJSON='{"test":[1,2,3,4]}');
  check(Doc.A['test'].ToJson='[1,2,3,4]');
  Doc.Clear;
  check(not Doc.FlattenAsNestedObject('wrong'));
  Doc.InitJSON('{"p.a1":5,"p.a2":"dfasdfa"}');
  check(not Doc.FlattenAsNestedObject('wrong'));
  check(Doc.ToJSON='{"p.a1":5,"p.a2":"dfasdfa"}');
  check(Doc.FlattenAsNestedObject('p'));
  check(Doc.ToJSON='{"p":{"a1":5,"a2":"dfasdfa"}}');
  check(not Doc.FlattenAsNestedObject('p'));
  s := '[{"Val1":"blabla","Val2":"bleble"},{"Val1":"blibli","Val2":"bloblo"}]';
  v := _Json(s);
  v1 := _Copy(v._(0)); // expect a true instance for v1.Val1 := ... below
  check(v1.val1='blabla');
  v2 := _Obj([]); // or TDocVariant.New(v2);
  v2.Val1 := 'blublu';
  v2.Val2 := 'blybly';
  v1.Val1 := v2.Val1;
  v1.Val2 := v2.Val2;
  check(VariantSaveJSON(v1)=VariantSaveJSON(v2));
  Doc.Clear;
  V := _JSON('{"ID": 1,"Notation": "ABC", "Price": 10.1, "CustomNotation": "XYZ"}');
  Doc.InitCopy(V, []);
  Doc.I['ID'] := 2;
  Doc.Delete('CustomNotation');
  s := Doc.ToJSON;
  check(s='{"ID":2,"Notation":"ABC","Price":10.1}');
  s := VariantSaveJSON(V);
  check(s='{"ID":1,"Notation":"ABC","Price":10.1,"CustomNotation":"XYZ"}');
  // some tests to avoid regression about bugs reported by users on forum
  lTable := TSQLTableJSON.Create('');
  try
    lTable.UpdateFrom(TEST_DATA_1,lRefreshed,nil);
    ndx := lTable.FieldIndex('RELATION_ID');
    Check(ndx=3);
    lTable.SortFields(ndx);
    doc.Clear;
    i := lTable.SearchFieldSorted('10',{RELATION_ID}ndx);
    lTable.ToDocVariant(i,variant(doc));
    doc.Delete('REC_ID');
    doc.Clear;
    i := lTable.SearchFieldSorted('11',{RELATION_ID}ndx);
    lTable.ToDocVariant(i,variant(doc));
    V := doc.Value['PHONE'];
    check(V='1234');
  finally
    lTable.Free;
  end;
end;

{$endif LVCL}

procedure TTestLowLevelTypes.RTTI;
var i: Integer;
    tmp: RawUTF8;
    auto: TPersistentAutoCreateFieldsTest;
    s: TSynLogInfos;
    astext: boolean;
    P: PUTF8Char;
    eoo: AnsiChar;
    e: TEmoji;
begin
  check(EMOJI_UTF8[eNone]='');
  checkEqual(BinToHex(EMOJI_UTF8[eGrinning]),'F09F9880');
  checkEqual(BinToHex(EMOJI_UTF8[ePray]),'F09F998F');
  check(EmojiFromText(Pointer(EMOJI_UTF8[eGrinning]),4)=eNone);
  check(EmojiFromText(nil,0)=eNone);
  checkEqual(EmojiToDots('toto'),'toto');
  for e := low(e) to high(e) do begin
    check(EmojiFromText(pointer(EMOJI_TEXT[e]),length(EMOJI_TEXT[e]))=e);
    if e=eNone then
      continue;
    check(length(EMOJI_UTF8[e])=4);
    P := Pointer(EMOJI_UTF8[e]);
    checkEqual(NextUTF8UCS4(P),$1f5ff+ord(e));
    FormatUTF8(':smile % ok',[EMOJI_TAG[e]],tmp);
    P := pointer(tmp);
    check(EmojiParseDots(P)=eNone);
    check(IdemPChar(P,'SMILE :'));
    inc(P,6);
    check(P^=':');
    check(EmojiParseDots(P)=e);
    check(IdemPChar(P,' OK'));
    checkEqual(EmojiToDots(EMOJI_UTF8[e]),EMOJI_TAG[e]);
    checkEqual(EmojiToDots(' '+EMOJI_UTF8[e]+' '),' '+EMOJI_TAG[e]+' ');
    checkEqual(EmojiToDots(EmojiFromDots(tmp)),tmp);
  end;
  tmp := ':) :( :JoY: :o :|';
  P := pointer(tmp);
  check(EmojiParseDots(P)=eSmiley);
  check(P^=' ');
  inc(P);
  check(EmojiParseDots(P)=eFrowning);
  check(IdemPChar(P,' :JOY:'));
  inc(P);
  check(EmojiParseDots(P)=eJoy);
  check(P^=' ');
  inc(P);
  check(EmojiParseDots(P)=eOpen_mouth);
  check(P^=' ');
  inc(P);
  check(EmojiParseDots(P)=eExpressionless);
  check(P^=#0);
  with PTypeInfo(TypeInfo(TSynLogInfo))^.EnumBaseType^ do
    for i := 0 to integer(high(TSynLogInfo)) do begin
{$ifdef VERBOSE}writeln(i,' ',GetEnumName(i)^, ' ',GetEnumNameTrimed(i));{$endif}
     tmp := GetEnumNameTrimed(i);
     Check(GetEnumNameValue(GetEnumName(i)^)=i);
     Check(GetEnumNameTrimedValue(tmp)=i);
     Check(GetEnumNameTrimedValue(pointer(tmp))=i);
     Check(GetEnumNameValue(tmp)=i);
     Check(GetEnumNameValue(pointer(tmp))=i);
     Check(GetEnumNameValue(SynCommons.GetEnumName(TypeInfo(TSynLogInfo),i)^)=i);
     Check(SynCommons.GetEnumNameValue(TypeInfo(TSynLogInfo),pointer(tmp),length(tmp),true)=i);
     tmp := GetEnumName(i)^;
     Check(SynCommons.GetEnumNameValue(TypeInfo(TSynLogInfo),pointer(tmp),length(tmp))=i);
  end;
  for astext := false to true do begin
    integer(s) := 0;
    for i := -1 to ord(high(TSynLogInfo)) do begin
      if i>=0 then
        SetBit(s,i);
      tmp := SaveJSON(s,TypeInfo(TSynLogInfos),astext);
      if astext then
        case i of
        -1: Check(tmp='[]');
        0:  Check(tmp='["sllNone"]');
        else if i=ord(high(TSynLogInfo)) then
            Check(tmp='["*"]');
        end else
        Check(GetCardinal(pointer(tmp))=cardinal(s));
      tmp := tmp+','; // mimics GetJsonField layout
      P := pointer(tmp);
      eoo := ' ';
      Check(GetSetNameValue(TypeInfo(TSynLogInfos),P,eoo)=cardinal(s));
      Check(eoo=',');
    end;
  end;
  Check(PTypeInfo(TypeInfo(TSynLogInfos))^.SetEnumType=
    PTypeInfo(TypeInfo(TSynLogInfo))^.EnumBaseType);
  with PTypeInfo(TypeInfo(TSQLRecordTest))^ do begin
    Check(InheritsFrom(TSQLRecordTest));
    Check(InheritsFrom(TSQLRecord));
    Check(not InheritsFrom(TSQLRecordPeople));
  end;
  Check(GetDisplayNameFromClass(nil)='');
  Check(GetDisplayNameFromClass(TSQLRecord)='Record');
  Check(GetDisplayNameFromClass(TSQLRecordPeople)='People');
  Check(GetDisplayNameFromClass(TObject)='Object');
  Check(GetDisplayNameFromClass(TSQLTable)='Table');
  Check(GetDisplayNameFromClass(TSynValidateRest)='ValidateRest');
  Check(InternalMethodInfo(TSQLRecord,'ABC')=nil);
  Check(InternalMethodInfo(TSQLRestServer,'ABC')=nil);
  Check(InternalMethodInfo(TSQLRestServer,'STAT')<>nil);
  Check(InternalMethodInfo(TSQLRestServer,'stat')^.MethodAddr=
    TSQLRestServer.MethodAddress('STAT'));
  Check(InternalMethodInfo(TSQLRestServer,'timestamp')<>nil);
  Check(InternalMethodInfo(TSQLRestServer,'timestamp')^.MethodAddr=
    TSQLRestServer.MethodAddress('TIMEstamp'));
  auto := TPersistentAutoCreateFieldsTest.CreateFake;
  try
    Check(auto.Value1<>nil);
    Check(auto.Value2<>nil);
    tmp := ObjectToJSON(auto);
    Check(tmp='{"Text":"text","Value1":{"Real":1.5,"Imaginary":2.5},'+
      '"Value2":{"Real":1.7,"Imaginary":2.7}}');
  finally
    auto.Free;
  end;
end;

{$endif DELPHI5OROLDER}

procedure TTestLowLevelTypes.UrlEncoding;
var i: integer;
    s,t: RawUTF8;
{$ifndef DELPHI5OROLDER}
    d: RawUTF8;
{$endif}
begin
  for i := 1 to 100 do begin
    s := RandomUTF8(i);
    t := UrlEncode(s);
    Check(UrlDecode(t)=s);
    {$ifndef DELPHI5OROLDER}
    d := 'seleCT='+t+'&where='+
      {$ifndef ENHANCEDRTL}Int32ToUtf8{$else}IntToStr{$endif}(i);
    Check(UrlEncode(['seleCT',s,'where',i])='?'+d);
    {$endif DELPHI5OROLDER}
  end;
end;

{$ifndef DELPHI5OROLDER}

procedure TTestLowLevelTypes._TSynTableStatement;
var Stmt: TSynTableStatement;
    Props: TSQLRecordProperties;
    bits: TSQLFieldBits;
    withID: boolean;
  procedure NewStmt(const SQL: RawUTF8);
  begin
    Stmt.Free;
    Stmt := TSynTableStatement.Create(SQL,Props.Fields.IndexByName,
      Props.SimpleFieldsBits[soSelect]);
    Check(Stmt.SQLStatement=SQL,'Statement should be valid');
  end;
  procedure CheckIdData(limit,offset: integer);
  begin
    Check(Stmt.TableName='tab');
    Check(Stmt.Where=nil,'no WHERE clause');
    Check((length(Stmt.Select)=2)and
      (Stmt.Select[0].Field=0) and
      (Props.Fields.List[Stmt.Select[1].Field-1].Name='Data'));
    Check(Stmt.Limit=limit);
    Check(Stmt.Offset=offset);
  end;
  procedure CheckWhere(isOR: Boolean);
  begin
    Check(Stmt.TableName='tab');
    Check(length(Stmt.Where)=2);
    Check(Stmt.Where[0].Field=0);
    Check(Stmt.Where[0].Operator=opGreaterThanOrEqualTo);
    Check(Stmt.Where[0].ValueInteger=10);
    Check(Stmt.Where[1].JoinedOR=isOR);
    Check(Props.Fields.List[Stmt.Where[1].Field-1].Name='YearOfBirth');
    Check(Stmt.Where[1].Operator=opGreaterThan);
    Check(Stmt.Where[1].ValueInteger=1600);
    Check(Stmt.Limit=10);
    Check(Stmt.Offset=20);
    Check((length(Stmt.Select)=2)and(Stmt.Select[1].Field=0)and
      (Props.Fields.List[Stmt.Select[0].Field-1].Name='Data'));
    Check(Stmt.OrderByField=nil);
  end;
begin
  Stmt := nil;
  Props := TSQLRecordPeople.RecordProps;
  NewStmt('select * from atable');
  Check(Stmt.TableName='atable');
  Check(Stmt.Where=nil);
  Stmt.SelectFieldBits(bits,withID);
  Check(withID);
  Check(IsEqual(bits,Props.SimpleFieldsBits[soSelect]));
  Check(Stmt.OrderByField=nil);
  NewStmt('select iD,Data from tab');
  CheckIdData(0,0);
  Check(Stmt.OrderByField=nil);
  NewStmt('select iD,Data from tab order by firstname');
  CheckIdData(0,0);
  Check((length(Stmt.OrderByField)=1)and(Props.Fields.List[Stmt.OrderByField[0]-1].Name='FirstName'));
  Check(not Stmt.OrderByDesc);
  NewStmt('select iD,Data from tab order by firstname desc');
  CheckIdData(0,0);
  Check((length(Stmt.OrderByField)=1)and(Props.Fields.List[Stmt.OrderByField[0]-1].Name='FirstName'));
  Check(Stmt.OrderByDesc);
  NewStmt('select rowid , Data from tab order by firstname , lastname desc');
  CheckIdData(0,0);
  Check((length(Stmt.OrderByField)=2) and
    (Props.Fields.List[Stmt.OrderByField[0]-1].Name='FirstName') and
    (Props.Fields.List[Stmt.OrderByField[1]-1].Name='LastName'));
  Check(Stmt.OrderByDesc);
  NewStmt('select rowid,Data from tab order by firstname,lastname limit 10');
  CheckIdData(10,0);
  Check((length(Stmt.OrderByField)=2) and
    (Props.Fields.List[Stmt.OrderByField[0]-1].Name='FirstName') and
    (Props.Fields.List[Stmt.OrderByField[1]-1].Name='LastName'));
  Check(not Stmt.OrderByDesc);
  NewStmt('select rowid,Data from tab group by firstname order by firstname,lastname');
  CheckIdData(0,0);
  Check((length(Stmt.GroupByField)=1) and
    (Props.Fields.List[Stmt.GroupByField[0]-1].Name='FirstName'));
  Check((length(Stmt.OrderByField)=2) and
    (Props.Fields.List[Stmt.OrderByField[0]-1].Name='FirstName') and
    (Props.Fields.List[Stmt.OrderByField[1]-1].Name='LastName'));
  NewStmt('select rowid,Data from tab group by firstname,lastname limit 10');
  CheckIdData(10,0);
  Check((length(Stmt.GroupByField)=2) and
    (Props.Fields.List[Stmt.GroupByField[0]-1].Name='FirstName') and
    (Props.Fields.List[Stmt.GroupByField[1]-1].Name='LastName'));
  Check(not Stmt.OrderByDesc);
  NewStmt('select iD,Data from tab limit   20');
  CheckIdData(20,0);
  Check(Stmt.OrderByField=nil);
  Check(not Stmt.OrderByDesc);
  NewStmt('select iD,Data from tab  offset   20');
  CheckIdData(0,20);
  Check(Stmt.OrderByField=nil);
  Check(not Stmt.OrderByDesc);
  NewStmt('select data,iD from tab where id >= 10 limit 10 offset 20 order by firstname desc');
  Check(Stmt.TableName='tab');
  Check(length(Stmt.Where)=1);
  Check(Stmt.Where[0].Field=0);
  Check(Stmt.Where[0].Operator=opGreaterThanOrEqualTo);
  Check(Stmt.Where[0].ValueInteger=10);
  Check(Stmt.Limit=10);
  Check(Stmt.Offset=20);
  Check((length(Stmt.Select)=2)and(Stmt.Select[1].Field=0)and
    (Props.Fields.List[Stmt.Select[0].Field-1].Name='Data'));
  Check((length(Stmt.OrderByField)=1)and(Props.Fields.List[Stmt.OrderByField[0]-1].Name='FirstName'));
  Check(Stmt.OrderByDesc);
  NewStmt('select iD,Data from tab where id in (1, 2, 3)');
  Check(Stmt.TableName='tab');
  Check(length(Stmt.Where)=1);
  Check(Stmt.Where[0].Field=0);
  Check(Stmt.Where[0].Operator=opIn);
  Check(Stmt.Where[0].Value='[1,2,3]');
  Check(Stmt.OrderByField=nil);
  NewStmt('select iD,Data from tab where firstname in ( ''a'' ,  ''b'', ''3''  ) order by id desc');
  Check(Stmt.TableName='tab');
  Check(length(Stmt.Where)=1);
  Check(Props.Fields.List[Stmt.Where[0].Field-1].Name='FirstName');
  Check(Stmt.Where[0].Operator=opIn);
  Check(Stmt.Where[0].Value='["a","b","3"]');
  Check((length(Stmt.OrderByField)=1)and(Stmt.OrderByField[0]=0));
  Check(Stmt.OrderByDesc);
  NewStmt('select data,iD from tab where id >= 10 and YearOfBirth > 1600 limit 10 offset 20');
  CheckWhere(false);
  NewStmt('select data,iD from tab where rowid>=10 or YearOfBirth>1600 offset 20 limit 10');
  CheckWhere(true);
  NewStmt('select data,iD from tab where id <> 100 or data is not null limit 20 offset 10');
  Check(Stmt.TableName='tab');
  Check(length(Stmt.Where)=2);
  Check(Stmt.Where[0].Field=0);
  Check(Stmt.Where[0].Operator=opNotEqualTo);
  Check(Stmt.Where[0].ValueInteger=100);
  Check(Stmt.Where[1].JoinedOR);
  Check(Props.Fields.List[Stmt.Where[1].Field-1].Name='Data');
  Check(Stmt.Where[1].Operator=opIsNotNull);
  Check(Stmt.Limit=20);
  Check(Stmt.Offset=10);
  Check((length(Stmt.Select)=2)and(Stmt.Select[1].Field=0)and
    (Props.Fields.List[Stmt.Select[0].Field-1].Name='Data'));
  Check(Stmt.OrderByField=nil);
  NewStmt('select data,iD from tab where firstname like "monet" or data is null limit 20 offset 10');
  Check(Stmt.TableName='tab');
  Check(length(Stmt.Where)=2);
  Check(Props.Fields.List[Stmt.Where[0].Field-1].Name='FirstName');
  Check(Stmt.Where[0].Operator=opLike);
  Check(Stmt.Where[0].Value='monet');
  Check(Stmt.Where[1].JoinedOR);
  Check(Props.Fields.List[Stmt.Where[1].Field-1].Name='Data');
  Check(Stmt.Where[1].Operator=opIsNull);
  Check(Stmt.Limit=20);
  Check(Stmt.Offset=10);
  Check((length(Stmt.Select)=2)and(Stmt.Select[1].Field=0)and
    (Props.Fields.List[Stmt.Select[0].Field-1].Name='Data'));
  Check(Stmt.OrderByField=nil);
  NewStmt('select count(*) from tab');
  Check(Stmt.TableName='tab');
  Check(Stmt.Where=nil);
  Check((length(Stmt.Select)=1)and(Stmt.Select[0].Field=0));
  Check((length(Stmt.Select)=1)and(Stmt.Select[0].FunctionName='count'));
  Check(Stmt.Limit=0);
  NewStmt('select count(*) from tab limit 10');
  Check(Stmt.TableName='tab');
  Check(Stmt.Where=nil);
  Check((length(Stmt.Select)=1)and(Stmt.Select[0].Field=0));
  Check((length(Stmt.Select)=1)and(Stmt.Select[0].FunctionName='count'));
  Check(Stmt.Limit=10);
  NewStmt('select count(*) from tab where yearofbirth>1000 limit 10');
  Check(Stmt.TableName='tab');
  Check(length(Stmt.Where)=1);
  Check(Props.Fields.List[Stmt.Where[0].Field-1].Name='YearOfBirth');
  Check(Stmt.Where[0].Operator=opGreaterThan);
  Check(Stmt.Where[0].ValueInteger=1000);
  Check((length(Stmt.Select)=1)and(Stmt.Select[0].Field=0));
  Check((length(Stmt.Select)=1)and(Stmt.Select[0].FunctionName='count'));
  Check(Stmt.Limit=10);
  NewStmt('select distinct ( yearofdeath )  from  tab where yearofbirth > :(1000): limit 20');
  Check(Stmt.TableName='tab');
  Check(length(Stmt.Where)=1);
  Check(Props.Fields.List[Stmt.Where[0].Field-1].Name='YearOfBirth');
  Check(Stmt.Where[0].Operator=opGreaterThan);
  Check(Stmt.Where[0].ValueInteger=1000);
  Check((length(Stmt.Select)=1) and
    (Props.Fields.List[Stmt.Select[0].Field-1].Name='YearOfDeath'));
  Check((length(Stmt.Select)=1) and (Stmt.Select[0].FunctionName='distinct'));
  Check(Stmt.Limit=20);
  NewStmt('select id from tab where id>:(1): and integerdynarraycontains ( yearofbirth , :(10): ) '+
    'order by firstname desc limit 20');
  Check(Stmt.TableName='tab');
  Check((length(Stmt.Select)=1) and (Stmt.Select[0].Field=0) and (Stmt.Select[0].Alias=''));
  Check(length(Stmt.Where)=2);
  Check(Stmt.Where[0].Field=0);
  Check(Stmt.Where[0].Operator=opGreaterThan);
  Check(Stmt.Where[0].ValueInteger=1);
  Check(Props.Fields.List[Stmt.Where[1].Field-1].Name='YearOfBirth');
  Check(Stmt.Where[1].FunctionName='INTEGERDYNARRAYCONTAINS');
  Check(Stmt.Where[1].ValueInteger=10);
  Check(Stmt.Where[1].Operator=opContains);
  Check((length(Stmt.OrderByField)=1)and(Props.Fields.List[Stmt.OrderByField[0]-1].Name='FirstName'));
  Check(Stmt.OrderByDesc);
  Check(Stmt.Limit=20);
  NewStmt('select max(yearofdeath) as maxYOD from tab where yearofbirth > :(1000):');
  Check(Stmt.TableName='tab');
  Check((length(Stmt.Select)=1) and
    (Props.Fields.List[Stmt.Select[0].Field-1].Name='YearOfDeath') and
    (Stmt.Select[0].Alias='maxYOD') and (Stmt.Select[0].ToBeAdded=0));
  Check(length(Stmt.Where)=1);
  Check(Props.Fields.List[Stmt.Where[0].Field-1].Name='YearOfBirth');
  Check(Stmt.Where[0].Operator=opGreaterThan);
  Check(Stmt.Where[0].ValueInteger=1000);
  Check((length(Stmt.Select)=1) and
    (Props.Fields.List[Stmt.Select[0].Field-1].Name='YearOfDeath'));
  Check((length(Stmt.Select)=1) and (Stmt.Select[0].FunctionName='max'));
  Check(Stmt.Limit=0);
  NewStmt('select max(yearofdeath)+115 as maxYOD from tab where yearofbirth > :(1000):');
  Check(Stmt.TableName='tab');
  Check((length(Stmt.Select)=1) and
    (Props.Fields.List[Stmt.Select[0].Field-1].Name='YearOfDeath') and
    (Stmt.Select[0].Alias='maxYOD') and (Stmt.Select[0].ToBeAdded=115));
  Check(length(Stmt.Where)=1);
  Check(Props.Fields.List[Stmt.Where[0].Field-1].Name='YearOfBirth');
  Check(Stmt.Where[0].Operator=opGreaterThan);
  Check(Stmt.Where[0].ValueInteger=1000);
  Check((length(Stmt.Select)=1) and
    (Props.Fields.List[Stmt.Select[0].Field-1].Name='YearOfDeath'));
  Check((length(Stmt.Select)=1) and (Stmt.Select[0].FunctionName='max'));
  Check(Stmt.Limit=0);
  Stmt.Free;
end;

procedure TTestLowLevelTypes._TSynMonitorUsage;
var id: TSynMonitorUsageID;
    now,id2: TTimelog;
    n: TTimeLogBits absolute now;
    i: integer;
    s,s2: RawUTF8;
begin
  id.Value := 0;
  now := TimeLogNowUTC and not pred(1 shl 12); // truncate to hour resolution
  id.FromTimeLog(now);
  s := n.Text(true);
  id2 := id.ToTimeLog;
  s2 := id.Text(true);
  Check(id2=now);
  Check(s2=s);
  for i := 1 to 200 do begin
    n.From(n.ToDateTime+Random*50);
    now := now and not pred(1 shl 12);
    s := n.Text(true);
    id.SetTime(mugYear,n.Year);
    id.SetTime(mugMonth,n.Month);
    id.SetTime(mugDay,n.Day);
    id.SetTime(mugHour,n.Hour);
    id2 := id.ToTimeLog;
    s2 := id.Text(true);
    Check(id2=now);
    Check(s2=s);
    Check(id.Granularity=mugHour);
    id.From(n.Year,n.Month,n.Day);
    Check(id.Granularity=mugDay);
    id.From(n.Year,n.Month);
    Check(id.Granularity=mugMonth);
    id.From(n.Year);
    Check(id.Granularity=mugYear);
  end;
end;


{ TTestBasicClasses }

procedure TTestBasicClasses._TSQLModel;
var M: TSQLModel;
    U: TSQLRestServerURI;
begin
  M := TSQLModel.Create([TSQLRecordTest]);
  try
    Check(M['Test']<>nil);
    Check(M['Test2']=nil);
    Check(M['TEST']=TSQLRecordTest);
  finally
    M.Free;
  end;
  Check(U.URI='');
  U.URI := 'addr:port/root';
  Check(U.Address='addr');
  Check(U.Port='port');
  Check(U.Root='root');
  U.URI := 'addr:port';
  Check(U.Address='addr');
  Check(U.Port='port');
  Check(U.Root='');
  U.URI := 'addr/root';
  Check(U.Address='addr');
  Check(U.Port='');
  Check(U.Root='root');
  U.URI := 'addr';
  Check(U.Address='addr');
  Check(U.Port='');
  Check(U.Root='');
end;

procedure TTestBasicClasses._TSQLRestServerFullMemory;
var Model: TSQLModel;
    Server: TSQLRestServerFullMemory;
    {$ifdef MSWINDOWS}
    Client: TSQLRestClientURIMessage;
    {$else}
    // Under Linux, no windows message loop : URIDll will be used !
    Client: TSQLRestClientURIDll;
    {$endif}
    R: TSQLRecordTest;
    Batch: TSQLRestBatch;
    IDs: TIDDynArray;
    i,j,n: integer;
    dummy, s: RawUTF8;
{$ifndef NOVARIANTS}
procedure CheckVariantWith(const V: Variant; const i: Integer; const offset: integer=0);
begin
  Check(V.ID=i);
  Check(V.Int=i);
  Check(V.Test=Int32ToUtf8(i));
  Check(V.Ansi=V.Test);
  Check(V.Unicode=V.Test);
  Check(V.ValFloat=i*2.5);
  Check(V.ValWord=i+offset);
  Check(V.ValDate=i+30000);
  Check(V.Data=V.Test);
  Check(DocVariantType.IsOfType(V.ValVariant));
  Check(VariantSaveJson(V.ValVariant)='{"id":'+V.Test+'}');
end;
var readonly: boolean;
    docs: variant;
    T: TSQLTable;
{$endif}
{$ifdef ISDELPHI2010}
var List: TObjectList<TSQLRecordTest>;
{$endif}
begin
  Model := TSQLModel.Create([TSQLRecordTest]);
  try
    DeleteFile('fullmem.data');
    Check(not FileExists('fullmem.data'));
    Server := TSQLRestServerFullMemory.Create(Model,'fullmem.data',true,true);
    try
      Server.CreateMissingTables;
      {$ifdef MSWINDOWS}
      Check(Server.ExportServerMessage('fullmem'));
      Client := TSQLRestClientURIMessage.Create(Model,'fullmem','fullmemclient',1000);
      {$else}
      Server.ExportServer; // initialize URIRequest() with the aStatic database
      USEFASTMM4ALLOC := true; // getmem() is 2x faster than GlobalAlloc()
      Client := TSQLRestClientURIDll.Create(Model,URIRequest);
      {$endif}
      try
        Client.ForceBlobTransfert := true;
        Check(Client.ServerTimestampSynchronize);
        Check(Client.SetUser('User','synopse'));
        Client.TransactionBegin(TSQLRecordTest);
        R := TSQLRecordTest.Create;
        try
          for i := 1 to 99 do begin
            R.FillWith(i);
            Check(Client.Add(R,true)=i);
          end;
          Client.Commit;
          Check(Client.BatchStart(TSQLRecordTest,1000));
          for i := 100 to 9999 do begin
            R.FillWith(i);
            Check(Client.BatchAdd(R,true,false,ALL_FIELDS)=i-100);
          end;
          Check(Client.BatchSend(IDs)=HTTP_SUCCESS);
          Check(Length(IDs)=9900);
          Check(not FileExists('fullmem.data'));
          Check(Client.CallBackPut('Flush','',dummy)=HTTP_SUCCESS);
          Check(FileExists('fullmem.data'));
          Check(Client.Retrieve(200,R));
          R.CheckWith(self,200);
        finally
          R.Free;
        end;
      finally
        Client.Free;
      end;
    finally
      Server.Free;
    end;
    Server := TSQLRestServerFullMemory.Create(Model,'fullmem.data',true,true);
    try
      Server.CreateMissingTables;
      {$ifdef MSWINDOWS}
      Check(Server.ExportServerMessage('fullmem'));
      Client := TSQLRestClientURIMessage.Create(Model,'fullmem','fullmemclient',1000);
      {$else}
      Server.ExportServer; // initialize URIRequest() with the aStatic database
      USEFASTMM4ALLOC := true; // getmem() is 2x faster than GlobalAlloc()
      Client := TSQLRestClientURIDll.Create(Model,URIRequest);
      {$endif}
      try
        Client.ForceBlobTransfert := true;
        Check(Client.ServerTimestampSynchronize);
        Check(Client.SetUser('User','synopse'));
        R := TSQLRecordTest.CreateAndFillPrepare(Client,'','*');
        try
          Check((R.FillTable<>nil) and (R.FillTable.RowCount=9999));
          i := 0;
          while R.FillOne do begin
            inc(i);
            R.CheckWith(self,i);
          end;
          Check(i=9999);
          for i := 1 to 9999 do begin
            Check(R.FillRow(i));
            R.CheckWith(self,i);
          end;
          for i := 1 to 19999 do begin
            j := Random32(9999)+1;
            Check(R.FillRow(j));
            R.CheckWith(self,j);
          end;
        finally
          R.Free;
        end;
        {$ifdef ISDELPHI2010}
        List := Client.RetrieveList<TSQLRecordTest>('*');
        if not CheckFailed(List<>nil) then
          try
            Check(List.Count=9999);
            for R in List do
              R.CheckWith(self,R.IDValue);
            for i := 0 to List.Count-1 do begin
              R := List[i];
              R.CheckWith(self,i+1);
            end;
          finally
            List.Free;
          end;
        {$endif}
        {$ifndef NOVARIANTS}
        for readonly := false to true do begin
          T := Client.MultiFieldValues(TSQLRecordTest,'*');
          if CheckFailed(T<>nil) then
            Continue;
          Check(T.RowCount=9999);
          T.ToDocVariant(docs,readonly);
          with DocVariantData(docs)^ do
            for j := 0 to Count-1 do
              CheckVariantWith(Values[j],j+1);
          T.Free;
        end;
        dummy := TSynMustache.Parse(
          '{{#items}}'#13#10'{{Int}}={{Test}}'#13#10'{{/items}}').Render(
          Client.RetrieveDocVariantArray(TSQLRecordTest,'items','Int,Test'));
        check(IdemPChar(pointer(dummy),'1=1'#$D#$A'2=2'#$D#$A'3=3'#$D#$A'4=4'));
        check(Hash32(dummy)=$BC89CA72);
        {$endif NOVARIANTS}
        Check(Client.UpdateField(TSQLRecordTest,100,'ValWord',[100+10]),
          'update one field of a given record');
        R := TSQLRecordTest.Create(Client,100);
        try
          R.CheckWith(self,100,10);
        finally
          R.Free;
        end;
        s := Client.OneFieldValues(TSQLRecordTest,'Test','ValWord=:(110):');
        Check(s='100,110');
        Check(Client.UpdateField(TSQLRecordTest,100,'ValWord',[100]));
        R := TSQLRecordTest.Create(Client,100);
        try
          R.CheckWith(self,100);
        finally
          R.Free;
        end;
        s := Client.OneFieldValues(TSQLRecordTest,'Test',FormatUTF8('ValWord=?',[],[110]));
        Check(s='110');
        Check(Client.UpdateField(TSQLRecordTest,'Unicode',['110'],'ValWord',[120]),
          'update one field of a given record');
        R := TSQLRecordTest.Create(Client,110);
        try
          R.CheckWith(self,110,10);
          Batch := TSQLRestBatch.Create(Server,TSQLRecordTest,30);
          try
            for i := 10000 to 10099 do begin
              R.FillWith(i);
              Check(Batch.Add(R,true,false,ALL_FIELDS)=i-10000);
            end;
            Check(Server.BatchSend(Batch,IDs)=HTTP_SUCCESS);
          finally
            Batch.Free;
          end;
        finally
          R.Free;
        end;
        Check(Length(IDs)=100);
        R := TSQLRecordTest.CreateAndFillPrepare(Server,'','*');
        try
          i := 0;
          while R.FillOne do begin
            inc(i);
            if i=110 then
              R.CheckWith(self,i,10) else
              R.CheckWith(self,i);
            {$ifdef NOVARIANTS} // FillPrepare([200,300]) below not available
            if (i=200) or (i=300) then begin
              R.FillWith(R.ID+10);
              Check(Client.Update(R,'ValWord,ValDate'),'update only 2 fields');
            end;
            {$endif}
          end;
          Check(i=10099);
        finally
          R.Free;
        end;
        {$ifndef NOVARIANTS} // SELECT .. IN ... is implemented via a TDocVariant
        R := TSQLRecordTest.CreateAndFillPrepare(Client,[200,300],'ValWord,ValDate,ID');
        try
          i := 0;
          while R.FillOne do begin
            inc(i);
            Check(R.ID>=200);
            R.FillWith(R.ID+10);
            Check(Client.Update(R,'ValWord,ValDate'),'update only 2 fields');
          end;
          Check(i=2);
        finally
          R.Free;
        end;
        {$endif}
        n := 20000;
        R := TSQLRecordTest.create;
        try
          for i := 10100 to n do begin
            R.FillWith(i);
            Check(Server.AddWithBlobs(R,false)=i);
          end;
        finally
          R.Free;
        end;
        CheckEqual(Server.TableRowCount(TSQLRecordTest),n);
        for i := 1 to n do
          if i and 511=0 then begin
            Check(Server.Delete(TSQLRecordTest,i));
            dec(n);
          end;
        CheckEqual(Server.TableRowCount(TSQLRecordTest),n);
        for i := 1 to n do
          Check(Server.MemberExists(TSQLRecordTest,i)=(i and 511<>0));
        R := TSQLRecordTest.CreateAndFillPrepare(Server,'','*');
        try
          i := 0;
          while R.FillOne do begin
            inc(i);
            if i and 511=0 then
              inc(i);
            if i=110 then
              R.CheckWith(self,i,10) else
            if (i=200) or (i=300) then begin
              Check(R.Int=i);
              Check(R.Test=Int32ToUtf8(i));
              Check(R.ValFloat=i*2.5);
              Check(R.ValWord=i+10);
              Check(R.ValDate=i+30010);
            end else
              R.CheckWith(self,i);
          end;
          Check(i=20000);
        finally
          R.Free;
        end;
      finally
        Client.Free;
      end;
    finally
      Server.Free;
    end;
  finally
    Model.Free;
  end;
end;

procedure TTestBasicClasses._TSQLRecord;
var i: integer;
    P: PPropInfo;
    s,s1,s2: RawUTF8;
    M: TSQLModel;
    T,T2: TSQLRecordTest;
{$ifndef LVCL}
    s3: RawUTF8;
    bin: RawByteString;
    valid: boolean;
{$endif}
{$ifndef NOVARIANTS}
    obj: Variant;
{$endif}
begin
  Check(isSelect('select * from toto'));
  Check(isSelect(' select * from toto'));
  Check(isSelect('vacuum'));
  Check(isSelect(' vacuum'));
  Check(isSelect('pragma'));
  Check(isSelect(' pragma'));
  Check(isSelect('with recursive cnt(x) as (values(1) union all '+
    'select x+1 from cnt where x<1000000) select x from cnt'));
  Check(not isSelect('update toto'));
  Check(not isSelect(' update toto'));
  Check(not isSelect('insert into toto'));
  Check(not isSelect(' insert into toto'));
  Check(not isSelect('delete from toto'));
  Check(not isSelect(' delete from toto'));
  Check(not isSelect('with recursive cnt(x) as (values(1) union all '+
    'select x+1 from cnt where x<1000000) insert into toto select x from cnt'));
  Check(GetTableNameFromSQLSelect('select a,b  from  titi',false)='titi');
  Check(GetTableNameFromSQLSelect('select a,b  from  titi limit 10',false)='titi');
  Check(GetTableNameFromSQLSelect('select a,b  from  titi,tutu',false)='titi');
  Check(GetTableNameFromSQLSelect('select a,b  from  titi,tutu order by a',false)='titi');
  Check(GetTableNameFromSQLSelect('select a,b  from  titi,tutu',true)='');
  Check(RawUTF8ArrayToCSV(GetTableNamesFromSQLSelect(
    'select a,b  from  titi where id=2'))='titi');
  Check(RawUTF8ArrayToCSV(GetTableNamesFromSQLSelect(
    'select a,b  from  titi,tutu'))='titi,tutu');
  Check(RawUTF8ArrayToCSV(GetTableNamesFromSQLSelect(
    'select a,b  from  titi, tutu ,  tata where a=2'))='titi,tutu,tata');
  T := TSQLRecordTest.Create;
  M := TSQLModel.Create([TSQLRecordTest]);
  for i := 0 to InternalClassPropInfo(TSQLRecordTest,P)-1 do begin
    Check(TSQLRecordTest.RecordProps.Fields.IndexByName(RawUTF8(P^.Name))=i);
    Check(T.RecordProps.Fields.ByRawUTF8Name(RawUTF8(P^.Name))<>nil);
    P := P^.Next;
  end;
  s := TSQLRecordTest.GetSQLCreate(M);
  Check(s='CREATE TABLE Test(ID INTEGER PRIMARY KEY AUTOINCREMENT, Int INTEGER, '+
    'Test TEXT COLLATE SYSTEMNOCASE, Unicode TEXT COLLATE SYSTEMNOCASE, '+
    'Ansi TEXT COLLATE NOCASE, ValFloat FLOAT, ValWord INTEGER, '+
    'ValDate TEXT COLLATE ISO8601, Next INTEGER, Data BLOB'+
    {$ifndef NOVARIANTS}', ValVariant TEXT COLLATE BINARY'+{$endif}');');
  s := TSQLRecordTest.RecordProps.SQLAddField(0);
  Check(s='ALTER TABLE Test ADD COLUMN Int INTEGER; ');
  s := TSQLRecordTest.RecordProps.SQLAddField(1000);
  Check(s='');
  T2 := TSQLRecordTest.Create;
  try
    Check(T.RecordProps.SQLTableName='Test');
    Check(T.SQLTableName='Test');
    Check(GetCaptionFromClass(T.RecordClass)='Record test');
    s := T.GetSQLSet;
    Check(s='Int=0, Test='''', Unicode='''', Ansi='''', ValFloat=0, ValWord=0, '+
      'ValDate='''', Next=0'{$ifndef NOVARIANTS}+', ValVariant=null'{$endif});
    s := T.GetSQLValues;
    Check(s='Int,Test,Unicode,Ansi,ValFloat,ValWord,ValDate,Next'+
      {$ifndef NOVARIANTS}',ValVariant'+{$endif}
      ' VALUES (0,'''','''','''',0,0,'''',0'+{$ifndef NOVARIANTS}',null'+{$endif}')');
{$ifndef LVCL}
    s := ObjectToJSON(T);
    Check(s='{"ID":0,"Int":0,"Test":"","Unicode":"","Ansi":"","ValFloat":0,'+
      '"ValWord":0,"ValDate":"","Next":0,"Data":"","ValVariant":null}');
{$endif}
    T.ValDate := 39882.888612; // a fixed date and time
    T.Ansi := 'abcde6ef90';
    T.fAnsi[6] := #$E9;
    T.fAnsi[9] := #$E0;
    T.fAnsi[10] := #$E9;
    T.Test := WinAnsiToUTF8(T.Ansi);
    T.Unicode := Utf8DecodeToRawUnicode(T.fTest);
    Check(RawUnicodeToWinAnsi(T.fUnicode)=T.fAnsi);
    // the same string is stored with some Delphi types, but will remain
    // identical in UTF-8 SQL, as all will be converted into UTF-8
    T.Valfloat := 3.141592653;
    T.ValWord := 1203;
    {$ifndef NOVARIANTS}
    T.ValVariant := 3.1416; // will be stored as TEXT, i.e. '3.1416'
    {$endif}
    s := T.GetSQLSet;
    Check(s='Int=0, Test='''+T.Test+''', Unicode='''+T.Test+
      ''', Ansi='''+T.Test+''', ValFloat=3.141592653, ValWord=1203, '+
      'ValDate=''2009-03-10T21:19:36'', Next=0'{$ifndef NOVARIANTS}+
      ', ValVariant=''3.1416'''{$endif});
    s := T.GetSQLValues;
    {$ifndef NOVARIANTS}
    Check(Hash32(s)=$2D344A5E);
    {$else}
    Check(Hash32(s)=$6DE61E87);
    {$endif}
    s := T.GetJSONValues(false,true,soSelect);
    s1 := '{"fieldCount":'+{$ifndef NOVARIANTS}'10'{$else}'9'{$endif}+
      ',"values":["RowID","Int","Test","Unicode","Ansi",'+
      '"ValFloat","ValWord","ValDate","Next"'{$ifndef NOVARIANTS}+
      ',"ValVariant"'{$endif}+',0,0,"'+T.Test+'","'+
      T.Test+'","'+T.Test+'",3.141592653,1203,"2009-03-10T21:19:36",0'
      {$ifndef NOVARIANTS}+',3.1416'{$endif}+']}';
    CheckEqual(s,s1);
    Check(T.SameValues(T));
    Check(not T.SameValues(T2));
    T2.FillFrom(s);
    Check(T.SameValues(T2));
    Check(T2.GetJSONValues(false,true,soSelect)=s);
    T.fID := 10;
    s := T.GetJSONValues(true,true,soSelect);
    {$ifdef VERBOSE}writeln(s);{$endif}
    T2.ClearProperties;
    Check(not T.SameValues(T2));
    T2.FillFrom(s);
    Check(T.SameValues(T2));
    Check(T2.GetJSONValues(true,true,soSelect)=s);
{$ifndef NOVARIANTS}
    obj := T.GetSimpleFieldsAsDocVariant;
    s3 := VariantSaveJSON(obj);
    Check(s3=s);
{$endif}
{$ifndef LVCL}
    s := ObjectToJSON(T);
    Check(s='{"ID":10,"Int":0,"Test":"'+T.Test+'","Unicode":"'+T.Test+
      '","Ansi":"'+T.Test+'","ValFloat":3.141592653,"ValWord":1203,'+
      '"ValDate":"2009-03-10T21:19:36","Next":0,"Data":""'{$ifndef NOVARIANTS}
        +',"ValVariant":3.1416'{$endif}+'}');
    T2.ClearProperties;
    Check(not T.SameValues(T2));
    Check(JSONToObject(T2,pointer(s),valid)=nil);
    Check(valid);
    Check(T.SameValues(T2));
{$endif}
    T.Int := 1234567890123456;
    s := T.GetJSONValues(true,true,soSelect);
    Check(s='{"RowID":10,"Int":1234567890123456,"Test":"'+T.Test+'","Unicode":"'+T.Test+
      '","Ansi":"'+T.Test+'","ValFloat":3.141592653,"ValWord":1203,'+
      '"ValDate":"2009-03-10T21:19:36","Next":0'+
      {$ifndef NOVARIANTS}',"ValVariant":3.1416'+{$endif}'}');
    T2.ClearProperties;
    Check(not T.SameValues(T2));
    T2.FillFrom(s);
    Check(T.SameValues(T2));
    Check(T2.GetJSONValues(true,true,soSelect)=s);
    Check(T2.Int=1234567890123456);
    {$ifndef NOVARIANTS}
    T.ValVariant := UTF8ToSynUnicode(T.Test);
    {$endif}
    s := T.GetJSONValues(true,true,soSelect);
    s1 := '{"RowID":10,"Int":1234567890123456,"Test":"'+T.Test+'","Unicode":"'+T.Test+
      '","Ansi":"'+T.Test+'","ValFloat":3.141592653,"ValWord":1203,'+
      '"ValDate":"2009-03-10T21:19:36","Next":0';
    Check(s=s1{$ifndef NOVARIANTS}+',"ValVariant":"'+T.Test+'"'{$endif}+'}');
    s := T.GetSQLSet;
    s2 := 'Int=1234567890123456, Test='''+T.Test+''', Unicode='''+T.Test+
      ''', Ansi='''+T.Test+''', ValFloat=3.141592653, ValWord=1203, '+
      'ValDate=''2009-03-10T21:19:36'', Next=0';
    Check(s=s2{$ifndef NOVARIANTS}+', ValVariant='''+T.Test+''''{$endif});
    {$ifndef NOVARIANTS}
    T.ValVariant := _JSON('{name:"John",int:1234}');
    s := T.GetSQLSet;
    Check(s=s2+', ValVariant=''{"name":"John","int":1234}''','JSON object as text');
    s := T.GetJSONValues(true,true,soSelect);
    Check(s=s1+',"ValVariant":{"name":"John","int":1234}}');
    T2.ClearProperties;
    Check(not T.SameValues(T2));
    T2.FillFrom(s);
    {$ifdef MSWINDOWS}
    s := VariantSaveMongoJSON(T2.ValVariant,modMongoStrict);
    Check(s=VariantSaveMongoJSON(T.ValVariant,modMongoStrict));
    Check(T.SameValues(T2));
    {$endif}
    s := T.GetJSONValues(true,true,soSelect);
    Check(T2.GetJSONValues(true,true,soSelect)=s);
    s := GetJSONObjectAsSQL(s,true,false,0,true);
    Check(s=StringReplaceAll(s2,', ',',')+',ValVariant=''{"name":"John","int":1234}''');
    s := ObjectToJSON(T);
    delete(s1,3,3); // "RowID":10 -> "ID":10
    Check(s=s1+',"Data":"","ValVariant":{"name":"John","int":1234}}');
    bin := T.GetBinary;
    T2.ClearProperties;
    Check(T2.SetBinary(pointer(bin),PAnsiChar(pointer(bin))+length(bin)));
    Check(T.SameValues(T2));
    T2.ClearProperties;
    Check(T2.SetBinary(bin));
    Check(T.SameValues(T2));
    bin := VariantSave(T.ValVariant);
    Check(bin<>'');
    Check(VariantLoad(T2.fVariant,pointer(bin),@JSON_OPTIONS[true])<>nil);
    {$ifdef MSWINDOWS}
    Check(VariantSaveMongoJSON(T2.fVariant,modMongoStrict)='{"name":"John","int":1234}');
    {$endif}
    {$endif}
  finally
    M.Free;
    T2.Free;
    T.Free;
  end;
end;

procedure TTestBasicClasses._TSQLRecordSigned;
var R: TSQLRecordSigned;
    i: integer;
    Content: RawByteString;
begin
  R := TSQLRecordSigned.Create;
  try
    for i := 1 to 50 do begin
      Content := RandomString(5*Random(1000));
      Check(R.SetAndSignContent('User',Content));
      Check(R.SignedBy='User');
      Check(R.CheckSignature(Content));
      Content := Content+'?'; // invalidate content
      Check(not R.CheckSignature(Content));
      R.UnSign;
    end;
  finally
    R.Free;
  end;
end;

{$endif DELPHI5OROLDER}

{$ifdef UNICODE}
{$WARNINGS ON} // don't care about implicit string cast in tests
{$endif}

{ TTestCompression }

procedure TTestCompression.Setup;
begin
  Data := StringFromFile(ExeVersion.ProgramFileName);
end;

procedure TTestCompression.CleanUp;
begin
  FreeAndNil(M);
end;

const
  // uses a const table instead of a dynamic array, for better regression test
  crc32tab: array[byte] of cardinal =
    ($00000000, $77073096, $EE0E612C, $990951BA,
    $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
    $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
    $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
    $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116,
    $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

    $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
    $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
    $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
    $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
    $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086,
    $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
    $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
    $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
    $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
    $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
    $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
    $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
    $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

    $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
    $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
    $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
    $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
    $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
    $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

function UpdateCrc32(aCRC32: cardinal; inBuf: pointer; inLen: integer) : cardinal;
var i: integer;
begin // slowest reference version
  result := not aCRC32;
  for i := 1 to inLen do begin
    result := crc32tab[(result xor pByte(inBuf)^) and $ff] xor (result shr 8);
    inc(PByte(inBuf));
  end;
  result := not result;
end;

procedure TTestCompression.GZipFormat;
var Z: TSynZipCompressor;
    L,n: integer;
    P: PAnsiChar;
    crc2: Cardinal;
    st: TRawByteStringStream;
    s,tmp: RawByteString;
    gzr: TGZRead;
begin
  Check(crc32(0,@crc32tab,5)=$DF4EC16C,'crc32');
  Check(UpdateCrc32(0,@crc32tab,5)=$DF4EC16C,'crc32');
  Check(crc32(0,@crc32tab,1024)=$6FCF9E13,'crc32');
  Check(UpdateCrc32(0,@crc32tab,1024)=$6FCF9E13);
  Check(crc32(0,@crc32tab,1024-5)=$70965738,'crc32');
  Check(UpdateCrc32(0,@crc32tab,1024-5)=$70965738);
  Check(crc32(0,pointer(PtrInt(@crc32tab)+1),2)=$41D912FF,'crc32');
  Check(UpdateCrc32(0,pointer(PtrInt(@crc32tab)+1),2)=$41D912FF);
  Check(crc32(0,pointer(PtrInt(@crc32tab)+3),1024-5)=$E5FAEC6C,'crc32');
  Check(UpdateCrc32(0,pointer(PtrInt(@crc32tab)+3),1024-5)=$E5FAEC6C,'crc32');
  M := SynCommons.THeapMemoryStream.Create;
  Z := TSynZipCompressor.Create(M,6,szcfGZ);
  L := length(Data);
  P := Pointer(Data);
  crc0 := 0;
  crc2 := 0;
  while L<>0 do begin
    if L>1000 then
      n := 1000 else
      n := L;
    Z.Write(P^,n); // compress by little chunks to test streaming
    crc0 := crc32(crc0,P,n);
    crc2 := UpdateCrc32(crc2,P,n);
    inc(P,n);
    dec(L,n);
  end;
  Check(crc0=Z.CRC,'crc32');
  Check(crc2=crc0,'crc32');
  Z.Free;
  Check(GZRead(M.Memory,M.Position)=Data,'gzread');
  crc1 := crc32(0,M.Memory,M.Position);
  s := Data;
  Check(CompressGZip(s,true)='gzip');
  Check(CompressGZip(s,false)='gzip');
  Check(s=Data,'compressGZip');
  Check(gzr.Init(M.Memory,M.Position),'TGZRead');
  Check(gzr.uncomplen32=Cardinal(length(data)));
  Check(gzr.crc32=crc0);
  Check(gzr.ToMem=data,'ToMem');
  st := TRawByteStringStream.Create;
  try
    Check(gzr.ToStream(st),'ToStream');
    s := st.DataString;
    Check(s=Data,'ToStream?');
  finally
    st.Free;
  end;
  SetLength(tmp,gzr.uncomplen32 div 5);
  Check(gzr.ZStreamStart(pointer(tmp),length(tmp)),'ZStreamStart');
  s := '';
  repeat
    n := gzr.ZStreamNext;
    if n=0 then
      break;
    s := s+copy(tmp,1,n);
  until false;
  check(gzr.ZStreamDone,'ZStreamDone');
  Check(gzr.uncomplen32=Cardinal(length(s)));
  check(s=Data);
  s := Data;
  Check(CompressDeflate(s,true)='deflate');
  Check(CompressDeflate(s,false)='deflate');
  Check(s=Data,'CompressDeflate');
end;

procedure TTestCompression.InMemoryCompression;
var comp: Integer;
    tmp: RawByteString;
begin
  Check(CRC32string('TestCRC32')=$2CB8CDF3);
  tmp := RawByteString(Ident);
  for comp := 0 to 9 do
    Check(UnCompressString(CompressString(tmp,False,comp))=tmp);
  Check(UnCompressString(CompressString(Data,False,6))=Data);
end;


procedure TTestCompression.ZipFormat;
var FN,FN2: TFileName;
    ExeName: string;
    S: TRawByteStringStream;
procedure Test(Z: TZipRead; aCount: integer);
var i: integer;
    tmp: RawByteString;
    tmpFN: TFileName;
    info: TFileInfo;
begin
  with Z do
  try
    Check(Count=aCount,'count');
    i := NameToIndex('REP1\ONE.exe');
    Check(i=0,'0');
    FillcharFast(info,sizeof(info),0);
    Check(RetrieveFileInfo(i,info),'info');
    Check(integer(info.zfullSize)=length(Data),'siz');
    Check(info.zcrc32=crc0,'crc0');
    Check(UnZip(i)=Data,'unzip1');
    i := NameToIndex('REp2\ident.gz');
    Check(i=1,'unzip2');
    Check(Entry[i].infoLocal^.zcrc32=crc1,'crc1a');
    tmp := UnZip(i);
    Check(tmp<>'','unzip3');
    Check(crc32(0,pointer(tmp),length(tmp))=crc1,'crc1b');
    i := NameToIndex(ExeName);
    Check(i=2,'unzip4');
    Check(UnZip(i)=Data,'unzip6');
    Check(Entry[i].infoLocal^.zcrc32=info.zcrc32,'crc32');
    i := NameToIndex('REp2\ident2.gz');
    Check(i=3,'unzip5');
    Check(Entry[i].infoLocal^.zcrc32=crc1,'crc1c');
    tmp := UnZip(i);
    Check(tmp<>'','unzip7');
    Check(crc32(0,pointer(tmp),length(tmp))=crc1,'crc1d');
    if aCount=4 then
      Exit;
    i := NameToIndex('REP1\twO.exe');
    Check(i=4,'unzip8');
    Check(UnZip(i)=Data,'unzip9');
    tmpFN := 'TestSQL3zipformat.tmp';
    Check(UnZip('REP1\one.exe',tmpFN,true),'unzipa');
    Check(StringFromFile(tmpFN)=Data,'unzipb');
    Check(DeleteFile(tmpFN),'unzipc');
  finally
    Free;
  end;
end;
procedure Prepare(Z: TZipWriteAbstract);
begin
  with Z do
  try
    AddDeflated('rep1\one.exe',pointer(Data),length(Data));
    Check(Count=1,'cnt1');
    AddDeflated('rep2\ident.gz',M.Memory,M.Position);
    Check(Count=2,'cnt2');
    if Z is TZipWrite then
      TZipWrite(Z).AddDeflated(ExeVersion.ProgramFileName) else
      Z.AddDeflated(ExeName,pointer(Data),length(Data));
    Check(Count=3,'cnt3');
    AddStored('rep2\ident2.gz',M.Memory,M.Position);
    Check(Count=4,'cnt4');
  finally
    Free;
  end;
end;
{$ifdef MSWINDOWS} // PasZip.TZipRead uses memory mapped API
procedure TestPasZipRead(const FN: TFileName; Count: integer);
var pasZR: PasZip.TZipRead;
begin
  pasZR := PasZip.TZipRead.Create(FN);
  try
    Check(pasZR.Count=Count,'paszip1');
    Check(pasZR.NameToIndex('rep1\ONE.exe')=0,'paszip2');
    Check(pasZR.UnZip(0)=data,'paszip3');
  finally
    pasZR.Free;
  end;
end;
var pasZW: PasZip.TZipWrite;
{$endif MSWINDOWS}
var i: integer;
begin
  ExeName := ExtractFileName(ExeVersion.ProgramFileName);
  FN := ChangeFileExt(ExeVersion.ProgramFileName,'.zip');
  Prepare(TZipWrite.Create(FN));
  Test(TZipRead.Create(FN),4);
  S := TRawByteStringStream.Create;
  try
    Prepare(TZipWriteToStream.Create(S));
    Test(TZipRead.Create(pointer(S.DataString),length(S.DataString)),4);
  finally
    S.Free;
  end;
  with TZipWrite.CreateFrom(FN) do
  try
    Check(Count=4,'two4');
    AddDeflated('rep1\two.exe',pointer(Data),length(Data));
    Check(Count=5,'two5');
  finally
    Free;
  end;
  Test(TZipRead.Create(FN),5);
  {$ifdef MSWINDOWS}
  TestPasZipRead(FN,5);
  FN2 := ChangeFileExt(FN,'2.zip');
  pasZW := PasZip.TZipWrite.Create(FN2);
  try
    pasZW.AddDeflated('rep1\one.exe',pointer(Data),length(Data));
    Check(pasZW.Count=1,'paszipA');
    pasZW.AddDeflated('rep2\ident.gz',M.Memory,M.Position);
    Check(pasZW.Count=2,'paszipB');
    pasZW.AddDeflated(ExeVersion.ProgramFileName);
    Check(pasZW.Count=3,'paszipC');
    pasZW.AddStored('rep2\ident2.gz',M.Memory,M.Position);
    Check(pasZW.Count=4,'paszipD');
  finally
    pasZW.Free;
  end;
  TestPasZipRead(FN2,4);
  DeleteFile(FN2);
  {$endif}
  DeleteFile(FN);
  FN2 := ExeVersion.ProgramFilePath+'ddd.zip';
  with TZipWrite.Create(FN2) do
  try
    FN := ExeVersion.ProgramFilePath+'ddd';
    if not DirectoryExists(FN) then
      FN := ExeVersion.ProgramFilePath+'..'+PathDelim+'ddd';
    if DirectoryExists(FN) then begin
      AddFolder(FN,'*.pas');
      Check(Count>10);
      for i := 0 to Count-1 do
        Check(SameText(ExtractFileExt(Ansi7ToString(Entry[i].intName)),'.pas'),'ddd');
    end;
  finally
    Free;
  end;
  DeleteFile(FN2);
end;

procedure TTestCompression._SynLZO;
var s,t: AnsiString;
    i: integer;
begin
  for i := 0 to 1000 do begin
    t := RandomString(i*8);
    s := t;
    Check(CompressSynLZO(s,true)='synlzo');
    Check(CompressSynLZO(s,false)='synlzo');
    Check(s=t);
  end;
  s := Data;
  Check(CompressSynLZO(s,true)='synlzo');
  Check(CompressSynLZO(s,false)='synlzo');
  Check(s=Data);
end;

function Spaces(n: integer): RawUTF8;
begin
  SetString(result,nil,n);
  FillCharFast(pointer(result)^,n,32);
end;

function By4(pattern,n: integer): RawUTF8;
var i: integer;
begin
  SetString(result,nil,n*4);
  for i := 0 to n-1 do
    PIntegerArray(result)[i] := pattern;
end;

procedure TTestCompression._SynLZ;
var s,t,rle: RawByteString;
    i,j, complen2: integer;
    comp2,dec1: array of byte;
    {$ifdef CPUINTEL}
    comp1, dec2: array of byte;
    complen1: integer;
    {$endif CPUINTEL}
begin
  for i := 1 to 200 do begin
    s := SynLZCompress(StringOfChar(AnsiChar(i),i));
    t := SynLZDecompress(s);
    Check(t=StringOfChar(AnsiChar(i),i));
  end;
  rle := 'hello'+Spaces(10000)+'hello'+Spaces(1000)+'world';
  s := SynLZCompress(rle);
  t := SynLZDecompress(s);
  Check(t=rle);
  rle := 'hello'+by4($3031333,10000)+'hello'+by4($3031333,1000)+'world';
  s := SynLZCompress(rle);
  t := SynLZDecompress(s);
  Check(t=rle);
  for i := 0 to 1000 do begin
    s := StringOfChar(AnsiChar(' '),20);
    t := RandomTextParagraph(i, '.', s);
    SetString(s,PAnsiChar(pointer(t)),length(t)); // =UniqueString
    Check(CompressSynLZ(s,true)='synlz');
    Check(CompressSynLZ(s,false)='synlz');
    Check(s=t);
    Check(SynLZDecompress(SynLZCompress(s))=t);
    SetLength(comp2,SynLZcompressdestlen(length(s)));
    complen2 := SynLZcompress1pas(Pointer(s),length(s),pointer(comp2));
    Check(complen2<length(comp2));
    {$ifndef CPUINTEL}
    Check(@SynLZCompress1=@SynLZcompress1pas);
    Check(@SynLZDecompress1=@SynLZdecompress1pas);
    {$else}
    SetLength(comp1,SynLZcompressdestlen(length(s)));
    complen1 := SynLZcompress1(Pointer(s),length(s),pointer(comp1));
    Check(complen1<length(comp1));
    Check(complen1=complen2);
    Check(CompareMem(pointer(comp1),pointer(comp2),complen1));
    Check(SynLZdecompressdestlen(pointer(comp1))=length(s));
    Check(SynLZdecompressdestlen(pointer(comp2))=length(s));
    SetLength(dec1,Length(s));
    Check(SynLZdecompress1pas(Pointer(comp1),complen1,pointer(dec1))=length(s));
    Check(CompareMem(pointer(dec1),pointer(s),length(s)));
    SetLength(dec2,Length(s));
    Check(SynLZdecompress1(Pointer(comp2),complen2,pointer(dec2))=length(s));
    Check(CompareMem(pointer(dec1),pointer(s),length(s)));
    {$endif}
  end;
  SetLength(dec1,length(t));
  for j := 0 to length(t)-1 do begin
    FillCharFast(pointer(dec1)^,length(t),0);
    Check(SynLZdecompress1partial(pointer(comp2),complen2,Pointer(dec1),j)=j);
    Check(CompareMem(pointer(dec1),pointer(t),j));
  end;
  s := Data;
  Check(CompressSynLZ(s,true)='synlz');
  Check(Length(s)<Length(Data),'exelen');
  Check(CompressSynLZ(s,false)='synlz');
  Check(s=Data);
end;

procedure TTestCompression._TAlgoCompress;
  procedure TestAlgo(algo: TAlgoCompress);
  var s,t,s2,log: RawByteString;
      i, plain, comp: integer;
      timer: TPrecisionTimer;
      timecomp, timedecomp: Int64;
  begin
    if algo=nil then
      exit;
    for i := 1 to 50 do begin
      t := StringOfChar(AnsiChar(i),i)+t;
      s := StringOfChar(AnsiChar(i),i)+s;
      Check(algo.Decompress(algo.Compress(s))=t);
    end;
    plain := 0;
    comp := 0;
    timecomp := 0;
    timedecomp := 0;
    log := StringFromFile('bigTest.log');
    for i := 0 to 100 do begin
      if log<>'' then
        s := log else
        s := RandomTextParagraph(i*8);
      timer.Start;
      t := algo.Compress(s);
      inc(timecomp, timer.StopInMicroSec);
      timer.Start;
      s2 := algo.Decompress(t,aclNoCrcFast);
      inc(timedecomp, timer.StopInMicroSec);
      Check(s2=s, algo.ClassName);
      if (log<>'') and (s2<>s) then FileFromString(s2,'bigTest'+algo.ClassName+'.log');
      inc(plain, length(s));
      inc(comp, length(t));
      if log<>'' then
        break;
    end;
    AddConsole(format('%s %s->%s: comp %d:%dMB/s decomp %d:%dMB/s',
      [algo.ClassName, KB(plain), KB(comp),
       ((plain*Int64(1000*1000)) div timecomp)shr 20,
       ((comp*Int64(1000*1000)) div timecomp)shr 20,
       ((comp*Int64(1000*1000)) div timedecomp)shr 20,
       ((plain*Int64(1000*1000)) div timedecomp)shr 20]));
    s2 := algo.Decompress(algo.Compress(s),aclNoCrcFast);
    Check(s2=s, algo.ClassName);
    if (log<>'') and (s2<>s) then FileFromString(s2,'bigTestPartial'+algo.ClassName+'.log');
  end;
begin
  TestAlgo(AlgoSynLZ);
  Check(AlgoSynLZ.AlgoName='synlz');
  {$ifdef MSWINDOWS}
  if (Lizard=nil) and FileExists(ExeVersion.ProgramFilePath+LIZARD_LIB_NAME) then
    Lizard := TSynLizardDynamic.Create;
  {$endif}
  TestAlgo(AlgoLizard);
  TestAlgo(AlgoLizardFast);
  TestAlgo(AlgoLizardHuffman);
  {$ifndef DELPHI5OROLDER}
  TestAlgo(AlgoDeflate);
  TestAlgo(AlgoDeflateFast);
  Check(AlgoDeflateFast.AlgoName ='deflatefast');
  {$endif}
end;

{ FPC Linux x86-64 (in VM) with static linked library for a 53MB log file:
     TAlgoSynLz 53 MB->5 MB: comp 650:62MB/s decomp 90:945MB/s
     TAlgoLizard 53 MB->3.9 MB: comp 55:4MB/s decomp 139:1881MB/s
     TAlgoLizardFast 53 MB->6.8 MB: comp 695:89MB/s decomp 196:1522MB/s
     TAlgoDeflate 53 MB->4.8 MB: comp 71:6MB/s decomp 48:540MB/s
     TAlgoDeflateFast 53 MB->7 MB: comp 142:18MB/s decomp 56:428MB/s
  Delphi Win64 with external lizard1-64.dll:
     TAlgoSynLz 53 MB->5 MB: comp 667:63MB/s decomp 103:1087MB/s
     TAlgoLizard 53 MB->3.9 MB: comp 61:4MB/s decomp 169:2290MB/s
     TAlgoLizardFast 53 MB->6.8 MB: comp 690:89MB/s decomp 263:2039MB/s
     TAlgoLizardHuffman 53 MB->2 MB: comp 658:25MB/s decomp 86:2200MB/s
     TAlgoDeflate 53 MB->4.8 MB: comp 25:2MB/s decomp 19:214MB/s
     TAlgoDeflateFast 53 MB->7 MB: comp 52:6MB/s decomp 23:176MB/s
  speed difference may come from the FPC/Delphi heap manager, and/or the Linux VM
}

{ TTestCryptographicRoutines }

procedure TTestCryptographicRoutines._Adler32;
begin
  Check(Adler32SelfTest);
end;

procedure TTestCryptographicRoutines._Base64;
const
  Value64: RawUTF8 = 'SGVsbG8gL2Mn6XRhaXQg5+Ar';
var tmp: RawByteString;
    b64: RawUTF8;
    Value: WinAnsiString;
    i, L: Integer;
begin
  Value := 'Hello /c''0tait 67+';
  Value[10] := #$E9;
  Value[16] := #$E7;
  Value[17] := #$E0;
  Check(not IsBase64(Value));
  Check(SockBase64Encode(Value)=Value64);
  Check(BinToBase64(Value)=Value64);
  Check(IsBase64(Value64));
  tmp := StringFromFile(ExeVersion.ProgramFileName);
  b64 := SockBase64Encode(tmp);
  Check(IsBase64(b64));
  Check(SynCrtSock.SockBase64Decode(b64)=tmp);
  Check(BinToBase64(tmp)=b64);
  Check(Base64ToBin(b64)=tmp);
  tmp := '';
  for i := 1 to 1998 do begin
    b64 := SockBase64Encode(tmp);
    Check(SynCrtSock.SockBase64Decode(b64)=tmp);
    Check((tmp='') or IsBase64(b64));
    Check(BinToBase64(tmp)=b64);
    Check(Base64ToBin(b64)=tmp);
    if tmp<>'' then begin
      L := length(b64);
      Check(not IsBase64(pointer(b64),L-1));
      b64[Random(L)+1] := '&';
      Check(not IsBase64(pointer(b64),L));
    end;
    b64 := BinToBase64uri(tmp);
    Check(Base64uriToBin(b64)=tmp);
    tmp := tmp+AnsiChar(Random(255));
  end;
end;

{$ifdef MSWINDOWS} // same conditions as in SynCrtSock.pas
  {$ifndef DELPHI5OROLDER}
    // on Windows: enable Microsoft AES Cryptographic Provider (XP SP3 and up)
    {$define USE_PROV_RSA_AES}
  {$endif}
{$endif}

const
  TEST_AES_REF: array[0..2,0..4] of RawByteString = (
  // 128-bit
 ('aS24Jm0RHPz26P_RHqX-pGktuCZtERz89uj_0R6l_qRpLbgmbREc_Pbo_9Eepf6kB7pVFdRAcIoVhoTQPytzTQ',
  'aS24Jm0RHPz26P_RHqX-pCTLpnA2lH7fAWpovxWR8Voytqn9B_zTt6Zrt1Gjb4J5HUs6E7C9Uf4fV83SxyILCg',
  '0YRWak2ZiQj-cncKQ3atJtcclNgW9OiQPpY6mLvrfYQc_mORQygR9LFU2z2Prc8I5anMvOABB62Ei5AAWY8M0Q',
  '0YRWak2ZiQj-cncKQ3atJingGAyjpdvuFAvnZ4vDXweTPTJOFSBVUuqs9SW6vSkAyhtoFM9p-gO3IRZh227twA',
  '0YRWak2ZiQj-cncKQ3atJjjmhYzJAYmaqNOy9bCBqYa0YYLiSrlUwv9f4JqyVmPQg7w2zQjjdyHSCuYxA-coGQ'),
  // 192-bit
 ('3S2QhC78T0eesG3hiqtA2N0tkIQu_E9HnrBt4YqrQNjdLZCELvxPR56wbeGKq0DYJob7gbbvgBaFdm_Bwed4RQ',
  '3S2QhC78T0eesG3hiqtA2HNVuHHzMsrQOruEy1t6Q-AMQMszIPd_86pnqzIyzdSZut-CCacA9T5O8e8ZJKvZOQ',
  'a6wXR1K29yQvbGGkawiHN1RcFhrbtbne2w13ziEURY1Btg1oqiL-BqTGtEsu4LH5wLYcGNQJ21CR58LBtRysQg',
  'a6wXR1K29yQvbGGkawiHN4Cloz_9GlJhlEozeNI4MFjKwihToQP6_FDpDVHz21qUonhk6MZ9_-6vNvnGqbOTcg',
  'a6wXR1K29yQvbGGkawiHN7koCYngh0WS5R-rsGy5zSaC9txKnyHDavH1tkXlWZuxTjQCNHbiAIIRYK4giZDHzA'),
  // 256-bit
 ('Kw50ybT0hl8MXw1IcBFm5isOdMm09IZfDF8NSHARZuYrDnTJtPSGXwxfDUhwEWbmn9aUUA6_ZwXpKRiFMlXRiw',
  'Kw50ybT0hl8MXw1IcBFm5iV4ZAxvgHN-4j2F7ch7PWr6yHhbcp0Scqd2WDHZMRygi3thq9H3jKVo34_NPKdK1A',
  'vf-UrsBFA2NkziMn6szalnw24-wbPmG9lySgx0WLZZpfkTpw2euPIm6ZkFzjFa-lqr4yngOkvW99hPGzYEAjDw',
  'vf-UrsBFA2NkziMn6szalgQnKyYBxXxLhVI9s8D3cZkYsLsdfSUCTUY8moP2SenmHCWQWwaq_ibRCr4JngSkZQ',
  'vf-UrsBFA2NkziMn6szalimh8XYdFObdg_TwNyfX8Zy2Dk8YVPSDzzAvZ2Xx6WP_4owC6MIq7kZ2xPZ_d6vZmg'));

procedure TTestCryptographicRoutines._AES256;
var A: TAES;
    st, orig, crypted, s2, s3: RawByteString;
    Key: TSHA256Digest;
    s,b,p: TAESBlock;
    i,k,ks,m, len: integer;
    AES: TAESFull;
    PC: PAnsiChar;
    noaesni: boolean;
    Timer: array[boolean] of TPrecisionTimer;
    ValuesCrypted,ValuesOrig: array[0..1] of RawByteString;
    {$ifdef CPUINTEL}
    backup: TIntelCpuFeatures;
    {$endif CPUINTEL}
const MAX = 4096*1024;  // test 4 MB data, i.e. multi-threaded AES
      MODES: array[0..6{$ifdef USE_PROV_RSA_AES}+2{$endif}] of TAESAbstractClass =
        (TAESECB, TAESCBC, TAESCFB, TAESOFB, TAESCTR, TAESCFBCRC, TAESOFBCRC
         {$ifdef USE_PROV_RSA_AES}, TAESECB_API, TAESCBC_API{$endif});
      // TAESCFB_API and TAESOFB_API just do not work
begin
  {$ifdef CPUINTEL}
  backup := CpuFeatures;
  {$endif CPUINTEL}
  Check(AESSelfTest(true),'Internal Tables');
  SetLength(orig,MAX);
  SetLength(crypted,MAX+256);
  st := '1234essai';
  PInteger(UniqueRawUTF8(RawUTF8(st)))^ := Random(MaxInt);
  for noaesni := false to true do begin
    Timer[noaesni].Init;
    for k := 0 to 2 do begin
      ks := 128+k*64; // test keysize of 128, 192 and 256 bits
      for m := 0 to high(MODES) do begin
        st := RawUTF8(StringOfChar('x',50));
        with MODES[m].Create(pointer(st)^,ks) do
        try
          s2 := EncryptPKCS7(st,false);
          s3 := BinToBase64uri(s2);
          i := m;
          if i>=7 then // e.g. TAESECB_API -> TAESECB
            dec(i,7) else
          if i>=5 then
            dec(i,3);  // e.g. TAESCFBCRC -> TAESCFB
          CheckUTF8(TEST_AES_REF[k,i]=s3,'test vector %-%',[MODES[m],ks]);
          check(DecryptPKCS7(s2,false)=st);
        finally
          Free;
        end;
      end;
      SHA256Weak(st,Key);
      for i := 1 to 100 do begin
        move(Key,s,16);
        A.EncryptInit(Key,ks);
        A.Encrypt(s,b);
        A.Done;
        A.DecryptInit(Key,ks);
        A.Decrypt(b,p);
        A.Done;
        Check(CompareMem(@p,@s,sizeof(p)));
        Check(IsEqual(p,s));
        Timer[noaesni].Resume;
        Check(SynCrypto.AES(Key,ks,SynCrypto.AES(Key,ks,st,true),false)=st);
        Timer[noaesni].Pause;
        st := st+RandomString(4);
      end;
      PC := Pointer(orig);
      len := MAX;
      repeat // populate orig with random data
        if len>length(st) then
          i := length(st) else
          i := len;
        dec(len,i);
        move(pointer(st)^,PC^,i);
        inc(PC,i);
      until len=0;
      len := AES.EncodeDecode(Key,ks,MAX,True,nil,nil,pointer(orig),pointer(crypted));
      Check(len<MAX+256);
      Check(len>=MAX);
      len := AES.EncodeDecode(Key,ks,len,False,nil,nil,pointer(crypted),nil);
      try
        Check(len=MAX);
        Check(CompareMem(AES.outStreamCreated.Memory,pointer(orig),MAX));
        if not noaesni then begin
          for m := low(MODES) to high(MODES) do
          with MODES[m].Create(Key,ks) do
          try
            FillCharFast(pointer(@IV)^,sizeof(TAESBlock),1);
            //Timer.Start;
            for i := 0 to 256 do begin
              if i<64 then
                len := i else
              if i<128 then
                len := i*16 else
                len := i*32;
              FillCharFast(pointer(crypted)^,len,0);
              Encrypt(AES.outStreamCreated.Memory,pointer(crypted),len);
              FillCharFast(pointer(orig)^,len,0);
              Decrypt(pointer(crypted),pointer(orig),len);
              Check((len=0) or (not isZero(pointer(orig),len)) or
                isZero(AES.outStreamCreated.Memory,len));
              Check(CompareMem(AES.outStreamCreated.Memory,pointer(orig),len));
              s2 := copy(orig,1,len);
              Check(DecryptPKCS7(EncryptPKCS7(s2))=s2,IntToStr(len));
            end;
            //fRunConsole := Format('%s %s%d:%s'#10,[fRunConsole,Copy(MODES[m].ClassName,5,10),ks,Timer.Stop]);
            if m<length(ValuesCrypted) then begin
              ValuesCrypted[m] := Copy(crypted,1,len);
              ValuesOrig[m] := s2;
            end else
            if m>6 then begin
              Check(ValuesOrig[m-7]=s2);
              Check(ValuesCrypted[m-7]=Copy(crypted,1,len),MODES[m].ClassName);
            end;
          finally
            Free;
          end;
        end;
      finally
        AES.outStreamCreated.Free;
      end;
    end;
    {$ifndef CPUINTEL}
    break;
    {$else}
    if noaesni then begin
      fRunConsole := format('%s cypher 1..%d bytes with AES-NI: %s, without: %s',
        [fRunConsole,length(st),Timer[false].Stop,Timer[true].Stop]);
      Include(CpuFeatures,cfAESNI); // revert Exclude() below from previous loop
    end;
    if A.UsesAESNI then
      Exclude(CpuFeatures,cfAESNI) else
      break;
    {$endif CPUINTEL}
  end;
  {$ifdef CPUINTEL}
  CpuFeatures := backup;
  {$endif CPUINTEL}
end;

procedure TTestCryptographicRoutines._AES_GCM;
const
  hex32: THash256 = ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,
    $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f);
  buf32: THash256 = ($92,$4e,$17,$8a,$17,$fa,$1c,$a0,$e7,$48,$6f,$04,$04,$12,$3b,$91,
   $db,$f7,$97,$bb,$9d,$bd,$e9,$b1,$d4,$8d,$5c,$7f,$53,$16,$59,$12);
  tag32: array[0..15] of byte = ($10,$f9,$72,$b6,$f9,$e0,$a3,$c1,$cf,$9c,$cf,$56,$54,$3d,$ca,$79);
  K01: THash256 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  I01: array[0..11] of byte = (0,0,0,0,0,0,0,0,0,0,0,0);
  P01: array[0..15] of byte = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  C01: array[0..15] of byte = ($ce,$a7,$40,$3d,$4d,$60,$6b,$6e,$07,$4e,$c5,$d3,$ba,$f3,$9d,$18);
  T01: array[0..15] of byte = ($d0,$d1,$c8,$a7,$99,$99,$6b,$f0,$26,$5b,$98,$b5,$d4,$8a,$b9,$19);
  K02: THash256 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  I02: array[0..11] of byte = (0,0,0,0,0,0,0,0,0,0,0,0);
  H02: array[0..15] of byte = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  T02: array[0..15] of byte = ($2d,$45,$55,$2d,$85,$75,$92,$2b,$3c,$a3,$cc,$53,$84,$42,$fa,$26);
  K03: THash256 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  I03: array[0..11] of byte = (0,0,0,0,0,0,0,0,0,0,0,0);
  H03: array[0..15] of byte = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  P03: array[0..15] of byte = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  C03: array[0..15] of byte = ($ce,$a7,$40,$3d,$4d,$60,$6b,$6e,$07,$4e,$c5,$d3,$ba,$f3,$9d,$18);
  T03: array[0..15] of byte = ($ae,$9b,$17,$71,$db,$a9,$cf,$62,$b3,$9b,$e0,$17,$94,$03,$30,$b4);
  K04: THash256 = ($fb,$76,$15,$b2,$3d,$80,$89,$1d,$d4,$70,$98,$0b,$c7,$95,$84,$c8,
    $b2,$fb,$64,$ce,$60,$97,$8f,$4d,$17,$fc,$e4,$5a,$49,$e8,$30,$b7);
  I04: array[0..11] of byte = ($db,$d1,$a3,$63,$60,$24,$b7,$b4,$02,$da,$7d,$6f);
  P04: array[0..15] of byte = ($a8,$45,$34,$8e,$c8,$c5,$b5,$f1,$26,$f5,$0e,$76,$fe,$fd,$1b,$1e);
  C04: array[0..15] of byte = ($5d,$f5,$d1,$fa,$bc,$bb,$dd,$05,$15,$38,$25,$24,$44,$17,$87,$04);
  T04: array[0..15] of byte = ($4c,$43,$cc,$e5,$a5,$74,$d8,$a8,$8b,$43,$d4,$35,$3b,$d6,$0f,$9f);
  K05: THash256 = ($40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,
    $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f);
  I05: array[0..11] of byte = ($10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b);
  H05: array[0..19] of byte = (0,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13);
  P05: array[0..23] of byte = ($20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f,$30,$31,$32,$33,$34,$35,$36,$37);
  C05: array[0..23] of byte = ($59,$1b,$1f,$f2,$72,$b4,$32,$04,$86,$8f,$fc,$7b,$c7,$d5,$21,$99,$35,$26,$b6,$fa,$32,$24,$7c,$3c);
  T05: array[0..15] of byte = ($7d,$e1,$2a,$56,$70,$e5,$70,$d8,$ca,$e6,$24,$a1,$6d,$f0,$9c,$08);
  K07: THash256 = ($40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,
    $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f);
  I07: array[0..11] of byte = ($10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b);
  H07: THash256 = ($20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f,
    $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f);
  P07: array[0..255] of byte =(0,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17,
     $18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f,
     $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44,$45,$46,$47,
     $48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f,
     $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77,
     $78,$79,$7a,$7b,$7c,$7d,$7e,$7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,
     $90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,
     $a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf,
     $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,
     $d8,$d9,$da,$db,$dc,$dd,$de,$df,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,
     $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff);
  C07: array[0..255] of byte =($79,$3b,$3f,$d2,$52,$94,$12,$24,$a6,$af,$dc,$5b,$e7,$f5,$01,$b9,
     $15,$06,$96,$da,$12,$04,$5c,$1c,$60,$77,$d3,$ca,$c7,$74,$ac,$cf,$c3,$d5,$30,$d8,$48,$d6,$65,$d8,
     $1a,$49,$cb,$b5,0,$b8,$8b,$bb,$62,$4a,$e6,$1d,$16,$67,$22,$9c,$30,$2d,$c6,$ff,$0b,$b4,$d7,$0b,
     $db,$bc,$85,$66,$d6,$f5,$b1,$58,$da,$99,$a2,$ff,$2e,$01,$dd,$a6,$29,$b8,$9c,$34,$ad,$1e,$5f,$eb,
     $a7,$0e,$7a,$ae,$43,$28,$28,$9c,$36,$29,$b0,$58,$83,$50,$58,$1c,$a8,$b9,$7c,$cf,$12,$58,$fa,$3b,
     $be,$2c,$50,$26,$04,$7b,$a7,$26,$48,$96,$9c,$ff,$8b,$a1,$0a,$e3,$0e,$05,$93,$5d,$f0,$c6,$93,$74,
     $18,$92,$b7,$6f,$af,$67,$13,$3a,$bd,$2c,$f2,$03,$11,$21,$bd,$8b,$b3,$81,$27,$a4,$d2,$ee,$de,$ea,
     $13,$27,$64,$94,$f4,$02,$cd,$7c,$10,$7f,$b3,$ec,$3b,$24,$78,$48,$34,$33,$8e,$55,$43,$62,$87,$09,
     $2a,$c4,$a2,$6f,$5e,$a7,$ea,$4a,$d6,$8d,$73,$15,$16,$39,$b0,$5b,$24,$e6,$8b,$98,$16,$d1,$39,$83,
     $76,$d8,$e4,$13,$85,$94,$75,$8d,$b9,$ad,$3b,$40,$92,$59,$b2,$6d,$cf,$c0,$6e,$72,$2b,$e9,$87,$b3,
     $76,$7f,$70,$a7,$b8,$56,$b7,$74,$b1,$ba,$26,$85,$b3,$68,$09,$14,$29,$fc,$cb,$8d,$cd,$de,$09,$e4);
  T07: array[0..15] of byte = ($87,$ec,$83,$7a,$bf,$53,$28,$55,$b2,$ce,$a1,$69,$d6,$94,$3f,$cd);
  K08: THash256 = ($fb,$76,$15,$b2,$3d,$80,$89,$1d,$d4,$70,$98,$0b,$c7,$95,$84,$c8,
    $b2,$fb,$64,$ce,$60,$97,$87,$8d,$17,$fc,$e4,$5a,$49,$e8,$30,$b7);
  I08: array[0..11] of byte = ($db,$d1,$a3,$63,$60,$24,$b7,$b4,$02,$da,$7d,$6f);
  H08: array[0.. 0] of byte = ($36);
  P08: array[0.. 0] of byte = ($a9);
  C08: array[0.. 0] of byte = ($0a);
  T08: array[0..15] of byte = ($be,$98,$7d,0,$9a,$4b,$34,$9a,$a8,$0c,$b9,$c4,$eb,$c1,$e9,$f4);
  K09: THash256 = ($f8,$d4,$76,$cf,$d6,$46,$ea,$6c,$23,$84,$cb,$1c,$27,$d6,$19,$5d,
    $fe,$f1,$a9,$f3,$7b,$9c,$8d,$21,$a7,$9c,$21,$f8,$cb,$90,$d2,$89);
  I09: array[0..11] of byte = ($db,$d1,$a3,$63,$60,$24,$b7,$b4,$02,$da,$7d,$6f);
  H09: array[0..19] of byte = ($7b,$d8,$59,$a2,$47,$96,$1a,$21,$82,$3b,$38,$0e,$9f,$e8,$b6,$50,$82,$ba,$61,$d3);
  P09: array[0..19] of byte = ($90,$ae,$61,$cf,$7b,$ae,$bd,$4c,$ad,$e4,$94,$c5,$4a,$29,$ae,$70,$26,$9a,$ec,$71);
  C09: array[0..19] of byte = ($ce,$20,$27,$b4,$7a,$84,$32,$52,$01,$34,$65,$83,$4d,$75,$fd,$0f,$07,$29,$75,$2e);
  T09: array[0..15] of byte = ($ac,$d8,$83,$38,$37,$ab,$0e,$de,$84,$f4,$74,$8d,$a8,$89,$9c,$15);
  K10: THash256 = ($db,$bc,$85,$66,$d6,$f5,$b1,$58,$da,$99,$a2,$ff,$2e,$01,$dd,$a6,
    $29,$b8,$9c,$34,$ad,$1e,$5f,$eb,$a7,$0e,$7a,$ae,$43,$28,$28,$9c);
  I10: array[0..15] of byte = ($cf,$c0,$6e,$72,$2b,$e9,$87,$b3,$76,$7f,$70,$a7,$b8,$56,$b7,$74);
  P10: array[0..15] of byte = ($ce,$20,$27,$b4,$7a,$84,$32,$52,$01,$34,$65,$83,$4d,$75,$fd,$0f);
  C10: array[0..15] of byte = ($dc,$03,$e5,$24,$83,$0d,$30,$f8,$8e,$19,$7f,$3a,$ca,$ce,$66,$ef);
  T10: array[0..15] of byte = ($99,$84,$ef,$f6,$90,$57,$55,$d1,$83,$6f,$2d,$b0,$40,$89,$63,$4c);
  K11: THash256 = ($0e,$05,$93,$5d,$f0,$c6,$93,$74,$18,$92,$b7,$6f,$af,$67,$13,$3a,
    $bd,$2c,$f2,$03,$11,$21,$bd,$8b,$b3,$81,$27,$a4,$d2,$ee,$de,$ea);
  I11: array[0..16] of byte = ($74,$b1,$ba,$26,$85,$b3,$68,$09,$14,$29,$fc,$cb,$8d,$cd,$de,$09,$e4);
  H11: array[0..19] of byte = ($7b,$d8,$59,$a2,$47,$96,$1a,$21,$82,$3b,$38,$0e,$9f,$e8,$b6,$50,$82,$ba,$61,$d3);
  P11: array[0..19] of byte = ($90,$ae,$61,$cf,$7b,$ae,$bd,$4c,$ad,$e4,$94,$c5,$4a,$29,$ae,$70,$26,$9a,$ec,$71);
  C11: array[0..19] of byte = ($6b,$e6,$5e,$56,$06,$6c,$40,$56,$73,$8c,$03,$fe,$23,$20,$97,$4b,$a3,$f6,$5e,$09);
  T11: array[0..15] of byte = ($61,$08,$dc,$41,$7b,$f3,$2f,$7f,$b7,$55,$4a,$e5,$2f,$08,$8f,$87);
  procedure test(ptag: pointer; tlen: PtrInt; const key; kbits: PtrInt;
    pIV: pointer; IV_Len: PtrInt; pAAD: pointer; aLen: PtrInt;
    ctp: pointer; cLen: PtrInt; ptp: pointer; tn: integer);
  var tag: TAESBLock;
      ctxt: TAESGCMEngine;
      pt, ct: array[0..511] of byte;
  begin
    FillCharFast(pt,SizeOf(pt),0);
    CheckUTF8(ctxt.FullDecryptAndVerify(
      key,kbits,pIV,IV_Len,pAAD,aLen,ctp,@pt,cLen,ptag,tlen),
      'FullDecryptAndVerify #%',[tn]);
    CheckUTF8(CompareMem(@pt,ptp,cLen),'Plain #%',[tn]);
    FillCharFast(ct,SizeOf(ct),0);
    CheckUTF8(ctxt.FullEncryptAndAuthenticate(
      key,kbits,pIV,IV_len,pAAD,aLen,ptp,@ct,clen,tag),
      'FullEncryptAndAuthenticate #%',[tn]);
    CheckUTF8(CompareMem(@tag,ptag,tLen),'Tag #%',[tn]);
    CheckUTF8(CompareMem(@ct,ctp,cLen),'Encoded #%',[tn]);
  end;
var ctxt: TAESGCMEngine;
    key,tag: TAESBlock;
    buf: THash512;
    n: integer;
begin
  key := PAESBlock(@hex32)^;
  for n := 1 to 32 do begin
    Check(ctxt.Init(key,128));
    Check(ctxt.Reset(@hex32,n));
    Check(ctxt.Add_AAD(@hex32,n));
    Check(ctxt.Encrypt(@hex32,@buf,n));
    Check(ctxt.Final(tag));
    key := tag;
  end;
  Check(CompareMem(@buf32,@buf,SizeOf(buf32)));
  Check(CompareMem(@tag32,@tag,SizeOf(tag32)));
  test(@T01,16,K01,8*sizeof(K01),@I01,sizeof(I01),nil,0,@C01,sizeof(C01),@P01,01);
  test(@T02,16,K02,8*sizeof(K02),@I02,sizeof(I02),@H02,sizeof(H02),nil,0,nil,02);
  test(@T03,16,K03,8*sizeof(K03),@I03,sizeof(I03),@H03,sizeof(H03),@C03,sizeof(C03),@P03,03);
  test(@T04,16,K04,8*sizeof(K04),@I04,sizeof(I04),nil,0,@C04,sizeof(C04),@P04,04);
  test(@T05,16,K05,8*sizeof(K05),@I05,sizeof(I05),@H05,sizeof(H05),@C05,sizeof(C05),@P05,05);
  test(@T07,16,K07,8*sizeof(K07),@I07,sizeof(I07),@H07,sizeof(H07),@C07,sizeof(C07),@P07,07);
  test(@T08,16,K08,8*sizeof(K08),@I08,sizeof(I08),@H08,sizeof(H08),@C08,sizeof(C08),@P08,08);
  test(@T09,16,K09,8*sizeof(K09),@I09,sizeof(I09),@H09,sizeof(H09),@C09,sizeof(C09),@P09,09);
  test(@T10,16,K10,8*sizeof(K10),@I10,sizeof(I10),nil,0,@C10,sizeof(C10),@P10,10);
  test(@T11,16,K11,8*sizeof(K11),@I11,sizeof(I11),@H11,sizeof(H11),@C11,sizeof(C11),@P11,11);
end;

procedure TTestCryptographicRoutines._CompressShaAes;
var s1,s2: RawByteString;
    keysize,i: integer;
begin
  for keysize := 0 to 10 do begin
    CompressShaAesSetKey(RandomString(keysize));
    for i := 0 to 50 do begin
      s1 := RandomString(i*3);
      s2 := s1;
      Check(CompressShaAes(s1,true)='synshaaes');
      Check(CompressShaAes(s1,false)='synshaaes');
      Check(s1=s2);
    end;
  end;
end;

procedure TTestCryptographicRoutines._MD5;
var i,n: integer;
    md: TMD5;
    dig,dig2: TMD5Digest;
    tmp: TByteDynArray;
begin
  check(MD5SelfTest);
  check(htdigest('agent007','download area','secret')=
    'agent007:download area:8364d0044ef57b3defcfa141e8f77b65');
  check(MD5('')='d41d8cd98f00b204e9800998ecf8427e');
  check(MD5('a')='0cc175b9c0f1b6a831c399e269772661');
  check(MD5('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789')='d174ab98d277d9f5a5611c2c9f419d9f');
  SetLength(tmp,256);
  for n := 256-80 to 256 do begin
    md.Init;
    for i := 1 to n do
      md.Update(tmp[0],1);
    md.Final(dig);
    md.Full(pointer(tmp),n,dig2);
    check(IsEqual(dig,dig2));
    check(CompareMem(@dig,@dig2,sizeof(dig)));
  end;
end;

procedure TTestCryptographicRoutines._RC4;
var key, s, d: RawByteString;
    ks, i, len: integer;
    rc4, ref: TRC4;
begin
  Check(RC4SelfTest);
  key := RandomString(100);
  for ks := 1 to 10 do begin
    ref.InitSHA3(pointer(key)^,ks*10);
    for i := 0 to 100 do begin
      len := i*3;
      s := RandomAnsi7(len);
      SetString(d,nil,len);
      rc4 := ref;
      rc4.EncryptBuffer(pointer(s),pointer(d),len); // encrypt
      rc4 := ref;
      rc4.EncryptBuffer(pointer(d),pointer(d),len); // decrypt
      check(s=d);
    end;
  end;
end;

procedure TTestCryptographicRoutines._SHA1;
procedure SingleTest(const s: AnsiString; TDig: TSHA1Digest);
var SHA: TSHA1;
    Digest: TSHA1Digest;
    i: integer;
begin
  // 1. Hash complete AnsiString
  SHA.Full(pointer(s),length(s),Digest);
  Check(CompareMem(@Digest,@TDig,sizeof(Digest)));
  Check(IsEqual(Digest,TDig));
  // 2. one update call for all chars
  for i := 1 to length(s) do
    SHA.Update(@s[i],1);
  SHA.Final(Digest);
  Check(CompareMem(@Digest,@TDig,sizeof(Digest)));
  Check(IsEqual(Digest,TDig));
  // 3. test consistency with Padlock engine down results
{$ifdef USEPADLOCK}
  if not padlock_available then exit;
  padlock_available := false;  // force PadLock engine down
  SHA.Full(pointer(s),length(s),Digest);
  Check(CompareMem(@Digest,@TDig,sizeof(Digest)));
  Check(IsEqual(Digest,TDig));
{$ifdef PADLOCKDEBUG} write('=padlock '); {$endif}
  padlock_available := true; // restore previous value
{$endif}
end;
const
  Test1Out: TSHA1Digest=
    ($A9,$99,$3E,$36,$47,$06,$81,$6A,$BA,$3E,$25,$71,$78,$50,$C2,$6C,$9C,$D0,$D8,$9D);
  Test2Out: TSHA1Digest=
    ($84,$98,$3E,$44,$1C,$3B,$D2,$6E,$BA,$AE,$4A,$A1,$F9,$51,$29,$E5,$E5,$46,$70,$F1);
  DIG1 = '0c60c80f961f0e71f3a9b524af6012062fe037a6';
  DIG2 = 'ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957';
  DIG4096 = '4b007901b765489abead49d926f721d065a429c1';
var s: AnsiString;
    SHA: TSHA1;
    Hash: THash512Rec;
    Digest: TSHA1Digest absolute Hash;
    sign: TSynSigner;
begin
  SingleTest('abc',Test1Out);
  SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq',Test2Out);
  s := 'Wikipedia, l''encyclopedie libre et gratuite';
  SHA.Full(pointer(s),length(s),Digest);
  Check(SHA1DigestToString(Digest)='c18cc65028bbdc147288a2d136313287782b9c73');
  HMAC_SHA1('','',Digest);
  check(SHA1DigestToString(Digest)='fbdb1d1b18aa6c08324b7d64b71fb76370690e1d');
  HMAC_SHA1('key','The quick brown fox jumps over the lazy dog',Digest);
  check(SHA1DigestToString(Digest)='de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9');
  // from https://www.ietf.org/rfc/rfc6070.txt
  PBKDF2_HMAC_SHA1('password','salt',1,Digest);
  check(SHA1DigestToString(Digest)=DIG1);
  PBKDF2_HMAC_SHA1('password','salt',2,Digest);
  check(SHA1DigestToString(Digest)=DIG2);
  PBKDF2_HMAC_SHA1('password','salt',4096,Digest);
  check(SHA1DigestToString(Digest)=DIG4096);
  sign.PBKDF2(saSHA1,'password','salt',1,Hash);
  check(SHA1DigestToString(Digest)=DIG1);
  sign.PBKDF2(saSHA1,'password','salt',2,Hash);
  check(SHA1DigestToString(Digest)=DIG2);
  sign.PBKDF2(saSHA1,'password','salt',4096,Hash);
  check(SHA1DigestToString(Digest)=DIG4096);
end;

procedure TTestCryptographicRoutines._SHA256;
procedure DoTest;
procedure SingleTest(const s: AnsiString; const TDig: TSHA256Digest);
var SHA: TSHA256;
  Digest: TSHA256Digest;
  i: integer;
begin
  // 1. Hash complete AnsiString
  SHA.Full(pointer(s),length(s),Digest);
  Check(IsEqual(Digest,TDig));
  Check(CompareMem(@Digest,@TDig,sizeof(Digest)));
  // 2. one update call for each char
  SHA.Init;
  for i := 1 to length(s) do
    SHA.Update(@s[i],1);
  SHA.Final(Digest);
  Check(IsEqual(Digest,TDig));
  Check(CompareMem(@Digest,@TDig,sizeof(Digest)));
end;
const
  D1: TSHA256Digest =
    ($ba,$78,$16,$bf,$8f,$01,$cf,$ea,$41,$41,$40,$de,$5d,$ae,$22,$23,
     $b0,$03,$61,$a3,$96,$17,$7a,$9c,$b4,$10,$ff,$61,$f2,$00,$15,$ad);
  D2: TSHA256Digest =
    ($24,$8d,$6a,$61,$d2,$06,$38,$b8,$e5,$c0,$26,$93,$0c,$3e,$60,$39,
     $a3,$3c,$e4,$59,$64,$ff,$21,$67,$f6,$ec,$ed,$d4,$19,$db,$06,$c1);
  D3: TSHA256Digest =
    ($94,$E4,$A9,$D9,$05,$31,$23,$1D,$BE,$D8,$7E,$D2,$E4,$F3,$5E,$4A,
     $0B,$F4,$B3,$BC,$CE,$EB,$17,$16,$D5,$77,$B1,$E0,$8B,$A9,$BA,$A3);
  DIG4096 = 'c5e478d59288c841aa530db6845c4c8d962893a001ce4e11a4963873aa98134a';
var Digest: THash512Rec;
    Digests: THash256DynArray;
    sign: TSynSigner;
    c: AnsiChar;
    i: integer;
    sha: TSHA256;
begin
  SingleTest('abc',D1);
  SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq',D2);
  SHA256Weak('lagrangehommage',Digest.Lo); // test with len=256>64
  Check(IsEqual(Digest.Lo,D3));
  Check(CompareMem(@Digest,@D3,sizeof(Digest.Lo)));
  PBKDF2_HMAC_SHA256('password','salt',1,Digest.Lo);
  check(SHA256DigestToString(Digest.Lo)=
    '120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b');
  PBKDF2_HMAC_SHA256('password','salt',2,Digest.Lo);
  check(SHA256DigestToString(Digest.Lo)=
   'ae4d0c95af6b46d32d0adff928f06dd02a303f8ef3c251dfd6e2d85a95474c43');
  SetLength(Digests,2);
  check(IsZero(Digests[0]));
  check(IsZero(Digests[1]));
  PBKDF2_HMAC_SHA256('password','salt',2,Digests);
  check(IsEqual(Digests[0],Digest.Lo));
  check(not IsEqual(Digests[1],Digest.Lo));
  check(SHA256DigestToString(Digests[1])=
    '830651afcb5c862f0b249bd031f7a67520d136470f5ec271ece91c07773253d9');
  PBKDF2_HMAC_SHA256('password','salt',4096,Digest.Lo);
  check(SHA256DigestToString(Digest.Lo)= DIG4096);
  FillZero(Digest.b);
  sign.PBKDF2(saSha256,'password','salt',4096,Digest);
  check(SHA256DigestToString(Digest.Lo)= DIG4096);
  c := 'a';
  sha.Init;
  for i := 1 to 1000000 do
    sha.Update(@c,1);
  sha.Final(Digest.Lo);
  Check(SHA256DigestToString(Digest.Lo)=
    'cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0');
end;
begin
  DoTest;
  {$ifdef USEPADLOCK}
  if padlock_available then begin
    fRunConsole := fRunConsole+' using Padlock';
    padlock_available := false;  // force PadLock engine down
    DoTest;
    padlock_available := true;
  end;
  {$endif}
  {$ifdef CPUX64}
  if cfSSE41 in CpuFeatures then begin
    fRunConsole := fRunConsole+' using SSE4 instruction set';
    Exclude(CpuFeatures,cfSSE41);
    DoTest;
    Include(CpuFeatures,cfSSE41);
  end
  {$endif}
end;

procedure TTestCryptographicRoutines._SHA512;
  procedure Test(const password,secret,expected: RawUTF8; rounds: integer=0);
  var dig: THash512Rec;
      sign: TSynSigner;
  begin
    if rounds=0 then begin
      HMAC_SHA512(password,secret,dig.b);
      Check(SHA512DigestToString(dig.b)=expected);
      sign.Init(saSha512,password);
      sign.Update(secret);
      Check(sign.Final=expected);
    end else begin
      PBKDF2_HMAC_SHA512(password,secret,rounds,dig.b);
      Check(SHA512DigestToString(dig.b)=expected);
      FillZero(dig.b);
      sign.PBKDF2(saSha512,password,secret,rounds,dig);
      Check(SHA512DigestToString(dig.b)=expected);
    end;
  end;
const FOX: RawByteString = 'The quick brown fox jumps over the lazy dog';
var dig: TSHA512Digest;
    i: integer;
    sha: TSHA512;
    c: AnsiChar;
    temp: RawByteString;
begin // includes SHA-384, which is a truncated SHA-512
  Check(SHA384('')='38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63'+
    'f6e1da274edebfe76f65fbd51ad2f14898b95b');
  Check(SHA384('abc')='cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605'+
    'a43ff5bed8086072ba1e7cc2358baeca134c825a7');
  Check(SHA384('abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn'+
    'hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu')='09330c33f711'+
    '47e83d192fc782cd1b4753111b173b3b05d22fa08086e3b0f712fcc7c71a557e2db966c3e9fa91746039');
  Check(SHA512('')='cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d'+
    '36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e');
  Check(SHA512(FOX)='07e547d9586f6a73f73fbac0435ed76951218fb7d0c8d788a309d785'+
    '436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6');
  Check(SHA512(FOX+'.')='91ea1245f20d46ae9a037a989f54f1f790f0a47607eeb8a14d128'+
    '90cea77a1bbc6c7ed9cf205e67b7f2b8fd4c7dfd3a7a8617e45f3c463d481c7e586c39ac1ed');
  sha.Init;
  for i := 1 to length(FOX) do
    sha.Update(@FOX[i],1);
  sha.Final(dig);
  Check(SHA512DigestToString(dig)='07e547d9586f6a73f73fbac0435ed76951218fb7d0c'+
    '8d788a309d785436bbb642e93a252a954f23912547d1e8a3b5ed6e1bfd7097821233fa0538f3db854fee6');
  c := 'a';
  sha.Init;
  for i := 1 to 1000 do
    sha.Update(@c,1);
  sha.Final(dig);
  Check(SHA512DigestToString(dig)='67ba5535a46e3f86dbfbed8cbbaf0125c76ed549ff8'+
    'b0b9e03e0c88cf90fa634fa7b12b47d77b694de488ace8d9a65967dc96df599727d3292a8d9d447709c97');
  SetLength(temp,1000);
  FillCharFast(pointer(temp)^,1000,ord('a'));
  Check(SHA512(temp)=SHA512DigestToString(dig));
  for i := 1 to 1000000 do
    sha.Update(@c,1);
  sha.Final(dig);
  Check(SHA512DigestToString(dig)='e718483d0ce769644e2e42c7bc15b4638e1f98b13b2'+
    '044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b');
  Test('','','b936cee86c9f87aa5d3c6f2e84cb5a4239a5fe50480a'+
    '6ec66b70ab5b1f4ac6730c6c515421b327ec1d69402e53dfb49ad7381eb067b338fd7b0cb22247225d47');
  Test('key',FOX,'b42af09057bac1e2d41708e48a902e09b5ff7f12ab42'+
    '8a4fe86653c73dd248fb82f948a549f7b791a5b41915ee4d1ec3935357e4e2317250d0372afa2ebeeb3a');
  Test(FOX+FOX,FOX,'19e504ba787674baa63471436a4ec5a71ba359a0f2d375'+
    '12edd4db69dce1ec6a0e48f0ae460fc9342fbb453cf2942a0e3fa512dd361e30f0e8b8fc8c7a4ece96');
  Test('Jefe','what do ya want for nothing?','164b7a7bfcf819e2e395fbe73b56e0a387bd64222e8'+
    '31fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737');
  Test('password','salt','867f70cf1ade02cff3752599a3a53dc4af34c7a669815ae5'+
    'd513554e1c8cf252c02d470a285a0501bad999bfe943c08f050235d7d68b1da55e63f73b60a57fce',1);
  Test('password','salt','d197b1b33db0143e018b12f3d1d1479e6cdebdcc97c5c0f87'+
    'f6902e072f457b5143f30602641b3d55cd335988cb36b84376060ecd532e039b742a239434af2d5',4096);
  HMAC_SHA256('Jefe','what do ya want for nothing?',PHash256(@dig)^);
  Check(SHA256DigestToString(PHash256(@dig)^)='5bdcc146bf60754e6a042426089575c'+
    '75a003f089d2739839dec58b964ec3843');
  HMAC_SHA384('Jefe','what do ya want for nothing?',PHash384(@dig)^);
  Check(SHA384DigestToString(PHash384(@dig)^)='af45d2e376484031617f78d2b58a6b1'+
    'b9c7ef464f5a01b47e42ec3736322445e8e2240ca5e69e2c78b3239ecfab21649');
  PBKDF2_HMAC_SHA384('password','salt',4096,PHash384(@dig)^);
  Check(SHA384DigestToString(PHash384(@dig)^)='559726be38db125bc85ed7895f6e3cf574c7a01c'+
    '080c3447db1e8a76764deb3c307b94853fbe424f6488c5f4f1289626');
  PBKDF2_HMAC_SHA512('passDATAb00AB7YxDTT','saltKEYbcTcXHCBxtjD',1,dig);
  Check(SHA512DigestToString(dig)='cbe6088ad4359af42e603c2a33760ef9d4017a7b2aad10af46'+
    'f992c660a0b461ecb0dc2a79c2570941bea6a08d15d6887e79f32b132e1c134e9525eeddd744fa');
  PBKDF2_HMAC_SHA384('passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqK',
   'saltKEYbcTcXHCBxtjD2PnBh44AIQ6XUOCESOhXpEp3HrcG',1,PHash384(@dig)^);
  Check(SHA384DigestToString(PHash384(@dig)^)='0644a3489b088ad85a0e42be3e7f82500ec189366'+
    '99151a2c90497151bac7bb69300386a5e798795be3cef0a3c803227');
  { // rounds=100000 is slow, so not test by default
  PBKDF2_HMAC_SHA512('passDATAb00AB7YxDTT','saltKEYbcTcXHCBxtjD',100000,dig);
  Check(SHA512DigestToString(dig)='accdcd8798ae5cd85804739015ef2a11e32591b7b7d16f76819b30'+
    'b0d49d80e1abea6c9822b80a1fdfe421e26f5603eca8a47a64c9a004fb5af8229f762ff41f');
  PBKDF2_HMAC_SHA384('passDATAb00AB7YxDTTlRH2dqxDx19GDxDV1zFMz7E6QVqK','saltKEYbcTcXHCBxtj'+
    'D2PnBh44AIQ6XUOCESOhXpEp3HrcG',100000,PHash384(@dig)^);
  Check(SHA384DigestToString(PHash384(@dig)^)='bf625685b48fe6f187a1780c5cb8e1e4a7b0dbd'+
    '6f551827f7b2b598735eac158d77afd3602383d9a685d87f8b089af30');
  }
end;

procedure TTestCryptographicRoutines._SHA3;
const HASH1 = '79f38adec5c20307a98ef76e8324afbfd46cfd81b22e3973c65fa1bd9de31787';
      DK = '7bbdbe37ea70dd2ed640837ff8a926d381806ffa931695addd38ab950d35ad18801a8290e8d97fe14cdfd3cfdbcd0fe766d3e6e4636bd0a17d710a61678db363';
var instance: TSHA3;
    secret, data, encrypted: RawByteString;
    dig: THash256;
    h512: THash512Rec;
    s, i: integer;
    sign: TSynSigner;
begin
  // validate against official NIST vectors
  // taken from http://csrc.nist.gov/groups/ST/toolkit/examples.html#aHashing
  Check(instance.FullStr(SHA3_224, nil, 0) =
    '6B4E03423667DBB73B6E15454F0EB1ABD4597F9A1B078E3F5B5A6BC7');
  Check(instance.FullStr(SHA3_256, nil, 0) =
    'A7FFC6F8BF1ED76651C14756A061D662F580FF4DE43B49FA82D80A4B80F8434A');
  Check(instance.FullStr(SHA3_384, nil, 0) =
    '0C63A75B845E4F7D01107D852E4C2485C51A50AAAA94FC61995E71BBEE983A2AC3713831264ADB47FB6BD1E058D5F004');
  Check(instance.FullStr(SHA3_512, nil, 0) =
    'A69F73CCA23A9AC5C8B567DC185A756E97C982164FE25859E0D1DCC1475C80A615B2123AF1F5F94C11E3E9402C3AC558F500199D95B6D3E301758586281DCD26');
  Check(instance.FullStr(SHAKE_128, nil, 0) =
    '7F9C2BA4E88F827D616045507605853ED73B8093F6EFBC88EB1A6EACFA66EF26');
  Check(instance.FullStr(SHAKE_256, nil, 0) =
    '46B9DD2B0BA88D13233B3FEB743EEB243FCD52EA62B81B82B50C27646ED5762FD75DC4DDD8C0F200CB05019D67B592F6FC821C49479AB48640292EACB3B7C4BE');
  SetLength(data, 200);
  FillCharFast(pointer(data)^, 200, $A3);
  Check(instance.FullStr(SHA3_224, pointer(data), length(data)) =
    '9376816ABA503F72F96CE7EB65AC095DEEE3BE4BF9BBC2A1CB7E11E0');
  Check(instance.FullStr(SHA3_256, pointer(data), length(data)) =
    '79F38ADEC5C20307A98EF76E8324AFBFD46CFD81B22E3973C65FA1BD9DE31787');
  Check(instance.FullStr(SHA3_384, pointer(data), length(data)) =
    '1881DE2CA7E41EF95DC4732B8F5F002B189CC1E42B74168ED1732649CE1DBCDD76197A31FD55EE989F2D7050DD473E8F');
  Check(instance.FullStr(SHA3_512, pointer(data), length(data)) =
    'E76DFAD22084A8B1467FCF2FFA58361BEC7628EDF5F3FDC0E4805DC48CAEECA81B7C13C30ADF52A3659584739A2DF46BE589C51CA1A4A8416DF6545A1CE8BA00');
  instance.Init(SHA3_256);
  for i := 1 to length(data) do
    instance.Update(pointer(data), 1);
  instance.Final(dig);
  Check(SHA256DigestToString(dig) = HASH1);
  Check(sign.Full(saSha3256,data,nil,0) = HASH1);
  instance.Init(SHA3_256);
  instance.Update(pointer(data), 100);
  instance.Update(pointer(data), 50);
  instance.Update(pointer(data), 20);
  instance.Update(pointer(data), 10);
  instance.Update(pointer(data), 10);
  instance.Update(pointer(data), 5);
  instance.Update(pointer(data), 5);
  instance.Final(dig, true); // NoInit=true to check Extendable-Output Function
  Check(SHA256DigestToString(dig) = HASH1);
  instance.Final(dig, true);
  Check(SHA256DigestToString(dig) =
    'f85500852a5b9bb4a35440e7e4b4dba9184477a4c97b97ab0b24b91a8b04d1c8');
  for i := 1 to 200 do begin
    FillZero(dig);
    instance.Final(dig, true);
    Check(not IsZero(dig),'XOF mode');
  end;
  instance.Final(dig);
  Check(SHA256DigestToString(dig) =
    '75f8b0591e2baeae027d56c14ef3bc014d9dd29cce08b8b184528589147fc252','XOF vector');
  encrypted := instance.Cypher('secret', 'toto');
  Check(SynCommons.BinToHex(encrypted) = 'BF013A29');
  Check(SynCommons.BinToHexLower(encrypted) = 'bf013a29');
  for s := 0 to 3 do begin
    secret := RandomString(s * 3);
    Check(instance.Cypher(secret, '') = '');
    for i := 1 to 1000 do begin
      data := RandomString(i);
      encrypted := instance.Cypher(secret, data);
      Check((i<16) or (encrypted <> data));
      instance.InitCypher(secret);
      Check(instance.Cypher(encrypted) = data);
    end;
  end;
  PBKDF2_SHA3(SHA3_512,'pass','salt',1000,@h512);
  check(SHA512DigestToString(h512.b)=DK);
  FillZero(h512.b);
  sign.PBKDF2(saSha3512,'pass','salt',1000,h512);
  check(SHA512DigestToString(h512.b)=DK);
  // taken from https://en.wikipedia.org/wiki/SHA-3
  Check(SHA3(SHAKE_128, 'The quick brown fox jumps over the lazy dog') =
    'F4202E3C5852F9182A0430FD8144F0A74B95E7417ECAE17DB0F8CFEED0E3E66E');
  Check(SHA3(SHAKE_128, 'The quick brown fox jumps over the lazy dof') =
    '853F4538BE0DB9621A6CEA659A06C1107B1F83F02B13D18297BD39D7411CF10C');
end;

procedure TTestCryptographicRoutines._TAESPNRG;
var b1,b2: TAESBlock;
    a1,a2: TAESPRNG;
    s1,s2,split: RawByteString;
    c: cardinal;
    d: double;
    e: TSynExtended;
    i,stripes: integer;
    clo, chi, dlo, dhi, elo, ehi: integer;
begin
  TAESPRNG.Main.FillRandom(b1);
  TAESPRNG.Main.FillRandom(b2);
  Check(not IsEqual(b1,b2));
  Check(not CompareMem(@b1,@b2,sizeof(b1)));
  clo := 0;
  chi := 0;
  dlo := 0;
  dhi := 0;
  elo := 0;
  ehi := 0;
  a1 := TAESPRNG.Create;
  a2 := TAESPRNG.Create;
  try
    a1.FillRandom(b1);
    a2.FillRandom(b2);
    Check(not IsEqual(b1,b2));
    Check(not CompareMem(@b1,@b2,sizeof(b1)));
    Check(a1.FillRandom(0)='');
    Check(a1.FillRandomHex(0)='');
    for i := 1 to 2000 do begin
      s1 := a1.FillRandom(i);
      s2 := a2.FillRandom(i);
      check(length(s1)=i);
      check(length(s2)=i);
      if i>4 then
        check(s1<>s2);
      // compress the output to validate (somehow) its randomness
      check(length(SynLZCompress(s1))>i,'random should not compress');
      check(length(SynLZCompress(s2))>i,'random should not compress');
      s1 := a1.FillRandomHex(i);
      check(length(s1)=i*2);
      check(SynCommons.HexToBin(pointer(s1),nil,i));
      c := a1.Random32;
      check(c<>a2.Random32,'Random32 collision');
      if c<cardinal(maxint) then
        inc(clo) else
        inc(chi);
      check(a1.Random64<>a2.Random64);
      check(a1.Random32(i)<cardinal(i));
      d := a1.RandomDouble;
      check((d>=0)and(d<1));
      if d<0.5 then
        inc(dlo) else
        inc(dhi);
      d := a2.RandomDouble;
      check((d>=0)and(d<1));
      if d<0.5 then
        inc(dlo) else
        inc(dhi);
      e := a1.Randomext;
      check((e>=0)and(e<1));
      if e<0.5 then
        inc(elo) else
        inc(ehi);
      e := a2.Randomext;
      check((e>=0)and(e<1));
      if e<0.5 then
        inc(elo) else
        inc(ehi);
    end;
  finally
    a1.Free;
    a2.Free;
  end;
  Check(clo+chi=2000);
  Check(dlo+dhi=4000);
  Check(elo+ehi=4000);
  CheckUTF8((clo>=900) and (clo<=1100),'Random32 distribution clo=%',[clo]);
  CheckUTF8((dlo>=1800) and (dlo<=2100),'RandomDouble distribution dlo=%',[dlo]);
  CheckUTF8((elo>=1900) and (elo<=2100),'RandomExt distribution elo=%',[elo]);
  s1 := TAESPRNG.Main.FillRandom(100);
  for i := 1 to length(s1) do
    for stripes := 0 to 10 do begin
      split := TAESPRNG.Main.AFSplit(pointer(s1)^,i,stripes);
      check(length(split)=i*(stripes+1));
      check(TAESPRNG.AFUnsplit(split,pointer(s2)^,i));
      check(CompareMem(pointer(s1),pointer(s2),i));
    end;
  check(PosEx(s1,split)=0);
end;

procedure TTestCryptographicRoutines.CryptData(dpapi: boolean);
var i,size: integer;
    plain,enc,test: RawByteString;
    appsec: RawUTF8;
    func: function(const Data,AppSecret: RawByteString; Encrypt: boolean): RawByteString;
    tim: TPrecisionTimer;
const MAX = 1000;
begin
  {$ifdef MSWINDOWS}
  if dpapi then
    func := CryptDataForCurrentUserDPAPI else
  {$endif}
    func := CryptDataForCurrentUser;
  func('warmup','appsec',true);
  size := 0;
  tim.Start;
  for i := 0 to MAX-1 do begin
    plain := TAESPRNG.Main.FillRandom(i);
    check(length(plain)=i);
    UInt32ToUtf8(i,appsec);
    enc := func(plain,appsec,true);
    check((plain='') or (enc<>''));
    check(length(enc)>=length(plain));
    test := func(enc,appsec,false);
    check(length(test)=i);
    check(test=plain);
    inc(size,i+length(enc));
  end;
  if dpapi then
    NotifyTestSpeed('DPAPI',MAX*2,size,@tim) else
    NotifyTestSpeed('AES-CFB',MAX*2,size,@tim);
end;

procedure TTestCryptographicRoutines._CryptDataForCurrentUser;
begin
  CryptData(false);
end;

{$ifdef MSWINDOWS}
procedure TTestCryptographicRoutines._CryptDataForCurrentUserAPI;
begin
  CryptData(true);
end;
{$endif}

{$ifndef NOVARIANTS}
procedure TTestCryptographicRoutines._JWT;
  procedure test(one: TJWTAbstract);
  var t: RawUTF8;
      jwt: TJWTContent;
      i: integer;
      exp: TUnixTime;
  begin
    t := one.Compute(['http://example.com/is_root',true],'joe');
    check(t<>'');
    check(TJWTAbstract.VerifyPayload(t,'','joe','',@exp)=jwtValid);
    check(one.VerifyPayload(t,'','joe','',@exp)=jwtValid);
    check(one.CacheTimeoutSeconds=0);
    one.Options := one.Options+[joHeaderParse];
    one.Verify(t,jwt);
    check(jwt.result=jwtValid);
    check(jwt.reg[jrcIssuer]='joe');
    one.Options := one.Options-[joHeaderParse];
    one.CacheTimeoutSeconds := 60;
    check(one.CacheTimeoutSeconds=60);
    one.Verify(t,jwt);
    check(exp=GetCardinal(pointer(jwt.reg[jrcExpirationTime])));
    check(jwt.result=jwtValid);
    check(jwt.reg[jrcExpirationTime]<>'');
    check(jwt.reg[jrcIssuer]='joe');
    check(jwt.data.B['http://example.com/is_root']);
    check((jwt.reg[jrcIssuedAt]<>'')=(jrcIssuedAt in one.Claims));
    check((jwt.reg[jrcJWTID]<>'')=(jrcJWTID in one.Claims));
    for i := 1 to 1000 do begin
      Finalize(jwt);
      FillCharFast(jwt,sizeof(jwt),0);
      check(jwt.reg[jrcIssuer]='');
      one.Verify(t,jwt);
      check(jwt.result=jwtValid,'from cache');
      check(jwt.reg[jrcIssuer]='joe');
      check((jwt.reg[jrcJWTID]<>'')=(jrcJWTID in one.Claims));
    end;
    if (one.Algorithm<>'none') and (t[length(t)] in ['1'..'9','B'..'Z','b'..'z']) then begin
      dec(t[length(t)]); // invalidate signature
      one.Verify(t,jwt);
      check(jwt.result<>jwtValid);
    end;
    one.Free;
  end;
  procedure Benchmark(algo: TSignAlgo);
  var i: integer;
      tok: RawUTF8;
      j: TJWTAbstract;
      jwt: TJWTContent;
      tim: TPrecisionTimer;
  begin
    j := JWT_CLASS[algo].Create('secret',0,[jrcIssuer,jrcExpirationTime],[]);
    try
      tok := j.Compute([],'myself');
      tim.Start;
      for i := 1 to 1000 do begin
        jwt.result := jwtWrongFormat;
        j.Verify(tok,jwt);
        check(jwt.result=jwtValid);
        check(jwt.reg[jrcIssuer]='myself');
      end;
      NotifyTestSpeed('%',[JWT_TEXT[algo]],1000,0,@tim);
    finally
      j.Free;
    end;
  end;
var i: integer;
    j: TJWTAbstract;
    jwt: TJWTContent;
    secret: TECCCertificateSecret;
    tok: RawUTF8;
    tim: TPrecisionTimer;
    a: TSignAlgo;
begin
  test(TJWTNone.Create([jrcIssuer,jrcExpirationTime],[],60));
  test(TJWTNone.Create([jrcIssuer,jrcExpirationTime,jrcIssuedAt],[],60));
  test(TJWTNone.Create([jrcIssuer,jrcExpirationTime,jrcIssuedAt,jrcJWTID],[],60));
  test(TJWTHS256.Create('sec',100,[jrcIssuer,jrcExpirationTime],[],60));
  test(TJWTHS256.Create('sec',200,[jrcIssuer,jrcExpirationTime,jrcIssuedAt],[],60));
  test(TJWTHS256.Create('sec',10,[jrcIssuer,jrcExpirationTime,jrcIssuedAt,jrcJWTID],[],60));
  j := TJWTHS256.Create('secret',0,[jrcSubject],[]);
  try
    jwt.result := jwtWrongFormat;
    j.Verify('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibm'+
      'FtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeF'+
      'ONFh7HgQ',jwt); // reference from jwt.io
    check(jwt.result=jwtValid);
    check(jwt.reg[jrcSubject]='1234567890');
    check(jwt.data.U['name']='John Doe');
    check(jwt.data.B['admin']);
    j.Verify('eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibm'+
      'FtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeF'+
      'ONFh7hgQ',jwt); // altered one char in signature
    check(jwt.result=jwtInvalidSignature);
    tok := j.Compute(['uid','{1CCA336D-A78F-4EB6-B701-1DB8E749BD1F}'],'','subject');
    j.Verify(tok,jwt);
    Check(jwt.result=jwtValid);
    check(jwt.reg[jrcSubject]='subject');
    check(jwt.data.U['uid']='{1CCA336D-A78F-4EB6-B701-1DB8E749BD1F}');
  finally
    j.Free;
  end;
  for i := 1 to 10 do begin
    secret := TECCCertificateSecret.CreateNew(nil); // self-signed certificate
    test(TJWTES256.Create(secret,[jrcIssuer,jrcExpirationTime],[],60));
    test(TJWTES256.Create(secret,[jrcIssuer,jrcExpirationTime,jrcIssuedAt],[],60));
    test(TJWTES256.Create(secret,[jrcIssuer,jrcExpirationTime,jrcIssuedAt,jrcJWTID],[],60));
    secret.Free;
  end;
  for a := saSha256 to high(a) do
    Benchmark(a);
  secret := TECCCertificateSecret.CreateNew(nil);
  j := TJWTES256.Create(secret,[jrcIssuer,jrcExpirationTime],[],60);
  try
    tok := j.Compute([],'myself');
    tim.Start;
    for i := 1 to 100 do begin
      jwt.result := jwtWrongFormat;
      j.Verify(tok, jwt);
      check(jwt.result=jwtValid);
      check(jwt.reg[jrcIssuer]='myself');
    end;
    NotifyTestSpeed('ES256',100,0,@tim);
  finally
    j.Free;
    secret.Free;
  end;
end;
{$endif NOVARIANTS}

type
  TBenchmark = (
    // non cryptographic hashes
    bCRC32c, bXXHash32, bHash32,
    // cryptographic hashes
    bMD5, bSHA1, bHMACSHA1, bSHA256, bHMACSHA256,
    bSHA384, bHMACSHA384, bSHA512, bHMACSHA512,
    bSHA3_256, bSHA3_512,
    // encryption
    bRC4,
    bAES128CFB, bAES128OFB, bAES128CFBCRC, bAES128OFBCRC, bAES128GCM,
    bAES256CFB, bAES256OFB, bAES256CFBCRC, bAES256OFBCRC, bAES256GCM,
    bSHAKE128, bSHAKE256
    );

procedure TTestCryptographicRoutines.Benchmark;
const SIZ: array[0..4] of integer = (8, 50, 100, 1000, 10000);
      COUNT = 500;
      AESCLASS: array[bAES128CFB .. bAES256GCM] of TAESAbstractClass = (
        TAESCFB, TAESOFB, TAESCFBCRC, TAESOFBCRC, TAESGCM,
        TAESCFB, TAESOFB, TAESCFBCRC, TAESOFBCRC, TAESGCM);
      AESBITS: array[bAES128CFB .. bAES256GCM] of integer = (
        128, 128, 128, 128, 128, 256, 256, 256, 256, 256);
var b: TBenchmark;
    s, i, size, n: integer;
    data, encrypted: RawByteString;
    dig: THash512Rec;
    MD5: TMD5;
    SHA1: TSHA1;
    SHA256: TSHA256;
    SHA384: TSHA384;
    SHA512: TSHA512;
    SHA3, SHAKE128, SHAKE256: TSHA3;
    RC4: TRC4;
    timer: TPrecisionTimer;
    time: array[TBenchmark] of Int64;
    AES: array[bAES128CFB .. bAES256GCM] of TAESAbstract;
    TXT: array[TBenchmark] of RawUTF8;
begin
  GetEnumTrimmedNames(TypeInfo(TBenchmark),@TXT);
  for b := low(b) to high(b) do
    TXT[b] := LowerCase(TXT[b]);
  for b := low(AES) to high(AES) do
    AES[b] := AESCLASS[b].Create(dig, AESBITS[b]);
  SHAKE128.InitCypher('secret', SHAKE_128);
  SHAKE256.InitCypher('secret', SHAKE_256);
  RC4.InitSHA3(dig,SizeOf(dig));
  FillCharFast(time,sizeof(time),0);
  size := 0;
  n := 0;
  for s := 0 to high(SIZ) do begin
    data := RandomString(SIZ[s]);
    SetLength(encrypted, SIZ[s]);
    for b := low(b) to high(b) do begin
      timer.Start;
      for i := 1 to COUNT do begin
        dig.d0 := 0;
        dig.d1 := 0;
        case b of
        bXXHash32:   dig.d0 := xxHash32(0,pointer(data),SIZ[s]);
        bHash32:     dig.d0 := Hash32(pointer(data),SIZ[s]);
        bCRC32c:     dig.d0 := crc32c(0,pointer(data),SIZ[s]);
        bMD5:        MD5.Full(pointer(data),SIZ[s],dig.h0);
        bSHA1:       SHA1.Full(pointer(data),SIZ[s],dig.b160);
        bHMACSHA1:   HMAC_SHA1('secret',data,dig.b160);
        bSHA256:     SHA256.Full(pointer(data),SIZ[s],dig.Lo);
        bHMACSHA256: HMAC_SHA256('secret',data,dig.Lo);
        bSHA384:     SHA384.Full(pointer(data),SIZ[s],dig.b384);
        bHMACSHA384: HMAC_SHA384('secret',data,dig.b384);
        bSHA512:     SHA512.Full(pointer(data),SIZ[s],dig.b);
        bHMACSHA512: HMAC_SHA512('secret',data,dig.b);
        bSHA3_256:   SHA3.Full(pointer(data),SIZ[s],dig.Lo);
        bSHA3_512:   SHA3.Full(pointer(data),SIZ[s],dig.b);
        bRC4:        RC4.EncryptBuffer(pointer(data), pointer(Encrypted), SIZ[s]);
        bAES128CFB, bAES128OFB, bAES256CFB, bAES256OFB:
                     AES[b].EncryptPKCS7(Data,{encrypt=}true);
        bAES128CFBCRC, bAES128OFBCRC, bAES256CFBCRC, bAES256OFBCRC,
        bAES128GCM, bAES256GCM:
                     AES[b].MACAndCrypt(Data,{encrypt=}true);
        bSHAKE128:   SHAKE128.Cypher(pointer(Data), pointer(Encrypted), SIZ[s]);
        bSHAKE256:   SHAKE256.Cypher(pointer(Data), pointer(Encrypted), SIZ[s]);
        end;
        Check((b >= bRC4) or (dig.d0 <> 0) or (dig.d1 <> 0));
      end;
      inc(time[b],NotifyTestSpeed('% %',[TXT[b],SIZ[s]],COUNT,SIZ[s]*COUNT,@timer,{onlylog=}true));
      //if b in [bSHA3_512,high(b)] then AddConsole('');
    end;
    inc(size,SIZ[s]*COUNT);
    inc(n,COUNT);
  end;
  for b := low(b) to high(b) do
    AddConsole(format('%d %s in %s i.e. %d/s or %s/s',
      [n, TXT[b], MicroSecToString(time[b]), (Int64(n)*1000000) div time[b],
       KB((Int64(size)*1000000) div time[b])]));
  for b := low(AES) to high(AES) do
    AES[b].Free;
end;

{
  some numbers, on a Core i7 (SSE4.2+AESNI) notebook, with SynCrypto 1.18.3800:

Delphi 7, Win32
  - Benchmark: 1,050,000 assertions passed  8.62s
     10000 crc32c 8 B in 161us i.e. 62111801/s, aver. 0us, 473.8 MB/s
     10000 xxhash32 8 B in 185us i.e. 54054054/s, aver. 0us, 412.4 MB/s
     10000 md5 8 B in 1.68ms i.e. 5948839/s, aver. 0us, 45.3 MB/s
     10000 sha1 8 B in 3.01ms i.e. 3312355/s, aver. 0us, 25.2 MB/s
     10000 hmacsha1 8 B in 11.97ms i.e. 835421/s, aver. 1us, 6.3 MB/s
     10000 sha256 8 B in 4.07ms i.e. 2451581/s, aver. 0us, 18.7 MB/s
     10000 hmacsha256 8 B in 16.15ms i.e. 619041/s, aver. 1us, 4.7 MB/s
     10000 sha512 8 B in 6.66ms i.e. 1500375/s, aver. 0us, 11.4 MB/s
     10000 hmacsha512 8 B in 29.67ms i.e. 336961/s, aver. 2us, 2.5 MB/s
     10000 sha3_256 8 B in 8.45ms i.e. 1182312/s, aver. 0us, 9 MB/s
     10000 sha3_512 8 B in 8.49ms i.e. 1177856/s, aver. 0us, 8.9 MB/s

     10000 aes128cfb 8 B in 996us i.e. 10040160/s, aver. 0us, 76.6 MB/s
     10000 aes128ofb 8 B in 903us i.e. 11074197/s, aver. 0us, 84.4 MB/s
     10000 aes128cfbcrc 8 B in 1.14ms i.e. 8726003/s, aver. 0us, 66.5 MB/s
     10000 aes128ofbcrc 8 B in 1.18ms i.e. 8460236/s, aver. 0us, 64.5 MB/s
     10000 aes256cfb 8 B in 1.55ms i.e. 6430868/s, aver. 0us, 49 MB/s
     10000 aes256ofb 8 B in 980us i.e. 10204081/s, aver. 0us, 77.8 MB/s
     10000 aes256cfbcrc 8 B in 1.22ms i.e. 8149959/s, aver. 0us, 62.1 MB/s
     10000 aes256ofbcrc 8 B in 1.25ms i.e. 7974481/s, aver. 0us, 60.8 MB/s
     10000 shake128 8 B in 538us i.e. 18587360/s, aver. 0us, 141.8 MB/s
     10000 shake256 8 B in 1.33ms i.e. 7485029/s, aver. 0us, 57.1 MB/s

     10000 crc32c 50 B in 129us i.e. 77519379/s, aver. 0us, 3.6 GB/s
     10000 xxhash32 50 B in 189us i.e. 52910052/s, aver. 0us, 2.4 GB/s
     10000 md5 50 B in 1.45ms i.e. 6863417/s, aver. 0us, 327.2 MB/s
     10000 sha1 50 B in 3.07ms i.e. 3247807/s, aver. 0us, 154.8 MB/s
     10000 hmacsha1 50 B in 11.89ms i.e. 840689/s, aver. 1us, 40 MB/s
     10000 sha256 50 B in 4.17ms i.e. 2396931/s, aver. 0us, 114.2 MB/s
     10000 hmacsha256 50 B in 16.06ms i.e. 622393/s, aver. 1us, 29.6 MB/s
     10000 sha512 50 B in 7.32ms i.e. 1364628/s, aver. 0us, 65 MB/s
     10000 hmacsha512 50 B in 26.80ms i.e. 373134/s, aver. 2us, 17.7 MB/s
     10000 sha3_256 50 B in 8.56ms i.e. 1167815/s, aver. 0us, 55.6 MB/s
     10000 sha3_512 50 B in 8.39ms i.e. 1191185/s, aver. 0us, 56.8 MB/s

     10000 aes128cfb 50 B in 1.75ms i.e. 5685048/s, aver. 0us, 271 MB/s
     10000 aes128ofb 50 B in 1.65ms i.e. 6056935/s, aver. 0us, 288.8 MB/s
     10000 aes128cfbcrc 50 B in 1.83ms i.e. 5443658/s, aver. 0us, 259.5 MB/s
     10000 aes128ofbcrc 50 B in 2.35ms i.e. 4239084/s, aver. 0us, 202.1 MB/s
     10000 aes256cfb 50 B in 1.96ms i.e. 5083884/s, aver. 0us, 242.4 MB/s
     10000 aes256ofb 50 B in 1.96ms i.e. 5094243/s, aver. 0us, 242.9 MB/s
     10000 aes256cfbcrc 50 B in 2.14ms i.e. 4662004/s, aver. 0us, 222.3 MB/s
     10000 aes256ofbcrc 50 B in 2.25ms i.e. 4428697/s, aver. 0us, 211.1 MB/s
     10000 shake128 50 B in 2.56ms i.e. 3898635/s, aver. 0us, 185.9 MB/s
     10000 shake256 50 B in 3.86ms i.e. 2590002/s, aver. 0us, 123.5 MB/s

     10000 crc32c 100 B in 164us i.e. 60975609/s, aver. 0us, 5.6 GB/s
     10000 xxhash32 100 B in 300us i.e. 33333333/s, aver. 0us, 3.1 GB/s
     10000 md5 100 B in 2.71ms i.e. 3679175/s, aver. 0us, 350.8 MB/s
     10000 sha1 100 B in 5.91ms i.e. 1692047/s, aver. 0us, 161.3 MB/s
     10000 hmacsha1 100 B in 14.65ms i.e. 682267/s, aver. 1us, 65 MB/s
     10000 sha256 100 B in 11.16ms i.e. 895495/s, aver. 1us, 85.4 MB/s
     10000 hmacsha256 100 B in 19.68ms i.e. 507897/s, aver. 1us, 48.4 MB/s
     10000 sha512 100 B in 7.14ms i.e. 1399972/s, aver. 0us, 133.5 MB/s
     10000 hmacsha512 100 B in 26.34ms i.e. 379535/s, aver. 2us, 36.1 MB/s
     10000 sha3_256 100 B in 8.43ms i.e. 1185677/s, aver. 0us, 113 MB/s
     10000 sha3_512 100 B in 16.43ms i.e. 608457/s, aver. 1us, 58 MB/s

     10000 aes128cfb 100 B in 2.33ms i.e. 4282655/s, aver. 0us, 408.4 MB/s
     10000 aes128ofb 100 B in 2.97ms i.e. 3365870/s, aver. 0us, 320.9 MB/s
     10000 aes128cfbcrc 100 B in 2.51ms i.e. 3976143/s, aver. 0us, 379.1 MB/s
     10000 aes128ofbcrc 100 B in 2.46ms i.e. 4055150/s, aver. 0us, 386.7 MB/s
     10000 aes256cfb 100 B in 2.92ms i.e. 3418803/s, aver. 0us, 326 MB/s
     10000 aes256ofb 100 B in 2.91ms i.e. 3425830/s, aver. 0us, 326.7 MB/s
     10000 aes256cfbcrc 100 B in 3.06ms i.e. 3259452/s, aver. 0us, 310.8 MB/s
     10000 aes256ofbcrc 100 B in 3.09ms i.e. 3229974/s, aver. 0us, 308 MB/s
     10000 shake128 100 B in 4.98ms i.e. 2006823/s, aver. 0us, 191.3 MB/s
     10000 shake256 100 B in 5.80ms i.e. 1721763/s, aver. 0us, 164.2 MB/s

     10000 crc32c 1000 B in 2.00ms i.e. 4995004/s, aver. 0us, 4.6 GB/s
     10000 xxhash32 1000 B in 2.08ms i.e. 4796163/s, aver. 0us, 4.4 GB/s
     10000 md5 1000 B in 20.95ms i.e. 477235/s, aver. 2us, 455.1 MB/s
     10000 sha1 1000 B in 45.64ms i.e. 219072/s, aver. 4us, 208.9 MB/s
     10000 hmacsha1 1000 B in 56.70ms i.e. 176363/s, aver. 5us, 168.1 MB/s
     10000 sha256 1000 B in 61.30ms i.e. 163121/s, aver. 6us, 155.5 MB/s
     10000 hmacsha256 1000 B in 72.31ms i.e. 138276/s, aver. 7us, 131.8 MB/s
     10000 sha512 1000 B in 54.13ms i.e. 184723/s, aver. 5us, 176.1 MB/s
     10000 hmacsha512 1000 B in 70.01ms i.e. 142828/s, aver. 7us, 136.2 MB/s
     10000 sha3_256 1000 B in 62.15ms i.e. 160890/s, aver. 6us, 153.4 MB/s
     10000 sha3_512 1000 B in 108.78ms i.e. 91927/s, aver. 10us, 87.6 MB/s

     10000 aes128cfb 1000 B in 16.28ms i.e. 613948/s, aver. 1us, 585.5 MB/s
     10000 aes128ofb 1000 B in 14.86ms i.e. 672540/s, aver. 1us, 641.3 MB/s
     10000 aes128cfbcrc 1000 B in 15.20ms i.e. 657505/s, aver. 1us, 627 MB/s
     10000 aes128ofbcrc 1000 B in 15.94ms i.e. 627273/s, aver. 1us, 598.2 MB/s
     10000 aes256cfb 1000 B in 20.52ms i.e. 487305/s, aver. 2us, 464.7 MB/s
     10000 aes256ofb 1000 B in 20.56ms i.e. 486357/s, aver. 2us, 463.8 MB/s
     10000 aes256cfbcrc 1000 B in 21.31ms i.e. 469241/s, aver. 2us, 447.5 MB/s
     10000 aes256ofbcrc 1000 B in 23.93ms i.e. 417780/s, aver. 2us, 398.4 MB/s
     10000 shake128 1000 B in 47.67ms i.e. 209744/s, aver. 4us, 200 MB/s
     10000 shake256 1000 B in 56.18ms i.e. 177973/s, aver. 5us, 169.7 MB/s

     10000 crc32c 9 KB in 23.40ms i.e. 427186/s, aver. 2us, 3.9 GB/s
     10000 xxhash32 9 KB in 15.91ms i.e. 628535/s, aver. 1us, 5.8 GB/s
     10000 md5 9 KB in 210.63ms i.e. 47475/s, aver. 21us, 452.7 MB/s
     10000 sha1 9 KB in 444.99ms i.e. 22472/s, aver. 44us, 214.3 MB/s
     10000 hmacsha1 9 KB in 458.45ms i.e. 21812/s, aver. 45us, 208 MB/s
     10000 sha256 9 KB in 607.39ms i.e. 16463/s, aver. 60us, 157 MB/s
     10000 hmacsha256 9 KB in 618.00ms i.e. 16181/s, aver. 61us, 154.3 MB/s
     10000 sha512 9 KB in 502.78ms i.e. 19889/s, aver. 50us, 189.6 MB/s
     10000 hmacsha512 9 KB in 525.57ms i.e. 19026/s, aver. 52us, 181.4 MB/s
     10000 sha3_256 9 KB in 564.61ms i.e. 17711/s, aver. 56us, 168.9 MB/s
     10000 sha3_512 9 KB in 1.05s i.e. 9506/s, aver. 105us, 90.6 MB/s

     10000 aes128cfb 9 KB in 149.54ms i.e. 66871/s, aver. 14us, 637.7 MB/s
     10000 aes128ofb 9 KB in 142.10ms i.e. 70368/s, aver. 14us, 671 MB/s
     10000 aes128cfbcrc 9 KB in 146.78ms i.e. 68124/s, aver. 14us, 649.6 MB/s
     10000 aes128ofbcrc 9 KB in 148.38ms i.e. 67393/s, aver. 14us, 642.7 MB/s
     10000 aes256cfb 9 KB in 198.82ms i.e. 50295/s, aver. 19us, 479.6 MB/s
     10000 aes256ofb 9 KB in 199.27ms i.e. 50181/s, aver. 19us, 478.5 MB/s
     10000 aes256cfbcrc 9 KB in 199.70ms i.e. 50073/s, aver. 19us, 477.5 MB/s
     10000 aes256ofbcrc 9 KB in 200.13ms i.e. 49966/s, aver. 20us, 476.5 MB/s
     10000 shake128 9 KB in 478.81ms i.e. 20884/s, aver. 47us, 199.1 MB/s
     10000 shake256 9 KB in 574.64ms i.e. 17402/s, aver. 57us, 165.9 MB/s

     50000 crc32c in 25.87ms i.e. 1932292/s or 4 GB/s
     50000 xxhash32 in 18.68ms i.e. 2676659/s or 5.5 GB/s
     50000 md5 in 237.45ms i.e. 210562/s or 448.1 MB/s
     50000 sha1 in 502.65ms i.e. 99471/s or 211.6 MB/s
     50000 hmacsha1 in 553.69ms i.e. 90302/s or 192.1 MB/s
     50000 sha256 in 688.12ms i.e. 72660/s or 154.6 MB/s
     50000 hmacsha256 in 742.24ms i.e. 67363/s or 143.3 MB/s
     50000 sha512 in 578.07ms i.e. 86493/s or 184 MB/s
     50000 hmacsha512 in 678.42ms i.e. 73700/s or 156.8 MB/s
     50000 sha3_256 in 652.23ms i.e. 76659/s or 163.1 MB/s
     50000 sha3_512 in 1.19s i.e. 41876/s or 89.1 MB/s

     50000 aes128cfb in 170.93ms i.e. 292517/s or 622.5 MB/s
     50000 aes128ofb in 162.51ms i.e. 307662/s or 654.7 MB/s
     50000 aes128cfbcrc in 167.51ms i.e. 298489/s or 635.2 MB/s
     50000 aes128ofbcrc in 170.34ms i.e. 293521/s or 624.6 MB/s
     50000 aes256cfb in 225.80ms i.e. 221427/s or 471.2 MB/s
     50000 aes256ofb in 225.71ms i.e. 221520/s or 471.4 MB/s
     50000 aes256cfbcrc in 227.46ms i.e. 219810/s or 467.8 MB/s
     50000 aes256ofbcrc in 230.69ms i.e. 216739/s or 461.2 MB/s
     50000 shake128 in 534.59ms i.e. 93528/s or 199 MB/s
     50000 shake256 in 641.85ms i.e. 77899/s or 165.7 MB/s
  Total failed: 0 / 1,302,057  - Cryptographic routines PASSED  10.49s

Delphi 10.2 Tokyo, Win64
  - Benchmark: 1,050,000 assertions passed  7.42s
     10000 crc32c 8 B in 114us i.e. 87719298/s, aver. 0us, 669.2 MB/s
     10000 xxhash32 8 B in 130us i.e. 76923076/s, aver. 0us, 586.8 MB/s
     10000 md5 8 B in 1.92ms i.e. 5208333/s, aver. 0us, 39.7 MB/s
     10000 sha1 8 B in 3.38ms i.e. 2950722/s, aver. 0us, 22.5 MB/s
     10000 hmacsha1 8 B in 13.21ms i.e. 756944/s, aver. 1us, 5.7 MB/s
     10000 sha256 8 B in 2.56ms i.e. 3900156/s, aver. 0us, 29.7 MB/s
     10000 hmacsha256 8 B in 10.08ms i.e. 991669/s, aver. 1us, 7.5 MB/s
     10000 sha512 8 B in 3.51ms i.e. 2845759/s, aver. 0us, 21.7 MB/s
     10000 hmacsha512 8 B in 13.41ms i.e. 745212/s, aver. 1us, 5.6 MB/s
     10000 sha3_256 8 B in 8.09ms i.e. 1235635/s, aver. 0us, 9.4 MB/s
     10000 sha3_512 8 B in 7.98ms i.e. 1252505/s, aver. 0us, 9.5 MB/s

     10000 aes128cfb 8 B in 1.18ms i.e. 8453085/s, aver. 0us, 64.4 MB/s
     10000 aes128ofb 8 B in 1.16ms i.e. 8620689/s, aver. 0us, 65.7 MB/s
     10000 aes128cfbcrc 8 B in 1.26ms i.e. 7936507/s, aver. 0us, 60.5 MB/s
     10000 aes128ofbcrc 8 B in 1.24ms i.e. 8058017/s, aver. 0us, 61.4 MB/s
     10000 aes256cfb 8 B in 1.65ms i.e. 6045949/s, aver. 0us, 46.1 MB/s
     10000 aes256ofb 8 B in 1.24ms i.e. 8058017/s, aver. 0us, 61.4 MB/s
     10000 aes256cfbcrc 8 B in 1.33ms i.e. 7468259/s, aver. 0us, 56.9 MB/s
     10000 aes256ofbcrc 8 B in 1.33ms i.e. 7490636/s, aver. 0us, 57.1 MB/s
     10000 shake128 8 B in 518us i.e. 19305019/s, aver. 0us, 147.2 MB/s
     10000 shake256 8 B in 614us i.e. 16286644/s, aver. 0us, 124.2 MB/s

     10000 crc32c 50 B in 125us i.e. 80000000/s, aver. 0us, 3.7 GB/s
     10000 xxhash32 50 B in 188us i.e. 53191489/s, aver. 0us, 2.4 GB/s
     10000 md5 50 B in 1.76ms i.e. 5656108/s, aver. 0us, 269.7 MB/s
     10000 sha1 50 B in 3.39ms i.e. 2947244/s, aver. 0us, 140.5 MB/s
     10000 hmacsha1 50 B in 13.27ms i.e. 753238/s, aver. 1us, 35.9 MB/s
     10000 sha256 50 B in 2.57ms i.e. 3888024/s, aver. 0us, 185.3 MB/s
     10000 hmacsha256 50 B in 10.04ms i.e. 995619/s, aver. 1us, 47.4 MB/s
     10000 sha512 50 B in 3.52ms i.e. 2839295/s, aver. 0us, 135.3 MB/s
     10000 hmacsha512 50 B in 13.44ms i.e. 743715/s, aver. 1us, 35.4 MB/s
     10000 sha3_256 50 B in 8.08ms i.e. 1236552/s, aver. 0us, 58.9 MB/s
     10000 sha3_512 50 B in 7.96ms i.e. 1256281/s, aver. 0us, 59.9 MB/s

     10000 aes128cfb 50 B in 2.11ms i.e. 4719207/s, aver. 0us, 225 MB/s
     10000 aes128ofb 50 B in 1.92ms i.e. 5208333/s, aver. 0us, 248.3 MB/s
     10000 aes128cfbcrc 50 B in 2.20ms i.e. 4526935/s, aver. 0us, 215.8 MB/s
     10000 aes128ofbcrc 50 B in 2.38ms i.e. 4185851/s, aver. 0us, 199.5 MB/s
     10000 aes256cfb 50 B in 2.46ms i.e. 4063388/s, aver. 0us, 193.7 MB/s
     10000 aes256ofb 50 B in 2.25ms i.e. 4438526/s, aver. 0us, 211.6 MB/s
     10000 aes256cfbcrc 50 B in 2.55ms i.e. 3907776/s, aver. 0us, 186.3 MB/s
     10000 aes256ofbcrc 50 B in 2.32ms i.e. 4310344/s, aver. 0us, 205.5 MB/s
     10000 shake128 50 B in 2.34ms i.e. 4260758/s, aver. 0us, 203.1 MB/s
     10000 shake256 50 B in 2.91ms i.e. 3431708/s, aver. 0us, 163.6 MB/s

     10000 crc32c 100 B in 144us i.e. 69444444/s, aver. 0us, 6.4 GB/s
     10000 xxhash32 100 B in 297us i.e. 33670033/s, aver. 0us, 3.1 GB/s
     10000 md5 100 B in 3.39ms i.e. 2945508/s, aver. 0us, 280.9 MB/s
     10000 sha1 100 B in 6.37ms i.e. 1569612/s, aver. 0us, 149.6 MB/s
     10000 hmacsha1 100 B in 16.37ms i.e. 610575/s, aver. 1us, 58.2 MB/s
     10000 sha256 100 B in 4.70ms i.e. 2123593/s, aver. 0us, 202.5 MB/s
     10000 hmacsha256 100 B in 12.34ms i.e. 809847/s, aver. 1us, 77.2 MB/s
     10000 sha512 100 B in 3.52ms i.e. 2840909/s, aver. 0us, 270.9 MB/s
     10000 hmacsha512 100 B in 13.43ms i.e. 744158/s, aver. 1us, 70.9 MB/s
     10000 sha3_256 100 B in 8.07ms i.e. 1238390/s, aver. 0us, 118.1 MB/s
     10000 sha3_512 100 B in 14.98ms i.e. 667244/s, aver. 1us, 63.6 MB/s

     10000 aes128cfb 100 B in 3.05ms i.e. 3272251/s, aver. 0us, 312 MB/s
     10000 aes128ofb 100 B in 3.03ms i.e. 3292723/s, aver. 0us, 314 MB/s
     10000 aes128cfbcrc 100 B in 3.10ms i.e. 3219575/s, aver. 0us, 307 MB/s
     10000 aes128ofbcrc 100 B in 2.70ms i.e. 3698224/s, aver. 0us, 352.6 MB/s
     10000 aes256cfb 100 B in 3.64ms i.e. 2743484/s, aver. 0us, 261.6 MB/s
     10000 aes256ofb 100 B in 3.23ms i.e. 3087372/s, aver. 0us, 294.4 MB/s
     10000 aes256cfbcrc 100 B in 3.76ms i.e. 2658867/s, aver. 0us, 253.5 MB/s
     10000 aes256ofbcrc 100 B in 3.30ms i.e. 3030303/s, aver. 0us, 288.9 MB/s
     10000 shake128 100 B in 4.46ms i.e. 2238638/s, aver. 0us, 213.4 MB/s
     10000 shake256 100 B in 5.55ms i.e. 1798884/s, aver. 0us, 171.5 MB/s

     10000 crc32c 1000 B in 469us i.e. 21321961/s, aver. 0us, 19.8 GB/s
     10000 xxhash32 1000 B in 1.69ms i.e. 5899705/s, aver. 0us, 5.4 GB/s
     10000 md5 1000 B in 27.18ms i.e. 367822/s, aver. 2us, 350.7 MB/s
     10000 sha1 1000 B in 49.08ms i.e. 203715/s, aver. 4us, 194.2 MB/s
     10000 hmacsha1 1000 B in 58.57ms i.e. 170724/s, aver. 5us, 162.8 MB/s
     10000 sha256 1000 B in 34.10ms i.e. 293246/s, aver. 3us, 279.6 MB/s
     10000 hmacsha256 1000 B in 41.55ms i.e. 240633/s, aver. 4us, 229.4 MB/s
     10000 sha512 1000 B in 24.76ms i.e. 403844/s, aver. 2us, 385.1 MB/s
     10000 hmacsha512 1000 B in 34.00ms i.e. 294117/s, aver. 3us, 280.4 MB/s
     10000 sha3_256 1000 B in 57.47ms i.e. 173994/s, aver. 5us, 165.9 MB/s
     10000 sha3_512 1000 B in 98.39ms i.e. 101628/s, aver. 9us, 96.9 MB/s

     10000 aes128cfb 1000 B in 20.87ms i.e. 479064/s, aver. 2us, 456.8 MB/s
     10000 aes128ofb 1000 B in 16.51ms i.e. 605656/s, aver. 1us, 577.6 MB/s
     10000 aes128cfbcrc 1000 B in 20.73ms i.e. 482346/s, aver. 2us, 460 MB/s
     10000 aes128ofbcrc 1000 B in 16.27ms i.e. 614363/s, aver. 1us, 585.9 MB/s
     10000 aes256cfb 1000 B in 25.92ms i.e. 385787/s, aver. 2us, 367.9 MB/s
     10000 aes256ofb 1000 B in 21.89ms i.e. 456787/s, aver. 2us, 435.6 MB/s
     10000 aes256cfbcrc 1000 B in 26.66ms i.e. 375051/s, aver. 2us, 357.6 MB/s
     10000 aes256ofbcrc 1000 B in 21.91ms i.e. 456412/s, aver. 2us, 435.2 MB/s
     10000 shake128 1000 B in 41.89ms i.e. 238686/s, aver. 4us, 227.6 MB/s
     10000 shake256 1000 B in 52.93ms i.e. 188925/s, aver. 5us, 180.1 MB/s

     10000 crc32c 9 KB in 4.02ms i.e. 2482621/s, aver. 0us, 23.1 GB/s
     10000 xxhash32 9 KB in 15.40ms i.e. 649139/s, aver. 1us, 6 GB/s
     10000 md5 9 KB in 267.76ms i.e. 37346/s, aver. 26us, 356.1 MB/s
     10000 sha1 9 KB in 472.80ms i.e. 21150/s, aver. 47us, 201.7 MB/s
     10000 hmacsha1 9 KB in 482.23ms i.e. 20736/s, aver. 48us, 197.7 MB/s
     10000 sha256 9 KB in 331.68ms i.e. 30149/s, aver. 33us, 287.5 MB/s
     10000 hmacsha256 9 KB in 338.41ms i.e. 29549/s, aver. 33us, 281.8 MB/s
     10000 sha512 9 KB in 241.79ms i.e. 41357/s, aver. 24us, 394.4 MB/s
     10000 hmacsha512 9 KB in 243.28ms i.e. 41103/s, aver. 24us, 391.9 MB/s
     10000 sha3_256 9 KB in 517.69ms i.e. 19316/s, aver. 51us, 184.2 MB/s
     10000 sha3_512 9 KB in 963.30ms i.e. 10380/s, aver. 96us, 99 MB/s

     10000 aes128cfb 9 KB in 197.43ms i.e. 50650/s, aver. 19us, 483 MB/s
     10000 aes128ofb 9 KB in 158.28ms i.e. 63176/s, aver. 15us, 602.4 MB/s
     10000 aes128cfbcrc 9 KB in 196.23ms i.e. 50959/s, aver. 19us, 485.9 MB/s
     10000 aes128ofbcrc 9 KB in 155.09ms i.e. 64478/s, aver. 15us, 614.9 MB/s
     10000 aes256cfb 9 KB in 250.88ms i.e. 39858/s, aver. 25us, 380.1 MB/s
     10000 aes256ofb 9 KB in 211.23ms i.e. 47341/s, aver. 21us, 451.4 MB/s
     10000 aes256cfbcrc 9 KB in 254.16ms i.e. 39344/s, aver. 25us, 375.2 MB/s
     10000 aes256ofbcrc 9 KB in 209.72ms i.e. 47681/s, aver. 20us, 454.7 MB/s
     10000 shake128 9 KB in 414.13ms i.e. 24146/s, aver. 41us, 230.2 MB/s
     10000 shake256 9 KB in 525.12ms i.e. 19043/s, aver. 52us, 181.6 MB/s

     50000 crc32c in 4.89ms i.e. 10206164/s or 21.2 GB/s
     50000 xxhash32 in 17.73ms i.e. 2819601/s or 5.8 GB/s
     50000 md5 in 302.06ms i.e. 165529/s or 352.2 MB/s
     50000 sha1 in 535.07ms i.e. 93445/s or 198.8 MB/s
     50000 hmacsha1 in 583.71ms i.e. 85658/s or 182.3 MB/s
     50000 sha256 in 375.66ms i.e. 133098/s or 283.2 MB/s
     50000 hmacsha256 in 412.48ms i.e. 121216/s or 257.9 MB/s
     50000 sha512 in 277.14ms i.e. 180411/s or 383.9 MB/s
     50000 hmacsha512 in 317.62ms i.e. 157418/s or 335 MB/s
     50000 sha3_256 in 599.45ms i.e. 83409/s or 177.5 MB/s
     50000 sha3_512 in 1.09s i.e. 45759/s or 97.3 MB/s

     50000 aes128cfb in 224.69ms i.e. 222528/s or 473.5 MB/s
     50000 aes128ofb in 180.94ms i.e. 276331/s or 588 MB/s
     50000 aes128cfbcrc in 223.56ms i.e. 223644/s or 475.9 MB/s
     50000 aes128ofbcrc in 177.72ms i.e. 281328/s or 598.7 MB/s
     50000 aes256cfb in 284.59ms i.e. 175687/s or 373.9 MB/s
     50000 aes256ofb in 239.87ms i.e. 208438/s or 443.6 MB/s
     50000 aes256cfbcrc in 288.51ms i.e. 173303/s or 368.8 MB/s
     50000 aes256ofbcrc in 238.61ms i.e. 209544/s or 445.9 MB/s
     50000 shake128 in 463.39ms i.e. 107899/s or 229.6 MB/s
     50000 shake256 in 587.17ms i.e. 85153/s or 181.2 MB/s
  Total failed: 0 / 1,302,089  - Cryptographic routines PASSED  8.86s
}

{ TTestECCCryptography }

const
  ECC_COUNT = {$ifdef CPU64}200{$else}50{$endif};

procedure TTestECCCryptography.ReferenceVectors;
var pr1,pr2: TECCPrivateKey;
    pu1,pu2: TECCPublicKey;
    h1,h2: TECCHash;
    si1,si2: TECCSignature;
    s1,s2,s3: TECCSecretKey;
begin
  SetLength(pub, ECC_COUNT);
  SetLength(priv, ECC_COUNT);
  SetLength(sign, ECC_COUNT);
  TAESPRNG.Main.FillRandom(@hash,sizeof(hash));
  Check(SynCommons.HexToBin(PAnsiChar(
    'DC5B79BD481E536DD8075D06C18D42B25B557B4671017BA2A26102B69FD9B70A'),@pr1,sizeof(pr1)));
  Check(SynCommons.HexToBin(PAnsiChar(
    '024698753E25650A3129320A7DDBA43D56051F4BEE3653897960A61FBC92AB24A5'),@pu1,sizeof(pu1)));
  Check(SynCommons.HexToBin(PAnsiChar(
    'CFA96FAC873F522897000815BE96338DE8D355D5F495DD5C5A4FEF0AEDB66D5B'),@pr2,sizeof(pr2)));
  Check(SynCommons.HexToBin(PAnsiChar(
    '0298D0D01FCE73146C10CD05E08BEA573BEE4EFC56D5EBAAC64B32672C8FAC1502'),@pu2,sizeof(pu2)));
  Check(SynCommons.HexToBin(PAnsiChar(
    '9509D00BBBA2308445BC73311C3887E935183F65D361D4C39E2FA432B7168599'),@h1,sizeof(h1)));
  Check(SynCommons.HexToBin(
    PAnsiChar('F04CD0AA3D40433C51F35D07DBF4E11C91C922791A8BA7B930B5C30716D8B26E4B65EFBF'+
    'BDC0526A94ABDAA31130248F0413AC33D5BFA903E09847AAF42FD043'),@si1,sizeof(si1)));
  Check(SynCommons.HexToBin(PAnsiChar(
    '3366C112F95B2F52836171CAD3F3441C4B3C75348859092B200DE5024CB0C91B'),@h2,sizeof(h2)));
  Check(SynCommons.HexToBin(PAnsiChar(
    'EEEF6F1D0A590BFC72B9D7DC0DB4BF36A8928DA2B8078FEE567808BB082525438CF68546'+
    '26E17FBB28528450E50E43AB2598ED2CD3ACC7B43865BEB843452713'),@si2,sizeof(si2)));
  Check(SynCommons.HexToBin(PAnsiChar(
    '51A0C8018EC725F9B9F821D826FEEC4CAE8843066685522F1961D25935EAA39E'),@s1,sizeof(s1)));
  Check(ecdsa_verify(pu1,h1,si1));
  Check(ecdsa_verify(pu2,h2,si2));
  FillZero(s2);
  Check(ecdh_shared_secret(pu1,pr2,s2));
  Check(IsEqual(s1,s2));
  Check(CompareMem(@s1,@s2,sizeof(s1)));
  FillZero(s3);
  Check(ecdh_shared_secret(pu2,pr1,s3));
  Check(IsEqual(s1,s3));
  Check(CompareMem(@s1,@s3,sizeof(s1)));
  {$ifdef HASUINT64} // pascal (fallback) version
  Check(ecdsa_verify_pas(pu1,h1,si1));
  Check(ecdsa_verify_pas(pu2,h2,si2));
  FillZero(s2);
  Check(ecdh_shared_secret_pas(pu1,pr2,s2));
  Check(IsEqual(s1,s2));
  FillZero(s3);
  Check(ecdh_shared_secret_pas(pu2,pr1,s3));
  Check(IsEqual(s1,s3));
  {$endif}
end;

procedure TTestECCCryptography._ecc_make_key;
var i: integer;
begin
  for i := 0 to ECC_COUNT-1 do
    Check(ecc_make_key(pub[i], priv[i]));
end;

procedure TTestECCCryptography._ecdsa_sign;
var i: integer;
begin
  for i := 0 to ECC_COUNT-1 do
    Check(ecdsa_sign(priv[i], hash, sign[i]));
end;

procedure TTestECCCryptography._ecdsa_verify;
var i: integer;
begin
  for i := 0 to ECC_COUNT-1 do
    check(ecdsa_verify(pub[i], hash, sign[i]));
end;

procedure TTestECCCryptography._ecdh_shared_secret;
var sec1,sec2: TECCSecretKey;
    i: integer;
begin
  for i := 0 to ECC_COUNT-2 do begin
    check(ecdh_shared_secret(pub[i],priv[i+1],sec1));
    check(ecdh_shared_secret(pub[i+1],priv[i],sec2));
    check(IsEqual(sec1,sec2));
  end;
end;

procedure TTestECCCryptography.CertificatesAndSignatures;
const
  PUBPRIV64: RawUTF8 =
    'AQAKAAoAFAAp49cdwmwTSgk7ocIs+iWCLVmLFDvnzMbgAAAAAAAAACnj1x3CbBN'+
    'KCTuhwiz6JYItWYsUO+fMxuAAAAAAAAAAAgm92LeP/SogOQAmFAKppFHFPPn1vRERJ1dwk5y8'+
    'AloD66iKgas4FCX8yprik12Unvk3K45kS1tIkga7U273SBAoDj5WP1ENURn7znVgPm5UPrMZO'+
    'vaZNdUuDPlCy1uzNJeQTIkgAAAAnddux+slXpcupBr3m2g/2skZyPIT0Y2mk9As06J2mMY=';
  PUBPRIVJSON: RawUTF8 =
    '{"Version":1,"Serial":"29E3D71DC26C134A093BA1C22CFA2582",'+
    '"Issuer":"synopse.info","IssueDate":"2016-08-11","ValidityStart":'+
    '"2016-08-11","ValidityEnd":"2016-08-21","AuthoritySerial":'+
    '"29E3D71DC26C134A093BA1C22CFA2582","AuthorityIssuer":"synopse.info",'+
    '"IsSelfSigned":true,"Base64":"';
const
  // Generated by tests
  MYPRIVKEY: array[0..255] of byte = (
    $39,$EC,$C0,$0D,$D0,$ED,$47,$DC,$2A,$14,$72,$80,$D7,$E2,$48,$C1,
    $87,$6F,$11,$60,$5C,$77,$1C,$C6,$9B,$A8,$AD,$FD,$95,$17,$45,$A3,
    $2F,$A0,$4A,$B3,$AF,$B4,$27,$13,$85,$16,$E0,$6C,$F7,$75,$F1,$C5,
    $7C,$75,$6D,$34,$8C,$8F,$AB,$AD,$AA,$EA,$94,$5F,$A7,$B6,$F1,$E3,
    $D4,$0E,$3D,$FE,$96,$ED,$5C,$53,$90,$98,$60,$1A,$85,$9D,$BF,$70,
    $0F,$B2,$9D,$9B,$B2,$66,$36,$26,$F7,$FD,$3A,$5F,$DC,$AE,$67,$3B,
    $8E,$C4,$61,$71,$5D,$F6,$1F,$9A,$2A,$20,$A0,$C9,$F8,$0D,$FB,$EE,
    $3A,$17,$FA,$50,$FA,$AB,$EF,$72,$F8,$1D,$55,$CA,$1F,$6A,$86,$CB,
    $AA,$0E,$58,$01,$1F,$8E,$6F,$CC,$EA,$ED,$98,$1B,$4D,$1F,$85,$89,
    $74,$F6,$03,$FB,$9F,$1A,$50,$95,$F2,$8C,$79,$78,$9A,$94,$5C,$7F,
    $2E,$CA,$06,$3E,$E7,$93,$7F,$93,$8F,$64,$6D,$27,$A4,$B3,$81,$CE,
    $DB,$B1,$2A,$28,$79,$B6,$22,$87,$9F,$91,$01,$53,$6B,$B1,$AF,$91,
    $60,$87,$8F,$61,$87,$55,$D0,$FF,$33,$73,$05,$FD,$39,$DC,$A9,$B7,
    $EA,$D3,$72,$D6,$A6,$00,$98,$D2,$91,$96,$19,$A9,$1D,$7C,$6C,$9B,
    $F8,$D0,$50,$31,$52,$C3,$D8,$1D,$9B,$54,$1B,$09,$8C,$CE,$36,$1B,
    $4F,$2A,$EC,$98,$9B,$A2,$F7,$C4,$A8,$78,$AD,$DA,$B5,$56,$89,$67);
  MYPRIVKEY_LEN = SizeOf(MYPRIVKEY);
  MYPRIVKEY_ROUNDS = 100;
  MYPRIVKEY_PASS = '123456';
  MYPRIVKEY_CYPH = '4e/QgInP';
var selfsignedroot, secret: TECCCertificateSecret;
    cert: TECCCertificate;
    sav, json, serial: RawUTF8;
    bin: RawByteString;
    {$ifndef DELPHI5OROLDER}
    json1,json2,jsonchain: RawUTF8;
    {$endif}
    chain: TECCCertificateChain;
    sign: TECCSignatureCertified;
    signcontent: TECCSignatureCertifiedContent;
begin
  chain := TECCCertificateChain.Create;
  try
    check(chain.Count=0);
    selfsignedroot := TECCCertificateSecret.CreateNew(nil,'synopse.info',10);
    check(selfsignedroot.IsSelfSigned);
    check(selfsignedroot.HasSecret);
    check(chain.IsValid(nil)=ecvBadParameter);
    check(chain.IsValid(selfsignedroot)=ecvValidSelfSigned);
    check(chain.Add(nil)=-1);
    check(chain.Add(selfsignedroot)=-1);
    check(chain.Count=0);
    check(chain.AddSelfSigned(selfsignedroot)=0);
    check(chain.Count=1);
    check(not chain.IsValidCached);
    chain.IsValidCached := true;
    selfsignedroot := TECCCertificateSecret.CreateNew(nil,'mORMot.net',0);
    serial := selfsignedroot.Serial;
    check(length(serial)=32);
    check(selfsignedroot.IsSelfSigned);
    check(selfsignedroot.HasSecret);
    check(chain.IsValid(nil)=ecvBadParameter);
    check(chain.IsValid(selfsignedroot)=ecvValidSelfSigned);
    check(chain.Add(nil)=-1);
    check(chain.Add(selfsignedroot)=-1);
    check(chain.Count=1);
    check(chain.AddSelfSigned(selfsignedroot)=1);
    check(chain.Count=2);
    secret := TECCCertificateSecret.CreateNew(selfsignedroot,'google.fr');
    check(chain.Count=2);
    check(secret.HasSecret);
    check(not secret.IsSelfSigned);
    check(chain.IsValid(secret)=ecvValidSigned);
    {$ifndef DELPHI5OROLDER}
    json1 := ObjectToJson(secret);
    {$endif}
    sav := secret.PublicToBase64;
    cert := TECCCertificate.CreateFromBase64(sav);
    check(cert.Serial=secret.Serial);
    check(not cert.IsSelfSigned);
    check(chain.IsValid(cert)=ecvValidSigned);
    check(cert.Issuer='google.fr');
    check(cert.AuthorityIssuer='mormot.net');
    check(chain.Add(cert)=2);
    check(chain.Count=3);
    check(chain.GetBySerial(cert.Content.Signed.Serial)=cert);
    {$ifndef DELPHI5OROLDER}
    json2 := ObjectToJson(cert);
    check(json1=json2,'serialization trim private key');
    {$endif}
    secret.Free;
    inc(sav[10]); // corrupt
    cert := TECCCertificate.Create;
    check(not cert.FromBase64(sav));
    check(chain.IsValid(cert)=ecvCorrupted);
    secret := TECCCertificateSecret.CreateFromBase64(PUBPRIV64);
    check(secret.HasSecret);
    check(secret.IsSelfSigned);
    check(chain.IsValid(secret.Content,true)=ecvValidSelfSigned);
    check(secret.Serial<>cert.Serial);
    check(secret.Serial='29E3D71DC26C134A093BA1C22CFA2582');
    {$ifndef DELPHI5OROLDER}
    json1 := ObjectToJson(secret);
    check(json1<>json2);
    json2 := PUBPRIVJSON+copy(PUBPRIV64,1,posEx('y1uzNJeQTIk',PUBPRIV64)+10)+'AAAAA"}';
    check(json1=json2,'no private key');
    jsonchain := ObjectToJson(chain);
    check(length(jsonchain)=1545);
    {$endif}
    sav := secret.SaveToSource('MyPrivKey','Generated by tests','123456');
//  FileFromString(sav,'privkey.pas');
    check(length(sav)=1467);
    secret.Free;
    cert.Free;
    check(selfsignedroot.SaveToSecureFile('pass','.',64,1000));
    secret := TECCCertificateSecret.CreateNew(selfsignedroot,'toto.com');
    check(chain.Count=3);
    check(chain.IsValid(secret)=ecvValidSigned);
    json := chain.SaveToJson;
    check(length(json)=718,'certificates have fixed len');
    chain.Free; // will release selfsignedroot
    chain := TECCCertificateChain.Create;
    check(chain.IsValid(secret)=ecvUnknownAuthority);
    check(chain.LoadFromJson(json));
    check(chain.SaveToJson=json);
    check(chain.Count=3);
    check(chain.IsValid(secret)=ecvValidSigned);
    {$ifndef DELPHI5OROLDER}
    json := ObjectToJson(chain);
    check(length(json)=1546);
    chain.SaveToFile('test');
    {$endif}
    bin := secret.SaveToSecureBinary('toto',64,1000);
    check(length(bin)=2320);
    secret.Free;
    secret := TECCCertificateSecret.CreateFromSecureBinary(
      @MYPRIVKEY,MYPRIVKEY_LEN,MYPRIVKEY_PASS,MYPRIVKEY_ROUNDS);
    check(secret.Serial='29E3D71DC26C134A093BA1C22CFA2582');
    check(chain.IsValid(secret.Content,true)=ecvValidSelfSigned);
    {$ifndef DELPHI5OROLDER}
    json2 := ObjectToJson(secret);
    check(json1=json2);
    {$endif}
    secret.Free;
    secret := TECCCertificateSecret.Create;
    check(chain.IsValid(secret)=ecvCorrupted);
    check(not secret.LoadFromSecureBinary(bin,'titi',1000));
    check(secret.LoadFromSecureBinary(bin,'toto',1000));
    check(chain.IsValid(secret)=ecvValidSigned);
    chain.Add(secret);
    check(chain.Count=4);
    sign := TECCSignatureCertified.CreateNew(secret,pointer(json),length(json));
    check(sign.Check);
    check(sign.AuthoritySerial=secret.Serial);
    check(sign.AuthorityIssuer=secret.Issuer);
    sav := sign.ToBase64;
    bin := sign.SaveToDERBinary;
    check(length(bin)>=ECC_BYTES*2+6);
    sign.Free;
    sign := TECCSignatureCertified.CreateFromBase64(sav);
    check(sign.Check);
    check(sign.Version=1);
    check(sign.Date=ECCText(NowECCDate));
    check(sign.AuthoritySerial=secret.Serial);
    check(sign.AuthorityIssuer='toto.com');
    check(sign.SaveToDERBinary=bin);
    check(chain.IsSigned(sign,pointer(json),length(json))=ecvValidSigned);
    signcontent := sign.Content;
    inc(signcontent.Signature[10]); // corrupt
    sign.Content := signcontent;
    check(sign.Check,'seems valid');
    check(chain.IsSigned(sign,pointer(json),length(json))=ecvInvalidSignature);
    dec(signcontent.Signature[10]);
    sign.Content := signcontent;
    check(chain.IsSigned(sign,pointer(json),length(json))=ecvValidSigned);
    check(chain.IsSigned(sav,pointer(json),length(json))=ecvValidSigned);
    dec(json[10]);
    check(chain.IsSigned(sign,pointer(json),length(json))=ecvInvalidSignature);
    check(chain.IsSigned(sav,pointer(json),length(json))=ecvInvalidSignature);
    chain.Clear;
    check(chain.Count=0);
    check(chain.IsSigned(sign,pointer(json),length(json))=ecvUnknownAuthority);
    sign.Free;
    selfsignedroot := TECCCertificateSecret.CreateFromSecureFile(
      '.',serial,'pass',1000);
    {$ifndef DELPHI5OROLDER}
    check(chain.LoadFromFile('test'));
    check(chain.Count=3);
    check(chain.IsValid(selfsignedroot)=ecvValidSelfSigned);
    check(selfsignedroot.IssueDate=ECCText(NowECCDate));
    check(selfsignedroot.Content.Signed.IssueDate=NowECCDate);
    check(chain.GetBySerial(serial)<>nil);
    chain.IsValidCached := true;
    check(ObjectToJson(chain)=jsonchain);
    {$endif}
    check(DeleteFile(selfsignedroot.SaveToSecureFileName));
    selfsignedroot.Free;
  finally
    chain.Free;
  end;
end;

{$ifndef DELPHI5OROLDER}
procedure TTestECCCryptography.ECCCommandLineTool;
var sw: ICommandLine;
    ctxt: TCommandLine;
    i: integer;
    previd,prevpass: RawUTF8;
    plainfn,rawfn: TFileName;
    keys: array of record
      priv,pub,test,crypt: TFileName;
      id,issuer,pass,text: RawUTF8;
      rounds: integer;
    end;
    exectemp: variant;
  function Exec(const nv: array of const; cmd: TECCCommand): PDocVariantData;
  var sw: ICommandLine;
      ctxt: TCommandLine;
  begin
    ctxt := TCommandLine.Create(nv);
    sw := ctxt;
    check(ECCCommand(cmd,sw)=eccSuccess);
    if CheckFailed(ctxt.ConsoleLines<>nil) then
      result := @DocVariantDataFake else begin
      exectemp := _JsonFast(ctxt.ConsoleLines[0]);
      result := _Safe(exectemp);
    end;
  end;
begin
  if DirectoryExists('synecc') then
    DirectoryDelete('synecc',FILES_ALL,true) else
    CreateDir('synecc');
  SetCurrentDir('synecc');
  try
    SetLength(keys,ECC_COUNT shr 4);
    for i := 0 to high(keys) do
    with keys[i] do begin
      formatUTF8('name%',[i],issuer);
      formatUTF8('pass%',[i],pass);
      rounds := 1000+i;
      ctxt := TCommandLine.Create([
        'auth',previd,'authpass',prevpass,'authrounds',rounds-1,
        'issuer',issuer,'days',30+i,'newpass',pass,'newrounds',rounds]);
      sw := ctxt;
      check(ECCCommand(ecNew,sw)=eccSuccess);
      if CheckFailed(ctxt.ConsoleLines<>nil) then
        exit;
      id := Trim(split(ctxt.ConsoleLines[high(ctxt.ConsoleLines)],'.'));
      priv := format('%s.private',[id]);
      pub := format('%s.public',[id]);
      previd := id;
      prevpass := pass;
      text := RandomTextParagraph(1000);
      test := format('test%d.txt',[i]);
      crypt := 'crypt-'+test;
      FileFromString(text,test);
      Exec(['file',test,'out',crypt,'auth',pub,'saltrounds',i+10],ecCrypt);
    end;
    sw := TCommandLine.Create([]);
    check(ECCCommand(ecChainAll,sw)=eccSuccess);
    for i := 0 to high(keys) do
    with keys[i] do begin
      with Exec(['auth',priv,'pass',pass,'rounds',rounds],ecInfoPriv)^ do begin
        check(I['Version']=1);
        check(U['Serial']=id);
        check(U['Issuer']=issuer);
      end;
      with Exec(['file',crypt],ecInfoCrypt)^ do begin
        check(I['Size']=length(text));
        check(U['recipient']=issuer);
        check(U['Recipientserial']=id);
        check(length(U['RandomPublicKey'])=sizeof(TECCPublicKey)*2);
        check(U['Algorithm']=ShortStringToAnsi7String(ToText(ecaPBKDF2_HMAC_SHA256_AES256_CFB_SYNLZ)^));
        check(O['Signature']^.VarType=varNull,'not signed');
        check(not B['Meta']);
      end;
      plainfn := 'plain-'+test;
      Exec(['file',crypt,'out',plainfn,'auth',priv,'authpass',pass,
        'authrounds',rounds,'saltrounds',i+10],ecDecrypt);
      check(StringFromFile(plainfn)=text);
      Exec(['file',test,'out',crypt,'auth',id,'pass',pass,'rounds',rounds],ecSign);
      Exec(['file',test,'out',crypt,'auth',pub,'saltrounds',i+10,'algo',
        ord(ecaPBKDF2_HMAC_SHA256_AES128_CTR)],ecCrypt);
      rawfn := 'raw-'+test;
      with Exec(['file',crypt,'rawfile',rawfn],ecInfoCrypt)^ do begin
        check(I['Size']=length(text));
        check(U['recipient']=issuer);
        check(U['Recipientserial']=id);
        check(length(U['RandomPublicKey'])=sizeof(TECCPublicKey)*2);
        check(U['Algorithm']='ecaPBKDF2_HMAC_SHA256_AES128_CTR');
        check(O['Signature']^.I['Version']=1,'signed');
        check(O['Signature']^.U['AuthoritySerial']=id);
        check(B['Meta']);
      end;
      check(PosEx(StringFromFile(rawfn),StringFromFile(crypt))=sizeof(TECIESHeader)+1);
      DeleteFile(plainfn);
      Exec(['file',crypt,'out',plainfn,'authpass',pass,'authrounds',rounds,
        'saltrounds',i+10],ecDecrypt);
      check(StringFromFile(plainfn)=text,'guess .private from header');
    end;
  finally
    SetCurrentDir('..');
  end;
end;
{$endif}

procedure TTestECCCryptography.ECDHEStreamProtocol;
const MAX = 10000;
var //timer: TPrecisionTimer;
    str: TRawByteStringDynArray;
  function Test(const prot: IProtocol; const name: string): integer;
  var i: integer;
      enc,after: RawByteString;
      ref: IProtocol; // to release memory
  begin
    ref := prot;
    result := 0;
    //timer.Start;
    for i := 0 to MAX do begin
      prot.Encrypt(str[i],enc);
      inc(result,length(str[i])+length(enc));
      check(length(enc)>=length(str[i]));
      check(prot.Decrypt(enc,after)=sprSuccess);
      check(after=str[i]);
    end;
    //fRunConsole := format('%s %s %s',[fRunConsole,name,KB(timer.PerSec(result))]);
  end;
var key: THash256;
    a: TECDHEAuth;
    c: TECDHEProtocolClient;
    s: TECDHEProtocolServer;
    cs, ss: TECCCertificateSecret;
    i: integer;
    enc,after: RawByteString;
  procedure handshake;
  var cf: TECDHEFrameClient;
      sf: TECDHEFrameServer;
  begin
    c := TECDHEProtocolClient.Create(a,nil,cs);
    s := TECDHEProtocolServer.Create(a,nil,ss);
{    c.EF := efAesCfb128;
    c.MAC := macHmacCrc32c;
    s.EF := c.EF;
    s.MAC := c.MAC; }
    c.ComputeHandshake(cf);
    Check(s.ComputeHandshake(cf,sf)=sprSuccess);
    Check(c.ValidateHandshake(sf)=sprSuccess);
  end;
begin
  SetLength(str,MAX+1);
  for i := 0 to MAX do
    str[i] := RandomString(i shr 3+1);
  Test(TProtocolNone.Create,'none');
  TAESPRNG.Main.FillRandom(key);
  Test(TProtocolAES.Create(TAESCFB,key,128),'aes');
  cs := TECCCertificateSecret.CreateNew(nil,'client');
  ss := TECCCertificateSecret.CreateNew(nil,'server');
  for a := low(a) to high(a) do begin
    handshake;
    for i := 0 to MAX do begin
      c.Encrypt(str[i],enc);
      check(s.CheckError(enc)=sprSuccess);
      check(s.Decrypt(enc,after)=sprSuccess);
      check(after=str[i]);
      if i and 7=0 then
         continue; // check asymmetric communication
      s.Encrypt(str[i],enc);
      check(c.CheckError(enc)=sprSuccess);
      check(c.Decrypt(enc,after)=sprSuccess);
      check(after=str[i]);
      if i and 3=0 then
         continue;
      c.Encrypt(str[i],enc);
      check(s.CheckError(enc)=sprSuccess);
      check(s.Decrypt(enc,after)=sprSuccess);
      check(after=str[i]);
      c.Encrypt(str[i],enc);
      inc(enc[2]);
      check(s.CheckError(enc)=sprInvalidMAC);
      check(s.Decrypt(enc,after)=sprInvalidMAC);
    end;
    c.Free;
    s.Free;
    handshake;
    Test(c,format('c%d',[ord(a)]));
    Test(s,format('s%d',[ord(a)]));
  end;
  cs.Free;
  ss.Free;
end;

{$ifdef MSWINDOWS}
{$ifndef FPC}
{$ifndef LVCL}

{ TTestSynopsePDF }

const
  FIXED_DATE = 40339.803675; // forced date to have the exact same Hash32 value

procedure TTestSynopsePDF._TPdfDocument;
var MS: THeapMemoryStream;
    i,y: integer;
    embed: boolean;
    expected: cardinal;
    WS: SynUnicode;
const
  Hash: array[boolean] of Cardinal =
    (2336277040,1967009088);
  Hash10: array[boolean] of Cardinal =
    (2379006506,1967009088);
  Name: array[boolean] of PDFString =
    ('Arial','Helvetica');
begin
  MS := THeapMemoryStream.Create;
  with TPdfDocument.Create do
  try
    for embed := false to true do begin
      Info.CreationDate := FIXED_DATE; // no FIXED_DATE nor creator variation in hashed value
      Info.Data.PdfTextByName('Producer').Value := 'Synopse PDF engine';
      StandardFontsReplace := embed;
      AddPage;
      Canvas.SetFont('arial',10,[]);
      Check(Canvas.Page.Font.Name=Name[embed]);
      y := 800;
      for i := 1 to 30 do begin
        Canvas.SetFont('Arial',9+i,[]);
        WS := 'Texte accentue n.'+IntToString(i);
        PWordArray(WS)^[13] := 233;
        PWordArray(WS)^[16] := 176;
        Canvas.TextOutW(100,y,pointer(WS));
        dec(y,9+i);
      end;
      SaveToStream(MS,FIXED_DATE);
      //MS.SaveToFile(ChangeFileExt(ExeVersion.ProgramFileName,'.pdf'));
      if OSVersion<wTen then
        expected := Hash[embed] else
        expected := Hash10[embed];
      Check(Hash32(MS.Memory,MS.Position)=expected);
      if not embed then begin
        if CharSet<>ANSI_CHARSET then
          break; // StandardFontsReplace will work only with ANSI code page
        NewDoc;
        MS.Clear;
      end;
    end;
  finally
    Free;
    MS.Free;
  end;
end;

procedure TTestSynopsePDF._TPdfDocumentGDI;
const
  EMF: RawByteString = // some compressed EMF file content
  'gDUBAAAAAABwgLjg3Z0LkBXVmcfPGd4KCqNRNCwBBaPDw1EU8RFkZlY2rkJQRkWBACIjRJFHYHaWkDgl'+
  'RLMkQTTo6hbZ4CMlURfQJXHjWuXsrsoaWUUqUckaJSWV4Got+MJVMLPnu6f/PX0/7wwX7vdxv0pTh+7T'+
  '/+5zfqdPT79uf/33zrkbXdtwS4VzY3xb/vZJzvUZ6tyAi8aNdc675rO9qz/WuZ6ODV1C6uzcM2HdOT5f'+
  'euutrq7fsi7ui2G6d2b+5SHN91Sqcx+1trZm11kVy8j9P85NdANCmuFuct/MTc1yC90c18AZOhg2djvX'+
  'b/qk0k3eWJfL0zTNm//Red4PHuV7v1QTF1x/oR+zqsav+79K98K+Grc/LLcjjHeFNChMj/lJjR8axlvD'+
  'cl8I7aX1J4T1c+VcELZc0Na9Wee6TZy1cM6sb474xR4q90JPdfVLNFqWUu+7a/yEUA7Np2HtnK/4pk8r'+
  '3WM76tyln7bNc67SPTekLtXRBprO6S/XuTF3TG+keQ+Gsmb+dErjf4bxlJD+Msy7rtF1J22wi/1GG7Wv'+
  'a+vDU0LqF1JFks92H9Yh7dhkuldIX07K6JQsh+7D8jT/hGSatMpMOdllLiW+TH18/1sW8jUhP9DtySzV'+
  'fGHh6cJDtj5Mh921GdOhPc3YF0OXfm5ffKqibbPU5Pa8GXl/Mwc3TG2kNnfNzDl19HC3a0+lG/etqFEe'+
  'Q8H23xv7uv2hJUnO7Xqv0v33kqmNh16md5Nmnx7L2kf7cml/A3xfpO2d3Rez/XNC2/xW7D8+s0y23+jw'+
  '09ExpPR+c25K6KPrQqK/tfoPKt1Hv+m8eGmXhf5Pod7dizot/v4TUxr7zhjubh85wm1tGOZ29B2WW4/m'+
  'NY0Y5k6cGftgTyjj5Y1h2V4D39jY8+rcsr5AfaPer3TLXpuS9gstQ8vS/LVhPnEsveevcsexDSGf+7sO'+
  'Ze4MG/XGsAxpbUNp/cv7jbZ3oX7L9gnt4+38LeWGw9UnW2fGPqF+WDUntpXmUZ9sm1W4T2jZYvuElj2Y'+
  'PsFyzpXWd7xPaHsfqE+6uY7/TupDf8x289zcMD60oTLH8e7+ytzBvlNWohr8mFhnOHet21eZ46Mxncdo'+
  '+t0w/9sLalz3O2r8fXW17mch0fzmk8b4k1+vc8eGZc8L49qQWsL0pa/HcyHKooHKGhry//haTd51ClU/'+
  'KczffeuExvv98XkaTf/XI0++ni1rdhgv39d2Ht4app//3oTGH4Y0cO30Rf/y0vzccls+a20dlJTP+4S2'+
  'd3vHt2KOddnzZvZ8OiQp9woXz8fzwwKVPr/Pi/lbLcRzKMfk41zhfbA+pNWOjv7hOiOk5SGNSK753Jhe'+
  'ta5zr9prE43WudTlD63JcF1Om+eud5PD9cKs3JXgHLfI/a2bmtvGSJ0z00idkvldkzFYi9kW7W27Q90W'+
  'uRT6qm9Y8OowHsy2xVWJ5jvYFhPC9JlJ+YXaUuy+dSDO3T6yfOg/z/m+L47zjNBn2pzfT1hWFeBcWSRn'+
  'tfL2nOQiG7EML8BZlWgVB+AcFrYo9mVN1hYXeZ53n2d91hXLeuZhYZ2S8MwqwDqjaNYRHbKG40gz7pO6'+
  'J1rmUiZdjrTsfRLueek4TcfnkUmeH6+z5fdIysT1R3Nzc7pcj3bKp+PGMZnyKX+8b9Pp7zWrUz6rr2T6'+
  'SqZXMb2K6bRPZHXKZ/UZTJ+R6IWu67Pb4ohkOxfaFkd0sC0on90WlWxbZHXKV7JtkdVXMr2K6VVMf9bl'+
  '68+6fH0G02ckeqF75G6Zv5GO+r4P6/s+rL1ZHflse7P6SqZXMb2K6dS+rI58tr1ZfQbTj2T7LuW/kNEH'+
  'MX0Q00czfTTTH2L7LuWzegvTW5i+nenbmV7H9DqmT2H6FKYvZvpipr/h8nXKZ/WPmf4x0/uw8vuw8pew'+
  '9Zew9X/E9B8x/VGmP5roha6t2rsebu/6q717mmLOF0e5tmeMR7q2+57lnWLCvSgxlX4vWt570HBVUdI9'+
  'aNgrS7oH5fc9tL35uXqTi8eta0M6MaSnXP65mvan20Pq49o/V88P0xeFXprlZoar/4Xh6vImNyDcrc4L'+
  'pc4J8+M9Qdt1f1fXdt2fvTcg7QiWR8J1QHYfPDXTroO5f+jo/u9A+29FgW24PKT+yfzxIeG+E9vwa4nW'+
  'o4NtON3Rdflad1a46q0O+0117l9sZ3abYbtl8xLXcR21qypp18gC7aoqsl0j8to130S7Lkja9dUC7bqg'+
  'yHadabC/xifpmgLtGl9ku84w2K7pCf+NBdo1vch2Vee1q6as7Wpy8TebhUm7vlOgXaR166BddJ91bl6b'+
  'qsraluVJW1YVaMvyItoyqsB+V6623J205f4Cbbm7iLacY6gt65K2bCrQlnVFtGVkXlsqy9qWJ5O2PFeg'+
  'LU8W0ZazDfXLlqQtrxVoy5Yi2pJ/vdDaWs62vJG05X8KtOWNItoywtBxbE/Slv0F2rKniLYUui4oV1sq'+
  'fGzLUf7zbSHtQG0pdC1wMG3JPi/p6Flc9vkLns0dzDMXanP/TBkTknxWr2Z6NdNrmV7L9Hqm1zO9gekN'+
  'TG9iehPTVzB9BdPXMH0N09czfT3TW5jewvRtTN/G9J1M38n0vUzfy/TuPl/v3sHzxgM9U7yC9Tc9v+3P'+
  '9GqmVzO9lum1TK9nej3TG5jewPQmpjcxfQXTVzB9DdPXMH0909czvYXpLUzfxvRtTN/J9J1M38v0vUzP'+
  '9jfp2f4+2GeqV7L+Rj6rVzO9mum1TK9lej3T65newPQGpjcxvYnpK5i+gulrmL6G6euZvp7pLUxvYfo2'+
  'pm9j+k6m72T6XqbvZXq2v5HP/v2fmFn/8iSf1Xv6fL0nW/94tv7xbP2j2fpHs/WPYesfw9bvzfTeTB/K'+
  '9KFMP43ppzF9MNMHM/0kpp/E9NFMH83085l+PtPPYfo5TD+L6WcxfQLTJzB9HNPHMf1ipl/M9LFMH8v0'+
  'a5l+LdOnMX0a0yczfTLTJzF9EtMXMX0R0xcwfQHT5zJ9LtO/wfRvMP1Wpt/K9GVMX8b0m5l+M9OXMn0p'+
  '0+9h+j1Mv4vpdzH9DqbfwfSVTF/J9IeZ/jDTH2L6Q0x/gOkPMH0t09cy/SmmP8X0XzL9l0z/OdN/zvTH'+
  'mf44019k+otMf4HpLzB9M9M3M/0Zpj/D9B1M38H03zH9d0zfzvTtTH+F6a8w/X2mv8/03UzfzfR3mP4O'+
  '03cxfRfTO7PzS2d2fvFM90z/jJX/GSv/U6ZT/pKQxrh4LUPXyAOSsXdt51n6XW1+8hszzxe6xsreX7b3'+
  'Hjn9rpIbkjcTs78jpfejrfnXc1XJujj3T3XxHJytN/u7jURddIzv5uO12FHKdc1K6rrxMNTVkNQ19zDU'+
  'RXV09fF81ku5rgVJXU2Hoa5vJXXd8mdW1y1JXX93GOqic3QXH8/1PZXr+rGLx9QHaXnluu5L6lp3GOr6'+
  'wMVzNR3Pf+N066J3TbaG9KeQXlWu64iw3Ta6eJ55Qrmu3sl+Qe+oPaJcV79Qx51hfHIY36tc10Afr3NP'+
  '9fF6WbMues91cRif7eO1vWZdtT7GkV0cxjco13WZT+7Xwvhq5bom+nitdo2P94+addF7+Je6GL85Ubmu'+
  'G3x8HrbQx/tyzbpu9vR7T7h/DePzlOtq9vQbhnO3efp9Wbcueu+d3hy709PvjLp13e1jOT9O6tSsa2Oo'+
  '4+QwfsLHeZp10bucNN4Rxl9SrusdH3+zovdv/0K5LordoOeVe318bq1Z16c+Ptv0FfEZq2Zdp4bM0WF8'+
  'ekXclpp11VXEe9u/pveJleuaVBHnT6uIy2rWdX1FvJe/qSLuI3TP3zsprouTewbA2bPPBvB+P9qRvufa'+
  'nN+W7LOCbLnt/UaN5xv0PlqvzPMRejaQvZ/o1g5bl8PABhawTTPEBhaw0TPvI42wgQVsswyxgQVs1xti'+
  'AwvY6Dn/EUbYwAK2uYbYwAK2eYbYwAK2hYbYwAI2es7XwwgbWMD2N4bYwAK2JYbYwAK2pYbYwAK2mw2x'+
  'gQVstxhiAwvYlhtiAwvYbjPEBhawrTDEBhaw/cAQG1jARrFeVs5ZYAHbnYbYwAK21YbYwAK2vzfEBhaw'+
  '3WuIDSxgW2OIDSxg+4khNrCA7T5DbGABG/1+ZOXYCxawPWSIDSxg+5khNrCA7VFDbGAB2wYX34OxwAYW'+
  'sD1miA0sYNvk4rsvFtjAArYnDLGBBWz0vmFXI2xgARu9K9nFCBtYwPa0i++CWGADC9j+LaRORtjAAjZ6'+
  'P7TCCBtYwJaLDzXCBhawPZ9UYoENLGDb4uK7QxbYwAI2eofqMyNsYAHbyyHtM8IGFrD9OqRPjLCBBWz0'+
  'jvfHRtjAAjZ6P/0jI2xgARt9GPUDI2xgARvFe+8xwgYWsP0+pP81wgYWsFG83TtG2MACtj+E9LYRNrCA'+
  '7e1kngU2sICNtuFbRtjAAjba9940wgYWsL3n4nHFAhtYwEbHuu1G2MACtr0u/x3zcrKBBWx03t9mhA0s'+
  'YKPrpReNsIEFbHQN/CsjbGABG93XbDbCBhaw0b3qfxhhAwvY6NnI00bYwAI2eob5r0bYwAI2em8qG2tS'+
  'TjawgI3i9P7ZCBtYwEbvgW4wwgYWsNG3ah8xwgYWsB3n4+8hFtjAAjb6jvoDRtjAArYvhvxaI2xgAVt/'+
  'H39TtcAGFrAN8DHW0gIbWMBG8XCrjbCBBWzkV7LKCBtYwEaxfT8wwgYWsA3xMY7YAhtYwEbxit81wgYW'+
  'sFX7+L6eBTawgI38fL5jhA0sYBvpY6y8BTawgG2Uj+/8WmADC9jO9/FdaQtsYAEbOWIuMMIGFrCNCfmb'+
  'jLCBBWzkS3GDETawgO2ikJ9thA0sYPuqT7yFDLCBBWyX+BgXZYENLGAb5+O30i2wgQVsE3yMw7PABhaw'+
  'Ucz/NUbYwAK2K3ziZWaADSxgo+8xXGGEDSxgm+zzv6lQTjawpLGxIX+ZETawgG16yH/NCBtYwDbTx29O'+
  'WmADSxob62MctwU2sIBtts//5kc52cCSxsZ6ckSywQaWNDbWk/eRDTawgI08tuqMsIEljY315O9lgw0s'+
  'YCOvtzFG2MACtiYfv/lrgQ0saWysj75JFtjAArZv+/gtZAtsYAFbs8//BlI52cCSxsb6/G8mlTU2NmEB'+
  '23d9/H61BTawgO17Pv/7T+VkA0saG+vjd70tsIEFbD/08btgFtjAksbG+ujxbYENLGlsrI/fOLPABhaw'+
  '3eXjd4sssIEljY0N+dONsIEFbP/g87/rVk42sICNvjk3zAgbWNLYWB99GSywgQVs94f8ECNsYAHbT33+'+
  't/3KyQaWNDbWRy8NC2xgAdvDPnrRWmADC9j+ycfv01lgA0saG2uIbQNjezzkTzHCBpY0NtbH9S2wgSWN'+
  'jTXE9gRje9JHb2YLbGBJY2N9/L6qBTawpLGxhtieZmz/7qNHkgU2sKSxsSE/0AgbWMC22RDbZsb2q5Af'+
  'YIQNLGlsrCG2LYztJZ//veRysoEFbNsMsW1jbL/2+d9jLmtsrM/3OHzVENurjO23hth+y9he9/nf8y5r'+
  'bGzCArY3DbG9ydh+72N9FtjAksbGGmLbydj+aIjtj4ztbR81C2xgAdu7htjeZWz0vf7st/PLyQaWNDbW'+
  'ENt7jO1DQ2wfMraPDbF9zNg+8fk+A2WNjU1YwLbfENt+xtZqiK2VsZH/Rl8jbGABW2dDbJ0ZW1dDbF0Z'+
  'Ww9DbD0YW8/EP8QCG1jS2FhDbEcxtj6G2PowtmMNsR3L2I4zxHYcYzuB+RGVkw0sYOtniK0fY+tviK0/'+
  'YxtoiG0gYxtkiG0QYzuF+WWVNTY2YQHbaYbYTmNsQw2xDWVsww2xDWdsZxhiO4OxjTDENoKxjayIZVlg'+
  'AwvYzjXEdi5jO98Q2/mMbbQhttGMrcYQWw1jqzPEVsfYxhpiG8vYLq6I3p8W2MCSxsYaYruEsY03xDae'+
  'sV1miO0yxjbRENtExnalIbYrGdskQ2yTGNtkQ2yTGdvXDbF9nbFNTzyDLbCBJY2NNcQ2k7E1GGJrYGyz'+
  'k/wlSd7FcsQ8mQ+lra1Opq0ansxSbBqezFJsGp7MUmwansxSbGAB2/WG2MCSxp270n3MpNg0PJml2MAC'+
  'NglPZik2DU9mKTawgG2RK93HTIoNLGCT8GSWYtPwZJZi0/BklmIDC9gkPJml2DQ8maXYNDyZpdjAArbb'+
  'DLGBJY07N8Sm4cksxQaWNO7c2TlnaXgyS7GBBWyrDbGBJY07N8Sm4cksxQYWsK0xxAaWNO7cEJuGJ7MU'+
  'G1jA9qCzc+wFSxp3bogNLGCT8GSWYtPwZJZiA0sad+5K9z2WYtPwZJZi0/BklmIDSxp3bohNw5NZig0s'+
  'adw5rWeEDSxp3Lkr3ZNZik3Dk1mKDSxp3Lkr3ZNZig0sYHuOdCNsYAGbhCezFJuGJ7MUG1jAJuHJLMWm'+
  '4cksxabhySzFBhawveJK92SWYgML2La70j2ZpdjAksadu9I9maXYwAI2CU9mKTYNT2YpNrCkceeudE9m'+
  'KTawgO0PrnRPZik2sKRx5650T2YpNg1PZik2sIBNwpNZik3Dk1mKTcOTWYpNw5NZig0sady5K92TWYoN'+
  'LGCj66VSPZml2MACNglPZik2DU9mKTYNT2YpNg1PZik2DU9mKTYNT2YpNg1PZik2DU9mKTYNT2YpNg1P'+
  'Zik2DU9mKTYNT2YpNg1PZik2DU9mKTawgE3Ck1mKTcOTWYpNw5NZik3Dk1mKTcOTWYpNw5NZik3Dk1mK'+
  'TcOTWYpNw5NZik3Dk1mKTcOTWYpNw5NZig0sady5L92TWYpNw5NZik3Dk1mKTcOTWYpNw5NZik3Dk1mK'+
  'TcOTWYpNw5NZik3Dk1mKTcOTWYpNw5NZLDbWy3syS7GBJY0796V7MkuxaXgyi8XGenlPZik2DU9msdhY'+
  'L+/JLBYb6+U9maXYNDyZxWJjvbwnsxSbhiezFJuGJ7NYbKyX92SWYtPwZJZiA0saG+tL92QWi4318p7M'+
  'UmwansxSbBqezGKxsV7ek1mKTcOTWSw21st7MovFxnp5T2YpNg1PZrHYWC/vySzFpuHJLMWm4cksFhvr'+
  '5T2Zpdg0PJml2DQ8mcViY728J7MUm4YnsxSbhiezWGysl/dklmLT8GQWi41NWNLYWENsGp7MUmwansxi'+
  'sbFe3pNZLDbWy3syS7FpeDKLxcZ6eU9mKTYNT2YpNg1PZrHYWMYm4cksxabhySzFpuHJLBYb6+U9maXY'+
  'NDyZpdg0PJnFYmO9vCezFJuGJ7NYbGzCksbGGmLT8GSWYtPwZBaLjU1YwCbhySzFpuHJLMWm4cksFhvr'+
  '5T2Zpdg0PJml2DQ8mcViY728J7MUm4YnsxSbhiezWGysgiezFJuGJ7NYbKyCJ7NYbKyCJ7MUm4Yns1hs'+
  'rIIns1hsrIInsxSbhiezWGysgiezFJuGJ7MUm4Yns1hsrIInsxSbhiezFJuGJ7NYbKyCJ7MUm4YnsxSb'+
  'hiezWGysgiezFJuGJ7NYbKyCJ7NYbKyCJ7MUm4Yns1hsrIInsxSbhiezFJuGJ7NYbKyCJ7MUm4YnsxSb'+
  'hiezWGysgiezFJuGJ7MUm4Yns1hsrIInsxSbhiezWGysgiezWGysgiezFJuGJ7NYbKyCJ7NYbKyCJ7MU'+
  'W7GezMWwYjowNF/uYhwkLfNBa2urywyrfFtT6t0MN9vNc3PD+NCG6Y1TrprWiAIr0vlD3I6Fw9woP8Q1'+
  'jz/dDXej3ZClzk14r9LVXTW1kca7FgzLrfNaWG5KyG/40tuNtM7LvavpM665+Q+G+dsXdVoc3x5vK5fK'+
  'WP0ebanRbmgol+oYnDQrW+6mTLl3Hln9FZS7n5W7K+Gi8ZIw3vqLIZ2yZd44wufWeWzS1Ebaxn17Dfzy'+
  'CW6jv67RdXcuf1/o69I+T+fTdim0j3Ry8TccmqZuqswsX2i+d/n7Qnv9f1RSHw29k+n/Bw==';
  METAFILE_HASH: array[boolean] of Cardinal = ($212C0E5A,$FB81AAAD);
var S: RawByteString;
    MS: THeapMemoryStream;
    MF: TMetaFile;
    Doc: TPdfDocument;
    Page: TPdfPage;
    orientation: boolean;
    H: cardinal;
    i,j: integer;
//    E: RawByteString; i,L,n: integer;
begin
{  S := SockBase64Encode(CompressString(StringFromFile('d:\temp\tmpCurve.emf')));
  E := '  EMF: RawByteString = // some compressed simple EMF file'#13#10;
  L := length(S);
  i := 1;
  while L>0 do begin
    if L>80 then
      n := 80 else
      n := L;
    E := E+'  '''+copy(S,i,n)+'''+'#13#10;
    dec(L,n);
    inc(i,n);
  end;
  FileFromString(E,'test.pas');}
  S := UncompressString(Base64ToBin(EMF));
  Check(Hash32(S)=$5BB4C8B1);
  MS := THeapMemoryStream.Create;
  try
    with TPdfDocument.Create do
    try
      Info.CreationDate := FIXED_DATE; // force fixed date and creator for Hash32()
      Info.Data.PdfTextByName('Producer').Value := 'Synopse PDF engine';
      //CompressionMethod := cmNone; useful for debugg purposes of metafile enum
      AddPage;
      MF := TMetaFile.Create;
      try
        MS.Write(pointer(S)^,length(S));
        MS.Position := 0;
        MF.LoadFromStream(MS);
        Canvas.RenderMetaFile(MF);
        Check(Canvas.Page.Font.Name='Tahoma');
      finally
        MF.Free;
      end;
      MS.Clear;
      SaveToStream(MS,FIXED_DATE);
      // force constant Arial,Bold and Tahoma FontBBox
      SetString(s,PAnsiChar(MS.Memory),MS.Position);
      MS.SaveToFile(ChangeFileExt(ExeVersion.ProgramFileName,'.pdf'));
      if (GetACP<>1252) {$ifdef CPU64}or true{$endif} then
        Check(length(s)>6500) else begin
        i := PosEx('/FontBBox[',s);
        if CheckFailed(i<>0) then exit;
        FillCharFast(s[i],32,32);
        j := PosEx('/FontBBox[',s);
        if CheckFailed(j<>0) then exit;
        FillCharFast(s[j],32,32);
        i := PosEx('/FontBBox[',s);
        if CheckFailed(i<>0)then exit;
        FillCharFast(s[i],32,32);
        H := Hash32(s);
        Check(H=$FE2D27CA);
      end;
    finally
      Free;
    end;
    MF := TMetafile.Create;
    try
      // create test metafile
      MF.Width := 700;
      MF.Height := 700;
      with TMetafileCanvas.Create(MF, GetDC(0)) do
      try
        MoveTo(0, 0);
        LineTo(700, 700);
        MoveTo(0, 700);
        LineTo(700, 0);
      finally
        Free;
      end;
      // create page in portrait/landscape orientation, and render metafile to it
      for orientation := false to true do begin
        Doc := TPdfDocument.Create;
        try
          Doc.GeneratePDF15File := True;
          Doc.Info.CreationDate := FIXED_DATE; // force fixed date for Hash32()
          Doc.Info.Data.PdfTextByName('Producer').Value := 'Synopse PDF engine';
          Doc.DefaultPaperSize := psA4;
          Page := Doc.AddPage;
          Page.PageLandscape := orientation;
          MS.Clear;
          Doc.Canvas.RenderMetaFile(MF);
          Doc.SaveToStream(MS,FIXED_DATE);
          H := Hash32(MS.Memory,MS.Position);
          Check(H=METAFILE_HASH[orientation]);
        finally
          Doc.Free;
        end;
      end;
    finally
      MF.Free;
    end;
  finally
    MS.Free;
  end;
end;
{$endif}
{$endif}
{$endif}

const
  UTF8_E0_F4_BYTES: array[0..5] of byte = ($E0,$E7,$E8,$E9,$EA,$F4);
var
  _uE0,_uE7,_uE8,_uE9,_uEA,_uF4: RawUTF8;

{$ifndef DELPHI5OROLDER}

{ TTestSQLite3Engine }

function TTestSQLite3Engine.OnBackupProgress(Sender: TSQLDatabaseBackupThread): Boolean;
begin
  BackupProgressStep := Sender.Step;
  result := true;
end;

procedure InternalSQLFunctionCharIndex(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); cdecl;
var StartPos: integer;
begin
  case argc of
  2: StartPos := 1;
  3: begin
    StartPos := sqlite3.value_int64(argv[2]);
    if StartPos<=0 then
      StartPos := 1;
  end;
  else begin
    ErrorWrongNumberOfArgs(Context);
    exit;
  end;
  end;
  if (sqlite3.value_type(argv[0])=SQLITE_NULL) or
     (sqlite3.value_type(argv[1])=SQLITE_NULL) then
    sqlite3.result_int64(Context,0) else
    sqlite3.result_int64(Context,SynCommons.PosEx(
      sqlite3.value_text(argv[0]),sqlite3.value_text(argv[1]),StartPos));
end;

{$ifdef UNICODE}
{$WARNINGS OFF} // don't care about implicit string cast in tests
{$endif}

{.$define WITHUNSAFEBACKUP}
{ define this if you really need the old blocking TSQLRestServerDB backup methods
  - those methods are deprecated - you should use DB.BackupBackground() instead
  - should match mORMotSQLite3.pas unit }

const // BLOBs are stored as array of byte to avoid any charset conflict
  BlobDali: array[0..3] of byte = (97,233,224,231);
  BlobMonet: array[0..13] of byte = (224,233,231,ord('d'),ord('s'),ord('j'),
        ord('d'),ord('s'),ord('B'),ord('L'),ord('O'),ord('B'),ord('2'),ord('3'));

procedure TTestSQLite3Engine.DatabaseDirectAccess;
procedure InsertData(n: integer);
var i: integer;
    s: string;
    ins: RawUTF8;
    R: TSQLRequest;
begin
  // this code is a lot faster than sqlite3 itself, even if it use Utf8 encoding:
  // -> we test the engine speed, not the test routines speed :)
 ins := 'INSERT INTO People (FirstName,LastName,Data,YearOfBirth,YearOfDeath) VALUES (''';
 for i := 1 to n do begin
   str(i,s);
   // we put some accents in order to test UTF-8 encoding
   R.Prepare(Demo.DB,ins+'Salvador'+RawUTF8(s)+''', ''Dali'', ?, 1904, 1989);');
   R.Bind(1,@BlobDali,4); // Bind Blob
   R.Execute;
   Demo.Execute(ins+'Samuel Finley Breese'+s+''', ''Morse'', ''a'+_uE9+_uE0+_uE7+''', 1791, 1872);');
   Demo.Execute(ins+'Sergei'+s+''', ''Rachmaninoff'', '''+_uE9+'z'+_uE7+'b'', 1873, 1943);');
   Demo.Execute(ins+'Alexandre'+s+''', ''Dumas'', '''+_uE9+_uE7+'b'', 1802, 1870);');
   Demo.Execute(ins+'Franz'+s+''', ''Schubert'', '''+_uE9+_uE0+_uE7+'a'', 1797, 1828);');
   Demo.Execute(ins+'Leonardo'+s+''', ''da Vin'+_uE7+'i'', ''@'+_uE7+'b'', 1452, 1519);');
   Demo.Execute(ins+'Aldous Leonard'+s+''', ''Huxley'', '''+_uE9+_uE0+''', 1894, 1963);');
   R.Prepare(Demo.DB,ins+'Claud'+_uE8+s+#10#7''', ''M'+_uF4+'net'', ?, 1840, 1926);');
   R.Bind(1,@BlobMonet,sizeof(BlobMonet)); // Bind Blob
   R.Execute;
   R.Prepare(Demo.DB,'INSERT INTO People (FirstName,LastName,Data,YearOfBirth,'+
     'YearOfDeath) VALUES (?,?,?,?,?)');
   R.BindS(1,'Albert'+s);
   R.BindS(2,'Einstein');
   R.Bind(3,_uE9+_uE7+'p');
   R.Bind(4,1879);
   R.Bind(5,1955);
   R.Execute;
//   Demo.Execute(ins+'Albert'+s+''', ''Einstein'', '''+_uE9+_uE7+'p'', 1879, 1955);');
   Demo.Execute(ins+'Johannes'+s+''', ''Gutenberg'', '''+_uEA+'mls'', 1400, 1468);');
   Demo.Execute(ins+'Jane'+s+''', ''Aust'+_uE8+'n'', '''+_uE7+_uE0+_uE7+'m'', 1775, 1817);');
 end;
end;
var
  SoundexValues: array[0..5] of RawUTF8;
  Names: TRawUTF8DynArray;
  i,i1,i2: integer;
  Res: Int64;
  id: TID;
  password, s: RawUTF8;
  R: TSQLRequest;
begin
  Check(JSONGetID('{"id":123}',id) and (id=123));
  Check(JSONGetID('{"rowid":1234}',id) and (id=1234));
  Check(JSONGetID(' { "id": 123}',id) and (id=123));
  Check(JSONGetID(' { "ROWID": 1234}',id) and (id=1234));
  Check(JSONGetID('{id:123}',id) and (id=123));
  Check(JSONGetID('{rowid:1234}',id) and (id=1234));
  Check(not JSONGetID('{"id":0}',id));
  Check(not JSONGetID('{"id":-10}',id));
  Check(not JSONGetID('{"id":null}',id));
  Check(not JSONGetID('{"ROWID":null}',id));
  Check(not JSONGetID('{id:0}',id));
  Check(not JSONGetID('{id:-10}',id));
  Check(not JSONGetID('{"ide":123}',id));
  Check(not JSONGetID('{"rowide":1234}',id));
  Check(not JSONGetID('{"as":123}',id));
  Check(not JSONGetID('{"s":1234}',id));
  Check(not JSONGetID('"ide":123}',id));
  Check(not JSONGetID('{ "rowide":1234}',id));
  if ClassType=TTestMemoryBased then
    TempFileName := SQLITE_MEMORY_DATABASE_NAME else begin
    TempFileName := 'test.db3';
    DeleteFile(TempFileName); // use a temporary file
    {$ifndef NOSQLITE3ENCRYPT}
    if ClassType<>TTestFileBasedMemoryMap then
      // memory map is not compatible with our encryption
      password := 'password1';
    {$endif}
  end;
  EncryptedFile := (password<>'');
  Demo := TSQLDataBase.Create(TempFileName,password);
  Demo.Synchronous := smOff;
  Demo.LockingMode := lmExclusive;
  if ClassType=TTestFileBasedMemoryMap then
    Demo.MemoryMappedMB := 256; // will do nothing for SQLite3 < 3.7.17
  R.Prepare(Demo.DB,'select mod(?,?)');
  for i1 := 0 to 100 do
    for i2 := 1 to 100 do begin
      R.Bind(1,i1);
      R.Bind(2,i2);
      check(R.Step=SQLITE_ROW);
      check(R.FieldInt(0)=i1 mod i2);
      R.Reset;
    end;
  R.Close;
  SoundexValues[0] := 'bonjour';
  SoundexValues[1] := 'bonchour';
  SoundexValues[2] := 'Bnjr';
  SoundexValues[3] := 'mohammad';
  SoundexValues[4] := 'mohhhammeeet';
  SoundexValues[5] := 'bonjourtr'+_uE8+'slongmotquid'+_uE9+'passe';
  for i1 := 0 to high(SoundexValues) do begin
    s := FormatUTF8('SELECT SoundEx("%");',[SoundexValues[i1]]);
    Demo.Execute(s,res);
    Check(res=SoundExUTF8(pointer(SoundexValues[i1])),s);
  end;
  for i1 := 0 to high(SoundexValues) do begin
    s := FormatUTF8('SELECT SoundExFr("%");',[SoundexValues[i1]]);
    Demo.Execute(s,res);
    Check(res=SoundExUTF8(pointer(SoundexValues[i1]),nil,sndxFrench),s);
  end;
  for i1 := 0 to high(SoundexValues) do begin
    s := FormatUTF8('SELECT SoundExEs("%");',[SoundexValues[i1]]);
    Demo.Execute(s,res);
    Check(res=SoundExUTF8(pointer(SoundexValues[i1]),nil,sndxSpanish),s);
  end;
  Demo.RegisterSQLFunction(InternalSQLFunctionCharIndex,2,'CharIndex');
  Demo.RegisterSQLFunction(InternalSQLFunctionCharIndex,3,'CharIndex');
  for i1 := 0 to high(SoundexValues) do begin
    s := FormatUTF8('SELECT CharIndex("o","%");',[SoundexValues[i1]]);
    Demo.Execute(s,res);
    Check(res=PosEx('o',SoundexValues[i1]),s);
    s := FormatUTF8('SELECT CharIndex("o","%",5);',[SoundexValues[i1]]);
    Demo.Execute(s,res);
    Check(res=PosEx('o',SoundexValues[i1],5),s);
  end;
  Demo.UseCache := true; // use the cache for the JSON requests
  Demo.WALMode := InheritsFrom(TTestFileBasedWAL); // test Write-Ahead Logging
  Check(Demo.WALMode=InheritsFrom(TTestFileBasedWAL));
  Demo.Execute(
    ' CREATE TABLE IF NOT EXISTS People (' +
      ' ID INTEGER PRIMARY KEY,'+
      ' FirstName TEXT COLLATE SYSTEMNOCASE,' +
      ' LastName TEXT,' +
      ' Data BLOB,'+
      ' YearOfBirth INTEGER,' +
      ' YearOfDeath INTEGER); ');
  // Inserting data 1x without transaction ');
  InsertData(1);
  { Insert some sample data - now with transaction. Multiple records are
    inserted and not yet commited until the transaction is finally ended.
    This single transaction is very fast compared to multiple individual
    transactions. It is even faster than other database engines. }
  Demo.TransactionBegin;
  InsertData(1000);
  Demo.Commit;
  Req := 'SELECT * FROM People WHERE LastName=''M'+_uF4+'net'' ORDER BY FirstName;';
  Check(WinAnsiToUtf8(Utf8ToWinAnsi(Req))=Req,'WinAnsiToUtf8/Utf8ToWinAnsi');
  JS := Demo.ExecuteJSON(Req); // get result in JSON format
  FileFromString(JS,'Test1.json');
  Check(Hash32(JS)=$40C1649A,'Expected ExecuteJSON result not retrieved');
  {$ifndef NOSQLITE3ENCRYPT}
  if password<>'' then begin // check file encryption password change
    Check(Demo.MemoryMappedMB=0,'mmap pragma disallowed');
    FreeAndNil(Demo); // if any exception occurs in Create(), Demo.Free is OK
    check(IsSQLite3File(TempFileName));
    check(IsSQLite3FileEncrypted(TempFileName));
    check(not IsOldSQLEncryptTable(TempFileName));
    check(not ChangeSQLEncryptTablePassWord(TempFileName,'password1','password1'));
    check(IsSQLite3File(TempFileName));
    check(IsSQLite3FileEncrypted(TempFileName));
    check(not IsOldSQLEncryptTable(TempFileName));
    check(ChangeSQLEncryptTablePassWord(TempFileName,'password1',''));
    check(IsSQLite3File(TempFileName));
    check(not IsOldSQLEncryptTable(TempFileName));
    check(not IsSQLite3FileEncrypted(TempFileName));
    check(ChangeSQLEncryptTablePassWord(TempFileName,'','NewPass'));
    check(IsSQLite3File(TempFileName));
    check(IsSQLite3FileEncrypted(TempFileName));
    check(not IsOldSQLEncryptTable(TempFileName));
    Demo := TSQLDataBase.Create(TempFileName,'NewPass'); // reuse the temporary file
    Demo.Synchronous := smOff;
    Demo.LockingMode := lmExclusive; 
    Demo.UseCache := true; // use the cache for the JSON requests
    Demo.WALMode := InheritsFrom(TTestFileBasedWAL); // test Write-Ahead Logging
    Check(Demo.WALMode=InheritsFrom(TTestFileBasedWAL));
    Check(Demo.MemoryMappedMB=0,'mmap pragma disallowed');
    Check(Hash32(Demo.ExecuteJSON(Req))=$40C1649A,'ExecuteJSON crypted');
    Check(Demo.MemoryMappedMB=0,'mmap pragma disallowed');
  end else
  {$endif}
  if ClassType=TTestFileBasedMemoryMap then begin // force re-open to test reading
    FreeAndNil(Demo);
    Demo := TSQLDataBase.Create(TempFileName,password);
    Demo.Synchronous := smOff;
    Demo.LockingMode := lmExclusive;
    Demo.MemoryMappedMB := 256;
    Demo.UseCache := true;
  end;
  Demo.GetTableNames(Names);
  Check(length(Names)=1);
  Check(Names[0]='People');
  Demo.Execute('SELECT Concat(FirstName," and ") FROM People WHERE LastName="Einstein"',s);
  Check(Hash32(s)=$68A74D8E,'Albert1 and Albert1 and Albert2 and Albert3 and ...');
  i1 := Demo.Execute('SELECT FirstName from People WHERE FirstName like "%eona%"',Names);
  check(i1=2002,'like/strcspn');
  check(Names[i1]='');
  for i := 0 to i1-1 do
    check(PosEx('eona',Names[i])>0);
end;

procedure TTestSQLite3Engine.VirtualTableDirectAccess;
const
  LOG1: RawUTF8 = 'D:\Dev\lib\SQLite3\exe\TestSQL3.exe 1.2.3.4 (2011-04-07)'#13#10+
    'Host=MyPC User=MySelf CPU=2*0-15-1027 OS=2.3=5.1.2600 Wow64=0 Freq=3579545'#13#10+
    'TSynLog 1.13 LVCL 2011-04-07 12:04:09'#13#10#13#10+
    '20110407 12040904 debug {"TObjectList(00AF8D00)":["TObjectList(00AF8D20)",'+
    '"TObjectList(00AF8D60)","TFileVersion(00ADC0B0)","TSynMapFile(00ACC990)"]}';
var Res: Int64;
    s,s2,s3: RawUTF8;
    n: PtrInt;
begin
  // register the Log virtual table module to this connection
  RegisterVirtualTableModule(TSQLVirtualTableLog,Demo);
  // test Log virtual table module
  FileFromString(LOG1,'temptest.log');
  Demo.Execute('CREATE VIRTUAL TABLE test USING log(temptest.log);');
  Demo.Execute('select count(*) from test',Res);
  Check(Res=1);
  n := 0;
  s := Demo.ExecuteJSON('select * from test',False,@n);
  Check(s<>'');
  Check(n=Res);
  s2 := Demo.ExecuteJSON('select * from test where rowid=2',False,@n);
  Check(s2='{"fieldCount":3,"values":["DateTime","Level","Content"],"rowCount":0}'#$A);
  Check(n=0);
  s2 := Demo.ExecuteJSON('select * from test where rowid=1',False,@n);
  Check(s2<>'');
  Check(s=s2);
  Check(n=1);
  n := 0;
  s3 := Demo.ExecuteJSON('select * from test where level=2',False,@n);
  Check(n=1);
  Check(s3='{"fieldCount":3,"values":["DateTime","Level","Content","2011-04-07T12:04:09.064",'+
    '2,"20110407 12040904 debug {\"TObjectList(00AF8D00)\":[\"TObjectList(00AF8D20)\",'+
    '\"TObjectList(00AF8D60)\",\"TFileVersion(00ADC0B0)\",\"TSynMapFile(00ACC990)\"]}"],'+
    '"rowCount":1}'#$A);
  s3 := Demo.ExecuteJSON('select * from test where level=3',False,@n);
  Check(s3='{"fieldCount":3,"values":["DateTime","Level","Content"],"rowCount":0}'#$A);
  Check(n=0);
end;

{$ifdef TEST_REGEXP}
procedure TTestSQLite3Engine.RegexpFunction;
const EXPRESSIONS: array[0..2] of RawUTF8 = ('\bFinley\b','^Samuel F','\bFinley\b');
var Model: TSQLModel;
    Client: TSQLRestClientDB;
    i,n: integer;
begin
  Model := TSQLModel.Create([TSQLRecordPeople]);
  Client := TSQLRestClientDB.Create(Model,nil,'test.db3',TSQLRestServerDB,false,'');
  try
    if CheckFailed(CreateRegExpFunction(Client.Server.DB.DB)) then
      exit;
    for i := 0 to high(EXPRESSIONS) do
      with TSQLRecordPeople.CreateAndFillPrepare(Client,'FirstName REGEXP ?',[EXPRESSIONS[i]]) do
      try
        if not CheckFailed(fFill<>nil) then begin
          Check(fFill.Table.RowCount=1001);
          n := 0;
          while FillOne do begin
            Check(LastName='Morse');
            Check(IdemPChar(pointer(FirstName),'SAMUEL FINLEY '));
            inc(n);
          end;
          Check(n=1001);
        end;
        Client.Server.DB.CacheFlush; // force compile '\bFinley\b' twice
      finally
        Free;
      end;
  finally
    Client.Free;
    Model.Free;
  end;
end;
{$endif TEST_REGEXP}

type
  TSQLRecordPeopleVersioned = class(TSQLRecordPeople)
  protected
    fVersion: TRecordVersion;
  published
    property Version: TRecordVersion read fVersion write fVersion;
  end;

procedure TestMasterSlaveRecordVersion(Test: TSynTestCase; const DBExt: TFileName);
  procedure TestMasterSlave(Master,Slave: TSQLRestServer; SynchronizeFromMaster: TSQLRest);
  var res: TRecordVersion;
      Rec1,Rec2: TSQLRecordPeopleVersioned;
  begin
    if SynchronizeFromMaster<>nil then
      res := Slave.RecordVersionSynchronizeSlave(TSQLRecordPeopleVersioned,SynchronizeFromMaster,500) else
      res := Slave.RecordVersionCurrent;
    Test.Check(res=Master.RecordVersionCurrent);
    Rec1 := TSQLRecordPeopleVersioned.CreateAndFillPrepare(Master,'order by ID','*');
    Rec2 := TSQLRecordPeopleVersioned.CreateAndFillPrepare(Slave,'order by ID','*');
    try
      Test.Check(Rec1.FillTable.RowCount=Rec2.FillTable.RowCount);
      while Rec1.FillOne do begin
        Test.Check(Rec2.FillOne);
        Test.Check(Rec1.SameRecord(Rec2),'simple fields');
        Test.Check(Rec1.Version=Rec2.Version);
      end;
    finally
      Rec1.Free;
      Rec2.Free;
    end;
  end;
var Model: TSQLModel;
    Master,Slave1,Slave2: TSQLRestServerDB;
    MasterAccess: TSQLRestClientURI;
    IDs: TIDDynArray;
    Rec: TSQLRecordPeopleVersioned;
    Slave2Callback: IServiceRecordVersionCallback;
    i,n: integer;
    timeout: Int64;
  function CreateServer(const DBFileName: TFileName; DeleteDBFile: boolean): TSQLRestServerDB;
  begin
    if DeleteDBFile then
      DeleteFile(DBFileName);
    result := TSQLRestServerDB.Create(TSQLModel.Create(Model),DBFileName,false,'');
    result.Model.Owner := result;
    result.DB.Synchronous := smOff;
    result.DB.LockingMode := lmExclusive;
    result.CreateMissingTables;
  end;
  procedure CreateMaster(DeleteDBFile: boolean);
  var serv: TSQLHttpServer;
      ws: TSQLHttpClientWebsockets;
  begin
    Master := CreateServer('testversion'+DBExt,DeleteDBFile);
    if Test is TTestBidirectionalRemoteConnection then begin
      serv := TTestBidirectionalRemoteConnection(Test).fHttpServer;
      Test.Check(serv.AddServer(Master));
      serv.WebSocketsEnable(Master,'key2').Settings.SetFullLog;
      ws := TSQLHttpClientWebsockets.Create('127.0.0.1',HTTP_DEFAULTPORT,TSQLModel.Create(Model));
      ws.Model.Owner := ws;
      ws.WebSockets.Settings.SetFullLog;
      Test.Check(ws.WebSocketsUpgrade('key2')='');
      MasterAccess := ws;
    end else
      MasterAccess := TSQLRestClientDB.Create(Master);
  end;
begin
  Model := TSQLModel.Create(
    [TSQLRecordPeople,TSQLRecordPeopleVersioned,TSQLRecordTableDeleted],'root0');
  CreateMaster(true);
  Slave1 := CreateServer('testversionreplicated'+DBExt,true);
  Slave2 := CreateServer('testversioncallback'+DBExt,true);
  try
    Rec := TSQLRecordPeopleVersioned.CreateAndFillPrepare(StringFromFile('Test1.json'));
    try // Rec contains 1001 input rows of data
      TestMasterSlave(Master,Slave1,MasterAccess);
      TestMasterSlave(Master,Slave2,MasterAccess);
      n := Rec.FillTable.RowCount;
      Test.Check(n>100);
      for i := 0 to 9 do begin // first test raw direct add
        Test.Check(Rec.FillOne);
        Master.Add(Rec,true,true);
      end;
      TestMasterSlave(Master,Slave1,MasterAccess);
      if Test is TTestBidirectionalRemoteConnection then
        Test.Check(TTestBidirectionalRemoteConnection(Test).fHttpServer.
          RemoveServer(Master));
      if Test is TTestBidirectionalRemoteConnection then
        TTestBidirectionalRemoteConnection(Test).fHttpServer.RemoveServer(Master);
      Master.Free; // test TSQLRestServer.InternalRecordVersionMaxFromExisting
      MasterAccess.Free;
      CreateMaster(false);
      MasterAccess.BatchStart(TSQLRecordPeopleVersioned,10000);
      while Rec.FillOne do // fast add via Batch
        Test.Check(MasterAccess.BatchAdd(Rec,true,true)>=0);
      Test.Check(MasterAccess.BatchSend(IDs)=HTTP_SUCCESS);
      Test.Check(n=length(IDs)+10);
      Test.Check(Rec.FillRewind);
      for i := 0 to 9 do
        Test.Check(Rec.FillOne);
      for i := 0 to high(IDs) do
        if Rec.FillOne then
          Test.Check(IDs[i]=Rec.IDValue) else
          Test.Check(false);
      TestMasterSlave(Master,Slave1,MasterAccess);
      TestMasterSlave(Master,Slave2,MasterAccess);
      if Test is TTestBidirectionalRemoteConnection then begin
        // asynchronous synchronization via websockets
        Test.Check(Master.RecordVersionSynchronizeMasterStart(true));
        Test.Check(Slave2.RecordVersionSynchronizeSlaveStart(
            TSQLRecordPeopleVersioned,MasterAccess,nil));
      end else begin
        // direct synchronization within the same process
        Slave2Callback := TServiceRecordVersionCallback.Create(
          Slave2,MasterAccess,TSQLRecordPeopleVersioned,nil);
        Master.RecordVersionSynchronizeSubscribeMaster(TSQLRecordPeopleVersioned,
          Slave2.RecordVersionCurrent,Slave2Callback);
      end;
      Test.Check(Rec.FillRewind);
      for i := 0 to 20 do begin
        Test.Check(Rec.FillOne);
        Rec.YearOfBirth := Rec.YearOfBirth+1;
        if i and 3=1 then
          Test.Check(Master.Delete(TSQLRecordPeopleVersioned,Rec.IDValue)) else
          Test.Check(Master.Update(Rec));
        if i and 3=2 then begin
          Rec.YearOfBirth := Rec.YearOfBirth+4;
          Test.Check(Master.Update(Rec),'update twice to increase Version');
        end;
      end;
      TestMasterSlave(Master,Slave1,MasterAccess);
      TestMasterSlave(Master,Slave1,MasterAccess);
      if Test is TTestBidirectionalRemoteConnection then begin
        timeout := GetTickCount64+3000;
        repeat sleep(1)
        until (GetTickCount64>timeout) or // wait all callbacks to be received
              (Slave2.RecordVersionCurrent=Master.RecordVersionCurrent);
        Test.Check(Slave2.RecordVersionSynchronizeSlaveStop(TSQLRecordPeopleVersioned));
      end;
      TestMasterSlave(Master,Slave2,nil);
      TestMasterSlave(Master,Slave2,MasterAccess);
    finally
      Rec.Free;
    end;
    if Test is TTestBidirectionalRemoteConnection then
      TTestBidirectionalRemoteConnection(Test).fHttpServer.RemoveServer(Master);
  finally
    Slave2Callback := nil;
    Slave1.Free; // warning: Free should be in this order for callbacks release
    Slave2.Free;
    Master.Free;
    MasterAccess.Free;
    Model.Free;
  end;
end;

procedure TTestSQLite3Engine._TRecordVersion;
begin
  TestMasterSlaveRecordVersion(self,'.db3');
end;

procedure TTestMemoryBased._TSQLTableWritable;
  procedure Test(intern: TRawUTF8Interning);
  var s1,s2: TSQLTableJSON;
      w: TSQLTableWritable;
      f,r: integer;
  begin
    s1 := TSQLTableJSON.CreateFromTables([TSQLRecordPeople],'',JS);
    s2 := TSQLTableJSON.CreateFromTables([TSQLRecordPeople],'',JS);
    w := TSQLTableWritable.CreateFromTables([TSQLRecordPeople],'',JS);
    try // merge the same data twice, and validate duplicated columns
      w.NewValuesInterning := intern;
      check(w.RowCount=s1.RowCount);
      check(w.FieldCount=s1.FieldCount);
      w.Join(s2,'rowid','ID'); // s2 will be sorted -> keep s1 untouched
      check(w.RowCount=s1.RowCount);
      check(w.FieldCount=s1.FieldCount*2-1);
      for f := 0 to s1.FieldCount-1 do begin
        check(w.FieldIndex(s1.FieldNames[f])=f);
        if f>0 then // f=0='ID' is not duplicated
          check(w.FieldIndex(s1.FieldNames[f]+'2')=f+s1.FieldCount-1);
      end;
      for r := 1 to w.RowCount do begin
        for f := 0 to s1.FieldCount-1 do begin
          check(StrComp(s1.Get(r,f),w.Get(r,f))=0);
          if f>0 then
            check(StrComp(s1.Get(r,f),w.Get(r,f+s1.FieldCount-1))=0);
        end;
      end;
      if intern<>nil then
        check(intern.Count=0);
      for r := 0 to w.RowCount do
        w.Update(r,1,UInt32ToUTF8(r and 127));
      for r := 1 to w.RowCount do
        check(w.GetAsInteger(r,1)=r and 127);
      if intern<>nil then
        check(intern.Count=128);
    finally
      s1.Free;
      s2.Free;
      w.Free;
      intern.Free;
    end;
  end;
begin
  Test(nil);
  Test(TRawUTF8Interning.Create);
end;

type
   TSQLRecordMapBox = class(TSQLRecordRTree)
   protected
     fMinX, fMaxX, fMinY, fMaxY: double;
   published
     property MinX: double read fMinX write fMinX;
     property MaxX: double read fMaxX write fMaxX;
     property MinY: double read fMinY write fMinY;
     property MaxY: double read fMaxY write fMaxY;
   end;
   TSQLRecordMapBoxI = class(TSQLRecordRTreeInteger)
   protected
     fMinX, fMaxX, fMinY, fMaxY: integer;
   published
     property MinX: integer read fMinX write fMinX;
     property MaxX: integer read fMaxX write fMaxX;
     property MinY: integer read fMinY write fMinY;
     property MaxY: integer read fMaxY write fMaxY;
   end;
   TSQLRecordMapBoxPlain = class(TSQLRecord)
   protected
     fMinX, fMaxX, fMinY, fMaxY: double;
   published
     property MinX: double read fMinX write fMinX;
     property MaxX: double read fMaxX write fMaxX;
     property MinY: double read fMinY write fMinY;
     property MaxY: double read fMaxY write fMaxY;
   end;

procedure TTestMemoryBased._RTree;
var Model: TSQLModel;
    Client: TSQLRestClientDB;
    Box: TSQLRecordMapBox;
    BoxI: TSQLRecordMapBoxI;
    //BoxPlain: TSQLRecordMapBoxPlain;
    i: integer;
    timer: TPrecisionTimer;
procedure CheckBox(i: integer);
begin
  Check(Box.fID=i*2);
  CheckSame(Box.MinX,i*1.0);
  CheckSame(Box.MaxX,i*1.0+0.5);
  CheckSame(Box.MinY,i*2.0);
  CheckSame(Box.MaxY,i*2.0+0.5);
end;
procedure CheckBoxI(i: integer);
begin
  Check(BoxI.fID=i*2);
  Check(BoxI.MinX=i);
  Check(BoxI.MaxX=i+2);
  Check(BoxI.MinY=i*2);
  Check(BoxI.MaxY=i*2+2);
end;
{procedure CheckBoxPlain(i: integer);
begin
  Check(BoxPlain.fID=i*2);
  CheckSame(BoxPlain.MinX,i*1.0);
  CheckSame(BoxPlain.MaxX,i*1.0+0.5);
  CheckSame(BoxPlain.MinY,i*2.0);
  CheckSame(BoxPlain.MaxY,i*2.0+0.5);
end;}
const COUNT=10000;
begin
  Model := TSQLModel.Create([TSQLRecordMapBox,TSQLRecordMapBoxI,TSQLRecordMapBoxPlain]);
  Client := TSQLRestClientDB.Create(Model,nil,SQLITE_MEMORY_DATABASE_NAME,TSQLRestServerDB,false,'');
  try
    (Client.Server as TSQLRestServer).CreateMissingTables;
    {timer.Start;
    BoxPlain := TSQLRecordMapBoxPlain.Create;
    try
      Client.TransactionBegin(TSQLRecordMapBoxPlain);
      for i := 1 to COUNT do begin
        BoxPlain.fID := i*2; // force ID
        BoxPlain.MinX := i*1.0;
        BoxPlain.MaxX := i*1.0+0.5;
        BoxPlain.MinY := i*2.0;
        BoxPlain.MaxY := i*2.0+0.5;
        Check(Client.Add(BoxPlain,true,true)=i*2);
      end;
      Client.Commit;
      writeln('added in ',timer.Stop); timer.Start;
      with Client.Server as TSQLRestServer do begin
        CreateSQLIndex(TSQLRecordMapBoxPlain,'MinX',false);
        CreateSQLIndex(TSQLRecordMapBoxPlain,'MaxX',false);
        CreateSQLIndex(TSQLRecordMapBoxPlain,'MinY',false);
        CreateSQLIndex(TSQLRecordMapBoxPlain,'MaxY',false);
      end;
      writeln('indexes created in ',timer.Stop); timer.Start;
      for i := 1 to COUNT do begin
        Check(Client.Retrieve(i*2,BoxPlain));
        CheckBoxPlain(i);
      end;
      writeln('retrieved by id in ',timer.Stop); timer.Start;
      for i := 1 to COUNT do begin
        BoxPlain.FillPrepare(Client,'MinX<=? and ?<=MaxX and MinY<=? and ?<=MaxY',
          [i*1.0+0.25,i*1.0+0.25,i*2.0+0.25,i*2.0+0.25]);
        Check(BoxPlain.FillOne);
        CheckBoxPlain(i);
        Check(not BoxPlain.FillOne);
      end;
      writeln('retrieved by coords in ',timer.Stop); timer.Start;
    finally
      BoxPlain.Free;
    end;
    NotifyTestSpeed('Without RTree',COUNT,0,@timer);}
    timer.Start;
    Box := TSQLRecordMapBox.Create;
    try
      Client.TransactionBegin(TSQLRecordMapBox);
      for i := 1 to COUNT do begin
        Box.fID := i*2; // force ID
        Box.MinX := i*1.0;
        Box.MaxX := i*1.0+0.5;
        Box.MinY := i*2.0;
        Box.MaxY := i*2.0+0.5;
        Check(Client.Add(Box,true,true)=i*2);
      end;
      Client.Commit;
      for i := 1 to COUNT do begin
        Check(Client.Retrieve(i*2,Box));
        CheckBox(i);
      end;
      for i := 1 to COUNT do begin
        Box.FillPrepare(Client,'MinX<=? and ?<=MaxX and MinY<=? and ?<=MaxY',
          [i*1.0+0.25,i*1.0+0.25,i*2.0+0.25,i*2.0+0.25]);
        Check(Box.FillOne);
        CheckBox(i);
        Check(not Box.FillOne);
      end;
      Box.FillPrepare(Client,'MinX<=? and ?<=MaxX and MinY<=? and ?<=MaxY',
        [1.0,1.0,2.0,2.0]);
      Check(Box.FillOne);
      CheckBox(1);
      Box.FillPrepare(Client,'MinX<=? and ?<=MaxX and MinY<=? and ?<=MaxY',
        [1.5,1.5,2.5,2.5]);
      Check(Box.FillOne);
      CheckBox(1);
    finally
      Box.Free;
    end;
    NotifyTestSpeed('With RTree',COUNT,0,@timer);
    timer.Start;
    BoxI := TSQLRecordMapBoxI.Create;
    try
      Client.TransactionBegin(TSQLRecordMapBoxI);
      for i := 1 to COUNT do begin
        BoxI.fID := i*2; // force ID
        BoxI.MinX := i;
        BoxI.MaxX := i+2;
        BoxI.MinY := i*2;
        BoxI.MaxY := i*2+2;
        Check(Client.Add(BoxI,true,true)=i*2);
      end;
      Client.Commit;
      for i := 1 to COUNT do begin
        Check(Client.Retrieve(i*2,BoxI));
        CheckBoxI(i);
      end;
      for i := 1 to COUNT do begin
        BoxI.FillPrepare(Client,'MinX<=? and ?<=MaxX and MinY<=? and ?<=MaxY',
          [i+1,i+1,i*2+1,i*2+1]);
        Check(BoxI.FillOne);
        CheckBoxI(i);
        Check(not BoxI.FillOne);
      end;
      BoxI.FillPrepare(Client,'MinX<=? and ?<=MaxX and MinY<=? and ?<=MaxY',
        [1,1,2,2]);
      Check(BoxI.FillOne);
      CheckBoxI(1);
      BoxI.FillPrepare(Client,'MinX<=? and ?<=MaxX and MinY<=? and ?<=MaxY',
        [3,3,4,4]);
      Check(BoxI.FillOne);
      CheckBoxI(1);
    finally
      BoxI.Free;
    end;
    NotifyTestSpeed('With RTreeInteger',COUNT,0,@timer);
  finally
    Client.Free;
    Model.Free;
  end;
end;
{
  Delphi Win32:
   10000 With RTree in 806.64ms i.e. 12396/s, aver. 80us
   10000 With RTreeInteger in 750.94ms i.e. 13316/s, aver. 75us

   10000 Without RTree in 16.82s i.e. 594/s, aver. 1.68ms (no index)
   10000 Without RTree in 22.96s i.e. 435/s, aver. 2.29ms (with indexes created last)
    added in 136.90ms
    indexes created in 25.02ms
    retrieved by id in 119.87ms
    retrieved by coords in 22.71s
   10000 Without RTree in 23.13s i.e. 432/s, aver. 2.31ms (with indexes created first)

  Delphi Win64:
    10000 With RTree in 737ms i.e. 13568/s, aver. 73us
    10000 With RTreeInteger in 621.83ms i.e. 16081/s, aver. 62us
  FPC Win32:
    10000 With RTree in 852.12ms i.e. 11735/s, aver. 85us
    10000 With RTreeInteger in 764.59ms i.e. 13078/s, aver. 76us
  FPC Win64:
    10000 With RTree in 718.39ms i.e. 13919/s, aver. 71us
    10000 With RTreeInteger in 667.80ms i.e. 14974/s, aver. 66us
  FPC Linux64 (within Windows Linux Layer):
    10000 With RTree in 1.08s i.e. 9218/s, aver. 108us
    10000 With RTreeInteger in 1s i.e. 9966/s, aver. 100us
}


const SHARD_MAX = 10000;
      SHARD_RANGE = 1000;

function TTestMemoryBased.CreateShardDB(maxshard: Integer): TSQLRestServer;
begin
  result := TSQLRestServer.CreateWithOwnModel([TSQLRecordTest],false,'shardtest');
  Check(result.StaticDataAdd(TSQLRestStorageShardDB.Create(
    TSQLRecordTest,result,SHARD_RANGE,[],'',maxshard)));
end;

procedure TTestMemoryBased.ShardWrite;
var R: TSQLRecordTest;
    i: integer;
    db: TSQLRestServer;
    b: TSQLRestBatch;
begin
  DirectoryDelete(ExeVersion.ProgramFilePath,'Test0*.dbs',True);
  db := CreateShardDB(100);
  try
    R := TSQLRecordTest.Create;
    try
      for i := 1 to 50 do begin
        R.FillWith(i);
        Check(db.AddWithBlobs(R)=i);
        R.CheckWith(self,i);
      end;
      b := TSQLRestBatch.Create(db,TSQLRecordTest,SHARD_RANGE div 3,[boExtendedJSON]);
      try
        for i := 51 to SHARD_MAX do begin
          R.FillWith(i);
          Check(b.Add(R,true,false,ALL_FIELDS)=i-51);
        end;
        Check(db.BatchSend(b)=HTTP_SUCCESS);
      finally
        b.Free;
      end;
    finally
      R.Free;
    end;
  finally
    db.Free;
  end;
end;

procedure TTestMemoryBased.ShardRead;
var R: TSQLRecordTest;
    i: integer;
    db: TSQLRestServer;
begin
  db := CreateShardDB(100);
  try
    R := TSQLRecordTest.Create;
    try
      for i := 1 to SHARD_MAX do begin
        Check(db.Retrieve(i,R));
        Check(db.RetrieveBlobFields(R));
        R.CheckWith(self,i,0);
      end;
    finally
      R.Free;
    end;
  finally
    db.Free;
  end;
end;

procedure TTestMemoryBased.ShardReadAfterPurge;
var R: TSQLRecordTest;
    i: integer;
    db: TSQLRestServer;
begin
  Check(DeleteFile(ExeVersion.ProgramFilePath+'Test0000.dbs'));
  Check(DeleteFile(ExeVersion.ProgramFilePath+'Test0001.dbs'));
  db := CreateShardDB(100);
  try
    R := TSQLRecordTest.Create;
    try
      for i := 1 to SHARD_RANGE*2 do
        Check(not db.Retrieve(i,R));
      for i := SHARD_RANGE*2+1 to SHARD_MAX do begin
        Check(db.Retrieve(i,R));
        Check(db.RetrieveBlobFields(R));
        R.CheckWith(self,i,0);
      end;
    finally
      R.Free;
    end;
  finally
    db.Free;
  end;
end;

procedure TTestMemoryBased._MaxShardCount;
var R: TSQLRecordTest;
    i,last: integer;
    db: TSQLRestServer;
    b: TSQLRestBatch;
begin
  db := CreateShardDB(5);
  try
    R := TSQLRecordTest.Create;
    try
      last := SHARD_MAX-SHARD_RANGE*5;
      for i := 1 to last do
        Check(not db.Retrieve(i,R));
      for i := last+1 to SHARD_MAX do begin
        Check(db.Retrieve(i,R));
        Check(db.RetrieveBlobFields(R));
        R.CheckWith(self,i,0);
      end;
      b := TSQLRestBatch.Create(db,TSQLRecordTest,SHARD_RANGE div 3,[boExtendedJSON]);
      try
        for i := SHARD_MAX+1 to SHARD_MAX+2000 do begin
          R.FillWith(i);
          Check(b.Add(R,true)=i-(SHARD_MAX+1));
        end;
        Check(db.BatchSend(b)=HTTP_SUCCESS);
      finally
        b.Free;
      end;
      last := SHARD_MAX+2000-SHARD_RANGE*5;
      for i := 1 to last do
        Check(not db.Retrieve(i,R));
      for i := last+1 to SHARD_MAX+2000 do begin
        Check(db.Retrieve(i,R));
        R.CheckWith(self,i,0,false);
      end;
    finally
      R.Free;
    end;
  finally
    db.Free;
  end;
end;


{ TTestClientServerAccess }

{$WARN SYMBOL_PLATFORM OFF}
procedure TTestClientServerAccess._TSQLHttpClient;
var Resp: TSQLTable;
    len: integer;
begin
  Client := TSQLHttpClient.Create('127.0.0.1',HTTP_DEFAULTPORT,Model);
  fRunConsole := fRunConsole+'using '+string(Client.ClassName);
  (Client as TSQLHttpClientGeneric).Compression := [];
  Resp := Client.List([TSQLRecordPeople],'*');
  if CheckFailed(Resp<>nil) then
    exit;
  try
    Check(Resp.InheritsFrom(TSQLTableJSON));
    len := Length(TSQLTableJSON(Resp).PrivateInternalCopy)-16;
    if not CheckFailed(len>0) then
      Check(Hash32(pointer(TSQLTableJSON(Resp).PrivateInternalCopy),len)=$F11CEAC0);
    //FileFromString(Resp.GetODSDocument,'people.ods');
  finally
    Resp.Free;
  end;
end;
{$WARN SYMBOL_PLATFORM ON}

{$ifdef MSWINDOWS}
class function TTestClientServerAccess.RegisterAddUrl(OnlyDelete: boolean): string;
begin
  result := THttpApiServer.AddUrlAuthorize('root',HTTP_DEFAULTPORT,false,'+',OnlyDelete);
end;
{$endif}

procedure TTestClientServerAccess._TSQLHttpServer;
begin
  Model := TSQLModel.Create([TSQLRecordPeople],'root');
  Check(Model<>nil);
  Check(Model.GetTableIndex('people')>=0);
  try
    DataBase := TSQLRestServerDB.Create(Model,'test.db3');
    DataBase.DB.Synchronous := smOff;
    DataBase.DB.LockingMode := lmExclusive;
    Server := TSQLHttpServer.Create(
      HTTP_DEFAULTPORT,[DataBase],'+',HTTP_DEFAULT_MODE,16,secSynShaAes);
    fRunConsole := fRunConsole+'using '+Server.HttpServer.APIVersion;
    Database.NoAJAXJSON := true; // expect not expanded JSON from now on
  except
    on E: Exception do
      Check(false,E.Message);
  end;
end;

procedure TTestClientServerAccess.CleanUp;
begin
  FreeAndNil(Client); // should already be nil
  Server.Shutdown;
  FreeAndNil(Server);
  FreeAndNil(DataBase);
  FreeAndNil(Model);
end;

{$define WTIME}

const
  CLIENTTEST_WHERECLAUSE = 'FirstName Like "Sergei1%"';

procedure TTestClientServerAccess.ClientTest;
const IDTOUPDATE = 3;
{$ifdef WTIME}
  LOOP=1000;
var Timer: ILocalPrecisionTimer;
{$else}
  LOOP=100;
{$endif}
var i,siz: integer;
    Resp: TSQLTable;
    Rec, Rec2: TSQLRecordPeople;
    Refreshed: boolean;

  procedure TestOne;
  var i: integer;
  begin
    i := Rec.YearOfBirth;
    Rec.YearOfBirth := 1982;
    Check(Client.Update(Rec));
    Rec2.ClearProperties;
    Check(Client.Retrieve(IDTOUPDATE,Rec2));
    Check(Rec2.YearOfBirth=1982);
    Rec.YearOfBirth := i;
    Check(Client.Update(Rec));
    if Client.InheritsFrom(TSQLRestClientURI) then begin
      Check(TSQLRestClientURI(Client).UpdateFromServer([Rec2],Refreshed));
      Check(Refreshed,'should have been refreshed');
    end else
      Check(Client.Retrieve(IDTOUPDATE,Rec2));
    Check(Rec.SameRecord(Rec2));
  end;

begin
{$ifdef WTIME}
  Timer := TLocalPrecisionTimer.CreateAndStart;
{$endif}
  // first calc result: all transfert protocols have to work from cache
  Resp := Client.List([TSQLRecordPeople],'*',CLIENTTEST_WHERECLAUSE);
  if CheckFailed(Resp<>nil) then
    exit;
  siz := length(TSQLTableJSON(Resp).PrivateInternalCopy)-16;
  if not CheckFailed(siz=4818) then
    Check(Hash32(pointer(TSQLTableJSON(Resp).PrivateInternalCopy),siz)=$8D727024);
  Resp.Free;
{$ifdef WTIME}
  fRunConsole := format('%s%s, first %s, ',[fRunConsole,KB(siz),Timer.Stop]);
{$endif}
  // test global connection speed and caching (both client and server sides)
  Rec2 := TSQLRecordPeople.Create;
  Rec := TSQLRecordPeople.Create(Client,IDTOUPDATE);
  try
    Check(Rec.ID=IDTOUPDATE,'retrieve record');
    Check(Database.Cache.CachedEntries=0);
    Check(Client.Cache.CachedEntries=0);
    Check(Client.Cache.CachedMemory=0);
    TestOne;
    Check(Client.Cache.CachedEntries=0);
    Client.Cache.SetCache(TSQLRecordPeople); // cache whole table
    Check(Client.Cache.CachedEntries=0);
    Check(Client.Cache.CachedMemory=0);
    TestOne;
    Check(Client.Cache.CachedEntries=1);
    Check(Client.Cache.CachedMemory>0);
    Client.Cache.Clear; // reset cache settings
    Check(Client.Cache.CachedEntries=0);
    Client.Cache.SetCache(Rec); // cache one = SetCache(TSQLRecordPeople,Rec.ID)
    Check(Client.Cache.CachedEntries=0);
    Check(Client.Cache.CachedMemory=0);
    TestOne;
    Check(Client.Cache.CachedEntries=1);
    Check(Client.Cache.CachedMemory>0);
    Client.Cache.SetCache(TSQLRecordPeople);
    TestOne;
    Check(Client.Cache.CachedEntries=1);
    Client.Cache.Clear;
    Check(Client.Cache.CachedEntries=0);
    TestOne;
    Check(Client.Cache.CachedEntries=0);
    if not (Client.InheritsFrom(TSQLRestClientDB)) then begin // server-side
      Database.Cache.SetCache(TSQLRecordPeople);
      TestOne;
      Check(Client.Cache.CachedEntries=0);
      Check(Database.Cache.CachedEntries=1);
      Database.Cache.Clear;
      Check(Client.Cache.CachedEntries=0);
      Check(Database.Cache.CachedEntries=0);
      Database.Cache.SetCache(TSQLRecordPeople,Rec.ID);
      TestOne;
      Check(Client.Cache.CachedEntries=0);
      Check(Database.Cache.CachedEntries=1);
      Database.Cache.SetCache(TSQLRecordPeople);
      Check(Database.Cache.CachedEntries=0);
      TestOne;
      Check(Database.Cache.CachedEntries=1);
      if Client.InheritsFrom(TSQLRestClientURI) then
        TSQLRestClientURI(Client).ServerCacheFlush else
        Database.Cache.Flush;
      Check(Database.Cache.CachedEntries=0);
      Check(Database.Cache.CachedMemory=0);
      Database.Cache.Clear;
    end;
  finally
    Rec2.Free;
    Rec.Free;
  end;
  // test average speed for a 5 KB request
  Resp := Client.List([TSQLRecordPeople],'*',CLIENTTEST_WHERECLAUSE);
  Check(Resp<>nil);
  Resp.Free;
{$ifdef WTIME}
  Timer.Start;
{$endif}
  for i := 1 to LOOP do begin
    Resp := Client.List([TSQLRecordPeople],'*',CLIENTTEST_WHERECLAUSE);
    if CheckFailed(Resp<>nil) then
      exit;
    try
      Check(Resp.InheritsFrom(TSQLTableJSON));
      // every answer contains 113 rows, for a total JSON size of 4803 bytes
      siz := length(TSQLTableJSON(Resp).PrivateInternalCopy)-16;
      if not CheckFailed(siz>0) then
        Check(Hash32(pointer(TSQLTableJSON(Resp).PrivateInternalCopy),siz)=$8D727024);
    finally
      Resp.Free;
    end;
  end;
{$ifdef WTIME}
  fRunConsole := format('%sdone %s i.e. %d/s, aver. %s, %s/s',
    [fRunConsole,Timer.Stop,Timer.PerSec(LOOP),Timer.ByCount(LOOP),
     KB(Timer.PerSec(4898*(LOOP+1)))]);
{$endif}
end;

procedure TTestClientServerAccess.HttpClientKeepAlive;
begin
  (Client as TSQLHttpClientGeneric).KeepAliveMS := 20000;
  (Client as TSQLHttpClientGeneric).Compression := [];
  ClientTest;
end;

procedure TTestClientServerAccess.HttpClientMultiConnect;
begin
  (Client as TSQLHttpClientGeneric).KeepAliveMS := 0;
  (Client as TSQLHttpClientGeneric).Compression := [];
  ClientTest;
end;

procedure TTestClientServerAccess.HttpClientEncrypted;
begin
  (Client as TSQLHttpClientGeneric).KeepAliveMS := 20000;
  (Client as TSQLHttpClientGeneric).Compression := [hcSynShaAes];
  ClientTest;
end;

procedure TTestClientServerAccess.HTTPClientCustomEncryptionAesSha;
var rnd: THash256;
    sign: TSynSigner;
begin
  TAESPRNG.Main.FillRandom(rnd);
  sign.Init(saSha256,'secret1');
  Client.SetCustomEncryption(TAESOFB.Create(rnd),@sign,AlgoSynLZ);
  DataBase.SetCustomEncryption(TAESOFB.Create(rnd),@sign,AlgoSynLZ);
  ClientTest;
end;

procedure TTestClientServerAccess.HTTPClientCustomEncryptionAes;
var rnd: THash256;
begin
  TAESPRNG.Main.FillRandom(rnd);
  Client.SetCustomEncryption(TAESOFB.Create(rnd),nil,AlgoSynLZ);
  DataBase.SetCustomEncryption(TAESOFB.Create(rnd),nil,AlgoSynLZ);
  ClientTest;
end;

procedure TTestClientServerAccess.HTTPClientCustomEncryptionSha;
var sign: TSynSigner;
begin
  sign.Init(saSha256,'secret2');
  Client.SetCustomEncryption(nil,@sign,AlgoSynLZ);
  DataBase.SetCustomEncryption(nil,@sign,AlgoSynLZ);
  ClientTest;
  Client.SetCustomEncryption(nil,nil,nil); // disable custom encryption
  DataBase.SetCustomEncryption(nil,nil,nil);
end;

procedure TTestClientServerAccess.HttpSeveralDBServers;
var Instance: array[0..2] of record
      Model: TSQLModel;
      Database: TSQLRestServerDB;
      Client: TSQLHttpClient;
    end;
    i: integer;
    Rec: TSQLRecordPeople;
begin
  Rec := TSQLRecordPeople.CreateAndFillPrepare(Database,CLIENTTEST_WHERECLAUSE);
  try
    Check(Rec.FillTable.RowCount=113);
    // release main http client/server and main database instances
    CleanUp;
    assert(Client=nil);
    assert(Server=nil);
    assert(DataBase=nil);
    // create 3 TSQLRestServerDB + TSQLHttpClient instances (and TSQLModel)
    for i := 0 to high(Instance) do
    with Instance[i] do begin
      Model := TSQLModel.Create([TSQLRecordPeople],'root'+Int32ToUtf8(i));
      DataBase := TSQLRestServerDB.Create(Model,SQLITE_MEMORY_DATABASE_NAME);
      Database.NoAJAXJSON := true; // expect not expanded JSON from now on
      DataBase.CreateMissingTables;
    end;
    // launch one HTTP server for all TSQLRestServerDB instances
    Server := TSQLHttpServer.Create(HTTP_DEFAULTPORT,
      [Instance[0].Database,Instance[1].Database,Instance[2].Database],
      '+',HTTP_DEFAULT_MODE,4,secNone);
    // initialize the clients
    for i := 0 to high(Instance) do
    with Instance[i] do
      Client := TSQLHttpClient.Create('127.0.0.1',HTTP_DEFAULTPORT,Model);
    // fill remotely all TSQLRestServerDB instances
    for i := 0 to high(Instance) do
    with Instance[i] do begin
      Client.TransactionBegin(TSQLRecordPeople);
      Check(Rec.FillRewind);
      while Rec.FillOne do
        Check(Client.Add(Rec,true,true)=Rec.fID);
      Client.Commit;
    end;
    // test remote access to all TSQLRestServerDB instances
    try
      for i := 0 to high(Instance) do begin
        Client := Instance[i].Client;
        DataBase := Instance[i].DataBase;
        try
          ClientTest;
          {$ifdef WTIME}
          if i<high(Instance) then
            fRunConsole := fRunConsole+#13#10+'     ';
          {$endif}
        finally
          Client := nil;
          DataBase := nil;
        end;
      end;
    finally
      Client := nil;
      Database := nil;
      // release all TSQLRestServerDB + TSQLHttpClient instances (and TSQLModel)
      for i := high(Instance) downto 0 do
      with Instance[i] do begin
        FreeAndNil(Client);
        Server.RemoveServer(DataBase);
        FreeAndNil(DataBase);
        FreeAndNil(Model);
      end;
    end;
  finally
    Rec.Free;
  end;
end;

{
procedure TTestClientServerAccess.HttpClientMultiConnectDelphi;
begin
  if (self=nil) or (Server=nil) then
    exit; // if already Delphi code, nothing to test
  (Client as TSQLHttpClientGeneric).KeepAliveMS := 0;
  ClientTest;
end;

procedure TTestClientServerAccess.HttpClientKeepAliveDelphi;
begin
  if (self=nil) or (Server=nil) or (Server.HttpServer is THttpServer) then
    exit; // if already Delphi code, nothing to test
  Server.Free;
  Server := TSQLHttpServer.Create(HTTP_DEFAULTPORT,[DataBase],'+',true);
  (Client as TSQLHttpClientGeneric).KeepAliveMS := 10000;
  ClientTest;
end;
}

{$ifdef MSWINDOWS}
procedure TTestClientServerAccess.NamedPipeAccess;
begin
  Check(DataBase.ExportServerNamedPipe('test'));
  Client.Free;
  Client := TSQLRestClientURINamedPipe.Create(Model,'test');
  ClientTest;
  // note: 1st connection is slower than with HTTP (about 100ms), because of
  // Sleep(128) in TSQLRestServerNamedPipe.Execute: but we should connect
  // localy only once, and avoiding Context switching is a must-have
  FreeAndNil(Client);
  Check(DataBase.CloseServerNamedPipe);
end;
{$endif}

procedure TTestClientServerAccess.DirectInProcessAccess;
var stats: RawUTF8;
begin
  FreeAndNil(Client);
  Client := TSQLRestClientDB.Create(Model,
    TSQLModel.Create([TSQLRecordPeople],'root'),
    DataBase.DB,TSQLRestServerTest);
  ClientTest;
  Client.CallBackGet('stat',['withall',true],stats);
  FileFromString(JSONReformat(stats),'statsClientServer.json');
  FreeAndNil(Client);
end;

{$ifdef MSWINDOWS}
procedure TTestClientServerAccess.LocalWindowMessages;
begin
  Check(DataBase.ExportServerMessage('test'));
  Client := TSQLRestClientURIMessage.Create(Model,'test','Client',1000);
  ClientTest;
  FreeAndNil(Client);
end;
{$endif}


{ TTestExternalDatabase }

type
  TSQLRecordPeopleExt = class(TSQLRecord)
  private
    fFirstName: RawUTF8;
    fLastName: RawUTF8;
    fData: TSQLRawBlob;
    fYearOfBirth: integer;
    fYearOfDeath: word;
    {$ifndef NOVARIANTS}
    fValue: TVariantDynArray;
    {$endif}
    fLastChange: TModTime;
    fCreatedAt: TCreateTime;
  published
    property FirstName: RawUTF8 index 40 read fFirstName write fFirstName;
    property LastName: RawUTF8 index 40 read fLastName write fLastName;
    property Data: TSQLRawBlob read fData write fData;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
    {$ifndef NOVARIANTS}
    property Value: TVariantDynArray read fValue write fValue;
    {$endif}
    property LastChange: TModTime read fLastChange;
    property CreatedAt: TCreateTime read fCreatedAt write fCreatedAt;
  end;

  TSQLRecordOnlyBlob = class(TSQLRecord)
  private
    fData: TSQLRawBlob;
  published
    property Data: TSQLRawBlob read fData write fData;
  end;

  TSQLRecordTestJoin = class(TSQLRecord)
  private
    fName: RawUTF8;
    fPeople: TSQLRecordPeopleExt;
  published
    property Name: RawUTF8 index 30 read fName write fName;
    property People: TSQLRecordPeopleExt read fPeople write fPeople;
  end;

  TSQLFTSTest = class(TSQLRecordFTS3)
  private
    fSubject: RawUTF8;
    fBody: RawUTF8;
  published
    property Subject: RawUTF8 read fSubject write fSubject;
    property Body: RawUTF8 read fBody write fBody;
  end;

  TSQLASource = class;
  TSQLADest = class;
  TSQLADests = class(TSQLRecordMany)
  private
    fTime: TDateTime;
    fDest: TSQLADest;
    fSource: TSQLASource;
  published
    property Source: TSQLASource read fSource;
    property Dest: TSQLADest read fDest;
    property AssociationTime: TDateTime read fTime write fTime;
  end;
  TSQLASource = class(TSQLRecordSigned)
  private
    fDestList: TSQLADests;
  published
    property SignatureTime;
    property Signature;
    property DestList: TSQLADests read fDestList;
  end;
  TSQLADest = class(TSQLRecordSigned)
  published
    property SignatureTime;
    property Signature;
  end;
  TSQLRecordPeopleArray = class(TSQLRecordPeople)
  private
    fInts: TIntegerDynArray;
    fCurrency: TCurrencyDynArray;
{$ifdef PUBLISHRECORD}
    fRec: TFTSMatchInfo;
{$endif PUBLISHRECORD}
    fFileVersion: TFVs;
    fUTF8: RawUTF8;
  published
{$ifdef PUBLISHRECORD}
    property Rec: TFTSMatchInfo read fRec write fRec;
{$endif PUBLISHRECORD}
    property UTF8: RawUTF8 read fUTF8 write fUTF8;
    property Ints: TIntegerDynArray index 1 read fInts write fInts;
    property Currency: TCurrencyDynArray index 2 read fCurrency write fCurrency;
    property FileVersion: TFVs index 3 read fFileVersion write fFileVersion;
  end;
{$ifndef LVCL}
  TSQLRecordPeopleObject = class(TSQLRecordPeople)
  private
    fPersistent: TCollTst;
    fUTF8: TRawUTF8List;
  public
    /// will create an internal TCollTst instance for Persistent property
    constructor Create; override;
    /// will release the internal TCollTst instance for Persistent property
    destructor Destroy; override;
  published
    property UTF8: TRawUTF8List read fUTF8;
    property Persistent: TCollTst read fPersistent;
  end;
{$endif}
  TSQLRecordDali1 = class(TSQLRecordVirtualTableAutoID)
  private
    fYearOfBirth: integer;
    fFirstName: RawUTF8;
    fYearOfDeath: word;
  published
    property FirstName: RawUTF8 read fFirstName write fFirstName;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
  end;
  TSQLRecordDali2 = class(TSQLRecordDali1);

  // class hooks to force DMBS property for TTestExternalDatabase.AutoAdaptSQL
  TSQLDBConnectionPropertiesHook = class(TSQLDBConnectionProperties);
  TSQLRestStorageExternalHook = class(TSQLRestStorageExternal);

  TSQLRecordPeopleID = type TID;
  TSQLRecordPeopleToBeDeletedID = type TID;

  TSQLRecordCustomProps = class(TSQLRecordPeople)
  protected
    fGUID: TGUID;
    fPeopleID: TID;
    fPeople: TSQLRecordPeopleID;
    fPeopleCascade: TSQLRecordPeopleToBeDeletedID;
    {$ifdef PUBLISHRECORD}
    fGUIDXE6: TGUID;
    {$endif}
    class procedure InternalRegisterCustomProperties(Props: TSQLRecordProperties); override;
  public
    property GUID: TGUID read fGUID write fGUID;
  published
    property PeopleID: TID read fPeopleID write fPeopleID;
    property People: TSQLRecordPeopleID read fPeople write fPeople;
    property PeopleCascade: TSQLRecordPeopleToBeDeletedID read fPeopleCascade write fPeopleCascade;
    {$ifdef PUBLISHRECORD}
    property GUIDXE6: TGUID read fGUIDXE6 write fGUIDXE6;
    {$endif}
  end;

{ TSQLRecordCustomProps }

class procedure TSQLRecordCustomProps.InternalRegisterCustomProperties(Props: TSQLRecordProperties);
begin
  Props.RegisterCustomPropertyFromTypeName(self,'TGUID','GUID',
    @TSQLRecordCustomProps(nil).fGUID,[aIsUnique],38);
end;

/// will be re-used by both TTestSQLite3Engine and TTestExternalDatabase
procedure InternalTestMany(Test: TSynTestCase; aClient: TSQLRestClient);
var MS: TSQLASource;
    MD, MD2: TSQLADest;
    i: integer;
    sID, dID: array[1..100] of Integer;
    res: TIDDynArray;
  procedure CheckOK;
  begin
    if Test.CheckFailed(MS.FillTable<>nil) then
      exit;
    Test.Check(MS.FillTable.RowCount>=length(sId));
    while MS.FillOne do begin
      Test.Check(MS.DestList.Source.fID=MS.fID);
      Test.Check(MS.DestList.Dest.SignatureTime<>0);
      MS.ClearProperties;
      MS.DestList.Source.ClearProperties;
      MS.DestList.Dest.ClearProperties;
    end;
    MS.FillClose;
  end;
begin
  MS := TSQLASource.Create;
  MD := TSQLADest.Create;
  with Test do
  try
    MD.fSignatureTime := TimeLogNow;
    MS.fSignatureTime := MD.fSignatureTime;
    Check(MS.DestList<>nil);
    Check(MS.DestList.InheritsFrom(TSQLRecordMany));
    Check(aClient.TransactionBegin(TSQLASource)); // faster process
    for i := 1 to high(dID) do begin
      MD.fSignature := FormatUTF8('% %',[aClient.ClassName,i]);
      dID[i] := aClient.Add(MD,true);
      Check(dID[i]>0);
    end;
    for i := 1 to high(sID) do begin
      MS.fSignature := FormatUTF8('% %',[aClient.ClassName,i]);
      sID[i] := aClient.Add(MS,True);
      Check(sID[i]>0);
      MS.DestList.AssociationTime := i;
      Check(MS.DestList.ManyAdd(aClient,sID[i],dID[i])); // associate both lists
      Check(not MS.DestList.ManyAdd(aClient,sID[i],dID[i],true)); // no dup
    end;
    aClient.Commit;
    for i := 1 to high(dID) do begin
      Check(MS.DestList.SourceGet(aClient,dID[i],res));
      if not CheckFailed(length(res)=1) then
        Check(res[0]=sID[i]);
      Check(MS.DestList.ManySelect(aClient,sID[i],dID[i]));
      Check(MS.DestList.AssociationTime=i);
    end;
    for i := 1 to high(sID) do begin
      Check(MS.DestList.DestGet(aClient,sID[i],res));
      if CheckFailed(length(res)=1) then
        continue; // avoid GPF
      Check(res[0]=dID[i]);
      Check(MS.DestList.FillMany(aClient,sID[i])=1);
      Check(MS.DestList.FillOne);
      Check(Integer(MS.DestList.Source)=sID[i]);
      Check(Integer(MS.DestList.Dest)=dID[i]);
      Check(MS.DestList.AssociationTime=i);
      Check(not MS.DestList.FillOne);
      Check(MS.DestList.DestGetJoined(aClient,'',sID[i],res));
      if not CheckFailed(length(res)=1) then
        Check(res[0]=dID[i]);
      Check(MS.DestList.DestGetJoined(aClient,'ADest.SignatureTime=:(0):',sID[i],res));
      Check(length(res)=0);
      Check(MS.DestList.DestGetJoined(aClient,
        FormatUTF8('ADest.SignatureTime=?',[],[MD.SignatureTime]),sID[i],res));
//   'ADest.SignatureTime=:('+Int64ToUTF8(MD.SignatureTime)+'):',sID[i],res));
      if CheckFailed(length(res)=1) then
        continue; // avoid GPF
      Check(res[0]=dID[i]);
      MD2 := MS.DestList.DestGetJoined(aClient,
        FormatUTF8('ADest.SignatureTime=?',[],[MD.SignatureTime]),sID[i]) as TSQLADest;
//   'ADest.SignatureTime=:('+Int64ToUTF8(MD.SignatureTime)+'):',sID[i]) as TSQLADest;
      if CheckFailed(MD2<>nil) then
        continue;
      try
        Check(MD2.FillOne);
        Check(MD2.ID=dID[i]);
        Check(MD2.Signature=FormatUTF8('% %',[aClient.ClassName,i]));
      finally
        MD2.Free;
      end;
    end;
    Check(MS.FillPrepareMany(aClient,'', [],[]));
    CheckOK;
    Check(MS.FillPrepareMany(aClient,'DestList.Dest.SignatureTime<>?',[],[0]));
    CheckOK;
    Check(MS.FillPrepareMany(aClient,
      'DestList.Dest.SignatureTime<>% and RowID>=? and DestList.AssociationTime<>0 '+
      'and SignatureTime=DestList.Dest.SignatureTime '+
      'and DestList.Dest.Signature<>"DestList.AssociationTime"',[0],[sID[1]]));
    if CheckFailed(MS.FillTable<>nil) then
      exit;
    Check(MS.FillTable.RowCount=length(sID));
    for i := 1 to high(sID) do begin
      MS.SignatureTime := 0;
      MS.DestList.Dest.SignatureTime := 0;
      if CheckFailed(MS.FillOne) then
        break;
      Check(MS.fID=sID[i]);
      Check(MS.SignatureTime=MD.fSignatureTime);
      Check(MS.DestList.AssociationTime=i);
      Check(MS.DestList.Dest.fID=dID[i]);
      Check(MS.DestList.Dest.SignatureTime=MD.fSignatureTime);
      Check(MS.DestList.Dest.Signature=FormatUTF8('% %',[aClient.ClassName,i]));
    end;
    MS.FillClose;
    Check(aClient.TransactionBegin(TSQLADests)); // faster process
    for i := 1 to high(sID) shr 2 do
      Check(MS.DestList.ManyDelete(aClient,sID[i*4],dID[i*4]));
    aClient.Commit;
    for i := 1 to high(sID) do
      if i and 3<>0 then begin
        Check(MS.DestList.ManySelect(aClient,sID[i],dID[i]));
        Check(MS.DestList.AssociationTime=i);
      end else
        Check(not MS.DestList.ManySelect(aClient,sID[i],dID[i]));
  finally
    MD.Free;
    MS.Free;
  end;
end;

type
  TSQLRecordMyHistory = class(TSQLRecordHistory);

procedure TTestExternalDatabase.ExternalRecords;
var SQL: RawUTF8;
begin
  if CheckFailed(fExternalModel=nil) then exit; // should be called once
  fExternalModel := TSQLModel.Create(
    [TSQLRecordPeopleExt,TSQLRecordOnlyBlob,TSQLRecordTestJoin,
     TSQLASource,TSQLADest,TSQLADests,TSQLRecordPeople,TSQLRecordMyHistory]);
  ReplaceParamsByNames(StringOfChar('?',200),SQL);
  Check(Hash32(SQL)=$AD27D1E0,'excludes :IF :OF');
end;

procedure TTestExternalDatabase.AutoAdaptSQL;
var SQLOrigin, s: RawUTF8;
    Props: TSQLDBConnectionProperties;
    Server: TSQLRestServer;
    Ext: TSQLRestStorageExternalHook;
  procedure Test(aDBMS: TSQLDBDefinition; AdaptShouldWork: boolean;
    const SQLExpected: RawUTF8='');
  var SQL: RawUTF8;
  begin
    SQL := SQLOrigin;
    TSQLDBConnectionPropertiesHook(Props).fDBMS := aDBMS;
    Check((Props.DBMS=aDBMS)or(aDBMS=dUnknown));
    Check(Ext.AdaptSQLForEngineList(SQL)=AdaptShouldWork);
    Check(SameTextU(SQL,SQLExpected) or not AdaptShouldWork,SQLExpected+#13#10+SQL);
  end;
  procedure Test2(const Orig,Expected: RawUTF8);
  var DBMS: TSQLDBDefinition;
  begin
    SQLOrigin := Orig;
    for DBMS := low(DBMS) to high(DBMS) do
      Test(DBMS,true,Expected);
  end;
begin
  check(ReplaceParamsByNumbers('',s)=0);
  check(s='');
  check(ReplaceParamsByNumbers('toto titi',s)=0);
  check(s='toto titi');
  check(ReplaceParamsByNumbers('toto=? titi',s)=1);
  check(s='toto=$1 titi');
  check(ReplaceParamsByNumbers('toto=? titi=?',s)=2);
  check(s='toto=$1 titi=$2');
  check(ReplaceParamsByNumbers('toto=? titi=? and a=''''',s)=2);
  check(s='toto=$1 titi=$2 and a=''''');
  check(ReplaceParamsByNumbers('toto=? titi=? and a=''dd''',s)=2);
  check(s='toto=$1 titi=$2 and a=''dd''');
  check(ReplaceParamsByNumbers('toto=? titi=? and a=''d''''d''',s)=2);
  check(s='toto=$1 titi=$2 and a=''d''''d''');
  check(ReplaceParamsByNumbers('toto=? titi=? and a=''d?d''',s)=2);
  check(s='toto=$1 titi=$2 and a=''d?d''');
  check(ReplaceParamsByNumbers('1?2?3?4?5?6?7?8?9?10?11?12? x',s)=12);
  check(s='1$12$23$34$45$56$67$78$89$910$1011$1112$12 x');
  checkequal(BoundArrayToJSONArray(TRawUTF8DynArrayFrom([])),'');
  checkequal(BoundArrayToJSONArray(TRawUTF8DynArrayFrom(['1'])),'{1}');
  checkequal(BoundArrayToJSONArray(TRawUTF8DynArrayFrom(['''1'''])),'{"1"}');
  checkequal(BoundArrayToJSONArray(TRawUTF8DynArrayFrom(['1','2','3'])),'{1,2,3}');
  checkequal(BoundArrayToJSONArray(TRawUTF8DynArrayFrom(['''1''','2','''3'''])),'{"1",2,"3"}');
  checkequal(BoundArrayToJSONArray(TRawUTF8DynArrayFrom(['''1"1''','2','''"3\'''])),'{"1\"1",2,"\"3\\"}');

  check(TSQLDBConnectionProperties.IsSQLKeyword(dUnknown,'SELEct'));
  check(not TSQLDBConnectionProperties.IsSQLKeyword(dUnknown,'toto'));
  check(TSQLDBConnectionProperties.IsSQLKeyword(dOracle,'SELEct'));
  check(not TSQLDBConnectionProperties.IsSQLKeyword(dOracle,'toto'));
  check(TSQLDBConnectionProperties.IsSQLKeyword(dOracle,' auDIT '));
  check(not TSQLDBConnectionProperties.IsSQLKeyword(dMySQL,' auDIT '));
  check(TSQLDBConnectionProperties.IsSQLKeyword(dSQLite,'SELEct'));
  check(TSQLDBConnectionProperties.IsSQLKeyword(dSQLite,'clustER'));
  check(not TSQLDBConnectionProperties.IsSQLKeyword(dSQLite,'value'));
  
  Server := TSQLRestServer.Create(fExternalModel);
  try
    Props := TSQLDBSQLite3ConnectionProperties.Create(SQLITE_MEMORY_DATABASE_NAME,'','','');
    try
      VirtualTableExternalMap(fExternalModel,TSQLRecordPeopleExt,Props,'SampleRecord').
        MapField('LastChange','Changed');
      Ext := TSQLRestStorageExternalHook.Create(TSQLRecordPeopleExt,Server);
      try
        Test2('select rowid,firstname from PeopleExt where rowid=2',
              'select id,firstname from SampleRecord where id=2');
        Test2('select rowid,firstname from PeopleExt where rowid=?',
              'select id,firstname from SampleRecord where id=?');
        Test2('select rowid,firstname from PeopleExt where rowid>=?',
              'select id,firstname from SampleRecord where id>=?');
        Test2('select rowid,firstname from PeopleExt where rowid<?',
              'select id,firstname from SampleRecord where id<?');
        Test2('select rowid,firstname from PeopleExt where rowid=2 and lastname=:(''toto''):',
              'select id,firstname from SampleRecord where id=2 and lastname=:(''toto''):');
        Test2('select rowid,firstname from PeopleExt where rowid=2 and rowID=:(2): order by rowid',
              'select id,firstname from SampleRecord where id=2 and id=:(2): order by id');
        Test2('select rowid,firstname from PeopleExt where rowid=2 or lastname=:(''toto''):',
              'select id,firstname from SampleRecord where id=2 or lastname=:(''toto''):');
        Test2('select rowid,firstname from PeopleExt where rowid=2 and not lastname like ?',
              'select id,firstname from SampleRecord where id=2 and not lastname like ?');
        Test2('select rowid,firstname from PeopleExt where rowid=2 and not (lastname like ?)',
              'select id,firstname from SampleRecord where id=2 and not (lastname like ?)');
        Test2('select rowid,firstname from PeopleExt where (rowid=2 and lastname="toto") or lastname like ?',
              'select id,firstname from SampleRecord where (id=2 and lastname="toto") or lastname like ?');
        Test2('select rowid,firstname from PeopleExt where (rowid=2 or lastname=:("toto"):) and lastname like ?',
              'select id,firstname from SampleRecord where (id=2 or lastname=:("toto"):) and lastname like ?');
        Test2('select rowid,firstname from PeopleExt where (rowid=2) and (lastname="toto" or lastname like ?)',
              'select id,firstname from SampleRecord where (id=2) and (lastname="toto" or lastname like ?)');
        Test2('select rowid,firstname from PeopleExt where (rowid=2) and (lastname=:("toto"): or (lastname like ?))',
              'select id,firstname from SampleRecord where (id=2) and (lastname=:("toto"): or (lastname like ?))');
        Test2('select rowid,firstname from PeopleExt where rowid=2 order by RowID',
              'select id,firstname from SampleRecord where id=2 order by ID');
        Test2('select rowid,firstname from PeopleExt where rowid=2 order by RowID DeSC',
              'select id,firstname from SampleRecord where id=2 order by ID desc');
        Test2('select rowid,firstname from PeopleExt order by RowID,firstName DeSC',
              'select id,firstname from SampleRecord order by ID,firstname desc');
        Test2('select rowid, firstName from PeopleExt order by RowID, firstName',
              'select id,firstname from SampleRecord order by ID,firstname');
        Test2('select rowid, firstName from PeopleExt  order by RowID, firstName asC',
              'select id,firstname from SampleRecord order by ID,firstname');
        Test2('select rowid,firstname from PeopleExt where firstname like :(''test''): order by lastname',
              'select id,firstname from SampleRecord where firstname like :(''test''): order by lastname');
        Test2('   select    COUNT(*)  from   PeopleExt   ',
              'select count(*) from SampleRecord');
        Test2('select count(*) from PeopleExt where rowid=2',
              'select count(*) from SampleRecord where id=2');
        Test2('select count(*) from PeopleExt where rowid=2 /*tobeignored*/',
              'select count(*) from SampleRecord where id=2');
        Test2('select count(*) from PeopleExt where /*tobeignored*/ rowid=2',
              'select count(*) from SampleRecord where id=2');
        Test2('select Distinct(firstname) , max(lastchange)+100 from PeopleExt where rowid >= :(2):',
              'select Distinct(FirstName),max(Changed)+100 as LastChange from SampleRecord where ID>=:(2):');
        Test2('select Distinct(lastchange) , max(rowid)-100 as newid from PeopleExt where rowid >= :(2):',
              'select Distinct(Changed) as lastchange,max(id)-100 as newid from SampleRecord where ID>=:(2):');
        SQLOrigin := 'select rowid,firstname from PeopleExt where   rowid=2   limit 2';
        Test(dUnknown,false);
        Test(dDefault,false);
        Test(dOracle,true,'select id,firstname from SampleRecord where rownum<=2 and id=2');
        Test(dMSSQL,true,'select top(2) id,firstname from SampleRecord where id=2');
        Test(dJet,true,'select top 2 id,firstname from SampleRecord where id=2');
        Test(dMySQL,true,'select id,firstname from SampleRecord where id=2 limit 2');
        Test(dSQLite,true,'select id,firstname from SampleRecord where id=2 limit 2');
        SQLOrigin := 'select rowid,firstname from PeopleExt where rowid=2 order by LastName limit 2';
        Test(dUnknown,false);
        Test(dDefault,false);
        Test(dOracle,true,'select id,firstname from SampleRecord where rownum<=2 and id=2 order by LastName');
        Test(dMSSQL,true,'select top(2) id,firstname from SampleRecord where id=2 order by LastName');
        Test(dJet,true,'select top 2 id,firstname from SampleRecord where id=2 order by LastName');
        Test(dMySQL,true,'select id,firstname from SampleRecord where id=2 order by LastName limit 2');
        Test(dSQLite,true,'select id,firstname from SampleRecord where id=2 order by LastName limit 2');
        SQLOrigin := 'select rowid,firstname from PeopleExt where firstname=:(''test''): limit 2';
        Test(dUnknown,false);
        Test(dDefault,false);
        Test(dOracle,true,'select id,firstname from SampleRecord where rownum<=2 and firstname=:(''test''):');
        Test(dMSSQL,true,'select top(2) id,firstname from SampleRecord where firstname=:(''test''):');
        Test(dJet,true,'select top 2 id,firstname from SampleRecord where firstname=:(''test''):');
        Test(dMySQL,true,'select id,firstname from SampleRecord where firstname=:(''test''): limit 2');
        Test(dSQLite,true,'select id,firstname from SampleRecord where firstname=:(''test''): limit 2');
        SQLOrigin := 'select id,firstname from PeopleExt limit 2';
        Test(dUnknown,false);
        Test(dDefault,false);
        Test(dOracle,true,'select id,firstname from SampleRecord where rownum<=2');
        Test(dMSSQL,true,'select top(2) id,firstname from SampleRecord');
        Test(dJet,true,'select top 2 id,firstname from SampleRecord');
        Test(dMySQL,true,'select id,firstname from SampleRecord limit 2');
        Test(dSQLite,true,'select id,firstname from SampleRecord limit 2');
        SQLOrigin := 'select id,firstname from PeopleExt order by firstname limit 2';
        Test(dUnknown,false);
        Test(dDefault,false);
        Test(dOracle,true,'select id,firstname from SampleRecord where rownum<=2 order by firstname');
        Test(dMSSQL,true,'select top(2) id,firstname from SampleRecord order by firstname');
        Test(dJet,true,'select top 2 id,firstname from SampleRecord order by firstname');
        Test(dMySQL,true,'select id,firstname from SampleRecord order by firstname limit 2');
        Test(dSQLite,true,'select id,firstname from SampleRecord order by firstname limit 2');
        SqlOrigin := 'SELECT RowID,firstname FROM PeopleExt WHERE :(3001): '+
          'BETWEEN firstname AND RowID LIMIT 1';
        Test(dSQLite,false);
      finally
        Ext.Free;
      end;
    finally
      Props.Free;
    end;
  finally
    Server.Free;
  end;
end;


procedure TTestExternalDatabase.CleanUp;
begin
  FreeAndNil(fExternalModel);
  FreeAndNil(fPeopleData);
  inherited;
end;

procedure TTestExternalDatabase.ExternalViaREST;
begin
  Test(true,false);
end;

procedure TTestExternalDatabase.ExternalViaVirtualTable;
begin
  Test(false,false);
end;

procedure TTestExternalDatabase.ExternalViaRESTWithChangeTracking;
begin
  Test(true,true);
end;

{$ifdef MSWINDOWS}
{$ifdef USEZEOS}
procedure TTestExternalDatabase.FirebirdEmbeddedViaZDBCOverHTTP;
var R: TSQLRecordPeople;
    Model: TSQLModel;
    Props: TSQLDBConnectionProperties;
    Server: TSQLRestServerDB;
    Http: TSQLHttpServer;
    Client: TSQLRestClientURI;
    i,n: integer;
    ids: array[0..3] of TID;
    res: TIDDynArray;
begin
  if not FileExists(FIREBIRDEMBEDDEDDLL) then
    exit;
  Model := TSQLModel.Create([TSQLRecordPeople]);
  try
    R := TSQLRecordPeople.Create;
    try
      DeleteFile('test.fdb'); // will be re-created at first connection
      Props := TSQLDBZEOSConnectionProperties.Create(
        TSQLDBZEOSConnectionProperties.URI(dFirebird,'',FIREBIRDEMBEDDEDDLL,False),
        'test.fdb','','');
      try
        VirtualTableExternalMap(Model,TSQLRecordPeople,Props,'peopleext').
          MapFields(['ID','key','YearOfBirth','yob']);
        Server := TSQLRestServerDB.Create(Model,SQLITE_MEMORY_DATABASE_NAME);
        try
          Server.CreateMissingTables;
          Http := TSQLHttpServer.Create(HTTP_DEFAULTPORT,Server);
          Client := TSQLHttpClient.Create('localhost',HTTP_DEFAULTPORT,TSQLModel.Create(Model));
          Client.Model.Owner := Client;
          try
            R.FillPrepare(fPeopleData);
            if not CheckFailed(R.fFill<>nil) then begin
              Client.BatchStart(TSQLRecordPeople,5000);
              n := 0;
              while R.FillOne do begin
                R.YearOfBirth := n;
                Client.BatchAdd(R,true);
                inc(n);
              end;
              Check(Client.BatchSend(res)=HTTP_SUCCESS);
              Check(length(res)=n);
              for i := 1 to 100 do begin
                R.ClearProperties;
                Check(Client.Retrieve(res[Random(n)],R));
                Check(R.ID<>0);
                Check(res[R.YearOfBirth]=R.ID);
              end;
            end;
            for i := 0 to high(ids) do begin
              R.YearOfBirth := i;
              ids[i] := Client.Add(R,true);
            end;
            for i := 0 to high(ids) do begin
              Check(Client.Retrieve(ids[i],R));
              Check(R.YearOfBirth=i);
            end;
            for i := 0 to high(ids) do begin
              Client.BatchStart(TSQLRecordPeople);
              Client.BatchDelete(ids[i]);
              Check(Client.BatchSend(res)=HTTP_SUCCESS);
              Check(length(res)=1);
              Check(res[0]=HTTP_SUCCESS);
            end;
            for i := 0 to high(ids) do
              Check(not Client.Retrieve(ids[i],R));
            R.ClearProperties;
            for i := 0 to high(ids) do begin
              R.fID := ids[i];
              Check(Client.Update(R),'test locking');
            end;
            for i := 0 to high(ids) do begin
              R.YearOfBirth := i;
              ids[i] := Client.Add(R,true);
            end;
            for i := 0 to high(ids) do begin
              Check(Client.Retrieve(ids[i],R));
              Check(R.YearOfBirth=i);
            end;
          finally
            Client.Free;
            Http.Free;
          end;
        finally
          Server.Free;
        end;
      finally
        Props.Free;
      end;
    finally
      R.Free;
    end;
  finally
    Model.Free;
  end;
end;
{$endif}
{$endif}

{$ifndef CPU64}
{$ifndef LVCL}
{$ifdef MSWINDOWS}
procedure TTestExternalDatabase.JETDatabase;
var R: TSQLRecordPeople;
    Model: TSQLModel;
    Props: TSQLDBConnectionProperties;
    Client: TSQLRestClientDB;
    i,n, ID,LastID: integer;
begin
  Model := TSQLModel.Create([TSQLRecordPeople]);
  try
    R := TSQLRecordPeople.Create;
    R.FillPrepare(fPeopleData);
    if not CheckFailed(R.fFill<>nil) then
    try
      DeleteFile('test.mdb');
      Props := TOleDBJetConnectionProperties.Create('test.mdb','','','');
      try
        VirtualTableExternalRegister(Model,TSQLRecordPeople,Props,'');
        Client := TSQLRestClientDB.Create(Model,nil,SQLITE_MEMORY_DATABASE_NAME,TSQLRestServerDB);
        try
          Client.Server.CreateMissingTables;
          Client.TransactionBegin(TSQLRecordPeople);
          n := 0;
          while R.FillOne do begin
            inc(n);
            Check(Client.Add(R,true,true)=R.fFill.Table.IDColumnHiddenValue(n));
            if n>999 then
              break; // Jet is very slow e.g. within the Delphi IDE
          end;
          Client.Commit;
          R.FirstName := '';
          R.LastName := '';
          R.YearOfBirth := 100;
          R.fYearOfDeath := 0;
          R.Data := '';
          LastID := Client.Add(R,true);
          for i := 1 to n do begin
            R.ClearProperties;
            ID := R.fFill.Table.IDColumnHiddenValue(n);
            Check(Client.Retrieve(ID,R));
            Check(R.fID=ID);
            Check(R.ID=ID);
            Check(R.FirstName<>'');
            Check(R.YearOfBirth>=1400);
            Check(R.YearOfDeath>=1468);
          end;
          Check(Client.Retrieve(LastID,R));
          Check(R.FirstName='');
          Check(R.LastName='');
          Check(R.YearOfBirth=100);
          Check(R.fYearOfDeath=0);
          Check(R.Data='');
        finally
          Client.Free;
        end;
      finally
        Props.Free;
      end;
    finally
      R.Free;
    end;
  finally
    Model.Free;
  end;
end;
{$endif}
{$endif}
{$endif}

{$ifndef LVCL}
procedure TTestExternalDatabase._TQuery;
var Props: TSQLDBConnectionProperties;
    Query: TQuery;
    n: integer;
begin
  Props := TSQLDBSQLite3ConnectionProperties.Create('test.db3','','','');
  try
    Query := TQuery.Create(Props.MainConnection);
    try
      Query.SQL.Add('select * from People');
      Query.SQL.Add('where YearOfDeath=:YOD;');
      Query.ParamByName('YOD').AsInteger := 1872;
      Query.Open;
      n := 0;
      while not Query.Eof do begin
        Check(Query.FieldByName('ID').AsInteger>0);
        Check(Query.FieldByName('YearOfDeath').AsInteger=1872);
        Query.Next;
        inc(n);
      end;
      Check(n>500);
    finally
      Query.Free;
    end;
  finally
    Props.Free;
  end;
end;
{$endif}


procedure TTestExternalDatabase._SynDBRemote;
var Props: TSQLDBConnectionProperties;
  procedure DoTest(proxy: TSQLDBConnectionProperties; msg: PUTF8Char);
    procedure DoTests;
    var res: ISQLDBRows;
        id,lastid,n,n1: integer;
        IDs: TIntegerDynArray;
        {$ifndef LVCL}
        Row,RowDoc: variant;
        {$endif}
    procedure DoInsert;
    var i: integer;
    begin
      for i := 0 to high(IDs) do
        Check(proxy.ExecuteNoResult(
          'INSERT INTO People (ID,FirstName,LastName,YearOfBirth,YearOfDeath) '+
          'VALUES (?,?,?,?,?)',
          [IDs[i],'FirstName New '+Int32ToUtf8(i),'New Last',i+1400,1519])=1);
    end;
    function DoCount: integer;
    var res: ISQLDBRows;
    begin
      res := proxy.Execute('select count(*) from People where YearOfDeath=?',[1519]);
      Check(res.Step);
      result := res.ColumnInt(0);
    end;
  var log: ISynLog;
  begin
    log := TSynLogTestLog.Enter(proxy,msg);
    if proxy<>Props then
      Check(proxy.UserID='user');
    proxy.ExecuteNoResult('delete from people where ID>=?',[50000]);
    res := proxy.Execute('select * from People where YearOfDeath=?',[1519]);
    Check(res<>nil);
    n := 0;
    lastid := 0;
    while res.Step do begin
      id := res.ColumnInt('ID');
      Check(id<>lastid);
      Check(id>0);
      lastid := id;
      Check(res.ColumnInt('YearOfDeath')=1519);
      inc(n);
    end;
    Check(n=DoCount);
    n1 := n;
    n := 0;
    {$ifndef LVCL}
    Row := res.RowData;
    {$endif}
    if res.Step({rewind=}true) then
    repeat
      {$ifdef LVCL}
      Check(res.ColumnInt('ID')>0);
      Check(res.ColumnInt('YearOfDeath')=1519);
      {$else}
      Check(Row.ID>0);
      Check(Row.YearOfDeath=1519);
      res.RowDocVariant(RowDoc);
      Check(RowDoc.ID=Row.ID);
      Check(_Safe(RowDoc)^.I['YearOfDeath']=1519);
      {$endif}
      inc(n);
    until not res.Step;
    res.ReleaseRows;
    Check(n=n1);
    SetLength(IDs,50);
    FillIncreasing(pointer(IDs),50000,length(IDs));
    proxy.ThreadSafeConnection.StartTransaction;
    DoInsert;
    proxy.ThreadSafeConnection.Rollback;
    Check(DoCount=n);
    proxy.ThreadSafeConnection.StartTransaction;
    DoInsert;
    proxy.ThreadSafeConnection.Commit;
    n1 := DoCount;
    Check(n1=n+length(IDs));
    proxy.ExecuteNoResult('delete from people where ID>=?',[50000]);
    Check(DoCount=n);
  end;
  begin
    try
      DoTests;
    finally
      if proxy<>Props then
        proxy.Free;
    end;
  end;
var Server: TSQLDBServerAbstract;
const ADDR='127.0.0.1:'+HTTP_DEFAULTPORT;
begin
  Props := TSQLDBSQLite3ConnectionProperties.Create('test.db3','','','');
  try
    DoTest(Props,'raw Props');
    DoTest(TSQLDBRemoteConnectionPropertiesTest.Create(
      Props,'user','pass',TSQLDBProxyConnectionProtocol),'proxy test');
    DoTest(TSQLDBRemoteConnectionPropertiesTest.Create(
      Props,'user','pass',TSQLDBRemoteConnectionProtocol),'remote test');
    Server := {$ifndef ONLYUSEHTTPSOCKET}TSQLDBServerHttpApi{$else}TSQLDBServerSockets{$endif}.
      Create(Props,'root',HTTP_DEFAULTPORT,'user','pass');
    try
      DoTest(TSQLDBSocketConnectionProperties.Create(ADDR,'root','user','pass'),'socket');
      {$ifdef USEWININET}
      DoTest(TSQLDBWinHTTPConnectionProperties.Create(ADDR,'root','user','pass'),'winhttp');
      DoTest(TSQLDBWinINetConnectionProperties.Create(ADDR,'root','user','pass'),'wininet');
      {$endif}
      {$ifdef USELIBCURL}
      DoTest(TSQLDBCurlConnectionProperties.Create(ADDR,'root','user','pass'),'libcurl');
      {$endif}
    finally
      Server.Free;
    end;
  finally
    Props.Free;
  end;
end;

procedure TTestExternalDatabase.DBPropertiesPersistence;
var Props: TSQLDBConnectionProperties;
    json: RawUTF8;
begin
  Props := TSQLDBSQLite3ConnectionProperties.Create('server','','','');
  json := Props.DefinitionToJSON(14);
  Check(json='{"Kind":"TSQLDBSQLite3ConnectionProperties","ServerName":"server","DatabaseName":"","User":"","Password":""}');
  Props.Free;
  Props := TSQLDBSQLite3ConnectionProperties.Create('server','','','1234');
  json := Props.DefinitionToJSON(14);
  Check(json='{"Kind":"TSQLDBSQLite3ConnectionProperties","ServerName":"server","DatabaseName":"","User":"","Password":"MnVfJg=="}');
  Props.DefinitionToFile('connectionprops.json');
  Props.Free;
  Props := TSQLDBConnectionProperties.CreateFromFile('connectionprops.json');
  Check(Props.ClassType=TSQLDBSQLite3ConnectionProperties);
  Check(Props.ServerName='server');
  Check(Props.DatabaseName='');
  Check(Props.UserID='');
  Check(Props.PassWord='1234');
  Props.Free;
  DeleteFile('connectionprops.json');
end;

procedure TTestExternalDatabase.CryptedDatabase;
var R,R2: TSQLRecordPeople;
    Model: TSQLModel;
    aID: integer;
    Client, Client2: TSQLRestClientDB;
    Res: TIDDynArray;
procedure CheckFilledRow;
begin
  Check(R.FillRewind);
  while R.FillOne do
  if not CheckFailed(R2.FillOne) then begin
    Check(R.ID<>0);
    Check(R2.ID<>0);
    Check(R.FirstName=R2.FirstName);
    Check(R.LastName=R2.LastName);
    Check(R.YearOfBirth=R2.YearOfBirth);
    Check(R.YearOfDeath=R2.YearOfDeath);
  end;
end;
{$ifndef NOSQLITE3ENCRYPT}
const password = 'pass';
{$else}
const password = '';
{$endif}
begin
  DeleteFile('testpass.db3');
  Model := TSQLModel.Create([TSQLRecordPeople]);
  try
    Client := TSQLRestClientDB.Create(Model,nil,'test.db3',TSQLRestServerDB,false,'');
    try
      R := TSQLRecordPeople.Create;
      Assert(fPeopleData=nil);
      fPeopleData := Client.List([TSQLRecordPeople],'*');
      R.FillPrepare(fPeopleData);
      try
        Client2 := TSQLRestClientDB.Create(Model,nil,'testpass.db3',TSQLRestServerDB,false,password);
        try
          Client2.Server.DB.Synchronous := smOff;
          Client2.Server.DB.LockingMode := lmExclusive;
          Client2.Server.DB.WALMode := true;
          Client2.Server.CreateMissingTables;
          Check(Client2.TransactionBegin(TSQLRecordPeople));
          Check(Client2.BatchStart(TSQLRecordPeople));
          Check(Client2.BatchSend(Res)=200,'Void batch');
          Check(Res=nil);
          Client2.Commit;
          Check(Client2.TransactionBegin(TSQLRecordPeople));
          Check(Client2.BatchStart(TSQLRecordPeople));
          while R.FillOne do begin
            Check(R.ID<>0);
            Check(Client2.BatchAdd(R,true)>=0);
          end;
          Check(Client2.BatchSend(Res)=200,'INSERT batch');
          Client2.Commit;
        finally
          Client2.Free;
        end;
        Check(IsSQLite3File('testpass.db3'));
        Check(IsSQLite3FileEncrypted('testpass.db3')=(password<>''),'encrypt1');
        // try to read then update the crypted file
        Client2 := TSQLRestClientDB.Create(Model,nil,'testpass.db3',TSQLRestServerDB,false,password);
        try
          Client2.Server.DB.Synchronous := smOff;
          Client2.Server.DB.LockingMode := lmExclusive;
          R2 := TSQLRecordPeople.CreateAndFillPrepare(Client2,'');
          try
            CheckFilledRow;
            R2.FirstName := 'One';
            aID := Client2.Add(R2,true);
            Check(aID<>0);
            R2.FillPrepare(Client2,'');
            CheckFilledRow;
            R2.ClearProperties;
            Check(R2.FirstName='');
            Check(Client2.Retrieve(aID,R2));
            Check(R2.FirstName='One');
          finally
            R2.Free;
          end;
        finally
          Client2.Free;
        end;
        Check(IsSQLite3File('testpass.db3'));
        Check(IsSQLite3FileEncrypted('testpass.db3')=(password<>''),'encrypt2');
        {$ifndef NOSQLITE3ENCRYPT}
        // now read it after uncypher
        check(ChangeSQLEncryptTablePassWord('testpass.db3',password,''));
        Check(IsSQLite3File('testpass.db3'));
        Check(not IsSQLite3FileEncrypted('testpass.db3'),'encrypt3');
        Client2 := TSQLRestClientDB.Create(Model,nil,'testpass.db3',TSQLRestServerDB,false,'');
        try
          R2 := TSQLRecordPeople.CreateAndFillPrepare(Client2,'');
          try
            CheckFilledRow;
            R2.ClearProperties;
            Check(R2.FirstName='');
            Check(Client2.Retrieve(aID,R2));
            Check(R2.FirstName='One');
          finally
            R2.Free;
          end;
        finally
          Client2.Free;
        end;
        {$endif}
      finally
        R.Free;
      end;
    finally
      Client.Free;
    end;
  finally
    Model.Free;
  end;
end;


procedure TTestExternalDatabase.Test(StaticVirtualTableDirect, TrackChanges: boolean);
const BLOB_MAX = 1000;
var RInt,RInt1: TSQLRecordPeople;
    RExt: TSQLRecordPeopleExt;
    RBlob: TSQLRecordOnlyBlob;
    RJoin: TSQLRecordTestJoin;
    RHist: TSQLRecordMyHistory;
    Tables: TRawUTF8DynArray;
    i,n, aID: integer;
    ok: Boolean;
    BatchID,BatchIDUpdate,BatchIDJoined: TIDDynArray;
    ids: array[0..3] of TID;
    aExternalClient: TSQLRestClientDB;
    fProperties: TSQLDBConnectionProperties;
    {$ifndef NOVARIANTS}
    json: RawUTF8;
    {$endif}
    Start, Updated: TTimeLog; // will work with both TModTime and TCreateTime properties
procedure HistoryCheck(aIndex,aYOB: Integer; aEvent: TSQLHistoryEvent);
var Event: TSQLHistoryEvent;
    Timestamp: TModTime;
    R: TSQLRecordPeopleExt;
begin
  RExt.ClearProperties;
  Check(RHist.HistoryGet(aIndex,Event,Timestamp,RExt));
  Check(Event=aEvent);
  Check(Timestamp>=Start);
  if Event=heDelete then
    exit;
  Check(RExt.ID=400);
  Check(RExt.FirstName='Franz36');
  Check(RExt.YearOfBirth=aYOB);
  R := RHist.HistoryGet(aIndex) as TSQLRecordPeopleExt;
  if CheckFailed(R<>nil) then
    exit;
  Check(R.ID=400);
  Check(R.FirstName='Franz36');
  Check(R.YearOfBirth=aYOB);
  R.Free;
end;
procedure HistoryChecks;
var i: integer;
begin
  RHist := TSQLRecordMyHistory.CreateHistory(aExternalClient,TSQLRecordPeopleExt,400);
  try
    Check(RHist.HistoryCount=504);
    HistoryCheck(0,1797,heAdd);
    HistoryCheck(1,1828,heUpdate);
    HistoryCheck(2,1515,heUpdate);
    for i := 1 to 500 do
      HistoryCheck(i+2,i,heUpdate);
    HistoryCheck(503,0,heDelete);
  finally
    RHist.Free;
  end;
end;
var historyDB: TSQLRestServerDB;
begin
  // run tests over an in-memory SQLite3 external database (much faster than file)
  DeleteFile('extdata.db3');
  fProperties := TSQLDBSQLite3ConnectionProperties.Create('extdata.db3','','','');
  (fProperties.MainConnection as TSQLDBSQLite3Connection).Synchronous := smOff;
  (fProperties.MainConnection as TSQLDBSQLite3Connection).LockingMode := lmExclusive;
  Check(VirtualTableExternalMap(fExternalModel,TSQLRecordPeopleExt,fProperties,'PeopleExternal').
    MapField('ID','Key').
    MapField('YearOfDeath','YOD').
    MapAutoKeywordFields<>nil);
  Check(VirtualTableExternalRegister(fExternalModel,TSQLRecordOnlyBlob,fProperties,'OnlyBlobExternal'));
  Check(VirtualTableExternalRegister(fExternalModel,TSQLRecordTestJoin,fProperties,'TestJoinExternal'));
  Check(VirtualTableExternalRegister(fExternalModel,TSQLASource,fProperties,'SourceExternal'));
  Check(VirtualTableExternalRegister(fExternalModel,TSQLADest,fProperties,'DestExternal'));
  Check(VirtualTableExternalRegister(fExternalModel,TSQLADests,fProperties,'DestsExternal'));
  DeleteFile('testExternal.db3'); // need a file for backup testing
  if TrackChanges and StaticVirtualTableDirect then begin
    DeleteFile('history.db3');
    historyDB := TSQLRestServerDB.Create(
      TSQLModel.Create([TSQLRecordMyHistory],'history'),
      'history.db3',false);
  end else
    historyDB := nil;
  aExternalClient := TSQLRestClientDB.Create(fExternalModel,nil,'testExternal.db3',TSQLRestServerDB);
  try
    if historyDB<>nil then begin
      historyDB.Model.Owner := historyDB;
      historyDB.DB.Synchronous := smOff;
      historyDB.DB.LockingMode := lmExclusive;
      historyDB.CreateMissingTables;
      Check(aExternalClient.Server.RemoteDataCreate(TSQLRecordMyHistory,historyDB)<>nil,
        'TSQLRecordMyHistory should not be accessed from an external process');
    end;
    aExternalClient.Server.DB.Synchronous := smOff;
    aExternalClient.Server.DB.LockingMode := lmExclusive;
    aExternalClient.Server.DB.GetTableNames(Tables);
    Check(Tables=nil); // we reset the testExternal.db3 file
    Start := aExternalClient.ServerTimestamp;
    aExternalClient.Server.StaticVirtualTableDirect := StaticVirtualTableDirect;
    aExternalClient.Server.CreateMissingTables;
    if TrackChanges then
      aExternalClient.Server.TrackChanges([TSQLRecordPeopleExt],TSQLRecordMyHistory,100,10,65536);
    Check(aExternalClient.Server.CreateSQLMultiIndex(
      TSQLRecordPeopleExt,['FirstName','LastName'],false));
    InternalTestMany(self,aExternalClient);
    assert(fPeopleData<>nil);
    RInt := TSQLRecordPeople.Create;
    RInt1 := TSQLRecordPeople.Create;
    try
      RInt.FillPrepare(fPeopleData);
      Check(RInt.FillTable<>nil);
      Check(RInt.FillTable.RowCount>0);
      Check(not aExternalClient.TableHasRows(TSQLRecordPeopleExt));
      Check(aExternalClient.TableRowCount(TSQLRecordPeopleExt)=0);
      Check(not aExternalClient.Server.TableHasRows(TSQLRecordPeopleExt));
      Check(aExternalClient.Server.TableRowCount(TSQLRecordPeopleExt)=0);
      RExt := TSQLRecordPeopleExt.Create;
      try
        n := 0;
        while RInt.FillOne do begin
          if RInt.fID<100 then // some real entries for backup testing
            aExternalClient.Add(RInt,true,true);
          RExt.Data := RInt.Data;
          RExt.FirstName := RInt.FirstName;
          RExt.LastName := RInt.LastName;
          RExt.YearOfBirth := RInt.YearOfBirth;
          RExt.YearOfDeath := RInt.YearOfDeath;
          {$ifndef NOVARIANTS}
          RExt.Value := ValuesToVariantDynArray(['text',RInt.YearOfDeath]);
          {$endif}
          RExt.fLastChange := 0;
          RExt.CreatedAt := 0;
          if RInt.fID>100 then begin
            if aExternalClient.BatchCount=0 then
              aExternalClient.BatchStart(TSQLRecordPeopleExt,5000);
            aExternalClient.BatchAdd(RExt,true);
          end else begin
            aID := aExternalClient.Add(RExt,true);
            Check(aID<>0);
            Check(RExt.LastChange>=Start);
            Check(RExt.CreatedAt>=Start);
            RExt.ClearProperties;
            Check(RExt.YearOfBirth=0);
            Check(RExt.FirstName='');
            {$ifndef NOVARIANTS}
            Check(RExt.Value=nil);
            {$endif}
            Check(aExternalClient.Retrieve(aID,RExt));
            Check(RExt.FirstName=RInt.FirstName);
            Check(RExt.LastName=RInt.LastName);
            Check(RExt.YearOfBirth=RInt.YearOfBirth);
            Check(RExt.YearOfDeath=RInt.YearOfDeath);
            Check(RExt.YearOfBirth<>RExt.YearOfDeath);
            {$ifndef NOVARIANTS}
            json := FormatUTF8('["text",%]',[RInt.YearOfDeath]);
            Check(VariantDynArrayToJSON(RExt.Value)=json);
            {$endif}
          end;
          inc(n);
        end;
        Check(aExternalClient.Retrieve(1,RInt1));
        Check(RInt1.fID=1);
        Check(n=fPeopleData.RowCount);
        Check(aExternalClient.BatchSend(BatchID)=HTTP_SUCCESS);
        Check(length(BatchID)=n-99);
        Check(aExternalClient.TableHasRows(TSQLRecordPeopleExt));
        Check(aExternalClient.TableMaxID(TSQLRecordPeopleExt)=n);
        Check(aExternalClient.TableRowCount(TSQLRecordPeopleExt)=n);
        Check(aExternalClient.Server.TableHasRows(TSQLRecordPeopleExt));
        Check(aExternalClient.Server.TableRowCount(TSQLRecordPeopleExt)=n);
        Check(RInt.FillRewind);
        while RInt.FillOne do begin
          RExt.FillPrepare(aExternalClient,'FirstName=? and LastName=?',
            [RInt.FirstName,RInt.LastName]); // query will use index -> fast :)
          while RExt.FillOne do begin
            Check(RExt.FirstName=RInt.FirstName);
            Check(RExt.LastName=RInt.LastName);
            Check(RExt.YearOfBirth=RInt.YearOfBirth);
            Check(RExt.YearOfDeath=RInt.YearOfDeath);
            Check(RExt.YearOfBirth<>RExt.YearOfDeath);
            {$ifndef NOVARIANTS}
            Check(VariantDynArrayToJSON(RExt.Value)=FormatUTF8('["text",%]',[RInt.YearOfDeath]));
            {$endif}
          end;
        end;
        Updated := aExternalClient.ServerTimestamp;
        Check(Updated>=Start);
        for i := 1 to BatchID[high(BatchID)] do
          if i mod 100=0 then begin
            RExt.fLastChange := 0;
            RExt.CreatedAt := 0;
            {$ifndef NOVARIANTS}
            RExt.Value := nil;
            {$endif}
            Check(aExternalClient.Retrieve(i,RExt,true),'for update');
            Check(RExt.YearOfBirth<>RExt.YearOfDeath);
            Check(RExt.CreatedAt<=Updated);
            {$ifndef NOVARIANTS}
            Check(VariantDynArrayToJSON(RExt.Value)=FormatUTF8('["text",%]',[RExt.YearOfDeath]));
            {$endif}
            RExt.YearOfBirth := RExt.YearOfDeath; // YOB=YOD for 1/100 rows
            if i>4000 then begin
              if aExternalClient.BatchCount=0 then
                aExternalClient.BatchStart(TSQLRecordPeopleExt,10000);
              Check(aExternalClient.BatchUpdate(RExt)>=0,'BatchUpdate 1/100 rows');
            end else begin
              Check(aExternalClient.Update(RExt),'Update 1/100 rows');
              Check(aExternalClient.UnLock(RExt));
              Check(RExt.LastChange>=Updated);
              RExt.ClearProperties;
              {$ifndef NOVARIANTS}
              Check(RExt.Value=nil);
              {$endif}
              Check(RExt.YearOfDeath=0);
              Check(RExt.YearOfBirth=0);
              Check(RExt.CreatedAt=0);
              Check(aExternalClient.Retrieve(i,RExt),'after update');
              Check(RExt.YearOfBirth=RExt.YearOfDeath);
              Check(RExt.CreatedAt>=Start);
              Check(RExt.CreatedAt<=Updated);
              Check(RExt.LastChange>=Updated);
              {$ifndef NOVARIANTS}
              Check(VariantDynArrayToJSON(RExt.Value)=FormatUTF8('["text",%]',[RExt.YearOfDeath]));
              {$endif}
            end;
          end;
        Check(aExternalClient.BatchSend(BatchIDUpdate)=HTTP_SUCCESS);
        Check(length(BatchIDUpdate)=70);
        for i := 1 to BatchID[high(BatchID)] do
          if i and 127=0 then
          if i>4000 then begin
            if aExternalClient.BatchCount=0 then
              aExternalClient.BatchStart(TSQLRecordPeopleExt);
            Check(aExternalClient.BatchDelete(i)>=0,'BatchDelete 1/128 rows');
          end else
            Check(aExternalClient.Delete(TSQLRecordPeopleExt,i),'Delete 1/128 rows');
        Check(aExternalClient.BatchSend(BatchIDUpdate)=HTTP_SUCCESS);
        Check(length(BatchIDUpdate)=55);
        n := aExternalClient.TableRowCount(TSQLRecordPeople);
        Check(aExternalClient.Server.TableRowCount(TSQLRecordPeopleExt)=10925);
        Check(aExternalClient.Server.StaticVirtualTable[TSQLRecordPeople]=nil);
        Check(aExternalClient.Server.StaticVirtualTable[TSQLRecordPeopleExt]<>nil);
        Check(aExternalClient.Server.StaticVirtualTable[TSQLRecordOnlyBlob]<>nil);
        {$ifdef WITHUNSAFEBACKUP}
        aExternalClient.Server.BackupGZ(aExternalClient.Server.DB.FileName+'.gz');
        Check(aExternalClient.Server.StaticVirtualTable[TSQLRecordPeople]=nil);
        Check(aExternalClient.Server.StaticVirtualTable[TSQLRecordPeopleExt]<>nil);
        Check(aExternalClient.Server.StaticVirtualTable[TSQLRecordOnlyBlob]<>nil);
        {$endif}
        for i := 1 to BatchID[high(BatchID)] do begin
          RExt.fLastChange := 0;
          RExt.CreatedAt := 0;
          RExt.YearOfBirth := 0;
          ok := aExternalClient.Retrieve(i,RExt,false);
          Check(ok=(i and 127<>0),'deletion');
          if ok then begin
            {$ifndef NOVARIANTS}
            Check(VariantDynArrayToJSON(RExt.Value)=FormatUTF8('["text",%]',[RExt.YearOfDeath]));
            {$endif}
            Check(RExt.CreatedAt>=Start);
            Check(RExt.CreatedAt<=Updated);
            if i mod 100=0 then begin
              Check(RExt.YearOfBirth=RExt.YearOfDeath,'Update');
              Check(RExt.LastChange>=Updated);
            end else begin
              Check(RExt.YearOfBirth<>RExt.YearOfDeath,'Update');
              Check(RExt.LastChange>=Start);
              Check(RExt.LastChange<=Updated);
            end;
          end;
        end;
        aExternalClient.Retrieve(400,RExt);
        Check(RExt.fID=400);
        Check(RExt.FirstName='Franz36');
        Check(RExt.YearOfBirth=1828);
        aExternalClient.UpdateField(TSQLRecordPeopleExt,400,'YearOfBirth',[1515]);
        RInt1.ClearProperties;
        Check(aExternalClient.Retrieve(1,RInt1));
        Check(RInt1.fID=1);
        {$ifdef WITHUNSAFEBACKUP}
        RInt1.YearOfBirth := 1972;
        Check(aExternalClient.Update(RInt1)); // for RestoreGZ() below
        Check(aExternalClient.TableRowCount(TSQLRecordPeople)=n);
        {$endif} // life backup/restore does not work with current sqlite3-64.dll
        for i := 0 to high(ids) do begin
          RExt.YearOfBirth := i;
          ids[i] := aExternalClient.Add(RExt,true);
        end;
        for i := 0 to high(ids) do begin
          Check(aExternalClient.Retrieve(ids[i],RExt));
          Check(RExt.YearOfBirth=i);
        end;
        for i := 0 to high(ids) do begin
          aExternalClient.BatchStart(TSQLRecordPeopleExt);
          aExternalClient.BatchDelete(ids[i]);
          Check(aExternalClient.BatchSend(BatchID)=HTTP_SUCCESS);
          Check(length(BatchID)=1);
          Check(BatchID[0]=HTTP_SUCCESS);
        end;
        for i := 0 to high(ids) do
          Check(not aExternalClient.Retrieve(ids[i],RExt));
        RExt.ClearProperties;
        for i := 0 to high(ids) do begin
          RExt.fID := ids[i];
          Check(aExternalClient.Update(RExt),'test locking');
        end;
      finally
        RExt.Free;
      end;
      RJoin := TSQLRecordTestJoin.Create;
      try
        aExternalClient.BatchStart(TSQLRecordTestJoin,1000);
        for i := 1 to BLOB_MAX do
        if i and 127<>0 then begin
          RJoin.Name := Int32ToUTF8(i);
          RJoin.People := TSQLRecordPeopleExt(i);
          aExternalClient.BatchAdd(RJoin,true);
        end;
        Check(aExternalClient.BatchSend(BatchIDJoined)=HTTP_SUCCESS);
        Check(length(BatchIDJoined)=993);
        RJoin.FillPrepare(aExternalClient);
        Check(RJoin.FillTable.RowCount=993);
        i := 1;
        while RJoin.FillOne do begin
          if i and 127=0 then
            inc(i); // deleted item
          Check(GetInteger(pointer(RJoin.Name))=i);
          Check(RJoin.People.ID=i,'retrieve ID from pointer');
          inc(i);
        end;
      finally
        RJoin.Free;
      end;
      for i := 0 to high(BatchIDJoined) do begin
        RJoin := TSQLRecordTestJoin.CreateJoined(aExternalClient,BatchIDJoined[i]);
        try
          Check(RJoin.FillTable.FieldType(0)=sftInteger);
          Check(RJoin.FillTable.FieldType(3)=sftUTF8Text);
          Check(RJoin.ID=BatchIDJoined[i]);
          Check(PtrUInt(RJoin.People)>1000);
          Check(GetInteger(pointer(RJoin.Name))=RJoin.People.ID);
          {$ifndef NOVARIANTS}
          Check(length(RJoin.People.Value)=2);
          Check(RJoin.People.Value[0]='text');
          Check(RJoin.People.Value[1]=RJoin.People.YearOfDeath);
          {$endif}
          RJoin.ClearProperties;
          Check(RJoin.ID=0);
          Check(RJoin.People.ID=0);
        finally
          RJoin.Free;
        end;
      end;
      Check(not aExternalClient.Server.TableHasRows(TSQLRecordOnlyBlob));
      Check(aExternalClient.Server.TableRowCount(TSQLRecordOnlyBlob)=0);
      RBlob := TSQLRecordOnlyBlob.Create;
      try
        aExternalClient.ForceBlobTransfertTable[TSQLRecordOnlyBlob] := true;
        aExternalClient.TransactionBegin(TSQLRecordOnlyBlob);
        for i := 1 to BLOB_MAX do begin
          Rblob.Data := Int32ToUtf8(i);
          Check(aExternalClient.Add(RBlob,true)=i);
          Check(RBlob.ID=i);
        end;
        aExternalClient.Commit;
        for i := 1 to BLOB_MAX do begin
          Check(aExternalClient.Retrieve(i,RBlob));
          Check(GetInteger(pointer(RBlob.Data))=i);
        end;
        aExternalClient.TransactionBegin(TSQLRecordOnlyBlob);
        for i := BLOB_MAX downto 1 do begin
          RBlob.fID := i;
          RBlob.Data := Int32ToUtf8(i*2);
          Check(aExternalClient.Update(RBlob));
        end;
        aExternalClient.Commit;
        for i := 1 to BLOB_MAX do begin
          Check(aExternalClient.Retrieve(i,RBlob));
          Check(GetInteger(pointer(RBlob.Data))=i*2);
        end;
        aExternalClient.ForceBlobTransfertTable[TSQLRecordOnlyBlob] := false;
        RBlob.ClearProperties;
        for i := 1 to BLOB_MAX do begin
          Check(aExternalClient.Retrieve(i,RBlob));
          Check(RBlob.Data='');
        end;
      finally
        RBlob.Free;
      end;
      Check(aExternalClient.TableHasRows(TSQLRecordOnlyBlob));
      Check(aExternalClient.TableRowCount(TSQLRecordOnlyBlob)=1000);
      Check(aExternalClient.TableRowCount(TSQLRecordPeople)=n);
      RInt1.ClearProperties;
      {$ifdef WITHUNSAFEBACKUP}
      aExternalClient.Retrieve(1,RInt1);
      Check(RInt1.fID=1);
      Check(RInt1.FirstName='Salvador1');
      Check(RInt1.YearOfBirth=1972);
      Check(aExternalClient.Server.RestoreGZ(aExternalClient.Server.DB.FileName+'.gz'));
      {$endif} // life backup/restore does not work with current sqlite3-64.dll
      Check(aExternalClient.Server.StaticVirtualTable[TSQLRecordPeople]=nil);
      Check(aExternalClient.Server.StaticVirtualTable[TSQLRecordPeopleExt]<>nil);
      Check(aExternalClient.Server.StaticVirtualTable[TSQLRecordOnlyBlob]<>nil);
      Check(aExternalClient.TableHasRows(TSQLRecordPeople));
      Check(aExternalClient.TableRowCount(TSQLRecordPeople)=n);
      RInt1.ClearProperties;
      aExternalClient.Retrieve(1,RInt1);
      Check(RInt1.fID=1);
      Check(RInt1.FirstName='Salvador1');
      Check(RInt1.YearOfBirth=1904);
    finally
      RInt.Free;
      RInt1.Free;
    end;
    if TrackChanges then begin
      RExt := TSQLRecordPeopleExt.Create;
      try
        RHist := TSQLRecordMyHistory.CreateHistory(aExternalClient,TSQLRecordPeopleExt,400);
        try
          Check(RHist.HistoryCount=3);
          HistoryCheck(0,1797,heAdd);
          HistoryCheck(1,1828,heUpdate);
          HistoryCheck(2,1515,heUpdate);
         finally
          RHist.Free;
        end;
        for i := 1 to 500 do begin
          RExt.YearOfBirth := i;
          aExternalClient.Update(RExt,'YearOfBirth');
        end;
        aExternalClient.Delete(TSQLRecordPeopleExt,400);
        HistoryChecks;
        aExternalClient.Server.TrackChangesFlush(TSQLRecordMyHistory);
        HistoryChecks;
      finally
        RExt.Free;
      end;
    end;
  finally
    aExternalClient.Free;
    fProperties.Free;
    historyDB.Free;
  end;
end;


procedure TTestSQLite3Engine._TSQLRestClientDB;
var V,V2: TSQLRecordPeople;
    VA: TSQLRecordPeopleArray;
{$ifndef LVCL}
    VO: TSQLRecordPeopleObject;
{$endif}
    VP: TSQLRecordCustomProps;
    FV: TFV;
    ModelC: TSQLModel;
    Client: TSQLRestClientDB;
    Server: TSQLRestServer;
    aStatic: TSQLRestStorageInMemory;
    Curr: Currency;
    DaVinci, s: RawUTF8;
    Refreshed: boolean;
    J: TSQLTableJSON;
    i, n, nupd, ndx: integer;
    IntArray: TInt64DynArray;
    Results: TIDDynArray;
    List: TObjectList;
    Data: TSQLRawBlob;
    DataS: THeapMemoryStream;
    a,b: double;
    BackupFN: TFileName;
procedure checks(Leonard: boolean; Client: TSQLRestClient; const msg: string);
var ID: integer;
begin
  ID := V.ID; // ClearProperties do ID := 0;
  V.ClearProperties; // reset values
  Check(Client.Retrieve(ID,V),msg); // internaly call URL()
  if Leonard then
    Check(V.FirstName='Leonard') else
    Check(V.FirstName='Leonardo1',msg);
  Check(V.LastName=DaVinci,msg);
  Check(V.YearOfBirth=1452,msg);
  Check(V.YearOfDeath=1519,msg);
end;
procedure TestDynArray(aClient: TSQLRestClient);
var i, j, k, l: integer;
    IDs: TInt64DynArray;
begin
  VA.ClearProperties;
  for i := 1 to n do begin
    aClient.Retrieve(i,VA);
    Check(VA.ID=i);
    Check(VA.LastName='Dali');
    Check(length(VA.Ints)=i shr 5);
    Check(length(VA.Currency)=i shr 5);
    Check(length(VA.FileVersion)=i shr 5);
    if i and 31=0 then begin
      Check(VA.UTF8='');
      for j := 0 to high(VA.Ints) do
        Check(VA.Ints[j]=(j+1) shl 5);
      for j := 0 to high(VA.Currency) do
        Check(PInt64(@VA.Currency[j])^=(j+1)*3200);
      for j := 0 to high(VA.FileVersion) do
        with VA.FileVersion[j] do begin
          k := (j+1) shl 5;
          Check(Major=k);
          Check(Minor=k+2000);
          Check(Release=k+3000);
          Check(Build=k+4000);
          Check(Main=IntToStr(k));
          Check(Detailed=IntToStr(k+1000));
        end;
    end else begin
      Check(GetInteger(pointer(VA.UTF8))=i);
      for j := 0 to high(VA.FileVersion) do
        with VA.FileVersion[j] do begin
          k := (j+1) shl 5;
          Check(Major=k);
          Check(Minor=k+2000);
          Check(Release=k+3000);
          Check(Build=k+4000);
        end;
    end;
{$ifdef PUBLISHRECORD}
    Check(VA.fRec.nPhrase=i);
    Check(VA.fRec.nCol=i*2);
    Check(VA.fRec.hits[2].docs_with_hits=i*3);
{$endif PUBLISHRECORD}
  end;
  for i := 1 to n shr 5 do begin
    k := i shl 5;
    aClient.OneFieldValues(TSQLRecordPeopleArray,'ID',
      FormatUTF8('IntegerDynArrayContains(Ints,?)',[],[k]),IDs);
    l := n+1-32*i;
    Check(length(IDs)=l);
    for j := 0 to high(IDs) do
      Check(IDs[j]=k+j);
    aClient.OneFieldValues(TSQLRecordPeopleArray,'ID',
      FormatUTF8('CardinalDynArrayContains(Ints,?)',[],[k]),IDs);
    Check(length(IDs)=l);
    for j := 0 to high(IDs) do
      Check(IDs[j]=k+j);
    aClient.OneFieldValues(TSQLRecordPeopleArray,'ID',
      FormatUTF8('MyIntegerDynArrayContains(Ints,:("%"):)',
        [BinToBase64WithMagic(@k,sizeof(k))]),IDs);
    Check(length(IDs)=l);
    for j := 0 to high(IDs) do
      Check(IDs[j]=k+j);
  end;
end;
{$ifndef LVCL}
procedure TestObject(aClient: TSQLRestClient);
var i, j, k: integer;
begin
  for i := 1 to n do begin
    VO.ClearProperties;
    aClient.Retrieve(i,VO);
    Check(VO.ID=i);
    Check(VO.LastName='Morse');
    Check(VO.UTF8.Count=i shr 5);
    for j := 0 to VO.UTF8.Count-1 do
      Check(GetInteger(pointer(VO.UTF8[j]))=(j+1) shl 5);
    Check(VO.Persistent.One.Length=i);
    Check(VO.Persistent.One.Color=i+100);
    Check(GetInteger(pointer(VO.Persistent.One.Name))=i);
    Check(VO.Persistent.Coll.Count=i shr 5);
    for j := 0 to VO.Persistent.Coll.Count-1 do
     with VO.Persistent.Coll[j] do begin
       k := (j+1) shl 5;
       Check(Color=k+1000);
       Check(Length=k*2);
       Check(GetInteger(pointer(Name))=k*3);
     end;
  end;
end;
{$endif LVCL}
procedure TestFTS3(aClient: TSQLRestClient);
var FTS: TSQLFTSTest;
    StartID, i: integer;
    IntResult: TIDDynArray;
    c: Char;
const COUNT=400;
begin
  if CheckFailed(Length(IntArray)>COUNT*2) then
    exit;
  FTS := TSQLFTSTest.Create;
  try
    if aClient=Client then
      StartID := 0 else
      StartID := COUNT;
    Check(aClient.TransactionBegin(TSQLFTSTest)); // MUCH faster with this
    for i := StartID to StartID+COUNT-1 do begin
      FTS.DocID := IntArray[i];
      FTS.Subject := aClient.OneFieldValue(TSQLRecordPeople,'FirstName',FTS.DocID);
      Check(IdemPChar(pointer(FTS.Subject),'SALVADOR'));
      FTS.Body := FTS.Subject+' bodY'+IntToStr(FTS.DocID);
      aClient.Add(FTS,true);
    end;
    aClient.Commit; // Commit must be BEFORE OptimizeFTS3, memory leak otherwise
    Check(FTS.OptimizeFTS3Index(Client.Server));
    for i := StartID to StartID+COUNT-1 do begin
      Check(IdemPChar(pointer(aClient.OneFieldValue(TSQLFTSTest,'Subject',IntArray[i])),'SALVADOR'));
      FTS.DocID := 0;
      FTS.Subject := '';
      FTS.Body := '';
      Check(aClient.Retrieve(IntArray[i],FTS));
      Check(FTS.DocID=IntArray[i]);
      Check(IdemPChar(pointer(FTS.Subject),'SALVADOR'));
      Check(PosEx(Int32ToUtf8(FTS.DocID),FTS.Body,1)>0);
    end;
    Check(aClient.FTSMatch(TSQLFTSTest,'Subject MATCH "salVador1"',IntResult));
    for i := 0 to high(IntResult) do
      Check(SameTextU(aClient.OneFieldValue(
        TSQLRecordPeople,'FirstName',IntResult[i]),'SALVADOR1'));
    Check(aClient.FTSMatch(TSQLFTSTest,'Subject MATCH "salVador1*"',IntResult));
    for i := 0 to high(IntResult) do
      Check(IdemPChar(pointer(aClient.OneFieldValue(
        TSQLRecordPeople,'FirstName',IntResult[i])),'SALVADOR1'));
    Check(not aClient.FTSMatch(TSQLFTSTest,'body*',IntResult,[1]),'invalid count');
    for c := '1' to '9' do begin
      Check(aClient.FTSMatch(TSQLFTSTest,'Body MATCH "body'+c+'*"',IntResult));
      Check(length(IntResult)>0);
      for i := 0 to high(IntResult) do
        Check(IntToStr(IntResult[i])[1]=c);
      Check(aClient.FTSMatch(TSQLFTSTest,'body'+c+'*',IntResult,[1,0.5]),'rank');
      Check(length(IntResult)>0);
      for i := 0 to high(IntResult) do
        Check(IntToStr(IntResult[i])[1]=c);
    end;
  finally
    FTS.Free;
  end;
end;
procedure TestVirtual(aClient: TSQLRestClient; DirectSQL: boolean; const Msg: string;
  aClass: TSQLRecordClass);
var n, i, ndx, added: integer;
    VD, VD2: TSQLRecordDali1;
    Rest: TSQLRest;
    stor: TSQLRestStorageInMemoryExternal;
    fn: TFileName;
begin
  Client.Server.StaticVirtualTableDirect := DirectSQL;
  Check(Client.Server.ExecuteFmt('DROP TABLE %',[aClass.SQLTableName]));
  Client.Server.CreateMissingTables;
  VD := aClass.Create as TSQLRecordDali1;
  try
    if aClient.TransactionBegin(aClass) then
    try
      // add some items to the file
      V2.FillPrepare(aClient,'LastName=:("Dali"):');
      n := 0;
      while V2.FillOne do begin
        VD.FirstName := V2.FirstName;
        VD.YearOfBirth := V2.YearOfBirth;
        VD.YearOfDeath := V2.YearOfDeath;
        inc(n);
        added := aClient.Add(VD,true);
        CheckUTF8(added=n,'% Add %<>%',[Msg,added,n]);
      end;
      // update some items in the file
      Check(aClient.TableRowCount(aClass)=1001,'Check SQL Count(*)');
      for i := 1 to n do begin
        VD.ClearProperties;
        Check(VD.ID=0);
        Check(VD.FirstName='');
        Check(VD.YearOfBirth=0);
        Check(VD.YearOfDeath=0);
        Check(aClient.Retrieve(i,VD),Msg);
        Check(VD.ID=i);
        Check(IdemPChar(pointer(VD.FirstName),'SALVADOR'));
        Check(VD.YearOfBirth=1904);
        Check(VD.YearOfDeath=1989);
        VD.YearOfBirth := VD.YearOfBirth+i;
        VD.YearOfDeath := VD.YearOfDeath+i;
        Check(aClient.Update(VD),Msg);
      end;
      // check SQL requests
      for i := 1 to n do begin
        VD.ClearProperties;
        Check(VD.ID=0);
        Check(VD.FirstName='');
        Check(VD.YearOfBirth=0);
        Check(VD.YearOfDeath=0);
        CheckUTF8(aClient.Retrieve(i,VD),'% Retrieve',[Msg]);
        Check(IdemPChar(pointer(VD.FirstName),'SALVADOR'));
        Check(VD.YearOfBirth=1904+i);
        Check(VD.YearOfDeath=1989+i);
      end;
      CheckUTF8(aClient.TableRowCount(aClass)=1001,'% RowCount',[Msg]);
      Rest := Client.Server.StaticVirtualTable[aClass];
      Check((Rest as TSQLRestStorageInMemoryExternal).Modified);
      aClient.Commit; // write to file
      // try to read directly from file content
      Rest := Client.Server.StaticVirtualTable[aClass];
      if CheckFailed(Rest<>nil) then
        exit;
      fn := TSQLRestStorageInMemoryExternal(Rest).FileName;
      if fn<>'' then begin
        // no file content if ':memory' DB
        TSQLRestStorageInMemoryExternal(Rest).UpdateFile; // force update (COMMIT not always calls xCommit)
        stor := TSQLRestStorageInMemoryExternal.Create(
          aClass,nil,fn,{bin=}aClass=TSQLRecordDali2);
        try
          Check(stor.Count=n);
          for i := 1 to n do begin
            ndx := stor.IDToIndex(i);
            if CheckFailed(ndx>=0) then
              continue;
            VD2 := stor.Items[ndx] as TSQLRecordDali1;
            if CheckFailed(VD2<>nil) then
              continue;
            Check(VD2.ID=i);
            Check(IdemPChar(pointer(VD2.FirstName),'SALVADOR'));
            Check(VD2.YearOfBirth=1904+i);
            Check(VD2.YearOfDeath=1989+i);
          end;
        finally
          stor.Free;
        end;
      end;
    except
      aClient.RollBack; // will run an error - but this code is correct
    end;
  finally
    VD.Free;
  end;
end;
function TestTable(T: TSQLTable): boolean;
var aR,aF: integer;
    db: TSQLTable;
begin
  result := false;
  if T=nil then
    exit;
  db := TSQLTableDB.Create(Demo,[],Req,true);
  try
    if (db.RowCount<>T.RowCount) or (db.FieldCount<>T.FieldCount) then begin
      Check(False);
      exit;
    end;
    for aR := 0 to db.RowCount do // compare all result values
      for aF := 0 to db.FieldCount-1 do
        if StrComp(pointer(db.Get(aR,aF)),pointer(T.Get(aR,aF)))<>0 then begin
         Check(False);
         exit;
       end;
    result := true;
  finally
    db.Free;                                  
    T.Free;
  end;
end;
{$ifdef MSWINDOWS}
procedure TestClientDist(ClientDist: TSQLRestClientURI);
var i: integer;
    ids: array[0..3] of TID;
    res: TIDDynArray;
begin
  try
    Check(ClientDist.SetUser('User','synopse'));
    TestFTS3(ClientDist);
    TestDynArray(ClientDist);
    {$ifndef LVCL}
    TestObject(ClientDist);
    {$endif}
    InternalTestMany(self,ClientDist);
    TestVirtual(ClientDist,false,'Remote Virtual Table access via SQLite',TSQLRecordDali1);
    TestVirtual(ClientDist,false,'Remote Virtual Table access via SQLite',TSQLRecordDali2);
    TestVirtual(ClientDist,true,'Remote Direct Virtual Table',TSQLRecordDali1);
    TestVirtual(ClientDist,true,'Remote Direct Virtual Table',TSQLRecordDali2);
    Check(TestTable(ClientDist.List([TSQLRecordPeople],'*',s)),'through URI and JSON');
    for i := 0 to high(IntArray) do begin
      Check(ClientDist.RetrieveBlob(TSQLRecordPeople,IntArray[i],'Data',Data));
      Check((length(Data)=4) and (PInteger(pointer(Data))^=IntArray[i]));
      V2.fID := IntArray[i]; // debug use - do NOT set ID in your programs!
      Check(V2.DataAsHex(ClientDist)=SynCommons.BinToHex(Data));
      a := Random;
      b := Random;
      CheckSame(TSQLRecordPeople.Sum(Client,a,b,false),a+b);
      CheckSame(TSQLRecordPeople.Sum(Client,a,b,true),a+b);
    end;
    V.FirstName := 'Leonardo1';
    Check(ClientDist.Update(V));
    checks(false,ClientDist,'check remote UPDATE/POST');
    V.FirstName := 'Leonard';
    Check(ClientDist.Update(V));
    checks(true,ClientDist,'check remote UPDATE/POST');
    for i := 0 to high(ids) do begin
      V2.YearOfBirth := i;
      ids[i] := ClientDist.Add(V2,true);
    end;
    for i := 0 to high(ids) do begin
      Check(ClientDist.Retrieve(ids[i],V2));
      Check(V2.YearOfBirth=i);
    end;
    for i := 0 to high(ids) do begin
      ClientDist.BatchStart(TSQLRecordPeople);
      ClientDist.BatchDelete(ids[i]);
      Check(ClientDist.BatchSend(res)=HTTP_SUCCESS);
      Check(length(res)=1);
      Check(res[0]=HTTP_SUCCESS);
    end;
    for i := 0 to high(ids) do
      Check(not ClientDist.Retrieve(ids[i],V2));
    V2.ClearProperties;
    for i := 0 to high(ids) do begin
      V2.fID := ids[i];
      Check(ClientDist.Update(V2),'test locking');
    end;
//  time := GetTickCount; while time=GetTickCount do; time := GetTickCount;
    for i := 1 to 400 do // speed test: named pipes are OK
      checks(true,ClientDist,'caching speed test');
//  writeln('NamedPipe connection time is ',GetTickCount-time,'ms');
  finally
    ClientDist.Free;
  end;
end;
{$endif}
procedure Direct(const URI: RawUTF8; Hash: cardinal; const head: RawUTF8='');
var call: TSQLRestURIParams;
begin
  FillCharFast(call,sizeof(call),0);
  call.Method :='GET';
  call.url := URI;
  call.InHead := head;
  TSQLRestServerAuthenticationDefault.ClientSessionSign(Client,call);
  call.RestAccessRights := @SUPERVISOR_ACCESS_RIGHTS;
  Server.URI(call);
  Check(Hash32(call.OutBody)=Hash);
end;
var ClientDist: TSQLRestClientURI;
    json: RawUTF8;
begin
  V := TSQLRecordPeople.Create;
  VA := TSQLRecordPeopleArray.Create;
  {$ifndef LVCL}
  VO := TSQLRecordPeopleObject.Create;
  {$endif}
  VP := TSQLRecordCustomProps.Create;
  V2 := nil;
  try
    if ClassType<>TTestMemoryBased then begin
      DeleteFile('dali1.json');
      DeleteFile('dali2.data');
    end;
    Demo.RegisterSQLFunction(TypeInfo(TIntegerDynArray),@SortDynArrayInteger,
      'MyIntegerDynArrayContains');
    ModelC := TSQLModel.Create(
      [TSQLRecordPeople, TSQLFTSTest,
       TSQLASource, TSQLADest, TSQLADests, TSQLRecordPeopleArray
       {$ifndef LVCL}, TSQLRecordPeopleObject{$endif},
       TSQLRecordDali1,TSQLRecordDali2, TSQLRecordCustomProps],'root');
    ModelC.VirtualTableRegister(TSQLRecordDali1,TSQLVirtualTableJSON);
    ModelC.VirtualTableRegister(TSQLRecordDali2,TSQLVirtualTableBinary);
    try
      Client := TSQLRestClientDB.Create(ModelC,nil,Demo,TSQLRestServerTest,true);
      try
        Client.Server.DB.Synchronous := smOff;
        Client.Server.DB.LockingMode := lmExclusive;
        with Client.Server.Model do
          for i := 0 to high(Tables) do
            if not CheckFailed(GetTableIndex(Tables[i])=i) then
              Check(GetTableIndex(Tables[i].SQLTableName)=i);
        // direct client access test
        Client.Server.CreateMissingTables; // NEED Dest,Source,Dests,...
        Check(Client.SetUser('User','synopse')); // use default user
        DaVinci := 'da Vin'+_uE7+'i';
        Check(Client.Retrieve('LastName='''+DaVinci+'''',V));
        Check(V.FirstName='Leonardo1');
        Check(V.LastName=DaVinci);
        Check(V.YearOfBirth=1452);
        Check(V.YearOfDeath=1519);
        checks(false,Client,'Retrieve');
        Check(V.ID=6,'check RETRIEVE/GET');
        Check(Client.Delete(TSQLRecordPeople,V.ID),'check DELETE');
        Check(not Client.Retrieve(V.ID,V),'now this record must not be available');
        Check(Client.Add(V,true)>0,'check ADD/PUT');
        checks(false,Client,'check created value is well retrieved');
        checks(false,Client,'check caching');
        V2 := V.CreateCopy as TSQLRecordPeople;
        Check(V2.SameValues(V));
        V2.Free;
        V2 := TSQLRecordPeople.Create(Client,V.ID);
        Check(V2.SameValues(V));
        Check(Client.Retrieve(V.ID,V2,true),'with LOCK');
        Check(V2.SameValues(V));
        V.FirstName := 'Leonard';
        Check(Client.Update(V));
        Check(Client.UnLock(V),'unlock');
        checks(true,Client,'check UPDATE/POST');
        if Client.SessionUser=nil then // only if has the right for EngineExecute
          Check(Client.Execute('VACUUM;'),'check direct Execute()') else
          Check(Client.Server.Execute('VACUUM;'));
        Check(V2.FirstName='Leonardo1');
        Check(not V2.SameValues(V),'V and V2 must differ');
        Check(Client.UpdateFromServer([V2],Refreshed));
        Check(Refreshed,'V2 value will be synchronized with V');
        Check(V2.SameValues(V));
        Check(Client.UpdateFromServer([V2],Refreshed));
        Check(not Refreshed);
        Req := StringReplace(Req,'*',
          Client.Model.Props[TSQLRecordPeople].SQL.TableSimpleFields[true,false],[]);
        s := 'LastName=''M'+_uF4+'net'' ORDER BY FirstName';
        J := Client.List([TSQLRecordPeople],'*',s);
        Check(Client.UpdateFromServer([J],Refreshed));
        Check(not Refreshed);
        Check(TestTable(J),'incorrect TSQLTableJSON');
        Check(Client.OneFieldValues(TSQLRecordPeople,'ID','LastName=:("Dali"):',IntArray));
        Check(length(IntArray)=1001);
        for i := 0 to high(IntArray) do
          Check(Client.OneFieldValue(TSQLRecordPeople,'LastName',IntArray[i])='Dali');
        List := Client.RetrieveList(TSQLRecordPeople,'Lastname=?',['Dali'],'ID,LastName');
        if not CheckFailed(List<>nil) then begin
          Check(List.Count=Length(IntArray));
          for i := 0 to List.Count-1 do
          with TSQLRecordPeople(List.List[i]) do begin
            Check(ID=IntArray[i]);
            Check(LastName='Dali');
            Check(FirstName='');
          end;
          List.Free;
        end;
        Client.Server.SessionsSaveToFile('sessions.data');
        Client.Server.SessionsLoadFromFile('sessions.data',false);
        Check(Client.TransactionBegin(TSQLRecordPeople)); // for UpdateBlob() below
        for i := 0 to high(IntArray) do begin
          Check(Client.RetrieveBlob(TSQLRecordPeople,IntArray[i],'Data',Data));
          Check(Length(Data)=sizeof(BlobDali));
          Check(CompareMem(pointer(Data),@BlobDali,sizeof(BlobDali)));
          Check(Client.RetrieveBlob(TSQLRecordPeople,IntArray[i],'Data',DataS));
          Check((DataS.Size=4) and (PCardinal(DataS.Memory)^=$E7E0E961));
          DataS.Free;
          Check(Client.UpdateBlob(TSQLRecordPeople,IntArray[i],'Data',@IntArray[i],4));
          Check(Client.RetrieveBlob(TSQLRecordPeople,IntArray[i],'Data',Data));
          Check((length(Data)=4) and (PInteger(pointer(Data))^=IntArray[i]));
          V2.fID := IntArray[i]; // debug use - do NOT set ID in your programs!
          Check(V2.DataAsHex(Client)=SynCommons.BinToHex(Data));
          a := Random;
          b := Random;
          Check(SameValue(TSQLRecordPeople.Sum(Client,a,b,false),a+b,1E-10));
          Check(SameValue(TSQLRecordPeople.Sum(Client,a,b,true),a+b,1E-10));
        end;
        Client.Commit;
        Check(Client.TransactionBegin(TSQLRecordPeopleArray));
        V2.FillPrepare(Client,'LastName=:("Dali"):');
        n := 0;
        while V2.FillOne do begin
          VA.FillFrom(V2); // fast copy some content from TSQLRecordPeople
          inc(n);
          if n and 31=0 then begin
            VA.UTF8 := '';
            VA.DynArray('Ints').Add(n);
            Curr := n*0.01;
            VA.DynArray(2).Add(Curr);
            FV.Major := n;
            FV.Minor := n+2000;
            FV.Release := n+3000;
            FV.Build := n+4000;
            str(n,FV.Main);
            str(n+1000,FV.Detailed);
            VA.DynArray('FileVersion').Add(FV);
          end else
            str(n,VA.fUTF8);
          {$ifdef PUBLISHRECORD}
          VA.fRec.nPhrase := n;
          VA.fRec.nCol := n*2;
          VA.fRec.hits[2].docs_with_hits := n*3;
          {$endif PUBLISHRECORD}
          Check(Client.Add(VA,true)=n);
        end;
        Client.Commit;
        {$ifndef LVCL}
        if Client.TransactionBegin(TSQLRecordPeopleObject) then
        try
          V2.FillPrepare(Client,'LastName=:("Morse"):');
          n := 0;
          while V2.FillOne do begin
            VO.FillFrom(V2); // fast copy some content from TSQLRecordPeople
            inc(n);
            VO.Persistent.One.Color := n+100;
            VO.Persistent.One.Length := n;
            VO.Persistent.One.Name := Int32ToUtf8(n);
            if n and 31=0 then begin
              VO.UTF8.Add(VO.Persistent.One.Name);
              with VO.Persistent.Coll.Add do begin
                Color := n+1000;
                Length := n*2;
                Name := Int32ToUtf8(n*3);
              end;
            end;
            Check(Client.Add(VO,true)=n);
          end;
          Client.Commit;
        except
          Client.RollBack;
        end;
        {$endif LVCL}
        TestFTS3(Client);
        TestDynArray(Client);
        {$ifndef LVCL}
        TestObject(Client);
        {$endif}
        InternalTestMany(self,Client);
        // RegisterVirtualTableModule(TSQLVirtualTableJSON) done above
        TestVirtual(Client,false,'Virtual Table access via SQLite 1',TSQLRecordDali1);
        TestVirtual(Client,false,'Virtual Table access via SQLite 1',TSQLRecordDali2);
        TestVirtual(Client,true,'Direct Virtual Table access 1',TSQLRecordDali1);
        TestVirtual(Client,true,'Direct Virtual Table access 2',TSQLRecordDali2);
        // remote client access test (via named pipes)
        {$ifdef MSWINDOWS}
        Check(Client.Server.ExportServerNamedPipe('Test'),'declare Test server');
        TestClientDist(TSQLRestClientURINamedPipe.Create(ModelC,'Test'));
        {$endif}
        // check custom properties content
        {$ifndef LVCL}
        if Client.TransactionBegin(TSQLRecordPeopleObject) then
        try
          V2.FillPrepare(Client,'LastName=:("Morse"):');
          n := 0;
          while V2.FillOne do begin
            VP.FillFrom(V2); // fast copy some content from TSQLRecordPeople
            inc(n);
            VP.fGUID.D1 := n;
            {$ifdef PUBLISHRECORD}
            VP.fGUIDXE6.D1 := n shl 1;
            {$endif}
            Check(Client.Add(VP,true)=n);
          end;
          Client.Commit;
          VP.FillPrepare(Client);
          while VP.FillOne do begin
            check(VP.LastName='Morse');
            check(Integer(VP.GUID.D1)=VP.ID);
            {$ifdef PUBLISHRECORD}
            check(Integer(VP.GUIDXE6.D1)=VP.ID shl 1);
            {$endif}
          end;
        except
          Client.RollBack;
        end;
        {$endif}
        // test backup API
        BackupFN := Format('backupbackground%s.dbsynlz',[ClassName]);
        deleteFile(BackupFN);
        BackupTimer.Start;
        Check(Client.DB.BackupBackground(BackupFN,1024,0,OnBackupProgress,true)); 
        // test per-one and batch requests
        if ClassType=TTestMemoryBased then begin // time consuming, so do it once
          Server := TSQLRestServerTest.Create(TSQLModel.Create([TSQLRecordPeople]),false);
          try
            Server.Model.Owner := Server; // we just use TSQLRecordPeople here
            Server.NoAJAXJSON := true;
            DeleteFile('People.json');
            DeleteFile('People.data');
            Server.StaticDataCreate(TSQLRecordPeople,'People.data',true);
            json := Demo.ExecuteJSON('SELECT * From People');
            aStatic := Server.StaticDataServer[TSQLRecordPeople] as TSQLRestStorageInMemory;
            Check(aStatic<>nil);
            aStatic.LoadFromJSON(json); // test Add() and JSON fast loading
            for i := 0 to aStatic.Count-1 do begin
              Check(Client.Retrieve(aStatic.ID[i],V),'test statement+bind speed');
              Check(V.SameRecord(aStatic.Items[i]),'static retrieve');
            end;
            // test our 'REST-minimal' SELECT statement SQL engine
            Direct('/root/People?select=%2A&where=id%3D012',$96F68454);
            Direct('/root/People?select=%2A&where=id%3D:(012):',$96F68454);
            Direct('/root/People?select=%2A&where=LastName%3D%22M%C3%B4net%22',$BBDCF3A6);
            Direct('/root/People?select=%2A&where=YearOfBirth%3D1873',$AF4BCA94);
            Direct('/root/People?select=%2A',$17AE45E3);
            Direct('/root/People?select=%2A&where=YearOfBirth%3D1873&startindex=10&results=20',$453C7201);
            Server.URIPagingParameters.SendTotalRowsCountFmt := ',"Total":%';
            Direct('/root/People?select=%2A&where=YearOfBirth%3D1873&startindex=10&results=2',$79AFDD53);
            Server.NoAJAXJSON := false;
            Direct('/root/People?select=%2A&where=YearOfBirth%3D1873&startindex=10&results=2',
              $69FDAF5D,'User-Agent: Ajax');
            Server.NoAJAXJSON := true;
            Server.URIPagingParameters.SendTotalRowsCountFmt := '';
            // test Retrieve() and Delete()
            Server.ExportServer; // initialize URIRequest() with the aStatic database
            USEFASTMM4ALLOC := true; // getmem() is 2x faster than GlobalAlloc()
            ClientDist := TSQLRestClientURIDll.Create(ModelC,URIRequest);
            try
              SetLength(IntArray,(aStatic.Count-1)shr 2);
              for i := 0 to high(IntArray) do begin
                IntArray[i] := aStatic.ID[i*4];
                Check(ClientDist.Retrieve(IntArray[i],V));
                Check(V.SameRecord(aStatic.Items[i*4]));
              end;
              Check(V.FillPrepare(Client,IntArray));
              for i := 0 to High(IntArray) do begin
                Check(V.FillOne);
                Check(V.ID=IntArray[i]);
                Check(V.SameRecord(aStatic.Items[i*4]));
              end;
              V.FillClose; // so that BatchUpdate(V) below will set all fields
              if ClientDist.TransactionBegin(TSQLRecordPeople) then
              try
                for i := 0 to high(IntArray) do
                  Check(ClientDist.Delete(TSQLRecordPeople,IntArray[i]));
                for i := 0 to high(IntArray) do
                  Check(not ClientDist.Retrieve(IntArray[i],V));
                for i := 0 to aStatic.Count-1 do begin
                  Check(ClientDist.Retrieve(aStatic.ID[i],V));
                  V.YearOfBirth := Random(MaxInt)-Random(MaxInt);
                  Check(ClientDist.Update(V));
                  Check(ClientDist.Retrieve(aStatic.ID[i],V));
                  Check(V.SameRecord(aStatic.Items[i]));
                end;
                ClientDist.Commit;
              except
                ClientDist.RollBack;
              end else
                Check(False,'TransactionBegin');
              // test BATCH sequence usage
              if ClientDist.TransactionBegin(TSQLRecordPeople) then
              try
                Check(ClientDist.BatchStart(TSQLRecordPeople,5000));
                n := 0;
                for i := 0 to aStatic.Count-1 do
                  if i and 7=0 then begin
                    IntArray[n] := aStatic.ID[i];
                    inc(n);
                  end;
                for i := 0 to n-1 do
                  // note that here a warning does make sense, since Server.DB=nil
                  Check(ClientDist.BatchDelete(IntArray[i])=i);
                nupd := 0;
                for i := 0 to aStatic.Count-1 do
                  if i and 7<>0 then begin // not yet deleted in BATCH mode
                     Check(ClientDist.Retrieve(aStatic.ID[i],V));
                     V.YearOfBirth := 1800+nupd;
                     Check(ClientDist.BatchUpdate(V)=nupd+n);
                     inc(nupd);
                   end;
                V.LastName := 'New';
                for i := 0 to 1000 do begin
                  V.FirstName := RandomUTF8(10);
                  V.YearOfBirth := i+1000;
                  Check(ClientDist.BatchAdd(V,true)=n+nupd+i);
                end;
                Check(ClientDist.BatchSend(Results)=200);
                Check(Length(Results)=9260);
                ClientDist.Commit;
                for i := 0 to n-1 do
                  Check(not ClientDist.Retrieve(IntArray[i],V),'BatchDelete');
                for i := 0 to high(Results) do
                  if i<nupd+n then
                    Check(Results[i]=200) else begin
                    Check(Results[i]>0);
                    ndx := aStatic.IDToIndex(Results[i]);
                    Check(ndx>=0);
                    with TSQLRecordPeople(aStatic.Items[ndx]) do begin
                      Check(LastName='New','BatchAdd');
                      Check(YearOfBirth=1000+i-nupd-n);
                    end;
                  end;
                for i := 0 to aStatic.Count-1 do
                  with TSQLRecordPeople(aStatic.Items[i]) do
                    if LastName='New' then
                      break else
                      Check(YearOfBirth=1800+i,'BatchUpdate');
              except
                ClientDist.RollBack;
              end else
                Check(False,'TransactionBegin');
              // test BATCH update from partial FillPrepare
              V.FillPrepare(ClientDist,'LastName=?',['New'],'ID,YearOfBirth');
              if ClientDist.TransactionBegin(TSQLRecordPeople) then
              try
                Check(ClientDist.BatchStart(TSQLRecordPeople));
                n := 0;
                V.LastName := 'NotTransmitted';
                while V.FillOne do begin
                  Check(V.LastName='NotTransmitted');
                  Check(V.YearOfBirth=n+1000);
                  V.YearOfBirth := n;
                  if n and 3=0 then
                    // will update only V.YearOfBirth specifically
                    ClientDist.BatchUpdate(V,
                      TSQLRecordPeople.RecordProps.FieldBitsFromCSV('YearOfBirth')) else
                    // will update only V.YearOfBirth as in previous FillPrepare
                    ClientDist.BatchUpdate(V);
                  inc(n);
                end;
                Check(n=1001);
                SetLength(Results,0);
                Check(ClientDist.BatchSend(Results)=200);
                Check(length(Results)=1001);
                for i := 0 to high(Results) do
                  Check(Results[i]=200);
                ClientDist.Commit;
              except
                ClientDist.RollBack;
              end else
                Check(False,'TransactionBegin');
              V.FillPrepare(ClientDist,'LastName=?',['New'],'YearOfBirth');
              n := 0;
              while V.FillOne do begin
                Check(V.LastName='NotTransmitted');
                Check(V.YearOfBirth=n);
                V.YearOfBirth := 1000;
                inc(n);
              end;
              Check(n=length(Results));
              V.FillClose;
              V.LastName := 'last';
              V.FirstName := 'first';
              V.fID := 4294967297;
              Check(ClientDist.Add(V,true,True)=V.ID);
              V.ClearProperties;
              ClientDist.Retrieve(4294967297,V);
              Check(V.FirstName='first');
              Check(V.ID=4294967297);
            finally
              ClientDist.Free;
            end;
            aStatic.UpdateFile; // force People.data file content write
            aStatic.ReloadFromFile;
            Check(aStatic.Retrieve(11,V),'reload from people.data');
            Check(V.FirstName='Jane1');
            Check(aStatic.Retrieve(4294967297,V));
            Check(V.FirstName='first');
            aStatic.FileName := 'People.json';
            aStatic.BinaryFile := false;
            aStatic.Modified := true;
            aStatic.UpdateFile; // force People.json file content write
            aStatic.ReloadFromFile;
            Check(aStatic.Retrieve(11,V),'reload from people.json');
            Check(V.FirstName='Jane1');
            Check(aStatic.Retrieve(4294967297,V));
            Check(V.FirstName='first');
            aStatic.Delete(TSQLRecordPeople,4294967297);
            aStatic.UpdateFile;
          finally
            {$ifdef MSWINDOWS}
            USEFASTMM4ALLOC := false;
            {$endif}
            Server.Free;
          end;
        end;
        Client.DB.BackupBackgroundWaitUntilFinished;
      finally
        Client.Free;
      end;
    finally
      ModelC.Free;
    end;
  finally
    V.Free;
    V2.Free;
    VA.Free;
    VP.Free;
    {$ifndef LVCL}
    VO.Free;
    {$endif}
    FreeAndNil(Demo);
  end;
  {$ifndef NOSQLITE3ENCRYPT}
  if EncryptedFile then begin
    check(ChangeSQLEncryptTablePassWord(TempFileName,'NewPass','')); // uncrypt file
    Check(IsSQLite3File(TempFileName));
  end;
  {$endif}
end;

procedure TTestSQLite3Engine._TSQLTableJSON;
var J: TSQLTableJSON;
    i1, i2, aR, aF, F1,F2, n: integer;
    Comp, Comp1,Comp2: TUTF8Compare;
    {$ifdef UNICODE}
    Peoples: TObjectList<TSQLRecordPeople>;
    {$endif}
    {$ifndef LVCL}
    row: variant;
    {$endif}
    {$ifndef NOVARIANTS}
    lContactDataQueueDynArray: TDynArray;
    lContactDataQueueArray: TRawUTF8DynArray;
    lContactDataQueueJSON: TDocVariantData;
    lData, s: RawUTF8;
    lDocData: TDocVariantData;
const
  TEST_DATA = '['+
  '{"REC_ID":29915,"CHANNEL":117,"PHONE":"5004392222,12345678","RINGS":0,' +
    '"QUEUE_CALL":2,"PRIORITY":25,"TIMESTAMP_CALL":"2017-10-26T04:48:14",' +
    '"RETRIES_CALL":2,"CONNECTION_TYPE":0,"DISCONNECTION_TYPE":0,"STATUS_CALL":9,'+
    '"GC_STATUS_CALL":5404,"START_COMMUNICATION":"","HELLO":0,"EXTENSION":null,' +
    '"NODE":1,"RESULT_CALL":0,"CONNECT_TIME":0,"SKILL":null,"AGENT_POSITION":0,' +
    '"COMM_RESULT_CODE":null,"V01_TM":"Marcie","V02_TM":"Sayton",'+
    '"V03_TM":"msaytonpe@umn.edu"},'+
	'{"REC_ID":29916,"CHANNEL":132,"PHONE":"1763252375","RINGS":0,"QUEUE_CALL":2,' +
    '"PRIORITY":25,"TIMESTAMP_CALL":"2017-10-26T04:48:14","RETRIES_CALL":2,' +
    '"CONNECTION_TYPE":0,"DISCONNECTION_TYPE":0,"STATUS_CALL":9,'+
    '"GC_STATUS_CALL":5404,"START_COMMUNICATION":"","HELLO":0,"EXTENSION":null,' +
    '"NODE":1,"RESULT_CALL":0,"CONNECT_TIME":0,"SKILL":null,"AGENT_POSITION":0,' +
    '"COMM_RESULT_CODE":null,"V01_TM":"Orsola","V02_TM":"Hainge",'+
    '"V03_TM":"ohaingepf@reverbnation.com"},'+
	'{"REC_ID":29917,"CHANNEL":174,"PHONE":"9149556917","RINGS":0,"QUEUE_CALL":2,' +
    '"PRIORITY":25,"TIMESTAMP_CALL":"2017-10-26T04:48:14","RETRIES_CALL":2,' +
    '"CONNECTION_TYPE":0,"DISCONNECTION_TYPE":0,"STATUS_CALL":9,'+
    '"GC_STATUS_CALL":5404,"START_COMMUNICATION":"","HELLO":0,"EXTENSION":null,' +
    '"NODE":1,"RESULT_CALL":0,"CONNECT_TIME":0,"SKILL":null,"AGENT_POSITION":0,' +
    '"COMM_RESULT_CODE":null,"V01_TM":"Storm","V02_TM":"Jenton",'+
    '"V03_TM":"sjentonpg@senate.gov"}]';
    {$endif}
begin
  J := TSQLTableJSON.Create('',JS);
  try
    J.SetFieldType('YearOfBirth',sftModTime);
    if JS<>'' then // avoid memory leak
      with TSQLTableDB.Create(Demo,[],Req,true) do
      try
        Check(RowCount=J.RowCount);
        Check(FieldCount=J.FieldCount);
        SetFieldType('YearOfBirth',sftModTime);
        for aR := 0 to RowCount do
          for aF := 0 to FieldCount-1 do
           if (aR>0) and (aF=3) then  // aF=3=Blob
             Check(GetBlob(aR,aF)=J.GetBlob(aR,aF)) else begin
             Check((GetW(aR,aF)=J.GetW(aR,aF)) and
                  (GetA(aR,aF)=J.GetA(aR,aF)) and
                  (length(GetW(aR,aF))shr 1=LengthW(aR,aF)),
                  Format('Get() in Row=%d Field=%d',[aR,aF]));
              if (aR>0) and (aF>3) then begin
                Check(GetDateTime(aR,af)=J.GetDateTime(aR,aF));
                Check(GetAsDateTime(aR,af)=J.GetAsDateTime(aR,aF));
              end;
            end;
      finally
        Free;
      end;
    Demo.Execute('VACUUM;');
    with TSQLTableDB.Create(Demo,[],Req,true) do // re-test after VACCUM
    try
      Check(RowCount=J.RowCount);
      Check(FieldCount=J.FieldCount);
      Check(FieldIndex('ID')=0);
      Check(FieldIndex('RowID')=0);
      for aF := 0 to FieldCount-1 do
        Check(FieldIndex(J.Get(0,aF))=aF);
      for aR := 0 to RowCount do
        for aF := 0 to FieldCount-1 do // aF=3=Blob
          Check((aF=3) or (StrIComp(Get(aR,aF),J.Get(aR,aF))=0));
      n := 0;
      while Step do begin
        for aF := 0 to FieldCount-1 do // aF=3=Blob
          Check((aF=3) or (StrIComp(FieldBuffer(aF),J.Get(StepRow,aF))=0));
        inc(n);
      end;
      check(n=J.RowCount);
      {$ifndef LVCL}
      n := 0;
      if not CheckFailed(Step(true,@row)) then
        repeat
          Check(row.ID=J.GetAsInteger(StepRow,FieldIndex('ID')));
          Check(row.FirstName=J.GetU(StepRow,FieldIndex('FirstName')));
          Check(row.LastName=J.GetU(StepRow,FieldIndex('LastName')));
          Check(row.YearOfBirth=J.GetAsInteger(StepRow,FieldIndex('YearOfBirth')));
          Check(row.YearOfDeath=J.GetAsInteger(StepRow,FieldIndex('YearOfDeath')));
          inc(n);
       until not Step(false,@row);
      check(n=J.RowCount);
      {$endif}
      with ToObjectList(TSQLRecordPeople) do
      try
        check(Count=J.RowCount);
        for aR := 1 to Count do
        with TSQLRecordPeople(Items[aR-1])  do begin
          Check(fID=J.GetAsInteger(aR,FieldIndex('ID')));
          Check(FirstName=J.GetU(aR,FieldIndex('FirstName')));
          Check(LastName=J.GetU(aR,FieldIndex('LastName')));
          Check(YearOfBirth=J.GetAsInteger(aR,FieldIndex('YearOfBirth')));
          Check(YearOfDeath=J.GetAsInteger(aR,FieldIndex('YearOfDeath')));
        end;
      finally
        Free;
      end;
      {$ifdef UNICODE}
      Peoples := ToObjectList<TSQLRecordPeople>;
      try
        Check(Peoples.Count=J.RowCount);
        for aR := 1 to Peoples.Count do
        with Peoples[aR-1] do begin
          Check(ID=J.GetAsInteger(aR,FieldIndex('ID')));
          Check(FirstName=J.GetU(aR,FieldIndex('FirstName')));
          Check(LastName=J.GetU(aR,FieldIndex('LastName')));
          Check(YearOfBirth=J.GetAsInteger(aR,FieldIndex('YearOfBirth')));
          Check(YearOfDeath=J.GetAsInteger(aR,FieldIndex('YearOfDeath')));
        end;
      finally
        Peoples.Free;
      end;
      {$endif}
    finally
      Free;
    end;
    for aF := 0 to J.FieldCount-1 do begin
      J.SortFields(aF);
      Comp := J.SortCompare(aF);
      if @Comp<>nil then // BLOB field will be ignored
        for aR := 1 to J.RowCount-1 do // ensure data sorted in increasing order
          Check(Comp(pointer(J.Get(aR,aF)),pointer(J.Get(aR+1,aF)))<=0,'SortCompare');
    end;
    for aF := 0 to J.FieldCount-1 do begin
      J.SortFields(aF,false);
      Comp := J.SortCompare(aF);
      if @Comp<>nil then // BLOB field will be ignored
        for aR := 1 to J.RowCount-1 do // ensure data sorted in decreasing order
          Check(Comp(pointer(J.Get(aR,aF)),pointer(J.Get(aR+1,aF)))>=0,'SortCompare');
    end;
    for F1 := 0 to J.FieldCount-1 do
    for F2 := 0 to J.FieldCount-1 do
      if F1<>F2 then begin
        Comp1 := J.SortCompare(F1);
        Comp2 := J.SortCompare(F2);
        if (@Comp1=nil) or (@Comp2=nil) then
          continue; // BLOB fields will be ignored
        J.SortFields([F1,F2],[],[]);
        for aR := 1 to J.RowCount-1 do begin
          // ensure data sorted in increasing order for both fields
          aF := Comp1(pointer(J.Get(aR,F1)),pointer(J.Get(aR+1,F1)));
          Check(aF<=0,'SortCompare');
          if aF=0 then // 1st field idem -> check sorted by 2nd field
            Check(Comp2(pointer(J.Get(aR,F2)),pointer(J.Get(aR+1,F2)))<=0);
        end;
      end;
    for F1 := 0 to J.FieldCount-1 do
    for F2 := 0 to J.FieldCount-1 do
      if F1<>F2 then begin
        Comp1 := J.SortCompare(F1);
        Comp2 := J.SortCompare(F2);
        if (@Comp1=nil) or (@Comp2=nil) then
          continue; // BLOB fields will be ignored
        J.SortFields([F1,F2],[false,true],[]); // 1st=DESC, 2nd=ASC order
        for aR := 1 to J.RowCount-1 do begin
          // ensure data sorted in expected order for both fields
          aF := Comp1(pointer(J.Get(aR,F1)),pointer(J.Get(aR+1,F1)));
          Check(aF>=0,'SortCompare');
          if aF=0 then // 1st field idem -> check ASC sorted by 2nd field
            Check(Comp2(pointer(J.Get(aR,F2)),pointer(J.Get(aR+1,F2)))<=0);
        end;
      end;
  finally
    J.Free;
  end;
  if false then
    with TSQLTableDB.Create(Demo,[TSQLRecordPeople],
      'select id,FirstName,LastName,YearOfBirth,YearOfDeath from people',true) do
    try
      FileFromString(GetODSDocument(false),'false.ods');
      FileFromString(GetODSDocument(true),'true.ods');
    finally
      Free;
    end;
  // some tests to avoid regression about bugs reported by users on forum
  {$ifndef NOVARIANTS}
  J := TSQLTableJSON.Create('',TEST_DATA);
  try
    check(J.fieldCount=24);
    check(J.rowCount=3);
    lData := j.GetJSONValues(true);
    check(lData[1]='[');
    check(JSONArrayCount(@lData[2])=J.rowCount);
    check(Hash32(lData)=$B1C13092);
    lData := j.GetJSONValues(false);
    check(Hash32(lData)=$6AB30A2);
  finally
    J.Free;
  end;
  lContactDataQueueJSON.InitJSON(TEST_DATA);
  lContactDataQueueDynArray.Init(TypeInfo(TRawUTF8DynArray), lContactDataQueueArray);
  lContactDataQueueJSON.ToRawUTF8DynArray(lContactDataQueueArray);
  lData := lContactDataQueueDynArray.SaveToJSON;
  lDocData.InitJSON(lData, [dvoJSONObjectParseWithinString]);
  check(lDocData.Count=3);
  check(Hash32(lDocData.ToJSON)=$FCF948A5);
  check(lDocData.Value[0].QUEUE_CALL=2);
  s := TEST_DATA;
  i1 := PosEx(',"CHANNEL":132',s);
  i2 := PosEx('}',s,i1);
  delete(s,i1,i2-i1); // truncate the 2nd object
  J := TSQLTableJSON.Create('',s);
  try
    check(J.fieldCount=24);
    if not checkfailed(J.rowCount=3) then
      check(J.Get(2,J.FieldCount-1)=nil);
    check(J.Get(J.rowCount,J.FieldCount-1)='sjentonpg@senate.gov');
  finally
    J.Free;
  end;
  {$endif NOVARIANTS}
end;

{$ifdef UNICODE}
{$WARNINGS ON} // don't care about implicit string cast in tests
{$endif}


{ TSQLRestServerTest }

procedure TSQLRestServerTest.DataAsHex(Ctxt: TSQLRestServerURIContext);
var aData: TSQLRawBlob;
begin
  if (self=nil) or (Ctxt.Table<>TSQLRecordPeople) or (Ctxt.TableID<0) then
    Ctxt.Error('Need a valid record and its ID') else
  if RetrieveBlob(TSQLRecordPeople,Ctxt.TableID,'Data',aData) then
    Ctxt.Results([SynCommons.BinToHex(aData)]) else
    Ctxt.Error('Impossible to retrieve the Data BLOB field');
end;

procedure TSQLRestServerTest.Sum(Ctxt: TSQLRestServerURIContext);
var a,b: double;
begin
  if UrlDecodeNeedParameters(Ctxt.Parameters,'A,B') then begin
    while Ctxt.Parameters<>nil do begin
      UrlDecodeDouble(Ctxt.Parameters,'A=',a);
      UrlDecodeDouble(Ctxt.Parameters,'B=',b,@Ctxt.Parameters);
    end;
    Ctxt.Results([a+b]);
  end else
    Ctxt.Error('Missing Parameter');
end;

procedure TSQLRestServerTest.Sum2(Ctxt: TSQLRestServerURIContext);
begin
  with Ctxt do
    Results([InputDouble['a']+InputDouble['b']]);
end;

var
  GlobalInterfaceTestMode: (
    itmDirect, itmClient,
    itmLocked, itmMainThread, itmPerInterfaceThread, itmHttp) = itmDirect;

{$ifndef LVCL}

{ TSQLRecordPeopleObject }

constructor TSQLRecordPeopleObject.Create;
begin
  inherited;
  fPersistent := TCollTst.Create;
  fUTF8 := TRawUTF8List.Create;
end;

destructor TSQLRecordPeopleObject.Destroy;
begin
  Persistent.Free;
  UTF8.Free;
  inherited;
end;


{ TCollTestsI }

class function TCollTestsI.GetClass: TCollectionItemClass;
begin
  result := TCollTest;
end;

{$endif LVCL}


{ TComplexNumber }

constructor TComplexNumber.Create(aReal, aImaginary: double);
begin
  Real := aReal;
  Imaginary := aImaginary;
end;



{ TServiceCalculator }

type
  TServiceCalculator = class(TInjectableObject, ICalculator)
  public
    function Add(n1,n2: integer): integer;
    function Subtract(n1,n2: double): double;
    procedure Swap(var n1,n2: double);
    function Multiply(n1,n2: Int64): Int64;
    procedure ToText(Value: Currency; var Result: RawUTF8);
    function ToTextFunc(Value: double): string;
    function StackIntMultiply(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10: integer): Int64;
    function StackFloatMultiply(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10: double): Int64;
    function SpecialCall(Txt: RawUTF8; var Int: integer; var Card: cardinal; field: TSynTableFieldTypes;
      fields: TSynTableFieldTypes; var options: TSynTableFieldOptions): TSynTableFieldTypes;
    function ComplexCall(const Ints: TIntegerDynArray; const Strs1: TRawUTF8DynArray;
      var Str2: TWideStringDynArray; const Rec1: TVirtualTableModuleProperties;
      var Rec2: TSQLRestCacheEntryValue; Float1: double; var Float2: double): TSQLRestCacheEntryValue;
    function DirectCall(const Data: TSQLRawBlob): integer;
    function RepeatJsonArray(const item: RawUTF8; count: integer): RawJSON;  
    function RepeatTextArray(const item: RawUTF8; count: integer): RawUTF8;
    function Test(A,B: Integer): RawUTF8;
  end;

  TServiceComplexCalculator = class(TServiceCalculator,IComplexCalculator)
  protected
    procedure EnsureInExpectedThread;
  public
    procedure Substract(n1,n2: TComplexNumber; out Result: TComplexNumber);
    function IsNull(n: TComplexNumber): boolean;
    function TestBlob(n: TComplexNumber): TServiceCustomAnswer;
    {$ifndef NOVARIANTS}
    function TestVariants(const Text: RawUTF8; V1: Variant; var V2: variant): variant;
    {$endif}
    {$ifndef LVCL}
    procedure Collections(Item: TCollTest; var List: TCollTestsI; out Copy: TCollTestsI);
    destructor Destroy; override;
    {$endif LVCL}
    function GetCurrentThreadID: PtrUInt;
    function EchoRecord(const Nav: TConsultaNav): TConsultaNav;
    function GetCustomer(CustomerId: Integer; out CustomerData: TCustomerData): Boolean;
    procedure FillPeople(var People: TSQLRecordPeople);
  end;

  TServiceComplexNumber = class(TInterfacedObject,IComplexNumber)
  private
    fReal: double;
    fImaginary: double;
    function GetImaginary: double;
    function GetReal: double;
    procedure SetImaginary(const Value: double);
    procedure SetReal(const Value: double);
  public
    procedure Assign(aReal, aImaginary: double);
    procedure Add(aReal, aImaginary: double);
    property Real: double read GetReal write SetReal;
    property Imaginary: double read GetImaginary write SetImaginary;
  end;

  TServiceUserGroupSession = class(TInterfacedObject,ITestUser,ITestGroup,ITestSession)
  public
    function GetContextSessionID: integer;
    function GetContextSessionUser: integer;
    function GetContextSessionGroup: integer;
  end;

  TServicePerThread = class(TInterfacedObjectWithCustomCreate,ITestPerThread)
  protected
    fThreadIDAtCreation: PtrUInt; // TThreadID  = ^TThreadRec under BSD
  public
    constructor Create; override;
    function GetContextServiceInstanceID: PtrUInt;
    function GetThreadIDAtCreation: PtrUInt;
    function GetCurrentThreadID: PtrUInt;
    function GetCurrentRunningThreadID: PtrUInt;
  end;


function TServiceCalculator.Add(n1, n2: integer): integer;
begin
  result := n1+n2;
end;

function TServiceCalculator.Multiply(n1, n2: Int64): Int64;
begin
  result := n1*n2;
end;

function TServiceCalculator.StackIntMultiply(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10: integer): Int64;
begin
  result := n1*n2*n3*n4*n5*n6*n7*n8*n9*n10;
end;

function TServiceCalculator.StackFloatMultiply(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10: double): Int64;
begin
  result := round(n1*n2*n3*n4*n5*n6*n7*n8*n9*n10);
end;

function TServiceCalculator.SpecialCall(Txt: RawUTF8; var Int: integer;
  var Card: cardinal; field, fields: TSynTableFieldTypes;
  var options: TSynTableFieldOptions): TSynTableFieldTypes;
var dummy: IComplexNumber;
begin
  TryResolve(TypeInfo(IComplexNumber),dummy);
  inc(Int,length(Txt));
  inc(Card);
  result := fields+field;
  Include(options,tfoUnique);
  Exclude(options,tfoIndex);
end;

function TServiceCalculator.Subtract(n1, n2: double): double;
begin
  result := n1-n2;
end;

procedure TServiceCalculator.Swap(var n1,n2: double);
var tmp: double;
begin
  tmp := n2;
  n2 := n1;
  n1 := tmp;
end;

function TServiceCalculator.Test(A, B: Integer): RawUTF8;
begin
  result := Int32ToUtf8(A+B);
end;

procedure TServiceCalculator.ToText(Value: Currency; var Result: RawUTF8);
begin
  result := Curr64ToStr(PInt64(@Value)^);
end;

function TServiceCalculator.ToTextFunc(Value: double): string;
begin
  result := DoubleToString(Value);
end;

function TServiceCalculator.ComplexCall(const Ints: TIntegerDynArray;
  const Strs1: TRawUTF8DynArray; var Str2: TWideStringDynArray; const Rec1: TVirtualTableModuleProperties;
  var Rec2: TSQLRestCacheEntryValue; Float1: double; var Float2: double): TSQLRestCacheEntryValue;
var i: integer;
begin
  result := Rec2;
  result.JSON := StringToUTF8(Rec1.FileExtension);
  i := length(Str2);
  SetLength(Str2,i+1);
  Str2[i] := UTF8ToWideString(RawUTF8ArrayToCSV(Strs1));
  inc(Rec2.ID);
  dec(Rec2.Timestamp512);
  Rec2.JSON := IntegerDynArrayToCSV(pointer(Ints),length(Ints));
  Float2 := Float1;
end;

function TServiceCalculator.DirectCall(const Data: TSQLRawBlob): integer;
var i: integer;
begin
  result := length(Data);
  for i := 1 to result do
    if Data[i]<>#1 then
      result := 0;
end;

function TServiceCalculator.RepeatJsonArray(const item: RawUTF8; count: integer): RawJSON;
var buf: array[word] of byte;
begin
  with TTextWriter.CreateOwnedStream(@buf, SizeOf(buf)) do
    try
      Add('[');
      while count > 0 do begin
        Add('"');
        AddJSONEscape(pointer(item));
        Add('"',',');
        dec(count);
      end;
      CancelLastComma;
      Add(']');
      SetText(RawUTF8(result));
    finally
      Free;
    end;
end;

function TServiceCalculator.RepeatTextArray(const item: RawUTF8; count: integer): RawUTF8;
var buf: array[word] of byte;
begin
  with TTextWriter.CreateOwnedStream(@buf, SizeOf(buf)) do
    try
      while count > 0 do begin
        AddJSONEscape(pointer(item));
        dec(count);
      end;
      SetText(result);
    finally
      Free;
    end;
end;


{ TServiceComplexCalculator }

function TServiceComplexCalculator.IsNull(n: TComplexNumber): boolean;
begin
  result := (n.Real=0) and (n.Imaginary=0);
end;

procedure TServiceComplexCalculator.Substract(n1, n2: TComplexNumber; out Result: TComplexNumber);
begin
  result.Real := n1.Real-n2.Real;
  result.Imaginary := n1.Imaginary-n2.Imaginary;
end;

function TServiceComplexCalculator.EchoRecord(const Nav: TConsultaNav): TConsultaNav;
begin
  result := Nav;
end;

function GetThreadID: PtrUInt;
begin // avoid name conflict with TServiceComplexCalculator.GetCurrentThreadID
  result := PtrUInt(GetCurrentThreadId);
end;

procedure TServiceComplexCalculator.EnsureInExpectedThread;
begin
  case GlobalInterfaceTestMode of
  itmDirect, itmClient, itmMainThread:
    if GetThreadID<>PtrUInt(MainThreadID) then
      raise Exception.Create('Shall be in main thread');
  itmPerInterfaceThread, itmHttp, itmLocked:
    if GetThreadID=PtrUInt(MainThreadID) then
      raise Exception.Create('Shall NOT be in main thread') else
    if ServiceContext.RunningThread=nil then
      raise Exception.Create('Shall have a known RunningThread');
  end;
end;

function TServiceComplexCalculator.TestBlob(n: TComplexNumber): TServiceCustomAnswer;
begin
  EnsureInExpectedThread;
  Result.Header := TEXT_CONTENT_TYPE_HEADER;
  if n.Real = maxInt then
    Result.Content := StringOfChar(AnsiChar('-'), 600) else
    Result.Content := FormatUTF8('%,%',[n.Real,n.Imaginary]);
end;

{$ifndef NOVARIANTS}
function TServiceComplexCalculator.TestVariants(const Text: RawUTF8; V1: Variant; var V2: variant): variant;
begin
  V2 := V2+V1;
  VariantLoadJSON(Result,Text);
end;
{$endif}

function TServiceComplexCalculator.GetCurrentThreadID: PtrUInt;
begin
  result := GetThreadID;
end;

function TServiceComplexCalculator.GetCustomer(CustomerId: Integer;
  out CustomerData: TCustomerData): Boolean;
begin
  CustomerData.Id := CustomerId;
  CustomerData.AccountNum := Int32ToUtf8(CustomerID);
  result := True;
end;

procedure TServiceComplexCalculator.FillPeople(var People: TSQLRecordPeople);
begin
  People.LastName := FormatUTF8('Last %',[People.ID]);
  People.FirstName := FormatUTF8('First %',[People.ID]);
end;

{$ifndef LVCL}
procedure TServiceComplexCalculator.Collections(Item: TCollTest;
  var List: TCollTestsI; out Copy: TCollTestsI);
begin
  CopyObject(Item,List.Add);
  CopyObject(List,Copy);
end;

destructor TServiceComplexCalculator.Destroy;
begin
  EnsureInExpectedThread;
  inherited;
end;
{$endif LVCL}


{ TServiceComplexNumber }

procedure TServiceComplexNumber.Add(aReal, aImaginary: double);
begin
  fReal := fReal+aReal;
  fImaginary := fImaginary+aImaginary;
end;

procedure TServiceComplexNumber.Assign(aReal, aImaginary: double);
begin
  fReal := aReal;
  fImaginary := aImaginary;
end;

function TServiceComplexNumber.GetImaginary: double;
begin
  result := fImaginary;
end;

function TServiceComplexNumber.GetReal: double;
begin
  result := fReal;
end;

procedure TServiceComplexNumber.SetImaginary(const Value: double);
begin
  fImaginary := Value;
end;

procedure TServiceComplexNumber.SetReal(const Value: double);
begin
  fReal := Value;
end;


{ TServiceUserGroupSession }

function TServiceUserGroupSession.GetContextSessionGroup: integer;
begin
  with PServiceRunningContext(@ServiceContext)^ do
    if Request=nil then
      result := 0 else
      result := Request.SessionGroup;
end;

function TServiceUserGroupSession.GetContextSessionID: integer;
begin
  with PServiceRunningContext(@ServiceContext)^ do
    if Request=nil then
      result := 0 else
      result := Request.Session;
end;

function TServiceUserGroupSession.GetContextSessionUser: integer;
begin
  with PServiceRunningContext(@ServiceContext)^ do
    if Request=nil then
      result := 0 else
      result := Request.SessionUser;
end;


{ TServicePerThread }

constructor TServicePerThread.Create;
begin
  inherited;
  fThreadIDAtCreation := PtrUInt(GetThreadID);
end;

function TServicePerThread.GetCurrentThreadID: PtrUInt;
begin
  result := PtrUInt(GetThreadID);
  with PServiceRunningContext(@ServiceContext)^ do
    if Request<>nil then
      if PtrUInt(Result)<>Request.ServiceInstanceID then
        raise Exception.Create('Unexpected ServiceInstanceID');
end;

function TServicePerThread.GetThreadIDAtCreation: PtrUInt;
begin
  result := fThreadIDAtCreation;
end;

function TServicePerThread.GetContextServiceInstanceID: PtrUInt;
begin
  with PServiceRunningContext(@ServiceContext)^ do
    if Request=nil then
      result := 0 else begin
      result := Request.ServiceInstanceID;
      if result<>PtrUInt(GetThreadID) then
        raise Exception.Create('Unexpected ThreadID');
    end;
end;

function TServicePerThread.GetCurrentRunningThreadID: PtrUInt;
var Thread: TThread;
begin
  Thread := ServiceContext.RunningThread;
  if (Thread=nil) and (GlobalInterfaceTestMode=itmHttp) then
    raise Exception.Create('Unexpected Thread=nil');
  if Thread=nil then
    result := 0 else begin
    result := PtrUInt(Thread.ThreadID);
    if result<>PtrUInt(GetThreadID) then
      raise Exception.Create('Unexpected ThreadID');
  end;
end;


{ TTestServiceOrientedArchitecture }

procedure TTestServiceOrientedArchitecture.Test(const Inst: TTestServiceInstances;
  Iterations: Cardinal=700);
procedure TestCalculator(const I: ICalculator);
var
    {$ifdef CPU64}
    i1,i2: int64;
    {$else}
    i1,i2: integer;
    {$endif}
    t,i3: integer;
    c: cardinal;
    cu: currency;
    n1,n2,s1,s2: double;
    o: TSynTableFieldOptions;
    Ints: TIntegerDynArray;
    Strs1: TRawUTF8DynArray;
    Str2: TWideStringDynArray;
    Rec1: TVirtualTableModuleProperties;
    Rec2, RecRes: TSQLRestCacheEntryValue;
    s: RawUTF8;
    r: string;
begin
  Setlength(Ints,2);
  CSVToRawUTF8DynArray('one,two,three',Strs1);
  for t := 1 to Iterations do begin
    i1 := Random(MaxInt)-Random(MaxInt);
    i2 := Random(MaxInt)-i1;
    Check(I.Add(i1,i2)=i1+i2);
    Check(I.Multiply(i1,i2)=Int64(i1)*Int64(i2));
    n1 := Random*1E-9-Random*1E-8;
    n2 := n1*Random;
    CheckSame(I.Subtract(n1,n2),n1-n2);
    s1 := n1;
    s2 := n2;
    CheckSame(s1,n1);
    CheckSame(s2,n2);
    I.Swap(s1,s2);
    CheckSame(s1,n2);
    CheckSame(s2,n1);
    cu := i1*0.01;
    I.ToText(cu,s);
    Check(s=Curr64ToStr(PInt64(@cu)^));
    r := I.ToTextFunc(n1);
    CheckSame(StrToFloat(r),n1);
    o := [tfoIndex,tfoCaseInsensitive];
    i3 := i1;
    c := cardinal(i2);
    Check(I.SpecialCall(s,i3,c,
      [tftDouble],[tftWinAnsi,tftVarInt64],o)=
      [tftWinAnsi,tftVarInt64,tftDouble]);
    Check(i3=i1+length(s));
    Check(c=cardinal(i2)+1);
    Check(o=[tfoUnique,tfoCaseInsensitive]);
    Ints[0] := i1;
    Ints[1] := i2;
    SetLength(Str2,3);
    Str2[0] := 'ABC';
    Str2[1] := 'DEF';
    Str2[2] := 'GHIJK';
    FillCharFast(Rec1,sizeof(Rec1),0);
    Rec1.Features := [vtTransaction,vtSavePoint];
    Rec1.FileExtension := ExeVersion.ProgramFileName;
    Rec2.ID := i1;
    Rec2.Timestamp512 := c;
    Rec2.JSON := 'abc';
    RecRes := I.ComplexCall(Ints,Strs1,Str2,Rec1,Rec2,n1,n2);
    Check(length(Str2)=4);
    Check(Str2[0]='ABC');
    Check(Str2[1]='DEF');
    Check(Str2[2]='GHIJK');
    Check(Str2[3]='one,two,three');
    Check(Rec1.Features=[vtTransaction,vtSavePoint]);
    Check(Rec1.FileExtension=ExeVersion.ProgramFileName);
    Check(Rec2.ID=i1+1);
    Check(Rec2.Timestamp512=c-1);
    Check(Rec2.JSON=IntegerDynArrayToCSV(pointer(Ints),length(Ints)));
    Check(RecRes.ID=i1);
    Check(RecRes.Timestamp512=c);
    Check(RecRes.JSON=StringToUTF8(Rec1.FileExtension));
    CheckSame(n1,n2);
    Rec1.FileExtension := ''; // to avoid memory leak
  end;
  n1 := 0;
  RecRes := I.ComplexCall(Ints,nil,Str2,Rec1,Rec2,n1,n2);
  Check(length(Str2)=5);
  Check(Str2[0]='ABC');
  Check(Str2[1]='DEF');
  Check(Str2[2]='GHIJK');
  Check(Str2[3]='one,two,three');
  Check(Str2[4]='');
  s := StringToUTF8(StringOfChar(#1,100));
  check(I.DirectCall(s)=100);
  s := StringToUTF8(StringOfChar('-',600));
  t := length(I.RepeatJsonArray(s, 100));
  checkutf8(t = 1 + 100 * 603, 'RawJSON %', [KB(t)]);
  t := length(I.RepeatTextArray(s, 100));
  checkutf8(t = 100 * 600, 'RawUTF8 %', [KB(t)]);
end;
var s: RawUTF8;
{$ifndef LVCL}
    data: TCustomerData;
    people: TSQLRecordPeople;
    cust: TServiceCustomAnswer;
    c: cardinal;
    n1,n2: double;
    C1,C2,C3: TComplexNumber;
    Item: TCollTest;
    List,Copy: TCollTestsI;
    j: integer;
    x,y: PtrUInt; // TThreadID  = ^TThreadRec under BSD
{$endif}
{$ifndef NOVARIANTS}
    V1,V2,V3: variant;
{$endif}
{$ifdef UNICODE}
    Nav: TConsultaNav;
{$endif}
begin
  Check(Inst.I.Add(1,2)=3);
  Check(Inst.I.Multiply($1111333,$222266667)=$24693E8DB170B85);
  Check(Inst.I.StackIntMultiply(1,2,3,4,5,6,7,8,9,10)=3628800);
  Check(Inst.I.StackFloatMultiply(1,2,3,4,5,6,7,8,9,10)=3628800);
  CheckSame(Inst.I.Subtract(23,20),3);
  Inst.I.ToText(3.14,s);
  Check(s='3.14');
  Check(Inst.I.ToTextFunc(777)='777');
  x := Inst.CT.GetCurrentThreadID;
  if GlobalInterfaceTestMode<>itmHttp then begin
    y := Inst.CT.GetThreadIDAtCreation;
    Check(x=y);
  end;
  case GlobalInterfaceTestMode of
  itmMainThread:
    Check(Inst.CC.GetCurrentThreadID=PtrUInt(MainThreadID));
  itmPerInterfaceThread,itmLocked:
    Check(Inst.CC.GetCurrentThreadID<>PtrUInt(MainThreadID));
  end;
  TestCalculator(Inst.I);
  TestCalculator(Inst.CC); // test the fact that CC inherits from ICalculator
  {$ifndef LVCL}   /// in LVCL, TPersistent doesn't have any RTTI information
  C3 := TComplexNumber.Create(0,0);
  C1 := TComplexNumber.Create(2,3);
  C2 := TComplexNumber.Create(20,30);
  List := TCollTestsI.Create;
  Copy := TCollTestsI.Create;
  Item := TCollTest.Create(nil);
  try
    Check(Inst.CC.IsNull(C3));
    for c := 0 to Iterations do begin
      Check(not Inst.CC.IsNull(C1));
      C3.Imaginary := 0;
      Inst.CC.Substract(C1,C2,C3);
      CheckSame(C3.Real,c-18.0);
      CheckSame(C3.Imaginary,-27);
      cust := Inst.CC.TestBlob(C3);
      Check(PosEx(TEXT_CONTENT_TYPE_HEADER,cust.Header)>0);
      Check(cust.Content=FormatUTF8('%,%',[C3.Real,C3.Imaginary]));
{$ifndef NOVARIANTS}
      V1 := C3.Real;
      V2 := c;
      case c mod 3 of
      0: s := DoubleToStr(C3.Real);
      1: s := Int32ToUtf8(c);
      2: s := QuotedStr(Int32ToUtf8(c),'"');
      end;
      V3 := Inst.CC.TestVariants(s,V1,V2);
      CheckSame(V1,C3.Real);
      CheckSame(V2,C3.Real+c);
      Check(VariantSaveJSON(V3)=s);
{$endif}
      Check(Inst.CC.GetCustomer(c,data));
      Check(data.Id=integer(c));
      Check(GetCardinal(pointer(data.AccountNum))=c);
      people := TSQLRecordPeople.Create;
      try
        people.fID := c;
        Inst.CC.FillPeople(people);
        Check(people.ID=c);
        Check(people.LastName=FormatUTF8('Last %',[c]));
        Check(people.FirstName=FormatUTF8('First %',[c]));
      finally
        people.Free;
      end;
{$ifdef UNICODE}
      Nav.MaxRows := c;
      Nav.Row0 := c*2;
      Nav.RowCount := c*3;
      Nav.IsSQLUpdateBack := c and 1=0;
      Nav.EOF := c and 1=1;
      with Inst.CC.EchoRecord(Nav) do begin
        Check(MaxRows=c);
        Check(Row0=c*2);
        Check(RowCount=c*3);
        Check(IsSQLUpdateBack=(c and 1=0));
        Check(EOF=(c and 1=1));
      end;
{$endif}
      if c mod 10=1 then begin
        Item.Color := Item.Color+1;
        Item.Length := Item.Color*2;
        Item.Name := Int32ToUtf8(Item.Color);
        Inst.CC.Collections(Item,List,Copy);
      end;
      if not CheckFailed(List.Count=Item.Color) or
         not CheckFailed(Copy.Count=List.Count) then
        for j := 0 to List.Count-1 do begin
          with TCollTest(List.Items[j]) do begin
            Check(Color=j+1);
            Check(Length=Color*2);
            Check(GetInteger(pointer(Name))=Color);
          end;
          with TCollTest(Copy.Items[j]) do begin
            Check(Color=j+1);
            Check(Length=Color*2);
            Check(GetInteger(pointer(Name))=Color);
          end;
        end;
      C1.Real := C1.Real+1;
    end;
    C3.Real := maxInt; // magic value for huge content
    cust := Inst.CC.TestBlob(C3);
    j := length(cust.Content);
    checkutf8(j = 600, 'TestBlob len=%', [j]);
  finally
    C3.Free;
    C1.Free;
    C2.Free;
    Item.Free;
    List.Free;
    Copy.Free;
  end;
  n2 := Inst.CN.Imaginary;
  for c := 0 to Iterations shr 2 do begin
    CheckSame(Inst.CN.Imaginary,n2,1E-9);
    n1 := Random*1000;
    Inst.CN.Real := n1;
    CheckSame(Inst.CN.Real,n1);
    CheckSame(Inst.CN.Imaginary,n2,1E-9);
    n2 := Random*1000;
    Inst.CN.Imaginary := n2;
    CheckSame(Inst.CN.Real,n1);
    CheckSame(Inst.CN.Imaginary,n2,1E-9);
    Inst.CN.Add(1,2);
    CheckSame(Inst.CN.Real,n1+1,1E-9);
    n2 := n2+2;
    CheckSame(Inst.CN.Imaginary,n2,1E-9);
  end;
  {$endif}
  Inst.CN.Assign(3.14,1.05946);
  CheckSame(Inst.CN.Real,3.14);
  CheckSame(Inst.CN.Imaginary,1.05946);
  Check(Inst.CU.GetContextSessionID=Inst.ExpectedSessionID);
  Check(Inst.CG.GetContextSessionGroup=Inst.ExpectedGroupID);
  Check(Inst.CS.GetContextSessionUser=Inst.ExpectedUserID);
  x := Inst.CT.GetCurrentThreadID;
  y := Inst.CT.GetThreadIDAtCreation;
  case GlobalInterfaceTestMode of
  itmDirect: begin
    Check(x=y);
    Check(Inst.CT.GetCurrentRunningThreadID=0);
    Check(Inst.CT.GetContextServiceInstanceID=0);
  end;
  itmClient, itmPerInterfaceThread: begin
    Check(x=y);
    Check(Inst.CT.GetCurrentRunningThreadID=0);
    Check(Inst.CT.GetContextServiceInstanceID<>0);
  end;
  itmLocked, itmMainThread: begin
    Check(x=y);
    Check(Inst.CT.GetCurrentRunningThreadID<>0);
    Check(Inst.CT.GetContextServiceInstanceID<>0);
  end;
  itmHttp: begin
    Check(Inst.CT.GetCurrentRunningThreadID<>0);
    Check(Inst.CT.GetCurrentThreadID<>PtrUInt(MainThreadID));
    Check(Inst.CT.GetContextServiceInstanceID<>0);
  end;
  end;
end;

procedure TTestServiceOrientedArchitecture.SetOptions(aAsJSONObject: boolean;
  aOptions: TServiceMethodOptions);
var s: integer;
begin
  with fClient.Server.Services do
    for s := 0 to Count-1 do
      with Index(s) as TServiceFactoryServer do begin
        ResultAsJSONObject := aAsJSONObject;
        if InterfaceTypeInfo<>TypeInfo(ITestPerThread) then
          SetOptions([],aOptions);
     end;
end;

procedure TTestServiceOrientedArchitecture.ClientTest(aRouting: TSQLRestServerURIContextClass;
  aAsJSONObject: boolean; {$ifndef LVCL}aRunInOtherThread: boolean;{$endif}
  aOptions: TServiceMethodOptions);
var Inst: TTestServiceInstances;
    O: TObject;
    sign: RawUTF8;
    stat: TSynMonitorInputOutput;
begin
  FillCharFast(Inst,sizeof(Inst),0);
  GlobalInterfaceTestMode := itmClient;
  {$ifndef LVCL}
  if aRunInOtherThread then
    if optExecLockedPerInterface in aOptions then
      GlobalInterfaceTestMode := itmLocked else
    if optExecInMainThread in aOptions then
      GlobalInterfaceTestMode := itmMainThread else
    if optExecInPerInterfaceThread in aOptions then
      GlobalInterfaceTestMode := itmPerInterfaceThread;
  {$endif}
  (fClient.Services['Calculator'] as TServiceFactoryClient).
    ParamsAsJSONObject := aAsJSONObject;
  SetOptions(aAsJSONObject,aOptions);
  fClient.Server.ServicesRouting := aRouting;
  fClient.ServicesRouting := aRouting;
  (fClient.Server.Services as TServiceContainerServer).PublishSignature := true;
  sign := fClient.Services['Calculator'].RetrieveSignature;
  Check(sign=fClient.Server.Services['Calculator'].RetrieveSignature);
  (fClient.Server.Services as TServiceContainerServer).PublishSignature := false;
  Check(fClient.Services['Calculator'].RetrieveSignature='');
  // once registered, can be accessed by its GUID or URI
  if CheckFailed(fClient.Services.Info(TypeInfo(ICalculator)).Get(Inst.I)) or
     CheckFailed(fClient.Services.Info(TypeInfo(IComplexCalculator)).Get(Inst.CC)) or
     CheckFailed(fClient.Services.Info(TypeInfo(IComplexNumber)).Get(Inst.CN)) or
     CheckFailed(fClient.Services.Info(TypeInfo(ITestUser)).Get(Inst.CU)) or
     CheckFailed(fClient.Services.Info(TypeInfo(ITestSession)).Get(Inst.CS)) or
     CheckFailed(fClient.Services.Info(TypeInfo(ITestGroup)).Get(Inst.CG)) or
     CheckFailed(fClient.Services.Info(TypeInfo(ITestPerThread)).Get(Inst.CT)) then
    exit;
  O := ObjectFromInterface(Inst.I);
  Check((O<>nil) and (copy(O.ClassName,1,21)='TInterfacedObjectFake'));
  Inst.ExpectedSessionID := fClient.SessionID;
  if CheckFailed(fClient.SessionUser<>nil) then
    exit;
  fClient.Retrieve('LogonName=?',[],[fClient.SessionUser.LogonName],fClient.SessionUser);
  Inst.ExpectedUserID := fClient.SessionUser.ID;
  Inst.ExpectedGroupID := fClient.SessionUser.GroupRights.ID;
  Test(Inst);
  Inst.I := nil;
  if CheckFailed(fClient.Services.Info(ICalculator).Get(Inst.I)) then
    exit;
  Test(Inst);
  Inst.I := nil;
  if CheckFailed(fClient.Services.Resolve(ICalculator,Inst.I)) then
    exit;
  Test(Inst);
  Finalize(Inst);
  if CheckFailed(fClient.Services['Calculator'].Get(Inst.I)) or
     CheckFailed(fClient.Services['ComplexCalculator'].Get(Inst.CC)) or
     CheckFailed(fClient.Services['ComplexNumber'].Get(Inst.CN))
{$ifdef ISDELPHI2010}
     then exit;
  Inst.CU := fClient.Service<ITestUser>;
  if CheckFailed(Inst.CU<>nil) then exit;
  Inst.CS := fClient.Service<ITestSession>;
  if CheckFailed(Inst.CS<>nil) then exit;
  Inst.CG := fClient.Service<ITestGroup>;
  if CheckFailed(Inst.CG<>nil) then exit;
  Inst.CT := fClient.Service<ITestPerThread>;
  if CheckFailed(Inst.CT<>nil) then exit;
{$else} or
     CheckFailed(fClient.Services['TestUser'].Get(Inst.CU)) or
     CheckFailed(fClient.Services['TestSession'].Get(Inst.CS)) or
     CheckFailed(fClient.Services['TestGroup'].Get(Inst.CG)) or
     CheckFailed(fClient.Services['testperthread'].Get(Inst.CT)) then
    exit;
{$endif}
  {$ifndef CPUARM}
  // The FPC arm optimizer ruins a return address at level -O2
  // So, disable this test until a suitable fix is found.
  Inst.CN.Imaginary;
  {$endif}
  Test(Inst);
  SetOptions(false,[]);
  stat := (fClient.Server.Services['Calculator'] as TServiceFactoryServer).Stat['ToText'];
  Check(stat.TaskCount>0);
end;

procedure TTestServiceOrientedArchitecture.DirectCall;
var Inst: TTestServiceInstances;
begin
  FillCharFast(Inst,sizeof(Inst),0); // all Expected..ID=0
  Inst.I := TServiceCalculator.Create;
  Inst.CC := TServiceComplexCalculator.Create;
  Inst.CN := TServiceComplexNumber.Create;
  Inst.CS := TServiceUserGroupSession.Create;
  Inst.CG := TServiceUserGroupSession.Create;
  Inst.CU := TServiceUserGroupSession.Create;
  Inst.CT := TServicePerThread.Create;
  Test(Inst);
  Test(Inst);
  Test(Inst);
end;

procedure TTestServiceOrientedArchitecture.ServerSide;
var Inst: TTestServiceInstances;
begin
  FillCharFast(Inst,sizeof(Inst),0); // all Expected..ID=0
  if CheckFailed(fModel<>nil) or CheckFailed(fClient<>nil) or
     CheckFailed(fClient.Server.Services.Count=7) or
     CheckFailed(fClient.Server.Services.Index(0).Get(Inst.I)) or
     CheckFailed(Assigned(Inst.I)) or
     CheckFailed(fClient.Server.Services.Info(TypeInfo(ICalculator)).Get(Inst.I)) or
     CheckFailed(fClient.Server.Services.Info(TypeInfo(IComplexCalculator)).Get(Inst.CC)) or
     CheckFailed(fClient.Server.Services.Info(TypeInfo(IComplexNumber)).Get(Inst.CN)) or
     CheckFailed(fClient.Server.Services.Info(TypeInfo(ITestUser)).Get(Inst.CU)) or
     CheckFailed(fClient.Server.Services.Info(TypeInfo(ITestSession)).Get(Inst.CS)) or
     CheckFailed(fClient.Server.Services.Info(TypeInfo(ITestGroup)).Get(Inst.CG)) or
     CheckFailed(fClient.Server.Services.Info(TypeInfo(ITestPerThread)).Get(Inst.CT)) then
    exit;
  Test(Inst);
  Finalize(Inst);
  Check(Inst.I=nil);
  if CheckFailed(fClient.Server.Services['Calculator'].Get(Inst.I)) or
     CheckFailed(fClient.Server.Services['ComplexCalculator'].Get(Inst.CC)) or
     CheckFailed(fClient.Server.Services['ComplexNumber'].Get(Inst.CN))
{$ifdef ISDELPHI2010}
     then exit;
  Inst.CU := fClient.Server.Service<ITestUser>;
  if CheckFailed(Inst.CU<>nil) then exit;
  Inst.CS := fClient.Server.Service<ITestSession>;
  if CheckFailed(Inst.CS<>nil) then exit;
  Inst.CG := fClient.Server.Service<ITestGroup>;
  if CheckFailed(Inst.CG<>nil) then exit;
  Inst.CT := fClient.Server.Service<ITestPerThread>;
  if CheckFailed(Inst.CT<>nil) then exit;
{$else} or
     CheckFailed(fClient.Server.Services['TestUser'].Get(Inst.CU)) or
     CheckFailed(fClient.Server.Services['TestSession'].Get(Inst.CS)) or
     CheckFailed(fClient.Server.Services['TestGroup'].Get(Inst.CG)) or
     CheckFailed(fClient.Server.Services['TestPerThread'].Get(Inst.CT)) then
    exit;
{$endif}
  Test(Inst);
  Test(Inst);
end;

procedure TTestServiceOrientedArchitecture.ServiceInitialization;
  function Ask(Method, Params,ParamsURI,ParamsObj: RawUTF8; ExpectedResult: cardinal): RawUTF8;
  var resp,data,uriencoded,head: RawUTF8;
  begin
    Params := ' [ '+Params+' ]'; // add some ' ' to test real-world values
    uriencoded := '?'+UrlEncode(Params);
    if fClient.Server.ServicesRouting=TSQLRestRoutingREST then begin
      SetString(data,PAnsiChar(pointer(Params)),length(Params)); // =UniqueString
      Check(fClient.URI('root/calculator.'+Method,'POST',@resp,nil,@data).Lo=ExpectedResult);
      if ExpectedResult=200 then begin
        Check(fClient.URI('root/CALCulator.'+Method+uriencoded,'POST',@data).Lo=ExpectedResult);
        Check(data=resp,'alternative URI-encoded-inlined parameters use');
        Check(fClient.URI('root/Calculator.'+Method+'?'+ParamsURI,'GET',@data).Lo=ExpectedResult);
        Check(data=resp,'alternative "param1=value1&param2=value2" URI-encoded scheme');
        Check(fClient.URI('root/Calculator.'+Method+'/1234?'+ParamsURI,'GET',@data).Lo=ExpectedResult);
        Check(data=resp,'alternative URI-encoded scheme with ClientDrivenID');
        SetString(data,PAnsiChar(pointer(Params)),length(Params)); // =UniqueString
        Check(fClient.URI('root/calculator/'+Method,'POST',@data,nil,@data).Lo=ExpectedResult);
        Check(data=resp,'interface/method routing');
        SetString(data,PAnsiChar(pointer(Params)),length(Params)); // =UniqueString
        Check(fClient.URI('root/calculator/'+Method+'/123','POST',@data,nil,@Params).Lo=ExpectedResult);
        Check(data=resp,'interface/method/clientdrivenID routing');
        Check(fClient.URI('root/CALCulator/'+Method+uriencoded,'POST',@data).Lo=ExpectedResult);
        Check(data=resp,'alternative URI-encoded-inlined parameters use');
        Check(fClient.URI('root/Calculator/'+Method+'?'+ParamsURI,'GET',@data).Lo=ExpectedResult);
        Check(data=resp,'alternative "param1=value1&param2=value2" URI-encoded scheme');
        SetString(data,PAnsiChar(pointer(ParamsObj)),length(ParamsObj)); // =UniqueString
        Check(fClient.URI('root/calculator/'+Method,'POST',@data,nil,@data).Lo=ExpectedResult);
        Check(data=resp,'alternative object-encoded-as-body parameters use');
        head := 'accept: application/xml';
        Check(fClient.URI('root/Calculator/'+Method+'?'+ParamsURI,'GET',@data,@head).Lo=ExpectedResult);
        Check(data<>resp,'returned as XML');
        check(head=XML_CONTENT_TYPE_HEADER);
        Check(IdemPChar(pointer(data),'<?XML'),'returned as XML');
      end;
    end else
    if fClient.Server.ServicesRouting=TSQLRestRoutingJSON_RPC then begin
      data := '{"method":"'+Method+'", "params":'+Params+'}';
      Check(fClient.URI('root/calculator','POST',@resp,nil,@data).Lo=ExpectedResult);
    end else
      raise Exception.Create('Invalid call');
    result := JSONDecode(resp,'result',nil,true);
    if IdemPChar(Pointer(result),'{"RESULT"') then
      result := JSONDecode(result,'result',nil,false) else
      result := copy(result,2,length(result)-2); // trim '[' + ']'
    if (result<>'') and (result[1]='"') then
      result := UnQuoteSQLString(result); // '"777"' -> '777'
    if (ExpectedResult=200) and (fClient.Server.ServicesRouting=TSQLRestRoutingREST) then begin
      resp := XMLUTF8_HEADER+'<result><Result>'+result+'</Result></result>';
      check(data=resp);
    end;
  end;
var S: TServiceFactory;
    i: integer;
    rout: integer;
    resp: RawUTF8;
const
  ROUTING: array[0..1] of TSQLRestServerURIContextClass =
    (TSQLRestRoutingREST,TSQLRestRoutingJSON_RPC);
const ExpectedURI: array[0..5] of RawUTF8 =
        ('Add','Multiply','Subtract','ToText','ToTextFunc','Swap');
      ExpectedParCount: array[0..5] of Integer = (4,4,4,3,3,3);
      ExpectedArgs: array[0..5] of TServiceMethodValueTypes =
        ([smvSelf,smvInteger],[smvSelf,smvInt64],[smvSelf,smvDouble],
         [smvSelf,smvCurrency,smvRawUTF8],[smvSelf,smvDouble,smvString],
         [smvSelf,smvDouble]);
      ExpectedTypes: array[0..4] of String[10] =
        ('Integer','Int64','Double','Currency','Double');
      ExpectedType: array[0..5] of TServiceMethodValueType =
        (smvInteger,smvInt64,smvDouble,smvCurrency,smvDouble,smvDouble);
      ExpectedResult: array[0..2] of String[10] = ('Integer','Int64','Double');
begin
  if CheckFailed(fModel=nil) then exit; // should be called once
  // create model, client and server
  fModel := TSQLModel.Create([TSQLRecordPeople,TSQLAuthUser,TSQLAuthGroup]);
  fClient := TSQLRestClientDB.Create(fModel,nil,'test.db3',TSQLRestServerDB,true);
  Check(fClient.SetUser('User','synopse'),'default user for Security tests');
  Check(fClient.Server.
    ServiceRegister(TServiceCalculator,[TypeInfo(ICalculator)],sicShared)<>nil,
    'register TServiceCalculator as the ICalculator implementation on the server');
  // verify ICalculator RTTI-generated details
  Check(fClient.Server.Services<>nil);
  if CheckFailed(fClient.Server.Services.Count=1) then exit;
  S := fClient.Server.Services.Index(0);
  if CheckFailed(S<>nil) then exit;
  Check(S.InterfaceURI='Calculator');
  Check(S.InstanceCreation=sicShared);
  Check(S.InterfaceTypeInfo^.Kind=tkInterface);
  Check(S.InterfaceTypeInfo^.Name='ICalculator');
  Check(GUIDToString(S.InterfaceIID)='{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}');
  Check(GUIDToRawUTF8(S.InterfaceIID)='{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}');
  Check(S.InterfaceMangledURI='7chgmrLOCU6H1EoW9Jbl_g');
  fClient.Server.Services.ExpectMangledURI := true;
  Check(fClient.Server.Services[S.InterfaceMangledURI]=S);
  fClient.Server.Services.ExpectMangledURI := false;
  Check(fClient.Server.Services['Calculator']=S);
  Check(fClient.Server.Services['CALCULAtor']=S);
  Check(fClient.Server.Services['CALCULAtors']=nil);
  if CheckFailed(length(S.InterfaceFactory.Methods)=13) then exit;
  Check(S.ContractHash='"4C65C91D6536270A"');
  Check(TServiceCalculator(nil).Test(1,2)='3');
  Check(TServiceCalculator(nil).ToTextFunc(777)='777');
  for i := 0 to high(ExpectedURI) do // SpecialCall interface not checked
    with S.InterfaceFactory.Methods[i] do begin
      Check(URI=ExpectedURI[i]);
      Check(length(Args)=ExpectedParCount[i]);
      Check(ArgsUsed=ExpectedArgs[i]);
      Check(Args[0].ParamName^='Self');
      Check(Args[0].ValueDirection=smdConst);
      Check(Args[0].ValueType=smvSelf);
      Check(Args[0].ArgTypeName^='ICalculator');
      Check(Args[1].ValueType=ExpectedType[i]);
      if i<3 then begin
        // 0 function Add(n1,n2: integer): integer;
        // 1 function Multiply(n1,n2: Int64): Int64;
        // 2 function Subtract(n1,n2: double): double;
        Check(Args[1].ParamName^='n1');
        Check(Args[1].ValueDirection=smdConst);
        Check(Args[2].ParamName^='n2');
        Check(Args[2].ValueDirection=smdConst);
        Check(Args[2].ValueType=ExpectedType[i]);
        Check(IdemPropName(Args[3].ArgTypeName^,ExpectedTypes[i]),string(Args[3].ArgTypeName^));
        Check(Args[3].ValueDirection=smdResult);
        Check(Args[3].ValueType=ExpectedType[i]);
      end else
      if i<5 then begin
        // 3 procedure ToText(Value: Currency; var Result: RawUTF8);
        // 4 function ToTextFunc(Value: double): string;
        Check(Args[1].ParamName^='Value');
        Check(Args[1].ValueDirection=smdConst);
        Check(Args[2].ParamName^='Result');
        if i<4 then
          Check(Args[2].ValueDirection=smdVar) else
          Check(Args[2].ValueDirection=smdResult);
        if i<4 then
          Check(Args[2].ValueType=smvRawUTF8) else
          Check(Args[2].ValueType=smvString);
      end else begin
        // 5 procedure Swap(var n1,n2: double);
        Check(Args[1].ParamName^='n1');
        Check(Args[1].ValueDirection=smdVar);
        Check(Args[2].ParamName^='n2');
        Check(Args[2].ValueDirection=smdVar);
      end;
    end;
  // IComplexCalculator + IComplexNumber services
  Check(fClient.Server.ServiceRegister(TServiceComplexCalculator,[TypeInfo(IComplexCalculator)],sicSingle)<>nil);
  Check(fClient.Server.ServiceRegister(TServiceComplexNumber,[TypeInfo(IComplexNumber)],sicClientDriven)<>nil);
  Check(fClient.Server.ServiceRegister(TServiceUserGroupSession,[TypeInfo(ITestSession)],sicPerSession)<>nil);
  Check(fClient.Server.ServiceRegister(TServiceUserGroupSession,[TypeInfo(ITestUser)],sicPerUser)<>nil);
  Check(fClient.Server.ServiceRegister(TServiceUserGroupSession,[TypeInfo(ITestGroup)],sicPerGroup)<>nil);
  Check(fClient.Server.ServiceRegister(TServicePerThread,[TypeInfo(ITestPerThread)],sicPerThread)<>nil);
  // JSON-level access
  for rout := low(ROUTING) to high(ROUTING) do begin
    fClient.ServicesRouting := ROUTING[rout];
    fClient.Server.ServicesRouting := ROUTING[rout];
    if rout=0 then
      (fClient.Server.Services['Calculator'] as TServiceFactoryServer).
        ResultAsXMLObjectIfAcceptOnlyXML := true;
    Check(Ask('None','1,2','one=1&two=2','{one:1,two=2}',400)='');
    Check(Ask('Add','1,2','n1=1&n2=2','{n1:1,n2:2}',200)='3');
    Check(Ask('Add','1,0','n2=1','{n2:1}',200)='1');
    Check(Ask('Multiply','2,3','n1=2&n2=3','{n0:"abc",n2:3,m:null,n1:2}',200)='6');
    Check(Ask('Subtract','23,20','n2=20&n1=23','{n0:"abc",n2:20,n1:23}',200)='3');
    Check(Ask('ToText','777,"abc"','result=abc&value=777','{result:"abc",value=777}',200)='777');
    Check(Ask('ToTextFunc','777','value=777','{result:"abc",value=777}',200)='777');
    if rout=0 then
      Check(fClient.URI('root/ComplexCalculator.GetCustomer?CustomerId=John%20Doe',
        'POST',@resp,nil,nil).Lo=406,'incorrect input');
  end;
  fClient.ServicesRouting := TSQLRestRoutingREST; // back to default
  fClient.Server.ServicesRouting := TSQLRestRoutingREST;
end;

procedure TTestServiceOrientedArchitecture.Security;
  procedure Test(Expected: TSQLFieldTables; const msg: string);
    function Ask(const Method, Params: RawUTF8): RawUTF8;
    var resp,data: RawUTF8;
    begin
      data := '{"method":"'+Method+'", "params": [ '+Params+' ]}';
      fClient.URI('root/calculator','POST',@resp,nil,@data);
      result := JSONDecode(resp,'result',nil,true);
    end;
  begin
    Check((Ask('None','1,2')=''),msg);
    CheckMatchAny(Ask('Add','1,2'),['[3]','{"Result":3}'],true,(1 in Expected),msg);
    CheckMatchAny(Ask('Multiply','2,3'),['[6]','{"Result":6}'],true,(2 in Expected),msg);
    CheckMatchAny(Ask('Subtract','23,20'),['[3]','{"Result":3}'],true,(3 in Expected),msg);
    CheckMatchAny(Ask('ToText','777,"abc"'),['["777"]','{"Result":"777"}'],true,(4 in Expected),msg);
    CheckMatchAny(Ask('ToTextFunc','777'),['["777"]','{"Result":"777"}'],true,(5 in Expected),msg);
  end;
var S: TServiceFactoryServer;
    GroupID: TID;
    g: TIDDynArray;
begin
  fClient.ServicesRouting := TSQLRestRoutingJSON_RPC;
  fClient.Server.ServicesRouting := TSQLRestRoutingJSON_RPC;
  GroupID := fClient.MainFieldID(TSQLAuthGroup,'User');
  Check(GroupID<>0);
  Check(fClient.MainFieldIDs(TSQLAuthGroup,['User','Admin'],g));
  Check(length(g)=2);
  Check((g[0]=GroupID) or (g[1]=GroupID));
  S := fClient.Server.Services['Calculator'] as TServiceFactoryServer;
  Test([1,2,3,4,5],'by default, all methods are allowed');
  S.AllowAll;
  Test([1,2,3,4,5],'AllowAll should change nothing');
  S.DenyAll;
  Test([],'DenyAll will reset all settings');
  S.AllowAll;
  Test([1,2,3,4,5],'back to full acccess for everybody');
  S.DenyAllByID([GroupID]);
  Test([],'our current user shall be denied');
  S.AllowAll;
  Test([1,2,3,4,5],'restore allowed for everybody');
  S.DenyAllByID([GroupID+1]);
  Test([1,2,3,4,5],'this group ID won''t affect the current user');
  S.DenyByID(['Add'],[GroupID]);
  Test([2,3,4,5],'exclude a specific method for the current user');
  S.DenyByID(['totext'],[GroupID]);
  Test([2,3,5],'exclude another method for the current user');
  S.AllowByID(['Add'],[GroupID+1]);
  Test([2,3,5],'this group ID won''t affect the current user');
  S.AllowByID(['Add'],[GroupID]);
  Test([1,2,3,5],'allow a specific method for the current user');
  S.AllowAllByID([0]);
  Test([1,2,3,5],'invalid group ID won''t affect the current user');
  S.AllowAllByID([GroupID]);
  Test([1,2,3,4,5],'restore allowed for the current user');
  Check(not fClient.SetUser('unknown','wrongpass'));
  Test([],'no authentication -> access denied');
  Check(fClient.SetUser('Admin','synopse'));
  Test([1,2,3,4,5],'authenticated user');
  S.DenyAll;
  Test([],'DenyAll works even for admins');
  S.AllowAll;
  Test([1,2,3,4,5],'restore allowed for everybody');
  S.AllowAllByName(['Supervisor']);
  Test([1,2,3,4,5],'this group name won''t affect the current Admin user');
  S.DenyAllByName(['Supervisor']);
  Test([1,2,3,4,5],'this group name won''t affect the current Admin user');
  S.DenyAllByName(['Supervisor','Admin']);
  Test([],'Admin group user was explicitely denied access');
  S.AllowAllByName(['Admin']);
  Test([1,2,3,4,5],'restore allowed for current Admin user');
  S.AllowAll;
  Check(fClient.SetUser('User','synopse'));
  Test([1,2,3,4,5],'restore allowed for everybody');
end;

procedure TTestServiceOrientedArchitecture.ClientSideREST;
begin
  Check(fClient.ServiceRegister([TypeInfo(ICalculator)],sicShared));
  Check(fClient.ServiceRegister([TypeInfo(IComplexCalculator)],sicSingle));
  Check(fClient.ServiceRegister([TypeInfo(ITestSession)],sicPerSession));
  Check(fClient.ServiceRegister([TypeInfo(ITestUser)],sicPerUser));
  Check(fClient.ServiceRegister([TypeInfo(ITestGroup)],sicPerGroup));
  Check(fClient.ServiceRegister([TypeInfo(ITestPerThread)],sicPerThread));
  ClientTest(TSQLRestRoutingREST,false);
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTServiceLogToDB;
var Log: TSQLRestServerDB;
begin
  {$ifdef Darwin}
  {$ifdef NOSQLITE3STATIC}
  // due to a very strange error during prepare_v2, this does not (yet) work on Darwin.
  // at least on Darwin with system sqlite 3.7.13
  // however, mORMots own static works perfect
  Check(1=0,'Not (yet) supported on Darwin !!');
  exit;
  {$endif}
  {$endif}
  DeleteFile('servicelog.db');
  Log := TSQLRestServerDB.CreateWithOwnModel([TSQLRecordServiceLog],'servicelog.db');
  try
    Log.DB.Synchronous := smOff;
    Log.DB.LockingMode := lmExclusive;
    Log.CreateMissingTables;
    (fClient.Server.ServiceContainer as TServiceContainerServer).SetServiceLog(Log);
    ClientTest(TSQLRestRoutingREST,false);
  finally
    (fClient.Server.ServiceContainer as TServiceContainerServer).SetServiceLog(nil);
    Log.Free;
  end;
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTSessionsStats;
var stats: RawUTF8;
    store: TSQLRestServerDB;
begin
  fClient.Server.StatLevels := SERVERDEFAULTMONITORLEVELS+[mlSessions];
  store := TSQLRestServerDB.CreateWithOwnModel([TSQLMonitorUsage],'servicestats.db3');
  try
    store.DB.Synchronous := smOff;
    store.DB.LockingMode := lmExclusive;
    store.CreateMissingTables;
    fClient.Server.StatUsage := TSynMonitorUsageRest.Create(store,1);
    ClientTest(TSQLRestRoutingREST,false);
    fClient.CallBackGet('stat',['withall',true],stats);
    JSONReformatToFile(stats,'statsSessions.json');
    fClient.Server.StatLevels := SERVERDEFAULTMONITORLEVELS;
    fClient.Server.StatUsage := nil;
  finally
    store.Free;
  end;
end;

procedure TTestServiceOrientedArchitecture.ClientSideJSONRPC;
begin
  ClientTest(TSQLRestRoutingJSON_RPC,false);
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTAsJSONObject;
begin
  ClientTest(TSQLRestRoutingREST,true);
end;

procedure TTestServiceOrientedArchitecture.TestOverHTTP;
var HTTPServer: TSQLHttpServer;
    HTTPClient: TSQLHttpClient;
    Inst: TTestServiceInstances;
    json: RawUTF8;
    i: integer;
    URI: TSQLRestServerURIDynArray;
const SERVICES: array[0..4] of RawUTF8 = (
  'Calculator','ComplexCalculator','TestUser','TestGroup','TestPerThread');
begin
  fClient.Server.ServicesRouting := TSQLRestRoutingREST; // back to default
  GlobalInterfaceTestMode := itmHttp;
  HTTPServer := TSQLHttpServer.Create(HTTP_DEFAULTPORT,[fClient.Server],'+',
    {$ifdef ONLYUSEHTTPSOCKET}useHttpSocket{$else}useHttpApiRegisteringURI{$endif},
    8,secNone);
  try
    FillCharFast(Inst,sizeof(Inst),0); // all Expected..ID=0
    HTTPClient := TSQLHttpClient.Create('127.0.0.1',HTTP_DEFAULTPORT,fModel);
    try
      HTTPClient.ServicePublishOwnInterfaces(fClient.Server);
      //HTTPClient.OnIdle := TLoginForm.OnIdleProcess; // from mORMotUILogin
      // HTTPClient.Compression := [hcSynShaAes]; // 350ms (300ms for [])
      Check(HTTPClient.SetUser('User','synopse'));
      // register services on the client side
      Check(HTTPClient.ServiceRegister([TypeInfo(ICalculator)],sicShared));
      Check(HTTPClient.ServiceRegister([TypeInfo(IComplexCalculator)],sicSingle));
      Check(HTTPClient.ServiceRegister([TypeInfo(ITestSession)],sicPerSession));
      Check(HTTPClient.ServiceRegister([TypeInfo(ITestUser)],sicPerUser));
      Check(HTTPClient.ServiceRegister([TypeInfo(ITestGroup)],sicPerGroup));
      Check(HTTPClient.ServiceRegister([TypeInfo(ITestPerThread)],sicPerThread));
      // retrieve service instances
      if CheckFailed(HTTPClient.Services.Info(TypeInfo(ICalculator)).Get(Inst.I)) or
         CheckFailed(HTTPClient.Services.Info(TypeInfo(IComplexCalculator)).Get(Inst.CC)) or
         CheckFailed(HTTPClient.Services.Info(TypeInfo(IComplexNumber)).Get(Inst.CN)) or
         CheckFailed(HTTPClient.Services.Info(TypeInfo(ITestUser)).Get(Inst.CU)) or
         CheckFailed(HTTPClient.Services.Info(TypeInfo(ITestSession)).Get(Inst.CS)) or
         CheckFailed(HTTPClient.Services.Info(TypeInfo(ITestGroup)).Get(Inst.CG)) or
         CheckFailed(HTTPClient.Services.Info(TypeInfo(ITestPerThread)).Get(Inst.CT)) then
        exit;
      Inst.ExpectedSessionID := HTTPClient.SessionID;
      HTTPClient.Retrieve('LogonName=?',[],[HTTPClient.SessionUser.LogonName],HTTPClient.SessionUser);
      Inst.ExpectedUserID := HTTPClient.SessionUser.ID;
      Inst.ExpectedGroupID := HTTPClient.SessionUser.GroupRights.ID;
      //SetOptions(false{$ifndef LVCL},true,[optExecInMainThread]{$endif});
      Check(HTTPClient.CallBackGet('stat',['findservice','toto'],json)=HTTP_SUCCESS);
      Check(json='[]');
      for i := 0 to High(SERVICES) do begin
        Check(HTTPClient.CallBackGet('stat',['findservice',SERVICES[i]],json)=HTTP_SUCCESS);
        Check(json<>'[]');
        Check(HTTPClient.ServiceRetrieveAssociated(SERVICES[i],URI));
        Check(length(URI)=1);
        Check(URI[0].Port=HTTP_DEFAULTPORT);
        Check(URI[0].Root=fClient.Model.Root);
      end;
      Check(HTTPClient.ServiceRetrieveAssociated(IComplexNumber,URI));
      Check(length(URI)=1);
      Check(HTTPClient.ServiceRetrieveAssociated(ITestSession,URI));
      Check(length(URI)=1);
      Test(Inst,100);
      //SetOptions(false{$ifndef LVCL},true,[]{$endif});
    finally
      Finalize(Inst);
      HTTPClient.Free;
    end;
  finally
    HTTPServer.Free;
    GlobalInterfaceTestMode := itmClient;
  end;
end;

procedure TTestServiceOrientedArchitecture.ClientAlgo(
  algo: TSQLRestServerAuthenticationSignedURIAlgo);
begin
  (fClient.Server.AuthenticationRegister(TSQLRestServerAuthenticationDefault) as
    TSQLRestServerAuthenticationDefault).Algorithm := algo;
  fClient.SetUser('User','synopse');
  ClientTest(TSQLRestRoutingREST,false);
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTSignWithCRC32C;
begin
  ClientAlgo(suaCRC32C)
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTSignWithXXHASH;
begin
  ClientAlgo(suaXXHASH);
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTSignWithMD5;
begin
  ClientAlgo(suaMD5);
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTSignWithSHA256;
begin
  ClientAlgo(suaSHA256);
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTSignWithSHA512;
begin
  ClientAlgo(suaSHA512);
  (fClient.Server.AuthenticationRegister(TSQLRestServerAuthenticationDefault) as
    TSQLRestServerAuthenticationDefault).Algorithm := suaCRC32;
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTWeakAuthentication;
begin
  fClient.Server.ServicesRouting := TSQLRestRoutingJSON_RPC; // back to previous
  fClient.Server.AuthenticationUnregister(
    [{$ifdef MSWINDOWS}TSQLRestServerAuthenticationSSPI,{$endif}
     TSQLRestServerAuthenticationDefault]);
  fClient.Server.AuthenticationRegister(TSQLRestServerAuthenticationNone);
  TSQLRestServerAuthenticationNone.ClientSetUser(fClient,'User','');
  ClientTest(TSQLRestRoutingREST,false);
  fClient.Server.AuthenticationUnregister(TSQLRestServerAuthenticationNone);
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTBasicAuthentication;
begin
  fClient.SessionClose;
  fClient.Server.AuthenticationRegister(TSQLRestServerAuthenticationHttpBasic);
  TSQLRestServerAuthenticationHttpBasic.ClientSetUser(fClient,'User','synopse');
  ClientTest(TSQLRestRoutingREST,false);
  fClient.Server.AuthenticationUnregister(TSQLRestServerAuthenticationHttpBasic);
  // restore default authentications
  fClient.Server.AuthenticationRegister(
    [{$ifdef MSWINDOWS}TSQLRestServerAuthenticationSSPI,{$endif}
     TSQLRestServerAuthenticationDefault]);
  fClient.SetUser('User','synopse');
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTCustomRecordLayout;
begin
  TTextWriter.RegisterCustomJSONSerializer(TypeInfo(TSQLRestCacheEntryValue),
    TTestServiceOrientedArchitecture.CustomReader,
    TTestServiceOrientedArchitecture.CustomWriter);
  try
    ClientTest(TSQLRestRoutingREST,false);
  finally
    TTextWriter.RegisterCustomJSONSerializer(TypeInfo(TSQLRestCacheEntryValue),nil,nil);
  end;
end;

class function TTestServiceOrientedArchitecture.CustomReader(P: PUTF8Char;
  var aValue; out aValid: Boolean{$ifndef NOVARIANTS};
  CustomVariantOptions: PDocVariantOptions{$endif}): PUTF8Char;
var V: TSQLRestCacheEntryValue absolute aValue;
    Values: array[0..2] of TValuePUTF8Char;
begin // {"ID":1786554763,"Timestamp":323618765,"JSON":"D:\\TestSQL3.exe"}
  result := JSONDecode(P,['ID','Timestamp','JSON'],@Values);
  if result=nil then
    aValid := false else begin
    V.ID := GetInt64(Values[0].Value);
    V.Timestamp512 := Values[1].ToCardinal;
    Values[2].ToUTF8(V.JSON);
    aValid := true;
  end;
end;

class procedure TTestServiceOrientedArchitecture.CustomWriter(
  const aWriter: TTextWriter; const aValue);
var V: TSQLRestCacheEntryValue absolute aValue;
begin
  aWriter.AddJSONEscape(['ID',V.ID,'Timestamp',Int64(V.Timestamp512),'JSON',V.JSON]);
end;

procedure TTestServiceOrientedArchitecture.Cleanup;
var stats: RawUTF8;
begin
  if fClient<>nil then begin
    fClient.CallBackGet('stat',['withtables',true,'withsqlite3',true,
      'withmethods',true,'withinterfaces',true,'withsessions',true],stats);
    FileFromString(JSONReformat(stats),'stats.json');
  end;
  FreeAndNil(fClient);
  FreeAndNil(fModel);
end;

{$ifndef LVCL}

{ TTestThread }

type
  TTestThread = class(TThread)
  protected
    Options: TServiceMethodOptions;
    procedure Execute; override;
  public
    Test: TTestServiceOrientedArchitecture;
  end;

procedure TTestThread.Execute;
begin
  try
    Test.fClient.Server.BeginCurrentThread(self);
    Test.ClientTest(TSQLRestRoutingREST,false,true,Options);
    Test.fClient.Server.EndCurrentThread(self);
  finally
    Test := nil; // mark tests finished
  end;
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTMainThread;
begin
  with TTestThread.Create(true) do
  try
    Test := self;
    Options := [optExecInMainThread,optFreeInMainThread];
    {$ifdef ISDELPHI2010}
    Start;
    {$else}
    Resume;
    {$endif}
    while Test<>nil do
      CheckSynchronize{$ifndef DELPHI6OROLDER}(1){$endif};
  finally
    Free;
  end;
  fClient.Server.ServicesRouting := TSQLRestRoutingJSON_RPC; // back to previous
end;

procedure TTestServiceOrientedArchitecture.ClientSideRESTBackgroundThread;
begin
  ClientTest(TSQLRestRoutingREST,false,true,
    [optExecInPerInterfaceThread,optFreeInPerInterfaceThread]);
  fClient.Server.ServicesRouting := TSQLRestRoutingJSON_RPC; // back to previous
end;

{$endif LVCL}

procedure TTestServiceOrientedArchitecture.ClientSideRESTLocked;
begin
{$ifdef LVCL}
  ClientTest(TSQLRestRoutingREST,false,[optExecLockedPerInterface]);
{$else}
  with TTestThread.Create(true) do
  try
    Test := self;
    Options := [optExecLockedPerInterface];
    {$ifdef ISDELPHI2010}
    Start;
    {$else}
    Resume;
    {$endif}
    while Test<>nil do
      CheckSynchronize{$ifndef DELPHI6OROLDER}(1){$endif};
  finally
    Free;
  end;
{$endif}
  fClient.Server.ServicesRouting := TSQLRestRoutingJSON_RPC; // back to previous
end;

type
  IChild = interface;

  IParent = interface
    procedure SetChild(const Value: IChild);
    function GetChild: IChild;
    function HasChild: boolean;
    property Child: IChild read GetChild write SetChild;
  end;

  IChild = interface
    procedure SetParent(const Value: IParent);
    function GetParent: IParent;
    property Parent: IParent read GetParent write SetParent;
  end;

  TParent = class(TInterfacedObject, IParent)
  private
    FChild: IChild;
    procedure SetChild(const Value: IChild);
    function GetChild: IChild;
  public
    destructor Destroy; override;
    function HasChild: boolean;
    property Child: IChild read GetChild write SetChild;
  end;

  TChild = class(TInterfacedObject, IChild)
  private
    FParent: IParent;
    procedure SetParent(const Value: IParent);
    function GetParent: IParent;
  public
    constructor Create(const AParent: IParent; SetChild: boolean);
    destructor Destroy; override;
    property Parent: IParent read GetParent write SetParent;
  end;

  TUseWeakRef = (direct,weakref,zeroing);

var
  ParentDestroyed, ChildDestroyed: boolean;
  UseWeakRef: TUseWeakRef;

procedure TTestServiceOrientedArchitecture.WeakInterfaces;
var Parent: IParent;
    Child, Child2: IChild;
    P: TParent;
    C: TChild;
procedure Init(aWeakRef: TUseWeakRef);
begin
  ParentDestroyed := false;
  ChildDestroyed := false;
  UseWeakRef := aWeakRef;
  Check(Parent=nil);
  Check(Child=nil);
  P := TParent.Create;
  Parent := P;
  Check(ObjectFromInterface(Parent)=P);
  C := TChild.Create(Parent,true);
  Child := C;
  Check(ObjectFromInterface(Child)=C);
  Parent.Child := Child;
end;
procedure WeakTest(aWeakRef: TUseWeakRef);
var Child2: IChild;
begin
  Init(aWeakRef);
  Check(ParentDestroyed=false);
  Check(ChildDestroyed=false);
  Child2 := Parent.Child;
  Child2 := nil; // otherwise memory leak, but it is OK
  Check(ChildDestroyed=false);
  Child := nil;
  Check(ChildDestroyed=true);
  Check(ParentDestroyed=false);
  Check(Parent.HasChild=(aWeakRef=weakref),'ZEROed Weak');
  Parent := nil;
end;
begin
  Init(direct);
  Parent := nil;
  Check(ParentDestroyed=false);
  Check(ChildDestroyed=false);
  Child := nil;
  Check(ParentDestroyed=false,'Without weak reference: memory leak');
  Check(ChildDestroyed=false);
  P._Release;
  Check(ParentDestroyed=true,'Manual release');
  Check(ChildDestroyed=true);
  WeakTest(weakref);
  Init(zeroing);
  Check(ParentDestroyed=false);
  Check(ChildDestroyed=false);
  Child2 := Parent.Child;
  Child2 := nil;
  Check(ChildDestroyed=false);
  Parent := nil;
  Check(ParentDestroyed=false);
  Check(ChildDestroyed=false);
  Child := nil;
  Check(ParentDestroyed=true);
  Check(ChildDestroyed=true);
  WeakTest(zeroing);
  Init(zeroing);
  Check(Parent.HasChild);
  Child2 := TChild.Create(Parent,false);
  Check(Parent.HasChild);
  Parent.Child := Child2;
  Check(Parent.HasChild);
  Child2 := nil;
  Check(not Parent.HasChild);
  Check(ChildDestroyed=true);
  ChildDestroyed := false;
  Check(not Parent.HasChild);
  Child := nil;
  Check(ParentDestroyed=false);
  Check(ChildDestroyed=true);
  Check(not Parent.HasChild);
  ChildDestroyed := false;
  Parent := nil;
  Check(ParentDestroyed=true);
  Check(ChildDestroyed=false);
end;


{ TParent }

destructor TParent.Destroy;
begin
  ParentDestroyed := true;
  if UseWeakRef=weakref then
    SetWeak(@FChild,nil);
  inherited;
end;

function TParent.GetChild: IChild;
begin
  result := FChild;
end;

function TParent.HasChild: boolean;
begin
  result := FChild<>nil;
end;

procedure TParent.SetChild(const Value: IChild);
begin
  case UseWeakRef of
  direct:  FChild := Value;
  weakref: SetWeak(@FChild,Value);
  zeroing: SetWeakZero(self,@FChild,Value);
  end;
end;

{ TChild }

constructor TChild.Create(const AParent: IParent; SetChild: boolean);
begin
  FParent := AParent;
  if SetChild then
    FParent.Child := self;
end;

destructor TChild.Destroy;
begin
  ChildDestroyed := true;
  inherited;
end;

function TChild.GetParent: IParent;
begin
  result := FParent;
end;

procedure TChild.SetParent(const Value: IParent);
begin
  case UseWeakRef of
  direct:  FParent := Value;
  weakref: SetWeak(@FParent,Value);
  zeroing: SetWeakZero(self,@FParent,Value);
  end;
end;

type
  TLoginController = class
  protected
    fUserRepository: IUserRepository;
    fSmsSender: ISmsSender;
  public
    constructor Create(const aUserRepository: IUserRepository;
      const aSmsSender: ISmsSender);
    procedure ForgotMyPassword(const UserName: RawUTF8);
  end;

constructor TLoginController.Create(const aUserRepository: IUserRepository;
  const aSmsSender: ISmsSender);
begin
  fUserRepository := aUserRepository;
  fSmsSender := aSmsSender;
end;

procedure TLoginController.ForgotMyPassword(const UserName: RawUTF8);
var U: TUser;
begin
  U := fUserRepository.GetUserByName(UserName);
  Assert(U.Name=UserName,'internal verification');
  U.Password := Int32ToUtf8(Random(MaxInt));
  U.MobilePhoneNumber := Int32ToUtf8(Random(MaxInt));
  if fSmsSender.Send('Your new password is '+U.Password,U.MobilePhoneNumber) then
    fUserRepository.Save(U);
end;

procedure TTestServiceOrientedArchitecture.IntSubtractJSON(
  Ctxt: TOnInterfaceStubExecuteParamsJSON);
var P: PUTF8Char;
begin
  if Ctxt.Sender is TInterfaceMock then
    Ctxt.TestCase.Check(Ctxt.EventParams='toto');
  P := pointer(Ctxt.Params);
  Ctxt.Returns([GetNextItemDouble(P)-GetNextItemDouble(P)]);
  // Ctxt.Result := '['+DoubleToStr(GetNextItemDouble(P)-GetNextItemDouble(P))+']';
end;

{$ifndef NOVARIANTS}
procedure TTestServiceOrientedArchitecture.IntSubtractVariant(
  Ctxt: TOnInterfaceStubExecuteParamsVariant);
begin
  if Ctxt.Sender is TInterfaceMock then
    Ctxt.TestCase.Check(Ctxt.EventParams='toto');
  Ctxt['result'] := Ctxt['n1']-Ctxt['n2'];
  // with Ctxt do Output[0] := Input[0]-Input[1];
end;

procedure TTestServiceOrientedArchitecture.IntSubtractVariantVoid(
  Ctxt: TOnInterfaceStubExecuteParamsVariant);
begin
end;
{$endif}

procedure TTestServiceOrientedArchitecture.MocksAndStubs;
var I: ICalculator;
    n: integer;
    UserRepository: IUserRepository;
    SmsSender: ISmsSender;
    U: TUser;
    log, UJSON: RawUTF8;
    HashGetUserByNameToto: cardinal;
    Stub: TInterfaceStub;
    Mock: TInterfaceMockSpy;
begin
  Stub := TInterfaceStub.Create(TypeInfo(ICalculator),I).
    SetOptions([imoLogMethodCallsAndResults]);
  Check(I.Add(10,20)=0,'Default result');
  log := Stub.LogAsText;
  Check(log='Add(10,20)=[0]');
  I := nil;
  Stub := TInterfaceStub.Create(TypeInfo(ICalculator),I).
    Returns('Add','30').
    Returns('Multiply',[60]).
    Returns('Multiply',[2,35],[70]).
    ExpectsCount('Multiply',qoEqualTo,2).
    ExpectsCount('Subtract',qoGreaterThan,0).
    ExpectsCount('ToTextFunc',qoLessThan,2).
    ExpectsTrace('Add',Hash32('Add(10,30)=[30]')).
    ExpectsTrace('Multiply','Multiply(10,30)=[60],Multiply(2,35)=[70]').
    ExpectsTrace('Multiply',[10,30],'Multiply(10,30)=[60]').
    ExpectsTrace('Add(10,30)=[30],Multiply(10,30)=[60],'+
      'Multiply(2,35)=[70],Subtract(2.3,1.2)=[0],ToTextFunc(2.3)=["default"]').
    Returns('ToTextFunc',['default']);
  Check(I.Add(10,30)=30);
  Check(I.Multiply(10,30)=60);
  Check(I.Multiply(2,35)=70);
  Check(I.Subtract(2.3,1.2)=0,'Default result');
  Check(I.ToTextFunc(2.3)='default');
  Check(Stub.LogHash=$34FA7AAF);
  I := nil; // release Stub -> will check all expectations
  TInterfaceMock.Create(TypeInfo(ICalculator),I,self).
    Returns('Add','30').
    Fails('Add',[1,2],'expected failure').
    SetOptions([imoMockFailsWillPassTestCase]). // -> Check(true)
    ExpectsCount('Add',qoEqualTo,3).
    ExpectsCount('Add',[10,30],qoNotEqualTo,1).
    Executes('Subtract',IntSubtractJSON,'toto').
    Returns('Multiply',[60]).
    Returns('Multiply',[2,35],[70]).
    Returns('ToTextFunc',[2.3],['two point three']).
    Returns('ToTextFunc',['default']);
  Check(I.ToTextFunc(2.3)='two point three');
  Check(I.ToTextFunc(2.4)='default');
  Check(I.Add(10,30)=30);
  n := Assertions;
  I.Add(1,2); // will launch TInterfaceMock.InternalCheck -> Check(true)
  n := Assertions-n; // tricky code due to Check() inlined Assertions modif.
  Check(n=1,'test should have passed');
  Check(I.Multiply(10,30)=60);
  Check(I.Multiply(2,35)=70);
  for n := 1 to 10000 do
    CheckSame(I.Subtract(n*10.5,n*0.5),n*10,1E-9);
  n := Assertions;
  I := nil; // release TInterfaceMock -> will check all expectations
  n := Assertions-n;
  Check(n=2,'Add count<>3');
  TInterfaceStub.Create(TypeInfo(ISmsSender),SmsSender).
    Returns('Send',[true]);
  U.Name := 'toto';
  UJSON := RecordSaveJSON(U,TypeInfo(TUser));
  HashGetUserByNameToto := Hash32('GetUserByName("toto")=['+UJSON+']');
  Mock := TInterfaceMockSpy.Create(TypeInfo(IUserRepository),UserRepository,self);
  Mock.Returns('GetUserByName','"toto"',UJSON).
       ExpectsCount('GetUserByName',qoEqualTo,1).
       ExpectsCount('GetUserByName',['toto'],qoEqualTo,1).
       ExpectsCount('GetUserByName','"tata"',qoEqualTo,0).
       ExpectsTrace('GetUserByName',['toto'],HashGetUserByNameToto).
       ExpectsTrace('GetUserByName',HashGetUserByNameToto).
       ExpectsCount('Save',qoEqualTo,1);
  with TLoginController.Create(UserRepository,SmsSender) do
  try
    ForgotMyPassword('toto');
  finally
    Free;
  end;
  Mock.Verify('Save');
  Mock.Verify('GetUserByName',['toto'],qoEqualTo,1);
  Mock.Verify('GetUserByName','"toto"',qoNotEqualTo,2);
  Mock.Verify('GetUserByName',['toto'],'['+UJSON+']');
  UserRepository := nil; // will release TInterfaceMock and check Excepts*()
  SmsSender := nil;
  {$ifndef NOVARIANTS}
  TInterfaceStub.Create(IID_ICalculator,I).
     Executes('Subtract',IntSubtractVariantVoid,'titi');
  check(I.Subtract(10,20)=0);
  {$endif}
  TInterfaceStub.Create(IID_ICalculator,I).Returns('Subtract',[10,20],[3]).
     {$ifndef NOVARIANTS}
     Executes('Subtract',IntSubtractVariant,'toto').
     {$endif}
     Fails('Add','expected exception').
     Raises('Add',[1,2],ESynException,'expected exception');
  {$ifndef NOVARIANTS}
  for n := 1 to 10000 do
    CheckSame(I.Subtract(n*10.5,n*0.5),n*10,1E-9);
  {$endif}
  Check(I.Subtract(10,20)=3,'Explicit result');
  {$WARN SYMBOL_PLATFORM OFF}
  {$ifndef KYLIX3}
  {$ifndef FPC}
  if DebugHook<>0 then
  {$endif}
  {$endif}
    exit; // avoid exceptions in IDE
  {$WARN SYMBOL_PLATFORM ON}
  with TSynLog.Family.ExceptionIgnore do begin
    Add(EInterfaceFactoryException);
    Add(ESynException);
  end;
  try
    I.Add(0,0);
    Check(false);
  except
    on E: EInterfaceFactoryException do
      Check(Pos('TInterfaceStub returned error: expected exception',E.Message)>0,E.Message);
  end;
  try
    I.Add(1,2);
    Check(false);
  except
    on E: ESynException do
      Check(E.Message='expected exception',E.Message);
  end;
  with TSynLog.Family.ExceptionIgnore do begin
    Delete(IndexOf(EInterfaceFactoryException));
    Delete(IndexOf(ESynException));
  end;
end;


{$endif DELPHI5OROLDER}

{$ifndef DELPHI5OROLDER}

{ TTestMultiThreadProcess }

type
  TTestMultiThreadProcessThread = class(TSynThread)
  protected
    fTest: TTestMultiThreadProcess;
    fID: integer;
    fEvent: TEvent;
    fIterationCount: integer;
    fProcessFinished: boolean;
    fIDs: TIntegerDynArray;
    procedure Execute; override;
    procedure LaunchProcess;
  public
    constructor Create(aTest: TTestMultiThreadProcess; aID: integer); reintroduce;
    destructor Destroy; override;
  end;

procedure TTestMultiThreadProcess.CleanUp;
begin
  DatabaseClose;
  FreeAndNil(fModel);
  FreeAndNil(fThreads);
end;

constructor TTestMultiThreadProcess.Create(Owner: TSynTests; const Ident: string);
begin
  inherited;
  fMinThreads := 1;
  fMaxThreads := 50;
  fOperationCount := 200;
  fClientPerThread := 1;
end;

function TTestMultiThreadProcess.CreateClient: TSQLRest;
var ClientIP: RawByteString;
begin
  if fClientOnlyServerIP='' then
    ClientIP := '127.0.0.1' else
    ClientIP := fClientOnlyServerIP;
  if fTestClass=TSQLRestServerDB then
    result := fDatabase else
  {$ifdef MSWINDOWS}
  if fTestClass=TSQLRestClientURINamedPipe then
    result := TSQLRestClientURINamedPipe.Create(fModel,'test') else
  {$endif}
  if fTestClass=TSQLRestClientDB then
    result := TSQLRestClientDB.Create(fDatabase) else
  {$ifdef MSWINDOWS}
  if fTestClass=TSQLRestClientURIMessage then begin
    result := TSQLRestClientURIMessage.Create(fModel,'test',
      'Client'+IntToStr(GetCurrentThreadId),1000);
    TSQLRestClientURIMessage(result).DoNotProcessMessages := true;
  end else
  {$endif}
  if fTestClass.InheritsFrom(TSQLHttpClientGeneric) then begin
    result := TSQLHttpClientGenericClass(fTestClass).Create(ClientIP,HTTP_DEFAULTPORT,fModel);
    if fTestClass=TSQLHttpClientWebsockets then
      with (result as TSQLHttpClientWebsockets) do begin
        WebSockets.Settings.SetFullLog;
        WebSocketsUpgrade('wskey');
      end;
  end else
    raise ESynException.CreateUTF8('Invalid fTestClass=%',[fTestClass]);
end;

procedure TTestMultiThreadProcess.CreateThreadPool;
var i: integer;
begin
  fModel := TSQLModel.Create([TSQLRecordPeople]);
  fThreads := TSynObjectList.Create;
  for i := 1 to fMaxThreads do
    fThreads.Add(TTestMultiThreadProcessThread.Create(self,i));
  Check(fThreads.Count=fMaxThreads);
end;

procedure TTestMultiThreadProcess.DatabaseClose;
begin
  if fDatabase=nil then
    exit;
  fHttpServer.Shutdown;
  FreeAndNil(fHttpServer);
  FreeAndNil(fDatabase);
  fTestClass := nil;
end;

const
  TTESTMULTITHREADPROCESS_DBFILENAME = 'testMT.db3';

procedure TTestMultiThreadProcess.Test(aClass: TSQLRestClass;
  aHttp: TSQLHttpServerOptions; aWriteMode: TSQLRestServerAcquireMode);
var n: integer;
    i,j: integer;
    allFinished: boolean;
    Thread: TTestMultiThreadProcessThread;
    {$ifdef MSWINDOWS}
    aMsg: TMsg;
    {$endif}
begin
  if CheckFailed(fTestClass=nil) then
    exit;
  fTestClass := aClass;
  // 1. Prepare a new blank SQLite3 database in high speed mode
  if fClientOnlyServerIP='' then begin
    DeleteFile(TTESTMULTITHREADPROCESS_DBFILENAME);
    if CheckFailed(not FileExists(TTESTMULTITHREADPROCESS_DBFILENAME)) or
       CheckFailed(aClass<>nil) then
      exit;
    fDatabase := TSQLRestServerDB.Create(fModel,TTESTMULTITHREADPROCESS_DBFILENAME);
    fDatabase.AcquireWriteMode := aWriteMode;
    fDatabase.DB.Synchronous := smOff;
    fDatabase.DB.LockingMode := lmExclusive;
    fDatabase.NoAJAXJSON := true;
    fDatabase.CreateMissingTables;
    {$ifdef MSWINDOWS}
    if fTestClass=TSQLRestClientURINamedPipe then
      fDatabase.ExportServerNamedPipe('test') else
    if fTestClass=TSQLRestClientURIMessage then
      fDatabase.ExportServerMessage('test') else
    {$endif}
    if fTestClass.InheritsFrom(TSQLHttpClientGeneric) then begin
      fHttpServer := TSQLHttpServer.Create(HTTP_DEFAULTPORT,[fDataBase],'+',aHttp);
      if aHttp=useBidirSocket then
        fHttpServer.WebSocketsEnable(fDatabase,'wskey').Settings.SetFullLog;
    end;
  end;
  // 2. Perform the tests
  fRunningThreadCount := fMinThreads;
  repeat
    // 2.1. Reset the DB content between loops
    if (fRunningThreadCount>1) and (fDatabase<>nil) then
      fDatabase.DB.Execute('delete from people');
    // 2.2. Launch the background client threads
    fTimer.Start;
    for n := 0 to fRunningThreadCount-1 do begin
      TTestMultiThreadProcessThread(fThreads[n]).LaunchProcess;
      sleep(10); // ensure thread process is actually started
    end;
    // 2.3. Wait for the background client threads process to be finished
    repeat
      {$ifdef MSWINDOWS}
      if (fTestClass=TSQLRestClientURIMessage) or
         (fClientOnlyServerIP<>'') then
        while PeekMessage(aMsg,0,0,0,PM_REMOVE) do begin
          TranslateMessage(aMsg);
          DispatchMessage(aMsg);
        end;
      {$endif}
      {$ifndef LVCL}
      if (fDatabase<>nil) and (fDatabase.AcquireWriteMode=amMainThread) then
        CheckSynchronize{$ifndef DELPHI6OROLDER}(1){$endif};
      {$endif}
      SleepHiRes(0);
      allFinished := true;
      for n := 0 to fRunningThreadCount-1 do
        if not TTestMultiThreadProcessThread(fThreads.List[n]).fProcessFinished then begin
          allFinished := false;
          break;
        end;
    until allFinished;
    fTimer.Stop;
    fRunConsole := Format('%s%d=%d/s  ',[fRunConsole,
      fRunningThreadCount,fTimer.PerSec(fOperationCount*2)]);
    // 2.4. Check INSERTed IDs consistency
    for n := 0 to fRunningThreadCount-1 do
      with TTestMultiThreadProcessThread(fThreads.List[n]) do
      for i := 0 to fRunningThreadCount-1 do
        if i<>n then begin
          Thread := fThreads.List[i];
          for j := 0 to high(fIDs) do
            if fIDs[j]>0 then
            if IntegerScanExists(pointer(Thread.fIDs),Thread.fIterationCount,fIDs[j]) then
              Check(false,format('Duplicate ID %d for thread %d and %d',[fIDs[j],i,n]));
        end;
    // 2.5. Execution sequence is with 1,2,5,10,30,50 concurent threads
    if fRunningThreadCount=1 then
      fRunningThreadCount := 2 else
    if fRunningThreadCount=2 then
       fRunningThreadCount := 5 else
    if fRunningThreadCount=5 then
      {$ifdef MSWINDOWS}
      if fTestClass=TSQLRestClientURINamedPipe then
        break else
      {$endif}
      {$ifdef CPUARM3264}
      if fTestClass=TSQLHttpClientWebsockets then
        break else
      {$endif CPUARM3264}
        fRunningThreadCount := 10 else
      {$ifdef MSWINDOWS}
      if fTestClass=TSQLRestClientURIMessage then
        break else
      {$endif}
        fRunningThreadCount := fRunningThreadCount+20;
  until fRunningThreadCount>fMaxThreads;
  // 3. Cleanup for this protocol (but reuse the same threadpool)
  DatabaseClose;
  Check(fDatabase=nil);
end;

procedure TTestMultiThreadProcess.Locked;
begin // 1=7310/s  2=8689/s  5=7693/s  10=3893/s  30=1295/s  50=777/s
  // (numbers are taken from a Xeon Phi 2 @ 1.5GHz with 288 cores)
  Test(TSQLRestClientDB,HTTP_DEFAULT_MODE,amLocked);
end;

procedure TTestMultiThreadProcess.Unlocked;
begin // 1=7342/s  2=9400/s  5=7693/s  10=3894/s  30=1295/s  50=777/s
  Test(TSQLRestClientDB,HTTP_DEFAULT_MODE,amUnlocked);
end;

procedure TTestMultiThreadProcess.BackgroundThread;
begin // 1=6173/s  2=7299/s  5=7244/s  10=3912/s  30=1301/s  50=777/s
  Test(TSQLRestClientDB,HTTP_DEFAULT_MODE,amBackgroundThread);
end;

{$ifndef LVCL}
procedure TTestMultiThreadProcess.MainThread;
begin // 1=5000/s  2=5911/s  5=4260/s  10=2663/s  30=1126/s  50=707/s
  Test(TSQLRestClientDB,HTTP_DEFAULT_MODE,amMainThread);
end;
{$endif}

{$ifndef ONLYUSEHTTPSOCKET}
procedure TTestMultiThreadProcess.WindowsAPI;
begin
  {$ifdef USEWININET}
  Test(TSQLHttpClientWinHTTP,useHttpApi);
  {$endif}
end;
{$endif}

procedure TTestMultiThreadProcess.SocketAPI;
begin //  1=2470/s  2=3866/s  5=3608/s  10=3556/s  30=1303/s  50=780/s
  Test(TSQLHttpClientWinSock,useHttpSocket);
end;

procedure TTestMultiThreadProcess.Websockets;
begin // 1=2433/s  2=3389/s  5=3208/s  10=3354/s  30=1303/s  50=778/s
  Test(TSQLHttpClientWebsockets,useBidirSocket);
end;

{$ifdef USELIBCURL}
procedure TTestMultiThreadProcess._libcurl;
begin //  1=48/s  2=95/s  5=234/s  10=433/s  30=729/s  50=594/s
  Test(TSQLHttpClientCurl,useHttpSocket);
end;
{$endif}

procedure TTestMultiThreadProcess._TSQLRestClientDB;
begin //  1=7347/s  2=8100/s  5=7654/s  10=3898/s  30=1295/s  50=777/s
  Test(TSQLRestClientDB);
end;

{$ifdef MSWINDOWS}
procedure TTestMultiThreadProcess._TSQLRestClientURIMessage;
begin
  Test(TSQLRestClientURIMessage);
end;

procedure TTestMultiThreadProcess._TSQLRestClientURINamedPipe;
begin
  Test(TSQLRestClientURINamedPipe);
end;
{$endif}

procedure TTestMultiThreadProcess._TSQLRestServerDB;
begin //  1=9332/s  2=9300/s  5=7826/s  10=3891/s  30=1295/s  50=777/s
  Test(TSQLRestServerDB);
end;


{ TTestMultiThreadProcessThread }

constructor TTestMultiThreadProcessThread.Create(aTest: TTestMultiThreadProcess; aID: integer);
begin
  FreeOnTerminate := false;
  fEvent := TEvent.Create(nil,false,false,'');
  fTest := aTest;
  fID := aID;
  SetLength(fIDs,fTest.fOperationCount);
  inherited Create(False);
end;

destructor TTestMultiThreadProcessThread.Destroy;
begin
  fProcessFinished := true;
  fEvent.SetEvent; // notify terminate
  Sleep(0); // is expected for proper process
  inherited Destroy;
  FreeAndNil(fEvent);
end;

procedure TTestMultiThreadProcessThread.Execute;
var Rest: array of TSQLRest;
    Rec: TSQLRecordPeople;
    i,n,r: integer;
begin
  SetCurrentThreadName('% #%',[self,fID]);
  Rec := TSQLRecordPeople.Create;
  try
    Rec.LastName := 'Thread '+CardinalToHex(PtrUInt(GetCurrentThreadId));
    while not Terminated do
    case FixedWaitFor(fEvent,INFINITE) of
      wrSignaled:
        if Terminated or fProcessFinished then // from Destroy
          break else
          try
            try
              SetLength(Rest,fTest.ClientPerThread);
              for i := 0 to high(Rest) do
                Rest[i] := fTest.CreateClient;
              if not fTest.CheckFailed(Rest<>nil) then begin
                n := 0;
                r := 0;
                for i := 0 to fIterationCount-1 do begin
                  Rec.FirstName := FormatUTF8('%/%',[i,fIterationCount-1]);
                  Rec.YearOfBirth := 1000+i;
                  Rec.YearOfDeath := 1040+i;
                  fIDs[i] := Rest[r].Add(Rec,true);
                  if r=high(Rest) then
                    r := 0 else
                    inc(r);
                  if fTest.CheckFailed(fIDs[i]<>0,'Rest.Add') then
                    break;
                  inc(n);
                end;
                for i := 0 to n-1 do
                  if fTest.CheckFailed(Rest[r].Retrieve(fIDs[i],Rec)) then
                    break else begin
                    fTest.Check(Rec.YearOfBirth=1000+i);
                    fTest.Check(Rec.YearOfDeath=1040+i);
                    //if (Rec.YearOfBirth<>1000+i) or (Rec.YearOfDeath<>1040+i) then writeln(i,'  ',ObjectToJSON(Rec));
                    if r=high(Rest) then
                      r := 0 else
                      inc(r);
                  end;
                end;
            finally
              for i := 0 to high(Rest) do
              if Rest[i]<>fTest.fDatabase then
                FreeAndNil(Rest[i]);
              fProcessFinished := true;
            end;
          except
            on E: Exception do
              fTest.Check(False,E.Message);
          end;
    end;
  finally
    Rec.Free;
  end;
  fProcessFinished := true;
end;

procedure TTestMultiThreadProcessThread.LaunchProcess;
begin
  fProcessFinished := false;
  fIterationCount := fTest.fOperationCount div fTest.fRunningThreadCount;
  fEvent.SetEvent;
  Sleep(0); // is expected for proper process
end;


{ TTestBidirectionalRemoteConnection }

procedure TTestBidirectionalRemoteConnection.WebsocketsJSONProtocol;
begin
  WebsocketsLowLevel(TWebSocketProtocolJSON.Create(''),focText);
end;

procedure TTestBidirectionalRemoteConnection.WebsocketsBinaryProtocol;
begin
  WebsocketsLowLevel(TWebSocketProtocolBinary.Create('',false,'',false),focBinary);
end;
procedure TTestBidirectionalRemoteConnection.WebsocketsBinaryProtocolEncrypted;
begin
  WebsocketsLowLevel(TWebSocketProtocolBinary.Create('',false,'pass',false),focBinary);
end;

procedure TTestBidirectionalRemoteConnection.WebsocketsBinaryProtocolCompressed;
begin
  WebsocketsLowLevel(TWebSocketProtocolBinary.Create('',false,'',true),focBinary);
end;

procedure TTestBidirectionalRemoteConnection.WebsocketsBinaryProtocolCompressEncrypted;
begin
  WebsocketsLowLevel(TWebSocketProtocolBinary.Create('',false,'pass',true),focBinary);
end;

type // to access protected low-level frame methods
  TWebSocketProtocolRestHook = class(TWebSocketProtocolRest);

procedure TTestBidirectionalRemoteConnection.WebsocketsLowLevel(
  protocol: TWebSocketProtocol; opcode: TWebSocketFrameOpCode);
procedure TestOne(const content,contentType: RawByteString);
var C1,C2: THttpServerRequest;
    P2: TWebSocketProtocol;
    frame: TWebSocketFrame;
    head: RawUTF8;
    noAnswer1,noAnswer2: boolean;
begin
  C1 := THttpServerRequest.Create(nil,0,nil);
  C2 := THttpServerRequest.Create(nil,0,nil);
  P2 := protocol.Clone('');
  try
    C1.Prepare('url','POST','headers',content,contentType,'',false);
    noAnswer1 := opcode=focBinary;
    noAnswer2 := not noAnswer1;
    TWebSocketProtocolRestHook(protocol).InputToFrame(C1,noAnswer1,frame,head);
    Check(frame.opcode=opcode);
    TWebSocketProtocolRestHook(P2).FrameToInput(frame,noAnswer2,C2);
    Check(noAnswer1=noAnswer2);
    Check(C2.URL='url');
    Check(C2.Method='POST');
    Check(C2.InHeaders='headers');
    Check(C2.InContentType=contentType);
    Check(C2.InContent=content);
    C1.OutContent := content;
    C1.OutContentType := contentType;
    C1.OutCustomHeaders := 'outheaders';
    frame.opcode := focContinuation;
    head := 'answer';
    TWebSocketProtocolRestHook(protocol).OutputToFrame(C1,200,head,frame);
    Check(frame.opcode=opcode);
    Check(TWebSocketProtocolRestHook(P2).FrameToOutput(frame,C2)=200);
    Check(C2.OutContent=content);
    Check(C2.OutContentType=contentType);
    Check(C2.OutCustomHeaders='outheaders');
  finally
    P2.Free;
    C2.Free;
    C1.Free;
  end;
end;
begin
  try
    TestOne('content',TEXT_CONTENT_TYPE);
    TestOne('{"content":1234}',JSON_CONTENT_TYPE);
    TestOne('"content"',JSON_CONTENT_TYPE);
    TestOne('["json",2]',JSON_CONTENT_TYPE);
    TestOne('binary'#0'data',BINARY_CONTENT_TYPE);
  finally
    protocol.Free;
  end;
end;

type
  TBidirCallbackInterfacedObject = class(TInterfacedObject,IBidirCallback)
  protected
    fValue: Integer;
  public
    function Value: Integer;
    procedure AsynchEvent(a: integer);
  end;
  TBidirCallback = class(TInterfacedCallback,IBidirCallback)
  protected
    fValue: Integer;
  public
    function Value: Integer;
    procedure AsynchEvent(a: integer);
  end;

function TBidirServer.TestRest(a,b: integer; out c: RawUTF8): variant;
begin
  c := Int32ToUtf8(a+b);
  result := _ObjFast(['a',a,'b',b,'c',c]);
end;

function TBidirServer.TestRestCustom(a: integer): TServiceCustomAnswer;
begin
  result.Header := BINARY_CONTENT_TYPE_HEADER;
  result.Content := Int32ToUtf8(a)+#0#1;
  result.Status := HTTP_SUCCESS;
end;

function TBidirServer.TestCallback(d: Integer; const callback: IBidirCallback): boolean;
begin
  fCallback := callback;
  result := d<>0;
end;

procedure TBidirServer.LaunchAsynchCallback(a: integer);
begin
  if Assigned(fCallback) then
    fCallback.AsynchEvent(a);
end;

function TBidirServer.LaunchSynchCallback: integer;
begin
  if Assigned(fCallback) then
    result := fCallback.Value else
    result := 0;
end;

procedure TBidirServer.RemoveCallback;
begin
  fCallback := nil;
end;

procedure TBidirCallbackInterfacedObject.AsynchEvent(a: integer);
begin
  inc(fValue,a);
end;

function TBidirCallbackInterfacedObject.Value: integer;
begin
  result := fValue;
end;

procedure TBidirCallback.AsynchEvent(a: integer);
begin
  inc(fValue,a);
end;

function TBidirCallback.Value: integer;
begin
  result := fValue;
end;

const
  WEBSOCKETS_KEY = 'key';

procedure TTestBidirectionalRemoteConnection.RunHttpServer;
var
  port: integer;
begin
  TInterfaceFactory.RegisterInterfaces([TypeInfo(IBidirService),TypeInfo(IBidirCallback)]);
  // sicClientDriven services expect authentication for sessions
  fServer := TSQLRestServerFullMemory.CreateWithOwnModel([],true);
  fServer.CreateMissingTables;
  fBidirServer := TBidirServer.Create;
  Check(fServer.ServiceDefine(fBidirServer,[IBidirService])<>nil);
  fHttpServer := TSQLHttpServer.Create(HTTP_DEFAULTPORT,[],'+',useBidirSocket);
  Check(fHttpServer.AddServer(fServer));
  fHttpServer.WebSocketsEnable(fServer,WEBSOCKETS_KEY,true).Settings.SetFullLog;
  //(fHttpServer.HttpServer as TWebSocketServer).HeartbeatDelay := 5000;
  port := UTF8ToInteger(HTTP_DEFAULTPORT);
  fPublicRelayClientsPort := ToUTF8(port+1);
  fPublicRelayPort := ToUTF8(port+2);
end;

procedure TTestBidirectionalRemoteConnection.TestRest(Rest: TSQLRest);
var I: IBidirService;
    a,b: integer;
    c: RawUTF8;
    v: variant;
    res: TServiceCustomAnswer;
begin
  Rest.Services.Resolve(IBidirService,I);
  if CheckFailed(Assigned(I), 'Rest IBidirService') then
    exit;
  for a := -10 to 10 do
    for b := -10 to 10 do begin
      v := I.TestRest(a,b,c);
      check(GetInteger(pointer(c))=a+b);
      if CheckFailed(DocVariantType.IsOfType(v)) then
        continue;
      check(v.a=a);
      check(v.b=b);
      check(v.c=c);
    end;
  for a := -10 to 10 do begin
    res := I.TestRestCustom(a);
    check(res.Status=HTTP_SUCCESS);
    check(GetInteger(pointer(res.Content))=a);
    check(res.Content[Length(res.Content)]=#1);
  end;
end;

procedure TTestBidirectionalRemoteConnection.TestCallback(Rest: TSQLRest);
var I: IBidirService;
    d: integer;
    subscribed: IBidirCallback;
procedure WaitUntilNotified;
var timeout: Int64;
begin
  timeout := GetTickCount64+5000;
  while (subscribed.value<>6) and (GetTickCount64<timeout) do sleep(1);
  Check(subscribed.value=6);
end;
begin
  Rest.Services.Resolve(IBidirService,I);
  if CheckFailed(Assigned(I), 'Callback IBidirService') then
    exit;
  subscribed := TBidirCallbackInterfacedObject.Create;
  for d := -5 to 6 do begin
    check(I.TestCallback(d,subscribed)=(d<>0));
    I.LaunchAsynchCallback(d);
  end;
  WaitUntilNotified;
  check(fBidirServer.LaunchSynchCallback=6);
  Rest.Services.CallBackUnRegister(subscribed); // manual callback release notify
  subscribed := TBidirCallback.Create(Rest,IBidirCallback); // auto notification
  for d := -5 to 6 do begin
    check(I.TestCallback(d,subscribed)=(d<>0));
    I.LaunchAsynchCallback(d);
  end;
  WaitUntilNotified;
  subscribed := TBidirCallback.Create(Rest,IBidirCallback);
  for d := -5 to 6 do begin
    check(I.TestCallback(d,subscribed)=(d<>0));
    I.LaunchAsynchCallback(d);
    I.RemoveCallback;
  end;
  WaitUntilNotified;
  check(fBidirServer.LaunchSynchCallback=0);
end; // here TBidirCallback.Free will notify Rest.Services.CallBackUnRegister()

procedure TTestBidirectionalRemoteConnection.SOACallbackOnServerSide;
begin
  TestRest(fServer);
  TestCallback(fServer);
  TestRest(fServer);
end;

function TTestBidirectionalRemoteConnection.NewClient(const port: SockString): TSQLHttpClientWebsockets;
begin
  result := TSQLHttpClientWebsockets.Create('127.0.0.1',port,TSQLModel.Create(fServer.Model));
  result.Model.Owner := result;
  result.WebSockets.Settings.SetFullLog;
end;

procedure TTestBidirectionalRemoteConnection.SOACallbackViaWebsockets(
  Ajax, Relay: boolean);
  procedure ServiceDefine(c: TSQLHttpClientWebsockets; const msg: string);
  begin
    Check(c.SetUser('User','synopse'),'setuser'+msg);
    Check(c.ServiceDefine(IBidirService,sicShared)<>nil,'IBidirService'+msg);
  end;
var c1, c2: TSQLHttpClientWebsockets;
    port: SockString;
    stats: RawUTF8;
begin
  if Relay then
    port := fPublicRelayClientsPort else
    port := HTTP_DEFAULTPORT;
  c1 := NewClient(port);
  try
    // check plain HTTP REST calls
    Check(c1.ServerTimestampSynchronize);
    ServiceDefine(c1,'1');
    TestRest(c1);
    // check WebSockets communication
    CheckEqual(c1.WebSocketsUpgrade(WEBSOCKETS_KEY,Ajax,true), '', 'WebSocketsUpgrade1');
    TestCallback(c1);
    c2 := NewClient(port);
    try
      CheckEqual(c2.WebSocketsUpgrade(WEBSOCKETS_KEY,Ajax,true), '', 'WebSocketsUpgrade2');
      ServiceDefine(c2,'2');
      TestCallback(c2);
      if Relay then begin
        stats := HttpGet('127.0.0.1',fPublicRelayPort,'/stats','');
        check(PosEx('"version"', stats)>0,'stats');
      end;
      TestRest(c1);
      TestRest(c2);
    finally
      c2.Free;
    end;
  finally
    c1.Free;
  end;
end;

procedure TTestBidirectionalRemoteConnection.SOACallbackViaJSONWebsockets;
begin
  SOACallbackViaWebsockets({ajax=}true,{relay=}false);
end;

procedure TTestBidirectionalRemoteConnection.SOACallbackViaBinaryWebsockets;
begin
  SOACallbackViaWebsockets({ajax=}false,{relay=}false);
end;

procedure TTestBidirectionalRemoteConnection.RelayStart;
const
  RELAYKEY = 'aes256secret';
var
  stats: RawUTF8;
begin
  fPublicRelay := TPublicRelay.Create(nil, fPublicRelayClientsPort,
    fPublicRelayPort, RELAYKEY, TJWTHS256.Create('jwtsecret', 100, [], []));
  fPrivateRelay := TPrivateRelay.Create(nil, '127.0.0.1',fPublicRelayPort,
    RELAYKEY, fPublicRelay.ServerJWT.Compute([]), '127.0.0.1',
    HTTP_DEFAULTPORT, 'X-Real-IP');
  check(not fPrivateRelay.Connected);
  check(fPrivateRelay.TryConnect);
  checkEqual(HttpGet('127.0.0.1',fPublicRelayPort,'/invalid',''), '', 'wrong URI');
  stats := HttpGet('127.0.0.1',fPublicRelayPort,'/stats','');
  check(PosEx('version', stats)>0,'stats');
end;

procedure TTestBidirectionalRemoteConnection.RelaySOACallbackViaJSONWebsockets;
begin
  SOACallbackViaWebsockets({ajax=}true,{relay=}true);
end;

procedure TTestBidirectionalRemoteConnection.RelayConnectionRecreate;
begin
  check(fPrivateRelay.TryConnect);
end;

procedure TTestBidirectionalRemoteConnection.RelaySOACallbackViaBinaryWebsockets;
begin
  SOACallbackViaWebsockets({ajax=}false,{relay=}true);
end;

procedure TTestBidirectionalRemoteConnection.RelayShutdown;
var
  stats: RaWUTF8;
begin
  stats := HttpGet('127.0.0.1',fPublicRelayPort,'/stats','');
  check(PosEx('"version"', stats)>0,'stats');
  fPrivateRelay.Free;
  sleep(100);
  stats := HttpGet('127.0.0.1',fPublicRelayPort,'/stats','');
  check(PosEx('"version"', stats)>0,'stats');
  fPublicRelay.Free;
end;

procedure TTestBidirectionalRemoteConnection._TRecordVersion;
begin
  TestMasterSlaveRecordVersion(Self,'ws.db3');
end;

procedure TTestBidirectionalRemoteConnection.CleanUp;
begin
  FreeAndNil(fHttpServer);
  FreeAndNil(fServer);
end;


{ TTestDDDSharedUnits }

procedure TTestDDDSharedUnits.AuthenticationModel;
begin
  TDDDAuthenticationSHA256.RegressionTests(self);
  TDDDAuthenticationMD5.RegressionTests(self);
end;

procedure TTestDDDSharedUnits.EmailValidationProcess;
begin
  TestDddInfraEmailer(TSQLRestServerDB,self);
end;

procedure TTestDDDSharedUnits.UserModel;
begin
  TCountry.RegressionTests(self);
  TPersonContactable.RegressionTests(self);
end;

procedure TTestDDDSharedUnits.UserCQRSRepository;
begin
  TInfraRepoUserFactory.RegressionTests(self);
end;

type
  // The infratructure REST class implementing the Query and Command Interfaces for TTest
  TDDDThreadsTestRest = class(TDDDRepositoryRestCommand, IDDDThreadsCommand)
  public
    function SelectByDescription(const aDescription: RawUTF8): TCQRSResult;
    function SelectAll: TCQRSResult;
    function Get(out aAggregate: TDDDTest): TCQRSResult;
    function GetAll(out aAggregates: TDDDTestObjArray): TCQRSResult;
    function GetNext(out aAggregate: TDDDTest): TCQRSResult;
    function Add(const aAggregate: TDDDTest): TCQRSResult;
    function Update(const aUpdatedAggregate: TDDDTest): TCQRSResult;
  end;

  // REST Factory for TDDDThreadsTestRest instances
  TDDDThreadsTestRestFactory = class(TDDDRepositoryRestFactory)
  public
    constructor Create(aRest: TSQLRest; aOwner: TDDDRepositoryRestManager = nil); reintroduce;
  end;

  // Custom TSQLHttpClient encapsulating the remote IDDDThreadsCommand interface.
  TDDDThreadsHttpClient = class(TSQLHttpClient)
  private
    // Internal Model
    fModel: TSQLModel;
    // IDDDThreadsCommand interface. Will be assigned inside SetUser
    fMyCommand: IDDDThreadsCommand;
  public
    constructor Create(const aServer, aPort: AnsiString); reintroduce;
    destructor Destroy; override;
    function SetUser(const aUserName, aPassword: RawUTF8; aHashedPassword: Boolean = false): boolean; reintroduce;
    property MyCommand: IDDDThreadsCommand read fMyCommand;
  end;

  // The thread used by TTestDDDMultiThread.ClientTest
  TDDDThreadsThread = class(TSynThread)
  private
    fHttpClient: TDDDThreadsHttpClient;
    fRequestCount: integer;
    fId: integer;
    fIsError: boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const aId, aRequestCount: integer); reintroduce;
    destructor Destroy; override;
    property IsError: boolean read fIsError;
  end;

{ TDDDThreadsTestRest }

function TDDDThreadsTestRest.SelectByDescription(const aDescription: RawUTF8): TCQRSResult;
begin
  result := ORMSelectOne('Description=?', [aDescription], (aDescription = ''));
end;

function TDDDThreadsTestRest.SelectAll: TCQRSResult;
begin
  result := ORMSelectAll('', []);
end;

function TDDDThreadsTestRest.Get(out aAggregate: TDDDTest): TCQRSResult;
begin
  result := ORMGetAggregate(aAggregate);
end;

function TDDDThreadsTestRest.GetAll(out aAggregates: TDDDTestObjArray): TCQRSResult;
begin
  result := ORMGetAllAggregates(aAggregates);
end;

function TDDDThreadsTestRest.GetNext(out aAggregate: TDDDTest): TCQRSResult;
begin
  result := ORMGetNextAggregate(aAggregate);
end;

function TDDDThreadsTestRest.Add(const aAggregate: TDDDTest): TCQRSResult;
begin
  result := ORMAdd(aAggregate);
end;

function TDDDThreadsTestRest.Update(const aUpdatedAggregate: TDDDTest): TCQRSResult;
begin
  result := ORMUpdate(aUpdatedAggregate);
end;


{ TInfraRepoUserFactory }

constructor TDDDThreadsTestRestFactory.Create(aRest: TSQLRest; aOwner: TDDDRepositoryRestManager);
begin
  inherited Create(IDDDThreadsCommand, TDDDThreadsTestRest, TDDDTest, aRest, TSQLRecordDDDTest, aOwner);
end;


{ TTestDDDMultiThread }

procedure TTestDDDMultiThread.CleanUp;
begin
  if Assigned(fHttpServer) then
    FreeAndNil(fHttpServer);
  if Assigned(fRestServer) then
    FreeAndNil(fRestServer);
end;

procedure TTestDDDMultiThread.DeleteOldDatabase;
begin
  if FileExists(ChangeFileExt(ParamStr(0), '.db3')) then
    SysUtils.DeleteFile(ChangeFileExt(ParamStr(0), '.db3'));
  CheckNot(FileExists(ChangeFileExt(ParamStr(0), '.db3')));
end;

procedure TTestDDDMultiThread.StartServer;
begin
  fRestServer := TSQLRestServerDB.CreateWithOwnModel([TSQLRecordDDDTest], ChangeFileExt(ParamStr(0), '.db3'), true);
  with fRestServer do begin
    DB.Synchronous := smNormal;
    DB.LockingMode := lmExclusive;
    CreateMissingTables();
    TInterfaceFactory.RegisterInterfaces([TypeInfo(IDDDThreadsQuery), TypeInfo(IDDDThreadsCommand)]);
    ServiceContainer.InjectResolver([TDDDThreadsTestRestFactory.Create(fRestServer)], true);
    ServiceDefine(TDDDThreadsTestRest, [IDDDThreadsCommand], sicClientDriven);
  end;
  fHttpServer := TSQLHttpServer.Create(HTTP_DEFAULTPORT, fRestServer, '+',
    {$ifdef ONLYUSEHTTPSOCKET}useHttpSocket{$else}useHttpApiRegisteringURI{$endif});
  Check(fHttpServer.DBServerCount>0);
end;

procedure TTestDDDMultiThread.MultiThreadedClientsTest;
begin
  ClientTest(20, 50);
end;

procedure TTestDDDMultiThread.SingleClientTest;
var
  HttpClient: TDDDThreadsHttpClient;
  test: TDDDTest;
  i: integer;
const
  MAX = 1000;
begin
  HttpClient := TDDDThreadsHttpClient.Create('127.0.0.1', HTTP_DEFAULTPORT);
  try
    Check(HttpClient.SetUser('Admin', 'synopse'));
    test := TDDDTest.Create;
    try
      for i := 0 to MAX - 1 do begin
        test.Description := FormatUTF8('test-%', [i]);
        Check(HttpClient.MyCommand.Add(test) = cqrsSuccess);
      end;
      Check(HttpClient.MyCommand.Commit = cqrsSuccess);
    finally
      test.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

function TTestDDDMultiThread.ClientTest(const aClients, aRequests: integer): boolean;
var
  i,count: integer;
  arrThreads: array of TDDDThreadsThread;
  {$ifdef MSWINDOWS}
  arrHandles: array of THandle;
  {$endif}
  rWait: Cardinal;
begin
  result := false;
  count := fRestServer.TableRowCount(TSQLRecordDDDTest);
  SetLength(arrThreads, aClients);
  {$ifdef MSWINDOWS}
  SetLength(arrHandles, aClients);
  {$endif}
  for i := Low(arrThreads) to High(arrThreads) do begin
    arrThreads[i] := TDDDThreadsThread.Create(i, aRequests);
    {$ifdef MSWINDOWS}
    arrHandles[i] := arrThreads[i].Handle;
    {$endif}
    arrThreads[i].Start;
  end;
  try
    {$ifdef MSWINDOWS}
    repeat
      rWait := WaitForMultipleObjects(aClients, @arrHandles[0], True, INFINITE);
    until rWait <> WAIT_TIMEOUT;
    {$else}
    repeat
      Sleep(10);
      rWait := 0;
      for i := Low(arrThreads) to High(arrThreads) do
        if not arrThreads[i].Terminated then
          inc(rWait);
    until rWait=0;
    {$endif}
  finally
    for i := Low(arrThreads) to High(arrThreads) do begin
      CheckNot(arrThreads[i].IsError);
      arrThreads[i].Free;
    end;
    Check(fRestServer.TableRowCount(TSQLRecordDDDTest)=count+aClients*aRequests);
  end;
end;

{ TDDDThreadsHttpClient }

constructor TDDDThreadsHttpClient.Create(const aServer, aPort: AnsiString);
begin
  fModel := TSQLModel.Create([TSQLRecordDDDTest]);
  fModel.Owner := self;
  inherited Create(aServer, aPort, fModel);
end;

destructor TDDDThreadsHttpClient.Destroy;
begin
  fMyCommand := nil;
  inherited;
end;

function TDDDThreadsHttpClient.SetUser(const aUserName, aPassword: RawUTF8; aHashedPassword: Boolean = false): boolean;
begin
  result := inherited SetUser(aUserName, aPassword, aHashedPassword);
  if result then begin
    ServiceDefine([IDDDThreadsCommand], sicClientDriven);
    Services.Resolve(IDDDThreadsCommand, fMyCommand);
  end;
end;


{ TDDDThreadsThread }

constructor TDDDThreadsThread.Create(const aID, aRequestCount: integer);
begin
  inherited Create(true);
  fRequestCount := aRequestCount;
  fId := aId;
  fIsError := false;
  fHttpClient := TDDDThreadsHttpClient.Create('127.0.0.1', HTTP_DEFAULTPORT);
end;

destructor TDDDThreadsThread.Destroy;
begin
  fHttpClient.Free;
  inherited;
end;

procedure TDDDThreadsThread.Execute;
var
  i: integer;
  test: TDDDTest;
  success: boolean;
begin
  fHttpClient.SetUser('Admin', 'synopse');
  for i := 1 to 150 {15000} do
    fHttpClient.ServerTimestampSynchronize; // calls root/timestamp
  test := TDDDTest.Create;
  try
    success := true;
    i := fRequestCount; // circumvent weird FPC bug on ARM
    while i>0 do begin
      test.Description := FormatUTF8('test-%-%', [fID, i]);
      success := success and (fHttpClient.MyCommand.Add(test) = cqrsSuccess);
      if not success then
        break;
      dec(i);
    end;
    if success then
      success := fHttpClient.MyCommand.Commit = cqrsSuccess;
    if not success then begin
      fIsError := true;
      raise Exception.Create('Something went wrong!');
    end;
  finally
    test.Free;
    Terminate;
  end;
end;


{$endif DELPHI5OROLDER}

{ TTestProtocols }

procedure TTestProtocols.RTSPOverHTTP;
var proxy: TRTSPOverHTTPServer;
begin
  proxy := TRTSPOverHTTPServer.Create('127.0.0.1','3999','3998',TSynLog,nil,nil);
  try
    proxy.RegressionTests(self,{$ifdef Darwin}10{$else}100{$endif},10);
  finally
    proxy.Free;
  end;
end;


initialization
  {$ifndef LVCL}
  {$ifdef ISDELPHIXE}FormatSettings.{$endif}{$ifdef FPC}FormatSettings.{$endif}
    DecimalSeparator := '.';
  {$endif LVCL}
  _uE0 := WinAnsiToUtf8(@UTF8_E0_F4_BYTES[0],1);
  _uE7 := WinAnsiToUtf8(@UTF8_E0_F4_BYTES[1],1);
  _uE8 := WinAnsiToUtf8(@UTF8_E0_F4_BYTES[2],1);
  _uE9 := WinAnsiToUtf8(@UTF8_E0_F4_BYTES[3],1);
  _uEA := WinAnsiToUtf8(@UTF8_E0_F4_BYTES[4],1);
  _uF4 := WinAnsiToUtf8(@UTF8_E0_F4_BYTES[5],1);
end.

