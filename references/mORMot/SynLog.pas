/// logging functions used by Synopse projects
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynLog;

(*
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

*)


{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
{$ifdef MSWINDOWS}
  Windows,
  Messages,
{$endif}
{$ifdef KYLIX3}
  Types,
  LibC,
  SynKylix,
{$endif}
  Classes,
{$ifndef LVCL}
  SyncObjs, // for TEvent
  Contnrs,  // for TObjectList
  {$ifdef HASINLINENOTX86}
  Types,
  {$endif}
{$endif}
{$ifndef NOVARIANTS}
  Variants,
{$endif}
  SysUtils,
  SynLZ, // needed e.g. for TSynMapFile .mab format
  SynCommons,
  SynTable;


{ ************ Logging classes and functions }

type
  /// a debugger symbol, as decoded by TSynMapFile from a .map file
  TSynMapSymbol = packed record
    /// symbol internal name
    Name: RawUTF8;
    /// starting offset of this symbol in the executable
    // - addresses are integer, since map be <0 in Kylix .map files
    Start: integer;
    /// end offset of this symbol in the executable
    // - addresses are integer, since map be <0 in Kylix .map files
    Stop: integer;
  end;
  PSynMapSymbol = ^TSynMapSymbol;
  /// a dynamic array of symbols, as decoded by TSynMapFile from a .map file
  TSynMapSymbolDynArray = array of TSynMapSymbol;

  /// a debugger unit, as decoded by TSynMapFile from a .map file
  TSynMapUnit = packed record
    /// Name, Start and Stop of this Unit
    Symbol: TSynMapSymbol;
    /// associated source file name
    FileName: RawUTF8;
    /// list of all mapped source code lines of this unit
    Line: TIntegerDynArray;
    /// start code address of each source code line
    Addr: TIntegerDynArray;
  end;
  /// a dynamic array of units, as decoded by TSynMapFile from a .map file
  TSynMapUnitDynArray = array of TSynMapUnit;

  {$M+}
  /// retrieve a .map file content, to be used e.g. with TSynLog to provide
  // additional debugging information
  // - original .map content can be saved as .mab file in a more optimized format
  TSynMapFile = class
  protected
    fMapFile: TFileName;
    fSymbol: TSynMapSymbolDynArray;
    fUnit: TSynMapUnitDynArray;
    fSymbols: TDynArray;
    fUnits: TDynArrayHashed;
    fSymCount, fUnitCount, fUnitSynLogIndex,fUnitSystemIndex: integer;
    fCodeOffset: PtrUInt;
    fHasDebugInfo: boolean;
  public
    /// get the available debugging information
    // - if aExeName is specified, will use it in its search for .map/.mab
    // - if aExeName is not specified, will use the currently running .exe/.dll
    // - it will first search for a .map matching the file name: if found,
    // will be read to retrieve all necessary debugging information - a .mab
    // file will be also created in the same directory (if MabCreate is TRUE)
    // - if .map is not not available, will search for the .mab file
    // - if no .mab is available, will search for a .mab appended to the .exe/.dll
    // - if nothing is available, will log as hexadecimal pointers, without
    // debugging information
    constructor Create(const aExeName: TFileName=''; MabCreate: boolean=true);
    /// save all debugging information in the .mab custom binary format
    // - if no file name is specified, it will be saved as ExeName.mab or DllName.mab
    // - this file content can be appended to the executable via SaveToExe method
    // - this function returns the created file name
    function SaveToFile(const aFileName: TFileName=''): TFileName;
    /// save all debugging informat in our custom binary format
    procedure SaveToStream(aStream: TStream);
    /// append all debugging information to an executable (or library)
    // - the executable name must be specified, because it's impossible to
    // write to the executable of a running process
    // - this method will work for .exe and for .dll (or .ocx)
    procedure SaveToExe(const aExeName: TFileName);
    /// save all debugging information as JSON content
    // - may be useful from debugging purposes
    procedure SaveToJson(W: TTextWriter); overload;
    /// save all debugging information as a JSON file
    // - may be useful from debugging purposes
    procedure SaveToJson(const aJsonFile: TFileName; aHumanReadable: Boolean=false); overload;
    /// add some debugging information about the supplied absolute memory address
    // - will create a global TSynMapFile instance for the current process, if
    // necessary
    // - if no debugging information is available (.map or .mab), will write
    // the raw address pointer as hexadecimal
    // - under FPC, currently calls BacktraceStrFunc() which may be very slow
    class procedure Log(W: TTextWriter; aAddressAbsolute: PtrUInt;
      AllowNotCodeAddr: boolean);
    /// compute the relative memory address from its absolute (pointer) value
    function AbsoluteToOffset(aAddressAbsolute: PtrUInt): integer;
    /// retrieve a symbol according to a relative code address
    // - use fast O(log n) binary search
    function FindSymbol(aAddressOffset: integer): integer;
    /// retrieve an unit and source line, according to a relative code address
    // - use fast O(log n) binary search
    function FindUnit(aAddressOffset: integer; out LineNumber: integer): integer; overload;
    /// retrieve an unit information, according to the unit name
    // - will search within Units array
    function FindUnit(const aUnitName: RawUTF8): integer; overload;
    /// return the symbol location according to the supplied absolute address
    // - i.e. unit name, symbol name and line number (if any), as plain text
    // - returns '' if no match found
    function FindLocation(aAddressAbsolute: PtrUInt): RawUTF8; overload;
    /// return the symbol location according to the supplied ESynException
    // - i.e. unit name, symbol name and line number (if any), as plain text
    // - under FPC, currently calls BacktraceStrFunc() which may be very slow
    class function FindLocation(exc: ESynException): RawUTF8; overload;
    /// return the low-level stack trace exception information into human-friendly text
    class function FindStackTrace(const Ctxt: TSynLogExceptionContext): TRawUTF8DynArray;
    /// returns the file name of
    // - if unitname = '', returns the main file name of the current executable
    class function FindFileName(const unitname: RawUTF8): TFileName;
    /// returns the global TSynMapFile instance associated with the current
    // executable
    class function FromCurrentExecutable: TSynMapFile;
    /// all symbols associated to the executable
    property Symbols: TSynMapSymbolDynArray read fSymbol;
    /// all units, including line numbers, associated to the executable
    property Units: TSynMapUnitDynArray read fUnit;
  published
    /// the associated file name
    property FileName: TFileName read fMapFile;
    /// equals true if a .map or .mab debugging information has been loaded
    property HasDebugInfo: boolean read fHasDebugInfo;
  end;
  {$M-}

  /// an exception which wouldn't be logged and intercepted by this unit
  // - only this exact class will be recognized by TSynLog: inheriting it
  // will trigger the interception, as any other regular exception
  ESynLogSilent = class(ESynException);

  {$M+} { we need the RTTI for the published methods of the logging classes }

  TSynLog = class;

  /// class-reference type (metaclass) of a TSynLog family
  // - since TSynLog classes store their information per type, you usually
  // will store a reference to a logging family (i.e. logging settings) using
  // a TSynLogClass variable, whereas TSynLog would point to the active logging
  // instance
  TSynLogClass = class of TSynLog;

  TSynLogFamily = class;
  TSynLogFile = class;

  {$M-}

  /// a generic interface used for logging a method
  // - you should create one TSynLog instance at the beginning of a block code
  // using TSynLog.Enter: the ISynLog will be released automaticaly by the
  // compiler at the end of the method block, marking it's executation end
  // - all logging expect UTF-8 encoded text, i.e. usualy English text
  ISynLog = interface(IUnknown)
    ['{527AC81F-BC41-4717-B089-3F74DE56F1AE}']
    /// call this method to add some information to the log at a specified level
    // - will use TTextWriter.Add(...,twOnSameLine) to append its content
    // - % = #37 indicates a string, integer, floating-point, class parameter
    // to be appended as text (e.g. class name), any variant as JSON...
    // - note that cardinal values should be type-casted to Int64() (otherwise
    // the integer mapped value will be transmitted, therefore wrongly)
    // - if Instance is set, it will log the corresponding class name and address
    // (to be used if you didn't call TSynLog.Enter() method first)
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUTF8; const TextArgs: array of const;
      Instance: TObject=nil); overload;
    /// call this method to add some information to the log at a specified level
    // - if Instance is set and Text is not '', it will log the corresponding
    // class name and address (to be used e.g. if you didn't call TSynLog.Enter()
    // method first)
    // - if Instance is set and Text is '', will behave the same as
    // Log(Level,Instance), i.e. write the Instance as JSON content
    procedure Log(Level: TSynLogInfo; const Text: RawUTF8;
      Instance: TObject=nil; TextTruncateAtLength: integer=maxInt); overload;
    {$ifdef UNICODE}
    /// call this method to add some VCL string to the log at a specified level
    // - this overloaded version will avoid a call to StringToUTF8()
    procedure Log(Level: TSynLogInfo; const Text: string; Instance: TObject=nil); overload;
    {$endif}
    /// call this method to add the content of an object to the log at a
    // specified level
    // - TSynLog will write the class and hexa address - TSQLLog will write the
    // object JSON content
    procedure Log(Level: TSynLogInfo; Instance: TObject); overload;
    /// call this method to add the content of most low-level types to the log
    // at a specified level
    // - TSynLog will handle enumerations and dynamic array; TSQLLog will be
    // able to write TObject/TSQLRecord and sets content as JSON
    procedure Log(Level: TSynLogInfo; const aName: RawUTF8;
      aTypeInfo: pointer; const aValue; Instance: TObject); overload;
    /// call this method to add the caller address to the log at the specified level
    // - if the debugging info is available from TSynMapFile, will log the
    // unit name, associated symbol and source code line
    procedure Log(Level: TSynLogInfo=sllTrace); overload;
    /// call this method to add some multi-line information to the log at a
    // specified level
    // - LinesToLog content will be added, one line per one line, delimited
    // by #13#10 (CRLF)
    // - if a line starts with IgnoreWhenStartWith (already uppercase), it won't
    // be added to the log content (to be used e.g. with '--' for SQL statements)
    procedure LogLines(Level: TSynLogInfo; LinesToLog: PUTF8Char;
      aInstance: TObject=nil; const IgnoreWhenStartWith: PAnsiChar=nil);
    /// retrieve the associated logging instance
    function Instance: TSynLog;
  end;

  {$ifndef DELPHI5OROLDER}
  /// a mORMot-compatible calback definition
  // - used to notify a remote mORMot server via interface-based serivces
  // for any incoming event, using e.g. TSynLogCallbacks.Subscribe
  ISynLogCallback = interface(IInvokable)
    ['{9BC218CD-A7CD-47EC-9893-97B7392C37CF}']
    /// each line of the TTextWriter internal instance will trigger this method
    // - the format is similar to TOnTextWriterEcho, as defined in SynCommons
    // - an initial call with Level=sllNone and the whole previous Text may be
    // transmitted, if ReceiveExistingKB is set for TSynLogCallbacks.Subscribe()
    procedure Log(Level: TSynLogInfo; const Text: RawUTF8);
  end;

  /// store a subscribe to ISynLogCallback
  TSynLogCallback = record
    Levels: TSynLogInfos;
    Callback: ISynLogCallback;
  end;
  /// store the all subscribed ISynLogCallback
  TSynLogCallbackDynArray = array of TSynLogCallback;

  /// can manage a list of ISynLogCallback registrations
  TSynLogCallbacks = class(TSynPersistentLock)
  protected
    fCount: integer;
    fCurrentlyEchoing: boolean;
  public
    /// direct access to the registration storage
    Registration: TSynLogCallbackDynArray;
    /// high-level access to the registration storage
    Registrations: TDynArray;
    /// the TSynLog family actually associated with those callbacks
    TrackedLog: TSynLogFamily;
    /// initialize the registration storage for a given TSynLogFamily instance
    constructor Create(aTrackedLog: TSynLogFamily); reintroduce;
    /// finalize the registration storage for a given TSynLogFamily instance
    destructor Destroy; override;
    /// register a callback for a given set of log levels
    // - you can specify a number of KB of existing log content to send to the
    // monitoring tool, before the actual real-time process
    function Subscribe(const Levels: TSynLogInfos;
      const Callback: ISynLogCallback; ReceiveExistingKB: cardinal=0): integer; virtual;
    /// unregister a callback previously registered by Subscribe()
    procedure Unsubscribe(const Callback: ISynLogCallback); virtual;
    /// notify a given log event
    // - matches the TOnTextWriterEcho signature
    function OnEcho(Sender: TTextWriter; Level: TSynLogInfo;
      const Text: RawUTF8): boolean;
  published
    /// how many registrations are currently defined
    property Count: integer read fCount;
  end;
  {$endif}

  /// this event can be set for a TSynLogFamily to archive any deprecated log
  // into a custom compressed format
  // - will be called by TSynLogFamily when TSynLogFamily.Destroy identify
  // some outdated files
  // - the aOldLogFileName will contain the .log file with full path
  // - the aDestinationPath parameter will contain 'ArchivePath\log\YYYYMM\'
  // - should return true on success, false on error
  // - example of matching event handler are EventArchiveDelete/EventArchiveSynLZ
  // or EventArchiveZip in SynZip.pas
  // - this event handler will be called one time per .log file to archive,
  // then one last time with aOldLogFileName='' in order to close any pending
  // archive (used e.g. by EventArchiveZip to open the .zip only once)
  TSynLogArchiveEvent = function(const aOldLogFileName, aDestinationPath: TFileName): boolean;

  /// this event can be set for a TSynLogFamily to customize the file rotation
  // - will be called by TSynLog.PerformRotation
  // - should return TRUE if the function did process the file name
  // - should return FALSE if the function did not do anything, so that the
  // caller should perform the rotation as usual
  TSynLogRotateEvent = function(aLog: TSynLog; const aOldLogFileName: TFileName): boolean;

  /// how threading is handled by the TSynLogFamily
  // - proper threading expects the TSynLog.NotifyThreadEnded method to be called
  // when a thread is about to terminate, e.g. from TSQLRest.EndCurrentThread
  // - by default, ptMergedInOneFile will indicate that all threads are logged
  // in the same file, in occurence order
  // - if set to ptOneFilePerThread, it will create one .log file per thread
  // - if set to ptIdentifiedInOnFile, a new column will be added for each
  // log row, with the corresponding ThreadID - LogView tool will be able to
  // display per-thread logging, if needed - note that your application shall
  // use a thread pool (just like all mORMot servers classes do), otherwise
  // some random hash collision may occur if Thread IDs are not recycled enough
  // - if set to ptNoThreadProcess, no thread information is gathered, and all
  // Enter/Leave would be merged into a single call - but it may be mandatory
  // to use this option if TSynLog.NotifyThreadEnded is not called (e.g. from
  // legacy code), and that your process experiment instability issues
  TSynLogPerThreadMode = (
    ptMergedInOneFile, ptOneFilePerThread, ptIdentifiedInOnFile, ptNoThreadProcess);

  /// how stack trace shall be computed during logging
  TSynLogStackTraceUse = (stManualAndAPI,stOnlyAPI,stOnlyManual);

  /// how file existing shall be handled during logging
  TSynLogExistsAction = (acOverwrite, acAppend);

  /// callback signature used by TSynLogFamilly.OnBeforeException
  // - should return false to log the exception, or true to ignore it
  TSynLogOnBeforeException = function(const aExceptionContext: TSynLogExceptionContext;
    const aThreadName: RawUTF8): boolean of object;

  /// store simple log-related settings
  // - see also TDDDLogSettings in dddInfraSettings.pas and TSynDaemonSettings
  // in mORMotService.pas, which may be more integrated
  TSynLogSettings = class(TSynPersistent)
  protected
    fLevels: TSynLogInfos;
    fDestinationPath: TFileName;
    fRotateFileCount: integer;
    fLogClass: TSynLogClass;
  public
    /// set some default values
    constructor Create; override;
    /// define the log information into the supplied TSynLog class
    // - if you don't call this method, the logging won't be initiated
    procedure SetLog(aLogClass: TSynLogClass = nil);
    /// read-only access to the TSynLog class, if SetLog() has been called
    property LogClass: TSynLogClass read fLogClass;
  published
    /// the log levels to be used for the log file
    // - i.e. a combination of none or several logging event
    // - if "*" is serialized, unneeded sllNone won't be part of the set
    // - default is LOG_STACKTRACE
    property Levels: TSynLogInfos read fLevels write fLevels;
    /// allow to customize where the logs should be written
    // - default is the system log folder (e.g. /var/log on Linux)
    property DestinationPath: TFileName read fDestinationPath write fDestinationPath;
    /// how many files will be rotated (default is 2)
    property RotateFileCount: integer read fRotateFileCount write fRotateFileCount;
  end;

  /// regroup several logs under an unique family name
  // - you should usualy use one family per application or per architectural
  // module: e.g. a server application may want to log in separate files the
  // low-level Communication, the DB access, and the high-level process
  // - initialize the family settings before using them, like in this code:
  // ! with TSynLogDB.Family do begin
  // !   Level := LOG_VERBOSE;
  // !   PerThreadLog := ptOneFilePerThread;
  // !   DestinationPath := 'C:\Logs';
  // ! end;
  //- then use the logging system inside a method:
  // ! procedure TMyDB.MyMethod;
  // ! var log: ISynLog;
  // ! begin
  // !   log := TSynLogDB.Enter(self,'MyMethod');
  // !   // do some stuff
  // !   log.Log(sllInfo,'method run with no problem and value=%',[value]);
  // ! end; // here log will be released and method leaving will be logged
  TSynLogFamily = class
  protected
    fLevel, fLevelStackTrace: TSynLogInfos;
    fArchiveAfterDays: Integer;
    fArchivePath: TFileName;
    fOnArchive: TSynLogArchiveEvent;
    fOnRotate: TSynLogRotateEvent;
    fPerThreadLog: TSynLogPerThreadMode;
    fIncludeComputerNameInFileName: boolean;
    fCustomFileName: TFileName;
    fGlobalLog: TSynLog;
    fSynLogClass: TSynLogClass;
    fIdent: integer;
    fDestinationPath: TFileName;
    fDefaultExtension: TFileName;
    fBufferSize: integer;
    fHRTimestamp: boolean;
    fLocalTimestamp: boolean;
    fWithUnitName: boolean;
    fWithInstancePointer: boolean;
    fNoFile: boolean;
    fAutoFlush: cardinal;
    {$ifdef MSWINDOWS}
    fNoEnvironmentVariable: boolean;
    {$endif}
    {$ifndef NOEXCEPTIONINTERCEPT}
    fHandleExceptions: boolean;
    {$endif}
    fStackTraceLevel: byte;
    fStackTraceUse: TSynLogStackTraceUse;
    fFileExistsAction: TSynLogExistsAction;
    fExceptionIgnore: TList;
    fOnBeforeException: TSynLogOnBeforeException;
    fEchoToConsole: TSynLogInfos;
    fEchoToConsoleUseJournal: boolean;
    fEchoCustom: TOnTextWriterEcho;
    fEchoRemoteClient: TObject;
    fEchoRemoteClientOwned: boolean;
    fEchoRemoteEvent: TOnTextWriterEcho;
    fEndOfLineCRLF: boolean;
    fDestroying: boolean;
    fRotateFileCurrent: cardinal;
    fRotateFileCount: cardinal;
    fRotateFileSize: cardinal;
    fRotateFileAtHour: integer;
    fRotateFileNoCompression: boolean;
    function CreateSynLog: TSynLog;
    procedure StartAutoFlush;
    procedure SetDestinationPath(const value: TFileName);
    procedure SetLevel(aLevel: TSynLogInfos);
    procedure SynLogFileListEcho(const aEvent: TOnTextWriterEcho; aEventAdd: boolean);
    procedure SetEchoToConsole(aEnabled: TSynLogInfos);
    procedure SetEchoToConsoleUseJournal(aValue: boolean);
    procedure SetEchoCustom(const aEvent: TOnTextWriterEcho);
    function GetSynLogClassName: string;
    function GetExceptionIgnoreCurrentThread: boolean;
    procedure SetExceptionIgnoreCurrentThread(
      aExceptionIgnoreCurrentThread: boolean);
  public
    /// intialize for a TSynLog class family
    // - add it in the global SynLogFileFamily[] list
    constructor Create(aSynLog: TSynLogClass);
    /// release associated memory
    // - will archive older DestinationPath\*.log files, according to
    // ArchiveAfterDays value and ArchivePath
    destructor Destroy; override;

    /// retrieve the corresponding log file of this thread and family
    // - creates the TSynLog if not already existing for this current thread
    function SynLog: TSynLog;
    /// register one object and one echo callback for remote logging
    // - aClient is typically a mORMot's TSQLHttpClient or a TSynLogCallbacks
    // instance as defined in this unit
    // - if aClientOwnedByFamily is TRUE, its life time will be manage by this
    // TSynLogFamily: it will stay alive until this TSynLogFamily is destroyed,
    // or the EchoRemoteStop() method called
    // - aClientEvent should be able to send the log row to the remote server
    procedure EchoRemoteStart(aClient: TObject; const aClientEvent: TOnTextWriterEcho;
      aClientOwnedByFamily: boolean);
    /// stop echo remote logging
    // - will free the aClient instance supplied to EchoRemoteStart
    procedure EchoRemoteStop;
    /// can be used to retrieve up to a specified amount of KB of existing log
    // - expects a single file to be opened for this family
    // - will retrieve the log content for the current file, truncating the
    // text up to the specified number of KB (an up to 128 MB at most)
    function GetExistingLog(MaximumKB: cardinal): RawUTF8;
    /// callback to notify the current logger that its thread is finished
    // - method follows TNotifyThreadEvent signature, which can be assigned to
    // TSynBackgroundThreadAbstract.OnAfterExecute
    // - is called e.g. by TSQLRest.EndCurrentThread
    procedure OnThreadEnded(Sender: TThread);

    /// you can add some exceptions to be ignored to this list
    // - for instance, EConvertError may be added to the list, as such:
    // ! TSQLLog.Family.ExceptionIgnore.Add(EConvertError);
    // - you may also trigger ESynLogSilent exceptions for silent process
    // - see also ExceptionIgnoreCurrentThread property, if you want a per-thread
    // filtering of all exceptions
    property ExceptionIgnore: TList read fExceptionIgnore;
    /// allow to (temporarly) ignore exceptions in the current thread
    // - this property will affect all TSynLogFamily instances, for the
    // current thread
    // - may be used in a try...finally block e.g. when notifying the exception
    // to a third-party service, or during a particular process
    // - see also ExceptionIgnore property - which is also checked in addition
    // to this flag
    property ExceptionIgnoreCurrentThread: boolean
      read GetExceptionIgnoreCurrentThread write SetExceptionIgnoreCurrentThread;
    /// you can let exceptions be ignored from a callback
    // - if set and returns true, the given exception won't be logged
    // - execution of this event handler is protected via the logs global lock
    // - may be handy e.g. when working with code triggerring a lot of
    // exceptions (e.g. Indy), where ExceptionIgnore could be refined
    property OnBeforeException: TSynLogOnBeforeException
      read fOnBeforeException write fOnBeforeException;
    /// event called to archive the .log content after a defined delay
    // - Destroy will parse DestinationPath folder for *.log files matching
    // ArchiveAfterDays property value
    // - you can set this property to EventArchiveDelete in order to delete deprecated
    // files, or EventArchiveSynLZ to compress the .log file into our propertary
    // SynLZ format: resulting file name will be ArchivePath\log\YYYYMM\*.log.synlz
    // (use FileUnSynLZ function to uncompress it)
    // - if you use SynZip.EventArchiveZip, the log files will be archived in
    // ArchivePath\log\YYYYMM.zip
    // - the aDestinationPath parameter will contain 'ArchivePath\log\YYYYMM\'
    // - this event handler will be called one time per .log file to archive,
    // then one last time with aOldLogFileName='' in order to close any pending
    // archive (used e.g. by EventArchiveZip to open the .zip only once)
    property OnArchive: TSynLogArchiveEvent read fOnArchive write fOnArchive;
    /// event called to perform a custom file rotation
    // - will be checked by TSynLog.PerformRotation to customize the rotation
    // process and do not perform the default step, if the callback returns TRUE
    property OnRotate: TSynLogRotateEvent read fOnRotate write fOnRotate;
    /// if the some kind of events shall be echoed to the console
    // - note that it will slow down the logging process a lot (console output
    // is slow by nature under Windows, but may be convenient for interactive
    // debugging of services, for instance
    // - this property shall be set before any actual logging, otherwise it
    // will have no effect
    // - can be set e.g. to LOG_VERBOSE in order to echo every kind of events
    // - EchoCustom or EchoToConsole can be activated separately
    property EchoToConsole: TSynLogInfos read fEchoToConsole write SetEchoToConsole;
    /// For Linux with journald
    // - if true: redirect all EchoToConsole logging into journald service
    // - such logs can be exported into a format whichcan be viewed by our
    // LogView tool using a command (replacing UNIT with your unit name and
    // PROCESS with the executable name):
    // $ "journalctl -u UNIT --no-hostname -o short-iso-precise --since today | grep "PROCESS\[.*\]:  . " > todaysLog.log"
    property EchoToConsoleUseJournal: boolean read fEchoToConsoleUseJournal
      write SetEchoToConsoleUseJournal;
    /// can be set to a callback which will be called for each log line
    // - could be used with a third-party logging system
    // - EchoToConsole or EchoCustom can be activated separately
    // - you may even disable the integrated file output, via NoFile := true
    property EchoCustom: TOnTextWriterEcho read fEchoCustom write SetEchoCustom;
    /// the associated TSynLog class
    property SynLogClass: TSynLogClass read fSynLogClass;
  published
    /// the associated TSynLog class
    property SynLogClassName: string read GetSynLogClassName;
    /// index in global SynLogFileFamily[] and SynLogFileIndexThreadVar[] lists
    property Ident: integer read fIdent;
    /// the current level of logging information for this family
    // - can be set e.g. to LOG_VERBOSE in order to log every kind of events
    property Level: TSynLogInfos read fLevel write SetLevel;
    /// the levels which will include a stack trace of the caller
    // - by default, contains sllStackTrace,sllException,sllExceptionOS plus
    // sllError,sllFail,sllLastError,sllDDDError for Delphi only  - since FPC
    // BacktraceStrFunc() function is very slow
    // - exceptions will always trace the stack
    property LevelStackTrace: TSynLogInfos read fLevelStackTrace write fLevelStackTrace;
    /// the folder where the log must be stored
    // - by default, is in the executable folder
    property DestinationPath: TFileName read fDestinationPath write SetDestinationPath;
    /// the file extension to be used
    // - is '.log' by default
    property DefaultExtension: TFileName read fDefaultExtension write fDefaultExtension;
    /// if TRUE, the log file name will contain the Computer name - as '(MyComputer)'
    property IncludeComputerNameInFileName: boolean read fIncludeComputerNameInFileName write fIncludeComputerNameInFileName;
    /// can be used to customized the default file name
    // - by default, the log file name is computed from the executable name
    // (and the computer name if IncludeComputerNameInFileName is true)
    // - you can specify your own file name here, to be used instead
    // - this file name should not contain any folder, nor file extension (which
    // are set by DestinationPath and DefaultExtension properties)
    property CustomFileName: TFileName read fCustomFileName write fCustomFileName;
    /// the folder where old log files must be compressed
    // - by default, is in the executable folder, i.e. the same as DestinationPath
    // - the 'log\' sub folder name will always be appended to this value
    // - will then be used by OnArchive event handler to produce, with the
    // current file date year and month, the final path (e.g.
    // 'ArchivePath\Log\YYYYMM\*.log.synlz' or 'ArchivePath\Log\YYYYMM.zip')
    property ArchivePath: TFileName read fArchivePath write fArchivePath;
    /// number of days before OnArchive event will be called to compress
    // or delete deprecated files
    // - will be set by default to 7 days
    // - will be used by Destroy to call OnArchive event handler on time
    property ArchiveAfterDays: Integer read fArchiveAfterDays write fArchiveAfterDays;
    /// the internal in-memory buffer size, in bytes
    // - this is the number of bytes kept in memory before flushing to the hard
    // drive; you can call TSynLog.Flush method or set AutoFlushTimeOut to true
    // in order to force the writting to disk
    // - is set to 4096 by default (4 KB is the standard hard drive cluster size)
    property BufferSize: integer read fBufferSize write fBufferSize;
    /// define how thread will be identified during logging process
    // - by default, ptMergedInOneFile will indicate that all threads are logged
    // in the same file, in occurence order (so multi-thread process on server
    // side may be difficult to interpret)
    // - if RotateFileCount and RotateFileSizeKB/RotateFileDailyAtHour are set,
    // will be ignored (internal thread list shall be defined for one process)
    property PerThreadLog: TSynLogPerThreadMode read fPerThreadLog write fPerThreadLog;
    /// if TRUE, will log high-resolution time stamp instead of ISO 8601 date and time
    // - this is less human readable, but allows performance profiling of your
    // application on the customer side (using TSynLog.Enter methods)
    // - set to FALSE by default, or if RotateFileCount and RotateFileSizeKB /
    // RotateFileDailyAtHour are set (the high resolution frequency is set
    // in the log file header, so expects a single file)
    property HighResolutionTimestamp: boolean read fHRTimestamp write fHRTimestamp;
    /// by default, time logging will use error-safe UTC values as reference
    // - you may set this property to TRUE to store local time instead
    property LocalTimestamp: boolean read fLocalTimestamp write fLocalTimestamp;
    /// if TRUE, will log the unit name with an object instance if available
    // - unit name is available from RTTI if the class has published properties
    // - set to TRUE by default, for better debugging experience
    property WithUnitName: boolean read fWithUnitName write fWithUnitName;
    /// if TRUE, will log the pointer with an object instance class if available
    // - set to TRUE by default, for better debugging experience
    property WithInstancePointer: boolean read fWithInstancePointer write fWithInstancePointer;
    /// the time (in seconds) after which the log content must be written on
    // disk, whatever the current content size is
    // - by default, the log file will be written for every 4 KB of log (see
    // BufferSize property) - this will ensure that the main application won't
    // be slow down by logging
    // - in order not to loose any log, a background thread can be created
    // and will be responsible of flushing all pending log content every
    // period of time (e.g. every 10 seconds)
    property AutoFlushTimeOut: cardinal read fAutoFlush write fAutoFlush;
    {$ifdef MSWINDOWS}
    /// force no environment variables to be written to the log file
    // - may be usefull if they contain some sensitive information
    property NoEnvironmentVariable: boolean read fNoEnvironmentVariable write fNoEnvironmentVariable;
    {$endif}
    /// force no log to be written to any file
    // - may be usefull in conjunction e.g. with EchoToConsole or any other
    // third-party logging component
    property NoFile: boolean read fNoFile write fNoFile;
    /// auto-rotation of logging files
    // - set to 0 by default, meaning no rotation
    // - can be set to a number of rotating files: rotation and compression will
    // happen, and main file size will be up to RotateFileSizeKB number of bytes,
    // or when RotateFileDailyAtHour time is reached
    // - if set to 1, no .synlz backup will be created, so the main log file will
    // be restarted from scratch when it reaches RotateFileSizeKB size or when
    // RotateFileDailyAtHour time is reached
    // - if set to a number > 1, some rotated files will be compressed using the
    // SynLZ algorithm, and will be named e.g. as MainLogFileName.0.synlz ..
    // MainLogFileName.7.synlz for RotateFileCount=9 (total count = 9, including
    // 1 main log file and 8 .synlz files)
    property RotateFileCount: cardinal read fRotateFileCount write fRotateFileCount;
    /// maximum size of auto-rotated logging files, in kilo-bytes (per 1024 bytes)
    // - specify the maximum file size upon which .synlz rotation takes place
    // - is not used if RotateFileCount is left to its default 0
    property RotateFileSizeKB: cardinal read fRotateFileSize write fRotateFileSize;
    /// fixed hour of the day where logging files rotation should be performed
    // - by default, equals -1, meaning no rotation
    // - you can set a time value between 0 and 23 to force the rotation at this
    // specified hour
    // - is not used if RotateFileCount is left to its default 0
    property RotateFileDailyAtHour: integer read fRotateFileAtHour write fRotateFileAtHour;
    /// if set to TRUE, no #.synlz will be created at rotation but plain #.log file
    property RotateFileNoCompression: boolean read fRotateFileNoCompression write fRotateFileNoCompression;
    /// the recursive depth of stack trace symbol to write
    // - used only if exceptions are handled, or by sllStackTrace level
    // - default value is 30, maximum is 255
    // - if stOnlyAPI is defined as StackTraceUse under Windows XP, maximum
    // value may be around 60, due to RtlCaptureStackBackTrace() API limitations
    property StackTraceLevel: byte read fStackTraceLevel write fStackTraceLevel;
    /// how the stack trace shall use only the Windows API
    // - the class will use low-level RtlCaptureStackBackTrace() API to retrieve
    // the call stack: in some cases, it is not able to retrieve it, therefore
    // a manual walk of the stack can be processed - since this manual call can
    // trigger some unexpected access violations or return wrong positions,
    // you can disable this optional manual walk by setting it to stOnlyAPI
    // - default is stManualAndAPI, i.e. use RtlCaptureStackBackTrace() API and
    // perform a manual stack walk if the API returned no address (or <3); but
    // within the IDE, it will use stOnlyAPI, to ensure no annoyning AV occurs
    property StackTraceUse: TSynLogStackTraceUse read fStackTraceUse write fStackTraceUse;
    /// how existing log file shall be handled
    property FileExistsAction: TSynLogExistsAction read fFileExistsAction write fFileExistsAction;
    /// define how the logger will emit its line feed
    // - by default (FALSE), a single LF (#10) char will be written, to save
    // storage space
    // - you can set this property to TRUE, so that CR+LF (#13#10) chars will
    // be appended instead
    // - TSynLogFile class and our LogView tool will handle both patterns
    property EndOfLineCRLF: boolean read fEndOfLineCRLF write fEndOfLineCRLF;
  end;

  /// TSynLogThreadContext will define a dynamic array of such information
  // - used by TSynLog.Enter methods to handle recursivity calls tracing
  TSynLogThreadRecursion = record
    /// associated class instance to be displayed
    Instance: TObject;
    /// method name (or message) to be displayed
    // - may be a RawUTF8 if MethodNameLocal=mnEnterOwnMethodName
    MethodName: PUTF8Char;
    /// internal reference count used at this recursion level by TSynLog._AddRef
    RefCount: integer;
    /// the caller address, ready to display stack trace dump if needed
    Caller: PtrUInt;
    /// the time stamp at enter time
    EnterTimestamp: Int64;
    /// if the method name is local, i.e. shall not be displayed at Leave()
    MethodNameLocal: (mnAlways, mnEnter, mnLeave, mnEnterOwnMethodName);
  end;
  PSynLogThreadRecursion = ^TSynLogThreadRecursion;

  /// thread-specific internal context used during logging
  // - this structure is a hashed-per-thread variable
  TSynLogThreadContext = record
    /// the corresponding Thread ID
    ID: TThreadID;
    /// number of items stored in Recursion[]
    RecursionCount: integer;
    /// number of items available in Recursion[]
    // - faster than length(Recursion)
    RecursionCapacity: integer;
    /// used by TSynLog.Enter methods to handle recursivity calls tracing
    Recursion: array of TSynLogThreadRecursion;
    /// the associated thread name
    ThreadName: RawUTF8;
  end;
  // pointer to thread-specific context information
  PSynLogThreadContext = ^TSynLogThreadContext;

  /// file stream which ignores I/O write errors
  // - in case disk space is exhausted, TFileStreamWithoutWriteError.WriteBuffer
  // won't throw any exception, so application will continue to work
  // - used by TSynLog to let the application continue with no exception,
  // even in case of a disk/partition full of logs
  TFileStreamWithoutWriteError = class(TFileStream)
  public
    /// this overriden function returns Count, as if it was always sucessfull
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  /// a per-family and/or per-thread log file content
  // - you should create a sub class per kind of log file
  // ! TSynLogDB = class(TSynLog);
  // - the TSynLog instance won't be allocated in heap, but will share a
  // per-thread (if Family.PerThreadLog=ptOneFilePerThread) or global private
  // log file instance
  // - was very optimized for speed, if no logging is written, and even during
  // log write (using an internal TTextWriter)
  // - can use available debugging information via the TSynMapFile class, for
  // stack trace logging for exceptions, sllStackTrace, and Enter/Leave labelling
  TSynLog = class(TObject, ISynLog)
  protected
    fFamily: TSynLogFamily;
    fWriter: TTextWriterWithEcho;
    fWriterClass: TTextWriterClass;
    fWriterStream: TStream;
    fThreadContext: PSynLogThreadContext;
    fThreadID: TThreadID;
    fThreadLastHash: integer;
    fThreadIndex: integer;
    fStartTimestamp: Int64;
    fCurrentTimestamp: Int64;
    {$ifndef LINUX}
    fFrequencyTimestamp: Int64;
    {$endif}
    fStartTimestampDateTime: TDateTime;
    fStreamPositionAfterHeader: cardinal;
    fFileName: TFileName;
    fFileRotationSize: cardinal;
    fFileRotationNextHour: Int64;
    fThreadHash: TWordDynArray; // 8 KB buffer
    fThreadIndexReleased: TWordDynArray;
    fThreadIndexReleasedCount: integer;
    fThreadContexts: array of TSynLogThreadContext;
    fThreadContextCount: integer;
    fCurrentLevel: TSynLogInfo;
    fInternalFlags: set of (logHeaderWritten, logInitDone);
    fDisableRemoteLog: boolean;
    {$ifndef NOEXCEPTIONINTERCEPT} // for IsBadCodePtr() or any internal exception
    fThreadHandleExceptionBackup: TSynLog;
    {$endif}
    {$ifdef FPC}
    function QueryInterface(
      {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    {$else}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {$endif}
    class function FamilyCreate: TSynLogFamily;
    procedure CreateLogWriter; virtual;
    procedure LogInternal(Level: TSynLogInfo; const TextFmt: RawUTF8;
      const TextArgs: array of const; Instance: TObject); overload;
    procedure LogInternal(Level: TSynLogInfo; const Text: RawUTF8;
      Instance: TObject; TextTruncateAtLength: integer); overload;
    procedure LogInternal(Level: TSynLogInfo; const aName: RawUTF8;
      aTypeInfo: pointer; const aValue; Instance: TObject); overload;
    // any call to this method MUST call LogTrailerUnLock
    function LogHeaderLock(Level: TSynLogInfo; AlreadyLocked: boolean): boolean;
    procedure LogTrailerUnLock(Level: TSynLogInfo); {$ifdef HASINLINENOTX86}inline;{$endif}
    procedure LogCurrentTime; virtual;
    procedure LogFileInit; virtual;
    procedure LogFileHeader; virtual;
    {$ifndef DELPHI5OROLDER}
    procedure AddMemoryStats; virtual;
    {$endif}
    procedure AddErrorMessage(Error: cardinal);
    procedure AddStackTrace(Stack: PPtrUInt);
    procedure ComputeFileName; virtual;
    function GetFileSize: Int64; virtual;
    procedure PerformRotation; virtual;
    procedure AddRecursion(aIndex: integer; aLevel: TSynLogInfo);
    procedure LockAndGetThreadContext; {$ifdef HASINLINENOTX86}inline;{$endif}
    procedure GetThreadContextInternal;
    function NewRecursion: PSynLogThreadRecursion;
    procedure ThreadContextRehash;
    function Instance: TSynLog;
    function ConsoleEcho(Sender: TTextWriter; Level: TSynLogInfo;
      const Text: RawUTF8): boolean; virtual;
  public
    /// intialize for a TSynLog class instance
    // - WARNING: not to be called directly! Use Enter or Add class function instead
    constructor Create(aFamily: TSynLogFamily=nil); virtual;
    /// release all memory and internal handles
    destructor Destroy; override;
    /// flush all log content to file
    // - if ForceDiskWrite is TRUE, will wait until written on disk (slow)
    procedure Flush(ForceDiskWrite: boolean);
    /// flush all log content to file and close the file
    procedure CloseLogFile;
    /// flush all log content to file, close the file, and release the instance
    // - you should never call the Free method directly, since the instance
    // is registered in a global TObjectList and an access violation may
    // occur at application closing: you can use this Release method if you
    // are sure that you won't need this TSynLog instance any more
    // - ensure there is no pending Leave element in a stack-allocated ISynLog
    // (see below)
    // - can be used e.g. to release the instance when finishing a thread when
    // Family.PerThreadLog=ptOneFilePerThread:
    // ! var
    // !   TThreadLogger : TSynLogClass = TSynLog;
    // !
    // ! procedure TMyThread.Execute;
    // ! var log : ISynLog;
    // ! begin
    // !   log := TThreadLogger.Enter(self);
    // ! ...
    // !   log := nil; // to force logging end of method
    // !   TThreadLogger.SynLog.Release;
    // ! end;
    procedure Release;
    /// you may call this method when a thread is ended
    // - should be called in the thread context which is about to terminate,
    // in a situation where no other logging may occur from this thread any more
    // - it will release all thread-specific resource used by this TSynLog
    // - is called e.g. by TSQLRest.EndCurrentThread, via TSynLogFamily.OnThreadEnded
    procedure NotifyThreadEnded;
    /// handle generic method enter / auto-leave tracing
    // - this is the main method to be called within a procedure/function to trace:
    // ! procedure TMyDB.SQLExecute(const SQL: RawUTF8);
    // ! var log: ISynLog;
    // ! begin
    // !   log := TSynLogDB.Enter(self,'SQLExecute');
    // !   // do some stuff
    // !   log.Log(sllInfo,'SQL=%',[SQL]);
    // ! end; // here log will be released, and method leaving will be logged
    // - returning a ISynLog interface will allow you to have an automated
    // sllLeave log created when the method is left (thanks to the hidden
    // try..finally block generated by the compiler to protect the ISynLog var)
    // - WARNING: due to a limitation (feature?) of the FPC compiler and
    // Delphi 10.4 and later, you NEED to hold the returned value into a
    // local ISynLog variable; as a benefit, it is always convenient to define
    // a local variable to store the returned ISynLog and use it for any
    // specific logging within the method execution
    // - on Delphi earlier to 10.4 (and not FPC), you could just call Enter()
    // inside the method block, without any ISynLog interface variable - but
    // it is not very future-proof to write the following code:
    // ! procedure TMyDB.SQLFlush;
    // ! begin
    // !   TSynLogDB.Enter(self,'SQLFlush');
    // !   // do some stuff
    // ! end;
    // - if no Method name is supplied, it will use the caller address, and
    // will write it as hexa and with full unit and symbol name, if the debugging
    // information is available (i.e. if TSynMapFile retrieved the .map content;
    // note that this is not available yet on FPC):
    // ! procedure TMyDB.SQLFlush;
    // ! var log: ISynLog;
    // ! begin
    // !   log := TSynLogDB.Enter(self);
    // !   // do some stuff
    // ! end;
    // - note that supplying a method name is faster than using the .map content:
    // if you want accurate profiling, it's better to use a method name or not to
    // use a .map file - note that this method name shall be a constant, and not
    // a locally computed variable, since it may trigger some random GPF at
    // runtime - if it is a local variable, you can set aMethodNameLocal=true
    // - if TSynLogFamily.HighResolutionTimestamp is TRUE, high-resolution
    // time stamp will be written instead of ISO 8601 date and time: this will
    // allow performance profiling of the application on the customer side
    // - Enter() will write the class name (and the unit name for classes with
    // published properties, if TSynLogFamily.WithUnitName is true) for both
    // enter (+) and leave (-) events:
    //  $ 20110325 19325801  +    MyDBUnit.TMyDB(004E11F4).SQLExecute
    //  $ 20110325 19325801 info   SQL=SELECT * FROM Table;
    //  $ 20110325 19325801  -    01.512.320
    class function Enter(aInstance: TObject=nil; aMethodName: PUTF8Char=nil;
      aMethodNameLocal: boolean=false): ISynLog; overload;
    /// handle method enter / auto-leave tracing, with some custom text
    // - this overloaded method would not write the method name, but the supplied
    // text content, after expanding the parameters like FormatUTF8()
    // - it will append the corresponding sllLeave log entry when the method ends
    class function Enter(const TextFmt: RawUTF8; const TextArgs: array of const;
      aInstance: TObject=nil): ISynLog; overload;
    /// retrieve the current instance of this TSynLog class
    // - to be used for direct logging, without any Enter/Leave:
    // ! TSynLogDB.Add.Log(llError,'The % statement didn''t work',[SQL]);
    // - to be used for direct logging, without any Enter/Leave (one parameter
    // version - just the same as previous):
    // ! TSynLogDB.Add.Log(llError,'The % statement didn''t work',SQL);
    // - is just a wrapper around Family.SynLog - the same code will work:
    // ! TSynLogDB.Family.SynLog.Log(llError,'The % statement didn''t work',[SQL]);
    class function Add: TSynLog; {$ifdef HASINLINENOTX86}inline;{$endif}
    /// retrieve the family of this TSynLog class type
    class function Family: TSynLogFamily; overload; {$ifdef HASINLINENOTX86}inline;{$endif}
    /// returns a logging class which will never log anything
    // - i.e. a TSynLog sub-class with Family.Level := []
    class function Void: TSynLogClass;
    /// low-level method helper which can be called to make debugging easier
    // - log some warning message to the TSynLog family
    // - will force a manual breakpoint if tests are run from the IDE
    class procedure DebuggerNotify(Level: TSynLogInfo;
      const Format: RawUTF8; const Args: array of const);
    /// call this method to add some information to the log at the specified level
    // - will use TTextWriter.Add(...,twOnSameLine) to append its content
    // - % = #37 indicates a string, integer, floating-point, class parameter
    // to be appended as text (e.g. class name), any variant as JSON...
    // - note that cardinal values should be type-casted to Int64() (otherwise
    // the integer mapped value will be transmitted, therefore wrongly)
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUTF8; const TextArgs: array of const;
      aInstance: TObject=nil); overload;
    /// same as Log(Level,TextFmt,[]) but with one RawUTF8 parameter
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUTF8; const TextArg: RawUTF8;
      aInstance: TObject=nil); overload;
    /// same as Log(Level,TextFmt,[]) but with one Int64 parameter
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUTF8; const TextArg: Int64;
      aInstance: TObject=nil); overload;
    /// call this method to add some information to the log at the specified level
    // - if Instance is set and Text is not '', it will log the corresponding
    // class name and address (to be used e.g. if you didn't call TSynLog.Enter()
    // method first) - for instance
    // ! TSQLLog.Add.Log(sllDebug,'GarbageCollector',GarbageCollector);
    // will append this line to the log:
    // $ 0000000000002DB9 debug TObjectList(00425E68) GarbageCollector
    // - if Instance is set and Text is '', will behave the same as
    // Log(Level,Instance), i.e. write the Instance as JSON content
    procedure Log(Level: TSynLogInfo; const Text: RawUTF8;
      aInstance: TObject=nil; TextTruncateAtLength: integer=maxInt); overload;
    {$ifdef UNICODE}
    /// call this method to add some VCL string to the log at a specified level
    // - this overloaded version will avoid a call to StringToUTF8()
    procedure Log(Level: TSynLogInfo; const Text: string; aInstance: TObject=nil); overload;
    {$endif}
    /// call this method to add the content of an object to the log at a
    // specified level
    // - this default implementation will just write the class name and its hexa
    // pointer value, and handle TList, TCollections and TStrings - for instance:
    // ! TSynLog.Add.Log(sllDebug,GarbageCollector);
    // will append this line to the log:
    // $ 20110330 10010005 debug {"TObjectList(00B1AD60)":["TObjectList(00B1AE20)","TObjectList(00B1AE80)"]}
    // - if aInstance is an Exception, it will handle its class name and Message:
    // $ 20110330 10010005 debug "EClassName(00C2129A)":"Exception message"
    // - use TSQLLog from mORMot.pas unit to add the record content, written
    // as human readable JSON
    procedure Log(Level: TSynLogInfo; aInstance: TObject); overload;
    /// call this method to add the content of most low-level types to the log
    // at a specified level
    // - this overridden implementation will write the value content,
    // written as human readable JSON: handle dynamic arrays and enumerations
    // - TSQLLog from mORMot.pas unit will be able to write
    // TObject/TSQLRecord and sets content as JSON
    procedure Log(Level: TSynLogInfo; const aName: RawUTF8;
      aTypeInfo: pointer; const aValue; Instance: TObject); overload;
    /// call this method to add the caller address to the log at the specified level
    // - if the debugging info is available from TSynMapFile, will log the
    // unit name, associated symbol and source code line
    procedure Log(Level: TSynLogInfo); overload;
    /// allows to identify the current thread with a textual representation
    // - would append an sllInfo entry with "SetThreadName ThreadID=Name" text
    // - entry would also be replicated at the begining of any rotated log file
    procedure LogThreadName(const Name: RawUTF8; IgnoreIfAlreadySet: boolean=false);
    /// call this method to add some multi-line information to the log at a
    // specified level
    // - LinesToLog content will be added, one line per one line, delimited by
    // #13#10 (CRLF)
    // - if a line starts with IgnoreWhenStartWith (already uppercase), it won't
    // be added to the log content (to be used e.g. with '--' for SQL statements)
    procedure LogLines(Level: TSynLogInfo; LinesToLog: PUTF8Char; aInstance: TObject=nil;
      const IgnoreWhenStartWith: PAnsiChar=nil);
    /// manual low-level TSynLog.Enter execution without the ISynLog
    // - may be used to log Enter/Leave stack from non-pascal code
    // - each call to ManualEnter should be followed by a matching ManualLeave
    // - aMethodName should be a not nil constant text
    procedure ManualEnter(aMethodName: PUtf8Char; aInstance: TObject = nil);
    /// manual low-level ISynLog release after TSynLog.Enter execution
    // - each call to ManualEnter should be followed by a matching ManualLeave
    procedure ManualLeave;
    /// allow to temporary disable remote logging
    // - to be used within a try ... finally section:
    // ! log.DisableRemoteLog(true);
    // ! try
    // !   log.Log(....); // won't be propagated to the remote log
    // ! finally
    // !   log.DisableRemoteLog(false);
    // ! end;
    procedure DisableRemoteLog(value: boolean);
    /// the associated TSynLog class
    function LogClass: TSynLogClass; {$ifdef HASINLINENOTX86}inline;{$endif}
    /// Force log rotation; Can be used for example inside SUGHUP signal handler
    procedure ForceRotation;
    /// direct access to the low-level writing content
    // - should usually not be used directly, unless you ensure it is safe
    property Writer: TTextWriterWithEcho read fWriter;
  published
    /// the associated file name containing the log
    // - this is accurate only with the default implementation of the class:
    // any child may override it with a custom logging mechanism
    property FileName: TFileName read fFileName;
    /// the current size, in bytes, of the associated file containing the log
    property FileSize: Int64 read GetFileSize;
    /// the current number of thread contexts associated with this instance
    // - doesn't match necessary the number of threads of the process, but the
    // threads which are still marked as active for this TSynLog
    // - a huge number may therefore not indicate a potential "out of memory"
    // error, but a broken logic with missing NotifyThreadEnded calls
    property ThreadContextCount: integer read fThreadContextCount;
    /// the associated logging family
    property GenericFamily: TSynLogFamily read fFamily;
  end;

  /// reference-counted block code critical section with context logging
  // - race conditions are difficult to track: you could use this TAutoLockerDebug
  // instead of plain TAutoLocker class, to log some information at each
  // Enter/Leave process, and track unexpected blocking issues
  // - see also the global USELOCKERDEBUG conditional, defined in Synopse.inc,
  // which may be used to enable verbose logging at compile time:
  // ! fSafe: IAutoLocker;
  // ! ...
  // ! {$ifdef USELOCKERDEBUG}
  // ! fSafe := TAutoLockerDebug.Create(fLogClass,aModel.Root); // more verbose
  // ! {$else}
  // ! fSafe := TAutoLocker.Create;
  // ! {$endif}
  TAutoLockerDebug = class(TAutoLocker)
  protected
    fLog: TSynLogClass;
    fIdentifier: RawUTF8;
    fCounter: integer;
  public
    /// initialize the mutex, which would log its Enter/Leave process
    // - the supplied identifier should be a short text, able to specify the
    // lock execution context, e.g. the resource which is actually protected
    // - an associated TSQLLog instance should be specified as logging target
    constructor Create(aLog: TSynLogClass; const aIdentifier: RawUTF8); reintroduce;
    /// enter the mutex
    procedure Enter; override;
    /// leave the mutex
    procedure Leave; override;
  end;

  /// used by TSynLogFile to refer to a method profiling in a .log file
  // - i.e. map a sllEnter/sllLeave event in the .log file
  TSynLogFileProc = record
    /// the index of the sllEnter event in the TSynLogFile.fLevels[] array
    Index: cardinal;
    /// the associated time elapsed in this method (in micro seconds)
    // - computed from the sllLeave time difference (high resolution timer)
    Time: cardinal;
    /// the time elapsed in this method and not in nested methods
    // - computed from Time property, minus the nested calls
    ProperTime: cardinal;
  end;

  /// used by TSynLogFile to refer to global method profiling in a .log file
  // - i.e. map all sllEnter/sllLeave event in the .log file
  TSynLogFileProcDynArray = array of TSynLogFileProc;

  TSynLogFileProcArray = array[0..(MaxInt div sizeof(TSynLogFileProc))-1] of TSynLogFileProc;
  PSynLogFileProcArray = ^TSynLogFileProcArray;

  /// used by TSynLogFile.LogProcSort method
  TLogProcSortOrder = (
    soNone, soByName, soByOccurrence, soByTime, soByProperTime);

  /// used to parse a .log file, as created by TSynLog, into high-level data
  // - this particular TMemoryMapText class will retrieve only valid event lines
  // (i.e. will fill EventLevel[] for each line <> sllNone)
  // - Count is not the global text line numbers, but the number of valid events
  // within the file (LinePointers/Line/Strings will contain only event lines) -
  // it will not be a concern, since the .log header is parsed explicitely
  TSynLogFile = class(TMemoryMapText)
  protected
    /// map the events occurring in the .log file content
    fLevels: TSynLogInfoDynArray;
    fThreads: TWordDynArray;
    fThreadInfo: array of record
      Rows: cardinal;
      SetThreadName: TPUTF8CharDynArray;
    end;
    fThreadInfoMax: cardinal;
    fThreadsCount: integer;
    fThreadMax: cardinal;
    fLineLevelOffset: cardinal;
    fLineTextOffset: cardinal;
    fLineHeaderCountToIgnore: integer;
    /// as extracted from the .log header
    fExeName, fExeVersion, fInstanceName: RawUTF8;
    fHost, fUser, fCPU, fOSDetailed, fFramework: RawUTF8;
    fExeDate: TDateTime;
    fIntelCPU: TIntelCpuFeatures;
    fOS: TWindowsVersion;
    fOSServicePack: integer;
    fWow64: boolean;
    fStartDateTime: TDateTime;
    fDayCurrent: Int64; // as PInt64('20160607')^
    fDayChangeIndex: TIntegerDynArray;
    fDayCount: TIntegerDynArray;
    /// retrieve all used event levels
    fLevelUsed: TSynLogInfos;
    /// =0 if date time resolution, >0 if high-resolution time stamp
    fFreq: Int64;
    /// used by EventDateTime() to compute date from time stamp
    fFreqPerDay: double;
    /// custom headers, to be searched as .ini content
    fHeaderLinesCount: integer;
    fHeaders: RawUTF8;
    /// method profiling data
    fLogProcCurrent: PSynLogFileProcArray;
    fLogProcCurrentCount: integer;
    fLogProcNatural: TSynLogFileProcDynArray;
    fLogProcNaturalCount: integer;
    fLogProcMerged: TSynLogFileProcDynArray;
    fLogProcMergedCount: integer;
    fLogProcIsMerged: boolean;
    fLogProcStack: array of array of cardinal;
    fLogProcStackCount: array of integer;
    fLogProcSortInternalOrder: TLogProcSortOrder;
    /// used by ProcessOneLine//GetLogLevelTextMap
    fLogLevelsTextMap: array[TSynLogInfo] of cardinal;
    fIsJournald: boolean;
    procedure SetLogProcMerged(const Value: boolean);
    function GetEventText(index: integer): RawUTF8;
    function GetLogLevelFromText(LineBeg: PUTF8Char): TSynLogInfo;
    /// retrieve headers + fLevels[] + fLogProcNatural[], and delete invalid fLines[]
    procedure LoadFromMap(AverageLineLength: integer=32); override;
    /// compute fLevels[] + fLogProcNatural[] for each .log line during initial reading
    procedure ProcessOneLine(LineBeg, LineEnd: PUTF8Char); override;
    /// called by LogProcSort method
    function LogProcSortComp(A,B: PtrInt): PtrInt;
    procedure LogProcSortInternal(L,R: PtrInt);
  public
    /// initialize internal structure
    constructor Create; override;
    /// returns TRUE if the supplied text is contained in the corresponding line
    function LineContains(const aUpperSearch: RawUTF8; aIndex: Integer): Boolean; override;
    /// retrieve the date and time of an event
    // - returns 0 in case of an invalid supplied index
    function EventDateTime(aIndex: integer): TDateTime;
    /// retrieve the description text of an event, as native VCL string
    // - returns '' if supplied index is out of range
    // - if the text is not truly UTF-8 encoded, would use the current system
    // codepage to create a valid string
    // - you may specify a text to replace all #9 characters occurences
    // - is used e.g. in TMainLogView.ListDrawCell
    function EventString(index: integer; const replaceTabs: RawUTF8='';
      maxutf8len: Integer=0; includeFirstColumns: boolean=false): string;
    /// sort the LogProc[] array according to the supplied order
    procedure LogProcSort(Order: TLogProcSortOrder);
    /// return the number of matching events in the log
    function EventCount(const aSet: TSynLogInfos): integer;
    /// add a new line to the already parsed content
    // - overriden method which would identify the freq=%,%,% pseudo-header
    procedure AddInMemoryLine(const aNewLine: RawUTF8); override;
    /// returns the name of a given thread, according to the position in the log
    function ThreadName(ThreadID, CurrentLogIndex: integer): RawUTF8;
    /// returns the name of all threads, according to the position in the log
    // - result[0] stores the name of ThreadID = 1
    function ThreadNames(CurrentLogIndex: integer): TRawUTF8DynArray;
    /// returns all days of this log file
    // - only available for low-resolution timestamp, i.e. Freq=0
    procedure GetDays(out Days: TDateTimeDynArray);
    /// returns the number of occurences of a given thread
    function ThreadRows(ThreadID: integer): cardinal;
    /// retrieve the level of an event
    // - is calculated by Create() constructor
    // - EventLevel[] array index is from 0 to Count-1
    property EventLevel: TSynLogInfoDynArray read fLevels;
    /// retrieve all used event levels
    // - is calculated by Create() constructor
    property EventLevelUsed: TSynLogInfos read fLevelUsed;
    /// retrieve the description text of an event
    // - returns '' if supplied index is out of range
    // - see also EventString() function, for direct VCL use
    property EventText[index: integer]: RawUTF8 read GetEventText;
    /// retrieve all event thread IDs
    // - contains something if TSynLogFamily.PerThreadLog was ptIdentifiedInOnFile
    // - for ptMergedInOneFile (default) or ptOneFilePerThread logging process,
    // the array will be void (EventThread=nil)
    property EventThread: TWordDynArray read fThreads;
    /// the number of threads
    property ThreadsCount: cardinal read fThreadMax;
    /// profiled methods information
    // - is calculated by Create() constructor
    // - will contain the sllEnter index, with the associated elapsed time
    // - number of items in the array is retrieved by the LogProcCount property
    property LogProc: PSynLogFileProcArray read fLogProcCurrent;
    /// the current sort order
    property LogProcOrder: TLogProcSortOrder read fLogProcSortInternalOrder;
    /// if the method information must be merged for the same method name
    property LogProcMerged: boolean read fLogProcIsMerged write SetLogProcMerged;
    /// all used event levels, as retrieved at log file content parsing
    property LevelUsed: TSynLogInfos read fLevelUsed;
    /// high-resolution time stamp frequence, as retrieved from log file header
    // - equals 0 if date time resolution, >0 if high-resolution time stamp
    property Freq: Int64 read fFreq;
    /// the row indexes where the day changed
    // - only available for low-resolution timestamp, i.e. Freq=0
    // - if set, contains at least [0] if the whole log is over a single day
    property DayChangeIndex: TIntegerDynArray read fDayChangeIndex;
    /// the number of rows for each DayChangeIndex[] value
    property DayCount: TIntegerDynArray read fDayCount;
    /// custom headers, to be searched as .ini content
    property Headers: RawUTF8 read fHeaders;
    /// the available CPU features, as recognized at program startup
    // - is extracted from the last part of the CPU property text
    // - you could use the overloaded ToText() function to show it in an
    // human-friendly way
    property IntelCPU: TIntelCpuFeatures read fIntelCPU;
  published
    /// the associated executable name (with path)
    // - returns e.g. 'C:\Dev\lib\SQLite3\exe\TestSQL3.exe'
    property ExecutableName: RawUTF8 read fExeName;
    /// the associated executable version
    // - returns e.g. '0.0.0.0'
    property ExecutableVersion: RawUTF8 read fExeVersion;
    /// the associated executable build date and time
    property ExecutableDate: TDateTime read fExeDate;
    /// for a library, the associated instance name (with path)
    // - returns e.g. 'C:\Dev\lib\SQLite3\exe\TestLibrary.dll'
    // - for an executable, will be left void
    property InstanceName: RawUTF8 read fInstanceName;
    /// the computer host name in which the process was running on
    property ComputerHost: RawUTF8 read fHost;
    /// the computer user name who launched the process
    property RunningUser: RawUTF8 read fUser;
    /// the computer CPU in which the process was running on
    // - returns e.g. '1*0-15-1027'
    property CPU: RawUTF8 read fCPU;
    /// the computer Operating System in which the process was running on
    // - equals wUnknown on Linux or BSD - use DetailedOS instead
    property OS: TWindowsVersion read fOS;
    /// the Operating System Service Pack number
    // - not defined on Linux or BSD - use DetailedOS instead
    property ServicePack: integer read fOSServicePack;
    /// if the 32 bit process was running under WOW 64 virtual emulation
    property Wow64: boolean read fWow64;
    /// the computer Operating System in which the process was running on
    // - returns e.g. '2.3=5.1.2600' for Windows XP
    // - under Linux, it will return the full system version, e.g.
    // 'Ubuntu=Linux-3.13.0-43-generic#72-Ubuntu-SMP-Mon-Dec-8-19:35:44-UTC-2014'
    property DetailedOS: RawUTF8 read fOSDetailed;
    /// the associated framework information
    // - returns e.g. 'TSQLLog 1.18.2765 ERTL FTS3'
    property Framework: RawUTF8 read fFramework;
    /// the date and time at which the log file was started
    property StartDateTime: TDateTime read fStartDateTime;
    /// number of profiled methods in this .log file
    // - i.e. number of items in the LogProc[] array
    property LogProcCount: integer read fLogProcCurrentCount;
  end;

  /// used to parse a .log file and process into VCL/LCL/FMX
  // - would handle e.g. selection and search feature
  TSynLogFileView = class(TSynLogFile)
  protected
    fSelected: TIntegerDynArray;
    fSelectedCount: integer;
    fEvents: TSynLogInfos;
    fThreadSelected: TByteDynArray;
    fThreadSelectedMax: integer;
    procedure LoadFromMap(AverageLineLength: integer=32); override;
    function GetThreads(thread: integer): boolean;
    procedure SetThreads(thread: integer; value: boolean);
  public
    /// add a new line to the already parsed content
    // - overriden method would add the inserted index to Selected[]
    procedure AddInMemoryLine(const aNewLine: RawUTF8); override;
    /// search for the next matching TSynLogInfo, from the current row index
    // - returns -1 if no match was found
    function SearchNextEvent(aEvent: TSynLogInfo; aRow: integer): integer;
    /// search for the next matching text, from the current row index
    // - returns -1 if no match was found
    function SearchNextText(const aPattern: RawUTF8; aRow, aDelta: integer): integer;
    /// search for the previous matching text, from the current row index
    // - returns -1 if no match was found
    function SearchPreviousText(const aPattern: RawUTF8; aRow: integer): integer;
    /// search for the matching Enter/Leave item, from the current row index
    // - returns -1 if no match was found
    function SearchEnterLeave(aRow: integer): integer;
    /// search for the next specified thread, from the current row index
    // - returns -1 if no match was found
    function SearchThread(aThreadID: word; aRow: integer): integer;
    /// search for the next diverse thread, from the current row index
    // - returns -1 if no match was found
    function SearchNextThread(aRow: integer): integer;
    /// search for the next matching thread, from the current row index
    // - returns -1 if no match was found
    function SearchNextSameThread(aRow: integer): integer;
    /// search for the next row index, appearing after the supplied item index
    // - returns -1 if no match was found
    function SearchNextSelected(aIndex: integer): integer;
    /// search for the previous matching thread, from the current row index
    // - returns -1 if no match was found
    function SearchPreviousSameThread(aRow: integer): integer;
    /// returns the ready-to-be text of a cell of the main TDrawGrid
    function GetCell(aCol, aRow: integer; out aLevel: TSynLogInfo): string;
    /// returns the ready-to-be displayed text of one or several selected rows
    function GetLineForMemo(aRow,aTop,aBottom: integer): string;
    /// returns the ready-to-be copied text of a selected row
    function GetLineForClipboard(aRow: integer): string;
    /// fill all rows matching Events and Threads[] properties in Selected[]
    // - you may specify the current selected row index, which would return
    // the closest one after the selection has been applied
    function Select(aRow: integer): integer; virtual;
    /// set all Threads[] to a specified value
    procedure SetAllThreads(enabled: boolean);
    /// define the current selection range, according to event kinds
    // - once you have set Events and Threads[], call Select() to fill Selected[]
    property Events: TSynLogInfos read fEvents write fEvents;
    /// define the current selection range, according to a thread ID
    // - here the supplied thread ID starts at 1
    // - once you have set Events and Threads[], call Select() to fill Selected[]
    property Threads[thread: integer]: boolean read GetThreads write SetThreads;
    /// the row indexes of the selected entries
    property Selected: TIntegerDynArray read fSelected;
    /// how many entries are currently stored in Selected[]
    property SelectedCount: integer read fSelectedCount;
  end;

type
  /// a list of lof events families, used to gather events by type
  TSynLogFilter = (
    lfNone,lfAll,lfErrors,lfExceptions,lfProfile,lfDatabase,lfClientServer,
    lfDebug,lfCustom,lfDDD);

  /// syslog message facilities as defined by RFC 3164
  TSyslogFacility = (sfKern, sfUser, sfMail, sfDaemon, sfAuth, sfSyslog, sfLpr,
    sfNews, sfUucp, sfClock, sfAuthpriv, sfFtp, sfNtp, sfAudit, sfAlert, sfCron,
    sfLocal0, sfLocal1, sfLocal2, sfLocal3, sfLocal4, sfLocal5, sfLocal6, sfLocal7);

  /// syslog message severities as defined by RFC 5424
  TSyslogSeverity = (ssEmerg, ssAlert, ssCrit, ssErr, ssWarn, ssNotice, ssInfo, ssDebug);

const
  /// up to 16 TSynLogFamily, i.e. TSynLog children classes can be defined
  MAX_SYNLOGFAMILY = 15;

  /// can be set to TSynLogFamily.Level in order to log all available events
  LOG_VERBOSE: TSynLogInfos = [succ(sllNone)..high(TSynLogInfo)];

  /// contains the logging levels for which stack trace should be dumped
  // - which are mainly exceptions or application errors
  LOG_STACKTRACE: TSynLogInfos = [sllException,sllExceptionOS,
    sllLastError,sllError,sllDDDError];

  /// the text equivalency of each logging level, as written in the log file
  // - PCardinal(@LOG_LEVEL_TEXT[L][3])^ will be used for fast level matching
  // so text must be unique for characters [3..6] -> e.g. 'UST4'
  LOG_LEVEL_TEXT: array[TSynLogInfo] of string[7] = (
    '       ', ' info  ', ' debug ', ' trace ', ' warn  ', ' ERROR ',
    '  +    ', '  -    ',
    ' OSERR ', ' EXC   ', ' EXCOS ', ' mem   ', ' stack ', ' fail  ',
    ' SQL   ', ' cache ', ' res   ', ' DB    ', ' http  ', ' clnt  ', ' srvr  ',
    ' call  ', ' ret   ', ' auth  ',
    ' cust1 ', ' cust2 ', ' cust3 ', ' cust4 ', ' rotat ', ' dddER ', ' dddIN ',
    ' mon   ');

  /// RGB colors corresponding to each logging level
  // - matches the TColor values, as used by the VCL
  LOG_LEVEL_COLORS: array[Boolean,TSynLogInfo] of integer = (
    ($FFFFFF,$DCC0C0,$DCDCDC,$C0C0C0,$8080C0,$8080FF,$C0DCC0,$DCDCC0,
{  sllNone, sllInfo, sllDebug, sllTrace, sllWarning, sllError, sllEnter, sllLeave, }
     $C0C0F0, $C080FF, $C080F0, $C080C0, $C080C0,
{  sllLastError, sllException, sllExceptionOS, sllMemory, sllStackTrace,           }
     $4040FF, $B08080, $B0B080, $8080DC, $80DC80, $DC8080, $DCFF00, $DCD000,
{  sllFail, sllSQL, sllCache, sllResult, sllDB, sllHTTP, sllClient, sllServer,     }
     $DCDC80, $DC80DC, $DCDCDC,
{  sllServiceCall, sllServiceReturn, sllUserAuth,                                  }
     $D0D0D0, $D0D0DC, $D0D0C0, $D0D0E0, $20E0D0, $8080FF, $DCCDCD, $C0C0C0),
{  sllCustom1, sllCustom2, sllCustom3, sllCustom4, sllNewRun, sllDDDError,sllDDDInfo }
    ($000000,$000000,$000000,$000000,$000000,$FFFFFF,$000000,$000000,
     $FFFFFF,$FFFFFF,$FFFFFF,$000000,$000000,
     $FFFFFF,$FFFFFF,$000000,$FFFFFF,$000000,$000000,$000000,$000000,
     $000000,$000000,$000000,
     $000000,$000000,$000000,$000000,$000000,$FFFFFF,$000000,$000000));

  /// console colors corresponding to each logging level
  // - SynCommons' TextColor()
  LOG_CONSOLE_COLORS: array[TSynLogInfo] of TConsoleColor = (
  //    sllNone, sllInfo, sllDebug, sllTrace, sllWarning, sllError, sllEnter, sllLeave
    ccLightGray,ccWhite,ccLightGray,ccLightBlue,ccBrown,ccLightRed,ccGreen,ccGreen,
  //    sllLastError, sllException, sllExceptionOS, sllMemory, sllStackTrace,
    ccLightRed, ccLightRed, ccLightRed, ccLightGray, ccCyan,
  //    sllFail, sllSQL, sllCache, sllResult, sllDB, sllHTTP, sllClient, sllServer,
    ccLightRed, ccBrown, ccBlue, ccLightCyan, ccMagenta, ccCyan, ccLightCyan, ccLightCyan,
  //    sllServiceCall, sllServiceReturn, sllUserAuth,
    ccLightMagenta, ccLightMagenta, ccMagenta,
  //    sllCustom1, sllCustom2, sllCustom3, sllCustom4,
    ccLightGray, ccLightGray,ccLightGray,ccLightGray,
  //    sllNewRun, sllDDDError, sllDDDInfo, sllMonitoring
    ccLightMagenta, ccLightRed, ccWhite, ccLightBlue);

  /// how TLogFilter map TSynLogInfo events
  LOG_FILTER: array[TSynLogFilter] of TSynLogInfos = (
    [], [succ(sllNone)..high(TSynLogInfo)],
    [sllError,sllLastError,sllException,sllExceptionOS],
    [sllException,sllExceptionOS], [sllEnter,sllLeave],
    [sllSQL,sllCache,sllDB], [sllClient,sllServer,sllServiceCall, sllServiceReturn],
    [sllDebug,sllTrace,sllEnter], [sllCustom1..sllCustom4],[sllDDDError,sllDDDInfo]);

  /// the "magic" number used to identify .log.synlz compressed files, as
  // created by TSynLogFamily.EventArchiveSynLZ
  LOG_MAGIC = $ABA51051;

  /// may be used to log as Debug or Error event, depending on an Error: boolean
  LOG_DEBUGERROR: array[boolean] of TSynLogInfo = (sllDebug, sllError);

  /// may be used to log as Trace or Warning event, depending on an Error: boolean
  LOG_TRACEWARNING: array[boolean] of TSynLogInfo = (sllTrace, sllWarning);

  /// may be used to log as Info or Warning event, depending on an Error: boolean
  LOG_INFOWARNING: array[boolean] of TSynLogInfo = (sllInfo, sllWarning);

  /// used to convert a TSynLog event level into a syslog message severity
  LOG_TO_SYSLOG: array[TSynLogInfo] of TSyslogSeverity = (
   ssDebug, ssInfo, ssDebug, ssDebug, ssNotice, ssWarn,
  // sllNone, sllInfo, sllDebug, sllTrace, sllWarning, sllError,
   ssDebug, ssDebug,
  // sllEnter, sllLeave,
  ssWarn, ssErr, ssErr, ssDebug, ssDebug,
  // sllLastError, sllException, sllExceptionOS, sllMemory, sllStackTrace,
  ssNotice, ssDebug, ssDebug, ssDebug, ssDebug, ssDebug, ssDebug, ssDebug,
  // sllFail, sllSQL, sllCache, sllResult, sllDB, sllHTTP, sllClient, sllServer,
  ssDebug, ssDebug, ssDebug,
  // sllServiceCall, sllServiceReturn, sllUserAuth,
  ssDebug, ssDebug, ssDebug, ssDebug, ssNotice,
  // sllCustom1, sllCustom2, sllCustom3, sllCustom4, sllNewRun,
  ssWarn, ssInfo, ssDebug);
  // sllDDDError, sllDDDInfo, sllMonitoring);


/// returns the trimmed text value of a logging level
// - i.e. 'Warning' for sllWarning
function ToText(event: TSynLogInfo): RawUTF8; overload;

/// returns the trimmed text value of a logging levels set
function ToText(events: TSynLogInfos): ShortString; overload;

/// returns the ready-to-be displayed text of a TSynLogInfo value
function ToCaption(event: TSynLogInfo): string; overload;

/// returns the ready-to-be displayed text of a TSynLogFilter value
function ToCaption(filter: TSynLogFilter): string; overload;

/// returns a method event as text, using the .map/.mab information if available
function ToText(const Event: TMethod): RawUTF8; overload;

var
  /// the kind of .log file generated by TSynTestsLogged
  TSynLogTestLog: TSynLogClass = TSynLog;

var
  /// low-level variable used internaly by this unit
  // - do not access this variable in your code: defined here to allow inlining
  GlobalThreadLock: TRTLCriticalSection;

{$ifndef NOEXCEPTIONINTERCEPT}
  /// low-level variable used internaly by this unit
  // - do not access this variable in your code: defined here to allow inlining
  GlobalCurrentHandleExceptionSynLog: TSynLog;
{$endif NOEXCEPTIONINTERCEPT}

type
  /// storage of the information associated with an intercepted exception
  // - as returned by GetLastException() function
  TSynLogExceptionInfo = record
    /// low-level calling context
    // - as used by TSynLogExceptionToStr callbacks
    Context: TSynLogExceptionContext;
    /// associated Exception.Message content (if any)
    Message: string;
    /// ready-to-be-displayed text of the exception address
    Addr: RawUTF8;
  end;
  /// storage of the information associated with one or several exceptions
  // - as returned by GetLastExceptions() function
  TSynLogExceptionInfoDynArray = array of TSynLogExceptionInfo;

/// makes a thread-safe copy of the latest intercepted exception
function GetLastException(out info: TSynLogExceptionInfo): boolean;

/// returns some text about the latest intercepted exception
function GetLastExceptionText: RawUTF8;

/// makes a thread-safe copy of the latest intercepted exceptions
procedure GetLastExceptions(out result: TSynLogExceptionInfoDynArray;
  Depth: integer=0); overload;

{$ifndef NOVARIANTS}
/// returns a TDocVariant array of the latest intercepted exception texts
// - runs ToText() over all information returned by overloaded GetLastExceptions
function GetLastExceptions(Depth: integer=0): variant; overload;
{$endif}

/// convert low-level exception information into some human-friendly text
function ToText(var info: TSynLogExceptionInfo): RawUTF8; overload;

/// a TSynLogArchiveEvent handler which will delete older .log files
function EventArchiveDelete(const aOldLogFileName, aDestinationPath: TFileName): boolean;

/// a TSynLogArchiveEvent handler which will compress older .log files
// using our proprietary SynLZ format
// - resulting file will have the .synlz extension and will be located
// in the aDestinationPath directory, i.e. TSynLogFamily.ArchivePath+'\log\YYYYMM\'
// - use UnSynLZ.dpr tool to uncompress it into .log textual file
// - SynLZ is much faster than zip for compression content, but proprietary
function EventArchiveSynLZ(const aOldLogFileName, aDestinationPath: TFileName): boolean;

/// append some information to a syslog message memory buffer
// - following https://tools.ietf.org/html/rfc5424 specifications
// - ready to be sent via UDP to a syslog remote server
// - returns the number of bytes written to destbuffer (which should have
// destsize > 127)
function SyslogMessage(facility: TSyslogFacility; severity: TSyslogSeverity;
  const msg, procid, msgid: RawUTF8; destbuffer: PUTF8Char; destsize: PtrInt;
  trimmsgfromlog: boolean): PtrInt;

/// check if the supplied file name is a currently working log file
// - may be used to avoid e.g. infinite recursion when monitoring the log file
function IsActiveLogFile(const aFileName: TFileName): boolean;


implementation

{$ifdef FPC}
uses
  SynFPCTypInfo // small wrapper unit around FPC's TypInfo.pp
  {$ifdef Linux}
  , SynFPCLinux, BaseUnix, Unix, Errors, dynlibs
  {$endif} ;
{$endif FPC}

var
  _LogInfoText: array[TSynLogInfo] of RawUTF8;
  _LogInfoCaption: array[TSynLogInfo] of string;

function ToText(event: TSynLogInfo): RawUTF8;
begin
  result := _LogInfoText[event];
end;

function ToText(events: TSynLogInfos): ShortString;
begin
  GetSetNameShort(TypeInfo(TSynLogInfos), events, result, {trimleft=}true);
end;

function ToCaption(event: TSynLogInfo): string;
begin
  result := _LogInfoCaption[event];
end;

function ToCaption(filter: TSynLogFilter): string;
begin
  result := GetCaptionFromEnum(TypeInfo(TSynLogFilter), Ord(filter))
end;


{ TSynLogSettings }

constructor TSynLogSettings.Create;
begin
  inherited Create;
  fDestinationPath := GetSystemPath(spLog);
  fLevels := LOG_STACKTRACE + [sllNewRun];
  fRotateFileCount := 2;
end;

procedure TSynLogSettings.SetLog(aLogClass: TSynLogClass);
var
  f: TSynLogFamily;
begin
  if self = nil then
    exit;
  if aLogClass = nil then
    aLogClass := TSynLog;
  f := aLogClass.Family;
  f.DestinationPath := EnsureDirectoryExists(fDestinationPath);
  f.PerThreadLog := ptIdentifiedInOnFile; // ease multi-threaded server debug
  f.RotateFileCount := fRotateFileCount;
  if fRotateFileCount > 0 then begin
    f.RotateFileSizeKB := 20 * 1024; // rotate by 20 MB logs
    f.FileExistsAction := acAppend;  // as expected in rotation mode
  end
  else
    f.HighResolutionTimestamp := true;
  f.Level := fLevels;
  fLogClass := aLogClass;
end;


{ TSynMapFile }

const
  MAGIC_MAB = $A5A5A5A5;

function MatchPattern(P,PEnd,Up: PUTF8Char; var Dest: PUTF8Char): boolean;
begin
  result := false;
  repeat
    if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
    while NormToUpperAnsi7[P^]=Up^ do begin
      inc(P);
      if P>PEnd then exit;
      inc(Up);
      if (Up^=' ') and (P^ in [#1..' ']) then begin // ignore multiple spaces in P^
        while (P<PEnd) and (P^ in [#1..' ']) do inc(P);
        inc(Up);
      end;
    end;
    if Up^=#0 then // all chars of Up^ found in P^
      break else
    if Up^<>' ' then // P^ and Up^ didn't match
      exit;
    inc(Up);
  until false;
  while (P<PEnd) and (P^=' ') do inc(P); // ignore all spaces
  result := true;
  Dest := P;
end;

procedure ReadSymbol(var R: TFileBufferReader; var A: TDynArray);
var i, n, L: integer;
    S: PSynMapSymbol;
    Addr: cardinal;
    P: PByte;
begin
  n := R.ReadVarUInt32;
  A.Count := n;
  P := R.CurrentMemory;
  if (n=0) or (P=nil) then
    exit;
  S := A.Value^;
  for i := 0 to n-1 do begin
    L := FromVarUInt32(P); // inlined R.Read(S^.Name)
    FastSetString(S^.Name,P,L);
    inc(P,L);
    inc(PByte(S),A.ElemSize); // may be TSynMapSymbol or TSynMapUnit
  end;
  S := A.Value^;
  Addr := FromVarUInt32(P);
  S^.Start := Addr;
  for i := 1 to n-1 do begin
    inc(Addr,FromVarUInt32(P));
    S^.Stop := Addr-1;
    inc(PByte(S),A.ElemSize);
    S^.Start := Addr;
  end;
  S^.Stop := Addr+FromVarUInt32(P);
  R.Seek(PtrUInt(P)-PtrUInt(R.MappedBuffer));
end;

const
  /// Delphi linker starts the code section at this fixed offset
  CODE_SECTION = $1000;

constructor TSynMapFile.Create(const aExeName: TFileName=''; MabCreate: boolean=true);

  procedure LoadMap;
    var P, PEnd: PUTF8Char;
    procedure NextLine;
    begin
      while (P<PEnd) and (P^>=' ') do inc(P);
      if (P<PEnd) and (P^=#13) then inc(P);
      if (P<PEnd) and (P^=#10) then inc(P);
    end;
    function GetCode(var Ptr: integer): boolean;
    begin
      while (P<PEnd) and (P^=' ') do inc(P);
      result := false;
      if (P+10<PEnd) and
         (PInteger(P)^=ord('0')+ord('0')shl 8+ord('0')shl 16+ord('1')shl 24) and
         (P[4]=':') then begin
        if not HexDisplayToBin(PAnsiChar(P)+5,@Ptr,sizeof(Ptr)) then exit;
        while (P<PEnd) and (P^>' ') do inc(P);
        while (P<PEnd) and (P^=' ') do inc(P);
        if P<PEnd then
          result := true;
      end;
    end;
    procedure ReadSegments;
    var Beg: PAnsiChar;
        U: TSynMapUnit;
    begin
      NextLine;
      NextLine;
      while (P<PEnd) and (P^<' ') do inc(P);
      while (P+10<PEnd) and (P^>=' ') do begin
        if GetCode(U.Symbol.Start) and
           HexDisplayToBin(PAnsiChar(P),@U.Symbol.Stop,4) then begin
          while PWord(P)^<>ord('M')+ord('=')shl 8 do
            if P+10>PEnd then exit else inc(P);
          Beg := pointer(P+2);
          while (P<PEnd) and (P^>' ') do inc(P);
          FastSetString(U.Symbol.Name,Beg,P-Beg);
          inc(U.Symbol.Stop,U.Symbol.Start-1);
          if (U.Symbol.Name<>'') and
             ((U.Symbol.Start<>0) or (U.Symbol.Stop<>0)) then
            fUnits.FindHashedAndUpdate(U,{addifnotexisting=}true);
        end;
        NextLine;
      end;
    end;
    procedure ReadSymbols;
    var Beg: PUtf8Char;
        Sym: TSynMapSymbol;
        {$ifdef ISDELPHI2005ANDUP}
        u, l: PtrInt;
        LastUnitUp: RawUTF8; // e.g. 'MORMOT.CORE.DATA.'
        {$endif ISDELPHI2005ANDUP}
    begin
      NextLine;
      NextLine;
      while (P+10<PEnd) and (P^>=' ') do begin
        if GetCode(Sym.Start) then begin
          while (P<PEnd) and (P^=' ') do inc(P);
          Beg := pointer(P);
          while (P<PEnd) and (P^>' ') do inc(P);
          {$ifdef ISDELPHI2005ANDUP}
          // trim left 'UnitName.' for each symbol (since Delphi 2005)
          if (LastUnitUp <> '') and IdemPChar(Beg, pointer(LastUnitUp)) then
            // common case since symbols are grouped by address, i.e. by unit
            inc(Beg, length(LastUnitUp))
          else begin // manual unit name search
            LastUnitUp := '';
            for u := 0 to fUnits.Count - 1 do
              with fUnit[u].Symbol do begin
                l := length(Name);
                if (Beg[l] = '.') and (l > length(LastUnitUp)) and
                   IdemPropNameU(Name, Beg, l) then
                  LastUnitUp := UpperCase(Name); // find longest match
              end;
            if LastUnitUp <> '' then begin
              l := length(LastUnitUp);
              SetLength(LastUnitUp, l + 1);
              LastUnitUp[l] := '.';
              inc(Beg, l + 1);
            end;
          end;
          {$endif ISDELPHI2005ANDUP}
          FastSetString(Sym.Name,Beg,P-Beg);
          if (Sym.Name<>'') and not (Sym.Name[1] in ['$','?']) then
            fSymbols.Add(Sym);
        end;
        NextLine;
      end;
    end;
    procedure ReadLines;
    var Beg: PAnsiChar;
        i, Count, n: integer;
        aName: RawUTF8;
        added: boolean;
        U: ^TSynMapUnit;
    begin
      Beg := pointer(P);
      while P^<>'(' do if P=PEnd then exit else inc(P);
      FastSetString(aName,Beg,P-Beg);
      if aName='' then
        exit;
      i := fUnits.FindHashedForAdding(aName,added);
      U := @fUnit[i];
      if added then
        U^.Symbol.Name := aName; // should not occur, but who knows...
      if U^.FileName='' then begin
        inc(P); Beg := pointer(P);
        while P^<>')' do if P=PEnd then exit else inc(P);
        FastSetString(U^.FileName,Beg,P-Beg);
      end;
      NextLine;
      NextLine;
      n := length(U^.Line);
      Count := n; // same unit may appear multiple times in .map content
      while (P+10<PEnd) and (P^>=' ') do begin
        while (P<PEnd) and (P^=' ') do inc(P);
        repeat
          if Count=n then begin
            n := NextGrow(n);
            SetLength(U^.Line,n);
            SetLength(U^.Addr,n);
          end;
          U^.Line[Count] := GetNextItemCardinal(P,' ');
          if not GetCode(U^.Addr[Count]) then
            break;
          if U^.Addr[Count]<>0 then
            inc(Count); // occured with Delphi 2010 :(
        until (P>=PEnd) or (P^<' ');
        NextLine;
      end;
      SetLength(U^.Line,Count);
      SetLength(U^.Addr,Count);
    end;
  var i, s,u: integer;
      RehashNeeded: boolean;
  begin // LoadMap
    fSymbols.Capacity := 8000;
    with TSynMemoryStreamMapped.Create(fMapFile) do
    try
      // parse .map sections into fSymbol[] and fUnit[]
      P := Memory;
      PEnd := P+Size;
      while P<PEnd do
        if MatchPattern(P,PEnd,'DETAILED MAP OF SEGMENTS',P) then
          ReadSegments else
        if MatchPattern(P,PEnd,'ADDRESS PUBLICS BY VALUE',P) then
          ReadSymbols else
        if MatchPattern(P,PEnd,'LINE NUMBERS FOR',P) then
          ReadLines else
          NextLine;
      // now we should have read all .map content
      s := fSymbols.Count-1;
      RehashNeeded := false;
      for i := fUnits.Count-1 downto 0 do
        with fUnit[i] do
          if (Symbol.Start=0) and (Symbol.Stop=0) then begin
            fUnits.Delete(i); // occurs with Delphi 2010 :(
            RehashNeeded := true;
          end;
      u := fUnits.Count-1;
      if RehashNeeded then
        fUnits.ReHash; // as expected by TDynArrayHashed
      {$ifopt C+}
      for i := 1 to u do
         assert(fUnit[i].Symbol.Start>fUnit[i-1].Symbol.Stop);
      {$endif}
      for i := 0 to s-1 do
        fSymbol[i].Stop := fSymbol[i+1].Start-1;
      if (u>=0) and (s>=0) then
        fSymbol[s].Stop := fUnit[u].Symbol.Stop;
    finally
      Free;
    end;
  end;

  procedure LoadMab(const aMabFile: TFileName);
  var R: TFileBufferReader;
      i: integer;
      S: TCustomMemoryStream;
      MS: TMemoryStream;
  begin
    fMapFile := aMabFile;
    if FileExists(aMabfile) then
    try
      S := TSynMemoryStreamMapped.Create(aMabFile);
      try
        MS := StreamUnSynLZ(S,MAGIC_MAB);
        if MS<>nil then
        try
          R.OpenFrom(MS.Memory,MS.Size);
          ReadSymbol(R,fSymbols);
          ReadSymbol(R,fUnits{$ifdef UNDIRECTDYNARRAY}.InternalDynArray{$endif});
          fUnits.ReHash;
          for i := 0 to fUnits.Count-1 do
          with fUnit[i] do begin
            R.Read(FileName);
            R.ReadVarUInt32Array(Line);
            R.ReadVarUInt32Array(Addr);
          end;
          MabCreate := false;
        finally
          MS.Free;
        end;
      finally
        S.Free;
      end;
    except
      on Exception do; // invalid file -> ignore any problem
    end;
  end;

var i: integer;
    MabFile: TFileName;
    MapAge, MabAge: TDateTime;
    U: RawUTF8;
begin
  fSymbols.Init(TypeInfo(TSynMapSymbolDynArray),fSymbol,@fSymCount);
  fUnits.Init(TypeInfo(TSynMapUnitDynArray),fUnit,nil,nil,nil,@fUnitCount);
  fUnitSynLogIndex := -1;
  fUnitSystemIndex := -1;
  // 1. search for an external .map file matching the running .exe/.dll name
  if aExeName='' then begin
    fMapFile := GetModuleName(hInstance);
    {$ifdef MSWINDOWS}
    fCodeOffset := GetModuleHandle(pointer(ExtractFileName(fMapFile)))+CODE_SECTION;
    {$else}
    {$ifdef KYLIX3}
    fCodeOffset := GetTextStart; // from SysInit.pas
    {$endif}
    {$endif}
  end else
    fMapFile := aExeName;
  fMapFile := ChangeFileExt(fMapFile,'.map');
  MabFile := ChangeFileExt(fMapFile,'.mab');
  GlobalLock;
  try
    MapAge := FileAgeToDateTime(fMapFile);
    MabAge := FileAgeToDateTime(MabFile);
    if (MapAge>0) and (MabAge<MapAge) then
      LoadMap; // if no faster-to-load .mab available and accurate
    // 2. search for a .mab file matching the running .exe/.dll name
    if (fSymCount=0) and (MabAge<>0) then
      LoadMab(MabFile);
    // 3. search for an embedded compressed .mab file appended to the .exe/.dll
    if fSymCount=0 then
      if aExeName='' then
        LoadMab(GetModuleName(hInstance)) else
        LoadMab(aExeName);
    // finalize symbols
    if fSymCount>0 then begin
      for i := 1 to fSymCount-1 do
        assert(fSymbol[i].Start>fSymbol[i-1].Stop);
      SetLength(fSymbol,fSymCount);
      SetLength(fUnit,fUnitCount);
      fSymbols.Init(TypeInfo(TSynMapSymbolDynArray),fSymbol);
      fUnits.Init(TypeInfo(TSynMapUnitDynArray),fUnit);
      if MabCreate then
        SaveToFile(MabFile); // if just created from .map -> create .mab file
      U := 'SynLog';
      fUnitSynLogIndex := fUnits.FindHashed(U);
      U := 'System';
      fUnitSystemIndex := fUnits.FindHashed(U);
      fHasDebugInfo := true;
    end else
      fMapFile := '';
  finally
    GlobalUnLock;
  end;
end;

procedure WriteSymbol(var W: TFileBufferWriter; const A: TDynArray);
var i, n: integer;
    Diff: integer;
    S: PSynMapSymbol;
    P,Beg: PByte;
    tmp: RawByteString;
begin
  n := A.Count;
  W.WriteVarUInt32(n);
  if n=0 then exit;
  S := A.Value^;
  for i := 0 to n-1 do begin
    W.Write(S^.Name);
    inc(PByte(S),A.ElemSize); // may be TSynMapSymbol or TSynMapUnit
  end;
  S := A.Value^;
  Diff := S^.Start;
  W.WriteVarUInt32(Diff);
  P := pointer(W.DirectWritePrepare(n*5,tmp));
  Beg := P;
  for i := 1 to n-1 do begin
    inc(PByte(S),A.ElemSize);
    P := ToVarUInt32(S^.Start-Diff,P);
    Diff := S^.Start;
  end;
  P := ToVarUInt32(S^.Stop-Diff,P);
  W.DirectWriteFlush(PtrUInt(P)-PtrUInt(Beg),tmp);
end;

procedure TSynMapFile.SaveToStream(aStream: TStream);
var W: TFileBufferWriter;
    i: integer;
    MS: TMemoryStream;
begin
  MS := THeapMemoryStream.Create;
  W := TFileBufferWriter.Create(MS,1 shl 20); // 1 MB should be enough at first
  try
    WriteSymbol(W,fSymbols);
    WriteSymbol(W,fUnits{$ifdef UNDIRECTDYNARRAY}.InternalDynArray{$endif});
    for i := 0 to high(fUnit) do
    with fUnit[i] do begin
      W.Write(FileName);
      W.WriteVarUInt32Array(Line,length(Line),wkOffsetI); // not always increasing
      W.WriteVarUInt32Array(Addr,length(Addr),wkOffsetU); // always increasing
    end;
    W.Flush;
    StreamSynLZ(MS,aStream,MAGIC_MAB);
  finally
    MS.Free;
    W.Free;
  end;
end;

procedure TSynMapFile.SaveToJson(W: TTextWriter);
begin
  W.AddShort('{"Symbols":');
  W.AddDynArrayJSON(fSymbols);
  W.AddShort(',"Units":');
  W.AddDynArrayJSON(fUnits);
  W.Add('}');
end;

procedure TSynMapFile.SaveToJson(const aJsonFile: TFileName; aHumanReadable: Boolean);
var S: TFileStream;
    W: TTextWriter;
    json: RawUTF8;
begin
  if aHumanReadable then begin
    W := TTextWriter.CreateOwnedStream(65536);
    try
      SaveToJson(W);
      W.SetText(json);
      JSONBufferReformatToFile(pointer(json),aJsonFile)
    finally
      W.Free;
    end;
  end else begin
    S := TFileStream.Create(aJsonFile,fmCreate);
    try
      W := TTextWriter.Create(S,65536);
      try
        SaveToJson(W);
        W.FlushToStream;
      finally
        W.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

function TSynMapFile.SaveToFile(const aFileName: TFileName=''): TFileName;
var F: TFileStream;
begin
  if aFileName='' then
    result := ChangeFileExt(GetModuleName(hInstance),'.mab') else
    result := aFileName;
  DeleteFile(result);
  F := TFileStream.Create(result,fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TSynMapFile.SaveToExe(const aExeName: TFileName);
var mabfilename: TFileName;
    exe, mab: TMemoryStream;
    exesize, mabsize: PtrUInt;
begin
  if not FileExists(aExeName) then
    exit;
  mabfilename := SaveToFile(ChangeFileExt(aExeName,'.mab'));
  try
    exe := THeapMemoryStream.Create;
    mab := THeapMemoryStream.Create;
    try
      // load both files
      mab.LoadFromFile(mabfilename);
      mabsize := mab.Size;
      exe.LoadFromFile(aExeName);
      exesize := exe.Size;
      if exesize<16 then
        exit;
      // trim existing mab content
      exesize := StreamSynLZComputeLen(exe.Memory,exesize,MAGIC_MAB);
      exe.Size := exesize+mabsize;
      // append mab content to exe
      MoveFast(mab.Memory^,PAnsiChar(exe.Memory)[exesize],mabsize);
      exe.SaveToFile(aExeName);
    finally
      mab.Free;
      exe.Free;
    end;
  finally
    DeleteFile(mabfilename);
  end;
end;

function TSynMapFile.FindSymbol(aAddressOffset: integer): integer;
var L,R: integer;
begin
  R := high(fSymbol);
  L := 0;
  if (R>=0) and
     (aAddressOffset>=fSymbol[0].Start) and
     (aAddressOffset<=fSymbol[R].Stop) then
  repeat
    result := (L+R)shr 1;
    with fSymbol[result] do
      if aAddressOffset<Start then
        R := result-1 else
      if aAddressOffset>Stop then
        L := result+1 else
        exit;
  until L>R;
  result := -1;
end;

function TSynMapFile.FindUnit(aAddressOffset: integer; out LineNumber: integer): integer;
var L,R,n,max: integer;
begin
  LineNumber := 0;
  R := high(fUnit);
  L := 0;
  if (R>=0) and
     (aAddressOffset>=fUnit[0].Symbol.Start) and
     (aAddressOffset<=fUnit[R].Symbol.Stop) then
  repeat
    result := (L+R) shr 1;
    with fUnit[result] do
      if aAddressOffset<Symbol.Start then
        R := result-1 else
      if aAddressOffset>Symbol.Stop then
        L := result+1 else begin
        // unit found -> search line number
        L := 0;
        max := high(Addr);
        R := max;
        if R>=0 then
        repeat
          n := (L+R) shr 1;
          if aAddressOffset<Addr[n] then
            R := n-1 else
          if (n<max) and (aAddressOffset>=Addr[n+1]) then
            L := n+1 else begin
            LineNumber := Line[n];
            exit;
          end;
        until L>R;
        exit;
      end;
  until L>R;
  result := -1;
end;

var
  ExeInstanceMapFile: TSynMapFile;

function GetInstanceMapFile: TSynMapFile;
begin
  if ExeInstanceMapFile=nil then
    GarbageCollectorFreeAndNil(ExeInstanceMapFile,TSynMapFile.Create);
  result := ExeInstanceMapFile;
end;

function ToText(const Event: TMethod): RawUTF8;
begin
  FormatUTF8('% using %(%)', [GetInstanceMapFile.FindLocation(PtrUInt(Event.Code)),
    TObject(Event.Data), Event.Data], result);
end;

function TSynMapFile.AbsoluteToOffset(aAddressAbsolute: PtrUInt): integer;
begin
  if self=nil then
    result := 0 else
    result := PtrInt(aAddressAbsolute)-PtrInt(fCodeOffset);
end;

class procedure TSynMapFile.Log(W: TTextWriter; aAddressAbsolute: PtrUInt;
  AllowNotCodeAddr: boolean);
var {$ifdef FPC}s: ShortString;{$else}u, s, Line, offset: integer;{$endif}
begin
  if (W=nil) or (aAddressAbsolute=0) then
    exit;
  {$ifdef FPC}
  s := BacktraceStrFunc(pointer(aAddressAbsolute));
  if Pos('SynLog.pas',s)=0 then // don't log internal calls
    W.AddShort(s);
  {$else}
  with GetInstanceMapFile do
  if HasDebugInfo then begin
    offset := AbsoluteToOffset(aAddressAbsolute);
    s := FindSymbol(offset);
    u := FindUnit(offset,Line);
    if s<0 then begin
      if u<0 then begin
        if AllowNotCodeAddr then begin
          W.AddBinToHexDisplayMinChars(@aAddressAbsolute,SizeOf(aAddressAbsolute));
          W.Add(' ');
        end;
        exit;
      end;
    end else
      if (u>=0) and (s>=0) and not AllowNotCodeAddr then
        if u=fUnitSynLogIndex  then
          exit else // don't log stack trace internal to SynLog.pas :)
        if (u=fUnitSystemIndex) and (PosEx('Except',Symbols[s].Name)>0) then
          exit; // do not log stack trace of System.SysRaiseException
    W.AddBinToHexDisplayMinChars(@aAddressAbsolute,SizeOf(aAddressAbsolute));
    W.Add(' ');
    if u>=0 then begin
      W.AddString(Units[u].Symbol.Name);
      if s>=0 then
        if Symbols[s].Name=Units[u].Symbol.Name then
          s := -1 else
          W.Add('.');
    end;
    if s>=0 then
      W.AddString(Symbols[s].Name);
    W.Add(' ');
    if Line>0 then begin
      W.Add('(');
      W.Add(Line);
      W.Add(')',' ');
    end;
  end else begin // no .map info available -> display address
    W.AddBinToHexDisplayMinChars(@aAddressAbsolute,SizeOf(aAddressAbsolute));
    W.Add(' ');
  end;
  {$endif FPC}
end;

function TSynMapFile.FindLocation(aAddressAbsolute: PtrUInt): RawUTF8;
var u,s,Line,offset: integer;
begin
  if (self=nil) or (aAddressAbsolute=0) or not HasDebugInfo then begin
    PointerToHex(pointer(aAddressAbsolute),result);
    exit;
  end;
  offset := AbsoluteToOffset(aAddressAbsolute);
  s := FindSymbol(offset);
  u := FindUnit(offset,Line);
  if (s<0) and (u<0) then begin
    {$ifdef FPC} // note: BackTraceStrFunc is much slower than TSynMapFile.Log
    if @BackTraceStrFunc=@SysBackTraceStr then // has debug information?
      PointerToHex(pointer(aAddressAbsolute),result) else
      ShortStringToAnsi7String(BackTraceStrFunc(pointer(aAddressAbsolute)),result);
    {$endif FPC}
    exit;
  end;
  result := result+' ';
  if u>=0 then begin
    result := result+Units[u].Symbol.Name;
    if s>=0 then
      if Symbols[s].Name=result then
        s := -1 else
        result := result+'.';
  end;
  if s>=0 then
    result := result+Symbols[s].Name;
  if Line>0 then
    result := result+' ('+UInt32ToUtf8(Line)+')';
end;

class function TSynMapFile.FindLocation(exc: ESynException): RawUTF8;
begin
  if (exc=nil) or (exc.RaisedAt=nil) then
    result := '' else
    result := GetInstanceMapFile.FindLocation(PtrUInt(exc.RaisedAt));
end;

class function TSynMapFile.FindStackTrace(
  const Ctxt: TSynLogExceptionContext): TRawUTF8DynArray;
var i: PtrInt;
    exe: TSynMapFile;
begin
  result := nil;
  exe := GetInstanceMapFile;
  AddRawUTF8(result,exe.FindLocation(Ctxt.EAddr));
  for i := 0 to Ctxt.EStackCount-1 do
    if (i=0) or (PPtrUIntArray(Ctxt.EStack)[i]<>PPtrUIntArray(Ctxt.EStack)[i-1]) then
      AddRawUTF8(result,exe.FindLocation(PPtrUIntArray(Ctxt.EStack)[i]));
end;

function TSynMapFile.FindUnit(const aUnitName: RawUTF8): integer;
begin
  if (self<>nil) and (aUnitName<>'') then
    for result := 0 to high(fUnit) do
      if IdemPropNameU(fUnit[result].Symbol.Name,aUnitName) then
        exit;
  result := -1;
end;

class function TSynMapFile.FindFileName(const unitname: RawUTF8): TFileName;
var map: TSynMapFile;
    name: RawUTF8;
    u: integer;
begin
  result := '';
  map := GetInstanceMapFile;
  if map = nil then
    exit;
  if unitname='' then
    name := ExeVersion.ProgramName else
    name := unitname;
  u := map.FindUnit(name);
  if u>=0 then
    result := UTF8ToString(map.fUnit[u].FileName);
end;

class function TSynMapFile.FromCurrentExecutable: TSynMapFile;
begin
  result := GetInstanceMapFile;
end;


{ TSynLogFamily }

type
  /// an array to all available per-thread TSynLogFile instances
  TSynLogFileIndex = array[0..MAX_SYNLOGFAMILY] of integer;

var
  /// internal list of registered TSynLogFamily instances
  // - up to MAX_SYNLOGFAMILY+1 families may be defined
  SynLogFamily: TSynObjectList = nil;

  /// internal list of created TSynLog instances, one per each log file on disk
  // - do not use directly - necessary for inlining TSynLogFamily.SynLog method
  // - also used by AutoFlushProc() to get a global list of TSynLog instances
  SynLogFileList: TSynObjectListLocked = nil;

threadvar
  /// each thread can access to its own TSynLogFile
  // - is used to implement TSynLogFamily.PerThreadLog=ptOneFilePerThread option
  // - the current TSynLogFile instance of the living thread is
  // ! SynLogFileList[SynLogFileIndexThreadVar[TSynLogFamily.Ident]-1]
  SynLogFileIndexThreadVar: TSynLogFileIndex;
  /// each thread can have exceptions interception disabled
  // - as set by TSynLogFamily.ExceptionIgnoreCurrentThread property
  ExceptionIgnorePerThread: boolean;

/// if defined, will use AddVectoredExceptionHandler() API call
// - this one does not produce accurate stack trace by now, and is supported
// only since Windows XP
// - so default method using RTLUnwindProc should be prefered with Delphi
{.$define WITH_VECTOREXCEPT}

function ToText(var info: TSynLogExceptionInfo): RawUTF8;
begin
  with info.Context do
    if ELevel<>sllNone then begin
      if info.Addr='' then
        info.Addr := GetInstanceMapFile.FindLocation(EAddr);
      FormatUTF8('% % at %: % [%]',[_LogInfoCaption[ELevel],EClass,info.Addr,
        DateTimeToIso8601Text(UnixTimeToDateTime(ETimestamp),' '),
        StringToUTF8(info.Message)],result);
    end else
      result := '';
end;

function GetLastExceptionText: RawUTF8;
var info: TSynLogExceptionInfo;
begin
  if GetLastException(info) then
    result := ToText(info) else
    result := '';
end;

{$ifndef NOVARIANTS}
function GetLastExceptions(Depth: integer): variant;
var info: TSynLogExceptionInfoDynArray;
    i: integer;
begin
  VarClear(result);
  GetLastExceptions(info,Depth);
  if info=nil then
    exit;
  TDocVariantData(result).InitFast(length(info),dvArray);
  for i := 0 to high(info) do
    TDocVariantData(result).AddItemText(ToText(info[i]));
end;
{$endif}

function PrintUSAscii(P: PUTF8Char; const text: RawUTF8): PUTF8Char;
var i: PtrInt;
begin
  P^ := ' ';
  inc(P);
  for i := 1 to length(text) do
    if ord(text[i]) in [33..126] then begin // only printable ASCII chars
      P^ := text[i];
      inc(P);
    end;
  if P[-1]=' ' then begin
    P^ := '-'; // nothing appended -> NILVALUE
    inc(P);
  end;
  result := P;
end;

function SyslogMessage(facility: TSyslogFacility; severity: TSyslogSeverity;
  const msg, procid, msgid: RawUTF8; destbuffer: PUTF8Char; destsize: PtrInt;
  trimmsgfromlog: boolean): PtrInt;
var P: PAnsiChar;
    start: PUTF8Char;
    len: PtrInt;
    st: TSynSystemTime;
begin
  result := 0;
  if destsize<127 then
    exit;
  start := destbuffer;
  destbuffer^ := '<';
  destbuffer := AppendUInt32ToBuffer(destbuffer+1,ord(severity)+ord(facility) shl 3);
  PInteger(destbuffer)^ := ord('>')+ord('1')shl 8+ord(' ')shl 16; // VERSION=1
  inc(destbuffer,3);
  st.FromNowUTC;
  DateToIso8601PChar(destbuffer,true,st.Year,st.Month,st.Day);
  TimeToIso8601PChar(destbuffer+10,true,st.Hour,st.Minute,st.Second,st.MilliSecond,'T',true);
  destbuffer[23] := 'Z';
  inc(destbuffer,24);
  with ExeVersion do begin
    if length(Host)+length(ProgramName)+length(procid)+length(msgid)+
       (destbuffer-start)+15>destsize then
      exit; // avoid buffer overflow
    destbuffer := PrintUSAscii(destbuffer,Host); // HOST
    destbuffer := PrintUSAscii(destbuffer,ProgramName); // APP-NAME
  end;
  destbuffer := PrintUSAscii(destbuffer,procid); // PROCID
  destbuffer := PrintUSAscii(destbuffer,msgid);  // MSGID
  destbuffer := PrintUSAscii(destbuffer,'');     // no STRUCTURED-DATA
  destbuffer^ := ' ';
  inc(destbuffer);
  len := length(msg);
  P := pointer(msg);
  if trimmsgfromlog and (len>27) then
    if (P[0]='2') and (P[8]=' ') then begin
      inc(P,27); // trim e.g. '20160607 06442255  ! trace '
      dec(len,27);
    end else
    if SynCommons.HexToBin(P,nil,8) then begin
      inc(P,25); // trim e.g. '00000000089E5A13  " info '
      dec(len,25);
    end;
  while (len>0) and (P^<=' ') do begin // trim left spaces
    inc(P);
    dec(len);
  end;
  len := Utf8TruncatedLength(P,len,destsize-(destbuffer-start)-3);
  if not IsAnsiCompatible(P,len) then begin
    PInteger(destbuffer)^ := $bfbbef; // UTF-8 BOM
    inc(destbuffer,3);
  end;
  MoveFast(P^,destbuffer^,len);
  result := (destbuffer-start)+len;
end;

function IsActiveLogFile(const aFileName: TFileName): boolean;
var i: PtrInt;
    one: ^TSynLog;
    files: TSynObjectListLocked;
begin
  result := true;
  files := SynLogFileList;
  files.Safe.Lock;
  try
    one := pointer(files.List);
    for i := 1 to files.Count do
      if {$ifdef MSWINDOWS}CompareText(one^.FileName,aFileName)=0{$else}
         one^.FileName=aFileName{$endif} then
        exit else
        inc(one);
  finally
    files.Safe.UnLock;
  end;
  result := false;
end;

{$ifdef NOEXCEPTIONINTERCEPT}

function GetLastException(out info: TSynLogExceptionInfo): boolean;
begin
  result := false;
end;

procedure GetLastExceptions(out result: TSynLogExceptionInfoDynArray;
  Depth: integer);
begin
end;

{$else}

{$ifdef DELPHI5OROLDER}
  {$define WITH_PATCHEXCEPT}
{$endif}

{$ifdef KYLIX3}
  // Kylix has a totally diverse exception scheme
  {$define WITH_MAPPED_EXCEPTIONS}
{$endif}

{$ifdef FPC}
  {$ifdef WIN64}
    {$define WITH_VECTOREXCEPT} // use AddVectoredExceptionHandler Win64 API
  {$else}
    // Win32, Linux: intercept via the RaiseProc global variable
    {$define WITH_RAISEPROC}
  {$endif}
{$else}
  {$ifdef CPU64}
    {$define WITH_VECTOREXCEPT}
  {$endif}
{$endif}

const
  MAX_EXCEPTHISTORY = 15;
type
  TSynLogExceptionInfos = array[0..MAX_EXCEPTHISTORY] of TSynLogExceptionInfo;
var
  GlobalCurrentHandleExceptionHooked: boolean;
  GlobalLastException: TSynLogExceptionInfos;
  GlobalLastExceptionIndex: integer = -1;

function GetLastException(out info: TSynLogExceptionInfo): boolean;
begin
  if GlobalLastExceptionIndex<0 then begin
    result := false;
    exit; // no exception intercepted yet
  end;
  EnterCriticalSection(GlobalThreadLock);
  try
    info := GlobalLastException[GlobalLastExceptionIndex];
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
  info.Context.EInstance := nil; // avoid any GPF
  info.Context.EStack := nil;
  result := info.Context.ELevel<>sllNone;
end;

procedure GetLastExceptions(out result: TSynLogExceptionInfoDynArray;
  Depth: integer);
var infos: TSynLogExceptionInfos; // use thread-safe local copy
    index,last,n,i: integer;
begin
  if GlobalLastExceptionIndex<0 then
    exit; // no exception intercepted yet
  EnterCriticalSection(GlobalThreadLock);
  try
    infos := GlobalLastException;
    index := GlobalLastExceptionIndex;
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
  n := MAX_EXCEPTHISTORY+1;
  if (Depth>0) and (n>Depth) then
    n := Depth;
  SetLength(result,n);
  last := MAX_EXCEPTHISTORY;
  for i := 0 to n-1 do begin
    if i<=index then
      result[i] := infos[index-i] else begin
      result[i] := infos[last];
      dec(last);
    end;
    with result[i].Context do
      if ELevel=sllNone then begin
        SetLength(result,i); // truncate to latest available exception
        break;
      end else begin
        EInstance := nil; // avoid any GPF
        EStack := nil;
      end;
  end;
end;

// this is the main entry point for all intercepted exceptions
procedure SynLogException(const Ctxt: TSynLogExceptionContext);
  function GetHandleExceptionSynLog: TSynLog;
  var files: TSynObjectListLocked;
      Index: ^TSynLogFileIndex;
      i: PtrInt;
      ndx, n: cardinal;
  begin
    result := nil;
    files := SynLogFileList;
    if files.Count=0 then begin // no log content yet
      for i := 0 to SynLogFamily.Count-1 do
        with TSynLogFamily(SynLogFamily.List[i]) do
        if fHandleExceptions then begin
          result := SynLog;
          exit;
        end;
    end else begin
      files.Safe.Lock;
      try
        Index := @SynLogFileIndexThreadVar;
        n := files.Count;
        for i := 0 to high(Index^) do begin
          ndx := Index^[i]-1;
          if ndx<=n then begin
            result := files.List[ndx];
            if result.fFamily.fHandleExceptions then
              exit;
          end;
        end;
        for i := 0 to n-1 do begin
          result := files.List[i];
          if result.fFamily.fHandleExceptions then
            exit;
        end;
        result := nil;
      finally
        files.Safe.UnLock;
      end;
    end;
  end;
var log: TSynLog;
    info: ^TSynLogExceptionInfo;
    locked: boolean;
    {$ifdef FPC}i: PtrInt;{$endif}
label adr,fin;
begin
  if ExceptionIgnorePerThread then
    exit;
  {$ifdef CPU64DELPHI} // Delphi<XE6 in System.pas to retrieve x64 dll exit code
  {$ifndef ISDELPHIXE6}
  if (Ctxt.EInstance<>nil) and // Ctxt.EClass is EExternalException
     (PShortString(PPointer(PPtrInt(Ctxt.EInstance)^+vmtClassName)^)^=
      '_TExitDllException') then
    exit;
  {$endif ISDELPHIXE6}
  {$endif CPU64DELPHI}
  log := GlobalCurrentHandleExceptionSynLog;
  if (log=nil) or not log.fFamily.fHandleExceptions then
    log := GetHandleExceptionSynLog;
  if (log=nil) or not (Ctxt.ELevel in log.fFamily.Level) then
    exit;
  if (Ctxt.EClass=ESynLogSilent) or
     (log.fFamily.ExceptionIgnore.IndexOf(Ctxt.EClass)>=0) then
    exit;
  locked := false;
  try
    if Assigned(log.fFamily.OnBeforeException) then begin
      log.LockAndGetThreadContext; // protect and set fThreadContext
      locked := true;
      if log.fFamily.OnBeforeException(Ctxt,log.fThreadContext^.ThreadName) then
        exit;
    end;
    if log.LogHeaderLock(Ctxt.ELevel,locked) then begin
      locked := true;
      if GlobalLastExceptionIndex=MAX_EXCEPTHISTORY then
        GlobalLastExceptionIndex := 0 else
        inc(GlobalLastExceptionIndex);
      info := @GlobalLastException[GlobalLastExceptionIndex];
      info^.Context := Ctxt;
      {$ifdef FPC}
      if @BackTraceStrFunc<>@SysBackTraceStr then
        ShortStringToAnsi7String(BackTraceStrFunc(pointer(Ctxt.EAddr)),info^.Addr) else
      {$endif FPC}
        info^.Addr := '';
      if (Ctxt.ELevel=sllException) and (Ctxt.EInstance<>nil) then begin
        info^.Message := Ctxt.EInstance.Message;
        if Ctxt.EInstance.InheritsFrom(ESynException) then begin
          ESynException(Ctxt.EInstance).RaisedAt := pointer(Ctxt.EAddr);
          if ESynException(Ctxt.EInstance).CustomLog(log.fWriter,Ctxt) then
            goto fin;
          goto adr;
        end;
      end else
        info^.Message := '';
      if Assigned(DefaultSynLogExceptionToStr) and
         DefaultSynLogExceptionToStr(log.fWriter,Ctxt) then
        goto fin;
adr:  log.fWriter.Add(' [%] at ',[log.fThreadContext^.ThreadName],twOnSameLine);
      {$ifdef FPC} // note: BackTraceStrFunc is much slower than TSynMapFile.Log
      with log.fWriter do
      if @BackTraceStrFunc=@SysBackTraceStr then begin // no debug information
        AddPointer(Ctxt.EAddr); // write addresses as hexa
        for i := 0 to Ctxt.EStackCount-1 do
          if (i=0) or (Ctxt.EStack[i]<>Ctxt.EStack[i-1]) then begin
            Add(' ');
            AddPointer(Ctxt.EStack[i]);
          end;
      end else begin
        AddString(info^.Addr);
        for i := 0 to Ctxt.EStackCount-1 do
          if (i=0) or (Ctxt.EStack[i]<>Ctxt.EStack[i-1]) then
            AddShort(BackTraceStrFunc(pointer(Ctxt.EStack[i])));
      end;
      {$else}
      TSynMapFile.Log(log.fWriter,Ctxt.EAddr,true);
      {$ifndef WITH_VECTOREXCEPT} // stack frame OK for RTLUnwindProc by now
      log.AddStackTrace(Ctxt.EStack);
      {$endif}
      {$endif FPC}
fin:  log.fWriter.AddEndOfLine(log.fCurrentLevel);
      log.fWriter.FlushToStream; // we expect exceptions to be available on disk
    end;
  finally
    if locked then begin
      GlobalCurrentHandleExceptionSynLog := log.fThreadHandleExceptionBackup;
      LeaveCriticalSection(GlobalThreadLock);
    end;
  end;
end;

{$ifdef WITH_PATCHEXCEPT}

var
  // Delphi 5 doesn't define the needed RTLUnwindProc variable :(
  // so we will patch the System.pas RTL in-place
  RTLUnwindProc: Pointer;

procedure PatchCallRtlUnWind;
procedure Patch(P: PAnsiChar);
{   004038B6 52               push edx  // Save exception object
    004038B7 51               push ecx  // Save exception address
    004038B8 8B542428         mov edx,[esp+$28]
    004038BC 83480402         or dword ptr [eax+$04],$02
    004038C0 56               push esi  // Save handler entry
    004038C1 6A00             push $00
    004038C3 50               push eax
    004038C4 68CF384000       push $004038cf  // @@returnAddress
    004038C9 52               push edx
    004038CA E88DD8FFFF       call RtlUnwind
    ...
RtlUnwind:
    0040115C FF255CC14100     jmp dword ptr [$0041c15c]
    where $0041c15c is a pointer to the address of RtlUnWind in kernel32.dll
    -> we will replace [$0041c15c] by [RTLUnwindProc]    }
var i: Integer;
    addr: PAnsiChar;
begin
  for i := 0 to 31 do
    if (PCardinal(P)^=$6850006a) and  // push 0; push eax; push @@returnAddress
       (PWord(P+8)^=$E852) then begin // push edx; call RtlUnwind
      inc(P,10); // go to call RtlUnwind address
      if PInteger(P)^<0 then begin
        addr := P+4+PInteger(P)^;
        if PWord(addr)^=$25FF then begin // jmp dword ptr []
          PatchCodePtrUInt(Pointer(addr+2),cardinal(@RTLUnwindProc));
          exit;
        end;
      end;
    end else
    inc(P);
end;
asm
  mov eax,offset System.@HandleAnyException+200
  call Patch
end;

// the original unwider function, from the Windows API
procedure oldUnWindProc; external kernel32 name 'RtlUnwind';

{$endif WITH_PATCHEXCEPT}

{$ifdef WITH_MAPPED_EXCEPTIONS} // Kylix specific exception handling

{$W-} // disable stack frame generation (duplicate from Synopse.inc)

threadvar
  CurrentTopOfStack: Cardinal;

procedure ComputeCurrentTopOfStack;
const UNWINDFI_TOPOFSTACK  = $BE00EF00; // from SysInit.pas
var top: cardinal;
begin
  asm
    mov top,esp
  end;
  top := (top and (not 3))+4;
  try
    while PCardinal(top)^<>UNWINDFI_TOPOFSTACK do
      inc(top,4);
  except
  end;
  CurrentTopOfStack := top;
end;

function IsBadReadPtr(addr: pointer; len: integer): boolean;
begin
  try
    asm
      mov eax,addr
      mov ecx,len
 @s:  mov dl,[eax]
      inc eax
      dec ecx
      jnz @s
 @e:end;
    result := false; // if we reached here, everything is ok
  except
    result := true;
  end;
end;

// types and constants from from System.pas / unwind.h

type
  PInternalUnwindException = ^TInternalUnwindException;
  TInternalUnwindException = packed record
    exception_class: LongWord;
    exception_cleanup: Pointer;
    private_1: pointer;
    private_2: LongWord;
  end;

  PInternalRaisedException = ^TInternalRaisedException;
  TInternalRaisedException = packed record
    RefCount: Integer;
    ExceptObject: Exception;
    ExceptionAddr: PtrUInt;
    HandlerEBP: LongWord;
    Flags: LongWord;
    Cleanup: Pointer;
    Prev: PInternalRaisedException;
    ReleaseProc: Pointer;
  end;

const
  Internal_UW_EXC_CLASS_BORLANDCPP = $FBEE0001;
  Internal_UW_EXC_CLASS_BORLANDDELPHI = $FBEE0101;
  Internal_excIsBeingHandled = $00000001;
  Internal_excIsBeingReRaised = $00000002;

var oldUnwinder, newUnwinder: TUnwinder;

function HookedRaiseException(Exc: Pointer): LongBool; cdecl;
var ExcRec: PInternalRaisedException;
    Ctxt: TSynLogExceptionContext;
begin
  if GlobalCurrentHandleExceptionSynLog<>nil then
    if Exc<>nil then begin
      Ctxt.ECode := PInternalUnwindException(Exc)^.exception_class;
      case Ctxt.ECode of
      Internal_UW_EXC_CLASS_BORLANDDELPHI: begin
        ExcRec := PInternalUnwindException(Exc)^.private_1;
        if (ExcRec<>nil) and (ExcRec^.ExceptObject<>nil) then begin
          Ctxt.EInstance := ExcRec^.ExceptObject;
          Ctxt.EClass := PPointer(Ctxt.EInstance)^;
          if Ctxt.EInstance is EExternal then begin
            Ctxt.EAddr := EExternal(Ctxt.EInstance).ExceptionAddress;
            Ctxt.ELevel := sllExceptionOS;
          end else begin
            Ctxt.EAddr := ExcRec^.ExceptionAddr;
            Ctxt.ELevel := sllException;
          end;
          Ctxt.EStack := nil;
          Ctxt.EStackCount := 0;
          Ctxt.ETimestamp := UnixTimeUTC; // very fast API call
          SynLogException(Ctxt);
        end;
        // (ExcRec^.Flags and Internal_excIsBeingHandled)<>0)
        // (ExcRec^.Flags and Internal_excIsBeingReRaised)<>0)
      end;
      Internal_UW_EXC_CLASS_BORLANDCPP: ; // not handled
      end;
    end;
  if Assigned(oldUnwinder.RaiseException) then
    result := oldUnwinder.RaiseException(Exc) else
    result := false;
end;

{$else}

{$ifndef WITH_RAISEPROC}
// "regular" exception handling as defined in System.pas
type
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = record
    ExceptionCode: DWord;
    ExceptionFlags: DWord;
    OuterException: PExceptionRecord;
    ExceptionAddress: PtrUInt;
    NumberParameters: Longint;
    case {IsOsException:} Boolean of
    True:  (ExceptionInformation : array [0..14] of PtrUInt);
    False: (ExceptAddr: PtrUInt; ExceptObject: Exception);
  end;
  GetExceptionClass = function(const P: TExceptionRecord): ExceptClass;

const
  cDelphiExcept = $0EEDFAE0;
  cDelphiException = {$ifdef FPC}$E0465043{$else}$0EEDFADE{$endif};
{$endif WITH_RAISEPROC}
{$endif WITH_MAPPED_EXCEPTIONS}

{$ifdef MSWINDOWS}
const
  // see http://msdn.microsoft.com/en-us/library/xcb2z8hs
  cSetThreadNameException = $406D1388;

  DOTNET_EXCEPTIONNAME: array[0..83] of RawUTF8 = (
  'Access', 'AmbiguousMatch', 'appdomainUnloaded', 'Application', 'Argument',
  'ArgumentNull', 'ArgumentOutOfRange', 'Arithmetic', 'ArrayTypeMismatch',
  'BadImageFormat', 'CannotUnloadappdomain', 'ContextMarshal', 'Cryptographic',
  'CryptographicUnexpectedOperation', 'CustomAttributeFormat', 'DirectoryNotFound',
  'DirectoryNotFound', 'DivideByZero', 'DllNotFound', 'DuplicateWaitObject',
  'EndOfStream', 'EntryPointNotFound', '', 'ExecutionEngine', 'External',
  'FieldAccess', 'FileLoad', 'FileLoad', 'FileNotFound', 'Format',
  'IndexOutOfRange', 'InvalidCast', 'InvalidComObject', 'InvalidFilterCriteria',
  'InvalidOleVariantType', 'InvalidOperation', 'InvalidProgram', 'IO',
  'IsolatedStorage', 'MarshalDirective', 'MethodAccess', 'MissingField',
  'MissingManifestResource', 'MissingMember', 'MissingMethod',
  'MulticastNotSupported', 'NotFiniteNumber', 'NotImplemented', 'NotSupported',
  'NullReference', 'OutOfMemory', 'Overflow', 'PlatformNotSupported', 'Policy',
  'Rank', 'ReflectionTypeLoad', 'Remoting', 'RemotingTimeout', 'SafeArrayTypeMismatch',
  'SafeArrayRankMismatch', 'Security', 'SEH', 'Serialization', 'Server', 'StackOverflow',
  'SUDSGenerator', 'SUDSParser', 'SynchronizationLock', 'System', 'Target',
  'TargetInvocation', 'TargetParameterCount', 'ThreadAbort', 'ThreadInterrupted',
  'ThreadState', 'ThreadStop', 'TypeInitialization', 'TypeLoad', 'TypeUnloaded',
  'UnauthorizedAccess', 'InClassConstructor', 'KeyNotFound', 'InsufficientStack',
  'InsufficientMemory');
  // http://blogs.msdn.com/b/yizhang/archive/2010/12/17/interpreting-hresults-returned-from-net-clr-0x8013xxxx.aspx
  DOTNET_EXCEPTIONHRESULT: array[0..83] of cardinal = (
   $8013151A, $8000211D, $80131015, $80131600, $80070057, $80004003, $80131502,
   $80070216, $80131503, $8007000B, $80131015, $80090020, $80004001, $80131431,
   $80131537, $80070003, $80030003, $80020012, $80131524, $80131529, $801338,
   $80131522, $80131500, $80131506, $80004005, $80131507, $80131621, $80131018,
   $80070002, $80131537, $80131508, $80004002, $80131527, $80131601, $80131531,
   $80131509, $8013153A, $80131620, $80131450, $80131535, $80131510, $80131511,
   $80131532, $80131512, $80131513, $80131514, $80131528, $80004001, $80131515,
   $80004003, $8007000E, $80131516, $80131539, $80131416, $80131517,
   $80131602, $8013150B, $8013150B, $80131533, $80131538, $8013150A, $80004005,
   $8013150C, $8013150E, $800703E9, $80131500, $80131500, $80131518, $80131501,
   $80131603, $80131604, $80138002, $80131530, $80131519, $80131520, $80131521,
   $80131534, $80131522, $80131013, $80070005, $80131543, $80131577, $80131578,
   $8013153D);

type
  // avoid linking of ComObj.pas just for EOleSysError
  EOleSysError = class(Exception)
  public
    ErrorCode: cardinal;
  end;

function ExceptionInheritsFrom(E: TClass; const Name: ShortString): boolean;
begin // avoid linking of ComObj.pas just for EOleSysError
  while (E<>nil) and (E<>Exception) do
    if IdemPropName(PShortString(PPointer(PtrInt(E)+vmtClassName)^)^,Name) then begin
      result := true;
      exit;
    end else
      E := GetClassParent(E);
  result := false;
end;

{$endif MSWINDOWS}

function InternalDefaultSynLogExceptionToStr(
  WR: TTextWriter; const Context: TSynLogExceptionContext): boolean;
{$ifdef MSWINDOWS}
var i: integer;
    code: cardinal;
{$endif}
begin
  WR.AddClassName(Context.EClass);
  if (Context.ELevel=sllException) and (Context.EInstance<>nil) and
     (Context.EClass<>EExternalException) then begin
    {$ifdef MSWINDOWS}
    if ExceptionInheritsFrom(Context.EClass,'EOleSysError') then begin
      WR.Add(' ');
      code := EOleSysError(Context.EInstance).ErrorCode;
      WR.AddPointer(code);
      for i := 0 to high(DOTNET_EXCEPTIONHRESULT) do
        if DOTNET_EXCEPTIONHRESULT[i]=code then begin
          WR.AddShort(' [.NET/CLR unhandled ');
          WR.AddString(DOTNET_EXCEPTIONNAME[i]);
          WR.AddShort('Exception]');
        end; // no break on purpose, if ErrorCode matches more than one Exception
    end;
    {$endif}
    WR.Add(' ');
    if (WR.ClassType=TTextWriter) or
       not Context.EInstance.InheritsFrom(ESynException) then begin
      WR.AddShort('("');
      WR.AddJSONEscapeString(Context.EInstance.Message);
      WR.AddShort('")');
    end else
       WR.WriteObject(Context.EInstance);
  end else
  if Context.ECode<>0 then begin
    WR.AddShort(' (');
    WR.AddPointer(Context.ECode);
    WR.AddShort(')');
  end;
  result := false; // caller should append "at EAddr" and the stack trace
end;

{$ifdef WITH_RAISEPROC}
var
  OldRaiseProc : TExceptProc;

procedure SynRaiseProc(Obj: TObject; Addr: CodePointer; FrameCount: Longint; Frame: PCodePointer);
var Ctxt: TSynLogExceptionContext;
    LastError: DWORD;
begin
  if GlobalCurrentHandleExceptionSynLog<>nil then
    if (Obj<>nil) and (Obj.InheritsFrom(Exception)) then begin
      LastError := GetLastError;
      Ctxt.EClass := PPointer(Obj)^;
      Ctxt.EInstance := Exception(Obj);
      Ctxt.EAddr := PtrUInt(Addr);
      if Obj.InheritsFrom(EExternal) then
        Ctxt.ELevel := sllExceptionOS else
        Ctxt.ELevel := sllException;
      Ctxt.ETimestamp := UnixTimeUTC;
      Ctxt.EStack := pointer(Frame);
      Ctxt.EStackCount := FrameCount;
      SynLogException(Ctxt);
      SetLastError(LastError); // SynLogException() above may have changed this
    end;
  if Assigned(OldRaiseProc) then
    OldRaiseProc(Obj, Addr, FrameCount, Frame);
end;
{$else}
{$ifndef WITH_PATCHEXCEPT}
{$ifndef WITH_MAPPED_EXCEPTIONS}

procedure LogExcept(stack: PPtrUInt; const Exc: TExceptionRecord);
var Ctxt: TSynLogExceptionContext;
    LastError: DWORD;
begin
  {$ifdef MSWINDOWS}
  if Exc.ExceptionCode=cSetThreadNameException then
    exit;
  {$endif}
  LastError := GetLastError;
  Ctxt.ECode := Exc.ExceptionCode;
  if (Exc.ExceptionCode=cDelphiException) and (Exc.ExceptObject<>nil) then begin
    if Exc.ExceptObject.InheritsFrom(Exception) then
      Ctxt.EClass := PPointer(Exc.ExceptObject)^ else
      Ctxt.EClass := EExternalException;
    Ctxt.EInstance := Exc.ExceptObject;
    Ctxt.ELevel := sllException;
    Ctxt.EAddr := Exc.ExceptAddr;
  end else begin
    {$ifdef MSWINDOWS}
    if Assigned(ExceptClsProc) then
      Ctxt.EClass := GetExceptionClass(ExceptClsProc)(Exc) else
    {$endif}
      Ctxt.EClass := EExternal;
    Ctxt.EInstance := nil;
    Ctxt.ELevel := sllExceptionOS;
    Ctxt.EAddr := Exc.ExceptionAddress;
  end;
  Ctxt.EStack := stack;
  Ctxt.EStackCount := 0;
  Ctxt.ETimestamp := UnixTimeUTC; // fast API call
  SynLogException(Ctxt);
  SetLastError(LastError); // code above could have changed this
end;

{$ifdef WITH_VECTOREXCEPT}

type
  PExceptionInfo = ^TExceptionInfo;
  TExceptionInfo = packed record
    ExceptionRecord: PExceptionRecord;
    ContextRecord: pointer;
  end;

var
  AddVectoredExceptionHandler: function(FirstHandler: cardinal;
    VectoredHandler: pointer): PtrInt; stdcall;

function SynLogVectoredHandler(ExceptionInfo : PExceptionInfo): PtrInt; stdcall;
const EXCEPTION_CONTINUE_SEARCH = 0;
begin
  if GlobalCurrentHandleExceptionSynLog<>nil then
    LogExcept(nil,ExceptionInfo^.ExceptionRecord^);
  result := EXCEPTION_CONTINUE_SEARCH;
end;

{$else WITH_VECTOREXCEPT}

var oldUnWindProc: pointer;

procedure SynRtlUnwind(TargetFrame, TargetIp: pointer;
  ExceptionRecord: PExceptionRecord; ReturnValue: Pointer); stdcall;
asm
  pushad
  cmp  dword ptr GlobalCurrentHandleExceptionSynLog,0
  jz   @oldproc
  mov  eax,TargetFrame
  mov  edx,ExceptionRecord
  call LogExcept
@oldproc:
  popad
  pop ebp // hidden push ebp at asm level
  jmp oldUnWindProc
end;

{$endif WITH_VECTOREXCEPT}
{$endif WITH_MAPPED_EXCEPTIONS}
{$endif WITH_PATCHEXCEPT}
{$endif WITH_RAISEPROC}

{$endif NOEXCEPTIONINTERCEPT}


procedure TSynLogFamily.SetDestinationPath(const value: TFileName);
begin
  if value='' then
    fDestinationPath := ExeVersion.ProgramFilePath else
    fDestinationPath := IncludeTrailingPathDelimiter(value);
end;

procedure TSynLogFamily.SetLevel(aLevel: TSynLogInfos);
begin
  // ensure BOTH Enter+Leave are always selected at once, if any is set
  if sllEnter in aLevel then
    include(aLevel,sllLeave) else
  if sllLeave in aLevel then
    include(aLevel,sllEnter);
  fLevel := aLevel;
{$ifndef NOEXCEPTIONINTERCEPT}
  // intercept exceptions, if necessary
  fHandleExceptions := (sllExceptionOS in aLevel) or (sllException in aLevel);
  if fHandleExceptions and (GlobalCurrentHandleExceptionSynLog=nil) then begin
    SynLog; // force GlobalCurrentHandleExceptionSynLog definition
    if not GlobalCurrentHandleExceptionHooked then begin
      GlobalCurrentHandleExceptionHooked := true;
      {$ifdef WITH_MAPPED_EXCEPTIONS}
      GetUnwinder(oldUnwinder);
      newUnwinder := oldUnwinder;
      newUnwinder.RaiseException := HookedRaiseException;
      SetUnwinder(newUnwinder);
      {$else}
      {$ifdef WITH_VECTOREXCEPT}
      AddVectoredExceptionHandler :=
        GetProcAddress(GetModuleHandle(kernel32),'AddVectoredExceptionHandler');
      // RemoveVectoredContinueHandler() is available under 64 bit editions only
      if Assigned(AddVectoredExceptionHandler) then
        // available since Windows XP
        AddVectoredExceptionHandler(0,@SynLogVectoredHandler);
      {$else WITH_VECTOREXCEPT}
      {$ifdef WITH_PATCHEXCEPT}
      PatchCallRtlUnWind;
      {$else}
      {$ifdef WITH_RAISEPROC}
      OldRaiseProc := RaiseProc;
      RaiseProc := @SynRaiseProc;
      {$else}
      oldUnWindProc := RTLUnwindProc;
      RTLUnwindProc := @SynRtlUnwind;
      {$endif WITH_RAISEPROC}
      {$endif WITH_PATCHEXCEPT}
      {$endif WITH_VECTOREXCEPT}
      {$endif WITH_MAPPED_EXCEPTIONS}
    end;
  end;
{$endif NOEXCEPTIONINTERCEPT}
end;

procedure TSynLogFamily.SetEchoToConsole(aEnabled: TSynLogInfos);
begin
  if (self=nil) or (aEnabled=fEchoToConsole) then
    exit;
  fEchoToConsole := aEnabled;
end;

procedure TSynLogFamily.SetEchoToConsoleUseJournal(aValue: boolean);
begin
  if self<>nil then
    {$ifdef LINUXNOTBSD}
    if aValue and SystemdIsAvailable then
      fEchoToConsoleUseJournal := true else
    {$endif}
      fEchoToConsoleUseJournal := false;
end;

function TSynLogFamily.GetSynLogClassName: string;
begin
  if self=nil then
    result := '' else
    result := ClassName;
end;

constructor TSynLogFamily.Create(aSynLog: TSynLogClass);
begin
  fSynLogClass := aSynLog;
  fIdent := SynLogFamily.Add(self);
  fDestinationPath := ExeVersion.ProgramFilePath; // use .exe path
  fDefaultExtension := '.log';
  fArchivePath := fDestinationPath;
  fArchiveAfterDays := 7;
  fRotateFileAtHour := -1;
  fBufferSize := 4096;
  fStackTraceLevel := 30;
  fWithUnitName := true;
  fWithInstancePointer := true;
  {$ifndef FPC}
  if DebugHook<>0 then // never let stManualAndAPI trigger AV within the IDE
    fStackTraceUse := stOnlyAPI;
  {$endif}
  fExceptionIgnore := TList.Create;
  fLevelStackTrace := [sllStackTrace,sllException,sllExceptionOS
    {$ifndef FPC},sllError,sllFail,sllLastError,sllDDDError{$endif}];
end;

function TSynLogFamily.GetExceptionIgnoreCurrentThread: boolean;
begin
  result := ExceptionIgnorePerThread;
end;

procedure TSynLogFamily.SetExceptionIgnoreCurrentThread(
  aExceptionIgnoreCurrentThread: boolean);
begin
  ExceptionIgnorePerThread := aExceptionIgnoreCurrentThread;
end;

function TSynLogFamily.CreateSynLog: TSynLog;
var i: integer;
begin
  SynLogFileList.Safe.Lock;
  try
    result := fSynLogClass.Create(self);
    i := SynLogFileList.Add(result);
    if fPerThreadLog=ptOneFilePerThread then
      if (fRotateFileCount=0) and (fRotateFileSize=0) and (fRotateFileAtHour<0) then
        SynLogFileIndexThreadVar[fIdent] := i+1 else begin
        fPerThreadLog := ptIdentifiedInOnFile; // excluded by rotation
        fGlobalLog := result;
      end else
      fGlobalLog := result;
  finally
    SynLogFileList.Safe.UnLock;
  end;
end;

var
  AutoFlushSecondElapsed: cardinal;

{$ifdef MSWINDOWS}
  {$define AUTOFLUSHRAWWIN}
  // if defined, will use direct Windows API calls
{$endif}

{$ifdef AUTOFLUSHRAWWIN}
var
  AutoFlushThread: pointer;

procedure AutoFlushProc(P: pointer); stdcall;
  function Terminated: boolean;
  begin
    result :=  AutoFlushThread=nil;
  end;
{$else}
type
  // cross-platform / cross-compiler TThread-based flush
  TAutoFlushThread = class(TThread)
  protected
    fEvent: TEvent;
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

var
  AutoFlushThread: TAutoFlushThread;

constructor TAutoFlushThread.Create;
begin
  fEvent := TEvent.Create(nil,false,false,'');
  inherited Create(false);
end;

destructor TAutoFlushThread.Destroy;
begin
  inherited Destroy;
  fEvent.Free;
end;

procedure TAutoFlushThread.Execute;
{$endif}
var i: integer;
    files: TSynObjectListLocked;
begin
  SetThreadNameDefault(GetCurrentThreadID,'SynLog AutoFlushProc');
  try
    repeat
      {$ifdef AUTOFLUSHRAWWIN} // check every second for pending data
      for i := 1 to 10 do begin
        SleepHiRes(100);
        if Terminated then
          exit; // avoid GPF
      end;
      {$else}
      FixedWaitFor(fEvent,1000);
      if Terminated then
        exit;
      {$endif}
      files := SynLogFileList;
      if files.Count=0 then
        continue; // nothing to flush
      inc(AutoFlushSecondElapsed);
      files.Safe.Lock;
      try
        for i := 0 to files.Count-1 do
        with TSynLog(files.List[i]) do
          if Terminated then
            break else // avoid GPF
          if (fFamily.fAutoFlush<>0) and (fWriter<>nil) and (fWriter.PendingBytes>1) and
             (AutoFlushSecondElapsed mod fFamily.fAutoFlush=0) then begin
              {$ifdef AUTOFLUSHRAWWIN}
              if not IsMultiThread then
                if not fWriterStream.InheritsFrom(TFileStream) then
                  IsMultiThread := true; // only TFileStream is thread-safe
              {$endif}
              Flush(false); // write pending data
            end;
       finally
         files.Safe.UnLock;
       end;
    until Terminated;
  finally
    {$ifdef AUTOFLUSHRAWWIN}
    ExitThread(0);
    {$endif}
  end;
end;

procedure TSynLogFamily.StartAutoFlush;
{$ifdef AUTOFLUSHRAWWIN}var ID: cardinal;{$endif}
begin
  if (AutoFlushThread=nil) and (fAutoFlush<>0)
     {$ifndef FPC}and (DebugHook=0){$endif} then begin
    AutoFlushSecondElapsed := 0;
    {$ifdef AUTOFLUSHRAWWIN}
    AutoFlushThread := pointer(CreateThread(nil,0,@AutoFlushProc,nil,0,ID));
    {$else}
    AutoFlushThread := TAutoFlushThread.Create;
    {$endif}
  end;
end;

destructor TSynLogFamily.Destroy;
var SR: TSearchRec;
    oldTime,aTime: TDateTime;
    Y,M,D: word;
    aOldLogFileName, aPath: TFileName;
    tmp: array[0..7] of AnsiChar;
begin
  fDestroying := true;
  EchoRemoteStop;
  if AutoFlushThread<>nil then begin
    {$ifdef AUTOFLUSHRAWWIN}
    AutoFlushThread := nil; // Terminated=true to avoid GPF in AutoFlushProc
    {$else}
    AutoFlushThread.Terminate;
    AutoFlushThread.fEvent.SetEvent; // notify TAutoFlushThread.Execute
    FreeAndNil(AutoFlushThread); // wait for the TThread to be terminated
    {$endif}
  end;
  ExceptionIgnore.Free;
  try
    if Assigned(OnArchive) then
    if FindFirst(DestinationPath+'*'+DefaultExtension,faAnyFile,SR)=0 then
    try
      if ArchiveAfterDays<0 then
        ArchiveAfterDays := 0;
      oldTime := Now-ArchiveAfterDays;
      repeat
        if (SR.Name[1]='.') or (faDirectory and SR.Attr<>0) then
          continue;
        aTime := SearchRecToDateTime(SR);
        if (aTime=0) or (aTime>oldTime) then
          continue;
        aOldLogFileName := DestinationPath+SR.Name;
        if aPath='' then begin
          aPath := EnsureDirectoryExists(ArchivePath+'log');
          if aPath='' then
            break; // impossible to create the archive folder
          DecodeDate(aTime,Y,M,D);
          YearToPChar(Y,@tmp[0]);
          PWord(@tmp[4])^ := TwoDigitLookupW[M];
          PWord(@tmp[6])^ := ord(PathDelim);
          aPath := aPath+Ansi7ToString(tmp,7);
        end;
        OnArchive(aOldLogFileName,aPath);
      until FindNext(SR)<>0;
    finally
      try
        OnArchive('',aPath); // mark no more .log file to archive -> close .zip
      finally
        FindClose(SR);
      end;
    end;
  finally
    {$ifdef AUTOFLUSHRAWWIN} // release background thread once for all
    if AutoFlushThread<>nil then
      CloseHandle(THandle(AutoFlushThread));
    {$endif}
    inherited Destroy;
  end;
end;

function TSynLogFamily.SynLog: TSynLog;
var ndx: integer;
begin
  if self<>nil then begin
    if (fPerThreadLog=ptOneFilePerThread) and (fRotateFileCount=0) and
       (fRotateFileSize=0) and (fRotateFileAtHour<0) then begin
      ndx := SynLogFileIndexThreadVar[fIdent]-1;
      if ndx>=0 then // SynLogFileList.Safe.Lock/Unlock is not mandatory here
        result := SynLogFileList.List[ndx] else
        result := CreateSynLog;
    end else // for ptMergedInOneFile and ptIdentifiedInOnFile
      if fGlobalLog<>nil then
        result := fGlobalLog else
        result := CreateSynLog;
    {$ifndef NOEXCEPTIONINTERCEPT}
    if fHandleExceptions and (GlobalCurrentHandleExceptionSynLog<>result) then
      GlobalCurrentHandleExceptionSynLog := result;
    {$endif}
  end else
    result := nil;
end;

procedure TSynLogFamily.SynLogFileListEcho(const aEvent: TOnTextWriterEcho;
  aEventAdd: boolean);
var i: integer;
    files: TSynObjectListLocked;
    f: TSynLog;
begin
  if (self=nil) or (SynLogFileList.Count=0) or not Assigned(aEvent) then
    exit;
  files := SynLogFileList;
  files.Safe.Lock;
  try
    for i := 0 to files.Count-1 do begin
      f := files.List[i];
      if f.fFamily=self then
        if aEventAdd then
          f.fWriter.EchoAdd(aEvent) else
          f.fWriter.EchoRemove(aEvent);
    end;
  finally
    files.Safe.UnLock;
  end;
end;

procedure TSynLogFamily.SetEchoCustom(const aEvent: TOnTextWriterEcho);
begin
  if self=nil then
    exit;
  SynLogFileListEcho(fEchoCustom,false); // unsubscribe any previous
  fEchoCustom := aEvent;
  SynLogFileListEcho(aEvent,true); // subscribe new
end;

procedure TSynLogFamily.EchoRemoteStart(aClient: TObject;
  const aClientEvent: TOnTextWriterEcho; aClientOwnedByFamily: boolean);
begin
  EchoRemoteStop;
  fEchoRemoteClient := aClient;
  fEchoRemoteEvent := aClientEvent;
  fEchoRemoteClientOwned := aClientOwnedByFamily;
  SynLogFileListEcho(fEchoRemoteEvent,true); // subscribe
end;

procedure TSynLogFamily.EchoRemoteStop;
begin
  if fEchoRemoteClient=nil then
    exit;
  if fEchoRemoteClientOwned then
    try
      try
        fEchoRemoteEvent(nil,sllClient,
          FormatUTF8('%00%    Remote Client % Disconnected',
            [NowToString(false),LOG_LEVEL_TEXT[sllClient],self]));
      finally
        fEchoRemoteClient.Free;
      end;
    except
      on Exception do ;
    end;
  fEchoRemoteClient := nil;
  SynLogFileListEcho(fEchoRemoteEvent,false); // unsubscribe
  fEchoRemoteEvent := nil;
end;

function TSynLogFamily.GetExistingLog(MaximumKB: cardinal): RawUTF8;
const MAXPREVIOUSCONTENTSIZE = 1024*1024*128; // a 128 MB RawUTF8 is fair enough
var log: TSynLog;
    stream: TFileStream;
    endpos,start: Int64;
    c: AnsiChar;
    i,len,read,total: integer;
    P: PAnsiChar;
begin
  result := '';
  if SynLogFileList.Count<>0 then begin
    SynLogFileList.Safe.Lock;
    try
      for i := 0 to SynLogFileList.Count-1 do begin
        log := SynLogFileList.List[i];
        if log.fFamily<>self then
          continue;
        EnterCriticalSection(GlobalThreadLock);
        try
          log.Writer.FlushToStream;
          if log.Writer.Stream.InheritsFrom(TFileStream) then begin
            stream := TFileStream(log.Writer.Stream);
            endpos := stream.Position;
            try
              if endpos>MAXPREVIOUSCONTENTSIZE then
                len := MAXPREVIOUSCONTENTSIZE else
                len := MaximumKB shl 10;
              start := log.fStreamPositionAfterHeader;
              if (len<>0) and (endpos-start>len) then begin
                start := endpos-len;
                stream.Position := start;
                repeat
                  inc(start)
                until (stream.Read(c,1)=0) or (ord(c) in [10,13]);
              end else
                stream.Position := start;
              len := endpos-start;
              SetLength(result,len);
              P := pointer(result);
              total := 0;
              repeat
                read := stream.Read(P^,len);
                if read<=0 then begin
                  if total<>len then
                    SetLength(result,total); // truncate on read error
                  break;
                end;
                inc(P,read);
                dec(len,read);
                inc(total,read);
              until len=0;
            finally
              stream.Position := endpos;
            end;
          end;
        finally
          LeaveCriticalSection(GlobalThreadLock);
        end;
        break;
      end;
    finally
      SynLogFileList.Safe.UnLock;
    end;
  end;

end;

procedure TSynLogFamily.OnThreadEnded(Sender: TThread);
begin
  SynLog.NotifyThreadEnded;
end;


{ TFileStreamWithoutWriteError }

function TFileStreamWithoutWriteError.Write(const Buffer; Count: Longint): Longint;

begin
  inherited Write(Buffer,Count);
  result := Count; // ignore I/O errors
end;


{ TSynLog }

{$ifdef HASINLINENOTX86}
class function TSynLog.Family: TSynLogFamily;
begin
  result := pointer(Self);
  if result<>nil then begin
    result := PPointer(PtrInt(PtrUInt(result))+vmtAutoTable)^;
    if result=nil then
      result := FamilyCreate;
  end;
end;

class function TSynLog.Add: TSynLog;
begin
  result := pointer(Self);
  if result<>nil then begin // inlined TSynLog.Family (Add is already inlined)
    result := PPointer(PtrInt(PtrUInt(result))+vmtAutoTable)^;
    if result=nil then
      TSynLogFamily(pointer(result)) := FamilyCreate;
    result := TSynLogFamily(pointer(result)).SynLog;
  end;
end;
{$else}
class function TSynLog.Add: TSynLog;
asm
  push offset TSynLogFamily.SynLog
  jmp TSynLog.Family
end;

class function TSynLog.Family: TSynLogFamily;
asm
  or eax,eax
  jz @null
  mov edx,[eax+vmtAutoTable]
  or edx,edx
  jz FamilyCreate
  mov eax,edx
@null:
end;
{$endif}

procedure TSynLog.LockAndGetThreadContext;
var id: TThreadID;
begin
  EnterCriticalSection(GlobalThreadLock);
  id := GetCurrentThreadId;
  if id<>fThreadID then begin
    fThreadID := id;
    GetThreadContextInternal;
  end;
  {$ifndef NOEXCEPTIONINTERCEPT} // for IsBadCodePtr() or any internal exception
  fThreadHandleExceptionBackup := GlobalCurrentHandleExceptionSynLog;
  GlobalCurrentHandleExceptionSynLog := nil;
  {$endif}
end;

procedure TSynLog.LogTrailerUnLock(Level: TSynLogInfo);
begin
  try
    if Level in fFamily.fLevelStackTrace then
      AddStackTrace(nil);
    fWriter.AddEndOfLine(fCurrentLevel);
    if (fFileRotationNextHour<>0) and (GetTickCount64>=fFileRotationNextHour) then begin
      inc(fFileRotationNextHour,MSecsPerDay);
      PerformRotation;
    end else
    if (fFileRotationSize>0) and (fWriter.WrittenBytes>fFileRotationSize) then
      PerformRotation;
  finally
    {$ifndef NOEXCEPTIONINTERCEPT}
    GlobalCurrentHandleExceptionSynLog := fThreadHandleExceptionBackup;
    {$endif}
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

const
  // would handle up to 4096 threads, using 8 KB of RAM for the hash table
  MAXLOGTHREADBITS = 12;
  // maximum of thread IDs which can exist for a process
  // - shall be a power of 2 (used for internal TSynLog.fThreadHash)
  // - with the default 1MB stack size, max is around 2000 threads for Win32
  // - thread IDs are recycled when released via TSynLog.NotifyThreadEnded
  MAXLOGTHREAD = 1 shl MAXLOGTHREADBITS;

procedure TSynLog.GetThreadContextInternal;
var secondpass: boolean;
    id, hash: PtrUInt;
begin // should match TSynLog.ThreadContextRehash
  if fFamily.fPerThreadLog<>ptNoThreadProcess then begin
    secondpass := false;
    id := PtrUInt(fThreadID); // TThreadID  = ^TThreadRec under BSD
    hash := 0; // efficient TThreadID hash on all architectures
    repeat
      hash := hash xor (id and (MAXLOGTHREAD-1));
      id := id shr (MAXLOGTHREADBITS-1); // -1 for less collisions under Linux
    until id=0;
    fThreadIndex := fThreadHash[hash];
    fThreadLastHash := hash;
    if fThreadIndex<>0 then
      repeat
        fThreadContext := @fThreadContexts[fThreadIndex-1];
        if fThreadContext^.ID=fThreadID then // match found
          exit;
        // hash collision -> try next item in fThreadHash[] if possible
        if fThreadLastHash=MAXLOGTHREAD-1 then
          if secondpass then // avoid endless loop -> reuse last fThreadHash[]
            exit else begin
            fThreadLastHash := 0;
            secondpass := true;
          end else
          inc(fThreadLastHash);
        fThreadIndex := fThreadHash[fThreadLastHash];
      until fThreadIndex=0;
    // here we know that fThreadIndex=fThreadHash[hash]=0 -> register the thread
    if fThreadIndexReleasedCount>0 then begin // reuse NotifyThreadEnded() index
      dec(fThreadIndexReleasedCount);
      fThreadIndex := fThreadIndexReleased[fThreadIndexReleasedCount];
    end else begin // store a new entry
      if fThreadContextCount>=length(fThreadContexts) then
        SetLength(fThreadContexts,fThreadContextCount+128);
      inc(fThreadContextCount);
      fThreadIndex := fThreadContextCount;
    end;
    fThreadHash[fThreadLastHash] := fThreadIndex;
  end else
    fThreadIndex := 1;
  fThreadContext := @fThreadContexts[fThreadIndex-1];
  fThreadContext^.ID := fThreadID;
end;

function TSynLog.NewRecursion: PSynLogThreadRecursion;
begin
  with fThreadContext^ do begin
    if RecursionCount = RecursionCapacity then begin
      RecursionCapacity := NextGrow(RecursionCapacity);
      SetLength(Recursion, RecursionCapacity);
    end;
    result := @Recursion[RecursionCount];
    result^.Caller := 0; // no stack trace by default
    result^.RefCount := 0;
    inc(RecursionCount);
  end;
end;

procedure TSynLog.ThreadContextRehash;
var i: integer;
    id, hash: PtrUInt;
    secondpass: boolean;
    ctxt: PSynLogThreadContext;
begin // should match TSynLog.GetThreadContextInternal
  if fFamily.fPerThreadLog=ptNoThreadProcess then
    exit;
  FillcharFast(fThreadHash[0],MAXLOGTHREAD*sizeof(fThreadHash[0]),0);
  ctxt := pointer(fThreadContexts);
  for i := 1 to fThreadContextCount do begin
    id := PtrUInt(ctxt^.ID); // TThreadID  = ^TThreadRec under BSD
    if id<>0 then begin // not empty slot
      hash := 0; // efficient TThreadID hash on all architectures
      repeat
        hash := hash xor (id and (MAXLOGTHREAD-1));
        id := id shr (MAXLOGTHREADBITS-1); // -1 for less collisions under Linux
      until id=0;
      secondpass := false;
      repeat
        if fThreadHash[hash]=0 then
          break;
        // hash collision (no need to check the ID here)
        if hash=MAXLOGTHREAD-1 then
          if secondpass then // avoid endless loop
            break else begin
            hash := 0;
            secondpass := true;
          end else
          inc(hash);
      until false;
      fThreadHash[hash] := i;
    end;
    inc(ctxt);
  end;
end;

procedure TSynLog.NotifyThreadEnded;
begin
  if (self=nil) or (fThreadContextCount=0) then
    exit; // nothing to release
  LockAndGetThreadContext;
  try
    Finalize(fThreadContext^);
    FillcharFast(fThreadContext^,SizeOf(fThreadContext^),0);
    ThreadContextRehash; // fThreadHash[fThreadLastHash] := 0 is not enough
    if fThreadIndexReleasedCount>=length(fThreadIndexReleased) then
      SetLength(fThreadIndexReleased,fThreadIndexReleasedCount+128);
    fThreadIndexReleased[fThreadIndexReleasedCount] := fThreadIndex;
    inc(fThreadIndexReleasedCount); // allow naive but very efficient reuse
  finally
    {$ifndef NOEXCEPTIONINTERCEPT}
    GlobalCurrentHandleExceptionSynLog := fThreadHandleExceptionBackup;
    {$endif}
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

function TSynLog._AddRef: {$ifdef FPC}longint{$else}integer{$endif};
begin
  if fFamily.Level*[sllEnter,sllLeave]<>[] then
  try
    LockAndGetThreadContext;
    with fThreadContext^ do
    if RecursionCount>0 then
      with Recursion[RecursionCount-1] do begin
        if (RefCount=0) and (sllEnter in fFamily.Level) then begin
          LogHeaderLock(sllEnter,true);
          AddRecursion(RecursionCount-1,sllEnter);
        end;
        inc(RefCount);
        result := RefCount;
      end else
      result := 1; // should never be 0 (would release of TSynLog instance)
  finally
    {$ifndef NOEXCEPTIONINTERCEPT}
    GlobalCurrentHandleExceptionSynLog := fThreadHandleExceptionBackup;
    {$endif}
    LeaveCriticalSection(GlobalThreadLock);
  end else
    result := 1;
end;

{$ifdef MSWINDOWS}
var
  RtlCaptureStackBackTraceRetrieved: (btUntested, btOK, btFailed) = btUntested;
  RtlCaptureStackBackTrace: function(FramesToSkip, FramesToCapture: cardinal;
    BackTrace, BackTraceHash: pointer): byte; stdcall;
{$endif}

{$STACKFRAMES ON}
function TSynLog._Release: {$ifdef FPC}longint{$else}integer{$endif};
{$ifndef CPU64}
{$ifndef PUREPASCAL}
var aStackFrame: PtrInt;
{$endif}
{$endif}
begin
  if fFamily.Level*[sllEnter,sllLeave]<>[] then
  try
    LockAndGetThreadContext;
    with fThreadContext^ do
    if RecursionCount>0 then begin
      with Recursion[RecursionCount-1] do begin
        dec(RefCount);
        if RefCount=0 then begin
          if sllLeave in fFamily.Level then begin
            if MethodName=nil then begin
              {$ifdef CPU64}
              {$ifdef MSWINDOWS}
              if RtlCaptureStackBackTrace(1,1,@Caller,nil)=0 then
                Caller := 0 else
                dec(Caller,5); // ignore caller op codes
              {$else}
              Caller := 0; // no stack trace yet under Linux64
              {$endif}
              {$else}
              {$ifdef PUREPASCAL}
              Caller := 0; // e.g. ARM Linux
              {$else}
              asm
                mov eax,[ebp+16] // +4->_IntfClear +16->initial caller
                mov aStackFrame,eax
              end;
              Caller := aStackFrame-5;
              {$endif}
              {$endif}
            end;
            LogHeaderLock(sllLeave,true);
            AddRecursion(RecursionCount-1,sllLeave);
          end;
          dec(RecursionCount);
        end;
        result := RefCount;
      end;
    end else
      result := 1; // should never be 0 (would release TSynLog instance)
  finally
    {$ifndef NOEXCEPTIONINTERCEPT}
    GlobalCurrentHandleExceptionSynLog := fThreadHandleExceptionBackup;
    {$endif}
    LeaveCriticalSection(GlobalThreadLock);
  end else
    result := 1;
end;
{$STACKFRAMES OFF}

constructor TSynLog.Create(aFamily: TSynLogFamily);
begin
  if aFamily=nil then
    aFamily := Family;
  fFamily := aFamily;
  {$ifdef MSWINDOWS}
  if RtlCaptureStackBackTraceRetrieved=btUntested then begin
    if OSVersion<wXP then
      RtlCaptureStackBackTraceRetrieved := btFailed else begin
     @RtlCaptureStackBackTrace := GetProcAddress(
       GetModuleHandle(kernel32),'RtlCaptureStackBackTrace');
     if @RtlCaptureStackBackTrace=nil then
       RtlCaptureStackBackTraceRetrieved := btFailed else
       RtlCaptureStackBackTraceRetrieved := btOK;
    end;
  end;
  {$ifdef CPU64}
  assert(RtlCaptureStackBackTraceRetrieved=btOK);
  {$endif}
  {$endif}
  SetLength(fThreadHash,MAXLOGTHREAD); // 8 KB buffer
  SetLength(fThreadContexts,128);
end;

destructor TSynLog.Destroy;
begin
{$ifndef NOEXCEPTIONINTERCEPT}
  if fFamily.fHandleExceptions and (GlobalCurrentHandleExceptionSynLog=self) then
    GlobalCurrentHandleExceptionSynLog := nil;
{$endif}
  Flush(true);
  fWriterStream.Free;
  fWriter.Free;
  inherited;
end;

procedure TSynLog.CloseLogFile;
begin
  if fWriter=nil then
    exit;
  EnterCriticalSection(GlobalThreadLock);
  try
    fWriter.FlushFinal;
    FreeAndNil(fWriterStream);
    FreeAndNil(fWriter);
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

procedure TSynLog.Release;
begin
  SynLogFileList.Safe.Lock;
  try
    CloseLogFile;
    SynLogFileList.Remove(self);
    if fFamily.fPerThreadLog=ptOneFilePerThread then
      SynLogFileIndexThreadVar[fFamily.fIdent] := 0;
  finally
    SynLogFileList.Safe.UnLock;
  end;
  Free;
end;

procedure TSynLog.Flush(ForceDiskWrite: boolean);
begin
  if fWriter=nil then
    exit;
  EnterCriticalSection(GlobalThreadLock);
  try
    fWriter.FlushToStream;
    if ForceDiskWrite and fWriterStream.InheritsFrom(TFileStream) then
      FlushFileBuffers(TFileStream(fWriterStream).Handle);
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

function TSynLog.QueryInterface(
{$ifdef FPC}
  {$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): longint;
{$else}
  const IID: TGUID; out Obj): HResult;
{$endif}
begin
  Result := E_NOINTERFACE;
end;

{$STACKFRAMES ON}

class function TSynLog.Enter(aInstance: TObject; aMethodName: PUTF8Char;
  aMethodNameLocal: boolean): ISynLog;
var aSynLog: TSynLog;
    aStackFrame: PtrUInt;
begin
  // inlined aSynLog := Family.Add
  aSynLog := pointer(self);
  if aSynLog=nil then begin
    result := nil;
    exit;
  end;
  aSynLog := PPointer(PtrInt(PtrUInt(aSynLog))+vmtAutoTable)^;
  if aSynLog=nil then
    TSynLogFamily(pointer(aSynLog)) := FamilyCreate;
  aSynLog := TSynLogFamily(pointer(aSynLog)).SynLog;
  // recursively store parameters
  if sllEnter in aSynLog.fFamily.fLevel then begin
    aSynLog.LockAndGetThreadContext;
    with aSynLog.NewRecursion^ do
    try
      {$ifdef CPU64}
      {$ifdef MSWINDOWS}
      if RtlCaptureStackBackTrace(1,1,@aStackFrame,nil)=0 then
        aStackFrame := 0 else
        dec(aStackFrame,5); // ignore call TSynLog.Enter op codes
      {$else}
      aStackFrame := 0; // No stack trace yet under Linux64
      { TODO : use cross-platform get_caller_addr(get_frame) under FPC }
      {$endif}
      {$else}
      {$ifdef PUREPASCAL}
      aStackFrame := 0; // e.g. ARM Linux
      {$else}
      asm
        mov eax,[ebp+4]  // retrieve caller EIP from push ebp; mov ebp,esp
        sub eax,5        // ignore call TSynLog.Enter op codes
        mov aStackFrame,eax
      end;
      {$endif}
      {$endif}
      Instance := aInstance;
      MethodName := aMethodName;
      if aMethodNameLocal then
        MethodNameLocal := mnEnter else
        MethodNameLocal := mnAlways;
      Caller := aStackFrame;
    finally
      {$ifndef NOEXCEPTIONINTERCEPT}
      GlobalCurrentHandleExceptionSynLog := aSynLog.fThreadHandleExceptionBackup;
      {$endif}
      LeaveCriticalSection(GlobalThreadLock);
    end;
  end;
  // copy to ISynLog interface -> will call TSynLog._AddRef
  result := aSynLog;
end;
{$STACKFRAMES OFF}

class function TSynLog.Enter(const TextFmt: RawUTF8; const TextArgs: array of const;
  aInstance: TObject=nil): ISynLog;
var aSynLog: TSynLog;
begin
  aSynLog := Family.SynLog;
  if (aSynLog<>nil) and (sllEnter in aSynLog.fFamily.fLevel) then begin
    aSynLog.LockAndGetThreadContext;
    with aSynLog.NewRecursion^ do
    try
      Instance := aInstance;
      MethodName := nil; // avoid GPF in RawUTF8(pointer(MethodName)) below
      FormatUTF8(TextFmt,TextArgs,RawUTF8(pointer(MethodName)));
      MethodNameLocal := mnEnterOwnMethodName;
    finally
      {$ifndef NOEXCEPTIONINTERCEPT}
      GlobalCurrentHandleExceptionSynLog := aSynLog.fThreadHandleExceptionBackup;
      {$endif}
      LeaveCriticalSection(GlobalThreadLock);
    end;
  end;
  // copy to ISynLog interface -> will call TSynLog._AddRef
  result := aSynLog;
end;

procedure TSynLog.ManualEnter(aMethodName: PUtf8Char; aInstance: TObject);
begin
  if (self = nil) or
     (fFamily.fLevel * [sllEnter, sllLeave] = []) then
    exit;
  if aMethodName = nil then
    aMethodName := ' '; // something non void (call stack is irrelevant)
  LockAndGetThreadContext;
  try
    with NewRecursion^ do begin
      // inlined TSynLog.Enter
      Instance := aInstance;
      MethodName := aMethodName;
      MethodNameLocal := mnEnter;
      // inlined TSynLog._AddRef
      if sllEnter in fFamily.Level then begin
        LogHeaderLock(sllEnter, true);
        AddRecursion(fThreadContext^.RecursionCount - 1, sllEnter);
      end;
      inc(RefCount);
    end;
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

procedure TSynLog.ManualLeave;
begin
  if self <> nil then
    _Release;
end;

class function TSynLog.FamilyCreate: TSynLogFamily;
var PVMT: pointer;
begin // private sub function makes the code faster in most case
  if not InheritsFrom(TSynLog) then
    // invalid call
    result := nil else begin
    EnterCriticalSection(GlobalThreadLock);
    try
      // TSynLogFamily instance is stored into "AutoTable" unused VMT entry
      PVMT := pointer(PtrInt(PtrUInt(self))+vmtAutoTable);
      result := PPointer(PVMT)^;
      if result=nil then begin // protect from (unlikely) concurrent call
        // create the properties information from RTTI
        result := TSynLogFamily.Create(self); // stored in SynLogFamily list
        PatchCodePtrUInt(PVMT,PtrUInt(result),{LeaveUnprotected=}true);
      end;
    finally
      LeaveCriticalSection(GlobalThreadLock);
    end;
  end;
end;

type
  TSynLogVoid = class(TSynLog);

class function TSynLog.Void: TSynLogClass;
begin
  TSynLogVoid.Family.Level := [];
  result := TSynLogVoid;
end;

function TSynLog.Instance: TSynLog;
begin
  result := self;
end;

{$I-}
function TSynLog.ConsoleEcho(Sender: TTextWriter; Level: TSynLogInfo;
  const Text: RawUTF8): boolean;
{$ifdef MSWINDOWS}
var tmp: AnsiString;
{$endif}
{$ifdef LINUXNOTBSD}
var
  tmp, mtmp: RawUTF8;
  jvec: Array[0..1] of TioVec;
{$endif}
begin
  result := true;
  if not (Level in fFamily.fEchoToConsole) then
    exit;
  {$ifdef LINUXNOTBSD}
  if Family.EchoToConsoleUseJournal then begin
    if length(Text)<18 then // should be at last "20200615 08003008  "
      exit;
    FormatUTF8('PRIORITY=%', [LOG_TO_SYSLOG[Level]],tmp);
    jvec[0].iov_base := pointer(tmp);
    jvec[0].iov_len := length(tmp);
    // skip time "20200615 08003008  ." - journal do it for us; and first space after it
    FormatUTF8('MESSAGE=%', [PUTF8Char(pointer(Text))+18],mtmp);
    jvec[1].iov_base := pointer(mtmp);
    jvec[1].iov_len := length(mtmp);
    ExternalLibraries.sd_journal_sendv(@jvec[0],2);
    exit;
  end;
  {$endif}
  TextColor(LOG_CONSOLE_COLORS[Level]);
  {$ifdef MSWINDOWS}
  tmp := CurrentAnsiConvert.UTF8ToAnsi(Text);
  {$ifndef HASCODEPAGE}
  AnsiToOem(pointer(tmp),pointer(tmp));
  {$endif}
  writeln(tmp);
  {$else}
  writeln(Text);
  {$endif}
  ioresult;
  TextColor(ccLightGray);
end;
{$I+}

procedure TSynLog.Log(Level: TSynLogInfo; const TextFmt: RawUTF8; const TextArgs: array of const;
  aInstance: TObject);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,TextFmt,TextArgs,aInstance);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const TextFmt: RawUTF8; const TextArg: RawUTF8;
  aInstance: TObject=nil);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,TextFmt,[TextArg],aInstance);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const TextFmt: RawUTF8; const TextArg: Int64;
  aInstance: TObject=nil);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,TextFmt,[TextArg],aInstance);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const Text: RawUTF8; aInstance: TObject;
  TextTruncateAtLength: integer);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,Text,aInstance,TextTruncateAtLength);
end;

{$ifdef UNICODE}
procedure TSynLog.Log(Level: TSynLogInfo; const Text: string; aInstance: TObject);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,'%',[Text],aInstance);
end;
{$endif}

procedure TSynLog.LogLines(Level: TSynLogInfo; LinesToLog: PUTF8Char; aInstance: TObject;
  const IgnoreWhenStartWith: PAnsiChar);
procedure DoLog(LinesToLog: PUTF8Char);
var s: RawUTF8;
begin
  repeat
    s := trim(GetNextLine(LinesToLog,LinesToLog));
    if s<>'' then
      if (IgnoreWhenStartWith=nil) or not IdemPChar(pointer(s),IgnoreWhenStartWith) then
        LogInternal(Level,s,aInstance,maxInt);
  until LinesToLog=nil;
end;
begin
  if (self<>nil) and (Level in fFamily.fLevel) and (LinesToLog<>nil) then
    DoLog(LinesToLog);
end;

procedure TSynLog.LogThreadName(const Name: RawUTF8; IgnoreIfAlreadySet: boolean);
begin
  if (self<>nil) and (sllInfo in fFamily.fLevel) then
    if LogHeaderLock(sllInfo,false) then // inlined LogInternal
    try
      if IgnoreIfAlreadySet and (fThreadContext^.ThreadName<>'') then
        exit;
      fWriter.Add('SetThreadName %=%',[pointer(fThreadID),Name],twOnSameLine);
      fThreadContext^.ThreadName := Name;
    finally
      LogTrailerUnLock(sllInfo);
    end;
end;

function TSynLog.LogClass: TSynLogClass;
begin
  if self=nil then
    result := nil else
    result := PPointer(self)^;
end;

procedure TSynLog.ForceRotation;
begin
  EnterCriticalSection(GlobalThreadLock);
  try
    PerformRotation;
  finally
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

procedure TSynLog.DisableRemoteLog(value: boolean);
begin
  if (fDisableRemoteLog=value) or not Assigned(fFamily.fEchoRemoteEvent) then
    exit;
  if value then begin
    // fDisableRemoteLog=false -> remove from events, within the global mutex
    EnterCriticalSection(GlobalThreadLock);
    if fDisableRemoteLog=value then // unlikely set in-between
      LeaveCriticalSection(GlobalThreadLock) else begin
      fDisableRemoteLog := true;
      fWriter.EchoRemove(fFamily.fEchoRemoteEvent);
    end;
  end else begin
    // fDisableRemoteLog=true -> add to events, already within the global mutex
    fDisableRemoteLog := false;
    fWriter.EchoAdd(fFamily.fEchoRemoteEvent);
    LeaveCriticalSection(GlobalThreadLock);
  end;
end;

procedure TSynLog.Log(Level: TSynLogInfo; aInstance: TObject);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    if aInstance<>nil then
      LogInternal(Level,'',aInstance,maxInt) else
      LogInternal(Level,'Instance=nil',nil,maxInt);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const aName: RawUTF8;
  aTypeInfo: pointer; const aValue; Instance: TObject);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,aName,aTypeInfo,aValue,Instance);
end;

{$STACKFRAMES ON}
procedure TSynLog.Log(Level: TSynLogInfo);
var LastError: DWORD;
    {$ifndef FPC}aCaller: PtrUInt;{$endif}
begin
  if Level=sllLastError then
    LastError := GetLastError else
    LastError := 0;
  if (self<>nil) and (Level in fFamily.fLevel) then
  if LogHeaderLock(Level,false) then
  try
    if LastError<>0 then
      AddErrorMessage(LastError);
    {$ifndef FPC}
    {$ifdef CPU64}
    {$ifdef MSWINDOWS}
    if RtlCaptureStackBackTrace(1,1,@aCaller,nil)=0 then
      aCaller := 0 else
      dec(aCaller,5); // ignore call TSynLog.Enter op codes
    {$else}
    aCaller := 0; // no stack trace yet under Linux64
    {$endif}
    {$else}
    {$ifdef PUREPASCAL}
    aCaller := 0; // e.g. ARM Linux
    {$else}
    asm
      mov eax,[ebp+4]  // retrieve caller EIP from push ebp; mov ebp,esp
      sub eax,5        // ignore call TSynLog.Enter op codes
      mov aCaller,eax
    end;
    {$endif}
    {$endif}
    TSynMapFile.Log(fWriter,aCaller,false);
    {$endif}
  finally
    LogTrailerUnLock(Level);
    if LastError<>0 then
      SetLastError(LastError);
  end;
end;
{$STACKFRAMES OFF}

class procedure TSynLog.DebuggerNotify(Level: TSynLogInfo;
  const Format: RawUTF8; const Args: array of const);
var Msg: RawUTF8;
begin
  if Format<>''then begin
    FormatUTF8(Format,Args,Msg);
    Add.LogInternal(Level,Msg,nil,maxInt);
    {$ifdef MSWINDOWS}
    {$ifndef FPC} // external exception :(
    //OutputDebugStringA(pointer(CurrentAnsiConvert.UTF8ToAnsi(Msg)));
    {$endif FPC}
    {$else}
    {$I-}
    write(Msg,'  ');
    ioresult;
    {$I+}
    {$endif MSWINDOWS}
  end;
  {$ifndef FPC_OR_PUREPASCAL}
  if DebugHook<>0 then
    asm int 3 end; // force manual breakpoint if tests are run from the IDE
  {$endif}
end;

procedure TSynLog.LogFileInit;
begin
  {$ifdef LINUX}
  QueryPerformanceMicroSeconds(fStartTimestamp);
  {$else}
  QueryPerformanceCounter(fStartTimestamp);
  if not QueryPerformanceFrequency(fFrequencyTimestamp) then begin
    fFamily.HighResolutionTimestamp := false;
    fFrequencyTimestamp := 0;
  end else
  {$endif LINUX}
    if (fFileRotationSize>0) or (fFileRotationNextHour<>0) then
      fFamily.HighResolutionTimestamp := false;
  fStreamPositionAfterHeader := fWriter.WrittenBytes;
  if fFamily.LocalTimestamp then
    fStartTimestampDateTime := Now else
    fStartTimestampDateTime := NowUTC;
  Include(fInternalFlags,logInitDone);
end;

procedure TSynLog.LogFileHeader;
var WithinEvents: boolean;
    i: integer;
    {$ifdef MSWINDOWS}
    Env: PWideChar;
    P: PWideChar;
    L: Integer;
    {$endif}

  procedure NewLine;
  begin
    if WithinEvents then begin
      fWriter.AddEndOfLine(sllNewRun);
      LogCurrentTime;
      fWriter.AddShort(LOG_LEVEL_TEXT[sllNewRun]);
    end else
      fWriter.Add(#10);
  end;

begin
  WithinEvents := fWriter.WrittenBytes>0;
  // array of const is buggy under Delphi 5 :( -> use fWriter.Add*() below
  if WithinEvents then begin
    LogCurrentTime;
    fWriter.AddShort(LOG_LEVEL_TEXT[sllNewRun]);
    fWriter.AddChars('=',50);
    NewLine;
  end;
  with ExeVersion, fWriter do begin
    AddString(ProgramFullSpec);
    NewLine;
    AddShort('Host=');  AddString(Host);
    AddShort(' User='); AddString(User);
    AddShort(' CPU=');
    if CpuInfoText='' then
      Add(SystemInfo.dwNumberOfProcessors) else
      for i := 1 to length(CpuInfoText) do
        if not (ord(CpuInfoText[i]) in [1..32,ord(':')]) then
          Add(CpuInfoText[i]);
    {$ifdef MSWINDOWS}
    with SystemInfo, OSVersionInfo do begin
      Add('*');
      Add(wProcessorArchitecture); Add('-'); Add(wProcessorLevel); Add('-');
      Add(wProcessorRevision);
    {$endif}
      {$ifdef CPUINTEL}
      Add(':'); AddBinToHex(@CpuFeatures,SizeOf(CpuFeatures));
      {$endif}
      AddShort(' OS=');
    {$ifdef MSWINDOWS}
      Add(ord(OSVersion)); Add('.'); Add(wServicePackMajor);
      Add('='); Add(dwMajorVersion); Add('.'); Add(dwMinorVersion); Add('.');
      Add(dwBuildNumber);
    end;
    {$else}
    AddTrimLeftLowerCase(ToText(OS_KIND)); Add('=');
    AddTrimSpaces(@SystemInfo.uts.sysname); Add('-');
    AddTrimSpaces(@SystemInfo.uts.release);
    AddReplace(@SystemInfo.uts.version,' ','-');
    {$endif MSWINDOWS}
    if OSVersionInfoEx<> '' then begin
      Add('/'); AddTrimSpaces(OSVersionInfoEx); end;
    {$ifdef MSWINDOWS}
    AddShort(' Wow64='); Add(integer(IsWow64));
    AddShort(' Freq=');
    QueryPerformanceFrequency(fFrequencyTimestamp);
    Add(fFrequencyTimestamp);
    {$else}
    AddShort(' Wow64=0 Freq=1000000'); // taken by QueryPerformanceMicroSeconds()
    {$endif MSWINDOWS}
    if IsLibrary then begin
      AddShort(' Instance=');
      AddNoJSONEscapeString(InstanceFileName);
    end;
    {$ifdef MSWINDOWS}
    if not fFamily.fNoEnvironmentVariable then begin
      NewLine;
      AddShort('Environment variables=');
      Env := GetEnvironmentStringsW;
      P := pointer(Env);
      while P^<>#0 do begin
        L := StrLenW(P);
        if (L>0) and (P^<>'=') then begin
          AddNoJSONEscapeW(PWord(P),0);
          Add(#9);
        end;
        inc(P,L+1);
      end;
      FreeEnvironmentStringsW(Env);
      CancelLastChar(#9);
    end;
    {$endif MSWINDOWS}
    NewLine;
    AddClassName(self.ClassType);
    AddShort(' '+SYNOPSE_FRAMEWORK_FULLVERSION+' ');
    if fFamily.LocalTimestamp then
      AddDateTime(Now) else
      AddDateTime(NowUTC);
    if WithinEvents then
      AddEndOfLine(sllNone) else
      Add(#10,#10);
    FlushToStream;
    EchoReset; // header is not to be sent to console
  end;
  Include(fInternalFlags,logHeaderWritten);
  if not (logInitDone in fInternalFlags) then
    LogFileInit;
end;

{$ifndef DELPHI5OROLDER}
{$WARN SYMBOL_DEPRECATED OFF} // for GetHeapStatus
procedure TSynLog.AddMemoryStats;
begin
  {$ifdef MSWINDOWS}
  with GetHeapStatus do
    if TotalAddrSpace<>0 then
    fWriter.Add(' AddrSpace=% Uncommitted=% Committed=% Allocated=% Free=% '+
       'FreeSmall=% FreeBig=% Unused=% Overheap=% ',
      [TotalAddrSpace,TotalUncommitted,TotalCommitted,TotalAllocated,TotalFree,
       FreeSmall,FreeBig,Unused,Overhead]);
  {$endif}
end;
{$WARN SYMBOL_DEPRECATED ON}
{$endif}

procedure TSynLog.AddErrorMessage(Error: Cardinal);
{$ifdef MSWINDOWS}
var Len: Integer;
    Buffer: array[byte] of WideChar;
begin
  Len := FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY,
    nil, Error, 0, Buffer, SizeOf(Buffer), nil);
  while (Len>0) and (ord(Buffer[Len-1]) in [0..32,ord('.')]) do dec(Len);
  Buffer[Len] := #0;
  fWriter.Add(' ','"');
  fWriter.AddOnSameLineW(@Buffer,Len);
  fWriter.AddShort('" (');
{$else}
begin
  fWriter.AddShort('Error "');
  fWriter.AddAnsiString(StrError(Error), twOnSameLine);
  fWriter.AddShort('" (');
{$endif}
  fWriter.Add(Error);
  fWriter.Add(')',' ');
end;

procedure TSynLog.LogCurrentTime;
begin
  {$ifdef LINUX}
  if fFamily.HighResolutionTimestamp then begin
    QueryPerformanceMicroSeconds(fCurrentTimestamp);
  {$else}
  if fFamily.HighResolutionTimestamp and (fFrequencyTimestamp<>0) then begin
    QueryPerformanceCounter(fCurrentTimestamp);
  {$endif}
    dec(fCurrentTimestamp,fStartTimestamp);
    fWriter.AddBinToHexDisplay(@fCurrentTimestamp,sizeof(fCurrentTimestamp));
  end else
    fWriter.AddCurrentLogTime(fFamily.LocalTimestamp);
end;

function TSynLog.LogHeaderLock(Level: TSynLogInfo; AlreadyLocked: boolean): boolean;
var i: integer;
begin
  if not AlreadyLocked then
    LockAndGetThreadContext;
  try
    if fWriter=nil then
      CreateLogWriter; // file creation should be thread-safe
    if not (logHeaderWritten in fInternalFlags) then
      LogFileHeader else
      if not (logInitDone in fInternalFlags) then
        LogFileInit;
    if not (sllEnter in fFamily.Level) and (Level in fFamily.fLevelStackTrace) then
       for i := 0 to fThreadContext^.RecursionCount-1 do begin
         fWriter.AddChars(' ',i+24-byte(fFamily.HighResolutionTimestamp));
         AddRecursion(i,sllNone);
       end;
    LogCurrentTime;
    if fFamily.fPerThreadLog=ptIdentifiedInOnFile then
      fWriter.AddInt18ToChars3(fThreadIndex);
    fCurrentLevel := Level;
    fWriter.AddShort(LOG_LEVEL_TEXT[Level]);
    fWriter.AddChars(#9,fThreadContext^.RecursionCount-byte(Level in [sllEnter,sllLeave]));
    {$ifndef DELPHI5OROLDER}
    case Level of // handle additional text for some special error levels
      sllMemory: AddMemoryStats;
    end;
    {$endif}
    result := true;
  except
    on Exception do
     result := false ;
  end;
end;

procedure TSynLog.PerformRotation;
const _LOG_SYNLZ: array[boolean] of TFileName = ('.synlz','.log');
var currentMaxSynLZ: cardinal;
    i: integer;
    FN: array of TFileName;
begin
  fWriter.FlushFinal;
  FreeAndNil(fWriter);
  FreeAndNil(fWriterStream);
  currentMaxSynLZ := 0;
  if not (assigned(fFamily.fOnRotate) and
          fFamily.fOnRotate(self,fFileName)) then begin
    if fFamily.fRotateFileCount>1 then begin
      SetLength(FN,fFamily.fRotateFileCount-1);
      for i := fFamily.fRotateFileCount-1 downto 1 do begin
        FN[i-1] := ChangeFileExt(fFileName,
          '.'+IntToStr(i)+_LOG_SYNLZ[fFamily.fRotateFileNoCompression]);
        if (currentMaxSynLZ=0) and FileExists(FN[i-1]) then
          currentMaxSynLZ := i;
      end;
      if currentMaxSynLZ=fFamily.fRotateFileCount-1 then
        DeleteFile(FN[currentMaxSynLZ-1]); // delete e.g. '9.synlz'
      for i := fFamily.fRotateFileCount-2 downto 1 do
        RenameFile(FN[i-1],FN[i]); // e.g. '8.synlz' -> '9.synlz'
      if fFamily.fRotateFileNoCompression then
        RenameFile(fFileName,FN[0]) else      // main -> '1.log'
        FileSynLZ(fFileName,FN[0],LOG_MAGIC); // main -> '1.synlz'
    end;
    DeleteFile(fFileName);
  end;
  CreateLogWriter;
  LogFileHeader;
  if fFamily.fPerThreadLog=ptIdentifiedInOnFile then
    for i := 0 to fThreadContextCount-1 do
    with fThreadContexts[i] do
      if (pointer(ID)<>nil) and (ThreadName<>'') then begin // see TSynLog.LogThreadName
        LogCurrentTime;
        fWriter.AddInt18ToChars3(i+1);
        fWriter.AddShort(LOG_LEVEL_TEXT[sllInfo]);
        fWriter.Add('SetThreadName %=%',[pointer(ID),ThreadName],twOnSameLine);
        fWriter.AddEndOfLine(sllInfo);
      end;
end;

procedure TSynLog.LogInternal(Level: TSynLogInfo; const TextFmt: RawUTF8;
  const TextArgs: array of const; Instance: TObject);
var LastError: cardinal;
begin
  if Level=sllLastError then
    LastError := GetLastError else
    LastError := 0;
  if LogHeaderLock(Level,false) then
  try
    if Instance<>nil then
      fWriter.AddInstancePointer(Instance,' ',fFamily.WithUnitName,fFamily.WithInstancePointer);
    fWriter.Add(TextFmt,TextArgs,twOnSameLine,
      [woDontStoreDefault,woDontStoreEmptyString,woDontStore0,woFullExpand]);
    if LastError<>0 then
      AddErrorMessage(LastError);
  finally
    LogTrailerUnLock(Level);
    if LastError<>0 then
      SetLastError(LastError);
  end;
end;

procedure TSynLog.LogInternal(Level: TSynLogInfo; const Text: RawUTF8;
  Instance: TObject; TextTruncateAtLength: integer);
var LastError: cardinal;
begin
  if Level=sllLastError then
    LastError := GetLastError else
    LastError := 0;
  if LogHeaderLock(Level,false) then
  try
    if Text='' then begin
      if Instance<>nil then
        if Instance.InheritsFrom(Exception) then begin
          fWriter.AddInstanceName(Instance,':');
          if Instance.InheritsFrom(ESynException) then
            fWriter.WriteObject(Instance,[woFullExpand]) else begin
            fWriter.Add('"');
            fWriter.AddJSONEscapeString(Exception(Instance).Message);
            fWriter.Add('"');
          end;
        end else
          fWriter.WriteObject(Instance,[woFullExpand]);
    end else begin
      if Instance<>nil then
        fWriter.AddInstancePointer(Instance,' ',fFamily.WithUnitName,fFamily.WithInstancePointer);
      if length(Text)>TextTruncateAtLength then begin
        fWriter.AddOnSameLine(pointer(Text),TextTruncateAtLength);
        fWriter.AddShort('... (truncated) length=');
        fWriter.AddU(length(Text));
      end else
        fWriter.AddOnSameLine(pointer(Text));
    end;
    if LastError<>0 then
      AddErrorMessage(LastError);
  finally
    LogTrailerUnLock(Level);
    if LastError<>0 then
      SetLastError(LastError);
  end;
end;

procedure TSynLog.LogInternal(Level: TSynLogInfo; const aName: RawUTF8;
   aTypeInfo: pointer; const aValue; Instance: TObject);
begin
  if LogHeaderLock(Level,false) then
  try
    if Instance<>nil then
      fWriter.AddInstancePointer(Instance,' ',fFamily.WithUnitName,fFamily.WithInstancePointer);
    fWriter.AddOnSameLine(pointer(aName));
    fWriter.Add('=');
    fWriter.AddTypedJSON(aTypeInfo,aValue);
  finally
    LogTrailerUnLock(Level);
  end;
end;

procedure TSynLog.ComputeFileName;
var timeNow,hourRotate,timeBeforeRotate: TDateTime;
begin
  fFileName := fFamily.fCustomFileName;
  if fFileName='' then begin
    fFileName := UTF8ToString(ExeVersion.ProgramName);
    if fFamily.IncludeComputerNameInFileName then
      fFileName := fFileName+' ('+UTF8ToString(ExeVersion.Host)+')';
  end;
  fFileRotationSize := 0;
  if fFamily.fRotateFileCount>0 then begin
    if fFamily.fRotateFileSize>0 then
      fFileRotationSize := fFamily.fRotateFileSize shl 10; // size KB -> B
    if fFamily.fRotateFileAtHour in [0..23] then begin
      hourRotate := EncodeTime(fFamily.fRotateFileAtHour,0,0,0);
      timeNow := Time;
      if hourRotate<timeNow then
        hourRotate := hourRotate+1; // trigger will be tomorrow
      timeBeforeRotate := hourRotate-timeNow;
      fFileRotationNextHour := GetTickCount64+trunc(timeBeforeRotate*MSecsPerDay);
    end;
  end;
  if (fFileRotationSize=0) and (fFileRotationNextHour=0) then
    fFileName := fFileName+' '+Ansi7ToString(NowToString(false));
  {$ifdef MSWINDOWS}
  if IsLibrary and (fFamily.fCustomFileName='') then
    fFileName := fFileName+' '+ExtractFileName(GetModuleName(HInstance));
  {$endif}
  if fFamily.fPerThreadLog=ptOneFilePerThread then
    fFileName := fFileName+' '+Ansi7ToString(PointerToHex(pointer(GetCurrentThreadId)));
  fFileName := fFamily.fDestinationPath+fFileName+fFamily.fDefaultExtension;
end;

procedure TSynLog.CreateLogWriter;
var i,retry: integer;
    exists: boolean;
begin
  if fWriterStream=nil then begin
    ComputeFileName;
    if fFamily.NoFile then
      fWriterStream := TFakeWriterStream.Create else begin
      if FileExists(fFileName) then
        case fFamily.FileExistsAction of
        acOverwrite:
          DeleteFile(fFileName);
        acAppend:
          Include(fInternalFlags,logHeaderWritten);
        end;
      for retry := 0 to 2 do begin
        for i := 1 to 10 do
        try
          exists := FileExists(fFileName);
          if exists and (fFamily.FileExistsAction<>acOverwrite) then begin
            if fFamily.FileExistsAction=acAppend then
              Include(fInternalFlags,logHeaderWritten);
          end else
          if (fFileRotationSize=0) or not exists then
            TFileStream.Create(fFileName,fmCreate).Free;   // create a void file
          fWriterStream := TFileStreamWithoutWriteError.Create(fFileName,
            fmOpenReadWrite or fmShareDenyWrite); // open with read sharing
          break;
        except
          on Exception do
            SleepHiRes(100);
        end;
        if fWriterStream<>nil then
          break;
        fFileName := ChangeFileExt(fFileName,'-'+fFamily.fDefaultExtension);
      end;
    end;
    if fWriterStream=nil then // go on if file creation fails (e.g. RO folder)
      fWriterStream := TFakeWriterStream.Create;
    if (fFileRotationSize>0) or (fFamily.FileExistsAction<>acOverwrite) then
      fWriterStream.Seek(0,soFromEnd); // in rotation mode, append at the end
  end;
  if fWriterClass=nil then
    // set to TTextWriterWithEcho or TJSONSerializer if mORMot.pas is linked
    fWriterClass := DefaultTextWriterSerializer;
  if fWriter=nil then begin
    fWriter := fWriterClass.Create(fWriterStream,fFamily.BufferSize);
    fWriter.CustomOptions := fWriter.CustomOptions+[twoEnumSetsAsTextInRecord,twoFullSetsAsStar];
  end;
  fWriter.EndOfLineCRLF := fFamily.EndOfLineCRLF;
  if integer(fFamily.EchoToConsole)<>0 then
    fWriter.EchoAdd(ConsoleEcho);
  if Assigned(fFamily.EchoCustom) then
    fWriter.EchoAdd(fFamily.EchoCustom);
  if Assigned(fFamily.fEchoRemoteClient) then
    fWriter.EchoAdd(fFamily.fEchoRemoteEvent);
  if (AutoFlushThread=nil) and (fFamily.AutoFlushTimeOut<>0) then
    fFamily.StartAutoFlush;
end;

function TSynLog.GetFileSize: Int64;
begin
  if fWriterStream<>nil then begin
    EnterCriticalSection(GlobalThreadLock);
    try
      result := fWriterStream.Size;
    finally
      LeaveCriticalSection(GlobalThreadLock);
    end;
   end else
    result := 0;
end;

procedure TSynLog.AddRecursion(aIndex: integer; aLevel: TSynLogInfo);
begin
  // at entry, aLevel is sllEnter, sllLeave or sllNone (from LogHeaderBegin)
  with fThreadContext^ do
  if cardinal(aIndex)<cardinal(RecursionCount) then
  with Recursion[aIndex] do begin
    if aLevel<>sllLeave then begin
      if Instance<>nil then
        fWriter.AddInstancePointer(Instance,'.',fFamily.WithUnitName,fFamily.WithInstancePointer);
      if MethodName<>nil then begin
        if MethodNameLocal<>mnLeave then begin
          fWriter.AddOnSameLine(MethodName);
          case MethodNameLocal of
          mnEnter:
            MethodNameLocal := mnLeave;
          mnEnterOwnMethodName: begin
            MethodNameLocal := mnLeave;
            RawUTF8(pointer(MethodName)) := ''; // release temp string
          end;
          end;
        end;
      end {$ifndef FPC} else
        TSynMapFile.Log(fWriter,Caller,false){$endif};
    end;
    if (aLevel<>sllNone) {$ifndef LINUX}and (fFrequencyTimestamp<>0){$endif} then begin
      if not fFamily.HighResolutionTimestamp then begin // no previous TSynLog.LogCurrentTime call
        {$ifdef LINUX}QueryPerformanceMicroSeconds{$else}QueryPerformanceCounter{$endif}(fCurrentTimestamp);
        dec(fCurrentTimestamp,fStartTimestamp);
      end;
      case aLevel of
      sllEnter:
        EnterTimestamp := fCurrentTimestamp;
      sllLeave:
        fWriter.AddMicroSec({$ifdef LINUX}fCurrentTimestamp-EnterTimestamp{$else}
          ((fCurrentTimestamp-EnterTimestamp)*(1000*1000))div fFrequencyTimestamp{$endif});
      end; // may be sllNone when called from LogHeaderLock()
    end;
  end;
  fWriter.AddEndOfLine(aLevel);
end;

const
  MINIMUM_EXPECTED_STACKTRACE_DEPTH = 2;

procedure TSynLog.AddStackTrace(Stack: PPtrUInt);
{$ifdef FPC}
var frames: array[0..127] of pointer;
    i, n, depth: PtrInt;
begin
  depth := fFamily.StackTraceLevel;
  if (depth=0) or (@BackTraceStrFunc=@SysBackTraceStr) then
    exit;
  try
    n := CaptureBacktrace(2,length(frames),@frames[0]);
    if n>depth then
      n := depth;
    for i := 0 to n-1 do
      if (i=0) or (frames[i]<>frames[i-1]) then
        TSynMapFile.Log(fWriter,PtrUInt(frames[i]),false); // ignore any TSynLog.*
  except // don't let any unexpected GPF break the logging process
  end;
end;
{$else}
{$ifndef CPU64}
  procedure AddStackManual(Stack: PPtrUInt);
    function check2(xret: PtrUInt): Boolean;
    var i: PtrUInt;
    begin
      result := true;
      for i := 2 to 7 do
        if PWord(xret-i)^ and $38FF=$10FF then
          exit;
      result := false;
    end;
var st, max_stack, min_stack, depth: PtrUInt;
begin
  depth := fFamily.StackTraceLevel;
  if depth=0 then
    exit;
  asm
    mov min_stack,ebp
  end;
  if Stack=nil then // if no Stack pointer set, retrieve current one
    Stack := pointer(min_stack);
  {$ifdef WITH_MAPPED_EXCEPTIONS}
  max_stack := CurrentTopOfStack;
  if max_stack=0 then begin
    ComputeCurrentTopOfStack;
    max_stack := CurrentTopOfStack;
  end;
  {$else}
  asm
    mov eax,fs:[4]
    mov max_stack, eax
    // mov eax,fs:[18h]; mov ecx,dword ptr [eax+4]; mov max_stack,ecx
  end;
  {$endif WITH_MAPPED_EXCEPTIONS}
  fWriter.AddShort(' stack trace ');
  if PtrUInt(stack)>=min_stack then
  try
    while (PtrUInt(stack)<max_stack) do begin
      st := stack^;
      if ((st>max_stack) or (st<min_stack)) and
         not IsBadReadPtr(pointer(st-8),12) and
         ((pByte(st-5)^=$E8) or check2(st)) then begin
        TSynMapFile.Log(fWriter,st,false); // ignore any TSynLog.* methods
        dec(depth);
        if depth=0 then break;
      end;
      inc(stack);
    end;
  except
    // just ignore any access violation here
  end;
end;
{$endif}
{$ifdef WITH_MAPPED_EXCEPTIONS}
begin
  AddStackManual(Stack);
end;
{$else}
var n, i: integer;
    BackTrace: array[byte] of PtrUInt;
begin
  if fFamily.StackTraceLevel<=0 then
    exit;
  {$ifdef MSWINDOWS}
  if (fFamily.StackTraceUse=stOnlyManual) or
     (RtlCaptureStackBackTraceRetrieved<>btOK) then begin
    {$ifndef FPC}
    {$ifndef CPU64}
    AddStackManual(Stack);
    {$endif}
    {$endif}
  end else begin
    try
      n := RtlCaptureStackBackTrace(2,fFamily.StackTraceLevel,@BackTrace,nil);
      if (n<MINIMUM_EXPECTED_STACKTRACE_DEPTH) and
         (fFamily.StackTraceUse<>stOnlyAPI) then begin
        {$ifndef FPC}
        {$ifndef CPU64}
        AddStackManual(Stack);
        {$endif}
        {$endif}
      end else begin
        fWriter.AddShort(' stack trace API ');
        for i := 0 to n-1 do
          TSynMapFile.Log(fWriter,BackTrace[i],false); // ignore any TSynLog.*
      end;
    except
      // just ignore any access violation here
    end;
  end;
  {$endif MSWINDOWS}
end;
{$endif WITH_MAPPED_EXCEPTIONS}
{$endif FPC}


{ TAutoLockerDebug }

constructor TAutoLockerDebug.Create(aLog: TSynLogClass; const aIdentifier: RawUTF8);
begin
  inherited Create;
  fLog := aLog;
  fIdentifier := aIdentifier;
end;

procedure TAutoLockerDebug.Enter;
begin
  fLog.Add.Log(sllTrace,'Lock % %',[fIdentifier,fCounter]);
  inherited Enter;
  fLog.Add.Log(sllTrace,'Locked % %',[fIdentifier,fCounter]);
  inc(fCounter);
end;

procedure TAutoLockerDebug.Leave;
var n: integer;
begin
  dec(fCounter);
  n := fCounter;
  fLog.Add.Log(sllTrace,'Unlock % %',[fIdentifier,n]);
  inherited Leave;
  fLog.Add.Log(sllTrace,'Unlocked % %',[fIdentifier,n]);
end;


{ TSynLogFile }

constructor TSynLogFile.Create;
var L: TSynLogInfo;
begin
  for L := low(TSynLogInfo) to high(TSynLogInfo) do
    fLogLevelsTextMap[L] := PCardinal(@LOG_LEVEL_TEXT[L][3])^; // [3] -> e.g. 'UST4'
end;

function TSynLogFile.GetLogLevelFromText(LineBeg: PUTF8Char): TSynLogInfo;
var P: PtrInt;
begin
  P := PtrInt(IntegerScan(@fLogLevelsTextMap[succ(sllNone)],
    ord(high(TSynLogInfo)),PCardinal(LineBeg+fLineLevelOffset)^));
  if P<>0 then
    result := TSynLogInfo((P-PtrInt(PtrUInt(@fLogLevelsTextMap[succ(sllNone)])))shr 2+1) else
    result := sllNone;
end;

function TSynLogFile.EventCount(const aSet: TSynLogInfos): integer;
var i: integer;
begin
  result := 0;
  if integer(aSet)<>0 then
    for i := 0 to Count-1 do
      if fLevels[i] in aSet then
        inc(result);
end;

function TSynLogFile.LineContains(const aUpperSearch: RawUTF8; aIndex: Integer): Boolean;
begin
  if (self=nil) or (cardinal(aIndex)>=cardinal(fCount)) or (aUpperSearch='') then
    result := false else
    result := GetLineContains(PUTF8Char(fLines[aIndex])+fLineTextOffset,
      fMapEnd,pointer(aUpperSearch));
end;

function TSynLogFile.EventDateTime(aIndex: integer): TDateTime;
var Timestamp: Int64;
    P: PUTF8Char;
    Y,M,D, HH,MM,SS,MS: cardinal;
begin
  if cardinal(aIndex)>=cardinal(fCount) then
    result := 0 else
    if fFreq=0 then begin
      P := fLines[aIndex]; // YYYYMMDD hhmmsszz
      if Char4ToWord(P,Y) or Char2ToByte(P+4,M) or Char2ToByte(P+6,D) or
         Char2ToByte(P+9,HH) or Char2ToByte(P+11,MM) or Char2ToByte(P+13,SS) or
         Char2ToByte(P+15,MS) then
        Iso8601ToDateTimePUTF8CharVar(P,17,result) else
        if TryEncodeDate(Y,M,D,result) then
          // MS shl 4 = 16 ms resolution in TTextWriter.AddCurrentLogTime()
          result := result+EncodeTime(HH,MM,SS,MS shl 4) else
          result := 0;
    end else
      if HexDisplayToBin(fLines[aIndex],@Timestamp,sizeof(Timestamp)) then
        result := fStartDateTime+(Timestamp/fFreqPerDay) else
        result := 0;
end;

procedure TSynLogFile.LoadFromMap(AverageLineLength: integer=32);
  var PBeg, P, PEnd: PUTF8Char;
  function StrPosI(P,PEnd: PUTF8Char; SearchUp: PAnsiChar): PUTF8Char;
  begin
    result := P;
    while result<PEnd do
      if IdemPChar(result,SearchUp) then
        exit else
        inc(result);
    result := nil;
  end;
  function GetOne(const UP: RawUTF8; var S: RawUTF8): boolean;
  var LUP: integer;
  begin
    LUP := length(UP);
    P := StrPosI(PBeg,PEnd-LUP,pointer(UP));
    if P=nil then
      result := false else begin
      FastSetString(S,PBeg,P-PBeg);
      PBeg := P+LUP;
      result := pointer(S)<>nil;
    end;
  end;
  function ComputeProperTime(var procndx: Integer): cardinal; // returns leave
  var start, i: integer;
  begin
    start := procndx;
    with fLogProcNatural[procndx] do begin
      ProperTime := Time;
      result := Index;
    end;
    repeat
      inc(result);
      if result>=Cardinal(Count) then
        break;
      case fLevels[result] of
      sllEnter: begin
        inc(procndx);
        assert(fLogProcNatural[procndx].Index=result);
        result := ComputeProperTime(procndx);
      end;
      sllLeave: begin
        with fLogProcNatural[start] do
        for i := start+1 to procndx do
          dec(ProperTime,fLogProcNatural[i].ProperTime);
        break;
      end;
      end;
    until false;
  end;
  procedure CleanLevels(Log: TSynLogFile);
  var i, aCount, pCount, dCount, dValue, dMax: integer;
  begin
    aCount := 0;
    pCount := 0;
    dCount := 0;
    dMax := Length(fDayChangeIndex);
    if dMax>0 then
      dValue := fDayChangeIndex[0] else
      dValue := -1;
    with Log do
    for i := 0 to fCount-1 do
      if fLevels[i]<>sllNone then begin
        fLevels[aCount] := fLevels[i];
        fLines[aCount] := fLines[i];
        if fThreads<>nil then
          fThreads[aCount] := fThreads[i];
        if fLevels[i]=sllEnter then begin
          fLogProcNatural[pCount].Index := aCount;
          inc(pCount);
        end;
        if dValue=i then begin
          fDayChangeIndex[dCount] := aCount;
          inc(dCount);
          if dCount<dMax then
            dValue := fDayChangeIndex[dCount];
        end;
        inc(aCount);
      end;
    Log.fCount := aCount;
    assert(pCount=Log.fLogProcNaturalCount);
    if dMax>0 then begin
      SetLength(fDayCount,dMax);
      dec(dMax);
      for i := 0 to dMax-1 do
        fDayCount[i] := fDayChangeIndex[i+1]-fDayChangeIndex[i];
      fDayCount[dMax] := aCount-fDayChangeIndex[dMax];
    end;
  end;
var aWow64, feat: RawUTF8;
    i, j, Level: integer;
    TSEnter, TSLeave: Int64;
    OK: boolean;
begin
  // 1. calculate fLines[] + fCount and fLevels[] + fLogProcNatural[] from .log content
  fLineHeaderCountToIgnore := 3; fIsJournald := false;
  if IdemPChar(pointer(fMap.Buffer),'-- LOGS BEGIN AT') then begin
    //-- Logs begin at Sun 2020-06-07 12:42:31 EEST, end at Thu 2020-06-18 18:08:52 EEST. --
    fIsJournald := true;
    fHeaderLinesCount := 1;
    fLineHeaderCountToIgnore := 1;
  end else begin
    //2020-06-18T13:28:20.754089+0300 ub[12316]:
    Iso8601ToDateTimePUTF8CharVar(pointer(fMap.Buffer),26,fStartDateTime);
    if unaligned(fStartDateTime) <> 0 then begin
      if (fMap.Buffer+8)^ <> ' ' then //20200821 14450738 ... - syn log without header
        fIsJournald := true;
      fHeaderLinesCount := 0;
      fLineHeaderCountToIgnore := 0;
    end;
  end;
  inherited LoadFromMap(100);
  // 2. fast retrieval of header
  OK := false;
  try
    // journald export or TSynLog WITHOUT regular header
    if fIsJournald or (fLineHeaderCountToIgnore=0) then begin
      if LineSizeSmallerThan(1,34) then exit;
      Iso8601ToDateTimePUTF8CharVar(fLines[1],26,fStartDateTime);
      if fStartDateTime=0 then
        exit;
    end else begin // TSynLog regular header
{  C:\Dev\lib\SQLite3\exe\TestSQL3.exe 0.0.0.0 (2011-04-07 11:09:06)
   Host=BW013299 User=G018869 CPU=1*0-15-1027 OS=2.3=5.1.2600 Wow64=0 Freq=3579545
   TSynLog 1.13 LVCL 2011-04-07 12:04:09 }
    if (fCount<=fLineHeaderCountToIgnore) or LineSizeSmallerThan(0,24) or
       not IdemPChar(fLines[1],'HOST=') or (fLevels=nil) or (fLineLevelOffset=0) then
      exit;
    PBeg := fLines[0];
    PEnd := PBeg+LineSize(0)-12;
    if PEnd<PBeg then
      exit;
    if PEnd^='(' then begin  // '(2011-04-07)' format
      if (PEnd[-1]<>' ') or (PEnd[0]<>'(') or (PEnd[11]<>')') then
        exit;
      Iso8601ToDateTimePUTF8CharVar(PEnd+1,10,fExeDate);
    end else begin  // '(2011-04-07 11:09:06)' format
      dec(PEnd,9);
      if (PEnd<PBeg) or (PEnd[-1]<>' ') or (PEnd[0]<>'(') or (PEnd[20]<>')') then
        exit;
      Iso8601ToDateTimePUTF8CharVar(PEnd+1,19,fExeDate);
    end;
    dec(PEnd);
    P := PEnd;
    repeat if P<=PBeg then exit else dec(P) until P^=' ';
    FastSetString(fExeVersion,P+1,PEnd-P-1);
    repeat dec(P); if P<=PBeg then exit; until P^<>' ';
    FastSetString(fExeName,PBeg,P-PBeg+1);
    PBeg := PUTF8Char(fLines[1])+5;
    PEnd := PUTF8Char(fLines[1])+LineSize(1);
    if not GetOne(' USER=',fHost) or not GetOne(' CPU=',fUser) or
       not GetOne(' OS=',fCPU)    or not GetOne(' WOW64=',fOsDetailed) or
       not GetOne(' FREQ=',aWow64) then
      exit;
    Split(fCPU,':',fCpu,feat);
    SynCommons.HexToBin(pointer(feat),@fIntelCPU,SizeOf(fIntelCPU));
    fWow64 := aWow64='1';
    SetInt64(PBeg,fFreq);
    while (PBeg<PEnd) and (PBeg^>' ') do inc(PBeg);
    if IdemPChar(PBeg,' INSTANCE=') then // only available for a library log
      FastSetString(fInstanceName,PBeg+10,PEnd-PBeg-10);
    fHeaderLinesCount := 4;
    while fHeaderLinesCount<fCount do begin
      if PAnsiChar(fLines[fHeaderLinesCount-1])^<' ' then
        break; // end of header = void line
      inc(fHeaderLinesCount);
    end;
    if (LineSize(fHeaderLinesCount-1)<>0) or
       LineSizeSmallerThan(fHeaderLinesCount,16) then
      exit;
    if fHeaderLinesCount<>4 then
      FastSetString(fHeaders,fLines[2],PtrUInt(fLines[fHeaderLinesCount-2])-PtrUInt(fLines[2]));
    if PWord(fLines[fHeaderLinesCount])^<>ord('0')+ord('0')shl 8 then // YYYYMMDD -> 20101225 e.g.
      fFreq := 0 else // =0 if date time, >0 if high-resolution time stamp
      fFreqPerDay := fFreq*SecsPerDay;
    P := pointer(fOSDetailed);
    fOS := TWindowsVersion(GetNextItemCardinal(P,'.'));
    if fOS<>wUnknown then
      fOSServicePack := GetNextItemCardinal(P);
    P := fLines[fHeaderLinesCount-2]; // TSQLLog 1.18.2765 ERTL FTS3 2016-07-17T22:38:03
    i := LineSize(fHeaderLinesCount-2)-19; // length('2016-07-17T22:38:03')=19
    if i>0 then begin
      FastSetString(fFramework,PAnsiChar(P),i-1);
      Iso8601ToDateTimePUTF8CharVar(P+i,19,fStartDateTime);
    end;
    if fStartDateTime=0 then
      exit;
    end;
    // 3. compute fCount and fLines[] so that all fLevels[]<>sllNone
    CleanLevels(self);
    if Length(fLevels)-fCount>16384 then begin // size down only if worth it
      SetLength(fLevels,fCount);
      if fThreads<>nil then begin
        SetLength(fThreads,fCount);
        SetLength(fThreadInfo,fThreadMax+1);
      end;
    end;
    // 4. compute customer-side profiling
    SetLength(fLogProcNatural,fLogProcNaturalCount);
    for i := 0 to fLogProcNaturalCount-1 do
      if fLogProcNatural[i].Time>=99000000 then begin // overange 99.000.000 -> compute
        Level := 0;
        j := fLogProcNatural[i].Index;
        repeat
          inc(j);
          if j=fCount then break;
          case fLevels[j] of
          sllEnter: inc(Level);
          sllLeave: if Level=0 then begin
            if fFreq=0 then // adjust huge seconds timing from date/time column
              fLogProcNatural[i].Time :=
                Round((EventDateTime(j)-EventDateTime(fLogProcNatural[i].Index))*86400000000.0)+
                fLogProcNatural[i].Time mod 1000000 else begin
              HexDisplayToBin(fLines[fLogProcNatural[i].Index],@TSEnter,sizeof(TSEnter));
              HexDisplayToBin(fLines[j],@TSLeave,sizeof(TSLeave));
              fLogProcNatural[i].Time := ((TSLeave-TSEnter)*(1000*1000)) div fFreq;
            end;
            break;
          end else dec(Level);
          end;
        until false;
      end;
    i := 0;
    while i<fLogProcNaturalCount do begin
      ComputeProperTime(i);
      inc(i);
    end;
    LogProcMerged := false; // set LogProp[]
    OK := true;
  finally
    if not OK then begin
      Finalize(fLevels); // mark not a valid .log
      Finalize(fThreads);
      fLineLevelOffset := 0;
    end;
  end;
end;

procedure TSynLogFile.AddInMemoryLine(const aNewLine: RawUTF8);
var P: PUTF8Char;
begin
  if aNewLine='' then
    exit;
  P := pointer(aNewLine);
  if (PInteger(P)^=ord('f')+ord('r')shl 8+ord('e')shl 16+ord('q')shl 24) and
     (P[4]='=') then begin
    inc(P,5);
    fFreq := GetNextItemInt64(P);
    fFreqPerDay := fFreq*SecsPerDay;
    fStartDateTime := GetNextItemDouble(P);
    UTF8DecodeToString(P,StrLen(P),string(fFileName));
  end else
    inherited AddInMemoryLine(aNewLine);
end;

procedure TSynLogFile.LogProcSort(Order: TLogProcSortOrder);
begin
  if (fLogProcNaturalCount<=1) or (Order=fLogProcSortInternalOrder) then
    Exit;
  fLogProcSortInternalOrder := Order;
  LogProcSortInternal(0,LogProcCount-1);
end;

function StrICompLeftTrim(Str1, Str2: PUTF8Char): PtrInt;
var C1, C2: integer;
begin
  while Str1^ in [#9,' '] do inc(Str1);
  while Str2^ in [#9,' '] do inc(Str2);
  repeat
    C1 := NormToUpperByte[ord(Str1^)];
    C2 := NormToUpperByte[ord(Str2^)];
    if (C1<>C2) or (C1<32) then
      break;
    Inc(Str1);
    Inc(Str2);
  until false;
  Result := C1-C2;
end;

function TSynLogFile.LogProcSortComp(A, B: PtrInt): PtrInt;
begin
  case fLogProcSortInternalOrder of
    soByName: result :=
      StrICompLeftTrim(PUTF8Char(fLines[LogProc[A].Index])+fLineTextOffset,
                       PUTF8Char(fLines[LogProc[B].Index])+fLineTextOffset);
    soByOccurrence: result := LogProc[A].Index-LogProc[B].Index;
    soByTime:       result := LogProc[B].Time-LogProc[A].Time;
    soByProperTime: result := LogProc[B].ProperTime-LogProc[A].ProperTime;
    else  result := A-B;
  end;
end;

procedure TSynLogFile.LogProcSortInternal(L, R: PtrInt);
  procedure Exchg(var P1,P2: TSynLogFileProc);
  var c: TSynLogFileProc;
  begin
    c := P1;
    P1 := P2;
    P2 := c;
  end;
var I,J,P: PtrInt;
begin
  if L<R then
  repeat
    I := L; J := R;
    P := (L + R) shr 1;
    repeat
      while LogProcSortComp(I,P)<0 do inc(I);
      while LogProcSortComp(J,P)>0 do dec(J);
      if I<=J then begin
        Exchg(LogProc[i],LogProc[j]);
        if P = I then P := J else if P = J then P := I;
        Inc(I); Dec(J);
      end;
    until I>J;
    if J - L < R - I then begin // use recursion only for smaller range
      if L < J then
        LogProcSortInternal(L, J);
      L := I;
    end else begin
      if I < R then
        LogProcSortInternal(I, R);
      R := J;
    end;
  until L >= R;
end;

procedure TSynLogFile.ProcessOneLine(LineBeg, LineEnd: PUTF8Char);
  function DecodeMicroSec(P: PByte): integer;
  var B: integer;
  begin // fast decode 00.020.006 at the end of the line
    B := ConvertHexToBin[P^];   // 00
    if B>9 then
      result := -1 else begin
      result := B;
      inc(P);
      B := ConvertHexToBin[P^];
      if B>9 then
        result := -1 else begin
        result := result*10+B;
        inc(P,2);                 // .
        B := ConvertHexToBin[P^]; // 020
        if B>9 then
          result := -1 else begin
          result := result*10+B;
          inc(P);
          B := ConvertHexToBin[P^];
          if B>9 then
            result := -1 else begin
            result := result*10+B;
            inc(P);
            B := ConvertHexToBin[P^];
            if B>9 then
              result := -1 else begin
              result := result*10+B;
              inc(P,2);                 // .
              B := ConvertHexToBin[P^]; // 006
              if B>9 then
                result := -1 else begin
                result := result*10+B;
                inc(P);
                B := ConvertHexToBin[P^];
                if B>9 then
                  result := -1 else begin
                  result := result*10+B;
                  inc(P);
                  B := ConvertHexToBin[P^];
                  if B>9 then
                    result := -1 else
                    result := result*10+B;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
var thread,n: cardinal;
    MS: integer;
    L: TSynLogInfo;
    p: PUTF8Char;
    dcOffset: integer;
begin
  inherited ProcessOneLine(LineBeg,LineEnd);
  if length(fLevels)<fLinesMax then
    SetLength(fLevels,fLinesMax);
  if (fCount<=fLineHeaderCountToIgnore) or (LineEnd-LineBeg<24) then
    exit;
  if fIsJournald then
    dcOffset := 2 else // point to last 2 digit of year
    dcOffset := 0;
  if fLineLevelOffset=0 then begin
    if (fCount>50) or not (LineBeg[0] in ['0'..'9']) then
      exit; // definitively does not sound like a .log content
    if fIsJournald then begin
      p := PosChar(LineBeg, ']'); // time proc[pid]:
      if p=nil then
        exit; // not a log
      fLineLevelOffset := (p - LineBeg) + 4; // ":  "
      fDayCurrent := PInt64(LineBeg+dcOffset)^;
    end else if LineBeg[8]=' ' then begin
      // YYYYMMDD HHMMSS is one char bigger than Timestamp
      fLineLevelOffset := 19;
      fDayCurrent := PInt64(LineBeg)^;
      AddInteger(fDayChangeIndex,fCount-1);
    end else
      fLineLevelOffset := 18;
    if (LineBeg[fLineLevelOffset]='!') or // ! = thread 1
       (GetLogLevelFromText(LineBeg)=sllNone) then begin
      inc(fLineLevelOffset,3);
      fThreadsCount := fLinesMax;
      SetLength(fThreads,fLinesMax);
    end;
    fLineTextOffset := fLineLevelOffset+4;
    SetLength(fLogProcStack, fLinesMax);
    SetLength(fLogProcStackCount, fLinesMax);
  end;
  L := GetLogLevelFromText(LineBeg);
  if L=sllNone then
    exit;
  if (fDayChangeIndex<>nil) and (fDayCurrent<>PInt64(LineBeg+dcOffset)^) then begin
    fDayCurrent := PInt64(LineBeg+dcOffset)^;
    AddInteger(fDayChangeIndex,fCount-1);
  end;
  if fThreads<>nil then begin
    if fThreadsCount<fLinesMax then begin
      fThreadsCount := fLinesMax;
      SetLength(fThreads,fLinesMax);
    end;
    thread := Chars3ToInt18(LineBeg+fLineLevelOffset-5);
    fThreads[fCount-1] := thread;
    if thread>fThreadMax then begin
      fThreadMax := thread;
      if thread>=fThreadInfoMax then begin
        fThreadInfoMax := thread+256;
        SetLength(fThreadInfo,fThreadInfoMax);
      end;
    end;
    inc(fThreadInfo[thread].Rows);
    if (L=sllInfo) and IdemPChar(LineBeg+fLineLevelOffset+5,'SETTHREADNAME ') then
      with fThreadInfo[thread] do begin // see TSynLog.LogThreadName
        n := length(SetThreadName);
        SetLength(SetThreadName,n+1);
        SetThreadName[n] := LineBeg;
      end;
  end else
    thread := 0;
  fLevels[fCount-1] := L; // need exact match of level text
  include(fLevelUsed,L);
  case L of
  sllEnter: begin
    if Cardinal(fLogProcStackCount[thread])>=Cardinal(length(fLogProcStack[thread])) then
      SetLength(fLogProcStack[thread],length(fLogProcStack[thread])+256);
    fLogProcStack[thread][fLogProcStackCount[thread]] := fLogProcNaturalCount;
    inc(fLogProcStackCount[thread]);
    if Cardinal(fLogProcNaturalCount)>=Cardinal(length(fLogProcNatural)) then
      SetLength(fLogProcNatural,length(fLogProcNatural)+32768);
    // fLogProcNatural[].Index will be set in TSynLogFile.LoadFromMap
    inc(fLogProcNaturalCount);
  end;
  sllLeave:
  if (LineEnd-LineBeg>10) and (LineEnd[-4]='.') and (LineEnd[-8]='.') and
     (fLogProcStackCount[thread]>0) then begin // 00.020.006
    MS := DecodeMicroSec(PByte(LineEnd-10));
    if MS>=0 then begin
      dec(fLogProcStackCount[thread]);
      fLogProcNatural[fLogProcStack[thread][fLogProcStackCount[thread]]].Time := MS;
    end;
  end;
  end;
end;

function TSynLogFile.ThreadRows(ThreadID: integer): cardinal;
begin
  if fThreadInfo<>nil then
    result := fThreadInfo[ThreadID].Rows else
    result := 0;
end;

function TSynLogFile.ThreadName(ThreadID, CurrentLogIndex: integer): RawUTF8;
var i: integer;
    lineptr: PtrUInt;
    found: pointer;
begin
  if ThreadID=1 then
    result := 'Main Thread' else begin
    result := '';
    if cardinal(ThreadID)<=fThreadMax then
      with fThreadInfo[ThreadID] do
        if SetThreadName<>nil then begin
          found := SetThreadName[0];
          if cardinal(CurrentLogIndex)<cardinal(fCount) then begin
            lineptr := PtrUInt(fLines[CurrentLogIndex]);
            for i := length(SetThreadName)-1 downto 1 do
              if lineptr>=PtrUInt(SetThreadName[i]) then begin
                found := SetThreadName[i];
                break;
              end;
          end;
          FastSetString(result,found,GetLineSize(found,fMapEnd));
          delete(result,1,PosEx('=',result,40));
        end;
    if result='' then
      result := 'Thread';
  end;
  if cardinal(ThreadID)<=fThreadMax then
    result := FormatUTF8('% % (% rows)',[ThreadID,result,fThreadInfo[ThreadID].Rows]);
end;

function TSynLogFile.ThreadNames(CurrentLogIndex: integer): TRawUTF8DynArray;
var i: integer;
begin
  result := nil;
  SetLength(result,fThreadMax);
  if fThreadInfo=nil then
    exit;
  for i := 1 to fThreadMax do
    result[i-1] := ThreadName(i,CurrentLogIndex);
end;

procedure TSynLogFile.GetDays(out Days: TDateTimeDynArray);
var i,n: integer;
begin
  n := length(fDayChangeIndex);
  SetLength(Days,n);
  for i := 0 to n-1 do
    Days[i] := EventDateTime(fDayChangeIndex[i]);
end;

function TSynLogFile.GetEventText(index: integer): RawUTF8;
var L: cardinal;
begin
  if (self=nil) or (cardinal(index)>=cardinal(fCount)) then
    result := '' else begin
    L := GetLineSize(fLines[index],fMapEnd);
    if L<=fLineTextOffset then
      result := '' else
      FastSetString(result,PAnsiChar(fLines[index])+fLineTextOffset,L-fLineTextOffset);
  end;
end;

function TSynLogFile.EventString(index: integer; const replaceTabs: RawUTF8;
  maxutf8len: Integer; includeFirstColumns: boolean): string;
var tmp: RawUTF8;
    header: string;
begin
  tmp := GetEventText(index);
  if tmp = '' then begin
    result := '';
    exit;
  end;
  if maxutf8len>0 then
    Utf8TruncateToLength(tmp,maxutf8len);
  if replaceTabs<>'' then
    tmp := StringReplaceAll(tmp,#9,replaceTabs);
  if IsValidUTF8(pointer(tmp)) then
    result := UTF8ToString(tmp) else
    {$ifdef UNICODE}
    result := CurrentAnsiConvert.AnsiToUnicodeString(pointer(tmp),length(tmp));
    {$else}
    result := tmp;
    {$endif}
  if includeFirstColumns then begin
    UTF8DecodeToString(fLines[index],fLineTextOffset,header);
    result := header+result;
  end;
end;

procedure TSynLogFile.SetLogProcMerged(const Value: boolean);
var i: integer;
    P: ^TSynLogFileProc;
    O: TLogProcSortOrder;
begin
  fLogProcIsMerged := Value;
  O := fLogProcSortInternalOrder;
  if Value then begin
    if fLogProcMerged=nil then begin
      fLogProcCurrent := pointer(fLogProcNatural);
      fLogProcCurrentCount := fLogProcNaturalCount;
      LogProcSort(soByName); // sort by name to identify unique
      SetLength(fLogProcMerged,fLogProcNaturalCount);
      fLogProcMergedCount := 0;
      i := 0;
      P := pointer(fLogProcNatural);
      repeat
        with fLogProcMerged[fLogProcMergedCount] do begin
          repeat
            Index := P^.Index;
            inc(Time,P^.Time);
            inc(ProperTime,P^.ProperTime);
            inc(i);
            inc(P);
          until (i>=fLogProcNaturalCount) or
                (StrICompLeftTrim(PUTF8Char(fLines[LogProc[i-1].Index])+22,
                 PUTF8Char(fLines[P^.Index])+22)<>0);
        end;
        inc(fLogProcMergedCount);
      until i>=fLogProcNaturalCount;
      SetLength(fLogProcMerged,fLogProcMergedCount);
    end;
    fLogProcCurrent := pointer(fLogProcMerged);
    fLogProcCurrentCount := fLogProcMergedCount;
  end else begin
    fLogProcCurrent := pointer(fLogProcNatural);
    fLogProcCurrentCount := fLogProcNaturalCount;
  end;
  fLogProcSortInternalOrder := soNone;
  LogProcSort(O); // restore previous sort order
end;


function EventArchiveDelete(const aOldLogFileName, aDestinationPath: TFileName): boolean;
begin
  result := DeleteFile(aOldLogFileName);
end;

function EventArchiveSynLZ(const aOldLogFileName, aDestinationPath: TFileName): boolean;
begin // aDestinationPath = 'ArchivePath\log\YYYYMM\'
  Result := false;
  if (aOldLogFileName<>'') and FileExists(aOldLogFileName) then
  try
    if DirectoryExists(aDestinationPath) or CreateDir(aDestinationPath) then
      if FileSynLZ(aOldLogFileName,
         aDestinationPath+ExtractFileName(aOldLogFileName)+'.synlz',LOG_MAGIC) then
        result := DeleteFile(aOldLogFileName);
  except
    on Exception do
      result := false;
  end;
end;


{$ifndef DELPHI5OROLDER} // IInvokable was introduced with Delphi 6

{ TSynLogCallbacks }

constructor TSynLogCallbacks.Create(aTrackedLog: TSynLogFamily);
begin
  inherited Create;
  Registrations.Init(TypeInfo(TSynLogCallbackDynArray),Registration,@fCount);
  TrackedLog := aTrackedLog;
  aTrackedLog.EchoRemoteStart(self,OnEcho,false);
end;

destructor TSynLogCallbacks.Destroy;
begin
  if TrackedLog<>nil then
    if TrackedLog.fEchoRemoteClient=self then
      TrackedLog.EchoRemoteStop; // unregister OnEcho() event
  inherited Destroy;
end;

function TSynLogCallbacks.OnEcho(Sender: TTextWriter; Level: TSynLogInfo;
  const Text: RawUTF8): boolean;
var i: integer;
begin
  result := false;
  if (Count=0) or fCurrentlyEchoing then
    exit;
  Safe.Lock;
  try
    fCurrentlyEchoing := true; // avoid stack overflow if exception below
    for i := Count-1 downto 0 do
      if Level in Registration[i].Levels then
      try
        Registration[i].Callback.Log(Level,Text);
        result := true;
      except
        Registrations.Delete(i); // safer to unsubscribe ASAP
      end;
  finally
    fCurrentlyEchoing := false;
    Safe.UnLock;
  end;
end;

function TSynLogCallbacks.Subscribe(const Levels: TSynLogInfos;
  const Callback: ISynLogCallback; ReceiveExistingKB: cardinal): integer;
var Reg: TSynLogCallback;
    previousContent: RawUTF8;
begin
  if Assigned(Callback) then
  try
    if ReceiveExistingKB>0 then begin
      EnterCriticalSection(GlobalThreadLock);
      previousContent := TrackedLog.GetExistingLog(ReceiveExistingKB);
      if TrackedLog.HighResolutionTimestamp and (TrackedLog.fGlobalLog<>nil) then
        with TrackedLog.fGlobalLog do
        Callback.Log(sllNone,FormatUTF8('freq=%,%,%',[{$ifdef LINUX}1000000{$else}
          fFrequencyTimestamp{$endif},double(fStartTimestampDateTime),fFileName]));
      Callback.Log(sllNone,previousContent);
    end;
    Reg.Levels := Levels;
    Reg.Callback := Callback;
    Safe.Lock;
    try
      Registrations.Add(Reg);
    finally
      Safe.UnLock;
    end;
  finally
    if ReceiveExistingKB>0 then
      LeaveCriticalSection(GlobalThreadLock);
  end;
  result := length(previousContent);
end;

procedure TSynLogCallbacks.Unsubscribe(const Callback: ISynLogCallback);
var i: integer;
begin
  Safe.Lock;
  try
    for i := Count-1 downto 0 do
      if Registration[i].Callback=Callback then
        Registrations.Delete(i);
  finally
    Safe.UnLock;
  end;
end;

{$endif DELPHI5OROLDER}


{ TSynLogFileView }

procedure TSynLogFileView.LoadFromMap(AverageLineLength: integer);
begin
  inherited LoadFromMap(AverageLineLength);
  if fLevels<>nil then begin
    SetLength(fSelected,fCount);
    fSelectedCount := fCount;
    FillIncreasing(pointer(fSelected),0,fCount);
    SetLength(fThreadSelected,(fThreadMax shr 3)+1);
    SetAllThreads(true);
  end;
end;

procedure TSynLogFileView.AddInMemoryLine(const aNewLine: RawUTF8);
var index: integer;
    tm: cardinal;
begin
  tm := fThreadMax;
  inherited AddInMemoryLine(aNewLine);
  index := Count-1;
  if EventLevel[index] in fEvents then
    AddInteger(fSelected,fSelectedCount,index);
  if tm<>fThreadMax then begin
    tm := (fThreadMax shr 3)+1;
    if integer(tm)<>length(fThreadSelected) then
      SetLength(fThreadSelected,tm);
    SetBitPtr(pointer(fThreadSelected),fThreadMax-1)
  end;
end;

const
  TIME_FORMAT = 'hh:mm:ss.zzz';
  MAXLOGLINES = 300;

function TSynLogFileView.GetLineForMemo(aRow,aTop,aBottom: integer): string;
var tim: string;
    elapsed: TDateTime;
begin
  result := '';
  if cardinal(aRow)<cardinal(fSelectedCount) then
    aRow := fSelected[aRow];
  if cardinal(aRow)<cardinal(fCount) then begin
    result := EventString(aRow,'',0,true);
    if aBottom>aTop then begin
      elapsed := EventDateTime(aBottom)-EventDateTime(aTop);
      if Freq=0 then begin
        DateTimeToString(tim,TIME_FORMAT,elapsed);
        result := tim+#13#10+result;
      end else begin
        tim := IntToStr(trunc(elapsed*MSecsPerDay*1000) mod 1000);
        result := StringOfChar('0',3-length(tim))+tim+#13#10+result;
        DateTimeToString(tim,TIME_FORMAT,elapsed);
        result := tim+'.'+result;
      end;
      result := FormatString('% lines - time elapsed: %',[aBottom-aTop+1,result]);
    end;
  end;
end;

function TSynLogFileView.GetLineForClipboard(aRow: integer): string;
var dt: TDateTime;
begin
  result := '';
  if cardinal(aRow)<cardinal(fSelectedCount) then
    aRow := fSelected[aRow];
  if cardinal(aRow)<cardinal(fCount) then begin
    dt := EventDateTime(aRow);
    FormatString('% %'#9'%'#9,[DateToStr(dt),FormatDateTime(TIME_FORMAT,dt),
      _LogInfoCaption[EventLevel[aRow]]],result);
    if fThreads<>nil then
      result := result+IntToString(cardinal(fThreads[aRow]))+#9;
    result := result+EventString(aRow,'   ');
  end;
end;

function TSynLogFileView.GetCell(aCol, aRow: integer; out aLevel: TSynLogInfo): string;
begin
  aLevel := sllNone;
  result := '';
  if self<>nil then
    if cardinal(aRow)<cardinal(fSelectedCount) then begin
      aRow := fSelected[aRow];
      case aCol of
      0: DateTimeToString(result,TIME_FORMAT,EventDateTime(aRow));
      1: result := _LogInfoCaption[EventLevel[aRow]];
      2: if fThreads<>nil then
           result := IntToString(cardinal(fThreads[aRow]));
      3: result := EventString(aRow,'   ',MAXLOGLINES);
      end;
      aLevel := EventLevel[aRow];
    end else
      result := EventString(aRow,'   ',MAXLOGLINES);
end;

function TSynLogFileView.SearchNextEvent(aEvent: TSynLogInfo; aRow: integer): integer;
begin
  if cardinal(aRow)<cardinal(fSelectedCount) then begin
    // search from next item
    for result := aRow+1 to fSelectedCount-1 do
      if fLevels[fSelected[result]]=aEvent then
        exit;
    // search from beginning
    for result := 0 to aRow-1 do
      if fLevels[fSelected[result]]=aEvent then
        exit;
  end;
  result := -1;
end;

function TSynLogFileView.SearchNextText(
  const aPattern: RawUTF8; aRow, aDelta: integer): integer;
begin
  result := -1;
  if (self=nil) or (aPattern='') then
    exit;
  if fLevels=nil then begin // plain text search
    // search from next item
    for result := aRow+aDelta to fCount-1 do
      if LineContains(aPattern,result) then
        exit;
    // search from beginning
    for result := 0 to aRow-1 do
      if LineContains(aPattern,result) then
        exit;
  end else begin
    // search from next item
    for result := aRow+aDelta to fSelectedCount-1 do
      if LineContains(aPattern,fSelected[result]) then
        exit;
    // search from beginning
    for result := 0 to aRow-1 do
      if LineContains(aPattern,fSelected[result]) then
        exit;
  end;
  result := -1;
end;

function TSynLogFileView.SearchPreviousText(
  const aPattern: RawUTF8; aRow: integer): integer;
begin
  result := -1;
  if (self=nil) or (aPattern='') then
    exit;
  if fLevels=nil then begin // plain text search
    // search from previous item
    for result := aRow-1 downto 0 do
      if LineContains(aPattern,result) then
        exit;
    // search from end
    for result := fCount-1 downto aRow+1 do
      if LineContains(aPattern,result) then
        exit;
  end else begin
    // search from previous item
    for result := aRow-1 downto 0 do
      if LineContains(aPattern,fSelected[result]) then
        exit;
    // search from end
    for result := fCount-1 downto aRow+1 do
      if LineContains(aPattern,fSelected[result]) then
        exit;
  end;
  result := -1;
end;

function TSynLogFileView.SearchThread(aThreadID: word; aRow: integer): integer;
begin
  if (self<>nil) and (cardinal(aRow)<cardinal(fSelectedCount)) and (fThreads<>nil) then begin
    for result := aRow+1 to fSelectedCount-1 do
      if fThreads[fSelected[result]]=aThreadID then
        exit;
    for result := 0 to aRow-1 do
      if fThreads[fSelected[result]]=aThreadID then
        exit;
  end;
  result := -1;
end;

function TSynLogFileView.SearchNextThread(aRow: integer): integer;
var currentThreadID: Word;
begin
  if (self<>nil) and (cardinal(aRow)<cardinal(fSelectedCount)) and (fThreads<>nil) then begin
    result := aRow;
    currentThreadID := fThreads[fSelected[result]];
    repeat
      inc(result);
      if result=fSelectedCount then
        break;
      if fThreads[fSelected[result]]<>currentThreadID then
        exit; // found
    until false;
  end;
  result := -1;
end;

function TSynLogFileView.SearchNextSameThread(aRow: integer): integer;
var currentThreadID: Word;
begin
  if (self<>nil) and (cardinal(aRow)<cardinal(fSelectedCount)) and (fThreads<>nil) then begin
    result := aRow;
    currentThreadID := fThreads[fSelected[result]];
    repeat
      inc(result);
      if result=fSelectedCount then
        break;
      if fThreads[fSelected[result]]=currentThreadID then
        exit; // found
    until false;
  end;
  result := -1;
end;

function TSynLogFileView.SearchPreviousSameThread(aRow: integer): integer;
var currentThreadID: Word;
begin
  if (self<>nil) and (cardinal(aRow)<cardinal(fSelectedCount)) and (fThreads<>nil) then begin
    result := aRow;
    currentThreadID := fThreads[fSelected[result]];
    repeat
      dec(result);
      if result<0 then
        break;
      if fThreads[fSelected[result]]=currentThreadID then
        exit; // found
    until false;
  end;
  result := -1;
end;

function TSynLogFileView.SearchEnterLeave(aRow: integer): integer;
var Level,ndx: integer;
    currentThreadID: Word;
begin
  if (self=nil) or (cardinal(aRow)>=cardinal(fSelectedCount)) then begin
    result := -1;
    exit;
  end;
  Level := 0;
  result := aRow;
  ndx := fSelected[result];
  if EventThread<>nil then
    currentThreadID := EventThread[ndx] else
    currentThreadID := 0;
  case EventLevel[ndx] of
  sllEnter: // retrieve corresponding Leave event
    repeat
      inc(result);
      if result>=fSelectedCount then
        break;
      ndx := fSelected[result];
      case EventLevel[ndx] of
      sllEnter:
        if (currentThreadID=0) or (EventThread[ndx]=currentThreadID) then
          inc(Level);
      sllLeave:
        if (currentThreadID=0) or (EventThread[ndx]=currentThreadID) then
          if Level=0 then
            exit else
            dec(Level);
      end;
    until false;
  sllLeave: // retrieve corresponding Enter event
    repeat
      dec(result);
      if result<0 then
        break;
      ndx := fSelected[result];
      case EventLevel[ndx] of
      sllLeave:
        if (currentThreadID=0) or (EventThread[ndx]=currentThreadID) then
          inc(Level);
      sllEnter:
        if (currentThreadID=0) or (EventThread[ndx]=currentThreadID) then
          if Level=0 then
            exit else
            dec(Level);
      end;
    until false;
  end;
  result := -1;
end;

function TSynLogFileView.SearchNextSelected(aIndex: integer): integer;
begin
  for result := 0 to fSelectedCount-1 do
    if fSelected[result]>=aIndex then
      exit; // TODO: use faster binary search instead of this O(n) value?
  result := -1;
end;

function TSynLogFileView.Select(aRow: integer): integer;
var i, search: integer;
begin
  result := 0;
  if integer(fEvents)<>0 then begin
    if cardinal(aRow)<cardinal(fSelectedCount) then
      search := fSelected[aRow] else
      search := maxInt;
    fSelectedCount := 0;
    for i := 0 to Count-1 do
      if fLevels[i] in fEvents then
        if (fThreads=nil) or GetBitPtr(pointer(fThreadSelected),fThreads[i]-1) then begin
          if search<=i then begin
            result := fSelectedCount; // found the closed selected index
            search := maxInt;
          end;
          if fSelectedCount=length(fSelected) then
            SetLength(fSelected,NextGrow(fSelectedCount));
          fSelected[fSelectedCount] := i;
          inc(fSelectedCount);
        end;
  end;
end;

procedure TSynLogFileView.SetAllThreads(enabled: boolean);
const B: array[boolean] of byte = (0, 255);
begin
  FillcharFast(fThreadSelected[0],length(fThreadSelected),B[enabled]);
end;

procedure TSynLogFileView.SetThreads(thread: integer; value: boolean);
begin
  dec(thread);
  if cardinal(thread)<fThreadMax then
    if value then
      SetBitPtr(pointer(fThreadSelected),thread) else
      UnSetBitPtr(pointer(fThreadSelected),thread);
end;

function TSynLogFileView.GetThreads(thread: integer): boolean;
begin
  dec(thread);
  result := (cardinal(thread)<fThreadMax) and GetBitPtr(pointer(fThreadSelected),thread);
end;

const
  _TSynMapSymbol = 'Name:RawUTF8 Start,Stop:integer';
  _TSynMapUnit = 'Symbol:TSynMapSymbol FileName:RawUTF8 Line,Addr:TIntegerDynArray';

initialization
  assert(ord(sfLocal7)=23);
  assert(ord(ssDebug)=7);
  InitializeCriticalSection(GlobalThreadLock);   // deleted with the process
  SynLogFamily := TSynObjectList.Create;         // TSynLogFamily instances
  SynLogFileList := TSynObjectListLocked.Create; // TSynLog instances
  {$ifndef NOEXCEPTIONINTERCEPT}
  DefaultSynLogExceptionToStr := InternalDefaultSynLogExceptionToStr;
  {$endif}
  GetEnumTrimmedNames(TypeInfo(TSynLogInfo),@_LogInfoText);
  GetEnumCaptions(TypeInfo(TSynLogInfo),@_LogInfoCaption);
  _LogInfoCaption[sllNone] := '';
  TTextWriter.RegisterCustomJSONSerializerFromText([
    TypeInfo(TSynMapSymbol),_TSynMapSymbol,
    TypeInfo(TSynMapUnit),_TSynMapUnit]);

finalization
  SynLogFileList.Free; // release in proper order: TSynLog then TSynLogFamily
  SynLogFamily.Free;
  {$ifndef NOEXCEPTIONINTERCEPT}
  GlobalCurrentHandleExceptionSynLog := nil; // paranoid
  {$endif}
end.
