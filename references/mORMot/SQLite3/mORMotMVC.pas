/// implements MVC patterns over mORMot's ORM/SOA and SynMustache
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotMVC;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2018 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2018
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - achechulin

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


  Version 1.18
  - first public release, corresponding to mORMot Framework 1.18
    and feature request [bd94c11ab1]

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$else}
  {$ifdef FPC}
  SynFPCLinux,
  {$endif}
  {$ifdef KYLIX3}
  Types,
  LibC,
  SynKylix,
  {$endif}
  {$endif}
  SysUtils,
  Classes,
  {$ifndef LVCL}
  Contnrs,
  {$endif}
  Variants,
  SynCommons,
  SynLog,
  SynCrypto,
  SynMustache,
  mORMot,
  mORMotWrappers;

{ ====== Views ====== }

type
  /// define a particular rendered View
  // - as rendered by TMVCViewsAbtract.Render() method
  TMVCView = record
    /// the low-level content of this View
    Content: RawByteString;
    /// the MIME content type of this View
    ContentType: RawUTF8;
  end;

  /// an abstract class able to implement Views
  TMVCViewsAbtract = class
  protected
    fFactory: TInterfaceFactory;
    fLogClass: TSynLogClass;
    fViewTemplateFolder: TFileName;
    fFactoryErrorIndex: integer;
    function GetViewStaticFolder: TFileName;
    /// overriden implementations should return the rendered content
    procedure Render(methodIndex: Integer; const Context: variant; var View: TMVCView); virtual; abstract;
  public
    /// initialize the class
    constructor Create(aInterface: PTypeInfo; aLogClass: TSynLogClass);
    /// read-only access to the associated factory for the implementation class
    property Factory: TInterfaceFactory read fFactory;
    /// read-only access to the local folder containing the Mustache views
    property ViewTemplateFolder: TFileName read fViewTemplateFolder;
    /// retrieve the .static local folder name
    property ViewStaticFolder: TFileName read GetViewStaticFolder;
  end;

  /// general parameters defining the Mustache Views process
  // - used as a separate value so that we would be able to store the
  // settings in a file, e.g. encoded as a JSON object
  TMVCViewsMustacheParameters = record
    /// where the mustache template files are stored
    // - if not set, will search in a 'Views' folder under the current executable
    Folder: TFileName;
    /// the file extensions to search in the given Folder, specified as CSV
    // - if not set, will search for 'html,json,css'
    CSVExtensions: TFileName;
    /// defines if the view files should be checked for modification
    // - any value would automatically update the rendering template, if the file
    // changed after a given number of seconds - default is 5 seconds
    // - setting 0 would be slightly faster, since content would never be checked
    FileTimestampMonitorAfterSeconds: cardinal;
    /// file extension (e.g. '.html') to be used to create void templates
    // - default '' will create no void template file in the given Folder
    ExtensionForNotExistingTemplate: TFileName;
    /// set of block helpers to be registered to TSynMustache
    // - default will use TSynMustache.HelpersGetStandardList definition
    Helpers: TSynMustacheHelpers;
  end;

  /// a class able to implement Views using Mustache templates
  TMVCViewsMustache = class(TMVCViewsAbtract)
  protected
    fViewTemplateFileTimestampMonitor: cardinal;
    fViewPartials: TSynMustachePartials;
    fViewHelpers: TSynMustacheHelpers;
    fViews: array of record // follows fFactory.Methods[]
      Mustache: TSynMustache;
      Template: RawUTF8;
      MethodName: TFileName;
      SearchPattern: TFileName;
      FileName: TFileName;
      ShortFileName: TFileName;
      FileExt: TFileName;
      ContentType: RawUTF8;
      Locker: IAutoLocker;
      FileTimestamp: TDateTime;
      FileTimestampCheckTick: Int64;
    end;
    function GetRenderer(methodIndex: integer; out aContentType: RawUTF8): TSynMustache;
    class procedure md5(const Value: variant; out result: variant);
    class procedure sha1(const Value: variant; out result: variant);
    class procedure sha256(const Value: variant; out result: variant);
    /// overriden implementations should return the rendered content
    procedure Render(methodIndex: Integer; const Context: variant; var View: TMVCView); override;
  public
    /// create an instance of this ViewModel implementation class
    // - define the associated REST instance, the interface definition and the
    // local folder where the mustache template files are stored
    // - will search and parse the matching views (and associated *.partial)
    constructor Create(aInterface: PTypeInfo;
      const aParameters: TMVCViewsMustacheParameters;
      aLogClass: TSynLogClass=nil); reintroduce; overload; virtual;
    /// create an instance of this ViewModel implementation class
    // - this overloaded version will use default parameters (i.e. search for
    // html+json+css in the "Views" sub-folder under the executable)
    // - will search and parse the matching views (and associated *.partial),
    // optionally creating void templates for any missing view
    constructor Create(aInterface: PTypeInfo; aLogClass: TSynLogClass=nil;
      aExtensionForNotExistingTemplate: TFileName=''); overload;
    /// will add the supplied Expression Helpers definition
    // - returns self so that may be called in a fluent interface
    function RegisterExpressionHelpers(const aNames: array of RawUTF8;
      const aEvents: array of TSynMustacheHelperEvent): TMVCViewsMustache;
    /// will add Expression Helpers for some ORM tables
    // - e.g. to read a TSQLMyRecord from its ID value and put its fields
    // in the current rendering data context, you can write:
    // ! aView.RegisterExpressionHelpersForTables(aServer,[TSQLMyRecord]);
    // then use the following Mustache tag
    // ! {{#TSQLMyRecord MyRecordID}} ... {{/TSQLMyRecord MyRecordID}}
    // - returns self so that may be called in a fluent interface
    function RegisterExpressionHelpersForTables(aRest: TSQLRest;
      const aTables: array of TSQLRecordClass): TMVCViewsMustache; overload;
    /// will add Expression Helpers for all ORM tables of the supplied model
    // - e.g. to read a TSQLMyRecord from its ID value and put its fields
    // in the current rendering data context, you can write:
    // ! aView.RegisterExpressionHelpersForTables(aServer);
    // then use the following Mustache tag
    // ! {{#TSQLMyRecord MyRecordID}} ... {{/TSQLMyRecord MyRecordID}}
    // - returns self so that may be called in a fluent interface
    function RegisterExpressionHelpersForTables(
      aRest: TSQLRest): TMVCViewsMustache; overload;
    /// will add some Expression Helpers for hashing
    // - i.e. md5, sha1 and sha256 hashing
    // - would allow e.g. to compute a Gravatar URI via:
    // ! <img src=http://www.gravatar.com/avatar/{{md5 email}}?s=200></img>
    function RegisterExpressionHelpersForCrypto: TMVCViewsMustache;
    /// finalize the instance
    destructor Destroy; override;
  end;


{ ====== Sessions ====== }

type
  /// an abstract class able to implement ViewModel/Controller sessions
  // - see TMVCSessionWithCookies to implement cookie-based sessions
  // - this kind of ViewModel will implement client side storage of sessions,
  // storing any (simple) record content on the browser client side
  // - at login, a record containing session-related information (session ID,
  // display and login name, preferences, rights...) can be computed only once
  // on the server side from the Model, then stored on the client side (typically
  // in a cookie): later on, session information can be retrieved by the server
  // logic (via CheckAndRetrieve - note that any security attribute should be
  // verified against the Model), then the renderer (CheckAndRetrieveInfo
  // returning the record as TDocVariant in the data context "Session" field) -
  // such a pattern is very efficient and allows good scaling
  // - session are expected to be tied to the TMVCSessionAbstract instance
  // lifetime, so are lost after server restart, unless they are persisted
  // via LoadContext/SaveContext methods
  TMVCSessionAbstract = class
  public
    /// create an instance of this ViewModel implementation class
    constructor Create; virtual;
    /// will create a new session
    // - setting an optional record data, and returning the internal session ID
    // - you can supply a time period, after which the session will expire -
    // default is 1 hour - note that overriden methods may not implement it
    function Initialize(PRecordData: pointer=nil; PRecordTypeInfo: pointer=nil;
      SessionTimeOutMinutes: cardinal=60): integer; virtual; abstract;
    /// fast check if there is a session associated to the current context
    function Exists: boolean; virtual; abstract;
    /// retrieve the current session ID
    // - can optionally retrieve the associated record Data parameter
    function CheckAndRetrieve(PRecordData: pointer=nil; PRecordTypeInfo: pointer=nil): integer; virtual; abstract;
    /// retrieve the session information as a JSON object
    // - returned as a TDocVariant, including any associated record Data
    // - will call CheckAndRetrieve() then RecordSaveJSON() and _JsonFast()
    function CheckAndRetrieveInfo(PRecordDataTypeInfo: pointer): variant; virtual;
    /// clear the session
    procedure Finalize; virtual; abstract;
    /// return all session generation information as ready-to-be stored string
    // - to be retrieved via LoadContext, e.g. after restart
    function SaveContext: RawUTF8; virtual; abstract;
    /// restore session generation information from SaveContext format
    // - returns TRUE on success
    function LoadContext(const Saved: RawUTF8): boolean; virtual; abstract;
  end;

  /// information used by TMVCSessionWithCookies for cookie generation
  // - i.e. the session ID, cookie name, encryption and HMAC secret keys
  // - this data can be persisted so that the very same cookie information
  // are available after server restart
  TMVCSessionWithCookiesContext = packed record
    /// the cookie name, used for storage on the client side
    CookieName: RawUTF8;
    /// an increasing counter, to implement unique session ID
    SessionCount: integer;
    /// secret information, used for HMAC digital signature of cookie content
    Secret: THMAC_CRC32C;
    /// random IV used as CTR on Crypt[] secret key
    CryptNonce: Cardinal;
    /// secret information, used for encryption of the cookie content
    Crypt: array[byte] of byte;
  end;

  /// a class able to implement ViewModel/Controller sessions with cookies
  // - this kind of ViewModel will implement cookie-based sessions, able to
  // store any (simple) record content in the cookie, on the browser client side
  // - those cookies have the same feature set than JWT, but with a lower
  // payload (thanks to binary serialization), and cookie safety (not accessible
  // from JavaScript): they are digitally signed (with HMAC-CRC32C and a
  // temporary secret key), they include an unique session identifier (like
  // "jti" claim), issue and expiration dates (like "iat" and "exp" claims),
  // and they are encrypted with a temporary key - this secret keys is tied to
  // the TMVCSessionWithCookies instance lifetime, so new cookies are generated
  // after server restart, unless they are persisted via LoadContext/SaveContext
  // - signature and encryption are weak, but very fast, to avoid DDOS attacks
  TMVCSessionWithCookies = class(TMVCSessionAbstract)
  protected
    fContext: TMVCSessionWithCookiesContext;
    function GetCookie: RawUTF8; virtual; abstract;
    procedure SetCookie(const cookie: RawUTF8); virtual; abstract;
    procedure Crypt(P: PAnsiChar; bytes: integer);
  public
    /// create an instance of this ViewModel implementation class
    constructor Create; override;
    /// will initialize the session cookie
    // - setting an optional record data, which will be stored Base64-encoded
    // - will return the 32-bit internal session ID
    // - you can supply a time period, after which the session will expire -
    // default is 1 hour
    function Initialize(PRecordData: pointer=nil; PRecordTypeInfo: pointer=nil;
      SessionTimeOutMinutes: cardinal=60): integer; override;
    /// fast check if there is a cookie session associated to the current context
    function Exists: boolean; override;
    /// retrieve the session ID from the current cookie
    // - can optionally retrieve the record Data parameter stored in the cookie
    // - will return the 32-bit internal session ID, or 0 if the cookie is invalid
    function CheckAndRetrieve(PRecordData: pointer=nil; PRecordTypeInfo: pointer=nil): integer; override;
    /// clear the session
    // - by deleting the cookie on the client side
    procedure Finalize; override;
    /// return all cookie generation information as base64 encoded text 
    // - to be retrieved via LoadContext
    function SaveContext: RawUTF8; override;
    /// restore cookie generation information from SaveContext text format
    // - returns TRUE after checking the crc and unserializing the supplied data
    // - WARNING: if the unerlying record type structure changed (i.e. any
    // field is modified or added), restoration will lead to data corruption of
    // low-level binary content, then trigger unexpected GPF: if you change the
    // record type definition, do NOT use LoadContext - and reset all cookies
    function LoadContext(const Saved: RawUTF8): boolean; override;
    /// direct access to the low-level information used for cookies generation
    // - use SaveContext and LoadContext methods to persist this information
    // before server shutdown, so that the cookies can be re-used after restart
    property Context: TMVCSessionWithCookiesContext read fContext write fContext;
    /// you can customize the cookie name
    // - default is 'mORMot', and cookie is restricted to Path=/RestRoot
    property CookieName: RawUTF8 read fContext.CookieName write fContext.CookieName;
  end;

  /// implement a ViewModel/Controller sessions in a TSQLRestServer instance
  // - will use ServiceContext.Request threadvar to access the client cookies
  TMVCSessionWithRestServer = class(TMVCSessionWithCookies)
  protected
    function GetCookie: RawUTF8; override;
    procedure SetCookie(const cookie: RawUTF8); override;
  end;

  /// implement a single ViewModel/Controller in-memory session
  // - this kind of session could be used in-process, e.g. for a VCL/FMX GUI
  // - do NOT use it with multiple clients, e.g. from HTTP remote access
  TMVCSessionSingle = class(TMVCSessionWithCookies)
  protected
    fSingleCookie: RawUTF8;
    function GetCookie: RawUTF8; override;
    procedure SetCookie(const cookie: RawUTF8); override;
  end;


  { ====== Application Run ====== }

  /// record type to define commands e.g. to redirect to another URI
  // - do NOT access those record property directly, but rather use
  // TMVCApplication.GotoView/GotoError/GotoDefault methods, e.g.
  // !  function TBlogApplication.Logout: TMVCAction;
  // !  begin
  // !    CurrentSession.Finalize;
  // !    GotoDefault(result);
  // !  end;
  // - this record type should match exactly TServiceCustomAnswer layout,
  // so that TServiceMethod.InternalExecute() would handle it directly
  TMVCAction = record
    /// the method name to be executed
    RedirectToMethodName: RawUTF8;
    /// may contain a JSON object which will be used to specify parameters
    // to the specified method
    RedirectToMethodParameters: RawUTF8;
    /// which HTTP Status code should be returned
    // - if RedirectMethodName is set, will return 307 HTTP_TEMPORARYREDIRECT
    // by default, but you can set here the expected HTTP Status code, e.g.
    // 201 HTTP_CREATED or 404 HTTP_NOTFOUND
    ReturnedStatus: cardinal;
  end;

  TMVCApplication = class;

  /// abtract MVC rendering execution context
  // - you shoud not execute this abstract class, but any of the inherited class
  // - one instance inherited from this class would be allocated for each event
  // - may return some data (when inheriting from TMVCRendererReturningData), or
  // even simply display the value in a VCL/FMX GUI, without any output
  TMVCRendererAbstract = class
  protected
    fApplication: TMVCApplication;
    fMethodIndex: integer;
    fMethodReturnsAction: boolean;
    fInput: RawUTF8;
    procedure Renders(outContext: variant; status: cardinal;
      forcesError: boolean); virtual; abstract;
    function Redirects(const action: TMVCAction): boolean; virtual;
    procedure CommandError(const ErrorName: RawUTF8; const ErrorValue: variant;
      ErrorCode: Integer); virtual;
  public
    /// initialize a rendering process for a given MVC Application/ViewModel
    constructor Create(aApplication: TMVCApplication); reintroduce;
    /// main execution method of the rendering process
    // - Input should have been set with the incoming execution context
    procedure ExecuteCommand(aMethodIndex: integer); virtual;
    /// incoming execution context, to be processed via ExecuteCommand() method
    // - should be specified as a raw JSON object
    property Input: RawUTF8 read fInput write fInput;
  end;

  /// how TMVCRendererReturningData should cache its content
  TMVCRendererCachePolicy = (
    cacheNone,
    cacheRootIgnoringSession, cacheRootIfSession, cacheRootIfNoSession,
    cacheRootWithSession,
    cacheWithParametersIgnoringSession, cacheWithParametersIfSession,
    cacheWithParametersIfNoSession);

  TMVCRunWithViews = class;

  /// abstract MVC rendering execution context, returning some content
  // - the Output property would contain the content to be returned
  // - can be used to return e.g. some rendered HTML or some raw JSON,
  // or even some server-side generated report as PDF, using our mORMotReport.pas
  TMVCRendererReturningData = class(TMVCRendererAbstract)
  protected
    fRun: TMVCRunWithViews;
    fOutput: TServiceCustomAnswer;
    fCacheEnabled: boolean;
    fCacheCurrent: (noCache, rootCache, inputCache);
    fCacheCurrentSec: cardinal;
    fCacheCurrentInputValueKey: RawUTF8;
    function Redirects(const action: TMVCAction): boolean; override;
  public
    /// initialize a rendering process for a given MVC Application/ViewModel
    // - you need to specify a MVC Views engine, e.g. TMVCViewsMustache instance
    constructor Create(aRun: TMVCRunWithViews); reintroduce; virtual;
    /// main execution method of the rendering process
    // - this overriden method would handle proper caching as defined by
    // TMVCRunWithViews.SetCache()
    procedure ExecuteCommand(aMethodIndex: integer); override;
    /// caller should retrieve this value after ExecuteCommand method execution
    property Output: TServiceCustomAnswer read fOutput;
  end;

  TMVCRendererReturningDataClass = class of TMVCRendererReturningData;

  /// MVC rendering execution context, returning some rendered View content
  // - will use an associated Views templates system, e.g. a Mustache renderer
  TMVCRendererFromViews = class(TMVCRendererReturningData)
  protected
    // Renders() will fill Output using the corresponding View, to be sent back
    procedure Renders(outContext: variant; status: cardinal; forcesError: boolean); override;
  public
    /// initialize a rendering process for a given MVC Application/ViewModel
    // - this overriden constructor will ensure that cache is enabled
    constructor Create(aRun: TMVCRunWithViews); override;
  end;

  /// MVC rendering execution context, returning some un-rendered JSON content
  // - may be used e.g. for debugging purpose
  // - for instance, TMVCRunOnRestServer will return such context with the
  // supplied URI ends with '/json' (e.g. for any /root/method/json request)
  TMVCRendererJson = class(TMVCRendererReturningData)
  protected
    // Renders() will fill Output with the outgoing JSON, to be sent back
    procedure Renders(outContext: variant; status: cardinal; forcesError: boolean); override;
  end;

  /// abstract class used by TMVCApplication to run
  // - a single TMVCApplication logic may handle several TMVCRun instances
  TMVCRun = class
  protected
    fApplication: TMVCApplication;
  public
    /// link this runner class to a specified MVC application
    // - will also reset the associated Application.Session instance
    constructor Create(aApplication: TMVCApplication); reintroduce;
    /// method called to flush the caching mechanism for all MVC commands
    procedure NotifyContentChanged; virtual;
    /// you may call this method to flush any caching mechanism for a MVC command
    procedure NotifyContentChangedForMethod(aMethodIndex: integer); overload; virtual;
    /// you may call this method to flush any caching mechanism for a MVC command
    procedure NotifyContentChangedForMethod(const aMethodName: RawUTF8); overload;
    /// read-write access to the associated MVC Application/ViewModel instance
    property Application: TMVCApplication read fApplication write fApplication;
  end;

  /// abstract class used by TMVCApplication to run TMVCViews-based process
  // - this inherited class will host a MVC Views instance, and handle
  // an optional simple in-memory cache
  TMVCRunWithViews = class(TMVCRun)
  protected
    fViews: TMVCViewsAbtract;
    fCacheLocker: IAutoLocker;
    fCache: array of record
      Policy: TMVCRendererCachePolicy;
      TimeOutSeconds: cardinal;
      RootValue: RawUTF8;
      RootValueExpirationTime: cardinal;
      InputValues: TSynNameValue;
    end;
  public
    /// link this runner class to a specified MVC application
    constructor Create(aApplication: TMVCApplication;
      aViews: TMVCViewsAbtract=nil); reintroduce;
    /// method called to flush the caching mechanism for a MVC command
    procedure NotifyContentChangedForMethod(aMethodIndex: integer); override;
    /// defines the caching policy for a given MVC command
    // - a time expiration period (up to 5 minutes) can also be defined per
    // MVC command - leaving default 0 will set to 5 minutes expiration delay
    // - function calls can be chained to create some fluent definition interface
    // like in TAnyBLogapplication.Create:
    // ! fMainRunner := TMVCRunWithViews.Create(self).SetCache('default',cacheRoot);
    function SetCache(const aMethodName: RawUTF8; aPolicy: TMVCRendererCachePolicy;
      aTimeOutSeconds: cardinal=0): TMVCRunWithViews; virtual;
    /// finalize this instance
    destructor Destroy; override;
    /// read-write access to the associated MVC Views instance
    property Views: TMVCViewsAbtract read fViews;
  end;

  /// the kinds of optional content which may be published
  // - publishMvcInfo will define a /root/[aSubURI/]mvc-info HTML page,
  // which is pretty convenient when working with views
  // - publishStatic will define a /root/[aSubURI/].static sub-folder,
  // ready to serve any file available in the Views\.static local folder
  // - registerORMTableAsExpressions will register Mustache Expression Helpers
  // for every TSQLRecord table of the Server data model
  // - by default, TSQLRestServer authentication would be by-passed for all
  // MVC routes, unless bypassAuthentication option is undefined
  TMVCPublishOption = (publishMvcInfo, publishStatic,
    registerORMTableAsExpressions, bypassAuthentication);

  /// which kind of optional content should be publish
  TMVCPublishOptions = set of TMVCPublishOption;

  /// run TMVCApplication directly within a TSQLRestServer method-based service
  // - this is the easiest way to host and publish a MVC Application, optionally
  // in conjunction with REST/AJAX client access
  TMVCRunOnRestServer = class(TMVCRunWithViews)
  protected
    fRestServer: TSQLRestServer;
    fPublishOptions: TMVCPublishOptions;
    fMvcInfoCache: RawUTF8;
    fStaticCache: TSynNameValue;
    /// callback used for the rendering on the TSQLRestServer
    procedure RunOnRestServerRoot(Ctxt: TSQLRestServerURIContext);
    procedure RunOnRestServerSub(Ctxt: TSQLRestServerURIContext);
    procedure InternalRunOnRestServer(Ctxt: TSQLRestServerURIContext;
      const MethodName: RawUTF8);
  public
    /// this constructor will publish some views to a TSQLRestServer instance
    // - the associated RestModel can match the supplied TSQLRestServer, or be
    // another instance (if the data model is not part of the publishing server)
    // - all TMVCApplication methods would be registered to the TSQLRestServer,
    // as /root/methodName if aSubURI is '', or as /root/aSubURI/methodName
    // - if aApplication has no Views instance associated, this constructor will
    // initialize a Mustache renderer in its default folder, with '.html' void
    // template generation
    // - will also create a TMVCSessionWithRestServer for simple cookie sessions
    // - aPublishOptions could be used to specify integration with the server
    constructor Create(aApplication: TMVCApplication;
      aRestServer: TSQLRestServer=nil; const aSubURI: RawUTF8='';
      aViews: TMVCViewsAbtract=nil; aPublishOptions: TMVCPublishOptions=
        [low(TMVCPublishOption)..high(TMVCPublishOption)]); reintroduce;
  end;


  { ====== Application / ViewModel ====== }

  /// Exception class triggerred by mORMot MVC/MVVM applications internally
  // - those error are internal fatal errors of the server side process
  EMVCException = class(ESynException);

  /// Exception class triggerred by mORMot MVC/MVVM applications externally
  // - those error are external errors which should be notified to the client
  // - can be used to change the default view, e.g. on application error
  EMVCApplication = class(ESynException)
  protected
    fAction: TMVCAction;
  public
    /// same as calling TMVCApplication.GotoView()
    // - HTTP_TEMPORARYREDIRECT will change the URI, but HTTP_SUCCESS won't
    constructor CreateGotoView(const aMethod: RawUTF8;
      const aParametersNameValuePairs: array of const;
      aStatus: cardinal=HTTP_TEMPORARYREDIRECT);
    /// same as calling TMVCApplication.GotoError()
    constructor CreateGotoError(const aErrorMessage: string;
      aErrorCode: integer=HTTP_BADREQUEST); overload;
    /// same as calling TMVCApplication.GotoError()
    constructor CreateGotoError(aHtmlErrorCode: integer); overload;
    /// same as calling TMVCApplication.GotoDefault
    // - HTTP_TEMPORARYREDIRECT will change the URI, but HTTP_SUCCESS won't
    constructor CreateDefault(aStatus: cardinal=HTTP_TEMPORARYREDIRECT);
  end;

  /// defines the main and error pages for the ViewModel of one application
  IMVCApplication = interface(IInvokable)
    ['{C48718BF-861B-448A-B593-8012DB51E15D}']
    /// the default main page
    // - whole data context is retrieved and returned as a TDocVariant
    procedure Default(var Scope: variant);
    /// the error page
    // - in addition to the error message, a whole data context is retrieved
    // and returned as a TDocVariant
    procedure Error(var Msg: RawUTF8; var Scope: variant);
  end;

  /// parent class to implement a MVC/MVVM application
  // - you should inherit from this class, then implement an interface inheriting
  // from IMVCApplication to define the various commands of the application
  // - here the Model would be a TSQLRest instance, Views will be defined by
  // TMVCViewsAbtract (e.g. TMVCViewsMustache), and the ViewModel/Controller
  // will be implemented with IMVCApplication methods of the inherited class
  // - inherits from TInjectableObject, so that you could resolve dependencies
  // via services or stubs, following the IoC pattern
  TMVCApplication = class(TInjectableObject)
  protected
    fFactory: TInterfaceFactory;
    fFactoryEntry: pointer;
    fFactoryErrorIndex: integer;
    fSession: TMVCSessionAbstract;
    fRestModel: TSQLRest;
    fRestServer: TSQLRestServer;
    fLocker: IAutoLocker;
    // if any TMVCRun instance is store here, will be freed by Destroy
    // but note that a single TMVCApplication logic may handle several TMVCRun
    fMainRunner: TMVCRun;
    procedure SetSession(const Value: TMVCSessionAbstract);
    /// to be called when the data model did change to force content re-creation
    // - this default implementation will call fMainRunner.NotifyContentChanged
    procedure FlushAnyCache; virtual;
    /// generic IMVCApplication implementation
    procedure Error(var Msg: RawUTF8; var Scope: variant);
    /// every view will have this data context transmitted as "main":...
    function GetViewInfo(MethodIndex: integer): variant; virtual;
    /// compute the data context e.g. for the /mvc-info URI
    function GetMvcInfo: variant; virtual;
    /// wrappers to redirect to IMVCApplication standard methods
    // - if status is HTTP_TEMPORARYREDIRECT, it will change the URI
    // whereas HTTP_SUCCESS would just render the view for the current URI
    class procedure GotoView(var Action: TMVCAction; const MethodName: RawUTF8;
      const ParametersNameValuePairs: array of const;
      Status: cardinal=HTTP_TEMPORARYREDIRECT);
    class procedure GotoError(var Action: TMVCAction; const Msg: string;
      ErrorCode: integer=HTTP_BADREQUEST); overload;
    class procedure GotoError(var Action: TMVCAction; ErrorCode: integer); overload;
    class procedure GotoDefault(var Action: TMVCAction;
      Status: cardinal=HTTP_TEMPORARYREDIRECT);
  public
    /// initialize the instance of the MVC/MVVM application
    // - define the associated REST instance, and the interface definition for
    // application commands
    // - is not defined as constructor, since this TInjectableObject may
    // expect injection using the CreateInjected() constructor
    procedure Start(aRestModel: TSQLRest; aInterface: PTypeInfo); virtual;
    /// finalize the application
    // - and release any associated CurrentSession, Views, and fMainRunner
    destructor Destroy; override;

    /// read-only access to the associated mORMot REST instance implementing the
    // MVC data Model of the application
    // - is a TSQLRestServer instance e.g. for TMVCRunOnRestServer
    property RestModel: TSQLRest read fRestModel;
    /// read-only access to the associated factory for IMVCApplication interface
    property Factory: TInterfaceFactory read fFactory;
    /// read-write access to the associated Session instance
    property CurrentSession: TMVCSessionAbstract read fSession write SetSession;
    /// global mutex which may be used to protect ViewModel/Controller code
    // - you may call Locker.ProtectMethod in any implementation method to
    // ensure that no other thread would access the same data
    // - for store some cache data among methods, you may consider defining a
    // ILockedDocVariant private field, and use it to store values safely
    // - note that regular RestModel CRUD operations are already thread safe, so
    // it is not necessary to use this Locker with ORM or SOA methods
    property Locker: IAutoLocker read fLocker;
    /// read-write access to the main associated TMVCRun instance
    // - if any TMVCRun instance is stored here, will be freed by Destroy
    // - but note that a single TMVCApplication logic may handle several TMVCRun
    property MainRunner: TMVCRun read fMainRunner;
  end;

const
  /// the pseudo-method name for the MVC information html page
  MVCINFO_URI = 'mvc-info';

  /// the pseudo-method name for any static content for Views
  STATIC_URI = '.static';


implementation


{ TMVCViewsAbtract }

constructor TMVCViewsAbtract.Create(aInterface: PTypeInfo; aLogClass: TSynLogClass);
begin
  inherited Create;
  fFactory := TInterfaceFactory.Get(aInterface);
  fFactoryErrorIndex := fFactory.FindMethodIndex('Error');
  if aLogClass=nil then
    fLogClass := TSQLLog else
    fLogClass := aLogClass;
end;

function TMVCViewsAbtract.GetViewStaticFolder: TFileName;
begin
  result := ViewTemplateFolder+STATIC_URI;
end;


{ TMVCViewsMustache }

const
  MUSTACHE_METHODPARTIAL =
  '{{<method}}{{verb}} {{methodName}}{{#hasInParams}}({{#args}}{{^dirResult}}'+
  '{{dirName}} {{argName}}: {{typeDelphi}}{{commaArg}}{{/dirResult}}{{/args}})'+
  '{{/hasInParams}}{{#args}}{{#dirResult}}: {{typeDelphi}}{{/dirResult}}'+
  '{{/args}};{{/method}}';

  MUSTACHE_VOIDVIEW = MUSTACHE_METHODPARTIAL+
  '<<! void template created for the {{interfaceName}}.{{methodName}} View:'#13#10+
  ' defined as'#13#10'   {{>method}}'#13#10' with the following data context:'#13#10+
  '   * Main: variant'#13#10'{{#args}}{{#dirOutput}}   * {{argName}}:'+
  ' {{typePascal}}'#13#10'{{/dirOutput}}{{/args}}>>'#13#10;

  MUSTACHE_MVCINFO = MUSTACHE_METHODPARTIAL+
  '{{<url}}/{{root}}/{{methodName}}{{#hasInParams}}?'+
  '{{#args}}{{#dirInput}}{{argName}}=</b>..[{{typePascal}}]..<b>'+
  '{{#commaInSingle}}&{{/commaInSingle}}{{/dirInput}}{{/args}}{{/hasInParams}}{{/url}}'+
  '{{<mustache}}<b>&#123;{Main&#125;}</b>: variant{{#args}}{{#dirOutput}}<br><b>{&#123;'+
  '{{argName}}&#125;}</b>: {{typePascal}}{{/dirOutput}}{{/args}}{{/mustache}}'+
  '<html><head><title>{{Name}} Information</title></head><body '+
  'style="font-family:Verdana;"><h1>{{Name}} mORMotMVC Information</h1>'+
  '<p><strong>Generated by a <i>mORMot</i> {{mORMot}} server</strong><br>'+
  '<small>&copy;Synopse Informatique - <a href=https://synopse.info>'+
  'https://synopse.info</a></small></p><h2>Controller Definition</h2>'+
  '<p>Registered interface is:</p><pre>'#13#10+
  '  I{{name}} = interface(IInvokable)'#13#10'{{#methods}}'#13#10+
  '    {{>method}}'#13#10'{{/methods}}'#13#10'  end;'#13#10+
  '</pre><p>Use this page as reference when writing your <a href=http://blog.synopse.info'+
  '/post/2014/04/28/Mustache-Logic-less-templates-for-Delphi-part-1>Mustache</a> Views.</p>'+
  '<h2>Available Commands</h2><p>You can access the following commands:</p>'+
  '<ul>{{#methods}}<li><b>{{>url}}</b>{{/methods}}</ul><p>Any missing parameter '+
  'would be replaced by its default value.</p><h2>Available Views</h2>'+
  '<p>The following views are defined, with expected data context:</p><ul>'+
  '{{#methods}}{{^resultIsServiceCustomAnswer}}<li><b>{{>url}}</b><p>{{>mustache}}'+
  '</p></li>{{/resultIsServiceCustomAnswer}}{{/methods}}</ul><p>'+
  'Currently, all views are located in the <code>{{viewsFolder}}</code> folder.</p>';

  MUSTACHE_DEFAULTERROR =
  '<html><head><title>mORMotMVC Error</title></head><body style='+
  '"font-family:Verdana;"><h1>mORMotMVC Default Error Page</h1><p>A <code>'+
  '{{exceptionName}}</code> exception did raise during {{className}} process '+
  'with the following message:</p><pre>{{exceptionMessage}}</pre><p>'+
  'Triggered with the following context:</p><pre>{{originalErrorContext}}</pre>';


function MethodHasView(const aMethod: TServiceMethod): boolean;
begin // any method returning a TMVCAction do not have any associated view
  result := (aMethod.ArgsResultIndex<0) or
            (aMethod.Args[aMethod.ArgsResultIndex].ValueType<>smvRecord) or
            (aMethod.Args[aMethod.ArgsResultIndex].ArgTypeInfo<>TypeInfo(TMVCAction));
end;

constructor TMVCViewsMustache.Create(aInterface: PTypeInfo;
  const aParameters: TMVCViewsMustacheParameters; aLogClass: TSynLogClass);
var m: integer;
    LowerExt: TFileName;
    partialName: RawUTF8;
    info: variant;
    SR: TSearchRec;
begin
  inherited Create(aInterface,aLogClass);
  // get views
  fViewTemplateFileTimestampMonitor := aParameters.FileTimestampMonitorAfterSeconds;
  if aParameters.Folder='' then
    fViewTemplateFolder := ExeVersion.ProgramFilePath+'Views'+PathDelim else
    fViewTemplateFolder := IncludeTrailingPathDelimiter(aParameters.Folder);
  if not DirectoryExists(fViewTemplateFolder) then
    CreateDir(fViewTemplateFolder);
  if aParameters.CSVExtensions='' then
    LowerExt := ',html,json,css,' else
    LowerExt := ','+SysUtils.LowerCase(aParameters.CSVExtensions)+',';
  SetLength(fViews,fFactory.MethodsCount);
  for m := 0 to fFactory.MethodsCount-1 do
  if MethodHasView(fFactory.Methods[m]) then
  with fViews[m] do begin
    Locker := TAutoLocker.Create;
    MethodName := UTF8ToString(fFactory.Methods[m].URI);
    SearchPattern := fViewTemplateFolder+MethodName+'.*';
    if FindFirst(SearchPattern,faAnyFile-faDirectory,SR)=0 then
      try
        repeat
          FileExt := SysUtils.LowerCase(copy(ExtractFileExt(SR.Name),2,100));
          if Pos(','+FileExt+',',LowerExt)>0 then
            break; // found a template with the right extension
        until FindNext(SR)<>0;
        ShortFileName := SR.Name;
        FileName := fViewTemplateFolder+ShortFileName;
        ContentType := GetMimeContentType(nil,0,ShortFileName);
      finally
        FindClose(SR);
      end else begin
        fLogClass.Add.Log(
          sllWarning,'%.Create: Missing View file in %',[self,SearchPattern]);
        if aParameters.ExtensionForNotExistingTemplate<>'' then begin
          ShortFileName := MethodName+aParameters.ExtensionForNotExistingTemplate;
          FileName := fViewTemplateFolder+ShortFileName;
          info := ContextFromMethod(fFactory.Methods[m]);
          info.interfaceName := fFactory.InterfaceTypeInfo^.Name;
          FileFromString(StringReplaceChars(StringReplaceChars(
            TSynMustache.Parse(MUSTACHE_VOIDVIEW).Render(info),'<','{'),'>','}'),
            FileName);
        end;
      end;
  end;
  fViewHelpers := aParameters.Helpers;
  // get partials
  fViewPartials := TSynMustachePartials.Create;
  if FindFirst(fViewTemplateFolder+'*.partial',faAnyFile,SR)=0 then
  try
    repeat
      StringToUTF8(GetFileNameWithoutExt(SR.Name),partialName);
      try
        fViewPartials.Add(partialName,AnyTextFileToRawUTF8(fViewTemplateFolder+SR.Name,true));
      except
        on E: Exception do
          fLogClass.Add.Log(
            sllError,'%.Create: Invalid Partial file % - %',[self,SR.Name,E]);
      end;
    until FindNext(SR)<>0;
  finally
    FindClose(SR);
  end;
end;

constructor TMVCViewsMustache.Create(aInterface: PTypeInfo;
  aLogClass: TSynLogClass; aExtensionForNotExistingTemplate: TFileName);
var params: TMVCViewsMustacheParameters;
begin
  fillchar(params,sizeof(params),0);
  params.FileTimestampMonitorAfterSeconds := 5;
  params.ExtensionForNotExistingTemplate := aExtensionForNotExistingTemplate;
  params.Helpers := TSynMustache.HelpersGetStandardList;
  Create(aInterface,params,aLogClass);
end;

destructor TMVCViewsMustache.Destroy;
begin
  inherited;
  fViewPartials.Free;
end;

type
  THtmlTableStyleLabel = (labelFalse,labelTrue,labelOff,labelOn,labelValue);
  TExpressionHtmlTableStyle = class
  public
    class procedure StartTable(WR: TTextWriter); virtual;
    class procedure BeforeFieldName(WR: TTextWriter); virtual;
    class procedure BeforeValue(WR: TTextWriter); virtual;
    class procedure AddLabel(WR: TTextWriter; const text: string;
      kind: THtmlTableStyleLabel); virtual;
    class procedure AfterValue(WR: TTextWriter); virtual;
    class procedure EndTable(WR: TTextWriter); virtual;
  end;
  TExpressionHtmlTableStyleBootstrap = class(TExpressionHtmlTableStyle)
  public
    class procedure StartTable(WR: TTextWriter); override;
    class procedure AddLabel(WR: TTextWriter; const text: string;
      kind: THtmlTableStyleLabel); override;
  end;
  TExpressionHtmlTableStyleClass = class of TExpressionHtmlTableStyle;

  TExpressionHelperForTable = class
  public
    Rest: TSQLRest;
    Table: TSQLRecordClass;
    TableProps: TSQLRecordProperties;
    HtmlTableStyle: TExpressionHtmlTableStyleClass;
    constructor Create(aRest: TSQLRest; aTable: TSQLRecordClass;
      var aHelpers: TSynMustacheHelpers);
    procedure ExpressionGet(const Value: variant; out result: variant);
    procedure ExpressionHtmlTable(const Value: variant; out result: variant);
  end;

constructor TExpressionHelperForTable.Create(aRest: TSQLRest;
  aTable: TSQLRecordClass; var aHelpers: TSynMustacheHelpers);
var HelperName: RawUTF8;
begin
  aRest.PrivateGarbageCollector.Add(self);
  Rest := aRest;
  HelperName := RawUTF8(aTable.ClassName);
  Table := aTable;
  TableProps := aTable.RecordProps;
  TSynMustache.HelperAdd(aHelpers,HelperName,ExpressionGet);
  HtmlTableStyle := TExpressionHtmlTableStyleBootstrap;
  TSynMustache.HelperAdd(aHelpers,HelperName+'.HtmlTable',ExpressionHtmlTable);
end;

procedure TExpressionHelperForTable.ExpressionHtmlTable(const Value: variant;
  out result: variant);
var Rec: PDocVariantData;
    f,i,j,int: integer;
    Field: TSQLPropInfo;
    timelog: TTimeLogBits;
    caption: string;
    sets: TStringList;
    utf8: RawUTF8;
    W: TTextWriter;
const ONOFF: array[boolean] of THtmlTableStyleLabel = (labelOff,labelOn);
      ENUM: array[boolean,boolean] of THtmlTableStyleLabel =
        ((labelValue,labelValue),(labelFalse,labelTrue));
begin
  Rec := _Safe(Value);
  if Rec^.Kind=dvObject then begin
    W := TTextWriter.CreateOwnedStream;
    try
      HtmlTableStyle.StartTable(W);
      for f := 0 to TableProps.Fields.Count-1 do begin
        Field := TableProps.Fields.List[f];
        i := Rec^.GetValueIndex(Field.Name);
        if i<0 then
          continue;
        if not (Field.SQLFieldType in [sftAnsiText,sftUTF8Text,sftInteger,
           sftFloat,sftCurrency,sftTimeLog,sftModTime,sftCreateTime,sftDateTime,
           sftDateTimeMS,sftUnixTime,sftUnixMSTime,sftBoolean,sftEnumerate,sftSet]) then
          continue;
        HtmlTableStyle.BeforeFieldName(W);
        GetCaptionFromPCharLen(TrimLeftLowerCase(Field.Name),caption);
        W.AddHtmlEscapeString(caption);
        HtmlTableStyle.BeforeValue(W);
        VariantToUTF8(Rec^.Values[i],utf8);
        case Field.SQLFieldType of
        sftAnsiText,sftUTF8Text,sftInteger,sftFloat,sftCurrency:
          W.AddHtmlEscape(pointer(utf8));
        sftTimeLog,sftModTime,sftCreateTime:
          if VariantToInt64(Rec^.Values[i],timelog.Value) then
            W.AddHtmlEscapeString(timeLog.i18nText);
        sftDateTime, sftDateTimeMS: begin
          timelog.From(utf8);
          W.AddHtmlEscapeString(timeLog.i18nText);
        end;
        sftUnixTime, sftUnixMSTime:
          if VariantToInt64(Rec^.Values[i],timelog.Value) then begin
            if Field.SQLFieldType=sftUnixTime then
              timelog.FromUnixTime(timelog.Value) else
              timelog.FromUnixMSTime(timelog.Value);
            W.AddHtmlEscapeString(timeLog.i18nText);
          end;
        sftBoolean,sftEnumerate:
          if Field.InheritsFrom(TSQLPropInfoRTTIEnum) then begin
            caption := TSQLPropInfoRTTIEnum(Field).GetCaption(utf8,int);
            HtmlTableStyle.AddLabel(W,caption,
              ENUM[Field.SQLFieldType=sftBoolean,int<>0]);
          end;
        sftSet:
          if Field.InheritsFrom(TSQLPropInfoRTTISet) and
             VariantToInteger(Rec^.Values[i],int) then begin
            sets := TStringList.Create;
            try
              TSQLPropInfoRTTISet(Field).SetEnumType^.AddCaptionStrings(sets);
              for j := 0 to sets.Count-1 do begin
                HtmlTableStyle.AddLabel(W,sets[j],ONOFF[GetBit(int,j)]);
                W.AddShort('<br/>');
              end;
            finally
              sets.Free;
            end;
          end;
        end;
        HtmlTableStyle.AfterValue(W);
      end;
      HtmlTableStyle.EndTable(W);
      RawUTF8ToVariant(W.Text,result);
    finally
      W.Free;
    end;
  end else
   result := Value; // not an object -> return input value as is
end;

procedure TExpressionHelperForTable.ExpressionGet(const Value: variant;
  out result: variant);
var Rec: TSQLRecord;
    ID: integer;
begin
  SetVariantNull(result);
  if not VariantToInteger(Value,ID) then
    exit;
  Rec := Table.Create;
  try
    if Rest.Retrieve(ID,Rec) then
      result := Rec.GetSimpleFieldsAsDocVariant(true);
  finally
    Rec.Free;
  end;
end;

class procedure TExpressionHtmlTableStyle.AddLabel(WR: TTextWriter;
  const text: string; kind: THtmlTableStyleLabel);
const SETLABEL: array[THtmlTableStyleLabel] of string[3] = ('','','- ','+ ','');
begin
  WR.AddShort(SETLABEL[kind]);
  WR.AddHtmlEscapeString(text);
  WR.AddShort('&nbsp;');
end;

class procedure TExpressionHtmlTableStyle.AfterValue(WR: TTextWriter);
begin
  WR.AddShort('</td></tr>');
end;

class procedure TExpressionHtmlTableStyle.BeforeFieldName(WR: TTextWriter);
begin
  WR.AddShort('<tr><td>');
end;

class procedure TExpressionHtmlTableStyle.BeforeValue(WR: TTextWriter);
begin
  WR.AddShort('</td><td>');
end;

class procedure TExpressionHtmlTableStyle.EndTable(WR: TTextWriter);
begin
  WR.AddShort('</table>');
end;

class procedure TExpressionHtmlTableStyle.StartTable(WR: TTextWriter);
begin
  WR.AddShort('<table>');
end;

class procedure TExpressionHtmlTableStyleBootstrap.AddLabel(
  WR: TTextWriter; const text: string; kind: THtmlTableStyleLabel);
const SETLABEL: array[THtmlTableStyleLabel] of string[7] = (
  'danger','success','danger','success','primary');
begin
  WR.AddShort('<span class="label label-');
  WR.AddShort(SETLABEL[kind]);
  WR.Add('"','>');
  WR.AddHtmlEscapeString(text);
  WR.AddShort('</span>');
end;

class procedure TExpressionHtmlTableStyleBootstrap.StartTable(WR: TTextWriter);
begin
  WR.AddShort('<table class="table table-striped table-bordered">');
end;

function TMVCViewsMustache.RegisterExpressionHelpers(
  const aNames: array of RawUTF8;
  const aEvents: array of TSynMustacheHelperEvent): TMVCViewsMustache;
begin
  if self<>nil then
    TSynMustache.HelperAdd(fViewHelpers,aNames,aEvents);
  result := self;
end;

function TMVCViewsMustache.RegisterExpressionHelpersForTables(
  aRest: TSQLRest; const aTables: array of TSQLRecordClass): TMVCViewsMustache;
var t: integer;
begin
  if (self<>nil) and (aRest<>nil) then
    for t := 0 to high(aTables) do
      if aRest.Model.GetTableIndex(aTables[t])>=0 then
        TExpressionHelperForTable.Create(aRest,aTables[t],fViewHelpers);
  result := self;
end;

function TMVCViewsMustache.RegisterExpressionHelpersForTables(
  aRest: TSQLRest): TMVCViewsMustache;
var t: integer;
begin
  if (self<>nil) and (aRest<>nil) then
    for t := 0 to aRest.Model.TablesMax do
     TExpressionHelperForTable.Create(aRest,aRest.Model.Tables[t],fViewHelpers);
  result := self;
end;

function TMVCViewsMustache.RegisterExpressionHelpersForCrypto: TMVCViewsMustache;
begin
  result := RegisterExpressionHelpers(['md5','sha1','sha256'],[md5,sha1,sha256]);
end;

class procedure TMVCViewsMustache.md5(const Value: variant;
  out result: variant);
begin
  RawUTF8ToVariant(SynCrypto.MD5(ToUTF8(Value)),result);
end;

class procedure TMVCViewsMustache.sha1(const Value: variant;
  out result: variant);
begin
  RawUTF8ToVariant(SynCrypto.SHA1(ToUTF8(Value)),result);
end;

class procedure TMVCViewsMustache.sha256(const Value: variant;
  out result: variant);
begin
  RawUTF8ToVariant(SynCrypto.SHA256(ToUTF8(Value)),result);
end;

function TMVCViewsMustache.GetRenderer(methodIndex: integer;
  out aContentType: RawUTF8): TSynMustache;
var age: TDateTime;
begin
  if cardinal(methodIndex)>=fFactory.MethodsCount then
    raise EMVCException.CreateUTF8('%.Render(methodIndex=%)',[self,methodIndex]);
  with fViews[methodIndex], Locker.ProtectMethod do begin
    if MethodName='' then
      raise EMVCException.CreateUTF8('%.Render(''%''): not a View',[self,MethodName]);
    if (Mustache=nil) and (FileName='') then
      raise EMVCException.CreateUTF8('%.Render(''%''): Missing Template in ''%''',
        [self,MethodName,SearchPattern]);
    if (Mustache=nil) or ((fViewTemplateFileTimestampMonitor<>0) and
       (FileTimestampCheckTick<GetTickCount64)) then begin
      age := FileAgeToDateTime(FileName);
      if (Mustache=nil) or (age<>FileTimestamp) then begin
        Mustache := nil; // no Mustache.Free: TSynMustache instances are cached
        FileTimestamp := age;
        Template := AnyTextFileToRawUTF8(FileName,true);
        if Template<>'' then
        try
          Mustache := TSynMustache.Parse(Template);
        except
          on E: Exception do
            raise EMVCException.CreateUTF8('%.Render(''%''): Invalid Template: % - %',
              [self,FileName,E,E.Message]);
        end else
          raise EMVCException.CreateUTF8('%.Render(''%''): Missing Template in ''%''',
            [self,ShortFileName,SearchPattern]);
        if fViewTemplateFileTimestampMonitor<>0 then
          FileTimestampCheckTick := GetTickCount64+
            Int64(fViewTemplateFileTimestampMonitor)*Int64(1000);
      end;
    end;
    aContentType := ContentType;
    result := Mustache;
  end;
end;

procedure TMVCViewsMustache.Render(methodIndex: Integer; const Context: variant;
  var View: TMVCView);
begin
  View.Content := GetRenderer(methodIndex,View.ContentType).Render(
    Context,fViewPartials,fViewHelpers);
  if trim(View.Content)='' then
  with fViews[methodIndex] do begin
    Locker.Enter;
    Mustache := nil; // force reload ASAP
    Locker.Leave;
    raise EMVCException.CreateUTF8(
      '%.Render(''%''): Void "%" Template - please put some content!',
        [self,ShortFileName,FileName]);
  end;
end;


{ TMVCSessionAbstract }

constructor TMVCSessionAbstract.Create;
begin
  inherited;
end;

function TMVCSessionAbstract.CheckAndRetrieveInfo(
  PRecordDataTypeInfo: pointer): variant;
var rec: TByteDynArray; // to store locally any kind of record
    recJSON: RawUTF8;
    sessionID: integer;
begin
  SetLength(rec,RecordTypeInfoSize(PRecordDataTypeInfo));
  try
    SetVariantNull(result);
    sessionID := CheckAndRetrieve(pointer(rec),PRecordDataTypeInfo);
    if sessionID=0 then
      exit;
    if rec<>nil then begin
      SaveJSON(pointer(rec)^,PRecordDataTypeInfo,
        [twoEnumSetsAsBooleanInRecord,twoTrimLeftEnumSets],recJSON);
      _Json(recJSON,result,JSON_OPTIONS_FAST);
    end;
    _ObjAddProps(['id',sessionID],result);
  finally
    if rec<>nil then // manual finalization of managed fields
      RecordClear(pointer(rec)^,PRecordDataTypeInfo);
  end;
end;


{ TMVCSessionWithCookies }

constructor TMVCSessionWithCookies.Create;
var rnd: TByte64;
begin
  inherited Create;
  fContext.CookieName := 'mORMot';
  // temporary secret for encryption
  fContext.CryptNonce := Random32;
  TAESPRNG.Main.FillRandom(@fContext.Crypt,sizeof(fContext.Crypt));
  // temporary secret for HMAC-CRC32C
  TAESPRNG.Main.FillRandom(@rnd,sizeof(rnd));
  fContext.Secret.Init(@rnd,sizeof(rnd));
end;

procedure XorMemoryCTR(data: PCardinal; key256bytes: PCardinalArray; size,ctr: cardinal);
begin
  while size>=sizeof(Cardinal) do begin
    dec(size,sizeof(Cardinal));
    data^ := data^ xor key256bytes[ctr and $3f] xor ctr;
    inc(data);
    ctr := ((ctr xor (ctr shr 15))*2246822519); // prime-number ctr diffusion
    ctr := ((ctr xor (ctr shr 13))*3266489917);
    ctr := ctr xor (ctr shr 16);
  end;
  if size=0 then
    exit; // no padding
  repeat
    dec(size);
    PByteArray(data)[size] := PByteArray(data)[size] xor PByteArray(key256bytes)[ctr and $ff] xor ctr;
    inc(ctr);
  until size=0;
end;

procedure TMVCSessionWithCookies.Crypt(P: PAnsiChar; bytes: integer);
begin
  XorMemoryCTR(@P[4],@fContext.Crypt,bytes-4,xxHash32(fContext.CryptNonce,P,4));
end;

function TMVCSessionWithCookies.Exists: boolean;
begin
  result := GetCookie<>'';
end;

type
  TCookieContent = packed record
    head: packed record
      cryptnonce: cardinal;
      hmac: cardinal;    // = signature
      session: integer;  // = jti claim
      issued: cardinal;  // = iat claim
      expires: cardinal; // = exp claim
    end;
    data: array[0..2047] of byte; // binary serialization of record value
  end;
  PCookieContent = ^TCookieContent;

function TMVCSessionWithCookies.CheckAndRetrieve(PRecordData,PRecordTypeInfo: pointer): integer;
var cookie: RawUTF8;
    clen, len: integer;
    now: cardinal;
    tmp: TCookieContent;
begin
  result := 0;
  cookie := GetCookie;
  if cookie='' then
    exit;
  clen := length(cookie);
  len := Base64uriToBinLength(clen);
  if (len>=sizeof(tmp.head)) and (len<=sizeof(tmp)) and
     Base64uriDecode(pointer(cookie),@tmp,clen) then begin
    Crypt(@tmp,len);
    if (cardinal(tmp.head.session)<=cardinal(fContext.SessionCount)) then begin
      now := UnixTimeUTC;
      if (tmp.head.issued<=now) and (tmp.head.expires>=now) and
         (fContext.Secret.Compute(@tmp.head.session,len-8)=tmp.head.hmac) then
      if PRecordData=nil then
        result := tmp.head.session else
        if (PRecordTypeInfo<>nil) and (len>sizeof(tmp.head)) and
           (RecordLoad(PRecordData^,@tmp.data,PRecordTypeInfo)<>nil) then
          result := tmp.head.session;
    end;
  end;
  if result=0 then
    Finalize; // delete any invalid/expired cookie on server side
end;

function TMVCSessionWithCookies.Initialize(
  PRecordData,PRecordTypeInfo: pointer; SessionTimeOutMinutes: cardinal): integer;
var len: integer;
    tmp: TCookieContent;
begin
  if (PRecordData<>nil) and (PRecordTypeInfo<>nil) then
    len := RecordSaveLength(PRecordData^,PRecordTypeInfo) else
    len := 0;
  if len>sizeof(tmp.data) then // all cookies storage should be < 4K
    raise EMVCApplication.CreateGotoError('Big Fat Cookie');
  result := InterlockedIncrement(fContext.SessionCount);
  tmp.head.cryptnonce := Random32;
  tmp.head.session := result;
  tmp.head.issued := UnixTimeUTC;
  if SessionTimeOutMinutes=0 then
    tmp.head.expires := $FFFFFFFF else
    tmp.head.expires := tmp.head.issued+SessionTimeOutMinutes*60;
  if len>0 then
    RecordSave(PRecordData^,@tmp.data,PRecordTypeInfo);
  inc(len,sizeof(tmp.head));
  tmp.head.hmac := fContext.Secret.Compute(@tmp.head.session,len-8);
  Crypt(@tmp,len);
  SetCookie(BinToBase64URI(@tmp,len));
end;

procedure TMVCSessionWithCookies.Finalize;
begin
  SetCookie(COOKIE_EXPIRED);
end;

function TMVCSessionWithCookies.LoadContext(const Saved: RawUTF8): boolean;
begin
  result := RecordLoadBase64(pointer(Saved),length(Saved),
    fContext,TypeInfo(TMVCSessionWithCookiesContext));
end;

function TMVCSessionWithCookies.SaveContext: RawUTF8;
begin
  result := RecordSaveBase64(fContext,TypeInfo(TMVCSessionWithCookiesContext));
end;


{ TMVCSessionWithRestServer }

function TMVCSessionWithRestServer.GetCookie: RawUTF8;
begin
  result := ServiceContext.Request.InCookie[fContext.CookieName];
end;

procedure TMVCSessionWithRestServer.SetCookie(const cookie: RawUTF8);
var ctxt: TSQLRestServerURIContext;
begin
  ctxt := ServiceContext.Request;
  ctxt.OutSetCookie := fContext.CookieName+'='+cookie;
  ctxt.InCookie[CookieName] := cookie;
end;


{ TMVCSessionSingle }

function TMVCSessionSingle.GetCookie: RawUTF8;
begin
  result := fSingleCookie;
end;

procedure TMVCSessionSingle.SetCookie(const cookie: RawUTF8);
begin
  fSingleCookie := cookie;
end;


{ EMVCApplication }

constructor EMVCApplication.CreateDefault(aStatus: cardinal);
begin
  inherited CreateFmt('CreateDefault(%d)',[aStatus]);
  TMVCApplication.GotoDefault(fAction,aStatus);
end;

constructor EMVCApplication.CreateGotoError(const aErrorMessage: string;
  aErrorCode: integer);
begin
  inherited CreateFmt('Error #%d: %s',[aErrorCode,aErrorMessage]);
  TMVCApplication.GotoError(fAction,aErrorMessage,aErrorCode);
end;

constructor EMVCApplication.CreateGotoError(aHtmlErrorCode: integer);
begin
  inherited CreateFmt('Error=%d',[aHtmlErrorCode]);
  TMVCApplication.GotoError(fAction,aHtmlErrorCode);
end;

constructor EMVCApplication.CreateGotoView(const aMethod: RawUTF8;
  const aParametersNameValuePairs: array of const; aStatus: cardinal);
begin
  inherited CreateFmt('GotoView(''%s'',%d)',[aMethod,aStatus]);
  TMVCApplication.GotoView(fAction,aMethod,aParametersNameValuePairs,aStatus);
end;


{ TMVCApplication }

procedure TMVCApplication.Start(aRestModel: TSQLRest; aInterface: PTypeInfo);
var m: integer;
    entry: PInterfaceEntry;
begin
  fLocker := TAutoLocker.Create;
  fRestModel := aRestModel;
  fFactory := TInterfaceFactory.Get(aInterface);
  fFactoryErrorIndex := fFactory.FindMethodIndex('Error');
  if fFactoryErrorIndex<0 then
    raise EMVCException.CreateUTF8(
      '% does not implement the IMVCApplication.Error() method',[aInterface.Name]);
  entry := GetInterfaceEntry(fFactory.InterfaceIID);
  if entry=nil then
    raise EMVCException.CreateUTF8('%.Start(%): this class should implement %',
      [self,aRestModel,fFactory.InterfaceTypeInfo^.Name]);
  fFactoryEntry := PAnsiChar(self)+entry^.IOffset;
  for m := 0 to fFactory.MethodsCount-1 do
    if not MethodHasView(fFactory.Methods[m]) then
    with fFactory.Methods[m] do
      if ArgsOutFirst<>ArgsResultIndex then
        raise EMVCException.CreateUTF8(
          '%.Start(%): %.% var/out parameters not allowed with TMVCAction result',
          [self,aRestModel,fFactory.InterfaceTypeInfo^.Name,URI]) else
        // TServiceCustomAnswer maps TMVCAction in TMVCApplication.RunOnRestServer
        ArgsResultIsServiceCustomAnswer := true;
end;

destructor TMVCApplication.Destroy;
begin
  inherited;
  fMainRunner.Free;
  fSession.Free;
end;

procedure TMVCApplication.Error(var Msg: RawUTF8; var Scope: variant);
begin // do nothing: just pass input error Msg and data Scope to the view
end;

class procedure TMVCApplication.GotoView(var Action: TMVCAction; const MethodName: RawUTF8;
  const ParametersNameValuePairs: array of const; status: cardinal);
begin
  Action.ReturnedStatus := status;
  Action.RedirectToMethodName := MethodName;
  if high(ParametersNameValuePairs)<1 then
    Action.RedirectToMethodParameters := '' else
    Action.RedirectToMethodParameters := JSONEncode(ParametersNameValuePairs);
end;

class procedure TMVCApplication.GotoError(var Action: TMVCAction;
  const Msg: string; ErrorCode: integer);
begin
  GotoView(Action,'Error',['Msg',Msg],ErrorCode);
end;

class procedure TMVCApplication.GotoError(var Action: TMVCAction;
  ErrorCode: integer);
begin
  if ErrorCode<=HTTP_CONTINUE then
    ErrorCode := HTTP_BADREQUEST;
  GotoView(Action,'Error',['Msg',StatusCodeToErrorMsg(ErrorCode)],ErrorCode);
end;

class procedure TMVCApplication.GotoDefault(var Action: TMVCAction; Status: cardinal);
begin
  Action.ReturnedStatus := Status;
  Action.RedirectToMethodName := 'Default';
  Action.RedirectToMethodParameters := '';
end;

procedure TMVCApplication.SetSession(const Value: TMVCSessionAbstract);
begin
  FreeAndNil(fSession);
  fSession := Value;
end;

function TMVCApplication.GetViewInfo(MethodIndex: integer): variant;
begin
  if MethodIndex>=0 then
    result := _ObjFast(['pageName',fFactory.Methods[MethodIndex].URI]) else
    result := _ObjFast([]);
end;

function TMVCApplication.GetMvcInfo: variant;
begin
  result := _ObjFast(['name',fFactory.InterfaceTypeInfo^.Name,
    'mORMot',SYNOPSE_FRAMEWORK_VERSION,'root',RestModel.Model.Root,
    'methods',ContextFromMethods(fFactory)]);
end;

procedure TMVCApplication.FlushAnyCache;
begin
  if fMainRunner<>nil then
    fMainRunner.NotifyContentChanged;
end;


{ TMVCRendererAbstract }

constructor TMVCRendererAbstract.Create(aApplication: TMVCApplication);
begin
  fApplication := aApplication;
end;

procedure TMVCRendererAbstract.CommandError(const ErrorName: RawUTF8;
  const ErrorValue: variant; ErrorCode: Integer);
var renderContext: variant;
begin
  renderContext := _ObjFast([
    'main',fApplication.GetViewInfo(fMethodIndex),
    'msg',StatusCodeToErrorMsg(ErrorCode),
    'errorCode',ErrorCode,ErrorName,ErrorValue]);
  renderContext.originalErrorContext := JSONReformat(ToUTF8(renderContext));
  Renders(renderContext,ErrorCode,true);
end;

procedure TMVCRendererAbstract.ExecuteCommand(aMethodIndex: integer);
var action: TMVCAction;
    exec: TServiceMethodExecute;
    isAction: boolean;
    WR: TTextWriter;
    methodOutput: RawUTF8;
    renderContext: variant;
begin
  action.ReturnedStatus := HTTP_SUCCESS;
  fMethodIndex := aMethodIndex;
  try
    if fMethodIndex>=0 then begin
      repeat
        try
          isAction := fApplication.fFactory.Methods[fMethodIndex].ArgsResultIsServiceCustomAnswer;
          WR := TJSONSerializer.CreateOwnedStream;
          try
            WR.Add('{');
            exec := TServiceMethodExecute.Create(@fApplication.fFactory.Methods[fMethodIndex]);
            try
              exec.Options := [optVariantCopiedByReference];
              exec.ServiceCustomAnswerStatus := action.ReturnedStatus;
              if not exec.ExecuteJson([fApplication.fFactoryEntry],pointer(fInput),WR,true) then
                with fApplication.fFactory do
                raise EMVCException.CreateUTF8('%.CommandRunMethod: %.%() execution error',
                  [Self,InterfaceTypeInfo^.Name,Methods[fMethodIndex].URI]);
              action.RedirectToMethodName := exec.ServiceCustomAnswerHead;
              action.ReturnedStatus := exec.ServiceCustomAnswerStatus;
            finally
              exec.Free;
            end;
            if not isAction then
              WR.Add('}');
            WR.SetText(methodOutput);
          finally
            WR.Free;
          end;
          if isAction then
            // was a TMVCAction mapped in a TServiceCustomAnswer record
            action.RedirectToMethodParameters := methodOutput else begin
            // rendering, e.g. with fast Mustache {{template}}
            _Json(methodOutput,renderContext,JSON_OPTIONS_FAST);
            TDocVariantData(renderContext).AddValue(
              'main',fApplication.GetViewInfo(fMethodIndex));
            if fMethodIndex=fApplication.fFactoryErrorIndex then
              _ObjAddProps(['errorCode',action.ReturnedStatus,
                'originalErrorContext',JSONReformat(ToUTF8(renderContext))],
                renderContext);
            Renders(renderContext,action.ReturnedStatus,false);
            exit; // success
          end;
        except
          on E: EMVCApplication do
            action := E.fAction;
        end; // lower level exceptions will be handled below
        fInput := action.RedirectToMethodParameters;
        fMethodIndex := fApplication.fFactory.FindMethodIndex(action.RedirectToMethodName);
        if action.ReturnedStatus=0 then
          action.ReturnedStatus := HTTP_SUCCESS else
        if (action.ReturnedStatus=HTTP_TEMPORARYREDIRECT) or
           (action.ReturnedStatus=HTTP_FOUND) or
           (action.ReturnedStatus=HTTP_SEEOTHER) or
           (action.ReturnedStatus=HTTP_MOVEDPERMANENTLY) then
          if Redirects(action) then // if redirection is implemented
            exit else
            action.ReturnedStatus := HTTP_SUCCESS; // fallback is to handle here
      until fMethodIndex<0; // loop to handle redirection
    end;
    // if we reached here, there was a wrong URI -> render the 404 error page
    CommandError('notfound',true,HTTP_NOTFOUND);
  except
    on E: Exception do
      CommandError('exception',
        ObjectToVariantDebug(E,'%.ExecuteCommand',[self]),HTTP_SERVERERROR);
  end;
end;

function TMVCRendererAbstract.Redirects(const action: TMVCAction): boolean;
begin
  result := false;
end; // indicates redirection did not happen -> caller should do it manually


{ TMVCRendererFromViews }

constructor TMVCRendererFromViews.Create(aRun: TMVCRunWithViews);
begin
  inherited;
  fCacheEnabled := true;
end;

procedure TMVCRendererFromViews.Renders(outContext: variant;
  status: cardinal; forcesError: boolean);
var view: TMVCView;
begin
  if forcesError or (fMethodIndex=fRun.fViews.fFactoryErrorIndex) then
    try // last change rendering of the error page
      fRun.fViews.Render(fRun.fViews.fFactoryErrorIndex,outContext,view);
    except // fallback to default HTML error template, if current did not work
      on E: Exception do begin
        _ObjAddProps(['exceptionName',E.ClassName,
          'exceptionMessage',E.Message,'className',ClassName],outContext);
        view.Content := TSynMustache.Parse(MUSTACHE_DEFAULTERROR).Render(outContext);
        view.ContentType := HTML_CONTENT_TYPE;
      end;
    end else
      fRun.fViews.Render(fMethodIndex,outContext,view);
  fOutput.Content := view.Content;
  fOutput.Header := HEADER_CONTENT_TYPE+view.ContentType;
  fOutput.Status := status;
end;


{ TMVCRendererJson }

procedure TMVCRendererJson.Renders(outContext: variant;
  status: cardinal; forcesError: boolean);
begin
  fOutput.Content := JSONReformat(ToUTF8(outContext));
  fOutput.Header := JSON_CONTENT_TYPE_HEADER_VAR;
  fOutput.Status := status;
end;


{ TMVCRun }

constructor TMVCRun.Create(aApplication: TMVCApplication);
begin
  fApplication := aApplication;
  fApplication.SetSession(nil);
end;

procedure TMVCRun.NotifyContentChangedForMethod(aMethodIndex: integer);
begin // do nothing at this abstract level
end;

procedure TMVCRun.NotifyContentChanged;
var m: integer;
begin
  for m := 0 to fApplication.fFactory.MethodsCount-1 do
    NotifyContentChangedForMethod(m)
end;

procedure TMVCRun.NotifyContentChangedForMethod(const aMethodName: RawUTF8);
begin
  NotifyContentChangedForMethod(fApplication.fFactory.FindMethodIndex(aMethodName));
end;


{ TMVCRunWithViews }

constructor TMVCRunWithViews.Create(aApplication: TMVCApplication;
  aViews: TMVCViewsAbtract);
begin
  inherited Create(aApplication);
  fViews := aViews;
  fCacheLocker := TAutoLocker.Create;
end;

function TMVCRunWithViews.SetCache(const aMethodName: RawUTF8;
  aPolicy: TMVCRendererCachePolicy; aTimeOutSeconds: cardinal): TMVCRunWithViews;
const MAX_CACHE_TIMEOUT = 60*15; // 15 minutes
var aMethodIndex: integer;
begin
  with fCacheLocker.ProtectMethod do begin
    aMethodIndex := fApplication.fFactory.CheckMethodIndex(aMethodName);
    if fCache=nil then
      SetLength(fCache,fApplication.fFactory.MethodsCount);
    with fCache[aMethodIndex] do begin
      Policy := aPolicy;
      if aTimeOutSeconds-1>=MAX_CACHE_TIMEOUT then
        TimeOutSeconds := MAX_CACHE_TIMEOUT else
        TimeOutSeconds := aTimeOutSeconds;
      NotifyContentChangedForMethod(aMethodIndex);
    end;
  end;
  result := self;
end;

destructor TMVCRunWithViews.Destroy;
begin
  fViews.Free;
  inherited;
end;

procedure TMVCRunWithViews.NotifyContentChangedForMethod(aMethodIndex: integer);
begin
  inherited;
  with fCacheLocker.ProtectMethod do
  if cardinal(aMethodIndex)<cardinal(Length(fCache)) then
  with fCache[aMethodIndex] do
    case Policy of
    cacheRootIgnoringSession,cacheRootIfSession,cacheRootIfNoSession:
      RootValue := '';
    cacheRootWithSession,
    cacheWithParametersIgnoringSession,cacheWithParametersIfSession,
    cacheWithParametersIfNoSession:
      InputValues.Init(false);
    end;
end;


{ TMVCRunOnRestServer }

constructor TMVCRunOnRestServer.Create(aApplication: TMVCApplication;
  aRestServer: TSQLRestServer; const aSubURI: RawUTF8;
  aViews: TMVCViewsAbtract; aPublishOptions: TMVCPublishOptions);
var m: integer;
    bypass: boolean;
    method: RawUTF8;
begin
  if aApplication=nil then
    raise EMVCException.CreateUTF8('%.Create(aApplication=nil)',[self]);
  if aRestServer=nil then
    fRestServer := aApplication.RestModel as TSQLRestServer else
    fRestServer := aRestServer;
  if aViews=nil then
    {$ifdef WITHLOG}
    aViews := TMVCViewsMustache.Create(aApplication.fFactory.InterfaceTypeInfo,
      fRestServer.LogClass,'.html') else
    aViews.fLogClass := fRestServer.LogClass;
    {$else}
    aViews := TMVCViewsMustache.Create(aApplication.fFactory.InterfaceTypeInfo,
      TSQLLog,'.html');
    {$endif}
  inherited Create(aApplication,aViews);
  fPublishOptions := aPublishOptions;
  bypass := bypassAuthentication in fPublishOptions;
  if aSubURI<>'' then
    fRestServer.ServiceMethodRegister(aSubURI,RunOnRestServerSub,bypass) else begin
    for m := 0 to fApplication.fFactory.MethodsCount-1 do begin
      method := fApplication.fFactory.Methods[m].URI;
      if method[1]='_' then
        delete(method,1,1); // e.g. IService._Start() -> /service/start
      fRestServer.ServiceMethodRegister(method,RunOnRestServerRoot,bypass);
    end;
    if publishMvcInfo in fPublishOptions then
      fRestServer.ServiceMethodRegister(MVCINFO_URI,RunOnRestServerRoot,bypass);
    if publishStatic in fPublishOptions then
      fRestServer.ServiceMethodRegister(STATIC_URI,RunOnRestServerRoot,bypass);
  end;
  if (registerORMTableAsExpressions in fPublishOptions) and
     aViews.InheritsFrom(TMVCViewsMustache) then
    TMVCViewsMustache(aViews).RegisterExpressionHelpersForTables(fRestServer);
  fStaticCache.Init(true);
  fApplication.SetSession(TMVCSessionWithRestServer.Create);
end;

procedure TMVCRunOnRestServer.InternalRunOnRestServer(
  Ctxt: TSQLRestServerURIContext; const MethodName: RawUTF8);
var mvcinfo, inputContext: variant;
    rawMethodName,rawFormat,static: RawUTF8;
    staticFileName: TFileName;
    rendererClass: TMVCRendererReturningDataClass;
    renderer: TMVCRendererReturningData;
    methodIndex: integer;
    method: PServiceMethod;
begin
  Split(MethodName,'/',rawMethodName,rawFormat);
  if (publishMvcInfo in fPublishOptions) and
     IdemPropNameU(rawMethodName,MVCINFO_URI) then begin
    if fMvcInfoCache='' then begin
      mvcinfo := fApplication.GetMvcInfo;
      mvcinfo.viewsFolder := fViews.ViewTemplateFolder;
      fMvcInfoCache := TSynMustache.Parse(MUSTACHE_MVCINFO).Render(mvcinfo);
    end;
    Ctxt.Returns(fMvcInfoCache,HTTP_SUCCESS,HTML_CONTENT_TYPE_HEADER,True);
  end else
  if (publishStatic in fPublishOptions) and
     IdemPropNameU(rawMethodName,STATIC_URI) then begin
    // code below will use a local in-memory cache, but would do the same as:
    // Ctxt.ReturnFileFromFolder(fViews.ViewTemplateFolder+STATIC_URI);
    static := fStaticCache.Value(rawFormat,#0);
    if static=#0 then begin
      if PosEx('..',rawFormat)>0 then // avoid injection
        static := '' else begin
        staticFileName := UTF8ToString(StringReplaceChars(rawFormat,'/',PathDelim));
        static := StringFromFile(fViews.ViewTemplateFolder+STATIC_URI+PathDelim+staticFileName);
        if static<>'' then
          static := GetMimeContentType(nil,0,staticFileName)+#0+static;
      end;
      fStaticCache.Add(rawFormat,static);
    end;
    if static='' then
      Ctxt.Error('',HTTP_NOTFOUND) else begin
      Split(static,#0,rawFormat,static);
      Ctxt.Returns(static,HTTP_SUCCESS,HEADER_CONTENT_TYPE+rawFormat,True);
    end;
    exit;
  end else begin
     if IdemPropNameU(rawFormat,'json') then
      rendererClass := TMVCRendererJSON else
      rendererClass := TMVCRendererFromViews;
    renderer := rendererClass.Create(self);
    try
      if Ctxt.Method in [mGET,mPOST] then begin
        methodIndex := fApplication.fFactory.FindMethodIndex(rawMethodName);
        if methodIndex>=0 then begin
          inputContext := Ctxt.InputAsTDocVariant;
          if not VarIsEmpty(inputContext) then
          with _Safe(inputContext)^ do begin
            if (Kind=dvObject) and (Count>0) then begin
              // try {"p.a1":5,"p.a2":"dfasdfa"} -> {"p":{"a1":5,"a2":"dfasdfa"}}
              method := @fViews.fFactory.Methods[methodIndex];
              if method^.ArgsInputValuesCount=1 then
                FlattenAsNestedObject(RawUTF8(method^.Args[method^.ArgsInFirst].ParamName^));
            end;
            renderer.fInput := ToJSON;
          end;
        end;
        renderer.ExecuteCommand(methodIndex);
      end else
        renderer.CommandError('notfound',true,HTTP_NOTFOUND);
      Ctxt.Returns(renderer.Output.Content,renderer.Output.Status,
        renderer.Output.Header,True,true);
    finally
      renderer.Free;
    end;
  end;
end;

procedure TMVCRunOnRestServer.RunOnRestServerRoot(Ctxt: TSQLRestServerURIContext);
begin
  InternalRunOnRestServer(Ctxt,Ctxt.URI+'/'+Ctxt.URIBlobFieldName);
end;

procedure TMVCRunOnRestServer.RunOnRestServerSub(Ctxt: TSQLRestServerURIContext);
begin
  if Ctxt.URIBlobFieldName='' then
    Ctxt.Redirect(Ctxt.URIWithoutSignature+'/default') else
    InternalRunOnRestServer(Ctxt,Ctxt.URIBlobFieldName);
end;


{ TMVCRendererReturningData }

constructor TMVCRendererReturningData.Create(aRun: TMVCRunWithViews);
begin
  fRun := aRun;
  inherited Create(fRun.Application);
end;

procedure TMVCRendererReturningData.ExecuteCommand(aMethodIndex: integer);
  procedure SetOutputValue(const aValue: RawUTF8);
  begin
    fOutput.Status := HTTP_SUCCESS;
    Split(aValue,#0,fOutput.Header,RawUTF8(fOutput.Content));
  end;
  function RetrievedFromInputValues(const aKey: RawUTF8;
    const aInputValues: TSynNameValue): boolean;
  var i: integer;
  begin
    i := aInputValues.Find(aKey);
    if (i>=0) and (aInputValues.List[i].Value<>'') and
       (fCacheCurrentSec<cardinal(aInputValues.List[i].Tag)) then begin
      SetOutputValue(aInputValues.List[i].Value);
      result := true;
    end else begin
      fCacheCurrent := inputCache;
      fCacheCurrentInputValueKey := aKey;
      result := false;
    end;
  end;
var sessionID: integer;
label doRoot,doInput;
begin
  if not fCacheEnabled then begin
    inherited ExecuteCommand(aMethodIndex);
    exit;
  end;
  fCacheCurrent := noCache;
  fCacheCurrentSec := GetTickCount64 div 1000;
  fRun.fCacheLocker.Enter;
  try
    if cardinal(aMethodIndex)<cardinal(Length(fRun.fCache)) then
    with fRun.fCache[aMethodIndex] do begin
      case Policy of
      cacheRootIgnoringSession:
        if fInput='' then
   doRoot:if (RootValue<>'') and (fCacheCurrentSec<RootValueExpirationTime) then begin
            SetOutputValue(RootValue);
            exit;
          end else
            fCacheCurrent := rootCache;
      cacheRootIfSession:
        if (fInput='') and fApplication.CurrentSession.Exists then
          goto doRoot;
      cacheRootIfNoSession:
        if (fInput='') and not fApplication.CurrentSession.Exists then
          goto doRoot;
      cacheRootWithSession:
        if fInput='' then begin
          sessionID := fApplication.CurrentSession.CheckAndRetrieve;
          if sessionID=0 then
            goto doRoot else
          if RetrievedFromInputValues(UInt32ToUtf8(sessionID),InputValues) then
            exit;
        end;
      cacheWithParametersIgnoringSession:
  doInput:if fInput='' then
            goto doRoot else
          if RetrievedFromInputValues(fInput,InputValues) then
            exit;
      cacheWithParametersIfSession:
        if fApplication.CurrentSession.Exists then
          goto doInput;
      cacheWithParametersIfNoSession:
        if not fApplication.CurrentSession.Exists then
          goto doInput;
      end;
    end;
  finally
    fRun.fCacheLocker.Leave; // ExecuteCommand() process should not be locked
  end;
  inherited ExecuteCommand(aMethodIndex);
  if fCacheCurrent<>noCache then
  try
    fRun.fCacheLocker.Enter;
    with fRun.fCache[aMethodIndex] do begin
      inc(fCacheCurrentSec,TimeOutSeconds);
      case fCacheCurrent of
      rootCache:
        if fOutput.Status=HTTP_SUCCESS then begin
          RootValue := fOutput.Header+#0+fOutput.Content;
          RootValueExpirationTime := fCacheCurrentSec;
        end else
          RootValue := '';
      inputCache:
        if fOutput.Status=HTTP_SUCCESS then
          InputValues.Add(fCacheCurrentInputValueKey,fOutput.Header+#0+fOutput.Content,fCacheCurrentSec) else
          InputValues.Add(fCacheCurrentInputValueKey,'');
      end;
    end;
  finally
    fRun.fCacheLocker.Leave;
  end;
end;

function TMVCRendererReturningData.Redirects(const action: TMVCAction): boolean;
begin
  fOutput.Header := 'Location: '+UrlEncodeJsonObject(action.RedirectToMethodName,
    pointer(action.RedirectToMethodParameters),['main']);
  fOutput.Status := action.ReturnedStatus;
  result := true;
end;


initialization
  assert(sizeof(TMVCAction)=sizeof(TServiceCustomAnswer));
end.

