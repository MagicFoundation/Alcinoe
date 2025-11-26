unit Alcinoe.HTTP.Client.Net.Pool;

interface

{$I Alcinoe.inc}

uses
  System.classes,
  System.Net.URLClient,
  System.Net.HttpClient,
  Alcinoe.Common;

type

  {$IFNDEF ALCompilerVersionSupported130}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-4664 was corrected'}
    // If https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-4664 has been fixed then
    // replace
    //   TALNetHttpClientPoolOnSuccessRefProc = reference to procedure (const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AContext: TObject);
    //   TALNetHttpClientPoolOnSuccessObjProc = procedure (const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AContext: TObject) of object;
    // with
    //   TALNetHttpClientPoolOnSuccessRefProc = reference to procedure (const AResponse: IHTTPResponse; var AContext: TObject);
    //   TALNetHttpClientPoolOnSuccessObjProc = procedure (const AResponse: IHTTPResponse; var AContext: TObject) of object;
    // You will need also to replace
    //   ALDecompressHttpResponseBody(LHTTPResponse.ContentEncoding, LResponseContent);
    // with
    //   ALDecompressHttpResponseBody(LHTTPResponse);
  {$ENDIF}

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALNetHttpClientPoolCanStartRefFunc = reference to function (var AContext: Tobject): boolean;
  TALNetHttpClientPoolCanStartObjFunc = function (var AContext: Tobject): boolean of object;
  TALNetHttpClientPoolOnSuccessRefProc = reference to procedure (const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AContext: TObject);
  TALNetHttpClientPoolOnSuccessObjProc = procedure (const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AContext: TObject) of object;
  TALNetHttpClientPoolOnErrorRefProc = reference to procedure (const AResponse: IHTTPResponse; const AErrMessage: string; var AContext: Tobject);
  TALNetHttpClientPoolOnErrorObjProc = procedure (const AResponse: IHTTPResponse; const AErrMessage: string; var AContext: Tobject) of object;
  TALNetHttpClientPoolCacheDataProc = procedure(const AUrl: String; const AHeaders: TNetHeaders; const AResponse: IHTTPResponse) of object;
  TALNetHttpClientPoolRetrieveCachedDataProc = function(const AUrl: String; const AHeaders: TNetHeaders; out AResponse: IHTTPResponse): boolean of object;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALNetHttpClientPoolRequest = Class(Tobject)
  private
    FUrl: String;
    FCanStartRefFunc: TALNetHttpClientPoolCanStartRefFunc;
    FCanStartObjFunc: TALNetHttpClientPoolCanStartObjFunc;
    FOnSuccessRefProc: TALNetHttpClientPoolOnSuccessRefProc;
    FOnSuccessObjProc: TALNetHttpClientPoolOnSuccessObjProc;
    FOnErrorRefProc: TALNetHttpClientPoolOnErrorRefProc;
    FOnErrorObjProc: TALNetHttpClientPoolOnErrorObjProc;
    FGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
    FContext: Tobject;
    FUseCache: Boolean;
    FHeaders: TNetHeaders;
  public
    constructor Create(
                  const AUrl: String;
                  const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
                  const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
                  const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
                  const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
                  const AContext: Tobject;
                  const AUseCache: Boolean;
                  const AHeaders: TNetHeaders); overload;
    constructor Create(
                  const AUrl: String;
                  const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
                  const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
                  const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
                  const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
                  const AContext: Tobject;
                  const AUseCache: Boolean;
                  const AHeaders: TNetHeaders); overload;
    destructor Destroy; override;
    Property Url: String read FUrl;
    Property CanStartRefFunc: TALNetHttpClientPoolCanStartRefFunc read FCanStartRefFunc;
    Property CanStartObjFunc: TALNetHttpClientPoolCanStartObjFunc read FCanStartObjFunc;
    Property OnSuccessRefProc: TALNetHttpClientPoolOnSuccessRefProc read FOnSuccessRefProc;
    Property OnSuccessObjProc: TALNetHttpClientPoolOnSuccessObjProc read FOnSuccessObjProc;
    Property OnErrorRefProc: TALNetHttpClientPoolOnErrorRefProc read FOnErrorRefProc;
    Property OnErrorObjProc: TALNetHttpClientPoolOnErrorObjProc read FOnErrorObjProc;
    Property GetPriorityFunc: TALWorkerThreadGetPriorityFunc read FGetPriorityFunc;
    Property Context: Tobject read FContext;
    Property UseCache: Boolean read FUseCache;
    Property Headers: TNetHeaders read FHeaders;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALNetHttpClientPool = Class(TALWorkerThreadPool)
  private
    class function CreateInstance: TALNetHttpClientPool;
    class function GetInstance: TALNetHttpClientPool; static;
  protected
    class var FInstance: TALNetHttpClientPool;
  public
    type
      TCreateInstanceFunc = function: TALNetHttpClientPool;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALNetHttpClientPool read GetInstance;
    class function HasInstance: Boolean; inline;
  private
    FCacheData: TALNetHttpClientPoolCacheDataProc;
    FRetrieveCachedData: TALNetHttpClientPoolRetrieveCachedDataProc;
    function DoGetPriority(const AContext: Tobject): Int64;
    procedure DoGet(var AContext: Tobject);
  protected
  public
    // RefFunc / RefProc
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AContext: Tobject; // Context will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: Int64;
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AContext: Tobject; // Context will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: TALWorkerThreadGetPriorityFunc; // [MultiThread]
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AContext: Tobject; // Context will be free by the worker thread
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AContext: Tobject; // Context will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: Int64;
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AContext: Tobject; // Context will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: TALWorkerThreadGetPriorityFunc; // [MultiThread]
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AContext: Tobject; // Context will be free by the worker thread
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    // ObjFunc / ObjProc
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AContext: Tobject; // Context will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: Int64;
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AContext: Tobject; // Context will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: TALWorkerThreadGetPriorityFunc; // [MultiThread]
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AContext: Tobject; // Context will be free by the worker thread
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AContext: Tobject; // Context will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: Int64;
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AContext: Tobject; // Context will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: TALWorkerThreadGetPriorityFunc; // [MultiThread]
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AContext: Tobject; // Context will be free by the worker thread
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AHeaders: TNetHeaders = nil;
                const AAsync: Boolean = True); overload;
    // CacheData
    Property CacheData: TALNetHttpClientPoolCacheDataProc read FCacheData write FCacheData;
    Property RetrieveCachedData: TALNetHttpClientPoolRetrieveCachedDataProc read FRetrieveCachedData write FRetrieveCachedData;
  end;

implementation

uses
  System.SysUtils,
  Alcinoe.url,
  Alcinoe.HTTP,
  Alcinoe.HTTP.Client.Net,
  Alcinoe.StringUtils;

{*********************************************}
constructor TALNetHttpClientPoolRequest.Create(
              const AUrl: String;
              const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
              const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
              const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
              const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
              const AContext: Tobject;
              const AUseCache: Boolean;
              const AHeaders: TNetHeaders);
begin
  inherited create;
  FUrl := AUrl;
  FCanStartRefFunc := ACanStartCallBack;
  FCanStartObjFunc := nil;
  FOnSuccessRefProc := AOnSuccessCallBack;
  FOnSuccessObjProc := nil;
  FOnErrorRefProc := AOnErrorCallBack;
  FOnErrorObjProc := nil;
  FGetPriorityFunc := AGetPriorityFunc;
  FContext := AContext;
  FUseCache := AUseCache;
  FHeaders := AHeaders;
end;

{*********************************************}
constructor TALNetHttpClientPoolRequest.Create(
              const AUrl: String;
              const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
              const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
              const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
              const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
              const AContext: Tobject;
              const AUseCache: Boolean;
              const AHeaders: TNetHeaders);
begin
  inherited create;
  FUrl := AUrl;
  FCanStartRefFunc := nil;
  FCanStartObjFunc := ACanStartCallBack;
  FOnSuccessRefProc := nil;
  FOnSuccessObjProc := AOnSuccessCallBack;
  FOnErrorRefProc := nil;
  FOnErrorObjProc := AOnErrorCallBack;
  FGetPriorityFunc := AGetPriorityFunc;
  FContext := AContext;
  FUseCache := AUseCache;
  FHeaders := AHeaders;
end;

{*********************************************}
destructor TALNetHttpClientPoolRequest.Destroy;
begin
  ALFreeAndNil(FContext);
  inherited Destroy;
end;

{***********************************************************************}
class function TALNetHttpClientPool.CreateInstance: TALNetHttpClientPool;
begin
  //https://stackoverflow.com/questions/70054035/what-the-ideal-maximum-parallels-http-connections-an-android-app-can-have
  result := TALNetHttpClientPool.Create(8);
end;

{********************************************************************}
class function TALNetHttpClientPool.GetInstance: TALNetHttpClientPool;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance)
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALNetHttpClientPool.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{*************}
//[MultiThread]
function TALNetHttpClientPool.DoGetPriority(const AContext: Tobject): Int64;
begin
  var LNetHttpClientPoolRequest := TALNetHttpClientPoolRequest(AContext);
  if assigned(LNetHttpClientPoolRequest.GetPriorityFunc) then
    Result := LNetHttpClientPoolRequest.GetPriorityFunc(LNetHttpClientPoolRequest.FContext)
  else
    Result := 0;
end;

{*************}
//[MultiThread]
procedure TALNetHttpClientPool.DoGet(var AContext: Tobject);
begin

  var LNetHttpClientPoolRequest := TALNetHttpClientPoolRequest(AContext);
  try

    // Check whether this request is allowed to start
    if ((not assigned(LNetHttpClientPoolRequest.CanStartObjFunc)) and
        (not assigned(LNetHttpClientPoolRequest.CanStartRefFunc))) or
       (assigned(LNetHttpClientPoolRequest.CanStartObjFunc) and
        LNetHttpClientPoolRequest.CanStartObjFunc(LNetHttpClientPoolRequest.FContext)) or
       (assigned(LNetHttpClientPoolRequest.CanStartRefFunc) and
        LNetHttpClientPoolRequest.CanStartRefFunc(LNetHttpClientPoolRequest.FContext)) then begin

      //create the http
      var LHTTPResponse: IHTTPResponse := nil;
      var LResponseContent := TMemoryStream.Create;
      try

        // Prepare HTTP call
        if LNetHttpClientPoolRequest.Url <> '' then begin
          if AlIsHttpOrHttpsUrl(LNetHttpClientPoolRequest.Url) then begin
            if (not LNetHttpClientPoolRequest.UseCache) or
               (not assigned(RetrieveCachedData)) or
               (not RetrieveCachedData(LNetHttpClientPoolRequest.Url, LNetHttpClientPoolRequest.Headers, LHTTPResponse)) then begin

              Var LUri := Turi.Create(LNetHttpClientPoolRequest.Url);
              var LNetHttpClient := ALAcquireKeepAliveNetHttpClient(LUri);
              try
                // Note: if the connection drops, the server may close the socket
                // and the client can still return "success" with partial content.
                LHTTPResponse := LNetHttpClient.Get(LNetHttpClientPoolRequest.Url, LResponseContent, LNetHttpClientPoolRequest.Headers);
              finally
                ALReleaseKeepAliveNetHttpClient(LUri, LNetHttpClient);
              end;

              // Handle Content-Encoding (gzip, deflate, br, ...)
              ALDecompressHttpResponseBody(LHTTPResponse.ContentEncoding, LResponseContent);

              // Treat 4xx (client error) and 5xx (server error) as failures
              if (LHTTPResponse.StatusCode >= 400) and (LHTTPResponse.StatusCode <= 599) then begin
                if assigned(LNetHttpClientPoolRequest.OnErrorObjProc) then
                  LNetHttpClientPoolRequest.OnErrorObjProc(LHTTPResponse, ALFormatW('HTTP request failed (%d)', [LHTTPResponse.StatusCode]), LNetHttpClientPoolRequest.FContext)
                else if assigned(LNetHttpClientPoolRequest.OnErrorRefProc) then
                  LNetHttpClientPoolRequest.OnErrorRefProc(LHTTPResponse, ALFormatW('HTTP request failed (%d)', [LHTTPResponse.StatusCode]), LNetHttpClientPoolRequest.FContext);
                exit;
              end;

              // Store successful HTTP responses in the cache (if enabled)
              if (LNetHttpClientPoolRequest.UseCache) and
                 (assigned(CacheData)) then CacheData(LNetHttpClientPoolRequest.Url, LNetHttpClientPoolRequest.Headers, LHTTPResponse);

            end;
          end
          else
            // Non-HTTP(S) URL: treat it as a local file path
            LResponseContent.LoadFromFile(LNetHttpClientPoolRequest.Url);
        end;

        // Notify the caller that the request completed successfully
        if assigned(LNetHttpClientPoolRequest.OnSuccessObjProc) then
          LNetHttpClientPoolRequest.OnSuccessObjProc(LHTTPResponse, LResponseContent, LNetHttpClientPoolRequest.FContext)
        else if assigned(LNetHttpClientPoolRequest.OnSuccessRefProc) then
          LNetHttpClientPoolRequest.OnSuccessRefProc(LHTTPResponse, LResponseContent, LNetHttpClientPoolRequest.FContext);

      finally
        LHTTPResponse := nil;
        ALFreeandNil(LResponseContent);
      end;

    end;

  except
    on E: exception do begin
      if assigned(LNetHttpClientPoolRequest.OnErrorObjProc) then
        LNetHttpClientPoolRequest.OnErrorObjProc(nil{AResponse}, E.Message, LNetHttpClientPoolRequest.FContext)
      else if assigned(LNetHttpClientPoolRequest.OnErrorRefProc) then
        LNetHttpClientPoolRequest.OnErrorRefProc(nil{AResponse}, E.Message, LNetHttpClientPoolRequest.FContext);
    end;
  end;

end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: Int64;
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      nil{AGetPriorityFunc},
      AContext,
      AUseCache,
      AHeaders), // const AContext: Tobject;
    APriority, // const APriority: Int64;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: TALWorkerThreadGetPriorityFunc;
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      APriority,
      AContext,
      AUseCache,
      AHeaders), // const AContext: Tobject;
    DoGetPriority, // const APriority: TALWorkerThreadGetPriorityFunc;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      nil{AGetPriorityFunc},
      AContext,
      true{AUseCache},
      AHeaders), // const AContext: Tobject;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      nil{AGetPriorityFunc},
      nil{AContext},
      true{AUseCache},
      AHeaders), // const AContext: Tobject;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: Int64;
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
    AContext, // const AContext: Tobject; // Context will be free by the worker thread
    AUseCache, // const AUseCache: Boolean;
    APriority, // const APriority: Int64;
    AHeaders, // const AHeaders: TNetHeaders = nil
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: TALWorkerThreadGetPriorityFunc;
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
    AContext, // const AContext: Tobject; // Context will be free by the worker thread
    AUseCache, // const AUseCache: Boolean;
    APriority, // const APriority: TALWorkerThreadGetPriorityFunc;
    AHeaders, // const AHeaders: TNetHeaders = nil
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
    AContext, // const AContext: Tobject; // Context will be free by the worker thread
    AHeaders, // const AHeaders: TNetHeaders = nil
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
    AHeaders, // const AHeaders: TNetHeaders = nil
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: Int64;
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      nil{AGetPriorityFunc},
      AContext,
      AUseCache,
      AHeaders), // const AContext: Tobject;
    APriority, // const APriority: Int64;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: TALWorkerThreadGetPriorityFunc;
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      APriority,
      AContext,
      AUseCache,
      AHeaders), // const AContext: Tobject;
    DoGetPriority, // const APriority: TALWorkerThreadGetPriorityFunc;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      nil{AGetPriorityFunc},
      AContext,
      true{AUseCache},
      AHeaders), // const AContext: Tobject;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      nil{AGetPriorityFunc},
      nil{AContext},
      true{AUseCache},
      AHeaders), // const AContext: Tobject;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: Int64;
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
    AContext, // const AContext: Tobject; // Context will be free by the worker thread
    AUseCache, // const AUseCache: Boolean;
    APriority, // const APriority: Int64;
    AHeaders, // const AHeaders: TNetHeaders = nil
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: TALWorkerThreadGetPriorityFunc;
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
    AContext, // const AContext: Tobject; // Context will be free by the worker thread
    AUseCache, // const AUseCache: Boolean;
    APriority, // const APriority: TALWorkerThreadGetPriorityFunc;
    AHeaders, // const AHeaders: TNetHeaders = nil
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
    AContext, // const AContext: Tobject; // Context will be free by the worker thread
    AHeaders, // const AHeaders: TNetHeaders = nil
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AHeaders: TNetHeaders = nil;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
    AHeaders, // const AHeaders: TNetHeaders = nil
    AAsync); // const AAsync: Boolean = True
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.HTTP.Client.Net.Pool','initialization');
  {$ENDIF}
  TALNetHttpClientPool.FInstance := nil;
  TALNetHttpClientPool.CreateInstanceFunc := @TALNetHttpClientPool.CreateInstance;

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.HTTP.Client.Net.Pool','finalization');
  {$ENDIF}
  ALFreeAndNil(TALNetHttpClientPool.FInstance);

end.