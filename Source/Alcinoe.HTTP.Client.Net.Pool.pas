unit Alcinoe.HTTP.Client.Net.Pool;

interface

{$I Alcinoe.inc}

uses
  System.classes,
  System.Net.URLClient,
  System.Net.HttpClient,
  Alcinoe.Common;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALNetHttpClientPoolCanStartRefFunc = reference to function (var AContext: Tobject): boolean;
  TALNetHttpClientPoolCanStartObjFunc = function (var AContext: Tobject): boolean of object;
  TALNetHttpClientPoolOnSuccessRefProc = reference to procedure (const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AContext: TObject);
  TALNetHttpClientPoolOnSuccessObjProc = procedure (const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AContext: TObject) of object;
  TALNetHttpClientPoolOnErrorRefProc = reference to procedure (const AErrMessage: string; var AContext: Tobject);
  TALNetHttpClientPoolOnErrorObjProc = procedure (const AErrMessage: string; var AContext: Tobject) of object;
  TALNetHttpClientPoolCacheDataProc = procedure(const aUrl: String; const AHTTPResponse: IHTTPResponse; const aData: TMemoryStream) of object;
  TALNetHttpClientPoolRetrieveCachedDataProc = function(const aUrl: String; out AHTTPResponse: IHTTPResponse; const aData: TMemoryStream): boolean of object;

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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _HttpGetUrl(const aUrl: String; const LResponseContent: TStream; const AHeaders: TNetHeaders): IHTTPResponse;
  begin
    Var LUri := Turi.Create(aUrl);
    var LNetHttpClient := ALAcquireKeepAliveNetHttpClient(LUri);
    try
      // In case we lose the connection, this might return success with
      // partial content. What a mess!
      result := LNetHttpClient.Get(aUrl, LResponseContent, AHeaders);
    finally
      ALReleaseKeepAliveNetHttpClient(LUri, LNetHttpClient);
    end;
  end;

begin

  var LNetHttpClientPoolRequest := TALNetHttpClientPoolRequest(AContext);

  //protect the following code from exception
  try

    //init the http
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

        //get from the url
        if LNetHttpClientPoolRequest.Url <> '' then begin
          if AlIsHttpOrHttpsUrl(LNetHttpClientPoolRequest.Url) then begin
            if (not LNetHttpClientPoolRequest.UseCache) or
               (not assigned(RetrieveCachedData)) or
               (not RetrieveCachedData(LNetHttpClientPoolRequest.Url, LHTTPResponse, LResponseContent)) then begin
              LHTTPResponse := _HttpGetUrl(LNetHttpClientPoolRequest.Url, LResponseContent, LNetHttpClientPoolRequest.Headers);
              // Client error responses (400 – 499)
              // Server error responses (500 – 599)
              if (LHTTPResponse = nil) or
                 ((LHTTPResponse.StatusCode >= 400) and (LHTTPResponse.StatusCode <= 599)) then begin
                if assigned(LNetHttpClientPoolRequest.OnErrorObjProc) then
                  LNetHttpClientPoolRequest.OnErrorObjProc(ALFormatW('HTTP request failed (%d)', [LHTTPResponse.StatusCode]), LNetHttpClientPoolRequest.FContext)
                else if assigned(LNetHttpClientPoolRequest.OnErrorRefProc) then
                  LNetHttpClientPoolRequest.OnErrorRefProc(ALFormatW('HTTP request failed (%d)', [LHTTPResponse.StatusCode]), LNetHttpClientPoolRequest.FContext);
                exit;
              end;
              ALDecompressHttpResponseBody(LHTTPResponse.ContentEncoding, LResponseContent);
              if (LNetHttpClientPoolRequest.UseCache) and
                 (assigned(CacheData)) then CacheData(LNetHttpClientPoolRequest.Url, LHTTPResponse, LResponseContent);
            end;
          end
          else LResponseContent.LoadFromFile(LNetHttpClientPoolRequest.Url);
        end;

        //fire the OnSuccess
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
        LNetHttpClientPoolRequest.OnErrorObjProc(E.Message, LNetHttpClientPoolRequest.FContext)
      else if assigned(LNetHttpClientPoolRequest.OnErrorRefProc) then
        LNetHttpClientPoolRequest.OnErrorRefProc(E.Message, LNetHttpClientPoolRequest.FContext);
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