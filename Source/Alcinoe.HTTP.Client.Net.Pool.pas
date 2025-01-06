unit Alcinoe.HTTP.Client.Net.Pool;

interface

{$I Alcinoe.inc}

uses
  System.classes,
  System.Net.HttpClient,
  System.Net.URLClient,
  Alcinoe.Common;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALNetHttpClientPoolCanStartRefFunc = reference to function (var AExtData: Tobject): boolean;
  TALNetHttpClientPoolCanStartObjFunc = function (var AExtData: Tobject): boolean of object;
  TALNetHttpClientPoolOnSuccessRefProc = reference to procedure (const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AExtData: TObject);
  TALNetHttpClientPoolOnSuccessObjProc = procedure (const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AExtData: TObject) of object;
  TALNetHttpClientPoolOnErrorRefProc = reference to procedure (const AErrMessage: string; var AExtData: Tobject);
  TALNetHttpClientPoolOnErrorObjProc = procedure (const AErrMessage: string; var AExtData: Tobject) of object;
  TALNetHttpClientPoolCacheDataProc = procedure(const aUrl: String; const AHTTPResponse: IHTTPResponse; const aData: TMemoryStream) of object;
  TALNetHttpClientPoolRetrieveCachedDataProc = function(const aUrl: String; out AHTTPResponse: IHTTPResponse; const aData: TMemoryStream): boolean of object;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALNetHttpClientPoolRequest = Class(Tobject)
  private
    FUrl: String;
    FCanStartCallBackRefFunc: TALNetHttpClientPoolCanStartRefFunc;
    FCanStartCallBackObjFunc: TALNetHttpClientPoolCanStartObjFunc;
    FOnSuccessCallBackRefProc: TALNetHttpClientPoolOnSuccessRefProc;
    FOnSuccessCallBackObjProc: TALNetHttpClientPoolOnSuccessObjProc;
    FOnErrorCallBackRefProc: TALNetHttpClientPoolOnErrorRefProc;
    FOnErrorCallBackObjProc: TALNetHttpClientPoolOnErrorObjProc;
    FExtData: Tobject;
    FUseCache: Boolean;
  public
    constructor Create(
                  const AUrl: String;
                  const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
                  const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
                  const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
                  const AExtData: Tobject;
                  const AUseCache: Boolean); overload;
    constructor Create(
                  const AUrl: String;
                  const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
                  const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
                  const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
                  const AExtData: Tobject;
                  const AUseCache: Boolean); overload;
    destructor Destroy; override;
    Property Url: String read FUrl;
    Property CanStartCallBackRefFunc: TALNetHttpClientPoolCanStartRefFunc read FCanStartCallBackRefFunc;
    Property CanStartCallBackObjFunc: TALNetHttpClientPoolCanStartObjFunc read FCanStartCallBackObjFunc;
    Property OnSuccessCallBackRefProc: TALNetHttpClientPoolOnSuccessRefProc read FOnSuccessCallBackRefProc;
    Property OnSuccessCallBackObjProc: TALNetHttpClientPoolOnSuccessObjProc read FOnSuccessCallBackObjProc;
    Property OnErrorCallBackRefProc: TALNetHttpClientPoolOnErrorRefProc read FOnErrorCallBackRefProc;
    Property OnErrorCallBackObjProc: TALNetHttpClientPoolOnErrorObjProc read FOnErrorCallBackObjProc;
    Property ExtData: Tobject read FExtData;
    Property UseCache: Boolean read FUseCache;
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
  private
    FCacheData: TALNetHttpClientPoolCacheDataProc;
    FRetrieveCachedData: TALNetHttpClientPoolRetrieveCachedDataProc;
    procedure DoGet(var AExtData: Tobject);
  protected
  public
    // RefFunc / RefProc
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: Int64;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: TALWorkerThreadGetPriorityFunc; // [MultiThread]
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: Int64;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: TALWorkerThreadGetPriorityFunc; // [MultiThread]
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc; // [MultiThread]
                const AAsync: Boolean = True); overload;
    // ObjFunc / ObjProc
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: Int64;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: TALWorkerThreadGetPriorityFunc; // [MultiThread]
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: Int64;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: TALWorkerThreadGetPriorityFunc; // [MultiThread]
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc; // [MultiThread]
                const AAsync: Boolean = True); overload;
    // CacheData
    Property CacheData: TALNetHttpClientPoolCacheDataProc read FCacheData write FCacheData;
    Property RetrieveCachedData: TALNetHttpClientPoolRetrieveCachedDataProc read FRetrieveCachedData write FRetrieveCachedData;
  end;

implementation

uses
  system.SysUtils,
  Alcinoe.HTTP.Client.Net,
  Alcinoe.HTTP.Client,
  Alcinoe.Cipher,
  Alcinoe.StringUtils;

{*********************************************}
constructor TALNetHttpClientPoolRequest.Create(
              const AUrl: String;
              const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
              const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
              const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
              const AExtData: Tobject;
              const AUseCache: Boolean);
begin
  inherited create;
  FUrl := AUrl;
  FCanStartCallBackRefFunc := ACanStartCallBack;
  FCanStartCallBackObjFunc := nil;
  FOnSuccessCallBackRefProc := AOnSuccessCallBack;
  FOnSuccessCallBackObjProc := nil;
  FOnErrorCallBackRefProc := AOnErrorCallBack;
  FOnErrorCallBackObjProc := nil;
  FExtData := AExtData;
  FUseCache := AUseCache;
end;

{*********************************************}
constructor TALNetHttpClientPoolRequest.Create(
              const AUrl: String;
              const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
              const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
              const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
              const AExtData: Tobject;
              const AUseCache: Boolean);
begin
  inherited create;
  FUrl := AUrl;
  FCanStartCallBackRefFunc := nil;
  FCanStartCallBackObjFunc := ACanStartCallBack;
  FOnSuccessCallBackRefProc := nil;
  FOnSuccessCallBackObjProc := AOnSuccessCallBack;
  FOnErrorCallBackRefProc := nil;
  FOnErrorCallBackObjProc := AOnErrorCallBack;
  FExtData := AExtData;
  FUseCache := AUseCache;
end;

{*********************************************}
destructor TALNetHttpClientPoolRequest.Destroy;
begin
  ALFreeAndNil(FExtData);
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

{**********************************************************}
procedure TALNetHttpClientPool.DoGet(var AExtData: Tobject);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _HttpGetUrl(const aUrl: String; const LResponseContent: TStream): IHTTPResponse;
  begin
    Var LUri := Turi.Create(aUrl);
    var LNetHttpClient := ALAcquireKeepAliveNetHttpClient(LUri);
    try
      // In case we lose the connection, this might return success with
      // partial content. What a mess!
      result := LNetHttpClient.Get(aUrl, LResponseContent);
    finally
      ALReleaseKeepAliveNetHttpClient(LUri, LNetHttpClient);
    end;
  end;

begin

  var LNetHttpClientPoolRequest := TALNetHttpClientPoolRequest(AExtData);

  //protect the following code from exception
  try

    //init the http
    if ((not assigned(LNetHttpClientPoolRequest.CanStartCallBackObjFunc)) and
        (not assigned(LNetHttpClientPoolRequest.CanStartCallBackRefFunc))) or
       (assigned(LNetHttpClientPoolRequest.CanStartCallBackObjFunc) and
        LNetHttpClientPoolRequest.CanStartCallBackObjFunc(LNetHttpClientPoolRequest.FExtData)) or
       (assigned(LNetHttpClientPoolRequest.CanStartCallBackRefFunc) and
        LNetHttpClientPoolRequest.CanStartCallBackRefFunc(LNetHttpClientPoolRequest.FExtData)) then begin

      //create the http
      var LHTTPResponse: IHTTPResponse := nil;
      var LResponseContent := TMemoryStream.Create;
      try

        //get from the url
        if LNetHttpClientPoolRequest.Url <> '' then begin
          if AlIsHttpOrHttpsUrl(LNetHttpClientPoolRequest.Url) then begin
            if LNetHttpClientPoolRequest.UseCache then begin
              if (not assigned(RetrieveCachedData)) or
                 (not RetrieveCachedData(LNetHttpClientPoolRequest.Url, LHTTPResponse, LResponseContent)) then begin
                LHTTPResponse := _HttpGetUrl(LNetHttpClientPoolRequest.Url, LResponseContent);
                // Client error responses (400 – 499)
                // Server error responses (500 – 599)
                if (LHTTPResponse = nil) or
                   ((LHTTPResponse.StatusCode >= 400) and (LHTTPResponse.StatusCode <= 599)) then begin
                  if assigned(LNetHttpClientPoolRequest.OnErrorCallBackObjProc) then
                    LNetHttpClientPoolRequest.OnErrorCallBackObjProc(ALFormatW('HTTP request failed (%d)', [LHTTPResponse.StatusCode], ALDefaultFormatSettingsW), LNetHttpClientPoolRequest.FExtData)
                  else if assigned(LNetHttpClientPoolRequest.OnErrorCallBackRefProc) then
                    LNetHttpClientPoolRequest.OnErrorCallBackRefProc(ALFormatW('HTTP request failed (%d)', [LHTTPResponse.StatusCode], ALDefaultFormatSettingsW), LNetHttpClientPoolRequest.FExtData);
                  exit;
                end;
                ALDecompressHttpResponseContent(LHTTPResponse.ContentEncoding, LResponseContent);
                if (assigned(CacheData)) then CacheData(LNetHttpClientPoolRequest.Url, LHTTPResponse, LResponseContent);
              end;
            end
            else LHTTPResponse := _HttpGetUrl(LNetHttpClientPoolRequest.Url, LResponseContent);
          end
          else LResponseContent.LoadFromFile(LNetHttpClientPoolRequest.Url);
        end;

        //fire the OnSuccess
        if assigned(LNetHttpClientPoolRequest.OnSuccessCallBackObjProc) then
          LNetHttpClientPoolRequest.OnSuccessCallBackObjProc(LHTTPResponse, LResponseContent, LNetHttpClientPoolRequest.FExtData)
        else if assigned(LNetHttpClientPoolRequest.OnSuccessCallBackRefProc) then
          LNetHttpClientPoolRequest.OnSuccessCallBackRefProc(LHTTPResponse, LResponseContent, LNetHttpClientPoolRequest.FExtData);

      finally
        LHTTPResponse := nil;
        ALFreeandNil(LResponseContent);
      end;

    end;

  except
    on E: exception do begin
      if assigned(LNetHttpClientPoolRequest.OnErrorCallBackObjProc) then
        LNetHttpClientPoolRequest.OnErrorCallBackObjProc(E.Message, LNetHttpClientPoolRequest.FExtData)
      else if assigned(LNetHttpClientPoolRequest.OnErrorCallBackRefProc) then
        LNetHttpClientPoolRequest.OnErrorCallBackRefProc(E.Message, LNetHttpClientPoolRequest.FExtData);
    end;
  end;

end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: Int64;
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      AExtData,
      AUseCache), // const AExtData: Tobject;
    APriority, // const APriority: Int64;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: TALWorkerThreadGetPriorityFunc;
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      AExtData,
      AUseCache), // const AExtData: Tobject;
    APriority, // const APriority: TALWorkerThreadGetPriorityFunc;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      AExtData,
      true{AUseCache}), // const AExtData: Tobject;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      nil{AExtData},
      true{AUseCache}), // const AExtData: Tobject;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: Int64;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
    AExtData, // const AExtData: Tobject; // ExtData will be free by the worker thread
    AUseCache, // const AUseCache: Boolean;
    APriority, // const APriority: Int64;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: TALWorkerThreadGetPriorityFunc;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
    AExtData, // const AExtData: Tobject; // ExtData will be free by the worker thread
    AUseCache, // const AUseCache: Boolean;
    APriority, // const APriority: TALWorkerThreadGetPriorityFunc;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
    AExtData, // const AExtData: Tobject; // ExtData will be free by the worker thread
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartRefFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessRefProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorRefProc;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: Int64;
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      AExtData,
      AUseCache), // const AExtData: Tobject;
    APriority, // const APriority: Int64;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: TALWorkerThreadGetPriorityFunc;
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      AExtData,
      AUseCache), // const AExtData: Tobject;
    APriority, // const APriority: TALWorkerThreadGetPriorityFunc;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      AExtData,
      true{AUseCache}), // const AExtData: Tobject;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AAsync: Boolean = True);
begin
  ExecuteProc(
    DoGet, // const AProc: TALWorkerThreadProc;
    TALNetHttpClientPoolRequest.Create(
      AUrl,
      ACanStartCallBack,
      AOnSuccessCallBack,
      AOnErrorCallBack,
      nil{AExtData},
      true{AUseCache}), // const AExtData: Tobject;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: Int64;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
    AExtData, // const AExtData: Tobject; // ExtData will be free by the worker thread
    AUseCache, // const AUseCache: Boolean;
    APriority, // const APriority: Int64;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: TALWorkerThreadGetPriorityFunc;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
    AExtData, // const AExtData: Tobject; // ExtData will be free by the worker thread
    AUseCache, // const AUseCache: Boolean;
    APriority, // const APriority: TALWorkerThreadGetPriorityFunc;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
    AExtData, // const AExtData: Tobject; // ExtData will be free by the worker thread
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartObjFunc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessObjProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorObjProc;
    AAsync); // const AAsync: Boolean = True
end;

initialization
  TALNetHttpClientPool.FInstance := nil;
  TALNetHttpClientPool.CreateInstanceFunc := @TALNetHttpClientPool.CreateInstance;

finalization
  ALFreeAndNil(TALNetHttpClientPool.FInstance);

end.
