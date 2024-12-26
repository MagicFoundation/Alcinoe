unit Alcinoe.HTTP.Client.Net.Pool;

interface

{$I Alcinoe.inc}

uses
  System.classes,
  System.Net.HttpClient,
  System.Net.URLClient,
  Alcinoe.Common;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALNetHttpClientPoolCanStartProc = reference to function (var AExtData: Tobject): boolean;
  TALNetHttpClientPoolOnSuccessProc = reference to procedure (const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AExtData: TObject);
  TALNetHttpClientPoolOnErrorProc = reference to procedure (const AErrMessage: string; var AExtData: Tobject);
  TALNetHttpClientPoolCacheDataProc = reference to procedure(const aUrl: String; const AHTTPResponse: IHTTPResponse; const aData: TMemoryStream);
  TALNetHttpClientPoolRetrieveCachedDataProc = reference to function(const aUrl: String; out AHTTPResponse: IHTTPResponse; const aData: TMemoryStream): boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALNetHttpClientPoolRequest = Class(Tobject)
  private
    FUrl: String;
    FCanStartCallBack: TALNetHttpClientPoolCanStartProc;
    FOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
    FOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
    FExtData: Tobject;
    FUseCache: Boolean;
  public
    constructor Create(
                  const AUrl: String;
                  const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
                  const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
                  const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
                  const AExtData: Tobject;
                  const AUseCache: Boolean);
    destructor Destroy; override;
    Property Url: String read FUrl;
    Property CanStartCallBack: TALNetHttpClientPoolCanStartProc read FCanStartCallBack;
    Property OnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc read FOnSuccessCallBack;
    Property OnErrorCallBack: TALNetHttpClientPoolOnErrorProc read FOnErrorCallBack;
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
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartProc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: Int64;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartProc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: TALWorkerThreadGetPriorityFunc; // [MultiThread]
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartProc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const ACanStartCallBack: TALNetHttpClientPoolCanStartProc; // [MultiThread]
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc; // [MultiThread]
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: Int64;
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AUseCache: Boolean;
                const APriority: TALWorkerThreadGetPriorityFunc; // [MultiThread]
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc; // [MultiThread]
                const AExtData: Tobject; // ExtData will be free by the worker thread
                const AAsync: Boolean = True); overload;
    procedure Get(
                const AUrl: String;
                const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc; // [MultiThread]
                const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc; // [MultiThread]
                const AAsync: Boolean = True); overload;
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
              const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
              const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
              const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
              const AExtData: Tobject;
              const AUseCache: Boolean);
begin
  inherited create;
  FUrl := AUrl;
  FCanStartCallBack := ACanStartCallBack;
  FOnSuccessCallBack := AOnSuccessCallBack;
  FOnErrorCallBack := AOnErrorCallBack;
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
    if (not assigned(LNetHttpClientPoolRequest.CanStartCallBack)) or
       (LNetHttpClientPoolRequest.CanStartCallBack(LNetHttpClientPoolRequest.FExtData)) then begin

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
                // Client error responses (400 � 499)
                // Server error responses (500 � 599)
                if (LHTTPResponse = nil) or
                   ((LHTTPResponse.StatusCode >= 400) and (LHTTPResponse.StatusCode <= 599)) then begin
                  if assigned(LNetHttpClientPoolRequest.OnErrorCallBack) then
                    LNetHttpClientPoolRequest.OnErrorCallBack('HTTP request failed', LNetHttpClientPoolRequest.FExtData);
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
        LNetHttpClientPoolRequest.OnSuccessCallBack(LHTTPResponse, LResponseContent, LNetHttpClientPoolRequest.FExtData);

      finally
        LHTTPResponse := nil;
        ALFreeandNil(LResponseContent);
      end;

    end;

  except
    on E: exception do begin
      if assigned(LNetHttpClientPoolRequest.OnErrorCallBack) then
        LNetHttpClientPoolRequest.OnErrorCallBack(E.Message, LNetHttpClientPoolRequest.FExtData);
    end;
  end;

end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
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
            const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
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
            const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
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
            const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
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
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: Int64;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
    AExtData, // const AExtData: Tobject; // ExtData will be free by the worker thread
    AUseCache, // const AUseCache: Boolean;
    APriority, // const APriority: Int64;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
            const AUseCache: Boolean;
            const APriority: TALWorkerThreadGetPriorityFunc;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
    AExtData, // const AExtData: Tobject; // ExtData will be free by the worker thread
    AUseCache, // const AUseCache: Boolean;
    APriority, // const APriority: TALWorkerThreadGetPriorityFunc;
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
            const AExtData: Tobject; // ExtData will be free by the worker thread
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
    AExtData, // const AExtData: Tobject; // ExtData will be free by the worker thread
    AAsync); // const AAsync: Boolean = True
end;

{*********************************}
procedure TALNetHttpClientPool.Get(
            const AUrl: String;
            const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
            const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
            const AAsync: Boolean = True);
begin
  Get(
    AUrl, // const AUrl: String;
    nil, // const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
    AOnSuccessCallBack, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
    AOnErrorCallBack, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
    AAsync); // const AAsync: Boolean = True
end;

initialization
  TALNetHttpClientPool.FInstance := nil;
  TALNetHttpClientPool.CreateInstanceFunc := @TALNetHttpClientPool.CreateInstance;

finalization
  ALFreeAndNil(TALNetHttpClientPool.FInstance);

end.
