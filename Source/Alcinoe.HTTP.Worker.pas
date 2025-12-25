unit Alcinoe.HTTP.Worker;

interface

{$I Alcinoe.inc}

uses
  System.SysUtils,
  System.Messaging,
  System.Classes,
  System.Net.URLClient,
  {$IF defined(IOS)}
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  Alcinoe.StringList,
  {$ENDIF}
  Alcinoe.HTTP.Client;

Type

  {****************************}
  TALHttpWorker = Class(TObject)
  private
    class function CreateInstance: TALHttpWorker;
    class function GetInstance: TALHttpWorker; static;
  protected
    class var FInstance: TALHttpWorker;
  public
    type
      TCreateInstanceFunc = function: TALHttpWorker;
    class var CreateInstanceFunc: TCreateInstanceFunc;
    class property Instance: TALHttpWorker read GetInstance;
    class function HasInstance: Boolean; inline;
  private

    {$REGION 'IOS'}
    {$IF defined(IOS)}
    const
      KEY_BACKGROUND_SESSION_IDENTIFIER = 'ALHttpWorker';
      KEY_RESPONSE_BODY_FILE_PATH = 'response_body_file_path';
      KEY_REQUEST_ID = 'request_id';
      KEY_REQUEST_BODY_FILE_PATH = 'request_body_file_path';
      KEY_DELETE_REQUEST_BODY_FILE = 'delete_request_body_file';
      KEY_ENQUEUE_TIME = 'enqueue_time';
      KEY_BACKOFF_ATTEMPTS = 'backoff_attempts';
    {$ENDIF}
    {$ENDREGION}

  private

    {$REGION 'IOS'}
    {$IF defined(IOS)}
    type
      // -------------------
      // TURLSessionDelegate
      TURLSessionDelegate = class(TOCLocal, NSURLSessionDelegate, NSURLSessionTaskDelegate, NSURLSessionDataDelegate, NSURLSessionDownloadDelegate)
      public
        // NSURLSessionDelegate
        procedure URLSession(session: NSURLSession; didReceiveChallenge: NSURLAuthenticationChallenge; completionHandler: Pointer); overload; cdecl;
        procedure URLSession(session: NSURLSession; didBecomeInvalidWithError: NSError); overload; cdecl;
        procedure URLSessionDidFinishEventsForBackgroundURLSession(session: NSURLSession); cdecl;
        // NSURLSessionTaskDelegate
        procedure URLSession(session: NSURLSession; task: NSURLSessionTask; needNewBodyStreamFromOffset: Int64; completionHandler: Pointer); overload; cdecl;
        procedure URLSession(session: NSURLSession; task: NSURLSessionTask; didSendBodyData: Int64; totalBytesSent: Int64; totalBytesExpectedToSend: Int64); overload; cdecl;
        procedure URLSession(session: NSURLSession; task: NSURLSessionTask; didReceiveInformationalResponse: NSHTTPURLResponse); overload; cdecl;
        procedure URLSession(session: NSURLSession; task: NSURLSessionTask; didFinishCollectingMetrics: NSURLSessionTaskMetrics); overload; cdecl;
        procedure URLSession(session: NSURLSession; task: NSURLSessionTask; didCompleteWithError: NSError); overload; cdecl;
        procedure URLSession(session: NSURLSession; task: NSURLSessionTask; needNewBodyStream: Pointer); overload; cdecl;
        procedure URLSession(session: NSURLSession; task: NSURLSessionTask; willBeginDelayedRequest: NSURLRequest; completionHandler: Pointer); overload; cdecl;
        procedure URLSession(session: NSURLSession; didCreateTask: NSURLSessionTask); overload; cdecl;
        procedure URLSession(session: NSURLSession; task: NSURLSessionTask; willPerformHTTPRedirection: NSHTTPURLResponse; newRequest: NSURLRequest; completionHandler: Pointer); overload; cdecl;
        procedure URLSession(session: NSURLSession; task: NSURLSessionTask; didReceiveChallenge: NSURLAuthenticationChallenge; completionHandler: Pointer); overload; cdecl;
        [MethodName('URLSession:taskIsWaitingForConnectivity:')]
        procedure URLSessionTaskIsWaitingForConnectivity(session: NSURLSession; taskIsWaitingForConnectivity: NSURLSessionTask); cdecl;
        // NSURLSessionDataDelegate
        procedure URLSession(session: NSURLSession; dataTask: NSURLSessionDataTask; willCacheResponse: NSCachedURLResponse; completionHandler: Pointer); overload; cdecl;
        procedure URLSession(session: NSURLSession; dataTask: NSURLSessionDataTask; didReceiveData: NSData); overload; cdecl;
        procedure URLSession(session: NSURLSession; dataTask: NSURLSessionDataTask; didBecomeStreamTask: NSURLSessionStreamTask); overload; cdecl;
        procedure URLSession(session: NSURLSession; dataTask: NSURLSessionDataTask; didBecomeDownloadTask: NSURLSessionDownloadTask); overload; cdecl;
        procedure URLSession(session: NSURLSession; dataTask: NSURLSessionDataTask; didReceiveResponse: NSURLResponse; completionHandler: Pointer); overload; cdecl;
        // NSURLSessionDownloadDelegate
        procedure URLSession(session: NSURLSession; downloadTask: NSURLSessionDownloadTask; didResumeAtOffset: Int64; expectedTotalBytes: Int64); overload; cdecl;
        procedure URLSession(session: NSURLSession; downloadTask: NSURLSessionDownloadTask; didWriteData: Int64; totalBytesWritten: Int64; totalBytesExpectedToWrite: Int64); overload; cdecl;
        procedure URLSession(session: NSURLSession; downloadTask: NSURLSessionDownloadTask; didFinishDownloadingToURL: NSURL); overload; cdecl;
      end;
    {$ENDIF}
    {$ENDREGION}

  public
    const
      // !! This value is also hardcoded in ALHttpWorker.java !!
      REQUEST_EXPIRATION_DAYS = 7;
  public
    type
      // -----------
      // TWorkResult
      TWorkResult = (Success, Failure, Canceled);
      // ------------------
      // TWorkResultMessage
      TWorkResultMessage  = class(TMessage<TWorkResult>)
      private
        FRequestId: String;
        FResponse: TALHttpClientResponseW;
      public
        constructor Create(const AResult: TWorkResult; const ARequestId: String; AResponse: TALHttpClientResponseW); virtual;
        destructor Destroy; override;
        property RequestId: String read FRequestId;
        property Response: TALHttpClientResponseW read FResponse;
      end;
  private
    FDefaultUserAgent: String;
  private

    {$REGION 'ANDROID'}
    {$IF defined(ANDROID)}
    class procedure BroadcastReceivedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
    {$ENDIF}
    {$ENDREGION}

    {$REGION 'IOS'}
    {$IF defined(IOS)}
    FHandleEventsForBackgroundURLSessionCompletionHandlerImpl: Pointer;
    FURLSession: NSURLSession;
    FURLSessionDelegate: TURLSessionDelegate;
    class var FRequestIdsToCancel: TALStringListW;
    class procedure BackgroundNetworkTaskMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
    procedure getAllTasksCompletionHandler(tasks: NSArray);
    {$ENDIF}
    {$ENDREGION}

  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function GetTempFilename(Const AExt: String = '.tmp'): String;
    property DefaultUserAgent: String read FDefaultUserAgent write FDefaultUserAgent;
    function Enqueue(
               const AUrl: String;
               const AMethod: String;
               const ABodyString: String;
               const AHeaders: TNetHeaders): String; overload; virtual;
    function Enqueue(
               const AUrl: String;
               const AMethod: String;
               const ABodyFilePath: String;
               const ADeleteBodyFile: Boolean;
               const AHeaders: TNetHeaders): String; overload; virtual;
    function Enqueue(
               const AUrl: String;
               const AMethod: String;
               const ABodyBytes: TBytes;
               const AHeaders: TNetHeaders): String; overload; virtual;
    function Enqueue(
               const AUrl: String;
               const AMethod: String;
               const ABodyStream: Tstream;
               const AHeaders: TNetHeaders): String; overload; virtual;
    procedure Cancel(const ARequestID: String); virtual;
  End;

implementation

uses
  System.DateUtils,
  system.IOUtils,
  {$IF defined(ANDROID)}
  AndroidApi.JNI.JavaTypes,
  Androidapi.Helpers,
  Alcinoe.AndroidApi.AndroidX,
  Alcinoe.BroadcastReceiver,
  {$ENDIF}
  {$IF defined(IOS)}
  System.Math,
  Macapi.ObjCRuntime,
  Macapi.Helpers,
  FMX.Platform,
  {$ENDIF}
  {$IF defined(MSWindows) or defined(ALMacOS)}
  system.net.HttpClient,
  Alcinoe.HTTP.Client.Net,
  {$ENDIF}
  Alcinoe.Files,
  Alcinoe.StringUtils,
  Alcinoe.HTTP,
  Alcinoe.Common;

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; didReceiveChallenge: NSURLAuthenticationChallenge; completionHandler: Pointer);
var
  // No idea why ignored parameter is needed, but removing it makes everything crash.
  // https://stackoverflow.com/questions/79827070/how-to-correctly-call-an-ios-completionhandler-block-from-delphi-without-crashin?noredirect=1#comment140867316_79827070
  // (void (NS_SWIFT_SENDABLE ^)(NSURLSessionAuthChallengeDisposition disposition, NSURLCredential * _Nullable credential))completionHandler
  LCompletionHandlerImpl: procedure(disposition: NSURLSessionAuthChallengeDisposition; ignored: pointer; credential: Pointer{NSURLCredential}); cdecl;
begin

  //
  // Requests credentials from the delegate in response to an authentication
  // request from the remote server.
  //
  // This method handles task-level authentication challenges. The
  // NSURLSessionDelegate protocol also provides a session-level authentication
  // delegate method. The method called depends on the type of authentication challenge:
  //
  //  * For session-level challenges—NSURLAuthenticationMethodNTLM, NSURLAuthenticationMethodNegotiate,
  //    NSURLAuthenticationMethodClientCertificate, or NSURLAuthenticationMethodServerTrust—the
  //    NSURLSession object calls the session delegate’s URLSession:didReceiveChallenge:completionHandler:
  //    method. If your app does not provide a session delegate method, the NSURLSession object
  //    calls the task delegate’s URLSession:task:didReceiveChallenge:completionHandler: method
  //    to handle the challenge.
  //
  //  * For non-session-level challenges (all others), the NSURLSession object calls the session
  //    delegate’s URLSession:task:didReceiveChallenge:completionHandler: method to handle the challenge.
  //    If your app provides a session delegate and you need to handle authentication, then you must
  //    either handle the authentication at the task level or provide a task-level handler that
  //    calls the per-session handler explicitly. The session delegate’s
  //    URLSession:didReceiveChallenge:completionHandler: method is not called for non-session-level
  //    challenges.
  //

  {$IF defined(DEBUG)}
  ALLog(
    Classname + '.URLSession:session:didReceiveChallenge:completionHandler | '+
    'host='+NSStrToStr(didReceiveChallenge.protectionSpace.host)+' | '+
    'method='+NSStrToStr(didReceiveChallenge.protectionSpace.authenticationMethod)+' | '+
    'prevFail='+ALIntToStrW(didReceiveChallenge.previousFailureCount)+'');
  {$ENDIF}

  @LCompletionHandlerImpl := imp_implementationWithBlock(completionHandler);
  try
    LCompletionHandlerImpl(NSURLSessionAuthChallengePerformDefaultHandling{disposition}, nil{ignored}, nil{credential});
  finally
    imp_removeBlock(@LCompletionHandlerImpl);
  end;

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; didBecomeInvalidWithError: NSError);
begin

  //
  // Tells the URL session that the session has been invalidated.
  //
  // If you invalidate a session by calling its finishTasksAndInvalidate method,
  // the session waits until after the final task in the session finishes or
  // fails before calling this delegate method. If you call the
  // invalidateAndCancel method, the session calls this delegate method
  // immediately.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:didBecomeInvalidWithError');
  {$ENDIF}

  var LURLSession := session;
  var LURLSessionDelegate := Self;

  TThread.Queue(nil,
    procedure
    begin
      LUrlSession.release;
      ALFreeAndNil(LURLSessionDelegate);
    end)

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSessionDidFinishEventsForBackgroundURLSession(session: NSURLSession);
var
  // (void (^)()) completionHandler
  LCompletionHandlerImpl: procedure(); cdecl;
begin

  //
  // Tells the delegate that all messages enqueued for a session have been delivered.
  //
  // In iOS, when a background transfer completes or requires credentials, if your
  // app is no longer running, your app is automatically relaunched in the
  // background, and the app’s UIApplicationDelegate is sent an
  // application:handleEventsForBackgroundURLSession:completionHandler: message.
  // This call contains the identifier of the session that caused your app to be
  // launched. You should then store that completion handler before creating a
  // background configuration object with the same identifier, and creating a
  // session with that configuration. The newly created session is automatically
  // reassociated with ongoing background activity.
  //
  // When your app later receives a URLSessionDidFinishEventsForBackgroundURLSession:
  // message, this indicates that all messages previously enqueued for this session
  // have been delivered, and that it is now safe to invoke the previously stored
  // completion handler or to begin any internal updates that may result in
  // invoking the completion handler.
  //
  // Important: Because the provided completion handler is part of UIKit, you
  // must call it on your main thread.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSessionDidFinishEventsForBackgroundURLSession:session');
  {$ENDIF}

  TThread.Queue(nil,
    procedure
    begin
      if not TALHttpWorker.HasInstance then exit;
      if TALHttpWorker.Instance.FHandleEventsForBackgroundURLSessionCompletionHandlerImpl <> nil then begin
        @LCompletionHandlerImpl := TALHttpWorker.Instance.FHandleEventsForBackgroundURLSessionCompletionHandlerImpl;
        try
          LCompletionHandlerImpl();
        finally
          imp_removeBlock(@LCompletionHandlerImpl);
          TALHttpWorker.Instance.FHandleEventsForBackgroundURLSessionCompletionHandlerImpl := nil;
        end;
      end;
    end);

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; task: NSURLSessionTask; needNewBodyStreamFromOffset: Int64; completionHandler: Pointer);
var
  // (void (^)(NSInputStream * bodyStream)) completionHandler
  LCompletionHandlerImpl: procedure(bodyStream: Pointer{NSInputStream}); cdecl;
begin

  //
  // Tells the delegate when a task requires a new request body stream to send to the remote server.
  //
  // The task calls this delegate method under two circumstances:
  //
  // * To provide the initial request body stream if the task was created with
  //   uploadTaskWithStreamedRequest:
  //
  // * To provide a replacement request body stream if the task needs to resend a
  //   request that has a body stream because of an authentication challenge or
  //   other recoverable server error.
  //
  // Note: You don’t need to implement this method if your code provides the request body
  // using a file URL or a data object.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:task:needNewBodyStreamFromOffset:completionHandler');
  {$ENDIF}

  // NOTE: Not tested, as we do not use uploadTaskWithStreamedRequest.
  @LCompletionHandlerImpl := imp_implementationWithBlock(completionHandler);
  try
    LCompletionHandlerImpl(nil{bodyStream});
  finally
    imp_removeBlock(@LCompletionHandlerImpl);
  end;

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; task: NSURLSessionTask; didSendBodyData: Int64; totalBytesSent: Int64; totalBytesExpectedToSend: Int64);
begin

  //
  // Periodically informs the delegate of the progress of sending body content to the server.
  //
  // The totalBytesSent and totalBytesExpectedToSend parameters are also available as
  // NSURLSessionTask properties countOfBytesSent and countOfBytesExpectedToSend. Or,
  // since NSURLSessionTask supports NSProgressReporting, you can use the task’s progress
  // property instead, which may be more convenient.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:task:didSendBodyData:totalBytesSent:totalBytesExpectedToSend');
  {$ENDIF}

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; task: NSURLSessionTask; didReceiveInformationalResponse: NSHTTPURLResponse);
begin

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:task:didReceiveInformationalResponse');
  {$ENDIF}

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; task: NSURLSessionTask; didFinishCollectingMetrics: NSURLSessionTaskMetrics);
begin

  //
  // Tells the delegate that the session finished
  // collecting metrics for the task.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:task:didFinishCollectingMetrics');
  {$ENDIF}

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; task: NSURLSessionTask; didCompleteWithError: NSError);
begin

  //
  // Tells the delegate that the task finished transferring data.
  //
  // The only errors your delegate receives through the error parameter are
  // client-side errors, such as being unable to resolve the hostname or
  // connect to the host. To check for server-side errors, inspect the
  // response property of the task parameter received by this callback.
  //

  {$IF defined(DEBUG)}
  if (didCompleteWithError <> nil) and (didCompleteWithError.code <> 0) then
    allog(
      Classname + '.URLSession:session:task:didCompleteWithError',
      'ErrorCode: ' + ALIntToStrW(didCompleteWithError.code) + ' | ' +
      'ErrorMsg: ' + NSStrToStr(didCompleteWithError.localizedDescription),
      TalLogType.WARN)
  else
    allog(Classname + '.URLSession:session:task:didCompleteWithError');
  {$ENDIF}

  var LCanceled: Boolean := (didCompleteWithError <> nil) and (didCompleteWithError.code = NSURLErrorCancelled);
  var LRequestID: String := '';
  var LResponseBodyfilePath: String := '';
  var LRequestBodyFilePath: String := '';
  var LDeleteRequestBodyFile: Boolean := False;
  var LResponse: TALHttpClientResponseW := nil;
  Try

    var LLst := TALStringListW.create;
    Try

      LLst.LineBreak := ';';
      LLst.TrailingLineBreak := False;
      LLst.Text := NSStrToStr(task.taskDescription);

      LRequestId := LLst.Values[TALHttpWorker.KEY_REQUEST_ID];
      LResponseBodyfilePath := LLst.Values[TALHttpWorker.KEY_RESPONSE_BODY_FILE_PATH];
      LRequestBodyFilePath := LLst.Values[TALHttpWorker.KEY_REQUEST_BODY_FILE_PATH];
      LDeleteRequestBodyFile := ALStrToBool(LLst.Values[TALHttpWorker.KEY_DELETE_REQUEST_BODY_FILE]);

      var LHTTPURLResponse: NSHTTPURLResponse;
      if ((didCompleteWithError = nil) or (didCompleteWithError.code = 0)) and (task.Response <> nil) then
        LHTTPURLResponse := TNSHTTPURLResponse.Wrap(NSObjectToID(task.Response))
      else
        LHTTPURLResponse := nil;

      // IOException -> retry
      // Server-side error (5xx) -> retry
      if (not LCanceled) and
         ((LHTTPURLResponse = nil) or ((LHTTPURLResponse.statusCode >= 500) and (LHTTPURLResponse.statusCode < 600))) then begin

        // If task.originalRequest = nil => non-retriable error
        if task.originalRequest = nil then raise Exception.Create('Error 1B60A279-AE73-48F9-B09F-74629596CE04');

        // More than 7 days => non-retriable error
        If DaysBetween(ALUTCNow, ALUnixMsToDateTime(ALStrToInt64Def(LLst.Values[TALHttpWorker.KEY_ENQUEUE_TIME], 0))) > REQUEST_EXPIRATION_DAYS
          then raise Exception.Create('Request expired: it has been pending for more than 7 days and will not be retried');

        var LNewTask: NSURLSessionTask := nil;
        if task.isKindOfClass(objc_getClass('NSURLSessionDownloadTask')) then LNewTask := session.downloadTaskWithRequest(task.originalRequest)
        else if task.isKindOfClass(objc_getClass('NSURLSessionUploadTask')) then begin
          if (LRequestBodyFilePath = '') or (not TFile.Exists(LRequestBodyFilePath)) then
            raise Exception.Create('Cannot recreate the upload task because the request body file is missing');
          var LFileURL := TNSURL.OCClass.fileURLWithPath(StrToNSStr(LRequestBodyFilePath));
          LNewTask := session.uploadTaskWithRequest(task.originalRequest, LFileURL);
        end
        else Raise Exception.Create('Error 6AC28368-C4F8-4317-9C19-C91D1986B94D');

        // Taken from JobInfo.BACKOFF_POLICY_EXPONENTIAL
        // public static final long MIN_BACKOFF_MILLIS = 10 * 1000; // 10 seconds.
        // public static final long MAX_BACKOFF_MILLIS = 5 * 60 * 60 * 1000; // 5 hours.
        // public static final long DEFAULT_BACKOFF_DELAY_MILLIS = 30000L;
        // delayMillis = (long) Math.scalb(backoff, backoffAttempts - 1);
        var LBackoffAttempts: Integer := ALStrToIntDef(LLst.Values[TALHttpWorker.KEY_BACKOFF_ATTEMPTS], 0);
        Var LDelay: int64 := 30_000 * Round(Power(2.0, LBackoffAttempts));
        LDelay := Min(LDelay, 5 * 60 * 60 * 1000); // 5 hours
        var LBeginDate := TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSinceNow(LDelay / 1000));
        LNewTask.setEarliestBeginDate(LBeginDate);

        LLst.Values[TALHttpWorker.KEY_BACKOFF_ATTEMPTS] := ALIntToStrW(LBackoffAttempts + 1);
        LLst.Values[TALHttpWorker.KEY_RESPONSE_BODY_FILE_PATH] := '';
        LNewTask.setTaskDescription(StrToNSStr(LLst.Text));

        {$IF defined(DEBUG)}
        ALLog(
          ClassName + '.URLSession:session:task:didCompleteWithError',
          'Scheduling retry #' + ALIntToStrW(LBackoffAttempts) + ' in ' + ALIntToStrW(LDelay div 1000) + 's',
          TalLogType.INFO);
        {$ENDIF}

        LNewTask.resume;

        If (LResponseBodyfilePath <> '') and Tfile.Exists(LResponseBodyfilePath) then
          Tfile.Delete(LResponseBodyfilePath);

        exit;

      end;

      // Successful HTTP 2xx, 4xx, etc.
      LResponse := TALHttpClientResponseW.Create;
      if LHTTPURLResponse <> nil then begin
        LResponse.StatusCode := LHTTPURLResponse.statusCode;
        var LAllHeaderFields := LHTTPURLResponse.allHeaderFields;
        if LAllHeaderFields <> nil then begin
          var LAllKeys := LAllHeaderFields.allKeys;
          var LAllValues := LAllHeaderFields.allValues;
          for var I := 0 to LAllKeys.count - 1 do begin
            var LName := NSStrToStr(TNSString.Wrap(LAllKeys.objectAtIndex(I)));
            var LValue := NSStrToStr(TNSString.Wrap(LAllValues.objectAtIndex(I)));
            LResponse.Headers.Values[LName] := LValue;
          end;
        end;
      end;

      if (LResponseBodyfilePath <> '') and TFile.Exists(LResponseBodyfilePath) then
        LResponse.BodyStream := TFileStream.Create(LResponseBodyfilePath, FmOpenRead or fmShareDenyWrite); // /.nofollow/private/var/mobile/Containers/Data/Application/99F12BF0-0410-456C-B242-5F6198735633/Library/Caches/com.apple.nsurlsessiond/Downloads/ALFmxHttpWorkerDemo/CFNetworkDownload_ml1euK.tmp

      if LDeleteRequestBodyFile and (LRequestBodyfilePath <> '') and Tfile.Exists(LRequestBodyfilePath) then
        Tfile.Delete(LRequestBodyfilePath);

      TThread.Queue(nil,
        Procedure
        Begin
          try
            try
              var LWorkResult: TWorkResult;
              If (LResponse.StatusCode >= 200) and (LResponse.StatusCode < 300) then LWorkResult := TWorkResult.Success
              else if LCanceled then LWorkResult := TWorkResult.Canceled
              else LWorkResult := TWorkResult.Failure;
              TMessageManager.DefaultManager.SendMessage(
                TALHttpWorker.Instance, // const Sender: TObject
                TWorkResultMessage.Create(LWorkResult, LRequestID, LResponse), // AMessage: TMessage
                True); // ADispose: Boolean
            finally
              If (LResponseBodyfilePath <> '') and Tfile.Exists(LResponseBodyfilePath) then
                Tfile.Delete(LResponseBodyfilePath);
            end;
          Except
            On E: Exception do
              ALLog('TALHttpWorker', E);
          end;
        end);

    finally
      ALFreeAndNil(LLst);
    end;

  except
    on E: Exception do begin
      ALLog('TALHttpWorker', E);
      ALFreeAndNil(LResponse);
      If (LResponseBodyfilePath <> '') and Tfile.Exists(LResponseBodyfilePath) then
        Tfile.Delete(LResponseBodyfilePath);
      if LDeleteRequestBodyFile and (LRequestBodyfilePath <> '') and Tfile.Exists(LRequestBodyfilePath) then
        Tfile.Delete(LRequestBodyfilePath);
      TThread.Queue(nil,
        Procedure
        Begin
          try
            TMessageManager.DefaultManager.SendMessage(
              TALHttpWorker.Instance, // const Sender: TObject
              TWorkResultMessage.Create(TWorkResult.Failure, LRequestID, nil), // AMessage: TMessage
              True); // ADispose: Boolean
          Except
            On E: Exception do
              ALLog('TALHttpWorker', E);
          end;
        end);
    end;
  end;

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; task: NSURLSessionTask; needNewBodyStream: Pointer);
var
  // (void (^)(NSInputStream * bodyStream)) completionHandler
  LCompletionHandlerImpl: procedure(bodyStream: Pointer{NSInputStream}); cdecl;
begin

  //
  // Tells the delegate when a task requires a new request body stream to send to the remote server.
  //
  // The task calls this delegate method under two circumstances:
  //
  // * To provide the initial request body stream if the task was created with
  //    uploadTaskWithStreamedRequest:
  //
  // * To provide a replacement request body stream if the task needs to resend a
  //   request that has a body stream because of an authentication challenge or
  //   other recoverable server error.
  //
  // Note: You don’t need to implement this method if your code provides the
  // request body using a file URL or a data object.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:task:needNewBodyStream');
  {$ENDIF}

  // NOTE: Not tested, as we do not use uploadTaskWithStreamedRequest.
  @LCompletionHandlerImpl := imp_implementationWithBlock(needNewBodyStream);
  try
    LCompletionHandlerImpl(nil{bodyStream});
  finally
    imp_removeBlock(@LCompletionHandlerImpl);
  end;

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; task: NSURLSessionTask; willBeginDelayedRequest: NSURLRequest; completionHandler: Pointer);
var
  // No idea why ignored parameter is needed, but removing it makes everything crash.
  // https://stackoverflow.com/questions/79827070/how-to-correctly-call-an-ios-completionhandler-block-from-delphi-without-crashin?noredirect=1#comment140867316_79827070
  // (void (NS_SWIFT_SENDABLE ^)(NSURLSessionDelayedRequestDisposition disposition, NSURLRequest * _Nullable newRequest))completionHandler
  LCompletionHandlerImpl: procedure(disposition: NSURLSessionDelayedRequestDisposition; ignored: pointer; newRequest: pointer{NSURLRequest}); cdecl;
begin

  //
  // Tells the delegate that a delayed URL session task will now begin loading.
  //
  // This method is called when a background session task with a delayed start time (as set with the
  // earliestBeginDate property) is ready to start. This delegate method should only be
  // implemented if the request might become stale while waiting for the network load and
  // needs to be replaced by a new request.
  //
  // For loading to continue, the delegate must call the completion handler, passing in a
  // disposition that indicates how the task should proceed. Passing the NSURLSessionDelayedRequestCancel
  // disposition is equivalent to calling cancel on the task directly.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:task:willBeginDelayedRequest:completionHandler');
  {$ENDIF}

  @LCompletionHandlerImpl := imp_implementationWithBlock(completionHandler);
  try
    LCompletionHandlerImpl(NSURLSessionDelayedRequestContinueLoading{disposition}, nil{ignored}, nil{newRequest});
  finally
    imp_removeBlock(@LCompletionHandlerImpl);
  end;

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; didCreateTask: NSURLSessionTask);
begin

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:didCreateTask');
  {$ENDIF}

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; task: NSURLSessionTask; willPerformHTTPRedirection: NSHTTPURLResponse; newRequest: NSURLRequest; completionHandler: Pointer);
var
  // (void (^)(NSURLRequest * )) completionHandler
  LCompletionHandlerImpl: procedure(request: Pointer{NSURLRequest}); cdecl;
begin

  //
  // Tells the delegate that the remote server requested an HTTP redirect.
  //
  // This method is called only for tasks in default and ephemeral sessions.
  // Tasks in background sessions automatically follow redirects.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:task:willPerformHTTPRedirection:newRequest:completionHandler');
  {$ENDIF}

  // NOTE: Not tested, as we only use background sessions.
  @LCompletionHandlerImpl := imp_implementationWithBlock(completionHandler);
  try
    LCompletionHandlerImpl(NSObjectToID(newRequest));
  finally
    imp_removeBlock(@LCompletionHandlerImpl);
  end;

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; task: NSURLSessionTask; didReceiveChallenge: NSURLAuthenticationChallenge; completionHandler: Pointer);
var
  // No idea why ignored parameter is needed, but removing it makes everything crash.
  // https://stackoverflow.com/questions/79827070/how-to-correctly-call-an-ios-completionhandler-block-from-delphi-without-crashin?noredirect=1#comment140867316_79827070
  // (void (^)(NSURLSessionAuthChallengeDisposition disposition, NSURLCredential * credential)) completionHandler
  LCompletionHandlerImpl: procedure(disposition: NSURLSessionAuthChallengeDisposition; ignored: pointer; credential: Pointer{NSURLCredential}); cdecl;
begin

  //
  // Requests credentials from the delegate in response to an authentication request from the remote server.
  //
  // This method handles task-level authentication challenges. The NSURLSessionDelegate protocol
  // also provides a session-level authentication delegate method. The method called depends on the
  // type of authentication challenge:
  //
  // * For session-level challenges—NSURLAuthenticationMethodNTLM, NSURLAuthenticationMethodNegotiate,
  //   NSURLAuthenticationMethodClientCertificate, or NSURLAuthenticationMethodServerTrust—the NSURLSession
  //   object calls the session delegate’s URLSession:didReceiveChallenge:completionHandler: method. If
  //   your app does not provide a session delegate method, the NSURLSession object calls the task
  //   delegate’s URLSession:task:didReceiveChallenge:completionHandler: method to handle the challenge.
  //
  // * For non-session-level challenges (all others), the NSURLSession object calls the session delegate’s
  //   URLSession:task:didReceiveChallenge:completionHandler: method to handle the challenge. If your app
  //   provides a session delegate and you need to handle authentication, then you must either handle
  //   the authentication at the task level or provide a task-level handler that calls the per-session
  //   handler explicitly. The session delegate’s URLSession:didReceiveChallenge:completionHandler: method
  //   is not called for non-session-level challenges.
  //

  {$IF defined(DEBUG)}
  ALLog(
    Classname + '.URLSession:session:task:didReceiveChallenge:completionHandler | '+
    'host='+NSStrToStr(didReceiveChallenge.protectionSpace.host)+' | '+
    'method='+NSStrToStr(didReceiveChallenge.protectionSpace.authenticationMethod)+' | '+
    'prevFail='+ALIntToStrW(didReceiveChallenge.previousFailureCount)+'');
  {$ENDIF}

  @LCompletionHandlerImpl := imp_implementationWithBlock(completionHandler);
  try
    LCompletionHandlerImpl(NSURLSessionAuthChallengePerformDefaultHandling{disposition}, nil{ignored}, nil{credential});
  finally
    imp_removeBlock(@LCompletionHandlerImpl);
  end;

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSessionTaskIsWaitingForConnectivity(session: NSURLSession; taskIsWaitingForConnectivity: NSURLSessionTask);
begin

  //
  // Tells the delegate that the task is waiting until suitable connectivity
  // is available before beginning the network load.
  //
  // This method is called if the waitsForConnectivity property of
  // NSURLSessionConfiguration is true, and sufficient connectivity is
  // unavailable. The delegate can use this opportunity to update the user
  // interface; for example, by presenting an offline mode or a
  // cellular-only mode.
  //
  // This method is called, at most, once per task, and only if connectivity
  // is initially unavailable. It is never called for background sessions
  // because waitsForConnectivity is ignored for those sessions.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSessionTaskIsWaitingForConnectivity:session:taskIsWaitingForConnectivity');
  {$ENDIF}

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; dataTask: NSURLSessionDataTask; willCacheResponse: NSCachedURLResponse; completionHandler: Pointer);
var
  // (void (^)(NSCachedURLResponse * cachedResponse)) completionHandler
  LCompletionHandlerImpl: procedure(cachedResponse: pointer{NSCachedURLResponse}); cdecl;
begin

  //
  // Asks the delegate whether the data (or upload) task should store the response in the cache.
  //
  // The session calls this delegate method after the task finishes receiving all of the expected data.
  // If you don’t implement this method, the default behavior is to use the caching policy specified
  // in the session’s configuration object. The primary purpose of this method is to prevent caching
  // of specific URLs or to modify the userInfo dictionary associated with the URL response.
  //
  // This method is called only if the NSURLProtocol handling the request decides to cache the
  // response. As a rule, responses are cached only when all of the following are true:
  //
  // * The request is for an HTTP or HTTPS URL (or your own custom networking protocol that supports caching).
  // * The request was successful (with a status code in the 200–299 range).
  // * The provided response came from the server, rather than out of the cache.
  // * The session configuration’s cache policy allows caching.
  // * The provided URLRequest object’s cache policy (if applicable) allows caching.
  // * The cache-related headers in the server’s response (if present) allow caching.
  // * The response size is small enough to reasonably fit within the cache. (For example,
  //   if you provide a disk cache, the response must be no larger than about 5% of
  //   the disk cache size.)
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:dataTask:willCacheResponse:completionHandler');
  {$ENDIF}

  @LCompletionHandlerImpl := imp_implementationWithBlock(completionHandler);
  try
    // NULL to prevent caching the response
    LCompletionHandlerImpl(nil{cachedResponse});
  finally
    imp_removeBlock(@LCompletionHandlerImpl);
  end;

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; dataTask: NSURLSessionDataTask; didReceiveData: NSData);
begin

  //
  // Tells the delegate that the data task has received some of the expected data.
  //
  // Because the data object parameter is often pieced together from a number of
  // different data objects, whenever possible, use the enumerateByteRangesUsingBlock:
  // method to iterate through the data rather than using the bytes method (which
  // flattens the data object into a single memory block).
  //
  // This delegate method may be called more than once, and each call provides
  // only data received since the previous call. The app is responsible for
  // accumulating this data if needed.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:dataTask:didReceiveData');
  {$ENDIF}

  if (didReceiveData <> nil) and (didReceiveData.length > 0) then begin
    var LLst := TALStringListW.create;
    Try

      LLst.LineBreak := ';';
      LLst.TrailingLineBreak := False;
      LLst.Text := NSStrToStr(dataTask.taskDescription);

      var LFileStream: TFileStream;
      var LResponseBodyFilePath := LLst.Values[TALHttpWorker.KEY_RESPONSE_BODY_FILE_PATH];
      if LResponseBodyFilePath = '' then begin
        LResponseBodyFilePath := TALHttpWorker.GetTempFilename;
        LLst.Values[TALHttpWorker.KEY_RESPONSE_BODY_FILE_PATH] := LResponseBodyFilePath;
        dataTask.setTaskDescription(StrToNSStr(LLst.Text));
        LFileStream := TfileStream.Create(LResponseBodyFilePath, fmCreate or fmShareExclusive)
      end
      else
        LFileStream := TfileStream.Create(LResponseBodyFilePath, fmOpenReadWrite or fmShareDenyWrite);

      try

        var LBytes: TBytes;
        SetLength(LBytes, didReceiveData.length);
        didReceiveData.getBytes(@LBytes[0], length(LBytes));

        LFileStream.Position := LFileStream.Size;
        LFileStream.Size := LFileStream.Size + int64(didReceiveData.length);
        LFileStream.WriteBuffer(LBytes, Length(LBytes));

      finally
        ALFreeAndNil(LFileStream);
      end;

    Finally
      ALFreeAndNil(LLst);
    end;
  end;

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; dataTask: NSURLSessionDataTask; didBecomeStreamTask: NSURLSessionStreamTask);
begin

  //
  // Tells the delegate that the data task was changed to a stream task.
  //
  // When your URLSession:dataTask:didReceiveResponse:completionHandler: delegate
  // method uses the NSURLSessionResponseBecomeStream disposition to convert the
  // request to use a stream, the session calls this delegate method to provide
  // you with the new stream task. After this call, the session delegate receives
  // no further delegate method calls related to the original data task.
  //
  // For requests that were pipelined, the stream task allows only reading, and
  // the object immediately sends the delegate message URLSession:writeClosedForStreamTask:.
  // You can disable pipelining for all requests in a session by setting the
  // HTTPShouldUsePipelining property on its NSURLSessionConfiguration object, or
  // for individual requests by setting the HTTPShouldUsePipelining property on
  // an NSURLRequest object.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:dataTask:didBecomeStreamTask');
  {$ENDIF}

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; dataTask: NSURLSessionDataTask; didBecomeDownloadTask: NSURLSessionDownloadTask);
begin

  //
  // Tells the delegate that the data task was changed to a download task.
  //
  // When your URLSession:dataTask:didReceiveResponse:completionHandler: delegate
  // method uses the NSURLSessionResponseBecomeDownload disposition to convert the
  // request to use a download, the session calls this delegate method to provide
  // you with the new download task. After this call, the session delegate receives
  // no further delegate method calls related to the original data task.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:dataTask:didBecomeDownloadTask');
  {$ENDIF}

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; dataTask: NSURLSessionDataTask; didReceiveResponse: NSURLResponse; completionHandler: Pointer);
var
  // (void (^)(NSURLSessionResponseDisposition disposition)) completionHandler
  LCompletionHandlerImpl: procedure(param1: NSURLSessionResponseDisposition); cdecl;
begin

  //
  // Tells the delegate that the data task received the initial reply (headers) from the server.
  //
  // Implementing this method is optional unless you need to cancel the transfer or convert it
  // to a download task when the response headers are first received. If you don’t provide
  // this delegate method, the session always allows the task to continue.
  //
  // You also implement this method if you need to support the fairly obscure multipart/x-mixed-replace
  // content type. With that content type, the server sends a series of parts, each of which
  // is intended to replace the previous part. The session calls this method at the beginning
  // of each part, followed by one or more calls to URLSession:dataTask:didReceiveData: with
  // the contents of that part.
  //
  // Each time the URLSession:dataTask:didReceiveResponse:completionHandler: method is called for
  // a part, collect the data received for the previous part (if any) and process the data
  // as needed for your application. This processing can include storing the data to the
  // filesystem, parsing it into custom types, or displaying it to the user. Next, begin
  // receiving the next part by calling the completion handler with the NSURLSessionResponseAllow
  // constant. Finally, if you have also implemented URLSession:task:didCompleteWithError:, the
  // session will call it after sending all the data for the last part.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:dataTask:didReceiveResponse:completionHandler');
  {$ENDIF}

  @LCompletionHandlerImpl := imp_implementationWithBlock(completionHandler);
  try
    LCompletionHandlerImpl(NSURLSessionResponseAllow);
  finally
    imp_removeBlock(@LCompletionHandlerImpl);
  end;

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; downloadTask: NSURLSessionDownloadTask; didResumeAtOffset: Int64; expectedTotalBytes: Int64);
begin

  //
  // Tells the delegate that the download task has resumed downloading.
  //
  // If a resumable download task is canceled or fails, you can request a
  // resumeData object that provides enough information to restart the download
  // in the future. Later, you can call downloadTaskWithResumeData: or
  // downloadTaskWithResumeData:completionHandler: with that data.
  //
  // When you call those methods, you get a new download task. As soon as you
  // resume that task, the session calls this method with that new task to
  //indicate that the download is resumed.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:downloadTask:didResumeAtOffset:expectedTotalBytes');
  {$ENDIF}

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; downloadTask: NSURLSessionDownloadTask; didWriteData: Int64; totalBytesWritten: Int64; totalBytesExpectedToWrite: Int64);
begin

  //
  // Periodically informs the delegate about the download’s progress.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:downloadTask:didWriteData:totalBytesWritten:totalBytesExpectedToWrite');
  {$ENDIF}

end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.TURLSessionDelegate.URLSession(session: NSURLSession; downloadTask: NSURLSessionDownloadTask; didFinishDownloadingToURL: NSURL);
begin

  //
  // Tells the delegate that a download task has finished downloading.
  //

  {$IF defined(DEBUG)}
  ALLog(Classname + '.URLSession:session:downloadTask:didFinishDownloadingToURL');
  {$ENDIF}

  if didFinishDownloadingToURL <> nil then begin
    var LSourceFileName := NSStrToStr(didFinishDownloadingToURL.path); // /.nofollow/private/var/mobile/Containers/Data/Application/99F12BF0-0410-456C-B242-5F6198735633/Library/Caches/com.apple.nsurlsessiond/Downloads/ALFmxHttpWorkerDemo/CFNetworkDownload_ml1euK.tmp
    if LSourceFileName <> '' then begin

      var LResponseBodyfilePath := TALHttpWorker.GetTempFilename; // /var/mobile/Containers/Data/Application/E7193C5D-99A8-4E33-BB55-2074B3475A69/Library/Application Support/ALHttpWorker/e6dafe6d-8675-4779-9f50-79c750dab32e.tmp
      TFile.copy(LSourceFileName{SourceFileName}, LResponseBodyfilePath{DestFileName});

      var LLst := TALStringListW.create;
      Try
        LLst.LineBreak := ';';
        LLst.TrailingLineBreak := False;
        LLst.Text := NSStrToStr(downloadTask.taskDescription);
        LLst.Values[TALHttpWorker.KEY_RESPONSE_BODY_FILE_PATH] := LResponseBodyFilePath;
        downloadTask.setTaskDescription(StrToNSStr(LLst.Text));
      Finally
        ALFreeAndNil(LLst);
      end;

    end;
  end;

end;
{$ENDIF}

{*******************************************************************************************************************************************}
constructor TALHttpWorker.TWorkResultMessage.Create(const AResult: TWorkResult; const ARequestId: String; AResponse: TALHttpClientResponseW);
begin
  Inherited Create(AResult);
  FRequestId := ARequestId;
  FResponse := AResponse;
end;

{**************************************************}
destructor TALHttpWorker.TWorkResultMessage.Destroy;
begin
  ALFreeAndNil(FResponse);
  Inherited;
end;

{*******************************}
constructor TALHttpWorker.Create;
begin

  inherited Create;

  FDefaultUserAgent := 'ALHttpWorker/1.0';
  var LTmpPath := ALExtractFilePath(GetTempFilename);
  If not TDirectory.Exists(LTmpPath) then TDirectory.CreateDirectory(LTmpPath);
  AlEmptyDirectoryW(
    LTmpPath, // const Directory: String;
    false, // SubDirectory: Boolean;
    false, // Const RemoveEmptySubDirectory: Boolean = True;
    '*', // Const FileNameMask: String = '*';
    IncDay(Now, -1 * REQUEST_EXPIRATION_DAYS * 2)); // Const MinFileAge: TdateTime = ALNullDate): Boolean; overload;

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  FHandleEventsForBackgroundURLSessionCompletionHandlerImpl := nil;

  // An NSURLSessionConfiguration object defines the behavior and policies to use when
  // uploading and downloading data using an NSURLSession object. When uploading or
  // downloading data, creating a configuration object is always the first step you must
  // take. You use this object to configure the timeout values, caching policies,
  // connection requirements, and other types of information that you intend to use
  // with your NSURLSession object.
  var LURLSessionConfiguration := TNSURLSessionConfiguration.OCClass.backgroundSessionConfigurationWithIdentifier(StrToNSStr(KEY_BACKGROUND_SESSION_IDENTIFIER));
  // Reject all cookies.
  LURLSessionConfiguration.SetHTTPCookieAcceptPolicy(NSHTTPCookieAcceptPolicyNever);
  LURLSessionConfiguration.SetHTTPShouldSetCookies(False);
  // This property determines the request timeout interval for all tasks within sessions
  // based on this configuration. The request timeout interval controls how long (in seconds)
  // a task should wait for additional data to arrive before giving up. The timer associated
  // with this value is reset whenever new data arrives. When the request timer reaches the
  // specified interval without receiving any new data, it triggers a timeout.
  // The default value is 60.
  LURLSessionConfiguration.SetTimeoutIntervalForRequest(180);
  // A Boolean value that indicates whether the session should wait for connectivity
  // to become available, or fail immediately.
  LURLSessionConfiguration.setWaitsForConnectivity(True);

  // A session delegate object that handles requests for authentication and other
  // session-related events. This delegate object is responsible for handling authentication
  // challenges, for making caching decisions, and for handling other session-related events.
  // If nil, the class should be used only with methods that take completion handlers.
  FURLSessionDelegate := TURLSessionDelegate.Create;

  // Creates a session with the specified session configuration, delegate, and operation queue.
  FURLSession := TNSURLSession.OCClass.sessionWithConfiguration(
                   LURLSessionConfiguration, // configuration: NSURLSessionConfiguration;
                   FURLSessionDelegate.GetObjectID, // delegate: Pointer;
                   nil); // delegateQueue: NSOperationQueue
  FURLSession.retain;
  {$ENDIF}
  {$ENDREGION}

end;

{*******************************}
destructor TALHttpWorker.Destroy;
begin

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  // This method returns immediately without waiting for tasks to finish. Once
  // a session is invalidated, new tasks cannot be created in the session, but
  // existing tasks continue until completion. After the last task finishes and
  // the session makes the last delegate call related to those tasks, the
  // session calls the URLSession:didBecomeInvalidWithError: method on its
  // delegate, then breaks references to the delegate and callback objects.
  // After invalidation, session objects cannot be reused.
  FURLSession.finishTasksAndInvalidate;
  {$ENDIF}
  {$ENDREGION}

  inherited;

end;

{*********************************************************}
class function TALHttpWorker.CreateInstance: TALHttpWorker;
begin
  result := TALHttpWorker.Create;
end;

{*************}
//[MultiThread]
class function TALHttpWorker.GetInstance: TALHttpWorker;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunc;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance)
  end;
  Result := FInstance;
end;

{*************}
//[MultiThread]
class function TALHttpWorker.HasInstance: Boolean;
begin
  result := FInstance <> nil;
end;

{*************}
//[MultiThread]
class function TALHttpWorker.GetTempFilename(Const AExt: String = '.tmp'): String;
begin
  Result := TPath.Combine(ALGetAppDataPathW, 'ALHttpWorker', ALLowercase(ALNewGUIDStringW(True{WithoutBracket}, False{WithoutHyphen})) + AExt);
end;

{********************}
{$IF defined(ANDROID)}
class procedure TALHttpWorker.BroadcastReceivedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
begin
  var LBroadcastReceivedMessage := TALBroadcastReceiver.TBroadcastReceivedMessage(Msg);
  if LBroadcastReceivedMessage.action = JStringToString(TJALHttpWorker.JavaClass.ACTION_HTTP_COMPLETED) then begin
    var LResponse := TALHttpClientResponseW.Create;
    Try
      var LSuccess := LBroadcastReceivedMessage.Value.getExtras.getBoolean(TJALHttpWorker.JavaClass.EXTRA_HTTP_SUCCESS, false);
      var LCanceled := LBroadcastReceivedMessage.Value.getExtras.getBoolean(TJALHttpWorker.JavaClass.EXTRA_HTTP_CANCELED, false);
      var LRequestID := JstringToString(LBroadcastReceivedMessage.Value.getExtras.getString(TJALHttpWorker.JavaClass.EXTRA_HTTP_REQUEST_ID, StringToJstring('')));
      var LResponseStatusCode := LBroadcastReceivedMessage.Value.getExtras.getInt(TJALHttpWorker.JavaClass.EXTRA_HTTP_RESPONSE_STATUS_CODE, 0);
      var LResponseHeaders := JstringToString(LBroadcastReceivedMessage.Value.getExtras.getString(TJALHttpWorker.JavaClass.EXTRA_HTTP_RESPONSE_HEADERS, StringToJstring('')));
      var LResponseBodyfilePath := JstringToString(LBroadcastReceivedMessage.Value.getExtras.getString(TJALHttpWorker.JavaClass.EXTRA_HTTP_RESPONSE_BODY_FILE_PATH, StringToJstring('')));
      try
        if LResponseStatusCode > 0 then begin
          LResponse.StatusCode := LResponseStatusCode;
          LResponse.Headers.RawHeaderText := LResponseHeaders;
          if LResponseBodyfilePath <> '' then LResponse.BodyStream := TFileStream.Create(LResponseBodyfilePath, FmOpenRead or fmShareDenyWrite);
        end
        else
          ALFreeAndNil(LResponse);
        var LWorkResult: TWorkResult;
        If LSuccess then LWorkResult := TWorkResult.Success
        else if LCanceled then LWorkResult := TWorkResult.Canceled
        else LWorkResult := TWorkResult.Failure;
        TMessageManager.DefaultManager.SendMessage(
          TALHttpWorker.Instance, // const Sender: TObject
          TWorkResultMessage.Create(LWorkResult, LRequestID, LResponse), // AMessage: TMessage
          True); // ADispose: Boolean
      finally
        If (LResponseBodyfilePath <> '') and Tfile.Exists(LResponseBodyfilePath) then
          Tfile.Delete(LResponseBodyfilePath);
      end;
    Except
      On E: Exception do
        ALLog('TALHttpWorker', E);
    end;
  end;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
class procedure TALHttpWorker.BackgroundNetworkTaskMessageHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
begin
  if not (Msg is TBackgroundNetworkTaskMessage) then
    Exit;

  var LBackgroundNetworkTaskMessage := TBackgroundNetworkTaskMessage(Msg);

  {$IF defined(DEBUG)}
  ALLog(Classname + '.BackgroundNetworkTaskMessageHandler - Identifier: ' + LBackgroundNetworkTaskMessage.value.identifier);
  {$ENDIF}

  if LBackgroundNetworkTaskMessage.value.identifier = TALHttpWorker.KEY_BACKGROUND_SESSION_IDENTIFIER then
    TALHttpWorker.Instance.FHandleEventsForBackgroundURLSessionCompletionHandlerImpl := imp_implementationWithBlock(LBackgroundNetworkTaskMessage.value.CompletionHandler);
end;
{$ENDIF}

{*************}
//[MultiThread]
{$IF defined(IOS)}
procedure TALHttpWorker.getAllTasksCompletionHandler(tasks: NSArray);
begin
  if not TALHttpWorker.HasInstance then exit;
  ALMonitorEnter(TALHttpWorker.FRequestIdsToCancel{$IF defined(DEBUG)}, 'TALHttpWorker.Cancel'{$ENDIF});
  Try
    for var I := 0 to Tasks.count - 1 do begin
      var LTask := TNSURLSessionTask.Wrap(Tasks.objectAtIndex(I));
      var LLst := TALStringListW.create;
      Try
        LLst.LineBreak := ';';
        LLst.TrailingLineBreak := False;
        LLst.Text := NSStrToStr(LTask.taskDescription);
        var LRequestId: String := LLst.Values[TALHttpWorker.KEY_REQUEST_ID];
        var LIdx := TALHttpWorker.FRequestIdsToCancel.IndexOf(LRequestId);
        if LIdx >= 0 then begin
          {$IF defined(DEBUG)}
          ALLog('TALHttpWorker.getAllTasksCompletionHandler', 'Cancelling task with RequestId=' + LRequestId);
          {$ENDIF}
          TALHttpWorker.FRequestIdsToCancel.Delete(LIdx);
          LTask.cancel;
        end;
      finally
        ALFreeAndNil(LLst);
      end;
    end;
  finally
    ALMonitorExit(TALHttpWorker.FRequestIdsToCancel{$IF defined(DEBUG)}, 'TALHttpWorker.Cancel'{$ENDIF});
  end;
end;
{$ENDIF}

{*************}
//[MultiThread]
function TALHttpWorker.enqueue(
           const AUrl: String;
           const AMethod: String;
           const ABodyString: String;
           const AHeaders: TNetHeaders): String;
begin

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  var LHeaders: String := '';
  var LUserAgentHeaderPresent: Boolean := False;
  for var I := Low(AHeaders) to High(AHeaders) do begin
    LUserAgentHeaderPresent := LUserAgentHeaderPresent or ALSameTextW(AHeaders[i].Name, 'User-Agent');
    if AHeaders[i].Value <> '' then
      LHeaders := LHeaders + ALIfThenW(LHeaders <> '', #13#10) + AHeaders[i].Name + ': ' + AHeaders[i].Value;
  end;
  if (not LUserAgentHeaderPresent) and (DefaultUserAgent <> '') then
    LHeaders := LHeaders + ALIfThenW(LHeaders <> '', #13#10) + 'User-Agent: ' + DefaultUserAgent;
  var LUUID := TJALHttpWorker.JavaClass.enqueue(
                 TAndroidHelper.Context, // context: JContext;
                 StringToJString(AUrl), // url: JString;
                 StringToJString(AMethod), // method: JString;
                 StringToJString(ABodyString), // bodyString: JString;
                 StringToJString(LHeaders)); // headers: JString)
  Result := JStringToString(LUUID.toString);
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  Result := enqueue(
              AUrl, // const AUrl: String;
              AMethod, // const AMethod: String;
              TEncoding.UTF8.GetBytes(ABodyString), // const ABodyBytes: TBytes;
              AHeaders); // const AHeaders: TNetHeaders)
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'MSWindows/MacOS'}
  {$IF defined(MSWindows) or defined(ALMacOS)}
  Result := ALNewGUIDStringW(True{WithoutBracket});
  var LRequestID := Result;
  TThread.CreateAnonymousThread(
    procedure
    begin

      Var LUri := TUri.Create(AUrl);
      var LHttpClient := ALAcquireKeepAliveNetHttpClient(LUri);
      try
        LHttpClient.UserAgent := DefaultUserAgent;

        var LResponse := TALHttpClientResponseW.Create;
        try

          var LHTTPRequest: IHTTPRequest := LHttpClient.GetRequest(AMethod, LUri);
          LHTTPRequest.SourceStream := TALStringStreamW.Create(ABodyString, TEncoding.UTF8, False);
          Try
            var LHTTPResponse := LHttpClient.Execute(LHttpRequest, LResponse.BodyStream, AHeaders);
            LResponse.StatusCode := LHTTPResponse.StatusCode;
            var LResponseHeader := LHTTPResponse.Headers;
            for var I := Low(LResponseHeader) to High(LResponseHeader) do
              LResponse.Headers.Values[LResponseHeader[I].Name] := LResponseHeader[I].Value;
          finally
            ALFreeAndNil(LHTTPRequest.SourceStream);
          end;

          TThread.Queue(nil,
            Procedure
            Begin
              try
                var LWorkResult: TWorkResult;
                If (LResponse.StatusCode >= 200) and (LResponse.StatusCode < 300) then LWorkResult := TWorkResult.Success
                else LWorkResult := TWorkResult.Failure;
                TMessageManager.DefaultManager.SendMessage(
                  TALHttpWorker.Instance, // const Sender: TObject
                  TWorkResultMessage.Create(LWorkResult, LRequestID, LResponse), // AMessage: TMessage
                  True); // ADispose: Boolean
              Except
                On E: Exception do
                  ALLog('TALHttpWorker', E);
              end;
            end);

        except
          on E: Exception do begin
            ALLog('TALHttpWorker', E);
            ALFreeAndNil(LResponse);
            TThread.Queue(nil,
              Procedure
              Begin
                try
                  TMessageManager.DefaultManager.SendMessage(
                    TALHttpWorker.Instance, // const Sender: TObject
                    TWorkResultMessage.Create(TWorkResult.Failure, LRequestID, nil), // AMessage: TMessage
                    True); // ADispose: Boolean
                Except
                  On E: Exception do
                    ALLog('TALHttpWorker', E);
                end;
              end);
          end;
        end;

      finally
        ALReleaseKeepAliveNetHttpClient(LUri, LHttpClient);
      end;

    end).Start;
  {$ENDIF}
  {$ENDREGION}

end;

{*************}
//[MultiThread]
function TALHttpWorker.enqueue(
           const AUrl: String;
           const AMethod: String;
           const ABodyFilePath: String;
           const ADeleteBodyFile: Boolean;
           const AHeaders: TNetHeaders): String;
begin

  If (ABodyFilePath = '') or (not Tfile.Exists(ABodyFilePath)) then
    raise Exception.Create('Request body file not found');

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  var LHeaders: String := '';
  var LUserAgentHeaderPresent: Boolean := False;
  for var I := Low(AHeaders) to High(AHeaders) do begin
    LUserAgentHeaderPresent := LUserAgentHeaderPresent or ALSameTextW(AHeaders[i].Name, 'User-Agent');
    if AHeaders[i].Value <> '' then
      LHeaders := LHeaders + ALIfThenW(LHeaders <> '', #13#10) + AHeaders[i].Name + ': ' + AHeaders[i].Value;
  end;
  if (not LUserAgentHeaderPresent) and (DefaultUserAgent <> '') then
    LHeaders := LHeaders + ALIfThenW(LHeaders <> '', #13#10) + 'User-Agent: ' + DefaultUserAgent;
  var LUUID := TJALHttpWorker.JavaClass.enqueue(
                 TAndroidHelper.Context, // context: JContext;
                 StringToJString(AUrl), // url: JString;
                 StringToJString(AMethod), // method: JString;
                 StringToJString(ABodyFilePath), // bodyFilePath: JString;
                 ADeleteBodyFile, // deleteBodyFile: Boolean
                 StringToJString(LHeaders)); // headers: JString)
  Result := JStringToString(LUUID.toString);
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  Result := ALNewGUIDStringW(True{WithoutBracket});
  var LNSUrl := StrToNSURL(AUrl);
  if LNSUrl = nil then raise Exception.CreateFmt('Invalid URL: %s', [AUrl]);
  var LURLRequest := TNSMutableURLRequest.Wrap(TNSMutableURLRequest.OCClass.requestWithURL(LNSUrl));
  LURLRequest.setHTTPMethod(StrToNSStr(AMethod));
  var LUserAgentHeaderPresent: Boolean := False;
  for var I := Low(AHeaders) to High(AHeaders) do begin
    LUserAgentHeaderPresent := LUserAgentHeaderPresent or ALSameTextW(AHeaders[i].Name, 'User-Agent');
    if AHeaders[i].Value <> '' then
      LURLRequest.addValue(StrToNSStr(AHeaders[I].Value), StrToNSStr(AHeaders[I].Name));
  end;
  if (not LUserAgentHeaderPresent) and (DefaultUserAgent <> '') then
    LURLRequest.addValue(StrToNSStr(DefaultUserAgent), StrToNSStr('User-Agent'));

  // https://stackoverflow.com/questions/79827816/nsurlsession-how-to-upload-a-large-file-and-download-a-large-file-in-backgroun
  // We can only use the uploadTaskWithRequest, in ios their is no concept like uploadTaskWithRequest+downloadTaskWithRequest
  var LFileURL := TNSURL.OCClass.fileURLWithPath(StrToNSStr(ABodyFilePath));
  var LTask := FURLSession.uploadTaskWithRequest(LURLRequest, LFileURL);
  var LLst := TALStringListW.create;
  Try
    LLst.LineBreak := ';';
    LLst.TrailingLineBreak := False;
    LLst.Values[TALHttpWorker.KEY_REQUEST_ID] := Result;
    LLst.Values[TALHttpWorker.KEY_REQUEST_BODY_FILE_PATH] := ABodyFilePath;
    LLst.Values[TALHttpWorker.KEY_DELETE_REQUEST_BODY_FILE] := ALBoolToStrW(ADeleteBodyFile);
    LLst.Values[TALHttpWorker.KEY_ENQUEUE_TIME] := ALIntToStrW(ALDateTimeToUnixMs(ALUTCNow));
    LTask.setTaskDescription(StrToNSStr(LLst.Text));
  Finally
    ALFreeAndNil(LLst);
  end;
  LTask.resume;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'MSWindows/MacOS'}
  {$IF defined(MSWindows) or defined(ALMacOS)}
  Result := ALNewGUIDStringW(True{WithoutBracket});
  var LRequestID := Result;
  TThread.CreateAnonymousThread(
    procedure
    begin

      Var LUri := TUri.Create(AUrl);
      var LHttpClient := ALAcquireKeepAliveNetHttpClient(LUri);
      try
        LHttpClient.UserAgent := DefaultUserAgent;

        var LResponse := TALHttpClientResponseW.Create;
        try

          var LHTTPRequest: IHTTPRequest := LHttpClient.GetRequest(AMethod, LUri);
          LHTTPRequest.SourceStream := TfileStream.Create(ABodyFilePath, FmOpenRead or fmShareDenyWrite);
          Try
            var LHTTPResponse := LHttpClient.Execute(LHttpRequest, LResponse.BodyStream, AHeaders);
            LResponse.StatusCode := LHTTPResponse.StatusCode;
            var LResponseHeader := LHTTPResponse.Headers;
            for var I := Low(LResponseHeader) to High(LResponseHeader) do
              LResponse.Headers.Values[LResponseHeader[I].Name] := LResponseHeader[I].Value;
          finally
            ALFreeAndNil(LHTTPRequest.SourceStream);
          end;

          if ADeleteBodyFile and Tfile.Exists(ABodyfilePath) then
            Tfile.Delete(ABodyfilePath);

          TThread.Queue(nil,
            Procedure
            Begin
              try
                var LWorkResult: TWorkResult;
                If (LResponse.StatusCode >= 200) and (LResponse.StatusCode < 300) then LWorkResult := TWorkResult.Success
                else LWorkResult := TWorkResult.Failure;
                TMessageManager.DefaultManager.SendMessage(
                  TALHttpWorker.Instance, // const Sender: TObject
                  TWorkResultMessage.Create(LWorkResult, LRequestID, LResponse), // AMessage: TMessage
                  True); // ADispose: Boolean
              Except
                On E: Exception do
                  ALLog('TALHttpWorker', E);
              end;
            end);

        except
          on E: Exception do begin
            ALLog('TALHttpWorker', E);
            ALFreeAndNil(LResponse);
            if ADeleteBodyFile and Tfile.Exists(ABodyfilePath) then
              Tfile.Delete(ABodyfilePath);
            TThread.Queue(nil,
              Procedure
              Begin
                try
                  TMessageManager.DefaultManager.SendMessage(
                    TALHttpWorker.Instance, // const Sender: TObject
                    TWorkResultMessage.Create(TWorkResult.Failure, LRequestID, nil), // AMessage: TMessage
                    True); // ADispose: Boolean
                Except
                  On E: Exception do
                    ALLog('TALHttpWorker', E);
                end;
              end);
          end;
        end;

      finally
        ALReleaseKeepAliveNetHttpClient(LUri, LHttpClient);
      end;

    end).Start;
  {$ENDIF}
  {$ENDREGION}

end;

{*************}
//[MultiThread]
function TALHttpWorker.enqueue(
           const AUrl: String;
           const AMethod: String;
           const ABodyBytes: TBytes;
           const AHeaders: TNetHeaders): String;
begin

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  If length(ABodyBytes) = 0 then begin
    Result := enqueue(
                AUrl, // const AUrl: String;
                AMethod, // const AMethod: String;
                '', // const ABodyString: String;
                AHeaders); // const AHeaders: TNetHeaders)
  end
  else begin
    var LFilename := GetTempFilename;
    try
      TFile.WriteAllBytes(LFilename, ABodyBytes);
      result := enqueue(
                  AUrl, // const AUrl: String;
                  AMethod, // const AMethod: String;
                  LFilename, // const ABodyFilePath: String;
                  true, // const ADeleteBodyFile: Boolean;
                  AHeaders); // const AHeaders: TNetHeaders): String;
    except
      On E: Exception do begin
        If TFile.Exists(LFilename) then TFile.Delete(LFilename);
        Raise;
      end;
    end;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  If length(ABodyBytes) > 4096 then begin
    var LFilename := GetTempFilename;
    try
      TFile.WriteAllBytes(LFilename, ABodyBytes);
      result := enqueue(
                  AUrl, // const AUrl: String;
                  AMethod, // const AMethod: String;
                  LFilename, // const ABodyFilePath: String;
                  true, // const ADeleteBodyFile: Boolean;
                  AHeaders); // const AHeaders: TNetHeaders): String;
    except
      On E: Exception do begin
        If TFile.Exists(LFilename) then TFile.Delete(LFilename);
        Raise;
      end;
    end;
  end
  else begin
    Result := ALNewGUIDStringW(True{WithoutBracket});
    var LNSUrl := StrToNSURL(AUrl);
    if LNSUrl = nil then raise Exception.CreateFmt('Invalid URL: %s', [AUrl]);
    var LURLRequest := TNSMutableURLRequest.Wrap(TNSMutableURLRequest.OCClass.requestWithURL(LNSUrl));
    LURLRequest.setHTTPMethod(StrToNSStr(AMethod));
    var LUserAgentHeaderPresent: Boolean := False;
    for var I := Low(AHeaders) to High(AHeaders) do begin
      LUserAgentHeaderPresent := LUserAgentHeaderPresent or ALSameTextW(AHeaders[i].Name, 'User-Agent');
      if AHeaders[i].Value <> '' then
        LURLRequest.addValue(StrToNSStr(AHeaders[I].Value), StrToNSStr(AHeaders[I].Name));
    end;
    if (not LUserAgentHeaderPresent) and (DefaultUserAgent <> '') then
      LURLRequest.addValue(StrToNSStr(DefaultUserAgent), StrToNSStr('User-Agent'));

    if length(ABodyBytes) > 0 then begin
      var LData: NSData;
      LData := TNSData.Wrap(TNSData.OCClass.dataWithBytes(@ABodyBytes[0], Length(ABodyBytes)));
      LURLRequest.setHTTPBody(LData);
    end;

    // https://stackoverflow.com/questions/79827816/nsurlsession-how-to-upload-a-large-file-and-download-a-large-file-in-backgroun
    // We can only use the DownloadTaskWithRequest, in ios their is no concept like UploadTaskWithRequest+DownloadTaskWithRequest
    var LTask := FURLSession.DownloadTaskWithRequest(LURLRequest);
    var LLst := TALStringListW.create;
    Try
      LLst.LineBreak := ';';
      LLst.TrailingLineBreak := False;
      LLst.Values[TALHttpWorker.KEY_REQUEST_ID] := Result;
      LLst.Values[TALHttpWorker.KEY_ENQUEUE_TIME] := ALIntToStrW(ALDateTimeToUnixMs(ALUTCNow));
      LTask.setTaskDescription(StrToNSStr(LLst.Text));
    Finally
      ALFreeAndNil(LLst);
    end;
    LTask.resume;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'MSWindows/MacOS'}
  {$IF defined(MSWindows) or defined(ALMacOS)}
  Result := ALNewGUIDStringW(True{WithoutBracket});
  var LRequestID := Result;
  TThread.CreateAnonymousThread(
    procedure
    begin

      Var LUri := TUri.Create(AUrl);
      var LHttpClient := ALAcquireKeepAliveNetHttpClient(LUri);
      try
        LHttpClient.UserAgent := DefaultUserAgent;

        var LResponse := TALHttpClientResponseW.Create;
        try

          var LHTTPRequest: IHTTPRequest := LHttpClient.GetRequest(AMethod, LUri);
          LHTTPRequest.SourceStream := TBytesStream.Create(ABodyBytes);
          Try
            var LHTTPResponse := LHttpClient.Execute(LHttpRequest, LResponse.BodyStream, AHeaders);
            LResponse.StatusCode := LHTTPResponse.StatusCode;
            var LResponseHeader := LHTTPResponse.Headers;
            for var I := Low(LResponseHeader) to High(LResponseHeader) do
              LResponse.Headers.Values[LResponseHeader[I].Name] := LResponseHeader[I].Value;
          finally
            ALFreeAndNil(LHTTPRequest.SourceStream);
          end;

          TThread.Queue(nil,
            Procedure
            Begin
              try
                var LWorkResult: TWorkResult;
                If (LResponse.StatusCode >= 200) and (LResponse.StatusCode < 300) then LWorkResult := TWorkResult.Success
                else LWorkResult := TWorkResult.Failure;
                TMessageManager.DefaultManager.SendMessage(
                  TALHttpWorker.Instance, // const Sender: TObject
                  TWorkResultMessage.Create(LWorkResult, LRequestID, LResponse), // AMessage: TMessage
                  True); // ADispose: Boolean
              Except
                On E: Exception do
                  ALLog('TALHttpWorker', E);
              end;
            end);

        except
          on E: Exception do begin
            ALLog('TALHttpWorker', E);
            ALFreeAndNil(LResponse);
            TThread.Queue(nil,
              Procedure
              Begin
                try
                  TMessageManager.DefaultManager.SendMessage(
                    TALHttpWorker.Instance, // const Sender: TObject
                    TWorkResultMessage.Create(TWorkResult.Failure, LRequestID, nil), // AMessage: TMessage
                    True); // ADispose: Boolean
                Except
                  On E: Exception do
                    ALLog('TALHttpWorker', E);
                end;
              end);
          end;
        end;

      finally
        ALReleaseKeepAliveNetHttpClient(LUri, LHttpClient);
      end;

    end).Start;
  {$ENDIF}
  {$ENDREGION}

end;

{*************}
//[MultiThread]
function TALHttpWorker.enqueue(
           const AUrl: String;
           const AMethod: String;
           const ABodyStream: Tstream;
           const AHeaders: TNetHeaders): String;
begin

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  if ABodyStream.size = 0 then begin
    Result := enqueue(
                AUrl, // const AUrl: String;
                AMethod, // const AMethod: String;
                '', // const ABodyString: String;
                AHeaders); // const AHeaders: TNetHeaders)
  end
  else begin
    var LFilename := GetTempFilename;
    Try
      var LFileStream := TFileStream.Create(LFilename, fmCreate or fmShareExclusive);
      try
        ABodyStream.Position := 0;
        LFileStream.CopyFrom(ABodyStream);
      finally
        ALFreeAndNil(LFileStream);
      end;
      result := enqueue(
                  AUrl, // const AUrl: String;
                  AMethod, // const AMethod: String;
                  LFilename, // const ABodyFilePath: String;
                  true, // const ADeleteBodyFile: Boolean;
                  AHeaders); // const AHeaders: TNetHeaders): String;
    except
      On E: Exception do begin
        If TFile.Exists(LFilename) then TFile.Delete(LFilename);
        Raise;
      end;
    end;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  if ABodyStream.size <= 4096 then begin
    Var LBytes: TBytes;
    Setlength(LBytes, ABodyStream.Size);
    ABodyStream.Position := 0;
    ABodyStream.ReadBuffer(Lbytes, length(Lbytes));
    Result := enqueue(
                AUrl, // const AUrl: String;
                AMethod, // const AMethod: String;
                LBytes, // const ABodyBytes: TBytes;
                AHeaders); // const AHeaders: TNetHeaders)
  end
  else begin
    var LFilename := GetTempFilename;
    Try
      var LFileStream := TFileStream.Create(LFilename, fmCreate or fmShareExclusive);
      try
        ABodyStream.Position := 0;
        LFileStream.CopyFrom(ABodyStream);
      finally
        ALFreeAndNil(LFileStream);
      end;
      result := enqueue(
                  AUrl, // const AUrl: String;
                  AMethod, // const AMethod: String;
                  LFilename, // const ABodyFilePath: String;
                  true, // const ADeleteBodyFile: Boolean;
                  AHeaders); // const AHeaders: TNetHeaders): String;
    except
      On E: Exception do begin
        If TFile.Exists(LFilename) then TFile.Delete(LFilename);
        Raise;
      end;
    end;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'MSWindows/MacOS'}
  {$IF defined(MSWindows) or defined(ALMacOS)}
  Var LBytes: TBytes;
  Setlength(LBytes, ABodyStream.Size);
  ABodyStream.Position := 0;
  ABodyStream.ReadBuffer(Lbytes, length(Lbytes));
  Result := enqueue(
              AUrl, // const AUrl: String;
              AMethod, // const AMethod: String;
              LBytes, // const ABodyBytes: TBytes;
              AHeaders); // const AHeaders: TNetHeaders)
  {$ENDIF}
  {$ENDREGION}

end;

{*******************************************************}
procedure TALHttpWorker.Cancel(const ARequestID: String);
begin

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  TJALHttpWorker.JavaClass.Cancel(
    TAndroidHelper.Context, // context: JContext;
    TJUUID.JavaClass.fromString(StringToJstring(ARequestID))); // requestId: JUUID
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  ALMonitorEnter(TALHttpWorker.FRequestIdsToCancel{$IF defined(DEBUG)}, 'TALHttpWorker.Cancel'{$ENDIF});
  Try
    TALHttpWorker.FRequestIdsToCancel.add(ARequestID);
  finally
    ALMonitorExit(TALHttpWorker.FRequestIdsToCancel{$IF defined(DEBUG)}, 'TALHttpWorker.Cancel'{$ENDIF});
  end;
  FURLSession.getAllTasksWithCompletionHandler(getAllTasksCompletionHandler);
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'MSWindows/MacOS'}
  {$IF defined(MSWindows) or defined(ALMacOS)}
  // Not implemented under Windows
  {$ENDIF}
  {$ENDREGION}

end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.HTTP.Worker','initialization');
  {$ENDIF}
  TALHttpWorker.FInstance := nil;
  TALHttpWorker.CreateInstanceFunc := @TALHttpWorker.CreateInstance;
  {$IF defined(ANDROID)}
  TMessageManager.DefaultManager.SubscribeToMessage(TALBroadcastReceiver.TBroadcastReceivedMessage, TALHttpWorker.BroadcastReceivedHandler);
  TALBroadcastReceiver.Instance;
  {$ENDIF}
  {$IF defined(IOS)}
  TMessageManager.DefaultManager.SubscribeToMessage(TBackgroundNetworkTaskMessage, TALHttpWorker.BackgroundNetworkTaskMessageHandler);
  TALHttpWorker.FRequestIdsToCancel := TALStringListW.Create;
  {$ENDIF}
  TThread.ForceQueue(nil,
    Procedure
    Begin
      {$IF defined(ANDROID)}
      TALBroadcastReceiver.Instance;
      {$ENDIF}
      TALHttpWorker.Instance;
    End);

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.HTTP.Worker','finalization');
  {$ENDIF}
  ALFreeAndNil(TALHttpWorker.FInstance);
  {$IF defined(ANDROID)}
  TMessageManager.DefaultManager.Unsubscribe(TALBroadcastReceiver.TBroadcastReceivedMessage, TALHttpWorker.BroadcastReceivedHandler);
  {$ENDIF}
  {$IF defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TBackgroundNetworkTaskMessage, TALHttpWorker.BackgroundNetworkTaskMessageHandler);
  ALFreeAndNil(TALHttpWorker.FRequestIdsToCancel);
  {$ENDIF}

end.