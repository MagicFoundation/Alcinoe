unit Alcinoe.iOSApi.BackgroundTasks;

interface

{$I Alcinoe.inc}

uses
  Macapi.CoreFoundation,
  Macapi.CoreServices,
  Macapi.Dispatch,
  Macapi.Mach,
  Macapi.ObjCRuntime,
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.Foundation;

{$M+}

const

  {**************************************}
  BGTaskSchedulerErrorCodeUnavailable = 1;
  BGTaskSchedulerErrorCodeTooManyPendingTaskRequests = 2;
  BGTaskSchedulerErrorCodeNotPermitted = 3;

type

  {**************************}
  BGTaskScheduler = interface;
  BGTaskRequest = interface;
  BGTask = interface;
  BGAppRefreshTaskRequest = interface;
  BGProcessingTaskRequest = interface;
  BGProcessingTask = interface;
  BGAppRefreshTask = interface;
  BGTaskSchedulerErrorCode = NSInteger;
  TBackgroundTasksLaunchHandler = procedure(task: BGTask) of object;
  TBackgroundTasksCompletionHandler = procedure(taskRequests: NSArray) of object;
  TBackgroundTasksExpirationHandler = procedure of object;

  {*************************************}
  BGTaskSchedulerClass = interface(NSObjectClass)
    ['{170680B1-029E-4F5E-9E0C-E5AF137AEE5B}']
    function sharedScheduler : BGTaskScheduler; cdecl;
  end;
  BGTaskScheduler = interface(NSObject)
    ['{797041F8-7F8F-4F79-9D84-6DADB67C2E9B}']
    function registerForTaskWithIdentifier(identifier: NSString; usingQueue: dispatch_queue_t; launchHandler: TBackgroundTasksLaunchHandler) : Boolean; cdecl;
    function submitTaskRequest(taskRequest: BGTaskRequest; error: NSError) : Boolean; cdecl;
    procedure cancelTaskRequestWithIdentifier(identifier: NSString); cdecl;
    procedure cancelAllTaskRequests; cdecl;
    procedure getPendingTaskRequestsWithCompletionHandler(completionHandler: TBackgroundTasksCompletionHandler); cdecl;
  end;
  TBGTaskScheduler = class(TOCGenericImport<BGTaskSchedulerClass, BGTaskScheduler>)  end;
  PBGTaskScheduler = Pointer;

  {***********************************************}
  BGTaskRequestClass = interface(NSObjectClass)
    ['{B28ED8E7-0B5F-4040-A605-830485DFCD35}']
  end;
  BGTaskRequest = interface(NSObject)
    ['{B85E51AD-60FA-4E5E-99B4-7CDAC11FB67E}']
    function identifier : NSString; cdecl;
    procedure setEarliestBeginDate(earliestBeginDate: NSDate); cdecl;
    function earliestBeginDate : NSDate; cdecl;
  end;
  TBGTaskRequest = class(TOCGenericImport<BGTaskRequestClass, BGTaskRequest>)  end;
  PBGTaskRequest = Pointer;

  {****************************}
  BGTaskClass = interface(NSObjectClass)
    ['{B92EA1E2-5688-4905-BE66-120A7AF3536A}']
  end;
  BGTask = interface(NSObject)
    ['{288AF07F-65C6-4786-AC11-953FAC35E024}']
    function identifier : NSString; cdecl;
    procedure setExpirationHandler(expirationHandler: TBackgroundTasksExpirationHandler); cdecl;
    function expirationHandler : TBackgroundTasksExpirationHandler; cdecl;
    procedure setTaskCompletedWithSuccess(success: Boolean); cdecl;
  end;
  TBGTask = class(TOCGenericImport<BGTaskClass, BGTask>)  end;
  PBGTask = Pointer;

  {**************************************************}
  BGAppRefreshTaskRequestClass = interface(BGTaskRequestClass)
    ['{43B446D7-5211-4692-919E-41FC77C01BE6}']
  end;
  BGAppRefreshTaskRequest = interface(BGTaskRequest)
    ['{0D0CE9BE-C449-4BD9-B3C3-385E1D8F96D3}']
    function initWithIdentifier(identifier: NSString) : BGAppRefreshTaskRequest; cdecl;
  end;
  TBGAppRefreshTaskRequest = class(TOCGenericImport<BGAppRefreshTaskRequestClass, BGAppRefreshTaskRequest>)  end;
  PBGAppRefreshTaskRequest = Pointer;

  {**************************************************}
  BGProcessingTaskRequestClass = interface(BGTaskRequestClass)
    ['{E4D6EC24-BBB6-4DDD-99F8-98B4D2FA3CFA}']
  end;
  BGProcessingTaskRequest = interface(BGTaskRequest)
    ['{12D572FA-69D2-41EB-87A7-86B94631C45E}']
    function initWithIdentifier(identifier: NSString) : BGProcessingTaskRequest; cdecl;
    procedure setRequiresNetworkConnectivity(requiresNetworkConnectivity: Boolean); cdecl;
    function requiresNetworkConnectivity : Boolean; cdecl;
    procedure setRequiresExternalPower(requiresExternalPower: Boolean); cdecl;
    function requiresExternalPower : Boolean; cdecl;
  end;
  TBGProcessingTaskRequest = class(TOCGenericImport<BGProcessingTaskRequestClass, BGProcessingTaskRequest>)  end;
  PBGProcessingTaskRequest = Pointer;

  {************************************}
  BGProcessingTaskClass = interface(BGTaskClass)
    ['{D8A7063A-4FF7-4A2A-94A4-5CBF1869BB9F}']
  end;
  BGProcessingTask = interface(BGTask)
    ['{34286102-85FB-483B-A61E-E2FA55F2EBB6}']
  end;
  TBGProcessingTask = class(TOCGenericImport<BGProcessingTaskClass, BGProcessingTask>)  end;
  PBGProcessingTask = Pointer;

  {************************************}
  BGAppRefreshTaskClass = interface(BGTaskClass)
    ['{2A0BC55E-7165-4635-9BA9-B031B6127C4D}']
  end;
  BGAppRefreshTask = interface(BGTask)
    ['{AD93C9F9-0BC3-4F79-9309-E56A4386DC1A}']
  end;
  TBGAppRefreshTask = class(TOCGenericImport<BGAppRefreshTaskClass, BGAppRefreshTask>)  end;
  PBGAppRefreshTask = Pointer;

{*******************************************}
function BGTaskSchedulerErrorDomain: Pointer;

const
  libBackgroundTasks = '/System/Library/Frameworks/BackgroundTasks.framework/BackgroundTasks';

implementation

{*******************************************}
function BGTaskSchedulerErrorDomain: Pointer;
begin
  Result := CocoaPointerConst(libBackgroundTasks, 'BGTaskSchedulerErrorDomain');
end;

{******************************************************************************************}
procedure StubProc1; cdecl; external libBackgroundTasks name 'OBJC_CLASS_$_BGTaskScheduler';

end.