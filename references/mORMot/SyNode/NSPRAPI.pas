/// Netscape Portable Runtime *.h header port to Delphi
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit NSPRAPI;

interface

{ NSPR library APIs }

const
  NSPRLib = 'nspr4'{$IFDEF MSWINDOWS} + '.dll'{$ENDIF};
const
  /// numbers of micro secs per second
  PRMJ_USEC_PER_SEC = 1000000;

  /// stipulate that the process should wait no time as defined by NSPR
  // - i.e. will return immediately
  // - defined in the PRIntervalTime namespace
  PR_INTERVAL_NO_WAIT    =$0;
  /// stipulate that the process should wait forever as defined by NSPR
  // - i.e. will never time out
  // - defined in the PRIntervalTime namespace
  PR_INTERVAL_NO_TIMEOUT =$ffffffff;
  
type
  /// unsigned 32 bit integer type as defined by NSPR
  PRUint32 = Cardinal;
  /// interval time type as defined by NSPR
  PRIntervalTime = PRUint32;

  /// a mutex/lock resource as defined by NSPR
  PRLock = Pointer;
  /// a event resource as defined by NSPR
  PRCondVar = Pointer;
  /// a thread resource as defined by NSPR
  PRThread = Pointer;

  /// status codes as defined by NSPR
  PRStatus = (PR_FAILURE = -1, PR_SUCCESS = 0);

  /// thread type as defined by NSPR
  PRThreadType = (PR_USER_THREAD, PR_SYSTEM_THREAD);

  /// thread priority as defined by NSPR
  // - PR_PRIORITY_LOW is the lowest possible priority
  // - PR_PRIORITY_NORMAL is the most common expected priority
  // - PR_PRIORITY_HIGH is the slightly more aggressive scheduling
  // - PR_PRIORITY_URGENT is there because it does little good to have one
  // more priority value
    PRThreadPriority = (
    PR_PRIORITY_FIRST = 0,
    PR_PRIORITY_LOW = 0,
    PR_PRIORITY_NORMAL = 1,
    PR_PRIORITY_HIGH = 2,
    PR_PRIORITY_URGENT = 3,
    PR_PRIORITY_LAST = 3);

  /// thread scope as defined by NSPR
  PRThreadScope = (PR_LOCAL_THREAD, PR_GLOBAL_THREAD, PR_GLOBAL_BOUND_THREAD);

  /// thread state as defined by NSPR
  PRThreadState = (PR_JOINABLE_THREAD, PR_UNJOINABLE_THREAD);

/// allocates a new NSPR mutex/lock
function PR_NewLock: PRLock; cdecl; external NSPRLib;

/// allocates a new NSPR event
function PR_NewCondVar(lock: PRLock): PRCondVar; cdecl; external NSPRLib;

/// free a previously allocated NSPR event
procedure PR_DestroyCondVar(cvar: PRCondVar); cdecl; external NSPRLib;

/// notify a previously allocated NSPR event
function PR_NotifyCondVar(cvar: PRCondVar): PRStatus; cdecl; external NSPRLib;

/// notify all previously allocated NSPR event
function PR_NotifyAllCondVar(cvar: PRCondVar): PRStatus; cdecl; external NSPRLib;

/// wait until a previously allocated NSPR event is notified
function PR_WaitCondVar(cvar: PRCondVar; timeout: PRIntervalTime): PRStatus;
  cdecl; external NSPRLib;

/// enter a previously allocated NSPR mutex/lock
procedure PR_Lock(lock: PRLock); cdecl; external NSPRLib;

/// leave a previously allocated NSPR mutex/lock
function PR_Unlock(lock: PRLock): PRStatus; cdecl; external NSPRLib;

/// free a previously allocated NSPR lock
procedure PR_DestroyLock(lock: PRLock); cdecl; external NSPRLib;

/// join a NSPR thread
function PR_JoinThread(thred: PRThread): PRStatus; cdecl; external NSPRLib;

/// initializes a NSPR thread
function PR_CreateThread(
  type_: PRThreadType; start: pointer; arg: pointer;
  priority: PRThreadPriority; scope: PRThreadScope;
  state: PRThreadState; stackSize: PRUint32): PRThread; cdecl; external NSPRLib;

/// change the current NSPR thread name
function PR_SetCurrentThreadName(name: PAnsiChar): PRStatus;
  cdecl; external NSPRLib;

/// returns the number of ticks per seconds as expected by NSPR
function PR_TicksPerSecond(): PRUint32; cdecl; external NSPRLib;

implementation

end.
