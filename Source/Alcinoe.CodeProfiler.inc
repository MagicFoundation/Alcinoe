var
  ALCodeProfilerEnabled: Boolean = True;
  ALCodeProfilerServerName: String = '';

// Do not group calls. Each function/procedure call generates one row.
// The result will be displayed as a call tree, for example:
// procedure A - 1 call - 310 ms
//   procedure B - 1 call - 215 ms
//     procedure C - 1 call - 14 ms
//     procedure C - 1 call - 11 ms
//     procedure D - 1 call - 85 ms
//     procedure C - 1 call - 21 ms
//   procedure B - 1 call - 35 ms
//   procedure B - 1 call - 41 ms
//   procedure C - 1 call - 22 ms
//
// NOTE: This option can use a lot of memory. If you enable it, it is recommended
// to limit profiling to the specific code you want to measure by using
// ALCodeProfilerStart and ALCodeProfilerStop.
{.$DEFINE ALCodeProfilerHistoryGroupNone}

// Group calls by procedure ID.
// The result will be displayed as a flat grid, for example:
// procedure A - 1 call - 310 ms
// procedure B - 3 calls - 291 ms
// procedure C - 4 calls - 68 ms
// procedure D - 1 call - 85 ms
//
// NOTE: This is the fastest option and has the lowest impact on each function call.
{.$DEFINE ALCodeProfilerHistoryGroupByProcID}

{$IF defined(ALCodeProfilerHistoryGroupByProcID)}
// Ignore the thread ID. The calls made from every thread are merged together
// instead of producing one row per procedure and per thread, for example:
// procedure A - 3 calls - 310 ms  = 1 call from the main thread + 2 calls from a background thread
//
// NOTE: This option is only available with ALCodeProfilerHistoryGroupByProcID.
// All the threads then share the same metrics, which are updated atomically,
// so it slightly increases the cost of each function call, but it also greatly
// reduces the memory usage as only one history is allocated for the whole
// process instead of one per thread.
{.$DEFINE ALCodeProfilerIgnoreThreadID}
{$ENDIF}

// Group calls by call stack.
// The result will be displayed as a grouped call tree, for example:
// procedure A - 1 call - 310 ms
//   procedure B - 3 calls - 291 ms
//     procedure C - 3 calls - 46 ms
//     procedure D - 1 call - 85 ms
//   procedure C - 1 call - 22 ms
{$DEFINE ALCodeProfilerHistoryGroupByCallStack}

// Capacity of the history, in number of rows.
//
// NOTE: With ALCodeProfilerHistoryGroupByProcID the history is a flat array
// indexed by the procedure ID, so it only needs to be big enough to hold the
// highest procedure ID of ALCodeProfilerProcIDMap.txt. The value below is
// updated by the Alcinoe Code Profiler GUI each time the markers are inserted
// or removed, so there is no reason to edit it by hand.
{$IF defined(ALCodeProfilerHistoryGroupByProcID)}
const
  ALCodeProfilerHistoryCapacity = 1000000;
{$ELSE}
const
  ALCodeProfilerHistoryCapacity = 1000000; {1 000 000 * 32 Bytes = 32 MB or with gap = 2 097 152 * 32 Bytes = 67.11 MB}
{$ENDIF}
