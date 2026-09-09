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

// Group calls by call stack.
// The result will be displayed as a grouped call tree, for example:
// procedure A - 1 call - 310 ms
//   procedure B - 3 calls - 291 ms
//     procedure C - 3 calls - 46 ms
//     procedure D - 1 call - 85 ms
//   procedure C - 1 call - 22 ms
{$DEFINE ALCodeProfilerHistoryGroupByCallStack}
