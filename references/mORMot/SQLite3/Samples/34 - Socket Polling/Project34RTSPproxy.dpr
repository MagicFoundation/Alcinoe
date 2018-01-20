program Project34RTSPproxy;

{$APPTYPE CONSOLE}

(*

  Synopse mORMot framework

  Sample 34 - RTSP over HTTP proxy
    Implements RTSP stream tunnelling over HTTP.

 Purpose of this sample is to illustrate low-level use of TAsynchConnections,
 on both Windows and Linux.

 The HTTP transport is built from two separate HTTP GET and POST requests
 initiated by the client. The server then binds the connections to form a
 virtual full-duplex connection.

 See https://goo.gl/CX6VA3 for reference material about this horrible, but
 widely accepted, Apple's hack.


  Version 1.18
  - Initial Release

  first line of uses clause below must be {$I SynDprUses.inc} to enable FastMM4

*)

uses
  {$I SynDprUses.inc}
  SysUtils,
  SynCommons,
  SynLog,
  SynCrtSock,
  SynBidirSock,
  mORMot, // just for object serialization in logs
  SynProtoRTSPHTTP;

{$R *.res}

const
  CONCURRENT = 500;
  // one socket consumes two file descriptors, so each stream opens 6 files;
  // under Linux with "ulimit -H -n" = 4096, maximum is 4096/6 = 680 streams:
  // add "* hard nofiles 65535" to /etc/security/limits.conf to make it higher

{
  Some rough numbers, on various Operating Systems:

  CONCURRENT  OS             API      Time      Sockets  Polled   Steps
  100         Windows XP     select   190 ms    300      200      10
  100         Windows Seven  select   190 ms    300      200      10
  100         Linux          poll     200 ms    300      200      10
  100         Linux          epoll    190 ms    300      200      10
  500         Windows XP     select   544 ms    1500     1000     10
  500         Windows Seven  select   990 ms    1500     1000     10
  500         Linux          poll     380 ms    1500     1000     10
  500         Linux          epoll    344 ms    1500     1000     10
  5000        Windows XP               N/A
  5000        Windows Seven  select   27.61 s   15000    10000    10
  5000        Windows Seven  WSAPoll  33.70 s   15000    10000    10
  5000        Linux          poll     2.70 s    15000    10000    10
  5000        Linux          epoll    2.59 s    15000    10000    10
  10000       Windows XP               N/A
  10000       Windows Seven  select   116.32 s  30000    20000    10
  10000       Windows Seven  WSAPoll  118.23 s  30000    20000    10
  10000       Linux          poll     9.48 s    30000    20000    10
  10000       Linux          epoll    9.33 s    30000    20000    10
  10000       Linux          epoll    65.1 s    30000    20000    100
  10000       Linux          epoll    10 min    30000    20000    1000
  10000       Linux          epoll    20 min    30000    20000    2000

  Purpose of this test is to create a given number of concurrent GET/POST HTTP
  requests, creating one RTSP connection each. Then we run POST and GET small
  operations on all connections, in a loop, and check the proper RTSP transfer.
  So here polling will have most of the sockets notified with a few pending
  bytes proxied during each loop, which is probably the worse case possible.
  Above numbers are published to give a performance idea of this micro-benchmark
  testing, on various systems and APIs.

  All process did take place with logs enabled, on the same physical PC.
  Note that the Windows Seven native system (not a VM) may be slow down by its
  AntiVirus software, whereas the XP VM did not have any AntiVirus installed.
  WSAPoll API was very disapointing: it is slightly slower than plain Select!
  In the future, we will eventually uses the IOCP API on Windows, which is told
  to be much faster (but also much more difficult to implement).
  Memory consumption was similar on all OS and API methods.

  In all cases, the Linux VM with poll/epoll did show the best scaling abilities.
  The latest test case, creating a lot of traffic, was very stable about its
  CPU and memory consumption (most time spent in the kernel), and reported
  "reads=22,520,000 (1 GB) writes=2,510,000 (426 MB)" impressive statistics. 
}

var
  server: TRTSPOverHTTPServer;
  timer: TPrecisionTimer;
  clients, steps: integer;
begin
  if (paramcount = 0) or not TryStrToInt(paramstr(1), clients) then
    clients := CONCURRENT
  else if (paramcount = 1) or not TryStrToInt(paramstr(2), steps) then
    steps := 10;
  TSynLog.Family.HighResolutionTimeStamp := true;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOnFile;
  if steps<200 then
    TSynLog.Family.Level := LOG_VERBOSE
  else
    TSynLog.Family.Level := LOG_STACKTRACE + [sllCustom1];
  TSynLog.Family.EchoToConsole := LOG_STACKTRACE + [sllCustom1];
  server := TRTSPOverHTTPServer.Create('127.0.0.1', '4999', '4998', TSynLog, nil, nil);
  try
    //server.Clients.Options := [paoWritePollOnly];
    //server.Options := [acoVerboseLog];
    writeln(server.ClassName, ' running');
    writeln('  performing tests with ', clients, ' concurrent streams using ',
      server.Clients.PollRead.PollClass.ClassName, #10);
    timer.Start;
    server.RegressionTests(nil, clients, steps);
    writeln(#10'  tests finished in ', timer.Stop);
    {$ifdef MSWINDOWS}
    writeln('Press [Enter] to close server.');
    Readln;
    {$endif}
  finally
    server.Free;
  end;
end.
