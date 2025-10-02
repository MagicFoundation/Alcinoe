program mORMotHttpServer;

{
  Some numbers on my dev PC (Core i5-13500 running Debian 12 at that time):

  usr@pc:~ sudo apt install wrk
  usr@pc:~ wrk -c 1024 -t 8 http://localhost:8888/echo
  Running 10s test @ http://localhost:8888/echo
    8 threads and 1024 connections
    Thread Stats   Avg      Stdev     Max   +/- Stdev
      Latency   612.25us  566.42us  31.92ms   99.69%
      Req/Sec   184.44k    12.24k  217.95k    71.12%
    14680687 requests in 10.10s, 2.16GB read
  Requests/sec: 1453501.13
  Transfer/sec:    219.01MB

  usr@pc:~ sudo apt install apache2-utils
  usr@pc:~ ab -t 20 -n 100000 -c 1000 -k http://localhost:8888/echo
  This is ApacheBench, Version 2.3 <$Revision: 1903618 $>

  Benchmarking localhost (be patient)
  Finished 100000 requests

  Server Software:        mORMot2
  Server Hostname:        localhost
  Server Port:            8888

  Document Path:          /echo
  Document Length:        53 bytes

  Concurrency Level:      1000
  Time taken for tests:   0.313 seconds
  Complete requests:      100000
  Failed requests:        0
  Keep-Alive requests:    100000
  Total transferred:      18200000 bytes
  HTML transferred:       5300000 bytes
  Requests per second:    319062.72 [#/sec] (mean)
  Time per request:       3.134 [ms] (mean)
  Time per request:       0.003 [ms] (mean, across all concurrent requests)
  Transfer rate:          56708.41 [Kbytes/sec] received

  Connection Times (ms)
                min  mean[+/-sd] median   max
  Connect:        0    0   1.0      0      17
  Processing:     1    3   0.2      3      17
  Waiting:        0    3   0.2      3       5
  Total:          1    3   1.1      3      22

}

{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R ..\..\src\mormot.win.default.manifest.res}
{$endif OSWINDOWS}

uses
  {$I mormot.uses.inc} // may include mormot.core.fpcx64mm.pas
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.log,
  mormot.core.text,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.server,
  mormot.net.async;

  
type
  TSimpleHttpAsyncServer = class
  private
    fHttpServer: THttpServerSocketGeneric;
  protected
    // this is where the process would take place
    function DoOnRequest(Ctxt: THttpServerRequestAbstract): cardinal;
  public
    constructor Create;
    destructor Destroy; override;
  end;


{ TSimpleHttpAsyncServer }

{********************************************************************************************}
function  ALRandomStrA(const aLength: Longint; const aCharset: Array of ansiChar): AnsiString; overload;
var X: Longint;
    P: Pansichar;
begin
  if aLength <= 0 then exit('');
  SetLength(Result, aLength);
  P := pansiChar(Result);
  for X:=1 to aLength do begin
    P^ := aCharset[Random(length(aCharset))];
    inc(P);
  end;
end;

{********************************************************}
function ALRandomStrA(const aLength: Longint): AnsiString; overload;
begin
  Result := ALRandomStrA(aLength,['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']);
end;

function TSimpleHttpAsyncServer.DoOnRequest(Ctxt: THttpServerRequestAbstract): cardinal;
begin

  {$REGION '/hello'}
  If SameText(String(Ctxt.Url), '/hello') then begin
    Ctxt.OutContent :=
      '''
      <!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="UTF-8">
        <style>
          body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
            font-size: 18px;
            color: #333;
            margin: 20px;
            line-height: 1.4;
          }
        </style>
      </head>
      <body>
        Hello World
      </body>
      </html>
      ''';
    Ctxt.OutContentType := 'text/html; charset=utf-8';
    result := HTTP_SUCCESS;
  end
  {$ENDREGION}

  {$REGION '/1mb'}
  else If SameText(String(Ctxt.Url), '/1mb') then begin
    Ctxt.OutContentType := 'text/plain';
    Ctxt.OutContent := ALRandomStrA(1_000_000);
    result := HTTP_SUCCESS;
  end
  {$ENDREGION}

  {$REGION '/10mb'}
  else If SameText(String(Ctxt.Url), '/10mb') then begin
    Ctxt.OutContentType := 'text/plain';
    Ctxt.OutContent := ALRandomStrA(10_000_000);
    result := HTTP_SUCCESS;
  end
  {$ENDREGION}

  {$REGION '404'}
  else
    result := HTTP_NOTFOUND;
  {$ENDREGION}

end;

constructor TSimpleHttpAsyncServer.Create;
begin
  inherited Create;
  fHttpServer := THttpAsyncServer.Create(
    '8888', nil, nil, '', SystemInfo.dwNumberOfProcessors + 1, 30000,
    [hsoNoXPoweredHeader,
     hsoNoStats,
     hsoHeadersInterning,
     hsoThreadSmooting
    //, hsoLogVerbose
    ]);
  //writeln('DropPriviledges=', DropPriviledges('abouchez'));
  fHttpServer.HttpQueueLength := 100000; // needed e.g. from wrk/ab benchmarks
  fHttpServer.OnRequest := DoOnRequest;
  fHttpServer.WaitStarted; // raise exception e.g. on binding issue
end;

destructor TSimpleHttpAsyncServer.Destroy;
begin
  fHttpServer.Free;
  inherited Destroy;
end;

var
  simpleServer: TSimpleHttpAsyncServer;

begin
  with TSynLog.Family do
    begin
      Level :=
        //LOG_VERBOSE +
        LOG_FILTER[lfErrors];
      //EchoToConsole := Level;
      PerThreadLog := ptIdentifiedInOneFile;
      HighResolutionTimestamp := true;
      AutoFlushTimeOut := 1;
    end;

  simpleServer := TSimpleHttpAsyncServer.Create();
  try
    {$I-}
    writeln;
    TextColor(ccLightGreen);
    writeln(simpleServer.fHttpServer.ClassName, ' running on localhost:8888'#10);
    TextColor(ccWhite);
    writeln('try curl http://localhost:8888/hello');
    writeln('try curl http://localhost:8888/1mb');
    writeln('try curl http://localhost:8888/10mb'#10);
    TextColor(ccLightGray);
    writeln('Press [Enter] to quit'#10);
    TextColor(ccCyan);
    ConsoleWaitForEnterKey;
    writeln(ObjectToJson(simpleServer.fHttpServer, [woHumanReadable]));
    TextColor(ccLightGray);
    {$ifdef FPC_X64MM}
    WriteHeapStatus(' ', 16, 8, {compileflags=}true);
    {$endif FPC_X64MM}
  finally
    simpleServer.Free;
  end;

end.

