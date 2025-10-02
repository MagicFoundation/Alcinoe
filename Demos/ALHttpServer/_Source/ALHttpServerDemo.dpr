program ALHttpServerDemo;

{$APPTYPE CONSOLE}

{$R *.res}

{$I Alcinoe.inc}

uses
  {$IFDEF DEBUG}
  FastMM5,
  {$ENDIF}
  System.SysUtils,
  System.IOUtils,
  System.AnsiStrings,
  Alcinoe.HTTP,
  Alcinoe.HTTP.Server,
  Alcinoe.HTTP.Server.HttpSys,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.Files,
  Alcinoe.Common;

var

  {*******************************}
  MainHttpServer: TALHttpSysServer;

Type

  {*************************}
  THttpRequestHandler = class
  public
    class procedure OnRequest(Const ARequest: TALHttpServerRequest; Const AResponse: TALHttpServerResponse);
  end;

{**************************************************************************************************************************}
class procedure THttpRequestHandler.OnRequest(const ARequest: TALHttpServerRequest; const AResponse: TALHttpServerResponse);
begin

  {$REGION '/hello'}
  If ALSameTextA(ARequest.CookedUrl.Path, '/hello') then begin
    AResponse.BodyString :=
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
    AResponse.Headers.ContentType := 'text/html; charset=utf-8';
  end
  {$ENDREGION}

  {$REGION '/stats'}
  else If ALSameTextA(ARequest.CookedUrl.Path, '/stats') then begin
    AResponse.BodyString :=
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
      ''' +
      '<b>Worker Thread Count:</b> ' + ALIntToStrA(MainHttpServer.WorkerThreadCount) + '<br />' + #13#10 +
      '<b>Peak Worker Thread Count:</b> ' + ALIntToStrA(MainHttpServer.PeakWorkerThreadCount) + '<br />' + #13#10 +
      '<b>Busy Worker Thread Count:</b> ' + ALIntToStrA(MainHttpServer.BusyWorkerThreadCount) + '<br />' + #13#10 +
      '<b>Peak Busy Worker Thread Count:</b> ' + ALIntToStrA(MainHttpServer.PeakBusyWorkerThreadCount) + '<br />' + #13#10 +
      '<b>IOCP Pending Initial Receive Count:</b> ' + ALIntToStrA(MainHttpServer.IOCPPendingInitialReceiveCount) + '<br />' + #13#10 +
      '<b>IOCP Pending Continuation Count:</b> ' + ALIntToStrA(MainHttpServer.IOCPPendingContinuationCount) + '<br />' + #13#10 +
      '''
      </body>
      </html>
      ''';
  end
  {$ENDREGION}

  {$REGION '/echo'}
  else If ALSameTextA(ARequest.CookedUrl.Path, '/echo') then begin
    var LRawHeaders := TALStringListA.Create;
    Try
      LRawHeaders.NameValueSeparator := ':';
      LRawHeaders.Text := ARequest.Headers.RawHeaderText;
      var LHttpVersionStr: AnsiString;
      case ARequest.Version of
        TALHttpVersion.v0_9: LHttpVersionStr := '0.9';
        TALHttpVersion.v1_0: LHttpVersionStr := '1.0';
        TALHttpVersion.v1_1: LHttpVersionStr := '1.1';
        TALHttpVersion.v2: LHttpVersionStr := '2';
        TALHttpVersion.v3: LHttpVersionStr := '3';
        else LHttpVersionStr := '';
      end;
      for var I := 0 to LRawHeaders.Count - 1 do
        LRawHeaders[i] := '<b>' + LRawHeaders.Names[I] + '</b>: ' + ALTrim(LRawHeaders.ValueFromIndex[i]) + '<br/>';
      AResponse.BodyString :=
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
        ''' +
        '<b>' + ARequest.Verb + ' ' + ARequest.RawUrl + ' HTTP/' + LHttpVersionStr + '</b><br />' + #13#10 +
        LRawHeaders.Text +
        '''
        </body>
        </html>
        ''';
    Finally
      ALFreeAndNil(LRawHeaders);
    End;
    AResponse.Headers.ContentType := 'text/html; charset=utf-8';
  end
  {$ENDREGION}

  {$REGION '/cache'}
  else If ALSameTextA(ARequest.CookedUrl.Path, '/cache') then begin
    Aresponse.CachePolicyType := TALHttpServerResponse.TCachePolicyType.TimeToLive;
    Aresponse.CacheSecondsToLive := 60;
    AResponse.Headers.ContentType := 'text/html; charset=UTF-8';
    AResponse.BodyString :=
      '''
      <!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="UTF-8">
        <title>Cache Demo</title>
        <style>
          body { font-family: sans-serif; margin: 2em; }
          .marker { font-weight: bold; color: darkblue; }
        </style>
      </head>
      <body>
        <h1>Cache Demo for <code>/cache</code></h1>
        <p>
          Random marker generated by server:
          <span class="marker">
      ''' +
      AnsiString(Random.ToString) +
      '''
          </span>
        </p>
        <p>
          If http.sys kernel cache is working, this number will stay the same
          until the cache expires (<code>CacheSecondsToLive=60</code>).
        </p>
      </body>
      </html>
      ''';
  end
  {$ENDREGION}

  {$REGION '/1mb'}
  else If ALSameTextA(ARequest.CookedUrl.Path, '/1mb') then begin
    AResponse.Headers.ContentType := 'text/plain';
    AResponse.BodyString := ALRandomStrA(1_000_000);
  end
  {$ENDREGION}

  {$REGION '/10mb'}
  else If ALSameTextA(ARequest.CookedUrl.Path, '/10mb') then begin
    AResponse.Headers.ContentType := 'text/plain';
    AResponse.BodyString := ALRandomStrA(10_000_000);
  end
  {$ENDREGION}

  {$REGION '404'}
  else
    Aresponse.StatusCode := 404;
  {$ENDREGION}

end;

begin

  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

  try

    MainHttpServer := TALHttpSysServer.Create;
    try
      MainHttpServer.OnRequest := THttpRequestHandler.OnRequest;
      MainHttpServer.LoggingInfo.Enabled := True;
      MainHttpServer.LoggingInfo.SoftwareName := 'ALHttpServerDemo';
      MainHttpServer.LoggingInfo.DirectoryName := ALGetModulePathW + 'Log\';
      If not TDirectory.Exists(MainHttpServer.LoggingInfo.DirectoryName) then
        TDirectory.CreateDirectory(MainHttpServer.LoggingInfo.DirectoryName);
      MainHttpServer.UrlPrefixes.Add('http://+:23456/');

      Write('Enable HTTPS on port 34567 (required for HTTP/2/HTTP/3)? [Y/n] ');
      var LAnswer: string := '';
      ReadLn(LAnswer);
      LAnswer := ALTrim(ALLowerCase(LAnswer));
      if (LAnswer = '') or (LAnswer = 'y') or (LAnswer = 'yes') or (LAnswer = '1') or (LAnswer = 'true') then
        MainHttpServer.UrlPrefixes.Add('https://+:34567/')
      else
        Writeln('Note: Without HTTPS, clients will only negotiate HTTP/1.1.');
      Writeln('');

      Writeln('===============================================================');
      Writeln('  ALHttpServerDemo');
      Writeln('  Engine   : http.sys (kernel-mode)  |  IOCP');
      Writeln('  Protocols: HTTP/1.1 · HTTP/2 · HTTP/3 (QUIC)');
      Writeln('  Listening on:');
      Writeln('    ' + ALStringReplaceW(string(MainHttpServer.UrlPrefixes.Text), '+', 'localhost', [RfReplaceALL]).Trim.Replace(#13#10, #13#10'    '));
      Writeln('  Test endpoints:');
      Writeln('    /hello   → returns plain text "hello world"');
      Writeln('    /stats   → live worker/IOCP counters');
      Writeln('    /echo    → reflects request line + headers');
      Writeln('    /cache   → 60s kernel cache demo');
      Writeln('    /1mb     → returns a 1 MB payload');
      Writeln('    /10mb    → returns a 10 MB payload');
      Writeln('  Logging (W3C): ', MainHttpServer.LoggingInfo.DirectoryName);
      Writeln('  Press Ctrl+C or hit <Enter> to stop');
      Writeln('===============================================================');

      MainHttpServer.Start;
      readln;
      MainHttpServer.Stop;
    finally
      ALFreeAndNil(MainHttpServer);
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.