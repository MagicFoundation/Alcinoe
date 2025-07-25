unit Alcinoe.HTTP.Client.Net;

interface

{$I Alcinoe.inc}

uses
  System.Net.HttpClient,
  System.net.URLClient;

Function ALCreateNetHTTPClient(Const aAllowcookies: Boolean = False): THttpClient;
function ALAcquireKeepAliveNetHttpClient(const aURI: TUri): THTTPClient;
procedure ALReleaseKeepAliveNetHttpClient(const aURI: TUri; var aHTTPClient: THTTPClient);
procedure ALReleaseAllKeepAliveNetHttpClients;

implementation

uses
  System.NetConsts,
  System.SysUtils,
  System.Types,
  System.Generics.Collections,
  Alcinoe.Common,
  Alcinoe.HTTP.Client,
  Alcinoe.StringUtils;

{*}
var
  _ALNetHttpClientKeepAlives: TObjectDictionary<String, TobjectList<THTTPClient>>;

{********************************************************************************}
Function ALCreateNetHTTPClient(Const aAllowcookies: Boolean = False): THttpClient;
Begin
  Result := THttpClient.Create;
  Result.AllowCookies := aAllowcookies;
  Result.Accept := 'text/html, */*';
  {$IF not Defined(ALHttpGzipAuto)}
  //http://developer.android.com/reference/java/net/HttpURLConnection.html
  //By default, this implementation of HttpURLConnection requests that servers use gzip compression and
  //it automatically decompresses the data for callers of getInputStream(). The Content-Encoding and
  //Content-Length response headers are cleared in this case. Gzip compression can be disabled by
  //setting the acceptable encodings in the request header
  //it's the same for IOS and OSX
  Result.AcceptEncoding := 'gzip';
  {$ENDIF}
  Result.UserAgent := 'Mozilla/5.0 (Windows NT 10.0; Win64; x64)';
  Result.ConnectionTimeout := ALCreateHttpClientConnectTimeout;
  Result.ResponseTimeout := ALCreateHttpClientReceiveTimeout;
  Result.SendTimeout := ALCreateHttpClientSendTimeout;
end;

{**********************************************************************}
function ALAcquireKeepAliveNetHttpClient(const aURI: TUri): THTTPClient;
var LList: TobjectList<THTTPClient>;
begin
  ALMonitorEnter(_ALNetHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALAcquireKeepAliveNetHttpClient'{$ENDIF});
  try
    if _ALNetHttpClientKeepAlives.TryGetValue(AlLowerCase(aURI.Scheme) + '://' + AlLowerCase(aURI.Host) + ':' + ALIntToStrW(aURI.port), LList) then begin
      if LList.Count > 0 then result := LList.ExtractItem(LList.Last, TDirection.FromEnd)
      else result := ALCreateNetHTTPClient;
    end
    else result := ALCreateNetHTTPClient;
  finally
    ALMonitorExit(_ALNetHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALAcquireKeepAliveNetHttpClient'{$ENDIF});
  end;
end;

{****************************************************************************************}
procedure ALReleaseKeepAliveNetHttpClient(const aURI: TUri; var aHTTPClient: THTTPClient);
var LList: TobjectList<THTTPClient>;
begin
  ALMonitorEnter(_ALNetHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALReleaseKeepAliveNetHttpClient'{$ENDIF});
  try
    if _ALNetHttpClientKeepAlives.TryGetValue(AlLowerCase(aURI.Scheme) + '://' + AlLowerCase(aURI.Host) + ':' + ALIntToStrW(aURI.port), LList) then begin
      while LList.Count >= ALMaxKeepAliveHttpClientPerHost do
        LList.Delete(0);
      LList.Add(aHTTPClient);
      aHTTPClient := nil;
    end
    else begin
      LList := TobjectList<THTTPClient>.create(true{aOwnObject});
      try
        LList.Add(aHTTPClient);
        aHTTPClient := nil;
        if not _ALNetHttpClientKeepAlives.TryAdd(AlLowerCase(aURI.Scheme) + '://' + AlLowerCase(aURI.Host) + ':' + ALIntToStrW(aURI.port), LList) then ALFreeAndNil(LList);
      except
        ALFreeAndNil(LList);
        raise;
      end;
    end;
  finally
    ALMonitorExit(_ALNetHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALReleaseKeepAliveNetHttpClient'{$ENDIF});
  end;
end;

{********************************************}
procedure ALReleaseAllKeepAliveNetHttpClients;
begin
  ALMonitorEnter(_ALNetHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALReleaseAllKeepAliveNetHttpClients'{$ENDIF});
  try
    _ALNetHttpClientKeepAlives.Clear;
  finally
    ALMonitorExit(_ALNetHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALReleaseAllKeepAliveNetHttpClients'{$ENDIF});
  end;
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.HTTP.Client.Net','initialization');
  {$ENDIF}
  _ALNetHttpClientKeepAlives := TObjectDictionary<String, TobjectList<THTTPClient>>.create([doOwnsValues]);

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.HTTP.Client.Net','finalization');
  {$ENDIF}
  ALFreeAndNil(_ALNetHttpClientKeepAlives);

end.
