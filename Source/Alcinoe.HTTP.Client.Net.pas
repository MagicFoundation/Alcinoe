unit Alcinoe.HTTP.Client.Net;

interface

{$I Alcinoe.inc}

uses
  System.Net.HttpClient,
  System.net.URLClient;

Function ALCreateNetHTTPClient(Const AAllowcookies: Boolean = False): THttpClient;
function ALAcquireKeepAliveNetHttpClient(const AURI: TUri): THTTPClient;
procedure ALReleaseKeepAliveNetHttpClient(const AURI: TUri; var AHTTPClient: THTTPClient);
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
  ALNetHttpClientKeepAlives: TObjectDictionary<String, TobjectList<THTTPClient>>;

{********************************************************************************}
Function ALCreateNetHTTPClient(Const AAllowcookies: Boolean = False): THttpClient;
Begin
  Result := THttpClient.Create;
  Result.AllowCookies := AAllowcookies;
  Result.AutomaticDecompression := [THTTPCompressionMethod.Any];
  Result.UserAgent := 'ALNetHTTPClient/1.0';
  Result.ConnectionTimeout := ALCreateHttpClientConnectTimeout;
  Result.ResponseTimeout := ALCreateHttpClientReceiveTimeout;
  Result.SendTimeout := ALCreateHttpClientSendTimeout;
end;

{**********************************************************************}
function ALAcquireKeepAliveNetHttpClient(const AURI: TUri): THTTPClient;
begin
  ALMonitorEnter(ALNetHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALAcquireKeepAliveNetHttpClient'{$ENDIF});
  try
    var LList: TobjectList<THTTPClient>;
    if ALNetHttpClientKeepAlives.TryGetValue(AlLowerCase(AURI.Scheme) + '://' + AlLowerCase(AURI.Host) + ':' + ALIntToStrW(AURI.port), LList) then begin
      if LList.Count > 0 then result := LList.ExtractItem(LList.Last, TDirection.FromEnd)
      else result := ALCreateNetHTTPClient;
    end
    else result := ALCreateNetHTTPClient;
  finally
    ALMonitorExit(ALNetHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALAcquireKeepAliveNetHttpClient'{$ENDIF});
  end;
end;

{****************************************************************************************}
procedure ALReleaseKeepAliveNetHttpClient(const AURI: TUri; var AHTTPClient: THTTPClient);
begin
  ALMonitorEnter(ALNetHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALReleaseKeepAliveNetHttpClient'{$ENDIF});
  try
    var LList: TobjectList<THTTPClient>;
    if ALNetHttpClientKeepAlives.TryGetValue(AlLowerCase(AURI.Scheme) + '://' + AlLowerCase(AURI.Host) + ':' + ALIntToStrW(AURI.port), LList) then begin
      while LList.Count >= ALMaxKeepAliveHttpClientPerHost do
        LList.Delete(0);
      LList.Add(AHTTPClient);
      AHTTPClient := nil;
    end
    else begin
      LList := TobjectList<THTTPClient>.create(true{aOwnObject});
      try
        LList.Add(AHTTPClient);
        AHTTPClient := nil;
        if not ALNetHttpClientKeepAlives.TryAdd(AlLowerCase(AURI.Scheme) + '://' + AlLowerCase(AURI.Host) + ':' + ALIntToStrW(AURI.port), LList) then ALFreeAndNil(LList);
      except
        ALFreeAndNil(LList);
        raise;
      end;
    end;
  finally
    ALMonitorExit(ALNetHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALReleaseKeepAliveNetHttpClient'{$ENDIF});
  end;
end;

{********************************************}
procedure ALReleaseAllKeepAliveNetHttpClients;
begin
  ALMonitorEnter(ALNetHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALReleaseAllKeepAliveNetHttpClients'{$ENDIF});
  try
    ALNetHttpClientKeepAlives.Clear;
  finally
    ALMonitorExit(ALNetHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALReleaseAllKeepAliveNetHttpClients'{$ENDIF});
  end;
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.HTTP.Client.Net','initialization');
  {$ENDIF}
  ALNetHttpClientKeepAlives := TObjectDictionary<String, TobjectList<THTTPClient>>.create([doOwnsValues]);

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.HTTP.Client.Net','finalization');
  {$ENDIF}
  ALFreeAndNil(ALNetHttpClientKeepAlives);

end.