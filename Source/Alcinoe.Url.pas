unit Alcinoe.Url;

interface

{$I Alcinoe.inc}

uses
  Alcinoe.StringList;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCookedUrlA = Class(TObject)
  private
    FScheme: AnsiString;
    FHost: AnsiString;
    FPort: Integer;
    FUserName: AnsiString;
    FPassword: AnsiString;
    FPath: AnsiString;
    FQueryString: AnsiString;
    FQueryParams: TALNVStringListA;
    FAnchor: AnsiString;
    function GetQueryString: AnsiString;
    procedure SetQueryString(const AValue: AnsiString);
    function GetQueryParams: TALStringsA;
    procedure QueryParamsChanged(Sender: TObject);
  public
    constructor Create(const AUrl: AnsiString; Const ADecode: Boolean = False; Const APlusAsSpaces: Boolean = True); virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure CrackUrl(const AUrl: AnsiString; Const ADecode: Boolean = False; Const APlusAsSpaces: Boolean = True); virtual;
    function GetFullUrl(const AEncode: Boolean = False; Const ASpacesAsPlus: Boolean = True): AnsiString;
    property Scheme: AnsiString read FScheme write FScheme;
    property Host: AnsiString read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property UserName: AnsiString read FUserName write FUserName;
    property Password: AnsiString read FPassword write FPassword;
    property Path: AnsiString read FPath write FPath;
    property QueryString: AnsiString read GetQueryString write SetQueryString;
    property QueryParams: TALStringsA read GetQueryParams;
    property Anchor: AnsiString read FAnchor write FAnchor;
  end;

Function  AlIsHttpOrHttpsUrl(const AUrl: AnsiString): Boolean; overload;
Function  AlIsHttpOrHttpsUrl(const AUrl: String): Boolean; overload;
{$IFDEF MSWINDOWS}
function  AlCombineUrlA(const ARelativeUrl: AnsiString; const ABaseUrl: AnsiString): AnsiString;
{$ENDIF}

function ALUrlEncode(const AStr: AnsiString; Const ASpacesAsPlus: Boolean = True): AnsiString; overload;
function ALUrlEncode(const AStr: String; Const ASpacesAsPlus: Boolean = True): String; overload;
function ALUrlDecode(const AStr: AnsiString; const APlusAsSpaces: Boolean = True): AnsiString; overload;
function ALUrlDecode(const AStr: String; const APlusAsSpaces: Boolean = True): String; overload;

implementation

uses
  System.Math,
  System.AnsiStrings,
  System.SysUtils,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.Wininet,
  {$ENDIF}
  Alcinoe.Common,
  Alcinoe.StringUtils;

{****************************************************************************************************************************}
constructor TALCookedUrlA.Create(const AUrl: AnsiString; Const ADecode: Boolean = False; Const APlusAsSpaces: Boolean = True);
begin
  inherited create;
  FQueryParams := nil;
  CrackUrl(AUrl, ADecode, APlusAsSpaces);
end;

{*******************************}
destructor TALCookedUrlA.Destroy;
begin
  ALFreeAndNil(FQueryParams);
  inherited;
end;

{****************************}
procedure TALCookedUrlA.Clear;
begin
  FScheme := '';
  FHost := '';
  FPort := 0;
  FUserName := '';
  FPassword := '';
  FPath := '';
  FQueryString := '';
  ALFreeAndNil(FQueryParams);
  FAnchor := '';
end;

{****************************************************************************************************************************}
procedure TALCookedUrlA.CrackUrl(const AUrl: AnsiString; Const ADecode: Boolean = False; Const APlusAsSpaces: Boolean = True);
begin

  Clear;
  if AUrl = '' then exit;
  var P1,P2,P3,P4: Integer;

  {$REGION 'Scheme'}
  P1 := ALPosA('://', AUrl);  // ftp://xxxx:yyyyy@[2001:db8::1]:8443/path/filename.xxx?param1=value1#anchor
                              //    ^P1
  If P1 > 0 then begin
    FScheme := AlCopyStr(AUrl, 1, P1-1); // ftp
    P1 := P1 + 3; // ftp://xxxx:yyyyy@[2001:db8::1]:8443/path/filename.xxx?param1=value1#anchor
                  //       ^P1
  end
  else
    P1 := 1;

  var LPathPos := ALPosA('/',AUrl); // ftp://xxxx:yyyyy@[2001:db8::1]:8443/path/filename.xxx?param1=value1#anchor
                                    //     ^ ^P1
                                    //     ^LPathPos
  if LPathPos = P1 - 2 then LPathPos := ALPosA('/',AUrl, P1); // ftp://xxxx:yyyyy@[2001:db8::1]:8443/path/filename.xxx?param1=value1#anchor
                                                              //                                    ^LPathPos
  var LQueryPos := AlPosA('?',AUrl); // ftp://xxxx:yyyyy@[2001:db8::1]:8443/path/filename.xxx?param1=value1#anchor
                                     //       ^P1                          ^LPathPos         ^LQueryPos
  var LAnchorPos := AlPosA('#',AUrl);  // ftp://xxxx:yyyyy@[2001:db8::1]:8443/path/filename.xxx?param1=value1#anchor
                                       //       ^P1                          ^LPathPos         ^LQueryPos    ^LAnchorPos
  if (LPathPos <= 0) then LPathPos := length(AUrl)+1;
  if (LQueryPos <= 0) then LQueryPos := length(AUrl)+1;
  if (LAnchorPos <= 0) then LAnchorPos := length(AUrl)+1;

  P2 := MinIntValue([LPathPos,LQueryPos,LAnchorPos]); // ftp://xxxx:yyyyy@[2001:db8::1]:8443/path/filename.xxx?param1=value1#anchor
                                                      //       ^P1                          ^P2
  If P1 > P2 then begin
    FScheme := '';
    P1 := 1;
  end;
  {$ENDREGION}

  {$REGION 'UserName/Password'}
  if FScheme <> '' then begin
    P3 := AlposA('@',AUrl, P1); // ftp://xxxx:yyyyy@[2001:db8::1]:8443/path/filename.xxx?param1=value1#anchor
                                //       ^P1       ^P3                ^P2
    If (P3 > 0) and (P3 < P2) then begin
      P4 := ALPosA(':', AUrl, P1); // ftp://xxxx:yyyyy@[2001:db8::1]:8443/path/filename.xxx?param1=value1#anchor
                                   //       ^P1 ^P4   ^P3                ^P2
      if (P4 > 0) and (P4 < P3) then begin
        FUserName := ALcopyStr(AUrl, P1, P4-P1);
        inc(P4); // ftp://xxxx:yyyyy@[2001:db8::1]:8443/path/filename.xxx?param1=value1#anchor
                 //       ^P1  ^P4  ^P3                ^P2
        FPassword := ALcopyStr(AUrl, P4, P3-P4);
      end
      else
        FUserName := ALcopyStr(AUrl, P1, P3-P1);
      P1 := P3 + 1; // ftp://xxxx:yyyyy@[2001:db8::1]:8443/path/filename.xxx?param1=value1#anchor
                    //                  ^P1               ^P2
    end;
  end;
  {$ENDREGION}

  {$REGION 'Host/Port'}
  if FScheme <> '' then begin
    P3 := AlposA(']',AUrl, P1); // ftp://xxxx:yyyyy@[2001:db8::1]:8443/path/filename.xxx?param1=value1#anchor
                                //                  ^P1         ^P3   ^P2
    if (P3 > 0) and (P3 < P2) then P4 := ALposA(':', AUrl, P3+1) // ftp://xxxx:yyyyy@[2001:db8::1]:8443/path/filename.xxx?param1=value1#anchor
    else P4 := ALposA(':', AUrl, P1);                            //                  ^P1          ^P4  ^P2
    if (P4 > 0) and (P4 < P2) then begin
      FHost := ALcopyStr(AUrl, P1, P4-P1);
      inc(P4); // ftp://xxxx:yyyyy@[2001:db8::1]:8443/path/filename.xxx?param1=value1#anchor
               //                  ^P1           ^P4 ^P2
      If not ALTryStrToInt(ALcopyStr(AUrl, P4, P2-P4), FPort) then
        FPort := 0;
    end
    else begin
      FHost := ALcopyStr(AUrl, P1, P2-P1);
      if ALSameTextA(FScheme, 'http') then FPort := 80
      else if ALSameTextA(FScheme, 'https') then FPort := 443
      else if ALSameTextA(FScheme, 'ws') then FPort := 80
      else if ALSameTextA(FScheme, 'wss') then FPort := 443
      else if ALSameTextA(FScheme, 'ftp') then FPort := 21
    end;
    P1 := P2; // ftp://xxxx:yyyyy@[2001:db8::1]:21/path/filename.xxx?param1=value1#anchor
              //                                  ^P1
  end;
  {$ENDREGION}

  {$REGION 'Anchor'}
  P2 := AlposA('#',AUrl, P1); // ftp://xxxx:yyyyy@[2001:db8::1]:21/path/filename.xxx?param1=value1#anchor
                              //                                  ^P1                             ^P2
  if P2 > 0 then FAnchor := ALCopyStr(AUrl, P2+1, MaxInt)
  else P2 := Length(AUrl) + 1;
  {$ENDREGION}

  {$REGION 'QueryString'}
  P3 := AlposA('?',AUrl, P1); // ftp://xxxx:yyyyy@[2001:db8::1]:21/path/filename.xxx?param1=value1#anchor
                              //                                  ^P1               ^P3           ^P2
  if (P3 > 0) and (P3 < P2) then FQueryString := ALCopyStr(AUrl, P3+1, P2-P3-1)
  else P3 := P2;
  {$ENDREGION}

  {$REGION 'Path'}
  FPath := ALCopyStr(AUrl, P1, P3-P1);
  {$ENDREGION}

  if ADecode then begin
    FUserName := ALUrlDecode(FUserName, False{APlusAsSpaces});
    FPassword := ALUrlDecode(FPassword, False{APlusAsSpaces});
    FPath := ALUrlDecode(FPath, False{APlusAsSpaces});
    var LCount := QueryParams.Count;
    TALNVStringListA(QueryParams).onchanged := nil;
    Try
      For var I := 0 to LCount - 1 do begin
        if QueryParams.ItemHasNameValue(I) then
          QueryParams.AddNameValue(
            ALUrlDecode(QueryParams.Names[I], APlusAsSpaces),
            ALUrlDecode(QueryParams.ValueFromIndex[I], APlusAsSpaces))
        else
          QueryParams.Add(ALUrlDecode(QueryParams[I], APlusAsSpaces));
      end;
      While LCount > 0 do begin
        QueryParams.Delete(0);
        dec(LCount);
      end;
    Finally
      TALNVStringListA(QueryParams).onchanged := QueryParamsChanged;
    End;
    FAnchor := ALUrlDecode(FAnchor, False{APlusAsSpaces});
  end;

end;

{*****************************************************************************************************************}
function TALCookedUrlA.GetFullUrl(const AEncode: Boolean = False; Const ASpacesAsPlus: Boolean = True): AnsiString;
begin
  Var LLength := length(FScheme) + 3{://} +
                 length(FHost) +
                 {:FPort} 6 +
                 length(FUserName) + 1{@} +
                 length(FPassword) + 1{:} +
                 length(FPath) +
                 length(QueryString) + 1{?} +
                 length(FAnchor) + 1{#};
  if AEncode then LLength := LLength * 3;
  var SB := TALStringBuilderA.Create(LLength);
  try

    {$REGION 'Encode'}
    if AEncode then begin
      If (FScheme <> '') then begin
        SB.Append(FScheme);
        SB.Append('://');
      end;
      If (FUserName <> '') and (FPassword <> '') then begin
        SB.Append(ALUrlEncode(FUserName, false{ASpacesAsPlus}));
        SB.Append(':');
        SB.Append(ALUrlEncode(FPassword, false{ASpacesAsPlus}));
        SB.Append('@');
      end
      else If (FUserName <> '') then begin
        SB.Append(ALUrlEncode(FUserName, false{ASpacesAsPlus}));
        SB.Append('@');
      end;
      if (FHost <> '') and (FHost[low(FHost)] = '[') and (FHost[high(FHost)] = ']') then SB.Append(FHost)
      else SB.Append(ALUrlEncode(FHost, false{ASpacesAsPlus}));
      var LPort := FPort;
      if ALSameTextA(FScheme, 'http') then begin
        if LPort = 80 then LPort := 0;
      end
      else if ALSameTextA(FScheme, 'https') then begin
        if LPort = 443 then LPort := 0;
      end
      else if ALSameTextA(FScheme, 'ws') then begin
        if LPort = 80 then LPort := 0;
      end
      else if ALSameTextA(FScheme, 'wss') then begin
        if LPort = 443 then LPort := 0;
      end
      else if ALSameTextA(FScheme, 'ftp') then begin
        if LPort = 21 then LPort := 0;
      end;
      if LPort <> 0 then begin
        SB.Append(':');
        SB.Append(ALIntToStrA(LPort));
      end;
      SB.Append(ALPercentEncode(FPath, ['A'..'Z','a'..'z','0'..'9','-','.','_','~', '/'], false{ASpacesAsPlus}));
      if QueryParams.Count <> 0 then begin
        SB.Append('?');
        for var I := 0 to QueryParams.Count - 1 do begin
          if I > 0 then SB.Append('&');
          if QueryParams.ItemHasNameValue(I) then begin
            SB.Append(ALUrlEncode(QueryParams.Names[I], ASpacesAsPlus));
            SB.Append('=');
            SB.Append(ALUrlEncode(QueryParams.ValueFromIndex[I], ASpacesAsPlus));
          end
          else
            SB.Append(ALUrlEncode(QueryParams[I], ASpacesAsPlus));
        end;
      end;
      if FAnchor <> '' then begin
        SB.Append('#');
        SB.Append(ALUrlEncode(FAnchor, false{ASpacesAsPlus}));
      end;
    end
    {$ENDREGION}

    {$REGION 'Raw'}
    else begin
      If (FScheme <> '') then begin
        SB.Append(FScheme);
        SB.Append('://');
      end;
      If (FUserName <> '') and (FPassword <> '') then begin
        SB.Append(FUserName);
        SB.Append(':');
        SB.Append(FPassword);
        SB.Append('@');
      end
      else If (FUserName <> '') then begin
        SB.Append(FUserName);
        SB.Append('@');
      end;
      SB.Append(FHost);
      var LPort := FPort;
      if ALSameTextA(FScheme, 'http') then begin
        if LPort = 80 then LPort := 0;
      end
      else if ALSameTextA(FScheme, 'https') then begin
        if LPort = 443 then LPort := 0;
      end
      else if ALSameTextA(FScheme, 'ws') then begin
        if LPort = 80 then LPort := 0;
      end
      else if ALSameTextA(FScheme, 'wss') then begin
        if LPort = 443 then LPort := 0;
      end
      else if ALSameTextA(FScheme, 'ftp') then begin
        if LPort = 21 then LPort := 0;
      end;
      if LPort <> 0 then begin
        SB.Append(':');
        SB.Append(ALIntToStrA(LPort));
      end;
      SB.Append(FPath);
      if QueryString <> '' then begin
        SB.Append('?');
        SB.Append(QueryString);
      end;
      if FAnchor <> '' then begin
        SB.Append('#');
        SB.Append(FAnchor);
      end;
    end;
    {$ENDREGION}

    Result := SB.ToString(true{AUpdateCapacity});

  finally
    ALFreeAndNil(SB);
  end;
end;

{************************************************}
function TALCookedUrlA.GetQueryString: AnsiString;
begin
  if (FQueryString = '') and (FQueryParams <> nil) then FQueryString := FQueryParams.Text;
  Result := FQueryString;
end;

{***************************************************************}
procedure TALCookedUrlA.SetQueryString(const AValue: AnsiString);
begin
  if (AValue <> FQueryString) then begin
    ALFreeandNil(FQueryParams);
    FQueryString := AValue;
  end;
end;

{*************************************************}
function TALCookedUrlA.GetQueryParams: TALStringsA;
begin
  If FQueryParams = nil then begin
    FQueryParams := TALNVStringListA.Create;
    FQueryParams.LineBreak := '&';
    FQueryParams.IncludeTrailingLineBreakInText := False;
    FQueryParams.Text := FQueryString;
    FQueryParams.OnChanged := QueryParamsChanged;
  end;
  Result := FQueryParams;
end;

{**********************************************************}
procedure TALCookedUrlA.QueryParamsChanged(Sender: TObject);
begin
  FQueryString := '';
end;

{************************************************************}
Function  AlIsHttpOrHttpsUrl(const AUrl: AnsiString): Boolean;
begin
  var LLowUrl := low(AUrl);
  Result := (AUrl <> '') and
            (length(AUrl) > 7) and
            (AUrl[LLowUrl] in ['h','H']) and
            (AUrl[LLowUrl+1] in ['t','T']) and
            (AUrl[LLowUrl+2] in ['t','T']) and
            (AUrl[LLowUrl+3] in ['p','P']);
  if Result then begin
    Result := (length(AUrl) > 8) and
              (AUrl[LLowUrl+4] in ['s','S']) and
              (AUrl[LLowUrl+5] = ':') and
              (AUrl[LLowUrl+6] = '/') and
              (AUrl[LLowUrl+7] = '/');
    if not result then
      Result := (AUrl[LLowUrl+4] = ':') and
                (AUrl[LLowUrl+5] = '/') and
                (AUrl[LLowUrl+6] = '/');
  end;
end;

{********************************************************}
Function  AlIsHttpOrHttpsUrl(const AUrl: String): Boolean;
begin
  var LLowUrl := low(AUrl);
  Result := (AUrl <> '') and
            (length(AUrl) > 7) and
            (charInSet(AUrl[LLowUrl], ['h','H'])) and
            (charInSet(AUrl[LLowUrl+1], ['t','T'])) and
            (charInSet(AUrl[LLowUrl+2], ['t','T'])) and
            (charInSet(AUrl[LLowUrl+3], ['p','P']));
  if Result then begin
    Result := (length(AUrl) > 8) and
              (charInSet(AUrl[LLowUrl+4], ['s','S'])) and
              (AUrl[LLowUrl+5] = ':') and
              (AUrl[LLowUrl+6] = '/') and
              (AUrl[LLowUrl+7] = '/');
    if not result then
      Result := (AUrl[LLowUrl+4] = ':') and
                (AUrl[LLowUrl+5] = '/') and
                (AUrl[LLowUrl+6] = '/');
  end;
end;

{****************}
{$IFDEF MSWINDOWS}
function AlCombineUrlA(const ARelativeUrl: AnsiString; const ABaseUrl: AnsiString): AnsiString;
begin
  var LCookedUrl := TALCookedUrlA.Create(ARelativeUrl);
  try

    {relative path.. so try to combine the url}
    if LCookedUrl.Scheme = '' then begin
      var LSize: Dword := INTERNET_MAX_URL_LENGTH;
      var LBuffer: AnsiString;
      SetLength(LBuffer, LSize);
      if InternetCombineUrlA(
           PAnsiChar(ABaseUrl), // lpszBaseUrl: LPCSTR
           PAnsiChar(ARelativeUrl), // lpszRelativeUrl: LPCSTR;
           @LBuffer[1], // lpszBuffer: LPSTR
           Lsize, // var lpdwBufferLength: DWORD;
           ICU_BROWSER_MODE or ICU_no_encode) then // dwFlags: DWORD
        Result := AlCopyStr(LBuffer, 1, LSize)
      else
        result := ARelativeUrl;
    end

    {not a relative path}
    else result := ARelativeUrl;

  finally
    ALFreeAndNil(LCookedUrl);
  end;
end;
{$ENDIF}

{********************************************************************************************}
function ALUrlEncode(const AStr: AnsiString; Const ASpacesAsPlus: Boolean = True): AnsiString;
begin
  // https://datatracker.ietf.org/doc/html/rfc3986#section-2.3
  Result := ALPercentEncode(AStr, ['A'..'Z','a'..'z','0'..'9','-','.','_','~'], ASpacesAsPlus);
end;

{************************************************************************************}
function ALUrlEncode(const AStr: String; Const ASpacesAsPlus: Boolean = True): String;
begin
  // https://datatracker.ietf.org/doc/html/rfc3986#section-2.3
  Result := ALPercentEncode(AStr, ['A'..'Z','a'..'z','0'..'9','-','.','_','~'], ASpacesAsPlus);
end;

{********************************************************************************************}
function ALUrlDecode(const AStr: AnsiString; const APlusAsSpaces: Boolean = True): AnsiString;
begin
  Result := ALPercentDecode(AStr, APlusAsSpaces);
end;

{************************************************************************************}
function ALUrlDecode(const AStr: String; const APlusAsSpaces: Boolean = True): String;
begin
  Result := ALPercentDecode(AStr, APlusAsSpaces);
end;

end.
