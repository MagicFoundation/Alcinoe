{*******************************************************************************
Misc function that use winsock (for exempple ALHostToIP, ALIPAddrToName or
ALgetLocalIPs)
*******************************************************************************}

unit ALWinSock;

interface

uses
  AlStringList;

function  ALHostToIP(const HostName: AnsiString; var Ip: AnsiString): Boolean;
function  ALIPAddrToName(const IPAddr : AnsiString): AnsiString;
function  ALgetLocalIPs: TALStrings;
function  ALgetLocalHostName: AnsiString;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  Winapi.WinSock2,
  System.Ansistrings,
  ALCommon,
  ALString;

{***************************************************************************}
function ALHostToIP(const HostName: AnsiString; var Ip: AnsiString): Boolean;
var WSAData : TWSAData;
    hostEnt : PHostEnt;
    addr : PAnsiChar;
begin
  if WSAStartup(MAKEWORD(2,2), WSAData) <> 0 then raiseLastOsError;
  try

    hostEnt := gethostbyname(PAnsiChar(hostName));
    if Assigned (hostEnt) then begin
      if Assigned (hostEnt^.h_addr_list) then begin
        addr := hostEnt^.h_addr_list^;
        if Assigned (addr) then begin
          IP := ALFormat ('%d.%d.%d.%d', [byte (addr [0]),
          byte (addr [1]), byte (addr [2]), byte (addr [3])]);
          Result := True;
        end
        else Result := False;
      end
      else Result := False
    end
    else Result := False;

  finally
    WSACleanup;
  end
end;

{*************************************************************}
function ALIPAddrToName(const IPAddr : AnsiString): AnsiString;
var SockAddrIn: TSockAddrIn;
    HostEnt: PHostEnt;
    WSAData: TWSAData;
begin
  if WSAStartup(MAKEWORD(2,2), WSAData) <> 0 then raiseLastOsError;
  Try
    SockAddrIn.sin_addr.s_addr:= inet_addr(PAnsiChar(IPAddr));
    HostEnt:= gethostbyaddr(@SockAddrIn.sin_addr.S_addr, 4, AF_INET);
    if HostEnt<>nil then result:=System.Ansistrings.StrPas(Hostent^.h_name)
    else result:='';
  finally
    WSACleanup;
  end;
end;

{*********************************}
function ALgetLocalIPs: TALStrings;
type
  TaPInAddr = array[0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
  Buffer: array[0..255] of AnsiChar;
  I: Integer;
  WSAData: TWSAData;
begin
  if WSAStartup(MAKEWORD(2,2), WSAData) <> 0 then raiseLastOsError;
  Try
    Result := TALStringList.Create;
    Result.Clear;
    GetHostName(Buffer, SizeOf(Buffer));
    phe := GetHostByName(buffer);
    if phe = nil then Exit;
    pPtr := PaPInAddr(phe^.h_addr_list);
    I    := 0;
    while pPtr^[I] <> nil do begin
      Result.Add(inet_ntoa(pptr^[I]^));
      Inc(I);
    end;
  finally
    WSACleanup;
  end;
end;

{***************************************}
function  ALgetLocalHostName: AnsiString;
var Buffer: array [0..255] of AnsiChar;
    WSAData: TWSAData;
begin
  if WSAStartup(MAKEWORD(2,2), WSAData) <> 0 then raiseLastOsError;
  Try

    if gethostname(Buffer, SizeOf(Buffer)) <> 0 then raise EALException.Create('Winsock GetHostName failed');
    Result := System.Ansistrings.StrPas(Buffer);

  finally
    WSACleanup;
  end;
end;

end.