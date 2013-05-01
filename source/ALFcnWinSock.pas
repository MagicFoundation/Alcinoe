{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe              
Author(s):    St�phane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      Alcinoe Winsock Functions
Version:      4.00

Description:  Misc function that use winsock (for exempple ALHostToIP,
              ALIPAddrToName or ALgetLocalIPs)

Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :     26/06/2012: Add xe2 support

Link :

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://static.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALFcnWinSock;

interface

uses AlStringList;

function  ALHostToIP(HostName: AnsiString; var Ip: AnsiString): Boolean;
function  ALIPAddrToName(IPAddr : AnsiString): AnsiString;
function  ALgetLocalIPs: TALStrings;
function  ALgetLocalHostName: AnsiString;

implementation

uses Windows,
     sysutils,
     {$IF CompilerVersion >= 23} {Delphi XE2}
     WinSock2,
     {$ELSE}
     WinSock,
     {$IFEND}
     AlFcnString;

{*********************************************************************}
function ALHostToIP(HostName: AnsiString; var Ip: AnsiString): Boolean;
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

{*******************************************************}
function ALIPAddrToName(IPAddr : AnsiString): AnsiString;
var SockAddrIn: TSockAddrIn;
    HostEnt: PHostEnt;
    WSAData: TWSAData;
begin
  if WSAStartup(MAKEWORD(2,2), WSAData) <> 0 then raiseLastOsError;
  Try
    SockAddrIn.sin_addr.s_addr:= inet_addr(PAnsiChar(IPAddr));
    HostEnt:= gethostbyaddr(@SockAddrIn.sin_addr.S_addr, 4, AF_INET);
    if HostEnt<>nil then result:=StrPas(Hostent^.h_name)
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
    Result := StrPas(Buffer);

  finally
    WSACleanup;
  end;
end;

end.