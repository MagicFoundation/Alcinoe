/// low level access to network Sockets for FPC (and Kylix) POSIX cross-platform
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynFPCSock;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synapse library.

  The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).
  Portions created by Lukas Gebauer are Copyright (C) 2003.
  All Rights Reserved.

  Portions created by Arnaud Bouchez are Copyright (C) 2021 Arnaud Bouchez.
  All Rights Reserved.

  Contributor(s):
  - Alfred Glaenzer

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

     Low level access to network Sockets
    *************************************

  Shared by Kylix and FPC for all POSIX systems.

}

{$ifdef FPC}

{$MODE DELPHI}
{$H+}

{.$define USELIBC}

{$ifdef ANDROID}
  {$define LINUX} // a Linux-based system
{$endif}

// BSD definition of socketaddr
{$if
     defined(OpenBSD) or
     defined(FreeBSD) or
     defined(Darwin) or
     defined(Haiku)
}
  {$DEFINE SOCK_HAS_SINLEN}               // BSD definition of socketaddr
{$endif}
{$ifdef SUNOS}
  {$DEFINE SOCK_HAS_SINLEN}
{$endif}

{$else}

{$ifdef LINUX}
  {$define KYLIX3}
{$else}
  this unit is for FPC or (Cross-)Kylix only!
{$endif}

{$endif FPC}

interface

uses
  SysUtils,
  {$ifdef FPC}
  BaseUnix,
  Unix,
  {$ifdef Linux}
  Linux, // for epoll support
  {$endif Linux}
  termio,
  netdb,
  Sockets, // most definitions are inlined in SynFPCSock to avoid Lazarus problems with Sockets.pp
  SynFPCLinux,
  {$else}
  {$ifdef KYLIX3}
  LibC,
  Types,
  KernelIoctl,
  SynKylix,
  {$endif}
  {$endif FPC}
  SyncObjs,
  Classes;

const
  InitSocketInterface = true;

procedure DestroySocketInterface;

{$MINENUMSIZE 4}

const
  DLLStackName = '';
  WinsockLevel = $0202;

  cLocalHost = '127.0.0.1';
  cAnyHost = '0.0.0.0';
  c6AnyHost = '::0';
  c6Localhost = '::1';
  cLocalHostStr = 'localhost';

{$ifdef FPC}
type
  TSocket = longint;

  TFDSet = Baseunix.TFDSet;
  PFDSet = ^TFDSet;
  Ptimeval = Baseunix.ptimeval;
  Ttimeval = Baseunix.ttimeval;

  PInAddr = ^TInAddr;
  TInAddr = sockets.in_addr;

  PSockAddrIn = ^TSockAddrIn;
  TSockAddrIn = sockets.TInetSockAddr;

  PInAddr6 = ^TInAddr6;
  TInAddr6 = sockets.Tin6_addr;

  PSockAddrIn6 = ^TSockAddrIn6;
  TSockAddrIn6 = sockets.TInetSockAddr6;

  TSockAddr = sockets.TSockAddr;
  PSockAddr = sockets.PSockAddr;

const
  FIONREAD        = termio.FIONREAD;
  FIONBIO         = termio.FIONBIO;
  FIOASYNC        = termio.FIOASYNC;

  IP_TOS             = sockets.IP_TOS;             { int; IP type of service and precedence.  }
  IP_TTL             = sockets.IP_TTL;             { int; IP time to live.  }
  IP_HDRINCL         = sockets.IP_HDRINCL;         { int; Header is included with data.  }
  IP_OPTIONS         = sockets.IP_OPTIONS;         { ip_opts; IP per-packet options.  }
  IP_RECVOPTS        = sockets.IP_RECVOPTS;        { bool }
  IP_RETOPTS         = sockets.IP_RETOPTS;         { bool }
  IP_MULTICAST_IF    = sockets.IP_MULTICAST_IF;    { in_addr; set/get IP multicast i/f }
  IP_MULTICAST_TTL   = sockets.IP_MULTICAST_TTL;   { u_char; set/get IP multicast ttl }
  IP_MULTICAST_LOOP  = sockets.IP_MULTICAST_LOOP;  { i_char; set/get IP multicast loopback }
  IP_ADD_MEMBERSHIP  = sockets.IP_ADD_MEMBERSHIP;  { ip_mreq; add an IP group membership }
  IP_DROP_MEMBERSHIP = sockets.IP_DROP_MEMBERSHIP; { ip_mreq; drop an IP group membership }

  SHUT_RD         = sockets.SHUT_RD;
  SHUT_WR         = sockets.SHUT_WR;
  SHUT_RDWR       = sockets.SHUT_RDWR;

  SOL_SOCKET    = sockets.SOL_SOCKET;

  SO_DEBUG      = sockets.SO_DEBUG;
  SO_REUSEADDR  = sockets.SO_REUSEADDR;
  {$ifdef BSD}
  SO_REUSEPORT  = sockets.SO_REUSEPORT;
  {$endif}
  SO_TYPE       = sockets.SO_TYPE;
  SO_ERROR      = sockets.SO_ERROR;
  SO_DONTROUTE  = sockets.SO_DONTROUTE;
  SO_BROADCAST  = sockets.SO_BROADCAST;
  SO_SNDBUF     = sockets.SO_SNDBUF;
  SO_RCVBUF     = sockets.SO_RCVBUF;
  SO_KEEPALIVE  = sockets.SO_KEEPALIVE;
  SO_OOBINLINE  = sockets.SO_OOBINLINE;
  SO_LINGER     = sockets.SO_LINGER;
  SO_RCVLOWAT   = sockets.SO_RCVLOWAT;
  SO_SNDLOWAT   = sockets.SO_SNDLOWAT;
  SO_RCVTIMEO   = sockets.SO_RCVTIMEO;
  SO_SNDTIMEO   = sockets.SO_SNDTIMEO;
{$IFDEF BSD}
  {$IFNDEF OPENBSD}
  {$IFDEF DARWIN}
  SO_NOSIGPIPE  = $1022;
  {$ELSE}
  SO_NOSIGPIPE	= $800;
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
  // we use Linux default here
  SOMAXCONN     = 128;

  IPV6_UNICAST_HOPS     = sockets.IPV6_UNICAST_HOPS;
  IPV6_MULTICAST_IF     = sockets.IPV6_MULTICAST_IF;
  IPV6_MULTICAST_HOPS   = sockets.IPV6_MULTICAST_HOPS;
  IPV6_MULTICAST_LOOP   = sockets.IPV6_MULTICAST_LOOP;
  IPV6_JOIN_GROUP       = sockets.IPV6_JOIN_GROUP;
  IPV6_LEAVE_GROUP      = sockets.IPV6_LEAVE_GROUP;

  MSG_OOB       = sockets.MSG_OOB;      // Process out-of-band data.
  MSG_PEEK      = sockets.MSG_PEEK;     // Peek at incoming messages.

  {$ifdef BSD}
  {$ifndef OpenBSD}
  // Works under MAC OS X and FreeBSD, but is undocumented, so FPC doesn't include it
  MSG_NOSIGNAL  = $20000;  // Do not generate SIGPIPE.
  {$else}
  MSG_NOSIGNAL  = $400;
  {$endif}
  {$else}
  {$ifdef SUNOS}
  MSG_NOSIGNAL  = $20000;  // Do not generate SIGPIPE.
  {$else}
  MSG_NOSIGNAL  = sockets.MSG_NOSIGNAL; // Do not generate SIGPIPE.
  {$endif}
  {$endif}

  { TCP options. }
  TCP_NODELAY     = $0001;

  { Address families. }
  AF_UNSPEC       = 0;               { unspecified }
  AF_LOCAL        = 1;
  AF_INET         = 2;               { internetwork: UDP, TCP, etc. }
  AF_UNIX         = AF_LOCAL;
  AF_MAX          = 24;

  { Protocol families, same as address families for now. }
  PF_UNSPEC       = AF_UNSPEC;
  PF_INET         = AF_INET;
  PF_MAX          = AF_MAX;

const
  WSAEINTR = ESysEINTR;
  WSAEBADF = ESysEBADF;
  WSAEACCES = ESysEACCES;
  WSAEFAULT = ESysEFAULT;
  WSAEINVAL = ESysEINVAL;
  WSAEMFILE = ESysEMFILE;
  WSAEWOULDBLOCK = ESysEWOULDBLOCK; // =WSATRY_AGAIN/ESysEAGAIN on POSIX
  WSAEINPROGRESS = ESysEINPROGRESS;
  WSAEALREADY = ESysEALREADY;
  WSATRY_AGAIN = ESysEAGAIN;
  WSAENOTSOCK = ESysENOTSOCK;
  WSAEDESTADDRREQ = ESysEDESTADDRREQ;
  WSAEMSGSIZE = ESysEMSGSIZE;
  WSAEPROTOTYPE = ESysEPROTOTYPE;
  WSAENOPROTOOPT = ESysENOPROTOOPT;
  WSAEPROTONOSUPPORT = ESysEPROTONOSUPPORT;
  WSAESOCKTNOSUPPORT = ESysESOCKTNOSUPPORT;
  WSAEOPNOTSUPP = ESysEOPNOTSUPP;
  WSAEPFNOSUPPORT = ESysEPFNOSUPPORT;
  WSAEAFNOSUPPORT = ESysEAFNOSUPPORT;
  WSAEADDRINUSE = ESysEADDRINUSE;
  WSAEADDRNOTAVAIL = ESysEADDRNOTAVAIL;
  WSAENETDOWN = ESysENETDOWN;
  WSAENETUNREACH = ESysENETUNREACH;
  WSAENETRESET = ESysENETRESET;
  WSAECONNABORTED = ESysECONNABORTED;
  WSAECONNRESET = ESysECONNRESET;
  WSAENOBUFS = ESysENOBUFS;
  WSAEISCONN = ESysEISCONN;
  WSAENOTCONN = ESysENOTCONN;
  WSAESHUTDOWN = ESysESHUTDOWN;
  WSAETOOMANYREFS = ESysETOOMANYREFS;
  WSAETIMEDOUT = ESysETIMEDOUT;
  WSAECONNREFUSED = ESysECONNREFUSED;
  WSAELOOP = ESysELOOP;
  WSAENAMETOOLONG = ESysENAMETOOLONG;
  WSAEHOSTDOWN = ESysEHOSTDOWN;
  WSAEHOSTUNREACH = ESysEHOSTUNREACH;
  WSAENOTEMPTY = ESysENOTEMPTY;
  WSAEPROCLIM = -1;
  WSAEUSERS = ESysEUSERS;
  WSAEDQUOT = ESysEDQUOT;
  WSAESTALE = ESysESTALE;
  WSAEREMOTE = ESysEREMOTE;
  WSASYSNOTREADY = -2;
  WSAVERNOTSUPPORTED = -3;
  WSANOTINITIALISED = -4;
  WSAEDISCON = -5;
  WSAHOST_NOT_FOUND = 1;
  WSANO_RECOVERY = 3;
  WSANO_DATA = -6;

{$else FPC} // Kylix3 definitions:

type
  TInAddr6 = packed record
  case byte of
    0: (u6_addr8  : array[0..15] of byte);
    1: (u6_addr16 : array[0..7] of Word);
    2: (u6_addr32 : array[0..3] of Cardinal);
    3: (s6_addr8  : array[0..15] of shortint);
    4: (s6_addr   : array[0..15] of shortint);
    5: (s6_addr16 : array[0..7] of smallint);
    6: (s6_addr32 : array[0..3] of LongInt);
  end;
  PInAddr6 = ^TInAddr6;

  TSockAddrIn6 = packed Record
    sin6_family   : sa_family_t;
    sin6_port     : word;
    sin6_flowinfo : cardinal;
    sin6_addr     : TInAddr6;
    sin6_scope_id : cardinal;
  end;

const
  WSAEINTR = EINTR;
  WSATRY_AGAIN = EAGAIN;
  WSAENETDOWN = ENETDOWN;
  WSAECONNABORTED = ECONNABORTED;
  WSAECONNRESET = ECONNRESET;
  WSAEWOULDBLOCK = EWOULDBLOCK;
  WSAEPROTONOSUPPORT = EPROTONOSUPPORT;
  WSAHOST_NOT_FOUND = HOST_NOT_FOUND;
  WSAETIMEDOUT = ETIMEDOUT;
  WSAEMFILE = EMFILE;

{$endif FPC}


const
  IPPROTO_IP     =   0;		{ Dummy					}
  IPPROTO_ICMP   =   1;		{ Internet Control Message Protocol }
  IPPROTO_IGMP   =   2;		{ Internet Group Management Protocol}
  IPPROTO_TCP    =   6;		{ TCP           			}
  IPPROTO_UDP    =   17;	{ User Datagram Protocol		}
  IPPROTO_IPV6   =   41;
  IPPROTO_ICMPV6 =   58;
  IPPROTO_RM     =  113;

  IPPROTO_RAW    =   255;
  IPPROTO_MAX    =   256;

  AF_INET6       = 10;              { Internetwork Version 6 }
  PF_INET6       = AF_INET6;

  SOCK_STREAM     = 1;               { stream socket }
  SOCK_DGRAM      = 2;               { datagram socket }
  SOCK_RAW        = 3;               { raw-protocol interface }
  SOCK_RDM        = 4;               { reliably-delivered message }
  SOCK_SEQPACKET  = 5;               { sequenced packet stream }

type
  TIP_mreq =  record
    imr_multiaddr: TInAddr;     // IP multicast address of group
    imr_interface: TInAddr;     // local IP address of interface
  end;

  TIPv6_mreq = record
    ipv6mr_multiaddr: TInAddr6; // IPv6 multicast address.
    ipv6mr_interface: integer;   // Interface index.
  end;

const
  INADDR_ANY       = $00000000;
  INADDR_LOOPBACK  = $7F000001;
  INADDR_BROADCAST = $FFFFFFFF;
  INADDR_NONE      = $FFFFFFFF;
  ADDR_ANY		     = INADDR_ANY;
  INVALID_SOCKET	 = TSocket(NOT(0));
  SOCKET_ERROR		 = -1;


type
  { Structure used for manipulating linger option. }
  PLinger = ^TLinger;
  TLinger = packed record
    l_onoff: integer;
    l_linger: integer;
  end;

const
  WSADESCRIPTION_LEN     =   256;
  WSASYS_STATUS_LEN      =   128;

type
  PWSAData = ^TWSAData;
  TWSAData = packed record
    wVersion: Word;
    wHighVersion: Word;
    szDescription: array[0..WSADESCRIPTION_LEN] of Char;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of Char;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PChar;
  end;

function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
function IN6_ADDR_EQUAL(const a: PInAddr6; const b: PInAddr6):boolean;
procedure SET_IN6_IF_ADDR_ANY (const a: PInAddr6);
procedure SET_LOOPBACK_ADDR6 (const a: PInAddr6);

var
  in6addr_any, in6addr_loopback : TInAddr6;

{$ifdef FPC} // some functions inlined redirection to Sockets.pp

procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet); inline;
function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean; inline;
procedure FD_SET(Socket: TSocket; var FDSet: TFDSet); inline;
procedure FD_ZERO(var FDSet: TFDSet); inline;

function ResolveIPToName(const IP: string; Family,SockProtocol,SockType: integer): string;
function ResolvePort(const Port: string; Family,SockProtocol,SockType: integer): Word;

function fpbind(s:cint; addrx: psockaddr; addrlen: tsocklen): cint; inline;
function fplisten(s:cint; backlog: cint): cint; inline;
function fprecv(s:cint; buf: pointer; len: size_t; Flags: cint): ssize_t; inline;
function fpsend(s:cint; msg:pointer; len:size_t; flags:cint): ssize_t; inline;

{$endif FPC}

const
  // we assume that the Posix OS has IP6 compatibility
  SockEnhancedApi = true;
  SockWship6Api = true;

type
  PVarSin = ^TVarSin;
  TVarSin = packed record
    {$ifdef SOCK_HAS_SINLEN}
    sin_len: cuchar;
    {$endif}
    case integer of
      0: (AddressFamily: sa_family_t);
      1: (
        case sin_family: sa_family_t of
          AF_INET: (sin_port: word;
                    sin_addr: TInAddr;
                    sin_zero: array[0..7] of Char);
          AF_INET6:(sin6_port:     word; // see sockaddr_in6
                    sin6_flowinfo: cardinal;
      	    	    sin6_addr:     TInAddr6;
      		    sin6_scope_id: cardinal);
          AF_UNIX: (sun_path: array[0..{$ifdef SOCK_HAS_SINLEN}103{$else}107{$endif}] of Char);
          );
  end;

function SizeOfVarSin(sin: TVarSin): integer;

function WSAStartup(wVersionRequired: Word; var WSData: TWSAData): Integer;
function WSACleanup: Integer;
function WSAGetLastError: Integer;
function GetHostName: string;
function Shutdown(s: TSocket; how: Integer): Integer;
function SetSockOpt(s: TSocket; level,optname: Integer; optval: pointer;
  optlen: Integer): Integer;
function GetSockOpt(s: TSocket; level,optname: Integer; optval: pointer;
  var optlen: Integer): Integer;
function SendTo(s: TSocket; Buf: pointer; len,flags: Integer; addrto: TVarSin): Integer;
function RecvFrom(s: TSocket; Buf: pointer; len,flags: Integer; var from: TVarSin): Integer;
function ntohs(netshort: word): word;
function ntohl(netlong: cardinal): cardinal;
function Listen(s: TSocket; backlog: Integer): Integer;
function IoctlSocket(s: TSocket; cmd: DWORD; var arg: integer): Integer;
function htons(hostshort: word): word;
function htonl(hostlong: cardinal): cardinal;
function GetSockName(s: TSocket; var name: TVarSin): Integer;
function GetPeerName(s: TSocket; var name: TVarSin): Integer;
function Connect(s: TSocket; const name: TVarSin): Integer;
function CloseSocket(s: TSocket): Integer;
function Bind(s: TSocket; const addr: TVarSin): Integer;
function Accept(s: TSocket; var addr: TVarSin): TSocket;
function Socket(af,Struc,Protocol: Integer): TSocket;
function Select(nfds: Integer; readfds,writefds,exceptfds: PFDSet;
  timeout: PTimeVal): Longint;

function IsNewApi(Family: integer): Boolean;
function SetVarSin(var Sin: TVarSin; const IP,Port: string;
  Family,SockProtocol,SockType: integer; PreferIP4: Boolean): integer;
function GetSinIP(const Sin: TVarSin): string;
function GetSinPort(const Sin: TVarSin): Integer;
procedure ResolveNameToIP(const Name: AnsiString; Family, SockProtocol, SockType: integer;
  IPList: TStrings; IPListClear: boolean = true);

const
  // poll() flag when there is data to read
  POLLIN       = $001;
  // poll() flag when there is urgent data to read
  POLLPRI      = $002;
  // poll() flag when writing now will not block
  POLLOUT      = $004;
  // poll() flag error condition (always implicitly polled for)
  POLLERR      = $008;
  // poll() flag hung up (always implicitly polled for)
  POLLHUP      = $010;
  // poll() flag invalid polling request (always implicitly polled for)
  POLLNVAL     = $020;
  // poll() flag when normal data may be read
  POLLRDNORM   = $040;
  // poll() flag when priority data may be read
  POLLRDBAND   = $080;
  // poll() flag when writing now will not block
  POLLWRNORM   = $100;
  // poll() flag when priority data may be written
  POLLWRBAND   = $200;
  // poll() flag extension for Linux
  POLLMSG      = $400;

type
  /// polling request data structure for poll()
  TPollFD = {packed} record
    /// file descriptor to poll
    fd: integer;
    /// types of events poller cares about
    // - mainly POLLIN and/or POLLOUT
    events: Smallint;
    /// types of events that actually occurred
    // - caller could just reset revents := 0 to reuse the structure
    revents: Smallint;
  end;
  PPollFD = ^TPollFD;
  TPollFDDynArray = array of TPollFD;

/// Poll the file descriptors described by the nfds structures starting at fds
// - if TIMEOUT is nonzero and not -1, allow TIMEOUT milliseconds for
// an event to occur; if TIMEOUT is -1, block until an event occurs
// - returns the number of file descriptors with events, zero if timed out,
// or -1 for errors
function poll(fds: PPollFD; nfds, timeout: integer): integer;

{$ifdef Linux}
const
  // associated file is available for read operations
  EPOLLIN  = $01;
  // urgent data available for read operations
  EPOLLPRI = $02;
  // associated file is available for write operations
  EPOLLOUT = $04;
  // error condition happened on the associated file descriptor
  EPOLLERR = $08;
  // hang up happened on the associated file descriptor
  EPOLLHUP = $10;
  // sets the One-Shot behaviour for the associated file descriptor
  // - i.e. after an event is pulled out, the file descriptor is disabled
  EPOLLONESHOT = $40000000;
  // sets the Edge-Triggered (ET) behaviour  for  the  associated file descriptor
  EPOLLET  = $80000000;

  EPOLL_CTL_ADD = 1;
  EPOLL_CTL_DEL = 2;
  EPOLL_CTL_MOD = 3;

type
  /// application-level data structure for epoll
  TEPollData = record
    case integer of
      0: (ptr: pointer);
      1: (fd: integer);
      2: (u32: cardinal);
      3: (u64: Int64);
      4: (obj: TObject);
  end;
  PEPollData = ^TEPollData;

  /// epoll descriptor data structure
  TEPollEvent = packed record
    events: cardinal;
    data: TEpollData;
  end;
  PEPollEvent = ^TEPollEvent;
  TEPollEventDynArray = array of TEPollEvent;

/// open an epoll file descriptor
function epoll_create(size: integer): integer;
  {$ifdef FPC}inline;{$endif} {$ifdef KYLIX3}cdecl;{$endif}

/// control interface for an epoll descriptor
function epoll_ctl(epfd, op, fd: integer; event: PEPollEvent): integer;
  {$ifdef FPC}inline;{$endif} {$ifdef KYLIX3}cdecl;{$endif}

/// wait for an I/O event on an epoll file descriptor
function epoll_wait(epfd: integer; events: PEPollEvent; maxevents, timeout: integer): integer;
  {$ifdef FPC}inline;{$endif} {$ifdef KYLIX3}cdecl;{$endif}

/// finalize an epoll file descriptor
// - call fpclose/libc.close
function epoll_close(epfd: integer): integer;
{$endif Linux}

var
  SynSockCS: TRTLCriticalSection;

implementation

{$ifdef USELIBC}
{$i SynFPCSockLIBC.inc}
{$endif}

function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
begin
  result := ((a^.u6_addr32[0]=0) and (a^.u6_addr32[1]=0) and
             (a^.u6_addr32[2]=0) and (a^.u6_addr32[3]=0));
end;

function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
begin
  result := ((a^.u6_addr32[0]=0) and (a^.u6_addr32[1]=0) and
             (a^.u6_addr32[2]=0) and
             (a^.u6_addr8[12]=0) and (a^.u6_addr8[13]=0) and
             (a^.u6_addr8[14]=0) and (a^.u6_addr8[15]=1));
end;

function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
begin
  result := ((a^.u6_addr8[0]=$FE) and (a^.u6_addr8[1]=$80));
end;

function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
begin
  result := ((a^.u6_addr8[0]=$FE) and (a^.u6_addr8[1]=$C0));
end;

function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
begin
  result := (a^.u6_addr8[0]=$FF);
end;

function IN6_ADDR_EQUAL(const a: PInAddr6; const b: PInAddr6): boolean;
begin
  result := CompareMem(a,b,sizeof(TInAddr6));
end;

procedure SET_IN6_IF_ADDR_ANY (const a: PInAddr6);
begin
  FillChar(a^,sizeof(TInAddr6),0);
end;

procedure SET_LOOPBACK_ADDR6 (const a: PInAddr6);
begin
  FillChar(a^,sizeof(TInAddr6),0);
  a^.u6_addr8[15] := 1;
end;


function WSAStartup(wVersionRequired: Word; var WSData: TWSAData): Integer;
begin
  with WSData do begin
    wVersion := wVersionRequired;
    wHighVersion := $202;
    szDescription := 'Synopse Sockets';
    szSystemStatus := 'Linux';
    iMaxSockets := 32768;
    iMaxUdpDg := 8192;
  end;
  result := 0;
end;

function WSACleanup: Integer;
begin
  result := 0;
end;

function WSAGetLastError: Integer;
begin
  result := {$ifdef KYLIX3}errno{$else}fpGetErrno{$endif};
end;

{$ifdef FPC}

function FD_ISSET(Socket: TSocket; var fdset: TFDSet): Boolean;
begin
  result := fpFD_ISSET(socket,fdset) <> 0;
end;

procedure FD_SET(Socket: TSocket; var fdset: TFDSet);
begin
  fpFD_SET(Socket,fdset);
end;

procedure FD_CLR(Socket: TSocket; var fdset: TFDSet);
begin
  fpFD_CLR(Socket,fdset);
end;

procedure FD_ZERO(var fdset: TFDSet);
begin
  fpFD_ZERO(fdset);
end;

{$ifndef USELIBC}
function fpbind(s:cint; addrx: psockaddr; addrlen: tsocklen): cint;
begin
  result := sockets.fpbind(s, addrx, addrlen);
end;

function fplisten(s:cint; backlog : cint): cint;
begin
  result := sockets.fplisten(s, backlog);
end;

function fprecv(s:cint; buf: pointer; len: size_t; Flags: cint): ssize_t;
begin
  result := sockets.fprecv(s, buf, len, Flags);
end;

function fpsend(s:cint; msg:pointer; len:size_t; flags:cint): ssize_t;
begin
  result := sockets.fpsend(s, msg, len, flags);
end;
{$endif USELIBC}

{$endif FPC}

function SizeOfVarSin(sin: TVarSin): integer;
begin
  case sin.sin_family of
    AF_INET:  result := SizeOf(TSockAddrIn);
    AF_INET6: result := SizeOf(TSockAddrIn6);
    AF_UNIX:  result := SizeOf(sockaddr_un);
  else        result := 0;
  end;
end;

{=============================================================================}

function Bind(s: TSocket; const addr: TVarSin): Integer;
begin
  {$ifdef KYLIX3}
  if LibC.Bind(s,PSockAddr(@addr)^,SizeOfVarSin(addr))=0 then
  {$else}
  if fpBind(s,@addr,SizeOfVarSin(addr))=0 then
  {$endif}
    result := 0 else
    result := SOCKET_ERROR;
end;

function Connect(s: TSocket; const name: TVarSin): Integer;
begin
  {$ifdef KYLIX3}
  if LibC.Connect(s,PSockAddr(@name)^,SizeOfVarSin(name))=0 then
  {$else}
  if fpConnect(s,@name,SizeOfVarSin(name))=0 then
  {$endif}
    result := 0 else
    result := SOCKET_ERROR;
end;

function GetSockName(s: TSocket; var name: TVarSin): Integer;
var len: integer;
begin
  len := SizeOf(name);
  FillChar(name,len,0);
  {$ifdef KYLIX3}
  result := LibC.getsockname(s,PSockAddr(@name)^,PSocketLength(@len)^);
  {$else}
  result := fpGetSockName(s,@name,@len);
  {$endif}
end;

function GetPeerName(s: TSocket; var name: TVarSin): Integer;
var len: integer;
begin
  len := SizeOf(name);
  FillChar(name,len,0);
  {$ifdef KYLIX3}
  result := LibC.getpeername(s,PSockAddr(@name)^,PSocketLength(@len)^);
  {$else}
  result := fpGetPeerName(s,@name,@len);
  {$endif}
end;

function GetHostName: string;
{$ifdef KYLIX3}
var tmp: array[byte] of char;
begin
  LibC.gethostname(tmp,sizeof(tmp)-1);
  result := tmp;
end;
{$else}
begin
  result := unix.GetHostName;
end;
{$endif}

function SendTo(s: TSocket; Buf: pointer; len,flags: Integer; addrto: TVarSin): Integer;
begin
  {$ifdef KYLIX3}
  result := LibC.sendto(s,Buf^,len,flags,PSockAddr(@addrto)^,SizeOfVarSin(addrto));
  {$else}
  result := fpSendTo(s,pointer(Buf),len,flags,@addrto,SizeOfVarSin(addrto));
  {$endif}
end;

function RecvFrom(s: TSocket; Buf: pointer; len,flags: Integer; var from: TVarSin): Integer;
var x: integer;
begin
  x := SizeOf(from);
  {$ifdef KYLIX3}
  result := LibC.recvfrom(s,Buf^,len,flags,PSockAddr(@from),PSocketLength(@x));
  {$else}
  result := fpRecvFrom(s,pointer(Buf),len,flags,@from,@x);
  {$endif}
end;

function Accept(s: TSocket; var addr: TVarSin): TSocket;
var x: integer;
begin
  x := SizeOf(addr);
  {$ifdef KYLIX3}
  result := LibC.accept(s,PSockAddr(@addr),PSocketLength(@x));
  {$else}
  result := fpAccept(s,@addr,@x);
  {$endif}
end;

function Shutdown(s: TSocket; how: Integer): Integer;
begin
  {$ifdef KYLIX3}
  result := LibC.shutdown(s,how);
  {$else}
  result := fpShutdown(s,how);
  {$endif}
end;

function SetSockOpt(s: TSocket; level,optname: Integer; optval: pointer;
  optlen: Integer): Integer;
begin
  result := {$ifdef KYLIX3}LibC.setsockopt{$else}fpsetsockopt{$endif}(
    s,level,optname,optval  ,optlen);
end;

function GetSockOpt(s: TSocket; level,optname: Integer; optval: pointer;
  var optlen: Integer): Integer;
begin
  {$ifdef KYLIX3}
  result := LibC.getsockopt(s,level,optname,pointer(optval),socklen_t(optlen));
  {$else}
  result := fpgetsockopt(s,level,optname,pointer(optval),@optlen);
  {$endif}
end;

function ntohs(netshort: word): word;
begin
  result := {$ifdef KYLIX3}LibC{$else}sockets{$endif}.ntohs(NetShort);
end;

function ntohl(netlong: cardinal): cardinal;
begin
  result := {$ifdef KYLIX3}LibC{$else}sockets{$endif}.ntohl(NetLong);
end;

function Listen(s: TSocket; backlog: Integer): Integer;
begin
  if {$ifdef KYLIX3}LibC.listen{$else}fpListen{$endif}(s,backlog)=0 then
    result := 0 else
    result := SOCKET_ERROR;
end;

function IoctlSocket(s: TSocket; cmd: DWORD; var arg: integer): Integer;
begin
  {$ifdef KYLIX3}
  result := ioctl(s,cmd,@arg);
  {$else}
  result := fpIoctl(s,cmd,@arg);
  {$endif}
end;

function htons(hostshort: word): word;
begin
  result := {$ifdef KYLIX3}LibC{$else}sockets{$endif}.htons(hostshort);
end;

function htonl(hostlong: cardinal): cardinal;
begin
  result := {$ifdef KYLIX3}LibC{$else}sockets{$endif}.htonl(hostlong);
end;

function CloseSocket(s: TSocket): Integer;
begin
  {$ifdef KYLIX3}
  result := Libc.__close(s);
  {$else}
  result := sockets.CloseSocket(s);
  {$endif}
end;

function Socket(af,Struc,Protocol: Integer): TSocket;
{$IF defined(BSD) AND NOT defined(OpenBSD)}
var
  on_off: integer;
{$ENDIF}
begin
  result := {$ifdef KYLIX3}LibC.socket{$else}fpSocket{$endif}(af,struc,protocol);
// ##### Patch for BSD to avoid "Project XXX raised exception class 'External: SIGPIPE'" error.
{$IF defined(BSD) AND NOT defined(OpenBSD)}
  if result <> INVALID_SOCKET then begin
    on_off := 1;
    fpSetSockOpt(result,integer(SOL_SOCKET),integer(SO_NOSIGPIPE),@on_off,SizeOf(integer));
  end;
{$ENDIF}
end;

function Select(nfds: Integer; readfds,writefds,exceptfds: PFDSet;
  timeout: PTimeVal): Longint;
begin
  result := {$ifdef KYLIX3}LibC.select{$else}fpSelect{$endif}(
    nfds,readfds,writefds,exceptfds,timeout);
end;

function IsNewApi(Family: integer): Boolean;
begin
  result := SockEnhancedApi;
  if not result then
    result := (Family=AF_INET6) and SockWship6Api;
end;

function GetSinPort(const Sin: TVarSin): Integer;
begin
  if (Sin.sin_family=AF_INET6) then
    result := ntohs(Sin.sin6_port) else
    result := ntohs(Sin.sin_port);
end;

function poll(fds: PPollFD; nfds, timeout: integer): integer;
begin
  {$ifdef KYLIX3}
  result := libc.poll(pointer(fds),nfds,timeout);
  {$else}
  result := fppoll(pointer(fds),nfds,timeout);
  {$endif}
end;

{$ifdef KYLIX3} // those functions only use the new API

function SetVarSin(var Sin: TVarSin; const IP,Port: string;
  Family,SockProtocol,SockType: integer; PreferIP4: Boolean): integer;
  function GetAddr(const IP, port: string; var Hints: addrinfo; var Sin: TVarSin): integer;
  var Addr: PAddressInfo;
  begin
    Addr := nil;
    try
      FillChar(Sin, Sizeof(Sin), 0);
      if Hints.ai_socktype=SOCK_RAW then begin
        Hints.ai_socktype := 0;
        Hints.ai_protocol := 0;
        result := LibC.getaddrinfo(pointer(IP), nil, @Hints, Addr);
      end else
        if (IP=cAnyHost) or (IP=c6AnyHost) then begin
          Hints.ai_flags := AI_PASSIVE;
          result := LibC.getaddrinfo(nil, pointer(Port), @Hints, Addr);
        end else
          if (IP = cLocalhost) or (IP = c6Localhost) then
            result := LibC.getaddrinfo(nil, pointer(Port), @Hints, Addr) else
            result := LibC.getaddrinfo(pointer(IP), pointer(Port), @Hints, Addr);
      if (Result=0) and (Addr<>nil) then
        Move(Addr^.ai_addr^, Sin, Addr^.ai_addrlen);
    finally
      if Assigned(Addr) then
        LibC.freeaddrinfo(Addr);
    end;
  end;

var Hints1, Hints2: addrinfo;
    Sin1, Sin2: TVarSin;
    TwoPass: boolean;
begin
  FillChar(Hints1, Sizeof(Hints1), 0);
  FillChar(Hints2, Sizeof(Hints2), 0);
  TwoPass := False;
  if Family=AF_UNSPEC then begin
    if PreferIP4 then begin
      Hints1.ai_family := AF_INET;
      Hints2.ai_family := AF_INET6;
      TwoPass := True;
    end else begin
      Hints1.ai_family := AF_INET6;
      Hints2.ai_family := AF_INET;
      TwoPass := True;
    end;
  end else
    Hints1.ai_family := Family;
  Hints1.ai_socktype := SockType;
  Hints1.ai_protocol := SockProtocol;
  Hints2.ai_socktype := SockType;
  Hints2.ai_protocol := SockProtocol;
  result := GetAddr(IP, Port, Hints1, Sin1);
  if result=0 then
    sin := sin1 else
    if TwoPass then begin
      result := GetAddr(IP, Port, Hints2, Sin2);
      if result=0 then
        sin := sin2;
    end;
end;

function GetSinIP(const Sin: TVarSin): string;
var host: array[0..NI_MAXHOST] of char;
    serv: array[0..NI_MAXSERV] of char;
    r: integer;
begin
  r := LibC.getnameinfo(PSockAddr(@sin)^,SizeOfVarSin(sin), host,NI_MAXHOST,
    serv,NI_MAXSERV, NI_NUMERICHOST+NI_NUMERICSERV);
  if r=0 then
    result := host else
    result := '';
end;

procedure ResolveNameToIP(const Name: AnsiString; Family, SockProtocol, SockType: integer;
  IPList: TStrings; IPListClear: boolean);
var
  Hints: TAddressInfo;
  Addr: PAddressInfo;
  AddrNext: PAddressInfo;
  r, prev: integer;
  host, serv: string;
  hostlen, servlen: integer;
begin
  if IPListClear then
    IPList.Clear;
  Addr := nil;
  try // we force to find TCP/IP
    FillChar(Hints, Sizeof(Hints), 0);
    Hints.ai_family := Family;
    Hints.ai_protocol := SockProtocol;
    Hints.ai_socktype := SockType;
    r := LibC.getaddrinfo(pointer(Name), nil, @Hints, Addr);
    if r=0 then begin
      AddrNext := Addr;
      while not(AddrNext=nil) do begin
        if not(((Family=AF_INET6) and (AddrNext^.ai_family=AF_INET))
          or ((Family=AF_INET) and (AddrNext^.ai_family=AF_INET6))) then begin
          hostlen := NI_MAXHOST;
          servlen := NI_MAXSERV;
          setlength(host,hostlen);
          setlength(serv,servlen);
          r := LibC.getnameinfo(AddrNext^.ai_addr^, AddrNext^.ai_addrlen,
            PChar(host), hostlen, PChar(serv), servlen,
            NI_NUMERICHOST + NI_NUMERICSERV);
          if r=0 then begin
            host := PChar(host);
            IPList.Add(host);
          end;
        end;
        AddrNext := AddrNext^.ai_next;
      end;
    end;
  finally
    if Assigned(Addr) then
      LibC.freeaddrinfo(Addr);
  end;
  if IPList.Count=0 then
    IPList.Add(cAnyHost);
end;

{$else} // FPC version

function SetVarSin(var Sin: TVarSin; const IP,Port: string;
  Family,SockProtocol,SockType: integer; PreferIP4: Boolean): integer;
var TwoPass: boolean;
    f1,f2: integer;

  function GetAddr(f:integer): integer;
  var a4: array[1..1] of TInAddr;
      a6: array[1..1] of TInAddr6;
      he: THostEntry;
  begin
    result := WSAEPROTONOSUPPORT;
    case f of
      AF_INET: begin
        if IP=cAnyHost then begin
          Sin.sin_family := AF_INET;
          result := 0;
        end else begin
          if lowercase(IP)=cLocalHostStr then
            a4[1].s_addr := htonl(INADDR_LOOPBACK) else begin
            a4[1].s_addr := 0;
            result := WSAHOST_NOT_FOUND;
            a4[1] := StrTonetAddr(IP);
            if a4[1].s_addr=INADDR_ANY then
              if GetHostByName(ip,he) then
                a4[1] := HostToNet(he.Addr) else
                Resolvename(ip,a4);
          end;
          if a4[1].s_addr <> INADDR_ANY then begin
            Sin.sin_family := AF_INET;
            sin.sin_addr := a4[1];
            result := 0;
          end;
        end;
      end;
      AF_INET6: begin
        if IP=c6AnyHost then begin
          Sin.sin_family := AF_INET6;
          result := 0;
        end else begin
          if lowercase(IP)=cLocalHostStr then
            SET_LOOPBACK_ADDR6(@a6[1]) else begin
            result := WSAHOST_NOT_FOUND;
            SET_IN6_IF_ADDR_ANY(@a6[1]);
            a6[1] := StrTonetAddr6(IP);
            if IN6_IS_ADDR_UNSPECIFIED(@a6[1]) then
              Resolvename6(ip,a6);
          end;
          if not IN6_IS_ADDR_UNSPECIFIED(@a6[1]) then begin
            Sin.sin_family := AF_INET6;
            sin.sin6_addr := a6[1];
            result := 0;
          end;
        end;
      end;
    end;
  end;
begin
  result := 0;
  if (Family=AF_UNIX) then begin
    Sin.AddressFamily := AF_UNIX;
    Move(IP[1],Sin.sun_path,length(IP));
    Sin.sun_path[length(IP)]:=#0;
    exit;
  end;
  FillChar(Sin,SizeOf(Sin),0);
  Sin.sin_port := Resolveport(port,family,SockProtocol,SockType);
  TwoPass := false;
  if Family=AF_UNSPEC then begin
    if PreferIP4 then begin
      f1 := AF_INET;
      f2 := AF_INET6;
      TwoPass := true;
    end else begin
      f2 := AF_INET6;
      f1 := AF_INET;
      TwoPass := true;
    end;
  end else
    f1 := Family;
  result := GetAddr(f1);
  if result <> 0 then
    if TwoPass then
      result := GetAddr(f2);
end;

function GetSinIP(const Sin: TVarSin): string;
begin
  result := '';
  case sin.AddressFamily of
    AF_INET:  result := NetAddrToStr(sin.sin_addr);
    AF_INET6: result := NetAddrToStr6(sin.sin6_addr);
  end;
end;

procedure ResolveNameToIP(const Name: AnsiString; Family, SockProtocol, SockType: integer;
  IPList: TStrings; IPListClear: boolean);
var x,n: integer;
    a4: array[1..255] of in_addr;
    a6: array[1..255] of Tin6_addr;
    he: THostEntry;
begin
  if IPListClear then
    IPList.Clear;
  if (family=AF_INET) or (family=AF_UNSPEC) then begin
    if lowercase(name)=cLocalHostStr then
      IpList.Add(cLocalHost)
    else if name=cAnyHost then
      IpList.Add(cAnyHost)
    else begin
      a4[1] := StrTonetAddr(name);
      if a4[1].s_addr=INADDR_ANY then
        if GetHostByName(name,he) then begin
          a4[1] := HostToNet(he.Addr);
          x := 1;
        end else
          x := Resolvename(name,a4) else
          x := 1;
      for n := 1  to x do
        IpList.Add(netaddrToStr(a4[n]));
    end;
  end;
  if (family=AF_INET6) or (family=AF_UNSPEC) then begin
    if lowercase(name)=cLocalHostStr then
      IpList.Add(c6LocalHost)
    else if name=c6AnyHost then
      IpList.Add(c6AnyHost)
    else begin
      a6[1] := StrTonetAddr6(name);
      if IN6_IS_ADDR_UNSPECIFIED(@a6[1]) then
        x := Resolvename6(name,a6) else
        x := 1;
      for n := 1  to x do
        IpList.Add(netaddrToStr6(a6[n]));
    end;
  end;
  if IPList.Count=0 then
    IPList.Add(cAnyHost);
end;

function ResolvePort(const Port: string; Family,SockProtocol,SockType: integer): Word;
var ProtoEnt: TProtocolEntry;
    ServEnt: TServiceEntry;
begin
  result := htons(StrToIntDef(Port,0));
  if result=0 then begin
    ProtoEnt.Name := '';
    GetProtocolByNumber(SockProtocol,ProtoEnt);
    ServEnt.port := 0;
    GetServiceByName(Port,ProtoEnt.Name,ServEnt);
    result := ServEnt.port;
  end;
end;

function ResolveIPToName(const IP: string; Family,SockProtocol,SockType: integer): string;
var n: integer;
    a4: array[1..1] of TInAddr;
    a6: array[1..1] of TInAddr6;
    a: array[1..1] of string;
begin
  result := IP;
  a4[1] := StrToNetAddr(IP);
  if a4[1].s_addr <> INADDR_ANY then begin
    n := ResolveAddress(nettohost(a4[1]),a);
    if n>0 then
      result := a[1];
  end else begin
    a6[1] := StrToNetAddr6(IP);
    n := ResolveAddress6(a6[1],a);
    if n>0 then
      result := a[1];
  end;
end;

{$endif KYLIX3}

{$ifdef Linux} // epoll is Linux-specific

{$ifdef FPC} // use Linux.pas wrappers
function epoll_create(size: integer): integer;
begin
  result := Linux.epoll_create(size);
end;

function epoll_ctl(epfd, op, fd: integer; event: PEPollEvent): integer;
begin
  result := Linux.epoll_ctl(epfd, op, fd, pointer(event));
end;

function epoll_wait(epfd: integer; events: PEPollEvent; maxevents, timeout: integer): integer;
begin
  result := Linux.epoll_wait(epfd, pointer(events), maxevents, timeout);
end;

function epoll_close(epfd: integer): integer;
begin
  result := fpClose(epfd);
end;
{$endif}

{$ifdef KYLIX3} // use libc.so wrappers
function epoll_create; external libcmodulename name 'epoll_create';
function epoll_ctl; external libcmodulename name 'epoll_ctl';
function epoll_wait; external libcmodulename name 'epoll_wait';

function epoll_close(epfd: integer): integer;
begin
  result := __close(epfd);
end;
{$endif}

{$endif Linux}

procedure DestroySocketInterface;
begin
  // nothing to do, since we use either the FPC units, either LibC.pas
end;

initialization
  SET_IN6_IF_ADDR_ANY(@in6addr_any);
  SET_LOOPBACK_ADDR6(@in6addr_loopback);
  InitializeCriticalSection(SynSockCS);

finalization
  DeleteCriticalSection(SynSockCS);
end.
