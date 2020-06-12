{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


//{$define uselibc:=cdecl; external;}

Uses
  initc;

threadvar internal_socketerror : cint;

const 
  {$ifdef BEOS}
    {$ifdef HAIKU}
    libname = 'network';
    {$else}
    libname = 'net';
    {$endif}
  {$else}
  libname='c';
  {$endif}

function cfpaccept      (s:cint; addrx : psockaddr; addrlen : psocklen):cint; cdecl; external libname name 'accept';
function cfpbind        (s:cint; addrx : psockaddr; addrlen : tsocklen):cint;  cdecl; external libname name 'bind';
function cfpconnect     (s:cint; name  : psockaddr; namelen : tsocklen):cint;  cdecl; external libname name 'connect';
function cfpgetpeername (s:cint; name  : psockaddr; namelen : psocklen):cint; cdecl; external libname name 'getpeername';
function cfpgetsockname (s:cint; name  : psockaddr; namelen : psocklen):cint; cdecl; external libname name 'getsockname';
function cfpgetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen : psocklen):cint; cdecl; external libname name 'getsockopt';
function cfplisten      (s:cint; backlog : cint):cint;                          cdecl; external libname name 'listen';
function cfprecv        (s:cint; buf: pointer; len: size_t; flags: cint):ssize_t; cdecl; external libname name 'recv';
function cfprecvfrom    (s:cint; buf: pointer; len: size_t; flags: cint; from : psockaddr; fromlen : psocklen):ssize_t; cdecl; external libname name 'recvfrom';
//function cfprecvmsg     (s:cint; msg: pmsghdr; flags:cint):ssize_t; cdecl; external libname name '';
function cfpsend        (s:cint; msg:pointer; len:size_t; flags:cint):ssize_t; cdecl; external libname name 'send';
function cfpsendto      (s:cint; msg:pointer; len:size_t; flags:cint; tox :psockaddr; tolen: tsocklen):ssize_t; cdecl; external libname name 'sendto';
//function cfpsendmsg   (s:cint; hdr: pmsghdr; flags:cint):ssize; cdecl; external libname name '';
function cfpsetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen :tsocklen):cint; cdecl; external libname name 'setsockopt';
function cfpshutdown    (s:cint; how:cint):cint; cdecl; external libname name 'shutdown';
function cfpsocket      (domain:cint; xtype:cint; protocol: cint):cint; cdecl; external libname name 'socket';

{$if defined(BEOS) and not defined(HAIKU)}
// function unavailable under BeOS
{$else}
function cfpsocketpair  (d:cint; xtype:cint; protocol:cint; sv:pcint):cint; cdecl; external libname name 'socketpair';
{$endif}


function fpaccept      (s:cint; addrx : psockaddr; addrlen : psocklen):cint;

begin
 fpaccept:=cfpaccept(s,addrx,addrlen);
 internal_socketerror:=fpgeterrno; 
end;

function fpbind (s:cint; addrx : psockaddr; addrlen : tsocklen):cint;
begin
  fpbind:=cfpbind (s,addrx,addrlen);
  internal_socketerror:=fpgeterrno;
end;

function fpconnect     (s:cint; name  : psockaddr; namelen : tsocklen):cint;
begin
  fpconnect:=cfpconnect (s,name,namelen);
  internal_socketerror:=fpgeterrno;
end;

function fpgetpeername (s:cint; name  : psockaddr; namelen : psocklen):cint;
begin
  fpgetpeername:=cfpgetpeername (s,name,namelen);
  internal_socketerror:=fpgeterrno;
end;

function fpgetsockname (s:cint; name  : psockaddr; namelen : psocklen):cint;
begin
  fpgetsockname:=cfpgetsockname(s,name,namelen);
  internal_socketerror:=fpgeterrno;
end;

function fpgetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen : psocklen):cint;
begin
  fpgetsockopt:=cfpgetsockopt(s,level,optname,optval,optlen);
  internal_socketerror:=fpgeterrno;
end;

function fplisten      (s:cint; backlog : cint):cint;
begin
  fplisten:=cfplisten(s,backlog);
  internal_socketerror:=fpgeterrno;
end;

function fprecv         (s:cint; buf: pointer; len: size_t; flags:cint):ssize_t;
begin
  fprecv:= cfprecv      (s,buf,len,flags);
  internal_socketerror:=fpgeterrno;
end;

function fprecvfrom    (s:cint; buf: pointer; len: size_t; flags: cint; from : psockaddr; fromlen : psocklen):ssize_t;
begin
  fprecvfrom:= cfprecvfrom (s,buf,len,flags,from,fromlen);
  internal_socketerror:=fpgeterrno;
end;

function fpsend         (s:cint; msg:pointer; len:size_t; flags:cint):ssize_t;
begin
  fpsend:=cfpsend (s,msg,len,flags);
  internal_socketerror:=fpgeterrno;
end;

function fpsendto       (s:cint; msg:pointer; len:size_t; flags:cint; tox :psockaddr; tolen: tsocklen):ssize_t;
begin
  fpsendto:=cfpsendto (s,msg,len,flags,tox,tolen);
  internal_socketerror:=fpgeterrno;
end;

function fpsetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen :tsocklen):cint;
begin
  fpsetsockopt:=cfpsetsockopt(s,level,optname,optval,optlen);
  internal_socketerror:=fpgeterrno;
end;

function fpshutdown     (s:cint; how:cint):cint;
begin
  fpshutdown:=cfpshutdown(s,how);
  internal_socketerror:=fpgeterrno;
end;

function fpsocket       (domain:cint; xtype:cint; protocol: cint):cint;
begin
  fpsocket:=cfpsocket(domain,xtype,protocol);
  internal_socketerror:=fpgeterrno;
end;

{$if defined(BEOS) and not defined(HAIKU)}
// function unavailable under BeOS
function fpsocketpair  (d:cint; xtype:cint; protocol:cint; sv:pcint):cint;
begin
  internal_socketerror:= -1; // TODO : check if it is an error 
end;
{$else}
function fpsocketpair  (d:cint; xtype:cint; protocol:cint; sv:pcint):cint;
begin
  fpsocketpair:=cfpsocketpair(d,xtype,protocol,sv);
  internal_socketerror:=fpgeterrno;
end;
{$endif}
