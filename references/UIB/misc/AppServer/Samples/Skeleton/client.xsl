<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="text" encoding="ascii"/>
<xsl:template match="/library">
<xsl:text>{ This file is generated automaticaly, do not modify }</xsl:text>
unit <xsl:value-of select="@name"/>_Client;
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}
interface
uses Classes, {$IFDEF FPC}sockets{$ELSE}WinSock{$ENDIF}, PDGUtils, <xsl:value-of select="@name"/>_Intf;

<xsl:for-each select="object">
  <xsl:if test="position() = 1">type</xsl:if>
  T<xsl:value-of select="@name"/>Client = class(TInterfacedObject, I<xsl:value-of select="@name"/>)
  <xsl:text>private</xsl:text>
    FSocketHandle: longint;  
  protected
    function EncodeStream(stream: TPooledMemoryStream; socket: longint): boolean; virtual;
    function DecodeStream(stream: TPooledMemoryStream; socket: longint): boolean; virtual;
  public
    property SocketHandle: longint read FSocketHandle;
    constructor Create(const server: string; port: word); virtual;
    destructor Destroy; override;<xsl:text/>
  <xsl:for-each select="function"><xsl:text>
  </xsl:text>
  <xsl:choose>
    <xsl:when test="@return">  function </xsl:when><xsl:otherwise>  procedure </xsl:otherwise></xsl:choose>
  <xsl:value-of select="@name"/>
    <xsl:for-each select="field">
      <xsl:if test="position()=1">(</xsl:if>
      <xsl:if test="@modifier">
	  <xsl:value-of select="@modifier"/><xsl:text> </xsl:text>
      </xsl:if>
      <xsl:value-of select="@name"/>: <xsl:value-of select="@type"/>
      <xsl:choose>
        <xsl:when test="position()!=last()">; </xsl:when>
        <xsl:otherwise>)</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
    <xsl:if test="@return!=''">
      <xsl:text>: </xsl:text> <xsl:value-of select="@return"/>
    </xsl:if>
    <xsl:text>; virtual; stdcall;</xsl:text>
  </xsl:for-each>
  end;
</xsl:for-each>
implementation
uses SysUtils;
<xsl:for-each select="object">
{ T<xsl:value-of select="@name"/>Client }

function T<xsl:value-of select="@name"/>Client.EncodeStream(stream: TPooledMemoryStream; socket: longint): boolean;
begin
  <xsl:choose>
    <xsl:when test="@compress='yes'">Result := CompressStream(stream, socket);</xsl:when>
    <xsl:otherwise>Result := stream.SaveToSocket(socket);</xsl:otherwise>
  </xsl:choose>
end;

function T<xsl:value-of select="@name"/>Client.DecodeStream(stream: TPooledMemoryStream; socket: longint): boolean;
begin
  <xsl:choose>
    <xsl:when test="@compress='yes'">Result := DecompressStream(socket, stream);</xsl:when>
    <xsl:otherwise>Result := stream.LoadFromSocket(socket);</xsl:otherwise>
  </xsl:choose>
end;

constructor T<xsl:value-of select="@name"/>Client.Create(const server: string; port: word);
var
  p: PHostEnt;
  Address: TSockAddr;
  uid: TGUID;
  SO_True: Integer;
begin
  SO_True := -1;
  FSocketHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  SetSockOpt(FSocketHandle, SOL_SOCKET, SO_REUSEADDR, PChar(@SO_True), SizeOf(SO_True));
  SetSockOpt(FSocketHandle, IPPROTO_TCP, TCP_NODELAY, PChar(@SO_True), SizeOf(SO_True));
  Address.sin_family := AF_INET;
  Address.sin_port := htons(port);
  p := gethostbyname(PChar(server));
  if p = nil then
    raise Exception.Create('Can''t find remote host.') else
    Address.sin_addr.S_addr := PInteger(p^.h_addr^)^;
  if connect(FSocketHandle, Address, sizeof(Address)) = SOCKET_ERROR then
    raise Exception.Create('Can''t connect to remote host.');
  uid := CLSID_<xsl:value-of select="@name"/>;
  send(FSocketHandle, uid, sizeOf(uid), 0);
end;

destructor T<xsl:value-of select="@name"/>Client.Destroy;
begin
  closesocket(FSocketHandle);
  inherited;
end;
<xsl:for-each select="function"><xsl:text>
</xsl:text>
<xsl:choose>
  <xsl:when test="@return">function T</xsl:when><xsl:otherwise>procedure T</xsl:otherwise></xsl:choose>
<xsl:value-of select="../@name"/>Client.<xsl:value-of select="@name"/>
  <xsl:for-each select="field">
    <xsl:if test="position()=1">(</xsl:if>
    <xsl:if test="@modifier">
        <xsl:value-of select="@modifier"/><xsl:text> </xsl:text>
    </xsl:if>
    <xsl:value-of select="@name"/>: <xsl:value-of select="@type"/>
    <xsl:choose>
      <xsl:when test="position()!=last()">; </xsl:when>
      <xsl:otherwise>)</xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>
  <xsl:if test="@return!=''">
    <xsl:text>: </xsl:text> <xsl:value-of select="@return"/>
  </xsl:if>
  <xsl:text>;</xsl:text>
var
  fn_stream: TPooledMemoryStream;
  fn_id: integer;
  fn_error: string;<xsl:text/>
<xsl:if test="@return='string'">
  Result_len: integer;</xsl:if>
<xsl:for-each select="field">
<xsl:if test="@type='string' or @type='TMemoryStream'"><xsl:text>
  </xsl:text><xsl:value-of select="@name"/>_len: integer;</xsl:if>
</xsl:for-each>
begin
  fn_stream := TPooledMemoryStream.Create;
  try
    fn_id := <xsl:value-of select="@fnid"/>;
    fn_stream.write(fn_id, sizeof(fn_id));<xsl:text/>     
<xsl:for-each select="field">
<xsl:if test="not(@modifier) or @modifier!='out'">
<xsl:choose>
  <xsl:when test="@type='string'"><xsl:text>
    </xsl:text><xsl:value-of select="@name"/>_len := Length(<xsl:value-of select="@name"/>);
    fn_stream.Write(<xsl:value-of select="@name"/>_len, sizeof(<xsl:value-of select="@name"/>_len));
    fn_stream.Write(PChar(<xsl:value-of select="@name"/>)^, <xsl:value-of select="@name"/>_len);<xsl:text/>
  </xsl:when>
  <xsl:when test="@type='TMemoryStream'"><xsl:text>
    </xsl:text><xsl:value-of select="@name"/>_len := <xsl:value-of select="@name"/>.size;
    fn_stream.Write(<xsl:value-of select="@name"/>_len, sizeof(<xsl:value-of select="@name"/>_len));
    fn_stream.Write(<xsl:value-of select="@name"/>.Memory^, <xsl:value-of select="@name"/>_len);<xsl:text/>
  </xsl:when>
  <xsl:otherwise>
    fn_stream.Write(<xsl:value-of select="@name"/>, sizeof(<xsl:value-of select="@name"/>));<xsl:text/>
  </xsl:otherwise>
</xsl:choose>
</xsl:if>
</xsl:for-each>
    fn_stream.Seek(0, soFromBeginning);
    if not EncodeStream(fn_stream, FSocketHandle) then
      raise ERemoteError.Create('Communication error.');
    fn_stream.Size := 0;
    if not DecodeStream(fn_stream, FSocketHandle) then
      raise ERemoteError.Create('Communication error.');
    fn_stream.Seek(0, soFromBeginning);<xsl:text/>
<xsl:for-each select="field">
<xsl:if test="@modifier='out' or @modifier='var'">
<xsl:choose>
  <xsl:when test="@type='string'"><xsl:text>
    </xsl:text>fn_stream.read(<xsl:value-of select="@name"/>_len, sizeof(<xsl:value-of select="@name"/>_len));
    SetLength(<xsl:value-of select="@name"/>, <xsl:value-of select="@name"/>_len);
    fn_stream.Read(PChar(<xsl:value-of select="@name"/>)^, <xsl:value-of select="@name"/>_len);<xsl:text/>
  </xsl:when>
  <xsl:when test="@type='TMemoryStream'"><xsl:text>
    </xsl:text>fn_stream.read(<xsl:value-of select="@name"/>_len, sizeof(<xsl:value-of select="@name"/>_len));
    <xsl:value-of select="@name"/>.size := <xsl:value-of select="@name"/>_len;
    <xsl:value-of select="@name"/>.Seek(0, soFromBeginning);
    fn_stream.read(<xsl:value-of select="@name"/>.Memory^, <xsl:value-of select="@name"/>_len);<xsl:text/>
  </xsl:when>
  <xsl:otherwise>
    fn_stream.Read(<xsl:value-of select="@name"/>, sizeof(<xsl:value-of select="@name"/>));<xsl:text/>
  </xsl:otherwise>
</xsl:choose>
</xsl:if>
</xsl:for-each>
<xsl:if test="@return">
<xsl:choose>
  <xsl:when test="@return='string'"><xsl:text>
    </xsl:text>fn_stream.read(Result_len, sizeof(Result_len));
    SetLength(Result, Result_len);
    fn_stream.Read(PChar(Result)^, Result_len);<xsl:text/>
  </xsl:when>
  <xsl:otherwise>
    fn_stream.Read(Result, SizeOf(Result));<xsl:text/>
  </xsl:otherwise>
</xsl:choose>
</xsl:if>
    fn_error := fn_stream.ReadString;
    if not(fn_error = '') then
      raise ERemoteError.Create(fn_error);
  finally
    fn_stream.Free;
  end;
end;
</xsl:for-each>
</xsl:for-each>
{$IFNDEF FPC}
var
  Data: TWSAData ;
initialization
  WSAStartup($0202, Data);
finalization
  WSACleanup;
{$ENDIF}
end.
</xsl:template>
</xsl:stylesheet>