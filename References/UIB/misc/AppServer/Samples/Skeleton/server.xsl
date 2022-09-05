<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="text" encoding="ascii"/>
<xsl:template match="/library">
<xsl:text>{ This file is generated automaticaly, do not modify }</xsl:text>
unit <xsl:value-of select="@name"/>_Server;
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}
interface
uses Classes, {$IFDEF FPC}sockets{$ELSE}WinSock{$ENDIF}, PDGUtils, PDGSocketStub, <xsl:value-of select="@name"/>_Intf {$ifdef madExcept}, madExcept {$endif};

<xsl:for-each select="object">
  <xsl:if test="position() = 1">type</xsl:if>
  T<xsl:value-of select="@name"/>Stub = class(TSocketStub, I<xsl:value-of select="@name"/>)
  <xsl:text>private</xsl:text>
  <xsl:for-each select="function">
    procedure Dec_<xsl:value-of select="@name"/><xsl:text>(stream: TPooledMemoryStream);</xsl:text>
  </xsl:for-each>
  <xsl:text>
  protected
    function EncodeStream(stream: TPooledMemoryStream; socket: longint): boolean; virtual;
    function DecodeStream(stream: TPooledMemoryStream; socket: longint): boolean; virtual;
    function Run: Cardinal; override;</xsl:text>
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
    <xsl:text>; virtual; stdcall; abstract;</xsl:text>
  </xsl:for-each>
  end;
</xsl:for-each>
implementation
uses SysUtils;

{$ifndef madExcept}
// work around Delphi Bug
const
  CMadExceptVersion = 'dummy';
{$endif}
<xsl:for-each select="object">
{ T<xsl:value-of select="@name"/>Stub }

function T<xsl:value-of select="@name"/>Stub.DecodeStream(stream: TPooledMemoryStream; socket: longint): boolean;
begin
  <xsl:choose>
    <xsl:when test="@compress='yes'">Result := DecompressStream(socket, stream);</xsl:when>
    <xsl:otherwise>Result := stream.LoadFromSocket(socket);</xsl:otherwise>
  </xsl:choose>
end;

function T<xsl:value-of select="@name"/>Stub.EncodeStream(stream: TPooledMemoryStream; socket: longint): boolean;
begin
  <xsl:choose>
    <xsl:when test="@compress='yes'">Result := CompressStream(stream, socket);</xsl:when>
    <xsl:otherwise>Result := stream.SaveToSocket(socket);</xsl:otherwise>
  </xsl:choose>
end;

function T<xsl:value-of select="@name"/>Stub.Run: Cardinal;
var
  fn: Integer;
  stream: TPooledMemoryStream;
  procedure SendException(e: Exception);
  begin
    stream.WriteString('[' + e.ClassName + '] ' + E.Message);
  end;
begin
  Result := 0;
  while not Stopped do
  begin
    stream := TPooledMemoryStream.Create;
    try
      if not DecodeStream(stream, SocketHandle) then exit;
      stream.Seek(0, soFromBeginning);
      if not(stream.Read(fn, sizeof(fn)) = SizeOf(fn)) then exit;
      try
        case fn of<xsl:text/>
        <xsl:for-each select="function"><xsl:text>
          </xsl:text><xsl:value-of select="@fnid"/> : Dec_<xsl:value-of select="@name"/>(stream);<xsl:text/>
        </xsl:for-each>
        else
          exit;
        end;
        stream.WriteInteger(0);
      except
        on E: Exception do
        begin
          SendException(E);
        {$ifdef madExcept}
          {$if (CMadExceptVersion = '3.0a') or (CMadExceptVersion = '3.0b')}
            HandleException(etNormal, E);
          {$else}
            HandleException(false, E);
          {$ifend}
        {$endif}
        end;
      end;
      stream.Seek(0, soFromBeginning);
      if not EncodeStream(stream, SocketHandle) then exit;
    finally
      stream.Free;
    end;
  end;
end;
<xsl:for-each select="function">
<xsl:variable name="ResultHaveBuffer" select="@return='TMemoryStream' or @return='string'"/>
<!-- Generate var section -->
procedure T<xsl:value-of select="../@name"/>Stub.Dec_<xsl:value-of select="@name"/>(stream: TPooledMemoryStream);<xsl:text/>
 <xsl:if test="@return">
var<xsl:text/>
  <xsl:if test="$ResultHaveBuffer">
  Return_len: integer;</xsl:if>
  Return: <xsl:value-of select="@return"/>;<xsl:text/>
  </xsl:if>
  <xsl:for-each select="field">
   <xsl:variable name="TypeHaveBuffer" select="@type='TMemoryStream' or @type='string'"/>
   <xsl:if test="not(../@return) and position()=1">
var</xsl:if>
  <xsl:if test="$TypeHaveBuffer"><xsl:text>
  </xsl:text><xsl:value-of select="@name"/>_len: integer;</xsl:if><xsl:text>
  </xsl:text><xsl:value-of select="@name"/>: <xsl:value-of select="@type"/>;<xsl:text/>
</xsl:for-each>
begin<xsl:text/>
<!-- create class -->
<xsl:for-each select="field">
<xsl:choose>
  <xsl:when test="@type='TMemoryStream'"><xsl:text>
  </xsl:text><xsl:value-of select="@name"/> := TMemoryStream.Create;</xsl:when>
</xsl:choose>
</xsl:for-each>

<xsl:for-each select="field">
  <!-- Read input data -->
  <xsl:if test="not(@modifier) or @modifier!='out'">
  <xsl:choose>
    <xsl:when test="@type='string'">
  stream.Read(<xsl:value-of select="@name"/>_len, sizeof(<xsl:value-of select="@name"/>_len));
  SetLength(<xsl:value-of select="@name"/>, <xsl:value-of select="@name"/>_len);
  if <xsl:value-of select="@name"/>_len > 0 then
    stream.Read(<xsl:value-of select="@name"/>[1], <xsl:value-of select="@name"/>_len);<xsl:text/>
    </xsl:when>
    <xsl:when test="@type='TMemoryStream'">
  stream.Read(<xsl:value-of select="@name"/>_len, sizeof(<xsl:value-of select="@name"/>_len));
  <xsl:value-of select="@name"/>.Size := <xsl:value-of select="@name"/>_len;
  stream.Read(<xsl:value-of select="@name"/>.Memory^, <xsl:value-of select="@name"/>_len);<xsl:text/>
    </xsl:when>
    <xsl:otherwise>
  stream.Read(<xsl:value-of select="@name"/>, sizeof(<xsl:value-of select="@name"/>));<xsl:text/>
    </xsl:otherwise>
  </xsl:choose>
  </xsl:if>
</xsl:for-each>
  try
    <!-- Invoke method -->
    <xsl:if test="@return">Return := <xsl:text/></xsl:if>
    <xsl:value-of select="@name"/>
    <xsl:for-each select="field">
      <xsl:if test="position()=1">(</xsl:if>
      <xsl:value-of select="@name"/>
      <xsl:choose>
        <xsl:when test="position()!=last()">, </xsl:when>
        <xsl:otherwise>)</xsl:otherwise>
      </xsl:choose>
    </xsl:for-each>
    <xsl:text>;</xsl:text>
  finally
    stream.Size := 0;<xsl:text/>
<!-- write output parametters -->
<xsl:for-each select="field">
  <xsl:if test="@modifier='var' or @modifier='out'">
  <xsl:choose>
    <xsl:when test="@type='string'"><xsl:text>
    </xsl:text><xsl:value-of select="@name"/>_len := length(<xsl:value-of select="@name"/>);
    stream.Write(<xsl:value-of select="@name"/>_len, SizeOf(<xsl:value-of select="@name"/>_len));
    if <xsl:value-of select="@name"/>_len > 0 then
      stream.Write(<xsl:value-of select="@name"/>[1], <xsl:value-of select="@name"/>_len);<xsl:text/>
    </xsl:when>
    <xsl:when test="@type='TMemoryStream'"><xsl:text>
    </xsl:text><xsl:value-of select="@name"/>_len := <xsl:value-of select="@name"/>.Size;
    stream.Write(<xsl:value-of select="@name"/>_len, SizeOf(<xsl:value-of select="@name"/>_len));
    <xsl:value-of select="@name"/>.SaveToStream(stream);<xsl:text/>
    </xsl:when>
    <xsl:otherwise>
    stream.Write(<xsl:value-of select="@name"/>, sizeof(<xsl:value-of select="@name"/>));<xsl:text/>
    </xsl:otherwise>
  </xsl:choose>
  </xsl:if>
</xsl:for-each>
<!-- Write Result (no class) -->
<xsl:if test="@return">
  <xsl:choose>
    <xsl:when test="@return='string'">
    Return_len := length(Return);
    stream.Write(Return_len, SizeOf(Return_len));
    if Return_len > 0 then
      stream.Write(Return[1], Return_len);<xsl:text/>
    </xsl:when>
    <xsl:otherwise>
    stream.Write(Return, sizeof(Return));<xsl:text/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:if>
<!-- Destroy class -->
<xsl:for-each select="field">
  <xsl:if test="@type='TMemoryStream'"><xsl:text>
    </xsl:text><xsl:value-of select="@name"/>.free;<xsl:text/>
  </xsl:if>
</xsl:for-each>
  end;
end;
</xsl:for-each>
</xsl:for-each>
end.
</xsl:template>
</xsl:stylesheet>
