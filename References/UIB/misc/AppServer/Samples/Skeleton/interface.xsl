<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="text" encoding="ascii"/>
<xsl:template match="/library">
<xsl:text>{ This file is generated automaticaly, do not modify }</xsl:text>
unit <xsl:value-of select="@name"/>_Intf;
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}
interface
uses Classes;

// Class identifiers
<xsl:for-each select="object">
    <xsl:if test="position() = 1">const</xsl:if>
    CLSID_<xsl:value-of select="@name"/>: TGUID = '<xsl:value-of select="@uid"/>';</xsl:for-each>

// Objects interfaces
<xsl:for-each select="object">
  <xsl:if test="position() = 1">type</xsl:if>
  I<xsl:value-of select="@name"/> = interface
  ['<xsl:value-of select="@uid"/><xsl:text>']</xsl:text>
  <xsl:for-each select="function"><xsl:text>
  </xsl:text>
    <xsl:choose>
      <xsl:when test="@return!=''">  function </xsl:when><xsl:otherwise>  procedure </xsl:otherwise></xsl:choose>
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
    <xsl:text>; stdcall;</xsl:text>
  </xsl:for-each>
  end;
</xsl:for-each>
implementation

end.
</xsl:template>
</xsl:stylesheet>
