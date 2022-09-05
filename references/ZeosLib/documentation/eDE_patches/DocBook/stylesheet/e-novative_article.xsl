<?xml version="1.0" encoding="iso-8859-1"?>

<!-- >e-novative> DocBook Environment (eDE)                                  -->
<!-- (c) 2002 e-novative GmbH, Munich, Germany                               -->
<!-- http://www.e-novative.de                                                -->

<!-- e-novative configuration for articles                                   -->

<!-- This file is part of eDE                                                -->

<!-- eDE is free software; you can redistribute it and/or modify             -->
<!-- it under the terms of the GNU General Public License as published by    -->
<!-- the Free Software Foundation; either version 2 of the License, or       -->
<!-- (at your option) any later version.                                     -->

<!-- eDE is distributed in the hope that it will be useful,                  -->
<!-- but WITHOUT ANY WARRANTY; without even the implied warranty of          -->
<!-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           -->
<!-- GNU General Public License for more details.                            -->

<!-- You should have received a copy of the GNU General Public License       -->
<!-- along with eDe; if not, write to the Free Software                      -->
<!-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA -->


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">


<!-- General configuration (all output formats)                              -->


<!-- Number of levels displayed in a table of contents (<sect1> - <sect5>)   -->
<!-- This controls the table of contents depth, not where they are created.  -->
<!-- Value: number                                                           -->
<!--<xsl:param name="toc.section.depth">0</xsl:param>-->
<xsl:param name="toc.section.depth">1</xsl:param>
<!--<xsl:param name="toc.section.depth">2</xsl:param>-->
<!--<xsl:param name="toc.section.depth">3</xsl:param>-->
<!--<xsl:param name="toc.section.depth">4</xsl:param>-->
<!--<xsl:param name="toc.section.depth">5</xsl:param>-->


<!-- Section level to display a table of contents for (<sect1> - <sect5>)    -->
<!-- This controls where to display a table of contents, not the depth.      -->
<!-- If 0, <sect1> has no table of contents, resulting in no table of        -->
<!-- contents (articles), or a table of contents for each chapter (books)    -->
<!--<xsl:param name="generate.section.toc.level">0</xsl:param>-->
<xsl:param name="generate.section.toc.level">1</xsl:param>
<!--<xsl:param name="generate.section.toc.level">2</xsl:param>-->
<!--<xsl:param name="generate.section.toc.level">3</xsl:param>-->
<!--<xsl:param name="generate.section.toc.level">4</xsl:param>-->
<!--<xsl:param name="generate.section.toc.level">5</xsl:param>-->


<!-- FO- (PDF) related configuration                                         -->


<!-- page sided layout                                                       -->
<!-- 0: single-sided-layout (page numbers are centered)                      -->
<!-- 1: double-sided layout (page numbers alternate at left and right)       -->
<xsl:param name="double.sided">0</xsl:param>
<!--<xsl:param name="double.sided">1</xsl:param>-->


<!-- Number of columns in text body                                          -->
<!-- Value: number                                                           -->
<!--<xsl:param name="column.count.body">1</xsl:param>-->
<xsl:param name="column.count.body">1</xsl:param>
<!--<xsl:param name="column.count.body">3</xsl:param>-->



</xsl:stylesheet>
