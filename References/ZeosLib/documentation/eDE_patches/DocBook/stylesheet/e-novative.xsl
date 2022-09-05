<?xml version="1.0" encoding="iso-8859-1"?>

<!-- >e-novative> DocBook Environment (eDE)                                  -->
<!-- (c) 2002 e-novative GmbH, Munich, Germany                               -->
<!-- http://www.e-novative.de                                                -->

<!-- e-novative General Configuration                                        -->

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


<!-- You should not modify this file directly (though you can), because      -->
<!-- any modifications to this file will be lost when you upgrade eDE.       -->


<!-- General configuration (all output formats)                              -->


<!-- Enumerate sections (<sect1> - <sect5>)                                  -->
<!-- value: 0: sections are unnumbered                                       -->
<!-- value: 1: sections are automatically numbered (with arabic numbers)     -->
<!--<xsl:param name="section.autolabel">0</xsl:param>-->
<xsl:param name="section.autolabel">1</xsl:param>


<!-- Enumerate FAQ list entries                                              -->
<!-- Value: 0: FAQ list entries are unnumbered                               -->
<!-- Value: 1: FAQ list entries are numbered with arabic numbers             -->
<!--<xsl:param name="qandadiv.autolabel">0</xsl:param>-->
<xsl:param name="qandadiv.autolabel">1</xsl:param>


<!-- Create an index (<index>)                                               -->
<!-- The document must contain an <index> tag pair where the index is        -->
<!-- supposed to appear in the document and <indexterm> tags that mark the   -->
<!-- terms that are supposed to appear in the index.                         -->
<!-- Value: 0: do not create an index                                        -->
<!-- Value: 1: create an index                                               -->
<!--<xsl:param name="generate.index">0</xsl:param>-->
<xsl:param name="generate.index">1</xsl:param>





<!-- HTML configuration                                                      -->




<!-- FO- (PDF) related configuration                                         -->


<!-- Paper format                                                            -->
<!-- All DIN A, B and C sizes are understood.                                -->
<xsl:param name="paper.type">A4</xsl:param>
<!--<xsl:param name="paper.type">A5</xsl:param>-->
<!--<xsl:param name="paper.type">USletter</xsl:param>-->


<!-- Paper orientation                                                       -->
<!-- value: portrait: portrait paper orientation (short edge horizontal)     -->
<!-- value: landscape: landscape paper orientation (short edge vertical)     -->
<xsl:param name="page.orientation">portrait</xsl:param>
<!--<xsl:param name="page.orientation">landscape</xsl:param>-->









<!-- html                                                                    -->


<!-- Display navigation header on each page above the document body          -->
<!-- The navigation consists of the next, previous, up and home links        -->
<!-- applies to: html output only                                            -->
<!-- value: 0: show navigation header                                        -->
<!-- value: 1: hide navigation header                                        -->

<!--<xsl:param name="suppress.header.navigation">0</xsl:param>-->
<xsl:param name="suppress.header.navigation">1</xsl:param>


<!-- Display ruler between navigation header and body                        -->
<!-- applies to: html output only                                            -->
<!-- value: 0: show ruler between navigation header and body                 -->
<!-- value: 1: hide ruler between navigation header and body                 -->

<!--<xsl:param name="header.rule">0</xsl:param>-->
<xsl:param name="header.rule">1</xsl:param>


<!-- Display a custom header on each html page                               -->
<!-- see c:\docbook\stylesheet\custom.xsl                                    -->
<!-- applies to: html output only                                            -->


<!-- Display navigation footer on each page below the document body          -->
<!-- The navigation consists of the next, previous, up and home links        -->
<!-- applies to: html output only                                            -->
<!-- value: 0: show navigation footer                                        -->
<!-- value: 1: hide navigation footer                                        -->

<xsl:param name="suppress.footer.navigation">0</xsl:param>
<!--<xsl:param name="suppress.footer.navigation">1</xsl:param>-->


<!-- Display ruler between body and navigation footer                        -->
<!-- applies to: html output only                                            -->
<!-- value: 0: hide ruler between body and navigation footer                 -->
<!-- value: 1: show ruler between body and navigation footer                 -->

<!--<xsl:param name="footer.rule">0</xsl:param>-->
<xsl:param name="footer.rule">1</xsl:param>


<!-- Section level to create individual pages (chunks) for                   -->
<!-- Note that for chapters always an individual page is created             -->
<!-- applies to: html chunked output only                                    -->
<!-- value: number: section level                                            -->

<!--<xsl:param name="chunk.section.depth">1</xsl:param>-->
<!--<xsl:param name="chunk.section.depth">2</xsl:param>-->
<xsl:param name="chunk.section.depth">3</xsl:param>
<!--<xsl:param name="chunk.section.depth">4</xsl:param>-->
<!--<xsl:param name="chunk.section.depth">5</xsl:param>-->


<!-- chunk-specific settings -->


<!-- create new page for top-level units                                     -->
<!-- applies to: ???                                                         -->
<!-- effectively creates an extra page for each table of contents            -->
<!-- value: 0: first section on same page as toc                             -->
<!-- value: 1: first section new page (toc on individual page)               -->
<xsl:param name="chunk.first.sections">1</xsl:param>


<!-- Use character entities instead of numeric entities where possible       -->
<xsl:param name="saxon.character.representation">native</xsl:param>


<!-- multiple profile values are semicolon-separated                         -->
<!-- applies to: html, fo                                                    -->
<xsl:param name="profile.separator">;</xsl:param>


<!-- profiling: limit output to given attribute values                     -->
<!-- imagine profiling as "filtering" out elements with certain attributes -->
<!-- from the full docbook source. this requires adding attributes         -->
<!-- to some elements, e.g. <para os="win98">                              -->
<!-- a param line must be present here for every profiled attribute        -->
<!-- individual entries are separated by the "profile.separator",          -->
<!-- by default the separator is a semicolon                               -->
<!-- applies to: all                                                       -->
<!-- value: list of attributes appearing in the document                   -->
<xsl:param name="profile.arch"></xsl:param>
<xsl:param name="profile.condition"></xsl:param>
<xsl:param name="profile.conformance"></xsl:param>
<xsl:param name="profile.lang"></xsl:param>
<!-- operating system: e.g. "Windows" or "Linux"                           -->
<xsl:param name="profile.os"></xsl:param>
<xsl:param name="profile.revision"></xsl:param>
<xsl:param name="profile.revisionflag"></xsl:param>
<xsl:param name="profile.role"></xsl:param>
<xsl:param name="profile.security"></xsl:param>
<!-- userlevel: e.g. "beginner", "advanced"                                -->
<xsl:param name="profile.userlevel"></xsl:param>
<xsl:param name="profile.vendor"></xsl:param>
<xsl:param name="profile.attribute"></xsl:param>
<xsl:param name="profile.value"></xsl:param>
<!-- TBD: complete these examples -->



<!-- where to place titles for certain elements                            -->
<!-- do not quote element names and parameters                             -->
<!-- applies to: all                                                       -->
<!-- value: "elementname" before                                           -->
<!-- value: "elementname" after                                            -->
<xsl:param name="formal.title.placement">
figure after
example before
equation after
table after
procedure after
</xsl:param>


<!-- display headers on blank page                                         -->
<!-- applies to: html, fo                                                  -->
<!-- value: 0: hide headers on blank pages                                 -->
<!-- value: 1: show headers on blank pages                                 -->
<xsl:param name="headers.on.blank.pages">0</xsl:param>


<!-- separator displayes between subsequent menu choices                   -->
<!-- this refers to <guimenu> and <guimenuitem> or <guisubmenu> tag        -->
<!-- applies to: html, fo                                                  -->
<!-- value: html string                                                    -->
<xsl:param name="menuchoice.menu.separator">-&gt;</xsl:param>
<!--<xsl:param name="menuchoice.menu.separator">/</xsl:param>-->


<!-- create glossary links for first terms only -->
<!-- first term is special markup for terms occuring the first time -->
<!-- probably no effect when no firstterm tag is used? - or no link -->
<!-- because no firstterm -->
<xsl:param name="firstterm.only.link">0</xsl:param>



<!-- html specific settings -->


<!-- add reference purpose and reference entry to toc -->
<xsl:param name="annotate.toc">1</xsl:param>


<!-- separator between toc numbers and labels -->
<!-- as default a dot and a non-breaking space is used -->
<!-- value: string -->
<!--<xsl:param name="autotoc.label.separator">.&#160;</xsl:param>-->



<!-- render callout lists as definition lists -->
<xsl:param name="callout.list.table">1</xsl:param>


<!-- do not create draft documents -->
<xsl:param name="draft.mode" select="'no'"/>
<!--<xsl:param name="draft.watermark.image" /> -->



<!-- enable better table sizing -->
<xsl:param name="tablecolumns.extension" select="'1'"/>


<!-- bibliography entry separator -->
<xsl:param name="biblioentry.item.separator">. </xsl:param>

<!-- number bibliography entries-->
<xsl:param name="bibliography.numbered">1</xsl:param>

<!--
<xsl:param name="bibliography.collection" select="'http://docbook.sourceforge.net/release/bibliography/bibliography.xml'"/>
-->

<!-- -->
<!--<xsl:param name="css.decoration">1</xsl:param>-->


<!-- create link to description (textobjects) for mediaobjects -->
<!-- value: 0: no link -->
<!-- value: 1: a link -->
<xsl:param name="html.longdesc">0</xsl:param>


<!-- draft watermark graphic -->
<xsl:param name="draft.watermark.image">images/draft.png</xsl:param>


<!-- create empty paragraphs for formatting-->
<!-- value: 0: no empty paragraphs -->
<!-- value: 1: create empty paragraphs -->
<xsl:param name="spacing.paras">0</xsl:param>


<!-- render segmented list as html table -->
<xsl:param name="segmentedlist.as.table">0</xsl:param>


<!-- render variable list as html table -->
<xsl:param name="variablelist.as.table">0</xsl:param>


<!-- format variable list as blocks (fop) -->
<xsl:param name="variablelist.as.blocks">0</xsl:param>


<!-- glossterm auto link -->
<xsl:param name="glossterm.auto.link">1</xsl:param>


<!-- show graphical page navigation -->
<!--
<xsl:param name="navig.graphics" select="1"/>
<xsl:param name="navig.graphics.extension" select="'.png'"/>
<xsl:param name="navig.graphics.path">images/</xsl:param>

show doc titles for next and prev link
<xsl:param name="navig.showtitles" select="1"/>
-->

<!-- if no file extension given for graphics, use this -->
<!--<xsl:param name="graphic.default.extension" select="'.png'"/>-->

<!-- create no extra page for the legal notice -->
<xsl:param name="generate.legalnotice.link" select="0"/>

<!-- create css formatted tables -->
<!--<xsl:param name="table.borders.with.css" select="1"/>-->




<!-- admonitions (caution, note, warning, important, tip)                    -->

<!-- use graphical symbols admonitions                                     -->
<!-- applies to: all                                                       -->
<!-- value: 0: use textual description for admonitions                     -->
<!-- value: 1: use graphical symbols for admonitions                       -->
<xsl:param name="admon.graphics">1</xsl:param>


<!-- css style for admonitions                                             -->
<!-- applies to: html                                                      -->
<!-- no style defined here; the classes are formatted by the css file      -->
<!-- value: css style definition, enclosed in a <xsl:text> tag pair        -->
<!-- value: (empty): no style definition (e.g. css class formatting)       -->
<!--<xsl:param name="admon.style"></xsl:param>-->



<!--
<xsl:text>margin-left: 15px;</xsl:text>
-->


<!-- generate numeric callouts                                             -->
<xsl:param name="callout.graphics">1</xsl:param>

<!--
<xsl:param name="generate.toc">
article   toc,figure,table,example,equation
sect1     toc
sect2     toc
</xsl:param>
-->


<!-- title indentation/left margin -->
<xsl:param name="title.margin.left">0cm</xsl:param>

<!-- indentation in table of contents -->
<!-- value is in points, used by fop processors that don´t support extensions and can´t calculate it -->
<xsl:param name="toc.indent.width">20</xsl:param>


<!-- display link target url after link name -->
<!-- (in print, only display name would appear) -->
<!-- suppresses target if target and name are identical -->
<!-- applies to: fo output only -->
<!-- 0: display url name only, suppress url target -->
<!-- 1: display url name and url target in brackets -->
<xsl:param name="ulink.show">1</xsl:param>


<!-- add page number to cross references -->
<xsl:param name="insert.xref.page.number">1</xsl:param>


<!-- add a custom header to every html page                                  -->
<!-- the header will be formatted by the css class "customheader"            -->
<!-- that is defined in the e-novative.css stylsheet                         -->
<!-- applies to: html output only                                            -->
<!-- You can modify the html code between the xsl:template tags or just      -->
<!-- comment the whole <xsl:template ... </xsl:template> block out           -->
<!-- to make the header disappear                                            -->

<xsl:template name="user.header.content">
<div id="customheader">
<a href="http://zeos.firmos.at/portal.php" style="color: #fff; font-weight: bold;">ZeosLib</a><b> - open source tools for your database solutions</b>
</div>
</xsl:template>


<!-- add a custom footer to every html page                                  -->
<!-- the footer will be formatted by the css class "customfooter"            -->
<!-- that is defined in the e-novative.css stylsheet                         -->
<!-- applies to: html output only                                            -->
<!-- You can modify the html code between the xsl:template tags or just      -->
<!-- comment the whole <xsl:template ... </xsl:template> block out.          -->
<!-- to make the footer disappear                                            -->

<xsl:template name="user.footer.content">
<div id="customfooter">
(c) 1999 - 2006 The Zeos Development Group
</div>
</xsl:template>


<!-- You should NOT change the following settings unless you know what you   -->
<!-- are doing and have a good reason for the modification. eDE relies       -->
<!-- on the following settings and requires them to function properly        -->


<!-- Indent generated html                                                   -->
<!-- Not supported by all XSLT processors.                                   -->
<xsl:param name="chunker.output.indent">yes</xsl:param>



<!-- CSS stylesheet to format the generated HTML                             -->
<!-- Note: the stylesheet url is relative to the location of the html page.  -->
<!-- To change the html formatting, you can create custom CSS stylesheets for-->
<!-- article and book and/or custom CSS stylesheets for individual documents.-->
<xsl:param name="html.stylesheet">ede.css</xsl:param>


<!-- Use element id as HTML filename                                         -->
<xsl:param name="use.id.as.filename">1</xsl:param>


<!-- Generate valid HTML                                                     -->
<!-- Avoids creation of nested html paragraphs (<p>) from nested <para> tags -->
<!-- (in strict HTML, nested paragraphs are not allowed).                    -->
<xsl:param name="make.valid.html">1</xsl:param>


<!-- Clean up HTML                                                           -->
<!-- Try to create "better" HTML by transforming the result HTML. Does not   -->
<!-- work with all XSLT processors.                                          -->
<xsl:param name="html.cleanup">0</xsl:param>


<!-- Use XSLT processor extensions                                           -->
<!-- Enables extensions that offer functionality beyond regular XML proessor -->
<!-- capabilities. -->
<xsl:param name="use.extensions">1</xsl:param>


<!-- Enable FOP extensions                                                   -->
<!-- Allows creation of PDF bookmarks to ease browsing of PDF documents.     -->
<xsl:param name="fop.extensions">1</xsl:param>


<!-- Use Tablecolumns extensions                                             -->
<!-- Improves HTML table display                                             -->
<xsl:param name="tablecolumns.extensions">0</xsl:param>


<!-- Use Graphicsize extensions                                              -->
<!-- Allows XSLT processor to retrieve the size from graphics. Does not work -->
<!-- with all XSLT processors.                                               -->
<xsl:param name="graphicsize.extensions">0</xsl:param>


<!-- Use Textinsert extensions                                               -->
<!-- Allows inserting text files directly into the XML source.               -->
<xsl:param name="textinsert.extensions">0</xsl:param>


</xsl:stylesheet>
