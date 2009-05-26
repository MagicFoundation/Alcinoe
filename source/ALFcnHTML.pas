{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      Alcinoe HTML Functions
Version:      3.51

Description:  Functions to work on Html Tag (extract Text, HTML Encode,
              HTML Decode, etc. The function ALHTMLdecode and
              ALHTMLEncode is to encode decode HTML entity
              like &nbsp;

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

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

History :     19/10/2005: Make The code independant of the current local
                          and work with UTF-8 encoding; Also build a most
                          complete list of HTML entities
              10/09/2007: create ALCompactHtmlTagParams in ALFcnHTML
              10/11/2007: move ALXMLTextElementDecode to ALUTF8XMLTextElementDecode and
              						to ALUTF8XMLTextElementDecode
              						add support for reference like &#x20AC; and &#39;
              						

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALFcnHTML;

interface

uses Classes;

{Html Tag function}
procedure ALANSIExtractHTMLText(HtmlContent: String; LstExtractedResourceText : Tstrings; Const DecodeHTMLText: Boolean = True);
procedure ALUTF8ExtractHTMLText(HtmlContent: UTF8String; LstExtractedResourceText : Tstrings; Const DecodeHTMLText: Boolean = True);
function  ALXMLTextElementEncode(Src: string; const useNumericReference: boolean = True): string;
function  ALUTF8XMLTextElementDecode(Src: Utf8string): Utf8string;
function  ALANSIXMLTextElementDecode(Src: string): string;
function  ALANSIHTMLEncode(Src: string; Const EncodeASCIIHtmlEntities: Boolean = True; const useNumericReference: boolean = True): string;
function  ALUTF8HTMLEncode(Src: UTF8String; Const EncodeASCIIHtmlEntities: Boolean = True; const useNumericReference: boolean = True): UTF8String;
function  ALANSIHTMLDecode(Src: string): string;
function  ALUTF8HTMLDecode(Src: Utf8string): Utf8string;

{for internal use}
procedure ALHideHtmlUnwantedTagForHTMLHandleTagfunct(Var HtmlContent: String; Const DeleteBodyOfUnwantedTag: Boolean = False; const ReplaceUnwantedTagCharBy: Char = #1);
procedure ALInitHtmlEntitiesLst(aLstHtmlEntities: Tstrings);
procedure ALCompactHtmlTagParams(TagParams: TStrings);

implementation

uses sysutils,
     alFcnString,
     ALQuickSortList;

Var vALhtml_LstEntities: Tstrings;

{*********************************************************}
procedure ALInitHtmlEntitiesLst(aLstHtmlEntities: Tstrings);
Begin

  aLstHtmlEntities.Clear;
  aLstHtmlEntities.AddObject('zwnj',pointer(8204)); // zero width non-joiner,   U+200C NEW RFC 2070 -->
  aLstHtmlEntities.AddObject('zwj',pointer(8205)); // zero width joiner, U+200D NEW RFC 2070 -->
  aLstHtmlEntities.AddObject('zeta',pointer(950)); // greek small letter zeta, U+03B6 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Zeta',pointer(918)); // greek capital letter zeta, U+0396 -->
  aLstHtmlEntities.AddObject('yuml',pointer(255)); // latin small letter y with diaeresis, U+00FF ISOlat1 -->
  aLstHtmlEntities.AddObject('Yuml',pointer(376)); // latin capital letter Y with diaeresis,   U+0178 ISOlat2 -->
  aLstHtmlEntities.AddObject('yen',pointer(165)); // yen sign = yuan sign, U+00A5 ISOnum -->
  aLstHtmlEntities.AddObject('yacute',pointer(253)); // latin small letter y with acute, U+00FD ISOlat1 -->
  aLstHtmlEntities.AddObject('Yacute',pointer(221)); // latin capital letter Y with acute, U+00DD ISOlat1 -->
  aLstHtmlEntities.AddObject('xi',pointer(958)); // greek small letter xi, U+03BE ISOgrk3 -->
  aLstHtmlEntities.AddObject('Xi',pointer(926)); // greek capital letter xi, U+039E ISOgrk3 -->
  aLstHtmlEntities.AddObject('weierp',pointer(8472)); // script capital P = power set    = Weierstrass p, U+2118 ISOamso -->
  aLstHtmlEntities.AddObject('uuml',pointer(252)); // latin small letter u with diaeresis, U+00FC ISOlat1 -->
  aLstHtmlEntities.AddObject('Uuml',pointer(220)); // latin capital letter U with diaeresis, U+00DC ISOlat1 -->
  aLstHtmlEntities.AddObject('upsilon',pointer(965)); // greek small letter upsilon,   U+03C5 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Upsilon',pointer(933)); // greek capital letter upsilon,   U+03A5 ISOgrk3 -->
  aLstHtmlEntities.AddObject('upsih',pointer(978)); // greek upsilon with hook symbol,   U+03D2 NEW -->
  aLstHtmlEntities.AddObject('uml',pointer(168)); // diaeresis = spacing diaeresis, U+00A8 ISOdia -->
  aLstHtmlEntities.AddObject('ugrave',pointer(249)); // latin small letter u with grave, U+00F9 ISOlat1 -->
  aLstHtmlEntities.AddObject('Ugrave',pointer(217)); // latin capital letter U with grave, U+00D9 ISOlat1 -->
  aLstHtmlEntities.AddObject('ucirc',pointer(251)); // latin small letter u with circumflex, U+00FB ISOlat1 -->
  aLstHtmlEntities.AddObject('Ucirc',pointer(219)); // latin capital letter U with circumflex, U+00DB ISOlat1 -->
  aLstHtmlEntities.AddObject('uArr',pointer(8657)); // upwards double arrow, U+21D1 ISOamsa -->
  aLstHtmlEntities.AddObject('uarr',pointer(8593)); // upwards arrow, U+2191 ISOnum-->
  aLstHtmlEntities.AddObject('uacute',pointer(250)); // latin small letter u with acute, U+00FA ISOlat1 -->
  aLstHtmlEntities.AddObject('Uacute',pointer(218)); // latin capital letter U with acute, U+00DA ISOlat1 -->
  aLstHtmlEntities.AddObject('trade',pointer(8482)); // trade mark sign, U+2122 ISOnum -->
  aLstHtmlEntities.AddObject('times',pointer(215)); // multiplication sign, U+00D7 ISOnum -->
  aLstHtmlEntities.AddObject('tilde',pointer(732)); // small tilde, U+02DC ISOdia -->
  aLstHtmlEntities.AddObject('thorn',pointer(254)); // latin small letter thorn, U+00FE ISOlat1 -->
  aLstHtmlEntities.AddObject('THORN',pointer(222)); // latin capital letter THORN, U+00DE ISOlat1 -->
  aLstHtmlEntities.AddObject('thinsp',pointer(8201)); // thin space, U+2009 ISOpub -->
  aLstHtmlEntities.AddObject('thetasym',pointer(977)); // greek small letter theta symbol,   U+03D1 NEW -->
  aLstHtmlEntities.AddObject('theta',pointer(952)); // greek small letter theta,   U+03B8 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Theta',pointer(920)); // greek capital letter theta,   U+0398 ISOgrk3 -->
  aLstHtmlEntities.AddObject('there4',pointer(8756)); // therefore, U+2234 ISOtech -->
  aLstHtmlEntities.AddObject('tau',pointer(964)); // greek small letter tau, U+03C4 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Tau',pointer(932)); // greek capital letter tau, U+03A4 -->
  aLstHtmlEntities.AddObject('szlig',pointer(223)); // latin small letter sharp s = ess-zed, U+00DF ISOlat1 -->
  aLstHtmlEntities.AddObject('supe',pointer(8839)); // superset of or equal to,    U+2287 ISOtech -->
  aLstHtmlEntities.AddObject('sup3',pointer(179)); // superscript three = superscript digit three = cubed, U+00B3 ISOnum -->
  aLstHtmlEntities.AddObject('sup2',pointer(178)); // superscript two = superscript digit two = squared, U+00B2 ISOnum -->
  aLstHtmlEntities.AddObject('sup1',pointer(185)); // superscript one = superscript digit one, U+00B9 ISOnum -->
  aLstHtmlEntities.AddObject('sup',pointer(8835)); // superset of, U+2283 ISOtech -->
  aLstHtmlEntities.AddObject('sum',pointer(8721)); // n-ary sumation, U+2211 ISOamsb -->
  aLstHtmlEntities.AddObject('sube',pointer(8838)); // subset of or equal to, U+2286 ISOtech -->
  aLstHtmlEntities.AddObject('sub',pointer(8834)); // subset of, U+2282 ISOtech -->
  aLstHtmlEntities.AddObject('spades',pointer(9824)); // black spade suit, U+2660 ISOpub -->
  aLstHtmlEntities.AddObject('sim',pointer(8764)); // tilde operator = varies with = similar to,    U+223C ISOtech -->
  aLstHtmlEntities.AddObject('sigmaf',pointer(962)); // greek small letter final sigma,   U+03C2 ISOgrk3 -->
  aLstHtmlEntities.AddObject('sigma',pointer(963)); // greek small letter sigma,   U+03C3 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Sigma',pointer(931)); // greek capital letter sigma,   U+03A3 ISOgrk3 -->
  aLstHtmlEntities.AddObject('shy',pointer(173)); // soft hyphen = discretionary hyphen, U+00AD ISOnum -->
  aLstHtmlEntities.AddObject('sect',pointer(167)); // section sign, U+00A7 ISOnum -->
  aLstHtmlEntities.AddObject('sdot',pointer(8901)); // dot operator, U+22C5 ISOamsb -->
  aLstHtmlEntities.AddObject('scaron',pointer(353)); // latin small letter s with caron,   U+0161 ISOlat2 -->
  aLstHtmlEntities.AddObject('Scaron',pointer(352)); // latin capital letter S with caron,   U+0160 ISOlat2 -->
  aLstHtmlEntities.AddObject('sbquo',pointer(8218)); // single low-9 quotation mark, U+201A NEW -->
  aLstHtmlEntities.AddObject('rsquo',pointer(8217)); // right single quotation mark,   U+2019 ISOnum -->
  aLstHtmlEntities.AddObject('rsaquo',pointer(8250)); // single right-pointing angle quotation mark,   U+203A ISO proposed -->
  aLstHtmlEntities.AddObject('rlm',pointer(8207)); // right-to-left mark, U+200F NEW RFC 2070 -->
  aLstHtmlEntities.AddObject('rho',pointer(961)); // greek small letter rho, U+03C1 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Rho',pointer(929)); // greek capital letter rho, U+03A1 -->
  aLstHtmlEntities.AddObject('rfloor',pointer(8971)); // right floor, U+230B ISOamsc  -->
  aLstHtmlEntities.AddObject('reg',pointer(174)); // registered sign = registered trade mark sign, U+00AE ISOnum -->
  aLstHtmlEntities.AddObject('real',pointer(8476)); // blackletter capital R = real part symbol,    U+211C ISOamso -->
  aLstHtmlEntities.AddObject('rdquo',pointer(8221)); // right double quotation mark,   U+201D ISOnum -->
  aLstHtmlEntities.AddObject('rceil',pointer(8969)); // right ceiling, U+2309 ISOamsc  -->
  aLstHtmlEntities.AddObject('rArr',pointer(8658)); // rightwards double arrow,    U+21D2 ISOtech -->
  aLstHtmlEntities.AddObject('rarr',pointer(8594)); // rightwards arrow, U+2192 ISOnum -->
  aLstHtmlEntities.AddObject('raquo',pointer(187)); // right-pointing double angle quotation mark = right pointing guillemet, U+00BB ISOnum -->
  aLstHtmlEntities.AddObject('rang',pointer(9002)); // right-pointing angle bracket = ket,    U+232A ISOtech -->
  aLstHtmlEntities.AddObject('radic',pointer(8730)); // square root = radical sign,    U+221A ISOtech -->
  aLstHtmlEntities.AddObject('quot',pointer(34)); // quotation mark = APL quote,   U+0022 ISOnum -->
  aLstHtmlEntities.AddObject('psi',pointer(968)); // greek small letter psi, U+03C8 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Psi',pointer(936)); // greek capital letter psi,   U+03A8 ISOgrk3 -->
  aLstHtmlEntities.AddObject('prop',pointer(8733)); // proportional to, U+221D ISOtech -->
  aLstHtmlEntities.AddObject('prod',pointer(8719)); // n-ary product = product sign,    U+220F ISOamsb -->
  aLstHtmlEntities.AddObject('Prime',pointer(8243)); // double prime = seconds = inches,    U+2033 ISOtech -->
  aLstHtmlEntities.AddObject('prime',pointer(8242)); // prime = minutes = feet, U+2032 ISOtech -->
  aLstHtmlEntities.AddObject('pound',pointer(163)); // pound sign, U+00A3 ISOnum -->
  aLstHtmlEntities.AddObject('plusmn',pointer(177)); // plus-minus sign = plus-or-minus sign, U+00B1 ISOnum -->
  aLstHtmlEntities.AddObject('piv',pointer(982)); // greek pi symbol, U+03D6 ISOgrk3 -->
  aLstHtmlEntities.AddObject('pi',pointer(960)); // greek small letter pi, U+03C0 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Pi',pointer(928)); // greek capital letter pi, U+03A0 ISOgrk3 -->
  aLstHtmlEntities.AddObject('phi',pointer(966)); // greek small letter phi, U+03C6 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Phi',pointer(934)); // greek capital letter phi,   U+03A6 ISOgrk3 -->
  aLstHtmlEntities.AddObject('perp',pointer(8869)); // up tack = orthogonal to = perpendicular,    U+22A5 ISOtech -->
  aLstHtmlEntities.AddObject('permil',pointer(8240)); // per mille sign, U+2030 ISOtech -->
  aLstHtmlEntities.AddObject('part',pointer(8706)); // partial differential, U+2202 ISOtech  -->
  aLstHtmlEntities.AddObject('para',pointer(182)); // pilcrow sign = paragraph sign, U+00B6 ISOnum -->
  aLstHtmlEntities.AddObject('ouml',pointer(246)); // latin small letter o with diaeresis, U+00F6 ISOlat1 -->
  aLstHtmlEntities.AddObject('Ouml',pointer(214)); // latin capital letter O with diaeresis, U+00D6 ISOlat1 -->
  aLstHtmlEntities.AddObject('otimes',pointer(8855)); // circled times = vector product,    U+2297 ISOamsb -->
  aLstHtmlEntities.AddObject('otilde',pointer(245)); // latin small letter o with tilde, U+00F5 ISOlat1 -->
  aLstHtmlEntities.AddObject('Otilde',pointer(213)); // latin capital letter O with tilde, U+00D5 ISOlat1 -->
  aLstHtmlEntities.AddObject('oslash',pointer(248)); // latin small letter o with stroke, = latin small letter o slash, U+00F8 ISOlat1 -->
  aLstHtmlEntities.AddObject('Oslash',pointer(216)); // latin capital letter O with stroke = latin capital letter O slash, U+00D8 ISOlat1 -->
  aLstHtmlEntities.AddObject('ordm',pointer(186)); // masculine ordinal indicator, U+00BA ISOnum -->
  aLstHtmlEntities.AddObject('ordf',pointer(170)); // feminine ordinal indicator, U+00AA ISOnum -->
  aLstHtmlEntities.AddObject('or',pointer(8744)); // logical or = vee, U+2228 ISOtech -->
  aLstHtmlEntities.AddObject('oplus',pointer(8853)); // circled plus = direct sum,    U+2295 ISOamsb -->
  aLstHtmlEntities.AddObject('omicron',pointer(959)); // greek small letter omicron, U+03BF NEW -->
  aLstHtmlEntities.AddObject('Omicron',pointer(927)); // greek capital letter omicron, U+039F -->
  aLstHtmlEntities.AddObject('omega',pointer(969)); // greek small letter omega,   U+03C9 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Omega',pointer(937)); // greek capital letter omega,   U+03A9 ISOgrk3 -->
  aLstHtmlEntities.AddObject('oline',pointer(8254)); // overline = spacing overscore,    U+203E NEW -->
  aLstHtmlEntities.AddObject('ograve',pointer(242)); // latin small letter o with grave, U+00F2 ISOlat1 -->
  aLstHtmlEntities.AddObject('Ograve',pointer(210)); // latin capital letter O with grave, U+00D2 ISOlat1 -->
  aLstHtmlEntities.AddObject('oelig',pointer(339)); // latin small ligature oe, U+0153 ISOlat2 -->
  aLstHtmlEntities.AddObject('OElig',pointer(338)); // latin capital ligature OE,   U+0152 ISOlat2 -->
  aLstHtmlEntities.AddObject('ocirc',pointer(244)); // latin small letter o with circumflex, U+00F4 ISOlat1 -->
  aLstHtmlEntities.AddObject('Ocirc',pointer(212)); // latin capital letter O with circumflex, U+00D4 ISOlat1 -->
  aLstHtmlEntities.AddObject('oacute',pointer(243)); // latin small letter o with acute, U+00F3 ISOlat1 -->
  aLstHtmlEntities.AddObject('Oacute',pointer(211)); // latin capital letter O with acute, U+00D3 ISOlat1 -->
  aLstHtmlEntities.AddObject('nu',pointer(957)); // greek small letter nu, U+03BD ISOgrk3 -->
  aLstHtmlEntities.AddObject('Nu',pointer(925)); // greek capital letter nu, U+039D -->
  aLstHtmlEntities.AddObject('ntilde',pointer(241)); // latin small letter n with tilde, U+00F1 ISOlat1 -->
  aLstHtmlEntities.AddObject('Ntilde',pointer(209)); // latin capital letter N with tilde, U+00D1 ISOlat1 -->
  aLstHtmlEntities.AddObject('nsub',pointer(8836)); // not a subset of, U+2284 ISOamsn -->
  aLstHtmlEntities.AddObject('notin',pointer(8713)); // not an element of, U+2209 ISOtech -->
  aLstHtmlEntities.AddObject('not',pointer(172)); // not sign, U+00AC ISOnum -->
  aLstHtmlEntities.AddObject('ni',pointer(8715)); // contains as member, U+220B ISOtech -->
  aLstHtmlEntities.AddObject('ne',pointer(8800)); // not equal to, U+2260 ISOtech -->
  aLstHtmlEntities.AddObject('ndash',pointer(8211)); // en dash, U+2013 ISOpub -->
  aLstHtmlEntities.AddObject('nbsp',pointer(160)); // no-break space = non-breaking space, U+00A0 ISOnum -->
  aLstHtmlEntities.AddObject('nabla',pointer(8711)); // nabla = backward difference,    U+2207 ISOtech -->
  aLstHtmlEntities.AddObject('mu',pointer(956)); // greek small letter mu, U+03BC ISOgrk3 -->
  aLstHtmlEntities.AddObject('Mu',pointer(924)); // greek capital letter mu, U+039C -->
  aLstHtmlEntities.AddObject('minus',pointer(8722)); // minus sign, U+2212 ISOtech -->
  aLstHtmlEntities.AddObject('middot',pointer(183)); // middle dot = Georgian comma = Greek middle dot, U+00B7 ISOnum -->
  aLstHtmlEntities.AddObject('micro',pointer(181)); // micro sign, U+00B5 ISOnum -->
  aLstHtmlEntities.AddObject('mdash',pointer(8212)); // em dash, U+2014 ISOpub -->
  aLstHtmlEntities.AddObject('macr',pointer(175)); // macron = spacing macron = overline = APL overbar, U+00AF ISOdia -->
  aLstHtmlEntities.AddObject('lt',pointer(60)); // less-than sign, U+003C ISOnum -->
  aLstHtmlEntities.AddObject('lsquo',pointer(8216)); // left single quotation mark,   U+2018 ISOnum -->
  aLstHtmlEntities.AddObject('lsaquo',pointer(8249)); // single left-pointing angle quotation mark,   U+2039 ISO proposed -->
  aLstHtmlEntities.AddObject('lrm',pointer(8206)); // left-to-right mark, U+200E NEW RFC 2070 -->
  aLstHtmlEntities.AddObject('loz',pointer(9674)); // lozenge, U+25CA ISOpub -->
  aLstHtmlEntities.AddObject('lowast',pointer(8727)); // asterisk operator, U+2217 ISOtech -->
  aLstHtmlEntities.AddObject('lfloor',pointer(8970)); // left floor = apl downstile,    U+230A ISOamsc  -->
  aLstHtmlEntities.AddObject('le',pointer(8804)); // less-than or equal to, U+2264 ISOtech -->
  aLstHtmlEntities.AddObject('ldquo',pointer(8220)); // left double quotation mark,   U+201C ISOnum -->
  aLstHtmlEntities.AddObject('lceil',pointer(8968)); // left ceiling = apl upstile,    U+2308 ISOamsc  -->
  aLstHtmlEntities.AddObject('lArr',pointer(8656)); // leftwards double arrow, U+21D0 ISOtech -->
  aLstHtmlEntities.AddObject('larr',pointer(8592)); // leftwards arrow, U+2190 ISOnum -->
  aLstHtmlEntities.AddObject('laquo',pointer(171)); // left-pointing double angle quotation mark = left pointing guillemet, U+00AB ISOnum -->
  aLstHtmlEntities.AddObject('lang',pointer(9001)); // left-pointing angle bracket = bra,    U+2329 ISOtech -->
  aLstHtmlEntities.AddObject('lambda',pointer(955)); // greek small letter lambda,   U+03BB ISOgrk3 -->
  aLstHtmlEntities.AddObject('Lambda',pointer(923)); // greek capital letter lambda,   U+039B ISOgrk3 -->
  aLstHtmlEntities.AddObject('kappa',pointer(954)); // greek small letter kappa,   U+03BA ISOgrk3 -->
  aLstHtmlEntities.AddObject('Kappa',pointer(922)); // greek capital letter kappa, U+039A -->
  aLstHtmlEntities.AddObject('iuml',pointer(239)); // latin small letter i with diaeresis, U+00EF ISOlat1 -->
  aLstHtmlEntities.AddObject('Iuml',pointer(207)); // latin capital letter I with diaeresis, U+00CF ISOlat1 -->
  aLstHtmlEntities.AddObject('isin',pointer(8712)); // element of, U+2208 ISOtech -->
  aLstHtmlEntities.AddObject('iquest',pointer(191)); // inverted question mark = turned question mark, U+00BF ISOnum -->
  aLstHtmlEntities.AddObject('iota',pointer(953)); // greek small letter iota, U+03B9 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Iota',pointer(921)); // greek capital letter iota, U+0399 -->
  aLstHtmlEntities.AddObject('int',pointer(8747)); // integral, U+222B ISOtech -->
  aLstHtmlEntities.AddObject('infin',pointer(8734)); // infinity, U+221E ISOtech -->
  aLstHtmlEntities.AddObject('image',pointer(8465)); // blackletter capital I = imaginary part,    U+2111 ISOamso -->
  aLstHtmlEntities.AddObject('igrave',pointer(236)); // latin small letter i with grave, U+00EC ISOlat1 -->
  aLstHtmlEntities.AddObject('Igrave',pointer(204)); // latin capital letter I with grave, U+00CC ISOlat1 -->
  aLstHtmlEntities.AddObject('iexcl',pointer(161)); // inverted exclamation mark, U+00A1 ISOnum -->
  aLstHtmlEntities.AddObject('icirc',pointer(238)); // latin small letter i with circumflex, U+00EE ISOlat1 -->
  aLstHtmlEntities.AddObject('Icirc',pointer(206)); // latin capital letter I with circumflex, U+00CE ISOlat1 -->
  aLstHtmlEntities.AddObject('iacute',pointer(237)); // latin small letter i with acute, U+00ED ISOlat1 -->
  aLstHtmlEntities.AddObject('Iacute',pointer(205)); // latin capital letter I with acute, U+00CD ISOlat1 -->
  aLstHtmlEntities.AddObject('hellip',pointer(8230)); // horizontal ellipsis = three dot leader,    U+2026 ISOpub  -->
  aLstHtmlEntities.AddObject('hearts',pointer(9829)); // black heart suit = valentine,    U+2665 ISOpub -->
  aLstHtmlEntities.AddObject('hArr',pointer(8660)); // left right double arrow,    U+21D4 ISOamsa -->
  aLstHtmlEntities.AddObject('harr',pointer(8596)); // left right arrow, U+2194 ISOamsa -->
  aLstHtmlEntities.AddObject('gt',pointer(62)); // greater-than sign, U+003E ISOnum -->
  aLstHtmlEntities.AddObject('ge',pointer(8805)); // greater-than or equal to,    U+2265 ISOtech -->
  aLstHtmlEntities.AddObject('gamma',pointer(947)); // greek small letter gamma,   U+03B3 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Gamma',pointer(915)); // greek capital letter gamma,   U+0393 ISOgrk3 -->
  aLstHtmlEntities.AddObject('frasl',pointer(8260)); // fraction slash, U+2044 NEW -->
  aLstHtmlEntities.AddObject('frac34',pointer(190)); // vulgar fraction three quarters = fraction three quarters, U+00BE ISOnum -->
  aLstHtmlEntities.AddObject('frac14',pointer(188)); // vulgar fraction one quarter = fraction one quarter, U+00BC ISOnum -->
  aLstHtmlEntities.AddObject('frac12',pointer(189)); // vulgar fraction one half = fraction one half, U+00BD ISOnum -->
  aLstHtmlEntities.AddObject('forall',pointer(8704)); // for all, U+2200 ISOtech -->
  aLstHtmlEntities.AddObject('fnof',pointer(402)); // latin small f with hook = function   = florin, U+0192 ISOtech -->
  aLstHtmlEntities.AddObject('exist',pointer(8707)); // there exists, U+2203 ISOtech -->
  aLstHtmlEntities.AddObject('euro',pointer(8364)); // euro sign, U+20AC NEW -->
  aLstHtmlEntities.AddObject('euml',pointer(235)); // latin small letter e with diaeresis, U+00EB ISOlat1 -->
  aLstHtmlEntities.AddObject('Euml',pointer(203)); // latin capital letter E with diaeresis, U+00CB ISOlat1 -->
  aLstHtmlEntities.AddObject('eth',pointer(240)); // latin small letter eth, U+00F0 ISOlat1 -->
  aLstHtmlEntities.AddObject('ETH',pointer(208)); // latin capital letter ETH, U+00D0 ISOlat1 -->
  aLstHtmlEntities.AddObject('eta',pointer(951)); // greek small letter eta, U+03B7 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Eta',pointer(919)); // greek capital letter eta, U+0397 -->
  aLstHtmlEntities.AddObject('equiv',pointer(8801)); // identical to, U+2261 ISOtech -->
  aLstHtmlEntities.AddObject('epsilon',pointer(949)); // greek small letter epsilon,   U+03B5 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Epsilon',pointer(917)); // greek capital letter epsilon, U+0395 -->
  aLstHtmlEntities.AddObject('ensp',pointer(8194)); // en space, U+2002 ISOpub -->
  aLstHtmlEntities.AddObject('emsp',pointer(8195)); // em space, U+2003 ISOpub -->
  aLstHtmlEntities.AddObject('empty',pointer(8709)); // empty set = null set = diameter,    U+2205 ISOamso -->
  aLstHtmlEntities.AddObject('egrave',pointer(232)); // latin small letter e with grave, U+00E8 ISOlat1 -->
  aLstHtmlEntities.AddObject('Egrave',pointer(200)); // latin capital letter E with grave, U+00C8 ISOlat1 -->
  aLstHtmlEntities.AddObject('ecirc',pointer(234)); // latin small letter e with circumflex, U+00EA ISOlat1 -->
  aLstHtmlEntities.AddObject('Ecirc',pointer(202)); // latin capital letter E with circumflex, U+00CA ISOlat1 -->
  aLstHtmlEntities.AddObject('eacute',pointer(233)); // latin small letter e with acute, U+00E9 ISOlat1 -->
  aLstHtmlEntities.AddObject('Eacute',pointer(201)); // latin capital letter E with acute, U+00C9 ISOlat1 -->
  aLstHtmlEntities.AddObject('divide',pointer(247)); // division sign, U+00F7 ISOnum -->
  aLstHtmlEntities.AddObject('diams',pointer(9830)); // black diamond suit, U+2666 ISOpub -->
  aLstHtmlEntities.AddObject('delta',pointer(948)); // greek small letter delta,   U+03B4 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Delta',pointer(916)); // greek capital letter delta,   U+0394 ISOgrk3 -->
  aLstHtmlEntities.AddObject('deg',pointer(176)); // degree sign, U+00B0 ISOnum -->
  aLstHtmlEntities.AddObject('dArr',pointer(8659)); // downwards double arrow, U+21D3 ISOamsa -->
  aLstHtmlEntities.AddObject('darr',pointer(8595)); // downwards arrow, U+2193 ISOnum -->
  aLstHtmlEntities.AddObject('Dagger',pointer(8225)); // double dagger, U+2021 ISOpub -->
  aLstHtmlEntities.AddObject('dagger',pointer(8224)); // dagger, U+2020 ISOpub -->
  aLstHtmlEntities.AddObject('curren',pointer(164)); // currency sign, U+00A4 ISOnum -->
  aLstHtmlEntities.AddObject('cup',pointer(8746)); // union = cup, U+222A ISOtech -->
  aLstHtmlEntities.AddObject('crarr',pointer(8629)); // downwards arrow with corner leftwards    = carriage return, U+21B5 NEW -->
  aLstHtmlEntities.AddObject('copy',pointer(169)); // copyright sign, U+00A9 ISOnum -->
  aLstHtmlEntities.AddObject('cong',pointer(8773)); // approximately equal to, U+2245 ISOtech -->
  aLstHtmlEntities.AddObject('clubs',pointer(9827)); // black club suit = shamrock,    U+2663 ISOpub -->
  aLstHtmlEntities.AddObject('circ',pointer(710)); // modifier letter circumflex accent,   U+02C6 ISOpub -->
  aLstHtmlEntities.AddObject('chi',pointer(967)); // greek small letter chi, U+03C7 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Chi',pointer(935)); // greek capital letter chi, U+03A7 -->
  aLstHtmlEntities.AddObject('cent',pointer(162)); // cent sign, U+00A2 ISOnum -->
  aLstHtmlEntities.AddObject('cedil',pointer(184)); // cedilla = spacing cedilla, U+00B8 ISOdia -->
  aLstHtmlEntities.AddObject('ccedil',pointer(231)); // latin small letter c with cedilla, U+00E7 ISOlat1 -->
  aLstHtmlEntities.AddObject('Ccedil',pointer(199)); // latin capital letter C with cedilla, U+00C7 ISOlat1 -->
  aLstHtmlEntities.AddObject('cap',pointer(8745)); // intersection = cap, U+2229 ISOtech -->
  aLstHtmlEntities.AddObject('bull',pointer(8226)); // bullet = black small circle,    U+2022 ISOpub  -->
  aLstHtmlEntities.AddObject('brvbar',pointer(166)); // broken bar = broken vertical bar, U+00A6 ISOnum -->
  aLstHtmlEntities.AddObject('beta',pointer(946)); // greek small letter beta, U+03B2 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Beta',pointer(914)); // greek capital letter beta, U+0392 -->
  aLstHtmlEntities.AddObject('bdquo',pointer(8222)); // double low-9 quotation mark, U+201E NEW -->
  aLstHtmlEntities.AddObject('auml',pointer(228)); // latin small letter a with diaeresis, U+00E4 ISOlat1 -->
  aLstHtmlEntities.AddObject('Auml',pointer(196)); // latin capital letter A with diaeresis, U+00C4 ISOlat1 -->
  aLstHtmlEntities.AddObject('atilde',pointer(227)); // latin small letter a with tilde, U+00E3 ISOlat1 -->
  aLstHtmlEntities.AddObject('Atilde',pointer(195)); // latin capital letter A with tilde, U+00C3 ISOlat1 -->
  aLstHtmlEntities.AddObject('asymp',pointer(8776)); // almost equal to = asymptotic to,    U+2248 ISOamsr -->
  aLstHtmlEntities.AddObject('aring',pointer(229)); // latin small letter a with ring above = latin small letter a ring, U+00E5 ISOlat1 -->
  aLstHtmlEntities.AddObject('Aring',pointer(197)); // latin capital letter A with ring above = latin capital letter A ring, U+00C5 ISOlat1 -->
  aLstHtmlEntities.AddObject('ang',pointer(8736)); // angle, U+2220 ISOamso -->
  aLstHtmlEntities.AddObject('and',pointer(8743)); // logical and = wedge, U+2227 ISOtech -->
  aLstHtmlEntities.AddObject('amp',pointer(38)); // ampersand, U+0026 ISOnum -->
  aLstHtmlEntities.AddObject('alpha',pointer(945)); // greek small letter alpha,   U+03B1 ISOgrk3 -->
  aLstHtmlEntities.AddObject('Alpha',pointer(913)); // greek capital letter alpha, U+0391 -->
  aLstHtmlEntities.AddObject('alefsym',pointer(8501)); // alef symbol = first transfinite cardinal,    U+2135 NEW -->
  aLstHtmlEntities.AddObject('agrave',pointer(224)); // latin small letter a with grave = latin small letter a grave, U+00E0 ISOlat1 -->
  aLstHtmlEntities.AddObject('Agrave',pointer(192)); // latin capital letter A with grave = latin capital letter A grave, U+00C0 ISOlat1 -->
  aLstHtmlEntities.AddObject('aelig',pointer(230)); // latin small letter ae = latin small ligature ae, U+00E6 ISOlat1 -->
  aLstHtmlEntities.AddObject('AElig',pointer(198)); // latin capital letter AE = latin capital ligature AE, U+00C6 ISOlat1 -->
  aLstHtmlEntities.AddObject('acute',pointer(180)); // acute accent = spacing acute, U+00B4 ISOdia -->
  aLstHtmlEntities.AddObject('acirc',pointer(226)); // latin small letter a with circumflex, U+00E2 ISOlat1 -->
  aLstHtmlEntities.AddObject('Acirc',pointer(194)); // latin capital letter A with circumflex, U+00C2 ISOlat1 -->
  aLstHtmlEntities.AddObject('aacute',pointer(225)); // latin small letter a with acute, U+00E1 ISOlat1 -->
  aLstHtmlEntities.AddObject('Aacute',pointer(193)); // latin capital letter A with acute, U+00C1 ISOlat1 -->

end;

{*********************************************}
{ because of such link: <A HREF = "obie2.html">
  that is split in 3 line in TagParams}
Procedure ALCompactHtmlTagParams(TagParams: TStrings);
Var i: integer;
    S1, S2, S3: String;
    P1, P2, P3: integer;
    Flag2, Flag3: boolean;
Begin
  i := 0;
  While i <= TagParams.Count - 2 do begin
    S1 := TagParams[i];
    S2 := TagParams[i+1];
    if i <= TagParams.Count - 3 then S3 := TagParams[i+2]
    else S3 := '';
    P1 := AlCharPos('=',S1);
    P2 := AlCharPos('=',S2);
    P3 := AlCharPos('=',S3);
    Flag2 := (S2 <> '') and (S2[1] in ['''','"']);
    Flag3 := (S3 <> '') and (S3[1] in ['''','"']);
    IF (P1 <= 0) and
       (S2 = '=') then begin {<A HREF = "obie2.html">}
      If (i <= TagParams.Count - 2) and
         (
          flag3 or
          (P3 <= 0)
         )
      then begin
        TagParams[i] := S1 + S2 + S3;
        tagParams.Delete(i+2);
      end
      else TagParams[i] := S1 + S2;
      tagParams.Delete(i+1);
    end
    else if (S1 <> '') and
            (P1 = length(S1)) and
            (flag2 or (P2 <=0)) then begin {<A HREF= "obie2.html">}
      TagParams[i] := S1 + S2;
      tagParams.Delete(i+1);
    end
    else if (S1 <> '') and
            (P1 <= 0) and
            (AlCharPos('=',S2) = 1)  then begin {<A HREF ="obie2.html">}
      TagParams[i] := S1 + S2;
      tagParams.Delete(i+1);
    end;
    inc(i);
  end;
end;

{******************************************************}
procedure ALUTF8ExtractHTMLText(HtmlContent: UTF8String;
                                LstExtractedResourceText : Tstrings;
                                Const DecodeHTMLText: Boolean = True);

  {-----------------------------------------------------------}
  procedure intenalAdd2LstExtractedResourceText(S: utf8String);
  Begin
    If DecodeHTMLText then Begin
      S := alUTF8HtmlDecode(trim(S));
      S := AlStringReplace(
                           S,
                           #13,
                           ' ',
                           [rfreplaceAll]
                          );
      S := AlStringReplace(
                           S,
                           #10,
                           ' ',
                           [rfreplaceAll]
                          );
      S := AlStringReplace(
                           S,
                           #9,
                           ' ',
                           [rfreplaceAll]
                          );
      While AlPos('  ',S) > 0 Do
        S := AlStringReplace(
                             S,
                             '  ',
                             ' ',
                             [rfreplaceAll]
                            );
      s := Trim(S);
    end;
    If s <> '' then LstExtractedResourceText.add(S);
  end;

Var P1: integer;
Begin
  ALHideHtmlUnwantedTagForHTMLHandleTagfunct(string(HtmlContent), True);
  HtmlContent := ALFastTagReplace(
                                  HtmlContent,
                                  '<',
                                  '>',
                                  #2, {this char is not use in html}
                                  [rfreplaceall]
                                 );
  HtmlContent := ALStringReplace(
                                 HtmlContent,
                                 #1, {default ReplaceUnwantedTagCharBy use by ALHideHtmlUnwantedTagForHTMLHandleTagfunct ; this char is not use in html}
                                 '<',
                                 [rfreplaceall]
                                );

  LstExtractedResourceText.Clear;
  P1 := ALpos(#2,HtmlContent);
  While P1 > 0 do begin
    If P1 > 1 then intenalAdd2LstExtractedResourceText(
                                                       ALCopyStr(
                                                                 HtmlContent,
                                                                 1,
                                                                 p1-1
                                                                )
                                                      );
    delete(HtmlContent,1,P1);
    P1 := ALpos(#2,HtmlContent);
  end;
  intenalAdd2LstExtractedResourceText(HtmlContent);
end;

{**************************************************}
procedure ALANSIExtractHTMLText(HtmlContent: String;
                                LstExtractedResourceText : Tstrings;
                                Const DecodeHTMLText: Boolean = True);
Var I: Integer;
Begin
  ALUTF8ExtractHTMLText(
                        AnsiToUTF8(HtmlContent),
                        LstExtractedResourceText,
                        DecodeHTMLText
                       );

  For i := 0 to LstExtractedResourceText.count - 1 do
    LstExtractedResourceText[I] := UTF8ToAnsi(LstExtractedResourceText[i]);
End;

{**********************************************************************************************}
{we use useNumericReference by default because it's compatible with XHTML, especially because
 of the &apos; entity}
function ALXMLTextElementEncode(Src: string; const useNumericReference: boolean = True): string;
var i, l: integer;
    Buf, P: PChar;
    ch: Integer;
begin
  Result := '';
  L := Length(src);
  if L = 0 then exit;
  GetMem(Buf, L * 6); // to be on the *very* safe side
  try
    P := Buf;
    for i := 1 to L do begin
      ch := Ord(src[i]);
      case ch of
        34: begin // quot "
              if useNumericReference then begin
                ALMove('&#34;', P^, 5);
                Inc(P, 5);
              end
              else begin
                ALMove('&quot;', P^, 6);
                Inc(P, 6);
              end;
            end;
        38: begin // amp  &
              if useNumericReference then begin
                ALMove('&#38;', P^, 5);
                Inc(P, 5);
              end
              else begin
                ALMove('&amp;', P^, 5);
                Inc(P, 5);
              end;
            end;
        39: begin // apos  '
              if useNumericReference then begin
                ALMove('&#39;', P^, 5);
                Inc(P, 5);
              end
              else begin
                ALMove('&apos;', P^, 6);  // !! warning this entity not work in HTML nor in XHTML under IE !!
                Inc(P, 6);
              end;
            end;
        60: begin // lt   <
              if useNumericReference then begin
                ALMove('&#60;', P^, 5);
                Inc(P, 5);
              end
              else begin
                ALMove('&lt;', P^, 4);
                Inc(P, 4);
              end;
            end;
        62: begin // gt   >
              if useNumericReference then begin
                ALMove('&#62;', P^, 5);
                Inc(P, 5);
              end
              else begin
                ALMove('&gt;', P^, 4);
                Inc(P, 4);
              end;
            end;
        else Begin
          P^:= Char(ch);
          Inc(P);
        end;
      end;
    end;
    SetString(Result, Buf, P - Buf);
  finally
    FreeMem(Buf);
  end;
end;

{***************************************************************}
function ALUTF8XMLTextElementDecode(Src: Utf8string): Utf8string;
var CurrentSrcPos, CurrentResultPos : Integer;
    j: integer;
    aTmpInteger: Integer;
    SrcLength: integer;
    aEntity: String;

    {--------------------------------------}
    procedure CopyCurrentSrcPosCharToResult;
    Begin
      result[CurrentResultPos] := src[CurrentSrcPos];
      inc(CurrentResultPos);
      inc(CurrentSrcPos);
    end;

    {---------------------------------------------------------------------------------}
    procedure CopyCharToResult(aUnicodeOrdEntity: Integer; aNewCurrentSrcPos: integer);
    Var aUTF8String: UTF8String;
        K: integer;
    Begin
      aUTF8String := UTF8Encode(WideChar(aUnicodeOrdEntity));
      For k := 1 to length(aUTF8String) do begin
        result[CurrentResultPos] := aUTF8String[k];
        inc(CurrentResultPos);
      end;
      CurrentSrcPos := aNewCurrentSrcPos;
    end;

begin
  {init var}
  CurrentSrcPos := 1;
  CurrentResultPos := 1;
  SrcLength := Length(src);
  SetLength(Result,SrcLength);

  {start loop}
  while (CurrentSrcPos <= SrcLength) do begin

    {HTMLentity detected}
    If src[CurrentSrcPos]='&' then begin

      {extract the HTML entity}
      j := CurrentSrcPos;
      while (J <= SrcLength) and (src[j] <> ';') and (j-CurrentSrcPos<=12) do inc(j);

      {HTML entity is valid}
      If (J<=SrcLength) and (j-CurrentSrcPos<=12) then Begin

        {HTML entity is numeric}
        IF (Src[CurrentSrcPos+1] = '#') then begin

          {HTML entity is hexa}
          IF (Src[CurrentSrcPos+2] = 'x') then begin
            if trystrtoint(
                           '$' + ALCopyStr(
                                           Src,
                                           CurrentSrcPos+3,
                                           j-CurrentSrcPos-3
                                          ),
                           aTmpInteger
                          )
            then CopyCharToResult(aTmpInteger, J+1)
            else CopyCurrentSrcPosCharToResult;
          end

          {HTML entity is numeric}
          else begin

            {numeric HTML entity is valid}
            if trystrtoint(
                           ALCopyStr(
                                     Src,
                                     CurrentSrcPos+2,
                                     j-CurrentSrcPos-2
                                    ),
                           aTmpInteger
                          )
            then CopyCharToResult(aTmpInteger, J+1)
            else CopyCurrentSrcPosCharToResult;

          end;

        end

        {HTML entity is litteral}
        else begin

          //amp
          aEntity := ALCopyStr(
                               Src,
                               CurrentSrcPos+1,
                               j-CurrentSrcPos-1
                              );

          If aEntity ='quot' then CopyCharToResult(34, J+1) // "
          else if aEntity = 'apos' then CopyCharToResult(39, J+1) // '
          else if aEntity = 'amp' then CopyCharToResult(38, J+1) // &
          else if aEntity = 'lt' then CopyCharToResult(60, J+1) // <
          else if aEntity = 'gt' then CopyCharToResult(62, J+1) // >
          else CopyCurrentSrcPosCharToResult;

        end;

      end
      else CopyCurrentSrcPosCharToResult;

    end
    else CopyCurrentSrcPosCharToResult;

  end;

  setLength(Result,CurrentResultPos-1);
end;

{*******************************************************}
function ALANSIXMLTextElementDecode(Src: string): string;
begin
  Result := Utf8toAnsi(
                       ALUTF8XMLTextElementDecode(
                                                  AnsiToUTF8(Src)
                                                 )
                      );
end;

{***********************************************************************************************************************************************}
function ALUTF8HTMLEncode(Src: UTF8String; Const EncodeASCIIHtmlEntities: Boolean = True; const useNumericReference: boolean = True): UTF8String;
var i, k, l: integer;
    Buf, P: PChar;
    aEntityStr: String;
    aEntityInt: Integer;
    aIndex: integer;
    aTmpWideString: WideString;
    LstUnicodeEntitiesNumber: TALIntegerList;
begin
  Result := '';
  If Src='' then Exit;

  LstUnicodeEntitiesNumber := TALIntegerList.create;
  Try
    if not useNumericReference then begin
      LstUnicodeEntitiesNumber.Duplicates := DupIgnore;
      LstUnicodeEntitiesNumber.Sorted := True;
      For i := 0 to vALhtml_LstEntities.Count - 1 do
        LstUnicodeEntitiesNumber.AddObject(integer(vALhtml_LstEntities.Objects[i]),pointer(i));
    end;

    aTmpWideString := UTF8Decode(Src);
    L := length(aTmpWideString);
    If L=0 then Exit;

    GetMem(Buf, length(Src) * 12); // to be on the *very* safe side
    try
      P := Buf;
      For i := 1 to L do begin
        aEntityInt := Integer(aTmpWideString[i]);
        Case aEntityInt of
          34: begin // quot "
                If EncodeASCIIHtmlEntities then begin
                  if useNumericReference then begin
                    ALMove('&#34;', P^, 5);
                    Inc(P, 5);
                  end
                  else begin
                    ALMove('&quot;', P^, 6);
                    Inc(P, 6);
                  end;
                end
                else Begin
                  P^ := '"';
                  Inc(P, 1);
                end;
              end;
          38: begin // amp  &
                If EncodeASCIIHtmlEntities then begin
                  if useNumericReference then begin
                    ALMove('&#38;', P^, 5);
                    Inc(P, 5);
                  end
                  else begin
                    ALMove('&amp;', P^, 5);
                    Inc(P, 5);
                  end;
                end
                else Begin
                  P^ := '&';
                  Inc(P, 1);
                end;
              end;
          39: begin //  '
                If EncodeASCIIHtmlEntities then begin
                  ALMove('&#39;', P^, 5);
                  Inc(P, 5);
                end
                else Begin
                  P^ := '''';
                  Inc(P, 1);
                end;
              end;
          60: begin // lt   <
                If EncodeASCIIHtmlEntities then begin
                  if useNumericReference then begin
                    ALMove('&#60;', P^, 5);
                    Inc(P, 5);
                  end
                  else begin
                    ALMove('&lt;', P^, 4);
                    Inc(P, 4);
                  end;
                end
                else Begin
                  P^ := '<';
                  Inc(P, 1);
                end;
              end;
          62: begin // gt   >
                If EncodeASCIIHtmlEntities then begin
                  if useNumericReference then begin
                    ALMove('&#62;', P^, 5);
                    Inc(P, 5);
                  end
                  else begin
                    ALMove('&gt;', P^, 4);
                    Inc(P, 4);
                  end;
                end
                else Begin
                  P^ := '>';
                  Inc(P, 1);
                end;
              end;
          else begin
            if (aEntityInt > 127) then begin
              if useNumericReference then aEntityStr := '&#'+inttostr(aEntityInt)+';'
              else begin
                aIndex := LstUnicodeEntitiesNumber.IndexOf(aEntityInt);
                If aIndex >= 0 Then begin
                  aEntityStr := vALhtml_LstEntities[integer(LstUnicodeEntitiesNumber.Objects[aIndex])];
                  If aEntityStr <> '' then aEntityStr := '&' + aEntityStr + ';'
                  else aEntityStr := '&#'+inttostr(aEntityInt)+';'
                end
                else aEntityStr := '&#'+inttostr(aEntityInt)+';'
              end;
            end
            else aEntityStr := aTmpWideString[i];

            for k := 1 to Length(aEntityStr) do begin
              P^ := aEntityStr[k];
              Inc(P)
            end;
          end;
        end;
      end;

      SetString(Result, Buf, P - Buf);

    finally
      FreeMem(Buf);
    end;

  finally
    LstUnicodeEntitiesNumber.free;
  end;

end;

{***************************************************************************************************************************************}
function ALANSIHTMLEncode(Src: string; Const EncodeASCIIHtmlEntities: Boolean = True; const useNumericReference: boolean = True): string;
Begin
  Result := Utf8toAnsi(
                       ALUTF8HTMLEncode(
                                        AnsiToUTF8(Src),
                                        EncodeASCIIHtmlEntities,
                                        useNumericReference
                                       )
                      );
end;

{*****************************************************}
function ALUTF8HTMLDecode(Src: Utf8string): Utf8string;
var CurrentSrcPos, CurrentResultPos : Integer;
    j: integer;
    aTmpInteger: Integer;
    SrcLength: integer;

    {--------------------------------------}
    procedure CopyCurrentSrcPosCharToResult;
    Begin
      result[CurrentResultPos] := src[CurrentSrcPos];
      inc(CurrentResultPos);
      inc(CurrentSrcPos);
    end;

    {---------------------------------------------------------------------------------}
    procedure CopyCharToResult(aUnicodeOrdEntity: Integer; aNewCurrentSrcPos: integer);
    Var aUTF8String: UTF8String;
        K: integer;
    Begin
      aUTF8String := UTF8Encode(WideChar(aUnicodeOrdEntity));
      For k := 1 to length(aUTF8String) do begin
        result[CurrentResultPos] := aUTF8String[k];
        inc(CurrentResultPos);
      end;
      CurrentSrcPos := aNewCurrentSrcPos;
    end;

begin
  {init var}
  CurrentSrcPos := 1;
  CurrentResultPos := 1;
  SrcLength := Length(src);
  SetLength(Result,SrcLength);

  {start loop}
  while (CurrentSrcPos <= SrcLength) do begin

    {HTMLentity detected}
    If src[CurrentSrcPos]='&' then begin

      {extract the HTML entity}
      j := CurrentSrcPos;
      while (J <= SrcLength) and (src[j] <> ';') and (j-CurrentSrcPos<=12) do inc(j);

      {HTML entity is valid}
      If (J<=SrcLength) and (j-CurrentSrcPos<=12) then Begin

        {HTML entity is numeric}
        IF (Src[CurrentSrcPos+1] = '#') then begin

          {HTML entity is hexa}
          IF (Src[CurrentSrcPos+2] = 'x') then begin
            if trystrtoint(
                           '$' + ALCopyStr(
                                           Src,
                                           CurrentSrcPos+3,
                                           j-CurrentSrcPos-3
                                          ),
                           aTmpInteger
                          )
            then CopyCharToResult(aTmpInteger, J+1)
            else CopyCurrentSrcPosCharToResult;
          end

          {HTML entity is numeric}
          else begin

            {numeric HTML entity is valid}
            if trystrtoint(
                           ALCopyStr(
                                     Src,
                                     CurrentSrcPos+2,
                                     j-CurrentSrcPos-2
                                    ),
                           aTmpInteger
                          )
            then CopyCharToResult(aTmpInteger, J+1)
            else CopyCurrentSrcPosCharToResult;

          end;

        end

        {HTML entity is litteral}
        else begin

          aTmpInteger := vALhtml_LstEntities.IndexOf(
                                                     ALCopyStr(
                                                               Src,
                                                               CurrentSrcPos+1,
                                                               j-CurrentSrcPos-1
                                                              )
                                                    );
          If aTmpInteger >= 0 then CopyCharToResult(integer(vALhtml_LstEntities.Objects[aTmpInteger]),J+1)
          else CopyCurrentSrcPosCharToResult;

        end;

      end
      else CopyCurrentSrcPosCharToResult;

    end
    else CopyCurrentSrcPosCharToResult;

  end;

  setLength(Result,CurrentResultPos-1);
end;

{*********************************************}
function ALANSIHTMLDecode(Src: string): string;
Begin
  Result := Utf8toAnsi(
                       ALUTF8HTMLDecode(
                                        AnsiToUTF8(Src)
                                       )
                      );
end;

{***************************************************************************}
procedure ALHideHtmlUnwantedTagForHTMLHandleTagfunct(Var HtmlContent: String;
                                                     Const DeleteBodyOfUnwantedTag: Boolean = False;
                                                     const ReplaceUnwantedTagCharBy: Char = #1); {this char is not use in html}
Var InDoubleQuote : Boolean;
    InSimpleQuote : Boolean;
    P1, P2 : integer;
    X1 : Integer;
    Str1 : String;
Begin
  P1 := 1;
  While P1 <= length(htmlContent) do
    If HtmlContent[P1] = '<' then begin

      X1 := P1;
      Str1 := '';
      while (X1 <= length(Htmlcontent)) and (not (htmlContent[X1] in ['>',' ',#13,#10,#9])) do begin
        Str1 := Str1 + HtmlContent[X1];
        inc(X1);
      end;

      InSimpleQuote := false;
      InDoubleQuote := false;

      {hide script content tag}
      if ALlowercase(str1) = '<script' then begin
        inc(P1, 7);
        While (P1 <= length(htmlContent)) do begin
          If (htmlContent[P1] = '''') and (not inDoubleQuote) then InSimpleQuote := Not InSimpleQuote
          else If (htmlContent[P1] = '"') and (not inSimpleQuote) then InDoubleQuote := Not InDoubleQuote
          else if (HtmlContent[P1] = '>') and (not InSimpleQuote) and (not InDoubleQuote) then break;
          inc(P1);
        end;
        IF P1 <= length(htmlContent) then inc(P1);

        P2 := P1;
        While (P1 <= length(htmlContent)) do begin
          if (HtmlContent[P1] = '<') then begin
            if (length(htmlContent) >= P1+8) and
               (HtmlContent[P1+1]='/') and
               (ALlowercase(HtmlContent[P1+2])='s') and
               (ALlowercase(HtmlContent[P1+3])='c') and
               (ALlowercase(HtmlContent[P1+4])='r') and
               (ALlowercase(HtmlContent[P1+5])='i') and
               (ALlowercase(HtmlContent[P1+6])='p') and
               (ALlowercase(HtmlContent[P1+7])='t') and
               (HtmlContent[P1+8]='>') then break
            else HtmlContent[P1] := ReplaceUnwantedTagCharBy;
          end;
          inc(P1);
        end;
        IF P1 <= length(htmlContent) then dec(P1);

        If DeleteBodyOfUnwantedTag then begin
          delete(htmlContent,P2,P1-P2 + 1);
          P1 := P2;
        end;
      end

      {hide script content tag}
      else if ALlowercase(str1) = '<style' then begin
        inc(P1, 6);
        While (P1 <= length(htmlContent)) do begin
          If (htmlContent[P1] = '''') and (not inDoubleQuote) then InSimpleQuote := Not InSimpleQuote
          else If (htmlContent[P1] = '"') and (not inSimpleQuote) then InDoubleQuote := Not InDoubleQuote
          else if (HtmlContent[P1] = '>') and (not InSimpleQuote) and (not InDoubleQuote) then break;
          inc(P1);
        end;
        IF P1 <= length(htmlContent) then inc(P1);

        P2 := P1;
        While (P1 <= length(htmlContent)) do begin
          if (HtmlContent[P1] = '<') then begin
            if (length(htmlContent) >= P1+7) and
               (HtmlContent[P1+1]='/') and
               (ALlowercase(HtmlContent[P1+2])='s') and
               (ALlowercase(HtmlContent[P1+3])='t') and
               (ALlowercase(HtmlContent[P1+4])='y') and
               (ALlowercase(HtmlContent[P1+5])='l') and
               (ALlowercase(HtmlContent[P1+6])='e') and
               (HtmlContent[P1+7]='>') then break
            else HtmlContent[P1] := ReplaceUnwantedTagCharBy;
          end;
          inc(P1);
        end;
        IF P1 <= length(htmlContent) then dec(P1);

        If DeleteBodyOfUnwantedTag then begin
          delete(htmlContent,P2,P1-P2 + 1);
          P1 := P2;
        end;
      end

      {hide comment content tag}
      else if str1 = '<!--' then begin
        P2 := P1;
        HtmlContent[P1] := ReplaceUnwantedTagCharBy;
        inc(P1,4);
        While (P1 <= length(htmlContent)) do begin
          if (HtmlContent[P1] = '>') and
             (P1>2) and
             (HtmlContent[P1-1]='-') and
             (HtmlContent[P1-2]='-') then break
          else if (HtmlContent[P1] = '<') then HtmlContent[P1] := ReplaceUnwantedTagCharBy;
          inc(P1);
        end;
        IF P1 <= length(htmlContent) then inc(P1);

        If DeleteBodyOfUnwantedTag then begin
          delete(htmlContent,P2,P1-P2);
          P1 := P2;
        end;
      end

      {hide text < tag}
      else if str1 = '<' then begin
        HtmlContent[P1] := ReplaceUnwantedTagCharBy;
        inc(P1);
      end

      else begin
        inc(P1, length(str1));
        While (P1 <= length(htmlContent)) do begin
          If (htmlContent[P1] = '''') and (not inDoubleQuote) then InSimpleQuote := Not InSimpleQuote
          else If (htmlContent[P1] = '"') and (not inSimpleQuote) then InDoubleQuote := Not InDoubleQuote
          else if (HtmlContent[P1] = '>') and (not InSimpleQuote) and (not InDoubleQuote) then break;
          inc(P1);
        end;
        IF P1 <= length(htmlContent) then inc(P1);
      end;

    end
    else inc(p1);
end;

Initialization
  vALhtml_LstEntities := TstringList.create;
  With (vALhtml_LstEntities as TstringList) do begin
    CaseSensitive := True;
    Duplicates := DupAccept;
    Sorted := True;
  end;
  ALInitHtmlEntitiesLst(vALhtml_LstEntities);

Finalization
  vALhtml_LstEntities.Free;

end.
