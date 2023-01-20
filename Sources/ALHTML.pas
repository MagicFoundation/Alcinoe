unit ALHTML;

interface

{$I Alcinoe.inc}

uses
  AlStringList;

{$IFNDEF ALHideAnsiString}

procedure ALExtractHTMLText(HtmlContent: AnsiString;
                            LstExtractedResourceText: TALStrings;
                            Const DecodeHTMLText: Boolean = True); overload;
function  ALExtractHTMLText(const HtmlContent: AnsiString;
                            Const DecodeHTMLText: Boolean = True): AnsiString; overload;
function  ALXMLCDataElementEncode(const Src: AnsiString): AnsiString;
function  ALXMLTextElementEncode(const Src: AnsiString; const useNumericReference: boolean = True): AnsiString;
procedure ALXMLTextElementDecodeV(var Str: AnsiString);
function  ALXMLTextElementDecode(const Src: AnsiString): AnsiString;
function  ALHTMLEncode(const Src: AnsiString;
                       const EncodeASCIIHtmlEntities: Boolean = True;
                       const useNumericReference: boolean = True): AnsiString;
function  ALHTMLDecode(const Src: AnsiString): AnsiString;
function  ALJavascriptEncode(const Src: AnsiString; const useNumericReference: boolean = true): AnsiString;
procedure ALJavascriptDecodeV(Var Str: AnsiString);
function  ALJavascriptDecode(const Src: AnsiString): AnsiString;
{$IFDEF MSWINDOWS}
function  ALRunJavascript(const aCode: AnsiString): AnsiString;
{$ENDIF}
procedure ALHideHtmlUnwantedTagForHTMLHandleTagfunct(Var HtmlContent: AnsiString;
                                                     Const DeleteBodyOfUnwantedTag: Boolean = False;
                                                     const ReplaceUnwantedTagCharBy: AnsiChar = #1);
procedure ALCompactHtmlTagParams(TagParams: TALStrings);

{$ENDIF}

function  ALJavascriptEncodeU(const Src: String; const useNumericReference: boolean = true): String;
procedure ALJavascriptDecodeVU(Var Str: String);
function  ALJavascriptDecodeU(const Src: String): String;



////////////////////////////
/// deprecated functions ///
////////////////////////////

{$IFNDEF ALHideAnsiString}
procedure ALUTF8ExtractHTMLText(HtmlContent: AnsiString;
                                LstExtractedResourceText: TALStrings;
                                Const DecodeHTMLText: Boolean = True); overload; deprecated 'use ALExtractHTMLText instead with SetMultiByteConversionCodePage(CP_UTF8)';
function  ALUTF8ExtractHTMLText(const HtmlContent: AnsiString;
                                Const DecodeHTMLText: Boolean = True): AnsiString; overload; deprecated 'use ALExtractHTMLText instead with SetMultiByteConversionCodePage(CP_UTF8)';
procedure ALUTF8XMLTextElementDecodeV(var Str: AnsiString); deprecated 'use ALXMLTextElementDecodeV instead with SetMultiByteConversionCodePage(CP_UTF8)';
function  ALUTF8XMLTextElementDecode(const Src: AnsiString): AnsiString; deprecated 'use ALXMLTextElementDecode instead with SetMultiByteConversionCodePage(CP_UTF8)';
function  ALUTF8HTMLEncode(const Src: AnsiString;
                           const EncodeASCIIHtmlEntities: Boolean = True;
                           const useNumericReference: boolean = True): AnsiString; deprecated 'use ALHTMLEncode instead with SetMultiByteConversionCodePage(CP_UTF8)';
function  ALUTF8HTMLDecode(const Src: AnsiString): AnsiString; deprecated 'use ALHTMLDecode instead with SetMultiByteConversionCodePage(CP_UTF8)';
procedure ALUTF8JavascriptDecodeV(Var Str: AnsiString); deprecated 'use ALJavascriptDecodeV instead with SetMultiByteConversionCodePage(CP_UTF8)';
function  ALUTF8JavascriptDecode(const Src: AnsiString): AnsiString; deprecated 'use ALJavascriptDecode instead with SetMultiByteConversionCodePage(CP_UTF8)';
{$ENDIF !ALHideAnsiString}


implementation

uses
  System.Math,
  System.Classes,
  System.sysutils,
  {$IFDEF MSWINDOWS}
  System.Win.Comobj,
  Winapi.Ole2,
  Winapi.ActiveX,
  {$ENDIF}
  ALCommon,
  ALString,
  ALQuickSortList;

{$IFNDEF ALHideAnsiString}

Var
  _ALHtmlEntities: TALStrings;

{************************************************************}
procedure ALInitHtmlEntities(const aHtmlEntities: TALStrings);
Begin

  aHtmlEntities.Clear;
  aHtmlEntities.AddObject('zwnj',pointer(8204)); // zero width non-joiner,   U+200C NEW RFC 2070 -->
  aHtmlEntities.AddObject('zwj',pointer(8205)); // zero width joiner, U+200D NEW RFC 2070 -->
  aHtmlEntities.AddObject('zeta',pointer(950)); // greek small letter zeta, U+03B6 ISOgrk3 -->
  aHtmlEntities.AddObject('Zeta',pointer(918)); // greek capital letter zeta, U+0396 -->
  aHtmlEntities.AddObject('yuml',pointer(255)); // latin small letter y with diaeresis, U+00FF ISOlat1 -->
  aHtmlEntities.AddObject('Yuml',pointer(376)); // latin capital letter Y with diaeresis,   U+0178 ISOlat2 -->
  aHtmlEntities.AddObject('yen',pointer(165)); // yen sign = yuan sign, U+00A5 ISOnum -->
  aHtmlEntities.AddObject('yacute',pointer(253)); // latin small letter y with acute, U+00FD ISOlat1 -->
  aHtmlEntities.AddObject('Yacute',pointer(221)); // latin capital letter Y with acute, U+00DD ISOlat1 -->
  aHtmlEntities.AddObject('xi',pointer(958)); // greek small letter xi, U+03BE ISOgrk3 -->
  aHtmlEntities.AddObject('Xi',pointer(926)); // greek capital letter xi, U+039E ISOgrk3 -->
  aHtmlEntities.AddObject('weierp',pointer(8472)); // script capital P = power set    = Weierstrass p, U+2118 ISOamso -->
  aHtmlEntities.AddObject('uuml',pointer(252)); // latin small letter u with diaeresis, U+00FC ISOlat1 -->
  aHtmlEntities.AddObject('Uuml',pointer(220)); // latin capital letter U with diaeresis, U+00DC ISOlat1 -->
  aHtmlEntities.AddObject('upsilon',pointer(965)); // greek small letter upsilon,   U+03C5 ISOgrk3 -->
  aHtmlEntities.AddObject('Upsilon',pointer(933)); // greek capital letter upsilon,   U+03A5 ISOgrk3 -->
  aHtmlEntities.AddObject('upsih',pointer(978)); // greek upsilon with hook symbol,   U+03D2 NEW -->
  aHtmlEntities.AddObject('uml',pointer(168)); // diaeresis = spacing diaeresis, U+00A8 ISOdia -->
  aHtmlEntities.AddObject('ugrave',pointer(249)); // latin small letter u with grave, U+00F9 ISOlat1 -->
  aHtmlEntities.AddObject('Ugrave',pointer(217)); // latin capital letter U with grave, U+00D9 ISOlat1 -->
  aHtmlEntities.AddObject('ucirc',pointer(251)); // latin small letter u with circumflex, U+00FB ISOlat1 -->
  aHtmlEntities.AddObject('Ucirc',pointer(219)); // latin capital letter U with circumflex, U+00DB ISOlat1 -->
  aHtmlEntities.AddObject('uArr',pointer(8657)); // upwards double arrow, U+21D1 ISOamsa -->
  aHtmlEntities.AddObject('uarr',pointer(8593)); // upwards arrow, U+2191 ISOnum-->
  aHtmlEntities.AddObject('uacute',pointer(250)); // latin small letter u with acute, U+00FA ISOlat1 -->
  aHtmlEntities.AddObject('Uacute',pointer(218)); // latin capital letter U with acute, U+00DA ISOlat1 -->
  aHtmlEntities.AddObject('trade',pointer(8482)); // trade mark sign, U+2122 ISOnum -->
  aHtmlEntities.AddObject('times',pointer(215)); // multiplication sign, U+00D7 ISOnum -->
  aHtmlEntities.AddObject('tilde',pointer(732)); // small tilde, U+02DC ISOdia -->
  aHtmlEntities.AddObject('thorn',pointer(254)); // latin small letter thorn, U+00FE ISOlat1 -->
  aHtmlEntities.AddObject('THORN',pointer(222)); // latin capital letter THORN, U+00DE ISOlat1 -->
  aHtmlEntities.AddObject('thinsp',pointer(8201)); // thin space, U+2009 ISOpub -->
  aHtmlEntities.AddObject('thetasym',pointer(977)); // greek small letter theta symbol,   U+03D1 NEW -->
  aHtmlEntities.AddObject('theta',pointer(952)); // greek small letter theta,   U+03B8 ISOgrk3 -->
  aHtmlEntities.AddObject('Theta',pointer(920)); // greek capital letter theta,   U+0398 ISOgrk3 -->
  aHtmlEntities.AddObject('there4',pointer(8756)); // therefore, U+2234 ISOtech -->
  aHtmlEntities.AddObject('tau',pointer(964)); // greek small letter tau, U+03C4 ISOgrk3 -->
  aHtmlEntities.AddObject('Tau',pointer(932)); // greek capital letter tau, U+03A4 -->
  aHtmlEntities.AddObject('szlig',pointer(223)); // latin small letter sharp s = ess-zed, U+00DF ISOlat1 -->
  aHtmlEntities.AddObject('supe',pointer(8839)); // superset of or equal to,    U+2287 ISOtech -->
  aHtmlEntities.AddObject('sup3',pointer(179)); // superscript three = superscript digit three = cubed, U+00B3 ISOnum -->
  aHtmlEntities.AddObject('sup2',pointer(178)); // superscript two = superscript digit two = squared, U+00B2 ISOnum -->
  aHtmlEntities.AddObject('sup1',pointer(185)); // superscript one = superscript digit one, U+00B9 ISOnum -->
  aHtmlEntities.AddObject('sup',pointer(8835)); // superset of, U+2283 ISOtech -->
  aHtmlEntities.AddObject('sum',pointer(8721)); // n-ary sumation, U+2211 ISOamsb -->
  aHtmlEntities.AddObject('sube',pointer(8838)); // subset of or equal to, U+2286 ISOtech -->
  aHtmlEntities.AddObject('sub',pointer(8834)); // subset of, U+2282 ISOtech -->
  aHtmlEntities.AddObject('spades',pointer(9824)); // black spade suit, U+2660 ISOpub -->
  aHtmlEntities.AddObject('sim',pointer(8764)); // tilde operator = varies with = similar to,    U+223C ISOtech -->
  aHtmlEntities.AddObject('sigmaf',pointer(962)); // greek small letter final sigma,   U+03C2 ISOgrk3 -->
  aHtmlEntities.AddObject('sigma',pointer(963)); // greek small letter sigma,   U+03C3 ISOgrk3 -->
  aHtmlEntities.AddObject('Sigma',pointer(931)); // greek capital letter sigma,   U+03A3 ISOgrk3 -->
  aHtmlEntities.AddObject('shy',pointer(173)); // soft hyphen = discretionary hyphen, U+00AD ISOnum -->
  aHtmlEntities.AddObject('sect',pointer(167)); // section sign, U+00A7 ISOnum -->
  aHtmlEntities.AddObject('sdot',pointer(8901)); // dot operator, U+22C5 ISOamsb -->
  aHtmlEntities.AddObject('scaron',pointer(353)); // latin small letter s with caron,   U+0161 ISOlat2 -->
  aHtmlEntities.AddObject('Scaron',pointer(352)); // latin capital letter S with caron,   U+0160 ISOlat2 -->
  aHtmlEntities.AddObject('sbquo',pointer(8218)); // single low-9 quotation mark, U+201A NEW -->
  aHtmlEntities.AddObject('rsquo',pointer(8217)); // right single quotation mark,   U+2019 ISOnum -->
  aHtmlEntities.AddObject('rsaquo',pointer(8250)); // single right-pointing angle quotation mark,   U+203A ISO proposed -->
  aHtmlEntities.AddObject('rlm',pointer(8207)); // right-to-left mark, U+200F NEW RFC 2070 -->
  aHtmlEntities.AddObject('rho',pointer(961)); // greek small letter rho, U+03C1 ISOgrk3 -->
  aHtmlEntities.AddObject('Rho',pointer(929)); // greek capital letter rho, U+03A1 -->
  aHtmlEntities.AddObject('rfloor',pointer(8971)); // right floor, U+230B ISOamsc  -->
  aHtmlEntities.AddObject('reg',pointer(174)); // registered sign = registered trade mark sign, U+00AE ISOnum -->
  aHtmlEntities.AddObject('real',pointer(8476)); // blackletter capital R = real part symbol,    U+211C ISOamso -->
  aHtmlEntities.AddObject('rdquo',pointer(8221)); // right double quotation mark,   U+201D ISOnum -->
  aHtmlEntities.AddObject('rceil',pointer(8969)); // right ceiling, U+2309 ISOamsc  -->
  aHtmlEntities.AddObject('rArr',pointer(8658)); // rightwards double arrow,    U+21D2 ISOtech -->
  aHtmlEntities.AddObject('rarr',pointer(8594)); // rightwards arrow, U+2192 ISOnum -->
  aHtmlEntities.AddObject('raquo',pointer(187)); // right-pointing double angle quotation mark = right pointing guillemet, U+00BB ISOnum -->
  aHtmlEntities.AddObject('rang',pointer(9002)); // right-pointing angle bracket = ket,    U+232A ISOtech -->
  aHtmlEntities.AddObject('radic',pointer(8730)); // square root = radical sign,    U+221A ISOtech -->
  aHtmlEntities.AddObject('quot',pointer(34)); // quotation mark = APL quote,   U+0022 ISOnum -->
  aHtmlEntities.AddObject('psi',pointer(968)); // greek small letter psi, U+03C8 ISOgrk3 -->
  aHtmlEntities.AddObject('Psi',pointer(936)); // greek capital letter psi,   U+03A8 ISOgrk3 -->
  aHtmlEntities.AddObject('prop',pointer(8733)); // proportional to, U+221D ISOtech -->
  aHtmlEntities.AddObject('prod',pointer(8719)); // n-ary product = product sign,    U+220F ISOamsb -->
  aHtmlEntities.AddObject('Prime',pointer(8243)); // double prime = seconds = inches,    U+2033 ISOtech -->
  aHtmlEntities.AddObject('prime',pointer(8242)); // prime = minutes = feet, U+2032 ISOtech -->
  aHtmlEntities.AddObject('pound',pointer(163)); // pound sign, U+00A3 ISOnum -->
  aHtmlEntities.AddObject('plusmn',pointer(177)); // plus-minus sign = plus-or-minus sign, U+00B1 ISOnum -->
  aHtmlEntities.AddObject('piv',pointer(982)); // greek pi symbol, U+03D6 ISOgrk3 -->
  aHtmlEntities.AddObject('pi',pointer(960)); // greek small letter pi, U+03C0 ISOgrk3 -->
  aHtmlEntities.AddObject('Pi',pointer(928)); // greek capital letter pi, U+03A0 ISOgrk3 -->
  aHtmlEntities.AddObject('phi',pointer(966)); // greek small letter phi, U+03C6 ISOgrk3 -->
  aHtmlEntities.AddObject('Phi',pointer(934)); // greek capital letter phi,   U+03A6 ISOgrk3 -->
  aHtmlEntities.AddObject('perp',pointer(8869)); // up tack = orthogonal to = perpendicular,    U+22A5 ISOtech -->
  aHtmlEntities.AddObject('permil',pointer(8240)); // per mille sign, U+2030 ISOtech -->
  aHtmlEntities.AddObject('part',pointer(8706)); // partial differential, U+2202 ISOtech  -->
  aHtmlEntities.AddObject('para',pointer(182)); // pilcrow sign = paragraph sign, U+00B6 ISOnum -->
  aHtmlEntities.AddObject('ouml',pointer(246)); // latin small letter o with diaeresis, U+00F6 ISOlat1 -->
  aHtmlEntities.AddObject('Ouml',pointer(214)); // latin capital letter O with diaeresis, U+00D6 ISOlat1 -->
  aHtmlEntities.AddObject('otimes',pointer(8855)); // circled times = vector product,    U+2297 ISOamsb -->
  aHtmlEntities.AddObject('otilde',pointer(245)); // latin small letter o with tilde, U+00F5 ISOlat1 -->
  aHtmlEntities.AddObject('Otilde',pointer(213)); // latin capital letter O with tilde, U+00D5 ISOlat1 -->
  aHtmlEntities.AddObject('oslash',pointer(248)); // latin small letter o with stroke, = latin small letter o slash, U+00F8 ISOlat1 -->
  aHtmlEntities.AddObject('Oslash',pointer(216)); // latin capital letter O with stroke = latin capital letter O slash, U+00D8 ISOlat1 -->
  aHtmlEntities.AddObject('ordm',pointer(186)); // masculine ordinal indicator, U+00BA ISOnum -->
  aHtmlEntities.AddObject('ordf',pointer(170)); // feminine ordinal indicator, U+00AA ISOnum -->
  aHtmlEntities.AddObject('or',pointer(8744)); // logical or = vee, U+2228 ISOtech -->
  aHtmlEntities.AddObject('oplus',pointer(8853)); // circled plus = direct sum,    U+2295 ISOamsb -->
  aHtmlEntities.AddObject('omicron',pointer(959)); // greek small letter omicron, U+03BF NEW -->
  aHtmlEntities.AddObject('Omicron',pointer(927)); // greek capital letter omicron, U+039F -->
  aHtmlEntities.AddObject('omega',pointer(969)); // greek small letter omega,   U+03C9 ISOgrk3 -->
  aHtmlEntities.AddObject('Omega',pointer(937)); // greek capital letter omega,   U+03A9 ISOgrk3 -->
  aHtmlEntities.AddObject('oline',pointer(8254)); // overline = spacing overscore,    U+203E NEW -->
  aHtmlEntities.AddObject('ograve',pointer(242)); // latin small letter o with grave, U+00F2 ISOlat1 -->
  aHtmlEntities.AddObject('Ograve',pointer(210)); // latin capital letter O with grave, U+00D2 ISOlat1 -->
  aHtmlEntities.AddObject('oelig',pointer(339)); // latin small ligature oe, U+0153 ISOlat2 -->
  aHtmlEntities.AddObject('OElig',pointer(338)); // latin capital ligature OE,   U+0152 ISOlat2 -->
  aHtmlEntities.AddObject('ocirc',pointer(244)); // latin small letter o with circumflex, U+00F4 ISOlat1 -->
  aHtmlEntities.AddObject('Ocirc',pointer(212)); // latin capital letter O with circumflex, U+00D4 ISOlat1 -->
  aHtmlEntities.AddObject('oacute',pointer(243)); // latin small letter o with acute, U+00F3 ISOlat1 -->
  aHtmlEntities.AddObject('Oacute',pointer(211)); // latin capital letter O with acute, U+00D3 ISOlat1 -->
  aHtmlEntities.AddObject('nu',pointer(957)); // greek small letter nu, U+03BD ISOgrk3 -->
  aHtmlEntities.AddObject('Nu',pointer(925)); // greek capital letter nu, U+039D -->
  aHtmlEntities.AddObject('ntilde',pointer(241)); // latin small letter n with tilde, U+00F1 ISOlat1 -->
  aHtmlEntities.AddObject('Ntilde',pointer(209)); // latin capital letter N with tilde, U+00D1 ISOlat1 -->
  aHtmlEntities.AddObject('nsub',pointer(8836)); // not a subset of, U+2284 ISOamsn -->
  aHtmlEntities.AddObject('notin',pointer(8713)); // not an element of, U+2209 ISOtech -->
  aHtmlEntities.AddObject('not',pointer(172)); // not sign, U+00AC ISOnum -->
  aHtmlEntities.AddObject('ni',pointer(8715)); // contains as member, U+220B ISOtech -->
  aHtmlEntities.AddObject('ne',pointer(8800)); // not equal to, U+2260 ISOtech -->
  aHtmlEntities.AddObject('ndash',pointer(8211)); // en dash, U+2013 ISOpub -->
  aHtmlEntities.AddObject('nbsp',pointer(160)); // no-break space = non-breaking space, U+00A0 ISOnum -->
  aHtmlEntities.AddObject('nabla',pointer(8711)); // nabla = backward difference,    U+2207 ISOtech -->
  aHtmlEntities.AddObject('mu',pointer(956)); // greek small letter mu, U+03BC ISOgrk3 -->
  aHtmlEntities.AddObject('Mu',pointer(924)); // greek capital letter mu, U+039C -->
  aHtmlEntities.AddObject('minus',pointer(8722)); // minus sign, U+2212 ISOtech -->
  aHtmlEntities.AddObject('middot',pointer(183)); // middle dot = Georgian comma = Greek middle dot, U+00B7 ISOnum -->
  aHtmlEntities.AddObject('micro',pointer(181)); // micro sign, U+00B5 ISOnum -->
  aHtmlEntities.AddObject('mdash',pointer(8212)); // em dash, U+2014 ISOpub -->
  aHtmlEntities.AddObject('macr',pointer(175)); // macron = spacing macron = overline = APL overbar, U+00AF ISOdia -->
  aHtmlEntities.AddObject('lt',pointer(60)); // less-than sign, U+003C ISOnum -->
  aHtmlEntities.AddObject('lsquo',pointer(8216)); // left single quotation mark,   U+2018 ISOnum -->
  aHtmlEntities.AddObject('lsaquo',pointer(8249)); // single left-pointing angle quotation mark,   U+2039 ISO proposed -->
  aHtmlEntities.AddObject('lrm',pointer(8206)); // left-to-right mark, U+200E NEW RFC 2070 -->
  aHtmlEntities.AddObject('loz',pointer(9674)); // lozenge, U+25CA ISOpub -->
  aHtmlEntities.AddObject('lowast',pointer(8727)); // asterisk operator, U+2217 ISOtech -->
  aHtmlEntities.AddObject('lfloor',pointer(8970)); // left floor = apl downstile,    U+230A ISOamsc  -->
  aHtmlEntities.AddObject('le',pointer(8804)); // less-than or equal to, U+2264 ISOtech -->
  aHtmlEntities.AddObject('ldquo',pointer(8220)); // left double quotation mark,   U+201C ISOnum -->
  aHtmlEntities.AddObject('lceil',pointer(8968)); // left ceiling = apl upstile,    U+2308 ISOamsc  -->
  aHtmlEntities.AddObject('lArr',pointer(8656)); // leftwards double arrow, U+21D0 ISOtech -->
  aHtmlEntities.AddObject('larr',pointer(8592)); // leftwards arrow, U+2190 ISOnum -->
  aHtmlEntities.AddObject('laquo',pointer(171)); // left-pointing double angle quotation mark = left pointing guillemet, U+00AB ISOnum -->
  aHtmlEntities.AddObject('lang',pointer(9001)); // left-pointing angle bracket = bra,    U+2329 ISOtech -->
  aHtmlEntities.AddObject('lambda',pointer(955)); // greek small letter lambda,   U+03BB ISOgrk3 -->
  aHtmlEntities.AddObject('Lambda',pointer(923)); // greek capital letter lambda,   U+039B ISOgrk3 -->
  aHtmlEntities.AddObject('kappa',pointer(954)); // greek small letter kappa,   U+03BA ISOgrk3 -->
  aHtmlEntities.AddObject('Kappa',pointer(922)); // greek capital letter kappa, U+039A -->
  aHtmlEntities.AddObject('iuml',pointer(239)); // latin small letter i with diaeresis, U+00EF ISOlat1 -->
  aHtmlEntities.AddObject('Iuml',pointer(207)); // latin capital letter I with diaeresis, U+00CF ISOlat1 -->
  aHtmlEntities.AddObject('isin',pointer(8712)); // element of, U+2208 ISOtech -->
  aHtmlEntities.AddObject('iquest',pointer(191)); // inverted question mark = turned question mark, U+00BF ISOnum -->
  aHtmlEntities.AddObject('iota',pointer(953)); // greek small letter iota, U+03B9 ISOgrk3 -->
  aHtmlEntities.AddObject('Iota',pointer(921)); // greek capital letter iota, U+0399 -->
  aHtmlEntities.AddObject('int',pointer(8747)); // integral, U+222B ISOtech -->
  aHtmlEntities.AddObject('infin',pointer(8734)); // infinity, U+221E ISOtech -->
  aHtmlEntities.AddObject('image',pointer(8465)); // blackletter capital I = imaginary part,    U+2111 ISOamso -->
  aHtmlEntities.AddObject('igrave',pointer(236)); // latin small letter i with grave, U+00EC ISOlat1 -->
  aHtmlEntities.AddObject('Igrave',pointer(204)); // latin capital letter I with grave, U+00CC ISOlat1 -->
  aHtmlEntities.AddObject('iexcl',pointer(161)); // inverted exclamation mark, U+00A1 ISOnum -->
  aHtmlEntities.AddObject('icirc',pointer(238)); // latin small letter i with circumflex, U+00EE ISOlat1 -->
  aHtmlEntities.AddObject('Icirc',pointer(206)); // latin capital letter I with circumflex, U+00CE ISOlat1 -->
  aHtmlEntities.AddObject('iacute',pointer(237)); // latin small letter i with acute, U+00ED ISOlat1 -->
  aHtmlEntities.AddObject('Iacute',pointer(205)); // latin capital letter I with acute, U+00CD ISOlat1 -->
  aHtmlEntities.AddObject('hellip',pointer(8230)); // horizontal ellipsis = three dot leader,    U+2026 ISOpub  -->
  aHtmlEntities.AddObject('hearts',pointer(9829)); // black heart suit = valentine,    U+2665 ISOpub -->
  aHtmlEntities.AddObject('hArr',pointer(8660)); // left right double arrow,    U+21D4 ISOamsa -->
  aHtmlEntities.AddObject('harr',pointer(8596)); // left right arrow, U+2194 ISOamsa -->
  aHtmlEntities.AddObject('gt',pointer(62)); // greater-than sign, U+003E ISOnum -->
  aHtmlEntities.AddObject('ge',pointer(8805)); // greater-than or equal to,    U+2265 ISOtech -->
  aHtmlEntities.AddObject('gamma',pointer(947)); // greek small letter gamma,   U+03B3 ISOgrk3 -->
  aHtmlEntities.AddObject('Gamma',pointer(915)); // greek capital letter gamma,   U+0393 ISOgrk3 -->
  aHtmlEntities.AddObject('frasl',pointer(8260)); // fraction slash, U+2044 NEW -->
  aHtmlEntities.AddObject('frac34',pointer(190)); // vulgar fraction three quarters = fraction three quarters, U+00BE ISOnum -->
  aHtmlEntities.AddObject('frac14',pointer(188)); // vulgar fraction one quarter = fraction one quarter, U+00BC ISOnum -->
  aHtmlEntities.AddObject('frac12',pointer(189)); // vulgar fraction one half = fraction one half, U+00BD ISOnum -->
  aHtmlEntities.AddObject('forall',pointer(8704)); // for all, U+2200 ISOtech -->
  aHtmlEntities.AddObject('fnof',pointer(402)); // latin small f with hook = function   = florin, U+0192 ISOtech -->
  aHtmlEntities.AddObject('exist',pointer(8707)); // there exists, U+2203 ISOtech -->
  aHtmlEntities.AddObject('euro',pointer(8364)); // euro sign, U+20AC NEW -->
  aHtmlEntities.AddObject('euml',pointer(235)); // latin small letter e with diaeresis, U+00EB ISOlat1 -->
  aHtmlEntities.AddObject('Euml',pointer(203)); // latin capital letter E with diaeresis, U+00CB ISOlat1 -->
  aHtmlEntities.AddObject('eth',pointer(240)); // latin small letter eth, U+00F0 ISOlat1 -->
  aHtmlEntities.AddObject('ETH',pointer(208)); // latin capital letter ETH, U+00D0 ISOlat1 -->
  aHtmlEntities.AddObject('eta',pointer(951)); // greek small letter eta, U+03B7 ISOgrk3 -->
  aHtmlEntities.AddObject('Eta',pointer(919)); // greek capital letter eta, U+0397 -->
  aHtmlEntities.AddObject('equiv',pointer(8801)); // identical to, U+2261 ISOtech -->
  aHtmlEntities.AddObject('epsilon',pointer(949)); // greek small letter epsilon,   U+03B5 ISOgrk3 -->
  aHtmlEntities.AddObject('Epsilon',pointer(917)); // greek capital letter epsilon, U+0395 -->
  aHtmlEntities.AddObject('ensp',pointer(8194)); // en space, U+2002 ISOpub -->
  aHtmlEntities.AddObject('emsp',pointer(8195)); // em space, U+2003 ISOpub -->
  aHtmlEntities.AddObject('empty',pointer(8709)); // empty set = null set = diameter,    U+2205 ISOamso -->
  aHtmlEntities.AddObject('egrave',pointer(232)); // latin small letter e with grave, U+00E8 ISOlat1 -->
  aHtmlEntities.AddObject('Egrave',pointer(200)); // latin capital letter E with grave, U+00C8 ISOlat1 -->
  aHtmlEntities.AddObject('ecirc',pointer(234)); // latin small letter e with circumflex, U+00EA ISOlat1 -->
  aHtmlEntities.AddObject('Ecirc',pointer(202)); // latin capital letter E with circumflex, U+00CA ISOlat1 -->
  aHtmlEntities.AddObject('eacute',pointer(233)); // latin small letter e with acute, U+00E9 ISOlat1 -->
  aHtmlEntities.AddObject('Eacute',pointer(201)); // latin capital letter E with acute, U+00C9 ISOlat1 -->
  aHtmlEntities.AddObject('divide',pointer(247)); // division sign, U+00F7 ISOnum -->
  aHtmlEntities.AddObject('diams',pointer(9830)); // black diamond suit, U+2666 ISOpub -->
  aHtmlEntities.AddObject('delta',pointer(948)); // greek small letter delta,   U+03B4 ISOgrk3 -->
  aHtmlEntities.AddObject('Delta',pointer(916)); // greek capital letter delta,   U+0394 ISOgrk3 -->
  aHtmlEntities.AddObject('deg',pointer(176)); // degree sign, U+00B0 ISOnum -->
  aHtmlEntities.AddObject('dArr',pointer(8659)); // downwards double arrow, U+21D3 ISOamsa -->
  aHtmlEntities.AddObject('darr',pointer(8595)); // downwards arrow, U+2193 ISOnum -->
  aHtmlEntities.AddObject('Dagger',pointer(8225)); // double dagger, U+2021 ISOpub -->
  aHtmlEntities.AddObject('dagger',pointer(8224)); // dagger, U+2020 ISOpub -->
  aHtmlEntities.AddObject('curren',pointer(164)); // currency sign, U+00A4 ISOnum -->
  aHtmlEntities.AddObject('cup',pointer(8746)); // union = cup, U+222A ISOtech -->
  aHtmlEntities.AddObject('crarr',pointer(8629)); // downwards arrow with corner leftwards    = carriage return, U+21B5 NEW -->
  aHtmlEntities.AddObject('copy',pointer(169)); // copyright sign, U+00A9 ISOnum -->
  aHtmlEntities.AddObject('cong',pointer(8773)); // approximately equal to, U+2245 ISOtech -->
  aHtmlEntities.AddObject('clubs',pointer(9827)); // black club suit = shamrock,    U+2663 ISOpub -->
  aHtmlEntities.AddObject('circ',pointer(710)); // modifier letter circumflex accent,   U+02C6 ISOpub -->
  aHtmlEntities.AddObject('chi',pointer(967)); // greek small letter chi, U+03C7 ISOgrk3 -->
  aHtmlEntities.AddObject('Chi',pointer(935)); // greek capital letter chi, U+03A7 -->
  aHtmlEntities.AddObject('cent',pointer(162)); // cent sign, U+00A2 ISOnum -->
  aHtmlEntities.AddObject('cedil',pointer(184)); // cedilla = spacing cedilla, U+00B8 ISOdia -->
  aHtmlEntities.AddObject('ccedil',pointer(231)); // latin small letter c with cedilla, U+00E7 ISOlat1 -->
  aHtmlEntities.AddObject('Ccedil',pointer(199)); // latin capital letter C with cedilla, U+00C7 ISOlat1 -->
  aHtmlEntities.AddObject('cap',pointer(8745)); // intersection = cap, U+2229 ISOtech -->
  aHtmlEntities.AddObject('bull',pointer(8226)); // bullet = black small circle,    U+2022 ISOpub  -->
  aHtmlEntities.AddObject('brvbar',pointer(166)); // broken bar = broken vertical bar, U+00A6 ISOnum -->
  aHtmlEntities.AddObject('beta',pointer(946)); // greek small letter beta, U+03B2 ISOgrk3 -->
  aHtmlEntities.AddObject('Beta',pointer(914)); // greek capital letter beta, U+0392 -->
  aHtmlEntities.AddObject('bdquo',pointer(8222)); // double low-9 quotation mark, U+201E NEW -->
  aHtmlEntities.AddObject('auml',pointer(228)); // latin small letter a with diaeresis, U+00E4 ISOlat1 -->
  aHtmlEntities.AddObject('Auml',pointer(196)); // latin capital letter A with diaeresis, U+00C4 ISOlat1 -->
  aHtmlEntities.AddObject('atilde',pointer(227)); // latin small letter a with tilde, U+00E3 ISOlat1 -->
  aHtmlEntities.AddObject('Atilde',pointer(195)); // latin capital letter A with tilde, U+00C3 ISOlat1 -->
  aHtmlEntities.AddObject('asymp',pointer(8776)); // almost equal to = asymptotic to,    U+2248 ISOamsr -->
  aHtmlEntities.AddObject('aring',pointer(229)); // latin small letter a with ring above = latin small letter a ring, U+00E5 ISOlat1 -->
  aHtmlEntities.AddObject('Aring',pointer(197)); // latin capital letter A with ring above = latin capital letter A ring, U+00C5 ISOlat1 -->
  aHtmlEntities.AddObject('ang',pointer(8736)); // angle, U+2220 ISOamso -->
  aHtmlEntities.AddObject('and',pointer(8743)); // logical and = wedge, U+2227 ISOtech -->
  aHtmlEntities.AddObject('amp',pointer(38)); // ampersand, U+0026 ISOnum -->
  aHtmlEntities.AddObject('alpha',pointer(945)); // greek small letter alpha,   U+03B1 ISOgrk3 -->
  aHtmlEntities.AddObject('Alpha',pointer(913)); // greek capital letter alpha, U+0391 -->
  aHtmlEntities.AddObject('alefsym',pointer(8501)); // alef symbol = first transfinite cardinal,    U+2135 NEW -->
  aHtmlEntities.AddObject('agrave',pointer(224)); // latin small letter a with grave = latin small letter a grave, U+00E0 ISOlat1 -->
  aHtmlEntities.AddObject('Agrave',pointer(192)); // latin capital letter A with grave = latin capital letter A grave, U+00C0 ISOlat1 -->
  aHtmlEntities.AddObject('aelig',pointer(230)); // latin small letter ae = latin small ligature ae, U+00E6 ISOlat1 -->
  aHtmlEntities.AddObject('AElig',pointer(198)); // latin capital letter AE = latin capital ligature AE, U+00C6 ISOlat1 -->
  aHtmlEntities.AddObject('acute',pointer(180)); // acute accent = spacing acute, U+00B4 ISOdia -->
  aHtmlEntities.AddObject('acirc',pointer(226)); // latin small letter a with circumflex, U+00E2 ISOlat1 -->
  aHtmlEntities.AddObject('Acirc',pointer(194)); // latin capital letter A with circumflex, U+00C2 ISOlat1 -->
  aHtmlEntities.AddObject('aacute',pointer(225)); // latin small letter a with acute, U+00E1 ISOlat1 -->
  aHtmlEntities.AddObject('Aacute',pointer(193)); // latin capital letter A with acute, U+00C1 ISOlat1 -->

end;

{*******************************************************************}
function  ALXMLCDataElementEncode(const Src: AnsiString): AnsiString;
Begin
  //  The preferred approach to using CDATA sections for encoding text that contains the triad "]]>" is to use multiple CDATA sections by splitting each
  //  occurrence of the triad just before the ">". For example, to encode "]]>" one would write:
  //  <![CDATA[]]]]><![CDATA[>]]>
  //  This means that to encode "]]>" in the middle of a CDATA section, replace all occurrences of "]]>" with the following:
  //  ]]]]><![CDATA[>
  Result := alStringReplace(Src,']]>',']]]]><![CDATA[>',[rfReplaceAll]);
End;

{*************************************************}
{we use useNumericReference by default because it's
 compatible with XHTML, especially because of the &apos; entity}
function ALXMLTextElementEncode(const Src: AnsiString; const useNumericReference: boolean = True): AnsiString;
var i, l: integer;
    Buf, P: PAnsiChar;
    ch: Integer;
begin
  Result := '';
  L := Length(src);
  if L = 0 then exit;
  GetMem(Buf, L * 6); // to be on the *very* safe side
  try
    P := Buf;
    for i := low(Src) to High(Src) do begin
      ch := Ord(src[i]);
      case ch of
        34: begin // quot "
              if useNumericReference then begin
                ALStrMove('&#34;', P, 5);
                Inc(P, 5);
              end
              else begin
                ALStrMove('&quot;', P, 6);
                Inc(P, 6);
              end;
            end;
        38: begin // amp  &
              if useNumericReference then begin
                ALStrMove('&#38;', P, 5);
                Inc(P, 5);
              end
              else begin
                ALStrMove('&amp;', P, 5);
                Inc(P, 5);
              end;
            end;
        39: begin // apos  '
              if useNumericReference then begin
                ALStrMove('&#39;', P, 5);
                Inc(P, 5);
              end
              else begin
                ALStrMove('&apos;', P, 6);  // !! warning this entity not work in HTML nor in XHTML under IE !!
                Inc(P, 6);
              end;
            end;
        60: begin // lt   <
              if useNumericReference then begin
                ALStrMove('&#60;', P, 5);
                Inc(P, 5);
              end
              else begin
                ALStrMove('&lt;', P, 4);
                Inc(P, 4);
              end;
            end;
        62: begin // gt   >
              if useNumericReference then begin
                ALStrMove('&#62;', P, 5);
                Inc(P, 5);
              end
              else begin
                ALStrMove('&gt;', P, 4);
                Inc(P, 4);
              end;
            end;
        else Begin
          P^:= AnsiChar(ch);
          Inc(P);
        end;
      end;
    end;
    SetString(Result, Buf, P - Buf);
  finally
    FreeMem(Buf);
  end;
end;

{*****************************************************}
procedure ALXMLTextElementDecodeV(var Str: AnsiString);

var CurrPos: integer;
    Ln: integer;
    PResHead: PAnsiChar;
    PResTail: PAnsiChar;
    Chars: array[1..10] of AnsiChar;
    IsUniqueString: boolean;

    {------------------------------}
    procedure _GenerateUniqueString;
    var Padding: integer;
    begin
      Padding := PResTail - PResHead;
      UniqueString(Str);
      PResHead := PAnsiChar(Str);
      PResTail := PResHead + Padding;
      IsUniqueString := true;
    end;

    {--------------------------------------------------------}
    function _DecimalToInt(I: integer; Ch: AnsiChar): integer;
    begin
      Result := I * 10 + Ord(Ch) - Ord('0');
    end;

    {----------------------------------------------------}
    function _HexToInt(I: integer; Ch: AnsiChar): integer;
    begin
      case Ch of
        '0'..'9': Result := I * 16 + Ord(Ch) - Ord('0');
        'a'..'f': Result := I * 16 + Ord(Ch) - Ord('a') + 10;
        'A'..'F': Result := I * 16 + Ord(Ch) - Ord('A') + 10;
        else raise EALException.Create('Wrong HEX-character found');
      end;
    end;

    {---------------------------------}
    procedure _CopyCurrPosCharToResult;
    begin
      if IsUniqueString then PResTail^ := Str[CurrPos];
      Inc(PResTail);
      Inc(CurrPos);
    end;

    {-----------------------------------------------------------------------}
    procedure _CopyAnsiCharToResult(aCharInt: integer; aNewCurrPos: integer);
    begin
      if not IsUniqueString then _GenerateUniqueString;
      PResTail^ := AnsiChar(aCharInt);
      inc(PResTail);
      CurrPos := aNewCurrPos;
    end;

    {--------------------------------------------------------------------------}
    procedure _CopyUnicodeCharToResult(aCharInt: integer; aNewCurrPos: integer);
    var LString: AnsiString;
        k: integer;
    begin
      if not IsUniqueString then _GenerateUniqueString;
      LString := AnsiString(Char(aCharInt));
      for k := low(LString) to high(LString) do begin
        PResTail^ := LString[k];
        Inc(PResTail);
      end;
      CurrPos := aNewCurrPos;
    end;

    {---------------------------------------------------------------}
    procedure _CopyHexadecimalEntityToResult(aEntityLength: integer); // aEntityLength include the last ; but not the first &
    var i: integer;
        Res: integer;
    begin
      Res := 0;
      for i := 3 to aEntityLength - 1 do  // 3 because Chars[1] = # and Chars[2] = x
        Res := _HexToInt(Res, Chars[i]);
      _CopyUnicodeCharToResult(Res, CurrPos + aEntityLength + 1); // ...&#x0af8;...
                                                                  //    ^CurrPos and aEntityLength=7
                                                                  // =>
                                                                  // ...&#x0af8;...
                                                                  //            ^CurrPos
    end;

    {-----------------------------------------------------------}
    procedure _CopyDecimalEntityToResult(aEntityLength: integer); // aEntityLength include the last ; but not the first &
    var i: integer;
        Res: integer;
    begin
      Res := 0;
      for i := 2 to aEntityLength - 1 do // 2 because Chars[1] = #
        Res := _DecimalToInt(Res, Chars[i]);
      _CopyUnicodeCharToResult(Res, CurrPos + aEntityLength + 1); // ...&#2345;...
                                                                  //    ^CurrPos and aEntityLength=6
                                                                  // =>
                                                                  // ...&#2345;...
                                                                  //           ^CurrPos
    end;

var i, j, l: integer;

begin

  {Init var}
  CurrPos := low(Str);
  Ln := High(Str);
  IsUniqueString := false;
  PResHead := PAnsiChar(Str);
  PResTail := PResHead;

  {Start loop}
  while CurrPos <= Ln do begin

    {XML-Entity detected}
    if Str[CurrPos] = '&' then begin

      {Construct chars array of the XML-entity}
      j := CurrPos + 1;
      i := 1;
      while (j <= Ln) and (Str[j] <> ';') and (i <= 10) do begin
        Chars[i] := Str[j];
        Inc(i);
        Inc(j);
      end;

      {If XML-entity is valid}
      if (j <= Ln) and (i <= 10) then begin

        {Fill the remaining part of array by #0}
        while i <= 10 do begin
          Chars[i] := #0;
          Inc(i);
        end;

        {Numeric XML-entity}
        // see: https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references
        // It states that both character types like &# or &#x (so decimal or hexadecimal) represent
        // Universal Character Set/Unicode code points.
        if Chars[1] = '#' then begin

          {Numeric hexadecimal XML-entity}
          if Chars[2] = 'x' then begin

            l := j - CurrPos; {Length of entity}

            // Chars[3] of entity should be in this case in 0..9,a..f,A..F and
            // all the others must be 0..9,a..f,A..F or #0
            if (Chars[3]  in ['A'..'F', 'a'..'f', '0'..'9']) and
               ((L <= 4) or (Chars[4]  in ['A'..'F', 'a'..'f', '0'..'9'])) and
               ((L <= 5) or (Chars[5]  in ['A'..'F', 'a'..'f', '0'..'9'])) and
               ((L <= 6) or (Chars[6]  in ['A'..'F', 'a'..'f', '0'..'9'])) and
               ((L <= 7) or (Chars[7]  in ['A'..'F', 'a'..'f', '0'..'9'])) and
               ((L <= 8) or (Chars[8]  in ['A'..'F', 'a'..'f', '0'..'9'])) and
               ((L <= 9) or (Chars[9]  in ['A'..'F', 'a'..'f', '0'..'9'])) and
               ((L <= 10) or (Chars[10] in ['A'..'F', 'a'..'f', '0'..'9'])) then _CopyHexadecimalEntityToResult(l{Length of entity})
            else _CopyCurrPosCharToResult;

          end

          {Plain numeric decimal XML-entity}
          else begin

            l := j - CurrPos; {Length of entity}

            // Chars[2] of entity should be in this case in 0..9 and
            // all the others must be 0..9 or #0
            if (Chars[2]  in ['0'..'9']) and
               ((L <= 3) or (Chars[3]  in ['0'..'9'])) and
               ((L <= 4) or (Chars[4]  in ['0'..'9'])) and
               ((L <= 5) or (Chars[5]  in ['0'..'9'])) and
               ((L <= 6) or (Chars[6]  in ['0'..'9'])) and
               ((L <= 7) or (Chars[7]  in ['0'..'9'])) and
               ((L <= 8) or (Chars[8]  in ['0'..'9'])) and
               ((L <= 9) or (Chars[9]  in ['0'..'9'])) and
               ((L <= 10) or (Chars[10] in ['0'..'9'])) then _CopyDecimalEntityToResult(l{Length of entity})
            else _CopyCurrPosCharToResult;

          end;

        end

        {literal XML-entity}
        else begin

          if      (Chars[1] = 'q') and
                  (Chars[2] = 'u') and
                  (Chars[3] = 'o') and
                  (Chars[4] = 't') then _CopyAnsiCharToResult(34, j + 1) // "

          else if (Chars[1] = 'a') and
                  (Chars[2] = 'p') and
                  (Chars[3] = 'o') and
                  (Chars[4] = 's') then _CopyAnsiCharToResult(39, j + 1) // '

          else if (Chars[1] = 'a') and
                  (Chars[2] = 'm') and
                  (Chars[3] = 'p') then _CopyAnsiCharToResult(38, j + 1) // &

          else if (Chars[1] = 'l') and
                  (Chars[2] = 't') then _CopyAnsiCharToResult(60, j + 1) // <

          else if (Chars[1] = 'g') and
                  (Chars[2] = 't') then _CopyAnsiCharToResult(62, j + 1) // >

          else _CopyCurrPosCharToResult;

        end;

      end
      else _CopyCurrPosCharToResult;

    end
    else _CopyCurrPosCharToResult;

  end;

  {Change the length the string only if some modifications was done.
   Else we don't need to do anything.}
  if PResTail - PResHead <> length(Str) then
    SetLength(Str, PResTail - PResHead);

end;

{*****************************************************************}
function ALXMLTextElementDecode(const Src: AnsiString): AnsiString;
begin
  result := Src;
  ALXMLTextElementDecodeV(result);
end;

{******************************************}
function ALHTMLEncode(const Src: AnsiString;
                      const EncodeASCIIHtmlEntities: Boolean = True;
                      const useNumericReference: boolean = True): AnsiString;

var i, k, l: integer;
    Buf, P: PAnsiChar;
    LEntityStr: AnsiString;
    LEntityInt: Integer;
    LIndex: integer;
    LTmpString: String;
    LstUnicodeEntitiesNumber: TALIntegerList;

begin
  Result := '';
  If Src='' then Exit;

  LstUnicodeEntitiesNumber := TALIntegerList.create;
  Try
    if not useNumericReference then begin
      LstUnicodeEntitiesNumber.Duplicates := DupIgnore;
      LstUnicodeEntitiesNumber.Sorted := True;
      For i := 0 to _ALHtmlEntities.Count - 1 do
        LstUnicodeEntitiesNumber.AddObject(integer(_ALHtmlEntities.Objects[i]),pointer(i));
    end;

    LTmpString := String(Src);
    L := length(LTmpString);
    If L=0 then Exit;

    GetMem(Buf, length(Src) * 12); // to be on the *very* safe side
    try
      P := Buf;
      For i := 1 to L do begin
        LEntityInt := Integer(LTmpString[i]);
        Case LEntityInt of
          34: begin // quot "
                If EncodeASCIIHtmlEntities then begin
                  if useNumericReference then begin
                    ALStrMove('&#34;', P, 5);
                    Inc(P, 5);
                  end
                  else begin
                    ALStrMove('&quot;', P, 6);
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
                    ALStrMove('&#38;', P, 5);
                    Inc(P, 5);
                  end
                  else begin
                    ALStrMove('&amp;', P, 5);
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
                  ALStrMove('&#39;', P, 5);
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
                    ALStrMove('&#60;', P, 5);
                    Inc(P, 5);
                  end
                  else begin
                    ALStrMove('&lt;', P, 4);
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
                    ALStrMove('&#62;', P, 5);
                    Inc(P, 5);
                  end
                  else begin
                    ALStrMove('&gt;', P, 4);
                    Inc(P, 4);
                  end;
                end
                else Begin
                  P^ := '>';
                  Inc(P, 1);
                end;
              end;
          else begin
            if (LEntityInt > 127) then begin
              if useNumericReference then LEntityStr := '&#'+ALIntToStr(LEntityInt)+';'
              else begin
                LIndex := LstUnicodeEntitiesNumber.IndexOf(LEntityInt);
                If LIndex >= 0 Then begin
                  LEntityStr := _ALHtmlEntities[integer(LstUnicodeEntitiesNumber.Objects[LIndex])];
                  If LEntityStr <> '' then LEntityStr := '&' + LEntityStr + ';'
                  else LEntityStr := '&#'+ALIntToStr(LEntityInt)+';'
                end
                else LEntityStr := '&#'+ALIntToStr(LEntityInt)+';'
              end;
            end
            else LEntityStr := ansistring(LTmpString[i]);

            for k := 1 to Length(LEntityStr) do begin
              P^ := LEntityStr[k];
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

{*******************************************************}
function ALHTMLDecode(const Src: AnsiString): AnsiString;

var CurrentSrcPos, CurrentResultPos : Integer;

  {---------------------------------------}
  procedure _CopyCurrentSrcPosCharToResult;
  Begin
    result[CurrentResultPos] := src[CurrentSrcPos];
    inc(CurrentResultPos);
    inc(CurrentSrcPos);
  end;

  {----------------------------------------------------------------------------------}
  procedure _CopyCharToResult(aUnicodeOrdEntity: Integer; aNewCurrentSrcPos: integer);
  Var LString: AnsiString;
      K: integer;
  Begin
    LString := AnsiString(Char(aUnicodeOrdEntity));
    For k := low(LString) to high(LString) do begin
      result[CurrentResultPos] := LString[k];
      inc(CurrentResultPos);
    end;
    CurrentSrcPos := aNewCurrentSrcPos;
  end;

var j: integer;
    LTmpInteger: Integer;
    SrcLength: integer;

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
            if ALTryStrToInt('$' + ALCopyStr(Src,
                                             CurrentSrcPos+3,
                                             j-CurrentSrcPos-3),
                             LTmpInteger)
            then _CopyCharToResult(LTmpInteger, J+1)
            else _CopyCurrentSrcPosCharToResult;
          end

          {HTML entity is numeric}
          else begin

            {numeric HTML entity is valid}
            if ALTryStrToInt(ALCopyStr(Src,
                                       CurrentSrcPos+2,
                                       j-CurrentSrcPos-2),
                             LTmpInteger)
            then _CopyCharToResult(LTmpInteger, J+1)
            else _CopyCurrentSrcPosCharToResult;

          end;

        end

        {HTML entity is litteral}
        else begin

          LTmpInteger := _ALHtmlEntities.IndexOf(ALCopyStr(Src,
                                                           CurrentSrcPos+1,
                                                           j-CurrentSrcPos-1));
          If LTmpInteger >= 0 then _CopyCharToResult(integer(_ALHtmlEntities.Objects[LTmpInteger]),J+1)
          else _CopyCurrentSrcPosCharToResult;

        end;

      end
      else _CopyCurrentSrcPosCharToResult;

    end
    else _CopyCurrentSrcPosCharToResult;

  end;

  setLength(Result,CurrentResultPos-1);
end;

{******************************************************************************************}
// https://developer.mozilla.org/en-US/docs/JavaScript/Guide/Values,_variables,_and_literals
function  ALJavascriptEncode(const Src: AnsiString; const useNumericReference: boolean = True): AnsiString;
var i, l: integer;
    Buf, P: PAnsiChar;
    ch: Integer;
begin
  Result := '';
  L := Length(src);
  if L = 0 then exit;
  if useNumericReference then GetMem(Buf, L * 6) // to be on the *very* safe side
  else GetMem(Buf, L * 2); // to be on the *very* safe side
  try
    P := Buf;
    for i := low(Src) to high(Src) do begin
      ch := Ord(src[i]);
      case ch of
        8: begin // Backspace
             if useNumericReference then begin
               ALStrMove('\u0008', P, 6);
               Inc(P, 6);
             end
             else begin
               ALStrMove('\b', P, 2);
               Inc(P, 2);
             end;
           end;
        9: begin // Tab
             if useNumericReference then begin
               ALStrMove('\u0009', P, 6);
               Inc(P, 6);
             end
             else begin
               ALStrMove('\t', P, 2);
               Inc(P, 2);
             end;
           end;
        10: begin // New line
              if useNumericReference then begin
                ALStrMove('\u000A', P, 6);
                Inc(P, 6);
              end
              else begin
                ALStrMove('\n', P, 2);
                Inc(P, 2);
              end;
            end;
        11: begin // Vertical tab
              if useNumericReference then begin
                ALStrMove('\u000B', P, 6);
                Inc(P, 6);
              end
              else begin
                ALStrMove('\v', P, 2);
                Inc(P, 2);
              end;
            end;
        12: begin // Form feed
              if useNumericReference then begin
                ALStrMove('\u000C', P, 6);
                Inc(P, 6);
              end
              else begin
                ALStrMove('\f', P, 2);
                Inc(P, 2);
              end;
            end;
        13: begin // Carriage return
              if useNumericReference then begin
                ALStrMove('\u000D', P, 6);
                Inc(P, 6);
              end
              else begin
                ALStrMove('\r', P, 2);
                Inc(P, 2);
              end;
            end;
        34: begin // Double quote
              if useNumericReference then begin
                ALStrMove('\u0022', P, 6);
                Inc(P, 6);
              end
              else begin
                ALStrMove('\"', P, 2);
                Inc(P, 2);
              end;
            end;
        38: begin // & ... we need to encode it because in javascript &#39; or &amp; will be converted to ' and error unterminated string
              ALStrMove('\u0026', P, 6);
              Inc(P, 6);
            end;
        39: begin // Apostrophe or single quote
              if useNumericReference then begin
                ALStrMove('\u0027', P, 6);
                Inc(P, 6);
              end
              else begin
                ALStrMove('\''', P, 2);
                Inc(P, 2);
              end;
            end;
        60: begin // < ... mostly to hide all </script> tag inside javascript.
                  // http://www.wwco.com/~wls/blog/2007/04/25/using-script-in-a-javascript-literal/
              ALStrMove('\u003C', P, 6);
              Inc(P, 6);
            end;
        62: begin // > ... mostly to hide all HTML tag inside javascript.
              ALStrMove('\u003E', P, 6);
              Inc(P, 6);
            end;
        92: begin // Backslash character (\).
              if useNumericReference then begin
                ALStrMove('\u005C', P, 6);
                Inc(P, 6);
              end
              else begin
                ALStrMove('\\', P, 2);
                Inc(P, 2);
              end;
            end;
        else Begin
          P^:= AnsiChar(ch);
          Inc(P);
        end;
      end;
    end;
    SetString(Result, Buf, P - Buf);
  finally
    FreeMem(Buf);
  end;
end;

{*************************************************}
procedure ALJavascriptDecodeV(Var Str: AnsiString);

var CurrPos : Integer;
    pResTail: PansiChar;
    pResHead: pansiChar;
    Ch1, Ch2, Ch3, Ch4, Ch5: ansiChar;
    IsUniqueString: boolean;

    {------------------------------}
    procedure _GenerateUniqueString;
    var Padding: integer;
    begin
      Padding := PResTail - PResHead;
      UniqueString(Str);
      PResHead := PAnsiChar(Str);
      PResTail := PResHead + Padding;
      IsUniqueString := true;
    end;

    {----------------------------------------------------}
    function _OctToInt(I: integer; Ch: ansiChar): integer;
    begin
      Result := I * 8 + Ord(Ch) - Ord('0');
    end;

    {----------------------------------------------------}
    function _HexToInt(I: integer; Ch: ansiChar): integer;
    begin
      case Ch of
        '0'..'9': Result := I * 16 + Ord(Ch) - Ord('0');
        'a'..'f': Result := I * 16 + Ord(Ch) - Ord('a') + 10;
        'A'..'F': Result := I * 16 + Ord(Ch) - Ord('A') + 10;
        else raise EALException.Create('Wrong HEX-character found');
      end;
    end;

    {---------------------------------}
    procedure _CopyCurrPosCharToResult;
    Begin
      if IsUniqueString then pResTail^ := Str[CurrPos];
      inc(pResTail);
      inc(CurrPos);
    end;

    {-----------------------------------------------------------------------}
    procedure _CopyAnsiCharToResult(aCharInt: Integer; aNewCurrPos: integer);
    begin
      if not IsUniqueString then _GenerateUniqueString;
      pResTail^ := AnsiChar(aCharInt);
      inc(pResTail);
      CurrPos := aNewCurrPos;
    end;

    {--------------------------------------------------------------------------}
    procedure _CopyUnicodeCharToResult(aCharInt: Integer; aNewCurrPos: integer); overload;
    var LString: AnsiString;
        K: integer;
    begin
      if not IsUniqueString then _GenerateUniqueString;
      LString := AnsiString(Char(aCharInt));
      For k := low(LString) to high(LString) do begin
        pResTail^ := LString[k];
        inc(pResTail);
      end;
      CurrPos := aNewCurrPos;
    end;

    {---------------------------------}
    procedure _CopyUnicodeCharToResult; overload;
    var I: integer;
    Begin
      I := _HexToInt(0, ch2);
      I := _HexToInt(I, ch3);
      I := _HexToInt(I, ch4);
      I := _HexToInt(I, ch5);
      _CopyUnicodeCharToResult(I, CurrPos+6);
    end;

    {------------------------------------------------------------------------}
    procedure _CopyIso88591CharToResult(aCharInt: byte; aNewCurrPos: integer);
    var LChar: WideChar;
        LString: AnsiString;
        K: integer;
    begin
      if not IsUniqueString then _GenerateUniqueString;
      if UnicodeFromLocaleChars(28591, //CodePage,
                                0, // Flags
                                @aCharInt,// LocaleStr
                                1, // LocaleStrLen
                                @LChar, // UnicodeStr
                                1)<> 1 then RaiseLastOSError; // UnicodeStrLen
      LString := AnsiString(LChar);
      for k := low(LString) to high(LString) do begin
        pResTail^ := LString[k];
        inc(pResTail);
      end;
      CurrPos := aNewCurrPos;
    end;

    {-------------------------------------}
    procedure _CopyHexIso88591CharToResult;
    var I: integer;
    Begin
      I := _HexToInt(0, ch2);
      I := _HexToInt(I, ch3);
      _CopyIso88591CharToResult(I, CurrPos+4);
    end;

    {-------------------------------------}
    procedure _CopyOctIso88591CharToResult;
    var I: integer;
    Begin
      I := _OctToInt(0, ch1);
      I := _OctToInt(I, ch2);
      I := _OctToInt(I, ch3);
      if I in [0..255] then _CopyIso88591CharToResult(I, CurrPos+4)
      else inc(CurrPos); // delete the \
    end;

var Ln: integer;

begin

  {init var}
  CurrPos := low(Str);
  Ln := high(Str);
  IsUniqueString := false;
  pResHead := PansiChar(Str);
  pResTail := pResHead;

  {start loop}
  while (CurrPos <= Ln) do begin

    {escape char detected}
    If Str[CurrPos]='\' then begin

      if (CurrPos <= Ln - 5) then begin
        Ch1 := Str[CurrPos + 1];
        Ch2 := Str[CurrPos + 2];
        Ch3 := Str[CurrPos + 3];
        Ch4 := Str[CurrPos + 4];
        Ch5 := Str[CurrPos + 5];
      end
      else if (CurrPos <= Ln - 3) then begin
        Ch1 := Str[CurrPos + 1];
        Ch2 := Str[CurrPos + 2];
        Ch3 := Str[CurrPos + 3];
        Ch4 := #0;
        Ch5 := #0;
      end
      else if (CurrPos <= Ln - 1) then begin
        Ch1 := Str[CurrPos + 1];
        Ch2 := #0;
        Ch3 := #0;
        Ch4 := #0;
        Ch5 := #0;
      end
      else begin
        Ch1 := #0;
        Ch2 := #0;
        Ch3 := #0;
        Ch4 := #0;
        Ch5 := #0;
      end;

      // Backspace
      if Ch1 = 'b' then _CopyAnsiCharToResult(8, CurrPos + 2)

      // Tab
      else if Ch1 = 't' then _CopyAnsiCharToResult(9, CurrPos + 2)

      // New line
      else if Ch1 = 'n' then _CopyAnsiCharToResult(10, CurrPos + 2)

      // Vertical tab
      else if Ch1 = 'v' then _CopyAnsiCharToResult(11, CurrPos + 2)

      // Form feed
      else if Ch1 = 'f' then _CopyAnsiCharToResult(12, CurrPos + 2)

      // Carriage return
      else if Ch1 = 'r' then _CopyAnsiCharToResult(13, CurrPos + 2)

      // Double quote
      else if Ch1 = '"' then _CopyAnsiCharToResult(34, CurrPos + 2)

      // Apostrophe or single quote
      else if Ch1 = '''' then _CopyAnsiCharToResult(39, CurrPos + 2)

      // Backslash character (\).
      else if Ch1 = '\' then _CopyAnsiCharToResult(92, CurrPos + 2)

      // The character with the Latin-1 encoding specified by up to three octal digits XXX between 0 and 377
      else if (Ch1 in ['0'..'7']) and
              (Ch2 in ['0'..'7']) and
              (Ch3 in ['0'..'7']) then _CopyOctIso88591CharToResult

      // The character with the Latin-1 encoding specified by the two hexadecimal digits XX between 00 and FF
      else if (Ch1 = 'x') and
              (Ch2 in ['A'..'F', 'a'..'f', '0'..'9']) and
              (Ch3 in ['A'..'F', 'a'..'f', '0'..'9']) then _CopyHexIso88591CharToResult

      // The Unicode character specified by the four hexadecimal digits XXXX.
      else if (Ch1 = 'u') and
              (ch2 in ['A'..'F', 'a'..'f', '0'..'9']) and
              (ch3 in ['A'..'F', 'a'..'f', '0'..'9']) and
              (ch4 in ['A'..'F', 'a'..'f', '0'..'9']) and
              (ch5 in ['A'..'F', 'a'..'f', '0'..'9']) then _CopyUnicodeCharToResult

      // delete the \
      else if CurrPos <= Ln - 1 then _CopyAnsiCharToResult(Ord(ch1), CurrPos + 2)

    end
    else _CopyCurrPosCharToResult;

  end;

  if pResTail-pResHead <> length(Str) then
    setLength(Str,pResTail-pResHead);

end;

{**************************************************************}
function  ALJavascriptDecode(const Src: AnsiString): AnsiString;
begin
  result := Src;
  ALJavascriptDecodeV(result);
end;

{$ENDIF !ALHideAnsiString}

{******************************************************************************************}
// https://developer.mozilla.org/en-US/docs/JavaScript/Guide/Values,_variables,_and_literals
function  ALJavascriptEncodeU(const Src: String; const useNumericReference: boolean = true): String;
var i, l: integer;
    Buf, P: PChar;
    ch: Integer;
begin
  Result := '';
  L := Length(src);
  if L = 0 then exit;
  if useNumericReference then GetMem(Buf, L * 6) // to be on the *very* safe side
  else GetMem(Buf, L * 2); // to be on the *very* safe side
  try
    P := Buf;
    for i := low(src) to high(src) do begin
      ch := Ord(src[i]);
      case ch of
        8: begin // Backspace
             if useNumericReference then begin
               ALStrMoveU('\u0008', P, 6);
               Inc(P, 6);
             end
             else begin
               ALStrMoveU('\b', P, 2);
               Inc(P, 2);
             end;
           end;
        9: begin // Tab
             if useNumericReference then begin
               ALStrMoveU('\u0009', P, 6);
               Inc(P, 6);
             end
             else begin
               ALStrMoveU('\t', P, 2);
               Inc(P, 2);
             end;
           end;
        10: begin // New line
              if useNumericReference then begin
                ALStrMoveU('\u000A', P, 6);
                Inc(P, 6);
              end
              else begin
                ALStrMoveU('\n', P, 2);
                Inc(P, 2);
              end;
            end;
        11: begin // Vertical tab
              if useNumericReference then begin
                ALStrMoveU('\u000B', P, 6);
                Inc(P, 6);
              end
              else begin
                ALStrMoveU('\v', P, 2);
                Inc(P, 2);
              end;
            end;
        12: begin // Form feed
              if useNumericReference then begin
                ALStrMoveU('\u000C', P, 6);
                Inc(P, 6);
              end
              else begin
                ALStrMoveU('\f', P, 2);
                Inc(P, 2);
              end;
            end;
        13: begin // Carriage return
              if useNumericReference then begin
                ALStrMoveU('\u000D', P, 6);
                Inc(P, 6);
              end
              else begin
                ALStrMoveU('\r', P, 2);
                Inc(P, 2);
              end;
            end;
        34: begin // Double quote
              if useNumericReference then begin
                ALStrMoveU('\u0022', P, 6);
                Inc(P, 6);
              end
              else begin
                ALStrMoveU('\"', P, 2);
                Inc(P, 2);
              end;
            end;
        38: begin // & ... we need to encode it because in javascript &#39; or &amp; will be converted to ' and error unterminated string
              ALStrMoveU('\u0026', P, 6);
              Inc(P, 6);
            end;
        39: begin // Apostrophe or single quote
              if useNumericReference then begin
                ALStrMoveU('\u0027', P, 6);
                Inc(P, 6);
              end
              else begin
                ALStrMoveU('\''', P, 2);
                Inc(P, 2);
              end;
            end;
        60: begin // < ... mostly to hide all </script> tag inside javascript.
                  // http://www.wwco.com/~wls/blog/2007/04/25/using-script-in-a-javascript-literal/
              ALStrMoveU('\u003C', P, 6);
              Inc(P, 6);
            end;
        62: begin // > ... mostly to hide all HTML tag inside javascript.
              ALStrMoveU('\u003E', P, 6);
              Inc(P, 6);
            end;
        92: begin // Backslash character (\).
              if useNumericReference then begin
                ALStrMoveU('\u005C', P, 6);
                Inc(P, 6);
              end
              else begin
                ALStrMoveU('\\', P, 2);
                Inc(P, 2);
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

{**************************}
{$WARN WIDECHAR_REDUCED OFF}
procedure ALJavascriptDecodeVU(Var Str: String);

var CurrPos : Integer;
    pResTail: PChar;
    pResHead: pChar;
    Ch1, Ch2, Ch3, Ch4, Ch5: Char;
    IsUniqueString: boolean;

    {------------------------------}
    procedure _GenerateUniqueString;
    var Padding: integer;
    begin
      Padding := PResTail - PResHead;
      UniqueString(Str);
      PResHead := PChar(Str);
      PResTail := PResHead + Padding;
      IsUniqueString := true;
    end;

    {------------------------------------------------}
    function _OctToInt(I: integer; Ch: Char): integer;
    begin
      Result := I * 8 + Ord(Ch) - Ord('0');
    end;

    {------------------------------------------------}
    function _HexToInt(I: integer; Ch: Char): integer;
    begin
      case Ch of
        '0'..'9': Result := I * 16 + Ord(Ch) - Ord('0');
        'a'..'f': Result := I * 16 + Ord(Ch) - Ord('a') + 10;
        'A'..'F': Result := I * 16 + Ord(Ch) - Ord('A') + 10;
        else raise EALExceptionU.Create('Wrong HEX-character found');
      end;
    end;

    {---------------------------------}
    procedure _CopyCurrPosCharToResult;
    Begin
      if IsUniqueString then pResTail^ := Str[CurrPos];
      inc(pResTail);
      inc(CurrPos);
    end;

    {-------------------------------------------------------------------}
    procedure _CopyCharToResult(aCharInt: Integer; aNewCurrPos: integer);
    begin
      if not IsUniqueString then _GenerateUniqueString;
      pResTail^ := Char(aCharInt);
      inc(pResTail);
      CurrPos := aNewCurrPos;
    end;

    {--------------------------------------------------------------------------}
    procedure _CopyUnicodeCharToResult(aCharInt: Integer; aNewCurrPos: integer); overload;
    begin
      if not IsUniqueString then _GenerateUniqueString;
      pResTail^ := Char(aCharInt);
      inc(pResTail);
      CurrPos := aNewCurrPos;
    end;

    {---------------------------------}
    procedure _CopyUnicodeCharToResult; overload;
    var I: integer;
    Begin
      I := _HexToInt(0, ch2);
      I := _HexToInt(I, ch3);
      I := _HexToInt(I, ch4);
      I := _HexToInt(I, ch5);
      _CopyUnicodeCharToResult(I, CurrPos+6);
    end;

    {------------------------------------------------------------------------}
    procedure _CopyIso88591CharToResult(aCharInt: byte; aNewCurrPos: integer);
    var LChar: WideChar;
    begin
      if not IsUniqueString then _GenerateUniqueString;
      if UnicodeFromLocaleChars(28591, //CodePage,
                                0, // Flags
                                @aCharInt,// LocaleStr
                                1, // LocaleStrLen
                                @LChar, // UnicodeStr
                                1) <> 1 then RaiseLastOSError; // UnicodeStrLen
      pResTail^ := LChar;
      inc(pResTail);
      CurrPos := aNewCurrPos;
    end;

    {-------------------------------------}
    procedure _CopyHexIso88591CharToResult;
    var I: integer;
    Begin
      I := _HexToInt(0, ch2);
      I := _HexToInt(I, ch3);
      _CopyIso88591CharToResult(I, CurrPos+4);
    end;

    {-------------------------------------}
    procedure _CopyOctIso88591CharToResult;
    var I: integer;
    Begin
      I := _OctToInt(0, ch1);
      I := _OctToInt(I, ch2);
      I := _OctToInt(I, ch3);
      if I in [0..255] then _CopyIso88591CharToResult(I, CurrPos+4)
      else inc(CurrPos); // delete the \
    end;

var Ln: integer;

begin

  {init var}
  CurrPos := low(Str);
  Ln := high(Str);
  IsUniqueString := false;
  pResHead := PChar(Str);
  pResTail := pResHead;

  {start loop}
  while (CurrPos <= Ln) do begin

    {escape char detected}
    If Str[CurrPos]='\' then begin

      if (CurrPos <= Ln - 5) then begin
        Ch1 := Str[CurrPos + 1];
        Ch2 := Str[CurrPos + 2];
        Ch3 := Str[CurrPos + 3];
        Ch4 := Str[CurrPos + 4];
        Ch5 := Str[CurrPos + 5];
      end
      else if (CurrPos <= Ln - 3) then begin
        Ch1 := Str[CurrPos + 1];
        Ch2 := Str[CurrPos + 2];
        Ch3 := Str[CurrPos + 3];
        Ch4 := #0;
        Ch5 := #0;
      end
      else if (CurrPos <= Ln - 1) then begin
        Ch1 := Str[CurrPos + 1];
        Ch2 := #0;
        Ch3 := #0;
        Ch4 := #0;
        Ch5 := #0;
      end
      else begin
        Ch1 := #0;
        Ch2 := #0;
        Ch3 := #0;
        Ch4 := #0;
        Ch5 := #0;
      end;

      // Backspace
      if Ch1 = 'b' then _CopyCharToResult(8, CurrPos + 2)

      // Tab
      else if Ch1 = 't' then _CopyCharToResult(9, CurrPos + 2)

      // New line
      else if Ch1 = 'n' then _CopyCharToResult(10, CurrPos + 2)

      // Vertical tab
      else if Ch1 = 'v' then _CopyCharToResult(11, CurrPos + 2)

      // Form feed
      else if Ch1 = 'f' then _CopyCharToResult(12, CurrPos + 2)

      // Carriage return
      else if Ch1 = 'r' then _CopyCharToResult(13, CurrPos + 2)

      // Double quote
      else if Ch1 = '"' then _CopyCharToResult(34, CurrPos + 2)

      // Apostrophe or single quote
      else if Ch1 = '''' then _CopyCharToResult(39, CurrPos + 2)

      // Backslash character (\).
      else if Ch1 = '\' then _CopyCharToResult(92, CurrPos + 2)

      // The character with the Latin-1 encoding specified by up to three octal digits XXX between 0 and 377
      else if (Ch1 in ['0'..'7']) and
              (Ch2 in ['0'..'7']) and
              (Ch3 in ['0'..'7']) then _CopyOctIso88591CharToResult

      // The character with the Latin-1 encoding specified by the two hexadecimal digits XX between 00 and FF
      else if (Ch1 = 'x') and
              (Ch2 in ['A'..'F', 'a'..'f', '0'..'9']) and
              (Ch3 in ['A'..'F', 'a'..'f', '0'..'9']) then _CopyHexIso88591CharToResult

      // The Unicode character specified by the four hexadecimal digits XXXX.
      else if (Ch1 = 'u') and
              (ch2 in ['A'..'F', 'a'..'f', '0'..'9']) and
              (ch3 in ['A'..'F', 'a'..'f', '0'..'9']) and
              (ch4 in ['A'..'F', 'a'..'f', '0'..'9']) and
              (ch5 in ['A'..'F', 'a'..'f', '0'..'9']) then _CopyUnicodeCharToResult

      // delete the \
      else if CurrPos <= Ln - 1 then _CopyCharToResult(Ord(ch1), CurrPos + 2)

    end
    else _CopyCurrPosCharToResult;

  end;

  if pResTail-pResHead <> length(Str) then
    setLength(Str,pResTail-pResHead);

end;
{$WARN WIDECHAR_REDUCED ON}

{*******************************************************}
function  ALJavascriptDecodeU(const Src: String): String;
begin
  result := Src;
  ALJavascriptDecodeVU(result);
end;

{$IFNDEF ALHideAnsiString}

{****************}
{$IFDEF MSWINDOWS}
{This function evaluates the Javascript code given in the
 parameter "aCode" and returns result. The function works
 similar to browser's console, so you can send even the code
 like this "2+2" => returns "4".}
function ALRunJavascript(const aCode: AnsiString): AnsiString;
var HandleResult: HResult;

    {$REGION '_MakeExecution'}
    // see: http://stackoverflow.com/questions/2653797/why-does-couninitialize-cause-an-error-on-exit
    // we create COM-object with CreateOleObject here to make that its creation is handled inside of
    // THIS scope (function MakeExecution) and its destroying is handled inside of this function too
    // on the last "end;" of this function.
    function _MakeExecution(const aCode: AnsiString): AnsiString;
    var LJavaScript: OleVariant;
    begin
      LJavaScript          := CreateOleObject('ScriptControl');
      LJavaScript.Language := 'JavaScript';
      result               := AnsiString(LJavaScript.Eval(String(aCode)));
    end;
    {$ENDREGION}

begin
  // we create here the COM-server that will be actually destroyed
  // on calling of CoUninitialize. What it will do on destroy it depends
  // on the operation system.
  //              |
  //              V
  HandleResult := CoInitializeEx(nil, COINIT_MULTITHREADED);
  if HandleResult <> S_OK then raise EALException.Create('ALRunJavascript: cannot initialize OLE-object');
  try
    result := _MakeExecution(aCode);
  finally
    // Here we deactivate and destroy the COM-server. When it will be destroyed then all the existing
    // OLE-objects will be orphaned, so normally they should be already killed at this time. BUT the
    // problem here that COM-objects mostly destroyed when we reach the end of scope (assume last "end;"
    // of the function). So when the objects are created in THIS scope they will be killed after this
    // CoUninitialize but they cannot be killed on that step because COM-server is already destroyed and
    // no links are kept. This way COM-objects are created in the another scope (local function makeExecution).
    CoUninitialize;
  end;
end;
{$ENDIF}

{*******************************************************************************}
procedure ALHideHtmlUnwantedTagForHTMLHandleTagfunct(Var HtmlContent: AnsiString;
                                                     Const DeleteBodyOfUnwantedTag: Boolean = False;
                                                     const ReplaceUnwantedTagCharBy: AnsiChar = #1); {this char is not use in html}

Var InDoubleQuote : Boolean;
    InSimpleQuote : Boolean;
    P1, P2 : integer;
    X1 : Integer;
    Str1 : AnsiString;

Begin
  P1 := 1;
  While P1 <= length(htmlContent) do begin
    If HtmlContent[P1] = '<' then begin

      X1 := P1;
      Str1 := '';
      while (X1 <= length(Htmlcontent)) and (not (htmlContent[X1] in ['>',' ',#13,#10,#9])) do begin
        Str1 := Str1 + HtmlContent[X1];
        inc(X1);
      end;

      InSimpleQuote := false;
      InDoubleQuote := false;

      //hide script tag
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

      //hide style tag
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

      //hide comment tag
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

      //hide text < tag
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
end;

{*********************************************}
{ because of such link: <A HREF = "obie2.html">
  that is split in 3 line in TagParams}
Procedure ALCompactHtmlTagParams(TagParams: TALStrings);
Var i: integer;
    S1, S2, S3: AnsiString;
    P1, P2, P3: integer;
    Flag2, Flag3: boolean;
Begin
  i := 0;
  While i <= TagParams.Count - 2 do begin
    S1 := TagParams[i];
    S2 := TagParams[i+1];
    if i <= TagParams.Count - 3 then S3 := TagParams[i+2]
    else S3 := '';
    P1 := AlPos('=',S1);
    P2 := AlPos('=',S2);
    P3 := AlPos('=',S3);
    Flag2 := (S2 <> '') and (S2[1] in ['''','"']);
    Flag3 := (S3 <> '') and (S3[1] in ['''','"']);
    IF (P1 <= 0) and
       (S2 = '=') then begin {<A HREF = "obie2.html">}
      If (i <= TagParams.Count - 2) and
         (flag3 or (P3 <= 0))
      then begin
        TagParams[i] := S1 + S2 + S3;
        TagParams.Delete(i+2);
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
            (AlPos('=',S2) = 1)  then begin {<A HREF ="obie2.html">}
      TagParams[i] := S1 + S2;
      tagParams.Delete(i+1);
    end;
    inc(i);
  end;
end;

{**************************************************}
procedure ALExtractHTMLText(HtmlContent: AnsiString;
                            LstExtractedResourceText : TALStrings;
                            Const DecodeHTMLText: Boolean = True);

  {-----------------------------------------------------}
  procedure _Add2LstExtractedResourceText(S: AnsiString);
  Begin
    If DecodeHTMLText then Begin
      S := alHtmlDecode(ALTrim(S));
      S := AlStringReplace(S, #13, ' ', [rfreplaceAll]);
      S := AlStringReplace(S, #10, ' ', [rfreplaceAll]);
      S := AlStringReplace(S, #9,  ' ', [rfreplaceAll]);
      While AlPos('  ',S) > 0 Do
        S := AlStringReplace(S, '  ', ' ', [rfreplaceAll]);
      S := ALTrim(S);
    end;
    If S <> '' then LstExtractedResourceText.add(S);
  end;

Var P1, P2: integer;

Begin
  ALHideHtmlUnwantedTagForHTMLHandleTagfunct(HtmlContent, True);
  HtmlContent := ALFastTagReplace(HtmlContent,
                                  '<',
                                  '>',
                                  #2, {this char is not use in html}
                                  [rfreplaceall]);
  HtmlContent := ALStringReplace(HtmlContent,
                                 #1, {default ReplaceUnwantedTagCharBy use by ALHideHtmlUnwantedTagForHTMLHandleTagfunct ; this char is not use in html}
                                 '<',
                                 [rfreplaceall]);
  HtmlContent := HtmlContent + #2;

  LstExtractedResourceText.Clear;
  P1 := 1;
  P2 := ALpos(#2,HtmlContent);
  While P2 > 0 do begin
    If P2 > P1 then _Add2LstExtractedResourceText(ALCopyStr(HtmlContent,
                                                            P1,
                                                            p2-P1));
    P1 := P2+1;
    P2 := ALposEX(#2,HtmlContent, P1);
  end;
end;

{********************************************************}
function  ALExtractHTMLText(const HtmlContent: AnsiString;
                            Const DecodeHTMLText: Boolean = True): AnsiString;
Var LstExtractedResourceText: TALStrings;
Begin
  LstExtractedResourceText := TALStringList.Create;
  Try
    ALExtractHTMLText(HtmlContent,
                      LstExtractedResourceText,
                      DecodeHTMLText);
    Result := ALTrim(
                AlStringReplace(
                  LstExtractedResourceText.Text,
                  #13#10,
                  ' ',
                  [rfReplaceAll]));
  finally
    LstExtractedResourceText.free;
  end;
end;

{$ENDIF !ALHideAnsiString}



////////////////////////////
/// deprecated functions ///
////////////////////////////

{$IFNDEF ALHideAnsiString}

{**********}
//deprecated
procedure ALUTF8ExtractHTMLText(HtmlContent: AnsiString;
                                LstExtractedResourceText: TALStrings;
                                Const DecodeHTMLText: Boolean = True);
begin
  ALExtractHTMLText(HtmlContent, LstExtractedResourceText, DecodeHTMLText);
end;

{**********}
//deprecated
function  ALUTF8ExtractHTMLText(const HtmlContent: AnsiString;
                                Const DecodeHTMLText: Boolean = True): AnsiString;
begin
  result := ALExtractHTMLText(HtmlContent, DecodeHTMLText);
end;

{**********}
//deprecated
procedure ALUTF8XMLTextElementDecodeV(var Str: AnsiString);
begin
  ALXMLTextElementDecodeV(Str);
end;

{**********}
//deprecated
function  ALUTF8XMLTextElementDecode(const Src: AnsiString): AnsiString;
begin
  result := ALXMLTextElementDecode(Src);
end;

{**********}
//deprecated
function  ALUTF8HTMLEncode(const Src: AnsiString;
                           const EncodeASCIIHtmlEntities: Boolean = True;
                           const useNumericReference: boolean = True): AnsiString;
begin
  result := ALHTMLEncode(Src, EncodeASCIIHtmlEntities, useNumericReference);
end;

{**********}
//deprecated
function  ALUTF8HTMLDecode(const Src: AnsiString): AnsiString;
begin
  result := ALHTMLDecode(Src);
end;

{**********}
//deprecated
procedure ALUTF8JavascriptDecodeV(Var Str: AnsiString);
begin
  ALJavascriptDecodeV(Str);
end;

{**********}
//deprecated
function  ALUTF8JavascriptDecode(const Src: AnsiString): AnsiString;
begin
  result := ALJavascriptDecode(Src);
end;

{$ENDIF !ALHideAnsiString}

Initialization

{$IFNDEF ALHideAnsiString}
  _ALHtmlEntities := TALStringList.create;
  TALStringList(_ALHtmlEntities).NameValueOptimization := False;
  ALInitHtmlEntities(_ALHtmlEntities);
  With (_ALHtmlEntities as TALStringList) do begin
    CaseSensitive := True;
    Duplicates := DupAccept;
    Sorted := True;
  end;
{$ENDIF}

Finalization
{$IFNDEF ALHideAnsiString}
  _ALHtmlEntities.Free;
{$ENDIF}

end.
