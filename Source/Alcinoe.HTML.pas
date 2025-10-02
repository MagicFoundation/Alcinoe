unit Alcinoe.HTML;

interface

{$I Alcinoe.inc}

uses
  System.Generics.Collections;

function  ALHTMLDecode(const Src: AnsiString): AnsiString; overload;
function  ALHTMLDecode(const Src: String): String; overload;
procedure ALHTMLDecodeInPlace(var Src: AnsiString); overload;
procedure ALHTMLDecodeInPlace(var Src: String); overload;
function  ALJavascriptEncode(const Src: AnsiString; const UseNumericReference: boolean = true): AnsiString; overload;
function  ALJavascriptEncode(const Src: String; const UseNumericReference: boolean = true): String; overload;
function  ALJavascriptDecode(const Src: AnsiString): AnsiString; overload;
function  ALJavascriptDecode(const Src: String): String; overload;
procedure ALJavascriptDecodeInPlace(Var Src: AnsiString); overload;
procedure ALJavascriptDecodeInPlace(Var Src: String); overload;
{$IF (defined(MSWINDOWS)) and (not defined(ALDPK))}
function  ALRunJavascript(const ACode: AnsiString): AnsiString;
{$ENDIF}

Var
  ALHtmlEntitiesByNameA: TDictionary<AnsiString, Cardinal>;
  ALHtmlEntitiesByCodeA: TDictionary<Cardinal, AnsiString>;
  ALHtmlEntitiesByNameW: TDictionary<String, Cardinal>;
  ALHtmlEntitiesByCodeW: TDictionary<Cardinal, String>;

implementation

uses
  System.Character,
  {$IF (defined(MSWINDOWS)) and (not defined(ALDPK))}
  System.Win.Comobj,
  Winapi.ActiveX,
  {$ENDIF}
  Alcinoe.Common,
  Alcinoe.StringUtils;

{***************************}
procedure ALInitHtmlEntities;
begin
  ALHtmlEntitiesByNameW.Add('zwnj', 8204); // zero width non-joiner,   U+200C NEW RFC 2070 -->
  ALHtmlEntitiesByNameW.Add('zwj', 8205); // zero width joiner, U+200D NEW RFC 2070 -->
  ALHtmlEntitiesByNameW.Add('zeta', 950); // greek small letter zeta, U+03B6 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Zeta', 918); // greek capital letter zeta, U+0396 -->
  ALHtmlEntitiesByNameW.Add('yuml', 255); // latin small letter y with diaeresis, U+00FF ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Yuml', 376); // latin capital letter Y with diaeresis,   U+0178 ISOlat2 -->
  ALHtmlEntitiesByNameW.Add('yen', 165); // yen sign = yuan sign, U+00A5 ISOnum -->
  ALHtmlEntitiesByNameW.Add('yacute', 253); // latin small letter y with acute, U+00FD ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Yacute', 221); // latin capital letter Y with acute, U+00DD ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('xi', 958); // greek small letter xi, U+03BE ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Xi', 926); // greek capital letter xi, U+039E ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('weierp', 8472); // script capital P = power set    = Weierstrass p, U+2118 ISOamso -->
  ALHtmlEntitiesByNameW.Add('uuml', 252); // latin small letter u with diaeresis, U+00FC ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Uuml', 220); // latin capital letter U with diaeresis, U+00DC ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('upsilon', 965); // greek small letter upsilon,   U+03C5 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Upsilon', 933); // greek capital letter upsilon,   U+03A5 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('upsih', 978); // greek upsilon with hook symbol,   U+03D2 NEW -->
  ALHtmlEntitiesByNameW.Add('uml', 168); // diaeresis = spacing diaeresis, U+00A8 ISOdia -->
  ALHtmlEntitiesByNameW.Add('ugrave', 249); // latin small letter u with grave, U+00F9 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Ugrave', 217); // latin capital letter U with grave, U+00D9 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('ucirc', 251); // latin small letter u with circumflex, U+00FB ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Ucirc', 219); // latin capital letter U with circumflex, U+00DB ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('uArr', 8657); // upwards double arrow, U+21D1 ISOamsa -->
  ALHtmlEntitiesByNameW.Add('uarr', 8593); // upwards arrow, U+2191 ISOnum-->
  ALHtmlEntitiesByNameW.Add('uacute', 250); // latin small letter u with acute, U+00FA ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Uacute', 218); // latin capital letter U with acute, U+00DA ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('trade', 8482); // trade mark sign, U+2122 ISOnum -->
  ALHtmlEntitiesByNameW.Add('times', 215); // multiplication sign, U+00D7 ISOnum -->
  ALHtmlEntitiesByNameW.Add('tilde', 732); // small tilde, U+02DC ISOdia -->
  ALHtmlEntitiesByNameW.Add('thorn', 254); // latin small letter thorn, U+00FE ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('THORN', 222); // latin capital letter THORN, U+00DE ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('thinsp', 8201); // thin space, U+2009 ISOpub -->
  ALHtmlEntitiesByNameW.Add('thetasym', 977); // greek small letter theta symbol,   U+03D1 NEW -->
  ALHtmlEntitiesByNameW.Add('theta', 952); // greek small letter theta,   U+03B8 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Theta', 920); // greek capital letter theta,   U+0398 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('there4', 8756); // therefore, U+2234 ISOtech -->
  ALHtmlEntitiesByNameW.Add('tau', 964); // greek small letter tau, U+03C4 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Tau', 932); // greek capital letter tau, U+03A4 -->
  ALHtmlEntitiesByNameW.Add('szlig', 223); // latin small letter sharp s = ess-zed, U+00DF ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('supe', 8839); // superset of or equal to,    U+2287 ISOtech -->
  ALHtmlEntitiesByNameW.Add('sup3', 179); // superscript three = superscript digit three = cubed, U+00B3 ISOnum -->
  ALHtmlEntitiesByNameW.Add('sup2', 178); // superscript two = superscript digit two = squared, U+00B2 ISOnum -->
  ALHtmlEntitiesByNameW.Add('sup1', 185); // superscript one = superscript digit one, U+00B9 ISOnum -->
  ALHtmlEntitiesByNameW.Add('sup', 8835); // superset of, U+2283 ISOtech -->
  ALHtmlEntitiesByNameW.Add('sum', 8721); // n-ary sumation, U+2211 ISOamsb -->
  ALHtmlEntitiesByNameW.Add('sube', 8838); // subset of or equal to, U+2286 ISOtech -->
  ALHtmlEntitiesByNameW.Add('sub', 8834); // subset of, U+2282 ISOtech -->
  ALHtmlEntitiesByNameW.Add('spades', 9824); // black spade suit, U+2660 ISOpub -->
  ALHtmlEntitiesByNameW.Add('sim', 8764); // tilde operator = varies with = similar to,    U+223C ISOtech -->
  ALHtmlEntitiesByNameW.Add('sigmaf', 962); // greek small letter final sigma,   U+03C2 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('sigma', 963); // greek small letter sigma,   U+03C3 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Sigma', 931); // greek capital letter sigma,   U+03A3 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('shy', 173); // soft hyphen = discretionary hyphen, U+00AD ISOnum -->
  ALHtmlEntitiesByNameW.Add('sect', 167); // section sign, U+00A7 ISOnum -->
  ALHtmlEntitiesByNameW.Add('sdot', 8901); // dot operator, U+22C5 ISOamsb -->
  ALHtmlEntitiesByNameW.Add('scaron', 353); // latin small letter s with caron,   U+0161 ISOlat2 -->
  ALHtmlEntitiesByNameW.Add('Scaron', 352); // latin capital letter S with caron,   U+0160 ISOlat2 -->
  ALHtmlEntitiesByNameW.Add('sbquo', 8218); // single low-9 quotation mark, U+201A NEW -->
  ALHtmlEntitiesByNameW.Add('rsquo', 8217); // right single quotation mark,   U+2019 ISOnum -->
  ALHtmlEntitiesByNameW.Add('rsaquo', 8250); // single right-pointing angle quotation mark,   U+203A ISO proposed -->
  ALHtmlEntitiesByNameW.Add('rlm', 8207); // right-to-left mark, U+200F NEW RFC 2070 -->
  ALHtmlEntitiesByNameW.Add('rho', 961); // greek small letter rho, U+03C1 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Rho', 929); // greek capital letter rho, U+03A1 -->
  ALHtmlEntitiesByNameW.Add('rfloor', 8971); // right floor, U+230B ISOamsc  -->
  ALHtmlEntitiesByNameW.Add('reg', 174); // registered sign = registered trade mark sign, U+00AE ISOnum -->
  ALHtmlEntitiesByNameW.Add('real', 8476); // blackletter capital R = real part symbol,    U+211C ISOamso -->
  ALHtmlEntitiesByNameW.Add('rdquo', 8221); // right double quotation mark,   U+201D ISOnum -->
  ALHtmlEntitiesByNameW.Add('rceil', 8969); // right ceiling, U+2309 ISOamsc  -->
  ALHtmlEntitiesByNameW.Add('rArr', 8658); // rightwards double arrow,    U+21D2 ISOtech -->
  ALHtmlEntitiesByNameW.Add('rarr', 8594); // rightwards arrow, U+2192 ISOnum -->
  ALHtmlEntitiesByNameW.Add('raquo', 187); // right-pointing double angle quotation mark = right pointing guillemet, U+00BB ISOnum -->
  ALHtmlEntitiesByNameW.Add('rang', 9002); // right-pointing angle bracket = ket,    U+232A ISOtech -->
  ALHtmlEntitiesByNameW.Add('radic', 8730); // square root = radical sign,    U+221A ISOtech -->
  ALHtmlEntitiesByNameW.Add('quot', 34); // quotation mark = APL quote,   U+0022 ISOnum -->
  ALHtmlEntitiesByNameW.Add('psi', 968); // greek small letter psi, U+03C8 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Psi', 936); // greek capital letter psi,   U+03A8 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('prop', 8733); // proportional to, U+221D ISOtech -->
  ALHtmlEntitiesByNameW.Add('prod', 8719); // n-ary product = product sign,    U+220F ISOamsb -->
  ALHtmlEntitiesByNameW.Add('Prime', 8243); // double prime = seconds = inches,    U+2033 ISOtech -->
  ALHtmlEntitiesByNameW.Add('prime', 8242); // prime = minutes = feet, U+2032 ISOtech -->
  ALHtmlEntitiesByNameW.Add('pound', 163); // pound sign, U+00A3 ISOnum -->
  ALHtmlEntitiesByNameW.Add('plusmn', 177); // plus-minus sign = plus-or-minus sign, U+00B1 ISOnum -->
  ALHtmlEntitiesByNameW.Add('piv', 982); // greek pi symbol, U+03D6 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('pi', 960); // greek small letter pi, U+03C0 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Pi', 928); // greek capital letter pi, U+03A0 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('phi', 966); // greek small letter phi, U+03C6 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Phi', 934); // greek capital letter phi,   U+03A6 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('perp', 8869); // up tack = orthogonal to = perpendicular,    U+22A5 ISOtech -->
  ALHtmlEntitiesByNameW.Add('permil', 8240); // per mille sign, U+2030 ISOtech -->
  ALHtmlEntitiesByNameW.Add('part', 8706); // partial differential, U+2202 ISOtech  -->
  ALHtmlEntitiesByNameW.Add('para', 182); // pilcrow sign = paragraph sign, U+00B6 ISOnum -->
  ALHtmlEntitiesByNameW.Add('ouml', 246); // latin small letter o with diaeresis, U+00F6 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Ouml', 214); // latin capital letter O with diaeresis, U+00D6 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('otimes', 8855); // circled times = vector product,    U+2297 ISOamsb -->
  ALHtmlEntitiesByNameW.Add('otilde', 245); // latin small letter o with tilde, U+00F5 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Otilde', 213); // latin capital letter O with tilde, U+00D5 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('oslash', 248); // latin small letter o with stroke, = latin small letter o slash, U+00F8 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Oslash', 216); // latin capital letter O with stroke = latin capital letter O slash, U+00D8 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('ordm', 186); // masculine ordinal indicator, U+00BA ISOnum -->
  ALHtmlEntitiesByNameW.Add('ordf', 170); // feminine ordinal indicator, U+00AA ISOnum -->
  ALHtmlEntitiesByNameW.Add('or', 8744); // logical or = vee, U+2228 ISOtech -->
  ALHtmlEntitiesByNameW.Add('oplus', 8853); // circled plus = direct sum,    U+2295 ISOamsb -->
  ALHtmlEntitiesByNameW.Add('omicron', 959); // greek small letter omicron, U+03BF NEW -->
  ALHtmlEntitiesByNameW.Add('Omicron', 927); // greek capital letter omicron, U+039F -->
  ALHtmlEntitiesByNameW.Add('omega', 969); // greek small letter omega,   U+03C9 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Omega', 937); // greek capital letter omega,   U+03A9 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('oline', 8254); // overline = spacing overscore,    U+203E NEW -->
  ALHtmlEntitiesByNameW.Add('ograve', 242); // latin small letter o with grave, U+00F2 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Ograve', 210); // latin capital letter O with grave, U+00D2 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('oelig', 339); // latin small ligature oe, U+0153 ISOlat2 -->
  ALHtmlEntitiesByNameW.Add('OElig', 338); // latin capital ligature OE,   U+0152 ISOlat2 -->
  ALHtmlEntitiesByNameW.Add('ocirc', 244); // latin small letter o with circumflex, U+00F4 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Ocirc', 212); // latin capital letter O with circumflex, U+00D4 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('oacute', 243); // latin small letter o with acute, U+00F3 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Oacute', 211); // latin capital letter O with acute, U+00D3 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('nu', 957); // greek small letter nu, U+03BD ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Nu', 925); // greek capital letter nu, U+039D -->
  ALHtmlEntitiesByNameW.Add('ntilde', 241); // latin small letter n with tilde, U+00F1 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Ntilde', 209); // latin capital letter N with tilde, U+00D1 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('nsub', 8836); // not a subset of, U+2284 ISOamsn -->
  ALHtmlEntitiesByNameW.Add('notin', 8713); // not an element of, U+2209 ISOtech -->
  ALHtmlEntitiesByNameW.Add('not', 172); // not sign, U+00AC ISOnum -->
  ALHtmlEntitiesByNameW.Add('ni', 8715); // contains as member, U+220B ISOtech -->
  ALHtmlEntitiesByNameW.Add('ne', 8800); // not equal to, U+2260 ISOtech -->
  ALHtmlEntitiesByNameW.Add('ndash', 8211); // en dash, U+2013 ISOpub -->
  ALHtmlEntitiesByNameW.Add('nbsp', 160); // no-break space = non-breaking space, U+00A0 ISOnum -->
  ALHtmlEntitiesByNameW.Add('nabla', 8711); // nabla = backward difference,    U+2207 ISOtech -->
  ALHtmlEntitiesByNameW.Add('mu', 956); // greek small letter mu, U+03BC ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Mu', 924); // greek capital letter mu, U+039C -->
  ALHtmlEntitiesByNameW.Add('minus', 8722); // minus sign, U+2212 ISOtech -->
  ALHtmlEntitiesByNameW.Add('middot', 183); // middle dot = Georgian comma = Greek middle dot, U+00B7 ISOnum -->
  ALHtmlEntitiesByNameW.Add('micro', 181); // micro sign, U+00B5 ISOnum -->
  ALHtmlEntitiesByNameW.Add('mdash', 8212); // em dash, U+2014 ISOpub -->
  ALHtmlEntitiesByNameW.Add('macr', 175); // macron = spacing macron = overline = APL overbar, U+00AF ISOdia -->
  ALHtmlEntitiesByNameW.Add('lt', 60); // less-than sign, U+003C ISOnum -->
  ALHtmlEntitiesByNameW.Add('lsquo', 8216); // left single quotation mark,   U+2018 ISOnum -->
  ALHtmlEntitiesByNameW.Add('lsaquo', 8249); // single left-pointing angle quotation mark,   U+2039 ISO proposed -->
  ALHtmlEntitiesByNameW.Add('lrm', 8206); // left-to-right mark, U+200E NEW RFC 2070 -->
  ALHtmlEntitiesByNameW.Add('loz', 9674); // lozenge, U+25CA ISOpub -->
  ALHtmlEntitiesByNameW.Add('lowast', 8727); // asterisk operator, U+2217 ISOtech -->
  ALHtmlEntitiesByNameW.Add('lfloor', 8970); // left floor = apl downstile,    U+230A ISOamsc  -->
  ALHtmlEntitiesByNameW.Add('le', 8804); // less-than or equal to, U+2264 ISOtech -->
  ALHtmlEntitiesByNameW.Add('ldquo', 8220); // left double quotation mark,   U+201C ISOnum -->
  ALHtmlEntitiesByNameW.Add('lceil', 8968); // left ceiling = apl upstile,    U+2308 ISOamsc  -->
  ALHtmlEntitiesByNameW.Add('lArr', 8656); // leftwards double arrow, U+21D0 ISOtech -->
  ALHtmlEntitiesByNameW.Add('larr', 8592); // leftwards arrow, U+2190 ISOnum -->
  ALHtmlEntitiesByNameW.Add('laquo', 171); // left-pointing double angle quotation mark = left pointing guillemet, U+00AB ISOnum -->
  ALHtmlEntitiesByNameW.Add('lang', 9001); // left-pointing angle bracket = bra,    U+2329 ISOtech -->
  ALHtmlEntitiesByNameW.Add('lambda', 955); // greek small letter lambda,   U+03BB ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Lambda', 923); // greek capital letter lambda,   U+039B ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('kappa', 954); // greek small letter kappa,   U+03BA ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Kappa', 922); // greek capital letter kappa, U+039A -->
  ALHtmlEntitiesByNameW.Add('iuml', 239); // latin small letter i with diaeresis, U+00EF ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Iuml', 207); // latin capital letter I with diaeresis, U+00CF ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('isin', 8712); // element of, U+2208 ISOtech -->
  ALHtmlEntitiesByNameW.Add('iquest', 191); // inverted question mark = turned question mark, U+00BF ISOnum -->
  ALHtmlEntitiesByNameW.Add('iota', 953); // greek small letter iota, U+03B9 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Iota', 921); // greek capital letter iota, U+0399 -->
  ALHtmlEntitiesByNameW.Add('int', 8747); // integral, U+222B ISOtech -->
  ALHtmlEntitiesByNameW.Add('infin', 8734); // infinity, U+221E ISOtech -->
  ALHtmlEntitiesByNameW.Add('image', 8465); // blackletter capital I = imaginary part,    U+2111 ISOamso -->
  ALHtmlEntitiesByNameW.Add('igrave', 236); // latin small letter i with grave, U+00EC ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Igrave', 204); // latin capital letter I with grave, U+00CC ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('iexcl', 161); // inverted exclamation mark, U+00A1 ISOnum -->
  ALHtmlEntitiesByNameW.Add('icirc', 238); // latin small letter i with circumflex, U+00EE ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Icirc', 206); // latin capital letter I with circumflex, U+00CE ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('iacute', 237); // latin small letter i with acute, U+00ED ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Iacute', 205); // latin capital letter I with acute, U+00CD ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('hellip', 8230); // horizontal ellipsis = three dot leader,    U+2026 ISOpub  -->
  ALHtmlEntitiesByNameW.Add('hearts', 9829); // black heart suit = valentine,    U+2665 ISOpub -->
  ALHtmlEntitiesByNameW.Add('hArr', 8660); // left right double arrow,    U+21D4 ISOamsa -->
  ALHtmlEntitiesByNameW.Add('harr', 8596); // left right arrow, U+2194 ISOamsa -->
  ALHtmlEntitiesByNameW.Add('gt', 62); // greater-than sign, U+003E ISOnum -->
  ALHtmlEntitiesByNameW.Add('ge', 8805); // greater-than or equal to,    U+2265 ISOtech -->
  ALHtmlEntitiesByNameW.Add('gamma', 947); // greek small letter gamma,   U+03B3 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Gamma', 915); // greek capital letter gamma,   U+0393 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('frasl', 8260); // fraction slash, U+2044 NEW -->
  ALHtmlEntitiesByNameW.Add('frac34', 190); // vulgar fraction three quarters = fraction three quarters, U+00BE ISOnum -->
  ALHtmlEntitiesByNameW.Add('frac14', 188); // vulgar fraction one quarter = fraction one quarter, U+00BC ISOnum -->
  ALHtmlEntitiesByNameW.Add('frac12', 189); // vulgar fraction one half = fraction one half, U+00BD ISOnum -->
  ALHtmlEntitiesByNameW.Add('forall', 8704); // for all, U+2200 ISOtech -->
  ALHtmlEntitiesByNameW.Add('fnof', 402); // latin small f with hook = function   = florin, U+0192 ISOtech -->
  ALHtmlEntitiesByNameW.Add('exist', 8707); // there exists, U+2203 ISOtech -->
  ALHtmlEntitiesByNameW.Add('euro', 8364); // euro sign, U+20AC NEW -->
  ALHtmlEntitiesByNameW.Add('euml', 235); // latin small letter e with diaeresis, U+00EB ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Euml', 203); // latin capital letter E with diaeresis, U+00CB ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('eth', 240); // latin small letter eth, U+00F0 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('ETH', 208); // latin capital letter ETH, U+00D0 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('eta', 951); // greek small letter eta, U+03B7 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Eta', 919); // greek capital letter eta, U+0397 -->
  ALHtmlEntitiesByNameW.Add('equiv', 8801); // identical to, U+2261 ISOtech -->
  ALHtmlEntitiesByNameW.Add('epsilon', 949); // greek small letter epsilon,   U+03B5 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Epsilon', 917); // greek capital letter epsilon, U+0395 -->
  ALHtmlEntitiesByNameW.Add('ensp', 8194); // en space, U+2002 ISOpub -->
  ALHtmlEntitiesByNameW.Add('emsp', 8195); // em space, U+2003 ISOpub -->
  ALHtmlEntitiesByNameW.Add('empty', 8709); // empty set = null set = diameter,    U+2205 ISOamso -->
  ALHtmlEntitiesByNameW.Add('egrave', 232); // latin small letter e with grave, U+00E8 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Egrave', 200); // latin capital letter E with grave, U+00C8 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('ecirc', 234); // latin small letter e with circumflex, U+00EA ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Ecirc', 202); // latin capital letter E with circumflex, U+00CA ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('eacute', 233); // latin small letter e with acute, U+00E9 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Eacute', 201); // latin capital letter E with acute, U+00C9 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('divide', 247); // division sign, U+00F7 ISOnum -->
  ALHtmlEntitiesByNameW.Add('diams', 9830); // black diamond suit, U+2666 ISOpub -->
  ALHtmlEntitiesByNameW.Add('delta', 948); // greek small letter delta,   U+03B4 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Delta', 916); // greek capital letter delta,   U+0394 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('deg', 176); // degree sign, U+00B0 ISOnum -->
  ALHtmlEntitiesByNameW.Add('dArr', 8659); // downwards double arrow, U+21D3 ISOamsa -->
  ALHtmlEntitiesByNameW.Add('darr', 8595); // downwards arrow, U+2193 ISOnum -->
  ALHtmlEntitiesByNameW.Add('Dagger', 8225); // double dagger, U+2021 ISOpub -->
  ALHtmlEntitiesByNameW.Add('dagger', 8224); // dagger, U+2020 ISOpub -->
  ALHtmlEntitiesByNameW.Add('curren', 164); // currency sign, U+00A4 ISOnum -->
  ALHtmlEntitiesByNameW.Add('cup', 8746); // union = cup, U+222A ISOtech -->
  ALHtmlEntitiesByNameW.Add('crarr', 8629); // downwards arrow with corner leftwards    = carriage return, U+21B5 NEW -->
  ALHtmlEntitiesByNameW.Add('copy', 169); // copyright sign, U+00A9 ISOnum -->
  ALHtmlEntitiesByNameW.Add('cong', 8773); // approximately equal to, U+2245 ISOtech -->
  ALHtmlEntitiesByNameW.Add('clubs', 9827); // black club suit = shamrock,    U+2663 ISOpub -->
  ALHtmlEntitiesByNameW.Add('circ', 710); // modifier letter circumflex accent,   U+02C6 ISOpub -->
  ALHtmlEntitiesByNameW.Add('chi', 967); // greek small letter chi, U+03C7 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Chi', 935); // greek capital letter chi, U+03A7 -->
  ALHtmlEntitiesByNameW.Add('cent', 162); // cent sign, U+00A2 ISOnum -->
  ALHtmlEntitiesByNameW.Add('cedil', 184); // cedilla = spacing cedilla, U+00B8 ISOdia -->
  ALHtmlEntitiesByNameW.Add('ccedil', 231); // latin small letter c with cedilla, U+00E7 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Ccedil', 199); // latin capital letter C with cedilla, U+00C7 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('cap', 8745); // intersection = cap, U+2229 ISOtech -->
  ALHtmlEntitiesByNameW.Add('bull', 8226); // bullet = black small circle,    U+2022 ISOpub  -->
  ALHtmlEntitiesByNameW.Add('brvbar', 166); // broken bar = broken vertical bar, U+00A6 ISOnum -->
  ALHtmlEntitiesByNameW.Add('beta', 946); // greek small letter beta, U+03B2 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Beta', 914); // greek capital letter beta, U+0392 -->
  ALHtmlEntitiesByNameW.Add('bdquo', 8222); // double low-9 quotation mark, U+201E NEW -->
  ALHtmlEntitiesByNameW.Add('auml', 228); // latin small letter a with diaeresis, U+00E4 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Auml', 196); // latin capital letter A with diaeresis, U+00C4 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('atilde', 227); // latin small letter a with tilde, U+00E3 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Atilde', 195); // latin capital letter A with tilde, U+00C3 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('asymp', 8776); // almost equal to = asymptotic to,    U+2248 ISOamsr -->
  ALHtmlEntitiesByNameW.Add('aring', 229); // latin small letter a with ring above = latin small letter a ring, U+00E5 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Aring', 197); // latin capital letter A with ring above = latin capital letter A ring, U+00C5 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('ang', 8736); // angle, U+2220 ISOamso -->
  ALHtmlEntitiesByNameW.Add('and', 8743); // logical and = wedge, U+2227 ISOtech -->
  ALHtmlEntitiesByNameW.Add('amp', 38); // ampersand, U+0026 ISOnum -->
  ALHtmlEntitiesByNameW.Add('alpha', 945); // greek small letter alpha,   U+03B1 ISOgrk3 -->
  ALHtmlEntitiesByNameW.Add('Alpha', 913); // greek capital letter alpha, U+0391 -->
  ALHtmlEntitiesByNameW.Add('alefsym', 8501); // alef symbol = first transfinite cardinal,    U+2135 NEW -->
  ALHtmlEntitiesByNameW.Add('agrave', 224); // latin small letter a with grave = latin small letter a grave, U+00E0 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Agrave', 192); // latin capital letter A with grave = latin capital letter A grave, U+00C0 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('aelig', 230); // latin small letter ae = latin small ligature ae, U+00E6 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('AElig', 198); // latin capital letter AE = latin capital ligature AE, U+00C6 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('acute', 180); // acute accent = spacing acute, U+00B4 ISOdia -->
  ALHtmlEntitiesByNameW.Add('acirc', 226); // latin small letter a with circumflex, U+00E2 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Acirc', 194); // latin capital letter A with circumflex, U+00C2 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('aacute', 225); // latin small letter a with acute, U+00E1 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('Aacute', 193); // latin capital letter A with acute, U+00C1 ISOlat1 -->
  ALHtmlEntitiesByNameW.Add('apos', 39); // apostrophe, U+0027

  var LArray := ALHtmlEntitiesByNameW.ToArray;
  for var I := Low(LArray) to High(LArray) do begin
    ALHtmlEntitiesByNameA.Add(AnsiString(LArray[i].Key), LArray[i].Value);
    ALHtmlEntitiesByCodeA.Add(LArray[i].Value, AnsiString(LArray[i].Key));
    ALHtmlEntitiesByCodeW.Add(LArray[i].Value, LArray[i].Key);
  end;
end;

{*******************************************************}
function ALHTMLDecode(const Src: AnsiString): AnsiString;
begin
  result := Src;
  ALHTMLDecodeInPlace(result);
end;

{***********************************************}
function ALHTMLDecode(const Src: String): String;
begin
  result := Src;
  ALHTMLDecodeInPlace(result);
end;

{*************************************************}
procedure ALHTMLDecodeInPlace(var Src: AnsiString);

var
  CurrPos: Integer;
  PResHead: PAnsiChar;
  PResTail: PAnsiChar;
  Chars: array[1..10] of AnsiChar;
  IsUniqueString: boolean;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _GenerateUniqueString;
    begin
      var Padding := PResTail - PResHead;
      UniqueString(Src);
      PResHead := PAnsiChar(Src);
      PResTail := PResHead + Padding;
      IsUniqueString := true;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    function _DecimalToInt(I: Cardinal; Ch: AnsiChar): Cardinal;
    begin
      Result := I * 10 + Ord(Ch) - Ord('0');
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    function _HexToInt(I: Cardinal; Ch: AnsiChar): Cardinal;
    begin
      case Ch of
        '0'..'9': Result := I * 16 + Ord(Ch) - Ord('0');
        'a'..'f': Result := I * 16 + Ord(Ch) - Ord('a') + 10;
        'A'..'F': Result := I * 16 + Ord(Ch) - Ord('A') + 10;
        // Should be unreachable because the caller pre-validates hex digits
        else raise EALException.Create('Wrong HEX-character found');
      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _CopyCurrPosCharToResult;
    begin
      if IsUniqueString then PResTail^ := Src[CurrPos];
      Inc(PResTail);
      Inc(CurrPos);
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _CopyUnicodeCharToResult(ACharInt: Cardinal; ANewCurrPos: Integer);
    begin
      if (ACharInt > 1114111{UnicodeLastChar}) or ((ACharInt >= UCS4Char(Char.MinHighSurrogate)) and (ACharInt <= UCS4Char(Char.MaxLowSurrogate))) then _CopyCurrPosCharToResult
      else begin
        if not IsUniqueString then _GenerateUniqueString;
        var LString := AnsiString(Char.ConvertFromUtf32(ACharInt));
        for var k := low(LString) to High(LString) do begin
          PResTail^ := LString[k];
          Inc(PResTail);
        end;
        CurrPos := ANewCurrPos;
      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _CopyHexadecimalEntityToResult(AEntityLength: Integer);
    begin
      var Res: Cardinal := 0;
      for var i := 3 to AEntityLength - 1 do  // 3 because Chars[1] = # and Chars[2] = x
        Res := _HexToInt(Res, Chars[i]);
      _CopyUnicodeCharToResult(Res, CurrPos + AEntityLength + 1); // ...&#x0af8;...
                                                                  //    ^CurrPos and AEntityLength=7
                                                                  // =>
                                                                  // ...&#x0af8;...
                                                                  //            ^CurrPos
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _CopyDecimalEntityToResult(AEntityLength: Integer);
    begin
      var Res: Cardinal := 0;
      for var i := 2 to AEntityLength - 1 do // 2 because Chars[1] = #
        Res := _DecimalToInt(Res, Chars[i]);
      _CopyUnicodeCharToResult(Res, CurrPos + AEntityLength + 1); // ...&#2345;...
                                                                  //    ^CurrPos and AEntityLength=6
                                                                  // =>
                                                                  // ...&#2345;...
                                                                  //           ^CurrPos
    end;

begin

  {Init var}
  CurrPos := low(Src);
  var Ln := High(Src);
  IsUniqueString := false;
  PResHead := PAnsiChar(Src);
  PResTail := PResHead;

  {Start loop}
  while CurrPos <= Ln do begin

    {XML-Entity detected}
    if Src[CurrPos] = '&' then begin

      {Construct chars array of the XML-entity}
      var j := CurrPos + 1;
      var i := 1;
      while (j <= Ln) and (Src[j] <> ';') and (i <= 10) do begin
        Chars[i] := Src[j];
        Inc(i);
        Inc(j);
      end;

      {If XML-entity is valid}
      if (j <= Ln) and (Src[j] = ';') then begin

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

            var l := j - CurrPos; {Length of entity}

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

            var l := j - CurrPos; {Length of entity}

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

          var p: PAnsiChar := @Chars[1];
          var LCharInt: Cardinal;
          if ALHtmlEntitiesByNameA.TryGetValue(AnsiString(p), LCharInt) then _CopyUnicodeCharToResult(LCharInt, j + 1)
          else _CopyCurrPosCharToResult;

        end;

      end
      else _CopyCurrPosCharToResult;

    end
    else _CopyCurrPosCharToResult;

  end;

  {Change the length the string only if some modifications was done.
   Else we don't need to do anything.}
  if PResTail - PResHead <> length(Src) then
    SetLength(Src, PResTail - PResHead);

end;

{**************************}
{$WARN WIDECHAR_REDUCED OFF}
procedure ALHTMLDecodeInPlace(var Src: String);

var
  CurrPos: Integer;
  PResHead: PChar;
  PResTail: PChar;
  Chars: array[1..10] of Char;
  IsUniqueString: boolean;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _GenerateUniqueString;
    begin
      var Padding := PResTail - PResHead;
      UniqueString(Src);
      PResHead := PChar(Src);
      PResTail := PResHead + Padding;
      IsUniqueString := true;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    function _DecimalToInt(I: Cardinal; Ch: Char): Cardinal;
    begin
      Result := I * 10 + Ord(Ch) - Ord('0');
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    function _HexToInt(I: Cardinal; Ch: Char): Cardinal;
    begin
      case Ch of
        '0'..'9': Result := I * 16 + Ord(Ch) - Ord('0');
        'a'..'f': Result := I * 16 + Ord(Ch) - Ord('a') + 10;
        'A'..'F': Result := I * 16 + Ord(Ch) - Ord('A') + 10;
        // Should be unreachable because the caller pre-validates hex digits
        else raise EALException.Create('Wrong HEX-character found');
      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _CopyCurrPosCharToResult;
    begin
      if IsUniqueString then PResTail^ := Src[CurrPos];
      Inc(PResTail);
      Inc(CurrPos);
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _CopyUnicodeCharToResult(ACharInt: Cardinal; ANewCurrPos: Integer);
    begin
      if (ACharInt > 1114111{UnicodeLastChar}) or ((ACharInt >= UCS4Char(Char.MinHighSurrogate)) and (ACharInt <= UCS4Char(Char.MaxLowSurrogate))) then _CopyCurrPosCharToResult
      else begin
        if not IsUniqueString then _GenerateUniqueString;
        var LString := Char.ConvertFromUtf32(ACharInt);
        for var k := low(LString) to High(LString) do begin
          PResTail^ := LString[k];
          Inc(PResTail);
        end;
        CurrPos := ANewCurrPos;
      end;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _CopyHexadecimalEntityToResult(AEntityLength: Integer);
    begin
      var Res: Cardinal := 0;
      for var i := 3 to AEntityLength - 1 do  // 3 because Chars[1] = # and Chars[2] = x
        Res := _HexToInt(Res, Chars[i]);
      _CopyUnicodeCharToResult(Res, CurrPos + AEntityLength + 1); // ...&#x0af8;...
                                                                  //    ^CurrPos and AEntityLength=7
                                                                  // =>
                                                                  // ...&#x0af8;...
                                                                  //            ^CurrPos
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _CopyDecimalEntityToResult(AEntityLength: Integer);
    begin
      var Res: Cardinal := 0;
      for var i := 2 to AEntityLength - 1 do // 2 because Chars[1] = #
        Res := _DecimalToInt(Res, Chars[i]);
      _CopyUnicodeCharToResult(Res, CurrPos + AEntityLength + 1); // ...&#2345;...
                                                                  //    ^CurrPos and AEntityLength=6
                                                                  // =>
                                                                  // ...&#2345;...
                                                                  //           ^CurrPos
    end;

begin

  {Init var}
  CurrPos := low(Src);
  var Ln := High(Src);
  IsUniqueString := false;
  PResHead := PChar(Src);
  PResTail := PResHead;

  {Start loop}
  while CurrPos <= Ln do begin

    {XML-Entity detected}
    if Src[CurrPos] = '&' then begin

      {Construct chars array of the XML-entity}
      var j := CurrPos + 1;
      var i := 1;
      while (j <= Ln) and (Src[j] <> ';') and (i <= 10) do begin
        Chars[i] := Src[j];
        Inc(i);
        Inc(j);
      end;

      {If XML-entity is valid}
      if (j <= Ln) and (Src[j] = ';') then begin

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

            var l := j - CurrPos; {Length of entity}

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

            var l := j - CurrPos; {Length of entity}

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

          var p: PChar := @Chars[1];
          var LCharInt: Cardinal;
          if ALHtmlEntitiesByNameW.TryGetValue(String(p), LCharInt) then _CopyUnicodeCharToResult(LCharInt, j + 1)
          else _CopyCurrPosCharToResult;

        end;

      end
      else _CopyCurrPosCharToResult;

    end
    else _CopyCurrPosCharToResult;

  end;

  {Change the length the string only if some modifications was done.
   Else we don't need to do anything.}
  if PResTail - PResHead <> length(Src) then
    SetLength(Src, PResTail - PResHead);

end;
{$WARN WIDECHAR_REDUCED ON}

{******************************************************************************************}
// https://developer.mozilla.org/en-US/docs/JavaScript/Guide/Values,_variables,_and_literals
function ALJavascriptEncode(const Src: AnsiString; const UseNumericReference: boolean = True): AnsiString;

var
  Sp, Rp: PAnsiChar;
  IsUniqueString: Boolean;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _GenerateUniqueString;
    begin
      SetLength(Result, Length(Src) * 6);
      Rp := PAnsiChar(Result);
      var Start := PAnsiChar(Src);
      var Prefix := Sp - Start;
      if Prefix > 0 then begin
        ALStrMove(Start, Rp, Prefix);
        Inc(Rp, Prefix);
      end;
      IsUniqueString := true;
    end;

begin
  IsUniqueString := False;
  Sp := PAnsiChar(Src);
  for var I := 1 to Length(Src) do begin
    case Sp^ of
      #8: begin // Backspace
            if not IsUniqueString then _GenerateUniqueString;
            if UseNumericReference then begin
              ALStrMove('\u0008', Rp, 6);
              Inc(Rp, 6);
            end
            else begin
              ALStrMove('\b', Rp, 2);
              Inc(Rp, 2);
            end;
          end;
      #9: begin // Tab
            if not IsUniqueString then _GenerateUniqueString;
            if UseNumericReference then begin
              ALStrMove('\u0009', Rp, 6);
              Inc(Rp, 6);
            end
            else begin
              ALStrMove('\t', Rp, 2);
              Inc(Rp, 2);
            end;
          end;
      #10: begin // New line
            if not IsUniqueString then _GenerateUniqueString;
             if UseNumericReference then begin
               ALStrMove('\u000A', Rp, 6);
               Inc(Rp, 6);
             end
             else begin
               ALStrMove('\n', Rp, 2);
               Inc(Rp, 2);
             end;
           end;
      #11: begin // Vertical tab
            if not IsUniqueString then _GenerateUniqueString;
             if UseNumericReference then begin
               ALStrMove('\u000B', Rp, 6);
               Inc(Rp, 6);
             end
             else begin
               ALStrMove('\v', Rp, 2);
               Inc(Rp, 2);
             end;
           end;
      #12: begin // Form feed
            if not IsUniqueString then _GenerateUniqueString;
             if UseNumericReference then begin
               ALStrMove('\u000C', Rp, 6);
               Inc(Rp, 6);
             end
             else begin
               ALStrMove('\f', Rp, 2);
               Inc(Rp, 2);
             end;
           end;
      #13: begin // Carriage return
            if not IsUniqueString then _GenerateUniqueString;
             if UseNumericReference then begin
               ALStrMove('\u000D', Rp, 6);
               Inc(Rp, 6);
             end
             else begin
               ALStrMove('\r', Rp, 2);
               Inc(Rp, 2);
             end;
           end;
      #34: begin // Double quote
            if not IsUniqueString then _GenerateUniqueString;
             if UseNumericReference then begin
               ALStrMove('\u0022', Rp, 6);
               Inc(Rp, 6);
             end
             else begin
               ALStrMove('\"', Rp, 2);
               Inc(Rp, 2);
             end;
           end;
      #38: begin // & ... we need to encode it because in javascript &#39; or &amp; will be converted to ' and error unterminated string
             if not IsUniqueString then _GenerateUniqueString;
             ALStrMove('\u0026', Rp, 6);
             Inc(Rp, 6);
           end;
      #39: begin // Apostrophe or single quote
             if not IsUniqueString then _GenerateUniqueString;
             if UseNumericReference then begin
               ALStrMove('\u0027', Rp, 6);
               Inc(Rp, 6);
             end
             else begin
               ALStrMove('\''', Rp, 2);
               Inc(Rp, 2);
             end;
           end;
      #60: begin // < ... mostly to hide all </script> tag inside javascript.
                 // http://www.wwco.com/~wls/blog/2007/04/25/using-script-in-a-javascript-literal/
             if not IsUniqueString then _GenerateUniqueString;
             ALStrMove('\u003C', Rp, 6);
             Inc(Rp, 6);
           end;
      #62: begin // > ... mostly to hide all HTML tag inside javascript.
             if not IsUniqueString then _GenerateUniqueString;
             ALStrMove('\u003E', Rp, 6);
             Inc(Rp, 6);
           end;
      #92: begin // Backslash character (\).
             if not IsUniqueString then _GenerateUniqueString;
             if UseNumericReference then begin
               ALStrMove('\u005C', Rp, 6);
               Inc(Rp, 6);
             end
             else begin
               ALStrMove('\\', Rp, 2);
               Inc(Rp, 2);
             end;
           end;
      else begin
        if IsUniqueString then begin
          Rp^ := Sp^;
          Inc(Rp);
        end;
      end;
    end;
    Inc(Sp);
  end;
  if IsUniqueString then
    SetLength(Result, Rp - PAnsiChar(Result))
  else
    Result := Src;
end;

{******************************************************************************************}
// https://developer.mozilla.org/en-US/docs/JavaScript/Guide/Values,_variables,_and_literals
function ALJavascriptEncode(const Src: String; const UseNumericReference: boolean = true): String;

var
  Sp, Rp: PChar;
  IsUniqueString: Boolean;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _GenerateUniqueString;
    begin
      SetLength(Result, Length(Src) * 6);
      Rp := PChar(Result);
      var Start := PChar(Src);
      var Prefix := Sp - Start;
      if Prefix > 0 then begin
        ALStrMove(Start, Rp, Prefix);
        Inc(Rp, Prefix);
      end;
      IsUniqueString := true;
    end;

begin
  IsUniqueString := False;
  Sp := PChar(Src);
  for var I := 1 to Length(Src) do begin
    case Sp^ of
      #8: begin // Backspace
            if not IsUniqueString then _GenerateUniqueString;
            if UseNumericReference then begin
              ALStrMove('\u0008', Rp, 6);
              Inc(Rp, 6);
            end
            else begin
              ALStrMove('\b', Rp, 2);
              Inc(Rp, 2);
            end;
          end;
      #9: begin // Tab
            if not IsUniqueString then _GenerateUniqueString;
            if UseNumericReference then begin
              ALStrMove('\u0009', Rp, 6);
              Inc(Rp, 6);
            end
            else begin
              ALStrMove('\t', Rp, 2);
              Inc(Rp, 2);
            end;
          end;
      #10: begin // New line
            if not IsUniqueString then _GenerateUniqueString;
             if UseNumericReference then begin
               ALStrMove('\u000A', Rp, 6);
               Inc(Rp, 6);
             end
             else begin
               ALStrMove('\n', Rp, 2);
               Inc(Rp, 2);
             end;
           end;
      #11: begin // Vertical tab
            if not IsUniqueString then _GenerateUniqueString;
             if UseNumericReference then begin
               ALStrMove('\u000B', Rp, 6);
               Inc(Rp, 6);
             end
             else begin
               ALStrMove('\v', Rp, 2);
               Inc(Rp, 2);
             end;
           end;
      #12: begin // Form feed
            if not IsUniqueString then _GenerateUniqueString;
             if UseNumericReference then begin
               ALStrMove('\u000C', Rp, 6);
               Inc(Rp, 6);
             end
             else begin
               ALStrMove('\f', Rp, 2);
               Inc(Rp, 2);
             end;
           end;
      #13: begin // Carriage return
            if not IsUniqueString then _GenerateUniqueString;
             if UseNumericReference then begin
               ALStrMove('\u000D', Rp, 6);
               Inc(Rp, 6);
             end
             else begin
               ALStrMove('\r', Rp, 2);
               Inc(Rp, 2);
             end;
           end;
      #34: begin // Double quote
            if not IsUniqueString then _GenerateUniqueString;
             if UseNumericReference then begin
               ALStrMove('\u0022', Rp, 6);
               Inc(Rp, 6);
             end
             else begin
               ALStrMove('\"', Rp, 2);
               Inc(Rp, 2);
             end;
           end;
      #38: begin // & ... we need to encode it because in javascript &#39; or &amp; will be converted to ' and error unterminated string
             if not IsUniqueString then _GenerateUniqueString;
             ALStrMove('\u0026', Rp, 6);
             Inc(Rp, 6);
           end;
      #39: begin // Apostrophe or single quote
             if not IsUniqueString then _GenerateUniqueString;
             if UseNumericReference then begin
               ALStrMove('\u0027', Rp, 6);
               Inc(Rp, 6);
             end
             else begin
               ALStrMove('\''', Rp, 2);
               Inc(Rp, 2);
             end;
           end;
      #60: begin // < ... mostly to hide all </script> tag inside javascript.
                 // http://www.wwco.com/~wls/blog/2007/04/25/using-script-in-a-javascript-literal/
             if not IsUniqueString then _GenerateUniqueString;
             ALStrMove('\u003C', Rp, 6);
             Inc(Rp, 6);
           end;
      #62: begin // > ... mostly to hide all HTML tag inside javascript.
             if not IsUniqueString then _GenerateUniqueString;
             ALStrMove('\u003E', Rp, 6);
             Inc(Rp, 6);
           end;
      #92: begin // Backslash character (\).
             if not IsUniqueString then _GenerateUniqueString;
             if UseNumericReference then begin
               ALStrMove('\u005C', Rp, 6);
               Inc(Rp, 6);
             end
             else begin
               ALStrMove('\\', Rp, 2);
               Inc(Rp, 2);
             end;
           end;
      else begin
        if IsUniqueString then begin
          Rp^ := Sp^;
          Inc(Rp);
        end;
      end;
    end;
    Inc(Sp);
  end;
  if IsUniqueString then
    SetLength(Result, Rp - PChar(Result))
  else
    Result := Src;
end;

{*************************************************************}
function ALJavascriptDecode(const Src: AnsiString): AnsiString;
begin
  result := Src;
  ALJavascriptDecodeInPlace(result);
end;

{*****************************************************}
function ALJavascriptDecode(const Src: String): String;
begin
  result := Src;
  ALJavascriptDecodeInPlace(result);
end;

{*******************************************************}
procedure ALJavascriptDecodeInPlace(Var Src: AnsiString);

var
  CurrPos : Integer;
  PResHead: PAnsiChar;
  PResTail: PAnsiChar;
  Ch1, Ch2, Ch3, Ch4, Ch5: AnsiChar;
  IsUniqueString: boolean;
  Ln: Integer;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _GenerateUniqueString;
  begin
    var Padding := PResTail - PResHead;
    UniqueString(Src);
    PResHead := PAnsiChar(Src);
    PResTail := PResHead + Padding;
    IsUniqueString := true;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _OctToInt(I: Cardinal; Ch: AnsiChar): Cardinal;
  begin
    Result := I * 8 + Ord(Ch) - Ord('0');
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _HexToInt(I: Cardinal; Ch: AnsiChar): Cardinal;
  begin
    case Ch of
      '0'..'9': Result := I * 16 + Ord(Ch) - Ord('0');
      'a'..'f': Result := I * 16 + Ord(Ch) - Ord('a') + 10;
      'A'..'F': Result := I * 16 + Ord(Ch) - Ord('A') + 10;
      // Should be unreachable because the caller pre-validates hex digits
      else raise EALException.Create('Wrong HEX-character found');
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyCurrPosCharToResult;
  begin
    if IsUniqueString then PResTail^ := Src[CurrPos];
    Inc(PResTail);
    Inc(CurrPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyAnsiCharToResult(ACharInt: Byte; ANewCurrPos: Integer);
  begin
    if not IsUniqueString then _GenerateUniqueString;
    PResTail^ := AnsiChar(ACharInt);
    Inc(PResTail);
    CurrPos := ANewCurrPos;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyUnicodeCharToResult(ACharInt: Cardinal; ANewCurrPos: Integer); overload;
  begin
    if (ACharInt > 1114111{UnicodeLastChar}) or ((ACharInt >= UCS4Char(Char.MinHighSurrogate)) and (ACharInt <= UCS4Char(Char.MaxLowSurrogate))) then _CopyAnsiCharToResult(Ord(ch1), CurrPos + 2) // delete the \
    else begin
      if not IsUniqueString then _GenerateUniqueString;
      var LString := AnsiString(Char.ConvertFromUtf32(ACharInt));
      for var k := low(LString) to High(LString) do begin
        PResTail^ := LString[k];
        Inc(PResTail);
      end;
      CurrPos := ANewCurrPos;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyUnicodeCharToResult(AHighSurrogateInt, ALowSurrogateInt: Cardinal; ANewCurrPos: Integer); overload;
  begin
    if not IsUniqueString then _GenerateUniqueString;
    // AHighSurrogateInt and ALowSurrogateInt have already been validated
    var LString := AnsiString(Char.ConvertFromUtf32(Char.ConvertToUtf32(char(AHighSurrogateInt), char(ALowSurrogateInt))));
    for var k := low(LString) to High(LString) do begin
      PResTail^ := LString[k];
      Inc(PResTail);
    end;
    CurrPos := ANewCurrPos;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyUnicodeCharToResult; overload;
  begin
    var I := _HexToInt(0, ch2);
    I := _HexToInt(I, ch3);
    I := _HexToInt(I, ch4);
    I := _HexToInt(I, ch5);
    // Special case I is a high surrogate.
    if (I >= UCS4Char(Char.MinHighSurrogate)) and (I <= UCS4Char(Char.MaxHighSurrogate)) and
       (CurrPos + 6 <= Ln - 5) and
       (Src[CurrPos + 6]='\') and
       (Src[CurrPos + 7]='u') then begin
      var Ch2Bis := Src[CurrPos + 8];
      var Ch3Bis := Src[CurrPos + 9];
      var Ch4Bis := Src[CurrPos + 10];
      var Ch5Bis := Src[CurrPos + 11];
      if (ch2Bis in ['A'..'F', 'a'..'f', '0'..'9']) and
         (ch3Bis in ['A'..'F', 'a'..'f', '0'..'9']) and
         (ch4Bis in ['A'..'F', 'a'..'f', '0'..'9']) and
         (ch5Bis in ['A'..'F', 'a'..'f', '0'..'9']) then begin
        var J := _HexToInt(0, ch2Bis);
        J := _HexToInt(J, ch3Bis);
        J := _HexToInt(J, ch4Bis);
        J := _HexToInt(J, ch5Bis);
        // Verify that the low surrogate is valid.
        if (J >= UCS4Char(Char.MinLowSurrogate)) and (J <= UCS4Char(Char.MaxLowSurrogate)) then begin
          _CopyUnicodeCharToResult(I, j, CurrPos+12);
          exit;
        end;
      end;
    end;
    _CopyUnicodeCharToResult(I, CurrPos+6);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyIso88591CharToResult(ACharInt: byte; ANewCurrPos: Integer);
  begin
    var LChar: WideChar;
    if UnicodeFromLocaleChars(
         28591, //CodePage,
         0, // Flags
         @ACharInt,// LocaleStr
         1, // LocaleStrLen
         @LChar, // UnicodeStr
         1) <> 1 then begin // UnicodeStrLen
      _CopyAnsiCharToResult(Ord(ch1), CurrPos + 2); // delete the \
      exit;
    end;
    if not IsUniqueString then _GenerateUniqueString;
    var LString := AnsiString(LChar);
    for var k := low(LString) to High(LString) do begin
      PResTail^ := LString[k];
      Inc(PResTail);
    end;
    CurrPos := ANewCurrPos;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyHexIso88591CharToResult;
  begin
    var I := _HexToInt(0, ch2);
    I := _HexToInt(I, ch3);
    _CopyIso88591CharToResult(I, CurrPos+4);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyOctIso88591CharToResult;
  begin
    var I := _OctToInt(0, ch1);
    I := _OctToInt(I, ch2);
    I := _OctToInt(I, ch3);
    if I in [0..255] then _CopyIso88591CharToResult(I, CurrPos+4)
    else _CopyAnsiCharToResult(Ord(ch1), CurrPos + 2); // delete the \
  end;

begin

  {Init var}
  CurrPos := low(Src);
  Ln := High(Src);
  IsUniqueString := false;
  PResHead := PAnsiChar(Src);
  PResTail := PResHead;

  {Start loop}
  while CurrPos <= Ln do begin

    {Escape char detected}
    If Src[CurrPos]='\' then begin

      if (CurrPos <= Ln - 5) then begin
        Ch1 := Src[CurrPos + 1];
        Ch2 := Src[CurrPos + 2];
        Ch3 := Src[CurrPos + 3];
        Ch4 := Src[CurrPos + 4];
        Ch5 := Src[CurrPos + 5];
      end
      else if (CurrPos <= Ln - 3) then begin
        Ch1 := Src[CurrPos + 1];
        Ch2 := Src[CurrPos + 2];
        Ch3 := Src[CurrPos + 3];
        Ch4 := #0;
        Ch5 := #0;
      end
      else if (CurrPos <= Ln - 1) then begin
        Ch1 := Src[CurrPos + 1];
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

      // delete the \ at the edge
      else break;

    end
    else _CopyCurrPosCharToResult;

  end;

  {Change the length the string only if some modifications was done.
   Else we don't need to do anything.}
  if PResTail-PResHead <> length(Src) then
    SetLength(Src,PResTail-PResHead);

end;

{**************************}
{$WARN WIDECHAR_REDUCED OFF}
procedure ALJavascriptDecodeInPlace(Var Src: String);

var
  CurrPos : Integer;
  PResHead: PChar;
  PResTail: PChar;
  Ch1, Ch2, Ch3, Ch4, Ch5: Char;
  IsUniqueString: boolean;
  Ln: Integer;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _GenerateUniqueString;
  begin
    var Padding := PResTail - PResHead;
    UniqueString(Src);
    PResHead := PChar(Src);
    PResTail := PResHead + Padding;
    IsUniqueString := true;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _OctToInt(I: Cardinal; Ch: Char): Cardinal;
  begin
    Result := I * 8 + Ord(Ch) - Ord('0');
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _HexToInt(I: Cardinal; Ch: Char): Cardinal;
  begin
    case Ch of
      '0'..'9': Result := I * 16 + Ord(Ch) - Ord('0');
      'a'..'f': Result := I * 16 + Ord(Ch) - Ord('a') + 10;
      'A'..'F': Result := I * 16 + Ord(Ch) - Ord('A') + 10;
      // Should be unreachable because the caller pre-validates hex digits
      else raise EALException.Create('Wrong HEX-character found');
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyCurrPosCharToResult;
  begin
    if IsUniqueString then PResTail^ := Src[CurrPos];
    Inc(PResTail);
    Inc(CurrPos);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyCharToResult(ACharInt: Cardinal; ANewCurrPos: Integer);
  begin
    if not IsUniqueString then _GenerateUniqueString;
    PResTail^ := Char(ACharInt);
    Inc(PResTail);
    CurrPos := ANewCurrPos;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyUnicodeCharToResult(ACharInt: Cardinal; ANewCurrPos: Integer); overload;
  begin
    if (ACharInt > 1114111{UnicodeLastChar}) or ((ACharInt >= UCS4Char(Char.MinHighSurrogate)) and (ACharInt <= UCS4Char(Char.MaxLowSurrogate))) then _CopyCharToResult(Ord(ch1), CurrPos + 2) // delete the \
    else begin
      if not IsUniqueString then _GenerateUniqueString;
      var LString := Char.ConvertFromUtf32(ACharInt);
      for var k := low(LString) to High(LString) do begin
        PResTail^ := LString[k];
        Inc(PResTail);
      end;
      CurrPos := ANewCurrPos;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyUnicodeCharToResult(AHighSurrogateInt, ALowSurrogateInt: Cardinal; ANewCurrPos: Integer); overload;
  begin
    if not IsUniqueString then _GenerateUniqueString;
    // AHighSurrogateInt and ALowSurrogateInt have already been validated
    var LString := Char.ConvertFromUtf32(Char.ConvertToUtf32(char(AHighSurrogateInt), char(ALowSurrogateInt)));
    for var k := low(LString) to High(LString) do begin
      PResTail^ := LString[k];
      Inc(PResTail);
    end;
    CurrPos := ANewCurrPos;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyUnicodeCharToResult; overload;
  begin
    var I := _HexToInt(0, ch2);
    I := _HexToInt(I, ch3);
    I := _HexToInt(I, ch4);
    I := _HexToInt(I, ch5);
    // Special case I is a high surrogate.
    if (I >= UCS4Char(Char.MinHighSurrogate)) and (I <= UCS4Char(Char.MaxHighSurrogate)) and
       (CurrPos + 6 <= Ln - 5) and
       (Src[CurrPos + 6]='\') and
       (Src[CurrPos + 7]='u') then begin
      var Ch2Bis := Src[CurrPos + 8];
      var Ch3Bis := Src[CurrPos + 9];
      var Ch4Bis := Src[CurrPos + 10];
      var Ch5Bis := Src[CurrPos + 11];
      if (ch2Bis in ['A'..'F', 'a'..'f', '0'..'9']) and
         (ch3Bis in ['A'..'F', 'a'..'f', '0'..'9']) and
         (ch4Bis in ['A'..'F', 'a'..'f', '0'..'9']) and
         (ch5Bis in ['A'..'F', 'a'..'f', '0'..'9']) then begin
        var J := _HexToInt(0, ch2Bis);
        J := _HexToInt(J, ch3Bis);
        J := _HexToInt(J, ch4Bis);
        J := _HexToInt(J, ch5Bis);
        // Verify that the low surrogate is valid.
        if (J >= UCS4Char(Char.MinLowSurrogate)) and (J <= UCS4Char(Char.MaxLowSurrogate)) then begin
          _CopyUnicodeCharToResult(I, j, CurrPos+12);
          exit;
        end;
      end;
    end;
    _CopyUnicodeCharToResult(I, CurrPos+6);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyIso88591CharToResult(ACharInt: byte; ANewCurrPos: Integer);
  begin
    var LChar: WideChar;
    if UnicodeFromLocaleChars(
         28591, //CodePage,
         0, // Flags
         @ACharInt,// LocaleStr
         1, // LocaleStrLen
         @LChar, // UnicodeStr
         1) <> 1 then begin // UnicodeStrLen
      _CopyCharToResult(Ord(ch1), CurrPos + 2); // delete the \
      exit;
    end;
    if not IsUniqueString then _GenerateUniqueString;
    PResTail^ := LChar;
    Inc(PResTail);
    CurrPos := ANewCurrPos;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyHexIso88591CharToResult;
  begin
    var I := _HexToInt(0, ch2);
    I := _HexToInt(I, ch3);
    _CopyIso88591CharToResult(I, CurrPos+4);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _CopyOctIso88591CharToResult;
  begin
    var I := _OctToInt(0, ch1);
    I := _OctToInt(I, ch2);
    I := _OctToInt(I, ch3);
    if I in [0..255] then _CopyIso88591CharToResult(I, CurrPos+4)
    else _CopyCharToResult(Ord(ch1), CurrPos + 2); // delete the \
  end;

begin

  {Init var}
  CurrPos := low(Src);
  Ln := High(Src);
  IsUniqueString := false;
  PResHead := PChar(Src);
  PResTail := PResHead;

  {Start loop}
  while CurrPos <= Ln do begin

    {Escape char detected}
    If Src[CurrPos]='\' then begin

      if (CurrPos <= Ln - 5) then begin
        Ch1 := Src[CurrPos + 1];
        Ch2 := Src[CurrPos + 2];
        Ch3 := Src[CurrPos + 3];
        Ch4 := Src[CurrPos + 4];
        Ch5 := Src[CurrPos + 5];
      end
      else if (CurrPos <= Ln - 3) then begin
        Ch1 := Src[CurrPos + 1];
        Ch2 := Src[CurrPos + 2];
        Ch3 := Src[CurrPos + 3];
        Ch4 := #0;
        Ch5 := #0;
      end
      else if (CurrPos <= Ln - 1) then begin
        Ch1 := Src[CurrPos + 1];
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

      // delete the \ at the edge
      else break;

    end
    else _CopyCurrPosCharToResult;

  end;

  {Change the length the string only if some modifications was done.
   Else we don't need to do anything.}
  if PResTail-PResHead <> length(Src) then
    SetLength(Src,PResTail-PResHead);

end;
{$WARN WIDECHAR_REDUCED ON}

{*************************************************}
{$IF (defined(MSWINDOWS)) and (not defined(ALDPK))}
{This function evaluates the Javascript code given in the
 parameter "ACode" and returns result. The function works
 similar to browser's console, so you can send even the code
 like this "2+2" => returns "4".}
function ALRunJavascript(const ACode: AnsiString): AnsiString;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // see: http://stackoverflow.com/questions/2653797/why-does-couninitialize-cause-an-error-on-exit
  // we create COM-object with CreateOleObject here to make that its creation is handled inside of
  // THIS scope (function MakeExecution) and its destroying is handled inside of this function too
  // on the last "end;" of this function.
  function _MakeExecution(const ACode: AnsiString): AnsiString;
  var LJavaScript: OleVariant;
  begin
    LJavaScript          := CreateOleObject('ScriptControl');
    LJavaScript.Language := 'JavaScript';
    result               := AnsiString(LJavaScript.Eval(String(ACode)));
  end;

begin
  // we create here the COM-server that will be actually destroyed
  // on calling of CoUninitialize. What it will do on destroy it depends
  // on the operation system.
  //              |
  //              V
  var HandleResult: HResult := CoInitializeEx(nil, COINIT_MULTITHREADED);
  if HandleResult <> S_OK then raise EALException.Create('ALRunJavascript: cannot initialize OLE-object');
  try
    result := _MakeExecution(ACode);
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

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.HTML','initialization');
  {$ENDIF}
  ALHtmlEntitiesByNameA := TDictionary<AnsiString, Cardinal>.Create;
  ALHtmlEntitiesByCodeA := TDictionary<Cardinal, AnsiString>.create;
  ALHtmlEntitiesByNameW := TDictionary<String, Cardinal>.Create;
  ALHtmlEntitiesByCodeW := TDictionary<Cardinal, String>.create;
  ALInitHtmlEntities;

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.HTML','finalization');
  {$ENDIF}
  ALFreeAndNil(ALHtmlEntitiesByNameA);
  ALFreeAndNil(ALHtmlEntitiesByCodeA);
  ALFreeAndNil(ALHtmlEntitiesByNameW);
  ALFreeAndNil(ALHtmlEntitiesByCodeW);

end.