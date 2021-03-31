unit ALMime;

interface

uses
  System.Types,
  ALStringList;

//From indy
Function  ALGetDefaultFileExtFromMimeContentType(aContentType: AnsiString): AnsiString;
Function  ALGetDefaultMIMEContentTypeFromExt(const aExt: AnsiString): AnsiString;

Var
  AlMimeContentTypeByExtList: TALStrings; {.htm=text/html}
  AlExtbyMimeContentTypeList: TALStrings; {text/html=.htm}

implementation

uses
  ALString,
  AlCommon;

{************************************************************************************}
Function ALGetDefaultFileExtFromMimeContentType(aContentType: AnsiString): AnsiString;
Var P: integer;
    Index : Integer;
Begin
  Result := '';

  aContentType := ALLowerCase(aContentType);
  P := AlPosEx(';',aContentType);
  if (P > 0) then delete(aContentType,P,MaxInt);
  aContentType := ALTrim(AContentType);

  Index := AlExtbyMimeContentTypeList.IndexOfName(aContentType);
  if Index <> -1 then Result := AlExtbyMimeContentTypeList.ValueFromIndex[Index];
end;

{******************************************************************************}
Function ALGetDefaultMIMEContentTypeFromExt(const aExt: AnsiString): AnsiString;
var Index : Integer;
    LExt: AnsiString;
begin
  LExt := AlLowerCase(aExt);
  If (LExt = '') or (LExt[low(AnsiString)] <> '.') then LExt := '.' + LExt;
  Index := AlMimeContentTypeByExtList.IndexOfName(LExt);
  if Index <> -1 then Result := AlMimeContentTypeByExtList.ValueFromIndex[Index]
  else Result := 'application/octet-stream';
end;

{************************}
procedure ALFillMimeTable;
var I: integer;
begin
  {NOTE:  All of these strings should never be translated
  because they are protocol specific and are important for some
  web-browsers}

  { Animation }
  AlMimeContentTypeByExtList.Add('.nml=animation/narrative');    {Do not Localize}

  { Audio }
  AlMimeContentTypeByExtList.Add('.aac=audio/mp4');
  AlMimeContentTypeByExtList.Add('.aif=audio/x-aiff');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.aifc=audio/x-aiff');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.aiff=audio/x-aiff');    {Do not Localize}

  AlMimeContentTypeByExtList.Add('.au=audio/basic');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.gsm=audio/x-gsm');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.kar=audio/midi');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.m3u=audio/mpegurl');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.m4a=audio/x-mpg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mid=audio/midi');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.midi=audio/midi');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mpega=audio/x-mpg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mp2=audio/x-mpg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mp3=audio/x-mpg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mpga=audio/x-mpg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.m3u=audio/x-mpegurl');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pls=audio/x-scpls');   {Do not Localize}
  AlMimeContentTypeByExtList.Add('.qcp=audio/vnd.qcelp');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ra=audio/x-realaudio');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ram=audio/x-pn-realaudio');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rm=audio/x-pn-realaudio');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sd2=audio/x-sd2');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sid=audio/prs.sid');   {Do not Localize}
  AlMimeContentTypeByExtList.Add('.snd=audio/basic');   {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wav=audio/x-wav');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wax=audio/x-ms-wax');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wma=audio/x-ms-wma');    {Do not Localize}

  AlMimeContentTypeByExtList.Add('.mjf=audio/x-vnd.AudioExplosion.MjuiceMediaFile');    {Do not Localize}

  { Image }
  AlMimeContentTypeByExtList.Add('.art=image/x-jg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.bmp=image/bmp');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.cdr=image/x-coreldraw');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.cdt=image/x-coreldrawtemplate');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.cpt=image/x-corelphotopaint');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.djv=image/vnd.djvu');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.djvu=image/vnd.djvu');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.gif=image/gif');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ief=image/ief');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ico=image/x-icon');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.jng=image/x-jng');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.jpg=image/jpeg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.jpeg=image/jpeg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.jpe=image/jpeg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pat=image/x-coreldrawpattern');   {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pcx=image/pcx');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pbm=image/x-portable-bitmap');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pgm=image/x-portable-graymap');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pict=image/x-pict');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.png=image/x-png');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pnm=image/x-portable-anymap');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pntg=image/x-macpaint');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ppm=image/x-portable-pixmap');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.psd=image/x-psd');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.qtif=image/x-quicktime');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ras=image/x-cmu-raster');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rf=image/vnd.rn-realflash');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rgb=image/x-rgb');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rp=image/vnd.rn-realpix');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sgi=image/x-sgi');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.svg=image/svg+xml');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.svgz=image/svg+xml');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.targa=image/x-targa');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.tif=image/x-tiff');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wbmp=image/vnd.wap.wbmp');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.webp=image/webp'); {Do not localize}
  AlMimeContentTypeByExtList.Add('.xbm=image/xbm');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.xbm=image/x-xbitmap');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.xpm=image/x-xpixmap');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.xwd=image/x-xwindowdump');    {Do not Localize}

  { Text }
  AlMimeContentTypeByExtList.Add('.323=text/h323');    {Do not Localize}

  AlMimeContentTypeByExtList.Add('.xml=text/xml');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.uls=text/iuls');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.txt=text/plain');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rtx=text/richtext');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wsc=text/scriptlet');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rt=text/vnd.rn-realtext');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.htt=text/webviewhtml');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.htc=text/x-component');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.vcf=text/x-vcard');    {Do not Localize}

  { Video }
  AlMimeContentTypeByExtList.Add('.asf=video/x-ms-asf');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.asx=video/x-ms-asf');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.avi=video/x-msvideo');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.dl=video/dl');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.dv=video/dv');  {Do not Localize}
  AlMimeContentTypeByExtList.Add('.flc=video/flc');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.fli=video/fli');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.gl=video/gl');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.lsf=video/x-la-asf');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.lsx=video/x-la-asf');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mng=video/x-mng');    {Do not Localize}

  AlMimeContentTypeByExtList.Add('.mp2=video/mpeg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mp3=video/mpeg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mp4=video/mpeg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mpeg=video/x-mpeg2a');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mpa=video/mpeg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mpe=video/mpeg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mpg=video/mpeg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ogv=video/ogg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.moov=video/quicktime');     {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mov=video/quicktime');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mxu=video/vnd.mpegurl');   {Do not Localize}
  AlMimeContentTypeByExtList.Add('.qt=video/quicktime');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.qtc=video/x-qtc'); {Do not loccalize}
  AlMimeContentTypeByExtList.Add('.rv=video/vnd.rn-realvideo');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ivf=video/x-ivf');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.webm=video/webm');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wm=video/x-ms-wm');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wmp=video/x-ms-wmp');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wmv=video/x-ms-wmv');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wmx=video/x-ms-wmx');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wvx=video/x-ms-wvx');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rms=video/vnd.rn-realvideo-secure');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.asx=video/x-ms-asf-plugin');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.movie=video/x-sgi-movie');    {Do not Localize}

  { Application }
  AlMimeContentTypeByExtList.Add('.7z=application/x-7z-compressed');   {Do not Localize}
  AlMimeContentTypeByExtList.Add('.a=application/x-archive');   {Do not Localize}
  AlMimeContentTypeByExtList.Add('.aab=application/x-authorware-bin');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.aam=application/x-authorware-map');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.aas=application/x-authorware-seg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.abw=application/x-abiword');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ace=application/x-ace-compressed');  {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ai=application/postscript');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.alz=application/x-alz-compressed');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ani=application/x-navi-animation');   {Do not Localize}
  AlMimeContentTypeByExtList.Add('.arj=application/x-arj');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.asf=application/vnd.ms-asf');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.bat=application/x-msdos-program');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.bcpio=application/x-bcpio');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.boz=application/x-bzip2');     {Do not Localize}
  AlMimeContentTypeByExtList.Add('.bz=application/x-bzip');
  AlMimeContentTypeByExtList.Add('.bz2=application/x-bzip2');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.cab=application/vnd.ms-cab-compressed');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.cat=application/vnd.ms-pki.seccat');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ccn=application/x-cnc');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.cco=application/x-cocoa');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.cdf=application/x-cdf');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.cer=application/x-x509-ca-cert');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.chm=application/vnd.ms-htmlhelp');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.chrt=application/vnd.kde.kchart');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.cil=application/vnd.ms-artgalry');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.class=application/java-vm');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.com=application/x-msdos-program');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.clp=application/x-msclip');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.cpio=application/x-cpio');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.cpt=application/mac-compactpro');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.cqk=application/x-calquick');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.crd=application/x-mscardfile');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.crl=application/pkix-crl');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.csh=application/x-csh');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.dar=application/x-dar');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.dbf=application/x-dbase');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.dcr=application/x-director');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.deb=application/x-debian-package');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.dir=application/x-director');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.dist=vnd.apple.installer+xml');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.distz=vnd.apple.installer+xml');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.dll=application/x-msdos-program');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.dmg=application/x-apple-diskimage');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.doc=application/msword');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.dot=application/msword');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.dvi=application/x-dvi');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.dxr=application/x-director');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ebk=application/x-expandedbook');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.eps=application/postscript');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.evy=application/envoy');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.exe=application/x-msdos-program');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.fdf=application/vnd.fdf');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.fif=application/fractals');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.flm=application/vnd.kde.kivio');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.fml=application/x-file-mirror-list');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.gzip=application/x-gzip');  {Do not Localize}
  AlMimeContentTypeByExtList.Add('.gnumeric=application/x-gnumeric');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.gtar=application/x-gtar');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.gz=application/x-gzip');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.hdf=application/x-hdf');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.hlp=application/winhlp');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.hpf=application/x-icq-hpf');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.hqx=application/mac-binhex40');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.hta=application/hta');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ims=application/vnd.ms-ims');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ins=application/x-internet-signup');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.iii=application/x-iphone');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.iso=application/x-iso9660-image');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.jar=application/java-archive');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.karbon=application/vnd.kde.karbon');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.kfo=application/vnd.kde.kformula');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.kon=application/vnd.kde.kontour');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.kpr=application/vnd.kde.kpresenter');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.kpt=application/vnd.kde.kpresenter');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.kwd=application/vnd.kde.kword');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.kwt=application/vnd.kde.kword');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.latex=application/x-latex');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.lha=application/x-lzh');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.lcc=application/fastman');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.lrm=application/vnd.ms-lrm');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.lz=application/x-lzip');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.lzh=application/x-lzh');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.lzma=application/x-lzma');  {Do not Localize}
  AlMimeContentTypeByExtList.Add('.lzo=application/x-lzop'); {Do not Localize}
  AlMimeContentTypeByExtList.Add('.lzx=application/x-lzx');
  AlMimeContentTypeByExtList.Add('.m13=application/x-msmediaview');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.m14=application/x-msmediaview');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mpp=application/vnd.ms-project');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mvb=application/x-msmediaview');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.man=application/x-troff-man');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mdb=application/x-msaccess');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.me=application/x-troff-me');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ms=application/x-troff-ms');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.msi=application/x-msi');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mpkg=vnd.apple.installer+xml');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mny=application/x-msmoney');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.nix=application/x-mix-transfer');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.o=application/x-object');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.oda=application/oda');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.odb=application/vnd.oasis.opendocument.database');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.odc=application/vnd.oasis.opendocument.chart');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.odf=application/vnd.oasis.opendocument.formula');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.odg=application/vnd.oasis.opendocument.graphics');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.odi=application/vnd.oasis.opendocument.image');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.odm=application/vnd.oasis.opendocument.text-master');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.odp=application/vnd.oasis.opendocument.presentation');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ods=application/vnd.oasis.opendocument.spreadsheet');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ogg=application/ogg');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.odt=application/vnd.oasis.opendocument.text');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.otg=application/vnd.oasis.opendocument.graphics-template');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.oth=application/vnd.oasis.opendocument.text-web');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.otp=application/vnd.oasis.opendocument.presentation-template');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ots=application/vnd.oasis.opendocument.spreadsheet-template');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ott=application/vnd.oasis.opendocument.text-template');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.p10=application/pkcs10');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.p12=application/x-pkcs12');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.p7b=application/x-pkcs7-certificates');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.p7m=application/pkcs7-mime');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.p7r=application/x-pkcs7-certreqresp');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.p7s=application/pkcs7-signature');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.package=application/vnd.autopackage');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pfr=application/font-tdpfr');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pkg=vnd.apple.installer+xml');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pdf=application/pdf');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pko=application/vnd.ms-pki.pko');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pl=application/x-perl');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pnq=application/x-icq-pnq');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pot=application/mspowerpoint');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pps=application/mspowerpoint');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ppt=application/mspowerpoint');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ppz=application/mspowerpoint');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ps=application/postscript');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pub=application/x-mspublisher');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.qpw=application/x-quattropro');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.qtl=application/x-quicktimeplayer');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rar=application/rar');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rdf=application/rdf+xml');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rjs=application/vnd.rn-realsystem-rjs');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rm=application/vnd.rn-realmedia');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rmf=application/vnd.rmf');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rmp=application/vnd.rn-rn_music_package');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rmx=application/vnd.rn-realsystem-rmx');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rnx=application/vnd.rn-realplayer');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rpm=application/x-redhat-package-manager');
  AlMimeContentTypeByExtList.Add('.rsml=application/vnd.rn-rsml');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rtsp=application/x-rtsp');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.rss=application/rss+xml');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.scm=application/x-icq-scm');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ser=application/java-serialized-object');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.scd=application/x-msschedule');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sda=application/vnd.stardivision.draw');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sdc=application/vnd.stardivision.calc');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sdd=application/vnd.stardivision.impress');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sdp=application/x-sdp');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.setpay=application/set-payment-initiation');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.setreg=application/set-registration-initiation');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sh=application/x-sh');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.shar=application/x-shar');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.shw=application/presentations');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sit=application/x-stuffit');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sitx=application/x-stuffitx');  {Do not localize}
  AlMimeContentTypeByExtList.Add('.skd=application/x-koan');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.skm=application/x-koan');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.skp=application/x-koan');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.skt=application/x-koan');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.smf=application/vnd.stardivision.math');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.smi=application/smil');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.smil=application/smil');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.spl=application/futuresplash');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ssm=application/streamingmedia');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sst=application/vnd.ms-pki.certstore');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.stc=application/vnd.sun.xml.calc.template');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.std=application/vnd.sun.xml.draw.template');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sti=application/vnd.sun.xml.impress.template');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.stl=application/vnd.ms-pki.stl');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.stw=application/vnd.sun.xml.writer.template');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.svi=application/softvision');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sv4cpio=application/x-sv4cpio');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sv4crc=application/x-sv4crc');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.swf=application/x-shockwave-flash');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.swf1=application/x-shockwave-flash');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sxc=application/vnd.sun.xml.calc');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sxi=application/vnd.sun.xml.impress');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sxm=application/vnd.sun.xml.math');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sxw=application/vnd.sun.xml.writer');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sxg=application/vnd.sun.xml.writer.global');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.t=application/x-troff');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.tar=application/x-tar');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.tcl=application/x-tcl');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.tex=application/x-tex');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.texi=application/x-texinfo');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.texinfo=application/x-texinfo');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.tbz=application/x-bzip-compressed-tar');   {Do not Localize}
  AlMimeContentTypeByExtList.Add('.tbz2=application/x-bzip-compressed-tar');   {Do not Localize}
  AlMimeContentTypeByExtList.Add('.tgz=application/x-compressed-tar');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.tlz=application/x-lzma-compressed-tar');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.tr=application/x-troff');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.trm=application/x-msterminal');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.troff=application/x-troff');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.tsp=application/dsptype');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.torrent=application/x-bittorrent');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ttz=application/t-time');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.txz=application/x-xz-compressed-tar'); {Do not localize}
  AlMimeContentTypeByExtList.Add('.udeb=application/x-debian-package');    {Do not Localize}

  AlMimeContentTypeByExtList.Add('.uin=application/x-icq');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.urls=application/x-url-list');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.ustar=application/x-ustar');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.vcd=application/x-cdlink');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.vor=application/vnd.stardivision.writer');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.vsl=application/x-cnet-vsl');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wcm=application/vnd.ms-works');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wb1=application/x-quattropro');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wb2=application/x-quattropro');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wb3=application/x-quattropro');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wdb=application/vnd.ms-works');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wks=application/vnd.ms-works');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wmd=application/x-ms-wmd');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wms=application/x-ms-wms');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wmz=application/x-ms-wmz');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wp5=application/wordperfect5.1');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wpd=application/wordperfect');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wpl=application/vnd.ms-wpl');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wps=application/vnd.ms-works');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wri=application/x-mswrite');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.xfdf=application/vnd.adobe.xfdf');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.xls=application/x-msexcel');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.xlb=application/x-msexcel');     {Do not Localize}
  AlMimeContentTypeByExtList.Add('.xpi=application/x-xpinstall');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.xps=application/vnd.ms-xpsdocument');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.xsd=application/vnd.sun.xml.draw');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.xul=application/vnd.mozilla.xul+xml');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.z=application/x-compress');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.zoo=application/x-zoo');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.zip=application/x-zip-compressed');    {Do not Localize}

  { WAP }
  AlMimeContentTypeByExtList.Add('.wbmp=image/vnd.wap.wbmp');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wml=text/vnd.wap.wml');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wmlc=application/vnd.wap.wmlc');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wmls=text/vnd.wap.wmlscript');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.wmlsc=application/vnd.wap.wmlscriptc');    {Do not Localize}

  { Non-web text}
  {
  IMPORTANT!!

  You should not use a text MIME type definition unless you are
  extremely certain that the file will NOT be a binary.  Some browsers
  will display the text instead of saving to disk and it looks ugly
  if a web-browser shows all of the 8bit charactors.
  }
  //of course, we have to add this :-).
  AlMimeContentTypeByExtList.Add('.asm=text/x-asm');   {Do not Localize}
  AlMimeContentTypeByExtList.Add('.p=text/x-pascal');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.pas=text/x-pascal');    {Do not Localize}

  AlMimeContentTypeByExtList.Add('.cs=text/x-csharp'); {Do not Localize}

  AlMimeContentTypeByExtList.Add('.c=text/x-csrc');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.c++=text/x-c++src');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.cpp=text/x-c++src');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.cxx=text/x-c++src');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.cc=text/x-c++src');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.h=text/x-chdr'); {Do not localize}
  AlMimeContentTypeByExtList.Add('.h++=text/x-c++hdr');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.hpp=text/x-c++hdr');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.hxx=text/x-c++hdr');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.hh=text/x-c++hdr');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.java=text/x-java');    {Do not Localize}

  { WEB }
  AlMimeContentTypeByExtList.Add('.css=text/css');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.js=text/javascript');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.htm=text/html');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.html=text/html');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.xhtml=application/xhtml+xml'); {Do not localize}
  AlMimeContentTypeByExtList.Add('.xht=application/xhtml+xml'); {Do not localize}
  AlMimeContentTypeByExtList.Add('.rdf=application/rdf+xml'); {Do not localize}
  AlMimeContentTypeByExtList.Add('.rss=application/rss+xml'); {Do not localize}

  AlMimeContentTypeByExtList.Add('.ls=text/javascript');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.mocha=text/javascript');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.shtml=server-parsed-html');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.xml=text/xml');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sgm=text/sgml');    {Do not Localize}
  AlMimeContentTypeByExtList.Add('.sgml=text/sgml');    {Do not Localize}

  { Message }
  AlMimeContentTypeByExtList.Add('.mht=message/rfc822');    {Do not Localize}


  for I := 0 to AlMimeContentTypeByExtList.Count - 1 do
    AlExtbyMimeContentTypeList.Add(AlMimeContentTypeByExtList.ValueFromIndex[I] + '=' + AlMimeContentTypeByExtList.Names[I]);

end;

Initialization
  AlMimeContentTypeByExtList := TALNVStringList.Create;
  AlExtbyMimeContentTypeList := TALNVStringList.Create;
  ALFillMimeTable;
  TALNVStringList(AlMimeContentTypeByExtList).Duplicates := dupAccept;
  TALNVStringList(AlMimeContentTypeByExtList).Sorted := true;
  TALNVStringList(AlExtbyMimeContentTypeList).Duplicates := dupAccept;
  TALNVStringList(AlExtbyMimeContentTypeList).Sorted := true;

finalization
  AlFreeandNil(AlMimeContentTypeByExtList);
  AlFreeandNil(AlExtbyMimeContentTypeList);

end.
