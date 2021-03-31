/// PDF file generation
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynPdf;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
   Achim Kalwa
   Alexander (chaa)
   aweste
   CoMPi
   Damien (ddemars)
   David Mead (MDW)
   David Heffernan
   FalconB
   Florian Grummel
   Harald Simon
   Josh Kelley (joshkel)
   Karel (vandrovnik)
   Kukhtin Igor
   LoukaO
   Marsh
   MChaos
   Mehrdad Momeni (nosa)
   mogulza
   Nzsolt
   Ondrej (reddwarf)
   Pierre le Riche
   Sinisa (sinisav)
   Sundazer

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Sponsors: https://synopse.info/fossil/wiki?name=HelpDonate
  Ongoing development and maintenance of the SynPDF library was sponsored
  in part by:
   https://www.helpndoc.com
    Easy to use yet powerful help authoring environment which can generate
    various documentation formats from a single source.
  Thanks for your contribution!

}


{$I Synopse.inc} // define HASINLINE CPU32 CPU64

{$ifndef MSWINDOWS}
  { disable features requiring OS specific APIs
    - until they are implemented }
  {$define NO_USE_SYNGDIPLUS}
  {$define NO_USE_UNISCRIBE}
  {$define NO_USE_METAFILE}
  {$define NO_USE_BITMAP}
{$endif}


{$define USE_PDFSECURITY}
{ - if defined, the TPdfDocument*.Create() constructor will have an additional
  AEncryption: TPdfEncryption parameter able to create secured PDF files
  - this feature will link the SynCrypto unit for MD5 and RC4 algorithms }
{$ifdef NO_USE_PDFSECURITY}
  { this special conditional can be set globaly for an application which doesn't
    need the security features, therefore dependency to SynCrypto unit }
  {$undef USE_PDFSECURITY}
{$endif}

{$define USE_UNISCRIBE}
{ - if defined, the PDF engine will use the Windows Uniscribe API to
  render Ordering and Shaping of the text (useful for Hebrew, Arabic and
  some Asiatic languages)
  - this feature need the TPdfDocument.UseUniscribe property to be forced to true
  according to the language of the text you want to render
  - can be undefined to safe some KB if you're sure you won't need it }
{$ifdef NO_USE_UNISCRIBE}
  { this special conditional can be set globaly for an application which doesn't
    need the UniScribe features }
  {$undef USE_UNISCRIBE}
{$endif}

{$define USE_SYNGDIPLUS}
{ - if defined, the PDF engine will use SynGdiPlus to handle all
    JPG, TIF, PNG and GIF image types (prefered way, but need XP or later OS)
  - if you'd rather use the default jpeg unit (and add some more code to your
    executable), undefine this conditional }
{$ifdef NO_USE_SYNGDIPLUS}
  { this special conditional can be set globaly for an application which doesn't
    need the SynGdiPlus features (like TMetaFile drawing), and would rather
    use the default jpeg unit }
  {$undef USE_SYNGDIPLUS}
{$endif}

{$define USE_SYNZIP}
{ - if defined, the PDF engine will use SynZip to handle the ZIP/deflate
    compression schema (this unit is faster than the default ZLib unit,
    and used by other units of the framework)
  - if you'd rather use the default ZLib unit (and add some more code to your
    executable), undefine this conditional }
{$ifdef NO_USE_SYNZIP}
  { this special conditional can be set globaly for an application for which
    standard ZLib unit is enough (not to be used with a mORMot application) }
  {$undef USE_SYNZIP}
{$endif}

{$define USE_BITMAP}
{ - if defined, the PDF engine will support TBitmap
  - it would induce a dependency to the VCL.Graphics unit }
{$ifdef NO_USE_BITMAP}
  { this special conditional can be set globaly for an application which doesn't
    need the TBitmap features }
  {$undef USE_BITMAP}
{$endif}

{$define USE_METAFILE}
{ - if defined, the PDF engine will support TMetaFile/TMetaFileCanvas
  - it would induce a dependency to the VCL.Graphics unit }
{$ifdef NO_USE_METAFILE}
  { this special conditional can be set globaly for an application which doesn't
    need the TMetaFile features }
  {$undef USE_METAFILE}
{$endif}

{$define USE_ARC}
{ - if defined, the PDF engine will support ARC, inducing a dependency to Math.pas }
{$ifdef NO_USE_ARC}
  {$undef USE_ARC}
{$endif}

{$ifdef USE_BITMAP}
  {$define USE_GRAPHICS_UNIT}
{$endif}
{$ifdef USE_METAFILE}
  {$define USE_GRAPHICS_UNIT}
{$endif}

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  WinSpool,
  {$ifdef USE_GRAPHICS_UNIT}
  {$ifdef ISDELPHIXE2}
  VCL.Graphics,
  {$else}
  Graphics,
  {$endif}
  {$endif}
  {$endif MSWINDOWS}
  {$ifdef USE_SYNGDIPLUS}
  SynGdiPlus, // use our GDI+ library for handling TJpegImage and such
  {$else}
  jpeg,
  {$endif}
  SysConst,
  SysUtils,
  Classes,
  {$ifdef USE_ARC}
  Math,
  {$endif}
  {$ifdef ISDELPHIXE3}
  System.Types,
  System.AnsiStrings,
  {$else}
  {$ifdef HASINLINE}
  Types,
  {$endif}
  {$endif}
  {$ifdef USE_SYNZIP}
  SynZip,
  {$else}
  ZLib,
  {$endif}
  {$ifdef USE_PDFSECURITY}
  SynCrypto,
  {$endif}
  SynCommons,
  SynLZ;

const
  MWT_IDENTITY = 1;
  MWT_LEFTMULTIPLY = 2;
  MWT_RIGHTMULTIPLY = 3;
  MWT_SET = 4;
  {$NODEFINE MWT_IDENTITY}
  {$NODEFINE MWT_LEFTMULTIPLY}
  {$NODEFINE MWT_RIGHTMULTIPLY}

{  some low-level record definition for True Type format table reading }

type
  PSmallIntArray = ^TSmallIntArray;
  TSmallIntArray = array[byte] of SmallInt;

  PPointArray = ^TPointArray;
  TPointArray = array[word] of TPoint;
  PSmallPointArray = ^TSmallPointArray;
  TSmallPointArray = array[word] of TSmallPoint;

  /// The 'cmap' table begins with an index containing the table version number
  // followed by the number of encoding tables. The encoding subtables follow.
  TCmapHeader = packed record
    /// Version number (Set to zero)
    version: word;
    /// Number of encoding subtables
    numberSubtables: word;
  end;
  /// points to every 'cmap' encoding subtables
  TCmapSubTableArray = packed array[byte] of packed record
    /// Platform identifier
    platformID: word;
    /// Platform-specific encoding identifier
    platformSpecificID: word;
    /// Offset of the mapping table
    offset: Cardinal;
  end;
  /// The 'hhea' table contains information needed to layout fonts whose
  // characters are written horizontally, that is, either left to right or
  // right to left
  TCmapHHEA = packed record
    version: longint;
    ascent: word;
    descent: word;
    lineGap: word;
    advanceWidthMax: word;
    minLeftSideBearing: word;
    minRightSideBearing: word;
    xMaxExtent: word;
    caretSlopeRise: SmallInt;
    caretSlopeRun: SmallInt;
    caretOffset: SmallInt;
    reserved: Int64;
    metricDataFormat: SmallInt;
    numOfLongHorMetrics: word;
  end;
  /// The 'head' table contains global information about the font
  TCmapHEAD = packed record
    version: longint;
    fontRevision: longint;
    checkSumAdjustment: cardinal;
    magicNumber: cardinal;
    flags: word;
    unitsPerEm: word;
    createdDate: Int64;
    modifiedDate: Int64;
    xMin: SmallInt;
    yMin: SmallInt;
    xMax: SmallInt;
    yMax: SmallInt;
    macStyle: word;
    lowestRec: word;
    fontDirection: SmallInt;
    indexToLocFormat: SmallInt;
    glyphDataFormat: SmallInt
  end;
  /// header for the 'cmap' Format 4 table
  // - this is a two-byte encoding format
  TCmapFmt4 = packed record
    format: word;
    length: word;
    language: word;
    segCountX2: word;
    searchRange: word;
    entrySelector: word;
    rangeShift: word;
  end;

type
  /// the PDF library use internaly AnsiString text encoding
  // - the corresponding charset is the current system charset, or the one
  // supplied as a parameter to TPdfDocument.Create
  PDFString = AnsiString;

  /// a PDF date, encoded as 'D:20100414113241'
  TPdfDate = PDFString;

  /// the internal pdf file format
  TPdfFileFormat = (pdf13, pdf14, pdf15, pdf16);

  /// PDF exception, raised when an invalid value is given to a constructor
  EPdfInvalidValue = class(Exception);

  /// PDF exception, raised when an invalid operation is triggered
  EPdfInvalidOperation = class(Exception);

  /// Page mode determines how the document should appear when opened
  TPdfPageMode = (
    pmUseNone, pmUseOutlines, pmUseThumbs, pmFullScreen);

  /// Line cap style specifies the shape to be used at the ends of open
  // subpaths when they are stroked
  TLineCapStyle = (
    lcButt_End, lcRound_End, lcProjectingSquareEnd);

  /// The line join style specifies the shape to be used at the corners of paths
  // that are stroked
  TLineJoinStyle = (
    ljMiterJoin, ljRoundJoin, ljBevelJoin);

  /// The text rendering mode determines whether text is stroked, filled, or used
  // as a clipping path
  TTextRenderingMode = (
    trFill, trStroke, trFillThenStroke, trInvisible,
    trFillClipping, trStrokeClipping, trFillStrokeClipping, trClipping);

  /// The annotation types determines the valid annotation subtype of TPdfDoc
  TPdfAnnotationSubType = (
    asTextNotes, asLink);

  /// The border style of an annotation
  TPdfAnnotationBorder = (
    abSolid, abDashed, abBeveled, abInset, abUnderline);

  /// Destination Type determines default user space coordinate system of
  // Explicit destinations
  TPdfDestinationType = (
    dtXYZ, dtFit, dtFitH, dtFitV, dtFitR, dtFitB, dtFitBH, dtFitBV);

  /// The page layout to be used when the document is opened
  TPdfPageLayout = (
    plSinglePage, plOneColumn, plTwoColumnLeft, plTwoColumnRight);

  /// Viewer preferences specifying how the reader User Interface must start
  // - vpEnforcePrintScaling will set the file version to be PDF 1.6
  TPdfViewerPreference = (
    vpHideToolbar, vpHideMenubar, vpHideWindowUI, vpFitWindow, vpCenterWindow,
    vpEnforcePrintScaling);

  /// set of Viewer preferences
  TPdfViewerPreferences = set of TPdfViewerPreference;

  /// available known paper size (psA4 is the default on TPdfDocument creation)
  TPDFPaperSize = (
    psA4, psA5, psA3, psA2, psA1, psA0, psLetter, psLegal, psUserDefined);

  /// define if streams must be compressed
  TPdfCompressionMethod = (
    cmNone, cmFlateDecode);

  /// the available PDF color range
  TPdfColor = -$7FFFFFFF-1..$7FFFFFFF;

  /// the PDF color, as expressed in RGB terms
  // - maps COLORREF / TColorRef as used e.g. under windows
  TPdfColorRGB = cardinal;

  /// the recognized families of the Standard 14 Fonts
  TPdfFontStandard = (pfsTimes, pfsHelvetica, pfsCourier);

  /// numerical ID for every XObject
  TXObjectID = integer;


const
  /// used for an used xref entry
  PDF_IN_USE_ENTRY = 'n';
  /// used for an unused (free) xref entry, e.g. the root entry
  PDF_FREE_ENTRY = 'f';
  /// used e.g. for the root xref entry
  PDF_MAX_GENERATION_NUM = 65535;

  PDF_ENTRY_CLOSED = 0;
  PDF_ENTRY_OPENED = 1;

  /// the Carriage Return and Line Feed values used in the PDF file generation
  // - expect #13 and #10 under Windows, but #10 (e.g. only Line Feed) is enough
  // for the PDF standard, and will create somewhat smaller PDF files
  CRLF = #10;
  /// the Line Feed value
  LF = #10;

  PDF_MIN_HORIZONTALSCALING = 10;
  PDF_MAX_HORIZONTALSCALING = 300;
  PDF_MAX_WORDSPACE = 300;
  PDF_MIN_CHARSPACE = -30;
  PDF_MAX_CHARSPACE = 300;
  PDF_MAX_FONTSIZE = 2000;
  PDF_MAX_ZOOMSIZE = 10;
  PDF_MAX_LEADING = 300;

  /// list of common fonts available by default since Windows 2000
  // - to not embedd these fonts in the PDF document, and save some KB,
  // just use the EmbeddedTTFIgnore property of TPdfDocument/TPdfDocumentGDI:
  // !   PdfDocument.EmbeddedTTFIgnore.Text := MSWINDOWS_DEFAULT_FONTS;
  // - note that this is useful only if the EmbeddedTTF property was set to TRUE
  MSWINDOWS_DEFAULT_FONTS: RawUTF8 =
    'Arial'#13#10'Courier New'#13#10'Georgia'#13#10+
    'Impact'#13#10'Lucida Console'#13#10'Roman'#13#10'Symbol'#13#10+
    'Tahoma'#13#10'Times New Roman'#13#10'Trebuchet'#13#10+
    'Verdana'#13#10'WingDings';

type
  /// PDF text paragraph alignment
  TPdfAlignment = (paLeftJustify, paRightJustify, paCenter);

  /// PDF gradient direction
  TGradientDirection = (gdHorizontal, gdVertical);

    /// a PDF coordinates rectangle
  TPdfRect = record
    Left, Top, Right, Bottom: Single;
  end;
  PPdfRect = ^TPdfRect;

  /// a PDF coordinates box
  TPdfBox = record
    Left, Top, Width, Height: Single;
  end;
  PPdfBox = ^TPdfBox;

  /// allowed types for PDF objects (i.e. TPdfObject)
  TPdfObjectType = (otDirectObject, otIndirectObject, otVirtualObject);

  TPdfObject = class;
  TPdfCanvas = class;
  TPdfFont = class;
  TPdfFontTrueType = class;
  TPdfDocument = class;

{$ifdef USE_PDFSECURITY}
  /// the available encryption levels
  // - in current version only RC4 40-bit and RC4 128-bit are available, which
  // correspond respectively to PDF 1.3 and PDF 1.4 formats
  // - for RC4 40-bit and RC4 128-bit, associated password are restricted to a
  // maximum length of 32 characters and could contain only characters from the
  // Latin-1 encoding (i.e. no accent)
  TPdfEncryptionLevel = (elNone, elRC4_40, elRC4_128);

  /// PDF can encode various restrictions on document operations which can be
  // granted or denied individually (some settings depend on others, though):
  // - Printing: If printing is not allowed, the print button in Acrobat will be
  // disabled. Acrobat supports a distinction between high-resolution and
  // low-resolution printing. Low-resolution printing generates a bitmapped
  // image of the page which is suitable only for personal use, but prevents
  // high-quality reproduction and re-distilling. Note that bitmap printing
  // not only results in low output quality, but will also considerably slow
  // down the printing process.
  // - General Editing: If this is disabled, any document modification is
  // prohibited. Content extraction and printing are allowed.
  // - Content Copying and Extraction: If this is disabled, selecting document
  // contents and copying it to the clipboard for repurposing the contents is
  // prohibited. The accessibility interface also is disabled. If you need to
  // search such documents with Acrobat you must select the Certified Plugins
  // Only preference in Acrobat.
  // - Authoring Comments and Form Fields: If this is disabled, adding,
  // modifying, or deleting comments and form fields is prohibited. Form field
  // filling is allowed.
  // - Form Field Fill-in or Signing: If this is enabled, users can sign and
  // fill in forms, but not create form fields.
  // - Document Assembly: If this is disabled, inserting, deleting or rotating
  // pages, or creating bookmarks and thumbnails is prohibited.
  TPdfEncryptionPermission = (epPrinting, epGeneralEditing, epContentCopy,
    epAuthoringComment, epFillingForms, epContentExtraction,
    epDocumentAssembly, epPrintingHighResolution);

  /// set of restrictions on PDF document operations
  TPdfEncryptionPermissions = set of TPdfEncryptionPermission;

  /// abstract class to handle PDF security
  TPdfEncryption = class
  protected
    fLevel: TPdfEncryptionLevel;
    fFlags: integer;
    fInternalKey: TByteDynArray;
    fPermissions: TPdfEncryptionPermissions;
    fUserPassword: string;
    fOwnerPassword: string;
    fDoc: TPdfDocument;
    procedure EncodeBuffer(const BufIn; var BufOut; Count: cardinal); virtual; abstract;
  public
    /// initialize the internal structures with the proper classes
    // - do not call this method directly, but class function TPdfEncryption.New()
    constructor Create(aLevel: TPdfEncryptionLevel; aPermissions: TPdfEncryptionPermissions;
      const aUserPassword, aOwnerPassword: string); virtual;
    /// prepare a specific document to be encrypted
    // - internally used by TPdfDocument.NewDoc method
    procedure AttachDocument(aDoc: TPdfDocument); virtual;
    /// will create the expected TPdfEncryption instance, depending on aLevel
    // - to be called as parameter of TPdfDocument/TPdfDocumentGDI.Create()
    // - currently, only elRC4_40 and elRC4_128 levels are implemented
    // - both passwords are expected to be ASCII-7 characters only
    // - aUserPassword will be asked at file opening: to be set to '' for not
    // blocking display, but optional permission
    // - aOwnerPassword shall not be '', and will be used internally to cypher
    // the pdf file content
    // - aPermissions can be either one of the PDF_PERMISSION_ALL /
    // PDF_PERMISSION_NOMODIF / PDF_PERSMISSION_NOPRINT / PDF_PERMISSION_NOCOPY /
    // PDF_PERMISSION_NOCOPYNORPRINT set of options
    // - typical use may be:
    // ! Doc := TPdfDocument.Create(false,0,false,
    // !   TPdfEncryption.New(elRC4_40,'','toto',PDF_PERMISSION_NOMODIF));
    // ! Doc := TPdfDocument.Create(false,0,false,
    // !   TPdfEncryption.New(elRC4_128,'','toto',PDF_PERMISSION_NOCOPYNORPRINT));
    class function New(aLevel: TPdfEncryptionLevel;
      const aUserPassword, aOwnerPassword: string;
      aPermissions: TPdfEncryptionPermissions): TPdfEncryption;
  end;

  /// internal 32 bytes buffer, used during encryption process
  TPdfBuffer32 = array[0..31] of byte;

  /// handle PDF security with RC4+MD5 scheme in 40-bit and 128-bit
  // - allowed aLevel parameters for Create() are only elRC4_40 and elRC4_128
  TPdfEncryptionRC4MD5 = class(TPdfEncryption)
  protected
    fLastObjectNumber: integer;
    fLastGenerationNumber: Integer;
    fUserPass, fOwnerPass: TPdfBuffer32;
    fLastRC4Key: TRC4;
    procedure EncodeBuffer(const BufIn; var BufOut; Count: cardinal); override;
  public
    /// prepare a specific document to be encrypted
    // - will compute the internal keys
    procedure AttachDocument(aDoc: TPdfDocument); override;
  end;
{$endif USE_PDFSECURITY}

  /// buffered writer class, specialized for PDF encoding
  TPdfWrite = class
  protected
    B, BEnd, BEnd4: PAnsiChar;
    fDestStream: TStream;
    fDestStreamPosition: integer;
    fCodePage: integer;
    fAddGlyphFont: (fNone, fMain, fFallBack);
    fDoc: TPdfDocument;
    Tmp: array[0..511] of AnsiChar;
    /// internal Ansi->Unicode conversion, using the CodePage used in Create()
    // - caller must release the returned memory via FreeMem()
    function ToWideChar(const Ansi: PDFString; out DLen: Integer): PWideChar;
    {$ifdef USE_UNISCRIBE}
    /// internal method using the Windows Uniscribe API
    // - return FALSE if PW was not appened to the PDF content, TRUE if OK
    function AddUnicodeHexTextUniScribe(PW: PWideChar; WinAnsiTTF: TPdfFontTrueType;
      NextLine: boolean; Canvas: TPdfCanvas): boolean;
    {$endif}
    /// internal method NOT using the Windows Uniscribe API
    procedure AddUnicodeHexTextNoUniScribe(PW: PWideChar; TTF: TPdfFontTrueType;
      NextLine: boolean; Canvas: TPdfCanvas);
    /// internal methods handling font fall-back
    procedure AddGlyphFromChar(Char: WideChar; Canvas: TPdfCanvas;
      TTF: TPdfFontTrueType; NextLine: PBoolean);
    procedure AddGlyphFlush(Canvas: TPdfCanvas; TTF: TPdfFontTrueType; NextLine: PBoolean);
  public
    /// create the buffered writer, for a specified destination stream
    constructor Create(Destination: TPdfDocument; DestStream: TStream);
    /// add a character to the buffer
    function Add(c: AnsiChar): TPdfWrite; overload; {$ifdef HASINLINE}inline;{$endif}
    /// add an integer numerical value to the buffer
    function Add(Value: Integer): TPdfWrite; overload;
    /// add an integer numerical value to the buffer
    // - and append a trailing space
    function AddWithSpace(Value: Integer): TPdfWrite; overload;
    /// add an integer numerical value to the buffer
    // - with a specified fixed number of digits (left filled by '0')
    function Add(Value, DigitCount: Integer): TPdfWrite; overload;
    /// add a floating point numerical value to the buffer
    // - up to 2 decimals are written
    function Add(Value: TSynExtended): TPdfWrite; overload;
    /// add a floating point numerical value to the buffer
    // - up to 2 decimals are written, together with a trailing space
    function AddWithSpace(Value: TSynExtended): TPdfWrite; overload;
    /// add a floating point numerical value to the buffer
    // - this version handles a variable number of decimals, together with
    // a trailing space - this is used by ConcatToCTM e.g. or enhanced precision
    function AddWithSpace(Value: TSynExtended; Decimals: cardinal): TPdfWrite; overload;
    /// direct raw write of some data
    // - no conversion is made
    function Add(Text: PAnsiChar; Len: integer): TPdfWrite; overload;
    /// direct raw write of some data
    // - no conversion is made
    function Add(const Text: RawByteString): TPdfWrite; overload;
    /// hexadecimal write of some row data
    // - row data is written as hexadecimal byte values, one by one
    function AddHex(const Bin: PDFString): TPdfWrite;
    /// add a word value, as Big-Endian 4 hexadecimal characters
    function AddHex4(aWordValue: cardinal): TPdfWrite;
    /// convert some text into unicode characters, then write it as as Big-Endian
    // 4 hexadecimal characters
    // - Ansi to Unicode conversion uses the CodePage set by Create() constructor
    function AddToUnicodeHex(const Text: PDFString): TPdfWrite;
    /// write some unicode text as as Big-Endian 4 hexadecimal characters
    function AddUnicodeHex(PW: PWideChar; WideCharCount: integer): TPdfWrite;
    /// convert some text into unicode characters, then write it as PDF Text
    // - Ansi to Unicode conversion uses the CodePage set by Create() constructor
    // - use (...) for all WinAnsi characters, or <..hexa..> for Unicode characters
    // - if NextLine is TRUE, the first written PDF Text command is not Tj but '
    // - during the text process, corresponding TPdfTrueTypeFont properties are
    // updated (Unicode version created if necessary, indicate used glyphs for
    // further Font properties writting to the PDF file content...)
    // - if the current font is not True Type, all Unicode characters are
    // drawn as '?'
    function AddToUnicodeHexText(const Text: PDFString; NextLine: boolean;
      Canvas: TPdfCanvas): TPdfWrite;
    /// write some Unicode text, as PDF text
    // - incoming unicode text must end with a #0
    // - use (...) for all WinAnsi characters, or <..hexa..> for Unicode characters
    // - if NextLine is TRUE, the first written PDF Text command is not Tj but '
    // - during the text process, corresponding TPdfTrueTypeFont properties are
    // updated (Unicode version created if necessary, indicate used glyphs for
    // further Font properties writting to the PDF file content...)
    // - if the current font is not True Type, all Unicode characters are
    // drawn as '?'
    function AddUnicodeHexText(PW: PWideChar; NextLine: boolean;
      Canvas: TPdfCanvas): TPdfWrite;
    /// write some Unicode text, encoded as Glyphs indexes, corresponding
    // to the current font
    function AddGlyphs(Glyphs: PWord; GlyphsCount: integer; Canvas: TPdfCanvas;
      AVisAttrsPtr: Pointer=nil): TPdfWrite;
    /// add some WinAnsi text as PDF text
    // - used by TPdfText object
    // - will optionally encrypt the content
    function AddEscapeContent(const Text: RawByteString): TPdfWrite;
    /// add some WinAnsi text as PDF text
    // - used by TPdfText object
    function AddEscape(Text: PAnsiChar; TextLen: integer): TPdfWrite;
    /// add some WinAnsi text as PDF text
    // - used by TPdfCanvas.ShowText method for WinAnsi text
    function AddEscapeText(Text: PAnsiChar; Font: TPdfFont): TPdfWrite;
    /// add some PDF /property value
    function AddEscapeName(Text: PAnsiChar): TPdfWrite;
    {$ifdef MSWINDOWS}
    /// add a PDF color, from its TPdfColorRGB RGB value
    function AddColorStr(Color: TPdfColorRGB): TPdfWrite;
    {$endif}
    /// add a TBitmap.Scanline[] content into the stream
    procedure AddRGB(P: PAnsiChar; PInc, Count: integer);
    /// add an ISO 8601 encoded date time (e.g. '2010-06-16T15:06:59-07:00')
    function AddIso8601(DateTime: TDateTime): TPdfWrite;
    /// add an integer value as binary, specifying a storage size in bytes
    function AddIntegerBin(value: integer; bytesize: cardinal): TPdfWrite;
  public
    /// flush the internal buffer to the destination stream
    procedure Save; {$ifdef HASINLINE}inline;{$endif}
    /// return the current position
    // - add the current internal buffer stream position to the destination
    // stream position
    function Position: Integer; {$ifdef HASINLINE}inline;{$endif}
    /// get the data written to the Writer as a PDFString
    // - this method could not use Save to flush the data, if all input was
    // inside the internal buffer (save some CPU and memory): so don't intend
    // the destination stream to be flushed after having called this method
    function ToPDFString: PDFString;
  end;

  /// object manager is a virtual class to manage instance of indirect PDF objects
  TPdfObjectMgr = class(TObject)
  public
    procedure AddObject(AObject: TPdfObject); virtual; abstract;
    function GetObject(ObjectID: integer): TPdfObject; virtual; abstract;
  end;

  /// master class for most PDF objects declaration
  TPdfObject = class(TObject)
  private
    FObjectType: TPdfObjectType;
    FObjectNumber: integer;
    FGenerationNumber: integer;
    FSaveAtTheEnd: boolean;
  protected
    procedure InternalWriteTo(W: TPdfWrite); virtual;
    procedure SetObjectNumber(Value: integer);
    function SpaceNotNeeded: boolean; virtual;
  public
    /// create the PDF object instance
    constructor Create; virtual;
    /// Write object to specified stream
    // - If object is indirect object then write references to stream
    procedure WriteTo(var W: TPdfWrite);
    /// write indirect object to specified stream
    // - this method called by parent object
    procedure WriteValueTo(var W: TPdfWrite);
    /// low-level force the object to be saved now
    // - you should not use this low-level method, unless you want to force
    // the FSaveAtTheEnd internal flag to be set to force, so that
    // TPdfDocument.SaveToStreamDirectPageFlush would flush the object content
    procedure ForceSaveNow;
    /// the associated PDF Object Number
    // - If you set an object number higher than zero, the object is considered
    // as indirect. Otherwise, the object is considered as direct object.
    property ObjectNumber: integer read FObjectNumber write SetObjectNumber;
    /// the associated PDF Generation Number
    property GenerationNumber: integer read FGenerationNumber;
    /// the corresponding type of this PDF object
    property ObjectType: TPdfObjectType read FObjectType;
  end;

  /// a virtual PDF object, with an associated PDF Object Number
  TPdfVirtualObject = class(TPdfObject)
  public
    constructor Create(AObjectId: integer); reintroduce;
  end;

  /// a PDF object, storing a boolean value
  TPdfBoolean = class(TPdfObject)
  private
    FValue: boolean;
  protected
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    constructor Create(AValue: Boolean); reintroduce;
    property Value: boolean read FValue write FValue;
  end;

  /// a PDF object, storing a NULL value
  TPdfNull = class(TPdfObject)
  protected
    procedure InternalWriteTo(W: TPdfWrite); override;
  end;

  /// a PDF object, storing a numerical (integer) value
  TPdfNumber = class(TPdfObject)
  private
    FValue: integer;
  protected
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    constructor Create(AValue: Integer); reintroduce;
    property Value: integer read FValue write FValue;
  end;

  /// a PDF object, storing a numerical (floating point) value
  TPdfReal = class(TPdfObject)
  private
    FValue: double;
  protected
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    constructor Create(AValue: double); reintroduce;
    property Value: double read FValue write FValue;
  end;

  /// a PDF object, storing a textual value
  // - the value is specified as a PDFString
  // - this object is stored as '(escapedValue)'
  // - in case of MBCS, conversion is made into Unicode before writing, and
  // stored as '<FEFFHexUnicodeEncodedValue>'
  TPdfText = class(TPdfObject)
  private
    FValue: RawByteString;
  protected
    procedure InternalWriteTo(W: TPdfWrite); override;
    function SpaceNotNeeded: boolean; override;
  public
    constructor Create(const AValue: RawByteString); reintroduce;
    property Value: RawByteString read FValue write FValue;
  end;

  /// a PDF object, storing a textual value
  // - the value is specified as an UTF-8 encoded string
  // - this object is stored as '(escapedValue)'
  // - in case characters with ANSI code higher than 8 Bits, conversion is made
  // into Unicode before writing, and '<FEFFHexUnicodeEncodedValue>'
  TPdfTextUTF8 = class(TPdfObject)
  private
    FValue: RawUTF8;
  protected
    procedure InternalWriteTo(W: TPdfWrite); override;
    function SpaceNotNeeded: boolean; override;
  public
    constructor Create(const AValue: RawUTF8); reintroduce;
    property Value: RawUTF8 read FValue write FValue;
  end;

  /// a PDF object, storing a textual value
  // - the value is specified as a generic VCL string
  // - this object is stored as '(escapedValue)'
  // - in case characters with ANSI code higher than 8 Bits, conversion is made
  // into Unicode before writing, and '<FEFFHexUnicodeEncodedValue>'
  TPdfTextString = class(TPdfTextUTF8)
  private
    function GetValue: string;
    procedure SetValue(const Value: string);
  public
    constructor Create(const AValue: string); reintroduce;
    property Value: string read GetValue write SetValue;
  end;

  /// a PDF object, storing a raw PDF content
  // - this object is stored into the PDF stream as the defined Value
  TPdfRawText = class(TPdfText)
  protected
    function SpaceNotNeeded: boolean; override;
    procedure InternalWriteTo(W: TPdfWrite); override;
  end;

  /// a PDF object, storing a textual value with no encryption
  // - the value is specified as a memory buffer
  // - this object is stored as '(escapedValue)'
  TPdfClearText = class(TPdfText)
  protected
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    constructor Create(Buffer: pointer; Len: integer); reintroduce;
  end;

  /// a PDF object, storing a PDF name
  // - this object is stored as '/Value'
  TPdfName = class(TPdfText)
  protected
    procedure InternalWriteTo(W: TPdfWrite); override;
  end;

  /// used to store an array of PDF objects
  TPdfArray = class(TPdfObject)
  private
    FArray: TList;
    FObjectMgr: TPdfObjectMgr;
    function GetItems(Index: integer): TPdfObject; {$ifdef HASINLINE}inline;{$endif}
    function GetItemCount: integer; {$ifdef HASINLINE}inline;{$endif}
  protected
    procedure InternalWriteTo(W: TPdfWrite); override;
    function SpaceNotNeeded: boolean; override;
  public
    /// create an array of PDF objects
    constructor Create(AObjectMgr: TPdfObjectMgr); reintroduce; overload;
    /// create an array of PDF objects, with some specified TPdfNumber values
    constructor Create(AObjectMgr: TPdfObjectMgr;
      const AArray: array of Integer); reintroduce; overload;
    /// create an array of PDF objects, with some specified TPdfNumber values
    constructor Create(AObjectMgr: TPdfObjectMgr;
      AArray: PWordArray; AArrayCount: integer); reintroduce; overload;
    /// create an array of PDF objects, with some specified TPdfName values
    constructor CreateNames(AObjectMgr: TPdfObjectMgr;
      const AArray: array of PDFString); reintroduce; overload;
    /// create an array of PDF objects, with some specified TPdfReal values
    constructor CreateReals(AObjectMgr: TPdfObjectMgr;
      const AArray: array of double); reintroduce; overload;
    /// release the instance memory, and all embedded objects instances
    destructor Destroy; override;
    /// Add a PDF object to the array
    // - if AItem already exists, do nothing
    function AddItem(AItem: TPdfObject): integer;
    /// insert a PDF object to the array
    // - if AItem already exists, do nothing
    procedure InsertItem(Index: Integer; AItem: TPdfObject);
    /// retrieve a TPDFName object stored in the array
    function FindName(const AName: PDFString): TPdfName;
    /// remove a specified TPDFName object stored in the array
    function RemoveName(const AName: PDFString): boolean;
    /// retrieve an object instance, stored in the array
    property Items[Index: integer]: TPdfObject read GetItems; default;
    /// retrieve the array size
    property ItemCount: integer read GetItemCount;
    /// the associated PDF Object Manager
    property ObjectMgr: TPdfObjectMgr read FObjectMgr;
    /// direct access to the internal TList instance
    // - not to be used normally
    property List: TList read FArray;
  end;

  /// PDF dictionary element definition
  TPdfDictionaryElement = class(TObject)
  private
    FKey: TPdfName;
    FValue: TPdfObject;
    FIsInternal: boolean;
    function GetKey: PDFString;
  public
    /// create the corresponding Key / Value pair
    constructor Create(const AKey: PDFString; AValue: TPdfObject; AInternal: Boolean=false);
    /// release the element instance, and both associated Key and Value
    destructor Destroy; override;
    /// the associated Key Name
    property Key: PDFString read GetKey;
    /// the associated Value stored in this element
    property Value: TPdfObject read FValue;
    /// if this element was created as internal, i.e. not to be saved to the PDF content
    property IsInternal: boolean read FIsInternal;
  end;

  /// a PDF Dictionary is used to manage Key / Value pairs
  TPdfDictionary = class(TPdfObject)
  private
    FArray: TList;
    FObjectMgr: TPdfObjectMgr;
    function GetItems(Index: integer): TPdfDictionaryElement; {$ifdef HASINLINE}inline;{$endif}
    function GetItemCount: integer; {$ifdef HASINLINE}inline;{$endif}
  protected
    function getTypeOf: PDFString;
    function SpaceNotNeeded: boolean; override;
    procedure DirectWriteto(W: TPdfWrite; Secondary: TPdfDictionary);
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    /// create the PDF dictionary
    constructor Create(AObjectMgr: TPdfObjectMgr); reintroduce;
    /// release the dictionay instance, and all associated elements
    destructor Destroy; override;
    /// fast find a value by its name
    function ValueByName(const AKey: PDFString): TPdfObject;
    /// fast find a boolean value by its name
    function PdfBooleanByName(const AKey: PDFString): TPdfBoolean; {$ifdef HASINLINE}inline;{$endif}
    /// fast find a numerical (integer) value by its name
    function PdfNumberByName(const AKey: PDFString): TPdfNumber; {$ifdef HASINLINE}inline;{$endif}
    /// fast find a textual value by its name
    function PdfTextByName(const AKey: PDFString): TPdfText; {$ifdef HASINLINE}inline;{$endif}
    /// fast find a textual value by its name
    // - return '' if not found, the TPdfText.Value otherwise
    function PdfTextValueByName(const AKey: PDFString): PDFString; {$ifdef HASINLINE}inline;{$endif}
    /// fast find a textual value by its name
    // - return '' if not found, the TPdfTextUTF8.Value otherwise
    function PdfTextUTF8ValueByName(const AKey: PDFString): RawUTF8; {$ifdef HASINLINE}inline;{$endif}
    /// fast find a textual value by its name
    // - return '' if not found, the TPdfTextString.Value otherwise
    function PdfTextStringValueByName(const AKey: PDFString): string; {$ifdef HASINLINE}inline;{$endif}
    /// fast find a numerical (floating-point) value by its name
    function PdfRealByName(const AKey: PDFString): TPdfReal; {$ifdef HASINLINE}inline;{$endif}
    /// fast find a name value by its name
    function PdfNameByName(const AKey: PDFString): TPdfName; {$ifdef HASINLINE}inline;{$endif}
    /// fast find a dictionary value by its name
    function PdfDictionaryByName(const AKey: PDFString): TPdfDictionary; {$ifdef HASINLINE}inline;{$endif}
    /// fast find an array value by its name
    function PdfArrayByName(const AKey: PDFString): TPdfArray; {$ifdef HASINLINE}inline;{$endif}
    /// add a specified Key / Value pair to the dictionary
    // - create PdfDictionaryElement with given key and value, and add it to list
    // - if the element exists, replace value of element by given value
    // - internal items are local to the framework, and not to be saved to the PDF content
    procedure AddItem(const AKey: PDFString; AValue: TPdfObject; AInternal: Boolean=false); overload;
    /// add a specified Key / Value pair (of type TPdfName) to the dictionary
    procedure AddItem(const AKey, AValue: PDFString); overload; {$ifdef HASINLINE}inline;{$endif}
    /// add a specified Key / Value pair (of type TPdfNumber) to the dictionary
    procedure AddItem(const AKey: PDFString; AValue: integer); overload; {$ifdef HASINLINE}inline;{$endif}
    /// add a specified Key / Value pair (of type TPdfText) to the dictionary
    procedure AddItemText(const AKey, AValue: PDFString); overload; {$ifdef HASINLINE}inline;{$endif}
    /// add a specified Key / Value pair (of type TPdfTextUTF8) to the dictionary
    // - the value can be any UTF-8 encoded text: it will be written as
    // Unicode hexadecimal to the PDF stream, if necessary
    procedure AddItemTextUTF8(const AKey: PDFString; const AValue: RawUTF8); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// add a specified Key / Value pair (of type TPdfTextUTF8) to the dictionary
    // - the value is a generic VCL string: it will be written as
    // Unicode hexadecimal to the PDF stream, if necessary
    procedure AddItemTextString(const AKey: PDFString; const AValue: string); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// remove the element specified by its Key from the dictionary
    // - if the element does not exist, do nothing
    procedure RemoveItem(const AKey: PDFString);
    /// retrieve any dictionary element
    property Items[Index: integer]: TPdfDictionaryElement read GetItems; default;
    /// retrieve the dictionary element count
    property ItemCount: integer read GetItemCount;
    /// retrieve the associated Object Manager
    property ObjectMgr: TPdfObjectMgr read FObjectMgr;
    /// retrieve the type of the pdfdictionary object, i.e. the 'Type' property name
    property TypeOf: PDFString read getTypeOf;
    /// direct access to the internal TList instance
    // - not to be used normally
    property List: TList read FArray;
  end;

  /// a temporary memory stream, to be stored into the PDF content
  // - typicaly used for the page content
  // - can be compressed, if the FlateDecode filter is set
  TPdfStream = class(TPdfObject)
  protected
    FAttributes: TPdfDictionary;
    FSecondaryAttributes: TPdfDictionary;
{$ifdef USE_PDFSECURITY}
    FDoNotEncrypt: boolean;
{$endif}
    FFilter: PDFString;
    FWriter: TPdfWrite;
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    /// create the temporary memory stream
    // - an optional DontAddToFXref is available, if you don't want to add
    // this object to the main XRef list of the PDF file
    constructor Create(ADoc: TPdfDocument; DontAddToFXref: boolean=false); reintroduce;
    /// release the memory stream
    destructor Destroy; override;
    /// retrieve the associated attributes, e.g. the stream Length
    property Attributes: TPdfDictionary read FAttributes;
    /// retrieve the associated buffered writer
    // - use this TPdfWrite instance to write some data into the stream
    property Writer: TPdfWrite read FWriter;
    /// retrieve the associated filter name
    property Filter: PDFString read FFilter write FFilter;
  end;

  /// used to handle object which are not defined in this library
  TPdfBinary = class(TPdfObject)
  protected
    FStream: TMemoryStream;
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    /// create the instance, i.e. its associated stream
    constructor Create; override;
    /// release the instance
    destructor Destroy; override;
    /// the associated memory stream, used to store the corresponding data
    // - the content of this stream will be written to the resulting
    property Stream: TMemoryStream read FStream;
  end;

  TPdfXref = class;
  TPdfObjectStream = class;

  /// the Trailer of the PDF File
  TPdfTrailer = class(TObject)
  private
    FAttributes: TPdfDictionary;
    FXrefAddress: integer;
    FCrossReference: TPdfStream;
    FObjectStream: TPdfObjectStream;
    FXRef: TPdfXref;
  protected
    procedure WriteTo(var W: TPdfWrite);
  public
    constructor Create(AObjectMgr: TPdfObjectMgr);
    destructor Destroy; override;
    procedure ToCrossReference(Doc: TPdfDocument);
    property XrefAddress: integer read FXrefAddress write FXrefAddress;
    property Attributes: TPdfDictionary read FAttributes;
  end;

  /// store one entry in the XRef list of the PDF file
  TPdfXrefEntry = class(TObject)
  private
    FEntryType: PDFString;
    FByteOffset: integer;
    FGenerationNumber: integer;
    FObjectStreamIndex: integer;
    FValue: TPdfObject;
  public
    /// create the entry, with the specified value
    // - if the value is nil (e.g. root entry), the type is 'f' (PDF_FREE_ENTRY),
    // otherwise the entry type is 'n' (PDF_IN_USE_ENTRY)
    constructor Create(AValue: TPdfObject);
    /// release the memory, and the associated value, if any
    destructor Destroy; override;
    /// write the XRef list entry
    procedure SaveToPdfWrite(var W: TPdfWrite);
    /// return either 'f' (PDF_FREE_ENTRY), either 'n' (PDF_IN_USE_ENTRY)
    property EntryType: PDFString read FEntryType write FEntryType;
    /// the position (in bytes) in the PDF file content stream
    // - to be ignored if ObjectStreamIndex>=0
    property ByteOffset: integer read FByteOffSet;
    /// the index of this object in the global compressed /ObjStm object stream
    // - equals -1 by default, i.e. if stored within the main file content stream
    property ObjectStreamIndex: Integer read FObjectStreamIndex;
    /// the associated Generation Number
    // - mostly 0, or 65535 (PDF_MAX_GENERATION_NUM) for the root 'f' entry
    property GenerationNumber: integer read FGenerationNumber write FGenerationNumber;
    /// the associated PDF object
    property Value: TPdfObject read FValue;
  end;

  /// store the XRef list of the PDF file
  TPdfXref = class(TPdfObjectMgr)
  private
    FXrefEntries: TList;
    function GetItem(ObjectID: integer): TPdfXrefEntry; {$ifdef HASINLINE}inline;{$endif}
    function GetItemCount: integer; {$ifdef HASINLINE}inline;{$endif}
  protected
    procedure WriteTo(var W: TPdfWrite);
  public
    /// initialize the XRef object list
    // - create first a void 'f' (PDF_FREE_ENTRY) as root
    constructor Create;
    /// release instance memory and all associated XRef objects
    destructor Destroy; override;
    /// register object to the xref table, and set corresponding object ID
    procedure AddObject(AObject: TPdfObject); override;
    /// retrieve an object from its object ID
    function GetObject(ObjectID: integer): TPdfObject; override;
    /// retrieve a XRef object instance, from its object ID
    property Items[ObjectID: integer]: TPdfXrefEntry read GetItem; default;
    /// retrieve the XRef object count
    property ItemCount: integer read GetItemCount;
  end;

  /// any object stored to the PDF file
  // - these objects are the main unit of the PDF file content
  // - these objects are written in the PDF file, followed by a "xref" table
  TPdfXObject = class(TPdfStream);

  /// generic PDF Outlines entries, stored as a PDF dictionary
  TPdfOutlines = class(TPdfDictionary);

  /// generic PDF Optional Content entry
  TPdfOptionalContentGroup = class(TPdfDictionary);

  TPdfInfo = class;
  TPdfCatalog = class;
  TPdfDestination = class;
  TPdfOutlineEntry = class;
  TPdfOutlineRoot = class;
  TPdfPage = class;
  TPdfPageClass = class of TPdfPage;

  /// potential font styles
  TPdfFontStyle = (pfsBold, pfsItalic, pfsUnderline, pfsStrikeOut);
  /// set of font styles
  TPdfFontStyles = set of TPdfFontStyle;

  /// the main class of the PDF engine, processing the whole PDF document
  TPdfDocument = class(TObject)
  protected
    FRoot: TPdfCatalog;
    FCurrentPages: TPdfDictionary;
    FOutputIntents: TPdfArray;
    FMetaData: TPdfStream;
    FCanvas: TPdfCanvas;
    FTrailer: TPdfTrailer;
    FXref: TPdfXref;
    FInfo: TPdfInfo;
    FFontList: TList;
    FObjectList: TList;
    FOutlineRoot: TPdfOutlineRoot;
    FStructTree: TPdfDictionary;
    FXObjectList: TPdfArray;
    FDefaultPageWidth: cardinal;
    FDefaultPageHeight: Cardinal;
    FDefaultPaperSize: TPDFPaperSize;
    FCompressionMethod: TPdfCompressionMethod;
    FUseOutlines: boolean;
    FUseOptionalContent: boolean;
    FCharSet: integer;
    FCodePage: cardinal;
    FTrueTypeFonts: TRawUTF8DynArray;
    FTrueTypeFontLastName: RawUTF8;
    FTrueTypeFontLastIndex: integer;
    FDC: HDC;
    FScreenLogPixels: Integer;
    FPrinterPxPerInch: TPoint;
    FStandardFontsReplace: boolean;
    fEmbeddedTTF: boolean;
    fEmbeddedWholeTTF: boolean;
    fEmbeddedTTFIgnore: TRawUTF8List;
    fRawPages: TList;
    {$ifdef USE_UNISCRIBE}
    fUseUniscribe: boolean;
    {$endif}
    fSelectedDCFontOld: HDC;
    fForceJPEGCompression: Integer;
    fForceNoBitmapReuse: boolean;
    fUseFontFallBack: boolean;
    fFontFallBackIndex: integer;
    /// a list of Bookmark text keys, associated to a TPdfDest object
    fBookMarks: TRawUTF8List;
    fMissingBookmarks: TRawUTF8List;
    /// internal temporary variable - used by CreateOutline
    fLastOutline: TPdfOutlineEntry;
    fFileFormat: TPdfFileFormat;
    fPDFA1: boolean;
    fSaveToStreamWriter: TPdfWrite;
    {$ifdef USE_PDFSECURITY}
    fEncryption: TPdfEncryption;
    fFileID: TMD5Digest;
    fEncryptionObject: TPdfDictionary;
    fCurrentObjectNumber: integer;
    fCurrentGenerationNumber: integer;
    {$endif USE_PDFSECURITY}
    function GetGeneratePDF15File: boolean;
    procedure SetGeneratePDF15File(const Value: boolean);
    function GetInfo: TPdfInfo;     {$ifdef HASINLINE}inline;{$endif}
    function GetOutlineRoot: TPdfOutlineRoot; {$ifdef HASINLINE}inline;{$endif}
    procedure SetStandardFontsReplace(const Value: boolean); {$ifdef HASINLINE}inline;{$endif}
    function GetEmbeddedTTFIgnore: TRawUTF8List;
    procedure SetDefaultPaperSize(const Value: TPDFPaperSize);
    procedure SetDefaultPageHeight(const Value: cardinal);
    procedure SetDefaultPageWidth(const Value: cardinal);
    procedure SetUseOptionalContent(const Value: boolean);
    procedure SetPDFA1(const Value: boolean);
    function GetDefaultPageLandscape: boolean;
    procedure SetDefaultPageLandscape(const Value: boolean);
    procedure SetFontFallBackName(const Value: string);
    function GetFontFallBackName: string;
  protected
    /// can be useful in descendant objects in other units
    fTPdfPageClass: TPdfPageClass;
    procedure RaiseInvalidOperation;
    procedure CreateInfo;
    /// get the PostScript Name of a TrueType Font
    // - use the Naming Table ('name') of the TTF content if not 7 bit ascii
    function TTFFontPostcriptName(aFontIndex: integer; AStyle: TPdfFontStyles;
      AFont: TPdfFontTrueType): PDFString;
    /// register the font in the font list
    procedure RegisterFont(aFont: TPdfFont);
    /// get the PDF font, from its internal PDF name (e.g. 'Helvetica-Bold')
    // - if the specified font exists in the font list, returns the corresponding object
    // - if the font doesn't exist yet, returns NIL
    function GetRegisteredNotTrueTypeFont(const APDFFontName: PDFString): TPdfFont;
    /// get the supplied TrueType Font from the internal font list
    // - warning: the font index is FTrueTypeFonts.IndexOf(AName)+1, since
    // font index 0 is reserved for all not True Type fonts
    // - if the true type font doesn't exist yet, returns NIL
    // - always return the WinAnsi version of the font: the caller has to
    // use the UnicodeFont property to get the corresponding Unicode aware
    // version, if it was used
    function GetRegisteredTrueTypeFont(AFontIndex: integer;
      AStyle: TPdfFontStyles; ACharSet: byte): TPdfFont; overload;
    /// get the supplied TrueType Font from the internal font list
    // - if the true type font doesn't exist yet, returns NIL
    function GetRegisteredTrueTypeFont(const AFontLog: TLogFontW): TPdfFont; overload;
    /// find an index of in FTrueTypeFonts[]
    function GetTrueTypeFontIndex(const AName: RawUTF8): integer;
    // select the specified font object, then return the fDC value
    function GetDCWithFont(TTF: TPdfFontTrueType): HDC;
    /// release the current document content
    procedure FreeDoc;
  public
    /// create the PDF document instance, with a Canvas and a default A4 paper size
    // - the current charset and code page are retrieved from the SysLocale
    // value, so the PDF engine is MBCS ready
    // - note that only Win-Ansi encoding allows use of embedded standard fonts
    // - you can specify a Code Page to be used for the PDFString encoding;
    // by default (ACodePage left to 0), the current system code page is used
    // - you can create a PDF/A-1 compliant document by setting APDFA1 to true
    // - you can set an encryption instance, by using TPdfEncryption.New()
    constructor Create(AUseOutlines: Boolean=false; ACodePage: integer=0;
      APDFA1: boolean=false
      {$ifdef USE_PDFSECURITY}; AEncryption: TPdfEncryption=nil{$endif}); reintroduce;
    /// release the PDF document instance
    destructor Destroy; override;
    /// create a new document
    // - this method is called first, by the Create constructor
    // - you can call it multiple time if you want to reset the whole document content
    procedure NewDoc;
    /// add a Page to the current PDF document
    function AddPage: TPdfPage; virtual;
    /// create a Pages object
    // - Pages objects can be nested, to save memory used by the Viewer
    // - only necessary if you have more than 8000 pages (this method is called
    // by TPdfDocument.NewDoc, so you shouldn't have to use it)
    function CreatePages(Parent: TPdfDictionary): TPdfDictionary;
    /// register an object (typicaly a TPdfImage) to the PDF document
    // - returns the internal index as added in FXObjectList[]
    function RegisterXObject(AObject: TPdfXObject; const AName: PDFString): integer;
    /// add then register an object (typicaly a TPdfImage) to the PDF document
    // - returns the internal index as added in FXObjectList[]
    function AddXObject(const AName: PDFString; AXObject: TPdfXObject): integer;
    /// save the PDF file content into a specified Stream
    procedure SaveToStream(AStream: TStream; ForceModDate: TDateTime=0); virtual;
    /// prepare to save the PDF file content into a specified Stream
    // - is called by SaveToStream() method
    // - you can then append other individual pages with SaveToStreamCurrentPage
    // to avoid most resource usage (e.g. for report creation)
    // - shall be finished by a SaveToStreamDirectEnd call
    procedure SaveToStreamDirectBegin(AStream: TStream; ForceModDate: TDateTime=0);
    /// save the current page content to the PDF file
    // - shall be made one or several times after a SaveToStreamDirectBegin() call
    // and before a final SaveToStreamDirectEnd call
    // - see TPdfDocumentGDI.SaveToStream() in this unit, and
    // TGDIPages.ExportPDFStream() in mORMotReport.pas for real use cases
    // - you can set FlushCurrentPageNow=true to force the current page to be
    // part of the flushed content
    procedure SaveToStreamDirectPageFlush(FlushCurrentPageNow: boolean=false); virtual;
    /// prepare to save the PDF file content into a specified Stream
    // - shall be made once after a SaveToStreamDirectBegin() call
    // - is called by SaveToStream() method
    procedure SaveToStreamDirectEnd;
    /// save the PDF file content into a specified file
    // - return FALSE on any writing error (e.g. if the file is opened in the
    // Acrobar Reader)
    function SaveToFile(const aFileName: TFileName): boolean;
    /// retrieve a XObject from its name
    // - this method will handle also the Virtual Objects
    function GetXObject(const AName: PDFString): TPdfXObject;
    /// retrieve a XObject index from its name
    // - this method won't handle the Virtual Objects
    function GetXObjectIndex(const AName: PDFString): integer;
    {$ifdef USE_BITMAP}
    /// retrieve a XObject TPdfImage index from its picture attributes
    // - returns '' if this image is not already there
    // - uses 4 hash codes, created with 4 diverse seeds, in order to avoid
    // false positives
    function GetXObjectImageName(const Hash: THash128Rec; Width, Height: Integer): PDFString;
    {$endif USE_BITMAP}
    /// wrapper to create an annotation
    // - the annotation is set to a specified position of the current page
    function CreateAnnotation(AType: TPdfAnnotationSubType; const ARect: TPdfRect;
      BorderStyle: TPdfAnnotationBorder=abSolid; BorderWidth: integer=1): TPdfDictionary;
    /// wrapper to create a Link annotation, specified by a bookmark
    // - the link is set to a specified rectangular position of the current page
    // - if the bookmark name is not existing (i.e. if it no such name has been
    // defined yet via the CreateBookMark method), it's added to the internal
    // fMissingBookmarks list, and will be linked at CreateBookMark method call
    function CreateLink(const ARect: TPdfRect; const aBookmarkName: RawUTF8;
      BorderStyle: TPdfAnnotationBorder=abSolid; BorderWidth: integer=1): TPdfDictionary;
    /// wrapper to create a hyper-link, with a specific URL value
    function CreateHyperLink(const ARect: TPdfRect; const url: RawUTF8;
      BorderStyle: TPdfAnnotationBorder=abSolid; BorderWidth: integer=0): TPdfDictionary;
    /// create an Outline entry at a specified position of the current page
    // - the outline tree is created from the specified numerical level (0=root),
    // just after the item added via the previous CreateOutline call
    // - the title is a generic VCL string, to handle fully Unicode support
    function CreateOutline(const Title: string; Level: integer; TopPosition: Single): TPdfOutlineEntry;
    /// create a Destination
    // - the current PDF Canvas page is associated with this destination object
    function CreateDestination: TPdfDestination;
    /// create an internal bookmark entry at a specified position of the current page
    // - the current PDF Canvas page is associated with the destination object
    // - a dtXYZ destination with the corresponding TopPosition Y value is defined
    // - the associated bookmark name must be unique, otherwise an exception is raised
    procedure CreateBookMark(TopPosition: Single; const aBookmarkName: RawUTF8);
    {$ifdef USE_BITMAP}
    /// create an image from a supplied bitmap
    // - returns the internal XObject name of the resulting TPDFImage
    // - if you specify a PPdfBox to draw the image at the given position/size
    // - if the same bitmap content is sent more than once, the TPDFImage will
    // be reused (it will therefore spare resulting pdf file space) - if the
    // ForceNoBitmapReuse is FALSE
    // - if ForceCompression property is set, the picture will be stored as a JPEG
    // - you can specify a clipping rectangle region as ClipRc parameter
    function CreateOrGetImage(B: TBitmap; DrawAt: PPdfBox=nil; ClipRc: PPdfBox=nil): PDFString;
    {$endif USE_BITMAP}
    // create a new optional content group (layer)
    // - returns a TPdfOptionalContentGroup needed for TPDFCanvas.BeginMarkedContent
    // - if ParentContentGroup is not nil, the new content group is a subgroup to ParentContentGroup
    // - Title is the string shown in the PDF Viewer
    // - Visible controls the initial state of the content group
    function CreateOptionalContentGroup(ParentContentGroup: TPdfOptionalContentGroup;
      const Title: string; Visible: Boolean=true): TPdfOptionalContentGroup;
    // create a Radio Optional ContentGroup
    // - ContentGroups is a array of TPdfOptionalContentGroups which should behave like
    // radiobuttons, i.e. only one active at a time
    // - visibility must be set with CreateOptionalContentGroup, only one group should be visible
    procedure CreateOptionalContentRadioGroup(const ContentGroups: array of TPdfOptionalContentGroup);
    /// retrieve the current PDF Canvas, associated to the current page
    property Canvas: TPdfCanvas read fCanvas;
    /// retrieve the PDF information, associated to the PDF document
    property Info: TPdfInfo read GetInfo;
    // retrieve the PDF Document Catalog, as root of the document's object hierarchy
    property Root: TPdfCatalog read fRoot;
    /// retrieve the PDF Outline, associated to the PDF document
    // - UseOutlines must be set to TRUE before any use of the OutlineRoot property
    property OutlineRoot: TPdfOutlineRoot read GetOutlineRoot;
    /// the default page width, used for new every page creation (i.e. AddPage method call)
    property DefaultPageWidth: cardinal read FDefaultPageWidth write SetDefaultPageWidth;
    /// the default page height, used for new every page creation (i.e. AddPage method call)
    property DefaultPageHeight: cardinal read FDefaultPageHeight write SetDefaultPageHeight;
    /// the default page orientation
    // - a call to this property will swap default page width and height if the
    // orientation is not correct
    property DefaultPageLandscape: boolean read GetDefaultPageLandscape write SetDefaultPageLandscape;
    /// the default page size, used for every new page creation (i.e. AddPage method call)
    // - a write to this property this will reset the default paper orientation
    // to Portrait: you must explicitely set DefaultPageLandscape to true, if needed
    property DefaultPaperSize: TPDFPaperSize read FDefaultPaperSize write SetDefaultPaperSize;
    /// the compression method used for page content storage
    // - is set by default to cmFlateDecode when the class instance is created
    property CompressionMethod: TPdfCompressionMethod read FCompressionMethod write FCompressionMethod;
    /// if set to TRUE, the used True Type fonts will be embedded to the PDF content
    // - not set by default, to save disk space and produce tiny PDF
    property EmbeddedTTF: boolean read fEmbeddedTTF write fEmbeddedTTF;
    /// you can add some font names to this list, if you want these fonts
    // NEVER to be embedded to the PDF file, even if the EmbeddedTTF property is set
    // - if you want to ignore all standard windows fonts, use:
    // !   EmbeddedTTFIgnore.Text := MSWINDOWS_DEFAULT_FONTS;
    property EmbeddedTTFIgnore: TRawUTF8List read GetEmbeddedTTFIgnore;
    /// if set to TRUE, the embedded True Type fonts will be totaly Embeddeded
    // - by default, is set to FALSE, meaning that a subset of the TTF font is
    // stored into the PDF file, i.e. only the used glyphs are stored
    // - this option is only available if running on Windows XP or later
    property EmbeddedWholeTTF: boolean read fEmbeddedWholeTTF write fEmbeddedWholeTTF;
    /// used to define if the PDF document will use outlines
    // - must be set to TRUE before any use of the OutlineRoot property
    property UseOutlines: boolean read FUseoutlines write FUseoutlines;
    // used to define if the PDF document will use optional content (layers)
    // - will also force PDF 1.5 as minimal file format
    // - must be set to TRUE before calling NewDoc
    // - warning: setting a value to this propery after creation will call the
    // NewDoc method, therefore will erase all previous content and pages
    // (including Info properties)
    property UseOptionalContent: boolean read FUseOptionalContent write SetUseOptionalContent;
    /// the current Code Page encoding used for this PDF Document
    property CodePage: cardinal read FCodePage;
    /// the current CharSet used for this PDF Document
    property CharSet: integer read FCharSet;
    /// set if the PDF engine must use standard fonts substitution
    // - if TRUE, 'Arial', 'Times New Roman' and 'Courier New' will be
    // replaced by the corresponding internal Type 1 fonts, defined in the Reader
    // - only works with current ANSI_CHARSET, i.e. if you want to display
    // some other unicode characters, don't enable this property: all non WinAnsi
    // glyphs would be replaced by a '?' sign
    // - default value is false (i.e. not embedded standard font)
    property StandardFontsReplace: boolean read FStandardFontsReplace write SetStandardFontsReplace;
{$ifdef USE_UNISCRIBE}
    /// set if the PDF engine must use the Windows Uniscribe API to
    // render Ordering and/or Shaping of the text
    // - useful for Hebrew, Arabic and some Asiatic languages handling
    // - set to FALSE by default, for faster content generation
    // - you can set this property temporary to TRUE, when using the Canvas
    // property, but this property must be set appropriately before the content
    // generation if you use any TPdfDocumentGdi.VCLCanvas text output with
    // such scripting (since the PDF rendering is done once just before the
    // saving, e.g. before SaveToFile() or SaveToStream() methods calls)
    // - the PDF engine don't handle Font Fallback yet: the font you use
    // must contain ALL glyphs necessary for the supplied unicode text - squares
    // or blanks will be drawn for any missing glyph/character
    property UseUniscribe: boolean read fUseUniscribe write fUseUniscribe;
{$endif}
    /// used to define if the PDF document will handle "font fallback" for
    // characters not existing in the current font: it will avoid rendering
    // block/square symbols instead of the correct characters (e.g. for Chinese text)
    // - will use the font specified by FontFallBackName property to add any
    // Unicode glyph not existing in the currently selected font
    // - default value is TRUE
    property UseFontFallBack: boolean read fUseFontFallBack write fUseFontFallBack;
    /// set the font name to be used for missing characters
    // - used only if UseFontFallBack is TRUE
    // - default value is 'Arial Unicode MS', if existing
    property FontFallBackName: string read GetFontFallBackName write SetFontFallBackName;

    /// this property can force saving all canvas bitmaps images as JPEG
    // - handle bitmaps added by VCLCanvas/TMetaFile and bitmaps added as TPdfImage
    // - by default, this property is set to 0 by the constructor of this class,
    // meaning that the JPEG compression is not forced, and the engine will use
    // the native resolution of the bitmap - in this case, the resulting
    // PDF file content will be bigger in size (e.g. use this for printing)
    // - 60 is the prefered way e.g. for publishing PDF over the internet
    // - 80/90 is a good ratio if you want to have a nice PDF to see on screen
    // - of course, this doesn't affect vectorial (i.e. emf) pictures
    property ForceJPEGCompression: integer read fForceJPEGCompression write fForceJPEGCompression;
    /// this property can force all canvas bitmaps to be stored directly
    // - by default, the library will try to match an existing same bitmap
    // content, and reuse the existing pdf object - you can set this property
    // for a faster process, if you do not want to use this feature
    property ForceNoBitmapReuse: boolean read fForceNoBitmapReuse write fForceNoBitmapReuse;
    /// direct read-only access to all corresponding TPdfPage
    // - can be useful in inherited classe
    property RawPages: TList read fRawPages;
    /// the resolution used for pixel to PDF coordinates conversion
    // - by default, contains the Number of pixels per logical inch
    // along the screen width
    // - you can override this value if you really need additional resolution
    // for your bitmaps and such - this is useful only with TPdfDocumentGDI and
    // its associated TCanvas: all TPdfDocument native TPdfCanvas methods use
    // the native resolution of the PDF, i.e. more than 7200 DPI (since we
    // write coordinates with 2 decimals per point - which is 1/72 inch)
    property ScreenLogPixels: Integer read FScreenLogPixels write FScreenLogPixels;
    /// is TRUE if the file was created in order to be PDF/A-1 compliant
    // - set APDFA1 parameter to true for Create constructor in order to use it
    // - warning: setting a value to this propery after creation will call the
    // NewDoc method, therefore will erase all previous content and pages
    // (including Info properties)
    property PDFA1: boolean read fPDFA1 write SetPDFA1;
    /// set to TRUE to force PDF 1.5 format, which may produce smaller files
    property GeneratePDF15File: boolean read GetGeneratePDF15File write SetGeneratePDF15File;
  end;

  /// a PDF page
  TPdfPage = class(TPdfDictionary)
  private
    function GetPageLandscape: Boolean;
    procedure SetPageLandscape(const Value: Boolean);
  protected
    fDoc: TPdfDocument;
    FMediaBox: TPdfArray;
    FWordSpace: Single;
    FCharSpace: Single;
    FFontSize: Single;
    FFont: TPdfFont;
    FLeading: Single;
    FHorizontalScaling: Single;
    procedure SetWordSpace(Value: Single);
    procedure SetCharSpace(Value: Single);
    procedure SetFontSize(Value: Single);
    procedure SetHorizontalScaling(Value: Single);
    procedure SetLeading(Value: Single);
    procedure SetPageWidth(AValue: integer); virtual;
    procedure SetPageHeight(AValue: integer); virtual;
    function GetPageWidth: Integer;
    function GetPageHeight: Integer;
    function GetResources(const AName: PDFString): TPdfDictionary; {$ifdef HASINLINE}inline;{$endif}
  public
    /// create the page with its internal VCL Canvas
    constructor Create(ADoc: TPdfDocument); reintroduce; virtual;
    /// calculate width of specified text according to current attributes
    // - this function is compatible with MBCS strings
    function TextWidth(const Text: PDFString): Single;
    /// calculate the number of chars which can be displayed in the specified
    // width, according to current attributes
    // - this function is compatible with MBCS strings, and returns
    // the index in Text, not the glyphs index
    function MeasureText(const Text: PDFString; Width: Single): integer;
  public
    /// retrieve or set the word Space attribute, in PDF coordinates of 1/72 inch
    property WordSpace: Single read FWordSpace write SetWordSpace;
    /// retrieve or set the Char Space attribute, in PDF coordinates of 1/72 inch
    property CharSpace: Single read FCharSpace write SetCharSpace;
    /// retrieve or set the Horizontal Scaling attribute, in PDF coordinates of 1/72 inch
    property HorizontalScaling: Single read FHorizontalScaling write SetHorizontalScaling;
    /// retrieve or set the text Leading attribute, in PDF coordinates of 1/72 inch
    property Leading: Single read FLeading write SetLeading;
    /// retrieve or set the font Size attribute, in system TFont.Size units
    property FontSize: Single read FFontSize write SetFontSize;
    /// retrieve the current used font
    // - for TPdfFontTrueType, this points not always to the WinAnsi version of
    // the Font, but can also point to the Unicode Version, if the last
    // drawn character by ShowText() was unicode - see TPdfWrite.AddUnicodeHexText
    property Font: TPdfFont read FFont write FFont;
    /// retrieve or set the current page width, in PDF coordinates of 1/72 inch
    property PageWidth: integer read GetPageWidth write SetPageWidth;
    /// retrieve or set the current page height, in PDF coordinates of 1/72 inch
    property PageHeight: integer read GetPageHeight write SetPageHeight;
    /// retrieve or set the paper orientation
    property PageLandscape: Boolean read GetPageLandscape write SetPageLandscape;
  end;

  /// is used to define how TMetaFile text positioning is rendered
  // - tpSetTextJustification will handle efficiently the fact that TMetaFileCanvas
  // used SetTextJustification() API calls to justify text: it will converted
  // to SetWordSpace() pdf rendering
  // - tpExactTextCharacterPositining will use the individual glyph positioning
  // information as specified within the TMetaFile content: resulting pdf size
  // will be bigger, but font kerning will be rendered as expected
  // - tpKerningFromAveragePosition will use global font kerning via
  // SetHorizontalScaling() pdf rendering
  TPdfCanvasRenderMetaFileTextPositioning = (
    tpKerningFromAveragePosition, tpSetTextJustification, tpExactTextCharacterPositining);

  /// is used to define how TMetaFile text is clipped
  // - by default, text will be clipped with the specified TEMRText.ptlReference
  // - you could set tcClipExplicit to clip following the specified rclBounds
  // - or tcAlwaysClip to use the current clipping region (if any)
  // - finally, tcNeverClip would disable whole text clipping process, which
  // has been reported to be preferred e.g. on Wine
  TPdfCanvasRenderMetaFileTextClipping = (
    tcClipReference, tcClipExplicit, tcAlwaysClip, tcNeverClip);

  {$ifdef USE_ARC}
  /// is used to define the TMetaFile kind of arc to be drawn
  TPdfCanvasArcType =(
    acArc, acArcTo, acArcAngle, acPie, acChoord);
  {$endif USE_ARC}

  /// access to the PDF Canvas, used to draw on the page
  TPdfCanvas = class(TObject)
  protected
    FContents: TPdfStream;
    FPage: TPdfPage;
    FPageFontList: TPdfDictionary;
    FDoc: TPdfDocument;
    // = 72/FDoc.FScreenLogPixels
    FFactor: single;
    // = ViewSize.cx/WinSize.cx*FFactor
    FFactorX: single;
    // = ViewSize.cy/WinSize.cy*FFactor
    FFactorY: single;
    // = (MulDiv(ViewOrg.x, WinSize.cx, ViewSize.cx) - WinOrg.x)*FFactor
    FOffsetX: single;
    // = FHeight - (MulDiv(ViewOrg.y, WinSize.cy, ViewSize.cy) - WinOrg.y)*FFactor
    FOffsetY: single;
    // = XOff,YOff parameters specified in RenderMetaFile()
    FOffsetXDef, FOffsetYDef: Single;
    // WorldTransform factor and offs
    FWorldFactorX, FWorldFactorY, FWorldOffsetX, FWorldOffsetY, FAngle,
    FWorldCos, FWorldSin: single;
    FDevScaleX, FDevScaleY: single;
    FWinSize, FViewSize: TSize;
    FWinOrg, FViewOrg: TPoint;
    FMappingMode: Integer;
    FEmfBounds: TRect;
    FPrinterPxPerInch: TPoint;
    FNewPath: Boolean;
    {$ifdef USE_UNISCRIBE}
    /// if Uniscribe-related methods must handle the text from right to left
    fRightToLeftText: Boolean;
    {$endif}
    /// parameters taken from RenderMetaFile() call
    fUseMetaFileTextPositioning: TPdfCanvasRenderMetaFileTextPositioning;
    fUseMetaFileTextClipping: TPdfCanvasRenderMetaFileTextClipping;
    fKerningHScaleBottom: Single;
    fKerningHScaleTop: Single;
    // some cache
    FPreviousRasterFontName: RawUTF8;
    FPreviousRasterFontIndex: integer;
    // result := FOffsetX + (X * fFactorX);
    function I2X(X: Integer): Single; overload;
    // result := FOffsetX + (X * fFactorX);
    function I2X(X: Single): Single; overload;
    // result := FOffsetY - Y * fFactorY;
    function I2Y(Y: Integer): Single; overload;
    // result := FOffsetY - Y * fFactorY;
    function I2Y(Y: Single): Single; overload;
    // wrapper call I2X() and I2Y() for conversion
    procedure LineToI(x, y: Integer); overload;
    procedure LineToI(x, y: Single); overload;
    // wrapper call I2X() and I2Y() for conversion
    procedure MoveToI(x, y: Integer); overload;
    procedure MoveToI(x, y: Single); overload;
    // wrapper call I2X() and I2Y() for conversion
    procedure CurveToCI(x1, y1, x2, y2, x3, y3: integer);
    // wrapper call I2X() and I2Y() for conversion
    procedure RoundRectI(x1,y1,x2,y2,cx,cy: integer);
   {$ifdef USE_ARC}
   procedure ARCI(centerx, centery, W, H, Sx, Sy, Ex, Ey: integer;
     clockwise: boolean; arctype: TPdfCanvasArcType; var position: TPoint);
   {$endif USE_ARC}
    // wrapper call I2X() and I2Y() for conversion (points to origin+size)
    function BoxI(Box: TRect; Normalize: boolean): TPdfBox; {$ifdef HASINLINE}inline;{$endif}
    // wrapper call I2X() and I2Y() for conversion
    procedure PointI(x, y: Single); {$ifdef HASINLINE}inline;{$endif}
    function RectI(Rect: TRect; Normalize: boolean): TPdfRect;
    procedure DrawXObjectPrepare(const AXObjectName: PDFString);
    // wrappers about offset calculation
    function ViewOffsetX(X: Single): Single;
    function ViewOffsetY(Y: Single): Single;
    function GetWorldFactorX: Single;
    function GetWorldFactorY: Single;
    property WorldFactorX: Single read GetWorldFactorX write FWorldFactorX;
    property WorldFactorY: Single read GetWorldFactorY write FWorldFactorY;
    // property getters
    function GetDoc: TPdfDocument;    {$ifdef HASINLINE}inline;{$endif}
    function GetPage: TPdfPage;       {$ifdef HASINLINE}inline;{$endif}
  public
    /// create the PDF canvas instance
    constructor Create(APdfDoc: TPdfDocument);

    /// pushes a copy of the entire graphics state onto the stack
    procedure GSave;                                             {  q   }
    /// restores the entire graphics state to its former value by popping
    // it from the stack
    procedure GRestore;                                          {  Q   }
    /// Modify the CTM by concatenating the specified matrix
    // - The current transformation matrix (CTM) maps positions from user
    // coordinates to device coordinates
    // - This matrix is modified by each application of the ConcatToCTM method
    // - CTM Initial value is  a matrix that transforms default user coordinates
    // to device coordinates
    // - since floating-point precision does make sense for a transformation
    // matrix, we added a custom decimal number parameter here
    procedure ConcatToCTM(a, b, c, d, e, f: Single; Decimals: Cardinal=6); {  cm  }

    /// Set the flatness tolerance in the graphics state
    // - see Section 6.5.1, "Flatness Tolerance" of the PDF 1.3 reference:
    // The flatness tolerance controls the maximum permitted distance in
    // device pixels between the mathematically correct path and an
    // approximation constructed from straight line segments
    // - Flatness is a number in the range 0 to 100; a value of 0 specifies
    // the output device's default flatness tolerance
    procedure SetFlat(flatness: Byte);                           {  i   }
    /// Set the line cap style in the graphics state
    // - The line cap style specifies the shape to be used at the
    // ends of open subpaths (and dashes, if any) when they are stroked
    procedure SetLineCap(linecap: TLineCapStyle);                {  J   }
    /// Set the line dash pattern in the graphics state
    // - The line dash pattern controls the pattern of dashes and gaps
    // used to stroke paths. It is specified by a dash array and a dash phase.
    // The dash array's elements are numbers that specify the lengths of
    // alternating dashes and gaps; the dash phase specifies the distance into
    // the dash pattern at which to start the dash. The elements of both the
    // dash array and the dash phase are expressed in user space units.
    // Before beginning to stroke a path, the dash array is cycled through,
    // adding up the lengths of dashes and gaps. When the accumulated length
    // equals the value specified by the dash phase, stroking of the path begins,
    // using the dash array cyclically from that point onward.
    procedure SetDash(const aarray: array of integer; phase: integer=0); {  d   }
    /// Set the line join style in the graphics state
    // - The  line join style specifies the shape to be used at the
    // corners of paths that are stroked
    procedure SetLineJoin(linejoin: TLineJoinStyle);             {  j   }
    /// Set the line width in the graphics state
    // - The line width parameter specifies the thickness of the line used
    // to stroke a path. It is a nonnegative number expressed in user space
    // units; stroking a path entails painting all points whose perpendicular
    // distance from the path in user space is less than or equal to half the
    // line width. The effect produced in device space depends on the current
    // transformation matrix (CTM) in effect at the time the path is stroked.
    // If the CTM specifies scaling by different factors in the x and y
    // dimensions, the thickness of stroked lines in device space will vary
    // according to their orientation. The actual line width achieved can differ
    // from the requested width by as much as 2 device pixels, depending on
    // the positions of lines with respect to the pixel grid.
    procedure SetLineWidth(linewidth: Single);                   {  w   }
    /// Set the miter limit in the graphics state
    // - When two line segments meet at a sharp angle and mitered joins have been
    // specified as the line join style, it is possible for the miter to extend
    // far beyond the thickness of the line stroking the path. The miter limit
    // imposes a maximum on the ratio of the miter length to the line width.
    // When the limit is exceeded, the join is converted from a miter to a bevel
    procedure SetMiterLimit(miterlimit: Single);                 {  M   }

    /// change the current coordinates position
    // - Begin a new subpath by moving the current point to coordinates
    // (x, y), omitting any connecting line segment. If the previous path
    // construction operator in the current path was also MoveTo(), the new MoveTo()
    // overrides it; no vestige of the previous MoveTo() call remains in the path.
    procedure MoveTo(x, y: Single);                              {  m   }
    /// Append a straight line segment from the current point to the point (x, y).
    // -  The new current point is (x, y)
    procedure LineTo(x, y: Single);                              {  l   }
    /// Append a cubic Bezier curve to the current path
    // - The curve extends from the current point to the point (x3, y3),
    // using (x1, y1) and (x2, y2) as the Bezier control points
    // - The new current point is (x3, y3)
    procedure CurveToC(x1, y1, x2, y2, x3, y3: Single);          {  c   }
    /// Append a cubic Bezier curve to the current path
    // - The curve extends from the current point to the point (x3, y3),
    // using the current point and (x2, y2) as the Bezier control points
    // - The new current point is (x3, y3)
    procedure CurveToV(x2, y2, x3, y3: Single);                  {  v   }
    /// Append a cubic Bezier curve to the current path
    // - The curve extends from the current point to the point (x3, y3),
    // using (x1, y1) and (x3, y3) as the Bezier control points
    // - The new current point is (x3, y3)
    procedure CurveToY(x1, y1, x3, y3: Single);                  {  y   }
    /// Append a rectangle to the current path as a complete subpath, with
    // lower-left corner (x, y) and dimensions  width and  height in user space
    procedure Rectangle(x, y, width, height: Single);            {  re  }
    /// Close the current subpath by appending a straight line segment
    // from the current point to the starting point of the subpath
    // - This operator terminates the current subpath; appending another
    // segment to the current path will begin a new subpath, even if the new
    // segment begins at the endpoint reached by the h operation
    // - If the current subpath is already closed or the current path is empty,
    // it does nothing
    procedure Closepath;                                         {  h   }
    /// End the path object without filling or stroking it
    // - This operator is a "path-painting no-op", used primarily for the
    // side effect of changing the clipping path
    procedure NewPath;                                           {  n   }
    /// Stroke the path
    procedure Stroke;                                            {  S   }
    /// Close and stroke the path
    // - This operator has the same effect as the sequence ClosePath; Stroke;
    procedure ClosePathStroke;                                   {  s   }
    /// Fill the path, using the nonzero winding number rule to determine
    // the region to fill
    procedure Fill;                                              {  f   }
    /// Fill the path, using the even-odd rule to determine the region to fill
    procedure EoFill;                                            {  f*  }
    /// Fill and then stroke the path, using the nonzero winding number rule
    // to determine the region to fill
    // - This produces the same result as constructing two identical path
    // objects, painting the first with Fill and the second with Stroke. Note,
    // however, that the filling and stroking portions of the operation consult
    // different values of several graphics state parameters, such as the color
    procedure FillStroke;                                        {  B   }
    /// Close, fill, and then stroke the path, using the nonzero winding number
    // rule to determine the region to fill
    // - This operator has the same effect as the sequence ClosePath; FillStroke;
    procedure ClosepathFillStroke;                               {  b   }
    /// Fill and then stroke the path, using the even-odd rule to determine
    // the region to fill
    // - This operator produces the same result as FillStroke, except that
    // the path is filled as if with Eofill instead of Fill
    procedure EofillStroke;                                      {  B*  }
    /// Close, fill, and then stroke the path, using the even-odd rule to
    // determine the region to fill
    // - This operator has the same effect as the sequence Closepath; EofillStroke;
    procedure ClosepathEofillStroke;                             {  b*  }
    /// Nonzero winding clipping path set
    // - Modify the current clipping path by intersecting it with the current path,
    // using the nonzero winding number rule to determine which regions
    // lie inside the clipping path
    // - The graphics state contains a clipping path that limits the regions of
    // the page affected by painting operators. The closed subpaths of this path
    // define the area that can be painted. Marks falling inside this area will
    // be applied to the page; those falling outside it will not. (Precisely what
    // is considered to be inside a path is discussed under "Filling", above.)
    // - The initial clipping path includes the entire page. Both clipping path
    // methods (Clip and EoClip) may appear after the last path construction operator
    // and before the path-painting operator that terminates a path object.
    // Although the clipping path operator appears before the painting operator,
    // it does not alter the clipping path at the point where it appears. Rather,
    // it modifies the effect of the succeeding painting operator. After the path
    // has been painted, the clipping path in the graphics state is set to the
    // intersection of the current clipping path and the newly constructed path.
    procedure Clip;                                              {  W   }
    /// Even-Odd winding clipping path set
    // - Modify the current clipping path by intersecting it with the current path,
    // using the even-odd rule to determine which regions lie inside the clipping path
    procedure EoClip;                                            {  W*  }

    /// Set the character spacing
    // - CharSpace is a number expressed in unscaled text space units.
    // - Character spacing is used by the ShowText and ShowTextNextLine methods
    // - Default value is 0
    procedure SetCharSpace(charSpace: Single);                   {  Tc  }
    /// Set the word spacing
    // - WordSpace is a number expressed in unscaled text space units
    // - word spacing is used by the ShowText and ShowTextNextLine methods
    // - Default value is 0
    procedure SetWordSpace(wordSpace: Single);                   {  Tw  }
    /// Set the horizontal scaling to (scale/100)
    // - hScaling is a number specifying the percentage of the normal width
    // - Default value is 100 (e.g. normal width)
    procedure SetHorizontalScaling(hScaling: Single);              {  Tz  }
    /// Set the text leading, Tl, to the specified leading value
    // - leading which is a number expressed in unscaled text space units;
    // it specifies the vertical distance between the baselines of adjacent
    // lines of text
    // - Text leading is used only by the MoveToNextLine and ShowTextNextLine methods
    // - you can force the next line to be just below the current one by calling:
    // ! SetLeading(Attributes.FontSize);
    // - Default value is 0
    procedure SetLeading(leading: Single);                       {  TL  }
    /// Set the font, Tf, to  font and the font size, Tfs , to size.
    // - font is the name of a font resource in the Font subdictionary of the
    // current resource dictionary (e.g. 'F0')
    // - size is a number representing a scale factor
    // - There is no default value for either font or size; they must be specified
    // using this method before any text is shown
    procedure SetFontAndSize(const fontshortcut: PDFString; size: Single);    {  Tf  }
      {$ifdef HASINLINE}inline;{$endif}
    /// Set the text rendering mode
    // - the text rendering mode determines whether text is stroked, filled,
    // or used as a clipping path
    procedure SetTextRenderingMode(mode: TTextRenderingMode);    {  Tr  }
    /// Set the text rise, Trise, to the specified value
    // - rise is a number expressed in unscaled text space units, which
    // specifies the distance, in unscaled text space units, to move the
    // baseline up or down from its default location. Positive values of
    // text rise move the baseline up. Adjustments to the baseline are
    // useful for drawing superscripts or subscripts. The default location of
    // the baseline can be restored by setting the text rise to 0.
    // - Default value is 0
    procedure SetTextRise(rise: word);                           {  Ts  }
    /// Begin a text object
    // - Text objects cannot be nested
    procedure BeginText;   {$ifdef HASINLINE}inline;{$endif}     {  BT  }
    /// End a text object, discarding the text matrix
    procedure EndText;     {$ifdef HASINLINE}inline;{$endif}     {  ET  }
    /// Move to the start of the next line, offset from the start of the current
    // line by (tx ,ty)
    // - tx and ty are numbers expressed in unscaled text space units
    procedure MoveTextPoint(tx, ty: Single); {$ifdef HASINLINE}inline;{$endif} {  Td  }
    /// set the Text Matrix to a,b,c,d and the text line Matrix x,y
    procedure SetTextMatrix(a, b, c, d, x, y: Single);           {  Tm  }
    /// Move to the start of the next line
    procedure MoveToNextLine;                                    {  T*  }
    {$ifdef HASVARUSTRING}
    /// Show a text string
    // - text is expected to be Unicode encoded
    // - if NextLine is TRUE, moves to the next line and show a text string;
    // in this case, method as the same effect as MoveToNextLine; ShowText(s);
    procedure ShowText(const text: UnicodeString; NextLine: boolean=false); overload; inline; {  Tj  or ' }
    {$endif}
    /// Show a text string
    // - text is expected to be Ansi-Encoded, in the current CharSet; if
    // some Unicode or MBCS conversion is necessary, it will be notified to the
    // corresponding
    // - if NextLine is TRUE, moves to the next line and show a text string;
    // in this case, method as the same effect as MoveToNextLine; ShowText(s);
    procedure ShowText(const text: PDFString; NextLine: boolean=false); overload; {  Tj  or ' }
    /// Show an Unicode Text string
    // - if NextLine is TRUE, moves to the next line and show a text string;
    // in this case, method as the same effect as MoveToNextLine; ShowText(s);
    procedure ShowText(PW: PWideChar; NextLine: boolean=false); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// Show an Unicode Text string, encoded as Glyphs or the current font
    // - PW must follow the ETO_GLYPH_INDEX layout, i.e. refers to an array as
    // returned from the GetCharacterPlacement: all glyph indexes are 16-bit values
    procedure ShowGlyph(PW: PWord; Count: integer); {$ifdef HASINLINE}inline;{$endif}
    /// Paint the specified XObject
    procedure ExecuteXObject(const xObject: PDFString);                   {  Do  }

    /// Set the color space to a Device-dependent RGB value
    // - this method set the color to use for nonstroking operations
    procedure SetRGBFillColor(Value: TPdfColor);                 {  rg  }
    /// Set the color space to a Device-dependent RGB value
    // - this method set the color to use for stroking operations
    procedure SetRGBStrokeColor(Value: TPdfColor);               {  RG  }
    /// Set the color space to a CMYK percent value
    // - this method set the color to use for nonstroking operations
    procedure SetCMYKFillColor(C, M, Y, K: integer);                 {  k  }
    /// Set the color space to a CMYK value
    // - this method set the color to use for stroking operations
    procedure SetCMYKStrokeColor(C, M, Y, K: integer);               {  K  }

    /// assign the canvas to the specified page
    procedure SetPage(APage: TPdfPage); virtual;
    /// set the current font for the PDF Canvas
    procedure SetPDFFont(AFont: TPdfFont; ASize: Single);
    /// set the current font for the PDF Canvas
    // - expect the font name to be either a standard embedded font
    // ('Helvetica','Courier','Times') or its Windows equivalency (i.e.
    // 'Arial','Courier New','Times New Roman'), either a UTF-8 encoded
    // True Type font name available on the system
    // - if no CharSet is specified (i.e. if it remains -1), the current document
    // CharSet parameter is used
    function SetFont(const AName: RawUTF8; ASize: Single; AStyle: TPdfFontStyles;
      ACharSet: integer=-1; AForceTTF: integer=-1; AIsFixedWidth: boolean=false): TPdfFont; overload;
    /// set the current font for the PDF Canvas
    // - this method use the Win32 structure that defines the characteristics
    // of the logical font
    function SetFont(ADC: HDC; const ALogFont: TLogFontW; ASize: single): TPdfFont; overload;

    /// show some text at a specified page position
    procedure TextOut(X, Y: Single; const Text: PDFString);
    /// show some unicode text at a specified page position
    procedure TextOutW(X, Y: Single; PW: PWideChar);
    /// show the text in the specified rectangle and alignment
    // - optional clipping can be applied
    procedure TextRect(ARect: TPdfRect; const Text: PDFString;
        Alignment: TPdfAlignment; Clipping: boolean);
    /// show the text in the specified rectangle and alignment
    // - text can be multiline, separated by CR + LF (i.e. #13#10)
    // - text can optionaly word wrap
    // - note: this method only work with embedded fonts by now, not true type
    // fonts (because it use text width measuring)
    procedure MultilineTextRect(ARect: TPdfRect;
        const Text: PDFString; WordWrap: boolean);
    /// draw the specified object (typicaly an image) with stretching
    procedure DrawXObject(X, Y, AWidth, AHeight: Single;
        const AXObjectName: PDFString);
    /// draw the specified object (typicaly an image) with stretching and clipping
    procedure DrawXObjectEx(X, Y, AWidth, AHeight: Single;
        ClipX, ClipY, ClipWidth, ClipHeight: Single; const AXObjectName: PDFString);
    /// draw an ellipse
    // - use Bezier curves internaly to draw the ellipse
    procedure Ellipse(x, y, width, height: Single);
    /// draw a rounded rectangle
    // - use Bezier curves internaly to draw the rounded rectangle
    procedure RoundRect(x1,y1,x2,y2,cx,cy: Single);
    /// calculate width of specified text according to current Canvas attributes
    // - works with MBCS strings
    function TextWidth(const Text: PDFString): Single;
    /// calculate width of specified text according to current Canvas attributes
    // - this function compute the raw width of the specified text, and won't
    // use HorizontalScaling, CharSpace nor WordSpace in its calculation
    function UnicodeTextWidth(PW: PWideChar): Single;
    /// calculate the number of chars which can be displayed in the specified
    // width, according to current attributes
    // - this function is compatible with MBCS strings, and returns
    // the index in Text, not the glyphs index
    // - note: this method only work with embedded fonts by now, not true type
    // fonts (because text width measuring is not yet implemented for them)
    function MeasureText(const Text: PDFString; AWidth: Single): integer;
    /// get the index of the next word in the supplied text
    // - this function is compatible with MBCS strings, and returns
    // the index in Text, not the glyphs index
    function GetNextWord(const S: PDFString; var Index: integer): PDFString;
    {$ifdef USE_METAFILE}
    /// draw a metafile content into the PDF page
    // - not 100% of content is handled yet, but most common are (even
    // metafiles embedded inside metafiles)
    // - UseSetTextJustification is to be set to true to ensure better rendering
    // if the EMF content used SetTextJustification() API call to justify text
    // - KerningHScaleBottom/KerningHScaleTop are limits below which and over
    // which Font Kerning is transformed into PDF Horizontal Scaling commands
    // - TextClipping can be set to fix some issues e.g. when using Wine
    procedure RenderMetaFile(MF: TMetaFile; ScaleX: Single=1.0; ScaleY: Single=0.0;
      XOff: single=0.0; YOff: single=0.0;
      TextPositioning: TPdfCanvasRenderMetaFileTextPositioning=tpSetTextJustification;
      KerningHScaleBottom: single=99.0; KerningHScaleTop: single=101.0;
      TextClipping: TPdfCanvasRenderMetaFileTextClipping=tcAlwaysClip);
    {$endif USE_METAFILE}
    // starts optional content (layer)
    // - Group must be registered with TPdfDocument.CreateOptionalContentGroup
    // - each BeginMarkedContent must have a corresponding EndMarkedContent
    // - nested BeginMarkedContent/EndMarkedContent are possible
    procedure BeginMarkedContent(Group: TPdfOptionalContentGroup);
    // ends optional content (layer)
    procedure EndMarkedContent;
  public
    /// retrieve the current Canvas content stream, i.e. where the PDF
    // commands are to be written to
    property Contents: TPdfStream read FContents;
    /// retrieve the current Canvas Page
    property Page: TPdfPage read GetPage;
    /// retrieve the associated PDF document instance which created this Canvas
    property Doc: TPdfDocument read GetDoc;
{$ifdef USE_UNISCRIBE}
    /// if Uniscribe-related methods must handle the text from right to left
    property RightToLeftText: Boolean read fRightToLeftText write fRightToLeftText;
{$endif}
  end;

  /// common ancestor to all dictionary wrapper classes
  TPdfDictionaryWrapper = class(TPersistent)
  private
    FData: TPdfDictionary;
    function GetHasData: boolean;
  protected
    procedure SetData(AData: TPdfDictionary);
  public
    /// the associated dictionary, containing all data
    property Data: TPdfDictionary read FData write SetData;
    /// return TRUE if has any data stored within
    property HasData: boolean read GetHasData;
  end;

  /// defines the data stored inside a EMR_GDICOMMENT message
  // - pgcOutline can be used to add an outline at the current position (i.e.
  // the last Y parameter of a Move): the text is the associated title, UTF-8 encoded
  // and the outline tree is created from the number of leading spaces in the title
  // - pgcBookmark will create a destination at the current position (i.e.
  // the last Y parameter of a Move), with some text supplied as bookmark name
  // - pgcLink/pgcLinkNoBorder will create a asLink annotation, expecting the data
  // to be filled with TRect inclusive-inclusive bounding rectangle coordinates,
  // followed by the corresponding bookmark name
  // - use the GDIComment*() functions to append the corresponding
  // EMR_GDICOMMENT message to a metafile content
  TPdfGDIComment =
    (pgcOutline, pgcBookmark, pgcLink, pgcLinkNoBorder);

  /// a dictionary wrapper class for the PDF document information fields
  // - all values use the generic VCL string type, and will be encoded
  // as Unicode if necessary
  TPdfInfo = class(TPdfDictionaryWrapper)
  private
    function GetAuthor: string;
    procedure SetAuthor(const Value: string);
    function GetCreationDate: TDateTime;
    procedure SetCreationDate(Value: TDateTime);
    function GetCreator: string;
    procedure SetCreator(const Value: string);
    function GetKeywords: string;
    procedure SetKeywords(const Value: string);
    function GetSubject: string;
    procedure SetSubject(const Value: string);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetModDate: TDateTime;
    procedure SetModDate(Value: TDateTime);
  public
    /// the PDF document Author
    property Author: string read GetAuthor write SetAuthor;
    /// the PDF document Creation Date
    property CreationDate: TDateTime read GetCreationDate write SetCreationDate;
    /// the Software or Library name which created this PDF document
    property Creator: string read GetCreator write SetCreator;
    /// the PDF document associated key words
    property Keywords: string read GetKeywords write SetKeywords;
    /// the PDF document modification date
    property ModDate: TDateTime read GetModDate write SetModDate;
    /// the PDF document subject
    property Subject: string read GetSubject write SetSubject;
    /// the PDF document title
    property Title: string read GetTitle write SetTitle;
  end;

  /// a dictionary wrapper class for the PDF document catalog fields
  // - It contains references to other objects defining the document's contents,
  // outline, article threads (PDF 1.1), named destinations, and other attributes.
  // In addition, it contains information about how the document should be displayed
  // on the screen, such as whether its outline and thumbnail page images should be
  // displayed automatically and whether some location other than the first page
  // should be shown when the document is opened
  TPdfCatalog = class(TPdfDictionaryWrapper)
  private
    FOpenAction: TPdfDestination;
    FOwner: TPdfDocument;
    procedure SetPageLayout(Value: TPdfPageLayout);
    procedure SetPageMode(Value: TPdfPageMode);
    procedure SetNonFullScreenPageMode(Value: TPdfPageMode);
    procedure SetViewerPreference(Value: TPdfViewerPreferences);
    procedure SetPages(APages: TPdfDictionary);
    function GetPageLayout: TPdfPageLayout;
    function GetPageMode: TPdfPageMode;
    function GetNonFullScreenPageMode: TPdfPageMode;
    function GetViewerPreference: TPdfViewerPreferences;
    function GetPages: TPdfDictionary;
  protected
    procedure SaveOpenAction;
  public
    /// a Destination to be displayed when the document is opened
    property OpenAction: TPdfDestination read FOpenAction write FOpenAction;
    /// The page layout to be used when the document is opened
    property PageLayout: TPdfPageLayout read GetPageLayout write SetPageLayout;
    /// Page mode determines how the document should appear when opened
    property NonFullScreenPageMode: TPdfPageMode read GetNonFullScreenPageMode write SetNonFullScreenPageMode;
    /// Page mode determines how the document should appear when opened
    property PageMode: TPdfPageMode read GetPageMode write SetPageMode;
    /// A viewer preferences dictionary specifying the way the document is to be
    // displayed on the screen
    // - If this entry is absent, viewer applications should use their own current
    // user preference settings
    property ViewerPreference: TPdfViewerPreferences read GetViewerPreference write SetViewerPreference;
    /// The page tree node that is the root of the document's page tree
    // - Required, must be an indirect reference
    // - you can set a value to it in order to add some nested pages
    property Pages: TPdfDictionary read GetPages write SetPages;
  end;

  /// a generic PDF font object
  TPdfFont = class(TPdfDictionaryWrapper)
  protected
    fName: PDFString;
    fShortCut: PDFString;
    fFirstChar, fLastChar: integer;
    fDefaultWidth: word;
    fAscent, fDescent: integer;
    fUnicode: boolean;
    /// index in TrueTypeFontsIndex[] + 1, 0 if not a TPdfFontTrueType
    // - same TPdfFontTrueType index may appear multiple times in the font list,
    // e.g. with normal, bold and/or italic attributes
    // - this hidden property is used by TPdfDocument for faster font list handling
    fTrueTypeFontsIndex: integer;
    /// contains a bit for every WinAnsi encoded char
    // - encoding in TPdfFont, even if used by TPdfFontWinAnsi descendent only
    fWinAnsiUsed: set of AnsiChar;
  public
    /// create the PDF font object instance
    constructor Create(AXref: TPdfXref; const AName: PDFString);
    /// mark some WinAnsi char as used
    procedure AddUsedWinAnsiChar(aChar: AnsiChar); {$ifdef HASINLINE}inline;{$endif}
    /// retrieve the width of a specified character
    // - implementation of this method is either WinAnsi (by TPdfFontWinAnsi),
    // either compatible with MBCS strings (TPdfFontCIDFontType2)
    // - return 0 by default (descendant must handle the Ansi charset)
    function GetAnsiCharWidth(const AText: PDFString; APos: integer): integer; virtual;
    /// the internal PDF font name (e.g. 'Helvetica-Bold')
    // - postscript font names are inside the unit: these postscript names
    // could not match the "official" True Type font name, stored as
    // UTF-8 in FTrueTypeFonts
    property Name: PDFString read FName;
    /// the internal PDF shortcut (e.g. 'F3')
    property ShortCut: PDFString read FShortCut;
    /// is set to TRUE if the font is dedicated to Unicode Chars
    property Unicode: boolean read fUnicode;
  end;

  PPdfWinAnsiWidth = ^TPdfWinAnsiWidth;
  TPdfWinAnsiWidth = array[#32..#255] of word;

  /// a generic PDF font object, handling at least WinAnsi encoding
  // - TPdfFontTrueType descendent will handle also Unicode chars,
  // for all WideChar which are outside the WinAnsi selection
  TPdfFontWinAnsi = class(TPdfFont)
  protected
    /// contain the Width array of the corresponding WinAnsi encoded char
    fWinAnsiWidth: PPdfWinAnsiWidth;
  public
    /// retrieve the width of a specified character
    // - implementation of this method expect WinAnsi encoding
    // - return the value contained in fWinAnsiWidth[] by default
    function GetAnsiCharWidth(const AText: PDFString; APos: integer): integer; override;
    /// release the used memory
    destructor Destroy; override;
  end;

  /// an embedded WinAnsi-Encoded standard Type 1 font
  // - handle Helvetica, Courier and Times font by now
  TPdfFontType1 = class(TPdfFontWinAnsi)
  protected
  public
    /// create a standard font instance, with a given name and char widths
    // - if WidthArray is nil, it will create a fixed-width font of 600 units
    // - WidthArray[0]=Ascent, WidthArray[1]=Descent, WidthArray[2..]=Width(#32..)
    constructor Create(AXref: TPdfXref; const AName: PDFString;
      WidthArray: PSmallIntArray); reintroduce;
  end;

  /// an embedded Composite CIDFontType2
  // - i.e. a CIDFont whose glyph descriptions are based on TrueType font technology
  // - typicaly handle Japan or Chinese standard fonts
  // - used with MBCS encoding, not WinAnsi
  TPdfFontCIDFontType2 = class(TPdfFont)
    { TODO: implement standard TPdfFontCIDFontType2 MBCS font }
  end;

  /// handle Unicode glyph description for a True Type Font
  // - cf http://www.microsoft.com/typography/OTSPEC/otff.htm#otttables
  // - handle Microsoft cmap format 4 encoding (i.e. most used
  // true type fonts on Windows)
  TPdfTTF = class
  protected
    // we use TWordDynArray for auto garbage collection and generic handling
    // - since the TTF file is big endian, we swap all words at loading, to
    // be used directly by the Intel x86 code; integer (longint) values
    // must take care of this byte swapping
    fcmap,
    fhead,
    fhhea,
    fhmtx: TWordDynArray;
  public
    // these are pointers to the useful data of the True Type Font:
    /// Font header
    head: ^TCmapHEAD;
    /// Horizontal header
    hhea: ^TCmapHHEA;
    /// Character to glyph mapping (cmap) table, in format 4
    fmt4: ^TCmapFmt4;
    /// Start character code for each cmap format 4 segment
    startCode: PWordArray;
    /// End characterCode for each cmap format 4 segment
    endCode: PWordArray;
    /// Delta for all character codes in each cmap format 4 segment
    idDelta: PSmallIntArray;
    /// Offsets into glyphIndexArray or 0
    idRangeOffset: PWordArray;
    /// Glyph index array (arbitrary length)
    glyphIndexArray: PWordArray;
  public
    /// create Unicode glyph description for a supplied True Type Font
    // - the HDC of its corresponding document must have selected the font first
    // - this constructor will fill fUsedWide[] and fUsedWideChar of aUnicodeTTF
    // with every available unicode value, and its corresponding glyph and width
    constructor Create(aUnicodeTTF: TPdfFontTrueType); reintroduce;
  end;

  /// this dynamic array stores details about used unicode characters
  // - every used unicode character has its own width and glyph index in the
  // true type font content
  TUsedWide = array of packed record
      case byte of
      0: (
        Width: word;
        Glyph: word; );
      1: (
        Int: integer; );
    end;

  /// handle TrueType Font
  // - handle both WinAnsi text and Unicode characters in two separate
  // TPdfFontTrueType instances (since PDF need two separate fonts with
  // diverse encoding)
  TPdfFontTrueType = class(TPdfFontWinAnsi)
  private
    function GetWideCharUsed: Boolean; {$ifdef HASINLINE}inline;{$endif}
  protected
    fStyle: TPdfFontStyles;
    fDoc: TPdfDocument;
    // note: fUsedWide[] and fUsedWideChar are used:
    // - in WinAnsi Fonts for glyphs used by ShowText
    // - in Unicode Fonts for all available glyphs from TPdfTTF values
    fUsedWideChar: TSortedWordArray;
    fUsedWide: TUsedWide;
    fHGDI: HGDIOBJ;
    fFixedWidth: boolean;
    fFontDescriptor: TPdfDictionary;
    fFontFile2: TPdfStream;
    fUnicodeFont: TPdfFontTrueType;
    fWinAnsiFont: TPdfFontTrueType;
    fIsSymbolFont: Boolean;
    // below are some bigger structures
    fLogFont: TLogFontW;
    fM: TTextMetric;
    fOTM: TOutlineTextmetric;
    procedure CreateAssociatedUnicodeFont;
    // update font description from used chars
    procedure PrepareForSaving;
    // low level adding of a glyph (returns the real glyph index found, 0 if none)
    function GetAndMarkGlyphAsUsed(aGlyph: word): word;
  public
    /// create the TrueType font object instance
    constructor Create(ADoc: TPdfDocument; AFontIndex: integer;
      AStyle: TPdfFontStyles; const ALogFont: TLogFontW; AWinAnsiFont: TPdfFontTrueType); reintroduce;
    /// release the associated memory and handles
    destructor Destroy; override;
    /// mark some UTF-16 codepoint as used
    // - return the index in fUsedWideChar[] and fUsedWide[]
    // - this index is the one just added, or the existing one if the value
    // was found to be already in the fUserWideChar[] array
    function FindOrAddUsedWideChar(aWideChar: WideChar): integer;
    /// retrieve the width of an UTF-16 codepoint
    // - WinAnsi characters are taken from fWinAnsiWidth[], unicode chars from
    // fUsedWide[].Width
    function GetWideCharWidth(aWideChar: WideChar): Integer;
    /// is set to TRUE if the PDF used any true type encoding
    property WideCharUsed: Boolean read GetWideCharUsed;
    /// the associated Font Styles
    property Style: TPdfFontStyles read fStyle;
    /// is set to TRUE if the font has a fixed width
    property FixedWidth: boolean read fFixedWidth;
    /// points to the corresponding Unicode font
    // - returns NIL if the Unicode font has not yet been created by the
    // CreateUnicodeFont method
    // - may return SELF if the font is itself the Unicode version
    property UnicodeFont: TPdfFontTrueType read fUnicodeFont;
    /// points to the corresponding WinAnsi font
    // - always return a value, whatever it is self
    property WinAnsiFont: TPdfFontTrueType read fWinAnsiFont;
  end;

  /// A destination defines a particular view of a document, consisting of the following:
  // - The page of the document to be displayed
  // - The location of the display window on that page
  // - The zoom factor to use when displaying the page
  TPdfDestination = class(TObject)
  private
    FDoc: TPdfDocument;
    FPage: TPdfPage;
    FType: TPdfDestinationType;
    FValues: array[0..3] of Integer;
    FZoom: Single;
    FReference: TObject;
    procedure SetElement(Index: integer; Value: Integer);
    procedure SetZoom(Value: Single);
    function GetElement(Index: integer): Integer;
    function GetPageWidth: Integer;
    function GetPageHeight: Integer;
  public
    /// create the PDF destination object
    // - the current document page is associated with this destination
    constructor Create(APdfDoc: TPdfDocument);
    /// release the object
    destructor Destroy; override;
    /// retrieve the array containing the location of the display window
    // - the properties values which are not used are ignored
    function GetValue: TPdfArray;
    /// Destination Type determines default user space coordinate system of
    // Explicit destinations
    property DestinationType: TPdfDestinationType read FType write FType;
    /// the associated PDF document which created this Destination object
    property Doc: TPdfDocument read FDoc;
    /// the associated Page
    property Page: TPdfPage read FPage;
    /// retrieve the left coordinate of the location of the display window
    property Left: Integer index 0 read GetElement write SetElement;
    /// retrieve the top coordinate of the location of the display window
    property Top: Integer index 1 read GetElement write SetElement;
    /// retrieve the righ tcoordinate of the location of the display window
    property Right: Integer index 2 read GetElement write SetElement;
    /// retrieve the bottom coordinate of the location of the display window
    property Bottom: Integer index 3 read GetElement write SetElement;
    /// the page height of the current page
    // - return the corresponding MediaBox value
    property PageHeight: Integer read GetPageHeight;
    /// the page width of the current page
    // - return the corresponding MediaBox value
    property PageWidth: Integer read GetPageWidth;
    /// the associated Zoom factor
    // - by default, the Zoom factor is 1
    property Zoom: Single read FZoom write SetZoom;
    /// an object associated to this destination, to be used for conveniance
    property Reference: TObject read FReference write FReference;
  end;

  /// an Outline entry in the PDF document
  TPdfOutlineEntry = class(TPdfDictionaryWrapper)
  private
    FParent: TPdfOutlineEntry;
    FNext: TPdfOutlineEntry;
    FPrev: TPdfOutlineEntry;
    FFirst: TPdfOutlineEntry;
    FLast: TPdfOutlineEntry;
    FDest: TPdfDestination;
    FDoc: TPdfDocument;
    FTitle: string;
    FOpened: boolean;
    FCount: integer;
    FReference: TObject;
    FLevel: integer;
  protected
    procedure Save; virtual;
  public
    /// create the Outline entry instance
    // - if TopPosition is set, a corresponding destination is created
    // on the current PDF Canvas page, at this Y position
    constructor Create(AParent: TPdfOutlineEntry;
      TopPosition: integer=-1); reintroduce;
    /// release the associated memory and reference object
    destructor Destroy; override;
    /// create a new entry in the outline tree
    // - this is the main method to create a new entry
    function AddChild(TopPosition: integer=-1): TPdfOutlineEntry;
    /// the associated PDF document which created this Destination object
    property Doc: TPdfDocument read FDoc;
    /// the parent outline entry of this entry
    property Parent: TPdfOutlineEntry read FParent;
    /// the next outline entry of this entry
    property Next: TPdfOutlineEntry read FNext;
    /// the previous outline entry of this entry
    property Prev: TPdfOutlineEntry read FPrev;
    /// the first outline entry of this entry list
    property First: TPdfOutlineEntry read FFirst;
    /// the last outline entry of this entry list
    property Last: TPdfOutlineEntry read FLast;
    /// the associated destination
    property Dest: TPdfDestination read FDest write FDest;
    /// the associated title
    // - is a generic VCL string, so is Unicode ready
    property Title: string read FTitle write FTitle;
    /// if the outline must be opened
    property Opened: boolean read FOpened write FOpened;
    /// an object associated to this destination, to be used for conveniance
    property Reference: TObject read FReference write FReference;
    /// an internal property (not exported to PDF content)
    property Level: integer read FLevel write FLevel;
  end;

  /// Root entry for all Outlines of the PDF document
  // - this is a "fake" entry which must be used as parent for all true
  // TPdfOutlineEntry instances, but must not be used as a true outline entry
  TPdfOutlineRoot = class(TPdfOutlineEntry)
  public
    /// create the Root entry for all Outlines of the PDF document
    constructor Create(ADoc: TPdfDocument); reintroduce;
    /// update internal parameters (like outline entries count) before saving
    procedure Save; override;
  end;

  {$ifdef USE_METAFILE}
  /// a PDF page, with its corresponding Meta File and Canvas
  TPdfPageGDI = class(TPdfPage)
  private
    // don't use these fVCL* properties directly, but via TPdfDocumentGdi.VCLCanvas
    fVCLMetaFileCompressed: RawByteString;
    fVCLCanvasSize: TSize;
    // it is in fact a TMetaFileCanvas instance from fVCLCurrentMetaFile
    fVCLCurrentCanvas: TCanvas;
    fVCLCurrentMetaFile: TMetaFile;
    // allow to create the meta file and its canvas only if necessary, and
    // compress the page content using SynLZ to reduce memory usage
    procedure CreateVCLCanvas;
    procedure SetVCLCurrentMetaFile;
    procedure FlushVCLCanvas;
  public
    /// release associated memory
    destructor Destroy; override;
  end;

  /// class handling PDF document creation using GDI commands
  // - this class allows using a VCL standard Canvas class
  // - handles also PDF creation directly from TMetaFile content
  TPdfDocumentGDI = class(TPdfDocument)
  private
    fUseMetaFileTextPositioning: TPdfCanvasRenderMetaFileTextPositioning;
    fUseMetaFileTextClipping: TPdfCanvasRenderMetaFileTextClipping;
    fKerningHScaleTop: Single;
    fKerningHScaleBottom: Single;
    function GetVCLCanvas: TCanvas;   {$ifdef HASINLINE}inline;{$endif}
    function GetVCLCanvasSize: TSize; {$ifdef HASINLINE}inline;{$endif}
  public
    /// create the PDF document instance, with a VCL Canvas property
    // - see TPdfDocument.Create connstructor for the arguments expectations
    constructor Create(AUseOutlines: Boolean=false; ACodePage: integer=0;
      APDFA1: boolean=false
      {$ifdef USE_PDFSECURITY}; AEncryption: TPdfEncryption=nil{$endif});
    /// add a Page to the current PDF document
    function AddPage: TPdfPage; override;
    /// save the PDF file content into a specified Stream
    // - this overridden method draw first the all VCLCanvas content into the PDF
    procedure SaveToStream(AStream: TStream; ForceModDate: TDateTime=0); override;
    /// save the current page content to the PDF file
    // - this overridden method flush the content from the VCLCanvas into the PDF
    // - it will reduce the used memory as much as possible, by-passing page
    // content compression
    // - typical use may be:
    // ! with TPdfDocumentGDI.Create do
    // !   try
    // !     Stream := TFileStream.Create(FileName, fmCreate);
    // !     try
    // !       SaveToStreamDirectBegin(Stream);
    // !       for i := 1 to 9 do
    // !       begin
    // !         AddPage;
    // !         with VCLCanvas do
    // !         begin
    // !           Font.Name := 'Times new roman';
    // !           Font.Size := 150;
    // !           Font.Style := [fsBold, fsItalic];
    // !           Font.Color := clNavy;
    // !           TextOut(100, 100, 'Page ' + IntToStr(i));
    // !         end;
    // !         SaveToStreamDirectPageFlush; // direct writing
    // !       end;
    // !       SaveToStreamDirectEnd;
    // !     finally
    // !       Stream.Free;
    // !     end;
    // !   finally
    // !     Free;
    // !   end;
    procedure SaveToStreamDirectPageFlush(FlushCurrentPageNow: boolean=false); override;
    /// the VCL Canvas of the current page
    property VCLCanvas: TCanvas read GetVCLCanvas;
    /// the VCL Canvas size of the current page
    // - useful to calculate coordinates for the current page
    // - filled with (0,0) before first call to VCLCanvas property
    property VCLCanvasSize: TSize read GetVCLCanvasSize;
    /// defines how TMetaFile text positioning is rendered
    // - default is tpSetTextJustification
    // - tpSetTextJustification if content used SetTextJustification() API calls
    // - tpExactTextCharacterPositining for exact font kerning, but resulting
    // in bigger pdf size
    // - tpKerningFromAveragePosition will compute average pdf Horizontal Scaling
    // in association with KerningHScaleBottom/KerningHScaleTop properties
    // - replace deprecated property UseSetTextJustification
    property UseMetaFileTextPositioning: TPdfCanvasRenderMetaFileTextPositioning
      read fUseMetaFileTextPositioning write fUseMetaFileTextPositioning;
    /// defines how TMetaFile text clipping should be applied
    // - tcNeverClip has been reported to work better e.g. when app is running
    // on Wine
    property UseMetaFileTextClipping: TPdfCanvasRenderMetaFileTextClipping
      read fUseMetaFileTextClipping write fUseMetaFileTextClipping;
    /// the % limit below which Font Kerning is transformed into PDF Horizontal
    // Scaling commands (when text positioning is tpKerningFromAveragePosition)
    // - set to 99.0 by default
    property KerningHScaleBottom: Single read fKerningHScaleBottom write fKerningHScaleBottom;
    /// the % limit over which Font Kerning is transformed into PDF Horizontal
    // Scaling commands (when text positioning is tpKerningFromAveragePosition)
    // - set to 101.0 by default
    property KerningHScaleTop: Single read fKerningHScaleTop write fKerningHScaleTop;
  end;
  {$endif USE_METAFILE}

  {$ifdef USE_BITMAP}
  /// generic image object
  // - is either bitmap encoded or jpeg encoded
  TPdfImage = class(TPdfXObject)
  private
    fPixelHeight: Integer;
    fPixelWidth: Integer;
    fHash: THash128Rec;
  public
    /// create the image from a supplied VCL TGraphic instance
    // - handle TBitmap and SynGdiPlus picture types, i.e. TJpegImage
    // (stored as jpeg), and TGifImage/TPngImage (stored as bitmap)
    // - use TPdfForm to handle TMetafile in vectorial format
    // - an optional DontAddToFXref is available, if you don't want to add
    // this object to the main XRef list of the PDF file
    constructor Create(aDoc: TPdfDocument; aImage: TGraphic; DontAddToFXref: boolean); reintroduce;
    /// create an image from a supplied JPEG file name
    // - will raise an EFOpenError exception if the file doesn't exist
    // - an optional DontAddToFXref is available, if you don't want to add
    // this object to the main XRef list of the PDF file
    constructor CreateJpegDirect(aDoc: TPdfDocument; const aJpegFileName: TFileName;
      DontAddToFXref: boolean=true); reintroduce; overload;
    /// create an image from a supplied JPEG content
    // - an optional DontAddToFXref is available, if you don't want to add
    // this object to the main XRef list of the PDF file
    constructor CreateJpegDirect(aDoc: TPdfDocument; aJpegFile: TMemoryStream;
      DontAddToFXref: boolean=true); reintroduce; overload;
    /// width of the image, in pixels units
    property PixelWidth: Integer read fPixelWidth;
    /// height of the image, in pixels units
    property PixelHeight: Integer read fPixelHeight;
  end;
  {$endif USE_BITMAP}

  {$ifdef USE_METAFILE}
  /// handle any form XObject
  // - A form XObject (see Section 4.9, of PDF reference 1.3) is a self-contained
  // description of an arbitrary sequence of graphics objects, defined as a
  // PDF content stream
  TPdfForm = class(TPdfXObject)
  private
    FFontList: TPdfDictionary;
  public
    /// create a form XObject from a supplied TMetaFile
    constructor Create(aDoc: TPdfDocumentGDI; aMetaFile: TMetafile); reintroduce;
  end;
  {$endif USE_METAFILE}

  /// a form XObject with a Canvas for drawing
  // - once created, you can create this XObject, then draw it anywhere on
  // any page - see sample
  TPdfFormWithCanvas = class(TPdfXObject)
  private
    FFontList: TPdfDictionary;
    FPage: TPdfPage;
    FCanvas: TPdfCanvas;
  public
    /// create a form XObject with TPDFCanvas
    constructor Create(aDoc: TPdfDocument; W, H: Integer); reintroduce;
    /// release used memory
    destructor Destroy; override;
    /// close the internal canvas
    procedure CloseCanvas;
    /// access to the private canvas associated with the PDF form XObject
    property Canvas: TPdfCanvas read FCanvas;
  end;

  /// used to handle compressed object stream (in PDF 1.5 format)
  TPdfObjectStream = class(TPdfXObject)
  protected
    fObjectCount: integer;
    fAddingStream: TPdfWrite;
    fObject: array of record
      Number: integer;
      Position: integer;
    end;
    procedure InternalWriteTo(W: TPdfWrite); override;
  public
    /// create the instance, i.e. its associated stream
    constructor Create(aDoc: TPdfDocument); reintroduce;
    /// release internal memory structures
    destructor Destroy; override;
    /// add an object to this compressed object stream
    // - returns the object index in this object stream
    function AddObject(Value: TPdfObject): integer;
    /// the number of compressed objects within this object stream
    property ObjectCount: integer read fObjectCount;
  end;


/// this function returns TRUE if the supplied text contain any MBCS character
// - typical call must check first if MBCS is currently enabled
// ! if SysLocale.FarEast and _HasMultiByteString(pointer(Text)) then ...
function _HasMultiByteString(Value: PAnsiChar): boolean;

/// convert a specified UTF-8 content into a PDFString value
function RawUTF8ToPDFString(const Value: RawUTF8): PDFString;

/// convert an unsigned integer into a PDFString text
function UInt32ToPDFString(Value: Cardinal): PDFString;

/// convert a date, into PDF string format, i.e. as 'D:20100414113241Z'
function _DateTimeToPdfDate(ADate: TDateTime): TPdfDate;

/// decode PDF date, encoded as 'D:20100414113241'
function _PdfDateToDateTime(const AText: TPdfDate): TDateTime;

/// wrapper to create a temporary PDF coordinates rectangle
function PdfRect(Left, Top, Right, Bottom: Single): TPdfRect; overload; {$ifdef HASINLINE}inline;{$endif}

/// wrapper to create a temporary PDF coordinates rectangle
function PdfRect(const Box: TPdfBox): TPdfRect; overload; {$ifdef HASINLINE}inline;{$endif}

/// wrapper to create a temporary PDF box
function PdfBox(Left, Top, Width, Height: Single): TPdfBox; {$ifdef HASINLINE}inline;{$endif}

/// reverse char orders for every hebrew and arabic words
// - just reverse all the UTF-16 codepoints in the supplied buffer
procedure L2R(W: PWideChar; L: integer);

/// convert some milli meters dimension to internal PDF twips value
function PdfCoord(MM: single): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the paper size used by the current selected printer
function CurrentPrinterPaperSize: TPDFPaperSize;

/// retrieve the current printer resolution
function CurrentPrinterRes: TPoint;

/// append a EMR_GDICOMMENT message for handling PDF bookmarks
// - will create a PDF destination at the current position (i.e. the last Y
// parameter of a Move), with some text supplied as bookmark name
procedure GDICommentBookmark(MetaHandle: HDC; const aBookmarkName: RawUTF8);

/// append a EMR_GDICOMMENT message for handling PDF outline
// - used to add an outline at the current position (i.e. the last Y parameter of
// a Move): the text is the associated title, UTF-8 encoded and the outline tree
// is created from the specified numerical level (0=root)
procedure GDICommentOutline(MetaHandle: HDC; const aTitle: RawUTF8; aLevel: Integer);

/// append a EMR_GDICOMMENT message for creating a Link into a specified bookmark
procedure GDICommentLink(MetaHandle: HDC; const aBookmarkName: RawUTF8; const aRect: TRect;
  NoBorder: boolean);


{$ifdef USE_PDFSECURITY}
const
  /// allow all actions for a pdf encrypted file
  // - to be used as parameter for TPdfEncryption.New() class method
  PDF_PERMISSION_ALL: TPdfEncryptionPermissions =
    [Low(TPdfEncryptionPermission)..high(TPdfEncryptionPermission)];

  /// disable modification and annotation of a pdf encrypted file
  // - to be used as parameter for TPdfEncryption.New() class method
  PDF_PERMISSION_NOMODIF: TPdfEncryptionPermissions = [epPrinting,
    epContentCopy, epPrintingHighResolution, epFillingForms,
    epContentExtraction, epDocumentAssembly];

  /// disable printing for a pdf encrypted file
  // - to be used as parameter for TPdfEncryption.New() class method
  PDF_PERSMISSION_NOPRINT: TPdfEncryptionPermissions = [epGeneralEditing,
    epContentCopy, epAuthoringComment, epContentExtraction, epDocumentAssembly];

  /// disable content extraction or copy for a pdf encrypted file
  // - to be used as parameter for TPdfEncryption.New() class method
  PDF_PERMISSION_NOCOPY: TPdfEncryptionPermissions = [epPrinting,
    epAuthoringComment, epPrintingHighResolution, epFillingForms];

  /// disable printing and content extraction or copy for a pdf encrypted file
  // - to be used as parameter for TPdfEncryption.New() class method
  PDF_PERMISSION_NOCOPYNORPRINT: TPdfEncryptionPermissions = [];
{$endif USE_PDFSECURITY}


(*
    Windows Uniscribe APIs

    Uniscribe is a set of APIs that allow a high degree of control for fine
    typography and for processing complex scripts
    - see http://msdn.microsoft.com/en-us/library/dd374091(v=VS.85).aspx
    - used by both SynPDF.pas and mORMotReport.pas (for TGDIPages)
    - NO_USE_UNISCRIBE conditional can be set globaly for an application
      which doesn't need the UniScribe features
*)


{$ifdef USE_UNISCRIBE}
const
  Usp10 = 'usp10.dll';
  /// error returned by Uniscribe when the current selected font
  // does not contain sufficient glyphs or shaping tables
  USP_E_SCRIPT_NOT_IN_FONT = HRESULT((SEVERITY_ERROR shl 31) or (FACILITY_ITF shl 16)) or $200;

type
  /// UniScribe script state flag elements
  // - r0,r1,r2,r3,r4: map TScriptState.uBidiLevel
  // - fOverrideDirection: Set when in LRO/RLO embedding
  // - fInhibitSymSwap: Set by U+206A (ISS), cleared by U+206B (ASS)
  // - fCharShape: Set by U+206D (AAFS), cleared by U+206C (IAFS)
  // - fDigitSubstitute: Set by U+206E (NADS), cleared by U+206F (NODS)
  // - fInhibitLigate: Equiv !GCP_Ligate, no Unicode control chars yet
  // - fDisplayZWG: Equiv GCP_DisplayZWG, no Unicode control characters yet
  // - fArabicNumContext: For EN->AN Unicode rule
  // - fGcpClusters: For Generating Backward Compatible GCP Clusters (legacy Apps)
  TScriptState_enum = (
    r0,r1,r2,r3,r4,
    fOverrideDirection, fInhibitSymSwap, fCharShape, fDigitSubstitute,
    fInhibitLigate, fDisplayZWG, fArabicNumContext, fGcpClusters);

  /// a set of UniScribe script state flags
  TScriptState_set = set of TScriptState_enum;

  PScriptState = ^TScriptState;

  /// an UniScribe script state
  // - uBidiLevel: Unicode Bidi algorithm embedding level (0..16)
  // - fFlags: Script state flags
  TScriptState = packed record
   case Byte of
    0: (uBidiLevel: Byte)    {:5};
    1: (fFlags: TScriptState_set)
  end;
  /// Uniscribe script analysis flag elements
  // - s0,s1,s2,s3,s4,s5,s6,s7,s8,s9: map TScriptAnalysis.eScript
  // - fRTL: Rendering direction
  // - fLayoutRTL: Set for GCP classes ARABIC/HEBREW and LOCALNUMBER
  // - fLinkBefore: Implies there was a ZWJ before this item
  // - fLinkAfter: Implies there is a ZWJ following this item.
  // - fLogicalOrder: Set by client as input to ScriptShape/Place
  // - fNoGlyphIndex: Generated by ScriptShape/Place - this item does not use
  // glyph indices
  TScriptAnalysis_enum = (
    s0,s1,s2,s3,s4,s5,s6,s7,s8,s9,
    fRTL, fLayoutRTL, fLinkBefore, fLinkAfter, fLogicalOrder, fNoGlyphIndex);

  /// a set of Uniscribe script analysis flags
  TScriptAnalysis_set = set of TScriptAnalysis_enum;

  PScriptAnalysis = ^TScriptAnalysis;
  /// an Uniscribe script analysis
  // - eScript:  Shaping engine
  // - fFlags: Script analysis flags
  // - s: Script state
  TScriptAnalysis = packed record
   case Byte of
    0: (eScript: Word);
    1: (fFlags: TScriptAnalysis_set;
        s: TScriptState);
  end;
  PScriptItem = ^TScriptItem;
  /// a Uniscribe script item, after analysis of a unicode text
  TScriptItem = packed record
    /// Logical offset to first character in this item
    iCharPos: Integer;
    /// corresponding Uniscribe script analysis
    a: TScriptAnalysis;
  end;

  /// all possible Uniscribe processing properties of a given language
  // - fNumeric:  if a script contains only digits
  // - fComplex: Script requires special shaping or layout
  // - fNeedsWordBreaking: Requires ScriptBreak for word breaking information
  // - fNeedsCaretInfo: Requires caret restriction to cluster boundaries
  // - bCharSet0 .. bCharSet7: Charset to use when creating font
  // - fControl: Contains only control characters
  // - fPrivateUseArea: This item is from the Unicode range U+E000 through U+F8FF
  // - fNeedsCharacterJustify: Requires inter-character justification
  // - fInvalidGlyph: Invalid combinations generate glyph wgInvalid in the glyph buffer
  // - fInvalidLogAttr: Invalid combinations are marked by fInvalid in the logical attributes
  // - fCDM: Contains Combining Diacritical Marks
  // - fAmbiguousCharSet: Script does not correspond 1// :1 with a charset
  // - fClusterSizeVaries: Measured cluster width depends on adjacent clusters
  // - fRejectInvalid: Invalid combinations should be rejected
  TScriptProperties_enum = (
    fNumeric, fComplex, fNeedsWordBreaking, fNeedsCaretInfo,
    bCharSet0, bCharSet1, bCharSet2, bCharSet3, bCharSet4, bCharSet5,
    bCharSet6, bCharSet7,
    fControl, fPrivateUseArea, fNeedsCharacterJustify, fInvalidGlyph,
    fInvalidLogAttr, fCDM, fAmbiguousCharSet, fClusterSizeVaries, fRejectInvalid);

  /// set of possible Uniscribe processing properties of a given language
  TScriptProperties_set = set of TScriptProperties_enum;
  PScriptProperties = ^TScriptProperties;

  /// Contains information about Uniscribe special processing for each script
  TScriptProperties = packed record
    /// Primary and sublanguage associated with script
    langid: Word;
    /// set of possible Uniscribe processing properties for a given language
    fFlags: TScriptProperties_set;
  end;

  PScriptPropertiesArray = ^TPScriptPropertiesArray;
  /// an array of Uniscribe processing information
  TPScriptPropertiesArray = array[byte] of PScriptProperties;

  /// Uniscribe visual (glyph) attributes
  // - a0 .. a3: map the Justification class number
  // - fClusterStart: First glyph of representation of cluster
  // - fDiacritic: Diacritic
  // - fZeroWidth: Blank, ZWJ, ZWNJ etc, with no width
  // - fReserved: General reserved bit
  TScriptVisAttr_enum = (
    a0,a1,a2,a3,
    fClusterStart,     {:1}  // First glyph of representation of cluster
    fDiacritic,        {:1}  // Diacritic
    fZeroWidth,        {:1}  // Blank, ZWJ, ZWNJ etc, with no width
    fReserved          {:1}  // General reserved
  );
  /// set of Uniscribe visual (glyph) attributes
  TScriptVisAttr_set = set of TScriptVisAttr_enum;

  PScriptVisAttr = ^TScriptVisAttr;
  /// Contains the visual (glyph) attributes that identify clusters and
  // justification points, as generated by ScriptShape
  // - uJustification: Justification class
  // - fFlags: Uniscribe visual (glyph) attributes
  // - fShapeReserved: Reserved for use by shaping engines
  TScriptVisAttr = packed record
   case Byte of
    0: (uJustification: Byte) {:4};
    1: (fFlags: TScriptVisAttr_set;
        fShapeReserved: Byte) {:8};
  end;

  TScriptControlAttr_enum = (
    fContextDigits,
    fInvertPreBoundDir,
    fInvertPostBoundDir,
    fLinkStringBefore,
    fLinkStringAfter,
    fNeutralOverride,
    fNumericOverride,
    fLegacyBidiClass,
    fScr0, fScr1, fScr2, fScr3, fScr4, fScr5, fScr6, fScr7);

  TScriptControlAttr_set = set of TScriptControlAttr_enum;

  TScriptControl = packed record
    uDefaultLanguage: Word;
    fFlags: TScriptControlAttr_set;
  end;
  PScriptControl = ^TScriptControl;

/// Uniscribe function to break a Unicode string into individually shapeable items
// - pwcInChars: Pointer to a Unicode string to itemize.
// - cInChars: Number of characters in pwcInChars to itemize.
// - cMaxItems: Maximum number of SCRIPT_ITEM structures defining items to process.
// - psControl: Optional. Pointer to a SCRIPT_CONTROL structure indicating the
// type of itemization to perform. Alternatively, the application can set this
// parameter to NULL if no SCRIPT_CONTROL properties are needed.
// - psState: Optional. Pointer to a SCRIPT_STATE structure indicating
// the initial bidirectional algorithm state. Alternatively, the application
// can set this parameter to NULL if the script state is not needed.
// - pItems: Pointer to a buffer in which the function retrieves SCRIPT_ITEM
// structures representing the items that have been processed. The buffer
// should be cMaxItems*sizeof(SCRIPT_ITEM) + 1 bytes in length. It is invalid
// to call this function with a buffer to hold less than two SCRIPT_ITEM
// structures. The function always adds a terminal item to the item analysis
// array so that the length of the item with zero-based index "i" is
// always available as:
// ! pItems[i+1].iCharPos - pItems[i].iCharPos;
// - pcItems: Pointer to the number of SCRIPT_ITEM structures processed
function ScriptItemize(
    const pwcInChars: PWideChar; cInChars: Integer; cMaxItems: Integer;
    const psControl: pointer; const psState: pointer;
    pItems: PScriptItem; var pcItems: Integer): HRESULT; stdcall; external Usp10;

/// Uniscribe function to retrieve information about the current scripts
// - ppSp: Pointer to an array of pointers to SCRIPT_PROPERTIES structures
// indexed by script.
// - piNumScripts: Pointer to the number of scripts. The valid range for this
// value is 0 through piNumScripts-1.
function ScriptGetProperties(out ppSp: PScriptPropertiesArray;
  out piNumScripts: Integer): HRESULT; stdcall; external Usp10;

/// Uniscribe function to convert an array of run embedding levels to a map
// of visual-to-logical position and/or logical-to-visual position
// - cRuns: Number of runs to process
// - pbLevel: Array of run embedding levels
// - piVisualToLogical: List of run indices in visual order
// - piLogicalToVisual: List of visual run positions
function ScriptLayout(cRuns: Integer; const pbLevel: PByte;
    piVisualToLogical: PInteger; piLogicalToVisual: PInteger): HRESULT; stdcall; external Usp10;

/// Uniscribe function to generate glyphs and visual attributes for an Unicode run
// - hdc: Optional (see under caching)
// - psc: Uniscribe font metric cache handle
// - pwcChars: Logical unicode run
// - cChars: Length of unicode run
// - cMaxGlyphs: Max glyphs to generate
// - psa: Result of ScriptItemize (may have fNoGlyphIndex set)
// - pwOutGlyphs: Output glyph buffer
// - pwLogClust: Logical clusters
// - psva: Visual glyph attributes
// - pcGlyphs: Count of glyphs generated
function ScriptShape(hdc: HDC; var psc: pointer; const pwcChars: PWideChar;
    cChars: Integer; cMaxGlyphs: Integer; psa: PScriptAnalysis;
    pwOutGlyphs: PWord; pwLogClust: PWord; psva: PScriptVisAttr;
    var pcGlyphs: Integer): HRESULT; stdcall; external Usp10;

/// Uniscribe function to apply the specified digit substitution settings
// to the specified script control and script state structures
function ScriptApplyDigitSubstitution(
    const psds: Pointer; const psControl: pointer;
    const psState: pointer): HRESULT; stdcall; external Usp10;

// C++Builder code should #include <usp10.h> directly instead of using these
{$NODEFINE TScriptState }
{$NODEFINE PScriptState }
{$NODEFINE TScriptAnalysis }
{$NODEFINE PScriptAnalysis }
{$NODEFINE TScriptVisAttr }
{$NODEFINE PScriptVisAttr }
{$NODEFINE TScriptItem }
{$NODEFINE PScriptItem }
{$NODEFINE ScriptItemize }
{$NODEFINE ScriptGetProperties }
{$NODEFINE ScriptLayout }
{$NODEFINE ScriptShape }
{$NODEFINE ScriptApplyDigitSubstitution }

{$endif USE_UNISCRIBE}


implementation

const
  // those constants are not defined in earlier Delphi revisions
  cPI: single = 3.141592654;
  cPIdiv180: single = 0.017453292;
  c180divPI: single = 57.29577951;
  c2PI: double = 6.283185307;
  cPIdiv2: double = 1.570796326;

function RGBA(r, g, b, a: cardinal): COLORREF; {$ifdef HASINLINE}inline;{$endif}
begin
  Result := ((r shr 8) or ((g shr 8) shl 8) or ((b shr 8) shl 16) or ((a shr 8) shl 24));
end;

procedure SwapBuffer(P: PWordArray; PLen: Integer);
var i: integer;
begin
  for i := 0 to PLen-1 do
    P^[i] := swap(P^[i]);
end;

function GetTTFData(aDC: HDC; aTableName: PAnsiChar; var Ref: TWordDynArray): pointer;
var L: cardinal;
begin
  result := nil;
  L := GetFontData(aDC,PCardinal(aTableName)^,0,nil,0);
  if L=GDI_ERROR then
    exit;
  SetLength(ref,L shr 1+1);
  if GetFontData(aDC,PCardinal(aTableName)^,0,pointer(ref),L)=GDI_ERROR then
    exit;
  result := pointer(ref);
  SwapBuffer(Result,L shr 1);
end;

function PrinterDriverExists: boolean;
var Flags, Count, NumInfo: dword;
    Level: Byte;
begin
  // avoid using fPrinter.printers.count as this will raise an
  // exception if no printer driver is installed...
  Count := 0;
  Flags := PRINTER_ENUM_CONNECTIONS or PRINTER_ENUM_LOCAL;
  Level := 4;
  EnumPrinters(Flags, nil, Level, nil, 0, Count, NumInfo);
  result := (count > 0);
end;

function ParseFetchedPrinterStr(Str: PChar): PChar;
var
  P: PChar;
begin
  Result := Str;
  if Str=nil then Exit;
  P := Str;
  while P^=' ' do Inc(P);
  Result := P;
  while (P^<>#0) and (P^<>',') do Inc(P);
  if P^=',' then
    P^ := #0;
end;

function CurrentPrinterPaperSize: TPDFPaperSize;
var PtrHdl: THandle;
    PtrPPI: TPoint;
    size: TSize;
    tmp: integer;
    PtrDestSize: TSize;
    DefaultPrinter: array[0..1023] of Char;
    PC: PChar;
begin
  result := psUserDefined;
  if not PrinterDriverExists then
    exit;
  GetProfileString('windows','device',nil,DefaultPrinter,SizeOf(DefaultPrinter)-1);
  PC := ParseFetchedPrinterStr(DefaultPrinter);
  if (PC=nil) or (PC^=#0) then
    exit;
  try
    PtrHdl := CreateDC(nil,PC,nil,nil);
    try
      PtrPPI.x := GetDeviceCaps(PtrHdl, LOGPIXELSX);
      PtrPPI.y := GetDeviceCaps(PtrHdl, LOGPIXELSY);
      PtrDestSize.cx := GetDeviceCaps(PtrHdl, PHYSICALWIDTH);
      PtrDestSize.cy := GetDeviceCaps(PtrHdl, PHYSICALHEIGHT);
      size.cx := mulDiv(PtrDestSize.cx, 254,PtrPPI.x *10);
      size.cy := mulDiv(PtrDestSize.cy, 254,PtrPPI.y *10);
    finally
      DeleteDC(PtrHdl);
    end;
  except
    On Exception do // raised e.g. if no Printer is existing
      exit;
  end;
  with size do begin
    if cx < cy then begin // handle landscape or portrait at once
      tmp := cx;
      cx := cy;
      cy := tmp;
    end;
    case cy of
      148: result := psA5;
      210: result := psA4; // A4 (297 x 210mm)
      216: if cx=279 then
             result := psLetter else
           if cx=356 then
             result := psLegal;
      297: if cx=420 then
             result := psA3;
    end;
  end;
end;

function CurrentPrinterRes: TPoint;
var DefaultPrinter: array[0..1023] of Char;
    PC: PChar;
    PtrHdl: THandle;
begin
  result.X := 300;
  result.Y := 300; // default standard printer resolution
  if not PrinterDriverExists then
    exit;
  GetProfileString('windows','device',nil,DefaultPrinter,SizeOf(DefaultPrinter)-1);
  PC := ParseFetchedPrinterStr(DefaultPrinter);
  if (PC=nil) or (PC^=#0) then
    exit;
  try
    PtrHdl := CreateDC(nil,PC,nil,nil);
    try
      result.x := GetDeviceCaps(PtrHdl, LOGPIXELSX);
      result.y := GetDeviceCaps(PtrHdl, LOGPIXELSY);
    finally
      DeleteDC(PtrHdl);
    end;
  except
    On Exception do // raised e.g. if no Printer is existing
      exit;
  end;
end;

procedure GDICommentBookmark(MetaHandle: HDC; const aBookmarkName: RawUTF8);
var Data: RawByteString;
    D: PAnsiChar;
    L: integer;
begin // high(TPdfGDIComment)<$47 so it will never begin with GDICOMMENT_IDENTIFIER
  L := length(aBookmarkName);
  SetLength(Data,L+1);
  D := pointer(Data);
  D^ := AnsiChar(pgcBookmark);
  MoveFast(pointer(aBookmarkName)^,D[1],L);
  Windows.GdiComment(MetaHandle,L+1,D);
end;

procedure GDICommentOutline(MetaHandle: HDC; const aTitle: RawUTF8; aLevel: Integer);
var Data: RawByteString;
    D: PAnsiChar;
    L: integer;
begin // high(TPdfGDIComment)<$47 so it will never begin with GDICOMMENT_IDENTIFIER
  L := length(aTitle);
  SetLength(Data,L+2);
  D := pointer(Data);
  D[0] := AnsiChar(pgcOutline);
  D[1] := AnsiChar(aLevel);
  MoveFast(pointer(aTitle)^,D[2],L);
  Windows.GdiComment(MetaHandle,L+2,D);
end;

procedure GDICommentLink(MetaHandle: HDC; const aBookmarkName: RawUTF8; const aRect: TRect;
  NoBorder: boolean);
var Data: RawByteString;
    D: PAnsiChar;
    L: integer;
begin // high(TPdfGDIComment)<$47 so it will never begin with GDICOMMENT_IDENTIFIER
  L := length(aBookmarkName);
  SetLength(Data,L+(1+sizeof(TRect)));
  D := pointer(Data);
  if NoBorder then
    D^ := AnsiChar(pgcLinkNoBorder) else
    D^ := AnsiChar(pgcLink);
  PRect(D+1)^ := aRect;
  MoveFast(pointer(aBookmarkName)^,D[1+sizeof(TRect)],L);
  Windows.GdiComment(MetaHandle,L+(1+sizeof(TRect)),D);
end;

{$ifndef DELPHI5OROLDER}
// used by TPdfFontTrueType.PrepareForSaving()
function GetTTCIndex(const FontName: RawUTF8; var ttcIndex: Word;
  const FontCount: LongWord): Boolean;
// Looks up ttcIndex from list of font names in known ttc font collections.
// For some locales, the lookup may fail
// Result must not be greater than FontCount-1
const
  // Font names for Simp/Trad Chinese, Japanese, Korean locales.
  BATANG_KO = #48148#53461;
  BATANGCHE_KO = BATANG_KO + #52404;
  GUNGSUH_KO = #44417#49436;
  GUNGSUHCHE_KO = GUNGSUH_KO + #52404;
  GULIM_KO = #44404#47548;
  GULIMCHE_KO = GULIM_KO + #52404;
  DOTUM_KO = #46027#50880;
  DOTUMCHE_KO = DOTUM_KO + #52404;
  MINGLIU_CH = #32048#26126#39636;
  PMINGLIU_CH = #26032 + MINGLIU_CH;
  MINGLIU_HK_CH = MINGLIU_CH + '_hkscs';
  MINGLIU_XB_CH = MINGLIU_CH + '-extb';
  PMINGLIU_XB_CH = PMINGLIU_CH + '-extb';
  MINGLIU_XBHK_CH = MINGLIU_CH + '-extb_hkscs';
  MSGOTHIC_JA = #65325#65331#32#12468#12471#12483#12463;
  MSPGOTHIC_JA = #65325#65331#32#65328#12468#12471#12483#12463;
  MSMINCHO_JA = #65325#65331#32#26126#26397;
  MSPMINCHO_JA = #65325#65331#32#65328#26126#26397;
  SIMSUN_CHS = #23435#20307;
  NSIMSUN_CHS = #26032#23435#20307;
var
  lcfn: SynUnicode;
begin
  result := True;
  UTF8ToSynUnicode(fontName,lcfn);
  lcfn := {$ifdef UNICODE}SysUtils.LowerCase{$else}WideLowerCase{$endif}(lcfn);
  // batang.ttc (Korean)
  if (lcfn='batang') or (lcfn=BATANG_KO) then
    ttcIndex := 0 else
  if (lcfn='batangche') or (lcfn=BATANGCHE_KO) then
    ttcIndex := 1 else
  if (lcfn='gungsuh') or (lcfn=GUNGSUH_KO) then
    ttcIndex := 2 else
  if (lcfn='gungsuhche') or (lcfn=GUNGSUHCHE_KO) then
    ttcIndex := 3 else
  // cambria.ttc
  if lcfn='cambria' then
    ttcIndex := 0 else
  if lcfn='cambria math' then
    ttcIndex := 1 else
  // gulim.ttc (Korean)
  if (lcfn='gulim') or (lcfn=GULIM_KO) then
    ttcIndex := 0 else
  if (lcfn='gulimche') or (lcfn=GULIMCHE_KO) then
    ttcIndex := 1 else
  if (lcfn='dotum') or (lcfn=DOTUM_KO) then
    ttcIndex := 2 else
  if (lcfn='dotumche') or (lcfn=DOTUMCHE_KO) then
    ttcIndex := 3 else
  // mingliu.ttc (Traditional Chinese)
  if (lcfn='mingliu') or (lcfn=MINGLIU_CH) then
    ttcIndex := 0 else
  if (lcfn='pmingliu') or (lcfn=PMINGLIU_CH) then
    ttcIndex := 1 else
  if (lcfn='mingliu_hkscs') or (lcfn=MINGLIU_HK_CH) then
    ttcIndex := 2 else
  // mingliub.ttc (Traditional Chinese)
  if (lcfn='mingliu-extb') or (lcfn=MINGLIU_XB_CH) then
    ttcIndex := 0 else
  if (lcfn='pmingliu-extb') or (lcfn=PMINGLIU_XB_CH) then
    ttcIndex := 1 else
  if (lcfn='mingliu_hkscs-extb') or (lcfn=MINGLIU_XBHK_CH) then
    ttcIndex := 2 else
  // msgothic.ttc (Japanese)
  if (lcfn='ms gothic') or
     (lcfn={$ifdef UNICODE}SysUtils.LowerCase{$else}WideLowerCase{$endif}(MSGOTHIC_JA)) then
    ttcIndex := 0 // MSGOTHIC_JA contains full-width uppercase chars
  else if (lcfn='ms pgothic') or
    (lcfn={$ifdef UNICODE}SysUtils.LowerCase{$else}WideLowerCase{$endif}(MSPGOTHIC_JA)) then
      ttcIndex := 1 else
  if lcfn='ms ui gothic' then
    ttcIndex := 2 else
  // msmincho.ttc (Japanese)
  if (lcfn='ms mincho') or
     (lcfn={$ifdef UNICODE}SysUtils.LowerCase{$else}WideLowerCase{$endif}(MSMINCHO_JA)) then
    ttcIndex := 0 else
  if (lcfn='ms pmincho') or
     (lcfn={$ifdef UNICODE}SysUtils.LowerCase{$else}WideLowerCase{$endif}(MSPMINCHO_JA)) then
    ttcIndex := 1 else
  // simsun.ttc (Simplified Chinese)
  if (lcfn='simsun') or (lcfn=SIMSUN_CHS) then
    ttcIndex := 0 else
  if (lcfn='nsimsun') or (lcfn=NSIMSUN_CHS) then
    ttcIndex := 1 else
    result := False;
  if result and (ttcIndex>(FontCount-1)) then
    result := False;
end;
{$endif DELPHI5OROLDER}


{$ifdef USE_ARC}
type
  tcaRes = (caMoveto, caLine, caCurve, caPosition);
  teaDrawtype = record
    res: tcaRes;
    pts: array[0..2] of record x, y: single;
  end;
  end;
  teaDrawArray = array of teaDrawtype;

function CalcCurveArcData(centerx, centery, W, H, Sx, Sy, Ex, Ey: integer;
  aClockWise: boolean; arctype: TPdfCanvasArcType; out res: teaDrawArray): boolean;
type
  TCoeff = array[0..3] of double;
  TCoeffArray = array[0..1, 0..3] of TCoeff;
const
  // coefficients for error estimation
  // while using cubic Bezier curves for approximation
  // 0 < b/a < 1/4
  coeffsLow: TCoeffArray = (
  ((3.85268, -21.229, -0.330434, 0.0127842),
   (-1.61486, 0.706564, 0.225945, 0.263682),
   (-0.910164, 0.388383, 0.00551445, 0.00671814),
   (-0.630184, 0.192402, 0.0098871, 0.0102527)),
  ((-0.162211, 9.94329, 0.13723, 0.0124084),
   (-0.253135, 0.00187735, 0.0230286, 0.01264),
   (-0.0695069, -0.0437594, 0.0120636, 0.0163087),
   (-0.0328856, -0.00926032, -0.00173573, 0.00527385)));
  // coefficients for error estimation
  // while using cubic Bezier curves for approximation
  // 1/4 <= b/a <= 1
  coeffsHigh: TCoeffArray = (
  ((0.0899116, -19.2349, -4.11711, 0.183362),
   (0.138148, -1.45804, 1.32044, 1.38474),
   (0.230903, -0.450262, 0.219963, 0.414038),
   (0.0590565, -0.101062, 0.0430592, 0.0204699)),
  ((0.0164649, 9.89394, 0.0919496, 0.00760802),
   (0.0191603, -0.0322058, 0.0134667, -0.0825018),
   (0.0156192, -0.017535, 0.00326508, -0.228157),
   (-0.0236752, 0.0405821, -0.0173086, 0.176187)));
  // safety factor to convert the "best" error approximation
  // into a "max bound" error
  safety: TCoeff = (0.001, 4.98, 0.207, 0.0067);

var fcx, fcy: double; //  center of the ellipse
    faRad, fbRad: double; // Semi-major axis
    feta1, feta2: double; //  Start End angle of the arc
    fx1, fy1, fx2, fy2: double;  //start and and endpoint
    fxLeft, fyUp: double;  // leftmost point of the arc
    fwidth, fheight: double; //   Horizontal width of the arc Vertical height of the arc
    fArctype: TPdfCanvasArcType;  //Indicator for center to endpoints line inclusion
    fClockWise : boolean;

procedure InitFuncData;
var lambda1, lambda2 : double;
begin
  fcx := centerx;
  fcy := centery;
  faRad := (W-1) / 2;
  fbRad := (H-1) / 2;
  fArctype := arctype;
  // Calculate Rotation at Start and EndPoint
  fClockWise := aClockWise;
  if aclockwise then begin
    lambda1 := ArcTan2(Sy - fcy, Sx - fcx);
    lambda2 := ArcTan2(Ey - fcy, Ex - fcx);
  end else begin
    lambda2 := ArcTan2(Sy - fcy, Sx - fcx);
    lambda1 := ArcTan2(Ey - fcy, Ex - fcx);
  end;
  feta1 := ArcTan2(sin(lambda1) / fbRad, cos(lambda1) / faRad);
  feta2 := ArcTan2(sin(lambda2) / fbRad, cos(lambda2) / faRad);
  // make sure we have eta1 <= eta2 <= eta1 + 2 PI
  feta2 := feta2 - (c2PI * floor((feta2 - feta1) / c2PI));
  // the preceding correction fails if we have exactly et2 - eta1 = 2 PI
  // it reduces the interval to zero length
  if SameValue(feta1, feta2) then
    feta2 := feta2 + c2PI;
  // start point
  fx1 := fcx + (faRad * cos(feta1));
  fy1 := fcy + (fbRad * sin(feta1));
  // end point
  fx2 := fcx + (faRad * cos(feta2));
  fy2 := fcy + (fbRad * sin(feta2));
  // Dimensions
  fxLeft := min(fx1, fx2);
  fyUp := min(fy1, fy2);
  fwidth := max(fx1, fx2) - fxLeft;
  fheight := max(fy1, fy2) - fyUp;
end;

function estimateError(etaA, etaB: double): double;
var coeffs: ^TCoeffArray;
    c0, c1, cos2, cos4, cos6, dEta, eta, x: double;

  function rationalFunction(x: double; const c: TCoeff): double;
  begin
    result := (x * (x * c[0] + c[1]) + c[2]) / (x + c[3]);
  end;

begin
  eta := 0.5 * (etaA + etaB);
  x := fbRad / faRad;
  dEta := etaB - etaA;
  cos2 := cos(2 * eta);
  cos4 := cos(4 * eta);
  cos6 := cos(6 * eta);
  // select the right coeficients set according to degree and b/a
  if x < 0.25 then
    coeffs := @coeffsLow else
    coeffs := @coeffsHigh;
  c0 := rationalFunction(x, coeffs[0][0]) +
    cos2 * rationalFunction(x, coeffs[0][1]) +
    cos4 * rationalFunction(x, coeffs[0][2]) +
    cos6 * rationalFunction(x, coeffs[0][3]);
  c1 := rationalFunction(x, coeffs[1][0]) +
    cos2 * rationalFunction(x, coeffs[1][1]) +
    cos4 * rationalFunction(x, coeffs[1][2]) +
    cos6 * rationalFunction(x, coeffs[1][3]);
  result := rationalFunction(x, safety) * faRad * exp(c0 + c1 * dEta);
end;

procedure BuildPathIterator;
var alpha: double;
    found: Boolean;
    n: integer;
    dEta, etaB, etaA: double;
    cosEtaB, sinEtaB, aCosEtaB, bSinEtaB, aSinEtaB, bCosEtaB, xB, yB, xBDot, yBDot: double;
    i: integer;
    t, xA, xADot, yA, yADot: double;
    ressize: integer; // Index var for result Array
    r: ^teaDrawtype;
    lstartx, lstarty : double;  // Start from
const
  defaultFlatness = 0.5; // half a pixel
begin
  // find the number of Bezier curves needed
  found := false;
  n := 1;
  while (not found) and (n < 1024) do begin
    dEta := (feta2 - feta1) / n;
    if dEta <= cPIdiv2 then begin
      etaB := feta1;
      found := true;
      for i := 0 to n - 1 do begin
        etaA := etaB;
        etaB := etaB + dEta;
        found := (estimateError(etaA, etaB) <= defaultFlatness);
        if not found then
          break;
      end;
    end;
    // if not found then
    n := n shl 1;
  end;
  dEta := (feta2 - feta1) / n;
  etaB := feta1;
  cosEtaB := cos(etaB);
  sinEtaB := sin(etaB);
  aCosEtaB := faRad * cosEtaB;
  bSinEtaB := fbRad * sinEtaB;
  aSinEtaB := faRad * sinEtaB;
  bCosEtaB := fbRad * cosEtaB;
  xB := fcx + aCosEtaB;
  yB := fcy + bSinEtaB;
  xBDot := -aSinEtaB;
  yBDot := +bCosEtaB;
  lstartx := xB;
  lstarty := yB;
  // calculate and reserve Space for the result
  ressize := n;
  case fArctype of
    acArc :     inc(ressize,1); // first move
    acArcTo:    inc(ressize,3); // first line and move
    acArcAngle: inc(ressize,1); // first move
    acPie:      inc(ressize,3); // first and last Line
    acChoord:   inc(ressize,2);
  end;
  SetLength(res, ressize);
  r := pointer(res);
  case fArctype of
    acArc: begin   // start with move
      r^.res := caMoveto;
      r^.pts[0].x := xB;
      r^.pts[0].y := yB;
      inc(r);
    end;
    acArcTo : begin   // start with line and move
      r^.res := caLine;
      if fClockwise then  begin
        r^.pts[0].x := fx1;
        r^.pts[0].y := fy1;
      end else begin
        r^.pts[0].x := fx2;
        r^.pts[0].y := fy2;
      end;
      inc(r);
      r^.res := caMoveto;
      r^.pts[0].x := fx1;
      r^.pts[0].y := fy1;
      inc(r);
    end;
    acArcAngle: ;
    acPie : begin
     r^.res := caMoveto;
     r^.pts[0].x := fcx;
     r^.pts[0].y := fcy;
     inc(r);
     r^.res := caLine;
     r^.pts[0].x := xB;
     r^.pts[0].y := yB;
     inc(r);
    end;
    acChoord : begin
     r^.res := caMoveto;
     r^.pts[0].x := xB;
     r^.pts[0].y := yB;
     inc(r);
    end;
  end;
  t := tan(0.5 * dEta);
  alpha := sin(dEta) * (sqrt(4 + 3 * t * t) - 1) / 3;
  for i := 0 to n - 1 do begin
    xA := xB;
    yA := yB;
    xADot := xBDot;
    yADot := yBDot;
    etaB := etaB + dEta;
    cosEtaB := cos(etaB);
    sinEtaB := sin(etaB);
    aCosEtaB := faRad * cosEtaB;
    bSinEtaB := fbRad * sinEtaB;
    aSinEtaB := faRad * sinEtaB;
    bCosEtaB := fbRad * cosEtaB;
    xB := fcx + aCosEtaB;
    yB := fcy + bSinEtaB;
    xBDot := -aSinEtaB;
    yBDot := bCosEtaB;
    r^.res := caCurve;
    r^.pts[0].x := xA + alpha * xADot;
    r^.pts[0].y := yA + alpha * yADot;
    r^.pts[1].x := xB - alpha * xBDot;
    r^.pts[1].y := yB - alpha * yBDot;
    r^.pts[2].x := xB;
    r^.pts[2].y := yB;
    inc(r);
  end; // Loop
  case fArctype of
  acArcTo: begin
    r^.res := caPosition;
    if fClockWise then begin
     r^.pts[0].x := fx2;
     r^.pts[0].y := fy2;
    end else begin
     r^.pts[0].x := fx1;
     r^.pts[0].y := fy1;
    end
  end;
  acPie: begin
    r^.res := caLine;
    r^.pts[0].x := fcx;
    r^.pts[0].y := fcy;
  end;
  acChoord: begin
    r^.res := caLine;
    r^.pts[0].x := lstartx;
    r^.pts[0].y := lstarty;
  end;
  end;
end;

begin
  res := nil;
  InitFuncData;  // Initialize Data
  buildPathIterator;
  result := length(res) > 1;
end;

{$endif USE_ARC}

{ TPdfObject }

constructor TPdfObject.Create;
begin
  FObjectNumber := -1;
end;

procedure TPdfObject.ForceSaveNow;
begin
  FSaveAtTheEnd := False;
end;

procedure TPdfObject.InternalWriteTo(W: TPdfWrite);
begin
{$ifdef USE_PDFSECURITY}
  if FObjectNumber>0 then
    with W.fDoc do begin
      fCurrentObjectNumber := FObjectNumber;
      fCurrentGenerationNumber := FGenerationNumber;
    end;
{$endif USE_PDFSECURITY}
end;

procedure TPdfObject.SetObjectNumber(Value: integer);
begin
  FObjectNumber := Value;
  if Value > 0 then
    FObjectType := otIndirectObject else
    FObjectType := otDirectObject;
end;

function TPdfObject.SpaceNotNeeded: boolean;
begin
  result := false;
end;

procedure TPdfObject.WriteTo(var W: TPdfWrite);
begin
  if FObjectType=otDirectObject then
    InternalWriteTo(W) else
    W.AddWithSpace(FObjectNumber).AddWithSpace(FGenerationNumber).Add('R');
end;

procedure TPdfObject.WriteValueTo(var W: TPdfWrite);
begin
  if FObjectType<>otIndirectObject then
    raise EPdfInvalidOperation.Create('WriteValueTo');
  W.AddWithSpace(FObjectNumber).AddWithSpace(FGenerationNumber).Add('obj'+CRLF);
  InternalWriteTo(W);
  W.Add(CRLF+'endobj'#10);
end;


{ PdfVirtualObject }

constructor TPdfVirtualObject.Create(AObjectId: integer);
begin
  inherited Create;
  FObjectNumber := AObjectId;
  FObjectType := otVirtualObject;
end;


{ TPdfNull }

procedure TPdfNull.InternalWriteTo(W: TPdfWrite);
begin
  W.Add('null');
end;


{ TPdfBoolean }

procedure TPdfBoolean.InternalWriteTo(W: TPdfWrite);
begin
  W.Add(BOOL_UTF8[Value]);
end;

constructor TPdfBoolean.Create(AValue: Boolean);
begin
  inherited Create;
  Value := AValue;
end;


{ TPdfNumber }

procedure TPdfNumber.InternalWriteTo(W: TPdfWrite);
begin
  W.Add(FValue);
end;

constructor TPdfNumber.Create(AValue: integer);
begin
  inherited Create;
  Value := AValue;
end;


{ TPdfReal }

procedure TPdfReal.InternalWriteTo(W: TPdfWrite);
begin
  W.Add(Value);
end;

constructor TPdfReal.Create(AValue: double);
begin
  inherited Create;
  Value := AValue;
end;


{ TPdfText }

procedure TPdfText.InternalWriteTo(W: TPdfWrite);
begin
  // if the value has multibyte character, convert the value to hex unicode.
  // otherwise, escape characters.
  if SysLocale.FarEast and _HasMultiByteString(pointer(FValue)) then
    W.Add('<FEFF').AddToUnicodeHex(FValue).Add('>') else
    W.Add('(').AddEscapeContent(FValue).Add(')');
end;

constructor TPdfText.Create(const AValue: RawByteString);
begin
  inherited Create;
  Value := AValue;
end;

function TPdfText.SpaceNotNeeded: boolean;
begin
  result := True;
end;


{ TPdfTextUTF8 }

constructor TPdfTextUTF8.Create(const AValue: RawUTF8);
begin
  inherited Create;
  Value := AValue;
end;

procedure TPdfTextUTF8.InternalWriteTo(W: TPdfWrite);
var Len: Integer;
begin
  // if the value has multibyte character, convert the value to hex unicode.
  // otherwise, escape characters
  if IsWinAnsiU8Bit(Pointer(FValue)) then
    W.Add('(').AddEscapeContent(Utf8ToWinAnsi(FValue)).Add(')') else
    W.Add('<FEFF').AddUnicodeHex(
      Pointer(Utf8DecodeToRawUnicodeUI(FValue,@Len)),Len shr 1).Add('>');
end;

function TPdfTextUTF8.SpaceNotNeeded: boolean;
begin
  result := True;
end;


{ TPdfTextString }

constructor TPdfTextString.Create(const AValue: string);
begin
  inherited Create(StringToUTF8(AValue));
end;

function TPdfTextString.GetValue: string;
begin
  result := UTF8ToString(FValue);
end;

procedure TPdfTextString.SetValue(const Value: string);
begin
  FValue := StringToUTF8(Value);
end;


{ TPdfRawText }

procedure TPdfRawText.InternalWriteTo(W: TPdfWrite);
begin
  W.Add(FValue);
end;

function TPdfRawText.SpaceNotNeeded: boolean;
begin
  result := false;
end;


{ TPdfClearText }

constructor TPdfClearText.Create(Buffer: pointer; Len: integer);
var tmp: RawByteString;
begin
  SetString(tmp,PAnsiChar(Buffer),Len);
  inherited Create(tmp);
end;

procedure TPdfClearText.InternalWriteTo(W: TPdfWrite);
begin
  W.Add('(').AddEscape(pointer(FValue),Length(FValue)).Add(')');
end;


{ TPdfName }

procedure TPdfName.InternalWriteTo(W: TPdfWrite);
begin
  W.Add('/').AddEscapeName(pointer(FValue));
end;


{ TPdfArray }

function TPdfArray.GetItems(Index: integer): TPdfObject;
begin
  result := TPdfObject(FArray[Index]);
  if result.ObjectType=otVirtualObject then
    if FObjectMgr<>nil then
      result := FObjectMgr.GetObject(result.ObjectNumber) else
      result := nil;
end;

function TPdfArray.GetItemCount: integer;
begin
  if self=nil then
    Result := 0 else
    Result := FArray.Count;
end;

procedure TPdfArray.InternalWriteTo(W: TPdfWrite);
var i: integer;
begin
  inherited;
  W.Add('[');
  for i := 0 to FArray.Count-1 do
  with TPdfObject(FArray.List[i]) do begin
    if (i<>0) and not SpaceNotNeeded then
      W.Add(' ');
    WriteTo(W);
  end;
  W.Add(']');
end;

constructor TPdfArray.Create(AObjectMgr: TPdfObjectMgr);
begin
  inherited Create;
  FArray := TList.Create;
  FObjectMgr := AObjectMgr;
end;

constructor TPdfArray.Create(AObjectMgr: TPdfObjectMgr;
  const AArray: array of Integer);
var i: integer;
begin
  Create(AObjectMgr);
  for i := 0 to High(AArray) do
    AddItem(TPdfNumber.Create(AArray[i]));
end;

constructor TPdfArray.Create(AObjectMgr: TPdfObjectMgr;
  AArray: PWordArray; AArrayCount: integer);
var i: integer;
begin
  Create(AObjectMgr);
  for i := 0 to AArrayCount-1 do
    AddItem(TPdfNumber.Create(AArray^[i]));
end;

constructor TPdfArray.CreateNames(AObjectMgr: TPdfObjectMgr;
  const AArray: array of PDFString);
var i: integer;
begin
  Create(AObjectMgr);
  for i := 0 to high(AArray) do
    AddItem(TPdfName.Create(AArray[i]));
end;

constructor TPdfArray.CreateReals(AObjectMgr: TPdfObjectMgr;
  const AArray: array of double);
var i: integer;
begin
  Create(AObjectMgr);
  for i := 0 to high(AArray) do
    AddItem(TPdfReal.Create(AArray[i]));
end;

destructor TPdfArray.Destroy;
var i: integer;
begin
  for i := 0 to FArray.Count-1 do
    TPdfObject(FArray.List[i]).Free;
  FArray.Free;
  inherited;
end;

function TPdfArray.AddItem(AItem: TPdfObject): integer;
begin
  result := FArray.IndexOf(AItem);
  if result>=0 then
    exit; // if AItem already exists, do nothing
  if AItem.ObjectType=otDirectObject then
    result := FArray.Add(AItem) else
    result := FArray.Add(TPdfVirtualObject.Create(AItem.ObjectNumber))
end;

procedure TPdfArray.InsertItem(Index: Integer; AItem: TPdfObject);
begin
  if FArray.IndexOf(AItem)>=0 then
    exit; // if AItem already exists, do nothing
  if AItem.ObjectType=otDirectObject then
    FArray.Insert(Index, AItem) else
    FArray.Insert(Index, TPdfVirtualObject.Create(AItem.ObjectNumber))
end;

function TPdfArray.FindName(const AName: PDFString): TPdfName;
var i: integer;
begin
  for i := 0 to ItemCount-1 do begin
    result := TPdfName(FArray.List[i]);
    if (result<>nil) and result.InheritsFrom(TPdfName) and
       (result.Value=AName) then
      Exit;
  end;
  result := nil;
end;

function TPdfArray.RemoveName(const AName: PDFString): boolean;
var AObject: TPdfObject;
begin
  result := false;
  AObject := FindName(AName);
  if AObject<>nil then begin
    FArray.Remove(AObject);
    if AObject.ObjectType=otDirectObject then
      AObject.Free;
    result := true;
  end;
end;

function TPdfArray.SpaceNotNeeded: boolean;
begin
  result := True;
end;


{ TPdfDictionaryElement }

function TPdfDictionaryElement.GetKey: PDFString;
begin
  if self=nil then
    result := '' else
    result := FKey.Value;
end;

constructor TPdfDictionaryElement.Create(const AKey: PDFString; AValue: TPdfObject;
  AInternal: Boolean);
begin
  if not (AValue is TPdfObject) then
    raise EPdfInvalidValue.Create('TPdfDictionaryElement');
  FKey := TPdfName.Create(AKey);
  FValue := AValue;
  FIsInternal := AInternal;
end;

destructor TPdfDictionaryElement.Destroy;
begin
  FKey.Free;
  FValue.Free;
  inherited;
end;


{ TPdfDictionary }

function TPdfDictionary.GetItems(Index: integer): TPdfDictionaryElement;
begin
  result := TPdfDictionaryElement(FArray[Index]);
end;

function TPdfDictionary.GetItemCount: integer;
begin
  if self=nil then
    Result := 0 else
    Result := FArray.Count;
end;

procedure TPdfDictionary.DirectWriteto(W: TPdfWrite; Secondary: TPdfDictionary);
procedure WriteArray(aArray: TList);
var i: integer;
    Element: TPdfDictionaryElement;
begin
  for i := 0 to aArray.Count-1 do begin
    Element := aArray.List[i];
    if not Element.IsInternal then begin
      Element.FKey.WriteTo(W);
      if not Element.FValue.SpaceNotNeeded then
        W.Add(' ');
      Element.FValue.WriteTo(W);
    end;
  end;
end;
begin
  inherited InternalWriteTo(W);
  W.Add('<<');
  WriteArray(FArray);
  if Secondary<>nil then
    WriteArray(Secondary.FArray);
  W.Add('>>');
end;

procedure TPdfDictionary.InternalWriteTo(W: TPdfWrite);
begin
  DirectWriteto(W,nil);
end;

constructor TPdfDictionary.Create(AObjectMgr: TPdfObjectMgr);
begin
  inherited Create;
  FArray := TList.Create;
  FObjectMgr := AObjectMgr;
end;

destructor TPdfDictionary.Destroy;
var i: integer;
begin
  for i := 0 to FArray.Count-1 do
    TPdfDictionaryElement(FArray[i]).Free;
  FArray.Free;
  inherited;
end;

function TPdfDictionary.ValueByName(const AKey: PDFString): TPdfObject;
var i: integer;
begin
  if self<>nil then
  for i := 0 to FArray.Count-1 do
  with TPdfDictionaryElement(FArray.List[i]) do
    if FKey.Value=AKey then begin
      result := Value;
      if result.ObjectType=otVirtualObject then
        if FObjectMgr<>nil then
          result := FObjectMgr.GetObject(result.ObjectNumber) else
          result := nil;
      exit;
    end;
  result := nil;
end;

function TPdfDictionary.PdfNumberByName(const AKey: PDFString): TPdfNumber;
begin
  result := TPdfNumber(ValueByName(AKey));
end;

function TPdfDictionary.PdfTextByName(const AKey: PDFString): TPdfText;
begin
  result := TPdfText(ValueByName(AKey));
end;

function TPdfDictionary.PdfTextValueByName(const AKey: PDFString): PDFString;
var P: TPdfText;
begin
  P := TPdfText(ValueByName(AKey));
  if P=nil then
    result := '' else
    result := P.Value;
end;

function TPdfDictionary.PdfTextStringValueByName(const AKey: PDFString): string;
var P: TPdfTextString;
begin
  P := TPdfTextString(ValueByName(AKey));
  if P=nil then
    result := '' else
    result := P.Value;
end;

function TPdfDictionary.PdfTextUTF8ValueByName(const AKey: PDFString): RawUTF8;
var P: TPdfTextUTF8;
begin
  P := TPdfTextUTF8(ValueByName(AKey));
  if P=nil then
    result := '' else
    result := P.Value;
end;

function TPdfDictionary.PdfRealByName(const AKey: PDFString): TPdfReal;
begin
  result := TPdfReal(ValueByName(AKey));
end;

function TPdfDictionary.PdfNameByName(const AKey: PDFString): TPdfName;
begin
  result := TPdfName(ValueByName(AKey));
end;

function TPdfDictionary.PdfDictionaryByName(const AKey: PDFString): TPdfDictionary;
begin
  result := TPdfDictionary(ValueByName(AKey));
end;

function TPdfDictionary.PdfArrayByName(const AKey: PDFString): TPdfArray;
begin
  result := TPdfArray(ValueByName(AKey));
end;

function TPdfDictionary.PdfBooleanByName(const AKey: PDFString): TPdfBoolean;
begin
  result := TPdfBoolean(ValueByName(AKey));
end;

procedure TPdfDictionary.AddItem(const AKey: PDFString; AValue: TPdfObject;
  AInternal: Boolean);
var FItem: TPdfDictionaryElement;
begin
  if self=nil then
    exit;
  RemoveItem(AKey);
  if AValue.ObjectType=otDirectObject then
    FItem := TPdfDictionaryElement.Create(AKey, AValue, AInternal) else
    FItem := TPdfDictionaryElement.Create(AKey,
      TPdfVirtualObject.Create(AValue.ObjectNumber), AInternal);
  FArray.Add(FItem);
end;

procedure TPdfDictionary.AddItem(const AKey, AValue: PDFString);
begin
  AddItem(AKey,TPdfName.Create(AValue));
end;

procedure TPdfDictionary.AddItem(const AKey: PDFString; AValue: integer);
begin
  AddItem(AKey,TPdfNumber.Create(AValue));
end;

procedure TPdfDictionary.AddItemText(const AKey, AValue: PDFString);
begin
  AddItem(AKey,TPdfText.Create(AValue));
end;

procedure TPdfDictionary.AddItemTextUTF8(const AKey: PDFString; const AValue: RawUTF8);
begin
  AddItem(AKey,TPdfTextUTF8.Create(AValue));
end;

procedure TPdfDictionary.AddItemTextString(const AKey: PDFString;
  const AValue: string);
begin
  AddItem(AKey,TPdfTextString.Create(AValue));
end;

procedure TPdfDictionary.RemoveItem(const AKey: PDFString);
var i: integer;
    FElement: TPdfDictionaryElement;
begin
  if Self<>nil then
  for i := 0 to FArray.Count-1 do begin
    FElement := FArray.List[i];
    if FElement.FKey.Value=AKey then begin
      FArray.Remove(FElement);
      FElement.Free;
      Break;
    end;
  end;
end;

function TPdfDictionary.getTypeOf: PDFString;
var PdfName: TPdfName;
begin
  PdfName := PdfNameByName('Type');
  if PdfName<>nil then
    result := PdfName.Value else
    result := '';
end;

function TPdfDictionary.SpaceNotNeeded: boolean;
begin
  result := True;
end;


{ TPdfStream }

procedure TPdfStream.InternalWriteTo(W: TPdfWrite);
var FLength: TPdfNumber;
    TmpStream: TMemoryStream;
    TmpSize: integer;
    Buf: pointer;
begin
  inherited;
  FLength := FAttributes.PdfNumberByName('Length');
  FWriter.Save; // flush FWriter content
  Buf := TMemoryStream(FWriter.fDestStream).Memory;
  TmpSize := FWriter.Position;
  TmpStream := nil;
  try
    if FFilter='FlateDecode' then
      if TmpSize<100 then // don't compress tiny blocks
        FFilter := '' else begin
        TmpStream := THeapMemoryStream.Create;
        {$ifdef USE_SYNZIP}
        TmpSize := CompressStream(Buf,TmpSize,TmpStream,7,true);
        {$else}
        with TCompressionStream.Create(clMax, TmpStream) do
        begin
          Write(Buf^,TmpSize);
          Free;
        end;
        TmpSize := TmpStream.Size;
        {$endif}
        Buf := TmpStream.Memory;
      end;
    FLength.Value := TmpSize;
    if FFilter<>'' then
      FAttributes.AddItem('Filter',FFilter);
    FAttributes.DirectWriteTo(W,FSecondaryAttributes);
  {$ifdef USE_PDFSECURITY}
    if (TmpSize>0) and (W.fDoc.fEncryption<>nil) and not FDoNotEncrypt then
      W.fDoc.fEncryption.EncodeBuffer(Buf^,Buf^,TmpSize);
  {$endif USE_PDFSECURITY}
    W.Add(#10'stream'#10).Add(Buf,TmpSize).
      Add(#10'endstream');
    FWriter.fDestStream.Size := 0; // release internal stream memory
  finally
    TmpStream.Free;
  end;
end;

constructor TPdfStream.Create(ADoc: TPdfDocument; DontAddToFXref: boolean=false);
var FXref: TPdfXRef;
begin
  inherited Create;
  if DontAddToFXref then
    FXRef := nil else begin
    FXRef := ADoc.FXref;
    FXRef.AddObject(self);
  end;
  FAttributes := TPdfDictionary.Create(FXref);
  FAttributes.AddItem('Length', TPdfNumber.Create(0));
  if ADoc.CompressionMethod=cmFlateDecode then
    FFilter := 'FlateDecode';
  FWriter := TPdfWrite.Create(ADoc,THeapMemoryStream.Create);
end;

destructor TPdfStream.Destroy;
begin
  FWriter.fDestStream.Free;
  FWriter.Free;
  FAttributes.Free;
  inherited;
end;


{ TPdfBinary }

procedure TPdfBinary.InternalWriteTo(W: TPdfWrite);
begin
  inherited;
  W.Add(Stream.Memory,FStream.Size);
end;

constructor TPdfBinary.Create;
begin
  inherited;
  FStream := THeapMemoryStream.Create;
end;

destructor TPdfBinary.Destroy;
begin
  FStream.Free;
  inherited;
end;


{ utility functions }

function _DateTimeToPdfDate(ADate: TDateTime): TPdfDate;
var D: array[2..8] of word;
    i: PtrInt;
begin
  DecodeDate(ADate,D[2],D[3],D[4]);
  DecodeTime(ADate,D[5],D[6],D[7],D[8]);
  SetLength(result,17);
  YearToPChar(D[2],pointer(PtrInt(result)+2));
  PWord(result)^ := ord('D')+ord(':')shl 8;
  for i := 3 to 7 do
    PWordArray(pointer(result))^[i] := TwoDigitLookupW[D[i]];
  PByteArray(result)[16] := ord('Z');
//  Assert(abs(_PdfDateToDateTime(result)-ADate)<MSecsPerSec);
end;

const // not existing before Delphi 7
  HoursPerDay   = 24;
  MinsPerHour   = 60;
  SecsPerMin    = 60;
  MSecsPerSec   = 1000;
  MinsPerDay    = HoursPerDay * MinsPerHour;
  SecsPerDay    = MinsPerDay * SecsPerMin;
  MSecsPerDay   = SecsPerDay * MSecsPerSec;

function _PdfDateToDateTime(const AText: TPdfDate): TDateTime;
var Y,M,D, H,MI,SS: cardinal;
begin
  if Length(AText)<16 then
    EConvertError.CreateRes(@SDateEncodeError);
  Y := ord(AText[3])*1000+ord(AText[4])*100+ord(AText[5])*10+ord(AText[6])
    -(48+480+4800+48000);
  M := ord(AText[7])*10+ord(AText[8])-(48+480);
  D := ord(AText[9])*10+ord(AText[10])-(48+480);
  result := EncodeDate(Y,M,D);
  H := ord(AText[11])*10+ord(AText[12])-(48+480);
  MI := ord(AText[13])*10+ord(AText[14])-(48+480);
  SS := ord(AText[15])*10+ord(AText[16])-(48+480);;
  if (H<24) and (MI<60) and (SS<60) then // inlined EncodeTime()
    result := result + (H * (MinsPerHour * SecsPerMin * MSecsPerSec) +
      MI * (SecsPerMin * MSecsPerSec) + SS * MSecsPerSec) / MSecsPerDay else
    EConvertError.CreateRes(@SDateEncodeError);
end;

function _HasMultiByteString(Value: PAnsiChar): boolean;
begin
  if Value<>nil then
    while true do
      if Value^=#0 then
        Break else
      if not (Value^ in LeadBytes) then
        inc(Value) else begin
        result := true;
        exit;
      end;
  result := false;
end;

function RawUTF8ToPDFString(const Value: RawUTF8): PDFString;
begin
  result := CurrentAnsiConvert.UTF8BufferToAnsi(pointer(Value),length(Value));
end;

function UInt32ToPDFString(Value : Cardinal): PDFString;
var tmp: array[0..23] of AnsiChar;
    P: PAnsiChar;
begin
  P := StrUInt32(@tmp[23],Value);
  SetString(result,P,@tmp[23]-P);
end;

function PdfRect(Left, Top, Right, Bottom: Single): TPdfRect;
begin
  result.Left := Left;
  result.Top := Top;
  result.Right := Right;
  result.Bottom := Bottom;
end;

function PdfRect(const Box: TPdfBox): TPdfRect;
begin
  result.Left := Box.Left;
  result.Top := Box.Top;
  result.Right := Box.Left+Box.Width;
  result.Bottom := Box.Top-Box.Height;
end;

function PdfBox(Left, Top, Width, Height: Single): TPdfBox;
begin
  result.Left := Left;
  result.Top := Top;
  result.Width := Width;
  result.Height := Height;
end;

function CombineTransform(xform1, xform2: XFORM): XFORM;
begin
  Result.eM11 := xform1.eM11 * xform2.eM11 + xform1.eM12 * xform2.eM21;
  Result.eM12 := xform1.eM11 * xform2.eM12 + xform1.eM12 * xform2.eM22;
  Result.eM21 := xform1.eM21 * xform2.eM11 + xform1.eM22 * xform2.eM21;
  Result.eM22 := xform1.eM21 * xform2.eM12 + xform1.eM22 * xform2.eM22;
  Result.eDx  := xform1.eDx  * xform2.eM11 + xform1.eDy  * xform2.eM21 + xform2.eDx;
  Result.eDy  := xform1.eDx  * xform2.eM12 + xform1.eDy  * xform2.eM22 + xform2.eDy;
end;

procedure InitTransformation(x: PXForm; var fIntFactorX, fIntFactorY,
  fIntOffsetX, fIntOffsetY: Single);
begin
  if Assigned(x) then
  begin
    fIntFactorX := x^.eM11;
    fIntFactorY := x^.eM22;
    fIntOffsetX := x^.eDx;
    fIntOffsetY := x^.eDy;
  end
  else
  begin
    fIntFactorX := 1;
    fIntFactorY := 1;
    fIntOffsetX := 0;
    fIntOffsetY := 0;
  end;
end;

function DefaultIdentityMatrix: XFORM;
begin
  Result.eM11 := 1;
  Result.eM12 := 0;
  Result.eM21 := 0;
  Result.eM22 := 1;
  Result.eDx := 0;
  Result.eDy := 0;
end;

function ScaleRect(r: TRect; fScaleX, fScaleY: Single): TRect;
begin
  Result.Left := Trunc(r.Left * fScaleX);
  Result.Top := Trunc(r.Top * fScaleY);
  Result.Right := Trunc(r.Right * fScaleX);
  Result.Bottom := Trunc(r.Bottom * fScaleY);
end;

function PrepareTransformation(fIntFactorX, fIntFactorY, fIntOffsetX,
  fIntOffsetY: Single): XForm;
begin
  Result.eM11 := fIntFactorX;
  Result.eM12 := 0;
  Result.eM21 := 0;
  Result.eM22 := fIntFactorY;
  Result.eDx := fIntOffsetX;
  Result.eDy := fIntOffsetY;
end;


{ TPdfWrite }

function TPdfWrite.Add(c: AnsiChar): TPdfWrite;
begin
  if B>=Bend then // avoid GPF
    Save;
  B^ := c;
  inc(B);
  result := self;
end;

function TPdfWrite.Add(Value: Integer): TPdfWrite;
var t: array[0..23] of AnsiChar;
    P: PAnsiChar;
begin
  if BEnd-B<=24 then
    Save;
  if Cardinal(Value)<1000 then
    if Cardinal(Value)<10 then begin
      B^ := AnsiChar(Value+48);
      inc(B);
    end else
    if Cardinal(Value)<100 then begin
      PWord(B)^ := TwoDigitLookupW[Value];
      inc(B,2);
    end else begin
      PCardinal(B)^ := PCardinal(SmallUInt32UTF8[Value])^;
      inc(B,3);
    end
  else begin
    P := StrInt32(@t[23],Value);
    MoveFast(P^,B^,@t[23]-P);
    inc(B,@t[23]-P);
  end;
  result := self;
end;

function TPdfWrite.Add(const Text: RawByteString): TPdfWrite;
var L: integer;
begin
  if PtrInt(Text)<>0 then begin
    {$ifdef HASINLINE}
    L := length(Text);
    {$else}
    L := PInteger(PtrInt(Text)-4)^;
    {$endif}
    if BEnd-B<=L then begin
      Save;
      inc(fDestStreamPosition,L);
      fDestStream.WriteBuffer(pointer(Text)^,L);
    end else begin
      MoveFast(pointer(Text)^,B^,L);
      inc(B,L);
    end;
  end;
  result := self;
end;

function TPdfWrite.Add(Text: PAnsiChar; Len: integer): TPdfWrite;
begin
  if BEnd-B<=Len then begin
    Save;
    inc(fDestStreamPosition,Len);
    fDestStream.WriteBuffer(Text^,Len);
  end else begin
    MoveFast(Text^,B^,Len);
    inc(B,Len);
  end;
  result := self;
end;

function TPdfWrite.Add(Value, DigitCount: Integer): TPdfWrite;
var t: array[0..15] of AnsiChar;
    i64: array[0..1] of Int64 absolute t;
begin
//  assert(DigitCount<high(t));
  if BEnd-B<=16 then
    Save;
  i64[0] := $3030303030303030; // t[0..14]='0'
  i64[1] := $2030303030303030; // t[15]=' '
  if Value<0 then
    Value := 0;
  StrUInt32(@t[15],Value);
  inc(DigitCount); // includes trailing t[15]=' '
  MoveFast(t[16-DigitCount],B^,DigitCount);
  inc(B,DigitCount);
  result := self;
end;

function TPdfWrite.Add(Value: TSynExtended): TPdfWrite;
var Buffer: ShortString;
    L: integer;
begin
  if BEnd-B<=32 then
    Save;
  str(Value:0:2,Buffer);
  L := ord(Buffer[0]);
  if Buffer[L]='0' then
    if Buffer[L-1]='0' then // '3.00' -> '3'
      dec(L,3) else
      dec(L); // '3.40' -> '3.4'
  MoveFast(Buffer[1],B^,L);
  inc(B,L);
  result := self;
end;

function TPdfWrite.AddColorStr(Color: TPdfColorRGB): TPdfWrite;
var X: array[0..3] of byte absolute Color;
begin
  {$ifdef MSWINDOWS}
  if integer(Color)<0 then
    Color := GetSysColor(Color and $ff);
  {$endif}
  result := AddWithSpace(X[0]/255).AddWithSpace(X[1]/255).AddWithSpace(X[2]/255);
end;

function TPdfWrite.AddEscapeContent(const Text: RawByteString): TPdfWrite;
{$ifdef USE_PDFSECURITY}
var tmp: TSynTempBuffer;
{$endif USE_PDFSECURITY}
begin
  if Text<>'' then
{$ifdef USE_PDFSECURITY}
    if fDoc.fEncryption<>nil then begin
      tmp.Init(length(Text));
      fDoc.fEncryption.EncodeBuffer(pointer(Text)^,tmp.buf^,tmp.len);
      AddEscape(tmp.buf,tmp.len);
      tmp.Done;
    end else
{$endif USE_PDFSECURITY}
    AddEscape(pointer(Text),length(Text));
  result := self;
end;

function TPdfWrite.AddEscape(Text: PAnsiChar; TextLen: integer): TPdfWrite;
var TextEnd: PAnsiChar;
begin
  TextEnd := Text+TextLen;
  while Text<TextEnd do begin
    if B>=Bend4 then
      Save;
    case Text^ of
     '(',')','\': PWord(B)^ := ord('\')+ord(Text^)shl 8;
     #0: begin
       PInteger(B)^ := ord('\')+ord('0')shl 8+ord('0')shl 16+ord('0')shl 24;
       inc(B,2);
     end;
     #8:  PWord(B)^ := ord('\')+ord('b')shl 8;
     #9:  PWord(B)^ := ord('\')+ord('t')shl 8;
     #10: PWord(B)^ := ord('\')+ord('n')shl 8;
     #12: PWord(B)^ := ord('\')+ord('f')shl 8;
     #13: PWord(B)^ := ord('\')+ord('r')shl 8;
     else begin
       B^ := Text^;
       Inc(B);
       Inc(Text);
       continue;
     end;
    end;
    Inc(B,2);
    Inc(Text);
  end;
  result := self;
end;

const // should be local for better code generation
  HexChars: array[0..15] of AnsiChar = '0123456789ABCDEF';

function TPdfWrite.AddEscapeName(Text: PAnsiChar): TPdfWrite;
const ESCAPENAME: set of AnsiChar =
   [#1..#31,'%','(',')','<','>','[',']','{','}','/','#',#127..#255];
var c: cardinal;
begin
  if Text<>nil then
  repeat
    if B>=Bend4 then
      Save;
    c := ord(Text^);
    if c=0 then
      break else
    if AnsiChar(c) in ESCAPENAME then begin
      B[0] := '#';
      B[1] := HexChars[c shr 4];
      B[2] := HexChars[c and $F];
      inc(B,3);
      Inc(Text);
     end else begin
       B^ := AnsiChar(c);
       Inc(B);
       Inc(Text);
     end;
  until false;
  result := self;
end;

function TPdfWrite.AddEscapeText(Text: PAnsiChar; Font: TPdfFont): TPdfWrite;
begin // this function is intented to use with Tj or '
  if Text<>nil then
  repeat
    if B>=Bend4 then
      Save;
    if Font<>nil then
      include(Font.fWinAnsiUsed,Text^);
    case Text^ of
     #0: Break;
     #160: begin        // fixed space is written as normal space
       B^ := ' ';
       inc(B);
       inc(Text);
     end;
     #40,#41,#92: begin // see PDF 2nd ed. p. 290
       B[1] := Text^;
       B[0] := '\';
       Inc(B,2);
       Inc(Text);
     end;
     else begin
       B^ := Text^;
       Inc(B);
       Inc(Text);
     end;
    end;
  until false;
  result := self;
end;

function TPdfWrite.AddHex(const Bin: PDFString): TPdfWrite;
var L, Len: integer;
    PW: Pointer;
begin
  Len := length(Bin);
  PW := pointer(Bin);
  repeat
    L := Len;
    if BEnd-B<=L*2 then begin
      Save;
      if L>high(Tmp) shr 1 then
        L := high(Tmp) shr 1;
    end;
    BinToHex(PW,B,L);
    inc(PtrInt(PW),L);
    inc(B,L*2);
    dec(Len,L);
  until Len=0;
  result := self;
end;

function TPdfWrite.AddHex4(aWordValue: cardinal): TPdfWrite;
var v: Cardinal;
begin
  if B>=BEnd4 then
    Save;
  v := aWordValue shr 8;
  aWordValue := aWordValue and $ff;
  B[0] := HexChars[v shr 4];            // MSB stored first (BigEndian)
  B[1] := HexChars[v and $F];
  B[2] := HexChars[aWordValue shr 4];   // LSB stored last (BigEndian)
  B[3] := HexChars[aWordValue and $F];
  inc(B,4);
  result := self;
end;

procedure TPdfWrite.AddRGB(P: PAnsiChar; PInc, Count: integer);
begin
  while Count>0 do begin
    dec(Count);
    if B>=BEnd4 then
      Save;
    B[0] := P[2];  // write the RGB value in expected order
    B[1] := P[1];
    B[2] := P[0];
    inc(B,3);
    inc(P,PInc);
  end;
end;

function TPdfWrite.AddIso8601(DateTime: TDateTime): TPdfWrite;
begin // add e.g. '2010-06-16T15:06:59'
  result := Add(DateTimeToIso8601(DateTime,true,'T'));
end;

function TPdfWrite.AddWithSpace(Value: TSynExtended): TPdfWrite;
var Buffer: ShortString;
    L: integer;
begin
  if BEnd-B<=32 then
    Save;
  // Value := Trunc(Value * 100 + 0.5) / 100; // 2 decim rounding done by str()
  if Abs(Value)<1E-2 then
    Add('0 ') else begin
    str(Value:0:2,Buffer); // fast conversion with no temp string, using '.'
    L := ord(Buffer[0]);
    if Buffer[L]='0' then
      if Buffer[L-1]='0' then // '3.00' -> '3 '
        dec(L,2) else // '3.40' -> '3.4 '
        else inc(L);  // '3.45' -> '3.45 '
    Buffer[L] := ' '; // append space at the end
    MoveFast(Buffer[1],B^,L);
    inc(B,L);
  end;
  result := self;
end;

function TPdfWrite.AddIntegerBin(value: integer; bytesize: cardinal): TPdfWrite;
var i: cardinal;
begin
  if BEnd-B<=4 then
    Save;
  for i := 1 to bytesize do
    B[i-1] := PAnsiChar(@value)[bytesize-i];
  inc(B,bytesize);
  result := self;
end;

function TPdfWrite.AddWithSpace(Value: TSynExtended; Decimals: cardinal): TPdfWrite;
var Buffer: ShortString;
    L: integer;
begin
  if BEnd-B<=32 then
    Save;
  str(Value:0:Decimals,Buffer);
  L := ord(Buffer[0])+1;
  Buffer[L] := ' '; // append space at the end
  MoveFast(Buffer[1],B^,L);
  inc(B,L);
  result := self;
end;

function TPdfWrite.ToWideChar(const Ansi: PDFString; out DLen: Integer): PWideChar;
var L: integer;
begin
  L := Length(Ansi)*2+2; // maximum possible length
  getmem(result,L);
  DLen := TSynAnsiConvert.Engine(fCodePage).AnsiBufferToUnicode(
    result,pointer(Ansi),Length(Ansi))-result; // use SynCommons fast conversion
end;

function TPdfWrite.AddToUnicodeHex(const Text: PDFString): TPdfWrite;
var PBuf: pointer;
    Len: integer;
begin
  PBuf := ToWideChar(Text,Len);
  AddUnicodeHex(PBuf,Len);
  FreeMem(PBuf);
  result := self;
end;

function TPdfWrite.AddUnicodeHex(PW: PWideChar; WideCharCount: integer): TPdfWrite;
procedure BinToHex4(Bin, Hex: PAnsiChar; BinWords: integer); // BigEndian order
var j, v: cardinal;
begin
  for j := 1 to BinWords do begin
    v := byte(Bin^);
    inc(Bin);
    Hex[2] := HexChars[v shr 4];   // LSB stored last (BigEndian)
    Hex[3] := HexChars[v and $F];
    v := byte(Bin^);
    inc(Bin);
    Hex[0] := HexChars[v shr 4];   // MSB stored first (BigEndian)
    Hex[1] := HexChars[v and $F];
    inc(Hex,4);
  end;
end;
var L: Integer;
{$ifdef USE_PDFSECURITY}
    sectmp: TSynTempBuffer;
{$endif USE_PDFSECURITY}
begin
  if WideCharCount>0 then begin
{$ifdef USE_PDFSECURITY}
    if fDoc.fEncryption<>nil then begin
      sectmp.Init(WideCharCount*2);
      fDoc.fEncryption.EncodeBuffer(PW^,sectmp.buf^,WideCharCount*2);
      PW := sectmp.buf;
    end;
{$endif USE_PDFSECURITY}
    repeat
      L := WideCharCount;
      if BEnd-B<=L*4 then begin
        Save;
        if L>high(Tmp) shr 2 then
          L := high(Tmp) shr 2; // max WideCharCount allowed in Tmp[]
      end;
      BinToHex4(pointer(PW),B,L);
      inc(PtrInt(PW),L*2);
      inc(B,L*4);
      dec(WideCharCount,L);
    until WideCharCount=0;
{$ifdef USE_PDFSECURITY}
    if fDoc.fEncryption<>nil then
      sectmp.Done;
{$endif USE_PDFSECURITY}
  end;
  result := self;
end;

function TPdfWrite.AddToUnicodeHexText(const Text: PDFString;
  NextLine: boolean; Canvas: TPdfCanvas): TPdfWrite;
var PBuf: pointer;
    Len: integer;
begin
  PBuf := ToWideChar(Text,Len);
  AddUnicodeHexText(PBuf,NextLine,Canvas);
  FreeMem(PBuf);
  result := self;
end;

const
  SHOWTEXTCMD: array[boolean] of PDFString = (' Tj'#10,' '''#10);

/// reverse char orders for every hebrew and arabic words
procedure L2R(W: PWideChar; L: integer);
var tmp: TSynTempBuffer;
    i: integer;
begin
  tmp.Init(W,L*2);
  dec(L);
  for i := 0 to L do
    W[i] := PWideChar(tmp.buf)[L-i];
end;

function PdfCoord(MM: single): integer;
begin
  result := round(2.8346456693*MM);
end;

{$ifdef USE_UNISCRIBE}

function TPdfWrite.AddUnicodeHexTextUniScribe(PW: PWideChar;
  WinAnsiTTF: TPdfFontTrueType; NextLine: boolean; Canvas: TPdfCanvas): boolean;
// see http://msdn.microsoft.com/en-us/library/dd317792(v=VS.85).aspx
var L, i,j: integer;
    res: HRESULT;
    max, count, numSp: integer;
    Sp: PScriptPropertiesArray;
    items: array of TScriptItem;
    level: array of byte;
    VisualToLogical: array of integer;
    psc: pointer; // opaque Uniscribe font metric cache
    complex,R2L: boolean;
    glyphs: array of TScriptVisAttr;
    glyphsCount: integer;
    OutGlyphs, LogClust: array of word;
    AScriptControl: TScriptControl;
    AScriptState: TScriptState;
procedure Append(i: Integer);
// local procedure used to add glyphs from items[i] to the PDF content stream
var L: integer;
    W: PWideChar;
procedure DefaultAppend;
var tmpU: array of WideChar;
begin
  SetLength(tmpU,L+1); // we need the text to be ending with #0
  MoveFast(W^,tmpU[0],L*2);
  AddUnicodeHexTextNoUniScribe(pointer(tmpU),WinAnsiTTF,false,Canvas);
end;
begin
  L := items[i+1].iCharPos-items[i].iCharPos; // length of this shapeable item
  if L=0 then
    exit; // nothing to append
  W := PW+items[i].iCharPos;
  res := ScriptShape(0,psc,W,L,max,@items[i].a,
    pointer(OutGlyphs),pointer(LogClust),pointer(glyphs),glyphsCount);
  case res of
    E_OUTOFMEMORY: begin // max was not big enough (should never happen)
      DefaultAppend;
      exit;
    end;
    E_PENDING, USP_E_SCRIPT_NOT_IN_FONT: begin // need HDC and a selected font object
      res := ScriptShape(Canvas.FDoc.GetDCWithFont(WinAnsiTTF),
        psc,W,L,max,@items[i].a,
        pointer(OutGlyphs),pointer(LogClust),pointer(glyphs),glyphsCount);
      if res<>0 then begin // we won't change font if necessary, sorry
        // we shall implement the complex technic as stated by
        // http://msdn.microsoft.com/en-us/library/dd374105(v=VS.85).aspx
        DefaultAppend;
        exit;
      end;
    end;
    0: ; // success -> will add glyphs just below
    else exit;
  end;
  // add glyphs to the PDF content
  // (NextLine has already been handled: not needed here)
  AddGlyphs(pointer(OutGlyphs),glyphsCount,Canvas,pointer(glyphs));
end;
begin
  result := false; // on UniScribe error, handle as Unicode
  // 1. Breaks a Unicode string into individually shapeable items
  L := StrLenW(PW)+1; // include last #0
  max := L+2; // should be big enough
  SetLength(items,max);
  count := 0;
  FillCharFast(AScriptControl, SizeOf(TScriptControl), 0);
  FillCharFast(AScriptState, SizeOf(TScriptState), 0);
  if ScriptApplyDigitSubstitution(nil,@AScriptControl,@AScriptState) <> 0 then
    exit;
  if Canvas.RightToLeftText then
    AScriptState.uBidiLevel := 1;
  if ScriptItemize(PW,L,max,@AScriptControl,@AScriptState,pointer(items),count) <> 0 then
    exit; // error trying processing Glyph Shaping -> fast return
  // 2. guess if requiring glyph shaping or layout
  ScriptGetProperties(sP,numSp);
  complex := false;
  R2L := false;
  for i := 0 to Count-2 do // don't need Count-1 = Terminator
    if fComplex in sP^[items[i].a.eScript and (1 shl 10-1)]^.fFlags then
      complex := true else
      if fRTL in items[i].a.fFlags then
        R2L := true;
  if not complex and not R2L then
    exit; // avoid slower UniScribe if content does not require it
  // 3. get Visual Order, i.e. how to render the content from left to right
  SetLength(level,count);
  for i := 0 to Count-1 do
    level[i] := items[i].a.s.uBidiLevel;
  SetLength(VisualToLogical,count);
  if ScriptLayout(Count,pointer(level),pointer(VisualToLogical),nil)<>0 then
    exit;
  // 4. now we have enough information to start drawing
  result := true;
  if NextLine then
    Canvas.MoveToNextLine; // manual NextLine handling
  // 5. add glyphs for all shapeable items
  max := (L*3)shr 1+32; // should be big enough - allocate only once
  SetLength(glyphs,max);
  SetLength(OutGlyphs,max);
  SetLength(LogClust,max);
  psc := nil; // cached for the same character style used
  // append following logical order
  for j := 0 to Count-2 do // Count-2: ignore last ending item
    Append(VisualToLogical[j]);
end;
{$endif}

procedure TPdfWrite.AddGlyphFromChar(Char: WideChar; Canvas: TPdfCanvas;
  TTF: TPdfFontTrueType; NextLine: PBoolean);
var aChanged: boolean;
    aTTF: TPdfFontTrueType;
    Glyph: word;
begin
  assert((TTF<>nil) and (TTF=TTF.WinAnsiFont));
  aChanged := fAddGlyphFont=fNone;
  Glyph := TTF.fUsedWide[TTF.FindOrAddUsedWideChar(Char)].Glyph;
  with Canvas.fDoc do
    if fPDFA1 and (Glyph=0) and (fFontFallBackIndex<0) then
      raise Exception.Create('PDFA/1 expects font fallback to be enabled, '+
        'and the required font is not available on this system') else
    if (Glyph=0) and fUseFontFallBack and (fFontFallBackIndex>=0) then begin
      if fAddGlyphFont=fMain then
        AddGlyphFlush(Canvas,TTF,NextLine);
      fAddGlyphFont := fFallBack;
      aTTF := Canvas.SetFont('',Canvas.FPage.FontSize,TTF.fStyle,-1,fFontFallBackIndex) as TPdfFontTrueType;
      assert(aTTF=aTTF.WinAnsiFont);
      Glyph := aTTF.fUsedWide[aTTF.FindOrAddUsedWideChar(Char)].Glyph;
    end else begin
      if fAddGlyphFont=fFallBack then begin
        AddGlyphFlush(Canvas,TTF,NextLine);
        aChanged := true;
      end;
      fAddGlyphFont := fMain;
      aTTF := TTF;
    end;
  if (Canvas.FPage.Font<>aTTF.UnicodeFont) and (aTTF.UnicodeFont=nil) then
    aTTF.CreateAssociatedUnicodeFont;
  Canvas.SetPDFFont(aTTF.UnicodeFont,Canvas.FPage.FontSize);
  if aChanged then
    Add('<');
  AddHex4(Glyph);
end;

procedure TPdfWrite.AddGlyphFlush(Canvas: TPdfCanvas; TTF: TPdfFontTrueType;
  NextLine: PBoolean);
var aNextLine: boolean;
begin
  if fAddGlyphFont=fNone then
    exit;
  if NextLine=nil then
    aNextLine := false else begin
    aNextLine := NextLine^;
    NextLine^ := false;  // MoveToNextLine only once
  end;
  fAddGlyphFont := fNone;
  Add('>').Add(SHOWTEXTCMD[aNextLine]);
end;

procedure TPdfWrite.AddUnicodeHexTextNoUniScribe(PW: PWideChar;
  TTF: TPdfFontTrueType; NextLine: boolean; Canvas: TPdfCanvas);
var Ansi: integer;
    isSymbolFont: Boolean;
begin
  if TTF<>nil then begin
    if TTF.UnicodeFont<>nil then
      isSymbolFont := TTF.UnicodeFont.fIsSymbolFont else
      isSymbolFont := TTF.fIsSymbolFont;
    TTF := TTF.WinAnsiFont; // we expect the WinAnsi font in the code below
  end else
    isSymbolFont := false;
  Ansi := WideCharToWinAnsi(cardinal(PW^));
  if (TTF=nil) and (Ansi<0) then
    Ansi := ord('?'); // WinAnsi only font shows ? glyph for unicode chars
  while Ansi<>0 do begin
    if (Ansi>0) and not isSymbolFont then begin
      // add WinAnsi-encoded chars as such
      if (TTF<>nil) and (Canvas.FPage.Font<>TTF) then
        Canvas.SetPDFFont(TTF,Canvas.FPage.FontSize);
      Add('(');
      repeat
        case Ansi of
          40,41,92: Add('\');   // see PDF 2nd ed. p. 290
          160: Ansi := 32; // fixed space is written as normal space
        end;
        TTF.AddUsedWinAnsiChar(AnsiChar(Ansi));
        Add(AnsiChar(Ansi));
        Inc(PW);
        Ansi := WideCharToWinAnsi(cardinal(PW^));
        if (TTF=nil) and (Ansi<0) then
          Ansi := ord('?'); // WinAnsi only font shows ? glyph for unicode chars
      until Ansi<=0;
      Add(')').Add(SHOWTEXTCMD[NextLine]);
      NextLine := false; // MoveToNextLine only once
    end;
    if Ansi=0 then
      break;
    // here we know that PW^ is not a Win-Ansi glyph, and that TTF exists
    repeat
      AddGlyphFromChar(PW^,Canvas,TTF,@NextLine);
      inc(PW);
      Ansi := WideCharToWinAnsi(cardinal(PW^));
      if Ansi=160 then
        Ansi := 32;
      if Ansi=32 then
        if WideCharToWinAnsi(cardinal(PW[1]))<0 then
          continue; // we allow one space inside Unicode text
    until Ansi>=0;
    AddGlyphFlush(Canvas,TTF,@NextLine);
  end;
end;

function TPdfWrite.AddUnicodeHexText(PW: PWideChar; NextLine: boolean;
  Canvas: TPdfCanvas): TPdfWrite;
var TTF: TPdfFontTrueType;
begin
  if PW<>nil then begin
    with Canvas.FPage do
      if FFont.FTrueTypeFontsIndex=0 then
        TTF := nil else // mark we don't have an Unicode font, i.e. a TTF
        TTF := TPdfFontTrueType(FFont);
    {$ifdef USE_UNISCRIBE}
    // use the Windows Uniscribe API if required
    if not Canvas.fDoc.UseUniScribe or (TTF=nil) or
       not AddUnicodeHexTextUniScribe(PW,TTF.WinAnsiFont,NextLine,Canvas) then
    {$endif}
      // fastest version, without Ordering and/or Shaping of the text
      AddUnicodeHexTextNoUniScribe(PW,TTF,NextLine,Canvas);
  end;
  result := self;
end;

function TPdfWrite.AddGlyphs(Glyphs: PWord; GlyphsCount: integer;
  Canvas: TPdfCanvas; AVisAttrsPtr: Pointer): TPdfWrite;
var TTF: TPdfFontTrueType;
    first: boolean;
    glyph: integer;
    {$ifdef USE_UNISCRIBE}
    AVisAttrs: PScriptVisAttr;
    {$endif}
begin
  if (Glyphs<>nil) and (GlyphsCount>0) then begin
    with Canvas.FPage do
      if FFont.FTrueTypeFontsIndex=0 then
        TTF := nil else // mark we don't have an Unicode font, i.e. a TTF
        TTF := TPdfFontTrueType(FFont);
    if TTF<>nil then begin // we need a TTF font
      if (Canvas.FPage.Font<>TTF.UnicodeFont) and (TTF.UnicodeFont=nil) then
        TTF.CreateAssociatedUnicodeFont;
      Canvas.SetPDFFont(TTF.UnicodeFont,Canvas.FPage.FontSize);
      first := true;
      {$ifdef USE_UNISCRIBE}
      AVisAttrs := AVisAttrsPtr;
      {$endif}
      while GlyphsCount>0 do begin
        {$ifdef USE_UNISCRIBE}
        if (AVisAttrs=nil) or
           not(AVisAttrs^.fFlags*[fDiacritic,fZeroWidth]=[fZeroWidth]) then
        {$endif} begin
          glyph := TTF.WinAnsiFont.GetAndMarkGlyphAsUsed(Glyphs^);
          // this font shall by definition contain all needed glyphs
          // -> no Font Fallback is to be implemented here
          if first then begin
            first := false;
            Add('<');
          end;
          AddHex4(glyph);
        end;
        inc(Glyphs);
        dec(GlyphsCount);
        {$ifdef USE_UNISCRIBE}
        if AVisAttrs<>nil then
          inc(AVisAttrs);
        {$endif}
      end;
      if not first then
        Add('> Tj'#10);
    end;
  end;
  result := self;
end;

function TPdfWrite.AddWithSpace(Value: Integer): TPdfWrite;
var t: array[0..25] of AnsiChar;
    P: PAnsiChar;
    L: integer;
begin
  if BEnd-B<=16 then
    Save;
  if Cardinal(Value)<1000 then
    if Cardinal(Value)<10 then begin
      PWord(B)^ := Value+(48+32 shl 8);
      inc(B,2);
    end else
    if Cardinal(Value)<100 then begin
      PCardinal(B)^ := TwoDigitLookupW[Value]+32 shl 16;
      inc(B,3);
    end else begin
      PCardinal(B)^ := PCardinal(SmallUInt32UTF8[Value])^+32 shl 24;
      inc(B,4);
    end
  else begin
    t[24] := ' ';
    P := StrInt32(@t[24],Value);
    L := @t[25]-P;
    MoveFast(P^,B^,L);
    inc(B,@t[25]-P);
  end;
  result := self;
end;

constructor TPdfWrite.Create(Destination: TPdfDocument; DestStream: TStream);
begin
  fDoc := Destination;
  fDestStream := DestStream;
  fDestStreamPosition := fDestStream.Seek(0,soFromCurrent);
  fCodePage := fDoc.CodePage;
  B := @Tmp;
  Bend := B+high(Tmp);
  Bend4 := Bend-4;
end;

function TPdfWrite.Position: Integer;
begin
  result := fDestStreamPosition+B-@Tmp;
end;

procedure TPdfWrite.Save;
var L: integer;
begin
  L := B-@Tmp;
  inc(fDestStreamPosition,L);
  fDestStream.WriteBuffer(Tmp,L);
  B := @Tmp;
end;

function TPdfWrite.ToPDFString: PDFString;
begin
  if fDestStreamPosition=0 then
    // we remained in the internal buffer -> not necessary to use stream
    SetString(Result,Tmp,B-@Tmp) else begin
    // we used the stream -> flush remaining, and get whole data at once
    Save;
    result := '';
    SetLength(result,fDestStreamPosition);
    fDestStream.Seek(0,soFromBeginning);
    fDestStream.Read(pointer(result)^,fDestStreamPosition);
  end;
end;


{ Utility functions }

const
  PDF_PAGE_LAYOUT_NAMES: array[TPdfPageLayout] of PDFString = (
    'SinglePage', 'OneColumn', 'TwoColumnLeft', 'TwoColumnRight');

  PDF_PAGE_MODE_NAMES: array[TPdfPageMode] of PDFString = (
    'UseNone', 'UseOutlines', 'UseThumbs', 'FullScreen');

  PDF_ANNOTATION_TYPE_NAMES: array[0..12] of PDFString = (
    'Text', 'Link', 'Sound', 'FreeText', 'Stamp', 'Square', 'Circle',
    'StrikeOut', 'Highlight', 'Underline', 'Ink', 'FileAttachment', 'Popup');

  PDF_DESTINATION_TYPE_NAMES: array[TPdfDestinationType] of PDFString = (
    'XYZ', 'Fit', 'FitH', 'FitV', 'FitR', 'FitB', 'FitBH', 'FitBV');

procedure _Pages_AddKids(AParent: TPdfDictionary; AKid: TPdfDictionary);
var FKids: TPdfArray;
begin
  // adding page object to the parent pages object.
  FKids := AParent.PdfArrayByName('Kids');
  FKids.AddItem(AKid);
  AParent.PdfNumberByName('Count').Value := FKids.ItemCount;
end;


{ TPdfTrailer }

constructor TPdfTrailer.Create(AObjectMgr: TPdfObjectMgr);
begin
  inherited Create;
  FAttributes := TPdfDictionary.Create(AObjectMgr);
  FAttributes.AddItem('Size', TPdfNumber.Create(0));
end;

procedure TPdfTrailer.WriteTo(var W: TPdfWrite);
type TXRefType = (xrefFree, xrefInUse, xrefInUseCompressed);
const TYPEWIDTH = 1;
var offsetWidth,genWidth, i: integer;
    WR: TPdfWrite;
begin
  if FCrossReference=nil then begin
    W.Add('trailer' + CRLF);
    FAttributes.WriteTo(W);
  end else begin
    if FXrefAddress<60000 then
      offsetWidth := 2 else
    if FXrefAddress<13421772 then
      offsetWidth := 3 else
      offsetWidth := 4;
    if (FObjectStream=nil) or (FObjectStream.ObjectCount<255) then
      genWidth := 1 else
      if FObjectStream.ObjectCount<65535 then
        genWidth := 2 else
        genWidth := 3;
    FAttributes.AddItem('W',TPdfArray.Create(nil,[TYPEWIDTH,offsetWidth,genWidth]));
    WR := FCrossReference.Writer;
    WR.AddIntegerBin(ord(xrefFree),TYPEWIDTH).
       AddIntegerBin(0,offsetWidth).
       AddIntegerBin(-1,genWidth);
    for i := 1 to FXRef.ItemCount-1 do
    with FXRef.Items[i] do begin
      if ObjectStreamIndex>=0 then begin
        assert(FObjectStream<>nil);
        WR.AddIntegerBin(ord(xrefInUseCompressed),TYPEWIDTH);
        WR.AddIntegerBin(FObjectStream.ObjectNumber,offsetWidth);
        WR.AddIntegerBin(ObjectStreamIndex,genWidth);
      end else begin
        WR.AddIntegerBin(ord(xrefInUse),TYPEWIDTH);
        WR.AddIntegerBin(ByteOffset,offsetWidth);
        WR.AddIntegerBin(GenerationNumber,genWidth);
      end;
    end;
    FCrossReference.WriteValueTo(W);
  end;
  W.Add(CRLF + 'startxref' + CRLF).Add(FXrefAddress).
    Add(CRLF + '%%EOF' + CRLF);
end;

destructor TPdfTrailer.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

procedure TPdfTrailer.ToCrossReference(Doc: TPdfDocument);
var i: integer;
{$ifdef USE_PDFSECURITY}
    Enc: TPdfEncryption;
{$endif USE_PDFSECURITY}
begin
  FXRef := Doc.FXref;
  FCrossReference := TPdfStream.Create(Doc);
  FCrossReference.FSecondaryAttributes := FAttributes;
  FAttributes.AddItem('Type','XRef');
{$ifdef USE_PDFSECURITY}
  FCrossReference.FDoNotEncrypt := true;
  if Doc.fEncryption<>nil then
    exit; // still a bug with encryption + objectstream
{$endif USE_PDFSECURITY}
  FObjectStream := TPdfObjectStream.Create(Doc);
{$ifdef USE_PDFSECURITY}
  FObjectStream.FDoNotEncrypt := true;
  Enc := Doc.fEncryption;
  try
    Doc.fEncryption := nil; // force /ObjStm content not encrypted
{$endif USE_PDFSECURITY}
    for i := 1 to FXRef.ItemCount-1 do
    with FXRef.Items[i] do
      if (ByteOffset<=0) and Value.InheritsFrom(TPdfDictionary) then begin
        FByteOffset := maxInt; // mark already handlded
        FObjectStreamIndex := FObjectStream.AddObject(Value);
      end;
{$ifdef USE_PDFSECURITY}
  finally
    Doc.fEncryption := Enc;
  end;
{$endif USE_PDFSECURITY}
end;


{ TPdfXrefEntry }

constructor TPdfXrefEntry.Create(AValue: TPdfObject);
begin
  FByteOffset := -1;
  FObjectStreamIndex := -1;
  if AValue<>nil then begin
    FEntryType := PDF_IN_USE_ENTRY;
    FGenerationNumber := AValue.GenerationNumber;
    FValue := AValue;
  end else
    FEntryType := PDF_FREE_ENTRY;
end;

destructor TPdfXrefEntry.Destroy;
begin
  if FEntryType=PDF_IN_USE_ENTRY then
    FValue.Free;
  inherited;
end;

procedure TPdfXrefEntry.SaveToPdfWrite(var W: TPdfWrite);
begin
  W.Add(FByteOffset,10).Add(FGenerationNumber,5).Add(FEntryType).Add(' '#10);
end;


{ TPdfXref }

constructor TPdfXref.Create;
var RootEntry: TPdfXrefEntry;
begin
  FXrefEntries := TList.Create;
  // create first a void PDF_FREE_ENTRY as root
  RootEntry := TPdfXrefEntry.Create(nil);
  RootEntry.GenerationNumber := PDF_MAX_GENERATION_NUM;
  FXrefEntries.Add(RootEntry);
end;

destructor TPdfXref.Destroy;
var i: integer;
begin
  for i := 0 to FXrefEntries.Count-1 do
    GetItem(i).Free;
  FXrefEntries.Free;
  inherited;
end;

procedure TPdfXref.AddObject(AObject: TPdfObject);
var ObjectNumber: integer;
    XrefEntry: TPdfXrefEntry;
begin
  if (AObject=nil) or (AObject.ObjectType<>otDirectObject) then
    raise EPdfInvalidOperation.Create('AddObject');
  XrefEntry := TPdfXrefEntry.Create(AObject);
  ObjectNumber := FXrefEntries.Add(XrefEntry);
  AObject.SetObjectNumber(ObjectNumber);
end;

function TPdfXref.GetItem(ObjectID: integer): TPdfXrefEntry;
begin
  Result := TPdfXrefEntry(FXrefEntries[ObjectID]);
end;

function TPdfXref.GetItemCount: integer;
begin
  Result := FXrefEntries.Count;
end;

function TPdfXref.GetObject(ObjectID: integer): TPdfObject;
begin
  if cardinal(ObjectID)<cardinal(FXrefEntries.Count) then
    result := TPdfXrefEntry(FXrefEntries.List[ObjectID]).Value else
    result := nil;
end;

procedure TPdfXref.WriteTo(var W: TPdfWrite);
var i: integer;
begin
  W.Add('xref' + CRLF + '0 ').Add(FXrefEntries.Count).Add(#10);
  for i := 0 to FXrefEntries.Count- 1 do
    Items[i].SaveToPdfWrite(W);
end;


{ TPdfDocument }

function EnumFontsProcW(var LogFont: TLogFontW; var TextMetric: TTextMetric;
  FontType: Integer; var List: TRawUTF8DynArray): Integer; stdcall;
// we enumerate all available fonts, whatever the charset is, because
// we may won't enumerate Arial or Times New Roman if current FCharSet is
// chinese e.g.
var Temp: RawUTF8;
begin
  with LogFont do
    if ((FontType=DEVICE_FONTTYPE) or (FontType=TRUETYPE_FONTTYPE)) and (lfFaceName[0]<>'@') then begin
      Temp := RawUnicodeToUtf8(lfFaceName,StrLenW(lfFaceName));
      if (pointer(List)=nil) or (List[high(List)]<>Temp) then
        AddRawUTF8(List,Temp,true,true);
    end;
  Result := 1;
end;

function LCIDToCodePage(ALcid: LCID): Integer;
var Buffer: array [0..6] of Char;
begin
  GetLocaleInfo(ALcid, LOCALE_IDEFAULTANSICODEPAGE, Buffer, SizeOf(Buffer));
  Result:= StrToIntDef(Buffer, GetACP);
end;

constructor TPdfDocument.Create(AUseOutlines: Boolean; ACodePage: integer;
  APDFA1: boolean{$ifdef USE_PDFSECURITY}; AEncryption: TPdfEncryption{$endif});
var LFont: TLogFontW; // TLogFontW to add to FTrueTypeFonts array as UTF-8
    i: integer;
begin
  fPDFA1 := APDFA1;
  {$ifdef USE_PDFSECURITY}
  fEncryption := AEncryption;
  {$endif USE_PDFSECURITY}
  fTPdfPageClass := TPdfPage;
  if ACodePage=0 then
    FCodePage := LCIDToCodePage(SysLocale.DefaultLCID) else // GetACP can be<>SysLocale
    FCodePage := ACodePage;
  FCharSet := CodePageToCharSet(FCodePage);
  DefaultPaperSize := psA4;
  fRawPages := TList.Create;
  // retrieve the current reference GDI parameters
  FDC := CreateCompatibleDC(0);
  FScreenLogPixels := GetDeviceCaps(FDC, LOGPIXELSY);
  FCanvas := TPdfCanvas.Create(Self); // need FScreenLogPixels
  // retrieve true type fonts available for all charsets
  FillCharFast(LFont, sizeof(LFont), 0);
  LFont.lfCharset := DEFAULT_CHARSET; // enumerate ALL fonts
  EnumFontFamiliesExW(FDC, LFont, @EnumFontsProcW, PtrInt(@FTrueTypeFonts), 0);
  QuickSortRawUTF8(FTrueTypeFonts,length(FTrueTypeFonts),nil,@StrIComp);
  FCompressionMethod := cmFlateDecode; // deflate by default
  fBookMarks := TRawUTF8List.Create([fCaseSensitive,fNoDuplicate]);
  fMissingBookmarks := TRawUTF8List.Create;
  FUseOutlines := AUseOutlines;
  fUseFontFallBack := true;
  fFontFallBackIndex := GetTrueTypeFontIndex('Arial Unicode MS');
  if fFontFallBackIndex<0 then
    for i := 0 to high(FTrueTypeFonts) do
      if PosEx('Unicode',FTrueTypeFonts[i])>0 then begin
        fFontFallBackIndex := i;
        break;
      end;
  NewDoc;
end;

function TPdfDocument.GetInfo: TPdfInfo;
begin
  if FInfo=nil then
    CreateInfo;
  Result := FInfo;
end;

function TPdfDocument.GetOutlineRoot: TPdfOutlineRoot;
begin
  if not UseOutlines then
    RaiseInvalidOperation;
  Result := FOutlineRoot;
end;

destructor TPdfDocument.Destroy;
begin
  FreeDoc;
  FCanvas.Free;
  if fSelectedDCFontOld<>0 then
    SelectObject(FDC,fSelectedDCFontOld);
  DeleteDC(FDC);
  FEmbeddedTTFIgnore.Free;
  fRawPages.Free;
  fBookMarks.Free;
  fMissingBookmarks.Free;
  inherited;
  {$ifdef USE_PDFSECURITY}
  fEncryption.Free;
  {$endif USE_PDFSECURITY}
end;

function TPdfDocument.RegisterXObject(AObject: TPdfXObject; const AName: PDFString): integer;
begin
   // check object and register it
   if AObject=nil then
     raise EPdfInvalidValue.Create('RegisterXObject: no AObject');
   if AObject.Attributes.TypeOf<>'XObject' then
     raise EPdfInvalidValue.Create('RegisterXObject: no XObject');
   if AObject.ObjectType<>otIndirectObject then
     FXref.AddObject(AObject);
   if AObject.Attributes.ValueByName('Name')=nil then begin
     if GetXObject(AName)<>nil then
       raise EPdfInvalidValue.Createfmt('RegisterXObject: dup name %s', [AName]);
     result := FXObjectList.AddItem(AObject);
     AObject.Attributes.AddItem('Name', AName);
   end else
     result := -1;
end;

const
  PDF_PRODUCER = 'Synopse PDF engine '+SYNOPSE_FRAMEWORK_VERSION;

procedure TPdfDocument.CreateInfo;
var FInfoDictionary: TPdfDictionary;
begin
  FInfoDictionary := TPdfDictionary.Create(FXref);
  FXref.AddObject(FInfoDictionary);
  FInfoDictionary.AddItemText('Producer', PDF_PRODUCER);
  FTrailer.Attributes.AddItem('Info', FInfoDictionary);
  FInfo := TPdfInfo.Create;
  FInfo.SetData(FInfoDictionary);
  FObjectList.Add(FInfo);
end;

function TPdfDocument.CreatePages(Parent: TPdfDictionary): TPdfDictionary;
begin
  // create pages object and register to xref.
  result := TPdfDictionary.Create(FXref);
  result.FSaveAtTheEnd := True;
  FXref.AddObject(Result);
  with result do begin
    AddItem('Type', 'Pages');
    AddItem('Kids', TPdfArray.Create(FXref));
    AddItem('Count', 0);
  end;
  if (Parent<>nil) and (Parent.TypeOf='Pages') then
    _Pages_AddKids(Parent, result) else
    FRoot.Pages := Result;
end;

function TPdfDocument.GetXObjectIndex(const AName: PDFString): integer;
begin
  for result := 0 to FXObjectList.ItemCount-1 do
    with TPdfXObject(FXObjectList.FArray.List[result]) do
    if (FObjectType<>otVirtualObject) and (Attributes<>nil) and
       (TPdfName(Attributes.ValueByName('Name')).Value=AName) then
        exit;
  result := -1;
end;

function TPdfDocument.GetXObject(const AName: PDFString): TPdfXObject;
var i: integer;
begin
  for i := 0 to FXObjectList.ItemCount-1 do begin
    result := TPdfXObject(FXObjectList.FArray.List[i]);
    if result.FObjectType=otVirtualObject then begin
      result := TPdfXObject(FXRef.GetObject(result.FObjectNumber));
      if (result=nil) or not result.InheritsFrom(TPdfXObject) then
        continue;
    end;
    if result.Attributes<>nil then
      if TPdfName(result.Attributes.ValueByName('Name')).Value=AName then
        exit;
  end;
  Result := nil;
end;

{$ifdef USE_BITMAP}
function TPdfDocument.GetXObjectImageName(const Hash: THash128Rec;
  Width, Height: Integer): PDFString;
var Obj: TPdfXObject;
    Img: TPdfImage absolute Obj;
    i: integer;
begin
  for i := 0 to FXObjectList.ItemCount-1 do begin
    Obj := TPdfXObject(FXObjectList.FArray.List[i]);
    if Obj.FObjectType=otVirtualObject then
      Obj := TPdfXObject(FXRef.GetObject(Obj.FObjectNumber));
    if (Obj<>nil) and Obj.InheritsFrom(TPdfImage) and
       (Img.PixelWidth=Width) and (Img.PixelHeight=Height) and
       not IsZero(Img.fHash.b) and IsEqual(Img.fHash.b,Hash.b) and
       (Obj.Attributes<>nil) then begin
      result := TPdfName(Obj.Attributes.ValueByName('Name')).Value;
      if result<>'' then
        exit;
    end;
  end;
  result := '';
end;
{$endif USE_BITMAP}

function TPdfDocument.CreateAnnotation(AType: TPdfAnnotationSubType;
  const ARect: TPdfRect; BorderStyle: TPdfAnnotationBorder; BorderWidth: integer): TPdfDictionary;
var FAnnotation, FBorderStyle: TPdfDictionary;
    aArray: TPdfArray;
    aPage: TPdfPage;
const FLAGS_PRINT = 4;
      BS: array [TPdfAnnotationBorder] of PDFString = ('S','D','B','I','U');
begin
  // create new annotation and set the properties
  FAnnotation := TPdfDictionary.Create(FXref);
  FAnnotation.FSaveAtTheEnd := true;
  FXref.AddObject(FAnnotation);
  FAnnotation.AddItem('Type', 'Annot');
  FAnnotation.AddItem('Subtype', PDF_ANNOTATION_TYPE_NAMES[ord(AType)]);
  FAnnotation.AddItem('F',FLAGS_PRINT);
  if (BorderStyle<>abSolid) or (BorderWidth<>1) then begin
    FBorderStyle := TPdfDictionary.Create(FXRef);
    if BorderStyle<>abSolid then
      FBorderStyle.AddItem('S',BS[BorderStyle]);
    if BorderWidth<>1 then
      FBorderStyle.AddItem('W',BorderWidth);
    FAnnotation.AddItem('BS',FBorderStyle);
  end;
  with ARect do
    FAnnotation.AddItem('Rect',TPdfArray.CreateReals(FXRef,[Left,Top,Right,Bottom]));
  // adding annotation to the current page
  aPage := FCanvas.Page;
  aArray := aPage.PdfArrayByName('Annots');
  if aArray=nil then begin
    aArray := TPdfArray.Create(FXRef);
    aPage.AddItem('Annots', aArray);
  end;
  aArray.AddItem(FAnnotation);
  Result := FAnnotation;
end;

function TPdfDocument.CreateLink(const ARect: TPdfRect; const aBookmarkName: RawUTF8;
  BorderStyle: TPdfAnnotationBorder; BorderWidth: integer): TPdfDictionary;
var aDest: TPdfDestination;
begin
  result := CreateAnnotation(asLink,ARect,BorderStyle,BorderWidth);
  aDest := fBookmarks.GetObjectFrom(aBookmarkName);
  if aDest=nil then
    fMissingBookmarks.AddObject(aBookmarkName,result) else
    result.AddItem('Dest',aDest.GetValue);
end;

function TPdfDocument.CreateHyperLink(const ARect: TPdfRect; const url : RawUTF8;
  BorderStyle: TPdfAnnotationBorder; BorderWidth: integer): TPdfDictionary;
var aURIObj: TPdfDictionary;
begin
  result := CreateAnnotation(asLink,ARect,BorderStyle,BorderWidth);
  aURIObj := TPdfDictionary.Create(FXref);
  aURIObj.FSaveAtTheEnd := true;
  aURIObj.AddItem('S', 'URI');
  aURIObj.AddItemTextUTF8('URI', url);
  FXref.AddObject(aURIObj);
  Result.AddItem('A', aURIObj);
end;

function TPdfDocument.CreateDestination: TPdfDestination;
begin
  Result := TPdfDestination.Create(Self);
  FObjectList.Add(Result);
end;

procedure TPdfDocument.CreateBookMark(TopPosition: Single; const aBookmarkName: RawUTF8);
var aDest: TPdfDestination;
    i: integer;
begin
  if Canvas.FPage=nil then
    RaiseInvalidOperation; // we need a page to refer to
  if (aBookmarkName='') or (fBookMarks.IndexOf(aBookmarkName)>=0) then
    raise EPdfInvalidValue.CreateFmt('Duplicated or void bookmark name "%s"',[aBookmarkName]);
  aDest := CreateDestination;
  aDest.DestinationType := dtXYZ;
  aDest.Zoom := 0; // will leave Zoom factor unchanged
  aDest.Left := 0; // go to left side of the page
  aDest.Top := Round(TopPosition);
  fBookMarks.AddObject(aBookmarkName,aDest);
  with fMissingBookmarks do
    for i := Count-1 downto 0 do
      if Strings[i]=aBookmarkName then begin
        TPdfDictionary(Objects[i]).AddItem('Dest',aDest.GetValue);
        Delete(i);
      end;
end;

procedure TPdfDocument.NewDoc;
var CatalogDictionary: TPdfDictionary;
    Dico: TPdfDictionary;
    DicoD: TPdfDictionary;
    RGB: TPdfStream;
    ID: TPdfArray;
    IDs: PDFString;
    i: integer;
    NeedFileID: boolean;
    FileID: array[0..3] of cardinal;
    {$ifndef USE_PDFSECURITY}
    P: PAnsiChar;
    {$endif USE_PDFSECURITY}
const
  ICC: array[0..139] of cardinal = (
    805437440,1161970753,4098,1920233069,541214546,542792024,134270983,318769920,989868800,
    1886610273,1280331841,0,1701736302,0,0,0,0,3606446080,256,768802816,1161970753,
    0,0,0,0,0,0,0,0,0,0,0,167772160,1953656931,4227858432,838860800,1668506980,805371904,
    1795162112,1953526903,2617311232,335544320,1953524578,2952855552,335544320,1129469042,
    3288399872,234881024,1129469031,3556835328,234881024,1129469026,3825270784,234881024,
    1515804786,4093706240,335544320,1515804775,134348800,335544320,1515804770,469893120,
    335544320,1954047348,0,2037411651,1751607666,808591476,1092628528,1700949860,1937330976,
    1936549236,1668172064,1869640303,1702125938,100,1668506980,0,285212672,1651467329,
    1196564581,824713282,691550521,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,542792024,0,
    1374879744,256,3423994112,542792024,0,0,0,0,1987212643,0,16777216,13058,1987212643,0,
    16777216,13058,1987212643,0,16777216,13058,542792024,0,412876800,2773417984,4228120576,
    542792024,0,2368995328,748683264,2500788224,542792024,0,824573952,789577728,2629697536);
begin
  fLastOutline := nil;
  FreeDoc;
  FXref := TPdfXref.Create;
  FTrailer := TPdfTrailer.Create(FXref);
  FFontList := TList.Create;
  FXObjectList := TPdfArray.Create(FXref);
  FXObjectList.FSaveAtTheEnd := true;
  FObjectList := TList.Create;
  FRoot := TPdfCatalog.Create;
  FRoot.FOwner := self;
  CatalogDictionary := TPdfDictionary.Create(FXref);
  FXref.AddObject(CatalogDictionary);
  CatalogDictionary.AddItem('Type', 'Catalog');
  FTrailer.Attributes.AddItem('Root', CatalogDictionary);
  FRoot.SetData(CatalogDictionary);
  FRoot.PageLayout := plSinglePage;
  FObjectList.Add(FRoot);
  if UseOutlines then begin
    FOutlineRoot := TPdfOutlineRoot.Create(Self);
    FRoot.Data.AddItem('Outlines', FOutlineRoot.Data);
  end;
  CreateInfo;
  FInfo.CreationDate := now;
  FCurrentPages := CreatePages(nil); // nil -> create root Pages XObject
  FRoot.SetPages(FCurrentPages);
  NeedFileID := false;
  if FUseOptionalContent then begin
    if fFileFormat<pdf15 then
      fFileFormat := pdf15;
    Dico := TPdfDictionary.Create(FXRef);
    DicoD := TPdfDictionary.Create(FXRef);
    DicoD.AddItem('BaseState','ON');  // must be ON in default configuration
    DicoD.AddItem('OFF',TPDFArray.Create(FXRef));
    DicoD.AddItem('Order',TPDFArray.Create(FXRef));
    DicoD.AddItem('ListMode','AllPages');  // default value but some viewers cause trouble when missing
    DicoD.AddItem('RBGroups',TPDFArray.Create(FXRef));
    Dico.AddItem('D',DicoD);
    Dico.AddItem('OCGs',TPdfArray.Create(FXRef));
    FRoot.Data.AddItem('OCProperties',Dico);
  end;
  {$ifdef USE_PDFSECURITY}
  if fEncryption<>nil then
    NeedFileID := true;
  {$endif USE_PDFSECURITY}
  if PDFA1 then begin
    if fFileFormat<pdf14 then
      fFileFormat := pdf14;
    {$ifdef USE_PDFSECURITY}
    if fEncryption<>nil then
      raise EPdfInvalidOperation.Create('PDF/A-1 not allowed when encryption is enabled');
    {$endif USE_PDFSECURITY}
    fUseFontFallBack := true;
    FOutputIntents := TPdfArray.Create(FXref);
    Dico := TPdfDictionary.Create(FXRef);
    Dico.AddItem('Type','OutputIntent');
    Dico.AddItem('S','GTS_PDFA1');
    Dico.AddItemText('OutputConditionIdentifier','sRGB');
    Dico.AddItemText('RegistryName','http://www.color.org');
    RGB := TPdfStream.Create(self);
    RGB.Attributes.AddItem('N',3);
    RGB.Writer.Add(@ICC,sizeof(ICC));
    Dico.AddItem('DestOutputProfile',RGB);
    FOutputIntents.AddItem(Dico);
    CatalogDictionary.AddItem('OutputIntents',FOutputIntents);
    FMetaData := TPdfStream.Create(Self);
    FMetaData.Attributes.AddItem('Subtype','XML');
    FMetaData.Attributes.AddItem('Type','Metadata');
    FMetaData.FFilter := '';
    CatalogDictionary.AddItem('MarkInfo',TPdfRawText.Create('<</Marked true>>'));
    CatalogDictionary.AddItem('Metadata',FMetadata);
    FStructTree := TPdfDictionary.Create(FXRef);
    FRoot.Data.AddItem('StructTreeRoot',FStructTree);
    NeedFileID := true;
  end;
  if NeedFileID then begin
    Randomize;
    for i := 0 to high(FileID) do
      FileID[i] := cardinal(Random(MaxInt));
    inc(FileID[0],GetTickCount);
    {$ifdef USE_PDFSECURITY}
    fFileID := MD5Buf(FileID[0],16);
    IDs := '<'+RawByteString(MD5DigestToString(fFileID))+'>';
    {$else}
    SetLength(IDs,34);
    P := pointer(IDs);
    P[0] := '<';
    SynCommons.BinToHex(PAnsiChar(@FileID[0]),P+1,16);
    P[33] := '>';
    {$endif USE_PDFSECURITY}
    ID := TPdfArray.Create(FXref);
    ID.AddItem(TPdfRawText.Create(IDs));
    ID.AddItem(TPdfRawText.Create(IDs));
    FTrailer.Attributes.AddItem('ID',ID);
  end;
  {$ifdef USE_PDFSECURITY}
  if fEncryption<>nil then
    fEncryption.AttachDocument(self);
  {$endif USE_PDFSECURITY}
end;

function TPdfDocument.AddXObject(const AName: PDFString; AXObject: TPdfXObject): integer;
begin
  if GetXObject(AName)<>nil then
    raise EPdfInvalidValue.CreateFmt('AddXObject: dup name %s', [AName]);
  // check whether AImage is valid PdfImage or not.
  if (AXObject=nil) or (AXObject.Attributes=nil) or
    (AXObject.Attributes.TypeOf<>'XObject') or
    (AXObject.Attributes.PdfNameByName('Subtype')=nil) then
    raise EPdfInvalidValue.CreateFmt('AddXObject: invalid TPdfImage %s', [AName]);
  FXref.AddObject(AXObject);
  result := RegisterXObject(AXObject, AName);
end;

function TPdfDocument.AddPage: TPdfPage;
var FResources: TPdfDictionary;
begin
  if FCurrentPages=nil then
    raise EPdfInvalidOperation.Create('AddPage');
  // create a new page object and add it to the current pages dictionary
  result := fTPdfPageClass.Create(self);
  FXref.AddObject(result);
  fRawPages.Add(result); // pages may be nested
  _Pages_AddKids(FCurrentPages, result);
  result.AddItem('Type', 'Page');
  result.AddItem('Parent', FCurrentPages);
  // create page resources
  FResources := TPdfDictionary.Create(FXref);
  result.AddItem('Resources',FResources);
  FResources.AddItem('Font',TPdfDictionary.Create(FXref));
  FResources.AddItem('XObject',TPdfDictionary.Create(FXref));
  // create page content
  FResources.AddItem('ProcSet',TPdfArray.CreateNames(FXref,['PDF','Text','ImageC']));
  result.AddItem('Contents',TPdfStream.Create(self));
  // assign this page to the current PDF canvas
  FCanvas.SetPage(result);
end;

procedure TPdfDocument.FreeDoc;
var i: integer;
begin
  if FXObjectList<>nil then begin
    FreeAndNil(FXObjectList);
    for i := FFontList.Count-1 downto 0 do
      TObject(FFontList.List[i]).Free;
    FreeAndNil(FFontList);
    for i := FObjectList.Count-1 downto 0 do
      TObject(FObjectList.List[i]).Free;
    FreeAndNil(FObjectList);
    FreeAndNil(FXref);
    FreeAndNil(FTrailer);
  end;
end;

procedure TPdfDocument.SaveToStream(AStream: TStream; ForceModDate: TDateTime=0);
begin
  if FCanvas.Page=nil then
    RaiseInvalidOperation;
  SaveToStreamDirectBegin(AStream,ForceModDate);
  SaveToStreamDirectEnd;
end;

procedure TPdfDocument.RaiseInvalidOperation;
begin
  raise EPdfInvalidOperation.Create('TPdfDocument.Document is null');
end;

function TPdfDocument.SaveToFile(const aFileName: TFileName): boolean;
var FS: TFileStream;
begin
  try
    FS := TFileStream.Create(aFileName,fmCreate);
    try
      SaveToStream(FS);
      result := true;
    finally
      FS.Free;
    end;
  except
    on E: Exception do // error on file creation (opened in reader?)
      result := false;
  end;
end;

procedure TPdfDocument.SaveToStreamDirectBegin(AStream: TStream;
  ForceModDate: TDateTime);
const PDFHEADER: array[TPdfFileFormat] of PDFString = (
    '%PDF-1.3'#10, '%PDF-1.4'#10'%'#228#229#230#240#10,
    '%PDF-1.5'#10'%'#241#242#243#244#10, '%PDF-1.6'#10'%'#245#246#247#248#10);
begin
  if fSaveToStreamWriter<>nil then
    raise EPdfInvalidOperation.Create('SaveToStreamDirectBegin called twice');
  // write all objects to specified stream
  if ForceModDate=0 then
    FInfo.ModDate := Now else
    FInfo.ModDate := ForceModDate;
  FRoot.SaveOpenAction;
  // some PDF/A-1 requirements
  if PDFA1 then begin
    FMetaData.Writer.Add(RawByteString(
      '<?xpacket begin="'#$EF#$BB#$BF'" id="W5M0MpCehiHzreSzNTczkc9d"?>'+
      '<x:xmpmeta xmlns:x="adobe:ns:meta/" x:xmptk="SynPdf">'+
      '<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">'+
      '<rdf:Description rdf:about="" xmlns:xmp="http://ns.adobe.com/xap/1.0/">'+
      '<xmp:CreateDate>')).AddIso8601(Info.CreationDate).Add('Z</xmp:CreateDate>'+
      '<xmp:ModifyDate>').AddIso8601(Info.ModDate).Add('Z</xmp:ModifyDate>'+
      '<xmp:CreatorTool>').Add(StringToUTF8(Info.Creator)).
       Add('</xmp:CreatorTool></rdf:Description>'+
      '<rdf:Description rdf:about="" xmlns:dc="http://purl.org/dc/elements/1.1/">'+
      '<dc:title><rdf:Alt><rdf:li xml:lang="x-default">').
       Add(StringToUTF8(Info.Title)).Add('</rdf:li></rdf:Alt></dc:title>'+
      '<dc:creator><rdf:Seq><rdf:li xml:lang="x-default">').
       Add(StringToUTF8(Info.Author)).Add('</rdf:li></rdf:Seq></dc:creator>'+
      '<dc:description><rdf:Alt><rdf:li xml:lang="x-default">').
       Add(StringToUTF8(Info.Subject)).Add('</rdf:li></rdf:Alt></dc:description>'+
      '</rdf:Description>'+
      '<rdf:Description rdf:about="" xmlns:pdf="http://ns.adobe.com/pdf/1.3/">'+
      '<pdf:Keywords>').Add(StringToUTF8(Info.Keywords)).Add('</pdf:Keywords>'+
      '<pdf:Producer>'+PDF_PRODUCER+'</pdf:Producer></rdf:Description>'+
      '<rdf:Description rdf:about="" xmlns:pdfaid="http://www.aiim.org/pdfa/ns/id/">'+
      '<pdfaid:part>1</pdfaid:part><pdfaid:conformance>A</pdfaid:conformance>'+
      '</rdf:Description></rdf:RDF></x:xmpmeta><?xpacket end="w"?>');
  end;
  // write beginning of the content
  fSaveToStreamWriter := TPdfWrite.Create(self,AStream);
  fSaveToStreamWriter.Add(PDFHEADER[fFileformat]);
end;

procedure TPdfDocument.SaveToStreamDirectPageFlush(FlushCurrentPageNow: boolean);
var i: integer;
begin
  if (self=nil) or (fSaveToStreamWriter=nil) or (FCanvas.FPage=nil) then
    raise EPdfInvalidOperation.Create('SaveToStreamDirectPageFlush');
  if FlushCurrentPageNow then
    FCanvas.FPage.FSaveAtTheEnd := false;
  for i := 1 to FXref.ItemCount-1 do  // ignore FXref[0] = root PDF_FREE_ENTRY
    with FXref.Items[i] do
    if (ByteOffset<=0) and not Value.FSaveAtTheEnd then begin
      fByteOffset := fSaveToStreamWriter.Position;
      Value.WriteValueTo(fSaveToStreamWriter);
    end;
end;

procedure TPdfDocument.SaveToStreamDirectEnd;
var i: integer;
begin
  if (self=nil) or (fSaveToStreamWriter=nil) then
    raise EPdfInvalidOperation.Create('SaveToStreamDirectEnd');
  try
    // saving outline tree
    if UseOutlines then
      FOutlineRoot.Save;
    // update font details after all the pages are drawn
    for i := 0 to FFontList.Count-1 do
    with TPdfFontTrueType(FFontList.List[i]) do
      if fTrueTypeFontsIndex<>0 then
        PrepareForSaving;
    // write pending objects
    if fFileFormat>=pdf15 then
      FTrailer.ToCrossReference(self);
    for i := 1 to FXref.ItemCount-1 do
      with FXref.Items[i] do
      if ByteOffset<=0 then begin
        fByteOffset := fSaveToStreamWriter.Position;
        if Value<>FTrailer.FCrossReference then
          Value.WriteValueTo(fSaveToStreamWriter);
      end;
    FTrailer.XrefAddress := fSaveToStreamWriter.Position;
    if fFileFormat<pdf15 then
      FXref.WriteTo(fSaveToStreamWriter);
    FTrailer.Attributes.PdfNumberByName('Size').Value := FXref.ItemCount;
    FTrailer.WriteTo(fSaveToStreamWriter);
    fSaveToStreamWriter.Save; // flush TPdfWrite buffer into AStream
  finally
    FreeAndNil(fSaveToStreamWriter);
  end;
end;

procedure TPdfDocument.RegisterFont(aFont: TPdfFont);
var st: shortstring;
begin
  // fonts are not registered as xref, but registered in FontList[]
  str(FFontList.Count,st);
  aFont.FShortCut := 'F'+PDFString(st);
  aFont.Data.AddItem('Name', aFont.FShortCut);
  FFontList.Add(aFont);
end;

function TPdfDocument.GetRegisteredNotTrueTypeFont(const APDFFontName: PDFString): TPdfFont;
var i: integer;
begin
  // if specified standard font exists in fontlist, returns it
  with FFontList do
    for i := 0 to Count-1 do begin
      result := TPdfFont(List[i]);
      if (result.FTrueTypeFontsIndex=0) and
         (result.Name=APDFFontName) then
        exit;
    end;
  result := nil;
end;

function TPdfDocument.GetRegisteredTrueTypeFont(AFontIndex: integer;
  AStyle: TPdfFontStyles; ACharSet: byte): TPdfFont;
var i: integer;
begin
  // if specified font exists in fontlist, returns the WinAnsi version
  if AFontIndex>=0 then
  with FFontList do
    for i := 0 to Count-1 do begin
      result := TPdfFont(List[i]);
      if (result.FTrueTypeFontsIndex=AFontIndex) and
         not TPdfFontTrueType(result).Unicode and
         (TPdfFontTrueType(result).Style=AStyle) and
         (TPdfFontTrueType(result).fLogFont.lfCharSet=ACharSet) then
        exit;
    end;
  result := nil;
end;

function CompareLogFontW(const L1,L2: TLogFontW): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  if (L1.lfWeight<>L2.lfWeight) or (L1.lfItalic<>L2.lfItalic) then
    // ignore lfHeight/lfUnderline/lfStrikeOut:
    // font size/underline/strike are internal to PDF graphics state
    result := false else
    result := (AnsiICompW(L1.lfFaceName,L2.lfFaceName)=0);
end;

function TPdfDocument.GetTrueTypeFontIndex(const AName: RawUTF8): integer;
begin
  if StrIComp(pointer(FTrueTypeFontLastName),pointer(AName))=0 then begin
    result := FTrueTypeFontLastIndex; // simple but efficient cache
    exit;
  end;
  result := FastFindPUTF8CharSorted(pointer(FTrueTypeFonts),high(FTrueTypeFonts),pointer(AName),@StrIComp);
  if result>=0 then begin
    FTrueTypeFontLastName := AName;
    FTrueTypeFontLastIndex := result;
  end;
end;

function TPdfDocument.GetRegisteredTrueTypeFont(const AFontLog: TLogFontW): TPdfFont;
var i: integer;
begin
  // if specified font exists in fontlist, returns the WinAnsi version
  with FFontList do
    for i := 0 to Count-1 do begin
      result := TPdfFont(List[i]);
      if (result.FTrueTypeFontsIndex<>0) and
         not TPdfFontTrueType(result).Unicode and
        CompareLogFontW(TPdfFontTrueType(result).fLogFont,AFontLog) then
        exit;
    end;
  result := nil;
end;

procedure TPdfDocument.SetStandardFontsReplace(const Value: boolean);
begin
  if FCharSet<>ANSI_CHARSET then
    FStandardFontsReplace := false else
    FStandardFontsReplace := Value;
end;

function TPdfDocument.GetEmbeddedTTFIgnore: TRawUTF8List;
begin
  if fEmbeddedTTFIgnore=nil then
    fEmbeddedTTFIgnore := TRawUTF8List.Create([fCaseSensitive,fNoDuplicate]);
  result := fEmbeddedTTFIgnore;
end;

procedure TPdfDocument.SetDefaultPaperSize(const Value: TPDFPaperSize);
const PAPERSIZE: array[TPDFPaperSize] of array[0..1] of integer =
  ( (595,842), (419,595), (842,1190), (1190,1683), (1683,2382), (2382,3369),
    (612,792), (612,1008), (0,0) );
begin // psA4, psA5, psA3, psA2, psA1, psA0, psLetter, psLegal, psUserDefined
  FDefaultPaperSize := Value;
  if Value<>psUserDefined then begin
    FDefaultPageWidth := PAPERSIZE[Value,0];
    FDefaultPageHeight := PAPERSIZE[Value,1];
  end;
end;

procedure TPdfDocument.SetDefaultPageHeight(const Value: cardinal);
begin
  FDefaultPageHeight := Value;
  FDefaultPaperSize := psUserDefined;
end;

procedure TPdfDocument.SetDefaultPageWidth(const Value: cardinal);
begin
  FDefaultPageWidth := Value;
  FDefaultPaperSize := psUserDefined;
end;

function TPdfDocument.GetDefaultPageLandscape: boolean;
begin
  result := FDefaultPageWidth>FDefaultPageHeight;
end;

procedure TPdfDocument.SetDefaultPageLandscape(const Value: boolean);
var tmp: integer;
begin
  if Value<>DefaultPageLandscape then begin
    tmp := FDefaultPageHeight;
    FDefaultPageHeight := FDefaultPageWidth;
    FDefaultPageWidth := tmp;
  end;
end;

function TPdfDocument.GetDCWithFont(TTF: TPdfFontTrueType): HDC;
begin
  if self=nil then
    result := 0 else begin
    if fSelectedDCFontOld<>0 then // prevent resource leak
      SelectObject(FDC,fSelectedDCFontOld);
    fSelectedDCFontOld := SelectObject(FDC,TTF.fHGDI);
    result := fDC;
  end;
end;

const
  TTFCFP_MAC_PLATFORMID = 1;
  TTFCFP_MS_PLATFORMID = 3;
  TTFCFP_SYMBOL_CHAR_SET = 0;
  TTFCFP_UNICODE_CHAR_SET = 1;

  TTFCFP_FLAGS_SUBSET = 1;
  TTFMFP_SUBSET = 0;
  TTFCFP_FLAGS_TTC = 4;
  TTCF_TABLE = $66637474;

type
  /// a TTF name record used for the 'name' Format 4 table
  TNameRecord = packed record
    platformID: word;
    encodingID: word;
    languageID: word;
    nameID: word;
    length: word;
    offset: word;
  end;
  /// header for the 'name' Format 4 table
  TNameFmt4 = packed record
    /// Format selector (=0/1)
    format: word;
    /// Number of name records
    count: word;
    /// Offset to start of string storage (from start of table)
    stringOffset: word;
    /// The name records where count is the number of records
    FirstNameRecord: TNameRecord;
  end;

function TrueTypeFontName(const aFontName: RawUTF8; AStyle: TPdfFontStyles): PDFString;
var i: Integer;
begin // from PDF 1.3 #5.5.2
  SetString(result,PAnsiChar(pointer(aFontName)),length(aFontName));
  for i := length(result) downto 1 do
    if (Result[i]<=' ') or (Result[i]>=#127) then
      Delete(result,i,1); // spaces and not ASCII chars are removed
  if not IsAnsiCompatible(aFontName) then // unique non-void font name
    result := result+PDFString(CardinalToHexLower(CRC32string(aFontName)));
  if pfsItalic in AStyle then
    if pfsBold in AStyle then
      result := result+',BoldItalic' else
      result := result+',Italic' else
    if pfsBold in AStyle then
      result := result+',Bold';
end;

function TPdfDocument.TTFFontPostcriptName(aFontIndex: integer; AStyle: TPdfFontStyles;
  AFont: TPdfFontTrueType): PDFString;
// see http://www.microsoft.com/typography/OTSPEC/name.htm
const NAME_POSTCRIPT = 6;
var fName: TWordDynArray;
    name: ^TNameFmt4;
    aFontName: RawUTF8;
    i, L: integer;
    Rec: ^TNameRecord;
    PW: pointer;
begin
  aFontName := FTrueTypeFonts[aFontIndex];
  if IsAnsiCompatible(aFontName) or (AFont=nil) then begin
    result := TrueTypeFontName(aFontName,AStyle);
    exit; // no need to search for the PostScript name field in TTF content
  end;
  name := GetTTFData(GetDCWithFont(AFont),'name',fName);
  if (name=nil) or (name^.format<>0) then
    exit;
  Rec := @name^.FirstNameRecord;
  for i := 0 to name^.count-1 do
    if (Rec^.nameID=NAME_POSTCRIPT) and (Rec^.platformID=TTFCFP_MS_PLATFORMID) and
       (Rec^.encodingID=1) and (Rec^.languageID=$409) then begin
      PW := PAnsiChar(name)+name^.stringOffset+Rec^.offset;
      L := Rec^.length shr 1;
      if Rec^.offset and 1<>0 then begin // fix GetTTFData() wrong SwapBuffer()
        dec(PByte(PW));
        SwapBuffer(PW,L+1); // restore big-endian original unaligned buffer
        inc(PByte(PW));
        SwapBuffer(PW,L);   // convert from big-endian at correct odd offset
      end;
      RawUnicodeToUtf8(PW,L,aFontName);
      result := TrueTypeFontName(aFontName,AStyle); // adjust name and style
      exit;
    end else
    inc(Rec);
end;

function TPdfDocument.CreateOutline(const Title: string; Level: integer;
  TopPosition: Single): TPdfOutlineEntry;
begin
  result := nil;
  if self=nil then
    exit;
  if fLastOutline=nil then begin
    if not UseOutlines then
      exit; // will raise a GPF otherwise
    result := OutlineRoot;
  end else begin
    result := fLastOutline;
    while (Level<=result.Level) and (result.Parent<>nil) do
      result := result.Parent;
  end;
  result := result.AddChild(Trunc(TopPosition));
  result.Title := Title;
  result.Level := Level;
  fLastOutline := result;
end;

{$ifdef USE_BITMAP}
function TPdfDocument.CreateOrGetImage(B: TBitmap; DrawAt: PPdfBox; ClipRc: PPdfBox): PDFString;
var J: TJpegImage;
    Img: TPdfImage;
    Hash: THash128Rec;
    y,w,h,row: integer;
    nPals: cardinal;
    Pals: array of TPaletteEntry;
const PERROW: array[TPixelFormat] of byte = (0,1,4,8,15,16,24,32,0);
  procedure DoHash(bits: pointer; size: Integer);
  begin
    Hash.c0 := crc32c(Hash.c0,bits,size);
    Hash.c1 := crc32c(Hash.c1,bits,size);
    Hash.c2 := Hash.c2+Hash.c0; // naive, but sufficient, cascading
    Hash.c3 := Hash.c3+Hash.c1;
  end;
begin
  result := '';
  if (self=nil) or (B=nil) then exit;
  w := B.Width;
  h := B.Height;
  if ForceNoBitmapReuse then
    FillCharFast(Hash,sizeof(Hash),0) else begin
    row := PERROW[B.PixelFormat];
    if row=0 then begin
      B.PixelFormat := pf24bit;
      row := 24;
    end;
    Hash.c0 := 0;
    Hash.c1 := 1400305337; // 3 prime numbers
    Hash.c2 := 2468776129;
    Hash.c3 := 3121238909;
    if B.Palette<>0 then begin
      nPals := 0;
      if (GetObject(B.Palette,sizeof(nPals),@nPals)<>0) and (nPals>0) then begin
        SetLength(Pals,nPals);
        if GetPaletteEntries(B.Palette,0,nPals,Pals[0])=nPals then
          DoHash(pointer(Pals),nPals*sizeof(TPaletteEntry));
      end;
    end;
    row := BytesPerScanline(w,row,32);
    for y := 0 to h-1 do
      DoHash(B.ScanLine[y],row);
    result := GetXObjectImageName(Hash,w,h); // search for matching image
  end;
  if result='' then begin
     // create new if no existing TPdfImage match
    if ForceJPEGCompression=0 then
      Img := TPdfImage.Create(Canvas.fDoc,B,True) else begin
      J := TJpegImage.Create;
      try
        J.Assign(B);
        Img := TPdfImage.Create(Canvas.fDoc,J,False);
      finally
        J.Free;
      end;
    end;
    Img.fHash := Hash;
    result := 'SynImg'+UInt32ToPDFString(FXObjectList.ItemCount);
    if ForceJPEGCompression=0 then
      AddXObject(result,Img) else
      RegisterXObject(Img, result);
  end;
  // draw bitmap as XObject
  if DrawAt<>nil then begin
    if ClipRc<>nil then
      with DrawAt^ do
        Canvas.DrawXObjectEx(Left,Top,Width,Height,
          ClipRc^.Left,ClipRc^.Top,ClipRc^.Width,ClipRc^.Height, result) else
      with DrawAt^ do
        Canvas.DrawXObject(Left,Top,Width,Height, result);
  end;
end;
{$endif USE_BITMAP}

function TPdfDocument.CreateOptionalContentGroup(
  ParentContentGroup: TPdfOptionalContentGroup;
  const Title: string; Visible: Boolean): TPdfOptionalContentGroup;
var Dico, DicoD: TPdfDictionary;
    Arr: TPDFArray;

  function FindParentContentGroupArray(Current: TPDFArray): TPDFArray;
  var i: Integer;
  begin
    result := nil;
    if Current=nil then
      exit;
    for i := 0 to Current.ItemCount-1 do
      if Current.Items[i]=ParentContentGroup then begin
        if (i<Current.ItemCount-1) and Current.Items[i+1].InheritsFrom(TPDFArray) then
          result := TPDFArray(Current.Items[i+1]) else begin
          result := TPDFArray.Create(FXRef);
          Current.InsertItem(i+1, result);
        end;
        exit;
      end;
    for i := 0 to Current.ItemCount-1 do
      if Current.Items[i].InheritsFrom(TPDFArray) then begin
        result := FindParentContentGroupArray(TPDFArray(Current.Items[i]));
        if result<>nil then
          exit;
      end;
  end;

begin
  if FUseOptionalContent then begin
    result := TPdfOptionalContentGroup.Create(FXRef);
    FXref.AddObject(result);
    result.AddItem('Type','OCG');
    result.AddItemTextString('Name',Title);
    Dico := FRoot.Data.PdfDictionaryByName('OCProperties');
    if Dico<>nil then begin
      DicoD := Dico.PdfDictionaryByName('D');
      if DicoD<>nil then begin
        Arr := DicoD.PdfArrayByName('Order');
        if ParentContentGroup<>nil then
          Arr := FindParentContentGroupArray(Arr);
        if Arr<>nil then
          Arr.AddItem(result);
        if not Visible then begin
          Arr := DicoD.PdfArrayByName('OFF');
          if Arr<>nil then
            Arr.AddItem(result);
        end;
      end;
      Arr := Dico.PdfArrayByName('OCGs');
      if Arr<>nil then
        Arr.AddItem(result);
    end;
  end else
    result := nil;
end;

procedure TPdfDocument.CreateOptionalContentRadioGroup(
  const ContentGroups: array of TPdfOptionalContentGroup);
var i: Integer;
    Dico, DicoD: TPdfDictionary;
    Arr, RadioArr: TPDFArray;
begin
  if FUseOptionalContent and (Length(ContentGroups)>0) then begin
    Dico := FRoot.Data.PdfDictionaryByName('OCProperties');
    if Dico<>nil then begin
      DicoD := Dico.PdfDictionaryByName('D');
      if DicoD<>nil then begin
        Arr := DicoD.PdfArrayByName('RBGroups');
        if Arr<>nil then begin
          RadioArr := TPDFArray.Create(FXref);
          for i  :=  Low(ContentGroups) to High(ContentGroups) do
            if ContentGroups[i]<>nil then
              RadioArr.AddItem(ContentGroups[i]);
          if RadioArr.ItemCount>0 then
            Arr.AddItem(RadioArr) else
            FreeAndNil(RadioArr);
        end;
      end;
    end;
  end;
end;

procedure TPdfDocument.SetUseOptionalContent(const Value: boolean);
begin
  FUseOptionalContent := Value;
  NewDoc;
end;

procedure TPdfDocument.SetPDFA1(const Value: boolean);
begin
  fPDFA1 := Value;
  NewDoc;
end;

procedure TPdfDocument.SetFontFallBackName(const Value: string);
begin
  fFontFallBackIndex := GetTrueTypeFontIndex(StringToUTF8(Value));
end;

function TPdfDocument.GetFontFallBackName: string;
begin
  if fFontFallBackIndex>=0 then
    result := UTF8ToString(FTrueTypeFonts[fFontFallBackIndex]) else
    result := '';
end;

function TPdfDocument.GetGeneratePDF15File: boolean;
begin
  result := (self<>nil) and (fFileFormat>=pdf15);
end;

procedure TPdfDocument.SetGeneratePDF15File(const Value: boolean);
begin
  if fFileFormat<>pdf16 then
    if Value then
      fFileFormat := pdf15 else
      fFileFormat := pdf14;
end;


{ TPdfCanvas }

constructor TPdfCanvas.Create(APdfDoc: TPdfDocument);
begin
  FDoc := APdfDoc;
  FFactor := 72/FDoc.FScreenLogPixels; // PDF expect 72 pixels per inch
  FFactorX := FFactor;
  FFactorY := FFactor;
  FDevScaleX := 1;
  FDevScaleY := 1;
  FMappingMode := MM_TEXT;
  fUseMetaFileTextPositioning := tpSetTextJustification;
  fKerningHScaleBottom := 99.0;
  fKerningHScaleTop := 101.0;
end;

function TPdfCanvas.GetPage: TPdfPage;
begin
  if (Self<>nil) and (FPage<>nil) then
    result := FPage else
    raise EPdfInvalidOperation.Create('GetPage');
end;

procedure TPdfCanvas.SetPage(APage: TPdfPage);
begin
  FPage := APage;
  FPageFontList := FPage.GetResources('Font');
  FContents := TPdfStream(FPage.ValueByName('Contents'));
  FFactor := 72/FDoc.FScreenLogPixels; // PDF expect 72 pixels per inch
end;

procedure TPdfCanvas.SetPDFFont(AFont: TPdfFont; ASize: Single);
begin
  // check if this font is already the current font
  if (AFont=nil) or ((FPage.Font=AFont) and (FPage.FontSize=ASize)) then
    Exit;
  // add this font to the resource array of the current page
  if FPageFontList.ValueByName(AFont.ShortCut)=nil then
    FPageFontList.AddItem(AFont.ShortCut, AFont.Data);
  // change the font
  if FContents<>nil then
    SetFontAndSize(AFont.ShortCut, ASize); // e.g. SetFontAndSize('F0',12)
  FPage.Font := AFont;
  FPage.FontSize := ASize;
end;

procedure InitializeLogFontW(const aFontName: RawUTF8; aStyle: TPdfFontStyles;
  var aFont: TLogFontW);
begin
  FillCharFast(AFont,sizeof(AFont),0);
  with aFont do begin
    lfHeight := -1000;
    if pfsBold in AStyle then
      lfWeight := FW_BOLD else
      lfWeight := FW_NORMAL;
    lfItalic := Byte(pfsItalic in AStyle);
    lfUnderline := Byte(pfsUnderline in AStyle);
    lfStrikeOut := Byte(pfsStrikeOut in AStyle);
    UTF8ToWideChar(lfFaceName,Pointer(aFontName));
  end;
end;

const // see PDF ref 9.6.2.2: Standard Type 1 Fonts
  // WidthArray[30]=Ascent, WidthArray[31]=Descent,
  // WidthArray[32..255]=Width(#32..#255)
  ARIAL_W_ARRAY: array[30..255] of SmallInt = (
    905,-212,278,278,355,556,556,889,667,191,333,333,389,584,278,333,
    278,278,556,556,556,556,556,556,556,556,556,556,278,278,584,584,
    584,556,1015,667,667,722,722,667,611,778,722,278,500,667,556,833,
    722,778,667,778,722,667,611,722,667,944,667,667,611,278,278,278,
    469,556,333,556,556,500,556,556,278,556,556,222,222,500,222,833,
    556,556,556,556,333,500,278,556,500,722,500,500,500,334,260,334,
    584,0,556,0,222,556,333,1000,556,556,333,1000,667,333,1000,0,
    611,0,0,222,222,333,333,350,556,1000,333,1000,500,333,944,0,
    500,667,0,333,556,556,556,556,260,556,333,737,370,556,584,0,
    737,333,400,584,333,333,333,556,537,278,333,333,365,556,834,834,
    834,611,667,667,667,667,667,667,1000,722,667,667,667,667,278,278,
    278,278,722,722,778,778,778,778,778,584,778,722,722,722,722,667,
    667,611,556,556,556,556,556,556,889,500,556,556,556,556,278,278,
    278,278,556,556,556,556,556,556,556,584,611,556,556,556,556,500,556,500);
  ARIAL_BOLD_W_ARRAY: array[30..255] of SmallInt = (
    905,-212,278,333,474,556,556,889,722,238,333,333,389,584,278,333,
    278,278,556,556,556,556,556,556,556,556,556,556,333,333,584,584,
    584,611,975,722,722,722,722,667,611,778,722,278,556,722,611,833,
    722,778,667,778,722,667,611,722,667,944,667,667,611,333,278,333,
    584,556,333,556,611,556,611,556,333,611,611,278,278,556,278,889,
    611,611,611,611,389,556,333,611,556,778,556,556,500,389,280,389,
    584,0,556,0,278,556,500,1000,556,556,333,1000,667,333,1000,0,
    611,0,0,278,278,500,500,350,556,1000,333,1000,556,333,944,0,
    500,667,0,333,556,556,556,556,280,556,333,737,370,556,584,0,
    737,333,400,584,333,333,333,611,556,278,333,333,365,556,834,834,
    834,611,722,722,722,722,722,722,1000,722,667,667,667,667,278,278,
    278,278,722,722,778,778,778,778,778,584,778,722,722,722,722,667,
    667,611,556,556,556,556,556,556,889,556,556,556,556,556,278,278,
    278,278,611,611,611,611,611,611,611,584,611,611,611,611,611,556,611,556);
  ARIAL_ITALIC_W_ARRAY: array[30..255] of SmallInt = (
    905,-212,278,278,355,556,556,889,667,191,333,333,389,584,278,333,
    278,278,556,556,556,556,556,556,556,556,556,556,278,278,584,584,
    584,556,1015,667,667,722,722,667,611,778,722,278,500,667,556,833,
    722,778,667,778,722,667,611,722,667,944,667,667,611,278,278,278,
    469,556,333,556,556,500,556,556,278,556,556,222,222,500,222,833,
    556,556,556,556,333,500,278,556,500,722,500,500,500,334,260,334,
    584,0,556,0,222,556,333,1000,556,556,333,1000,667,333,1000,0,
    611,0,0,222,222,333,333,350,556,1000,333,1000,500,333,944,0,
    500,667,0,333,556,556,556,556,260,556,333,737,370,556,584,0,
    737,333,400,584,333,333,333,556,537,278,333,333,365,556,834,834,
    834,611,667,667,667,667,667,667,1000,722,667,667,667,667,278,278,
    278,278,722,722,778,778,778,778,778,584,778,722,722,722,722,667,
    667,611,556,556,556,556,556,556,889,500,556,556,556,556,278,278,
    278,278,556,556,556,556,556,556,556,584,611,556,556,556,556,500,556,500);
  ARIAL_BOLDITALIC_W_ARRAY: array[30..255] of SmallInt = (
    905,-212,278,333,474,556,556,889,722,238,333,333,389,584,278,333,
    278,278,556,556,556,556,556,556,556,556,556,556,333,333,584,584,
    584,611,975,722,722,722,722,667,611,778,722,278,556,722,611,833,
    722,778,667,778,722,667,611,722,667,944,667,667,611,333,278,333,
    584,556,333,556,611,556,611,556,333,611,611,278,278,556,278,889,
    611,611,611,611,389,556,333,611,556,778,556,556,500,389,280,389,
    584,0,556,0,278,556,500,1000,556,556,333,1000,667,333,1000,0,
    611,0,0,278,278,500,500,350,556,1000,333,1000,556,333,944,0,
    500,667,0,333,556,556,556,556,280,556,333,737,370,556,584,0,
    737,333,400,584,333,333,333,611,556,278,333,333,365,556,834,834,
    834,611,722,722,722,722,722,722,1000,722,667,667,667,667,278,278,
    278,278,722,722,778,778,778,778,778,584,778,722,722,722,722,667,
    667,611,556,556,556,556,556,556,889,556,556,556,556,556,278,278,
    278,278,611,611,611,611,611,611,611,584,611,611,611,611,611,556,611,556);
  TIMES_ROMAN_W_ARRAY: array[30..255] of SmallInt = (
    891,-216,250,333,408,500,500,833,778,180,333,333,500,564,250,333,
    250,278,500,500,500,500,500,500,500,500,500,500,278,278,564,564,
    564,444,921,722,667,667,722,611,556,722,722,333,389,722,611,889,
    722,722,556,722,667,556,611,722,722,944,722,722,611,333,278,333,
    469,500,333,444,500,444,500,444,333,500,500,278,278,500,278,778,
    500,500,500,500,333,389,278,500,500,722,500,500,444,480,200,480,
    541,0,500,0,333,500,444,1000,500,500,333,1000,556,333,889,0,
    611,0,0,333,333,444,444,350,500,1000,333,980,389,333,722,0,
    444,722,0,333,500,500,500,500,200,500,333,760,276,500,564,0,
    760,333,400,564,300,300,333,500,453,250,333,300,310,500,750,750,
    750,444,722,722,722,722,722,722,889,667,611,611,611,611,333,333,
    333,333,722,722,722,722,722,722,722,564,722,722,722,722,722,722,
    556,500,444,444,444,444,444,444,667,444,444,444,444,444,278,278,
    278,278,500,500,500,500,500,500,500,564,500,500,500,500,500,500,500,500);
  TIMES_ITALIC_W_ARRAY: array[30..255] of SmallInt = (
    891,-216,250,333,420,500,500,833,778,214,333,333,500,675,250,333,
    250,278,500,500,500,500,500,500,500,500,500,500,333,333,675,675,
    675,500,920,611,611,667,722,611,611,722,722,333,444,667,556,833,
    667,722,611,722,611,500,556,722,611,833,611,556,556,389,278,389,
    422,500,333,500,500,444,500,444,278,500,500,278,278,444,278,722,
    500,500,500,500,389,389,278,500,444,667,444,444,389,400,275,400,
    541,0,500,0,333,500,556,889,500,500,333,1000,500,333,944,0,
    556,0,0,333,333,556,556,350,500,889,333,980,389,333,667,0,
    389,556,0,389,500,500,500,500,275,500,333,760,276,500,675,0,
    760,333,400,675,300,300,333,500,523,250,333,300,310,500,750,750,
    750,500,611,611,611,611,611,611,889,667,611,611,611,611,333,333,
    333,333,722,667,722,722,722,722,722,675,722,722,722,722,722,556,
    611,500,500,500,500,500,500,500,667,444,444,444,444,444,278,278,
    278,278,500,500,500,500,500,500,500,675,500,500,500,500,500,444,500,444);
  TIMES_BOLD_W_ARRAY: array[30..255] of SmallInt = (
    891,-216,250,333,555,500,500,1000,833,278,333,333,500,570,250,333,
    250,278,500,500,500,500,500,500,500,500,500,500,333,333,570,570,
    570,500,930,722,667,722,722,667,611,778,778,389,500,778,667,944,
    722,778,611,778,722,556,667,722,722,1000,722,722,667,333,278,333,
    581,500,333,500,556,444,556,444,333,500,556,278,333,556,278,833,
    556,500,556,556,444,389,333,556,500,722,500,500,444,394,220,394,
    520,0,500,0,333,500,500,1000,500,500,333,1000,556,333,1000,0,
    667,0,0,333,333,500,500,350,500,1000,333,1000,389,333,722,0,
    444,722,0,333,500,500,500,500,220,500,333,747,300,500,570,0,
    747,333,400,570,300,300,333,556,540,250,333,300,330,500,750,750,
    750,500,722,722,722,722,722,722,1000,722,667,667,667,667,389,389,
    389,389,722,722,778,778,778,778,778,570,778,722,722,722,722,722,
    611,556,500,500,500,500,500,500,722,444,444,444,444,444,278,278,
    278,278,500,556,500,500,500,500,500,570,500,556,556,556,556,500,556,500);
  TIMES_BOLDITALIC_W_ARRAY: array[30..255] of SmallInt = (
    891,-216,250,389,555,500,500,833,778,278,333,333,500,570,250,333,
    250,278,500,500,500,500,500,500,500,500,500,500,333,333,570,570,
    570,500,832,667,667,667,722,667,667,722,778,389,500,667,611,889,
    722,722,611,722,667,556,611,722,667,889,667,611,611,333,278,333,
    570,500,333,500,500,444,500,444,333,500,556,278,278,500,278,778,
    556,500,500,500,389,389,278,556,444,667,500,444,389,348,220,348,
    570,0,500,0,333,500,500,1000,500,500,333,1000,556,333,944,0,
    611,0,0,333,333,500,500,350,500,1000,333,1000,389,333,722,0,
    389,611,0,389,500,500,500,500,220,500,333,747,266,500,606,0,
    747,333,400,570,300,300,333,576,500,250,333,300,300,500,750,750,
    750,500,667,667,667,667,667,667,944,667,667,667,667,667,389,389,
    389,389,722,722,722,722,722,722,722,570,722,722,722,722,722,611,
    611,500,500,500,500,500,500,500,722,444,444,444,444,444,278,278,
    278,278,500,556,500,500,500,500,500,570,500,556,556,556,556,444,500,444);
  STANDARDFONTS: array[0..11] of record
    Name: PDFString;
    Widths: PSmallIntArray;
  end = (
    (Name: 'Times-Roman'; Widths: @TIMES_ROMAN_W_ARRAY),
    (Name: 'Times-Bold'; Widths: @TIMES_BOLD_W_ARRAY),
    (Name: 'Times-Italic'; Widths: @TIMES_ITALIC_W_ARRAY),
    (Name: 'Times-BoldItalic'; Widths: @TIMES_BOLDITALIC_W_ARRAY),
    (Name: 'Helvetica'; Widths: @ARIAL_W_ARRAY),
    (Name: 'Helvetica-Bold'; Widths: @ARIAL_BOLD_W_ARRAY),
    (Name: 'Helvetica-Oblique'; Widths: @ARIAL_ITALIC_W_ARRAY),
    (Name: 'Helvetica-BoldOblique'; Widths: @ARIAL_BOLDITALIC_W_ARRAY),
    (Name: 'Courier'; Widths: nil), // Widths:nil -> set all widths to 600
    (Name: 'Courier-Bold'; Widths: nil),
    (Name: 'Courier-Oblique'; Widths: nil),
    (Name: 'Courier-BoldOblique'; Widths: nil));

function TPdfCanvas.SetFont(const AName: RawUTF8; ASize: single; AStyle: TPdfFontStyles;
  ACharSet: integer=-1; AForceTTF: integer=-1; AIsFixedWidth: boolean=false): TPdfFont;
const
  STAND_FONTS_PDF: array[TPdfFontStandard] of RawUTF8 = ('Times','Helvetica','Courier');
  STAND_FONTS_WIN: array[TPdfFontStandard] of RawUTF8 = ('Times New Roman','Arial','Courier New');
  STAND_FONTS_UPPER: array[TPdfFontStandard] of PAnsiChar = ('TIMES','HELVETICA','COURIER');
  procedure SetEmbeddedFont(Standard: TPdfFontStandard);
  var BaseIndex: integer;
  begin
    BaseIndex := ord(Standard)*4+(byte(AStyle) and 3);
    result := fDoc.GetRegisteredNotTrueTypeFont(STANDARDFONTS[BaseIndex].Name);
    if result=nil then begin // font not already registered -> add now
      with STANDARDFONTS[BaseIndex] do
        result := TPdfFontType1.Create(fDoc.FXref,Name,Widths);
      fDoc.RegisterFont(result);
    end;
    SetPDFFont(result,ASize); 
  end;
var AFont: TLogFontW;
    FontIndex: integer;
    f: TPdfFontStandard;
begin
  result := nil;
  if (self=nil) or (FDoc=nil) then
    exit; // avoid GPF
  if AForceTTF>=0 then
    // an existing true type font has been specified
    FontIndex := AForceTTF else begin
    // handle use embedded fonts for standard fonts, if needed
    if (fDoc.FCharSet=ANSI_CHARSET) and fDoc.StandardFontsReplace then begin
      // standard/embedded fonts are WinAnsi only
      for f := low(f) to high(f) do
        if SameTextU(AName,STAND_FONTS_PDF[f]) or
           SameTextU(AName,STAND_FONTS_WIN[f]) then begin
          SetEmbeddedFont(f);
          if result<>nil then
            exit; // we got a standard/embedded font
        end;
    end;
    if (FPreviousRasterFontName<>'') and (FPreviousRasterFontName=AName) then
      FontIndex := FPreviousRasterFontIndex else begin
      // search the font in the global system-wide true type fonts list
      FontIndex := fDoc.GetTrueTypeFontIndex(AName);
      if FontIndex<0 then begin // unknown, device or raster font
        if AIsFixedWidth then // sounds to be fixed-width -> set 'Courier New'
          FontIndex := fDoc.GetTrueTypeFontIndex(STAND_FONTS_WIN[pfsCourier]);
        // do not exist as is: find equivalency of some "standard" font
        for f := low(f) to high(f) do
          if (FontIndex<0) and IdemPChar(pointer(AName),STAND_FONTS_UPPER[f]) then
            FontIndex := fDoc.GetTrueTypeFontIndex(STAND_FONTS_WIN[f]);
        if FontIndex<0 then begin // use variable width default font
          FontIndex := FDoc.fFontFallBackIndex;
          if FontIndex<0 then
            FontIndex := fDoc.GetTrueTypeFontIndex('Arial');
          if FontIndex<0 then
            exit;
        end;
        FPreviousRasterFontName := AName;
        FPreviousRasterFontIndex := FontIndex;
      end;
    end;
  end;
  if ACharSet<0 then
    ACharSet := fDoc.CharSet; // force the current PDF Document charset
  result := fDoc.GetRegisteredTrueTypeFont(FontIndex+1,AStyle,ACharSet);
  if result=nil then begin
    // a font of this kind is not already registered -> create it
    FillCharFast(AFont,sizeof(AFont),0);
    with AFont do begin
      lfHeight := -1000;
      if pfsBold in AStyle then
        lfWeight := FW_BOLD else
        lfWeight := FW_NORMAL;
      lfItalic := Byte(pfsItalic in AStyle);
      lfUnderline := Byte(pfsUnderline in AStyle);
      lfStrikeOut := Byte(pfsStrikeOut in AStyle);
      lfCharSet := ACharSet;
      UTF8ToWideChar(lfFaceName,Pointer(fDoc.FTrueTypeFonts[FontIndex]));
    end;
    // we register now the WinAnsi font to the associated fDoc
    result := TPdfFontTrueType.Create(fDoc,FontIndex,AStyle,AFont,nil);
  end;
  if AForceTTF<0 then
    SetPDFFont(result,ASize);
end;

function TPdfCanvas.SetFont(ADC: HDC; const ALogFont: TLogFontW; ASize: single): TPdfFont;
var AStyle: TPdfFontStyles;
    AName: RawUTF8;
begin
  // try if the font is already registered
  result := fDoc.GetRegisteredTrueTypeFont(ALogFont);
  if Result<>nil then begin
    SetPDFFont(result,ASize); // use the existing font, update size if necessary
    exit;
  end;
  // font is not existing -> create new
  AName := RawUnicodeToUtf8(ALogFont.lfFaceName,StrLenW(ALogFont.lfFaceName));
  if ALogFont.lfItalic<>0 then
    AStyle := [pfsItalic] else
    byte(AStyle) := 0;
  if ALogFont.lfUnderline<>0 then
    include(AStyle,pfsUnderline);
  if ALogFont.lfStrikeOut<>0 then
    include(AStyle,pfsStrikeOut);
  if ALogFont.lfWeight>=FW_SEMIBOLD then
    include(AStyle,pfsBold);
  result := SetFont(AName,ASize,AStyle,ALogFont.lfCharSet,-1,
    (ALogFont.lfPitchAndFamily and 3) = FIXED_PITCH);
end;

procedure TPdfCanvas.TextOut(X, Y: Single; const Text: PDFString);
begin
  if FContents<>nil then begin
    FContents.Writer.Add('BT'#10).AddWithSpace(X).AddWithSpace(Y).Add('Td'#10);
    ShowText(Text);
    FContents.Writer.Add('ET'#10);
  end;
end;

procedure TPdfCanvas.TextOutW(X, Y: Single; PW: PWideChar);
begin
  if FContents<>nil then begin
    FContents.Writer.Add('BT'#10).AddWithSpace(X).AddWithSpace(Y).Add('Td'#10);
    ShowText(PW);
    FContents.Writer.Add('ET'#10);
  end;
end;

procedure TPdfCanvas.TextRect(ARect: TPdfRect; const Text: PDFString;
                            Alignment: TPdfAlignment; Clipping: boolean);
var tmpWidth: Single;
    XPos: Single;
begin
  // calculate text width and corresponding X position according to alignment
  tmpWidth := TextWidth(Text);
  case Alignment of
    paCenter: XPos := Round((ARect.Right - ARect.Left - tmpWidth) / 2);
    paRightJustify: XPos := ARect.Right - ARect.Left - Round(tmpWidth);
  else
    XPos := 0;
  end;
  // clipping client rect if needed
  if Clipping then begin
    GSave;
    with ARect do begin
      MoveTo(Left, Top);
      LineTo(Left, Bottom);
      LineTo(Right, Bottom);
      LineTo(Right, Top);
    end;
    ClosePath;
    Clip;
    NewPath;
  end;
  // show the text in the specified rectangle and alignment
  BeginText;
  MoveTextPoint(ARect.Left + XPos, ARect.Top - FPage.FontSize * 0.85);
  ShowText(Text);
  EndText;
  if Clipping then
    GRestore;
end;

procedure TPdfCanvas.MultilineTextRect(ARect: TPdfRect; const Text: PDFString;
  WordWrap: boolean);
var i: integer;
    S1, S2: PDFString;
    XPos, YPos: Single;
    tmpXPos: Single;
    tmpWidth: Single;
    ln: integer;
    ForceNL: boolean;
    FText: PDFString;

  procedure InternalShowText(S: PDFString; AWidth: Single);
  var i: Integer;
  begin
    i := MeasureText(S, AWidth); // simple clipping
    S := Copy(S, 1, i);
    ShowText(S);
  end;

begin
  YPos := ARect.Top - FPage.FontSize*0.85;
  XPos := ARect.Left;
  FText := Text;
  BeginText;
  MoveTextPoint(XPos, YPos);
  i := 1;
  S2 := GetNextWord(FText, i);
  XPos := XPos +  TextWidth(S2);
  if (Length(S2) > 0) and (S2[Length(S2)]=' ') then
    XPos := XPos + FPage.WordSpace;

  while i <= Length(FText) do begin
    ln := Length(S2);
    if (ln >= 2) and (S2[ln]=#10) and (S2[ln-1]=#13) then begin
      S2 := Copy(S2, 1, ln - 2);
      ForceNL := true;
    end else
      ForceNL := false;
    S1 := GetNextWord(FText, i);
    tmpWidth := TextWidth(S1);
    TmpXPos := XPos + tmpWidth;
    if (WordWrap and (TmpXPos > ARect.Right)) or ForceNL then begin
      if S2<>'' then
        InternalShowText(S2, ARect.Right - ARect.Left);
      S2 := '';
      MoveToNextLine;
      ARect.Top := ARect.Top - FPage.Leading;
      if ARect.Top < ARect.Bottom + FPage.FontSize then
        Break;
      XPos := ARect.Left;
    end;
    XPos := XPos + tmpWidth;
    if (Length(S1) > 0) and (S1[Length(S1)]=' ') then
      XPos := XPos + FPage.WordSpace;
    S2 := S2 + S1;
  end;

  if S2<>'' then
    InternalShowText(S2, ARect.Right - ARect.Left);
  EndText;
end;

procedure TPdfCanvas.DrawXObjectPrepare(const AXObjectName: PDFString);
var XObject: TPdfXObject;
    FPageResources: TPdfDictionary;
    i: integer;
begin
  // drawing object must be registered. check object name
  XObject := fDoc.GetXObject(AXObjectName);
  if XObject=nil then
    raise EPdfInvalidValue.CreateFmt('DrawXObject: unknown %s', [AXObjectName]);
  FPageResources := FPage.GetResources('XObject');
  if FPageResources=nil then
    raise EPdfInvalidValue.Create('DrawXObject: no XObject');
  if FPageResources.ValueByName(AXObjectName)=nil then
    FPageResources.AddItem(AXObjectName, XObject);
  {$ifdef USE_METAFILE}
  if XObject.InheritsFrom(TPdfForm) then
    with TPdfForm(XObject).FFontList do
      for i := 0 to ItemCount-1 do
      with Items[i] do
        if FPageFontList.ValueByName(Key)=nil then
          FPageFontList.AddItem(Key, Value);
  {$endif USE_METAFILE}
end;

procedure TPdfCanvas.DrawXObject(X, Y, AWidth, AHeight: Single;
    const AXObjectName: PDFString);
begin
  DrawXObjectPrepare(AXObjectName);
  GSave;
  ConcatToCTM(AWidth, 0, 0, AHeight, X, Y);
  ExecuteXObject(AXObjectName);
  GRestore;
end;

procedure TPdfCanvas.DrawXObjectEx(X, Y, AWidth, AHeight: Single;
      ClipX, ClipY, ClipWidth, ClipHeight: Single; const AXObjectName: PDFString);
begin
  DrawXObjectPrepare(AXObjectName);
  GSave;
  Rectangle(ClipX, ClipY, ClipWidth, ClipHeight);
  Clip;
  NewPath;
  fNewPath := False;
  ConcatToCTM(AWidth, 0, 0, AHeight, X, Y);
  ExecuteXObject(AXObjectName);
  GRestore;
end;


  {* Special Graphics State *}

procedure TPdfCanvas.GSave;
begin
  if FContents<>nil then
    FContents.Writer.Add('q'#10);
end;

procedure TPdfCanvas.GRestore;
begin
  if FContents<>nil then
    FContents.Writer.Add('Q'#10);
end;

procedure TPdfCanvas.ConcatToCTM(a, b, c, d, e, f: Single; Decimals: Cardinal);
begin
  if FContents<>nil then
    FContents.Writer.AddWithSpace(a,Decimals).AddWithSpace(b,Decimals).AddWithSpace(c,Decimals).
      AddWithSpace(d,Decimals).AddWithSpace(e,Decimals).AddWithSpace(f,Decimals).Add('cm'#10);
end;


  {* General Graphics State *}

procedure TPdfCanvas.SetFlat(flatness: Byte);
begin
  if FContents<>nil then
    FContents.Writer.Add(flatness).Add(' i'#10);
end;

procedure TPdfCanvas.SetLineCap(linecap: TLineCapStyle);
begin
  if FContents<>nil then
    FContents.Writer.Add(ord(linecap)).Add(' J'#10);
end;

procedure TPdfCanvas.SetDash(const aarray: array of integer; phase: integer);
var i: integer;
begin
  if FContents=nil then
    exit;
  FContents.Writer.Add('[');
  if (High(aarray) >= 0) and (aarray[0]<>0) then
    for i := 0 to High(aarray) do
      FContents.Writer.AddWithSpace(aarray[i]);
  FContents.Writer.Add('] ').Add(phase).Add(' d'#10);
end;

procedure TPdfCanvas.SetLineJoin(linejoin: TLineJoinStyle);
begin
  if FContents<>nil then
    FContents.Writer.Add(ord(linejoin)).Add(' j'#10);
end;

procedure TPdfCanvas.SetLineWidth(linewidth: Single);
begin
  if FContents<>nil then
    FContents.Writer.AddWithSpace(linewidth).Add('w'#10);
end;

procedure TPdfCanvas.SetMiterLimit(miterlimit: Single);
begin
  if FContents<>nil then
    FContents.Writer.AddWithSpace(miterlimit).Add('M'#10);
end;


  {* Paths *}

procedure TPdfCanvas.MoveTo(x, y: Single);
begin
  if FContents<>nil then
    FContents.Writer.AddWithSpace(x).AddWithSpace(y).Add('m'#10);
end;

procedure TPdfCanvas.LineTo(x, y: Single);
begin
  if FContents<>nil then
    FContents.Writer.AddWithSpace(x).AddWithSpace(y).Add('l'#10);
end;

procedure TPdfCanvas.CurveToC(x1, y1, x2, y2, x3, y3: Single);
begin
  if FContents<>nil then
    FContents.Writer.AddWithSpace(x1).AddWithSpace(y1).AddWithSpace(x2).
      AddWithSpace(y2).AddWithSpace(x3).AddWithSpace(y3).Add('c'#10);
end;

procedure TPdfCanvas.CurveToV(x2, y2, x3, y3: Single);
begin
  if FContents<>nil then
    FContents.Writer.AddWithSpace(x2).AddWithSpace(y2).AddWithSpace(x3).
      AddWithSpace(y3).Add('v'#10);
end;

procedure TPdfCanvas.CurveToY(x1, y1, x3, y3: Single);
begin
  if FContents<>nil then
    FContents.Writer.AddWithSpace(x1).AddWithSpace(y1).AddWithSpace(x3).
      AddWithSpace(y3).Add('y'#10);
end;

procedure TPdfCanvas.Rectangle(x, y, width, height: Single);
begin
  if FContents<>nil then
    FContents.Writer.AddWithSpace(x).AddWithSpace(y).AddWithSpace(width).
      AddWithSpace(height).Add('re'#10);
end;

procedure TPdfCanvas.Closepath;
begin
  if FContents<>nil then
    FContents.Writer.Add('h'#10);
end;

procedure TPdfCanvas.NewPath;
begin
  fNewPath := true;
  if FContents<>nil then
    FContents.Writer.Add('n'#10);
end;

procedure TPdfCanvas.Stroke;
begin
  if FContents<>nil then
    FContents.Writer.Add('S'#10);
end;

procedure TPdfCanvas.ClosePathStroke;
begin
  if FContents<>nil then
    FContents.Writer.Add('s'#10);
end;

procedure TPdfCanvas.Fill;
begin
  if FContents<>nil then
    FContents.Writer.Add('f'#10);
end;

procedure TPdfCanvas.Eofill;
begin
  if FContents<>nil then
    FContents.Writer.Add('f*'#10);
end;

procedure TPdfCanvas.FillStroke;
begin
  if FContents<>nil then
    FContents.Writer.Add('B'#10);
end;

procedure TPdfCanvas.ClosepathFillStroke;
begin
  if FContents<>nil then
    FContents.Writer.Add('b'#10);
end;

procedure TPdfCanvas.EofillStroke;
begin
  if FContents<>nil then
    FContents.Writer.Add('B*'#10);
end;

procedure TPdfCanvas.ClosepathEofillStroke;
begin
  if FContents<>nil then
    FContents.Writer.Add('b*'#10);
end;

procedure TPdfCanvas.Clip;
begin
  if FContents<>nil then
    FContents.Writer.Add('W'#10);
end;

procedure TPdfCanvas.Eoclip;
begin
  if FContents<>nil then
    FContents.Writer.Add('W*'#10);
end;


  {* Text handling *}

procedure TPdfCanvas.SetCharSpace(charSpace: Single);
begin
  if FPage.CharSpace=charSpace then Exit;
  FPage.SetCharSpace(charSpace);
  if FContents<>nil then
    FContents.Writer.AddWithSpace(charSpace).Add('Tc'#10);
end;

procedure TPdfCanvas.SetWordSpace(wordSpace: Single);
begin
  if FPage.WordSpace=wordSpace then Exit;
  FPage.SetWordSpace(wordSpace);
  if FContents<>nil then
    FContents.Writer.AddWithSpace(wordSpace).Add('Tw'#10);
end;

procedure TPdfCanvas.SetHorizontalScaling(hScaling: Single);
begin
  if FPage.HorizontalScaling=hScaling then Exit;
  FPage.SetHorizontalScaling(hScaling);
  if FContents<>nil then
    FContents.Writer.Add(hScaling).Add(' Tz'#10);
end;

procedure TPdfCanvas.SetLeading(leading: Single);
begin
  if FPage.Leading=leading then Exit;
  FPage.SetLeading(leading);
  if FContents<>nil then
    FContents.Writer.AddWithSpace(leading).Add('TL'#10);
end;

procedure TPdfCanvas.SetFontAndSize(const fontshortcut: PDFString; size: Single);
begin
  if FContents<>nil then
    FContents.Writer.Add('/').Add(fontshortcut).Add(' ').AddWithSpace(size).Add('Tf'#10);
end;

procedure TPdfCanvas.SetTextRenderingMode(mode: TTextRenderingMode);
begin
  if FContents<>nil then
    FContents.Writer.Add(ord(mode)).Add(' Tr'#10);
end;

procedure TPdfCanvas.SetTextRise(rise: word);
begin
  if FContents<>nil then
    FContents.Writer.Add(rise).Add(' Ts'#10);
end;

procedure TPdfCanvas.BeginText;
begin
  if FContents<>nil then
    FContents.Writer.Add('BT'#10);
end;

procedure TPdfCanvas.EndText;
begin
  if FContents<>nil then
    FContents.Writer.Add('ET'#10);
end;

procedure TPdfCanvas.MoveTextPoint(tx, ty: Single);
begin
  if FContents<>nil then
    FContents.Writer.AddWithSpace(tx).AddWithSpace(ty).Add('Td'#10);
end;

procedure TPdfCanvas.SetTextMatrix(a, b, c, d, x, y: Single);
begin
  if FContents<>nil then
    FContents.Writer.AddWithSpace(a).AddWithSpace(b).AddWithSpace(c).
      AddWithSpace(d).AddWithSpace(x).AddWithSpace(y).Add('Tm'#10);
end;

procedure TPdfCanvas.MoveToNextLine;
begin
  if FContents<>nil then
    FContents.Writer.Add('T*'#10);
end;

{$ifdef HASVARUSTRING}

procedure TPdfCanvas.ShowText(const text: UnicodeString; NextLine: boolean);
begin // direct call of the unicode text drawing method below
  ShowText(pointer(text),NextLine);
end;

{$endif}

procedure TPdfCanvas.ShowText(const text: PDFString; NextLine: boolean);
begin
  if (FContents<>nil) and (text<>'') then
    if (fDoc.FCharSet=ANSI_CHARSET) or IsAnsiCompatible(text) then begin
      if FPage.Font.Unicode and (FPage.FFont.FTrueTypeFontsIndex<>0) then
        SetPDFFont(TPdfFontTrueType(FPage.Font).WinAnsiFont,FPage.FontSize);
      FContents.Writer.Add('(').AddEscapeText(pointer(text),FPage.Font).Add(')').
        Add(SHOWTEXTCMD[NextLine])
    end else begin
      if FPage.FFont.FTrueTypeFontsIndex<>0 then
        // write TrueType text after conversion to unicode
        FContents.Writer.AddToUnicodeHexText(text,NextLine,self) else
        // this standard font should expect MBCS encoding
        FContents.Writer.Add('<').AddHex(text).Add('>').Add(SHOWTEXTCMD[NextLine]);
    end;
end;

procedure TPdfCanvas.ShowText(PW: PWideChar; NextLine: boolean);
begin
  if FContents<>nil then
    FContents.Writer.AddUnicodeHexText(PW,NextLine,self);
end;

procedure TPdfCanvas.ShowGlyph(PW: PWord; Count: integer);
begin
  if FContents<>nil then
    FContents.Writer.AddGlyphs(PW,Count,self);
end;

procedure TPdfCanvas.ExecuteXObject(const xObject: PDFString);
begin
  if FContents<>nil then
    FContents.Writer.Add('/').Add(xObject).Add(' Do'#10);
end;

procedure TPdfCanvas.SetRGBFillColor(Value: TPdfColor);
begin
  if FContents<>nil then
    FContents.Writer.AddColorStr(Value).Add('rg'#10);
end;

procedure TPdfCanvas.SetRGBStrokeColor(Value: TPdfColor);
begin
  if FContents<>nil then
    FContents.Writer.AddColorStr(Value).Add('RG'#10);
end;

procedure TPdfCanvas.SetCMYKFillColor(C, M, Y, K: integer);
begin
  if FContents<>nil then
    FContents.Writer.AddWithSpace(C/100).AddWithSpace(M/100).
      AddWithSpace(Y/100).AddWithSpace(K/100).Add('k'#10);
end;

procedure TPdfCanvas.SetCMYKStrokeColor(C, M, Y, K: integer);
begin
  if FContents<>nil then
    FContents.Writer.AddWithSpace(C/100).AddWithSpace(M/100).
      AddWithSpace(Y/100).AddWithSpace(K/100).Add('K'#10);
end;

function TPdfCanvas.TextWidth(const Text: PDFString): Single;
begin
  result := FPage.TextWidth(Text);
end;

{$ifdef DELPHI5OROLDER}
function StrCharLength(const Str: PChar): Cardinal;
begin
 result := Cardinal(CharNext(Str))-Cardinal(Str);
end;

function NextCharIndex(const S: string; Index: cardinal): cardinal;
begin
  Result := Index + 1;
  assert(Index <= cardinal(Length(S)));
  if SysLocale.FarEast and (S[Index] in LeadBytes) then
    Result := Index + StrCharLength(PChar(S) + Index - 1);
end;
{$endif}

const
  DEFAULT_PDF_WIDTH = 600;

function TPdfCanvas.UnicodeTextWidth(PW: PWideChar): Single;
var Ansi: PDFString;
    i, W, glyphW: integer;
begin
  W := 0;
  if PW<>nil then
    if FPage.FFont.FTrueTypeFontsIndex=0 then begin
      Ansi := CurrentAnsiConvert.UnicodeBufferToAnsi(PW,StrLenW(PW));
      i := 1;
      while i<length(Ansi) do begin // loop is MBCS ready
        inc(W,FPage.FFont.GetAnsiCharWidth(Ansi,i));
        if SysLocale.FarEast  then
          i := NextCharIndex(Ansi,i) else
          inc(i);
      end;
    end else
      with TPdfFontTrueType(FPage.FFont).WinAnsiFont do
      while PW^<>#0 do begin
        glyphW := GetWideCharWidth(PW^);
       if (glyphW=0) and (fDoc.fUseFontFallBack) and (fDoc.fFontFallBackIndex>=0) then
          glyphW := (SetFont('',FPage.FontSize,fStyle,-1,fDoc.fFontFallBackIndex)
            as TPdfFontTrueType).GetWideCharWidth(PW^);
        if glyphW=0 then
          glyphW := DEFAULT_PDF_WIDTH;
        inc(W,glyphW);
        inc(PW);
      end;
  Result := (W*fPage.fFontSize)/1000;
end;

function TPdfCanvas.MeasureText(const Text: PDFString; AWidth: Single): integer;
begin
  result := FPage.MeasureText(Text, AWidth);
end;

const
  // see http://paste.lisp.org/display/1105
  BEZIER: single = 0.55228477716; // = 4/3 * (sqrt(2) - 1);

procedure TPdfCanvas.Ellipse(x, y, width, height: single);
var w2,h2,xw2,yh2: single;
begin
  w2 := width/2;
  h2 := height/2;
  xw2 := x+w2;
  yh2 := y+h2;
  MoveTo(x, yh2);
  CurveToC(x, yh2-h2*BEZIER, xw2-w2*BEZIER, y, xw2, y);
  CurveToC(xw2+w2*BEZIER, y, x+width, yh2-h2*BEZIER, x+width, yh2);
  CurveToC(x+width, yh2+h2*BEZIER, xw2+w2*BEZIER, y+height, xw2, y+height);
  CurveToC(xw2-w2*BEZIER, y+height, x, yh2+h2*BEZIER, x, yh2);
end;

procedure TPdfCanvas.RoundRect(x1, y1, x2, y2, cx, cy: Single);
begin
  cx := cx/2;
  cy := cy/2;
  MoveTo(x1+cx, y1);
  LineTo(x2-cx, y1);
  CurveToC(x2-cx+BEZIER*cx, y1, x2, y1+cy-BEZIER*cy, x2, y1+cy);
  LineTo(x2, y2-cy);
  CurveToC(x2, y2-cy+BEZIER*cy, x2-cx+BEZIER*cx, y2, x2-cx, y2);
  LineTo(x1+cx, y2);
  CurveToC(x1+cx-BEZIER*cx, y2, x1, y2-cy+BEZIER*cy, x1, y2-cy);
  LineTo(x1, y1+cy);
  CurveToC(x1, y1+cy-BEZIER*cy, x1+cx-BEZIER*cx, y1, x1+cx, y1);
  ClosePath;
end;

function TPdfCanvas.GetNextWord(const S: PDFString; var Index: integer): PDFString;
var ln: integer;
    i: integer;
begin
  // getting a word from text
  result := '';
  ln := Length(S);
  if Index > ln then
    Exit;
  i := Index;
  while true do
    if (S[i]=#10) and (S[i-1]=#13) or (S[i]=' ') then begin
      result := Copy(S, Index, i - (Index -1));
      break;
    end else
    if i >= ln then begin
      result := Copy(S, Index, i - (Index - 1));
      break;
    end else
    if SysLocale.PriLangID=LANG_JAPANESE then
      if {$ifdef ISDELPHIXE4}System.AnsiStrings.{$endif}ByteType(S, i)=mbTrailByte then
        if (S[i+1]<>#129) or not(S[i+2] in [#65,#66]) then begin
          result := Copy(S, Index, i - (Index - 1));
          break;
        end else
          inc(i) else
      if ((i < ln) and
         ({$ifdef ISDELPHIXE4}System.AnsiStrings.{$endif}ByteType(S, i + 1)=mbLeadByte)) then begin
        result := Copy(S, Index, i - (Index - 1));
        break;
      end else
      inc(i) else
    inc(i);
   Index := i + 1;
end;

function TPdfCanvas.GetDoc: TPdfDocument;
begin
  if fDoc<>nil then
    result := fDoc else
    raise EPdfInvalidOperation.Create('GetDoc');
end;

function TPdfCanvas.ViewOffsetX(X: Single): Single;
begin
  Result := (((X - FWinOrg.X) * FViewSize.cx / FWinSize.cx) + FViewOrg.X)
end;

function TPdfCanvas.ViewOffsetY(Y: Single): Single;
begin
  Result := ((Y - FWinOrg.Y) * FViewSize.cy / FWinSize.cy) + FViewOrg.Y
end;

function TPdfCanvas.GetWorldFactorX: Single;
begin
  Result := FWorldFactorX
end;

function TPdfCanvas.GetWorldFactorY: Single;
begin
  Result := FWorldFactorY
end;

function TPdfCanvas.I2X(X: Integer): Single;
begin
  result := FOffsetXDef + (FWorldOffsetX + ViewOffsetX(X) * GetWorldFactorX) * FDevScaleX
end;

function TPdfCanvas.I2Y(Y: Integer): Single;
begin
  result := FPage.GetPageHeight - FOffsetYDef -
    (FWorldOffsetY + ViewOffsetY(Y) * GetWorldFactorY) * FDevScaleY
end;

function TPdfCanvas.I2X(X: Single): Single;
begin
  result := FOffsetXDef + (FWorldOffsetX + ViewOffsetX(X) * GetWorldFactorX) * FDevScaleX
end;

function TPdfCanvas.I2Y(Y: Single): Single;
begin
  result := FPage.GetPageHeight - FOffsetYDef -
    (FWorldOffsetY + ViewOffsetY(Y) * GetWorldFactorY) * FDevScaleY
end;

procedure TPdfCanvas.LineToI(x, y: integer);
begin
  LineTo(I2X(X),I2Y(Y));
end;

procedure TPdfCanvas.MoveToI(x, y: integer);
begin
  MoveTo(I2X(X),I2Y(Y));
end;

procedure TPdfCanvas.CurveToCI(x1, y1, x2, y2, x3, y3: integer);
begin
  CurveToC(I2X(x1),I2Y(y1),I2X(x2),I2Y(y2),I2X(x3),I2Y(y3));
end;

procedure TPdfCanvas.MoveToI(x, y: Single);
begin
  MoveTo(I2X(X),I2Y(Y));
end;

procedure TPdfCanvas.LineToI(x, y: Single);
begin
  LineTo(I2X(X),I2Y(Y));
end;

procedure TPdfCanvas.RoundRectI(x1, y1, x2, y2, cx, cy: integer);
begin
  RoundRect(I2X(x1),I2Y(y1),I2X(x2),I2Y(y2),
    cx * FDevScaleX * GetWorldFactorX,-cy * FDevScaleY * GetWorldFactorY);
end;

{$ifdef USE_ARC}
procedure TPdfCanvas.ARCI(centerx, centery, W, H, Sx, Sy, Ex, Ey: integer;
  clockwise: boolean; arctype: TPdfCanvasArcType; var position: TPoint);
var res: teaDrawArray;
    i: integer;
begin
  if CalcCurveArcData(centerx, centery, W, H, Sx, Sy, Ex, Ey, clockwise,  arctype, res) then
    for I := 0 to High(res) do
      with res[i] do
      case res of
        caMoveto:
          MoveTo(I2X(pts[0].x), i2y(pts[0].y));
        caLine:
          LineTo(I2X(pts[0].x), i2y(pts[0].y));
        caCurve:
          CurveToC(I2X(pts[0].x), i2y(pts[0].y),
            I2X(pts[1].x), i2y(pts[1].y),
            I2X(pts[2].x), i2y(pts[2].y));
        caPosition: begin
          position.x := Round(pts[0].x);
          position.y := Round(pts[0].y);
        end;
      end;
end;
{$endif USE_ARC}

procedure TPdfCanvas.PointI(x, y: Single);
begin
  Rectangle(I2X(X),I2Y(Y),1E-2,1E-2); //smalest difference 1E-2 because of rounding to two decimals
end;

function TPdfCanvas.BoxI(Box: TRect; Normalize: boolean): TPdfBox;
var r: TPdfRect;
begin
  // to PDF coordinates conversion
  r := RectI(Box,Normalize);
  result.Width := r.Right - r.Left;
  result.Height := r.Bottom - r.Top;
  result.Left := r.Left;
  result.Top := r.Top;
end;

procedure NormalizeRect(var Rect: TRect); overload;
var tmp: integer;
begin // PDF can't draw twisted rects -> normalize such values
  if Rect.Right<Rect.Left then begin
    tmp := Rect.Left;
    Rect.Left := Rect.Right;
    Rect.Right := tmp;
  end;
  if Rect.Bottom<Rect.Top then begin
    tmp := Rect.Top;
    Rect.Top := Rect.Bottom;
    Rect.Bottom := tmp;
  end;
end;

procedure NormalizeRect(var Rect: TPdfRect); overload;
var tmp: Single;
begin // PDF can't draw twisted rects -> normalize such values
  if Rect.Right<Rect.Left then begin
    tmp := Rect.Left;
    Rect.Left := Rect.Right;
    Rect.Right := tmp;
  end;
  if Rect.Bottom<Rect.Top then begin
    tmp := Rect.Top;
    Rect.Top := Rect.Bottom;
    Rect.Bottom := tmp;
  end;
end;

function TPdfCanvas.RectI(Rect: TRect; Normalize: boolean): TPdfRect;
begin
  result.Left := I2X(Rect.Left);
  result.Right := I2X(Rect.Right-1);
  result.Top := I2Y(Rect.Top);
  result.Bottom := I2Y(Rect.Bottom-1);
  if Normalize then
    NormalizeRect(result);
end;

procedure TPdfCanvas.BeginMarkedContent(Group : TPdfOptionalContentGroup);
var Resources, Properties: TPdfDictionary;
    ID: PDFString;
begin
  if (FContents=nil) or not FDoc.UseOptionalContent then
    exit;
  if Group<>nil  then begin
    ID := 'oc'+UInt32ToPDFString(Group.ObjectNumber);
    // register Group in page resources properties
    Resources := FPage.PdfDictionaryByName('Resources');
    if Resources<>nil then begin
      Properties := Resources.PdfDictionaryByName('Properties');
      if Properties = nil then begin
        Properties := TPdfDictionary.Create(FDoc.FXRef);
        Resources.AddItem('Properties', Properties);
      end;
      if Properties<>nil then
        Properties.AddItem(ID,Group);
    end;
    FContents.Writer.Add('/OC /').Add(ID).Add(' BDC'#10);
  end else
    FContents.Writer.Add('/OC BMC'#10);
end;

procedure TPdfCanvas.EndMarkedContent;
begin
  if (FContents<>nil) and FDoc.UseOptionalContent then
    FContents.Writer.Add('EMC'#10);
end;

{ TPdfDictionaryWrapper }

procedure TPdfDictionaryWrapper.SetData(AData: TPdfDictionary);
begin
  FData := AData;
  if FData<>nil then
    FData.FSaveAtTheEnd := true;
end;

function TPdfDictionaryWrapper.GetHasData: boolean;
begin
  result := (FData=nil);
end;


{ TPdfInfo }

procedure TPdfInfo.SetAuthor(const Value: String);
begin
  FData.AddItemTextString('Author', Value);
end;

procedure TPdfInfo.SetCreationDate(Value: TDateTime);
begin
  FData.AddItemText('CreationDate', _DateTimeToPdfDate(Value));
end;

procedure TPdfInfo.SetModDate(Value: TDateTime);
begin
  FData.AddItemText('ModDate', _DateTimeToPdfDate(Value));
end;

procedure TPdfInfo.SetCreator(const Value: String);
begin
  FData.AddItemTextString('Creator', Value);
end;

procedure TPdfInfo.SetTitle(const Value: String);
begin
  FData.AddItemTextString('Title', Value);
end;

procedure TPdfInfo.SetSubject(const Value: String);
begin
  FData.AddItemTextString('Subject', Value);
end;

procedure TPdfInfo.SetKeywords(const Value: String);
begin
  FData.AddItemTextString('Keywords', Value);
end;

function TPdfInfo.GetAuthor: String;
begin
  result := FData.PdfTextStringValueByName('Author');
end;

function TPdfInfo.GetCreationDate: TDateTime;
var P: TPdfText;
begin
  P := FData.PdfTextByName('CreationDate');
  if P<>nil then
    try
      result := _PdfDateToDateTime(P.Value);
    except
      result := 0;
    end else
    result := 0;
end;

function TPdfInfo.GetModDate: TDateTime;
var P: TPdfText;
begin
  P := FData.PdfTextByName('ModDate');
  if P<>nil then
    try
      result := _PdfDateToDateTime(P.Value);
    except
      result := 0;
    end else
    result := 0;
end;

function TPdfInfo.GetCreator: String;
begin
  result := FData.PdfTextStringValueByName('Creator');
end;

function TPdfInfo.GetTitle: String;
begin
  result := FData.PdfTextStringValueByName('Title');
end;

function TPdfInfo.GetSubject: String;
begin
  result := FData.PdfTextStringValueByName('Subject');
end;

function TPdfInfo.GetKeywords: String;
begin
  result := FData.PdfTextStringValueByName('Keywords');
end;


{ TPdfCatalog }

procedure TPdfCatalog.SaveOpenAction;
begin
  if (FOpenAction=nil) then
    FData.RemoveItem('OpenAction') else
    FData.AddItem('OpenAction', FOpenAction.GetValue);
end;

procedure TPdfCatalog.SetPageLayout(Value: TPdfPageLayout);
var FPageLayout: TPdfName;
begin
  FPageLayout := FData.PdfNameByName('PageLayout');
  if (FPageLayout=nil) or not FPageLayout.InheritsFrom(TPdfName) then
    FData.AddItem('PageLayout', PDF_PAGE_LAYOUT_NAMES[Value]) else
    FPageLayout.Value := PDF_PAGE_LAYOUT_NAMES[Value];
end;

function TPdfCatalog.GetPageLayout: TPdfPageLayout;
var FPageLayout: TPdfName;
    S: PDFString;
begin
  result := plSinglePage;
  FPageLayout := FData.PdfNameByName('PageLayout');
  if (FPageLayout=nil) or not FPageLayout.InheritsFrom(TPdfName) then
    Exit;
  S := FPageLayout.Value;
  for result := low(TPdfPageLayout) to high(TPdfPageLayout) do
    if PDF_PAGE_LAYOUT_NAMES[result]=S then
      exit;
  result := plSinglePage;
end;

function TPdfCatalog.GetNonFullScreenPageMode: TPdfPageMode;
var FDictionary: TPdfDictionary;
    FPageMode: TPdfName;
    S: PDFString;
begin
  result := pmUseNone;
  FDictionary := FData.PdfDictionaryByName('NonFullScreenPageMode');
  if FDictionary=nil then
    Exit;
  FPageMode := FDictionary.PdfNameByName('NonFullScreenPageMode');
  if (FPageMode=nil) or not (FPageMode is TPdfName) then
    Exit;
  S := FPageMode.Value;
  for result := Low(TPdfPageMode) to High(TPdfPageMode) do
    if PDF_PAGE_MODE_NAMES[result]=S then
      exit;
  result := pmUseNone;
end;

const
  PDF_PAGE_VIEWER_NAMES: array[TPdfViewerPreference] of PDFString = (
    'HideToolbar', 'HideMenubar', 'HideWindowUI', 'FitWindow', 'CenterWindow',
    'PrintScaling');

function TPdfCatalog.GetViewerPreference: TPdfViewerPreferences;
var FDictionary: TPdfDictionary;
    V: TPdfViewerPreference;
begin
  result := [];
  FDictionary := FData.PdfDictionaryByName('ViewerPreference');
  if FDictionary<>nil then
    for V := low(V) to high(V) do
      if FDictionary.PdfBooleanByName(PDF_PAGE_VIEWER_NAMES[V])<>nil then
        include(result,V);
end;

procedure TPdfCatalog.SetPageMode(Value: TPdfPageMode);
var FPageMode: TPdfName;
begin
  FPageMode := FData.PdfNameByName('PageMode');
  if (FPageMode=nil) or not (FPageMode is TPdfName) then
    FData.AddItem('PageMode', PDF_PAGE_MODE_NAMES[Value]) else
    FPageMode.Value := PDF_PAGE_MODE_NAMES[Value];
end;

procedure TPdfCatalog.SetNonFullScreenPageMode(Value: TPdfPageMode);
var FDictionary: TPdfDictionary;
    FPageMode: TPdfName;
begin
  FDictionary := FData.PdfDictionaryByName('ViewerPreferences');
  if FDictionary=nil then begin
    FDictionary := TPdfDictionary.Create(Data.ObjectMgr);
    Data.AddItem('ViewerPreferences', FDictionary);
  end;
  // if Value is pmFullScreen, remove 'PageMode' element (use default value)
  if (Value=pmFullScreen) or (Value=pmUseNone) then
    FDictionary.RemoveItem('NonFullScreenPageMode') else begin
    FPageMode := FDictionary.PdfNameByName('NonFullScreenPageMode');
    if (FPageMode=nil) or not (FPageMode is TPdfName) then
      FDictionary.AddItem('NonFullScreenPageMode', PDF_PAGE_MODE_NAMES[Value]) else
      FPageMode.Value := PDF_PAGE_MODE_NAMES[Value];
  end;
end;

procedure TPdfCatalog.SetViewerPreference(Value: TPdfViewerPreferences);
var V: TPdfViewerPreference;
    FDictionary: TPdfDictionary;
begin
  FDictionary := FData.PdfDictionaryByName('ViewerPreferences');
  if (FDictionary=nil) and (Value<>[]) then begin
    FDictionary := TPdfDictionary.Create(Data.ObjectMgr);
    FData.AddItem('ViewerPreferences', FDictionary);
  end;
  if FDictionary<>nil then begin
    for V := low(V) to high(V) do
      if V in Value then
        if V=vpEnforcePrintScaling then
          FDictionary.AddItem(PDF_PAGE_VIEWER_NAMES[V],TPdfName.Create('None')) else
          FDictionary.AddItem(PDF_PAGE_VIEWER_NAMES[V],TPdfBoolean.Create(true)) else
        FDictionary.RemoveItem(PDF_PAGE_VIEWER_NAMES[V]);
    if vpEnforcePrintScaling in Value then begin
      FDictionary.AddItem('Enforce', TPdfArray.CreateNames(Data.ObjectMgr,['PrintScaling']));
      if fOwner<>nil then
        fOwner.fFileFormat := pdf16;
    end else
      FDictionary.RemoveItem('Enforce');
  end;
end;

function TPdfCatalog.GetPageMode: TPdfPageMode;
var FPageMode: TPdfName;
    S: PDFString;
begin
  result := pmUseNone;
  FPageMode := FData.PdfNameByName('PageMode');
  if (FPageMode=nil) or not FPageMode.InheritsFrom(TPdfName) then
    Exit;
  S := FPageMode.Value;
  for result := Low(PDF_PAGE_MODE_NAMES) to High(PDF_PAGE_MODE_NAMES) do
    if PDF_PAGE_MODE_NAMES[result]=S then
      exit;
  result := pmUseNone;
end;

function TPdfCatalog.GetPages: TPdfDictionary;
begin
  result := FData.PdfDictionaryByName('Pages');
  if result=nil then
    raise EPdfInvalidOperation.Create('GetPages');
end;

procedure TPdfCatalog.SetPages(APages: TPdfDictionary);
begin
  if APages.TypeOf='Pages' then
    FData.AddItem('Pages', APages);
end;


{ TPdfFont }

function TPdfFont.GetAnsiCharWidth(const AText: PDFString; APos: integer): integer;
begin
  Result := 0;
end;

constructor TPdfFont.Create(AXref: TPdfXref; const AName: PDFString);
begin
  inherited Create;
  FName := AName;
  Data := TPdfDictionary.Create(AXref);
  AXref.AddObject(FData);
end;

procedure TPdfFont.AddUsedWinAnsiChar(aChar: AnsiChar);
begin
  if Self<>nil then
    include(fWinAnsiUsed,aChar);
end;


{ TPdfFontWinAnsi }

destructor TPdfFontWinAnsi.Destroy;
begin
  FreeMem(fWinAnsiWidth);
  inherited;
end;

function TPdfFontWinAnsi.GetAnsiCharWidth(const AText: PDFString; APos: integer): integer;
begin
  if (fWinAnsiWidth<>nil) and (AText[APos]>=#32) then
    result := fWinAnsiWidth[AText[APos]] else
    result := fDefaultWidth;
end;


{ TPdfFontType1 }

constructor TPdfFontType1.Create(AXref: TPdfXref; const AName: PDFString;
  WidthArray: PSmallIntArray);
var i: integer;
    c: AnsiChar;
    DefaultWidth: word;
    Widths: TPdfArray;
begin
  inherited Create(AXref, AName);
  // adding standard element to the font definition
  Data.AddItem('Type','Font');
  Data.AddItem('Subtype','Type1');
  Data.AddItem('Encoding','WinAnsiEncoding');
  Data.AddItem('FirstChar',32);
  Data.AddItem('LastChar',255);
  Data.AddItem('BaseFont', FName);
  // register font
  if WidthArray=nil then begin
     // [] -> Courier fixed-width font
    fDefaultWidth := DEFAULT_PDF_WIDTH;
    fAscent := 833;
    fDescent := -300;
  end else begin
    // WidthArray[0]=Ascent, WidthArray[1]=Descent, WidthArray[2..]=Width(#32..)
    fAscent := WidthArray^[0];
    fDescent := WidthArray^[1];
    // create "Width" table of the font  (256-32=224)
    Data.AddItem('Widths', TPdfArray.Create(AXref, @WidthArray^[2], 224), true);
  end;
  // initialize char widths array by default value (if missing width parameter
  // is defined, use it as default value.)
  if Data.PdfNumberByName('MissingWidth')<>nil then
    DefaultWidth := Data.PdfNumberByName('MissingWidth').Value else
    DefaultWidth := fDefaultWidth; // typicaly 600 for Times
  GetMem(fWinAnsiWidth,sizeof(fWinAnsiWidth^));
  for c := low(TPdfWinAnsiWidth) to high(TPdfWinAnsiWidth) do
    fWinAnsiWidth^[c] := DefaultWidth;
  FFirstChar := Data.PdfNumberByName('FirstChar').Value;
  FLastChar := Data.PdfNumberByName('LastChar').Value;
  // fill width array with "Widths" table values.
  Widths := Data.PdfArrayByName('Widths');
  if Widths<>nil then
  for i := 0 to Widths.ItemCount-1 do
    if FFirstChar+i >= 32 then
      fWinAnsiWidth^[AnsiChar(FFirstChar+i)] := TPdfNumber(Widths[i]).Value;
end;


{ TPdfFontTrueType }

const
  { collection of flags defining various characteristics of the font
    see PDF Reference 1.3 #5.7.1 }
  PDF_FONT_FIXED_WIDTH = 1;
  PDF_FONT_SERIF       = 2;
  PDF_FONT_SYMBOLIC    = 4;
  PDF_FONT_SCRIPT      = 8;
  PDF_FONT_STD_CHARSET = 32;
  PDF_FONT_ITALIC      = 64;
  PDF_FONT_ALL_CAP     = 65536;
  PDF_FONT_SMALL_CAP   = 131072;
  PDF_FONT_FORCE_BOLD  = 262144;

function TPdfFontTrueType.FindOrAddUsedWideChar(aWideChar: WideChar): integer;
var n, i: integer;
    aSymbolAnsiChar: AnsiChar;
begin
  self := WinAnsiFont;
  result := fUsedWideChar.Add(ord(aWideChar));
  if result<0 then begin
    result := -(result+1); // this WideChar was already existing -> return index
    exit;
  end;
  // this WideChar was just added -> reserve space in fUsedWide[]
  if length(fUsedWide)=fUsedWideChar.Count-1 then
    SetLength(fUsedWide,fUsedWideChar.Count+100);
  n := fUsedWideChar.Count-1;
  if result<n then
    MoveFast(fUsedWide[result],fUsedWide[result+1],(n-result)*4);
  // create associated Unicode Font if necessary
  if UnicodeFont=nil then
    CreateAssociatedUnicodeFont;
  // update fUsedWide[result] for current glyph
  i := UnicodeFont.fUsedWideChar.IndexOf(ord(aWideChar));
  if (i<0) and UnicodeFont.fIsSymbolFont then begin
    TSynAnsiConvert.Engine(fDoc.CodePage).UnicodeBufferToAnsi(
      @aSymbolAnsiChar,@aWideChar,1);
    aWideChar := WideChar($f000+ord(aSymbolAnsiChar));
    i := UnicodeFont.fUsedWideChar.IndexOf(ord(aWideChar));
  end;
  if i<0 then // if this glyph doesn't exist in this font -> set to zero
    i := 0 else
    i := UnicodeFont.fUsedWide[i].int;
  fUsedWide[result].int := i; // update Width and Glyph
end;

function TPdfFontTrueType.GetAndMarkGlyphAsUsed(aGlyph: word): word;
var i: integer;
begin
  result := aGlyph;
  // 1. check if not already registered as used
  with WinAnsiFont do // WinAnsiFont.fUsedWide[] = glyphs used by ShowText
    for i := 0 to fUsedWideChar.Count-1 do
      if fUsedWide[i].Glyph=aGlyph then
        exit; // fast return already existing glyph index
  // 2. register this glyph, and return TTF glyph
  with UnicodeFont do // UnicodeFont.fUsedWide[] = available glyphs from TPdfTTF
    for i := 0 to fUsedWideChar.Count-1 do
      if fUsedWide[i].Glyph=aGlyph then begin
        result := WinAnsiFont.fUsedWide[
          FindOrAddUsedWideChar(WideChar(fUsedWideChar.Values[i]))].Glyph;
        exit; // result may be 0 if this glyph doesn't exist in the CMAP content
      end;
  result := 0; // returns 0 if not found
end;

constructor TPdfFontTrueType.Create(ADoc: TPdfDocument; AFontIndex: integer;
  AStyle: TPdfFontStyles; const ALogFont: TLogFontW; AWinAnsiFont: TPdfFontTrueType);
var W: packed array of TABC;
    c: AnsiChar;
    aFontName: PDFString;
    Flags: integer;
begin
  if AWinAnsiFont<>nil then begin
    fWinAnsiFont := AWinAnsiFont;
    fUnicode := true;
    fUnicodeFont := self;
    fHGDI := AWinAnsiFont.fHGDI; // only one GDI resource is used for both
  end else begin
    fWinAnsiFont := self;
    fHGDI := CreateFontIndirectW(ALogFont);
  end;
  if AWinAnsiFont<>nil then // we use the Postscript Name here
    aFontName := AWinAnsiFont.fName else
    aFontName := ADoc.TTFFontPostcriptName(AFontIndex,AStyle,self);
  inherited Create(ADoc.FXref,aFontName);
  fDoc := ADoc;
  fTrueTypeFontsIndex := AFontIndex+1;
  fStyle := AStyle;
  // adding element to the dictionary
  Data.AddItem('Type', 'Font');
  Data.AddItem('BaseFont', FName);
  // retrieve font details
  fLogFont := ALogFont; // we always need our local copy
  if Unicode then begin
    // 1. Unicode Font
    Data.AddItem('Subtype', 'Type0');
    Data.AddItem('Encoding', 'Identity-H');
    // Retrieve some font details from WinAnsi version
    fFixedWidth := AWinAnsiFont.fFixedWidth;
    fDefaultWidth := AWinAnsiFont.fDefaultWidth;
    fM := AWinAnsiFont.fM;
    fOTM := AWinAnsiFont.fOTM;
    // get TrueType glyphs info
    fDoc.GetDCWithFont(self);
    TPdfTTF.Create(self).Free; // all the magic in one line :)
  end else begin
    // 2. WinAnsi Font
    Data.AddItem('Subtype', 'TrueType');
    Data.AddItem('Encoding', 'WinAnsiEncoding');
    // retrieve default WinAnsi characters widths
    fDoc.GetDCWithFont(self);
    GetTextMetrics(fDoc.FDC,fM);
    fOTM.otmSize := SizeOf(fOTM);
    GetOutlineTextMetrics(fDoc.FDC,SizeOf(fOTM),@fOTM);
    GetMem(fWinAnsiWidth,sizeof(fWinAnsiWidth^));
    SetLength(W,224);
    GetCharABCWidthsA(fDoc.FDC,32,255,W[0]);
    with W[0] do
      fDefaultWidth := integer(abcA+integer(abcB)+abcC);
    if fM.tmPitchAndFamily and TMPF_FIXED_PITCH=0 then begin
      fFixedWidth := true;
      for c := #32 to #255 do
        fWinAnsiWidth[c] := fDefaultWidth;
    end else
      for c := #32 to #255 do
        with W[ord(c)-32] do
          fWinAnsiWidth[c] := integer(abcA+integer(abcB)+abcC);
    // create font descriptor (the WinAnsi one is used also for unicode)
    FFontDescriptor := TPdfDictionary.Create(ADoc.FXref);
    FFontDescriptor.FSaveAtTheEnd := true;
    ADoc.FXref.AddObject(FFontDescriptor);
    FFontDescriptor.AddItem('Type','FontDescriptor');
    FFontDescriptor.AddItem('FontName',FName);
    FFontDescriptor.AddItem('Ascent',fOTM.otmAscent);
    FFontDescriptor.AddItem('CapHeight',666);
    FFontDescriptor.AddItem('Descent',fOTM.otmDescent);
    FFontDescriptor.AddItem('ItalicAngle',fOTM.otmItalicAngle);
    FFontDescriptor.AddItem('StemV',87);
{    if fFixedWidth then
      Flags := PDF_FONT_FIXED_WIDTH else
      Flags := 0;
    if (fsItalic in AStyle) and (fOTM.otmItalicAngle<>0) then
      Flags := Flags or PDF_FONT_ITALIC;
    if Flags=0 then
      Flags := PDF_FONT_STD_CHARSET;}
    if ALogFont.lfCharSet=SYMBOL_CHARSET then
      Flags := PDF_FONT_SYMBOLIC else
      Flags := PDF_FONT_STD_CHARSET;
    FFontDescriptor.AddItem('Flags',Flags);
    with fOTM.otmrcFontBox do
      FFontDescriptor.AddItem('FontBBox',
        TPdfArray.Create(fDoc.FXref, [Left,Bottom,Right,Top]));
    Data.AddItem('FontDescriptor',fFontDescriptor);
  end;
  fAscent := fOTM.otmAscent;
  fDescent := fOTM.otmDescent;
  FDoc.RegisterFont(self);
end;

destructor TPdfFontTrueType.Destroy;
begin
  if not Unicode then
    DeleteObject(fHGDI);
  inherited;
end;

function TPdfFontTrueType.GetWideCharUsed: Boolean;
begin
  result := (fUsedWideChar.Count>0);
end;

function TPdfFontTrueType.GetWideCharWidth(aWideChar: WideChar): Integer;
begin
  self := self.WinAnsiFont; // we need fUsedWide[] to be used glyphs
  result := WideCharToWinAnsi(ord(aWideChar));
  if result>=0 then
    if (fWinAnsiWidth<>nil) and (result>=32) then
      result := fWinAnsiWidth[AnsiChar(result)] else
      result := fDefaultWidth else
      result := fUsedWide[FindOrAddUsedWideChar(aWideChar)].Width;
end;

{ font subset embedding using Windows XP CreateFontPackage() FontSub.dll
  see http://msdn.microsoft.com/en-us/library/dd183502 }

function lpfnAllocate(Size: Integer): pointer; cdecl;
begin
  GetMem(result,Size);
end;

function lpfnReAllocate(Buffer: pointer; Size: Integer): pointer; cdecl;
begin
  ReallocMem(Buffer, Size);
  result := Buffer;
end;

procedure lpfnFree(Buffer: pointer); cdecl;
begin
  FreeMem(Buffer);
end;

var
  FontSub: THandle = INVALID_HANDLE_VALUE;
  CreateFontPackage: function(puchSrcBuffer: pointer; ulSrcBufferSize: cardinal;
    var puchFontPackageBuffer: PAnsiChar; var pulFontPackageBufferSize: cardinal;
    var pulBytesWritten: Cardinal; usFlags, usTTCIndex, usSubsetFormat,
    usSubsetLanguage, usSubsetPlatform, usSubsetEncoding: word;
    pusSubsetKeepList: PWordArray; usSubsetKeepListCount: word;
    lpfnAllocate, lpfnReAllocate, lpfnFree, reserved: pointer): cardinal; cdecl;

procedure TPdfFontTrueType.PrepareForSaving;
var c: AnsiChar;
    i, n, L, ndx, count: integer;
    Descendants: TPdfArray;
    Descendant, CIDSystemInfo: TPdfDictionary;
    ToUnicode: TPdfStream;
    DS: TStream;
    WR: TPdfWrite;
    ttfSize: cardinal;
    ttf: PDFString;
    SubSetData: PAnsiChar;
    SubSetMem: cardinal;
    SubSetSize: cardinal;
    Used: TSortedWordArray;
    usFlags: Word;  // For CreateFontPackage
    ttcIndex: Word; // For CreateFontPackage
    tableTag: Longword;
    {$ifndef DELPHI5OROLDER}
    ttcNumFonts: Longword;
    ttcBytes: array of byte;
    {$endif}
begin
  DS := THeapMemoryStream.Create;
  WR := TPdfWrite.Create(fDoc,DS);
  try
    if Unicode then begin
      // 1. Unicode Font (see PDF 1.3 reference #5.9)
      // create descendant font
      Descendant := TPdfDictionary.Create(fDoc.FXref);
      Descendant.AddItem('Type','Font');
      Descendant.AddItem('Subtype','CIDFontType2');
      Descendant.AddItem('BaseFont',FName);
      if fDoc.PDFA1 then
        Descendant.AddItem('CIDToGIDMap','Identity');
      CIDSystemInfo := TPdfDictionary.Create(FDoc.FXref);
      CIDSystemInfo.AddItem('Supplement',0);
      CIDSystemInfo.AddItemText('Ordering','Identity');
      CIDSystemInfo.AddItemText('Registry','Adobe');
      Descendant.AddItem('CIDSystemInfo',CIDSystemInfo);
      n := WinAnsiFont.fUsedWideChar.Count;
      if n>0 then begin
        fFirstChar := WinAnsiFont.fUsedWide[0].Glyph;
        fLastChar := WinAnsiFont.fUsedWide[n-1].Glyph;
      end;
      Descendant.AddItem('DW',WinAnsiFont.fDefaultWidth);
      if fDoc.PDFA1 or not WinAnsiFont.fFixedWidth then begin
        WR.Add('['); // fixed width will use /DW value
        // WinAnsiFont.fUsedWide[] contains glyphs used by ShowText
        for i := 0 to n-1 do
          with WinAnsiFont.fUsedWide[i] do
            if int<>0 then
              WR.Add(Glyph).Add('[').Add(Width).Add(']');
        Descendant.AddItem('W',TPdfRawText.Create(WR.Add(']').ToPDFString));
      end;
      Descendant.AddItem('FontDescriptor',WinAnsiFont.fFontDescriptor);
      fDoc.FXref.AddObject(Descendant);
      // create and associate descendant fonts array
      Descendants := TPdfArray.Create(fDoc.FXref);
      Descendants.AddItem(Descendant);
      Data.AddItem('DescendantFonts',Descendants);
      // create ToUnicode CMaping
      ToUnicode := TPdfStream.Create(fDoc);
      ToUnicode.Writer.Add('/CIDInit /ProcSet findresource begin'#10+
        '12 dict begin'#10'begincmap'#10'/CIDSystemInfo'#10'<<'#10'/Registry (').
        Add(ShortCut).Add('+0)'#10'/Ordering (UCS)'#10'/Supplement 0'#10'>> def'#10+
        '/CMapName /').Add(ShortCut).Add('+0 def'#10'/CMapType 2 def'#10+
        '1 begincodespacerange'#10'<').AddHex4(fFirstChar).Add('> <').
        AddHex4(fLastChar).Add('>'#10'endcodespacerange'#10);
      ndx := 0;
      while n>0 do begin
        if n>99 then
          L := 99 else
          L := n;
        count := L; // calculate real count of items in this beginbfchar
        for i := ndx to ndx+L-1 do
          if WinAnsiFont.fUsedWide[i].int=0 then
            dec(count);
        ToUnicode.Writer.Add(count).Add(' beginbfchar'#10);
        for i := ndx to ndx+L-1 do
          with WinAnsiFont.fUsedWide[i] do
            if int<>0 then
            ToUnicode.Writer.Add('<').AddHex4(Glyph).
              Add('> <').AddHex4(WinAnsiFont.fUsedWideChar.Values[i]).Add('>'#10);
        dec(n,L);
        inc(ndx,L);
        ToUnicode.Writer.Add('endbfchar'#10);
      end;
      ToUnicode.Writer.Add('endcmap'#10+
        'CMapName currentdict /CMap defineresource pop'#10'end'#10'end');
      Data.AddItem('ToUnicode', ToUnicode);
    end else begin
      // 2. WinAnsi Font
      for c := #32 to #255 do
        if c in fWinAnsiUsed then begin
          fFirstChar := ord(c);
          Break;
        end;
      for c := #255 downto #32 do
        if c in fWinAnsiUsed then begin
          fLastChar := ord(c);
          Break;
        end;
      if fFirstChar<>0 then begin
        Data.AddItem('FirstChar',fFirstChar);
        Data.AddItem('LastChar',fLastChar);
        WR.Add('[');
        for c := AnsiChar(fFirstChar) to AnsiChar(fLastChar) do
          if c in fWinAnsiUsed then
            WR.AddWithSpace(fWinAnsiWidth[c]) else
            WR.Add('0 ');
        FData.AddItem('Widths',TPdfRawText.Create(WR.Add(']').ToPDFString));
      end;
      // embedd True Type font into the PDF file (allow subset of used glyph)
      if fDoc.PDFA1 or (fDoc.EmbeddedTTF and
         ((fDoc.fEmbeddedTTFIgnore=nil) or (fDoc.fEmbeddedTTFIgnore.
           IndexOf(fDoc.FTrueTypeFonts[fTrueTypeFontsIndex-1])<0))) then begin
        fDoc.GetDCWithFont(self);
        {$ifndef DELPHI5OROLDER}
        // is the font in a .ttc collection?
        ttfSize := GetFontData(fDoc.FDC,TTCF_TABLE,0,nil,0);
        if ttfSize<>GDI_ERROR then begin
          // Yes, the font is in a .ttc collection
          // find out how many fonts are included in the collection
          SetLength(ttcBytes,4);
          if GetFontData(fDoc.FDC,TTCF_TABLE,8,pointer(ttcBytes),4) <> GDI_ERROR then
            ttcNumFonts := ttcBytes[3] else // Higher bytes will be zero
            ttcNumFonts := 1;
          // we need to find out the index of the font within the ttc collection
          // (this is not easy, so GetTTCIndex uses lookup on known ttc fonts)
          if (ttcNumFonts < 2) or not
             GetTTCIndex(fDoc.FTrueTypeFonts[fTrueTypeFontsIndex-1],ttcIndex,ttcNumFonts) then
            ttcIndex := 0;
          usFlags := TTFCFP_FLAGS_SUBSET or TTFCFP_FLAGS_TTC;
          tableTag := TTCF_TABLE;
        end else
        {$endif}
        begin
          ttfSize := GetFontData(fDoc.FDC,0,0,nil,0);
          usFlags := TTFCFP_FLAGS_SUBSET;
          ttcIndex := 0;
          tableTag := 0;
        end;
        if ttfSize<>GDI_ERROR then begin
          SetLength(ttf,ttfSize);
          if GetFontData(fDoc.FDC,tableTag,0,pointer(ttf),ttfSize)<>GDI_ERROR then begin
            fFontFile2 := TPdfStream.Create(fDoc);
            if not fDoc.fEmbeddedWholeTTF then begin
              if FontSub=INVALID_HANDLE_VALUE then begin
                FontSub := SafeLoadLibrary('FontSub.dll');
                if FontSub<>0 then
                  CreateFontPackage := GetProcAddress(FontSub,'CreateFontPackage');
              end;
              if (FontSub<>0) and (@CreateFontPackage<>nil) then begin
                // subset magic is done by Windows (API available since XP) :)
                Used.Count := 0;
                for i := fFirstChar to fLastChar do
                  if AnsiChar(i) in fWinAnsiUsed then
                    Used.Add(WinAnsiConvert.AnsiToWide[i]);
                with fUsedWideChar do
                  for i := 0 to Count-1 do
                    Used.Add(Values[i]);
                if CreateFontPackage(pointer(ttf),ttfSize,
                    SubSetData,SubSetMem,SubSetSize,
                    usFlags,ttcIndex,TTFMFP_SUBSET,0,
                    TTFCFP_MS_PLATFORMID,TTFCFP_UNICODE_CHAR_SET,
                    pointer(Used.Values),Used.Count,
                    @lpfnAllocate,@lpfnReAllocate,@lpfnFree,nil)=0 then begin
                  // subset was created successfully -> save to PDF file
                  SetString(ttf,SubSetData,SubSetSize);
                  FreeMem(SubSetData);
                end;
              end;
            end;
            fFontFile2.Writer.Add(ttf);
            fFontFile2.FAttributes.AddItem('Length1',length(ttf));
            // /FontDescriptor is common to WinAnsi and Unicode fonts
            fFontDescriptor.AddItem('FontFile2',fFontFile2);
          end;
        end;
      end;
    end;
  finally
    WR.Free;
    DS.Free;
  end;
end;

procedure TPdfFontTrueType.CreateAssociatedUnicodeFont;
begin
  fUnicodeFont := TPdfFontTrueType.Create(
    fDoc,fTrueTypeFontsIndex-1,fStyle,fLogFont,self);
end;


{ TPdfDestination }

constructor TPdfDestination.Create(APdfDoc: TPdfDocument);
begin
  inherited Create;
  FDoc := APdfDoc;
  if FDoc=nil then
    raise EPdfInvalidOperation.Create('TPdfDestination');
  FPage := FDoc.Canvas.Page;
  FZoom := 1;
end;

destructor TPdfDestination.Destroy;
begin
  if FReference<>nil then
    FReference.Free;
  inherited;
end;

function TPdfDestination.GetElement(Index: integer): Integer;
begin
  result := FValues[Index];
end;

procedure TPdfDestination.SetElement(Index: integer; Value: Integer);
begin
  if FValues[Index]<>Value then
    if Value < 0 then
      FValues[Index] := -1 else
      FValues[Index] := Value;
end;

procedure TPdfDestination.SetZoom(Value: Single);
begin
  if Value<>FZoom then
    if Value < 0 then
      raise EPdfInvalidValue.Create('Zoom<0') else
    if Value > PDF_MAX_ZOOMSIZE then
      raise EPdfInvalidValue.CreateFmt('Zoom>%d', [PDF_MAX_ZOOMSIZE]) else
      FZoom := Value;
end;

function TPdfDestination.GetPageWidth: Integer;
begin
  if FPage.FMediaBox<> nil then
    result := TPdfNumber(FPage.FMediaBox.Items[2]).Value else
    result := FDoc.DefaultPageWidth;
end;

function TPdfDestination.GetPageHeight: Integer;
begin
  if FPage.FMediaBox<>nil then
    result := TPdfNumber(FPage.FMediaBox.Items[3]).Value else
    result := FDoc.DefaultPageHeight;
end;

function TPdfDestination.GetValue: TPdfArray;
const
  DEST_MAX_VALUE = 100;
begin
  // create TPdfArray object from the specified values.
  // the values which are not used are ignored.
  result := TPdfArray.Create(FDoc.FXref);
  with result do begin
    AddItem(FPage);
    AddItem(TPdfName.Create(PDF_DESTINATION_TYPE_NAMES[FType]));
    case FType of
      // if the type is dtXYZ, only Left, Top and Zoom values are used,
      // other properties are ignored.
      dtXYZ:
        begin
          if FValues[0] >= -DEST_MAX_VALUE then
            AddItem(TPdfNumber.Create(Left)) else
            AddItem(TPdfNull.Create);
          if FValues[1] >= -DEST_MAX_VALUE then
            AddItem(TPdfNumber.Create(Top)) else
            AddItem(TPdfNull.Create);
          if FZoom < 0 then
            FZoom := 0;
          AddItem(TPdfReal.Create(FZoom));
        end;
      // if the type is dtFitR, all values except Zoom are used.
      dtFitR:
        begin
          if FValues[0] >= -DEST_MAX_VALUE then
            AddItem(TPdfNumber.Create(Left)) else
            AddItem(TPdfNull.Create);
          if FValues[1] >= -DEST_MAX_VALUE then
            AddItem(TPdfNumber.Create(Bottom)) else
            AddItem(TPdfNull.Create);
          if FValues[2] >= 0 then
            AddItem(TPdfNumber.Create(Right)) else
            AddItem(TPdfNull.Create);
          if FValues[3] >= 0 then
            AddItem(TPdfNumber.Create(Top)) else
            AddItem(TPdfNull.Create);
        end;
      // if the type is dtFitH or dtFitBH, only Top property is used.
      dtFitH, dtFitBH:
          if FValues[1] >= -DEST_MAX_VALUE then
            AddItem(TPdfNumber.Create(Top)) else
            AddItem(TPdfNull.Create);
      // if the type is dtFitV or dtFitBV, only Top property is used.
      dtFitV, dtFitBV:
          if FValues[0] >= -DEST_MAX_VALUE then
            AddItem(TPdfNumber.Create(Left)) else
            AddItem(TPdfNull.Create);
    end;
  end;
end;


{ TPdfOutlineEntry }

constructor TPdfOutlineEntry.Create(AParent: TPdfOutlineEntry;
  TopPosition: integer=-1);
begin
  inherited Create;
  if AParent=nil then
    raise EPdfInvalidValue.Create('CreateEntry');
  FParent := AParent;
  FDoc := AParent.Doc;
  Data := TPdfDictionary.Create(FDoc.FXref);
  FDoc.FXref.AddObject(Data);
  FDoc.FObjectList.Add(Self);
  if TopPosition>=0 then begin
    if FDoc.Canvas.FPage=nil then
      FDoc.RaiseInvalidOperation;
    FDest := FDoc.CreateDestination;
    FDest.DestinationType := dtXYZ;
    FDest.Zoom := 0; // will leave Zoom factor unchanged
    FDest.Left := 0; // go to left side of the page
    FDest.Top := TopPosition;
  end;
end;

destructor TPdfOutlineEntry.Destroy;
begin
  if FReference<>nil then
    FReference.Free;
  inherited;
end;

function TPdfOutlineEntry.AddChild(TopPosition: Integer=-1): TPdfOutlineEntry;
var TmpEntry: TPdfOutlineEntry;
begin
  // increment total Count variable
  inc(FCount);
  TmpEntry := Parent;
  while TmpEntry<>nil do begin
    TmpEntry.FCount := TmpEntry.FCount + 1;
    TmpEntry := TmpEntry.Parent;
  end;
  result := TPdfOutlineEntry.Create(Self,TopPosition);
  if FFirst=nil then
    FFirst := Result;
  if FLast<>nil then
    FLast.FNext := Result;
  Result.FPrev := FLast;
  FLast := Result;
end;

procedure TPdfOutlineEntry.Save;
begin
  Data.AddItem('Parent',FParent.Data);
  if Opened then
    Data.AddItem('Count', FCount) else
    Data.AddItem('Count', -FCount);
  Data.AddItemTextString('Title', FTitle);
  if FDest<>nil then
    Data.AddItem('Dest', FDest.GetValue);
  if FFirst<>nil then
  begin
    Data.AddItem('First', FFirst.Data);
    FFirst.Save;
  end;
  if FLast<>nil then
    Data.AddItem('Last', FLast.Data);
  if FPrev<>nil then
    Data.AddItem('Prev', FPrev.Data);
  if FNext<>nil then
  begin
    Data.AddItem('Next', FNext.Data);
    FNext.Save;
  end;
end;


{ TPdfOutlineRoot }

constructor TPdfOutlineRoot.Create(ADoc: TPdfDocument);
begin
  // no inherited Create() for this "fake" entry
  FDoc := ADoc;
  FOpened := true;
  Data := TPdfDictionary.Create(ADoc.FXref);
  FDoc.FXref.AddObject(Data);
  Data.AddItem('Type', 'Outlines');
  FDoc.FObjectList.Add(Self);
end;

procedure TPdfOutlineRoot.Save;
begin
  Data.AddItem('Count', FCount);
  if FFirst<>nil then begin
    Data.AddItem('First', FFirst.Data);
    FFirst.Save;
  end;
  if FLast<>nil then
    Data.AddItem('Last', FLast.Data);
end;


{ TPdfTTF }

constructor TPdfTTF.Create(aUnicodeTTF: TPdfFontTrueType);
var P: pointer;
    SubTable: ^TCmapSubTableArray absolute P;
    Header: ^TCmapHeader;
    i, n, code, ndx: PtrInt;
    off: cardinal;
    glyphIndex: integer;
    idDeltai, glyphi: PtrInt;
    W, numOfLongHorMetrics: word;
    fUnitsPerEmShr: cardinal;
begin
  // retrieve the 'cmap' (character code mapping) table
  // see http://developer.apple.com/fonts/TTRefMan/RM06/Chap6cmap.html
  // and http://www.microsoft.com/typography/OTSPEC/cmap.htm
  P := GetTTFData(aUnicodeTTF.fDoc.FDC,'cmap',fcmap);
  if P=nil then
    exit;
  Header := P;
  inc(PtrInt(P),SizeOf(TCmapHeader));
  off := 0;
  for i := 0 to Header^.numberSubtables-1 do
    with SubTable^[i] do
      if platformID=TTFCFP_MS_PLATFORMID then
        if platformSpecificID=TTFCFP_SYMBOL_CHAR_SET then begin
          aUnicodeTTF.fIsSymbolFont := true;
          off := offset;
        end else
        if platformSpecificID=TTFCFP_UNICODE_CHAR_SET then begin
          aUnicodeTTF.fIsSymbolFont := false;
          off := offset;
          break; // prefered specific ID
        end;
  if (off=0) or (off and 1<>0) then
    exit; // we handle only Microsoft platform
  i := LongRec(off).Lo; // offset swap to bswap conversion :)
  LongRec(off).Lo := LongRec(off).Hi;
  LongRec(off).Hi := i;
  if off>cardinal(Length(fcmap)*2) then
    exit; // avoid GPF
  fmt4 := Pointer(PtrUInt(fcmap)+off);
  with fmt4^ do begin
    if format<>4 then
      Exit; // we handle only cmap table format 4
    endCode := pointer(PtrUInt(@format)+sizeof(TCmapFmt4));
    startCode := pointer(PtrUInt(endCode)+segCountX2+2); // +2 = reservedPad
    idDelta := pointer(PtrUInt(startCode)+segCountX2);
    idRangeOffset := pointer(PtrUInt(idDelta)+segCountX2);
    glyphIndexArray := pointer(PtrUInt(idRangeOffset)+segCountX2);
  end;
  // 'head', 'hmtx' (horizontal metrics) and 'hhea' (Horizontal Header) tables
  // see http://developer.apple.com/fonts/TTRefMan/RM06/Chap6hmtx.html
  head := GetTTFData(aUnicodeTTF.fDoc.FDC,'head',fhead);
  if head=nil then
    exit;
  P := GetTTFData(aUnicodeTTF.fDoc.FDC,'hmtx',fhmtx);
  if P=nil then
    exit;
  hhea := GetTTFData(aUnicodeTTF.fDoc.FDC,'hhea',fhhea);
  if hhea=nil then
    exit;
  // fill aUnicodeTTF.fUsedWide[] and aUnicodeTTF.fUsedWideChar data
  n := fmt4^.segCountX2 shr 1;
  with aUnicodeTTF.fUsedWideChar do begin
    for i := 0 to n-1 do
      inc(Count,endCode[i]-startCode[i]+1);
    SetLength(Values,Count);
    SetLength(aUnicodeTTF.fUsedWide,Count);
  end;
  ndx := 0;
  for i := 0 to n-1 do begin
    idDeltai := idDelta[i];
    glyphi := idRangeOffset[i];
    if glyphi<>0 then
      glyphi := glyphi shr 1+i-n-startCode[i];
    for code := startCode[i] to endCode[i] do begin
      aUnicodeTTF.fUsedWideChar.Values[ndx] := code;
      if glyphi=0 then
        glyphIndex := code+idDeltai else begin
        glyphIndex := glyphIndexArray[glyphi+code];
        if glyphIndex<>0 then
          inc(glyphIndex,idDeltai);
      end;
      aUnicodeTTF.fUsedWide[ndx].Glyph := glyphIndex;
      inc(ndx);
    end;
  end;
  // UnitsPerEm range is from 16 to 16384. This value should be a power of 2.
  // (from http://www.microsoft.com/typography/OTSPEC/head.htm)
  fUnitsPerEmShr := 0; // fastest integer div for width calculating
  for i := 14 downto 4 do
    if GetBitPtr(@head^.UnitsPerEm,i) then begin
      fUnitsPerEmShr := i;
      break;
    end;
  if fUnitsPerEmShr<>0 then begin
    W := (cardinal(fhmtx[0])*1000) shr fUnitsPerEmShr;
    if aUnicodeTTF.FixedWidth then
      for i := 0 to aUnicodeTTF.fUsedWideChar.Count-1 do
        aUnicodeTTF.fUsedWide[i].Width := W
    else begin
      numOfLongHorMetrics := fhhea[17];
      for i := 0 to aUnicodeTTF.fUsedWideChar.Count-1 do
      with aUnicodeTTF.fUsedWide[i] do
        if Glyph<>0 then
          if Glyph<=numOfLongHorMetrics then
            Width := (cardinal(fhmtx[Glyph*2])*1000) shr fUnitsPerEmShr else
            Width := W;
    end;
  end;
end;


{ TPdfPage }

constructor TPdfPage.Create(ADoc: TPdfDocument);
begin
  if ADoc=nil then // e.g. for TPdfForm.Create
    inherited Create(nil) else begin
    inherited Create(ADoc.FXRef);
    fDoc := ADoc;
    // set page size
    FMediaBox := TPdfArray.Create(ADoc.FXref,
      [0,0,ADoc.DefaultPageWidth,ADoc.DefaultPageHeight]);
    AddItem('MediaBox',FMediaBox);
  end;
  FSaveAtTheEnd := true;
end;

function TPdfPage.GetPageHeight: Integer;
begin
  result := TPdfNumber(FMediaBox.Items[3]).Value;
end;

function TPdfPage.GetPageLandscape: Boolean;
begin
  result := PageWidth>PageHeight;
end;

function TPdfPage.GetPageWidth: Integer;
begin
  result := TPdfNumber(FMediaBox.Items[2]).Value;
end;

function TPdfPage.GetResources(const AName: PDFString): TPdfDictionary;
begin
  Result := PdfDictionaryByName('Resources').PdfDictionaryByName(AName);
end;

function TPdfPage.MeasureText(const Text: PDFString; Width: Single): integer;
var ch: AnsiChar;
    tmpWidth: Single;
    tmpTotalWidth: Single;
    i: Integer;
begin
  Result := 0;
  tmpTotalWidth := 0;
  i := 1;
  while i<=Length(Text) do begin
    ch := Text[i];
    tmpWidth := FFont.GetAnsiCharWidth(Text, i) * FFontSize / 1000;
    if (FHorizontalScaling<>0) and (FHorizontalScaling<>100) then
      tmpWidth := tmpWidth * FHorizontalScaling / 100;
    if tmpWidth > 0 then
      tmpWidth := tmpWidth + FCharSpace else
      tmpWidth := 0;
    if (ch=' ') and (FWordSpace > 0) and (i<>Length(Text)) then
      tmpWidth := tmpWidth + FWordSpace;
    tmpTotalWidth := tmpTotalWidth + tmpWidth;
    if tmpTotalWidth > Width then
      Break;
    if SysLocale.FarEast  then
      i := NextCharIndex(Text,i) else
      inc(i);
    Result := i;
  end;
end;

procedure TPdfPage.SetCharSpace(Value: Single);
begin
  if (Value < PDF_MIN_CHARSPACE) or (VALUE > PDF_MAX_CHARSPACE) then
    raise EPdfInvalidValue.Create('SetCharSpace');
  FCharSpace := Value;
end;

procedure TPdfPage.SetFontSize(Value: Single);
begin
  if (Value < 0) or (Value > PDF_MAX_FONTSIZE) then
    raise EPdfInvalidValue.Create('SetFontSize');
  FFontSize := Value;
end;

procedure TPdfPage.SetHorizontalScaling(Value: Single);
begin
  if Value < PDF_MIN_HORIZONTALSCALING then
    Value := PDF_MIN_HORIZONTALSCALING else
  if Value > PDF_MAX_HORIZONTALSCALING then
    Value := PDF_MAX_HORIZONTALSCALING;
  FHorizontalScaling := Value;
end;

procedure TPdfPage.SetLeading(Value: Single);
begin
  if (Value < 0) or (Value > PDF_MAX_LEADING) then
    raise EPdfInvalidValue.Create('SetLeading');
  FLeading := Value;
end;

procedure TPdfPage.SetPageHeight(AValue: integer);
begin
  TPdfNumber(FMediaBox.Items[3]).Value := AValue
end;

procedure TPdfPage.SetPageLandscape(const Value: Boolean);
var tmp: integer;
begin
  if Value<>PageLandscape then begin
    tmp := PageHeight;
    PageHeight := PageWidth;
    PageWidth := tmp;
  end;
end;

procedure TPdfPage.SetPageWidth(AValue: integer);
begin
  TPdfNumber(FMediaBox.Items[2]).Value := AValue
end;

procedure TPdfPage.SetWordSpace(Value: Single);
begin
  if Value < 0 then
    raise EPdfInvalidValue.Create('SetWordSpace');
  FWordSpace := Value;
end;

function TPdfPage.TextWidth(const Text: PDFString): Single;
var i: integer;
    ch: AnsiChar;
    tmpWidth: Single;
begin
  Result := 0;
  i := 1;
  while i<=Length(Text) do begin
    ch := Text[i];
    tmpWidth := FFont.GetAnsiCharWidth(Text, i) * FFontSize / 1000;
    if (FHorizontalScaling<>0) and (FHorizontalScaling<>100) then
      tmpWidth := tmpWidth * FHorizontalScaling / 100;
    if tmpWidth > 0 then
      tmpWidth := tmpWidth + FCharSpace else
      tmpWidth := 0;
    if (ch=' ') and (FWordSpace > 0) and (i<>Length(Text)) then
      tmpWidth := tmpWidth + FWordSpace;
    Result := Result + tmpWidth;
    if SysLocale.FarEast  then
      i := NextCharIndex(Text,i) else
      inc(i);
  end;
  Result := Result - FCharSpace;
end;


{$ifdef USE_METAFILE}

{ TPdfDocumentGDI }

function TPdfDocumentGDI.AddPage: TPdfPage;
begin
  if (FCanvas<>nil) and (FCanvas.FPage<>nil) then
    TPdfPageGdi(FCanvas.FPage).FlushVCLCanvas;
  result := inherited AddPage;
  FCanvas.FContents.FSaveAtTheEnd := true; // as expected in SaveToStream() below
end;

constructor TPdfDocumentGDI.Create(AUseOutlines: Boolean; ACodePage: integer;
  APDFA1: boolean{$ifdef USE_PDFSECURITY}; AEncryption: TPdfEncryption{$endif});
begin
  inherited;
  fTPdfPageClass := TPdfPageGdi;
  fUseMetaFileTextPositioning := tpSetTextJustification;
  fKerningHScaleBottom := 99.0;
  fKerningHScaleTop := 101.0;
end;

function TPdfDocumentGDI.GetVCLCanvas: TCanvas;
begin
  with TPdfPageGdi(FCanvas.FPage) do begin
    if fVCLCurrentCanvas=nil then
      CreateVCLCanvas;
    result := fVCLCurrentCanvas;
  end;
end;

function TPdfDocumentGDI.GetVCLCanvasSize: TSize;
begin
  if (FCanvas<>nil) and (FCanvas.FPage<>nil) then
  with TPdfPageGdi(FCanvas.FPage) do begin
    if fVCLCurrentCanvas=nil then
      CreateVCLCanvas;
    result := fVCLCanvasSize;
  end else
    Int64(result) := 0;
end;

procedure TPdfDocumentGDI.SaveToStream(AStream: TStream; ForceModDate: TDateTime);
var i: integer;
    P: TPdfPageGDI;
begin
  // write the file header
  SaveToStreamDirectBegin(AStream,ForceModDate);
  // then draw the pages VCL Canvas content on the fly (miminal memory use)
  for i := 0 to fRawPages.Count-1 do begin
    P := fRawPages.List[i];
    P.FlushVCLCanvas;
    if P.fVCLMetaFileCompressed<>'' then begin
      P.SetVCLCurrentMetaFile;
      try
        FCanvas.SetPage(P);
        FCanvas.RenderMetaFile(P.fVCLCurrentMetaFile,1,1,0,0,
          fUseMetaFileTextPositioning,KerningHScaleBottom,KerningHScaleTop,
          fUseMetaFileTextClipping);
      finally
        FreeAndNil(P.fVCLCurrentMetaFile);
      end;
      inherited SaveToStreamDirectPageFlush;
    end;
  end;
  // finish to write PDF content to destination stream
  SaveToStreamDirectEnd;
end;

procedure TPdfDocumentGDI.SaveToStreamDirectPageFlush(FlushCurrentPageNow: boolean);
var P: TPdfPageGDI;
begin
  if fRawPages.Count>0 then begin
    P := fRawPages.List[fRawPages.Count-1];
    if (P=FCanvas.FPage) and (P.fVCLMetaFileCompressed='') and
       (P.fVCLCurrentMetaFile<>nil) and (P.fVCLCurrentCanvas<>nil) then begin
      FreeAndNil(P.fVCLCurrentCanvas); // manual P.SetVCLCurrentMetaFile
      try
        FCanvas.FContents.FSaveAtTheEnd := false; // force flush NOW
        FCanvas.RenderMetaFile(P.fVCLCurrentMetaFile,1,1,0,0,
          fUseMetaFileTextPositioning,KerningHScaleBottom,KerningHScaleTop,
          fUseMetaFileTextClipping);
      finally
        FreeAndNil(P.fVCLCurrentMetaFile);
      end;
    end;
  end;
  inherited SaveToStreamDirectPageFlush;
end;


{ TPdfPageGDI }

procedure TPdfPageGDI.SetVCLCurrentMetaFile;
var tmp: RawByteString;
    Stream: TStream;
begin
  assert(fVCLCurrentMetaFile=nil);
  fVCLCurrentMetaFile := TMetaFile.Create;
  fVCLCanvasSize.cx := MulDiv(PageWidth,FDoc.FScreenLogPixels,72);
  fVCLCanvasSize.cy := MulDiv(PageHeight,FDoc.FScreenLogPixels,72);
  fVCLCurrentMetaFile.Width  := fVCLCanvasSize.cx;
  fVCLCurrentMetaFile.Height := fVCLCanvasSize.cy;
  if fVCLMetaFileCompressed<>'' then begin
    SetLength(tmp,SynLZdecompressdestlen(pointer(fVCLMetaFileCompressed)));
    SynLZdecompress1(Pointer(fVCLMetaFileCompressed),length(fVCLMetaFileCompressed),pointer(tmp));
    Stream := TRawByteStringStream.Create(tmp);
    try
      fVCLCurrentMetaFile.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TPdfPageGDI.CreateVCLCanvas;
begin
  SetVCLCurrentMetaFile;
  fVCLCurrentCanvas := TMetaFileCanvas.Create(fVCLCurrentMetaFile,FDoc.FDC);
end;

procedure TPdfPageGDI.FlushVCLCanvas;
var Stream: TRawByteStringStream;
    len: integer;
begin
  if (self=nil) or (fVCLCurrentCanvas=nil) then
    exit;
  FreeAndNil(fVCLCurrentCanvas);
  assert(fVCLCurrentMetaFile<>nil);
  Stream := TRawByteStringStream.Create;
  try
    fVCLCurrentMetaFile.SaveToStream(Stream);
    len := Length(Stream.DataString);
    SetLength(fVCLMetaFileCompressed,SynLZcompressdestlen(len));
    SetLength(fVCLMetaFileCompressed,
      SynLZcompress1(pointer(Stream.DataString),len,pointer(fVCLMetaFileCompressed)));
  finally
    Stream.Free;
  end;
  FreeAndNil(fVCLCurrentMetaFile);
end;


destructor TPdfPageGDI.Destroy;
begin
  FreeAndNil(fVCLCurrentCanvas);
  FreeAndNil(fVCLCurrentMetaFile);
  inherited;
end;

type
  TFontSpec = packed record
    angle: SmallInt; // -360..+360
    ascent, descent, cell: SmallInt;
  end;

  TPdfEnumStatePen = record
    null: boolean;
    color, style: integer;
    width: Single;
  end;

  /// a state of the EMF enumeration engine, for the PDF canvas
  // - used also for the SaveDC/RestoreDC stack
  TPdfEnumState = record
    Position: TPoint;
    Moved: boolean;
    WinSize, ViewSize: TSize;
    WinOrg, ViewOrg: TPoint;
    //transformation and clipping
    WorldTransform: XFORM; //current
    MetaRgn: TPdfBox;      //clipping
    ClipRgn: TPdfBox;      //clipping
    ClipRgnNull: Boolean;  //clipping
    MappingMode: Integer;
    PolyFillMode: Integer;
    StretchBltMode: Integer;
    ArcDirection: Integer;
    // current selected pen
    pen: TPdfEnumStatePen;
    // current selected brush
    brush: record
      null: boolean;
      color: integer;
      style: integer;
    end;
    // current selected font
    font: record
      color: integer;
      align: integer;
      BkMode, BkColor: integer;
      spec: TFontSpec;
      LogFont: TLogFontW; // better be the last entry in TPdfEnumState record
    end;
  end;

  /// internal data used during drawing
  // - contain the EMF enumeration engine state parameters
  TPdfEnum = class
  private
    fStrokeColor: integer;
    fFillColor: integer;
    fPenStyle: integer;
    fPenWidth: Single;
    fInLined: boolean;
    fInitTransformMatrix: XFORM;
    fInitMetaRgn: TPdfBox;
    procedure SetFillColor(const Value: integer);
    procedure SetStrokeColor(const Value: integer);
  protected
    Canvas: TPdfCanvas;
    // the pen/font/brush objects table, indexed like the THandleTable
    obj: array of record
      case kind: integer of
      OBJ_PEN:   (PenColor, PenStyle: integer; PenWidth: Single);
      OBJ_FONT:  (FontSpec: TFontSpec; LogFont: TLogFontW);
      OBJ_BRUSH: (BrushColor: integer; BrushNull: boolean; BrushStyle: integer);
    end;
    // SaveDC/RestoreDC stack
    nDC: integer;
    DC: array[0..31] of TPdfEnumState;
  public
    constructor Create(ACanvas: TPdfCanvas);
    procedure SaveDC;
    procedure RestoreDC;
    procedure NeedPen;
    procedure NeedBrushAndPen;
    procedure FlushPenBrush;
    procedure SelectObjectFromIndex(iObject: integer);
    procedure TextOut(var R: TEMRExtTextOut);
    procedure ScaleMatrix(Custom: PXForm; iMode: Integer);
    procedure HandleComment(Kind: TPdfGDIComment; P: PAnsiChar; Len: integer);
    procedure CreateFont(aLogFont: PEMRExtCreateFontIndirect);
    // if Canvas.Doc.JPEGCompression<>0, draw not as a bitmap but jpeg encoded
    procedure DrawBitmap(xs,ys,ws,hs, xd,yd,wd,hd,usage: integer;
      Bmi: PBitmapInfo; bits: pointer; clipRect: PRect; xSrcTransform: PXForm; dwRop: DWord;
      transparent: TPdfColorRGB = $FFFFFFFF);
    procedure FillRectangle(const Rect: TRect; ResetNewPath: boolean);
    // the current value set to SetRGBFillColor (rg)
    property FillColor: integer read fFillColor write SetFillColor;
    // the current value set to SetRGBStrokeColor (RG)
    property StrokeColor: integer read fStrokeColor write SetStrokeColor;
    // WorldTransform
    property InitTransformMatrix: XFORM read fInitTransformMatrix write fInitTransformMatrix;
    // MetaRgn - clipping
    procedure InitMetaRgn(ClientRect: TRect);
    procedure SetMetaRgn;
    // intersect - clipping
    function IntersectClipRect(const ClpRect: TPdfBox; const CurrRect: TPdfBox): TPdfBox;
    procedure ExtSelectClipRgn(data: PRgnDataHeader; iMode: DWord);
    // get current clipping area
    function GetClipRect: TPdfBox;
    procedure GradientFill(data: PEMGradientFill);
    procedure PolyPoly(data: PEMRPolyPolygon; iType: Integer);
  end;

const
  STOCKBRUSHCOLOR: array[WHITE_BRUSH..BLACK_BRUSH] of integer = (
    clWhite, $AAAAAA, $808080, $666666, clBlack);
  STOCKPENCOLOR: array[WHITE_PEN..BLACK_PEN] of integer = (
    clWhite, clBlack);

function CenterPoint(const Rect: TRect): TPoint; {$ifdef HASINLINE}inline;{$endif}
begin
  result.X := (Rect.Right+Rect.Left) div 2;
  result.Y := (Rect.Bottom+Rect.Top) div 2;
end;

/// EMF enumeration callback function, called from GDI
// - draw most content on PDF canvas (do not render 100% GDI content yet)
function EnumEMFFunc(DC: HDC; var Table: THandleTable; R: PEnhMetaRecord;
   NumObjects: DWord; E: TPdfEnum): LongBool; stdcall;
var i: integer;
    InitTransX: XForm;
    polytypes: PByteArray;
begin
  result := true;
  with E.DC[E.nDC] do
  case R^.iType of
  EMR_HEADER: begin
    SetLength(E.obj,PEnhMetaHeader(R)^.nHandles);
    WinOrg.X := 0;
    WinOrg.Y := 0;
    ViewOrg.X := 0;
    ViewOrg.Y := 0;
    MappingMode := GetMapMode(DC);
    PolyFillMode := GetPolyFillMode(DC);
    StretchBltMode := GetStretchBltMode(DC);
    ArcDirection := AD_COUNTERCLOCKWISE;
    InitTransX := DefaultIdentityMatrix;
    E.InitTransformMatrix := InitTransX;
    E.ScaleMatrix(@InitTransX, MWT_SET); // keep init
    E.InitMetaRgn(PEnhMetaHeader(R)^.rclBounds);
  end;
  EMR_SETWINDOWEXTEX:
    WinSize := PEMRSetWindowExtEx(R)^.szlExtent;
  EMR_SETWINDOWORGEX:
    WinOrg := PEMRSetWindowOrgEx(R)^.ptlOrigin;
  EMR_SETVIEWPORTEXTEX:
    ViewSize := PEMRSetViewPortExtEx(R)^.szlExtent;
  EMR_SETVIEWPORTORGEX:
    ViewOrg := PEMRSetViewPortOrgEx(R)^.ptlOrigin;
  EMR_SETBKMODE:
    font.BkMode := PEMRSetBkMode(R)^.iMode;
  EMR_SETBKCOLOR:
    if PEMRSetBkColor(R)^.crColor=cardinal(clNone) then
      font.BkColor := 0 else
      font.BkColor := PEMRSetBkColor(R)^.crColor;
  EMR_SETTEXTCOLOR:
    if PEMRSetTextColor(R)^.crColor=cardinal(clNone) then
      font.Color := 0 else
      font.Color := PEMRSetTextColor(R)^.crColor;
  EMR_SETTEXTALIGN:
    font.Align := PEMRSetTextAlign(R)^.iMode;
  EMR_EXTTEXTOUTA, EMR_EXTTEXTOUTW:
      E.TextOut(PEMRExtTextOut(R)^);
  EMR_SAVEDC:
    E.SaveDC;
  EMR_RESTOREDC:
    E.RestoreDC;
  EMR_SETWORLDTRANSFORM:
    E.ScaleMatrix(@PEMRSetWorldTransform(R)^.xform, MWT_SET);
  EMR_CREATEPEN:
    with PEMRCreatePen(R)^ do
    if ihPen-1<cardinal(length(E.Obj)) then
      with E.obj[ihPen-1] do begin
        kind := OBJ_PEN;
        PenColor := lopn.lopnColor;
        PenWidth := lopn.lopnWidth.X;
        PenStyle := lopn.lopnStyle;
      end;
  EMR_CREATEBRUSHINDIRECT:
    with PEMRCreateBrushIndirect(R)^ do
    if ihBrush-1<cardinal(length(E.Obj)) then
      with E.obj[ihBrush-1] do begin
        kind := OBJ_BRUSH;
        BrushColor := lb.lbColor;
        BrushNull := (lb.lbStyle=BS_NULL);
        BrushStyle := lb.lbStyle;
      end;
  EMR_EXTCREATEFONTINDIRECTW:
    E.CreateFont(PEMRExtCreateFontIndirect(R));
  EMR_DELETEOBJECT:
    with PEMRDeleteObject(R)^ do
      if ihObject-1<cardinal(length(E.Obj)) then // avoid GPF
        E.obj[ihObject-1].kind := 0;
  EMR_SELECTOBJECT:
    E.SelectObjectFromIndex(PEMRSelectObject(R)^.ihObject);
  EMR_MOVETOEX: begin
    Position := PEMRMoveToEx(R)^.ptl; // temp var to ignore unused moves
    if E.Canvas.FNewPath then begin
      E.Canvas.MoveToI(Position.X,Position.Y);
      Moved := true;
    end else
      Moved := false;
  end;
  EMR_LINETO: begin
    E.NeedPen;
    if not E.Canvas.FNewPath and not Moved then
      E.Canvas.MoveToI(Position.X,Position.Y);
    E.Canvas.LineToI(PEMRLineTo(R)^.ptl.X,PEMRLineTo(R)^.ptl.Y);
    Position := PEMRLineTo(R)^.ptl;
    Moved := false;
    E.fInLined := true;
    if not E.Canvas.FNewPath then
      if not pen.null then
        E.Canvas.Stroke
  end;
  EMR_RECTANGLE, EMR_ELLIPSE: begin
    E.NeedBrushAndPen;
    with E.Canvas.BoxI(PEMRRectangle(R)^.rclBox,true) do
    case R^.iType of
      EMR_RECTANGLE: E.Canvas.Rectangle(Left,Top,Width,Height);
      EMR_ELLIPSE:   E.Canvas.Ellipse(Left,Top,Width,Height);
    end;
    E.FlushPenBrush;
  end;
  EMR_ROUNDRECT: begin
    NormalizeRect(PEMRRoundRect(R)^.rclBox);
    E.NeedBrushAndPen;
    with PEMRRoundRect(R)^ do
      E.Canvas.RoundRectI(rclBox.left,rclBox.top,rclBox.right,rclBox.bottom,
        szlCorner.cx,szlCorner.cy);
    E.FlushPenBrush;
  end;
  {$ifdef USE_ARC}
  EMR_ARC: begin
    NormalizeRect(PEMRARC(R)^.rclBox);
    E.NeedPen;
    with PEMRARC(R)^, CenterPoint(rclBox) do
    E.Canvas.ARCI(x, y, rclBox.Right-rclBox.Left, rclBox.Bottom-rclBox.Top,
      ptlStart.x, ptlStart.y, ptlEnd.x, ptlEnd.y,
      e.dc[e.nDC].ArcDirection = AD_CLOCKWISE,
      acArc, Position);
      E.Canvas.Stroke;
   end;
  EMR_ARCTO: begin
    NormalizeRect(PEMRARCTO(R)^.rclBox);
    E.NeedPen;
     if not E.Canvas.FNewPath and not Moved then
      E.Canvas.MoveToI(Position.X,Position.Y);
    with PEMRARC(R)^, CenterPoint(rclBox) do  begin
    // E.Canvas.LineTo(ptlStart.x, ptlStart.y);
    E.Canvas.ARCI(x, y, rclBox.Right-rclBox.Left, rclBox.Bottom-rclBox.Top,
      ptlStart.x, ptlStart.y, ptlEnd.x, ptlEnd.y,
      e.dc[e.nDC].ArcDirection = AD_CLOCKWISE,
      acArcTo,
      Position);
    Moved := false;
    E.fInLined := true;
    if not E.Canvas.FNewPath then
      if not pen.null then
        E.Canvas.Stroke ;
     end;
   end;
  EMR_PIE: begin
    NormalizeRect(PEMRPie(R)^.rclBox);
    E.NeedBrushAndPen;
    with PEMRPie(R)^, CenterPoint(rclBox) do
      E.Canvas.ARCI(x, y, rclBox.Right-rclBox.Left, rclBox.Bottom-rclBox.Top,
        ptlStart.x, ptlStart.y, ptlEnd.x, ptlEnd.y,
        e.dc[e.nDC].ArcDirection = AD_CLOCKWISE,
        acPie, Position);
      if pen.null then
        E.Canvas.Fill else
        E.Canvas.FillStroke;
    end;
  EMR_CHORD: begin
    NormalizeRect(PEMRChord(R)^.rclBox);
    E.NeedBrushAndPen;
    with PEMRChord(R)^, CenterPoint(rclBox) do
      E.Canvas.ARCI(x, y, rclBox.Right-rclBox.Left, rclBox.Bottom-rclBox.Top,
        ptlStart.x, ptlStart.y, ptlEnd.x, ptlEnd.y,
        e.dc[e.nDC].ArcDirection = AD_CLOCKWISE,
        acChoord,Position);
      if pen.null then
        E.Canvas.Fill else
        E.Canvas.FillStroke;
  end;
  {$endif USE_ARC}
  EMR_FILLRGN: begin
    E.SelectObjectFromIndex(PEMRFillRgn(R)^.ihBrush);
    E.NeedBrushAndPen;
    E.FillRectangle(PRgnDataHeader(@PEMRFillRgn(R)^.RgnData[0])^.rcBound,false);
  end;
  EMR_POLYGON, EMR_POLYLINE, EMR_POLYGON16, EMR_POLYLINE16:
  if not brush.null or not pen.null then begin
    if R^.iType in [EMR_POLYGON,EMR_POLYGON16] then
      E.NeedBrushAndPen else
      E.NeedPen;
    if R^.iType in [EMR_POLYGON, EMR_POLYLINE] then begin
      E.Canvas.MoveToI(PEMRPolyLine(R)^.aptl[0].X,PEMRPolyLine(R)^.aptl[0].Y);
      for i := 1 to PEMRPolyLine(R)^.cptl-1 do
        E.Canvas.LineToI(PEMRPolyLine(R)^.aptl[i].X,PEMRPolyLine(R)^.aptl[i].Y);
      if PEMRPolyLine(R)^.cptl>0 then
        Position := PEMRPolyLine(R)^.aptl[PEMRPolyLine(R)^.cptl-1] else
        Position := PEMRPolyLine(R)^.aptl[0];
    end else begin
      E.Canvas.MoveToI(PEMRPolyLine16(R)^.apts[0].X,PEMRPolyLine16(R)^.apts[0].Y);
      if PEMRPolyLine16(R)^.cpts>0 then begin
        for i := 1 to PEMRPolyLine16(R)^.cpts-1 do
          E.Canvas.LineToI(PEMRPolyLine16(R)^.apts[i].X,PEMRPolyLine16(R)^.apts[i].Y);
        with PEMRPolyLine16(R)^.apts[PEMRPolyLine16(R)^.cpts-1] do begin
          Position.X := X;
          Position.Y := Y;
        end;
      end else begin
        Position.X := PEMRPolyLine16(R)^.apts[0].X;
        Position.Y := PEMRPolyLine16(R)^.apts[0].Y;
      end;
    end;
    Moved := false;
    if R^.iType in [EMR_POLYGON,EMR_POLYGON16] then begin
      E.Canvas.Closepath;
      E.FlushPenBrush;
    end else
    if not pen.null then
      E.Canvas.Stroke else // for lines
      E.Canvas.NewPath;
  end;
  EMR_POLYPOLYGON, EMR_POLYPOLYGON16, EMR_POLYPOLYLINE, EMR_POLYPOLYLINE16:
    E.PolyPoly(PEMRPolyPolygon(R), R^.iType);
  EMR_POLYBEZIER: begin
    if not pen.null then
      E.NeedPen;
    E.Canvas.MoveToI(PEMRPolyBezier(R)^.aptl[0].X,PEMRPolyBezier(R)^.aptl[0].Y);
    for i := 0 to (PEMRPolyBezier(R)^.cptl div 3)-1 do
      E.Canvas.CurveToCI(PEMRPolyBezier(R)^.aptl[i*3+1].X,PEMRPolyBezier(R)^.aptl[i*3+1].Y,
        PEMRPolyBezier(R)^.aptl[i*3+2].X,PEMRPolyBezier(R)^.aptl[i*3+2].Y,
        PEMRPolyBezier(R)^.aptl[i*3+3].X,PEMRPolyBezier(R)^.aptl[i*3+3].Y);
    if PEMRPolyBezier(R)^.cptl>0 then
      Position := PEMRPolyBezier(R)^.aptl[PEMRPolyBezier(R)^.cptl-1] else
      Position := PEMRPolyBezier(R)^.aptl[0];
    Moved := false;
    if not E.Canvas.FNewPath then
      if not pen.null then
        E.Canvas.Stroke else
        E.Canvas.NewPath;
  end;
  EMR_POLYBEZIER16: begin
    if not pen.null then
      E.NeedPen;
    E.Canvas.MoveToI(PEMRPolyBezier16(R)^.apts[0].X,PEMRPolyBezier16(R)^.apts[0].Y);
    if PEMRPolyBezier16(R)^.cpts>0 then begin
      for i := 0 to (PEMRPolyBezier16(R)^.cpts div 3)-1 do
        E.Canvas.CurveToCI(PEMRPolyBezier16(R)^.apts[i*3+1].X,PEMRPolyBezier16(R)^.apts[i*3+1].Y,
          PEMRPolyBezier16(R)^.apts[i*3+2].X,PEMRPolyBezier16(R)^.apts[i*3+2].Y,
          PEMRPolyBezier16(R)^.apts[i*3+3].X,PEMRPolyBezier16(R)^.apts[i*3+3].Y);
      with PEMRPolyBezier16(R)^.apts[PEMRPolyBezier16(R)^.cpts-1] do begin
        Position.X := X;
        Position.Y := Y;
      end;
    end else begin
      Position.X := PEMRPolyBezier16(R)^.apts[0].X;
      Position.Y := PEMRPolyBezier16(R)^.apts[0].Y;
    end;
    Moved := false;
    if not E.Canvas.FNewPath then
      if not pen.null then
        E.Canvas.Stroke else
        E.Canvas.NewPath;
  end;
  EMR_POLYBEZIERTO: begin
    if not pen.null then
      E.NeedPen;
    if not E.Canvas.FNewPath then
      if not Moved then
        E.Canvas.MoveToI(Position.X,Position.Y);
    if PEMRPolyBezierTo(R)^.cptl>0 then begin
      for i := 0 to (PEMRPolyBezierTo(R)^.cptl div 3)-1 do
        E.Canvas.CurveToCI(PEMRPolyBezierTo(R)^.aptl[i*3].X,PEMRPolyBezierTo(R)^.aptl[i*3].Y,
          PEMRPolyBezierTo(R)^.aptl[i*3+1].X,PEMRPolyBezierTo(R)^.aptl[i*3+1].Y,
          PEMRPolyBezierTo(R)^.aptl[i*3+2].X,PEMRPolyBezierTo(R)^.aptl[i*3+2].Y);
      Position := PEMRPolyBezierTo(R)^.aptl[PEMRPolyBezierTo(R)^.cptl-1];
    end;
    Moved := false;
    if not E.Canvas.FNewPath then
      if not pen.null then
        E.Canvas.Stroke else
        E.Canvas.NewPath;
  end;
  EMR_POLYBEZIERTO16: begin
    if not pen.null then
      E.NeedPen;
    if not E.Canvas.FNewPath then
      if not Moved then
        E.Canvas.MoveToI(Position.X,Position.Y);
    if PEMRPolyBezierTo16(R)^.cpts>0 then begin
      for i := 0 to (PEMRPolyBezierTo16(R)^.cpts div 3)-1 do
        E.Canvas.CurveToCI(PEMRPolyBezierTo16(R)^.apts[i*3].X,PEMRPolyBezierTo16(R)^.apts[i*3].Y,
          PEMRPolyBezierTo16(R)^.apts[i*3+1].X,PEMRPolyBezierTo16(R)^.apts[i*3+1].Y,
          PEMRPolyBezierTo16(R)^.apts[i*3+2].X,PEMRPolyBezierTo16(R)^.apts[i*3+2].Y);
      with PEMRPolyBezierTo16(R)^.apts[PEMRPolyBezierTo16(R)^.cpts-1] do begin
        Position.X := X;
        Position.Y := Y;
      end;
    end;
    Moved := false;
    if not E.Canvas.FNewPath then
      if not pen.null then
        E.Canvas.Stroke else
        E.Canvas.NewPath;
  end;
  EMR_POLYLINETO, EMR_POLYLINETO16: begin
    if not pen.null then
      E.NeedPen;
    if not E.Canvas.FNewPath then begin
      E.Canvas.NewPath;
      if not Moved then
        E.Canvas.MoveToI(Position.X,Position.Y);
    end;
    if R^.iType=EMR_POLYLINETO then begin
      if PEMRPolyLineTo(R)^.cptl>0 then begin
        for i := 0 to PEMRPolyLineTo(R)^.cptl-1 do
          E.Canvas.LineToI(PEMRPolyLineTo(R)^.aptl[i].X,PEMRPolyLineTo(R)^.aptl[i].Y);
        Position := PEMRPolyLineTo(R)^.aptl[PEMRPolyLineTo(R)^.cptl-1];
      end;
    end else // EMR_POLYLINETO16
    if PEMRPolyLineTo16(R)^.cpts>0 then begin
      for i := 0 to PEMRPolyLineTo16(R)^.cpts-1 do
        E.Canvas.LineToI(PEMRPolyLineTo16(R)^.apts[i].X,PEMRPolyLineTo16(R)^.apts[i].Y);
      with PEMRPolyLineTo16(R)^.apts[PEMRPolyLineTo16(R)^.cpts-1] do begin
        Position.X := X;
        Position.Y := Y;
      end;
    end;
    Moved := false;
    if not E.Canvas.FNewPath then
      if not pen.null then
        E.Canvas.Stroke else
        E.Canvas.NewPath;
  end;
  EMR_POLYDRAW:
  if PEMRPolyDraw(R)^.cptl>0 then begin
    if not pen.null then
      E.NeedPen;
    polytypes := @PEMRPolyDraw(R)^.aptl[PEMRPolyDraw(R)^.cptl];
    i := 0;
    while i<integer(PEMRPolyDraw(R)^.cptl) do begin
      case polytypes^[i] and not PT_CLOSEFIGURE of
        PT_LINETO: begin
          E.Canvas.LineToI(PEMRPolyDraw(R)^.aptl[i].X,PEMRPolyDraw(R)^.aptl[i].Y);
          if polytypes^[i] and PT_CLOSEFIGURE<>0 then begin
            E.Canvas.LineToI(Position.X, Position.Y);
            Position := PEMRPolyDraw(R)^.aptl[i];
          end;
        end;
        PT_BEZIERTO: begin
          E.Canvas.CurveToCI(PEMRPolyDraw(R)^.aptl[i+1].X,PEMRPolyDraw(R)^.aptl[i+1].Y,
            PEMRPolyDraw(R)^.aptl[i+2].X,PEMRPolyDraw(R)^.aptl[i+2].Y,
            PEMRPolyDraw(R)^.aptl[i+3].X,PEMRPolyDraw(R)^.aptl[i+3].Y);
          inc(i,3);
          if polytypes^[i] and PT_CLOSEFIGURE<>0 then begin
            E.Canvas.LineToI(Position.X, Position.Y);
            Position := PEMRPolyDraw(R)^.aptl[i];
          end;
        end;
        PT_MOVETO: begin
          E.Canvas.MoveToI(PEMRPolyDraw(R)^.aptl[i].X,PEMRPolyDraw(R)^.aptl[i].Y);
          Position := PEMRPolyDraw(R)^.aptl[i];
        end;
      else break; // invalid type
      end;
      inc(i);
    end;
    Position := PEMRPolyDraw(R)^.aptl[PEMRPolyDraw(R)^.cptl-1];
    Moved := False;
    if not E.Canvas.FNewPath then
      if not pen.null then
        E.Canvas.Stroke else
        E.Canvas.NewPath;
  end;
  EMR_POLYDRAW16:
  if PEMRPolyDraw16(R)^.cpts>0 then begin
    if not pen.null then
      E.NeedPen;
    polytypes := @PEMRPolyDraw16(R)^.apts[PEMRPolyDraw16(R)^.cpts];
    i := 0;
    while i<integer(PEMRPolyDraw16(R)^.cpts) do begin
      case polytypes^[i] and not PT_CLOSEFIGURE of
        PT_LINETO: begin
          E.Canvas.LineToI(PEMRPolyDraw16(R)^.apts[i].X,PEMRPolyDraw16(R)^.apts[i].Y);
          if polytypes^[i] and PT_CLOSEFIGURE<>0 then begin
            E.Canvas.LineToI(Position.X, Position.Y);
            with PEMRPolyDraw16(R)^.apts[i] do begin
              Position.X := x;
              Position.Y := y;
            end;
          end;
        end;
        PT_BEZIERTO: begin
          E.Canvas.CurveToCI(PEMRPolyDraw16(R)^.apts[i+1].X,PEMRPolyDraw16(R)^.apts[i+1].Y,
            PEMRPolyDraw16(R)^.apts[i+2].X,PEMRPolyDraw16(R)^.apts[i+2].Y,
            PEMRPolyDraw16(R)^.apts[i+3].X,PEMRPolyDraw16(R)^.apts[i+3].Y);
          inc(i,3);
          if polytypes^[i] and PT_CLOSEFIGURE<>0 then begin
            E.Canvas.LineToI(Position.X, Position.Y);
            with PEMRPolyDraw16(R)^.apts[i] do begin
              Position.X := x;
              Position.Y := y;
            end;
          end;
        end;
        PT_MOVETO: begin
          E.Canvas.MoveToI(PEMRPolyDraw16(R)^.apts[i].X,PEMRPolyDraw16(R)^.apts[i].Y);
          with PEMRPolyDraw16(R)^.apts[i] do begin
            Position.X := x;
            Position.Y := y;
          end;
        end;
      else break; // invalid type
      end;
      inc(i);
    end;
    with PEMRPolyDraw16(R)^.apts[PEMRPolyDraw16(R)^.cpts-1] do begin
      Position.X := x;
      Position.Y := y;
    end;
    Moved := false;
    if not E.Canvas.FNewPath then
      if not pen.null then
        E.Canvas.Stroke else
        E.Canvas.NewPath;
  end;
  EMR_BITBLT: begin
    with PEMRBitBlt(R)^ do // only handle RGB bitmaps (no palette)
      if (offBmiSrc<>0) and (offBitsSrc<>0) then begin
        E.DrawBitmap(xSrc,ySrc,cxDest,cyDest,xDest,yDest,cxDest,cyDest,iUsageSrc,
          pointer(PtrUInt(R)+offBmiSrc),pointer(PtrUInt(R)+offBitsSrc),
          @PEMRBitBlt(R)^.rclBounds, @PEMRBitBlt(R)^.xformSrc, PEMRBitBlt(R)^.dwRop);
      end else
    case PEMRBitBlt(R)^.dwRop of // we only handle PATCOPY = fillrect
      PATCOPY:
        with PEMRBitBlt(R)^ do
          E.FillRectangle(Rect(xDest,yDest,xDest+cxDest,yDest+cyDest),true);
    end;
  end;
  EMR_STRETCHBLT: begin
    with PEMRStretchBlt(R)^ do // only handle RGB bitmaps (no palette)
      if (offBmiSrc<>0) and (offBitsSrc<>0) then begin
        E.DrawBitmap(xSrc,ySrc,cxSrc,cySrc,xDest,yDest,cxDest,cyDest,iUsageSrc,
          pointer(PtrUInt(R)+offBmiSrc),pointer(PtrUInt(R)+offBitsSrc),
          @PEMRStretchBlt(R)^.rclBounds, @PEMRStretchBlt(R)^.xformSrc, PEMRStretchBlt(R)^.dwRop);
      end else
    case PEMRStretchBlt(R)^.dwRop of // we only handle PATCOPY = fillrect
      PATCOPY:
        with PEMRStretchBlt(R)^ do
          E.FillRectangle(Rect(xDest,yDest,xDest+cxDest,yDest+cyDest),true);
    end;
  end;
  EMR_STRETCHDIBITS:
    with PEMRStretchDIBits(R)^ do // only handle RGB bitmaps (no palette)
      if (offBmiSrc<>0) and (offBitsSrc<>0) then begin
        if WorldTransform.eM22 < 0 then
          with PBitmapInfo(PtrUInt(R)+offBmiSrc)^ do
            bmiHeader.biHeight := -bmiHeader.biHeight;
        E.DrawBitmap(xSrc,ySrc,cxSrc,cySrc,xDest,yDest,cxDest,cyDest,iUsageSrc,
          pointer(PtrUInt(R)+offBmiSrc),pointer(PtrUInt(R)+offBitsSrc),
          @PEMRStretchDIBits(R)^.rclBounds, nil, PEMRStretchDIBits(R)^.dwRop);
      end;
  EMR_TRANSPARENTBLT:
    with PEMRTransparentBLT(R)^ do // only handle RGB bitmaps (no palette)
      if (offBmiSrc<>0) and (offBitsSrc<>0) then
        E.DrawBitmap(xSrc,ySrc,cxSrc,cySrc,xDest,yDest,cxDest,cyDest,iUsageSrc,
          pointer(PtrUInt(R)+offBmiSrc),pointer(PtrUInt(R)+offBitsSrc),
          @PEMRTransparentBLT(R)^.rclBounds, @PEMRTransparentBLT(R)^.xformSrc,
          SRCCOPY, PEMRTransparentBLT(R)^.dwRop); // dwRop stores the transparent color
  EMR_GDICOMMENT:
    with PEMRGDICOMMENT(R)^ do
      if cbData>1 then
        E.HandleComment(TPdfGDIComment(Data[0]),PAnsiChar(@Data)+1,cbData-1);
  EMR_MODIFYWORLDTRANSFORM:
    with PEMRModifyWorldTransform(R)^ do
      E.ScaleMatrix(@PEMRModifyWorldTransform(R)^.xform, iMode);
  EMR_EXTCREATEPEN: // approx. - fast solution
    with PEMRExtCreatePen(R)^ do
    if ihPen-1<cardinal(length(E.Obj)) then
      with E.obj[ihPen-1] do begin
        kind := OBJ_PEN;
        PenColor := elp.elpColor;
        PenWidth := elp.elpWidth;
        PenStyle := elp.elpPenStyle and (PS_STYLE_MASK or PS_ENDCAP_MASK);
      end;
  EMR_SETMITERLIMIT:
    if PEMRSetMiterLimit(R)^.eMiterLimit>0.1 then
      E.Canvas.SetMiterLimit(PEMRSetMiterLimit(R)^.eMiterLimit);
  EMR_SETMETARGN:
    E.SetMetaRgn;
  EMR_EXTSELECTCLIPRGN:
    E.ExtSelectClipRgn(@PEMRExtSelectClipRgn(R)^.RgnData[0],PEMRExtSelectClipRgn(R)^.iMode);
  EMR_INTERSECTCLIPRECT:
    ClipRgn := E.IntersectClipRect(E.Canvas.BoxI(PEMRIntersectClipRect(r)^.rclClip,true),ClipRgn);
  EMR_SETMAPMODE:
    MappingMode := PEMRSetMapMode(R)^.iMode;
  EMR_BEGINPATH: begin
    E.Canvas.NewPath;
    if not Moved then begin
      E.Canvas.MoveToI(Position.X,Position.Y);
      Moved := true;
    end;
  end;
  EMR_ENDPATH:
    E.Canvas.fNewPath := false;
  EMR_ABORTPATH: begin
    E.Canvas.NewPath;
    E.Canvas.fNewPath := false;
    end;
  EMR_CLOSEFIGURE:
    E.Canvas.ClosePath;
  EMR_FILLPATH: begin
    if not brush.Null then begin
      E.FillColor := brush.color;
      E.Canvas.Fill;
    end;
    E.Canvas.NewPath;
    E.Canvas.fNewPath := false;
  end;
  EMR_STROKEPATH: begin
    if not pen.null then begin
      E.NeedPen;
      E.Canvas.Stroke;
    end;
    E.Canvas.NewPath;
    E.Canvas.fNewPath := false;
  end;
  EMR_STROKEANDFILLPATH: begin
    if not brush.Null then begin
      E.NeedPen;
      E.FillColor := brush.color;
      if not pen.null then
        if PolyFillMode=ALTERNATE then
          E.Canvas.EofillStroke else
          E.Canvas.FillStroke
      else if PolyFillMode=ALTERNATE then
        E.Canvas.EoFill else
        E.Canvas.Fill
    end else
    if not pen.null then begin
      E.NeedPen;
      E.Canvas.Stroke;
    end;
    E.Canvas.NewPath;
    E.Canvas.fNewPath := False;
  end;
  EMR_SETPOLYFILLMODE:
    PolyFillMode := PEMRSetPolyFillMode(R)^.iMode;
  EMR_GRADIENTFILL:
    E.GradientFill(PEMGradientFill(R));
  EMR_SETSTRETCHBLTMODE:
    StretchBltMode := PEMRSetStretchBltMode(R)^.iMode;
  EMR_SETARCDIRECTION:
    ArcDirection := PEMRSetArcDirection(R)^.iArcDirection;
  EMR_SETPIXELV: begin
    // prepare pixel size and color
    if pen.width<>1 then begin
      E.fPenWidth := E.Canvas.GetWorldFactorX * E.Canvas.FDevScaleX;
      E.Canvas.SetLineWidth(E.fPenWidth * E.Canvas.FFactorX);
    end;
    if PEMRSetPixelV(R)^.crColor<>Cardinal(pen.color) then
      E.Canvas.SetRGBStrokeColor(PEMRSetPixelV(R)^.crColor);
    // draw point
    Position := Point(PEMRSetPixelV(R)^.ptlPixel.X, PEMRSetPixelV(R)^.ptlPixel.Y);
    E.Canvas.PointI(Position.X,Position.Y);
    E.Canvas.Stroke;
    Moved := false;
    // roll back pixel size and color
    if pen.width<>1 then begin
      E.fPenWidth := pen.width * E.Canvas.GetWorldFactorX * E.Canvas.FDevScaleX;
      E.Canvas.SetLineWidth(E.fPenWidth * E.Canvas.FFactorX);
    end;
    if PEMRSetPixelV(R)^.crColor<>Cardinal(pen.color) then
      E.Canvas.SetRGBStrokeColor(pen.color);
  end;
  // TBD
  EMR_SMALLTEXTOUT,
  EMR_SETROP2,
  EMR_ALPHADIBBLEND,
  EMR_SETBRUSHORGEX,
  EMR_SETICMMODE,
  EMR_SELECTPALETTE,
  EMR_CREATEPALETTE,
  EMR_SETPALETTEENTRIES,
  EMR_RESIZEPALETTE,
  EMR_REALIZEPALETTE,
  EMR_EOF: ; //do nothing
  else
    R^.iType := R^.iType; // for debug purpose (breakpoint)
  end;
  case R^.iType of
    EMR_RESTOREDC,
    EMR_SETWINDOWEXTEX,
    EMR_SETWINDOWORGEX,
    EMR_SETVIEWPORTEXTEX,
    EMR_SETVIEWPORTORGEX,
    EMR_SETMAPMODE:
      E.ScaleMatrix(nil, MWT_SET); //recalc new transformation
  end;
end;

procedure TPdfCanvas.RenderMetaFile(MF: TMetaFile; ScaleX, ScaleY, XOff, YOff: single;
  TextPositioning: TPdfCanvasRenderMetaFileTextPositioning;
  KerningHScaleBottom, KerningHScaleTop: single;
  TextClipping: TPdfCanvasRenderMetaFileTextClipping);
var E: TPdfEnum;
    R: TRect;
begin
  R.Left := 0;
  R.Top := 0;
  R.Right := MF.Width;
  R.Bottom := MF.Height;
  if ScaleY=0 then
    ScaleY := ScaleX; // if ScaleY is ommited -> assume symetric coordinates
  E := TPdfEnum.Create(self);
  try
    FOffsetXDef := XOff;
    FOffsetYDef := YOff;
    FDevScaleX := ScaleX * FFactor;
    FDevScaleY := ScaleY * FFactor;
    FEmfBounds := R; // keep device rect
    fUseMetaFileTextPositioning := TextPositioning;
    fUseMetaFileTextClipping := TextClipping;
    fKerningHScaleBottom := KerningHScaleBottom;
    fKerningHScaleTop := KerningHScaleTop;
    if FDoc.FPrinterPxPerInch.X=0 then
      FDoc.FPrinterPxPerInch := CurrentPrinterRes; // caching for major speedup
    FPrinterPxPerInch := FDoc.FPrinterPxPerInch;
    with E.DC[0] do begin
      Int64(WinSize) := PInt64(@R.Right)^;
      ViewSize := WinSize;
    end;
    GSave;
    try
      EnumEnhMetaFile(fDoc.FDC,MF.Handle,@EnumEMFFunc,E,R);
    finally
      GRestore;
    end;
  finally
    E.Free;
  end;
end;



{ TPdfEnum }

constructor TPdfEnum.Create(ACanvas: TPdfCanvas);
begin
  Canvas := ACanvas;
  // set invalid colors or style -> force paint
  fFillColor := -1;
  fStrokeColor := -1;
  fPenStyle := -1;
  fPenWidth := -1;
  DC[0].brush.null := true;
  fInitTransformMatrix := DefaultIdentityMatrix;
  DC[0].WorldTransform := fInitTransformMatrix;
  fInitMetaRgn := PdfBox(0, 0, 0, 0);
  DC[0].ClipRgnNull := True;
  DC[0].MappingMode := MM_TEXT;
  DC[0].PolyFillMode := ALTERNATE;
  DC[0].StretchBltMode := STRETCH_DELETESCANS;
end;

procedure TPdfEnum.CreateFont(aLogFont: PEMRExtCreateFontIndirect);
var HF: HFONT;
    TM: TTextMetric;
    Old: HGDIOBJ;
    destDC: HDC;
begin
  destDC := Canvas.fDoc.FDC;
  HF := CreateFontIndirectW(aLogFont.elfw.elfLogFont);
  Old := SelectObject(destDC,HF);
  GetTextMetrics(destDC,TM);
  SelectObject(destDC,Old);
  DeleteObject(HF);
  if aLogFont^.ihFont-1<cardinal(length(obj)) then
    with obj[aLogFont^.ihFont-1] do begin
      kind := OBJ_FONT;
      MoveFast(aLogFont^.elfw.elfLogFont,LogFont,sizeof(LogFont));
      LogFont.lfPitchAndFamily := TM.tmPitchAndFamily;
      if LogFont.lfOrientation<>0 then
        FontSpec.angle := LogFont.lfOrientation div 10 else // -360..+360
        FontSpec.angle := LogFont.lfEscapement div 10;
      FontSpec.ascent := TM.tmAscent;
      FontSpec.descent := TM.tmDescent;
      FontSpec.cell := TM.tmHeight-TM.tmInternalLeading;
    end;
end;

procedure TPdfEnum.DrawBitmap(xs, ys, ws, hs, xd, yd, wd, hd, usage: integer;
  Bmi: PBitmapInfo; bits: pointer; clipRect: PRect; xSrcTransform: PXForm; dwRop: DWord;
  transparent: TPdfColorRGB = $FFFFFFFF);
var B: TBitmap;
    R: TRect;
    Box, ClipRc: TPdfBox;
    fIntFactorX, fIntFactorY, fIntOffsetX, fIntOffsetY: Single;
begin
  B := TBitmap.Create;
  try
    InitTransformation(xSrcTransform, fIntFactorX, fIntFactorY, fIntOffsetX, fIntOffsetY);
    // create a TBitmap with (0,0,ws,hs) bounds from DIB bits and info
    if Bmi^.bmiHeader.biBitCount=1 then
      B.Monochrome := true else
      B.PixelFormat := pf24bit;
    B.Width := ws;
    B.Height := hs;
    StretchDIBits(B.Canvas.Handle,0, 0, ws, hs, Trunc(xs+fIntOffsetX), Trunc(ys+fIntOffsetY),
      Trunc(ws*fIntFactorX), Trunc(hs*fIntFactorY), bits, Bmi^, usage, dwRop);
    if transparent <> $FFFFFFFF then begin
      if integer(transparent)<0 then
        transparent := GetSysColor(transparent and $ff);
      B.TransparentColor := transparent;
    end;
    // draw the bitmap on the PDF canvas
    with Canvas do begin
      R := Rect(xd, yd, wd+xd, hd+yd);
      NormalizeRect(R);
      Box := BoxI(R,true);
      ClipRc := GetClipRect;
      if (ClipRc.Width>0) and (ClipRc.Height>0) then
        Doc.CreateOrGetImage(B, @Box, @ClipRc) else // use cliping
        Doc.CreateOrGetImage(B, @Box, nil);
      // Doc.CreateOrGetImage() will reuse any matching TPdfImage
      // don't send bmi and bits parameters here, because of StretchDIBits above
    end;
  finally
    B.Free;
  end;
end;

// simulate gradient (not finished)
procedure TPdfEnum.GradientFill(data: PEMGradientFill);
type
  PTriVertex = ^TTriVertex;
  TTriVertex = packed record // circumvent some bug in older Delphi
    X: Longint;
    Y: Longint;
    Red: word; // COLOR16 wrongly defined in Delphi 6/7 e.g.
    Green: word;
    Blue: word;
    Alpha: word;
  end;
  PTriVertexArray = ^TTriVertexArray;
  TTriVertexArray = array[word] of TTriVertex;
  PGradientTriArray = ^TGradientTriArray;
  TGradientTriArray = array[word] of TGradientTriangle;
  PGradientRectArray = ^TGradientRectArray;
  TGradientRectArray = array[word] of TGradientRect;
var i: Integer;
    pGradientTriVertex: PTriVertexArray;
    pGradientTri: PGradientTriArray;
    pGradientRect: PGradientRectArray;
    pt1, pt2: PTriVertex;
//    Direction: TGradientDirection;
begin
  if data^.nVer>0 then begin
    pGradientTriVertex := @data.Ver;
    case data^.ulMode of
    GRADIENT_FILL_RECT_H, GRADIENT_FILL_RECT_V: begin
      Canvas.NewPath;
      pGradientRect := @pGradientTriVertex[data^.nVer];
{      Direction := gdHorizontal;
      if data^.ulMode = GRADIENT_FILL_RECT_V then
        Direction := gdVertical; }
      for i := 1 to data^.nTri do
      with pGradientRect[i-1] do begin
        pt1 := @pGradientTriVertex[UpperLeft];
        pt2 := @pGradientTriVertex[LowerRight];
        Canvas.MoveToI(pt1.X, pt1.Y);
        Canvas.LineToI(pt1.X, pt2.Y);
        Canvas.LineToI(pt2.X, pt2.Y);
        Canvas.LineToI(pt2.X, pt1.Y);
        Canvas.Closepath;
        Canvas.Fill;
      end;
    end;
    GRADIENT_FILL_TRIANGLE: begin
      Canvas.NewPath;
      pGradientTri := @pGradientTriVertex[data^.nVer];
      for i := 1 to data^.nTri do
      with pGradientTri[i-1] do begin
        with pGradientTriVertex[Vertex1] do begin
          FillColor := RGBA(Red,Green,Blue,0); // ignore Alpha
          Canvas.MoveToI(X,Y);
        end;
        with pGradientTriVertex[Vertex2] do Canvas.LineToI(X,Y);
        with pGradientTriVertex[Vertex3] do Canvas.LineToI(X,Y);
        with pGradientTriVertex[Vertex1] do Canvas.LineToI(X,Y);
        //DC[nDC].Moved := Point(pt1.X, pt1.Y);
        Canvas.Closepath;
        Canvas.Fill;
      end;
    end;
    end;
  end;
end;

procedure TPdfEnum.PolyPoly(data: PEMRPolyPolygon; iType: Integer);
var i, j, PolyOffs, PolyFirst: DWord;
    pPolyPointsArray: PPointArray;
    pPolyPointsArray16: PSmallPointArray;
    data16: PEMRPolyPolygon16 absolute data;
begin
  NeedBrushAndPen;
  if not Canvas.FNewPath then
    Canvas.NewPath;
  case iType of
    EMR_POLYPOLYGON, EMR_POLYPOLYLINE: begin
      PolyOffs := 0;
      pPolyPointsArray := pointer(PtrUInt(data) + SizeOf(TEMRPolyPolyline)
        - SizeOf(TPoint) + (data^.nPolys - 1)*SizeOf(DWORD));
      for i := 1 to data^.nPolys do begin
        PolyFirst := PolyOffs;
        Canvas.MoveToI(pPolyPointsArray[PolyOffs].X, pPolyPointsArray[PolyOffs].Y);
        Inc(PolyOffs);
        for j := 2 to data^.aPolyCounts[i-1] do begin
          Canvas.LineToI(pPolyPointsArray[PolyOffs].X, pPolyPointsArray[PolyOffs].Y);
          DC[nDC].Position := Point(pPolyPointsArray[PolyOffs].X, pPolyPointsArray[PolyOffs].Y);
          Inc(PolyOffs);
        end;
        Canvas.LineToI(pPolyPointsArray[PolyFirst].X, pPolyPointsArray[PolyFirst].Y);
        DC[nDC].Moved := false;
      end;
    end;
    EMR_POLYPOLYGON16, EMR_POLYPOLYLINE16: begin
      PolyOffs := 0;
      pPolyPointsArray16 := pointer(PtrUInt(data16) + SizeOf(TEMRPolyPolyline16)
        - SizeOf(TSmallPoint) + (data16^.nPolys - 1)*SizeOf(DWORD));
      for i := 1 to data16^.nPolys do begin
        PolyFirst := PolyOffs;
        Canvas.MoveToI(pPolyPointsArray16[PolyOffs].X, pPolyPointsArray16[PolyOffs].Y);
        Inc(PolyOffs);
        for j := 2 to data16^.aPolyCounts[i-1] do begin
          Canvas.LineToI(pPolyPointsArray16[PolyOffs].X, pPolyPointsArray16[PolyOffs].Y);
          DC[nDC].Position := Point(pPolyPointsArray16[PolyOffs].X, pPolyPointsArray16[PolyOffs].Y);
          Inc(PolyOffs);
        end;
        Canvas.LineToI(pPolyPointsArray16[PolyFirst].X, pPolyPointsArray16[PolyFirst].Y);
        DC[nDC].Moved := false;
      end;
    end;
  end;
  if iType in [EMR_POLYPOLYLINE, EMR_POLYPOLYLINE16] then begin // stroke
    if not DC[nDC].pen.null then
      Canvas.Stroke else
      Canvas.NewPath;
  end else begin // fill
    if not DC[nDC].brush.null then begin
      if not DC[nDC].pen.null then
        if DC[nDC].PolyFillMode=ALTERNATE then
          Canvas.EofillStroke else
          Canvas.FillStroke
      else if DC[nDC].PolyFillMode=ALTERNATE then
        Canvas.EoFill else
        Canvas.Fill
    end else
      if not DC[nDC].pen.null then
        Canvas.Stroke else
        Canvas.NewPath;
  end;
end;

procedure TPdfEnum.FillRectangle(const Rect: TRect; ResetNewPath: boolean);
begin
  if DC[nDC].brush.null then
    exit;
  Canvas.NewPath;
  FillColor := DC[nDC].brush.color;
  with Canvas.BoxI(Rect,true) do
    Canvas.Rectangle(Left,Top,Width,Height);
  Canvas.Fill;
  if ResetNewPath then
    Canvas.FNewPath := false;
end;

procedure TPdfEnum.FlushPenBrush;
begin
  with DC[nDC] do begin
    if brush.null then begin
      if not pen.null then
        Canvas.Stroke else
        Canvas.NewPath;
    end else
    if pen.null then
      Canvas.Fill else
      Canvas.FillStroke;
  end;
end;

procedure TPdfEnum.SelectObjectFromIndex(iObject: integer);
begin
  with DC[nDC] do begin
    if iObject<0 then begin // stock object?
      iObject := iObject and $7fffffff;
      case iObject of
        NULL_BRUSH:
          brush.null := true;
        WHITE_BRUSH..BLACK_BRUSH: begin
          brush.color := STOCKBRUSHCOLOR[iObject];
          brush.null := false;
        end;
        NULL_PEN: begin
          if fInLined and ((pen.style<>PS_NULL) or not pen.null) then begin
            fInLined := False;
            if not pen.null then
              Canvas.Stroke;
          end;
          pen.style := PS_NULL;
          pen.null := true;
        end;
        WHITE_PEN, BLACK_PEN: begin
          if fInLined and ((pen.color<>STOCKPENCOLOR[iObject]) or not pen.null) then begin
            fInLined := False;
            if not pen.null then
              Canvas.Stroke;
          end;
          pen.color := STOCKPENCOLOR[iObject];
          pen.null := false;
        end;
      end;
    end else
    if cardinal(iObject-1)<cardinal(length(Obj)) then // avoid GPF
      with Obj[iObject-1] do
      case Kind of // ignore any invalid reference
        OBJ_PEN: begin
          if fInLined and
            ((pen.color<>PenColor) or (pen.width<>PenWidth) or
             (pen.style<>PenStyle)) then begin
            fInLined := False;
            if not pen.null then
              Canvas.Stroke;
          end;
          pen.null := (PenWidth<0) or (PenStyle=PS_NULL); // !! 0 means as thick as possible
          pen.color := PenColor;
          pen.width := PenWidth;
          pen.style := PenStyle;
        end;
        OBJ_BRUSH: begin
          brush.null := BrushNull;
          brush.color := BrushColor;
          brush.style := BrushStyle;
        end;
        OBJ_FONT: begin
          font.spec := FontSpec;
          MoveFast(LogFont,font.LogFont,sizeof(LogFont));
        end;
      end;
  end;
end;

procedure TPdfEnum.HandleComment(Kind: TPdfGDIComment; P: PAnsiChar; Len: integer);
var Text: RawUTF8;
    W: integer;
begin
  try
    case Kind of
      pgcOutline:
      if Len>1 then begin
        SetString(Text,P+1,Len-1);
        Canvas.Doc.CreateOutline(UTF8ToString(Trim(Text)),PByte(P)^,
          Canvas.I2Y(DC[nDC].Position.Y));
      end;
      pgcBookmark: begin
        SetString(Text,P,Len);
        Canvas.Doc.CreateBookMark(Canvas.I2Y(DC[nDC].Position.Y),Text);
      end;
      pgcLink,pgcLinkNoBorder:
      if Len>Sizeof(TRect) then begin
        SetString(Text,P+SizeOf(TRect),Len-SizeOf(TRect));
        if Kind=pgcLink then
          W := 1 else
          W := 0;
        Canvas.Doc.CreateLink(Canvas.RectI(PRect(P)^,true),Text,abSolid,W);
      end;
    end;
  except
    on E: Exception do ; // ignore any error (continue EMF enumeration)
  end;
end;

procedure TPdfEnum.NeedBrushAndPen;
begin
  if fInlined then begin
    fInlined := false;
    Canvas.Stroke;
  end;
  NeedPen;
  with DC[nDC] do
  if not brush.null then
    FillColor := brush.color;
end;

procedure TPdfEnum.NeedPen;
begin
  with DC[nDC] do
  if not pen.null then begin
    StrokeColor := pen.color;
    if pen.style<>fPenStyle then begin
      case pen.style and PS_STYLE_MASK of
        PS_DASH:       Canvas.SetDash([4,4]);
        PS_DOT:        Canvas.SetDash([1,1]);
        PS_DASHDOT:    Canvas.SetDash([4,1,1,1]);
        PS_DASHDOTDOT: Canvas.SetDash([4,1,1,1,1,1]);
        else           Canvas.SetDash([]);
      end;
      case Pen.style and PS_ENDCAP_MASK of
        PS_ENDCAP_ROUND:  Canvas.SetLineCap(lcRound_End);
        PS_ENDCAP_SQUARE: Canvas.SetLineCap(lcProjectingSquareEnd);
        PS_ENDCAP_FLAT:   Canvas.SetLineCap(lcButt_End);
      end;
      fPenStyle := pen.style;
    end;
    if pen.width * Canvas.GetWorldFactorX * Canvas.FDevScaleX<>fPenWidth then begin
      if pen.width=0 then
        fPenWidth := Canvas.GetWorldFactorX * Canvas.FDevScaleX else
        fPenWidth := pen.width * Canvas.GetWorldFactorX * Canvas.FDevScaleX;
      Canvas.SetLineWidth(fPenWidth * Canvas.FFactorX);
    end;
  end else begin
    // pen.null need reset values
    fStrokeColor := -1;
    fPenWidth := -1;
    fPenStyle := -1;
  end;
end;

procedure TPdfEnum.RestoreDC;
begin
  Assert(nDC>0);
  dec(nDC);
end;

procedure TPdfEnum.SaveDC;
begin
  Assert(nDC<high(DC));
  DC[nDC+1] := DC[nDC];
  inc(nDC);
end;

procedure TPdfEnum.ScaleMatrix(Custom: PXForm; iMode: Integer);
var ScaleXForm: XForm;
    xdim, ydim: Single;
    mx, my: Integer;
begin
  if fInlined then begin
    fInlined := false;
    if not DC[nDC].pen.null then
      Canvas.Stroke;
  end;
  with DC[nDC], Canvas do begin
    FViewSize := ViewSize;
    FViewOrg := ViewOrg;
    FWinSize := WinSize;
    FWinOrg := WinOrg;
    case MappingMode of
      MM_TEXT: begin
        FViewSize.cx := 1;
        FViewSize.cy := 1;
        FWinSize.cx := 1;
        FWinSize.cy := 1;
      end;
      MM_LOMETRIC: begin
        FViewSize.cx := FPrinterPxPerInch.X;
        FViewSize.cy := -FPrinterPxPerInch.Y;
        FWinSize.cx := WinSize.cx*10;
        FWinSize.cy := WinSize.cy*10;
      end;
      MM_HIMETRIC: begin
        FViewSize.cx := FPrinterPxPerInch.X;
        FViewSize.cy := -FPrinterPxPerInch.Y;
        FWinSize.cx := WinSize.cx*100;
        FWinSize.cy := WinSize.cy*100;
      end;
      MM_LOENGLISH: begin
        FViewSize.cx := FPrinterPxPerInch.X;
        FViewSize.cy := -FPrinterPxPerInch.Y;
        FWinSize.cx := MulDiv(1000, WinSize.cx, 254);
        FWinSize.cy := MulDiv(1000, WinSize.cy, 254);
      end;
      MM_HIENGLISH: begin
        FViewSize.cx := FPrinterPxPerInch.X;
        FViewSize.cy := -FPrinterPxPerInch.Y;
        FWinSize.cx := MulDiv(10000, WinSize.cx, 254);
        FWinSize.cy := MulDiv(10000, WinSize.cy, 254);
      end;
      MM_TWIPS: begin
        FViewSize.cx := FPrinterPxPerInch.X;
        FViewSize.cy := -FPrinterPxPerInch.Y;
        FWinSize.cx := MulDiv(14400, WinSize.cx, 254);
        FWinSize.cy := MulDiv(14400, WinSize.cy, 254);
      end;
      MM_ISOTROPIC: begin
        FViewSize.cx := FPrinterPxPerInch.X;
        FViewSize.cy := -FPrinterPxPerInch.Y;
        FWinSize.cx := WinSize.cx*10;
        FWinSize.cy := WinSize.cy*10;
        xdim := Abs(FViewSize.cx * WinSize.cx / (FPrinterPxPerInch.X * FWinSize.cx));
        ydim := Abs(FViewSize.cy * WinSize.cy / (FPrinterPxPerInch.Y * FWinSize.cy));
        if (xdim > ydim) then begin
          if FViewSize.cx>=0 then
            mx := 1 else
            mx := -1;
          FViewSize.cx := Trunc(FViewSize.cx * ydim / xdim + 0.5);
          if FViewSize.cx=0 then
            FViewSize.cx := mx;
        end else begin
          if FViewSize.cy>=0 then
            my := 1 else
            my := -1;
          FViewSize.cy := Trunc(FViewSize.cy * xdim / ydim + 0.5);
          if FViewSize.cy=0 then
            FViewSize.cy := my;
        end;
      end;
      MM_ANISOTROPIC:
        ;  // TBD
    end;
    if FWinSize.cx=0 then // avoid EZeroDivide
      FFactorX := 1.0 else
      FFactorX := Abs(FViewSize.cx / FWinSize.cx);
    if FWinSize.cy=0 then // avoid EZeroDivide
      FFactorY := 1.0 else
      FFactorY := Abs(FViewSize.cy / FWinSize.cy);
    if Custom<>nil then begin
      // S.eM11=FFactorX S.eM12=0 S.eM21=0 S.eM22=FFactorY multiplied by Custom^
      case iMode of
        MWT_IDENTITY: // reset identity matrix
          WorldTransform := DefaultIdentityMatrix;
        MWT_LEFTMULTIPLY:
          WorldTransform := CombineTransform(Custom^, WorldTransform);
        MWT_RIGHTMULTIPLY:
          WorldTransform := CombineTransform(WorldTransform, Custom^);
        MWT_SET:
          WorldTransform := Custom^;
      end;
    end;
    // use transformation
    if Custom <> nil then
      ScaleXForm := Custom^
    else
      ScaleXForm := WorldTransform;
    if (ScaleXForm.eM11 > 0) and
       (ScaleXForm.eM22 > 0) and
       (ScaleXForm.eM12 = 0) and
       (ScaleXForm.eM21 = 0) then
    begin // Scale
      FWorldFactorX := ScaleXForm.eM11;
      FWorldFactorY := ScaleXForm.eM22;
      FWorldOffsetX := WorldTransform.eDx;
      FWorldOffsetY := WorldTransform.eDy;
    end
    else
    if (ScaleXForm.eM22 = ScaleXForm.eM11) and
       (ScaleXForm.eM21 = -ScaleXForm.eM12) then
    begin // Rotate
      FAngle := ArcSin(ScaleXForm.eM12) * c180divPI;
      FWorldCos := ScaleXForm.eM11;
      FWorldSin := ScaleXForm.eM12;
    end
    else
    if (ScaleXForm.eM11 = 0) and
       (ScaleXForm.eM22 = 0) and
       ((ScaleXForm.eM12 <> 0) or
       (ScaleXForm.eM21 <> 0)) then
    begin //Shear

    end
    else
    if ((ScaleXForm.eM11 < 0) or
        (ScaleXForm.eM22 < 0)) and
       (ScaleXForm.eM12 = 0) and
       (ScaleXForm.eM21 = 0) then
    begin //Reflection

    end;
  end;
end;

procedure TPdfEnum.InitMetaRgn(ClientRect: TRect);
begin
  fInitMetaRgn := Canvas.BoxI(ClientRect,true);
  DC[nDC].ClipRgnNull := True;
  DC[nDC].MetaRgn := fInitMetaRgn;
end;

procedure TPdfEnum.SetMetaRgn;
begin
  try
    with DC[nDC] do
    if not ClipRgnNull then begin
      MetaRgn := IntersectClipRect(ClipRgn, MetaRgn);
      FillCharFast(ClipRgn,sizeof(ClipRgn),0);
      ClipRgnNull := True;
    end;
  except
    on E: Exception do ; // ignore any error (continue EMF enumeration)
  end;
end;

function TPdfEnum.IntersectClipRect(const ClpRect: TPdfBox; const CurrRect: TPdfBox): TPdfBox;
begin
  Result := CurrRect;
  if (ClpRect.Width<>0) or (ClpRect.Height<>0) then begin // ignore null clipping area
    if ClpRect.Left > Result.Left then
      Result.Left := ClpRect.Left;
    if ClpRect.Top > Result.Top then
      Result.Top := ClpRect.Top;
    if (ClpRect.Left+ClpRect.Width) < (Result.Left+Result.Width) then
      Result.Width := (ClpRect.Left+ClpRect.Width) - Result.Left;
    if (ClpRect.Top + ClpRect.Height) < (Result.Top + Result.Height) then
      Result.Height := (ClpRect.Top + ClpRect.Height) - Result.Top;
    // fix rect
    if Result.Width<0 then
      Result.Width := 0;
    if Result.Height<0 then
      Result.Height := 0;
  end;
end;

procedure TPdfEnum.ExtSelectClipRgn(data: PRgnDataHeader; iMode: DWord);
var ExtClip: TRect;
begin
  try
    ExtClip := data^.rcBound;
    with DC[nDC] do
    case iMode of
      RGN_COPY: begin
        ClipRgn := MetaRgn;
        ClipRgnNull := False;
      end;
    end;
  except
    on E: Exception do ; // ignore any error (continue EMF enumeration)
  end;
end;

function TPdfEnum.GetClipRect: TPdfBox;
begin // get current clip area
  with DC[nDC] do
  if ClipRgnNull then
    Result := MetaRgn else
    Result := ClipRgn;
end;

procedure TPdfEnum.SetFillColor(const Value: integer);
begin
  if fFillColor=Value then
    exit;
  Canvas.SetRGBFillColor(Value);
  fFillColor := Value;
end;

procedure TPdfEnum.SetStrokeColor(const Value: integer);
begin
  if fStrokeColor=Value then
    exit;
  Canvas.SetRGBStrokeColor(Value);
  fStrokeColor := Value;
end;

function DXTextWidth(DX: PIntegerArray; n: Integer): integer;
var i: integer;
begin
  result := 0;
  for i := 0 to n-1 do
    inc(result,DX^[i]);
end;

procedure TPdfEnum.TextOut(var R: TEMRExtTextOut);
var nspace,i: integer;
    cur: cardinal;
    wW, measW, W,H,hscale: Single;
    DX: PIntegerArray; // not handled during drawing yet
    Posi: TPoint;
    AWidth, ASize, PosX, PosY: single;
    APDFFont: TPDFFont;
    tmp: array of WideChar; // R.emrtext is not #0 terminated -> use tmp[]
    tmpChar: array[0..1] of WideChar;
    a, acos, asin, fscaleX, fscaleY: single;
    AUseDX, WithClip, bOpaque: Boolean;
    ClipRect: TPdfBox;
    ASignX, ASignY: Integer;
    backRect: TRect;
    Positioning: TPdfCanvasRenderMetaFileTextPositioning;
    {$ifdef USE_UNISCRIBE}
    ADC: HDC;
    AnExtent: TSize;
    {$endif}

procedure DrawLine(var P: TPoint; aH: Single);
var tmp: TPdfEnumStatePen;
begin
  with DC[nDC] do begin
    tmp := Pen;
    pen.color := font.color;
    pen.width := aSize / 15 / Canvas.GetWorldFactorX / Canvas.FDevScaleX;
    pen.style := PS_SOLID;
    pen.null := False;
    NeedPen;
    if font.spec.angle=0 then begin
      // P = textout original coords
      // (-W,-H) = delta to text start pos (at baseline)
      // wW = text width
      // aH = delta H for drawed line (from baseline)
      Canvas.MoveToI(P.X  -W,(P.Y-(H-aH)));  //  deltax := -W     deltaY := (-H+aH)
      Canvas.LineToI(P.X -W+wW,(P.Y-(H-aH)));//  deltax := -W+wW  deltaY := (-H+aH)
    end else begin
      // rotation pattern :
      //       rdx = deltax * acos + deltay * asin
      //       rdy = deltay * acos - deltax * asin
      Canvas.MoveToI( P.X+( (-W)  *acos +(-H+aH)*asin ),
                      P.Y+((-H+aH)*acos -(-W)   *asin )  );

      Canvas.LineToI( P.X+((-W+wW) *acos +(-H+aH)*asin ),
                      P.Y+((-H+aH) *acos -(-W+wW)*asin )  );
    end;
    Canvas.Stroke;
    Pen := tmp;
    NeedPen;
  end;
end;

begin
  if R.emrtext.nChars>0 then
  with DC[nDC] do begin
    SetLength(tmp,R.emrtext.nChars+1); // faster than WideString for our purpose
    MoveFast(pointer(PtrUInt(@R)+R.emrtext.offString)^,tmp[0],R.emrtext.nChars*2);
    ASignY := 1;
    ASignX := 1;
    if (Canvas.FWorldFactorY) < 0 then
      ASignY := -1;
    if (Canvas.FWorldFactorX) < 0 then
      ASignX := -1;
    fscaleY := Abs(Canvas.fFactorY * Canvas.GetWorldFactorY * Canvas.FDevScaleY);
    fscaleX := Abs(Canvas.fFactorX * Canvas.GetWorldFactorX * Canvas.FDevScaleX);
    // guess the font size
    if font.LogFont.lfHeight<0 then
      ASize := Abs(font.LogFont.lfHeight)*fscaleY else
      ASize := Abs(font.spec.cell)*fscaleY;
    // ensure this font is selected (very fast if was already selected)
    APDFFont := Canvas.SetFont(Canvas.FDoc.FDC, font.LogFont, ASize);
    // calculate coordinates
    Positioning := Canvas.fUseMetaFileTextPositioning;
    if (R.emrtext.fOptions and ETO_GLYPH_INDEX<>0) then
      measW := 0 else begin
      AWidth := 0;
      {$ifdef USE_UNISCRIBE}
      if Assigned(APDFFont) and Canvas.fDoc.UseUniScribe and
         APDFFont.InheritsFrom(TPdfFontTrueType) then begin
        ADC := Canvas.FDoc.GetDCWithFont(TPdfFontTrueType(APDFFont));
        if GetTextExtentPoint32W(ADC,Pointer(tmp),R.emrtext.nChars,AnExtent) then
          AWidth := (AnExtent.cX * Canvas.FPage.FFontSize) / 1000;
      end;
      {$endif}
      if AWidth=0 then
        AWidth := Canvas.UnicodeTextWidth(Pointer(tmp));
      measW := Round(AWidth / fscaleX);
    end;
    AUseDX := R.emrtext.offDx > 0;
    {$ifdef USE_UNISCRIBE}
    if Canvas.fDoc.UseUniScribe then
      AUseDX := AUseDX and (R.emrtext.fOptions and ETO_GLYPH_INDEX <> 0);
    {$endif}
    if AUseDX then begin
      DX := pointer(PtrUInt(@R)+R.emrtext.offDx);
      W := DXTextWidth(DX, R.emrText.nChars);
      if W<R.rclBounds.Right-R.rclBounds.Left then // offDX=0 or within box
        DX := nil;
    end else
      DX := nil;
    if DX=nil then begin
      W := measW;
      if Positioning=tpExactTextCharacterPositining then
        Positioning := tpSetTextJustification; // exact position expects DX
    end;
    nspace := 0;
    hscale := 100;
    if measW<>0 then begin
      for i := 0 to R.emrtext.nChars-1 do
        if tmp[i]=' ' then
          inc(nspace);
      if (Positioning=tpSetTextJustification) and
         ((nspace=0) or ((W-measW)<nspace)) then
          Positioning := tpKerningFromAveragePosition;
      if (Positioning=tpExactTextCharacterPositining) and (font.spec.angle<>0) then
        Positioning := tpKerningFromAveragePosition;
      case Positioning of
      tpSetTextJustification:
        // we should have had a SetTextJustification() call -> modify word space
        with Canvas do
          SetWordSpace(((W-measW) * fscaleX)/nspace);
      tpKerningFromAveragePosition: begin
        // check if DX[] width differs from PDF width
        hscale := (W*100) / measW;
        // implement some global kerning if needed (allow hysteresis around 100%)
        if (hscale<Canvas.fKerningHScaleBottom) or
           (hscale>Canvas.fKerningHScaleTop) then begin
          if font.spec.angle=0 then
            Canvas.SetHorizontalScaling(hscale) else
            hscale := 100;
        end else
          hscale := 100;
      end;
      tpExactTextCharacterPositining:
        tmpChar[1] := #0;
      end;
    end else
      Positioning := tpSetTextJustification;
    wW := W;                                    // right x
    // H Align Mask = TA_CENTER or TA_RIGHT or TA_LEFT = TA_CENTER
    if (font.Align and TA_CENTER)=TA_CENTER then
      W := W/2                                  // center x
    else if (font.Align and TA_CENTER)=TA_LEFT then
      W := 0;                                   // left x
    // V Align mask = TA_BASELINE or TA_BOTTOM or TA_TOP = TA_BASELINE
    if (font.Align and TA_BASELINE) = TA_BASELINE then
      // always zero ?
      H := Abs(font.LogFont.lfHeight) - Abs(font.spec.cell)  // center y
    else if (font.Align and TA_BASELINE)= TA_BOTTOM then
      H := Abs(font.spec.descent)               // bottom y
    else
     // H := - Abs(font.spec.cell); // top
      // needs - vertical coords of baseline from top
      H := -abs(font.spec.ascent); // top
    if ASignY<0 then                            //inverted coordinates
      H := Abs(font.LogFont.lfHeight)+H;
    if ASignX<0 then
      W := W+wW;
    if (font.align and TA_UPDATECP)=TA_UPDATECP then
      Posi := Position else
      Posi := R.emrtext.ptlReference;
    // detect clipping
    if Canvas.fUseMetaFileTextClipping<>tcNeverClip then begin
      with R.emrtext.rcl do
        WithClip := (Right>Left) and (Bottom>Top);
      if WithClip then
        ClipRect := Canvas.BoxI(R.emrtext.rcl,true) else begin
        if Canvas.fUseMetaFileTextClipping=tcClipExplicit then
          with R.rclBounds do
            WithClip := (Right>Left) and (Bottom>Top);
        if WithClip then
          ClipRect := Canvas.BoxI(R.rclBounds,true) else begin
          WithClip := not ClipRgnNull and
                      (Canvas.fUseMetaFileTextClipping=tcAlwaysClip);
          if WithClip then
            ClipRect := GetClipRect;
        end;
      end;
    end else
      WithClip := False;
    bOpaque := not brush.null and (brush.Color<>clWhite) and
       ((R.emrtext.fOptions and ETO_OPAQUE<>0) or
        ((font.BkMode=OPAQUE) and (font.BkColor=brush.color)));
    if bOpaque then
      if WithClip then
        backRect := R.emrtext.rcl else begin
        backRect.TopLeft := Posi;
        backRect.BottomRight := Posi;
        inc(backRect.Right,Trunc(wW));
        inc(backRect.Bottom,Abs(font.LogFont.lfHeight));
      end;
    NormalizeRect(backRect);
    if WithClip then begin
      Canvas.GSave;
      Canvas.NewPath;
      Canvas.Rectangle(ClipRect.Left,ClipRect.Top,ClipRect.Width,ClipRect.Height);
      Canvas.ClosePath;
      Canvas.Clip;
      if bOpaque then begin
        FillRectangle(backRect,false);
        bOpaque := False; //do not handle more
      end else
        Canvas.NewPath;
      Canvas.fNewPath := False;
    end;
    // draw background (if any)
    if bOpaque then
      // don't handle BkMode, since global to the page, but only specific text
      // don't handle rotation here, since should not be used much
      FillRectangle(backRect,true);
    // draw text
    FillColor := font.color;
{$ifdef USE_UNISCRIBE}
    Canvas.RightToLeftText := (R.emrtext.fOptions and ETO_RTLREADING)<>0;
{$endif}
    Canvas.BeginText;
    if font.spec.angle<>0 then begin
      a := font.spec.angle*cPIdiv180;
      acos := cos(a);
      asin := sin(a);
      PosX := 0;
      PosY := 0;
      Canvas.SetTextMatrix(acos, asin, -asin, acos,
        Canvas.I2X(Posi.X-Round(W*acos+H*asin)),
        Canvas.I2Y(Posi.Y-Round(H*acos-W*asin)));
    end else
    if (WorldTransform.eM11 = WorldTransform.eM22) and
       (WorldTransform.eM12 = -WorldTransform.eM21) and
       not SameValue(ArcCos(WorldTransform.eM11), 0, 0.0001) then
    begin
      PosX := 0;
      PosY := 0;
      if SameValue(ArcCos(WorldTransform.eM11), 0, 0.0001) or       //0deg
         SameValue(ArcCos(WorldTransform.eM11), cPI, 0.0001) then   //180deg
        Canvas.SetTextMatrix(WorldTransform.eM11, WorldTransform.eM12, WorldTransform.eM21, WorldTransform.eM22,
          Canvas.I2X(posi.X * WorldTransform.eM11 + posi.Y * WorldTransform.eM21 + WorldTransform.eDx),
          Canvas.I2y(posi.X * WorldTransform.eM12 + posi.Y * WorldTransform.eM22 + WorldTransform.eDy))
      else
        Canvas.SetTextMatrix(-WorldTransform.eM11, -WorldTransform.eM12, -WorldTransform.eM21, -WorldTransform.eM22,
          Canvas.I2X(posi.X * WorldTransform.eM11 + posi.Y * WorldTransform.eM21 + WorldTransform.eDx),
          Canvas.I2y(posi.X * WorldTransform.eM12 + posi.Y * WorldTransform.eM22 + WorldTransform.eDy));
    end
    else
    begin
      acos := 0;
      asin := 0;
      if Canvas.fViewSize.cx>0 then
        PosX := Posi.X-W else // zero point left
        PosX := Posi.X+W;     //            right
      if Canvas.fViewSize.cy>0 then
        PosY := Posi.Y-H else // zero point beyond
        PosY := Posi.Y+H;     //            above
      Canvas.MoveTextPoint(Canvas.I2X(PosX),Canvas.I2Y(PosY));
    end;
    if (R.emrtext.fOptions and ETO_GLYPH_INDEX)<>0 then
      Canvas.ShowGlyph(pointer(tmp),R.emrtext.nChars) else
      if Positioning=tpExactTextCharacterPositining then begin
        cur := 0;
        repeat
          tmpChar[0] := tmp[cur];
          Canvas.ShowText(@tmpChar,false);
          if cur=R.emrtext.nChars-1 then
            break;
          PosX := PosX+DX^[cur];
          Canvas.EndText;
          Canvas.BeginText;
          Canvas.MoveTextPoint(Canvas.I2X(PosX),Canvas.I2Y(PosY));
          inc(cur);
        until false;
      end else
        Canvas.ShowText(pointer(tmp));
    Canvas.EndText;
    case Positioning of
    tpSetTextJustification:
      if nspace>0 then
        Canvas.SetWordSpace(0);
    tpKerningFromAveragePosition:
      if hscale<>100 then
        Canvas.SetHorizontalScaling(100); //reset hor. scaling
    end;
    // handle underline or strike out styles (direct draw PDF lines on canvas)
    if font.LogFont.lfUnderline<>0 then
      DrawLine(Posi, aSize / 8 / Canvas.GetWorldFactorX / Canvas.FDevScaleX);
    if font.LogFont.lfStrikeOut<>0 then
      DrawLine(Posi, - aSize / 3 / Canvas.GetWorldFactorX / Canvas.FDevScaleX);
    // end any pending clipped TextRect() region
    if WithClip then begin
      Canvas.GRestore;
      fFillColor := -1; // force set drawing color
    end;
    if not Canvas.FNewPath then begin
      if WithClip then
        if not DC[nDC].ClipRgnNull then begin
          ClipRect := GetClipRect;
          Canvas.GSave;
          Canvas.Rectangle(ClipRect.Left, ClipRect.Top, ClipRect.Width, ClipRect.Height);
          Canvas.Clip;
          Canvas.GRestore;
          Canvas.NewPath;
          Canvas.fNewPath := False;
        end;
    end else
      Canvas.fNewPath := False;
    if (font.align and TA_UPDATECP)=TA_UPDATECP then begin
      Position.X := Posi.X+Trunc(wW);
      Position.Y := Posi.Y;
    end;
  end;
end;


{ TPdfForm }

constructor TPdfForm.Create(aDoc: TPdfDocumentGDI; aMetaFile: TMetafile);
var P: TPdfPageGDI;
    FResources: TPdfDictionary;
    W,H: integer;
    oldPage: TPdfPage;
begin
  inherited Create(aDoc,true);
  W := aMetaFile.Width;
  H := aMetaFile.Height;
  P := TPdfPageGDI.Create(nil);
  try
    FResources := TPdfDictionary.Create(aDoc.FXref);
    FFontList := TPdfDictionary.Create(nil);
    FResources.AddItem('Font',FFontList);
    FResources.AddItem('ProcSet',TPdfArray.CreateNames(nil,['PDF','Text','ImageC']));
    with aDoc.FCanvas do begin
      oldPage := FPage;
      FPage := P;
      try
        FPageFontList := FFontList;
        FContents := self;
        FPage.SetPageHeight(H);
        FFactor := 1;
        RenderMetaFile(aMetaFile);
      finally
        if oldPage<>nil then
          SetPage(oldPage);
      end;
    end;
    FAttributes.AddItem('Type','XObject');
    FAttributes.AddItem('Subtype','Form');
    FAttributes.AddItem('BBox',TPdfArray.Create(nil,[0,0,W,H]));
    FAttributes.AddItem('Matrix',TPdfRawText.Create('[1 0 0 1 0 0]'));
    FAttributes.AddItem('Resources',FResources);
  finally
    P.Free;
  end;
end;

{$endif USE_METAFILE}

{$ifdef USE_BITMAP}

{ TPdfImage }

constructor TPdfImage.Create(aDoc: TPdfDocument; aImage: TGraphic; DontAddToFXref: boolean);
var B: TBitmap;
    PInc, y: integer;
    Pal: PDFString;
    Pals: array of TPaletteEntry;
    CA: TPdfArray;
    TransparentColor: TPdfColorRGB;
procedure NeedBitmap(PF: TPixelFormat);
begin
  B := TBitmap.Create; // create a temp bitmap (pixelformat may change)
  B.PixelFormat := PF;
  B.Width := fPixelWidth;
  B.Height := fPixelHeight;
  B.Canvas.Draw(0,0,aImage);
end;
procedure WritePal(P: PAnsiChar; Pal: PPaletteEntry);
var i: integer;
begin
  P^ := '<'; inc(P);
  for i := 0 to 255 do
  with Pal^ do begin
    P[0] := HexChars[peRed shr 4];
    P[1] := HexChars[peRed and $F];
    P[2] := HexChars[peGreen shr 4];
    P[3] := HexChars[peGreen and $F];
    P[4] := HexChars[peBlue shr 4];
    P[5] := HexChars[peBlue and $F];
    P[6] := ' ';
    inc(P,7);
    inc(Pal);
  end;
  P^ := '>';
end;
begin
  inherited Create(aDoc,DontAddToFXref);
  fPixelWidth := aImage.Width;
  fPixelHeight := aImage.Height;
  FAttributes.AddItem('Type','XObject');
  FAttributes.AddItem('Subtype','Image');
  if aImage.InheritsFrom(TJpegImage) then begin
    FAttributes.AddItem('ColorSpace','DeviceRGB');
    FFilter := 'DCTDecode';
    FWriter.Save; // flush to allow direct access to fDestStream
    with TJpegImage(aImage) do begin
      if aDoc.ForceJPEGCompression<>0 then
        CompressionQuality := aDoc.ForceJPEGCompression;
      {$ifdef USE_SYNGDIPLUS}
      if aDoc.ForceJPEGCompression=0 then // recompression only if necessary
        SaveInternalToStream(FWriter.fDestStream) else
      {$endif}
        SaveToStream(FWriter.fDestStream); // with CompressionQuality recompress
      end;
    FWriter.fDestStreamPosition := FWriter.fDestStream.Seek(0,soFromCurrent);
  end else begin
    if aImage.InheritsFrom(TBitmap) then
      B := TBitmap(aImage) else
      NeedBitmap(pf24bit);
    try
      case B.PixelFormat of
        pf1bit, pf4bit, pf8bit: begin
          if B.PixelFormat<>pf8bit then
            NeedBitmap(pf8bit);
          SetLength(Pals,256);
          if GetPaletteEntries(B.Palette,0,256,Pals[0])<>256 then
            raise EPdfInvalidValue.Create('TPdfImage');
          SetLength(Pal,7*256+2);
          WritePal(pointer(Pal),pointer(Pals));
          CA := TPdfArray.Create(nil);
          CA.AddItem(TPdfName.Create('Indexed'));
          CA.AddItem(TPdfName.Create('DeviceRGB'));
          CA.AddItem(TPdfNumber.Create(255));
          CA.AddItem(TPdfRawText.Create(Pal));
          FAttributes.AddItem('ColorSpace',CA);
          for y := 0 to fPixelHeight-1 do
            FWriter.Add(PAnsiChar(B.ScanLine[y]),fPixelWidth);
        end;
        else begin
          FAttributes.AddItem('ColorSpace','DeviceRGB');
          if not (B.PixelFormat in [pf24bit,pf32bit]) then
            NeedBitmap(pf24bit);
          if B.PixelFormat=pf24bit then
            PInc := 3 else
            PInc := 4;
          for y := 0 to fPixelHeight-1 do
            FWriter.AddRGB(B.ScanLine[y],PInc,fPixelWidth);
          if (PInc=3) and (B.TransparentMode=tmFixed) then begin
            // [ min1 max1 ... minn maxn ]
            TransparentColor := B.TransparentColor;
            FAttributes.AddItem('Mask',TPdfArray.CreateReals(nil,
              [(TransparentColor and $ff), (TransparentColor and $ff),
              (TransparentColor shr 8 and $ff), (TransparentColor shr 8 and $ff),
              (TransparentColor shr 16 and $ff), (TransparentColor shr 16 and $ff)]));
          end;
        end;
      end;
    finally
      if B<>aImage then
        B.Free;
    end;
  end;
  FAttributes.AddItem('Width',fPixelWidth);
  FAttributes.AddItem('Height',fPixelHeight);
  FAttributes.AddItem('BitsPerComponent',8);
end;

constructor TPdfImage.CreateJpegDirect(aDoc: TPdfDocument;
  const aJpegFileName: TFileName; DontAddToFXref: boolean);
var MS: THeapMemoryStream;
begin
  MS := THeapMemoryStream.Create;
  try
    MS.LoadFromFile(aJpegFileName);
    CreateJpegDirect(aDoc,MS,DontAddToFXRef);
  finally
    MS.Free;
  end;
end;

function GetJpegSize(jpeg: TMemoryStream; out width, height, BitDepth: integer): boolean;
var n: integer;
    b: byte;
    w: Word;
begin
  result := false;
  n := jpeg.Size-8;
  jpeg.Position := 0;
  if n<=0 then
    exit;
  jpeg.Read(w,2);
  if w<>$D8FF then
    exit; // invalid format
  jpeg.Read(b,1);
  while (jpeg.Position<n) and (b=$FF) do begin
    jpeg.Read(b,1);
    case b of
      $C0..$C3: begin
        jpeg.Seek(3,soFromCurrent);
        jpeg.Read(w,2);
        height := swap(w);
        jpeg.Read(w,2);
        width := swap(w);
        jpeg.Read(b,1);
        BitDepth := b*8;
        Result := true; // JPEG format OK
        exit;
      end;
      $FF:
        jpeg.Read(b,1);
      $D0..$D9, $01: begin
        jpeg.Seek(1,soFromCurrent);
        jpeg.Read(b,1);
      end;
      else begin
        jpeg.Read(w,2);
        jpeg.Seek(swap(w)-2, soFromCurrent);
        jpeg.Read(b,1);
      end;
    end;
  end;
end;

constructor TPdfImage.CreateJpegDirect(aDoc: TPdfDocument;
  aJpegFile: TMemoryStream; DontAddToFXref: boolean);
var Len, BitDepth: integer;
begin
  inherited Create(aDoc,DontAddToFXref);
  Len := aJpegFile.Size;
  if not GetJpegSize(aJpegFile,fPixelWidth,fPixelHeight,BitDepth) then
    exit; // JPEG format expected
  FAttributes.AddItem('Type','XObject');
  FAttributes.AddItem('Subtype','Image');
  FFilter := 'DCTDecode';
  FWriter.Save; // flush to allow direct access to fDestStream
  FWriter.Add(aJpegFile.Memory,Len);
  FWriter.fDestStreamPosition := FWriter.fDestStream.Seek(0,soFromCurrent);
  FAttributes.AddItem('Width',fPixelWidth);
  FAttributes.AddItem('Height',fPixelHeight);
  case BitDepth of
    8:  FAttributes.AddItem('ColorSpace','DeviceGray');
    24: FAttributes.AddItem('ColorSpace','DeviceRGB');
  end;
  FAttributes.AddItem('BitsPerComponent',8);
end;

{$endif USE_BITMAP}


{ TPdfFormWithCanvas }

constructor TPdfFormWithCanvas.Create(aDoc: TPdfDocument; W, H: Integer);
var FResources: TPdfDictionary;
begin
  inherited Create(aDoc,true);
  FResources := TPdfDictionary.Create(aDoc.FXref);
  FFontList := TPdfDictionary.Create(nil);
  FResources.AddItem('Font',FFontList);
  FResources.AddItem('ProcSet',TPdfArray.CreateNames(nil,['PDF','Text','ImageC']));
  FPage := TPdfPage.Create(nil);
  FCanvas := TPdfCanvas.Create(aDoc);
  FCanvas.FPage := FPage;
  FCanvas.FPageFontList := FFontList;
  FCanvas.FContents := self;
  FCanvas.FFactor := 1;
  FAttributes.AddItem('Type','XObject');
  FAttributes.AddItem('Subtype','Form');
  FAttributes.AddItem('BBox',TPdfArray.Create(nil,[0,0,W,H]));
  FAttributes.AddItem('Matrix',TPdfRawText.Create('[1 0 0 1 0 0]'));
  FAttributes.AddItem('Resources',FResources);
end;

destructor TPdfFormWithCanvas.Destroy;
begin
  CloseCanvas;
  inherited;
end;

procedure TPdfFormWithCanvas.CloseCanvas;
begin
  FreeAndNil(FCanvas);
  FreeAndNil(FPage);
end;


{$ifdef USE_PDFSECURITY}

{ TPdfEncryption }

constructor TPdfEncryption.Create(aLevel: TPdfEncryptionLevel;
  aPermissions: TPdfEncryptionPermissions;
  const aUserPassword, aOwnerPassword: string);
begin
  fLevel := aLevel;
  fPermissions := aPermissions;
  fUserPassword := aUserPassword;
  if aOwnerPassword='' then
    raise EPdfInvalidOperation.CreateFmt(
      '%s expect a non void owner password',[ClassName]);
  fOwnerPassword := aOwnerPassword;
end;

class function TPdfEncryption.New(aLevel: TPdfEncryptionLevel;
  const aUserPassword, aOwnerPassword: string;
  aPermissions: TPdfEncryptionPermissions): TPdfEncryption;
begin
  case aLevel of
  elRC4_40, elRC4_128:
    result := TPdfEncryptionRC4MD5.Create(aLevel,aPermissions,aUserPassword,aOwnerPassword);
  else
    result := nil;
  end;
end;

procedure TPdfEncryption.AttachDocument(aDoc: TPdfDocument);
begin
  fDoc := aDoc;
end;


{ TPdfEncryptionRC4MD5 }

const
  // see "Algorithm 3.2 Computing an encryption key" in the PDF reference doc
  PDF_PADDING: TPdfBuffer32 =
    ($28,$BF,$4E,$5E,$4E,$75,$8A,$41,$64,$00,$4E,$56,$FF,$FA,$01,$08,
     $2E,$2E,$00,$B6,$D0,$68,$3E,$80,$2F,$0C,$A9,$FE,$64,$53,$69,$7A);

procedure TPdfEncryptionRC4MD5.AttachDocument(aDoc: TPdfDocument);
procedure Pad(const source: string; var dest: TPdfBuffer32);
var L: integer;
    tmp: WinAnsiString;
begin
  tmp := StringToWinAnsi(source);
  L := Length(tmp);
  if L>SizeOf(dest) then
    L := SizeOf(dest) else
    MoveFast(PDF_PADDING,dest[L],SizeOf(dest)-L);
  MoveFast(pointer(tmp)^,dest,L);
end;
const HASHSIZE: array[elRC4_40..elRC4_128] of integer = (5,16);
      DICT: array[elRC4_40..elRC4_128] of record V,R,L: integer end =
        ((V:1;R:2;L:40),(V:2;R:3;L:128));
      FLAGPATTERN: array[elRC4_40..elRC4_128] of cardinal = ($FFFFFFC0,$FFFFF0C0);
      FLAGBIT: array[TPdfEncryptionPermission] of byte = (2,3,4,5,8,9,10,11);
var RC4: TRC4;
    MD5: TMD5;
    f: TPdfEncryptionPermission;
    Digest, Digest2: TMD5Digest;
    i,j: integer;
    own, usr: TPdfBuffer32;
begin
  inherited;
  // compute corresponding flags
  fFlags := FLAGPATTERN[fLevel];
  for f := low(f) to high(f) do
    if f in fPermissions then
      fFlags := fFlags or (1 shl FLAGBIT[f]);
  if fDoc.fFileFormat<pdf14 then
    fDoc.fFileFormat := pdf14;
  // calc fOwnerPass key (Algorithm 3.3 in PDF reference doc)
  Pad(fUserPassword,usr);
  Pad(fOwnerPassword,own);
  MD5.Full(@own,SizeOf(own),Digest);
  if fLevel=elRC4_128 then
    for i := 1 to 50 do
      MD5.Full(@Digest,sizeof(Digest),Digest);
  RC4.Init(Digest,HASHSIZE[fLevel]);
  RC4.Encrypt(usr,fOwnerPass,sizeof(fOwnerPass));
  if fLevel=elRC4_128 then
    for i := 1 to 19 do begin
      for j := 0 to high(Digest2) do
        Digest2[j] := Digest[j] xor i;
      RC4.Init(Digest2,sizeof(Digest2));
      RC4.Encrypt(fOwnerPass,fOwnerPass,sizeof(fOwnerPass));
    end;
  // calc main file key (Algorithm 3.2 in PDF reference doc)
  MD5.Init;
  MD5.Update(usr,SizeOf(usr));
  MD5.Update(fOwnerPass,sizeof(fOwnerPass));
  MD5.Update(fFlags,sizeof(fFlags));
  MD5.Update(aDoc.fFileID,sizeof(aDoc.fFileID));
  MD5.Final(Digest);
  if fLevel=elRC4_128 then
    for i := 1 to 50 do
      MD5.Full(@Digest,sizeof(Digest),Digest);
  SetLength(fInternalKey,HASHSIZE[fLevel]);
  MoveFast(Digest,fInternalKey[0],HASHSIZE[fLevel]);
  // calc fUserPass content
  case fLevel of
  elRC4_40: begin   // Algorithm 3.4 in PDF reference doc
    RC4.Init(fInternalKey[0],HASHSIZE[fLevel]);
    RC4.Encrypt(PDF_PADDING,fUserPass,sizeof(PDF_PADDING));
  end;
  elRC4_128: begin  // Algorithm 3.5 in PDF reference doc
    MD5.Init;
    MD5.Update(PDF_PADDING,sizeof(PDF_PADDING));
    MD5.Update(aDoc.fFileID,sizeof(aDoc.fFileID));
    MD5.Final(Digest);
    for i := 0 to 19 do begin
      for j := 0 to high(Digest2) do
        Digest2[j] := fInternalKey[j] xor i;
      RC4.Init(Digest2,SizeOf(Digest2));
      RC4.Encrypt(Digest,Digest,SizeOf(Digest));
    end;
    MoveFast(Digest,fUserPass,SizeOf(Digest));
    MoveFast(Digest,fUserPass[SizeOf(Digest)],SizeOf(Digest));
  end;
  end;
  // add encryption dictionary object
  if aDoc.fEncryptionObject=nil then
    aDoc.fEncryptionObject := TPdfDictionary.Create(aDoc.FXref);
  with aDoc.fEncryptionObject, DICT[fLevel] do begin
    AddItem('Filter','Standard');
    AddItem('V',V);
    AddItem('R',R);
    AddItem('Length',L);
    AddItem('P',fFlags); // expected to be written as signed integer
    AddItem('O',TPdfClearText.Create(@fOwnerPass,sizeof(fOwnerPass)));
    AddItem('U',TPdfClearText.Create(@fUserPass,sizeof(fUserPass)));
  end;
  aDoc.FTrailer.Attributes.AddItem('Encrypt',aDoc.fEncryptionObject);
end;

procedure TPdfEncryptionRC4MD5.EncodeBuffer(const BufIn; var BufOut; Count: cardinal);
// see http://www.cs.cmu.edu/~dst/Adobe/Gallery/anon21jul01-pdf-encryption.txt
// see "Algorithm 3.1 Encryption of data" in PDF Reference document
  procedure ComputeNewRC4Key;
  const KEYSIZE:  array[elRC4_40..elRC4_128] of integer = (10,16);
  var MD5: TMD5;
      Digest: TMD5Digest;
  begin
    MD5.Init;
    MD5.Update(fInternalKey[0],length(fInternalKey));
    MD5.Update(fDoc.fCurrentObjectNumber,3);
    MD5.Update(fDoc.fCurrentGenerationNumber,2);
    MD5.Final(Digest);
    fLastRC4Key.Init(Digest,KEYSIZE[fLevel]);
    fLastObjectNumber := fDoc.fCurrentObjectNumber;
    fLastGenerationNumber := fDoc.fCurrentGenerationNumber;
  end;
var work: TRC4; // Encrypt() changes the RC4 state -> local copy for reuse
begin
  if (fDoc.fCurrentObjectNumber<>fLastObjectNumber) or
     (fDoc.fCurrentGenerationNumber<>fLastGenerationNumber) then
    // a lot of string encodings have the same context
    ComputeNewRC4Key;
  work := fLastRC4Key;
  work.Encrypt(BufIn,BufOut,Count); // RC4 allows in-place encryption :)
end;

{$endif USE_PDFSECURITY}


{ TPdfObjectStream }

function TPdfObjectStream.AddObject(Value: TPdfObject): integer;
begin
  result := fObjectCount;
  inc(fObjectCount);
  if fObjectCount>length(fObject) then
    SetLength(fObject,fObjectCount+15);
  fObject[result].Number := Value.ObjectNumber;
  fObject[result].Position := fAddingStream.Position;
  Value.InternalWriteTo(fAddingStream);
end;

constructor TPdfObjectStream.Create(aDoc: TPdfDocument);
begin
  inherited Create(aDoc,false);
  Attributes.AddItem('Type','ObjStm');
  fAddingStream := TPdfWrite.Create(ADoc,THeapMemoryStream.Create);
end;

destructor TPdfObjectStream.Destroy;
begin
  fAddingStream.fDestStream.Free;
  fAddingStream.Free;
  inherited;
end;

procedure TPdfObjectStream.InternalWriteTo(W: TPdfWrite);
var i: integer;
begin
  Attributes.AddItem('N',fObjectCount);
  for i := 0 to fObjectCount-1 do
    with fObject[i] do
      Writer.Add(Number).Add(' ').Add(Position).Add(' ');
  Attributes.AddItem('First',Writer.Position);
  Writer.Add(fAddingStream.ToPDFString);
  inherited;
end;


initialization
  {$ifdef USE_SYNGDIPLUS}
  // initialize Gdi+ if necessary (and possible, i.e. not from a dll)
  if (Gdip=nil) and not IsLibrary then
    Gdip := TGDIPlus.Create('gdiplus.dll');
  {$endif}

finalization
  if (FontSub<>0) and (FontSub<>INVALID_HANDLE_VALUE) then
    FreeLibrary(FontSub);
end.


